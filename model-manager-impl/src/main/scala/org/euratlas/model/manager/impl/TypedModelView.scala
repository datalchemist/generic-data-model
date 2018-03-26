package org.euratlas.model.manager.impl

import akka.Done
import com.lightbend.lagom.scaladsl.persistence.AggregateEventTag
import com.lightbend.lagom.scaladsl.persistence.ReadSideProcessor
import com.lightbend.lagom.scaladsl.persistence.slick.SlickReadSide
import com.lightbend.lagom.scaladsl.persistence.EventStreamElement

import play.api.libs.json.{Format, Json,JsValue}

import PostgresDriverExt.api._

import scala.concurrent.{ExecutionContext,Future}

import org.euratlas.model.generic.DefaultGenericModel._

case class GenericInstance(modelId:String,entityClass:String,instanceId:String,instance:UntypedGenericEntityInstance,validationError:Option[String])

class TypedModelView(implicit ec: ExecutionContext) {
  
  implicit lazy val genericInstanceColumnType = MappedColumnType.base[UntypedGenericEntityInstance, JsValue](
    { b => Json.toJson(b) },   
    { i => Json.fromJson[UntypedGenericEntityInstance](i).get }
  )    
  
  class GenericInstances(tag: Tag) extends Table[GenericInstance](tag, "generic_instance") {
    def modelId = column[String]("model_id")
    def entityClass = column[String]("entity_class")
    def instanceId = column[String]("instance_id")
    def instance = column[UntypedGenericEntityInstance]("instance")
    def validationError = column[Option[String]]("validation_error")
    
    def pk = primaryKey("pk_generic_instance", (modelId,entityClass, instanceId))
    
    def * = (modelId,entityClass,instanceId,instance,validationError) <> ((GenericInstance.apply _).tupled, (GenericInstance.unapply _))
  }
  lazy val genericInstances = TableQuery[GenericInstances]  
  
  
  def createTableIfNotExists= {
    
    import slick.jdbc.meta.MTable

    MTable.getTables.flatMap { tables =>
      for {
        _ <-
          if (!tables.exists(_.name.name == genericInstances.baseTableRow.tableName)) {
            genericInstances.schema.create
          } else {
            DBIO.successful(())
          }
//        _ <-
//          if (!tables.exists(_.name.name == results.baseTableRow.tableName)) {
//            results.schema.create
//          } else {
//            DBIO.successful(())
//          }
      } yield Done
    }.transactionally
  }
  
  def addInstance(modelId:String,className:String,id:String)={
    genericInstances += GenericInstance(modelId,className,id,UntypedGenericEntityInstance(id,Map.empty,Map.empty,Map.empty),None)
  }
  def removeInstance(modelId:String,className:String,id:String)={
    genericInstances.filter(i => i.modelId===modelId && i.entityClass === className && i.instanceId === id).delete
  }
  def updateInstance(modelId:String,assertion:InstancePropertyAssertion)={
    for {
      cur <-genericInstances.filter(i => i.modelId===modelId && i.entityClass === assertion.entity.name && i.instanceId === assertion.entityInstance).result.headOption
      _ <- 
        cur match {
          case Some(c) =>
            genericInstances
            .filter(i => i.modelId===modelId && i.entityClass === assertion.entity.name && i.instanceId === assertion.entityInstance)
            .map(_.instance)
            .update(applyGenericInstancePropertyAssertion(c.instance,assertion))
          case None => DBIO.successful(Done)
        }
    } yield {
      cur
    }
  }
  
}

class TypedModelViewProcessor(
  readSide: SlickReadSide,
  repo: TypedModelView
) extends ReadSideProcessor[GenericModelEvent] {
  
  override def buildHandler(): ReadSideProcessor.ReadSideHandler[GenericModelEvent] = {
    readSide.builder[GenericModelEvent]("typed_view_offset")
    .setGlobalPrepare(repo.createTableIfNotExists)
    .setEventHandler[InstanceDefinitionAsserted](processInstanceDefinitionAsserted)
    .setEventHandler[InstancePropertyAsserted](processInstancePropertyAsserted)
    .build()
  }

  private def processInstanceDefinitionAsserted(eventElement: EventStreamElement[InstanceDefinitionAsserted]): DBIO[_] = 
    eventElement.event match {
      case InstanceDefinitionAsserted(modelId,EntityDefined(cls,name,prov)) => 
        repo.addInstance(modelId,cls.name,name)
      case InstanceDefinitionAsserted(modelId,EntityUndefined(cls,name,prov)) => 
        repo.removeInstance(modelId,cls.name,name)
    }
  
  private def processInstancePropertyAsserted(eventElement: EventStreamElement[InstancePropertyAsserted]): DBIO[_] = 
    eventElement.event match {
      case InstancePropertyAsserted(modelId,prevs,assertion) => repo.updateInstance(modelId,assertion)
    }
  
  override def aggregateTags: Set[AggregateEventTag[GenericModelEvent]] =  Set(GenericModelEvent.GenericModelEventTag)
}