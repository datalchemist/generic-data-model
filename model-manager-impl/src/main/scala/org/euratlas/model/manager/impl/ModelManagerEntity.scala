package org.euratlas.model.manager.impl

import java.util.UUID

import com.datastax.driver.core.utils.UUIDs

import java.time.LocalDateTime

import akka.Done
import com.lightbend.lagom.scaladsl.persistence.{AggregateEventTag,AggregateEvent,PersistentEntity}
import com.lightbend.lagom.scaladsl.persistence.PersistentEntity.ReplyType
import com.lightbend.lagom.scaladsl.playjson.{JsonSerializerRegistry, JsonSerializer}
import play.api.libs.json.{Format, Json,JsObject,JsValue}

import scala.collection.immutable.Seq

import org.euratlas.model.generic.DefaultGenericModel._

sealed trait GenericModelCommand[R] extends ReplyType[R]

case class AssertInstanceDefinition(modelId:String,assertion:InstanceDefinitionAssertion) extends GenericModelCommand[Done]
case class AssertInstanceProperty(modelId:String,assertion:InstancePropertyAssertion) extends GenericModelCommand[Done]

object AssertInstanceDefinition {implicit def fmt:Format[AssertInstanceDefinition]=Json.format[AssertInstanceDefinition]}
object AssertInstanceProperty {implicit def fmt:Format[AssertInstanceProperty]=Json.format[AssertInstanceProperty]}

sealed trait GenericModelEvent extends AggregateEvent[GenericModelEvent] {
  override def aggregateTag: AggregateEventTag[GenericModelEvent] =
    GenericModelEvent.GenericModelEventTag
}
object GenericModelEvent{
  val GenericModelEventTag = AggregateEventTag[GenericModelEvent]
}

case class InstanceDefinitionAsserted(modelId:String,assertion:InstanceDefinitionAssertion) extends GenericModelEvent
case class InstancePropertyAsserted(modelId:String,previousValue:Seq[JsValue],assertion:InstancePropertyAssertion) extends GenericModelEvent

object InstanceDefinitionAsserted {implicit def fmt:Format[InstanceDefinitionAsserted]=Json.format[InstanceDefinitionAsserted]}
object InstancePropertyAsserted {implicit def fmt:Format[InstancePropertyAsserted]=Json.format[InstancePropertyAsserted]}


case class EntityInstance(modelId:String,genericInstance:UntypedGenericEntityInstance)
object EntityInstance {implicit def fmt:Format[EntityInstance]=Json.format[EntityInstance]}

object ModelManagerSerializerRegistry extends JsonSerializerRegistry {
  override def serializers: Seq[JsonSerializer[_]] = Seq(
      
    JsonSerializer[AssertInstanceDefinition],
    JsonSerializer[AssertInstanceProperty],
    
    JsonSerializer[InstanceDefinitionAsserted],
    JsonSerializer[InstancePropertyAsserted],
    
    JsonSerializer[EntityInstance]
  )
}
class GenericModelEntity extends PersistentEntity {
  
  override type Command = GenericModelCommand[_]
  override type Event = GenericModelEvent
  override type State = Option[EntityInstance]
  override def initialState = None
  
  private def definedEntityCommand[E<:Event](modelId:String,ctx:CommandContext[Done],state:State,isInvalidInstance:UntypedGenericEntityInstance => Option[String] = _ => None)(evt:UntypedGenericEntityInstance => E)=
    state match {
      case None =>
        ctx.invalidCommand("Entity is not defined")
        ctx.done
      case Some(cur) => 
        if (cur.modelId != modelId) {
          ctx.invalidCommand(s"Command's model id does not corresponds to entity model id: ${cur.modelId} != ${modelId}")
          ctx.done
        }else {
          isInvalidInstance(cur.genericInstance) match {
            case Some(err) =>
              ctx.invalidCommand(s"Instance is not valid for this operation: $err")
              ctx.done
            case None =>
              ctx.thenPersist(evt(cur.genericInstance)) { case _ =>ctx.reply(Done)}
          }
        }
    }
  
  override def behavior: Behavior =
    Actions()
    .onCommand[AssertInstanceDefinition, Done] {
      case (AssertInstanceDefinition(modelId,a @ EntityDefined(cls,name,prov)), ctx, state) =>
        state match {
          case Some(cur) => 
            ctx.invalidCommand("Entity already defined")
            ctx.done
          case None =>
            ctx.thenPersist(InstanceDefinitionAsserted(modelId,a)) { case _ =>ctx.reply(Done)}
        }
      case (AssertInstanceDefinition(modelId,a @ EntityUndefined(cls,name,prov)), ctx, state) =>
        definedEntityCommand(modelId,ctx,state)(_ => InstanceDefinitionAsserted(modelId,a))
    }
    .onCommand[AssertInstanceProperty, Done] {
      case (AssertInstanceProperty(modelId,propAssertion), ctx, state) =>
        propAssertion match {
          case EntityPropertyRemoved(cls,name,propPath,remIdx,prov) => 
            definedEntityCommand(modelId,ctx,state,cur => if (remIdx<cur.getPropRec(propPath).size) None else Some("Not enough values"))(
              cur => InstancePropertyAsserted(modelId,cur.getPropRec(propAssertion.property),propAssertion)
            )
          case other => 
           definedEntityCommand(modelId,ctx,state)(cur => InstancePropertyAsserted(modelId,cur.getPropRec(propAssertion.property),propAssertion))
        }
    }
    .onEvent {
      case (InstanceDefinitionAsserted(modelId,EntityDefined(cls,name,prov)), state) => Some(EntityInstance(modelId,UntypedGenericEntityInstance(name,Map.empty,Map.empty,Map.empty)))
      case (InstanceDefinitionAsserted(modelId,EntityUndefined(cls,name,prov)), state) => None
      case (InstancePropertyAsserted(modelId,prevs,propAssertion),state) => state.map(ei => ei.copy(genericInstance=applyGenericInstancePropertyAssertion(ei.genericInstance,propAssertion)))
    }
}
