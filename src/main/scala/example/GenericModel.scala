package example

import play.api.libs.json.{Format, Json,JsObject,Reads,Writes}


//import akka.Done
//import com.lightbend.lagom.scaladsl.persistence.{AggregateEventTag,AggregateEvent,PersistentEntity}
//import com.lightbend.lagom.scaladsl.persistence.PersistentEntity.ReplyType
//import com.lightbend.lagom.scaladsl.playjson.{JsonSerializerRegistry, JsonSerializer}

object GenericModel {
  
  
  trait Entity {self => 
//    type Instance //<: {def id:String}
    sealed trait EntityProperty[A]{
      type PropType <: Property[A]
      val prop:PropType
      def :=(a:A)= this -> a //new EntityPropertyWithValue(this, a)
    }
    
    final case class EntityValueProperty[A](prop:ValueProperty[A]) extends EntityProperty[A] {
      type PropType =ValueProperty[A]
//      def :=(a:A)= new EntityValuePropertyWithValue(this, a)
    }
    final case class EntityReferenceProperty[R <: Entity,A](prop:ReferenceProperty[R,A]) extends EntityProperty[A] {
      type PropType =ReferenceProperty[R,A]
//      def :=(a:A)= new EntityReferenceValue(this, a)
    }
    
    sealed case class EntityPropertyWithValue[A](prop:EntityProperty[A],value:A)
    
//    sealed case class EntityValuePropertyWithValue[A](prop:EntityProperty[A],value:A)
    def prop[A](prop:ValueProperty[A])=EntityValueProperty[A](prop)
//    sealed case class EntityReferencePropertyWithValue[R <: Entity,A](ref:EntityReference[R,A],value:A)
    def ref[R <: Entity,A](ref:ReferenceProperty[R,A])=EntityReferenceProperty[R,A](ref)
  }
  
  
  trait Property[A] {
    implicit val fmt:Format[A]
    val name:String
  }
  trait ValueProperty[A] extends Property[A]
  trait ReferenceProperty[R <: Entity,V] extends Property[V] {
    val encode : (V) => Option[String]
    val name:String
  }
  
  
  
  def prop[A](n:String)(implicit f:Format[A])=new ValueProperty[A]{implicit val fmt=f;val name=n}
  def ref[R <: Entity,A](n:String,enc:A => Option[String])(implicit f:Format[A])=new ReferenceProperty[R,A]{implicit val fmt=f;val encode = enc;val name=n}

  
  trait Provenance
  
  def prov=new Provenance {}
  
  case class SetPropertyCommand[E<:Entity,V](entityId:String,property:ValueProperty[V],value:V,prov:Provenance)
  
  sealed trait ModelCommand[R] //extends ReplyType[R]
  
  case class SetEntityPropertyCommand[E<:Entity,V](entity:E,entityInstance:String,property:E#EntityProperty[V],value:V,prov:Provenance)
  extends ModelCommand[Unit] 
  case class UnsetEntityPropertyCommand[E<:Entity,V](entity:E,entityInstance:String,property:E#EntityProperty[V],prov:Provenance)
  extends ModelCommand[Unit] 
  case class AddEntityPropertyCommand[E<:Entity,V](entity:E,entityInstance:String,property:E#EntityProperty[Seq[V]],value:V,prov:Provenance)
  extends ModelCommand[Unit]
  case class RemoveEntityPropertyCommand[E<:Entity,V](entity:E,entityInstance:String,property:E#EntityProperty[Seq[V]],removeIdx:Int,prov:Provenance)
  extends ModelCommand[Unit] {
//    val propValue = entity.EntityValuePropertyWithValue(property,value)
  }
  
//  SetEntityPropertyCommand[Polity.type,String](p1,Polity.name,"Polity 1",prov)
   
  implicit class RichEntity[E<:Entity](entity:E) {
    class RichEntityInstance(instance:String) {
      class RichEntityInstanceProperty[V](p:entity.EntityProperty[V]) {
        def :=(v:V)=SetEntityPropertyCommand[E,V](entity,instance,p, v,prov)
        
      }
      def set[V](pv:(entity.EntityProperty[V],V))=1
      def set[V](p:(entity.EntityProperty[V]))=new RichEntityInstanceProperty(p)
    }
    def apply(entityInstance:String)=new RichEntityInstance(entityInstance) //{
//    def setPropertyCommand[V](entityInstance:String,property:entity.EntityProperty[V],value:V,prov:Provenance) =
//      SetEntityPropertyCommand[E,V](entity,entityInstance,property,entity.EntityPropertyWithValue(property,value),prov)
//    def setPropertyCommand2[V](entityInstance:entity.Instance,property:entity.EntityProperty[V],value:V,prov:Provenance) =
//      property := value
  }
//  p1.setProp(2)
  
  
  case class Model(
      entities : Map[Entity,Map[String,Map[Entity#EntityProperty[_],_]]] //,
//      entities2 : Map[Entity,Map[Entity#Instance,Map[Entity#EntityReferenceProperty[_,_],Entity#EntityPropertyWithValue[_]]]]
    ) {
    case class entity[E<:Entity](val e:E) {def instance(i:String)=entityInstance(e,i)}
    case class entityInstance[E<:Entity](val e:E,i:String) {def getProp[V](prop:e.EntityProperty[V])=Model.this.getProp[E,V](e,i,prop)}
    
    def getProp[E<:Entity,V](e:E,p:String,prop:E#EntityProperty[V])=
      entities.get(e)
      .flatMap(_.get(p).flatMap(_.get(prop).map{_.asInstanceOf[V]}))
    def getRef[E<:Entity,R<:Entity,V](e:E,r:R,p:String,prop:E#EntityReferenceProperty[R,V])=
      entities.get(e).flatMap(_.get(p).flatMap(_.get(prop).map{_.asInstanceOf[V]}))
      .flatMap(prop.prop.encode)
      .flatMap(rr=>entities.get(e).flatMap(_.get(rr)))
//      .map(_.asInstanceOf[e.EntityReferenceValue[R,V]])
        
    def updateEntityInstance[E<:Entity](entity:E,id:String,transform:Map[Entity#EntityProperty[_],_] => Map[Entity#EntityProperty[_],_])= {
      val currentInstanceMap=entities.getOrElse(entity, Map.empty)
      val currentPropsMap=currentInstanceMap.getOrElse(id, Map.empty)
      copy(entities=
        entities.updated(entity,currentInstanceMap.updated(id, transform(currentPropsMap)))
      )
    }
    
    def apply(cmd:ModelCommand[_])=cmd match {
      case cmd @ SetEntityPropertyCommand(entity,id,prop,value,prov) =>
        updateEntityInstance(entity,id,_.updated(prop,value))
      case cmd @ UnsetEntityPropertyCommand(entity,id,prop,prov) =>
        updateEntityInstance(entity,id,_ - prop)
      case cmd @ AddEntityPropertyCommand(entity,id,prop,value,prov) =>
        updateEntityInstance(entity,id,cur => cur.updated(prop,cur.getOrElse(prop, Seq.empty).asInstanceOf[Seq[_]] :+ value))
      case cmd @ RemoveEntityPropertyCommand(entity,id,prop,remIdx,prov) =>
        updateEntityInstance(entity,id,cur => cur.updated(prop,cur.getOrElse(prop, Seq.empty).asInstanceOf[Seq[_]].zipWithIndex.collect{case (v,idx) if  idx != remIdx => v}))
    }
    
//    implicit class RichEntityModel[E<:Entity](entity:E) {
      def entity[E<:Entity](entity:E,id:String)=entityInstance(entity,id)
//    }
    
  }
  
  
  lazy val f: Int => Int = {case 0 => 1 case other => other * f(other-1)}
  
  
//  import shapeless._ ; import syntax.singleton._ ; import record._
//  case class Polity(name:String,typ:PolityType.Value,dateFrom:Int,dateEnd:Int)
//  val genPolity = LabelledGeneric[Polity]
////  val polity = genPolity.from("toto" :: PolityType.Hierarchical :: 100 :: 100 :: HNil)
//  val genericPolity = genPolity.to(Polity("toto", PolityType.Hierarchical, 100, 100))
////  genericPolity("name") // = "ff"
  
//  val authorProp = ('author ->> "Benjamin Pierce") 
//  val rt = (RecordType name[String] & age[Int] &)
//  type PersonId = Record.`'firstName -> String, 'lastName -> String, 'title -> String`.T
//  case class Emp(firstName:String,lastName:String,title:String)
//   val employeeId : PersonId = Record(firstName = "Jane", lastName = "Doe", title = "software engineer")
//  val authorProp = ('author ->> "Benjamin Pierce") 
//   
//  val PolityRec =
//    authorProp ::
//    ("title"  ->> "Types and Programming Languages") ::
//    ("id"     ->>  262162091) ::
//    ("price"  ->>  44.11) ::
//    HNil
}




