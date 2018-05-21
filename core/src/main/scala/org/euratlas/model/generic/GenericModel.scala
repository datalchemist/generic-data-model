package org.euratlas.model.generic

import play.api.libs.json._
import julienrf.json.{derived => JsonDerivation}
import scala.collection.immutable.Seq

//trait Encoding {
//  type Value
//  type DecodeResult[_]
//  type DecodeError[_]
//  type DecodeSuccess[_]
//  
//  def decodeError[V](err:String):DecodeError
//  def decodeError[V](err:Throwable)
//
//  def stringCodec:Codec[String]
//  trait Encoder[V]{
//    def apply(v:V):Value
//    def map[VV](f:V=>VV):Encoder[VV]
//    def flatMap[VV](f:V=>VV):Encoder[VV]
//  }
//  trait Decoder[V]{def apply(v:Value):DecodeResult[V]}
//  trait Codec[V] extends Encoder[V] with Decoder[V]
//}




//package GenericModel {
  
//  val encoding : Encoding
    
  class PropertyAlreadyDefinedException(prop: String) extends Exception(s"Property already defined: $prop")
  class EntityClassAlreadyDefinedException(cls: String) extends Exception(s"EntityClass already defined: $cls")

  
  /** Entity-class base class
   *   and registry-based JSON formatting
   */
  trait EntityClass {
    val name:String
  }
  object EntityClass {
    private lazy val existingClasses = collection.mutable.Map.empty[String,EntityClass]
    private def checkAndAddClass(n:String,v:EntityClass) = 
      if (existingClasses contains n) throw new EntityClassAlreadyDefinedException(n)
      else {existingClasses(n)=v;v}
    
    def define(n:String):EntityClass = {
      val cls = new EntityClass{val name=n}
      define(cls)
    }
    def define(cls:EntityClass) : EntityClass= {
      checkAndAddClass(cls.name,cls)
    }
    def forName(n:String) = existingClasses.get(n)
    /** Json format based on name-based EntityClasses registry */
    implicit lazy val fmt:Format[EntityClass]={
      import play.api.libs.json._ // JSON library
      val baseFmt = (__ \ "entityClassName").format[String]
      val read=
        baseFmt.flatMap(cls => 
          forName(cls).fold(Reads.apply[EntityClass](_ => JsError(s"Could not find Entity class $cls")))(Reads.pure)
        )
      Format(read,Writes[EntityClass](c=>baseFmt.writes(c.name)))
    }
  }
  
  /** Property base classes 
   *   and registry-based JSON formatting
   */
  sealed trait Property  {
    val name:String
  }
  sealed trait ValueProperty[A] extends Property {
    implicit val fmt:Format[A]
  }
  sealed trait ReferenceProperty[R <: EntityClass,V] extends ValueProperty[V] {
    val encode : (V) => Option[String]
    val entity : R
  }
  sealed trait SubEntityProperty[E<:EntityClass]  extends Property { //extends Property[]
    val entity : E
  }
  object Property {
    private lazy val existingProperties = collection.mutable.Map.empty[String,Property]
    private[generic] def checkAndAddProp[P<:Property](n:String,v:P) = 
      if (existingProperties contains n) throw new PropertyAlreadyDefinedException(n)
      else {existingProperties(n)=v;v}
    
    def forName(n:String) = existingProperties.get(n)
    
    def subEntityId(id:String,prop:Property,idx:Option[Int])=s"${id}_${prop.name}${idx.fold("")(i=>s"_$i")}"
    
    /** Json format based on name-based EntityClasses registry */
    implicit lazy val fmt:Format[Property]={ // JSON library
      val baseFmt = (__ \ "propertyName").format[String]
      val read=
        baseFmt.flatMap{prop => 
          forName(prop).fold(Reads.apply[Property](_ => JsError(s"Could not find Property $prop")))(Reads.pure)
        }
      Format(read,Writes[Property](c=>{baseFmt.writes(c.name)}))
    }
    
    implicit def propValuesMapFmt[V](implicit fmt:Format[V]):Format[Map[Property,V]] = 
      Format(
        Reads[Map[Property,V]]((v:JsValue) =>
          __.lazyRead(Reads.map[V])
           .reads(v).flatMap{res =>
             res.map{case (p,v) => (p -> Property.forName(p)) -> v} match {
               case containsErrors if containsErrors.exists(!_._1._2.isDefined) =>
                 JsError(s"Could not find Property ${containsErrors.collect{case ((n,None),_) => n}.mkString(", ")}")
               case good => JsSuccess(good.map{case ((_,Some(p)),v) => (p:Property) -> v})
             }
           }
        ),
        Writes((v:Map[Property,V]) => Json.toJson(v.map{case (p,v) => p.name -> v}))
    )  
  }
  
  /** Base trait to define general properties */
  trait GeneralProperties {
    val namespace:String
    
    /** Props, refs, subentities constructors */
    def prop[A](n:String)(implicit f:Format[A])=
      Property.checkAndAddProp(n,new ValueProperty[A]{implicit lazy val fmt=f;lazy val name=if (namespace.isEmpty) n else s"$namespace:$n"})
    def subEntity[S<:EntityClass](e:S,n:String)=
      Property.checkAndAddProp(n,new SubEntityProperty[S]{val entity =e;lazy val name=if (namespace.isEmpty) n else s"$namespace:$n"})
    def ref[R <: EntityClass,A](r:R,n:String,enc:A => Option[String])(implicit f:Format[A])=
      Property.checkAndAddProp(n,new ReferenceProperty[R,A]{implicit lazy val fmt=f;lazy val entity =r;val encode = enc;lazy val name=if (namespace.isEmpty) n else s"$namespace:$n"})
  }

  sealed trait AssertionOperation
  sealed trait InstanceOperation extends AssertionOperation
  case object Define extends InstanceOperation
  case object Undefine extends InstanceOperation
  object Typed {
    sealed trait PropertyPath[V]{
      def valueFormat:Format[V]
      def refEntity:Option[ReferenceProperty[_<:EntityClass,V]]
    }
    case class Direct[V](p:ValueProperty[V]) extends PropertyPath[V]{def refEntity=None;def valueFormat=p.fmt}
    case class DirectRef[R<:EntityClass,V](p:ReferenceProperty[R,V]) extends PropertyPath[V]{def refEntity=Some(p);def valueFormat=p.fmt}
    case class SubPath[S<:EntityClass,V](p:SubEntityProperty[S],n:PropertyPath[V]) extends PropertyPath[V]{def refEntity=n.refEntity;def valueFormat=n.valueFormat}
    case class SubIndexedPath[S<:EntityClass,V](p:SubEntityProperty[S],i:Int,n:PropertyPath[V]) extends PropertyPath[V]{def refEntity=n.refEntity;def valueFormat=n.valueFormat}
    object PropertyPath {
      implicit def direct[V](p:ValueProperty[V])=Direct(p)
      implicit def directRef[R<:EntityClass,V](p:ReferenceProperty[R,V])=DirectRef(p)
    }    
    
    /** Operations on property path value */
    sealed trait PropertyOperation[V] extends AssertionOperation
    /** Single value operation */
    case class Set[V](v:V) extends PropertyOperation[V]
    case class Unset[V]() extends PropertyOperation[V]
    /** List values operation */
    case class Add[V](v:V) extends PropertyOperation[V]
    case class SetAt[V](idx:Int,v:V) extends PropertyOperation[V]
    case class Remove[V](idx:Int) extends PropertyOperation[V]
    
    case class PropertyPathAssertion[V](propertyPath:PropertyPath[V],operation:PropertyOperation[V])
    
//    case class EntityInstanceRef[E<:EntityClass](entityClass:E,instanceId:String)
//    
//    sealed trait Assertion[E<:EntityClass] {
//      def instanceRef:EntityInstanceRef[E]
//      def operation:AssertionOperation
//    }
//    case class InstanceAssertion[E<:EntityClass](instanceRef:EntityInstanceRef[E],operation:InstanceOperation) extends Assertion[E]
//    case class PropertyAssertion[E<:EntityClass,V](instanceRef:EntityInstanceRef[E],propertyPath:PropertyPath[V],operation:PropertyOperation[V]) extends Assertion[E]
  }
  
  object DSL {
    import Typed._
    
    /** Property-path construction DSL */
    sealed trait subPathGen {
      def -->[V](s:PropertyPath[V]):  PropertyPath[V] 
    }
    implicit class subPathGenBase[S<:EntityClass](p:SubEntityProperty[S]) extends subPathGen {
      def -->[SS<:EntityClass,V](s:SubEntityProperty[SS])=new subPathGenRec[S,SS](this,s)
      def -->[V](s:PropertyPath[V])=SubPath[S,V](p,s)
      
      def --(i:Int)=new subPathGenIdxBase(p,i)
    }
    class subPathGenIdxBase[S<:EntityClass](p:SubEntityProperty[S],i:Int) extends subPathGen {
      def -->[SS<:EntityClass,V](s:SubEntityProperty[SS])=new subPathGenRec[S,SS](this,s)
      def -->[V](s:PropertyPath[V])=SubIndexedPath[S,V](p,i,s)
    }
    class subPathGenRec[E<:EntityClass,S<:EntityClass](b:subPathGen,n:SubEntityProperty[S]) extends subPathGen  {
      def -->[V](s:PropertyPath[V]):  PropertyPath[V] = b --> SubPath(n,s)  
      
      def --(i:Int)=new subPathGenIdxRec(b,n,i)
    }
    class subPathGenIdxRec[E<:EntityClass,S<:EntityClass](b:subPathGen,n:SubEntityProperty[S],i:Int) extends subPathGen  {
      def -->[V](s:PropertyPath[V]):  PropertyPath[V] = b --> SubIndexedPath(n,i,s)  
    }  
    
    implicit class RichPropPath[V](prop:PropertyPath[V]) {
      class RichIndexedPropPath(idx:Int) {
        def :=(v:V) =
          PropertyPathAssertion(prop,SetAt(idx,v))
      }
      def :=(v:V) =
        PropertyPathAssertion(prop,Set(v))
      def unset() =
        PropertyPathAssertion(prop,Unset())
      def add(v:V) =
        PropertyPathAssertion(prop,Add(v))
      def rem(i:Int) =
        PropertyPathAssertion(prop,Remove(i))
      def apply(i:Int) = new RichIndexedPropPath(i)
    }
  }
  
  
  object Untyped {
    
    sealed trait PropertyPath{
      def refEntity:Option[Property]
    }
    case class Direct(p:Property) extends PropertyPath{def refEntity=None}
    case class DirectRef(p:Property) extends PropertyPath{def refEntity=Some(p)}
    case class SubPath(p:Property,n:PropertyPath) extends PropertyPath{def refEntity=n.refEntity}
    case class SubIndexedPath(p:Property,i:Int,n:PropertyPath) extends PropertyPath{def refEntity=n.refEntity}
    object PropertyPath {
      implicit def direct(p:Property)=Direct(p)
      implicit def directRef(p:Property)=DirectRef(p)
      def apply[V](pp:Typed.PropertyPath[V]):PropertyPath=pp match {
        case Typed.Direct(p) => Direct(p)
        case Typed.DirectRef(p) => DirectRef(p)
        case Typed.SubPath(p,n) => SubPath(p,apply(n))
        case Typed.SubIndexedPath(p,i,n) => SubIndexedPath(p,i,apply(n))
      }
    }    
    
    sealed trait PropertyOperation extends AssertionOperation
    /** Single value operation */
    case class Set(v:JsValue) extends PropertyOperation
    case class Unset() extends PropertyOperation
    /** List values operation */
    case class Add(v:JsValue) extends PropertyOperation
    case class SetAt(idx:Int,v:JsValue) extends PropertyOperation
    case class Remove(idx:Int) extends PropertyOperation
  
    object PropertyOperation {
      def apply[V](a:Typed.PropertyOperation[V])(implicit f:Format[V])=a match {
        case Typed.Set(v) => Set(Json.toJson(v))
        case Typed.Unset() => Unset()
        case Typed.Add(v) => Add(Json.toJson(v))
        case Typed.SetAt(i,v) => SetAt(i,Json.toJson(v))
        case Typed.Remove(i) => Remove(i)
      }
    }
    
    case class PropertyPathAssertion(propertyPath:PropertyPath,operation:PropertyOperation)
    object PropertyPathAssertion {
      def apply[V](pa:Typed.PropertyPathAssertion[V]):PropertyPathAssertion=
        PropertyPathAssertion(PropertyPath(pa.propertyPath),PropertyOperation(pa.operation)(pa.propertyPath.valueFormat))
    }
    
//    case class EntityInstanceRef(entityClass:EntityClass,instanceId:String)
//    object EntityInstanceRef {
//      def apply(r:Typed.EntityInstanceRef[_ <:EntityClass]):EntityInstanceRef=EntityInstanceRef(r.entityClass,r.instanceId)
//    }
//    sealed trait Assertion {
//      def instanceRef:EntityInstanceRef
//      def operation:AssertionOperation
//    }
//    case class InstanceAssertion(instanceRef:EntityInstanceRef,operation:InstanceOperation) extends Assertion
//    case class PropertyAssertion(instanceRef:EntityInstanceRef,propertyPath:PropertyPath,operation:PropertyOperation) extends Assertion
//  
//    object Assertion {
//      def apply(a:Typed.Assertion[_ <:EntityClass])=a match {
//        case Typed.InstanceAssertion(r,o) => InstanceAssertion(EntityInstanceRef(r),o)
//        case Typed.PropertyAssertion(r,p,o) => PropertyAssertion(EntityInstanceRef(r),PropertyPath(p),PropertyOperation(o)(p.valueFormat))
//      }
//    }
    
    case class GenericEntityInstance(id:String,
        propertiesMap:Map[Property,Seq[JsValue]],
        referencesMap:Map[Property,Seq[JsValue]],
        subEntitiesMap:Map[Property,Seq[GenericEntityInstance]]) {
      
      def values(p:Property):Seq[JsValue]=propertiesMap.getOrElse(p, Seq.empty)
      def subEntities(prop:Property):Seq[GenericEntityInstance]=subEntitiesMap.getOrElse(prop,Seq())
      
      def apply(pa:PropertyPathAssertion)=pa.operation match {
        case Set(v) => setPropRec(pa.propertyPath, v)
        case Unset() => unsetPropRec(pa.propertyPath)
        case Add(value) => addPropRec(pa.propertyPath, value)
        case SetAt(idx,value) => setPropRecAt(pa.propertyPath,idx, value)
        case Remove(remIdx) => removePropRec(pa.propertyPath, remIdx)
      }
      
      private def newSubEntity(prop:Property,idx:Option[Int])=GenericEntityInstance(Property.subEntityId(id,prop,idx),Map.empty,Map.empty,Map.empty)
      private def subEntity(prop:Property)=subEntitiesMap.get(prop).flatMap(_.headOption).getOrElse(newSubEntity(prop,None))
      private def updateSubEntity(prop:Property,update:GenericEntityInstance=>GenericEntityInstance)=
        subEntitiesMap.updated(prop, Seq(update(subEntity(prop))))
      private def updateSubEntity(prop:Property,i:Int,update:GenericEntityInstance=>GenericEntityInstance)={
        val subEnts=subEntities(prop).padTo(i+1, newSubEntity(prop,Some(i)))
        subEntitiesMap.updated(prop, subEnts.updated(i, update(subEnts(i))))
      }
      
      private def getPropRec(p:PropertyPath):Seq[JsValue]=p match {
        case SubIndexedPath(p,i,n) => subEntities(p).drop(i).headOption.getOrElse(newSubEntity(p,Some(i))).getPropRec(n)
        case SubPath(p,n) => subEntity(p).getPropRec(n)
        case Direct(p) => propertiesMap.getOrElse(p, Seq.empty)
        case DirectRef(p) => referencesMap.getOrElse(p, Seq.empty)
      }
      private def setPropRec(p:PropertyPath,v:JsValue):GenericEntityInstance=p match {
        case SubIndexedPath(p,i,n) => copy(subEntitiesMap=updateSubEntity(p,i,_.setPropRec(n,v)))
        case SubPath(p,n) => copy(subEntitiesMap=updateSubEntity(p,_.setPropRec(n,v)))
        case Direct(p) => copy(propertiesMap=propertiesMap.updated(p, Seq(v)))
        case DirectRef(p) => copy(referencesMap = referencesMap.updated(p, Seq(v)))
      }
      private def setPropRecAt(p:PropertyPath,idx:Int,v:JsValue):GenericEntityInstance=p match {
        case SubIndexedPath(p,i,n) => copy(subEntitiesMap=updateSubEntity(p,i,_.setPropRecAt(n,idx,v)))
        case SubPath(p,n) => copy(subEntitiesMap=updateSubEntity(p,_.setPropRecAt(n,idx,v)))
        case Direct(p) => 
          propertiesMap.getOrElse(p, Seq.empty) match {
            case cur if cur.size > idx && idx >= 0 =>
              copy(propertiesMap=propertiesMap.updated(p, cur.updated(idx, v)))
            case _ =>
              //idx not present, don't do anything @TODO
              copy()
          }
        case DirectRef(p) => 
          referencesMap.getOrElse(p, Seq.empty) match {
            case cur if cur.size > idx && idx >= 0 =>
              copy(referencesMap=referencesMap.updated(p, cur.updated(idx, v)))
            case _ =>
              //idx not present, don't do anything @TODO
              copy()
          }
      }
      private def unsetPropRec(p:PropertyPath):GenericEntityInstance=p match {
        case SubIndexedPath(p,i,n) => copy(subEntitiesMap=updateSubEntity(p,i,_.unsetPropRec(n)))
        case SubPath(p,n) => copy(subEntitiesMap=updateSubEntity(p,_.unsetPropRec(n)))
        case Direct(p) => copy(propertiesMap=propertiesMap - p)
        case DirectRef(p) => copy(referencesMap=referencesMap - p)
      }
      private def addPropRec(p:PropertyPath,v:JsValue):GenericEntityInstance=p match {
        case SubIndexedPath(p,i,n) => copy(subEntitiesMap=updateSubEntity(p,i,_.addPropRec(n,v)))
        case SubPath(p,n) => copy(subEntitiesMap=updateSubEntity(p,_.addPropRec(n,v)))
        case Direct(p) => copy(propertiesMap=propertiesMap.updated(p, propertiesMap.getOrElse(p, Seq.empty) :+ v))
        case DirectRef(p) => copy(referencesMap=referencesMap.updated(p, referencesMap.getOrElse(p, Seq.empty) :+ v))
      }
      private def removePropRec(p:PropertyPath,remIdx:Int):GenericEntityInstance=p match {
        case SubIndexedPath(p,i,n) => copy(subEntitiesMap=updateSubEntity(p,i,_.removePropRec(n,remIdx)))
        case SubPath(p,n) => copy(subEntitiesMap=updateSubEntity(p,_.removePropRec(n,remIdx)))
        case Direct(p) => 
          propertiesMap.getOrElse(p, Seq.empty).zipWithIndex.collect{case (v,idx) if  idx != remIdx => v} match {
            //no more values, unset property
            case empty if empty.isEmpty => copy(propertiesMap=propertiesMap - p)
            //otherwise, update with new values
            case newVals => copy(propertiesMap=propertiesMap.updated(p, newVals))
          }
        case DirectRef(p) => 
          referencesMap.getOrElse(p, Seq.empty).zipWithIndex.collect{case (v,idx) if  idx != remIdx => v} match {
            //no more values, unset property
            case empty if empty.isEmpty => copy(referencesMap=referencesMap - p)
            //otherwise, update with new values
            case newVals => copy(referencesMap=referencesMap.updated(p, newVals))
          }
      }
    }
    
    object json {
      implicit lazy val ppFmt : OFormat[PropertyPath] = JsonDerivation.oformat[PropertyPath]()
      implicit lazy val poFmt : OFormat[PropertyOperation] = JsonDerivation.oformat[PropertyOperation]()
      implicit lazy val ppaFmt : Format[PropertyPathAssertion] = Json.format[PropertyPathAssertion]
      import play.api.libs.json.Reads._ // Custom validation helpers
      import play.api.libs.functional.syntax._ // Combinator syntax
      
      implicit val instanceFormat: Format[GenericEntityInstance] = (
        (JsPath \ "id").format[String] and
        (JsPath \ "valueProps").format(Property.propValuesMapFmt[Seq[JsValue]]) and
        (JsPath \ "refProps").format(Property.propValuesMapFmt[Seq[JsValue]]) and
        (JsPath \ "subProps").lazyFormat(Property.propValuesMapFmt[Seq[GenericEntityInstance]])
      )(GenericEntityInstance.apply, unlift(GenericEntityInstance.unapply))
    }
  }  
  
  
  
  
//} 


trait TypedModelrrr {
 
}