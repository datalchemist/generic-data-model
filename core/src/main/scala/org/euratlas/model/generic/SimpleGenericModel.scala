//package org.euratlas.model.generic
//
//import play.api.libs.json._
//import julienrf.json.{derived => JsonDerivation}
//
//import scala.collection.immutable.Seq
//      
////object DefaultGenericModel extends SimpleGenericModel {
////  case class Prov(content:String)
////  type ModelAssertionProvenance=Prov
////  implicit lazy val provFmt: Format[Prov] = Json.format[Prov]
////}
//trait SimpleGenericModel {
//  
//  
//  
//  
//  
//  /**TODO
//   * 
//   *   EntityInstanceRef (EntityClass + instanceID)
//   *   PropertyOperation[V]
//   *     Set(v:V)
//   *     Set(i:Int,v:V)
//   *     Add(v:V)
//   *     Rem(i:Int)
//   *     
//   *   PropertyPathAssetion[V] = (PropertyPath[V],PropertyOperation[V])
//   *   
//   *   Assertion[V] = (EntityInstanceRef,PropertyPath[V],PropertyOperation[V])
//   * 
//   * 
//   * 	Define correctly typed/untyped classes
//   * -typed classes is for assertion DSL -> ensure correct value typing when writing assertiong
//   * -untyped classes(value is JsValue or other encoding format) is for 
//   *    transport/storage
//   *    generic instance model update
//   * -convert typed to untyped
//   * 
//   */
//  
//  
//  
//  
//  
//  
//  
//  class PropertyAlreadyDefinedException(prop: String) extends Exception(s"Property already defined: $prop")
//  class EntityClassAlreadyDefinedException(cls: String) extends Exception(s"EntityClass already defined: $cls")
//
//  
//  /** Entity-class base class
//   *   and registry-based JSON formatting
//   */
//  trait EntityClass {
//    val name:String
//  }
//  object EntityClass {
//    private lazy val existingClasses = collection.mutable.Map.empty[String,EntityClass]
//    private def checkAndAddClass(n:String,v:EntityClass) = 
//      if (existingClasses contains n) throw new EntityClassAlreadyDefinedException(n)
//      else {existingClasses(n)=v;v}
//    
//    def define(n:String):EntityClass = {
//      val cls = new EntityClass{val name=n}
//      define(cls)
//    }
//    def define(cls:EntityClass) : EntityClass= {
//      checkAndAddClass(cls.name,cls)
//    }
//    def forName(n:String) = existingClasses.get(n)
//    
//    /** Json format based on name-based EntityClasses registry */
//    implicit lazy val fmt:Format[EntityClass]={
//      import play.api.libs.json._ // JSON library
//      val baseFmt = (__ \ "entityClassName").format[String]
//      val read=
//        baseFmt.flatMap(cls => 
//          forName(cls).fold(Reads.apply[EntityClass](_ => JsError(s"Could not find Entity class $cls")))(Reads.pure)
//        )
//      Format(read,Writes[EntityClass](c=>baseFmt.writes(c.name)))
//    }
//  }
//  
//  /** Property base classes 
//   *   and registry-based JSON formatting
//   */
//  sealed trait Property  {
//    val name:String
//  }
//  sealed trait ValueProperty[A] extends Property {
//    implicit val fmt:Format[A]
//  }
//  sealed trait ReferenceProperty[R <: EntityClass,V] extends ValueProperty[V] {
//    val encode : (V) => Option[String]
//    val entity : R
//  }
//  sealed trait SubEntityProperty[E<:EntityClass]  extends Property { //extends Property[]
//    val entity : E
//  }
//  
//  object Property {
//    private lazy val existingProperties = collection.mutable.Map.empty[String,Property]
//    private[SimpleGenericModel] def checkAndAddProp[P<:Property](n:String,v:P) = 
//      if (existingProperties contains n) throw new PropertyAlreadyDefinedException(n)
//      else {existingProperties(n)=v;v}
//    
//    def forName(n:String) = existingProperties.get(n)
//    
//    def subEntityId(id:String,prop:Property,idx:Option[Int])=s"${id}_${prop.name}${idx.fold("")(i=>s"_$i")}"
//    
//    /** Json format based on name-based EntityClasses registry */
//    implicit lazy val fmt:Format[Property]={ // JSON library
//      val baseFmt = (__ \ "propertyName").format[String]
//      val read=
//        baseFmt.flatMap{prop => 
//          forName(prop).fold(Reads.apply[Property](_ => JsError(s"Could not find Property $prop")))(Reads.pure)
//        }
//      Format(read,Writes[Property](c=>{baseFmt.writes(c.name)}))
//    }
//    
//    implicit def propValuesMapFmt[V](implicit fmt:Format[V]):Format[Map[Property,V]] = 
//      Format(
//        Reads[Map[Property,V]]((v:JsValue) =>
//          __.lazyRead(Reads.map[V])
//           .reads(v).flatMap{res =>
//             res.map{case (p,v) => (p -> Property.forName(p)) -> v} match {
//               case containsErrors if containsErrors.exists(!_._1._2.isDefined) =>
//                 JsError(s"Could not find Property ${containsErrors.collect{case ((n,None),_) => n}.mkString(", ")}")
//               case good => JsSuccess(good.map{case ((_,Some(p)),v) => (p:Property) -> v})
//             }
//           }
//        ),
//        Writes((v:Map[Property,V]) => Json.toJson(v.map{case (p,v) => p.name -> v}))
//    )
//  }
//  /** Base trait to define general properties */
//  trait GeneralProperties {
//    val namespace:String
//    
//    /** Props, refs, subentities constructors */
//    def prop[A](n:String)(implicit f:Format[A])=
//      Property.checkAndAddProp(n,new ValueProperty[A]{implicit lazy val fmt=f;lazy val name=if (namespace.isEmpty) n else s"$namespace:$n"})
//    def subEntity[S<:EntityClass](e:S,n:String)=
//      Property.checkAndAddProp(n,new SubEntityProperty[S]{val entity =e;lazy val name=if (namespace.isEmpty) n else s"$namespace:$n"})
//    def ref[R <: EntityClass,A](r:R,n:String,enc:A => Option[String])(implicit f:Format[A])=
//      Property.checkAndAddProp(n,new ReferenceProperty[R,A]{implicit lazy val fmt=f;lazy val entity =r;val encode = enc;lazy val name=if (namespace.isEmpty) n else s"$namespace:$n"})
//  }
//  
//  
//  
//  /** Property-path to reference property (and/or sub-property) recursively, with optional indexing
//   *  ex:
//   *  	subEntityProp1 --> subEntityProp2 --> prop1
//   *  	subEntityProp1 -- 1 --> subEntityProp2 --> prop1		
//   *    
//   */
//  sealed trait PropertyPath[V]{
//    def valueFormat:Format[V]
//    def refEntity:Option[ReferenceProperty[_<:EntityClass,V]]
//  }
//  case class Direct[V](p:ValueProperty[V]) extends PropertyPath[V]{def refEntity=None;def valueFormat=p.fmt}
//  case class DirectRef[R<:EntityClass,V](p:ReferenceProperty[R,V]) extends PropertyPath[V]{def refEntity=Some(p);def valueFormat=p.fmt}
//  case class SubPath[S<:EntityClass,V](p:SubEntityProperty[S],n:PropertyPath[V]) extends PropertyPath[V]{def refEntity=n.refEntity;def valueFormat=n.valueFormat}
//  case class SubIndexedPath[S<:EntityClass,V](p:SubEntityProperty[S],i:Int,n:PropertyPath[V]) extends PropertyPath[V]{def refEntity=n.refEntity;def valueFormat=n.valueFormat}
//  object PropertyPath {
//    implicit def direct[V](p:ValueProperty[V])=Direct(p)
//    implicit def directRef[R<:EntityClass,V](p:ReferenceProperty[R,V])=DirectRef(p)
//  }
//  /** Property-path construction DSL */
//  sealed trait subPathGen {
//    def -->[V](s:PropertyPath[V]):  PropertyPath[V] 
//  }
//  implicit class subPathGenBase[S<:EntityClass](p:SubEntityProperty[S]) extends subPathGen {
//    def -->[SS<:EntityClass,V](s:SubEntityProperty[SS])=new subPathGenRec[S,SS](this,s)
//    def -->[V](s:PropertyPath[V])=SubPath[S,V](p,s)
//    
//    def --(i:Int)=new subPathGenIdxBase(p,i)
//  }
//  class subPathGenIdxBase[S<:EntityClass](p:SubEntityProperty[S],i:Int) extends subPathGen {
//    def -->[SS<:EntityClass,V](s:SubEntityProperty[SS])=new subPathGenRec[S,SS](this,s)
//    def -->[V](s:PropertyPath[V])=SubIndexedPath[S,V](p,i,s)
//  }
//  class subPathGenRec[E<:EntityClass,S<:EntityClass](b:subPathGen,n:SubEntityProperty[S]) extends subPathGen  {
//    def -->[V](s:PropertyPath[V]):  PropertyPath[V] = b --> SubPath(n,s)  
//    
//    def --(i:Int)=new subPathGenIdxRec(b,n,i)
//  }
//  class subPathGenIdxRec[E<:EntityClass,S<:EntityClass](b:subPathGen,n:SubEntityProperty[S],i:Int) extends subPathGen  {
//    def -->[V](s:PropertyPath[V]):  PropertyPath[V] = b --> SubIndexedPath(n,i,s)  
//  }  
//  /** Untyped property path (same without value typing)
//   * 
//   */
//  sealed trait UntypedPropertyPath {
//    def refEntity:Option[Property]
//  }
//  case class UntypedDirect(p:Property) extends UntypedPropertyPath{def refEntity=None}
//  case class UntypedDirectRef(p:Property) extends UntypedPropertyPath{def refEntity=Some(p)}
//  case class UntypedSubPath(p:Property,n:UntypedPropertyPath) extends UntypedPropertyPath{def refEntity=n.refEntity}
//  case class UntypedSubIndexedPath(p:Property,i:Int,n:UntypedPropertyPath) extends UntypedPropertyPath{def refEntity=n.refEntity}
//  object UntypedPropertyPath {
//    implicit def fromPropertyPath(p:PropertyPath[_]):UntypedPropertyPath = p match {
//      case Direct(p) => UntypedDirect(p)
//      case DirectRef(p) => UntypedDirectRef(p)
//      case SubPath(p,n) => UntypedSubPath(p,n)
//      case SubIndexedPath(p,i,n) => UntypedSubIndexedPath(p,i,n)
//    }
//    implicit lazy val fmt : OFormat[UntypedPropertyPath] = JsonDerivation.oformat[UntypedPropertyPath]()
//  }
//  
//  /** Asserts something (set/add value, unset/remove) about a PropertyPath
//   *  
//   */
//  sealed trait PropertyPathAssertion[V]
//  case class SetPropertyPathValue[V](path:PropertyPath[V],value:V) extends PropertyPathAssertion[V]
//  case class UnsetPropertyPathValue[V](path:PropertyPath[V]) extends PropertyPathAssertion[V]
//  case class AddPropertyPathValue[V](path:PropertyPath[V],value:V) extends PropertyPathAssertion[V]
//  case class RemovePropertyPathValue[V](path:PropertyPath[V],idx:Int) extends PropertyPathAssertion[V]
//  implicit class RichPropPathJson[V](prop:PropertyPath[V]) {
//    def :=(v:V) =
//      SetPropertyPathValue(prop,v)
//    def unset() =
//      UnsetPropertyPathValue(prop)
//    def add(v:V) =
//      AddPropertyPathValue(prop,v)
//    def rem(i:Int) =
//      RemovePropertyPathValue(prop,i)
//  }
//  
//  
////  sealed trait UntypedAssertion
////  
////  case class EntityExists(entity:EntityClass,entityInstance:String,prov:ModelAssertionProvenance) extends UntypedAssertion
////  case class EntityNotExists(entity:EntityClass,entityInstance:String,prov:ModelAssertionProvenance) extends UntypedAssertion
////  
////  case class DefineSingleEntityProperty(entity:EntityClass,entityInstance:String,property:UntypedPropertyPath,value:JsValue,prov:ModelAssertionProvenance) extends UntypedAssertion
////  case class UndefineProperty(entity:EntityClass,entityInstance:String,property:UntypedPropertyPath,prov:ModelAssertionProvenance) extends UntypedAssertion
////  case class AddEntityProperty(entity:EntityClass,entityInstance:String,property:UntypedPropertyPath,value:JsValue,prov:ModelAssertionProvenance) extends UntypedAssertion
////  case class RemoveEntityProperty(entity:EntityClass,entityInstance:String,property:UntypedPropertyPath,removeIdx:Int,prov:ModelAssertionProvenance) extends UntypedAssertion 
////  
////  object EntityExists {implicit lazy val fmt:Format[EntityExists]=Json.format[EntityExists]}
////  object EntityNotExists {implicit lazy val fmt:Format[EntityNotExists]=Json.format[EntityNotExists]}
////  object DefineSingleEntityProperty {
////    implicit lazy val fmt[V]:Format[DefineSingleEntityProperty]={
////    Json.format[DefineSingleEntityProperty]
////  }}
////  object UndefineProperty {implicit lazy val fmt:Format[UndefineProperty]=Json.format[UndefineProperty]}
//  
//  /**
//   * Properties is a map from EntityProperty to Seq of value
//   */
//  object UntypedGenericEntityInstance {
////    implicit lazy val propValuesMapFmt = Property.propValuesMapFmt[Seq[JsValue]] 
////    implicit lazy val subEntitiesMapFmt = Property.propValuesMapFmt[Seq[UntypedGenericEntityInstance]]
//    import play.api.libs.json._ // JSON library
//    import play.api.libs.json.Reads._ // Custom validation helpers
//    import play.api.libs.functional.syntax._ // Combinator syntax
//    
//    implicit val instanceFormat: Format[UntypedGenericEntityInstance] = (
//      (JsPath \ "id").format[String] and
//      (JsPath \ "valueProps").format(Property.propValuesMapFmt[Seq[JsValue]]) and
//      (JsPath \ "refProps").format(Property.propValuesMapFmt[Seq[JsValue]]) and
//      (JsPath \ "subProps").lazyFormat(Property.propValuesMapFmt[Seq[UntypedGenericEntityInstance]])
//    )(UntypedGenericEntityInstance.apply, unlift(UntypedGenericEntityInstance.unapply))
////    implicit lazy val fmt : OFormat[UntypedGenericEntityInstance] = Json.format[UntypedGenericEntityInstance]
//  }
//  case class UntypedGenericEntityInstance(id:String,
//      propertiesMap:Map[Property,Seq[JsValue]],
//      referencesMap:Map[Property,Seq[JsValue]],
//      subEntitiesMap:Map[Property,Seq[UntypedGenericEntityInstance]]) {
//    private def newSubEntity(prop:Property,idx:Option[Int])=UntypedGenericEntityInstance(Property.subEntityId(id,prop,idx),Map.empty,Map.empty,Map.empty)
//    private def subEntity(prop:Property)=subEntitiesMap.get(prop).flatMap(_.headOption).getOrElse(newSubEntity(prop,None))
//    private def updateSubEntity(prop:Property,update:UntypedGenericEntityInstance=>UntypedGenericEntityInstance)=
//      subEntitiesMap.updated(prop, Seq(update(subEntity(prop))))
//    private def updateSubEntity(prop:Property,i:Int,update:UntypedGenericEntityInstance=>UntypedGenericEntityInstance)={
//      val subEnts=subEntities(prop).padTo(i+1, newSubEntity(prop,Some(i)))
//      subEntitiesMap.updated(prop, subEnts.updated(i, update(subEnts(i))))
//    }
//    
//    def values(p:Property):Seq[JsValue]=propertiesMap.getOrElse(p, Seq.empty)
//    def subEntities(prop:Property):Seq[UntypedGenericEntityInstance]=subEntitiesMap.getOrElse(prop,Seq())
//      
//    def getPropRec(p:UntypedPropertyPath):Seq[JsValue]=p match {
//      case UntypedSubIndexedPath(p,i,n) => subEntities(p).drop(i).headOption.getOrElse(newSubEntity(p,Some(i))).getPropRec(n)
//      case UntypedSubPath(p,n) => subEntity(p).getPropRec(n)
//      case UntypedDirect(p) => propertiesMap.getOrElse(p, Seq.empty)
//      case UntypedDirectRef(p) => referencesMap.getOrElse(p, Seq.empty)
//    }
//    def setPropRec(p:UntypedPropertyPath,v:JsValue):UntypedGenericEntityInstance=p match {
//      case UntypedSubIndexedPath(p,i,n) => copy(subEntitiesMap=updateSubEntity(p,i,_.setPropRec(n,v)))
//      case UntypedSubPath(p,n) => copy(subEntitiesMap=updateSubEntity(p,_.setPropRec(n,v)))
//      case UntypedDirect(p) => copy(propertiesMap=propertiesMap.updated(p, Seq(v)))
//      case UntypedDirectRef(p) => copy(referencesMap = referencesMap.updated(p, Seq(v)))
//    }
//    def unsetPropRec(p:UntypedPropertyPath):UntypedGenericEntityInstance=p match {
//      case UntypedSubIndexedPath(p,i,n) => copy(subEntitiesMap=updateSubEntity(p,i,_.unsetPropRec(n)))
//      case UntypedSubPath(p,n) => copy(subEntitiesMap=updateSubEntity(p,_.unsetPropRec(n)))
//      case UntypedDirect(p) => copy(propertiesMap=propertiesMap - p)
//      case UntypedDirectRef(p) => copy(referencesMap=referencesMap - p)
//    }
//    def addPropRec(p:UntypedPropertyPath,v:JsValue):UntypedGenericEntityInstance=p match {
//      case UntypedSubIndexedPath(p,i,n) => copy(subEntitiesMap=updateSubEntity(p,i,_.addPropRec(n,v)))
//      case UntypedSubPath(p,n) => copy(subEntitiesMap=updateSubEntity(p,_.addPropRec(n,v)))
//      case UntypedDirect(p) => copy(propertiesMap=propertiesMap.updated(p, propertiesMap.getOrElse(p, Seq.empty) :+ v))
//      case UntypedDirectRef(p) => copy(referencesMap=referencesMap.updated(p, referencesMap.getOrElse(p, Seq.empty) :+ v))
//    }
//    def removePropRec(p:UntypedPropertyPath,remIdx:Int):UntypedGenericEntityInstance=p match {
//      case UntypedSubIndexedPath(p,i,n) => copy(subEntitiesMap=updateSubEntity(p,i,_.removePropRec(n,remIdx)))
//      case UntypedSubPath(p,n) => copy(subEntitiesMap=updateSubEntity(p,_.removePropRec(n,remIdx)))
//      case UntypedDirect(p) => 
//        propertiesMap.getOrElse(p, Seq.empty).zipWithIndex.collect{case (v,idx) if  idx != remIdx => v} match {
//          //no more values, unset property
//          case empty if empty.isEmpty => copy(propertiesMap=propertiesMap - p)
//          //otherwise, update with new values
//          case newVals => copy(propertiesMap=propertiesMap.updated(p, newVals))
//        }
//      case UntypedDirectRef(p) => 
//        referencesMap.getOrElse(p, Seq.empty).zipWithIndex.collect{case (v,idx) if  idx != remIdx => v} match {
//          //no more values, unset property
//          case empty if empty.isEmpty => copy(referencesMap=referencesMap - p)
//          //otherwise, update with new values
//          case newVals => copy(referencesMap=referencesMap.updated(p, newVals))
//        }
//    }
//
//    
//  }
//  
//  sealed trait AssertionOperation
//  sealed trait InstanceOperation extends AssertionOperation
//  case object Define extends InstanceOperation
//  case object Undefine extends InstanceOperation
//  object Typed {
//    sealed trait PropertyPath[V]{
//      def valueFormat:Format[V]
//      def refEntity:Option[ReferenceProperty[_<:EntityClass,V]]
//    }
//    case class Direct[V](p:ValueProperty[V]) extends PropertyPath[V]{def refEntity=None;def valueFormat=p.fmt}
//    case class DirectRef[R<:EntityClass,V](p:ReferenceProperty[R,V]) extends PropertyPath[V]{def refEntity=Some(p);def valueFormat=p.fmt}
//    case class SubPath[S<:EntityClass,V](p:SubEntityProperty[S],n:PropertyPath[V]) extends PropertyPath[V]{def refEntity=n.refEntity;def valueFormat=n.valueFormat}
//    case class SubIndexedPath[S<:EntityClass,V](p:SubEntityProperty[S],i:Int,n:PropertyPath[V]) extends PropertyPath[V]{def refEntity=n.refEntity;def valueFormat=n.valueFormat}
//    object PropertyPath {
//      implicit def direct[V](p:ValueProperty[V])=Direct(p)
//      implicit def directRef[R<:EntityClass,V](p:ReferenceProperty[R,V])=DirectRef(p)
//    }    
//    sealed trait PropertyOperation[V] extends AssertionOperation
//    /** Single value operation */
//    case class Set[V](v:V) extends PropertyOperation[V]
//    case class Unset[V]() extends PropertyOperation[V]
//    /** List values operation */
//    case class Add[V](v:V) extends PropertyOperation[V]
//    case class SetAt[V](idx:Int,v:V) extends PropertyOperation[V]
//    case class Remove[V](idx:Int) extends PropertyOperation[V]
//    
//    case class EntityInstanceRef[E<:EntityClass](entityClass:E,instanceId:String)
//    
//    sealed trait Assertion[E<:EntityClass] {
//      def instanceRef:EntityInstanceRef[E]
//      def operation:AssertionOperation
//    }
//    case class InstanceAssertion[E<:EntityClass](instanceRef:EntityInstanceRef[E],operation:InstanceOperation) extends Assertion[E]
//    case class PropertyAssertion[E<:EntityClass,V](instanceRef:EntityInstanceRef[E],propertyPath:PropertyPath[V],operation:PropertyOperation[V]) extends Assertion[E]
//  }
//  object Untyped {
//    sealed trait PropertyPath{
//      def refEntity:Option[Property]
//    }
//    case class Direct(p:Property) extends PropertyPath{def refEntity=None}
//    case class DirectRef(p:Property) extends PropertyPath{def refEntity=Some(p)}
//    case class SubPath(p:Property,n:PropertyPath) extends PropertyPath{def refEntity=n.refEntity}
//    case class SubIndexedPath(p:Property,i:Int,n:PropertyPath) extends PropertyPath{def refEntity=n.refEntity}
//    object PropertyPath {
//      implicit def direct(p:Property)=Direct(p)
//      implicit def directRef(p:Property)=DirectRef(p)
//      def apply[V](pp:Typed.PropertyPath[V]):PropertyPath=pp match {
//        case Typed.Direct(p) => Direct(p)
//        case Typed.DirectRef(p) => DirectRef(p)
//        case Typed.SubPath(p,n) => SubPath(p,apply(n))
//        case Typed.SubIndexedPath(p,i,n) => SubIndexedPath(p,i,apply(n))
//      }
//    }    
//    case class EntityInstanceRef(entityClass:EntityClass,instanceId:String)
//    object EntityInstanceRef {
//      def apply(r:Typed.EntityInstanceRef[_ <:EntityClass]):EntityInstanceRef=EntityInstanceRef(r.entityClass,r.instanceId)
//    }
//    sealed trait PropertyOperation extends AssertionOperation
//    /** Single value operation */
//    case class Set(v:JsValue) extends PropertyOperation
//    case class Unset() extends PropertyOperation
//    /** List values operation */
//    case class Add(v:JsValue) extends PropertyOperation
//    case class SetAt(idx:Int,v:JsValue) extends PropertyOperation
//    case class Remove(idx:Int) extends PropertyOperation
//  
//    object PropertyOperation {
//      def apply[V](a:Typed.PropertyOperation[V])(implicit f:Format[V])=a match {
//        case Typed.Set(v) => Set(Json.toJson(v))
//        case Typed.Unset() => Unset()
//        case Typed.Add(v) => Add(Json.toJson(v))
//        case Typed.SetAt(i,v) => SetAt(i,Json.toJson(v))
//        case Typed.Remove(i) => Remove(i)
//      }
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
//    
//    case class GenericEntityInstance(id:String,
//        propertiesMap:Map[Property,Seq[JsValue]],
//        referencesMap:Map[Property,Seq[JsValue]],
//        subEntitiesMap:Map[Property,Seq[GenericEntityInstance]]) {
//      
//      def values(p:Property):Seq[JsValue]=propertiesMap.getOrElse(p, Seq.empty)
//      def subEntities(prop:Property):Seq[GenericEntityInstance]=subEntitiesMap.getOrElse(prop,Seq())
//      
//      def apply(propPath:PropertyPath,operation:PropertyOperation)=operation match {
//        case Set(v) => setPropRec(propPath, v)
//        case Unset() => unsetPropRec(propPath)
//        case Add(value) => addPropRec(propPath, value)
//        case SetAt(idx,value) => setPropRecAt(propPath,idx, value)
//        case Remove(remIdx) => removePropRec(propPath, remIdx)
//      }
//      
//      private def newSubEntity(prop:Property,idx:Option[Int])=GenericEntityInstance(Property.subEntityId(id,prop,idx),Map.empty,Map.empty,Map.empty)
//      private def subEntity(prop:Property)=subEntitiesMap.get(prop).flatMap(_.headOption).getOrElse(newSubEntity(prop,None))
//      private def updateSubEntity(prop:Property,update:GenericEntityInstance=>GenericEntityInstance)=
//        subEntitiesMap.updated(prop, Seq(update(subEntity(prop))))
//      private def updateSubEntity(prop:Property,i:Int,update:GenericEntityInstance=>GenericEntityInstance)={
//        val subEnts=subEntities(prop).padTo(i+1, newSubEntity(prop,Some(i)))
//        subEntitiesMap.updated(prop, subEnts.updated(i, update(subEnts(i))))
//      }
//      
//      private def getPropRec(p:PropertyPath):Seq[JsValue]=p match {
//        case SubIndexedPath(p,i,n) => subEntities(p).drop(i).headOption.getOrElse(newSubEntity(p,Some(i))).getPropRec(n)
//        case SubPath(p,n) => subEntity(p).getPropRec(n)
//        case Direct(p) => propertiesMap.getOrElse(p, Seq.empty)
//        case DirectRef(p) => referencesMap.getOrElse(p, Seq.empty)
//      }
//      private def setPropRec(p:PropertyPath,v:JsValue):GenericEntityInstance=p match {
//        case SubIndexedPath(p,i,n) => copy(subEntitiesMap=updateSubEntity(p,i,_.setPropRec(n,v)))
//        case SubPath(p,n) => copy(subEntitiesMap=updateSubEntity(p,_.setPropRec(n,v)))
//        case Direct(p) => copy(propertiesMap=propertiesMap.updated(p, Seq(v)))
//        case DirectRef(p) => copy(referencesMap = referencesMap.updated(p, Seq(v)))
//      }
//      private def setPropRecAt(p:PropertyPath,idx:Int,v:JsValue):GenericEntityInstance=p match {
//        case SubIndexedPath(p,i,n) => copy(subEntitiesMap=updateSubEntity(p,i,_.setPropRecAt(n,idx,v)))
//        case SubPath(p,n) => copy(subEntitiesMap=updateSubEntity(p,_.setPropRecAt(n,idx,v)))
//        case Direct(p) => 
//          propertiesMap.getOrElse(p, Seq.empty) match {
//            case cur if cur.size > idx && idx >= 0 =>
//              copy(propertiesMap=propertiesMap.updated(p, cur.updated(idx, v)))
//            case _ =>
//              //idx not present, don't do anything @TODO
//              copy()
//          }
//        case DirectRef(p) => 
//          referencesMap.getOrElse(p, Seq.empty) match {
//            case cur if cur.size > idx && idx >= 0 =>
//              copy(referencesMap=referencesMap.updated(p, cur.updated(idx, v)))
//            case _ =>
//              //idx not present, don't do anything @TODO
//              copy()
//          }
//      }
//      private def unsetPropRec(p:PropertyPath):GenericEntityInstance=p match {
//        case SubIndexedPath(p,i,n) => copy(subEntitiesMap=updateSubEntity(p,i,_.unsetPropRec(n)))
//        case SubPath(p,n) => copy(subEntitiesMap=updateSubEntity(p,_.unsetPropRec(n)))
//        case Direct(p) => copy(propertiesMap=propertiesMap - p)
//        case DirectRef(p) => copy(referencesMap=referencesMap - p)
//      }
//      private def addPropRec(p:PropertyPath,v:JsValue):GenericEntityInstance=p match {
//        case SubIndexedPath(p,i,n) => copy(subEntitiesMap=updateSubEntity(p,i,_.addPropRec(n,v)))
//        case SubPath(p,n) => copy(subEntitiesMap=updateSubEntity(p,_.addPropRec(n,v)))
//        case Direct(p) => copy(propertiesMap=propertiesMap.updated(p, propertiesMap.getOrElse(p, Seq.empty) :+ v))
//        case DirectRef(p) => copy(referencesMap=referencesMap.updated(p, referencesMap.getOrElse(p, Seq.empty) :+ v))
//      }
//      private def removePropRec(p:PropertyPath,remIdx:Int):GenericEntityInstance=p match {
//        case SubIndexedPath(p,i,n) => copy(subEntitiesMap=updateSubEntity(p,i,_.removePropRec(n,remIdx)))
//        case SubPath(p,n) => copy(subEntitiesMap=updateSubEntity(p,_.removePropRec(n,remIdx)))
//        case Direct(p) => 
//          propertiesMap.getOrElse(p, Seq.empty).zipWithIndex.collect{case (v,idx) if  idx != remIdx => v} match {
//            //no more values, unset property
//            case empty if empty.isEmpty => copy(propertiesMap=propertiesMap - p)
//            //otherwise, update with new values
//            case newVals => copy(propertiesMap=propertiesMap.updated(p, newVals))
//          }
//        case DirectRef(p) => 
//          referencesMap.getOrElse(p, Seq.empty).zipWithIndex.collect{case (v,idx) if  idx != remIdx => v} match {
//            //no more values, unset property
//            case empty if empty.isEmpty => copy(referencesMap=referencesMap - p)
//            //otherwise, update with new values
//            case newVals => copy(referencesMap=referencesMap.updated(p, newVals))
//          }
//      }
//  
//      
//    }    
//    
//  }
//  
//  
//  sealed trait GenericModelAssertion {
//    def entity:EntityClass
//    def entityInstance:String
//  }
//  sealed trait InstanceDefinitionAssertion extends GenericModelAssertion
//  case class EntityDefined(entity:EntityClass,entityInstance:String) extends InstanceDefinitionAssertion
//  case class EntityUndefined(entity:EntityClass,entityInstance:String) extends InstanceDefinitionAssertion
//  sealed trait InstancePropertyAssertion extends GenericModelAssertion {
//    def property:UntypedPropertyPath
//  }
//  case class SingleEntityPropertyDefined(entity:EntityClass,entityInstance:String,property:UntypedPropertyPath,value:JsValue) extends InstancePropertyAssertion
//  case class PropertyUndefined(entity:EntityClass,entityInstance:String,property:UntypedPropertyPath) extends InstancePropertyAssertion
//  case class EntityPropertyAdded(entity:EntityClass,entityInstance:String,property:UntypedPropertyPath,value:JsValue) extends InstancePropertyAssertion
//  case class EntityPropertyRemoved(entity:EntityClass,entityInstance:String,property:UntypedPropertyPath,removeIdx:Int) extends InstancePropertyAssertion 
////  object ModelAssertion {
//    def applyGenericInstancePropertyAssertion(instance:UntypedGenericEntityInstance,assertion:InstancePropertyAssertion)={
//      assertion match {
//        case SingleEntityPropertyDefined(cls,name,propPath,value) => instance.setPropRec(propPath, value)
//        case PropertyUndefined(cls,name,propPath) => instance.unsetPropRec(propPath)
//        case EntityPropertyAdded(cls,name,propPath,value) => instance.addPropRec(propPath, value)
//        case EntityPropertyRemoved(cls,name,propPath,remIdx) => instance.removePropRec(propPath, remIdx)
//      }
//    }
////  }
//  
//  object EntityDefined { lazy val fmt:Format[EntityDefined]=Json.format[EntityDefined] }
//  object EntityUndefined { lazy val fmt:Format[EntityUndefined]=Json.format[EntityUndefined] }
//  
//  object SingleEntityPropertyDefined { lazy val fmt:Format[SingleEntityPropertyDefined]=Json.format[SingleEntityPropertyDefined] }
//  object PropertyUndefined { lazy val fmt:Format[PropertyUndefined]=Json.format[PropertyUndefined] }
//  object EntityPropertyAdded { lazy val fmt:Format[EntityPropertyAdded]=Json.format[EntityPropertyAdded] }
//  object EntityPropertyRemoved { lazy val fmt:Format[EntityPropertyRemoved]=Json.format[EntityPropertyRemoved] }  
//  
//  object InstanceDefinitionAssertion {
//    val reads: Reads[InstanceDefinitionAssertion] = {
//      (__ \ "type").read[String].flatMap {
//        case "EntityDefined" => EntityDefined.fmt.map(identity)
//        case "EntityUndefined" => EntityUndefined.fmt.map(identity)
//        case other => Reads(v => JsError(s"Unknown InstanceDefinitionAssertion type $other"))
//      }
//    }
//    val writes: Writes[InstanceDefinitionAssertion] = Writes { event =>
//      val (jsValue, eventType) = event match {
//        case m: EntityDefined => (Json.toJson(m)(EntityDefined.fmt), "EntityDefined")
//        case m: EntityUndefined => (Json.toJson(m)(EntityUndefined.fmt), "EntityUndefined")
//      }
//      jsValue.transform(__.json.update((__ \ 'type).json.put(JsString(eventType)))).get
//    }
//    lazy val fmt=Format(reads,writes)
//  }
//  object InstancePropertyAssertion {//implicit lazy val fmt:Format[InstancePropertyAssertion]=JsonDerivation.oformat[InstancePropertyAssertion]()
//    val reads: Reads[InstancePropertyAssertion] = {
//      (__ \ "type").read[String].flatMap {
//        case "SingleEntityPropertyDefined" => SingleEntityPropertyDefined.fmt.map(identity)
//        case "PropertyUndefined" => PropertyUndefined.fmt.map(identity)
//        case "EntityPropertyAdded" => EntityPropertyAdded.fmt.map(identity)
//        case "EntityPropertyRemoved" => EntityPropertyRemoved.fmt.map(identity)
//        case other => Reads(v => JsError(s"Unknown InstancePropertyAssertion type $other"))
//      }
//    }
//    val writes: Writes[InstancePropertyAssertion] = Writes { event =>
//      val (jsValue, eventType) = event match {
//        case m: SingleEntityPropertyDefined => (Json.toJson(m)(SingleEntityPropertyDefined.fmt), "SingleEntityPropertyDefined")
//        case m: PropertyUndefined => (Json.toJson(m)(PropertyUndefined.fmt), "PropertyUndefined")
//        case m: EntityPropertyAdded => (Json.toJson(m)(EntityPropertyAdded.fmt), "EntityPropertyAdded")
//        case m: EntityPropertyRemoved => (Json.toJson(m)(EntityPropertyRemoved.fmt), "EntityPropertyRemoved")
//      }
//      jsValue.transform(__.json.update((__ \ 'type).json.put(JsString(eventType)))).get
//    }
//    lazy val fmt=Format(reads,writes)
//  }
////  object ModelAssertion {implicit lazy val fmt:Format[ModelAssertion]=JsonDerivation.oformat[ModelAssertion]()}
//  object GenericModelAssertion {
//    val reads: Reads[GenericModelAssertion] = {
//      InstancePropertyAssertion.fmt.map(identity[GenericModelAssertion])
//      .orElse(InstanceDefinitionAssertion.fmt.map(identity[GenericModelAssertion]))
//    }
//    val writes: Writes[GenericModelAssertion] = Writes { event =>
//      event match {
//        case m: InstanceDefinitionAssertion => (Json.toJson(m)(InstanceDefinitionAssertion.writes))
//        case m: InstancePropertyAssertion => (Json.toJson(m)(InstancePropertyAssertion.writes))
//      }
//    }
//    lazy val fmt=Format(reads,writes)
//  }
//
//  /** Commands DSL */
//  implicit class RichEntity[E<:EntityClass](entity:E) {
//    def apply(entityInstance:String)=new RichEntityInstance(entityInstance)
//    class RichEntityInstance(instance:String) {
//      def create()=EntityDefined(entity,instance)
//      def remove()=EntityUndefined(entity,instance)
//      def update[V](p:PropertyPathAssertion[V])=new RichEntityInstancePropertyPath(p) //RichEntityInstanceProperty(p)
//      class RichEntityInstancePropertyPath[V](p:PropertyPathAssertion[V]) {
//        def apply()=p match {
//          case SetPropertyPathValue(p,v) => SingleEntityPropertyDefined(entity,instance,p,p.valueFormat.writes(v))
//          case UnsetPropertyPathValue(p) => PropertyUndefined(entity,instance,p)
//          case AddPropertyPathValue(p,v) => EntityPropertyAdded(entity,instance,p,p.valueFormat.writes(v))
//          case RemovePropertyPathValue(p,i) => EntityPropertyRemoved(entity,instance,p,i)
//        }
//      }
//    }
//  }
//
//
//}
//
//trait TypedModel extends SimpleGenericModel {
//  
//  implicit class InstanceTypedExt(i:UntypedGenericEntityInstance) {
//    def typedValues[V](p:ValueProperty[V])=
//      i.values(p).map(p.fmt.reads)
//      .foldLeft[Either[String,Vector[V]]](Right(Vector())){
//        case (Right(cur),JsSuccess(v,_)) => Right(cur :+ v) 
//        case (Right(cur),JsError(err)) => Left(s"Value parsing error: $err")
//        case (Left(err),_) => Left(err)
//        }
//  }
//
//  sealed trait TypedModelConstraintFailure
//  case class PropertyValueError[E <: TypedEntity](prop:E#ConstrainedEntityProperty,err:String) extends TypedModelConstraintFailure
//  case class UniquePropertyNotDefined[E <: TypedEntity](prop:E#ConstrainedEntityProperty) extends TypedModelConstraintFailure
//  case class UniquePropertyDefinedMultipleTimes[E <: TypedEntity](prop:E#ConstrainedEntityProperty) extends TypedModelConstraintFailure
//  case class MultiPropertyBelowMinCard[E <: TypedEntity](prop:E#ConstrainedEntityProperty,minCard:Int,numValues:Int) extends TypedModelConstraintFailure
//  case class MultiPropertyAboveMaxCard[E <: TypedEntity](prop:E#ConstrainedEntityProperty,maxCard:Int,numValues:Int) extends TypedModelConstraintFailure
//
//  type Validated[V]=scala.Either[TypedModelConstraintFailure,V]
//  type ValidatedFromInstance[V] = UntypedGenericEntityInstance => Validated[V]
//  
//  def validatedSeq[V](seq:Seq[Validated[V]]):Validated[Seq[V]]=
//    seq.foldRight(Right(Nil) : Validated[List[V]])((next,acc) => acc.right.flatMap(a => next.right.map(n => n :: a)))
//      
//  object TypedEntity {
//    type Aux[V]=TypedEntity{type TypedInstance=V}
//  }
//  
//  trait TypedEntity extends EntityClass {
//    import shapeless._
//    import poly._
//
//    type TypedInstance
//    val instanceGen:Generic[TypedInstance]{type Repr <: HList } //as we consider only simple case class for now 
////    val labelledInstanceGen:LabelledGeneric[TypedInstance]{type Repr <: HList }
//    val constructor : ValidatedFromInstance[TypedInstance]
//    val toGenericInstance : (String,TypedInstance) => UntypedGenericEntityInstance
//    
//
//    
//    sealed trait ConstrainedEntityProperty{
////      val prop:EntityProperty[A]
//    }
//    trait ConstrainedEntityValueProperty extends ConstrainedEntityProperty
//    case class OptionalValue[A](prop:ValueProperty[A]) extends ConstrainedEntityValueProperty //[A]
//    case class SingleValue[A](prop:ValueProperty[A],unique:Boolean) extends ConstrainedEntityValueProperty //[A]
//    case class MultiValue[A](prop:ValueProperty[A],minCard:Option[Int],maxCard:Option[Int]) extends ConstrainedEntityValueProperty//[A]
//    
//    trait ConstrainedEntitySubProperty extends ConstrainedEntityProperty
//    case class OptionalSubEntity[TS<:TypedEntity](ts:TS,prop:SubEntityProperty[_ >: TS <: EntityClass]) extends ConstrainedEntitySubProperty //[TS#TypedInstance]
//    case class SingleSubEntity[TS<:TypedEntity](ts:TS,prop:SubEntityProperty[_ >: TS <: EntityClass],unique:Boolean) extends ConstrainedEntitySubProperty
//    case class MultiSubEntity[TS<:TypedEntity](ts:TS,prop:SubEntityProperty[_ >: TS <: EntityClass],minCard:Option[Int],maxCard:Option[Int]) extends ConstrainedEntitySubProperty
//    
//    def unique[A](prop:ValueProperty[A])=SingleValue(prop,true)
//    def one[A](prop:ValueProperty[A])=SingleValue(prop,false)
//    def optional[A](prop:ValueProperty[A])=OptionalValue(prop)
//    def multiple[A](prop:ValueProperty[A])=MultiValue(prop,None,None) 
//    def atLeastOne[A](prop:ValueProperty[A])=MultiValue(prop,Some(1),None) 
//
//    def unique[TS<:TypedEntity](prop:SubEntityProperty[_ >: TS <: EntityClass],ts:TS)=SingleSubEntity[TS](ts,prop,true)
//    def one[TS<:TypedEntity](prop:SubEntityProperty[_ >: TS <: EntityClass],ts:TS)=SingleSubEntity[TS](ts,prop,false)
//    def optional[TS<:TypedEntity](prop:SubEntityProperty[_ >: TS <: EntityClass],ts:TS)=OptionalSubEntity[TS](ts,prop)
//    def multiple[TS<:TypedEntity](prop:SubEntityProperty[_ >: TS <: EntityClass],ts:TS)=MultiSubEntity[TS](ts,prop,None,None)
//    /** Following function defines type mapping between ConstrainedEntityProperty cases and target value type
//     * ConstrainedEntityProperty[V] => ValidatedFromInstance[R]
//     */
//    object GetValidatedFromInstance extends Poly1 {
//      implicit def caseS[V] = at[SingleValue[V]](x => (instance:UntypedGenericEntityInstance) => 
//        instance.typedValues(x.prop).left.map(err => new PropertyValueError(x,err)) flatMap {
//          case empty if empty.isEmpty => Left(UniquePropertyNotDefined(x)) : Validated[V]
//          case Seq(v) => Right(v) : Validated[V]
//          case mutliple if !x.unique => Right(mutliple.head) : Validated[V]
//          case _  => Left(UniquePropertyDefinedMultipleTimes(x)) : Validated[V]
//        })
//      implicit def caseO[V] = at[OptionalValue[V]](x => (instance:UntypedGenericEntityInstance) => 
//        instance.typedValues(x.prop).left.map(err => new PropertyValueError(x,err)).right.map(_.headOption) : Validated[Option[V]]
//      )
//      implicit def caseM[V] = at[MultiValue[V]](x => (instance:UntypedGenericEntityInstance) => 
//        instance.typedValues(x.prop).left.map(err => new PropertyValueError(x,err)) flatMap {
////        (instance.get(x.prop),x.minCard,x.maxCard) match {
//          case (res) if x.minCard.map(min=>res.size<min).getOrElse(false) => Left(MultiPropertyBelowMinCard(x,x.minCard.get,res.size)) : Validated[Seq[V]]
//          case (res) if x.maxCard.map(max=>res.size>max).getOrElse(false) => Left(MultiPropertyAboveMaxCard(x,x.maxCard.get,res.size)) : Validated[Seq[V]]
//          case (res) => Right(res) : Validated[Seq[V]]
//              
//        })
//      
//      implicit def caseSS[TS<:TypedEntity] = at[SingleSubEntity[TS]](x => (instance:UntypedGenericEntityInstance) => instance.subEntities(x.prop) match {
//        case empty if empty.isEmpty => Left(UniquePropertyNotDefined(x)) : Validated[TS#TypedInstance]
//        case Seq(v) => (x.ts.constructor(v)) : Validated[TS#TypedInstance]
//        case mutliple if !x.unique => (x.ts.constructor(mutliple.head)) : Validated[TS#TypedInstance]
//        case _  => Left(UniquePropertyDefinedMultipleTimes(x)) : Validated[TS#TypedInstance]
//      })
//      implicit def caseOS[TS<:TypedEntity] = at[OptionalSubEntity[TS]](x => (instance:UntypedGenericEntityInstance) => {
//        validatedSeq(instance.subEntities(x.prop).headOption.map(x.ts.constructor).toList).map(_.headOption) : Validated[Option[TS#TypedInstance]]
//      })
//      implicit def caseMS[TS<:TypedEntity] = at[MultiSubEntity[TS]](x => (instance:UntypedGenericEntityInstance) => 
//        (instance.subEntities(x.prop),x.minCard,x.maxCard) match {
//          case (res,Some(min),_) if res.size<min => Left(MultiPropertyBelowMinCard(x,min,res.size)) : Validated[Seq[TS#TypedInstance]]
//          case (res,_,Some(max)) if res.size>max => Left(MultiPropertyAboveMaxCard(x,max,res.size)) : Validated[Seq[TS#TypedInstance]]
//          case (res,_,_) => validatedSeq(res.map(x.ts.constructor)) : Validated[Seq[TS#TypedInstance]]
//              
//        })
//    }
//    //ValidatedFromInstance[V] => ValidatedFromInstance[L] => ValidatedFromInstance[V :: L]
//    object CombineValidatedFromInstance extends Poly2 {
//      implicit  def default[L <: HList,V] =
//        at[ValidatedFromInstance[V],ValidatedFromInstance[L]]{ (t,acc) => 
//          ((instance:UntypedGenericEntityInstance) => 
//            acc(instance).right.flatMap(a => t(instance).right.map(v => v :: a))
//         ) : ValidatedFromInstance[V :: L]
//       }
//    } 
//    
//    /**
//     * Defines a typed entity constructor (from generic entity instance) for type V
//     * The generic representation of V (simply constructed with shapeless.Generic[V]) must be given as the unique parameter (required for good type inferences)
//     */ 
//    def makeCons[VVL <: HList,C <: HList](cons:C)(implicit 
//        mapped:ops.hlist.Mapped.Aux[instanceGen.Repr,ValidatedFromInstance,VVL], 
//        mapper:ops.hlist.Mapper.Aux[GetValidatedFromInstance.type,C,VVL],
//        folder:ops.hlist.RightFolder.Aux[VVL,ValidatedFromInstance[HNil],CombineValidatedFromInstance.type,ValidatedFromInstance[instanceGen.Repr]]
//    ) : UntypedGenericEntityInstance => Validated[TypedInstance] =
//      //get HList of prop values getter functions (of type EntityInstance => Validated[V])
//      (cons.map(GetValidatedFromInstance)
//      //fold to get the ValidatedFromInstance out of the list :  L(ValidatedFromInstance[v]) => ValidatedFromInstance[L(v)]
//       .foldRight(((_:UntypedGenericEntityInstance) => Right(HNil)) : ValidatedFromInstance[HNil])(CombineValidatedFromInstance))
//       //map valid result to the generic constructor of target class
//       .andThen(_.right.map(instanceGen.from))
//    def makeConsWithId[NOID <: HList, VVL <: HList,C <: HList](cons:C)(implicit 
//        tail:ops.hlist.IsHCons.Aux[instanceGen.Repr,String,NOID],
//        mapped:ops.hlist.Mapped.Aux[NOID,ValidatedFromInstance,VVL], 
//        mapper:ops.hlist.Mapper.Aux[GetValidatedFromInstance.type,C,VVL],
//        folder:ops.hlist.RightFolder.Aux[VVL,ValidatedFromInstance[HNil],CombineValidatedFromInstance.type,ValidatedFromInstance[NOID]]
//    ) : UntypedGenericEntityInstance => Validated[TypedInstance] = {
//      val extractValues=
//        //get HList of prop values getter functions (of type EntityInstance => Validated[V])
//        (cons.map(GetValidatedFromInstance)
//        //fold to get the ValidatedFromInstance out of the list :  L(ValidatedFromInstance[v]) => ValidatedFromInstance[L(v)]
//         .foldRight(((_:UntypedGenericEntityInstance) => Right(HNil)) : ValidatedFromInstance[HNil])(CombineValidatedFromInstance))
//         //map valid result to the generic constructor of target class
//       (i:UntypedGenericEntityInstance) => {
//         extractValues(i).right.map(vs => instanceGen.from(tail.cons(i.id , vs)))
//       }
//    }
//       
//       
//    //ValidatedFromInstance[V] => ValidatedFromInstance[L] => ValidatedFromInstance[V :: L]
//    object GetUntypedPropertyValue extends Poly2 {
//      implicit  def caseO[V] =
//        at[Option[V],OptionalValue[V]]{ (v,p) => p.prop -> v.map(p.prop.fmt.writes).toList : (Property,Seq[JsValue])}
//      implicit  def caseS[V] =
//        at[V,SingleValue[V]]{ (v,p) =>p.prop ->  List(p.prop.fmt.writes(v)) : (Property,Seq[JsValue])}
//      implicit  def caseM[V] =
//        at[Seq[V],MultiValue[V]]{ (v,p) => p.prop -> v.map(p.prop.fmt.writes) : (Property,Seq[JsValue])}
//    }
//    object GetUntypedSubProperty extends Poly2 {
//      implicit  def caseOS[TS<:TypedEntity.Aux[V],V] =
//        at[(Option[V],String),OptionalSubEntity[TS]]{ (v,p) => p.prop -> v._1.map(p.ts.toGenericInstance(Property.subEntityId(v._2,p.prop,None),_)).toList : (Property,Seq[UntypedGenericEntityInstance])}
//      implicit  def caseSS[TS<:TypedEntity.Aux[V],V] =
//        at[(V,String),SingleSubEntity[TS]]{ (v,p) => p.prop -> List(p.ts.toGenericInstance(Property.subEntityId(v._2,p.prop,None),v._1)) : (Property,Seq[UntypedGenericEntityInstance])}
//      implicit  def caseMS[TS<:TypedEntity.Aux[V],V] =
//        at[(Seq[V],String),MultiSubEntity[TS]]{ (v,p) => p.prop -> v._1.zipWithIndex.map{case (vv,i) => p.ts.toGenericInstance(Property.subEntityId(v._2,p.prop,Some(i)),vv)} : (Property,Seq[UntypedGenericEntityInstance])}
//      
//    }        
//    def makeUnCons[C <: HList,VPL<:HList,VPV<:HList,SPI<:HList,SPL<:HList,SPV<:HList](cons:C)(implicit 
//        filt:ops.hlist.Filter.Aux[C,ConstrainedEntityValueProperty,VPL],
//        zip:ops.hlist.ZipWith.Aux[instanceGen.Repr,VPL,GetUntypedPropertyValue.type,VPV],
//        trav:ops.hlist.ToTraversable.Aux[VPV,List,(Property,Seq[JsValue])],
//        filt2:ops.hlist.Filter.Aux[C,ConstrainedEntitySubProperty,SPL],
//        zipc:ops.hlist.ZipConst.Aux[String,instanceGen.Repr,SPI],
//        zip2:ops.hlist.ZipWith.Aux[SPI,SPL,GetUntypedSubProperty.type,SPV],
//        trav2:ops.hlist.ToTraversable.Aux[SPV,List,(Property,Seq[UntypedGenericEntityInstance])]
//    ) : (String,TypedInstance) => UntypedGenericEntityInstance = (id:String, i:TypedInstance) => {
//      val genInst=instanceGen.to(i)
//      val (refs,vals) = trav.apply(zip.apply(genInst,filt.apply(cons))).partition(_._1.isInstanceOf[ReferenceProperty[_,_]])
//      UntypedGenericEntityInstance(id,vals.toMap,refs.toMap,trav2.apply(zip2.apply(zipc(id,genInst),filt2.apply(cons))).toMap)
//      throw new Exception("Need to perform differently")
////      vals.toMap
////      instanceGen.to(i).toList.zip(cons.toList).map{
////        case (v:Option[_],p:OptionalValue[_]) => v.toList : Seq[Any] 
////      }
////      UntypedGenericEntityInstance("",Map.empty,Map.empty,Map.empty)
//    }
////      //get HList of prop values getter functions (of type EntityInstance => Validated[V])
////      (cons.map(GetValidatedFromInstance)
////      //fold to get the ValidatedFromInstance out of the list :  L(ValidatedFromInstance[v]) => ValidatedFromInstance[L(v)]
////       .foldRight(((_:UntypedGenericEntityInstance) => Right(HNil)) : ValidatedFromInstance[HNil])(CombineValidatedFromInstance))
////       //map valid result to the generic constructor of target class
////       .andThen(_.right.map(instanceGen.from))
//         
//    def applyTypedInstanceAssertion(id:String,instance:TypedInstance,assertion:InstancePropertyAssertion)={
//      val genInstance=toGenericInstance(id,instance)
//      constructor(applyGenericInstancePropertyAssertion(genInstance,assertion))
//    }
//  
//  }
//}