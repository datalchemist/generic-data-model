package org.euratlas.model.generic

import play.api.libs.json.{JsValue,JsPath,Format, Json,JsObject,Reads,Writes}
      import play.api.libs.json._
import julienrf.json.{derived => JsonDerivation}

import scala.collection.immutable.Seq
      
object DefaultGenericModel2 extends SimpleGenericModel2 {
  type ModelAssertionProvenance=String
  implicit val provFmt: Format[String] = JsPath.format[String]
}
trait SimpleGenericModel2 {
  
  import shapeless._
  
  
  /**
   * 
   * Next steps:
   *  - Add Properties registry
   *  	- consider namespace (sub registry)
   *    - use to json formating property
   *  	
   * 
   * 
   * 
   * 
   * 
   * 
   * 
   * 
   * 
   * 
   * 
   * 
   * 
   * 
   * 
   */
  
  type ModelAssertionProvenance
  implicit val provFmt: Format[ModelAssertionProvenance]
  
  sealed trait Property  {
    val name:String
  }
  case class ValueProperty[A,T<:Symbol](val n:Witness.Aux[T])(implicit val fmt: Format[A]) extends Property{val name=n.value.name}
  class ReferenceProperty[R <: EntityClass,V,T<:Symbol](val entity : R,n:Witness.Aux[T],val encode : (V) => Option[String])(implicit fmt: Format[V]) extends ValueProperty[V,T](n)(fmt)
  case class SubEntityProperty[E<:EntityClass,T<:Symbol](val entity:E,val n:Witness.Aux[T])  extends Property{val name=n.value.name}
  
  trait EntityClass {
    val name:String
  }
  object EntityClass {
    private val existingClasses = collection.mutable.Map.empty[String,EntityClass]
    private def checkAndAddClass(n:String,v:EntityClass) = 
      if (existingClasses contains n) throw new Exception(s"Class defined multiple times: $n" ) 
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
    implicit val fmt:Format[EntityClass]={
      import play.api.libs.json._ // JSON library
      val baseFmt = (__ \ "entityClassName").format[String]
      val read=
        baseFmt.flatMap(cls => 
          forName(cls).fold(Reads.apply[EntityClass](_ => JsError(s"Could not find Entity class $cls")))(Reads.pure)
        )
      Format(read,Writes[EntityClass](c=>baseFmt.writes(c.name)))
    }
  }
  
//  case class PropertyValue(path:ValueProperty[_,_],value:Any) 
//  implicit def RichPropJson[V](prop:ValueProperty[V,_]) = new {
//    def dec(js:JsValue) =
//      prop.fmt.reads(js).map(v => PropertyValue(prop,v))
//  }
  object Property {
    private val existingProperties = collection.mutable.Map.empty[Symbol,Property]
    private[SimpleGenericModel2] def checkAndAddProp[P<:Property](n:Symbol,v:P) = 
      if (existingProperties contains n) throw new Exception(s"Property defined multiple times: $n" )
      else {existingProperties(n)=v;v}
    
    def forName(n:Symbol) = existingProperties.get(n)
    
    /** Json format based on name-based EntityClasses registry */
    implicit val fmt:Format[Property]={ // JSON library
      val baseFmt = (__ \ "propertyName").format[String]
      val read=
        baseFmt.flatMap(prop => 
          forName(Symbol(prop)).fold(Reads.apply[Property](_ => JsError(s"Could not find Property $prop")))(Reads.pure)
        )
      Format(read,Writes[Property](c=>baseFmt.writes(c.name)))
    }
    
    
    def propValuesMapFmt[V](implicit fmt:Format[V]) = 
      Format(
        Reads[Map[Property,V]]((v:JsValue) =>
           Reads.map[V].reads(v).flatMap{res =>
             res.map{case (p,v) => (p -> Property.forName(Symbol(p))) -> v} match {
               case containsErrors if containsErrors.exists(!_._1._2.isDefined) =>
                 JsError(s"Could not find Property ${containsErrors.collect{case ((n,None),_) => n}.mkString(", ")}")
               case good => JsSuccess(good.map{case ((_,Some(p)),v) => (p:Property) -> v})
             }
           }
        ),
        Writes((v:Map[Property,V]) => Json.toJson(v.map{case (p,v) => p.name -> v}))
    )
//    implicit val valuePropFmt=Format(
//      fmt.flatMap{
//        case vp:ValueProperty[_] => Reads.pure[ValueProperty[_]](vp)
//        case p => Reads.apply[ValueProperty[_]](_ => JsError(s"Incompatible property type"))
//      },
//      Writes[ValueProperty[_]](c=>fmt.writes(c))
//    )
//    implicit val refPropFmt : Format[ReferenceProperty[_<:EntityClass,_]]=Format(
//      fmt.flatMap{
//        case vp:ReferenceProperty[_<:EntityClass,_] => Reads.pure[ReferenceProperty[_,_]](vp)
//        case p => Reads.apply[ReferenceProperty[_<:EntityClass,_]](_ => JsError(s"Incompatible property type"))
//      },
//      Writes[ReferenceProperty[_,_]](c=>fmt.writes(c))
//    )
//    implicit val subPropFmt=Format(
//      fmt.flatMap{
//        case vp:SubEntityProperty[_] => Reads.pure[SubEntityProperty[_]](vp)
//        case p => Reads.apply[SubEntityProperty[_]](_ => JsError(s"Incompatible property type"))
//      },
//      Writes[SubEntityProperty[_]](c=>fmt.writes(c))
//    )
  }
  /** Base trait to define general properties */
  trait GeneralProperties {
    val namespace:String
    
    /** Props, refs, subentities constructors */
//    def prop[A](n:String)(implicit f:Format[A])=
    def prop[T<:Symbol](w:Witness.Aux[T]) = new { def apply[A](implicit f:Format[A]) = //new SPProp[A,T](w)}
      Property.checkAndAddProp(w.value,new ValueProperty[A,T](w))
    }
    def subEntity[T<:Symbol](w:Witness.Aux[T]) = new {def apply[S<:EntityClass](e:S)=
      Property.checkAndAddProp(w.value,new SubEntityProperty[S,T](e,w))
    }
    def ref[T<:Symbol](w:Witness.Aux[T]) = new {def apply[R <: EntityClass,A](r:R,enc:A => Option[String])(implicit f:Format[A])=
      Property.checkAndAddProp(w.value,new ReferenceProperty[R,A,T](r,w,enc)) //{implicit val fmt=f;val entity =r;val encode = enc;val name=if (namespace.isEmpty) n else s"$namespace:$n"})
    }
      
      
  }
  
  /** Property-path to reference property (and/or sub-property) recursively, with optional indexing
   *  ex:
   *  	subEntityProp1 --> subEntityProp2 --> prop1
   *  	subEntityProp1 -- 1 --> subEntityProp2 --> prop1		
   *    
   */
  sealed trait PropertyPath[V]{
//    def refEntity:Option[ReferenceProperty[_<:EntityClass,V]]
  }
  case class Direct[V,T<:Symbol](p:ValueProperty[V,T]) extends PropertyPath[V] //{def refEntity=None}
  case class DirectRef[R<:EntityClass,V,T<:Symbol](p:ReferenceProperty[R,V,T]) extends PropertyPath[V] //{def refEntity=Some(p)}
  case class SubPath[S<:EntityClass,V,T<:Symbol](p:SubEntityProperty[S,T],n:PropertyPath[V]) extends PropertyPath[V] //{def refEntity=n.refEntity}
  case class SubIndexedPath[S<:EntityClass,V,T<:Symbol](p:SubEntityProperty[S,T],i:Int,n:PropertyPath[V]) extends PropertyPath[V] //{def refEntity=n.refEntity}
  object PropertyPath {
    implicit def direct[V,T<:Symbol](p:ValueProperty[V,T])=Direct(p)
    implicit def directRef[R<:EntityClass,V,T<:Symbol](p:ReferenceProperty[R,V,T])=DirectRef(p)
  }
  /** Property-path construction DSL */
  sealed trait subPathGen {
    def -->[V](s:PropertyPath[V]):  PropertyPath[V] 
  }
  implicit class subPathGenBase[S<:EntityClass,T<:Symbol](p:SubEntityProperty[S,T]) extends subPathGen {
    def -->[SS<:EntityClass,V](s:SubEntityProperty[SS,T])=new subPathGenRec[S,SS,T](this,s)
    def -->[V](s:PropertyPath[V])=SubPath[S,V,T](p,s)
    
    def --(i:Int)=new subPathGenIdxBase(p,i)
  }
  class subPathGenIdxBase[S<:EntityClass,T<:Symbol](p:SubEntityProperty[S,T],i:Int) extends subPathGen {
    def -->[SS<:EntityClass,V](s:SubEntityProperty[SS,T])=new subPathGenRec[S,SS,T](this,s)
    def -->[V](s:PropertyPath[V])=SubIndexedPath[S,V,T](p,i,s)
  }
  class subPathGenRec[E<:EntityClass,S<:EntityClass,T<:Symbol](b:subPathGen,n:SubEntityProperty[S,T]) extends subPathGen  {
    def -->[V](s:PropertyPath[V]):  PropertyPath[V] = b --> SubPath(n,s)  
    
    def --(i:Int)=new subPathGenIdxRec(b,n,i)
  }
  class subPathGenIdxRec[E<:EntityClass,S<:EntityClass,T<:Symbol](b:subPathGen,n:SubEntityProperty[S,T],i:Int) extends subPathGen  {
    def -->[V](s:PropertyPath[V]):  PropertyPath[V] = b --> SubIndexedPath(n,i,s)  
  }  
  
  sealed trait UntypedPropertyPath {
    def refEntity:Option[Property]
  }
  case class UntypedDirect(p:Property) extends UntypedPropertyPath{def refEntity=None}
  case class UntypedDirectRef(p:Property) extends UntypedPropertyPath{def refEntity=Some(p)}
  case class UntypedSubPath(p:Property,n:UntypedPropertyPath) extends UntypedPropertyPath{def refEntity=n.refEntity}
  case class UntypedSubIndexedPath(p:Property,i:Int,n:UntypedPropertyPath) extends UntypedPropertyPath{def refEntity=n.refEntity}
  object UntypedPropertyPath {
    implicit def fromPropertyPath(p:PropertyPath[_]):UntypedPropertyPath = p match {
      case Direct(p) => UntypedDirect(p)
      case DirectRef(p) => UntypedDirectRef(p)
      case SubPath(p,n) => UntypedSubPath(p,n)
      case SubIndexedPath(p,i,n) => UntypedSubIndexedPath(p,i,n)
    }
    implicit def fmt : OFormat[UntypedPropertyPath] = JsonDerivation.oformat[UntypedPropertyPath]()
  }
  
  
//  sealed trait UntypedAssertion
//  
//  case class EntityExists(entity:EntityClass,entityInstance:String,prov:ModelAssertionProvenance) extends UntypedAssertion
//  case class EntityNotExists(entity:EntityClass,entityInstance:String,prov:ModelAssertionProvenance) extends UntypedAssertion
//  
//  case class DefineSingleEntityProperty(entity:EntityClass,entityInstance:String,property:UntypedPropertyPath,value:JsValue,prov:ModelAssertionProvenance) extends UntypedAssertion
//  case class UndefineProperty(entity:EntityClass,entityInstance:String,property:UntypedPropertyPath,prov:ModelAssertionProvenance) extends UntypedAssertion
//  case class AddEntityProperty(entity:EntityClass,entityInstance:String,property:UntypedPropertyPath,value:JsValue,prov:ModelAssertionProvenance) extends UntypedAssertion
//  case class RemoveEntityProperty(entity:EntityClass,entityInstance:String,property:UntypedPropertyPath,removeIdx:Int,prov:ModelAssertionProvenance) extends UntypedAssertion 
//  
//  object EntityExists {implicit val fmt:Format[EntityExists]=Json.format[EntityExists]}
//  object EntityNotExists {implicit val fmt:Format[EntityNotExists]=Json.format[EntityNotExists]}
//  object DefineSingleEntityProperty {
//    implicit def fmt[V]:Format[DefineSingleEntityProperty]={
//    Json.format[DefineSingleEntityProperty]
//  }}
//  object UndefineProperty {implicit val fmt:Format[UndefineProperty]=Json.format[UndefineProperty]}
    /**
   * Properties is a map from EntityProperty to Seq of value
   */
  object UntypedGenericEntityInstance {
    implicit val propValuesMapFmt = Property.propValuesMapFmt[Seq[JsValue]] 
    implicit val subEntitiesMapFmt = Property.propValuesMapFmt[Seq[UntypedGenericEntityInstance]]
    implicit def fmt : OFormat[UntypedGenericEntityInstance] = JsonDerivation.oformat[UntypedGenericEntityInstance]()
  }
  case class UntypedGenericEntityInstance(id:String,
      propertiesMap:Map[Property,Seq[JsValue]],
      referencesMap:Map[Property,Seq[JsValue]],
      subEntitiesMap:Map[Property,Seq[UntypedGenericEntityInstance]]) {
    private def newSubEntity(prop:Property)=UntypedGenericEntityInstance(s"${id}_${prop.name}",Map.empty,Map.empty,Map.empty)
    private def subEntity(prop:Property)=subEntitiesMap.get(prop).flatMap(_.headOption).getOrElse(newSubEntity(prop))
    private def updateSubEntity(prop:Property,update:UntypedGenericEntityInstance=>UntypedGenericEntityInstance)=
      subEntitiesMap.updated(prop, Seq(update(subEntity(prop))))
    private def updateSubEntity(prop:Property,i:Int,update:UntypedGenericEntityInstance=>UntypedGenericEntityInstance)={
      val subEnts=subEntities(prop).padTo(i+1, newSubEntity(prop))
      subEntitiesMap.updated(prop, subEnts.updated(i, update(subEnts(i))))
    }
    
    def values(p:Property):Seq[JsValue]=propertiesMap.getOrElse(p, Seq.empty)
    def subEntities(prop:Property):Seq[UntypedGenericEntityInstance]=subEntitiesMap.getOrElse(prop,Seq())
      
    def getPropRec(p:UntypedPropertyPath):Seq[JsValue]=p match {
      case UntypedSubIndexedPath(p,i,n) => subEntities(p).drop(i).headOption.getOrElse(newSubEntity(p)).getPropRec(n)
      case UntypedSubPath(p,n) => subEntity(p).getPropRec(n)
      case UntypedDirect(p) => propertiesMap.getOrElse(p, Seq.empty)
      case UntypedDirectRef(p) => referencesMap.getOrElse(p, Seq.empty)
    }
    def setPropRec(p:UntypedPropertyPath,v:JsValue):UntypedGenericEntityInstance=p match {
      case UntypedSubIndexedPath(p,i,n) => copy(subEntitiesMap=updateSubEntity(p,i,_.setPropRec(n,v)))
      case UntypedSubPath(p,n) => copy(subEntitiesMap=updateSubEntity(p,_.setPropRec(n,v)))
      case UntypedDirect(p) => copy(propertiesMap=propertiesMap.updated(p, Seq(v)))
      case UntypedDirectRef(p) => copy(referencesMap = referencesMap.updated(p, Seq(v)))
    }
    def unsetPropRec(p:UntypedPropertyPath):UntypedGenericEntityInstance=p match {
      case UntypedSubIndexedPath(p,i,n) => copy(subEntitiesMap=updateSubEntity(p,i,_.unsetPropRec(n)))
      case UntypedSubPath(p,n) => copy(subEntitiesMap=updateSubEntity(p,_.unsetPropRec(n)))
      case UntypedDirect(p) => copy(propertiesMap=propertiesMap - p)
      case UntypedDirectRef(p) => copy(referencesMap=referencesMap - p)
    }
    def addPropRec(p:UntypedPropertyPath,v:JsValue):UntypedGenericEntityInstance=p match {
      case UntypedSubIndexedPath(p,i,n) => copy(subEntitiesMap=updateSubEntity(p,i,_.addPropRec(n,v)))
      case UntypedSubPath(p,n) => copy(subEntitiesMap=updateSubEntity(p,_.addPropRec(n,v)))
      case UntypedDirect(p) => copy(propertiesMap=propertiesMap.updated(p, propertiesMap.getOrElse(p, Seq.empty) :+ v))
      case UntypedDirectRef(p) => copy(referencesMap=referencesMap.updated(p, referencesMap.getOrElse(p, Seq.empty) :+ v))
    }
    def removePropRec(p:UntypedPropertyPath,remIdx:Int):UntypedGenericEntityInstance=p match {
      case UntypedSubIndexedPath(p,i,n) => copy(subEntitiesMap=updateSubEntity(p,i,_.removePropRec(n,remIdx)))
      case UntypedSubPath(p,n) => copy(subEntitiesMap=updateSubEntity(p,_.removePropRec(n,remIdx)))
      case UntypedDirect(p) => 
        propertiesMap.getOrElse(p, Seq.empty).zipWithIndex.collect{case (v,idx) if  idx != remIdx => v} match {
          //no more values, unset property
          case empty if empty.isEmpty => copy(propertiesMap=propertiesMap - p)
          //otherwise, update with new values
          case newVals => copy(propertiesMap=propertiesMap.updated(p, newVals))
        }
      case UntypedDirectRef(p) => 
        referencesMap.getOrElse(p, Seq.empty).zipWithIndex.collect{case (v,idx) if  idx != remIdx => v} match {
          //no more values, unset property
          case empty if empty.isEmpty => copy(referencesMap=referencesMap - p)
          //otherwise, update with new values
          case newVals => copy(referencesMap=referencesMap.updated(p, newVals))
        }
    }

    
  }  
}

trait TypedModel2 extends SimpleGenericModel2 {
  
  implicit class InstanceTypedExt(i:UntypedGenericEntityInstance) {
    def typedValues[V,T <: Symbol](p:ValueProperty[V,T])=
      i.values(p).map(p.fmt.reads)
      .foldLeft[Either[String,Vector[V]]](Right(Vector())){
        case (Right(cur),JsSuccess(v,_)) => Right(cur :+ v) 
        case (Right(cur),JsError(err)) => Left(s"Value parsing error: $err")
        case (Left(err),_) => Left(err)
        }
  }

  sealed trait TypedModelConstraintFailure
  case class PropertyValueError[E <: TypedEntity](prop:E#ConstrainedEntityProperty,err:String) extends TypedModelConstraintFailure
  case class UniquePropertyNotDefined[E <: TypedEntity](prop:E#ConstrainedEntityProperty) extends TypedModelConstraintFailure
  case class UniquePropertyDefinedMultipleTimes[E <: TypedEntity](prop:E#ConstrainedEntityProperty) extends TypedModelConstraintFailure
  case class MultiPropertyBelowMinCard[E <: TypedEntity](prop:E#ConstrainedEntityProperty,minCard:Int,numValues:Int) extends TypedModelConstraintFailure
  case class MultiPropertyAboveMaxCard[E <: TypedEntity](prop:E#ConstrainedEntityProperty,maxCard:Int,numValues:Int) extends TypedModelConstraintFailure

  type Validated[V]=scala.Either[TypedModelConstraintFailure,V]
  type ValidatedFromInstance[V] = UntypedGenericEntityInstance => Validated[V]
  
  def validatedSeq[V](seq:Seq[Validated[V]]):Validated[Seq[V]]=
    seq.foldRight(Right(Nil) : Validated[List[V]])((next,acc) => acc.right.flatMap(a => next.right.map(n => n :: a)))
      
  
  
  trait TypedEntity extends EntityClass {
    import shapeless._
    import poly._

    type TypedInstance
    val instanceGen:Generic[TypedInstance]{type Repr <: HList } //as we consider only simple case class for now 
    val labelledInstanceGen:LabelledGeneric[TypedInstance]{type Repr <: HList }
    val constructor : ValidatedFromInstance[TypedInstance]
    

    
    sealed trait ConstrainedEntityProperty{
//      val prop:EntityProperty[A]
    }
    case class OptionalValue[A,T <: Symbol](prop:ValueProperty[A, T]) extends ConstrainedEntityProperty //[A]
    case class SingleValue[A,T <: Symbol](prop:ValueProperty[A, T],unique:Boolean) extends ConstrainedEntityProperty //[A]
    case class MultiValue[A,T <: Symbol](prop:ValueProperty[A, T],minCard:Option[Int],maxCard:Option[Int]) extends ConstrainedEntityProperty//[A]
    
    case class OptionalSubEntity[TS<:TypedEntity,T <: Symbol](ts:TS,prop:SubEntityProperty[_ >: TS <: EntityClass, T]) extends ConstrainedEntityProperty //[TS#TypedInstance]
    case class SingleSubEntity[TS<:TypedEntity,T <: Symbol](ts:TS,prop:SubEntityProperty[_ >: TS <: EntityClass, T],unique:Boolean) extends ConstrainedEntityProperty
    case class MultiSubEntity[TS<:TypedEntity,T <: Symbol](ts:TS,prop:SubEntityProperty[_ >: TS <: EntityClass, T],minCard:Option[Int],maxCard:Option[Int]) extends ConstrainedEntityProperty
    
    def unique[A,T <: Symbol](prop:ValueProperty[A, T])=SingleValue(prop,true)
    def one[A,T <: Symbol](prop:ValueProperty[A, T])(implicit sel:ops.record.Selector.Aux[labelledInstanceGen.Repr,T,A])=SingleValue(prop,false)
    def one[A,T <: Symbol](prop:ValueProperty[A, _ <: Symbol],n:Witness.Aux[T])(implicit sel:ops.record.Selector.Aux[labelledInstanceGen.Repr,T,A])=SingleValue(prop,false)
//    def oneRef[TS<:EntityClass,A,T <: Symbol](prop:ReferenceProperty[TS,A, T])(implicit sel:ops.record.Selector.Aux[labelledInstanceGen.Repr,T,A])=SingleValue(prop,false)
    
//    def one[A,T <: Symbol](prop:ValueProperty[A, T])=SingleValue(prop,false)
    def optional[A,T <: Symbol](prop:ValueProperty[A, T])(implicit sel:ops.record.Selector.Aux[labelledInstanceGen.Repr,T,Option[A]])=OptionalValue(prop)
    def multiple[A,T <: Symbol](prop:ValueProperty[A, T])=MultiValue(prop,None,None) 
    def atLeastOne[A,T <: Symbol](prop:ValueProperty[A, T])=MultiValue(prop,Some(1),None) 

    def unique[TS<:TypedEntity,T <: Symbol](prop:SubEntityProperty[_ >: TS <: EntityClass, T],ts:TS)=SingleSubEntity[TS, T](ts,prop,true)
    def one[TS<:TypedEntity,T <: Symbol](prop:SubEntityProperty[_ >: TS <: EntityClass, T],ts:TS)=SingleSubEntity[TS, T](ts,prop,false)
    def optional[TS<:TypedEntity,T <: Symbol](prop:SubEntityProperty[_ >: TS <: EntityClass, T],ts:TS)=OptionalSubEntity[TS, T](ts,prop)
    def multiple[TS<:TypedEntity,T <: Symbol](prop:SubEntityProperty[_ >: TS <: EntityClass, T],ts:TS)=MultiSubEntity[TS, T](ts,prop,None,None)
    /** Following function defines type mapping between ConstrainedEntityProperty cases and target value type
     * ConstrainedEntityProperty[V] => ValidatedFromInstance[R]
     */
    object GetValidatedFromInstance extends Poly1 {
      implicit def caseS[V, T <: Symbol] = at[SingleValue[V, T]](x => (instance:UntypedGenericEntityInstance) => 
        instance.typedValues(x.prop).left.map(err => new PropertyValueError(x,err)) flatMap {
          case empty if empty.isEmpty => Left(UniquePropertyNotDefined(x)) : Validated[V]
          case Seq(v) => Right(v) : Validated[V]
          case mutliple if !x.unique => Right(mutliple.head) : Validated[V]
          case _  => Left(UniquePropertyDefinedMultipleTimes(x)) : Validated[V]
        })
      implicit def caseO[V, T <: Symbol] = at[OptionalValue[V, T]](x => (instance:UntypedGenericEntityInstance) => 
        instance.typedValues(x.prop).left.map(err => new PropertyValueError(x,err)).right.map(_.headOption) : Validated[Option[V]]
      )
      implicit def caseM[V, T <: Symbol] = at[MultiValue[V, T]](x => (instance:UntypedGenericEntityInstance) => 
        instance.typedValues(x.prop).left.map(err => new PropertyValueError(x,err)) flatMap {
//        (instance.get(x.prop),x.minCard,x.maxCard) match {
          case (res) if x.minCard.map(min=>res.size<min).getOrElse(false) => Left(MultiPropertyBelowMinCard(x,x.minCard.get,res.size)) : Validated[Seq[V]]
          case (res) if x.maxCard.map(max=>res.size>max).getOrElse(false) => Left(MultiPropertyAboveMaxCard(x,x.maxCard.get,res.size)) : Validated[Seq[V]]
          case (res) => Right(res) : Validated[Seq[V]]
              
        })
      
      implicit def caseSS[TS<:TypedEntity, T <: Symbol] = at[SingleSubEntity[TS, T]](x => (instance:UntypedGenericEntityInstance) => instance.subEntities(x.prop) match {
        case empty if empty.isEmpty => Left(UniquePropertyNotDefined(x)) : Validated[TS#TypedInstance]
        case Seq(v) => (x.ts.constructor(v)) : Validated[TS#TypedInstance]
        case mutliple if !x.unique => (x.ts.constructor(mutliple.head)) : Validated[TS#TypedInstance]
        case _  => Left(UniquePropertyDefinedMultipleTimes(x)) : Validated[TS#TypedInstance]
      })
      implicit def caseOS[TS<:TypedEntity, T <: Symbol] = at[OptionalSubEntity[TS, T]](x => (instance:UntypedGenericEntityInstance) => {
        validatedSeq(instance.subEntities(x.prop).headOption.map(x.ts.constructor).toList).map(_.headOption) : Validated[Option[TS#TypedInstance]]
      })
      implicit def caseMS[TS<:TypedEntity, T <: Symbol] = at[MultiSubEntity[TS, T]](x => (instance:UntypedGenericEntityInstance) => 
        (instance.subEntities(x.prop),x.minCard,x.maxCard) match {
          case (res,Some(min),_) if res.size<min => Left(MultiPropertyBelowMinCard(x,min,res.size)) : Validated[Seq[TS#TypedInstance]]
          case (res,_,Some(max)) if res.size>max => Left(MultiPropertyAboveMaxCard(x,max,res.size)) : Validated[Seq[TS#TypedInstance]]
          case (res,_,_) => validatedSeq(res.map(x.ts.constructor)) : Validated[Seq[TS#TypedInstance]]
              
        })
    }
    //ValidatedFromInstance[V] => ValidatedFromInstance[L] => ValidatedFromInstance[V :: L]
    object CombineValidatedFromInstance extends Poly2 {
      implicit  def default[L <: HList,V] =
        at[ValidatedFromInstance[V],ValidatedFromInstance[L]]{ (t,acc) => 
          ((instance:UntypedGenericEntityInstance) => 
            acc(instance).right.flatMap(a => t(instance).right.map(v => v :: a))
         ) : ValidatedFromInstance[V :: L]
       }
    } 
    /**
     * Defines a typed entity constructor (from generic entity instance) for type V
     * The generic representation of V (simply constructed with shapeless.Generic[V]) must be given as the unique parameter (required for good type inferences)
     */ 
    def makeCons[VVL <: HList,C <: HList](cons:C)(implicit 
        mapped:ops.hlist.Mapped.Aux[instanceGen.Repr,ValidatedFromInstance,VVL], 
        mapper:ops.hlist.Mapper.Aux[GetValidatedFromInstance.type,C,VVL],
        folder:ops.hlist.RightFolder.Aux[VVL,ValidatedFromInstance[HNil],CombineValidatedFromInstance.type,ValidatedFromInstance[instanceGen.Repr]]
    ) : UntypedGenericEntityInstance => Validated[TypedInstance] =
      //get HList of prop values getter functions (of type EntityInstance => Validated[V])
      (cons.map(GetValidatedFromInstance)
      //fold to get the ValidatedFromInstance out of the list :  L(ValidatedFromInstance[v]) => ValidatedFromInstance[L(v)]
       .foldRight(((_:UntypedGenericEntityInstance) => Right(HNil)) : ValidatedFromInstance[HNil])(CombineValidatedFromInstance))
       //map valid result to the generic constructor of target class
       .andThen(_.right.map(instanceGen.from))
         
  
  }
}