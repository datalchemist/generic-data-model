package org.euratlas.model
package typed

import play.api.libs.json._

import scala.collection.immutable.Seq

//object TypedModel {
 import generic._
 import Untyped._
  sealed trait TypedModelConstraintFailure
  case class PropertyValueError[E <: TypedEntity](prop:E#ConstrainedEntityProperty,err:String) extends TypedModelConstraintFailure
  case class UniquePropertyNotDefined[E <: TypedEntity](prop:E#ConstrainedEntityProperty) extends TypedModelConstraintFailure
  case class UniquePropertyDefinedMultipleTimes[E <: TypedEntity](prop:E#ConstrainedEntityProperty) extends TypedModelConstraintFailure
  case class MultiPropertyBelowMinCard[E <: TypedEntity](prop:E#ConstrainedEntityProperty,minCard:Int,numValues:Int) extends TypedModelConstraintFailure
  case class MultiPropertyAboveMaxCard[E <: TypedEntity](prop:E#ConstrainedEntityProperty,maxCard:Int,numValues:Int) extends TypedModelConstraintFailure

  object TypedEntity {
    type Validated[V]=scala.Either[TypedModelConstraintFailure,V]
    type ValidatedFromInstance[V] = GenericEntityInstance => Validated[V]
    
    def validatedSeq[V](seq:Seq[Validated[V]]):Validated[Seq[V]]=
      seq.foldRight(Right(Nil) : Validated[List[V]])((next,acc) => acc.right.flatMap(a => next.right.map(n => n :: a)))
      
    type Aux[V]=TypedEntity{type TypedInstance=V}
    
     
    implicit class InstanceTypedExt(i:GenericEntityInstance) {
      def typedValues[V](p:ValueProperty[V])=
        i.values(p).map(p.fmt.reads)
        .foldLeft[Either[String,Vector[V]]](Right(Vector())){
          case (Right(cur),JsSuccess(v,_)) => Right(cur :+ v) 
          case (Right(cur),JsError(err)) => Left(s"Value parsing error: $err")
          case (Left(err),_) => Left(err)
          }
    }  
  }
  
  trait TypedEntity extends EntityClass {
    import shapeless._
    import poly._
    import TypedEntity._

    type TypedInstance
    val instanceGen:Generic[TypedInstance]{type Repr <: HList } //as we consider only simple case class for now 
//    val labelledInstanceGen:LabelledGeneric[TypedInstance]{type Repr <: HList }
    val constructor : ValidatedFromInstance[TypedInstance]
    val toGenericInstance : (String,TypedInstance) => GenericEntityInstance
    

    
    sealed trait ConstrainedEntityProperty{
//      val prop:EntityProperty[A]
    }
    trait ConstrainedEntityValueProperty extends ConstrainedEntityProperty
    case class OptionalValue[A](prop:ValueProperty[A]) extends ConstrainedEntityValueProperty //[A]
    case class SingleValue[A](prop:ValueProperty[A],unique:Boolean) extends ConstrainedEntityValueProperty //[A]
    case class MultiValue[A](prop:ValueProperty[A],minCard:Option[Int],maxCard:Option[Int]) extends ConstrainedEntityValueProperty//[A]
    
    trait ConstrainedEntitySubProperty extends ConstrainedEntityProperty
    case class OptionalSubEntity[TS<:TypedEntity](ts:TS,prop:SubEntityProperty[_ >: TS <: EntityClass]) extends ConstrainedEntitySubProperty //[TS#TypedInstance]
    case class SingleSubEntity[TS<:TypedEntity](ts:TS,prop:SubEntityProperty[_ >: TS <: EntityClass],unique:Boolean) extends ConstrainedEntitySubProperty
    case class MultiSubEntity[TS<:TypedEntity](ts:TS,prop:SubEntityProperty[_ >: TS <: EntityClass],minCard:Option[Int],maxCard:Option[Int]) extends ConstrainedEntitySubProperty
    
    def unique[A](prop:ValueProperty[A])=SingleValue(prop,true)
    def one[A](prop:ValueProperty[A])=SingleValue(prop,false)
    def optional[A](prop:ValueProperty[A])=OptionalValue(prop)
    def multiple[A](prop:ValueProperty[A])=MultiValue(prop,None,None) 
    def atLeastOne[A](prop:ValueProperty[A])=MultiValue(prop,Some(1),None) 

    def unique[TS<:TypedEntity](prop:SubEntityProperty[_ >: TS <: EntityClass],ts:TS)=SingleSubEntity[TS](ts,prop,true)
    def one[TS<:TypedEntity](prop:SubEntityProperty[_ >: TS <: EntityClass],ts:TS)=SingleSubEntity[TS](ts,prop,false)
    def optional[TS<:TypedEntity](prop:SubEntityProperty[_ >: TS <: EntityClass],ts:TS)=OptionalSubEntity[TS](ts,prop)
    def multiple[TS<:TypedEntity](prop:SubEntityProperty[_ >: TS <: EntityClass],ts:TS)=MultiSubEntity[TS](ts,prop,None,None)
    /** Following function defines type mapping between ConstrainedEntityProperty cases and target value type
     * ConstrainedEntityProperty[V] => ValidatedFromInstance[R]
     */
    object GetValidatedFromInstance extends Poly1 {
      implicit def caseS[V] = at[SingleValue[V]](x => (instance:GenericEntityInstance) => 
        instance.typedValues(x.prop).left.map(err => new PropertyValueError(x,err)) flatMap {
          case empty if empty.isEmpty => Left(UniquePropertyNotDefined(x)) : Validated[V]
          case Seq(v) => Right(v) : Validated[V]
          case mutliple if !x.unique => Right(mutliple.head) : Validated[V]
          case _  => Left(UniquePropertyDefinedMultipleTimes(x)) : Validated[V]
        })
      implicit def caseO[V] = at[OptionalValue[V]](x => (instance:GenericEntityInstance) => 
        instance.typedValues(x.prop).left.map(err => new PropertyValueError(x,err)).right.map(_.headOption) : Validated[Option[V]]
      )
      implicit def caseM[V] = at[MultiValue[V]](x => (instance:GenericEntityInstance) => 
        instance.typedValues(x.prop).left.map(err => new PropertyValueError(x,err)) flatMap {
//        (instance.get(x.prop),x.minCard,x.maxCard) match {
          case (res) if x.minCard.map(min=>res.size<min).getOrElse(false) => Left(MultiPropertyBelowMinCard(x,x.minCard.get,res.size)) : Validated[Seq[V]]
          case (res) if x.maxCard.map(max=>res.size>max).getOrElse(false) => Left(MultiPropertyAboveMaxCard(x,x.maxCard.get,res.size)) : Validated[Seq[V]]
          case (res) => Right(res) : Validated[Seq[V]]
              
        })
      
      implicit def caseSS[TS<:TypedEntity] = at[SingleSubEntity[TS]](x => (instance:GenericEntityInstance) => instance.subEntities(x.prop) match {
        case empty if empty.isEmpty => Left(UniquePropertyNotDefined(x)) : Validated[TS#TypedInstance]
        case Seq(v) => (x.ts.constructor(v)) : Validated[TS#TypedInstance]
        case mutliple if !x.unique => (x.ts.constructor(mutliple.head)) : Validated[TS#TypedInstance]
        case _  => Left(UniquePropertyDefinedMultipleTimes(x)) : Validated[TS#TypedInstance]
      })
      implicit def caseOS[TS<:TypedEntity] = at[OptionalSubEntity[TS]](x => (instance:GenericEntityInstance) => {
        validatedSeq(instance.subEntities(x.prop).headOption.map(x.ts.constructor).toList).map(_.headOption) : Validated[Option[TS#TypedInstance]]
      })
      implicit def caseMS[TS<:TypedEntity] = at[MultiSubEntity[TS]](x => (instance:GenericEntityInstance) => 
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
          ((instance:GenericEntityInstance) => 
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
    ) : GenericEntityInstance => Validated[TypedInstance] =
      //get HList of prop values getter functions (of type EntityInstance => Validated[V])
      (cons.map(GetValidatedFromInstance)
      //fold to get the ValidatedFromInstance out of the list :  L(ValidatedFromInstance[v]) => ValidatedFromInstance[L(v)]
       .foldRight(((_:GenericEntityInstance) => Right(HNil)) : ValidatedFromInstance[HNil])(CombineValidatedFromInstance))
       //map valid result to the generic constructor of target class
       .andThen(_.right.map(instanceGen.from))
    def makeConsWithId[NOID <: HList, VVL <: HList,C <: HList](cons:C)(implicit 
        tail:ops.hlist.IsHCons.Aux[instanceGen.Repr,String,NOID],
        mapped:ops.hlist.Mapped.Aux[NOID,ValidatedFromInstance,VVL], 
        mapper:ops.hlist.Mapper.Aux[GetValidatedFromInstance.type,C,VVL],
        folder:ops.hlist.RightFolder.Aux[VVL,ValidatedFromInstance[HNil],CombineValidatedFromInstance.type,ValidatedFromInstance[NOID]]
    ) : GenericEntityInstance => Validated[TypedInstance] = {
      val extractValues=
        //get HList of prop values getter functions (of type EntityInstance => Validated[V])
        (cons.map(GetValidatedFromInstance)
        //fold to get the ValidatedFromInstance out of the list :  L(ValidatedFromInstance[v]) => ValidatedFromInstance[L(v)]
         .foldRight(((_:GenericEntityInstance) => Right(HNil)) : ValidatedFromInstance[HNil])(CombineValidatedFromInstance))
         //map valid result to the generic constructor of target class
       (i:GenericEntityInstance) => {
         extractValues(i).right.map(vs => instanceGen.from(tail.cons(i.id , vs)))
       }
    }
       
       
    //ValidatedFromInstance[V] => ValidatedFromInstance[L] => ValidatedFromInstance[V :: L]
    object GetUntypedPropertyValue extends Poly2 {
      implicit  def caseO[V] =
        at[Option[V],OptionalValue[V]]{ (v,p) => p.prop -> v.map(p.prop.fmt.writes).toList : (Property,Seq[JsValue])}
      implicit  def caseS[V] =
        at[V,SingleValue[V]]{ (v,p) =>p.prop ->  List(p.prop.fmt.writes(v)) : (Property,Seq[JsValue])}
      implicit  def caseM[V] =
        at[Seq[V],MultiValue[V]]{ (v,p) => p.prop -> v.map(p.prop.fmt.writes) : (Property,Seq[JsValue])}
    }
    object GetUntypedSubProperty extends Poly2 {
      implicit  def caseOS[TS<:TypedEntity.Aux[V],V] =
        at[(Option[V],String),OptionalSubEntity[TS]]{ (v,p) => p.prop -> v._1.map(p.ts.toGenericInstance(Property.subEntityId(v._2,p.prop,None),_)).toList : (Property,Seq[GenericEntityInstance])}
      implicit  def caseSS[TS<:TypedEntity.Aux[V],V] =
        at[(V,String),SingleSubEntity[TS]]{ (v,p) => p.prop -> List(p.ts.toGenericInstance(Property.subEntityId(v._2,p.prop,None),v._1)) : (Property,Seq[GenericEntityInstance])}
      implicit  def caseMS[TS<:TypedEntity.Aux[V],V] =
        at[(Seq[V],String),MultiSubEntity[TS]]{ (v,p) => p.prop -> v._1.zipWithIndex.map{case (vv,i) => p.ts.toGenericInstance(Property.subEntityId(v._2,p.prop,Some(i)),vv)} : (Property,Seq[GenericEntityInstance])}
      
    }        
    def makeUnCons[C <: HList,VPL<:HList,VPV<:HList,SPI<:HList,SPL<:HList,SPV<:HList](cons:C)(implicit 
        filt:ops.hlist.Filter.Aux[C,ConstrainedEntityValueProperty,VPL],
        zip:ops.hlist.ZipWith.Aux[instanceGen.Repr,VPL,GetUntypedPropertyValue.type,VPV],
        trav:ops.hlist.ToTraversable.Aux[VPV,List,(Property,Seq[JsValue])],
        filt2:ops.hlist.Filter.Aux[C,ConstrainedEntitySubProperty,SPL],
        zipc:ops.hlist.ZipConst.Aux[String,instanceGen.Repr,SPI],
        zip2:ops.hlist.ZipWith.Aux[SPI,SPL,GetUntypedSubProperty.type,SPV],
        trav2:ops.hlist.ToTraversable.Aux[SPV,List,(Property,Seq[GenericEntityInstance])]
    ) : (String,TypedInstance) => GenericEntityInstance = (id:String, i:TypedInstance) => {
      val genInst=instanceGen.to(i)
      val (refs,vals) = trav.apply(zip.apply(genInst,filt.apply(cons))).partition(_._1.isInstanceOf[ReferenceProperty[_,_]])
      GenericEntityInstance(id,vals.toMap,refs.toMap,trav2.apply(zip2.apply(zipc(id,genInst),filt2.apply(cons))).toMap)
      throw new Exception("Need to perform differently")
//      vals.toMap
//      instanceGen.to(i).toList.zip(cons.toList).map{
//        case (v:Option[_],p:OptionalValue[_]) => v.toList : Seq[Any] 
//      }
//      UntypedGenericEntityInstance("",Map.empty,Map.empty,Map.empty)
    }
//      //get HList of prop values getter functions (of type EntityInstance => Validated[V])
//      (cons.map(GetValidatedFromInstance)
//      //fold to get the ValidatedFromInstance out of the list :  L(ValidatedFromInstance[v]) => ValidatedFromInstance[L(v)]
//       .foldRight(((_:UntypedGenericEntityInstance) => Right(HNil)) : ValidatedFromInstance[HNil])(CombineValidatedFromInstance))
//       //map valid result to the generic constructor of target class
//       .andThen(_.right.map(instanceGen.from))
         
    def applyPropertyPathAssertion(id:String,instance:TypedInstance,assertion:PropertyPathAssertion)={
      val genInstance=toGenericInstance(id,instance)
      constructor(genInstance.apply(assertion))
    }
  
  }  
//}