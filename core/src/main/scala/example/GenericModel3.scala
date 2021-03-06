package example

import play.api.libs.json.{Format, Json,JsObject,Reads,Writes}


//import akka.Done
//import com.lightbend.lagom.scaladsl.persistence.{AggregateEventTag,AggregateEvent,PersistentEntity}
//import com.lightbend.lagom.scaladsl.persistence.PersistentEntity.ReplyType
//import com.lightbend.lagom.scaladsl.playjson.{JsonSerializerRegistry, JsonSerializer}

object GenericModel3 {
  
  
  trait Entity {self => 
//    type Instance //<: {def id:String}
    
//    final case class EntityProperty[A](prop:ValueProperty[A])
//    sealed case class EntityPropertyWithValue[A](prop:EntityProperty[A],value:A)
//    def prop[A](prop:ValueProperty[A])=EntityProperty[A](prop)
//    
//    final case class EntityReferenceProperty[R <: Entity,V](prop:ReferenceProperty[R,V])
//    def ref[R <: Entity,V](prop:ReferenceProperty[R,V])=EntityReferenceProperty[R,V](prop)
//    
//    final case class EntitySubEntityProperty[S<:Entity](prop:SubEntityProperty[S])
//    def subEntity[S<:Entity](prop:SubEntityProperty[S])=EntitySubEntityProperty[S](prop)
  }
  
  
  trait Property[A] {
    implicit val fmt:Format[A]
    val name:String
  }
  trait ValueProperty[A] extends Property[A]
  trait ReferenceProperty[R <: Entity,V] extends Property[V] {
    val encode : (V) => Option[String]
    val name:String
    val entity : R
  }
  trait SubEntityProperty[E<:Entity] { //extends Property[]
    val name : String
    val entity : E
  }
  
  
  trait GeneralProperties {
    val existingProps = collection.mutable.Set.empty[String]
    def checkAndAddProp[V](n:String,v:V) = 
      if (existingProps contains n) throw new Exception(s"Property defined multiple times: $n" ) 
      else {existingProps+=n;v}
    def prop[A](n:String)(implicit f:Format[A])=
      checkAndAddProp(n,new ValueProperty[A]{implicit val fmt=f;val name=n})
    def subEntity[S<:Entity](e:S,n:String)=
      checkAndAddProp(n,new SubEntityProperty[S]{val entity =e;val name=n})
    def ref[R <: Entity,A](r:R,n:String,enc:A => Option[String])(implicit f:Format[A])=
      checkAndAddProp(n,new ReferenceProperty[R,A]{implicit val fmt=f;val entity =r;val encode = enc;val name=n})
  }
  sealed trait PropertyPath[V]{
    def refEntity:Option[ReferenceProperty[_<:Entity,V]]
  }
  case class Direct[V](p:Property[V]) extends PropertyPath[V]{def refEntity=None}
  case class DirectRef[R<:Entity,V](p:ReferenceProperty[R,V]) extends PropertyPath[V]{def refEntity=Some(p)}
  case class SubPath[S<:Entity,V](p:SubEntityProperty[S],n:PropertyPath[V]) extends PropertyPath[V]{def refEntity=n.refEntity}
  case class SubIndexedPath[S<:Entity,V](p:SubEntityProperty[S],i:Int,n:PropertyPath[V]) extends PropertyPath[V]{def refEntity=n.refEntity}
  object PropertyPath {
    implicit def direct[V](p:Property[V])=Direct(p)
    implicit def directRef[R<:Entity,V](p:ReferenceProperty[R,V])=DirectRef(p)
  }
  /** Property-path construction DSL */
  sealed trait subPathGen {
    def -->[V](s:PropertyPath[V]):  PropertyPath[V] 
  }
  implicit class subPathGenBase[S<:Entity](p:SubEntityProperty[S]) extends subPathGen {
    def -->[SS<:Entity,V](s:SubEntityProperty[SS])=new subPathGenRec[S,SS](this,s)
    def -->[V](s:PropertyPath[V])=SubPath[S,V](p,s)
    
    def --(i:Int)=new subPathGenIdxBase(p,i)
  }
  class subPathGenIdxBase[S<:Entity](p:SubEntityProperty[S],i:Int) extends subPathGen {
    def -->[SS<:Entity,V](s:SubEntityProperty[SS])=new subPathGenRec[S,SS](this,s)
    def -->[V](s:PropertyPath[V])=SubIndexedPath[S,V](p,i,s)
  }
  class subPathGenRec[E<:Entity,S<:Entity](b:subPathGen,n:SubEntityProperty[S]) extends subPathGen  {
    def -->[V](s:PropertyPath[V]):  PropertyPath[V] = b --> SubPath(n,s)  
    
    def --(i:Int)=new subPathGenIdxRec(b,n,i)
  }
  class subPathGenIdxRec[E<:Entity,S<:Entity](b:subPathGen,n:SubEntityProperty[S],i:Int) extends subPathGen  {
    def -->[V](s:PropertyPath[V]):  PropertyPath[V] = b --> SubIndexedPath(n,i,s)  
  }
  
  trait Provenance
  
  def prov=new Provenance {}
  
  case class SetPropertyCommand[E<:Entity,V](entityId:String,property:ValueProperty[V],value:V,prov:Provenance)
  
  sealed trait ModelCommand[R] //extends ReplyType[R]
  
  case class EntityExistsCommand[E<:Entity](entity:E,entityInstance:String,prov:Provenance) extends ModelCommand[Unit] 
  case class EntityNotExistsCommand[E<:Entity](entity:E,entityInstance:String,prov:Provenance) extends ModelCommand[Unit] 
  
  case class SetEntityPropertyCommand[E<:Entity,V](entity:E,entityInstance:String,property:PropertyPath[V],value:V,prov:Provenance)
  extends ModelCommand[Unit] 
  case class UnsetEntityPropertyCommand[E<:Entity,V](entity:E,entityInstance:String,property:PropertyPath[V],prov:Provenance)
  extends ModelCommand[Unit] 
  case class AddEntityPropertyCommand[E<:Entity,V](entity:E,entityInstance:String,property:PropertyPath[V],value:V,prov:Provenance)
  extends ModelCommand[Unit]
  case class RemoveEntityPropertyCommand[E<:Entity,V](entity:E,entityInstance:String,property:PropertyPath[V],removeIdx:Int,prov:Provenance)
  extends ModelCommand[Unit]
  
  /** Commands DSL */
  implicit class RichEntity[E<:Entity](entity:E) {
    def apply(entityInstance:String)=new RichEntityInstance(entityInstance)
    class RichEntityInstance(instance:String) {
      def create=EntityExistsCommand(entity,instance,prov)
      def set[V](p:PropertyPath[V])=new RichEntityInstancePropertyPath(p) //RichEntityInstanceProperty(p)
      class RichEntityInstancePropertyPath[V](p:PropertyPath[V]) {
        def :=(v:V)=SetEntityPropertyCommand[E,V](entity,instance,p, v,prov)
      }
    }
  }
  
  
  /**
   * Properties is a map from EntityProperty to Seq of value
   */
  case class EntityInstance(id:String,
      propertiesMap:Map[Property[_],Seq[_]],
      referencesMap:Map[ReferenceProperty[_,_],Seq[_]],
      subEntitiesMap:Map[SubEntityProperty[_],Seq[EntityInstance]]) {
    private def newSubEntity(prop:SubEntityProperty[_])=EntityInstance(s"${id}_${prop.name}",Map.empty,Map.empty,Map.empty)
    private def subEntity[S<:Entity](prop:SubEntityProperty[S])=subEntitiesMap.get(prop).flatMap(_.headOption).getOrElse(newSubEntity(prop))
    private def updateSubEntity[S<:Entity](prop:SubEntityProperty[S],update:EntityInstance=>EntityInstance)=
      subEntitiesMap.updated(prop, Seq(update(subEntity(prop))))
    private def updateSubEntity(prop:SubEntityProperty[_],i:Int,update:EntityInstance=>EntityInstance)={
      val subEnts=subEntities(prop).padTo(i+1, newSubEntity(prop))
      subEntitiesMap.updated(prop, subEnts.updated(i, update(subEnts(i))))
    }
    
    def get[V](p:Property[V]):Seq[V]=propertiesMap.getOrElse(p, Seq.empty).map(_.asInstanceOf[V])
    def subEntities(prop:SubEntityProperty[_]):Seq[EntityInstance]=subEntitiesMap.getOrElse(prop,Seq())
      
    def getPropRec[V](p:PropertyPath[V]):Seq[V]=p match {
      case SubIndexedPath(p,i,n) => subEntities(p).drop(i).headOption.getOrElse(newSubEntity(p)).getPropRec(n)
      case SubPath(p,n) => subEntity(p).getPropRec(n)
      case Direct(p) => propertiesMap.getOrElse(p, Seq.empty).map(_.asInstanceOf[V])
      case DirectRef(p) => referencesMap.getOrElse(p, Seq.empty).map(_.asInstanceOf[V])
    }
    def setPropRec[V](p:PropertyPath[V],v:V):EntityInstance=p match {
      case SubIndexedPath(p,i,n) => copy(subEntitiesMap=updateSubEntity(p,i,_.setPropRec(n,v)))
      case SubPath(p,n) => copy(subEntitiesMap=updateSubEntity(p,_.setPropRec(n,v)))
      case Direct(p) => copy(propertiesMap=propertiesMap.updated(p, Seq(v)))
      case DirectRef(p) => copy(referencesMap = referencesMap.updated(p, Seq(v)))
    }
    def unsetPropRec[V](p:PropertyPath[V]):EntityInstance=p match {
      case SubIndexedPath(p,i,n) => copy(subEntitiesMap=updateSubEntity(p,i,_.unsetPropRec(n)))
      case SubPath(p,n) => copy(subEntitiesMap=updateSubEntity(p,_.unsetPropRec(n)))
      case Direct(p) => copy(propertiesMap=propertiesMap - p)
      case DirectRef(p) => copy(referencesMap=referencesMap - p)
    }
    def addPropRec[V](p:PropertyPath[V],v:V):EntityInstance=p match {
      case SubIndexedPath(p,i,n) => copy(subEntitiesMap=updateSubEntity(p,i,_.addPropRec(n,v)))
      case SubPath(p,n) => copy(subEntitiesMap=updateSubEntity(p,_.addPropRec(n,v)))
      case Direct(p) => copy(propertiesMap=propertiesMap.updated(p, propertiesMap.getOrElse(p, Seq.empty) :+ v))
      case DirectRef(p) => copy(referencesMap=referencesMap.updated(p, referencesMap.getOrElse(p, Seq.empty) :+ v))
    }
    def removePropRec[V](p:PropertyPath[V],remIdx:Int):EntityInstance=p match {
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
  
  sealed trait ModelCommandFailure
  case class EntityNotExists[E<:Entity](e:E,id:String) extends ModelCommandFailure
  case class EntityAlreadyExists[E<:Entity](e:E,id:String) extends ModelCommandFailure
  
  /** GenericModel: updated with ModelCommand
   *  -> NOT TO BE USED for a whole big model that could not fit in memory
   *  - basic entity existence check
   *  - no reference check (quite easy to add but is it useful as it will not be the effective model updater)
   *  
   */
  case class Model(
      entities : Map[Entity,Map[String,EntityInstance]] //,
//      entities2 : Map[Entity,Map[Entity#Instance,Map[Entity#EntityReferenceProperty[_,_],PropertyWithValue[_]]]]
    ) {
    case class entity[E<:Entity](val e:E) {def instance(i:String)=entityInstance(e,i)}
    case class entityInstance[E<:Entity](val e:E,i:String) {
      def getProp[V](prop:PropertyPath[V])=Model.this.getProp[E,V](e,i,prop)
//      def getRef[R<:Entity](r:R,prop:Property[String])=Model.this.getRef[E,R](e,i,r,prop)      
    }
    
    def getProp[E<:Entity,V](e:E,p:String,prop:PropertyPath[V])=
      entities.get(e)
      .flatMap(_.get(p)
        .map(_.getPropRec(prop))
      )
      .getOrElse(Seq.empty)
//    def getRef[E<:Entity,R<:Entity](e:E,p:String,r:R,prop:Property[String])=
//      getProp(e,p,prop)
//      .flatMap(refId => entities.get(r).flatMap(_.get(refId)))
        
    def updateExistingEntityInstance[E<:Entity](entity:E,id:String,transform:EntityInstance => Either[ModelCommandFailure,EntityInstance])= {
      entities.get(entity).flatMap(_.get(id))
      .toRight(EntityNotExists(entity,id))
      .right.flatMap(current => 
        transform(current).right.map(newEntity =>
          copy(entities=entities.updated(entity,entities(entity).updated(id, newEntity)))
        )
      )
    }
    
    def apply(cmd:ModelCommand[_])=cmd match {
      case cmd @ EntityExistsCommand(entity,id,prov) =>
        val currentInstanceMap=entities.getOrElse(entity, Map.empty)
        if (currentInstanceMap contains id) Left(EntityAlreadyExists(entity,id))
        else Right(copy(entities=entities.updated(entity, currentInstanceMap + (id -> EntityInstance(id,Map.empty,Map.empty,Map.empty))))) 
      case cmd @ EntityNotExistsCommand(entity,id,prov) =>
        val currentInstanceMap=entities.getOrElse(entity, Map.empty)
        if (currentInstanceMap contains id) Right(copy(entities=entities.updated(entity, currentInstanceMap - id))) 
        else Left(EntityNotExists(entity,id))
      case cmd @ SetEntityPropertyCommand(entity,id,prop,value,prov) =>
        /** Beginning of a reference check  => discarded for the moment */
//        (prop.refEntity) match {
//          case Some(refEntity) => 
//            val ref =
//              refEntity.encode(value).flatMap(ref =>
//                entities.get(refEntity.entity)
//                .flatMap(_.get(ref))
//              )
//        }
        updateExistingEntityInstance(entity,id,v => Right(v.setPropRec(prop,value)))
      case cmd @ UnsetEntityPropertyCommand(entity,id,prop,prov) =>
        updateExistingEntityInstance(entity,id,v => Right(v.unsetPropRec(prop)))
      case cmd @ AddEntityPropertyCommand(entity,id,prop,value,prov) =>
        updateExistingEntityInstance(entity,id,v => Right(v.addPropRec(prop,value)))
      case cmd @ RemoveEntityPropertyCommand(entity,id,prop,remIdx,prov) =>
        updateExistingEntityInstance(entity,id,v => Right(v.removePropRec(prop,remIdx)))
    }
    
//    implicit class RichEntityModel[E<:Entity](entity:E) {
      def entity[E<:Entity](entity:E,id:String)=entityInstance(entity,id)
//    }
    
  }
  
  abstract class TypedModel(m: Model) {
    val typedEntitiesMap:Map[Entity, TypedEntity]
    lazy val typedEntities={
      for {
        (entity,instances) <- m.entities
        typedEntity <-typedEntitiesMap.get(entity)
        typedInstances = instances.map{case (id,inst) => id -> typedEntity.constructor(inst)}
      } yield typedEntity -> typedInstances
    }
    def typedInstances[E <: TypedEntity](typedEntity : E)=typedEntities.get(typedEntity).asInstanceOf[Option[Map[String, Either[String, typedEntity.TypedInstance]]]]
    
  }
  
  sealed trait TypedModelConstraintFailure
  case class UniquePropertyNotDefined[E <: TypedEntity](prop:E#ConstrainedEntityProperty) extends TypedModelConstraintFailure
  case class UniquePropertyDefinedMultipleTimes[E <: TypedEntity](prop:E#ConstrainedEntityProperty) extends TypedModelConstraintFailure
  case class MultiPropertyBelowMinCard[E <: TypedEntity](prop:E#ConstrainedEntityProperty,minCard:Int,numValues:Int) extends TypedModelConstraintFailure
  case class MultiPropertyAboveMaxCard[E <: TypedEntity](prop:E#ConstrainedEntityProperty,maxCard:Int,numValues:Int) extends TypedModelConstraintFailure
  trait TypedEntity extends Entity {
    import shapeless._
    import poly._

    type TypedInstance
    val instanceGen:Generic[TypedInstance]{type Repr <: HList } //as we consider only simple case class for now 
    val constructor : ValidatedFromInstance[TypedInstance]
    
    type Validated[V]=scala.Either[TypedModelConstraintFailure,V]
    type ValidatedFromInstance[V] = EntityInstance => Validated[V]
    
    def validatedSeq[V](seq:Seq[Validated[V]]):Validated[Seq[V]]=
      seq.foldRight(Right(Nil) : Validated[List[V]])((next,acc) => acc.right.flatMap(a => next.right.map(n => n :: a)))
    
    
    sealed trait ConstrainedEntityProperty{
//      val prop:EntityProperty[A]
    }
    case class OptionalValue[A](prop:Property[A]) extends ConstrainedEntityProperty //[A]
    case class SingleValue[A](prop:Property[A],unique:Boolean) extends ConstrainedEntityProperty //[A]
    case class MultiValue[A](prop:Property[A],minCard:Option[Int],maxCard:Option[Int]) extends ConstrainedEntityProperty//[A]
    
    case class OptionalSubEntity[TS<:TypedEntity](ts:TS,prop:SubEntityProperty[_ >: TS <: Entity]) extends ConstrainedEntityProperty //[TS#TypedInstance]
    case class SingleSubEntity[TS<:TypedEntity](ts:TS,prop:SubEntityProperty[_ >: TS <: Entity],unique:Boolean) extends ConstrainedEntityProperty
    case class MultiSubEntity[TS<:TypedEntity](ts:TS,prop:SubEntityProperty[_ >: TS <: Entity],minCard:Option[Int],maxCard:Option[Int]) extends ConstrainedEntityProperty
    
    def unique[A](prop:Property[A])=SingleValue(prop,true)
    def one[A](prop:Property[A])=SingleValue(prop,false)
    def optional[A](prop:Property[A])=OptionalValue(prop)
    def multiple[A](prop:Property[A])=MultiValue(prop,None,None) 
    def atLeastOne[A](prop:Property[A])=MultiValue(prop,Some(1),None) 

    def unique[TS<:TypedEntity](prop:SubEntityProperty[_ >: TS <: Entity],ts:TS)=SingleSubEntity[TS](ts,prop,true)
    def one[TS<:TypedEntity](prop:SubEntityProperty[_ >: TS <: Entity],ts:TS)=SingleSubEntity[TS](ts,prop,false)
    def optional[TS<:TypedEntity](prop:SubEntityProperty[_ >: TS <: Entity],ts:TS)=OptionalSubEntity[TS](ts,prop)
    def multiple[TS<:TypedEntity](prop:SubEntityProperty[_ >: TS <: Entity],ts:TS)=MultiSubEntity[TS](ts,prop,None,None)
    /** Following function defines type mapping between ConstrainedEntityProperty cases and target value type
     * ConstrainedEntityProperty[V] => ValidatedFromInstance[R]
     */
    object GetValidatedFromInstance extends Poly1 {
      implicit def caseS[V] = at[SingleValue[V]](x => (instance:EntityInstance) => instance.get(x.prop) match {
        case empty if empty.isEmpty => Left(UniquePropertyNotDefined(x)) : Validated[V]
        case Seq(v) => Right(v) : Validated[V]
        case mutliple if !x.unique => Right(mutliple.head) : Validated[V]
        case _  => Left(UniquePropertyDefinedMultipleTimes(x)) : Validated[V]
      })
      implicit def caseO[V] = at[OptionalValue[V]](x => (instance:EntityInstance) => Right(instance.get(x.prop).headOption) : Validated[Option[V]])
      implicit def caseM[V] = at[MultiValue[V]](x => (instance:EntityInstance) => 
        (instance.get(x.prop),x.minCard,x.maxCard) match {
          case (res,Some(min),_) if res.size<min => Left(MultiPropertyBelowMinCard(x,min,res.size)) : Validated[Seq[V]]
          case (res,_,Some(max)) if res.size>max => Left(MultiPropertyAboveMaxCard(x,max,res.size)) : Validated[Seq[V]]
          case (res,_,_) => Right(res) : Validated[Seq[V]]
              
        })
      
      implicit def caseSS[TS<:TypedEntity] = at[SingleSubEntity[TS]](x => (instance:EntityInstance) => instance.subEntities(x.prop) match {
        case empty if empty.isEmpty => Left(UniquePropertyNotDefined(x)) : Validated[TS#TypedInstance]
        case Seq(v) => (x.ts.constructor(v)) : Validated[TS#TypedInstance]
        case mutliple if !x.unique => (x.ts.constructor(mutliple.head)) : Validated[TS#TypedInstance]
        case _  => Left(UniquePropertyDefinedMultipleTimes(x)) : Validated[TS#TypedInstance]
      })
      implicit def caseOS[TS<:TypedEntity] = at[OptionalSubEntity[TS]](x => (instance:EntityInstance) => 
        validatedSeq(instance.subEntities(x.prop).headOption.map(x.ts.constructor).toSeq).map(_.headOption) : Validated[Option[TS#TypedInstance]]
      )
      implicit def caseMS[TS<:TypedEntity] = at[MultiSubEntity[TS]](x => (instance:EntityInstance) => 
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
          ((instance:EntityInstance) => 
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
    ) : EntityInstance => Validated[TypedInstance] =
      //get HList of prop values getter functions (of type EntityInstance => Validated[V])
      (cons.map(GetValidatedFromInstance)
      //fold to get the ValidatedFromInstance out of the list :  L(ValidatedFromInstance[v]) => ValidatedFromInstance[L(v)]
       .foldRight(((_:EntityInstance) => Right(HNil)) : ValidatedFromInstance[HNil])(CombineValidatedFromInstance))
       //map valid result to the generic constructor of target class
       .andThen(_.right.map(instanceGen.from))
       
    
   
    /** Followig commented codes is the various attempts to define a simple Seq[ConstrainedPropDef] to case class constructor generator */
    
//  class GetInstanceValueG(instance:EntityInstance) extends shapeless.Poly1 {
//    implicit def caseS[V] = at[SingleValue[V]](x => Right(instance.get(x.prop).head) : Validated[V])
//    implicit def caseO[V] = at[OptionalValue[V]](x => Right(instance.get(x.prop).headOption) : Validated[Option[V]])
//    implicit def caseM[V] = at[MultiValue[V]](x => Right(instance.get(x.prop)) : Validated[Seq[V]])
//  }
//    
//    def getV[A](prop:EntityProperty[A]):Seq[A]
////    object choose extends (Set ~> Option) {
////      def apply[T](s : Set[T]) = s.headOption
////    }
//    
//    
//    
//    object Get extends (Validated ~> Id) {
//  def apply[T](o : Validated[T]) = o.right.get
//}
//    object GetFL extends Poly2 {
//  implicit  def default[L <: HList,V] =
//    at[L, Validated[V]]{ (acc, t) => t.right.get :: acc }
//}
//    object GetFL2 extends Poly2 {
//  implicit  def default[L <: HList,V] =
//    at[Validated[V],Validated[L]]{ (t,acc) => acc.right.flatMap(a => t.right.map(v => v :: a)) }
//}
//    
//    object GetValue extends Poly1 {
//      implicit def caseS[V] = at[SingleValue[V]](x => getV(x.prop).head)
//      implicit def caseO[V] = at[OptionalValue[V]](x => getV(x.prop).headOption)
//      implicit def caseM[V] = at[MultiValue[V]](x => getV(x.prop))
//    }
////    type Cons <: shapeless.HList
//    class Cons[C <: HList,V<:HList](cons:C)(implicit mapped:ops.hlist.Mapped.Aux[V,ConstrainedEntityProperty,C])
//    class Cons2[C <: HList,V<:HList](cons:C)(implicit mapped:ops.hlist.Mapper.Aux[GetValue.type,C,V])
//    class Cons3[C <: HList,V<:HList](cons:C,gen:Generic[_]{type Repr=V})(implicit mapped:ops.hlist.Mapper.Aux[GetValue.type,C,V])
//    class Cons4[V] {
//      def apply[C <: HList,VL<:HList](cons:C)(implicit mapped:ops.hlist.Mapper.Aux[GetValue.type,C,VL], gen : Generic.Aux[V, VL])=1
//    }
//    class Cons5[V](instance:EntityInstance) {
//      object GetInstanceValue extends Poly1 {
//        implicit def caseS[V] = at[SingleValue[V]](x => instance.get(x.prop).head)
//        implicit def caseO[V] = at[OptionalValue[V]](x => instance.get(x.prop).headOption)
//        implicit def caseM[V] = at[MultiValue[V]](x => instance.get(x.prop))
//      }
//      def tt[VL <: HList]()(implicit gen : Generic.Aux[V, VL],mapped:ops.hlist.Mapped[VL,Option])=1
//      def apply[C <: HList,VL<:HList](cons:C)(implicit mapped:ops.hlist.Mapper.Aux[GetInstanceValue.type,C,VL], gen : Generic.Aux[V, VL])=1
//      
//    }
//    class Cons5b[V](instance:EntityInstance) {
//        object GetInstanceValue extends Poly1 {
//          implicit def caseS[V] = at[SingleValue[V]](x => instance.get(x.prop).head)
//          implicit def caseO[V] = at[OptionalValue[V]](x => instance.get(x.prop).headOption)
//          implicit def caseM[V] = at[MultiValue[V]](x => instance.get(x.prop))
//        }
//      def get[VL <: HList](implicit gen : Generic.Aux[V,VL])= new {
//        
//        def tt[VL <: HList]()(implicit gen : Generic.Aux[V, VL],mapped:ops.hlist.Mapped[VL,Option])=1
//        def apply[C <: HList](cons:C)(implicit mapped:ops.hlist.Mapper.Aux[GetInstanceValue.type,C,VL])=1
//      }
//      
//    }
//    class Cons6[V](instance:EntityInstance)(implicit gen : Generic[V]{type Repr <: HList}) {
//      def tt()(implicit mapped:ops.hlist.Mapped[gen.Repr,Option])=1
//      object GetInstanceValue extends Poly1 {
//        implicit def caseS[V] = at[SingleValue[V]](x => Some(instance.get(x.prop).head) : Option[V])
//        implicit def caseO[V] = at[OptionalValue[V]](x => Some(instance.get(x.prop).headOption) : Option[Option[V]])
//        implicit def caseM[V] = at[MultiValue[V]](x => Some(instance.get(x.prop)) : Option[Seq[V]])
//      }
//      def apply[C <: HList,OVL<:HList](cons:C)(implicit mapped:ops.hlist.Mapped.Aux[gen.Repr,Option,OVL], mapper:ops.hlist.Mapper.Aux[GetInstanceValue.type,C,OVL])=1
//      
//    }
//    class Cons6b[V](instance:EntityInstance) {
//      object GetInstanceValue extends Poly1 {
//        implicit def caseS[V] = at[SingleValue[V]](x => Some(instance.get(x.prop).head) : Option[V])
//        implicit def caseO[V] = at[OptionalValue[V]](x => Some(instance.get(x.prop).headOption) : Option[Option[V]])
//        implicit def caseM[V] = at[MultiValue[V]](x => Some(instance.get(x.prop)) : Option[Seq[V]])
//      }
//    object Get extends (Option ~> Id) {
//  def apply[T](o : Option[T]) = o.get
//}
//      def get[VL <: HList](implicit gen : Generic.Aux[V,VL],mapped:ops.hlist.Mapped[VL,Option])= new {
//        
//        def tt[VL <: HList]()(implicit gen : Generic.Aux[V, VL],mapped:ops.hlist.Mapped[VL,Option])=1
//        def apply[C <: HList](cons:C)(implicit mapper:ops.hlist.Mapper.Aux[GetInstanceValue.type,C,mapped.Out],idMapper:ops.hlist.Mapper.Aux[Get.type,mapped.Out,VL])=
//          gen.from(cons.map(GetInstanceValue).map(Get))
//        
//      }
//      
//    }
//    
//    
//    class Cons7[V](instance:EntityInstance) {
//      object GetInstanceValue extends Poly1 {
//        implicit def caseS[V] = at[SingleValue[V]](x => Right(instance.get(x.prop).head) : Validated[V])
//        implicit def caseO[V] = at[OptionalValue[V]](x => Right(instance.get(x.prop).headOption) : Validated[Option[V]])
//        implicit def caseM[V] = at[MultiValue[V]](x => Right(instance.get(x.prop)) : Validated[Seq[V]])
//      }
//      def get[VL <: HList](implicit gen : Generic.Aux[V,VL],mapped:ops.hlist.Mapped[VL,Validated])= new {
//        
//        def tt[VL <: HList]()(implicit gen : Generic.Aux[V, VL],mapped:ops.hlist.Mapped[VL,Option])=1
//        def apply[C <: HList](cons:C)(implicit mapper:ops.hlist.Mapper.Aux[GetInstanceValue.type,C,mapped.Out],idMapper:ops.hlist.RightFolder.Aux[mapped.Out,Validated[HNil],GetFL2.type,Validated[VL]])=
//          (cons.map(GetInstanceValue).foldRight(Right(HNil) : Validated[HNil])(GetFL2)).right.map(gen.from)
//        
//      }
//      
//    }
//    class Cons7b2[V] {
//      def get[VL <: HList](implicit gen : Generic.Aux[V,VL],mapped:ops.hlist.Mapped[VL,ValidatedFromInstance])= new {
//        def apply[C <: HList](cons:C)(implicit mapper:ops.hlist.Mapper.Aux[GetValidatedFromInstance.type,C,mapped.Out],idMapper:ops.hlist.RightFolder.Aux[mapped.Out,ValidatedFromInstance[HNil],CombineValidatedFromInstance.type,ValidatedFromInstance[VL]])=
////          (instance:EntityInstance) => 
//            (cons.map(GetValidatedFromInstance).foldRight(((_:EntityInstance) => Right(HNil)) : ValidatedFromInstance[HNil])(CombineValidatedFromInstance)).andThen(_.right.map(gen.from))
//        
//      }
//      
//    }       
//    class Cons8[V,VL <: HList,VVL <: HList](gen : Generic.Aux[V,VL])(implicit mapped:ops.hlist.Mapped.Aux[VL,ValidatedFromInstance,VVL])  {
//      def apply[C <: HList](cons:C)(implicit mapper:ops.hlist.Mapper.Aux[GetValidatedFromInstance.type,C,VVL],idMapper:ops.hlist.RightFolder.Aux[VVL,ValidatedFromInstance[HNil],CombineValidatedFromInstance.type,ValidatedFromInstance[VL]])=
//        //get HList of prop values getter functions (of type EntityInstance => Validated[V])
//        (cons.map(GetValidatedFromInstance)
//        //fold to get the ValidatedFromInstance out of the list :  L(ValidatedFromInstance[v]) => ValidatedFromInstance[L(v)]
//         .foldRight(((_:EntityInstance) => Right(HNil)) : ValidatedFromInstance[HNil])(CombineValidatedFromInstance))
//         //map valid result to the generic constructor of target class
//         .andThen(_.right.map(gen.from))
//    }    
//    class Cons9[VVL <: HList](implicit mapped:ops.hlist.Mapped.Aux[instanceGen.Repr,ValidatedFromInstance,VVL])  {
//      def apply[C <: HList](cons:C)(implicit mapper:ops.hlist.Mapper.Aux[GetValidatedFromInstance.type,C,VVL],idMapper:ops.hlist.RightFolder.Aux[VVL,ValidatedFromInstance[HNil],CombineValidatedFromInstance.type,ValidatedFromInstance[instanceGen.Repr]])=
//        //get HList of prop values getter functions (of type EntityInstance => Validated[V])
//        (cons.map(GetValidatedFromInstance)
//        //fold to get the ValidatedFromInstance out of the list :  L(ValidatedFromInstance[v]) => ValidatedFromInstance[L(v)]
//         .foldRight(((_:EntityInstance) => Right(HNil)) : ValidatedFromInstance[HNil])(CombineValidatedFromInstance))
//         //map valid result to the generic constructor of target class
//         .andThen(_.right.map(instanceGen.from))
//    }   
        
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
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




