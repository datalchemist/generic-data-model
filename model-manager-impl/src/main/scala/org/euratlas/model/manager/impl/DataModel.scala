package org.euratlas.model.manager.impl

import org.euratlas.model.generic.{TypedModel,TypedModel2,SimpleGenericModel}

import play.api.libs.json.{JsValue,JsPath,Format, Json,JsObject,Reads,Writes}

object HCCPolityType extends scala.Enumeration {
  val Hierarchical=Value
  val Cooperative=Value
  val Convergent=Value
  implicit lazy val fmt = Format(Reads.enumNameReads(HCCPolityType),Writes.enumNameWrites[HCCPolityType.type])
}
case class Polity(name:String,hccType:HCCPolityType.Value,uncertainty:Option[Int])
case class PolityRelation(source:String,target:String,regularity:Int,effectiveness:Int,uncertainty:Option[Int])
object HccDataModel extends TypedModel {
  type ModelAssertionProvenance=String
  implicit val provFmt: Format[String] = JsPath.format[String]
  
  
  
  
  /** First define general props (basically, a mapping between names and type) */
  object BaseProps extends GeneralProperties {
    val namespace=""
    
    val name = prop[String]("name")
    val hccPolityType = prop[HCCPolityType.Value]("hccType")
    
    
    val effectiveness = prop[Int]("effectiveness")
    val regularity = prop[Int]("regularity")
    
    val uncertainty = prop[Int]("uncertainty")
    
    val sourcePolity = ref[PolityClass,String](PolityClass,"sourcePolity",Some(_))
    val targetPolity = ref[PolityClass,String](PolityClass,"targetPolity",Some(_))
  }
  
  trait PolityClass extends EntityClass {val name="Polity"}
  object PolityClass extends PolityClass
  
  trait PolityRelationClass extends EntityClass {val name="PolityRelation"}
  object PolityRelationClass extends PolityRelationClass
  
  object TypedPolityClass extends PolityClass with TypedEntity {
    import BaseProps._
    import shapeless._
    type TypedInstance = Polity
    val instanceGen = Generic[Polity]
    val labelledInstanceGen = LabelledGeneric[Polity]
    
    val consProps = one(BaseProps.name) :: one(hccPolityType) :: optional(uncertainty) :: HNil
    val constructor = makeCons(consProps)
    val toGenericInstance = makeUnCons(consProps)
    
    
    import record._
import syntax.singleton._
    val p1=Polity("",HCCPolityType.Hierarchical,None)
    
    trait PPProp[A] {
      type TT <: Witness.Lt[Symbol]
      val n:TT
    }
    object PPProp {
      type Aux[A,T <: Witness.Lt[Symbol]] = PPProp[A] {type TT = T}
    }
    def ppp[A] = new { def mk[T <: Witness.Lt[Symbol]](w:T) = new PPProp[A]{type TT = T;val n=w} }
  class PProp[A,T<:Witness.Lt[Symbol]](val n:T){
      
    }
  class SPProp[A,T<:Symbol](val n:Witness.Aux[T]){
      
    }
    def pp[A] = new { def mk[T <: Witness.Lt[Symbol]](w:T) = new PProp[A,T](w)}
    def sp[A] = new { def mk[T<:Symbol](w:Witness.Aux[T]) = new SPProp[A,T](w)}
    def pppp[T<:Symbol](w:Witness.Aux[T]) = new { def apply[A] = new SPProp[A,T](w)}
    def TProp[A](n:Witness.Lt[Symbol])(implicit fmt:Format[A],sel:ops.record.Selector.Aux[labelledInstanceGen.Repr,n.T,A])=1
    def TProp2[A,S<:Symbol](polity:Polity,p:SPProp[A,S])(implicit sel:ops.record.Selector.Aux[labelledInstanceGen.Repr,S,A])=
      (labelledInstanceGen.to(polity).get(p.n))
      
    def TProp3[A,S<:Symbol](polity:Polity,p:SPProp[A,S],v:A)(implicit sel:ops.record.Updater[labelledInstanceGen.Repr,S])=
      (labelledInstanceGen.to(polity).updated(p.n, v))
    TProp[String](('name))
    val n = Witness('name)
  val nn=pp[String].mk(Witness('name))
  val nnn = ppp[String].mk(Witness('name))
  val snnn = pppp(('name))[String]
  import labelled.FieldType
  type TT = (n.T FieldType String)
    implicitly[ops.record.Selector.Aux[labelledInstanceGen.Repr,snnn.n.T,String]]
    labelledInstanceGen.to(p1)
    val cc = TProp2(p1,snnn)
//    def dd[A](polity:Polity)(p:TProp[A])(implicit sel:ops.record.Selector.Aux[labelledInstanceGen.Repr,A with labelled.KeyTag[p.name.T,A],A])=
//      labelledInstanceGen.to(polity)(p.name.narrow)
//    val nameProp=TProp[String]('name)
//    implicitly[ops.record.Selector[labelledInstanceGen.Repr,String with labelled.KeyTag[nameProp.name.T,String]]]
//    dd[String](p1)(nameProp)
    
  }
    
//    val gen = LabelledGeneric[Polity]
//    val geni = gen.to(p1)
//    def dd[T](s:Witness)(implicit sel:ops.record.Selector[gen.Repr,s.T])=
//      geni(s) 
   
//  object TypedPolityRelationClass extends PolityRelationClass with TypedEntity {
//    import BaseProps._
//    import shapeless._
//    type TypedInstance = PolityRelation
//    val instanceGen = Generic[PolityRelation]
//    val labelledInstanceGen = LabelledGeneric[PolityRelation]
//    
//    val consProps = one(sourcePolity) :: one(targetPolity) :: one(regularity) :: one(effectiveness) :: optional(uncertainty) :: HNil
//    val constructor = makeCons(consProps)
//    val toGenericInstance = makeUnCons(consProps)
//    
//  }
  
}


object HccDataModel2 extends TypedModel2 {
  type ModelAssertionProvenance=String
  implicit val provFmt: Format[String] = JsPath.format[String]
  
  
  
  
  /** First define general props (basically, a mapping between names and type) */
  object BaseProps extends GeneralProperties {
    val namespace=""
    
    val name = prop('name)[String]
    val hccPolityType = prop('hccType)[HCCPolityType.Value]
    
    
    val effectiveness = prop('effectiveness)[Int]
    val regularity = prop('regularity)[Int]
    
    val uncertainty = prop('uncertainty)[Int]
    
    val sourcePolity = ref('sourcePolity)[PolityClass,String](PolityClass,Some(_))
    val targetPolity = ref('targetPolity)[PolityClass,String](PolityClass,Some(_))
  }
  
  trait PolityClass extends EntityClass {val name="Polity"}
  object PolityClass extends PolityClass
  
  trait PolityRelationClass extends EntityClass {val name="PolityRelation"}
  object PolityRelationClass extends PolityRelationClass
  
  object TypedPolityClass extends PolityClass with TypedEntity {
    import BaseProps._
    import shapeless._
    type TypedInstance = Polity
    val instanceGen = Generic[Polity]
    val labelledInstanceGen = LabelledGeneric[Polity]
    one(BaseProps.name)
    val consProps = one(BaseProps.name,Witness('name)) :: one(hccPolityType) :: optional(uncertainty) :: HNil
    val constructor = makeCons(consProps)

    
  }
    
//    val gen = LabelledGeneric[Polity]
//    val geni = gen.to(p1)
//    def dd[T](s:Witness)(implicit sel:ops.record.Selector[gen.Repr,s.T])=
//      geni(s) 
   
  object TypedPolityRelationClass extends PolityRelationClass with TypedEntity {
    import BaseProps._
    import shapeless._
    type TypedInstance = PolityRelation
    val instanceGen = Generic[PolityRelation]
    val labelledInstanceGen = LabelledGeneric[PolityRelation]
    
    val consProps = one(sourcePolity,Witness('source)) :: one(targetPolity,Witness('target)) :: one(regularity) :: one(effectiveness) :: optional(uncertainty) :: HNil
    val constructor = makeCons(consProps)
    
    
  }
  
}