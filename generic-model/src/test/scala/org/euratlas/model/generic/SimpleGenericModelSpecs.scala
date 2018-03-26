package org.euratlas.model.generic

import org.scalatest._

import play.api.libs.json.{JsValue,JsPath,Format, Json,JsObject,Reads,Writes}

object SimpleGenericModelSpecs extends FlatSpec with Matchers {
  class SimpleGenericModelTest extends SimpleGenericModel {
    type ModelAssertionProvenance=String
    implicit val provFmt: Format[String] = JsPath.format[String]
    
    trait PolityClass extends EntityClass{val name= "Polity"}
    object PolityClass extends PolityClass
    trait AppelationClass extends EntityClass{val name= "Appelation"}
    object AppelationClass extends AppelationClass
    trait DateClass extends EntityClass{val name= "Date"}
    object DateClass extends DateClass
    
    /** First define general props (basically, a mapping between names and type) */
    object BaseProps extends GeneralProperties {
      val namespace=""
      
      val name = prop[String]("name")
      
      val effectiveness = prop[Int]("effectiveness")
      val regularity = prop[Int]("regularity")
      
      val uncertainty = prop[Int]("uncertainty")
      
      val sourcePolity = ref[PolityClass,String](PolityClass,"sourcePolity",Some(_))
      val targetPolity = ref[PolityClass,String](PolityClass,"targetPolity",Some(_))
      
      lazy val text = prop[String]("text")
      lazy val year = prop[Int]("year")
      
      lazy val date = subEntity[DateClass](DateClass,"date")
      lazy val appelation = subEntity[AppelationClass](AppelationClass,"apellation")
    }
  }
  
  case class Date(year:Int)
  case class Appelation(text:String,date:Option[Date])
  case class Polity(name:String,appelation:collection.immutable.Seq[Appelation])
  case class PolityRelation(source:String,date:Seq[Date])
  
  class SimpleTypedModelTest extends SimpleGenericModelTest with TypedModel {
    import shapeless._
    object TypedDateClass extends DateClass with TypedEntity {
      type TypedInstance = Date
      val instanceGen = Generic[Date]
      val consProps = one(BaseProps.year) :: HNil
      val constructor = makeCons(consProps)
      val toGenericInstance = makeUnCons(consProps)
    }
    object TypedAppelationClass extends AppelationClass with TypedEntity {
      type TypedInstance = Appelation
      val instanceGen = Generic[Appelation]
      val consProps = one(BaseProps.text) :: optional[TypedDateClass.type](BaseProps.date,TypedDateClass) :: HNil
      val constructor = makeCons(consProps)
      val toGenericInstance = makeUnCons(consProps)
    }
    object TypedPolityClass extends PolityClass with TypedEntity {
      import BaseProps._
      type TypedInstance = Polity
      val instanceGen = Generic[Polity]
      val labelledInstanceGen = LabelledGeneric[Polity]
      
      val consProps = one(BaseProps.name) :: multiple[TypedAppelationClass.type](BaseProps.appelation,TypedAppelationClass) :: HNil
      val constructor = makeCons(consProps)
      val toGenericInstance = makeUnCons(consProps)
    }
  }
  
  "SimpleGenericModel object" should "enable ModelDefinition with unique classes & properties" in (pending)
  it should "create generic instance from generic property events" in (pending)
  it should "enable TypedModel definition with bindings to generic classes & properties" in pending
  it should "convert generic instance to typed instance and back" in (pending)
  
  
}