package org.euratlas.model
package generic

import org.scalatest._
import org.scalatest.prop.Checkers
import org.scalacheck.{Gen, Arbitrary}
import org.scalacheck.Arbitrary.arbitrary

import play.api.libs.json.{JsNull,JsString,JsValue,JsPath,Format, Json,JsObject,Reads,Writes,JsNumber,JsArray}
import scala.collection.immutable.Seq

class SimpleGenericModelSpecs extends FlatSpec with Matchers with Checkers {
  import Untyped._
  
//  class SimpleGenericModelTest { //extends SimpleGenericModel {
    case class Prov(content:String)
    type ModelAssertionProvenance=Prov
    implicit lazy val provFmt: Format[Prov] = Json.format[Prov]
    
    trait PolityClass extends EntityClass{val name= "Polity"}
    object PolityClass extends PolityClass
    trait AppelationClass extends EntityClass{val name= "Appelation"}
    object AppelationClass extends AppelationClass
    trait DateClass extends EntityClass{val name= "Date"}
    object DateClass extends DateClass
    
    
    /** First define general props (basically, a mapping between names and type) */
    class BasePropsModel extends PropertyModel with EntityModel {
      val namespace=""
      
      define(PolityClass)
      define(AppelationClass)
      define(DateClass)
      
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
      
      val valueProps=List(name,effectiveness,regularity,uncertainty,text,year)
      val refProps=List(sourcePolity,targetPolity)
      val subProps=List(appelation,appelation)
      val allProps = valueProps ++ refProps ++ subProps
    
    /** Generator for property-based testing */
    implicit val propGen = Arbitrary(Gen.oneOf(allProps))
    implicit val entityGen = Arbitrary(Gen.oneOf(PolityClass,AppelationClass,DateClass))
    def pathGen(lvl:Int=0) : Gen[PropertyPath]=
      if (lvl > 5)
        Gen.oneOf(
          Gen.oneOf(valueProps).map(Direct),
          Gen.oneOf(refProps).map(DirectRef)
        )
      else
        Gen.oneOf(
          Gen.oneOf(valueProps).map(Direct),
          Gen.oneOf(refProps).map(DirectRef),
          for {prop <- Gen.oneOf(subProps); subPath <- pathGen(lvl+1)} yield SubPath(prop,subPath),
          for {prop <- Gen.oneOf(subProps); subPath <- pathGen(lvl+1); idx <- arbitrary[Int]} yield SubIndexedPath(prop,idx,subPath)
        )
    implicit val pathArbitrary = Arbitrary(pathGen())
    
    def jsValueGen(maxlvl:Int=2) : Gen[JsValue]=
      if (maxlvl == 0) {
        Gen.oneOf(Gen.const[JsValue](JsNull),arbitrary[String].map(JsString(_)),arbitrary[Int].map(JsNumber(_)))
      } else {
        Gen.oneOf(Gen.const[JsValue](JsNull),arbitrary[String].map(JsString(_)),arbitrary[Int].map(JsNumber(_)),
            Gen.resize(5,Gen.listOf(jsValueGen(maxlvl-1))).map(JsArray(_)),
            Gen.resize(5,Gen.mapOf(for{k<-arbitrary[String]; v <- jsValueGen(maxlvl-1)} yield k->v)).map(JsObject(_))
        )
      }
    
    def instValueGen(maxlvl:Int=2,usedSubProps:List[Property]=Nil) : Gen[GenericEntityInstance]=
      for {
        id <- arbitrary[String]
        propsMap <- Gen.mapOf[Property,Seq[JsValue]](for {prop <- Gen.oneOf(valueProps);v<-Gen.resize(3,Gen.listOf(jsValueGen(1)))} yield prop -> v)
        refsMap <- Gen.mapOf[Property,Seq[JsValue]](for {prop <- Gen.oneOf(refProps);v<-Gen.resize(3,Gen.listOf(jsValueGen(1)))} yield prop -> v)
        subsMap <- 
          if (maxlvl >0) Gen.mapOf[Property,Seq[GenericEntityInstance]](for {prop <- Gen.oneOf(subProps.diff(usedSubProps));v<-Gen.listOfN(1,instValueGen(maxlvl-1,prop :: usedSubProps))} yield prop -> v)
          else Gen.const[Map[Property,Seq[GenericEntityInstance]]](Map.empty)
      } yield GenericEntityInstance(id,propsMap,refsMap,subsMap)
    implicit val instanceArbitrary = Arbitrary(instValueGen())
    
    def assertionGen:Gen[PropertyPathAssertion] =
//      Gen.oneOf(
//        for {
//          entity <- arbitrary[EntityClass]
//          id <- arbitrary[String]
//          prov <- arbitrary[String].map(Prov(_))
//          a <- Gen.oneOf(
//            EntityDefined(entity,id,prov),
//            EntityUndefined(entity,id,prov)
//          )
//        } yield a,
        for {
//          entity <- arbitrary[EntityClass]
//          id <- arbitrary[String]
          propPath <- arbitrary[PropertyPath]
//          prov <- arbitrary[String].map(Prov(_))
          a <- Gen.oneOf(
              jsValueGen(1).map(value=>PropertyPathAssertion(propPath,Set(value))),
              Gen.const(PropertyPathAssertion(propPath,Unset())),
              jsValueGen(1).map(value=>PropertyPathAssertion(propPath,Add(value))),
              arbitrary[Int].map(i=>PropertyPathAssertion(propPath,Remove(i))),
              arbitrary[Int].flatMap(i=>jsValueGen(1).map(value=>PropertyPathAssertion(propPath,SetAt(i,value))))
          )
        } yield a
//      )
    implicit val modelAssertionArbitrary = Arbitrary(assertionGen)
//  }
    }
  
  case class Date(year:Int)
  case class Appelation(text:String,date:Option[Date])
  case class Polity(name:String,appelation:collection.immutable.Seq[Appelation])
  case class PolityRelation(source:String,date:Seq[Date])
  
case class Position(x:Int,y:Int)
object Position {implicit val format=Json.format[Position]}
  
  class SimpleTypedModelTest extends BasePropsModel { //with TypedModel {
    import shapeless._
    import typed._
//    import TypedEntity._
    
    val nameProp=name
    object TypedDateClass extends DateClass with TypedEntity {
      type TypedInstance = Date
      val instanceGen = Generic[Date]
      val consProps = one(year) :: HNil
      val constructor = makeCons(consProps)
      val toGenericInstance = makeUnCons(consProps)
    }
    object TypedAppelationClass extends AppelationClass with TypedEntity {
      type TypedInstance = Appelation
      val instanceGen = Generic[Appelation]
      val consProps = one(text) :: optional[TypedDateClass.type](date,TypedDateClass) :: HNil
      val constructor = makeCons(consProps)
      val toGenericInstance = makeUnCons(consProps)
    }
    object TypedPolityClass extends PolityClass with TypedEntity {
//      import BaseProps._
      type TypedInstance = Polity
      val instanceGen = Generic[Polity]
      val labelledInstanceGen = LabelledGeneric[Polity]
      
      val consProps = one(nameProp) :: multiple[TypedAppelationClass.type](appelation,TypedAppelationClass) :: HNil
      val constructor = makeCons(consProps)
      val toGenericInstance = makeUnCons(consProps)
    }
  }
  
  "SimpleGenericModel object" should "enable ModelDefinition with unique classes & properties" in {
    val model =new BasePropsModel
    intercept[PropertyAlreadyDefinedException] { 
      new PropertyModel {
        val namespace=""
        val name = prop[String]("name")
        val name2 = prop[Int]("name")
      }
    }
  }
  it should "create generic instance from generic property events" in (pending)
  it should "enable TypedModel definition with bindings to generic classes & properties" in pending
  it should "convert generic instance to typed instance and back" in (pending)
  
  
  def jsonCodecIdentity[A](implicit reads: Reads[A], owrites: Writes[A], arbA: Arbitrary[A]): Unit =
    check((a: A) => reads.reads(owrites.writes(a)).fold(_ => false, _ == a))
  
  it should "format generic entityClasses and properties paths to/from Json" in {
    val model = new BasePropsModel with json
    import model._

    Json.toJson(model.name) should equal(JsObject(Seq("propertyName" -> JsString(model.name.name))))
    jsonCodecIdentity[Property]
    
    Json.toJson(PolityClass) should equal(JsObject(Seq("entityClassName" -> JsString(PolityClass.name))))
    jsonCodecIdentity[EntityClass]
  }
  it should "format generic untyped property paths to/from Json" in {
    val model = new BasePropsModel with json
    import model._
//    import json._

    jsonCodecIdentity[PropertyPath]
  }
  it should "format generic instance to/from Json" in {
    val model = new BasePropsModel with json
    import model._
    jsonCodecIdentity[GenericEntityInstance]

  }
  it should "format generic property-path assertions to/from Json" in {
    val model = new BasePropsModel with json
    import model._
//    val graphPosition = BaseProps.prop[Position]("graphPosition")
//    val n=Json.toJson("name")
////    val a = SingleEntityPropertyDefined(PolityClass,"entityId",UntypedDirect(BaseProps.name),n,Prov(""))
//    val a=EntityUndefined(PolityClass,"",Prov(""))
//    implicit val assertionFmt=ModelAssertion.fmt
//    println(Json.toJson(a))
//    println(Json.fromJson[ModelAssertion](Json.toJson(a)))
//    Json.fromJson[ModelAssertion](Json.toJson(a)).get should equal(a)
    jsonCodecIdentity[PropertyPathAssertion]
  }
  
  
}