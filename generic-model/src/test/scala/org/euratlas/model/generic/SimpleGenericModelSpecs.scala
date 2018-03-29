package org.euratlas.model.generic

import org.scalatest._
import org.scalatest.prop.Checkers
import org.scalacheck.{Gen, Arbitrary}
import org.scalacheck.Arbitrary.arbitrary

import play.api.libs.json.{JsNull,JsString,JsValue,JsPath,Format, Json,JsObject,Reads,Writes,JsNumber,JsArray}
import scala.collection.immutable.Seq

class SimpleGenericModelSpecs extends FlatSpec with Matchers with Checkers {
  class SimpleGenericModelTest extends SimpleGenericModel {
    case class Prov(content:String)
    type ModelAssertionProvenance=Prov
    implicit lazy val provFmt: Format[Prov] = Json.format[Prov]
    
    trait PolityClass extends EntityClass{val name= "Polity"}
    object PolityClass extends PolityClass
    trait AppelationClass extends EntityClass{val name= "Appelation"}
    object AppelationClass extends AppelationClass
    trait DateClass extends EntityClass{val name= "Date"}
    object DateClass extends DateClass
    
    EntityClass.define(PolityClass)
    EntityClass.define(AppelationClass)
    EntityClass.define(DateClass)
    
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
    val valueProps=List(BaseProps.name,BaseProps.effectiveness,BaseProps.regularity,BaseProps.uncertainty,BaseProps.text,BaseProps.year)
    val refProps=List(BaseProps.sourcePolity,BaseProps.targetPolity)
    val subProps=List(BaseProps.appelation,BaseProps.appelation)
    val allProps = valueProps ++ refProps ++ subProps
    
    /** Generator for property-based testing */
    implicit val propGen = Arbitrary(Gen.oneOf(allProps))
    implicit val entityGen = Arbitrary(Gen.oneOf(PolityClass,AppelationClass,DateClass))
    def pathGen(lvl:Int=0) : Gen[UntypedPropertyPath]=
      if (lvl > 5)
        Gen.oneOf(
          Gen.oneOf(valueProps).map(UntypedDirect),
          Gen.oneOf(refProps).map(UntypedDirectRef)
        )
      else
        Gen.oneOf(
          Gen.oneOf(valueProps).map(UntypedDirect),
          Gen.oneOf(refProps).map(UntypedDirectRef),
          for {prop <- Gen.oneOf(subProps); subPath <- pathGen(lvl+1)} yield UntypedSubPath(prop,subPath),
          for {prop <- Gen.oneOf(subProps); subPath <- pathGen(lvl+1); idx <- arbitrary[Int]} yield UntypedSubIndexedPath(prop,idx,subPath)
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
    
    def instValueGen(maxlvl:Int=2,usedSubProps:List[Property]=Nil) : Gen[UntypedGenericEntityInstance]=
      for {
        id <- arbitrary[String]
        propsMap <- Gen.mapOf[Property,Seq[JsValue]](for {prop <- Gen.oneOf(valueProps);v<-Gen.resize(3,Gen.listOf(jsValueGen(1)))} yield prop -> v)
        refsMap <- Gen.mapOf[Property,Seq[JsValue]](for {prop <- Gen.oneOf(refProps);v<-Gen.resize(3,Gen.listOf(jsValueGen(1)))} yield prop -> v)
        subsMap <- 
          if (maxlvl >0) Gen.mapOf[Property,Seq[UntypedGenericEntityInstance]](for {prop <- Gen.oneOf(subProps.diff(usedSubProps));v<-Gen.listOfN(1,instValueGen(maxlvl-1,prop :: usedSubProps))} yield prop -> v)
          else Gen.const[Map[Property,Seq[UntypedGenericEntityInstance]]](Map.empty)
      } yield UntypedGenericEntityInstance(id,propsMap,refsMap,subsMap)
    implicit val instanceArbitrary = Arbitrary(instValueGen())
    
    def assertionGen:Gen[ModelAssertion] =
      Gen.oneOf(
        for {
          entity <- arbitrary[EntityClass]
          id <- arbitrary[String]
          prov <- arbitrary[String].map(Prov(_))
          a <- Gen.oneOf(
            EntityDefined(entity,id,prov),
            EntityUndefined(entity,id,prov)
          )
        } yield a,
        for {
          entity <- arbitrary[EntityClass]
          id <- arbitrary[String]
          propPath <- arbitrary[UntypedPropertyPath]
          prov <- arbitrary[String].map(Prov(_))
          a <- Gen.oneOf(
              jsValueGen(1).map(value=>SingleEntityPropertyDefined(entity,id,propPath,value,prov)),
              Gen.const(PropertyUndefined(entity,id,propPath,prov)),
              jsValueGen(1).map(value=>EntityPropertyAdded(entity,id,propPath,value,prov)),
              arbitrary[Int].map(i=>EntityPropertyRemoved(entity,id,propPath,i,prov))
          )
        } yield a
      )
    implicit val modelAssertionArbitrary = Arbitrary(assertionGen)
  }
  
  case class Date(year:Int)
  case class Appelation(text:String,date:Option[Date])
  case class Polity(name:String,appelation:collection.immutable.Seq[Appelation])
  case class PolityRelation(source:String,date:Seq[Date])
  
case class Position(x:Int,y:Int)
object Position {implicit val format=Json.format[Position]}
  
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
  
  "SimpleGenericModel object" should "enable ModelDefinition with unique classes & properties" in {
    val model =new SimpleGenericModelTest
    intercept[model.PropertyAlreadyDefinedException] { 
      new model.GeneralProperties {
        val namespace=""
        val name = prop[String]("name")
      }
      new model.GeneralProperties {
        val namespace=""
        val name = prop[Int]("name")
      }
    }
  }
  it should "create generic instance from generic property events" in (pending)
  it should "enable TypedModel definition with bindings to generic classes & properties" in pending
  it should "convert generic instance to typed instance and back" in (pending)
  
  
  def jsonCodecIdentity[A](implicit reads: Reads[A], owrites: Writes[A], arbA: Arbitrary[A]): Unit =
    check((a: A) => reads.reads(owrites.writes(a)).fold(_ => false, _ == a))
  
  it should "format generic entityClasses and properties paths to/from Json" in {
    val model = new SimpleGenericModelTest
    import model._

    Json.toJson(BaseProps.name) should equal(JsObject(Seq("propertyName" -> JsString(BaseProps.name.name))))
    jsonCodecIdentity[Property]
    
    Json.toJson(PolityClass) should equal(JsObject(Seq("entityClassName" -> JsString(PolityClass.name))))
    jsonCodecIdentity[EntityClass]
  }
  it should "format generic untyped property paths to/from Json" in {
    val model = new SimpleGenericModelTest
    import model._

    jsonCodecIdentity[UntypedPropertyPath]
  }
  it should "format generic instance to/from Json" in {
    val model = new SimpleGenericModelTest
    import model._
    
    jsonCodecIdentity[UntypedGenericEntityInstance]

  }
  it should "format generic assertions to/from Json" in {
    val model = new SimpleGenericModelTest
    import model._
    val graphPosition = BaseProps.prop[Position]("graphPosition")
    val n=Json.toJson("name")
//    val a = SingleEntityPropertyDefined(PolityClass,"entityId",UntypedDirect(BaseProps.name),n,Prov(""))
    val a=EntityUndefined(PolityClass,"",Prov(""))
    implicit val assertionFmt=ModelAssertion.fmt
    println(Json.toJson(a))
    println(Json.fromJson[ModelAssertion](Json.toJson(a)))
    Json.fromJson[ModelAssertion](Json.toJson(a)).get should equal(a)
    jsonCodecIdentity[ModelAssertion]
  }
  
  
}