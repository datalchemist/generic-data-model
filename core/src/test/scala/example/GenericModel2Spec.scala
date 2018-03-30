package example

import org.scalatest._
import play.api.libs.json.{Format, Json,JsObject,Reads,Writes}

class GenericModel2Spec extends FlatSpec with Matchers {
  import GenericModel2._
  import shapeless._
  import syntax._
  
  object HCCPolityType extends scala.Enumeration {
    val Hierarchical=Value
    val Cooperative=Value
    val Convergent=Value
    implicit lazy val fmt = Format(Reads.enumNameReads(HCCPolityType),Writes.enumNameWrites[HCCPolityType.type])
  }
  
  
  
  /** First define general props (basically, a mapping between names and type) */
  object Props extends GeneralProperties {
    val name = prop[String]("name")
    val hccPolityType = prop[HCCPolityType.Value]("hccType")
    val otherPolity = prop[String]("other") //,(s:String) => Some(s))
    
    lazy val text = prop[String]("text")
    lazy val year = prop[Int]("year")
    val name2 = prop[String]("name2")
    
    /** Sub-entity properties (refering to another dependent entity)
     *  Note usage of lazy val to avoid init problem because of circular dependency with corresponding Entity def (that refers to Props)
     *  Of course, sub-entity could not be circular, otherwise it is a simple ref
     *  Note the usage of a super-type of the Entity object (ex: DateEntity.type <: DateEntity) => this is required for TypedDefinition auto-derivation
     *  */
    lazy val date = subEntity[DateEntity](DateEntity,"date")
    lazy val appelation = subEntity[AppelationEntity](AppelationEntity,"apellation")
  }
  
  /**
   *  and Generic Entities definition, i.e. a grouping of general properties 
   */
  trait PolityEntity extends Entity {
    val name=prop(Props.name)
    val polityType=prop(Props.hccPolityType)
    val other = prop(Props.otherPolity)
    val appelation = subEntity(Props.appelation)
  }
  object PolityEntity extends PolityEntity
  trait AppelationEntity extends Entity {
    lazy val text=prop(Props.text)
    lazy val date=subEntity(Props.date)
  }
  object AppelationEntity extends AppelationEntity
  trait DateEntity extends Entity {
    lazy val year=prop(Props.year)
  }
  object DateEntity extends DateEntity
  
  /** Then, we derive Typed-model with binding to case classes*/
  case class Date(year:Int)
  case class Appelation(text:String,date:Option[Date])
  case class Polity(name:String,hccType:HCCPolityType.Value,appelation:Seq[Appelation])
  
  object TypedDateEntity extends DateEntity with TypedEntity {
    type TypedInstance = Date
    val instanceGen = Generic[Date]
    
    val consProps = one(year) :: HNil
    val constructor = makeCons(consProps)
    
  }
  object TypedAppelationEntity extends AppelationEntity with TypedEntity {
    type TypedInstance = Appelation
    val instanceGen = Generic[Appelation]
    
    val consProps = one(text) :: optional[TypedDateEntity.type](date,TypedDateEntity) :: HNil
    val constructor = makeCons(consProps)
    
  }
  object TypedPolityEntity extends PolityEntity with TypedEntity {
    type TypedInstance = Polity
    val instanceGen = Generic[Polity]
    
    val consProps = one(name) :: one(polityType) :: multiple[TypedAppelationEntity.type](appelation,TypedAppelationEntity) :: HNil
    val constructor = makeCons(consProps)
    
  }
  
    val model = new Model(Map.empty)

//    import model._
    val p1 = ("toto")
    val p2 = ("tata")
    PolityEntity(p1) set (PolityEntity.appelation --> AppelationEntity.text) := "ddd"
    PolityEntity(p1) set (PolityEntity.appelation -- 0 --> AppelationEntity.text) := "ddd"
    val commands=List(
      PolityEntity(p1) create,
      PolityEntity(p1) set PolityEntity.name := "Polity name",
      PolityEntity(p2) create,
      PolityEntity(p1) set PolityEntity.polityType := HCCPolityType.Hierarchical,
      PolityEntity(p2) set PolityEntity.other := "toto",
      PolityEntity(p1) set (PolityEntity.appelation --> AppelationEntity.text) := "Nom1",
      PolityEntity(p1) set (PolityEntity.appelation -- 1 --> AppelationEntity.text) := "Nom2",
      PolityEntity(p1) set (PolityEntity.appelation -- 1 --> AppelationEntity.date -- 0 --> DateEntity.year) := 2018
    )
    val finalModel = commands.foldLeft(model)((m,c)=>m(c).right.get)
  "The GenericModel object" should "add and retrieve simple properties to a model" in {
//    Polity.setPropertyCommand(p1,Polity.name,"Polity 1",prov)
//    Polity.name := "Polity name"
    
//    val model2 = model apply (Polity(p1) set Polity.name := "Polity name")
//    val model3 = model2.right.get apply (Polity(p1) set Polity.polityType := HCCPolityType.Hierarchical)
//    val model4 = (model3.right.get apply (Polity(p2) set Polity.other := "toto")).right.get
    
    import finalModel._
    val names :Seq[String]= entity(PolityEntity,p1).getProp(PolityEntity.name)
    entity(PolityEntity,p1).getProp(PolityEntity.name) should be (Seq("Polity name"))
    entity(PolityEntity,p1).getProp(PolityEntity.polityType) should be (Seq(HCCPolityType.Hierarchical))
    entity(PolityEntity,p2).getRef(PolityEntity,PolityEntity.other).map(_.id) should be (Seq(p1))
    entity(PolityEntity,p2).getProp(PolityEntity.polityType) should be (Seq())
    entity(PolityEntity,p1).getProp(PolityEntity.appelation --> AppelationEntity.text) should be (Seq("Nom1"))
    entity(PolityEntity,p1).getProp(PolityEntity.appelation -- 0 --> AppelationEntity.text) should be (Seq("Nom1"))
    entity(PolityEntity,p1).getProp(PolityEntity.appelation -- 1 --> AppelationEntity.text) should be (Seq("Nom2"))
    
  }
  "The GenericModel object" should "enable TypedModel" in {
    import finalModel._
    val typedModel = new TypedModel(finalModel) {
      val typedEntitiesMap:Map[Entity, TypedEntity] = Map(PolityEntity -> TypedPolityEntity)
    }
    val polities : Option[Map[String, Either[String, Polity]]]= typedModel.typedInstances(TypedPolityEntity)
    polities.flatMap(_.get(p1)) should be(Some(Right(Polity("Polity name",HCCPolityType.Hierarchical,Seq(Appelation("Nom1",None),Appelation("Nom2",Some(Date(2018))))))))
    polities.flatMap(_.get(p2)) should be(Some(Left(UniquePropertyNotDefined(TypedPolityEntity.one(TypedPolityEntity.polityType)))))
  }
}
