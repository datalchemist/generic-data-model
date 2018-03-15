package example

import org.scalatest._
import play.api.libs.json.{Format, Json,JsObject,Reads,Writes}

class GenericModelSpec extends FlatSpec with Matchers {
  import GenericModel._
  
  object HCCPolityType extends scala.Enumeration {
    val Hierarchical=Value
    val Cooperative=Value
    val Convergent=Value
    implicit lazy val fmt = Format(Reads.enumNameReads(HCCPolityType),Writes.enumNameWrites[HCCPolityType.type])
  }
  
  
  
  
  object Props {
    val name = prop[String]("name")
    val hccPolityType = prop[HCCPolityType.Value]("hccType")
    val otherPolity = ref[Polity.type,String]("other",(s:String) => Some(s))
  }
  
  
  
  
//  class Polity(val id:String) {    
//    override def equals(that: Any): Boolean =
//        that match {
//            case that: Polity => this.id == that.id
//            case _ => false
//    }
//    override def hashCode: Int = id.hashCode
//  }
  object Polity extends Entity {
//    type Instance=Polity
    val name=prop(Props.name)
    val polityType=prop(Props.hccPolityType)
    val other = ref(Props.otherPolity)
  }
//  object Polity extends Polity
  
  
  
  "The GenericModel object" should "add and retrieve simple properties to a model" in {
    val model = new Model(Map.empty)

//    import model._
    val p1 = ("toto")
    val p2 = ("tata")
//    Polity.setPropertyCommand(p1,Polity.name,"Polity 1",prov)
//    Polity.name := "Polity name"
    
    val model2 = model apply (Polity(p1) set Polity.name := "Polity name")
    val model3 = model2 apply (Polity(p1) set Polity.polityType := HCCPolityType.Hierarchical)
    val model4 = model3 apply (Polity(p2) set Polity.other := "toto")
    
    import model4._
    entity(Polity,p1).getProp(Polity.name) should be (Some("Polity name"))
    entity(Polity,p1).getProp(Polity.polityType) should be (Some(HCCPolityType.Hierarchical))
    entity(Polity,p2).getProp(Polity.name) should be (None)
    
  }
}
