package org.euratlas.model.manager.api

import java.util.UUID
import akka.{Done, NotUsed}
import com.lightbend.lagom.scaladsl.api.{Service, ServiceCall}
import play.api.libs.json.{Reads,Writes,Format, Json, JsObject}

/**
  * The PoliticalNet service interface.
  * <p>
  * This describes everything that Lagom needs to know about how to serve and
  * consume the PoliticalNetworkService.
  */
trait ModelManagerService extends Service {

  /** Read commands */
  def newModel(modelId: String) : ServiceCall[NotUsed, Done]
  
//  def createUser(): ServiceCall[NewUser, User]
//  def login: ServiceCall[UserCredentials, Done]

  override final def descriptor = {
    import Service._
    import com.lightbend.lagom.scaladsl.api.transport.Method
    // @formatter:off
    named("model-manager").withCalls(
      
      pathCall("/api/model/:id/create", newModel _)
    ).withAutoAcl(true)
    // @formatter:on
  }
}

