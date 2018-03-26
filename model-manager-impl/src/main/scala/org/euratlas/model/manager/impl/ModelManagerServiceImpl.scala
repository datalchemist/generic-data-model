package org.euratlas.model.manager.impl

import akka.{NotUsed,Done}

import com.lightbend.lagom.scaladsl.api.ServiceCall
import org.euratlas.model.manager.api.ModelManagerService
import scala.concurrent.Future
import scala.concurrent.ExecutionContext

/**
  * Implementation of the PoliticalNetworkService.
  */
class ModelManagerServiceImpl()(implicit ec:ExecutionContext) extends ModelManagerService {

  override def newModel(modelId: String) = ServiceCall { _ =>
    Future.successful(Done)
  }  
  
}

