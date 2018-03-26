package org.euratlas.model.manager.impl

import com.lightbend.lagom.scaladsl.api.ServiceLocator
import com.lightbend.lagom.scaladsl.api.ServiceLocator.NoServiceLocator
import com.lightbend.lagom.scaladsl.persistence.cassandra.{CassandraPersistenceComponents,WriteSideCassandraPersistenceComponents}
import com.lightbend.lagom.scaladsl.persistence.slick.ReadSideSlickPersistenceComponents
import com.lightbend.lagom.scaladsl.server._
import com.lightbend.lagom.scaladsl.devmode.LagomDevModeComponents
import play.api.libs.ws.ahc.AhcWSComponents
import play.api.db.HikariCPComponents
import org.euratlas.model.manager.api.ModelManagerService
import com.softwaremill.macwire._
import com.lightbend.lagom.scaladsl.client.ConfigurationServiceLocatorComponents

import com.lightbend.lagom.scaladsl.playjson.{JsonSerializerRegistry, JsonSerializer}

class ModelManagerLoader extends LagomApplicationLoader {

  override def load(context: LagomApplicationContext): LagomApplication =
    new ModelManagerApplication(context) with ConfigurationServiceLocatorComponents {
      //override def serviceLocator: ServiceLocator = NoServiceLocator
    }

  override def loadDevMode(context: LagomApplicationContext): LagomApplication =
    new ModelManagerApplication(context) with LagomDevModeComponents

  override def describeServices = List(
    readDescriptor[ModelManagerService]
  )
}

abstract class ModelManagerApplication(context: LagomApplicationContext)
  extends LagomApplication(context)
    with ReadSideSlickPersistenceComponents
    with WriteSideCassandraPersistenceComponents
    with HikariCPComponents
    with AhcWSComponents {

  // Bind the services that this server provides
  override lazy val lagomServer = LagomServer.forServices(
    bindService[ModelManagerService].to(wire[ModelManagerServiceImpl])
  )

  // Register the JSON serializer registry
  override lazy val jsonSerializerRegistry = ModelManagerSerializerRegistry
  
}
