import Dependencies._

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)
val macwire = "com.softwaremill.macwire" %% "macros" % "2.3.0" % "provided"
val playJsonJvm = "com.typesafe.play" %% "play-json" % "2.6.8"
val playJsonDerivedCodecsJvm = "org.julienrf" %% "play-json-derived-codecs" % "4.0.0"
val slickPg  ="com.github.tminglei" %% "slick-pg" % "0.16.0"
val slickPgJson  ="com.github.tminglei" %% "slick-pg_play-json" % "0.16.0"
val slickPgJts  ="com.github.tminglei" %% "slick-pg_jts" % "0.16.0"

lazy val `model-manager-api` = (project in file("model-manager-api"))
  .settings(
    EclipseKeys.eclipseOutput := Some("eclipse_target"),
    libraryDependencies ++= Seq(
      lagomScaladslApi
    )
  )
  .dependsOn(`generic-model`)

lazy val `model-manager-impl` = (project in file("model-manager-impl"))
  .enablePlugins(LagomScala)
  .settings(
    EclipseKeys.eclipseOutput := Some("eclipse_target"),
    libraryDependencies ++= Seq(
      lagomScaladslPersistenceCassandra,
      lagomScaladslPersistenceJdbc,
      lagomScaladslTestKit,
      slickPg,
      slickPgJson,
      slickPgJts,
      macwire,
      scalaTest
    ),
	lagomCassandraCleanOnStart in ThisBuild := false
  )
  .settings(lagomForkedTestSettings: _*)
  .dependsOn(`model-manager-api`,`generic-model`)

lazy val `generic-model` = (project in file("generic-model")).
  settings(
    EclipseKeys.eclipseOutput := Some("eclipse_target"),
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.4",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "GenericDataModel",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.3",
    libraryDependencies += playJsonJvm,
    libraryDependencies += playJsonDerivedCodecsJvm
  )
