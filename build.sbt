import Dependencies._

organization in ThisBuild := "org.euratlas"

version in ThisBuild := "0.2-SNAPSHOT"

val scalaVersionClass = "2.12"
val scalaVersionNum = "2.12.4"
val playJsonVersion = "2.6.8"

scalaVersion in ThisBuild := scalaVersionNum

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)


lazy val core = //(project in file("generic-model")).
  crossProject.crossType(CrossType.Pure)
  .settings(
    EclipseKeys.eclipseOutput := Some("eclipse_target"),
    name := "generic-data-model",
    libraryDependencies +=  "org.scalatest" %%% "scalatest" % "3.0.1" % Test,
    libraryDependencies +=  "org.scalacheck" %%% "scalacheck" % "1.13.4" % Test,
    libraryDependencies += "com.chuusai" %%% "shapeless" % "2.3.3",
    libraryDependencies += "com.typesafe.play" %%% "play-json" % playJsonVersion,
    libraryDependencies += "org.julienrf" %%% "play-json-derived-codecs" % "4.0.0"
  )

val coreJVM = 
  core.jvm
  .settings(
    name := "generic-data-model-jvm",
    EclipseKeys.eclipseOutput := Some("eclipse_target"),
  )
val coreJS = 
  core.js
  .settings(
    name := "generic-data-model-js",
    EclipseKeys.eclipseOutput := Some("eclipse_target"),
  )

val `generic-model` =
  project.in(file(".")).aggregate(coreJVM, coreJS)


