import Dependencies._

organization in ThisBuild := "org.euratlas"

version in ThisBuild := "0.4.3-SNAPSHOT"

val scalaVersionClass = "2.12"
val scalaVersionNum = "2.12.10"
val playJsonVersion = "2.6.8"

scalaVersion in ThisBuild := scalaVersionNum

publishTo in ThisBuild := Some("datuman-maven-repo" at "http://178.62.90.239:8081/artifactory/datuman-private-repo/")
credentials in ThisBuild  += Credentials(Path.userHome / ".sbt" / ".datuman-credentials")

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

lazy val core = //(project in file("generic-model")).
  crossProject.crossType(CrossType.Pure)
  .settings(
    name := "generic-data-model",
    libraryDependencies +=  "org.scalatest" %%% "scalatest" % "3.0.1" % Test,
    libraryDependencies +=  "org.scalacheck" %%% "scalacheck" % "1.13.4" % Test,
    libraryDependencies += "com.chuusai" %%% "shapeless" % "2.3.3",
    libraryDependencies += "com.typesafe.play" %%% "play-json" % playJsonVersion,
    libraryDependencies += "org.julienrf" %%% "play-json-derived-codecs" % "4.0.0"
  )

val genericModelJVM = 
  core.jvm
  .settings(
    //name := "generic-data-model-jvm",
  )
val genericModelJS = 
  core.js
  .settings(
    //name := "generic-data-model-js",
  )

val `generic-model` =
  project.in(file(".")).aggregate(genericModelJVM, genericModelJS)


