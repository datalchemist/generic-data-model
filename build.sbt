import Dependencies._

organization in ThisBuild := "org.euratlas"

version in ThisBuild := "0.1-SNAPSHOT"

val scalaVersionClass = "2.12"
val scalaVersionNum = "2.12.4"
val playJsonVersion = "2.6.8"

scalaVersion in ThisBuild := scalaVersionNum

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)
val macwire = "com.softwaremill.macwire" %% "macros" % "2.3.0" % "provided"
val playJsonJvm = "com.typesafe.play" %% "play-json" % playJsonVersion
val playJsonDerivedCodecsJvm = "org.julienrf" %% "play-json-derived-codecs" % "4.0.0"

val playGeojson = "au.id.jazzy" %% "play-geojson" % "1.5.0"


lazy val `generic-model` = (project in file("generic-model")).
  settings(
    EclipseKeys.eclipseOutput := Some("eclipse_target"),
    name := "generic-data-model",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.3",
    libraryDependencies += playJsonJvm,
    libraryDependencies += playJsonDerivedCodecsJvm
  )
