ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "Gryphon"
  )

organization := "com.phasmidsoftware"

version := "1.1.3-SNAPSHOT"

scalacOptions += "-deprecation"

// NOTE: if you reinstate these directories, you will need to manage the large crimes file (see code).
//Test / unmanagedSourceDirectories += baseDirectory.value / "src/it/scala"
//Test / unmanagedResourceDirectories += baseDirectory.value / "src/it/resources"

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

lazy val scalaModules = "org.scala-lang.modules"
lazy val scalaTestVersion = "3.2.15"

// NOTE: Issue #44: this library is not currently compatible with version 2.x.x of the parser-combinators library
lazy val scalaParserCombinatorsVersion = "1.1.2"
lazy val nScalaTimeVersion = "2.32.0"
lazy val tsecVersion = "0.4.0"

libraryDependencies ++= Seq(
    "com.phasmidsoftware" %% "tableparser" % "1.1.1",
    "org.typelevel" %% "cats-effect" % "3.4.8",
    "com.phasmidsoftware" %% "flog" % "1.0.8",
    "ch.qos.logback" % "logback-classic" % "1.4.5" % "runtime",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
    "org.scalatest" %% "scalatest" % scalaTestVersion % "test"
)
