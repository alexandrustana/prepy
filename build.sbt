import Dependencies._

ThisBuild / scalaVersion     := "2.13.0"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val libs = Dependencies

lazy val root = (project in file("."))
  .settings(
    name := "prepy",
    libraryDependencies ++= libs.test ++ libs.shapeless ++ libs.cats ++ libs.doobie
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
