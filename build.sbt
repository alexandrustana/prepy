ThisBuild / scalaVersion     := "2.13.0"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val libs = Dependencies

lazy val root = (project in file("."))
  .settings(Publish.sonatypeSettings)
  .settings(Compiler.commonSettings)
  .settings(
    name := "prepy",
    libraryDependencies ++= libs.specs2 ++ Seq(libs.shapeless, libs.cats, libs.doobie)
  )
