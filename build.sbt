lazy val libs = Dependencies

lazy val root = (project in file("."))
  .settings(Publish.sonatypeSettings)
  .settings(Compiler.commonSettings)
  .settings(
    name := "prepy",
    libraryDependencies ++= libs.specs2 ++ Seq(libs.shapeless, libs.cats, libs.doobie)
  )
