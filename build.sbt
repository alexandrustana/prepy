lazy val libs = Dependencies

addCommandAlias("recompile", ";clean;update;compile")
addCommandAlias("build", ";compile;Test/compile")
addCommandAlias("rebuild", ";clean;compile;Test/compile")
addCommandAlias("rebuild-update", ";clean;update;compile;Test/compile")
addCommandAlias("ci", ";scalafmtCheck;rebuild-update;test")
addCommandAlias("ci-quick", ";scalafmtCheck;build;test")
addCommandAlias("doLocal", ";clean;update;compile;publishLocal")

addCommandAlias("cleanPublishSigned", ";recompile;publishSigned")
addCommandAlias("do213Release", s";++${Compiler.scala2_13};cleanPublishSigned;sonatypeBundleRelease")
addCommandAlias("doRelease", ";do212Release")

lazy val root = (project in file("."))
  .settings(Publish.sonatypeSettings)
  .settings(Compiler.commonSettings)
  .settings(
    name := "prepy",
    libraryDependencies ++= libs.specs2 ++ Seq(libs.shapeless)
  )
  .dependsOn(ast, formatters)

lazy val ast = (project in file("ast"))
  .settings(Compiler.commonSettings)
  .settings(
    libraryDependencies ++= libs.specs2 ++ Seq(libs.shapeless, libs.cats, libs.macros, libs.doobie)
  )
  .dependsOn(formatters)

lazy val formatters = (project in file("formatters"))
  .settings(Compiler.commonSettings)
  .settings(
    libraryDependencies ++= libs.specs2
  )
