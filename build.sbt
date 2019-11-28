lazy val libs = Dependencies

addCommandAlias("recompile", ";clean;update;compile")
addCommandAlias("build", ";compile;Test/compile")
addCommandAlias("rebuild", ";clean;compile;Test/compile")
addCommandAlias("rebuild-update", ";clean;update;compile;Test/compile")
addCommandAlias("ci", ";scalafmtCheck;rebuild-update;test")
addCommandAlias("ci-quick", ";scalafmtCheck;build;test")
addCommandAlias("doLocal", ";clean;update;compile;publishLocal")

addCommandAlias("cleanPublishSigned", ";recompile;publishSigned")
addCommandAlias("do212Release", s";++${Compiler.scala2_12};cleanPublishSigned;sonatypeBundleRelease")
addCommandAlias("do213Release", s";++${Compiler.scala2_13};cleanPublishSigned;sonatypeBundleRelease")
addCommandAlias("doRelease", ";do212Release;do213Release")

lazy val root = (project in file("."))
  .settings(Publish.sonatypeSettings)
  .settings(Compiler.commonSettings)
  .settings(
    name := "prepy",
    libraryDependencies ++= libs.specs2 ++ Seq(libs.shapeless, libs.cats, libs.doobie)
  )
  .dependsOn(operators, formatters)

lazy val operators = (project in file("operators"))
  .settings(Compiler.commonSettings)
  .settings(
    libraryDependencies ++= libs.specs2 ++ Seq(libs.shapeless, libs.cats, libs.spire, libs.macros)
  )
  .dependsOn(formatters)

lazy val formatters = (project in file("formatters"))
  .settings(Compiler.commonSettings)
  .settings(
    libraryDependencies ++= libs.specs2
  )
