import sbt.Keys._
import sbt._

object Dependencies {

  object versions {
    val shapeless = "2.3.3"
    val cats      = "2.0.0-M4"
    val doobie    = "0.8.0-M1"
    val specs2    = "4.7.1"
    val spire     = "0.17.0-M1"
    val macros    = "2.13.1"
  }

  val shapeless: ModuleID = "com.chuusai"    %% "shapeless"    % versions.shapeless
  val cats:      ModuleID = "org.typelevel"  %% "cats-core"    % versions.cats
  val doobie:    ModuleID = "org.tpolecat"   %% "doobie-core"  % versions.doobie
  val spire:     ModuleID = "org.typelevel"  %% "spire"        % versions.spire
  val macros:    ModuleID = "org.scala-lang" % "scala-reflect" % versions.macros

  val specs2: Seq[ModuleID] = Seq(
    "org.specs2" %% "specs2-core" % versions.specs2 % "test",
    "org.specs2" %% "specs2-mock" % versions.specs2 % "test"
  )
}
