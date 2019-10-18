import sbt.Keys._
import sbt._

object Dependencies {

  object Versions {
    val specs2 = "4.7.1"
  }

  val shapeless = Seq("com.chuusai"   %% "shapeless"   % "2.3.3")
  val cats      = Seq("org.typelevel" %% "cats-core"   % "2.0.0")
  val doobie    = Seq("org.tpolecat"  %% "doobie-core" % "0.8.0-M1")

  val test = Seq(
    "org.specs2" %% "specs2-core" % Versions.specs2 % "test",
    "org.specs2" %% "specs2-mock" % Versions.specs2 % "test"
  )
}
