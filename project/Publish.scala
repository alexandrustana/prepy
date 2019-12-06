import sbt._
import sbt.Keys._
import xerial.sbt.Sonatype.SonatypeKeys._

object Publish {

  def sonatypeSettings: Seq[Setting[_]] = Seq(
    sonatypeProfileName        := Compiler.organizationName,
    publishArtifact in Compile := true,
    publishArtifact in Test    := false,
    publishMavenStyle          := true,
    pomIncludeRepository       := (_ => false),
    publishTo                  := sonatypePublishToBundle.value,
    licenses                   := Seq("APL2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
    scmInfo := Option(
      ScmInfo(
        url("https://github.com/alexandrustana/prepy"),
        "scm:git@github.com:alexandrustana/prepy.git"
      )
    ),
    developers := List(
      Developer(
        id    = "alexandrustana",
        name  = "Alexandru Stana",
        email = "alexandrustana2@gmail.com",
        url   = url("https://github.com/alexandrustana")
      )
    )
  )

  def noPublishSettings: Seq[Setting[_]] = Seq(
    publish              := {},
    publishLocal         := {},
    skip in publishLocal := true,
    skip in publish      := true,
    publishArtifact      := false
  )

}
