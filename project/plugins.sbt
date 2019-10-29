/**
 * Helps us publish the artifacts to sonatype, which in turn
 * pushes to maven central. Please follow instructions of setting
 * up as described in:
 * http://busymachines.github.io/busymachines-commons/docs/publishing-artifacts.html
 *
 * https://github.com/xerial/sbt-sonatype/releases
 */
addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "3.8") //https://github.com/xerial/sbt-sonatype/releases

/**
 *
 * Signs all the jars, used in conjunction with sbt-sonatype.
 *
 * Do not forget to include this in your global plugins as described in:
 * http://busymachines.github.io/busymachines-commons/docs/publishing-artifacts.html
 *
 * https://github.com/sbt/sbt-pgp/releases
 */
addSbtPlugin("com.jsuereth" % "sbt-pgp" % "2.0.0") //https://github.com/sbt/sbt-pgp/releases

/**
 * build configured in ``project/ReleaseProcess``
 *
 * https://github.com/sbt/sbt-release/releases
 */
addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.11") //https://github.com/sbt/sbt-release/releases

/**
 * The best thing since sliced bread.
 *
 * https://github.com/scalameta/sbt-scalafmt/releases
 *
 */
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.0.7") //https://github.com/scalameta/sbt-scalafmt/releases

/**
 * Refactoring/linting tool for scala.
 *
 * https://github.com/scalacenter/scalafix/releases
 * https://scalacenter.github.io/scalafix/
 *
 * From docs:
 * {{{
 *   // ===> sbt shell
 *
 *   > scalafixEnable                         // Setup scalafix for active session.
 *
 *   > scalafix                               // Run all rules configured in .scalafix.conf
 *
 *   > scalafix RemoveUnusedImports           // Run only RemoveUnusedImports rule
 *
 *   > myProject/scalafix RemoveUnusedImports // Run rule in one project only
 *
 * }}}
 */
addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.9.7") //https://github.com/scalacenter/scalafix/releases