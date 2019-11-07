/**
 * Helps us publish the artifacts to sonatype, which in turn
 * pushes to maven central.
 *
 * https://github.com/xerial/sbt-sonatype/releases
 */
addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "3.8") //https://github.com/xerial/sbt-sonatype/releases

/**
 *
 * Signs all the jars, used in conjunction with sbt-sonatype.
 *
 * https://github.com/sbt/sbt-pgp/releases
 */
addSbtPlugin("com.jsuereth" % "sbt-pgp" % "2.0.0") //https://github.com/sbt/sbt-pgp/releases

/**
 *
 * Helps us integrate with travis-ci and automate the deployment process
 *
 * https://github.com/dwijnand/sbt-travisci
*/
addSbtPlugin("com.dwijnand" % "sbt-travisci" % "1.2.0")

/**
 *
 * Enables generating code coverage reports
 *
 * https://github.com/scoverage/sbt-scoverage
 */
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.6.0")

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