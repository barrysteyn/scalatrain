import sbt._

object Version {
  val scala        = "2.11.1"
  val scalaParsers = "1.0.1"
  val scalaTest    = "2.2.0"
}

object Library {
  val scalaParsers = "org.scala-lang.modules" %% "scala-parser-combinators" % Version.scalaParsers
  val scalaTest    = "org.scalatest"          %% "scalatest"                % Version.scalaTest
}

object Dependencies {

  import Library._

  val scalaTrain = List(
    scalaParsers,
    scalaTest % "test"
  )
}
