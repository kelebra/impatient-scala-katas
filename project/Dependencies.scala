import sbt._

object Dependencies {

  lazy val scalaTest: ModuleID = "org.scalatest" %% "scalatest" % "3.0.1"
  lazy val scalaCheck: ModuleID = "org.scalacheck" %% "scalacheck" % "1.13.4"
  lazy val scalaMock: ModuleID = "org.scalamock" %% "scalamock-scalatest-support" % "3.5.0"
}
