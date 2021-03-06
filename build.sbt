import Dependencies._

organization := "com.kelebra.github"

name := "impatient-scala-katas"

scalaVersion := "2.11.8"

version := "0.1.0-SNAPSHOT"

lazy val `impatient-scala-katas` = (project in file("."))
  .settings(
    libraryDependencies += scalaTest % Test,
    libraryDependencies += scalaCheck % Test,
    libraryDependencies += scalaMock % Test
  )
