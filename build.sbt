import Dependencies._

ThisBuild / scalaVersion     := "2.13.10"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.qu4s"
ThisBuild / organizationName := "qu4s"
ThisBuild / scalacOptions := Seq("-unchecked", "-deprecation")

lazy val root = (project in file("."))
  .settings(
    name := "Qu4s",
    libraryDependencies ++= Seq(
      scalaTest % Test
    )
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
