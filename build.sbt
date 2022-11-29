ThisBuild / organization := "se.johan1a"
ThisBuild / scalaVersion := "3.2.0"

lazy val root = (project in file(".")).
  settings(
    name := "advent-of-code-2022",
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
  )
