ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

scalacOptions += "-Ymacro-annotations"

libraryDependencies += "org.scalafx" %% "scalafx" % "18.0.1-R28"

lazy val root = (project in file("."))
  .settings(
    name := "TowardsObjects"
  )
