ThisBuild / scalaVersion := "3.3.4"

lazy val root = (project in file("."))
  .settings(
    name := "greenhouse-variant25",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % Test
  )
