ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.11"

lazy val deps = Seq(
  "org.scalatest" %% "scalatest" % "3.2.15" % Test
)

lazy val root = (project in file("."))
  .settings(
    name := "simple-expression-interpreter",
    libraryDependencies ++= deps
  )
