name := "apibuilder-union-of-unions"

organization := "io.apibuilder.generator"

ThisBuild / scalaVersion := "2.13.8"

lazy val generated = project
  .in(file("generated"))
  .enablePlugins(PlayScala)
  .settings(
    libraryDependencies ++= Seq(
      ws
    )
  )

lazy val api = project
  .in(file("api"))
  .dependsOn(generated)
  .aggregate(generated)
  .enablePlugins(PlayScala)
  .settings(
    libraryDependencies ++= Seq(
      ws,
      specs2 % Test,
      "org.scalatestplus.play" %% "scalatestplus-play" % "5.1.0",
      "org.scalatest" %% "scalatest" % "3.2.10" % Test,
    )
  )
