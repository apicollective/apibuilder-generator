import play.PlayImport.PlayKeys._

name := "apidoc-example-union-types-discriminator"

organization := "io.apibuilder.generator"

scalaVersion in ThisBuild := "2.11.7"

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
    routesImport += "io.apibuilder.example.union.types.discriminator.v0._",
    libraryDependencies ++= Seq(
      ws,
      specs2 % Test,
      "org.scalatest" %% "scalatest" % "2.2.5" % Test,
      "org.scalatestplus" %% "play" % "1.4.0-M4" % Test
    )
  )
