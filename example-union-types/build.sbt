import play.PlayImport.PlayKeys._

name := "apidoc-example-union-types"

organization := "com.gilt.apidoc.generator"

scalaVersion in ThisBuild := "2.11.4"

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
    routesImport += "com.gilt.apidoc.example.union.types.v0.Bindables._",
    libraryDependencies ++= Seq(
      ws,
      "org.scalatestplus" %% "play" % "1.2.0" % "test"
    )
  )
