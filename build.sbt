import play.PlayImport.PlayKeys._

name := "apidoc-generator"

organization := "com.gilt.apidoc.generator"

scalaVersion in ThisBuild := "2.11.4"

// TODO: lib will eventually be published as a jar if it turns out
// that we need it. For now it is here mostly for reference - hoping
// we end up not needing it.
lazy val lib = project
  .in(file("lib"))
  .settings(
    libraryDependencies ++= Seq(
     "org.atteo" % "evo-inflector" % "1.2.1",
      "org.scalatestplus" %% "play" % "1.2.0" % "test"
    )
  )

lazy val generated = project
  .in(file("generated"))
  .enablePlugins(PlayScala)
  .settings(
    libraryDependencies ++= Seq(
      ws
    )
  )

lazy val generator = project
  .in(file("generator"))
  .dependsOn(generated, lib)
  .aggregate(generated, lib)
  .enablePlugins(PlayScala)
  .settings(
    routesImport += "com.gilt.apidoc.generator.Bindables._",
    libraryDependencies ++= Seq(
      ws,
      "org.scalatestplus" %% "play" % "1.2.0" % "test"
    )
  )
