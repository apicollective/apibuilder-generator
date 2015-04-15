import play.PlayImport.PlayKeys._

name := "apidoc-generator"

organization := "com.gilt.apidoc.generator"

scalaVersion in ThisBuild := "2.11.6"

// TODO: lib will eventually be published as a jar if it turns out
// that we need it. For now it is here mostly for reference - hoping
// we end up not needing it.
lazy val lib = project
  .in(file("lib"))
  .dependsOn(generated)
  .settings(commonSettings: _*)

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
  .dependsOn(scalaGenerator, rubyGenerator)
  .aggregate(scalaGenerator, rubyGenerator)
  .enablePlugins(PlayScala)
  .settings(
    routesImport += "com.gilt.apidoc.generator.v0.Bindables._",
    libraryDependencies ++= Seq(
      ws,
      "org.scalatestplus" %% "play" % "1.2.0" % "test"
    )
  )

lazy val scalaGenerator = project
  .in(file("scala-generator"))
  .dependsOn(lib, lib % "test->test")
  .settings(commonSettings: _*)

lazy val rubyGenerator = project
  .in(file("ruby-generator"))
  .dependsOn(lib, lib % "test->test")
  .settings(commonSettings: _*)

lazy val commonSettings: Seq[Setting[_]] = Seq(
  name <<= name("apidoc-" + _),
  organization := "com.gilt.apidoc",
  libraryDependencies ++= Seq(
    "org.atteo" % "evo-inflector" % "1.2.1",
    "org.scalatest" %% "scalatest" % "2.2.0" % "test"
  ),
  scalacOptions += "-feature"
)
