import play.PlayImport.PlayKeys._

name := "apidoc-generator"

organization := "io.apibuilder.generator"

val scalaVer = "2.11.11"

scalaVersion in ThisBuild := scalaVer

lazy val generated = project
  .in(file("generated"))
  .enablePlugins(PlayScala)
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      ws
    )
  )

// TODO: lib will eventually be published as a jar if it turns out
// that we need it. For now it is here mostly for reference - hoping
// we end up not needing it.
lazy val lib = project
  .in(file("lib"))
  .dependsOn(generated)
  .settings(commonSettings: _*)

lazy val generator = project
  .in(file("generator"))
  .dependsOn(scalaGenerator, rubyGenerator, javaGenerator, goGenerator, androidGenerator)
  .aggregate(scalaGenerator, rubyGenerator, javaGenerator, goGenerator, androidGenerator)
  .enablePlugins(PlayScala)
  .settings(commonSettings: _*)
  .settings(
    routesImport += "io.apibuilder.generator.v0.Bindables._",
    routesGenerator := InjectedRoutesGenerator,
    libraryDependencies ++= Seq(
      ws,
      "org.scalatestplus" %% "play" % "1.4.0" % "test"
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

lazy val javaGenerator = project
  .in(file("java-generator"))
  .dependsOn(lib, lib % "test->test")
  .settings(commonSettings: _*)

lazy val goGenerator = project
  .in(file("go-generator"))
  .dependsOn(lib, lib % "test->test")
  .settings(commonSettings: _*)

lazy val androidGenerator = project
  .in(file("android-generator"))
  .dependsOn(lib, lib % "test->test")
  .settings(
    commonSettings: _*
  )

lazy val commonSettings: Seq[Setting[_]] = Seq(
  name <<= name("apidoc-" + _),
  organization := "io.apibuilder",
  libraryDependencies ++= Seq(
    "org.atteo" % "evo-inflector" % "1.2.2",
    "org.scalatest" %% "scalatest" % "2.2.6" % "test",
    "org.mockito" % "mockito-all" % "1.10.19" % "test",
    "com.github.javaparser" % "javaparser-core" % "3.0.1" % "test",
    "org.scala-lang" % "scala-compiler" % scalaVer % "test",
    "com.squareup" % "javapoet" % "1.8.0",
    "com.squareup.retrofit2" % "retrofit" % "2.2.0"
  ),
  scalacOptions += "-feature",
  sources in (Compile,doc) := Seq.empty,
  publishArtifact in (Compile, packageDoc) := false
)
version := "0.5.1"
