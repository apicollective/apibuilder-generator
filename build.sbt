import play.PlayImport.PlayKeys._

import scoverage.ScoverageKeys

name := "apibuilder-generator"

organization := "io.apibuilder.generator"

val scalaVer = "2.11.12"

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
  .dependsOn(scalaGenerator, rubyGenerator, javaGenerator, goGenerator, androidGenerator, kotlinGenerator)
  .aggregate(scalaGenerator, rubyGenerator, javaGenerator, goGenerator, androidGenerator, kotlinGenerator)
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
  .settings(Seq(ScoverageKeys.coverageMinimum := 77.5))

lazy val rubyGenerator = project
  .in(file("ruby-generator"))
  .dependsOn(lib, lib % "test->test")
  .settings(commonSettings: _*)
  .settings(Seq(ScoverageKeys.coverageMinimum := 89.4))

lazy val javaGenerator = project
  .in(file("java-generator"))
  .dependsOn(lib, lib % "test->test")
  .settings(commonSettings: _*)
  .settings(Seq(ScoverageKeys.coverageMinimum := 44.5))

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
  .settings(Seq(ScoverageKeys.coverageMinimum := 77.8))

lazy val kotlinGenerator = project
  .in(file("kotlin-generator"))
  .dependsOn(lib, lib % "test->test")
  .settings(
    libraryDependencies ++= Seq(
      "com.fasterxml.jackson.module" % "jackson-module-kotlin" % "2.9.0",
      "com.fasterxml.jackson.datatype" % "jackson-datatype-joda" % "2.9.0",
      "com.squareup" % "kotlinpoet" % "0.6.0",
      "com.squareup.retrofit2" % "retrofit" % "2.3.0",
      "org.jetbrains.kotlin" % "kotlin-compiler" % "1.1.60" % "test",
      "org.scalatest" %% "scalatest" % "2.2.6" % "test",
      "org.mockito" % "mockito-all" % "1.10.19" % "test"
    )
  )
  .settings(Seq(ScoverageKeys.coverageMinimum := 93.5))

lazy val commonSettings: Seq[Setting[_]] = Seq(
  name <<= name("apibuilder-generator-" + _),
  organization := "io.apibuilder",
  ScoverageKeys.coverageFailOnMinimum := true,
  libraryDependencies ++= Seq(
    "org.atteo" % "evo-inflector" % "1.2.2",
    "org.scalatest" %% "scalatest" % "2.2.6" % "test",
    "org.mockito" % "mockito-all" % "1.10.19" % "test",
    "com.github.javaparser" % "javaparser-core" % "3.4.2" % "test",
    "org.scala-lang" % "scala-compiler" % scalaVer % "test",
    "com.squareup" % "javapoet" % "1.9.0",
    "com.squareup.retrofit2" % "retrofit" % "2.3.0",
    "io.reactivex.rxjava2" % "rxjava" % "2.1.3"
  ),
  scalacOptions += "-feature",
  sources in (Compile,doc) := Seq.empty,
  publishArtifact in (Compile, packageDoc) := false
)
version := "0.5.77"
