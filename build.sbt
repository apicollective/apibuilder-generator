import play.PlayImport.PlayKeys._

name := "apidoc-generator"

organization := "com.bryzek.apidoc.generator"

scalaVersion in ThisBuild := "2.11.7"

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
  .dependsOn(scalaGenerator, rubyGenerator, javaGenerator, androidGenerator)
  .aggregate(scalaGenerator, rubyGenerator, javaGenerator, androidGenerator)
  .enablePlugins(PlayScala)
  .settings(
    routesImport += "com.bryzek.apidoc.generator.v0.Bindables._",
    routesGenerator := InjectedRoutesGenerator,
    libraryDependencies ++= Seq(
      ws,
      "org.scalatestplus" %% "play" % "1.4.0-M3" % "test",
      "org.eclipse.jdt" % "org.eclipse.jdt.core" % "3.10.0"
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

lazy val androidGenerator = project
  .in(file("android-generator"))
  .dependsOn(lib, lib % "test->test")
  .settings(
    commonSettings: _*
  )

lazy val commonSettings: Seq[Setting[_]] = Seq(
  name <<= name("apidoc-" + _),
  organization := "com.bryzek.apidoc",
  libraryDependencies ++= Seq(
    "org.atteo" % "evo-inflector" % "1.2.1",
    "org.scalatest" %% "scalatest" % "2.2.0" % "test",
    "org.mockito" % "mockito-all" % "1.9.5" % "test",
    "com.squareup" % "javapoet" % "1.3.0",
    "com.squareup.retrofit" % "retrofit" % "2.0.0-beta2"
  ),
  scalacOptions += "-feature"
)
