
import scoverage.ScoverageKeys

name := "apibuilder-generator"

organization := "io.apibuilder.generator"

val scalaVer = "2.12.10"

scalaVersion in ThisBuild := scalaVer

lazy val generated = project
  .in(file("generated"))
  .enablePlugins(PlayScala)
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      ws,
      "org.scalacheck" %% "scalacheck" % "1.14.3" % Test
    )
  )

// TODO: lib will eventually be published as a jar if it turns out
// that we need it. For now it is here mostly for reference - hoping
// we end up not needing it.
lazy val lib = project
  .in(file("lib"))
  .dependsOn(generated % "compile; test->test")
  .settings(commonSettings: _*)

lazy val generator = project
  .in(file("generator"))
  .dependsOn(scalaGenerator, rubyGenerator, javaGenerator, goGenerator, androidGenerator, kotlinGenerator, javaAwsLambdaPojos, postmanGenerator, csvGenerator)
  .aggregate(scalaGenerator, rubyGenerator, javaGenerator, goGenerator, androidGenerator, kotlinGenerator, javaAwsLambdaPojos, postmanGenerator, csvGenerator)
  .enablePlugins(PlayScala)
  .settings(commonSettings: _*)
  .settings(
    routesImport += "io.apibuilder.generator.v0.Bindables.Core._",
    routesImport += "io.apibuilder.generator.v0.Bindables.Models._",
    routesGenerator := InjectedRoutesGenerator,
    libraryDependencies ++= Seq(
      ws,
      "org.scalatestplus.play" %% "scalatestplus-play" % "4.0.2" % "test"
    )
  )

lazy val javaAwsLambdaPojos = project
  .in(file("java-aws-lambda-pojos"))
  .dependsOn(lib, lib % "test->test")
  .settings(commonSettings: _*)
  .settings(
    Seq(ScoverageKeys.coverageMinimum := 69.5),
    libraryDependencies ++= Seq(
      "com.amazonaws" % "aws-java-sdk-dynamodb" % "1.11.461",
      "me.geso" % "tinyvalidator" % "0.9.1",
      "org.projectlombok" % "lombok" % "1.18.4"
    )
  )


lazy val scalaGenerator = project
  .in(file("scala-generator"))
  .dependsOn(lib, lib % "test->test")
  .settings(commonSettings: _*)
  .settings(
    Seq(ScoverageKeys.coverageMinimum := 84.0),
    scalacOptions += "-Ypartial-unification",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "1.5.0",
      "org.scalameta" %% "scalafmt-core" % "2.3.2"
    )
  )

lazy val rubyGenerator = project
  .in(file("ruby-generator"))
  .dependsOn(lib, lib % "test->test")
  .settings(commonSettings: _*)
  .settings(Seq(ScoverageKeys.coverageMinimum := 86.5))

lazy val javaGenerator = project
  .in(file("java-generator"))
  .dependsOn(lib, lib % "test->test")
  .settings(commonSettings: _*)
  .settings(Seq(ScoverageKeys.coverageMinimum := 66.98))

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

val kotlinLangVersion = "1.3.61"
val mockitoVersion = "3.2.0"
val scalatestVersion = "3.0.8"

lazy val kotlinGenerator = project
  .in(file("kotlin-generator"))
  .dependsOn(lib, lib % "test->test")
  .settings(
    fork in Test := true,
    baseDirectory in Test := file("."),
    libraryDependencies ++= Seq(
      "com.fasterxml.jackson.module" % "jackson-module-kotlin" % "2.9.9",
      "com.fasterxml.jackson.core" % "jackson-annotations" % "2.9.9",
      "com.fasterxml.jackson.core" % "jackson-databind" % "2.9.9",
      "com.fasterxml.jackson.datatype" % "jackson-datatype-joda" % "2.9.9",
      "org.threeten" % "threetenbp" % "1.3.8",
      "com.squareup" % "kotlinpoet" % "1.3.0",
      "com.squareup.retrofit2" % "retrofit" % "2.5.0",
      "com.jakewharton.retrofit" % "retrofit2-rxjava2-adapter" % "1.0.0",
      "org.jetbrains.kotlin" % "kotlin-stdlib" % kotlinLangVersion % "test",
      "org.jetbrains.kotlin" % "kotlin-stdlib-jdk8" % kotlinLangVersion % "test",
      "org.jetbrains.kotlin" % "kotlin-reflect" % kotlinLangVersion % "test",
      "org.jetbrains.kotlin" % "kotlin-compiler" % kotlinLangVersion % "test",
      "io.github.sullis" % "kotlin-compiler-util" % "0.0.2" % "test",
      "org.scalatest" %% "scalatest" % scalatestVersion % "test",
      "org.mockito" % "mockito-core" % mockitoVersion % "test"
    )
  )
  .settings(Seq(ScoverageKeys.coverageMinimum := 92.49, ScoverageKeys.coverageFailOnMinimum := true))

lazy val csvGenerator = project
  .in(file("csv-generator"))
  .dependsOn(lib, lib % "test->test")
  .settings(
    fork in Test := true,
    baseDirectory in Test := file("."),
    libraryDependencies ++= Seq(
      "org.apache.commons" % "commons-csv" % "1.7"
    )
  )
  .settings(Seq(ScoverageKeys.coverageMinimum := 77.50, ScoverageKeys.coverageFailOnMinimum := true))

lazy val postmanGenerator = project
  .in(file("postman-generator"))
  .dependsOn(lib, lib % "test->test")
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "ammonite-ops" % "1.6.3",
      "org.scalactic" %% "scalactic" % "3.0.5"
    )
  )

lazy val commonSettings: Seq[Setting[_]] = Seq(
  name ~= ("apibuilder-generator-" + _),
  organization := "io.apibuilder",
  ScoverageKeys.coverageFailOnMinimum := true,
  libraryDependencies ++= Seq(
    "org.atteo" % "evo-inflector" % "1.2.2",
    "org.scalatest" %% "scalatest" % scalatestVersion % "test",
    "org.mockito" % "mockito-core" % mockitoVersion % "test",
    "com.github.javaparser" % "javaparser-core" % "3.15.6" % "test",
    "org.scalameta" %% "scalameta" % "4.3.0" % "test",
    "com.squareup" % "javapoet" % "1.11.1",
    "com.squareup.retrofit2" % "retrofit" % "2.5.0",
    "io.reactivex.rxjava2" % "rxjava" % "2.2.4"
  ),
  libraryDependencies += guice,
  scalacOptions ++= Seq("-feature", "-Ycache-plugin-class-loader:last-modified", "-Ycache-macro-class-loader:last-modified"),
  sources in(Compile, doc) := Seq.empty,
  publishArtifact in(Compile, packageDoc) := false
)
version := "0.8.57"
