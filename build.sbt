name := "apibuilder-generator"

organization := "io.apibuilder.generator"

ThisBuild / scalaVersion := "2.13.14"

ThisBuild / javacOptions ++= Seq("-source", "17", "-target", "17")

lazy val allScalacOptions = Seq(
  "-deprecation",
  "-feature",
  "-Xfatal-warnings",
  "-unchecked",
  "-Xcheckinit",
  "-Xlint:adapted-args",
  "-Ycache-plugin-class-loader:last-modified",
  "-Ycache-macro-class-loader:last-modified",
  "-Ypatmat-exhaust-depth", "100", // Fixes: Exhaustivity analysis reached max recursion depth, not all missing cases are reported.
  "-Wconf:src=generated/.*:silent",
  "-Wconf:src=target/.*:silent", // silence the unused imports errors generated by the Play Routes
)

lazy val resolversSettings = Seq(
  resolvers += "Typesafe repository" at "https://repo.typesafe.com/typesafe/releases/",
  resolvers += "Flow repository" at "https://flow.jfrog.io/flow/libs-release/",
)


lazy val generated = project
  .in(file("generated"))
  .enablePlugins(PlayScala)
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      ws,
      "org.scalacheck" %% "scalacheck" % "1.18.0" % Test
    ),
    scalacOptions ++= allScalacOptions
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
  .dependsOn(elmGenerator, csharpGenerator, scalaGenerator, rubyGenerator, javaGenerator, goGenerator, androidGenerator, kotlinGenerator, graphQLGenerator, javaAwsLambdaPojos, postmanGenerator, csvGenerator)
  .aggregate(elmGenerator, csharpGenerator, scalaGenerator, rubyGenerator, javaGenerator, goGenerator, androidGenerator, kotlinGenerator, graphQLGenerator, javaAwsLambdaPojos, postmanGenerator, csvGenerator)
  .enablePlugins(PlayScala)
  .enablePlugins(JavaAgent)
  .settings(commonSettings: _*)
  .settings(
    javaAgents += "com.datadoghq" % "dd-java-agent" % "1.8.0",
    routesImport += "io.apibuilder.generator.v0.Bindables.Core._",
    routesImport += "io.apibuilder.generator.v0.Bindables.Models._",
    routesGenerator := InjectedRoutesGenerator,
    libraryDependencies ++= Seq(
      ws,
      "org.scalatestplus.play" %% "scalatestplus-play" % "7.0.1" % "test"
    ),
    scalacOptions ++= allScalacOptions,
    Test / javaOptions ++= Seq(
      "--add-exports=java.base/sun.security.x509=ALL-UNNAMED",
      "--add-opens=java.base/sun.security.ssl=ALL-UNNAMED"
    )
  )

lazy val javaAwsLambdaPojos = project
  .in(file("java-aws-lambda-pojos"))
  .dependsOn(lib, lib % "test->test")
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "com.amazonaws" % "aws-java-sdk-dynamodb" % "1.12.731",
      "me.geso" % "tinyvalidator" % "0.9.1",
      "org.projectlombok" % "lombok" % "1.18.32"
    )
  )


lazy val scalaGenerator = project
  .in(file("scala-generator"))
  .dependsOn(lib, lib % "test->test")
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalameta" %% "scalafmt-core" % "3.8.1"
    )
  )

lazy val csharpGenerator = project
  .in(file("csharp-generator"))
  .dependsOn(lib, lib % "test->test")
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.12.0"
    )
  )

lazy val elmGenerator = project
  .in(file("elm-generator"))
  .dependsOn(lib, lib % "test->test")
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.12.0"
    )
  )

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

lazy val graphQLGenerator = project
  .in(file("graphql-generator"))
  .dependsOn(lib, lib % "test->test")
  .settings(commonSettings: _*)
  .settings(resolversSettings)
  .settings(
    libraryDependencies ++= Seq(
      "io.apibuilder" %% "apibuilder-graphql" % "0.0.10",
    ),
  )

val mockitoVersion = "4.11.0"
val scalatestVersion = "3.2.18"
val jacksonVersion = "2.17.1"
val kotlinLangVersion = "1.9.24"

lazy val kotlinGenerator = project
  .in(file("kotlin-generator"))
  .dependsOn(lib, lib % "test->test")
  .settings(
    Test / fork := true,
    Test / baseDirectory := file("."),
    libraryDependencies ++= Seq(
      "com.fasterxml.jackson.module" % "jackson-module-kotlin" % jacksonVersion,
      "com.fasterxml.jackson.core" % "jackson-annotations" % jacksonVersion,
      "com.fasterxml.jackson.core" % "jackson-databind" % jacksonVersion,
      "com.fasterxml.jackson.datatype" % "jackson-datatype-joda" % jacksonVersion,
      "org.threeten" % "threetenbp" % "1.6.9",
      "com.squareup" % "kotlinpoet-jvm" % "1.17.0",
      "com.squareup.retrofit2" % "retrofit" % "2.11.0",
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

lazy val csvGenerator = project
  .in(file("csv-generator"))
  .dependsOn(lib, lib % "test->test")
  .settings(
    Test / fork := true,
    Test / baseDirectory := file("."),
    libraryDependencies ++= Seq(
      "org.apache.commons" % "commons-csv" % "1.11.0"
    )
  )

lazy val postmanGenerator = project
  .in(file("postman-generator"))
  .dependsOn(lib, lib % "test->test")
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "ammonite-ops" % "2.4.1",
    )
  )

lazy val commonSettings: Seq[Setting[_]] = Seq(
  name ~= ("apibuilder-generator-" + _),
  organization := "io.apibuilder",
  testOptions += Tests.Argument("-oF"),
  libraryDependencies ++= Seq(
    guice,
    "com.typesafe.play" %% "play-json-joda" % "2.9.4",
    "com.google.inject" % "guice" % "5.1.0",
    "com.google.inject.extensions" % "guice-assistedinject" % "5.1.0",
    "org.atteo" % "evo-inflector" % "1.3",
    "com.squareup.retrofit2" % "retrofit" % "2.11.0",
    "io.reactivex.rxjava2" % "rxjava" % "2.2.21",
    "org.typelevel" %% "cats-core" % "2.12.0",
    "org.scalatestplus.play" %% "scalatestplus-play" % "7.0.1" % "test",
    "org.scalatest" %% "scalatest" % scalatestVersion % Test,
    "org.scalatestplus" %% "scalacheck-1-15" % "3.2.11.0" % Test,
    "org.mockito" % "mockito-core" % mockitoVersion % Test,
    "com.github.javaparser" % "javaparser-core" % "3.25.10" % Test,
    "org.scalameta" %% "scalameta" % "4.9.5" % Test,
    "com.squareup" % "javapoet" % "1.13.0",
  ),
  scalacOptions ++= allScalacOptions,
  Test / javaOptions ++= Seq(
    "--add-exports=java.base/sun.security.x509=ALL-UNNAMED",
    "--add-opens=java.base/sun.security.ssl=ALL-UNNAMED"
  ),
  Compile / doc / sources := Seq.empty,
  Compile / packageDoc / publishArtifact := false,
)
