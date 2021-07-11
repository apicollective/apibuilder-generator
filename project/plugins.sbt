// Comment to get more information during initialization
logLevel := Level.Warn

// The Typesafe repository
resolvers += "Typesafe repository" at "https://repo.typesafe.com/typesafe/releases/"

// Use the Play sbt plugin for Play projects
addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.8.5")

addSbtPlugin("org.scoverage"    %% "sbt-scoverage"  % "1.8.2")

addSbtPlugin("io.github.davidgregory084" % "sbt-tpolecat" % "0.1.17")
