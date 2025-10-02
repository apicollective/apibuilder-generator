// Comment to get more information during initialization
logLevel := Level.Warn

// The Typesafe repository
resolvers += "Typesafe repository" at "https://repo.typesafe.com/typesafe/releases/"

// Use the Play sbt plugin for Play projects
addSbtPlugin("org.playframework" % "sbt-plugin" % "3.0.5")

// Java Agent plugin for monitoring
addSbtPlugin("com.github.sbt" % "sbt-javaagent" % "0.1.8")
