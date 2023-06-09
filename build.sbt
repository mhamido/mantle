val scala3Version = "3.2.2"
resolvers += Resolver.mavenCentral
scalacOptions += "-deprecation"

lazy val root = project
  .in(file("."))
  .settings(
    name := "mantle",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "0.7.29" % Test,
      "com.lihaoyi" %% "os-lib" % "0.9.1",
      "com.lihaoyi" %% "pprint" % "0.7.0",
      "com.lihaoyi" %% "mainargs" % "0.3.0",
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core" % "2.20.1",
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.20.1",
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.2.0"
    )
  )
