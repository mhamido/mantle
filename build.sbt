val scala3Version = "3.3.0"
resolvers += Resolver.mavenCentral
scalacOptions += "-deprecation"

// run / fork := true
run / connectInput := true

lazy val root = project
  .in(file("."))
  .settings(
    name := "mantle",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "1.0.0-M3" % Test,
      "com.lihaoyi" %% "os-lib" % "0.9.1",
      "com.lihaoyi" %% "pprint" % "0.7.0",
      "com.lihaoyi" %% "mainargs" % "0.3.0",
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core" % "2.20.1",
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.20.1",
    )
  )

// commands += Command.command("runExamples") { state =>
//   (baseDirectory.value / "examples" ** ".mt" ** ".mntl").getPaths().foreach { name => 
//     Command.process(s"run $name", state.copy())
//   }
//   state
// }