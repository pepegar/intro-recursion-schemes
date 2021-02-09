ThisBuild / scalaVersion := "2.12.12"

lazy val docs = project
  .in(file("docs"))
  .settings(
    resolvers ++= Seq(
      Resolver.sonatypeRepo("releases"),
      Resolver.sonatypeRepo("snapshots")
    ),
    Compile / scalacOptions ++= Seq(
      "-encoding", "UTF-8", // 2 args
      "-feature",
      "-language:existentials",
      "-language:higherKinds",
      "-language:implicitConversions",
    ),
    mdocIn := baseDirectory.value / "mdoc",
    mdocOut := baseDirectory.value / "mdoc-out",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.0.0",
      "io.higherkindness" %% "droste-core" % "0.8.0",
      "io.higherkindness" %% "droste-macros" % "0.8.0",
      "com.github.gsk-aiops" %% "bellman-algebra-parser" % "0.1.12",
    ),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full),
    addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.3" cross CrossVersion.full),
  )
  .enablePlugins(MdocPlugin)
