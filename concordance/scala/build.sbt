import Dependencies._

ThisBuild / scalaVersion     := "2.13.1"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "scala",
    libraryDependencies += scalaTest % Test
  )

// This pulls in cats as a dependency. Meow.
libraryDependencies += "org.typelevel" %% "cats-effect" % "2.1.3" // available for 2.12, 2.13

// Add to the to-do!
libraryDependencies += "co.fs2" %% "fs2-core" % "2.2.1" // For cats 2 and cats-effect 2

// optional I/O library
libraryDependencies += "co.fs2" %% "fs2-io" % "2.2.1"

// optional reactive streams interop
libraryDependencies += "co.fs2" %% "fs2-reactive-streams" % "2.2.1"

// optional experimental library
libraryDependencies += "co.fs2" %% "fs2-experimental" % "2.2.1"

libraryDependencies += {
  val version = scalaBinaryVersion.value match {
    case "2.10" => "1.0.3"
    case _ ⇒ "2.1.1"
  }
  "com.lihaoyi" % "ammonite" % version % "test" cross CrossVersion.full
}

// optional experimental library
libraryDependencies += "com.monovore" %% "decline" % "1.0.0"
libraryDependencies += "com.monovore" %% "decline-effect" % "1.0.0"

wartremoverErrors ++= Warts.unsafe

sourceGenerators in Test += Def.task {
  val file = (sourceManaged in Test).value / "amm.scala"
  IO.write(file, """object amm extends App { ammonite.Main.main(args) }""")
  Seq(file)
}.taskValue

// Optional, required for the `source` command to work
(fullClasspath in Test) ++= {
  (updateClassifiers in Test).value
    .configurations
    .find(_.configuration == Test.name)
    .get
    .modules
    .flatMap(_.artifacts)
    .collect{case (a, f) if a.classifier == Some("sources") => f}
}

scalacOptions += "-Xfatal-warnings"

// scalac options come from the sbt-tpolecat plugin so need to set any here
addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)
addCompilerPlugin("io.tryp" % "splain" % "0.5.3" cross CrossVersion.patch)

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
