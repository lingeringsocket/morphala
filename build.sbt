organization := "com.lingeringsocket"

name := "morphala"

version := "0.1-SNAPSHOT"

scalaVersion := "2.13.3"

scalacOptions := Seq(
  "-unchecked", "-feature", "-Xlint", "-Xlint:-nonlocal-return", "-Xlint:-unit-special",
  "-deprecation", "-Xfatal-warnings", "-Yrangepos",
  "-Ywarn-unused:imports,privates,locals,implicits"
)

maxErrors := 99

traceLevel.withRank(KeyRanks.Invisible) := 100

libraryDependencies ++= Seq(
  "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.9.9",
  "org.specs2" %% "specs2-core" % "4.10.5" % "test"
)

publishTo := Some(Resolver.file("file", new File(Path.userHome.absolutePath+"/.ivy2/local/com.lingeringsocket")))
