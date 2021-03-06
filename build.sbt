// morphala:  word morphology for Scala
// Copyright 2020-2020 John V. Sichi
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
name := "morphala"

ThisBuild / organization := "com.lingeringsocket"

ThisBuild / version := "0.1-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.3"

ThisBuild / githubWorkflowBuildPreamble := Seq(
  WorkflowStep.Sbt(List(
    "scalastyle",
    "test:scalastyle"
  ), name = Some("Scalastyle")),
  WorkflowStep.Run(List(
    "sudo apt-get install -y dos2unix",
    "src/test/resources/fetchExternal.sh"
  ), name = Some("Fetch External Data"))
)

ThisBuild / githubWorkflowPublishTargetBranches := Seq()

scalastyleFailOnError := true

scalacOptions := Seq(
  "-unchecked", "-feature", "-Xlint", "-Xlint:-nonlocal-return", "-Xlint:-unit-special",
  "-deprecation", "-Xfatal-warnings", "-Yrangepos",
  "-Ywarn-unused:imports,privates,locals,implicits"
)

maxErrors := 99

traceLevel.withRank(KeyRanks.Invisible) := 100

resolvers ++= Seq(Resolver.sonatypeRepo("snapshots"), Resolver.mavenLocal)

libraryDependencies ++= Seq(
  "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.12.0-rc2",
  "org.atteo" % "evo-inflector" % "1.2.2",
  "net.sf.extjwnl" % "extjwnl" % "2.0.2" % "test",
  "net.sf.extjwnl" % "extjwnl" % "2.0.2" % "test",
  "net.sf.extjwnl" % "extjwnl-data-wn31" % "1.2" % "test",
  "net.sf.extjwnl.mcr" % "extjwnl-data-spa-mcr30" % "1.0.5" % "test",
  "net.sf.extjwnl.mcr" % "extjwnl-data-alignment-mcr30" % "1.0.5" % "test",
  "com.fasterxml.jackson.dataformat" % "jackson-dataformat-xml" % "2.12.0-rc2" % "test",
  "org.specs2" %% "specs2-core" % "4.10.5" % "test"
)

publishTo := Some(Resolver.file("file", new File(Path.userHome.absolutePath+"/.ivy2/local/com.lingeringsocket")))
