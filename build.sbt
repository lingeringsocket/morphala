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
organization := "com.lingeringsocket"

name := "morphala"

version := "0.1-SNAPSHOT"

scalaVersion := "2.13.3"

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
  "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.9.9",
  "org.atteo" % "evo-inflector" % "1.2.2",
  "net.sf.extjwnl" % "extjwnl" % "2.0.2" % "test",
  "com.lingeringsocket" % "extjwnl-data-mcr30-2016" % "0.0.1" % "test",
  "org.specs2" %% "specs2-core" % "4.10.5" % "test"
)

publishTo := Some(Resolver.file("file", new File(Path.userHome.absolutePath+"/.ivy2/local/com.lingeringsocket")))
