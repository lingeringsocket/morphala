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
package com.lingeringsocket.morphala

import scala.collection._
import scala.io._
import scala.util._

import com.fasterxml.jackson.module.scala._
import com.fasterxml.jackson.databind._

object MorphalaUtils
{
  def resourceAsSource(resourceName : String) : Source =
  {
    Source.fromInputStream(
      getClass.getClassLoader.getResourceAsStream(resourceName)
    )
  }
}

object MorphalaJson
{
  import MorphalaUtils._

  private val map = readJson

  def wordSet(name : String) : Set[String] =
  {
    map(name).asInstanceOf[List[String]].toSet
  }

  def wordMap(name : String) : Map[String, String] =
  {
    map(name).asInstanceOf[Map[String, String]]
  }

  def wordListMap(name : String) : Map[String, List[String]] =
  {
    map(name).asInstanceOf[Map[String, List[String]]]
  }

  private def readJson : Map[String, _] =
  {
    val mapper = new ObjectMapper
    mapper.registerModule(DefaultScalaModule)
    val json = Using.resource(resourceAsSource("morphala/spanish.json")) {
      _.getLines().mkString("\n")
    }
    mapper.readValue(json, classOf[Map[String, _]])
  }
}
