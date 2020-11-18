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
package com.lingeringsocket.morphala.spanish

import scala.collection._
import scala.io._
import scala.util._

import com.fasterxml.jackson.module.scala._
import com.fasterxml.jackson.databind._

object MorphalaJson
{
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
    val json = Using.resource(
      Source.fromInputStream(
        getClass.getClassLoader.getResourceAsStream(
          "morphala/spanish.json"))
    ) {
      source => source.getLines().mkString("\n")
    }
    mapper.readValue(json, classOf[Map[String, _]])
  }
}

object SpanishUtils
{
  val REFLEXIVE = Array("me", "te", "se", "nos", "os", "se").map(_ + " ")

  val TO_BE_REFLEXIVE = Array.fill(6)("")

  private val ACCENTS = Map(
    'a' -> "á",
    'e' -> "é",
    'i' -> "í",
    'o' -> "ó",
    'u' -> "ú"
  )

  def root(infinitive : String) : String =
  {
    infinitive.dropRight(2)
  }

  def endVowel(infinitive : String) : Char =
  {
    ending(infinitive).head
  }

  def ending(infinitive : String) : String =
  {
    infinitive.takeRight(2)
  }

  def fixOrthography(word : String) : String =
  {
    word.replace("ñie", "ñe").replace("ñié", "ñé").replace("ñió", "ñó").
      replace("llie", "lle").replace("llié", "llé").replace("llió", "lló")
  }

  def accentFirstVowel(root : String) : String =
  {
    root.indexWhere(c => ("aeiou".indexOf(c) > -1)) match {
      case -1 => root
      case i => {
        root.patch(i, ACCENTS(root(i)), 1)
      }
    }
  }
}
