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

import com.lingeringsocket.morphala._

import org.specs2.mutable._

import com.fasterxml.jackson.module.scala._
import com.fasterxml.jackson.dataformat.xml._

import net.sf.extjwnl.dictionary._

import scala.util._

import SpanishMorphology._
import MorphalaUtils._

object SpanishNounMorphologySpec
{
  private val ENGLISH_DICT = Dictionary.getDefaultResourceInstance
}

class SpanishNounMorphologySpec extends Specification
{
  import SpanishNounMorphologySpec._

  private def checkPlural(singular : String, plural : String) =
  {
    pluralizeNoun(singular) must be equalTo plural
    singularizeNoun(plural) must be equalTo singular
  }

  "SpanishMorphology" should
  {
    "pluralize nouns" in
    {
      checkPlural("perro", "perros")
      checkPlural("casa", "casas")
      checkPlural("color", "colores")
      checkPlural("actriz", "actrices")
      checkPlural("frac", "fraques")
      checkPlural("zigzag", "zigzagues")

      // in certain cases, stress position changes
      checkPlural("carácter", "caracteres")
      checkPlural("espécimen", "especímenes")
      checkPlural("régimen", "regímenes")

      // otherwise, preserve stress position
      checkPlural("sofá", "sofás")
      checkPlural("esquí", "esquíes")
      checkPlural("león", "leones")
      checkPlural("imagen", "imágenes")
      checkPlural("avión", "aviones")
      checkPlural("inglés", "ingleses")
      checkPlural("cachupín", "cachupines")

      // weird cases
      checkPlural("tórax", "tórax")
      checkPlural("lunes", "lunes")
    }

    "verify database" in
    {
      val mapper = new XmlMapper
      mapper.registerModule(DefaultScalaModule)
      val xml = Using.resource(
        resourceAsSource("default-spanish-lexicon.xml")
      ) {
        _.getLines().mkString("\n")
      }
      val array = mapper.readValue(xml, classOf[Array[Map[String, String]]])
      val nouns = array.filter(_("category") == "noun")
      var count = 0
      val ignored = Using.resource(resourceAsSource("ignored-nouns.txt")) {
        _.getLines().toSet
      }
      nouns.foreach(noun => {
        val singular = noun("base")
        if (ENGLISH_DICT.lookupAllIndexWords(singular).size == 0) {
          noun.get("plural").orElse(
            noun.get("feminine_plural")
          ).foreach(plural => {
            if ((plural == (singular + "es")) || (plural == (singular + "s"))) {
              // these cases will be handled by blind truncation rules
            } else if (ignored.contains(singular)) {
              // weird cases we can't be bothered to deal with
            } else {
              checkPlural(singular, plural)
              count += 1
            }
          })
        }
      })
      count must be equalTo 340
    }
  }
}
