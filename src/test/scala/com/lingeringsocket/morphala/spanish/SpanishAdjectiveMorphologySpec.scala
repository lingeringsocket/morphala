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

import org.specs2.mutable._

import net.sf.extjwnl.data._

import scala.util._
import scala.collection._
import scala.jdk.CollectionConverters._

import java.io._

import SpanishMorphology._

class SpanishAdjectiveMorphologySpec extends Specification
{
  import WordnetDictionaries._

  private def checkPlural(singular : String, plural : String) =
  {
    pluralizeAdjective(singular) must be equalTo plural
  }

  "SpanishMorphology" should
  {
    "pluralize adjectives" in
    {
      checkPlural("rojo", "rojos")
      checkPlural("bonita", "bonitas")
      checkPlural("interesante", "interesantes")
      checkPlural("feliz", "felices")
      checkPlural("inglés", "ingleses")
    }

    "verify irregular forms" in
    {
      val newIrregulars = new mutable.TreeMap[String, String]

      val dict = SPANISH_DICT
      val morphProcessor = dict.getMorphologicalProcessor
      dict.getIndexWordIterator(POS.ADJECTIVE).asScala.foreach(adjective => {
        val singular = adjective.getLemma
        val plural = pluralizeAdjective(singular)
        if (singular.contains(' ') || singular.startsWith("`") ||
          (singular == plural) || singular.endsWith("i"))
        {
          // ignore these
        } else {
          val bases = morphProcessor.lookupAllBaseForms(
            POS.ADJECTIVE, plural).asScala.toSet
          if (!bases.contains(singular)) {
            newIrregulars.put(plural, singular)
          }
        }
      })

      if (newIrregulars.nonEmpty) {
        val file = "irregular_adjective_forms.txt"
        Using.resource(new PrintWriter(new FileWriter(file))) {
          pw => {
            newIrregulars.foreach {
              case (plural, singular) => {
                pw.println(s"a#$plural -addexc $singular")
              }
            }
          }
        }
        println(s"New irregular forms written to $file")
        println("Please review and merge into")
        println("extjwnl/supplemental_spa.txt")
      }
      newIrregulars.size must be equalTo 0
    }
  }
}
