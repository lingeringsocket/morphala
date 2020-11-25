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

import SpanishUtils._
import SpanishVerbConjugator._

// FIXME immutable
private[spanish] case class Conjugation(
  var verb : String = "",
  var pn : Int = 0,
  var original : String = "",
  var isReflexive : Boolean = false,
  var toBeReflexive : Array[String] = TO_BE_REFLEXIVE
)

object SpanishMorphology
{
  def pluralizeNoun(
    singular : String) : String =
  {
    if (singular.isEmpty) {
      return singular
    }
    val last = singular.last
    if (isUnaccentedVowel(last)) {
      singular + "s"
    } else if (isAccentedVowel(last)) {
      last match {
        case 'í' | 'ú' => {
          singular + "es"
        }
        case _ => {
          singular + "s"
        }
      }
    } else if (singular.endsWith("ión")) {
      singular.stripSuffix("ión") + "iones"
    } else if (singular.endsWith("z")) {
      singular.stripSuffix("z") + "ces"
    } else if (singular.endsWith("g")) {
      singular + "ues"
    } else if (singular.endsWith("c")) {
      singular.stripSuffix("c") + "ques"
    } else if (singular.endsWith("s") || singular.endsWith("x")) {
      val vowel = singular.takeRight(2).head
      if (isAccentedVowel(vowel)) {
        singular.dropRight(2) + unaccentedVowel(vowel) + "ses"
      } else {
        singular
      }
    } else {
      singular match {
        case "carácter" => "caracteres"
        case "espécimen" => "especímenes"
        case "régimen" => "regímenes"
        case _ => singular + "es"
      }
    }
  }

  def conjugateVerb(
    infinitive : String,
    verbConjugator : SpanishVerbConjugator,
    person : Int,
    plural : Boolean) : String =
  {
    val conjugation = new Conjugation
    conjugation.pn = person
    if (plural) {
      conjugation.pn += 3
    }
    conjugation.verb = infinitive
    if (infinitive.endsWith("se")) {
      conjugation.isReflexive = true
      conjugation.toBeReflexive = REFLEXIVE
      conjugation.verb = root(infinitive)
    } else {
      conjugation.isReflexive = false
      conjugation.toBeReflexive = TO_BE_REFLEXIVE
    }
    conjugation.original = conjugation.verb

    // fold accent on ir verbs
    if (endVowel(conjugation.verb) == 'í') {
      conjugation.verb = root(conjugation.verb) + "ir"
    }

    fixOrthography(verbConjugator.conjugate(conjugation))
  }
}
