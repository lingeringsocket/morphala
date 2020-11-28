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
  private def nextToVowel(word : String, i : Int) : Boolean =
  {
    val before = {
      if (i == 0) {
        false
      } else {
        isVowel(word(i - 1))
      }
    }
    val after = {
      if (i == (word.size - 1)) {
        false
      } else {
        isVowel(word(i + 1))
      }
    }
    before || after
  }

  private def analyzeStress(
    word : String
  ) : (Seq[Int], Int, Int) =
  {
    val noThongs = word.replace("uái", "XáX").
      replace("uiái", "XXaX").replace("iái", "XaX").
      replace("iéi", "XéX").replace("ai", "aX").
      replace("ei", "eX").replace("oi", "oX").
      replace("ui", "Xi").replace("au", "aX").
      replace("eu", "eX").replace("ia", "Xa").
      replace("ie", "Xe").replace("io", "Xo").
      replace("iu", "iX").replace("ua", "Xa").
      replace("ue", "Xe").replace("uo", "Xo")
    val vowelPos = word.indices.filter(i => {
      val letter = noThongs(i)
      isVowel(letter)
    })
    if (vowelPos.isEmpty) {
      (vowelPos, -1, -1)
    } else {
      val accentedVowelPos = vowelPos.filter(i => isAccentedVowel(word(i))).
        headOption.getOrElse(-1)
      val last = word.last
      val naturalPos = {
        if (isVowel(last) || (last == 'n') || (last == 's')) {
          vowelPos.takeRight(2).head
        } else {
          vowelPos.last
        }
      }
      (vowelPos, accentedVowelPos, naturalPos)
    }
  }

  private def adjustStress(
    before : String,
    after : String
  ) : String =
  {
    val (vowelPosBefore, accentedVowelPosBefore, naturalPosBefore) =
      analyzeStress(before)
    val (vowelPosAfter, accentedVowelPosAfter, naturalPosAfter) =
      analyzeStress(after)
    if (vowelPosBefore.isEmpty || vowelPosAfter.isEmpty) {
      return after
    }
    if (accentedVowelPosBefore == -1) {
      if (accentedVowelPosAfter == -1) {
        if (naturalPosBefore == naturalPosAfter) {
          // natural stress is preserved
          after
        } else {
          if (after.size > naturalPosBefore) {
            // preserve original stress
            after.patch(
              naturalPosBefore,
              accentedVowel(after(naturalPosBefore)).toString,
              1)
          } else {
            after
          }
        }
      } else {
        // already explicitly accented
        after
      }
    } else {
      if (accentedVowelPosBefore == naturalPosAfter) {
        after(accentedVowelPosBefore) match {
          case 'í' | 'ú' if (nextToVowel(after, accentedVowelPosBefore)) => {
            // preserve accent to avoid creating a diphthong
            after
          }
          case _ => {
            // accent has become redundant
            after.patch(
              accentedVowelPosBefore,
              unaccentedVowel(after(accentedVowelPosBefore)).toString,
              1)
          }
        }
      } else {
        // already explicitly accented
        after
      }
    }
  }

  def singularizeNoun(
    plural : String) : String =
  {
    if (plural.isEmpty) {
      return plural
    }
    val singular = {
      if (plural.endsWith("ces")) {
        plural.stripSuffix("ces") + "z"
      } else if (plural.endsWith("gues")) {
        plural.stripSuffix("ues")
      } else if (plural.endsWith("ques")) {
        plural.stripSuffix("ques") + "c"
      } else {
        plural match {
          case "caracteres" => return "carácter"
          case "especímenes" => return "espécimen"
          case "regímenes" => return "régimen"
          case "lunes" | "martes" | "miércoles" |
              "jueves" | "viernes" => plural
          case _ => {
            if (plural.endsWith("es")) {
              plural.stripSuffix("es")
            } else if (plural.endsWith("s")) {
              plural.stripSuffix("s")
            } else {
              plural
            }
          }
        }
      }
    }
    adjustStress(plural, singular)
  }

  def pluralizeNoun(
    singular : String) : String =
  {
    if (singular.isEmpty) {
      return singular
    }
    val last = singular.last
    val plural = {
      if (isUnaccentedVowel(last)) {
        singular + "s"
      } else if (isAccentedVowel(last)) {
        last match {
          case 'í' | 'ú' => {
            return singular + "es"
          }
          case _ => {
            singular + "s"
          }
        }
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
          case "carácter" => return "caracteres"
          case "espécimen" => return "especímenes"
          case "régimen" => return "regímenes"
          case _ => singular + "es"
        }
      }
    }
    adjustStress(singular, plural)
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
