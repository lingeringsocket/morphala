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

import SpanishVerbConjugator._
import SpanishUtils._

object SpanishGerund
{
  object IrregularMatch extends IrregularExactMap(
    MorphalaJson.wordMap("GERUND_IRREGULAR_MAP")
  )

  def gerund(infinitive : String) : String =
  {
    val (isReflexive, verb) = {
      if (infinitive.endsWith("se")) {
        (true, root(infinitive))
      } else {
        (false, infinitive)
      }
    }
    val base = fixOrthography(gerundImpl(verb))
    if (isReflexive) {
      val full = base + "se"
      adjustStress(base, full)
    } else {
      base
    }
  }

  def truncated(verb : String) : String =
  {
    if (verb == "poder") {
      "pod"
    } else {
      val full = gerundImpl(verb)
      val (before, after) = full.splitAt(full.size - 4)
      if ((after.head != 'a') && (before.last != 'ñ')) {
        full.dropRight(5)
      } else {
        before
      }
    }
  }

  private def gerundImpl(verb : String) : String =
  {
    verb match {
      case IrregularMatch(gerund) => {
        gerund
      }
      case _ if (endVowel(verb) == 'a') => {
        root(verb) + "ando"
      }
      case _ if (
        (endVowel(verb) == 'i') &&
          isStemChangeE(verb)
      ) => {
        val stemChanged = replaceLastInRoot(verb, 'e', "i")
        val stem = root(stemChanged)
        if (stem.last == 'ñ') {
          stem + "endo"
        } else {
          stem + "iendo"
        }
      }
      case _ if (verb(verb.size - 3) == 'ñ')  => {
        root(verb) + "endo"
      }
      case _ if (
        (endVowel(verb) == 'i') &&
          isStemChangeO(verb)
      ) => {
        root(replaceLastInRoot(verb, 'o', "u")) + "iendo"
      }
      case _ if(
        "aeiou".contains(verb.takeRight(3).head) &&
          (endVowel(verb) != 'a')
          && !verb.endsWith("inguir")
      ) => {
        root(verb) + "yendo"
      }
      case _ => {
        root(verb) + "iendo"
      }
    }
  }
}
