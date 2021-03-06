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

import SpanishUtils._

object SpanishParticiple
{
  private val IRREGULAR_MAP =
    MorphalaJson.wordMap("PARTICIPLE_IRREGULAR_MAP")
  object IrregularMatch extends IrregularMap(IRREGULAR_MAP)

  def pastParticiple(infinitive : String) : String =
  {
    val verb = infinitive.stripSuffix("se")
    verb match {
      case IrregularMatch(iSuffix) => {
        val (before, after) = verb.splitAt(iSuffix)
        before + IRREGULAR_MAP(after)
      }
      case _ if (endVowel(verb) == 'a') => {
        root(verb) + "ado"
      }
      case _ => {
        val stem = root(verb)
        if (verb.size > 2) {
          if ("aeo".contains(stem.last) || (endVowel(verb) == 'í')) {
            stem + "ído"
          } else {
            stem + "ido"
          }
        } else {
          "ido"
        }
      }
    }
  }
}
