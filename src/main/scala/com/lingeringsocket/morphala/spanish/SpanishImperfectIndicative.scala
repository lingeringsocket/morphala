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

object SpanishImperfectIndicative extends SpanishPast(
  Array("aba", "abas", "aba", "ábamos", "abais", "aban"),
  Array("ía", "ías", "ía", "íamos", "íais", "ían")
)
{
  object IrregularImperfectMatch extends IrregularArrayMap(
    MorphalaJson.wordListMap("IMPERFECT_INDICATIVE_IRREGULAR_MAP")
  )

  override protected[spanish] def conjugate(
    input : ConjugationInput) : String =
  {
    val verb = input.verb
    verb match {
      case IrregularImperfectMatch(conjugated) => {
        form(input, "", conjugated)
      }
      case _ => {
        form(input, root(verb), endingsAEI(verb))
      }
    }
  }
}
