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

object SpanishPresentIndicative extends SpanishPresent(
  Array("o", "as", "a", "amos", "áis", "an"),
  Array("o", "es", "e", "emos", "éis", "en"),
  Array("o", "es", "e", "imos", "ís", "en")
)
{
  import SpanishPresent._

  object IrregularMatch extends IrregularArrayMap(
    MorphalaJson.wordListMap("PRESENT_INDICATIVE_IRREGULAR_MAP")
  )

  override protected[spanish] def conjugate(
    input : ConjugationInput) : String =
  {
    val verb = input.verb

    def withStemChange = changeStem(verb)
    def verbRoot = root(verb)
    def stemChangedRoot = root(withStemChange)
    def endings = endingsAEI(verb)

    verb match {
      case IrregularMatch(conjugated) => {
        form(input, "", conjugated)
      }
      case YoChangeMatch(iSuffix) => {
        val (before, after) = verb.splitAt(iSuffix)
        input.pn match {
          case 0 => {
            form(input, before + YO_CHANGE_MAP(after))
          }
          case _ => {
            changedForm(
              input, stemChangedRoot, before + root(after), endings)
          }
        }
      }
      case IrregularCerCirMatch(suffixes) => {
        form(input, verb.dropRight(3), suffixes)
      }
      case IrregularUirMatch(newRoot) => {
        changedForm(input, newRoot, verbRoot, endings)
      }
      case IrregularUarIarMatch(newRoot) => {
        changedForm(input, newRoot, verbRoot, endings)
      }
      case _ => {
        changedForm(input, stemChangedRoot, verbRoot, endings)
      }
    }
  }
}
