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

import SpanishVerbConjugator._
import SpanishUtils._

object SpanishPresentSubjunctive extends SpanishPresentSubjunctiveOrImperative
{
  import SpanishPresent._
  import SpanishPresentSubjunctiveOrImperative._

  object IrregularMatch extends IrregularArrayMap(IRREGULAR_MAP)

  override protected[spanish] def conjugate(
    conjugation : Conjugation) : String =
  {
    val verb = conjugation.verb

    def verbRoot = root(verb)
    def endings = endingsAEI(verb)

    verb match {
      case IrregularMatch(conjugated) => {
        form(conjugation, "", conjugated)
      }
      case YoChangeMatch(iSuffix) => {
        val (before, after) = verb.splitAt(iSuffix)
        form(
          conjugation,
          before + YO_CHANGE_MAP(after).dropRight(1),
          endings)
      }
      case IrregularCerCirMatch(_) => {
        form(conjugation, substZC(verb), endings)
      }
      case IrregularUirMatch(newRoot) => {
        form(conjugation, newRoot, endings)
      }
      case IrregularUarIarMatch(newRoot) => {
        changedForm(conjugation, newRoot, verbRoot, endings)
      }
      case IrregularGuarMatch(newRoot) => {
        form(conjugation, newRoot, irregularEndings("guar"))
      }
      case _ => {
        val truncatedGerund = SpanishGerund.truncated(verb)
        changedForm(
          conjugation,
          carGarZar(changeStem(verb)),
          carGarZar(truncatedGerund + ending(verb)),
          endings
        )
      }
    }
  }
}
