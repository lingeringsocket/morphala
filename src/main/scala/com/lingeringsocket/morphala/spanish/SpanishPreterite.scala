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

import scala.collection._

object SpanishPreterite extends SpanishPast(
  Array("é", "aste", "ó", "amos", "asteis", "aron"),
  Array("í", "iste", "ió", "imos", "isteis", "ieron"),
  Array("í", "iste", "ió", "imos", "isteis", "ieron")
)
{
  import SpanishPast._

  override protected def irregularMap
      : Map[String, List[String]] = IRREGULAR_MAP

  override protected[spanish] def conjugate(
    input : ConjugationInput) : String =
  {
    val verb = input.verb
    def endings = endingsAEI(verb)

    verb match {
      case IrregularMatch(conjugated) => {
        form(input, "", conjugated)
      }
      case IrregularRootMatch(iSuffix) => {
        val (before, after) = verb.splitAt(iSuffix)
        val mapKey = after match {
          case "decir" | "traer" => "DecirTraer"
          case _ => "Ireg"
        }
        form(
          input,
          before + IRREGULAR_ROOT_MAP(after),
          irregularEndings(mapKey)
        )
      }
      case IrregularUcirMatch(newRoot) => {
        form(input, newRoot, irregularEndings("ucir"))
      }
      case IrregularPreteriteStemMatch(withStemChange) => {
        changedForm(input, withStemChange, verb, endings)
      }
      case IrregularYMatch(newRoot) => {
        form(input, newRoot, irregularEndings("addY"))
      }
      case IrregularUirUerMatch(newRoot) if (
        (verb.takeRight(4).head == 'g') || ((input.pn % 3) != 0)
      ) => {
        form(input, newRoot, irregularEndings("uir"))
      }
      case IrregularGuarMatch(newRoot) => {
        form(input, newRoot, irregularEndings("guar"))
      }
      case IrregularCarGarZarMatch(newRoot) if (input.pn == 0) => {
        form(input, newRoot, endings)
      }
      case _ => {
        form(input, root(verb), endings)
      }
    }
  }

  private def changedForm(
    input : ConjugationInput,
    stemChangedVerb : String,
    verb : String,
    ends : Array[String]
  ) : String =
  {
    val chosenVerb = input.pn match {
      case 2 | 5 => stemChangedVerb
      case _ => verb
    }
    form(input, root(chosenVerb), ends)
  }
}
