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

object SpanishImperfectSubjunctive extends SpanishPast(
  Array("ara", "aras", "ara", "áramos", "arais", "aran"),
  Array("iera", "ieras", "iera", "iéramos", "ierais", "ieran")
)
{
  import SpanishPast._

  private val IR_SER_CONJUGATED = Array(
    "fuera", "fueras", "fuera", "fuéramos", "fuerais", "fueran")

  private val ERA_ENDINGS = Array(
    "era", "eras", "era", "éramos", "erais", "eran")

  override protected[spanish] def conjugate(
    conjugation : Conjugation) : String =
  {
    val verb = conjugation.verb
    def endings = endingsAEI(verb)

    verb match {
      case "ir" | "ser" => {
        form(conjugation, "", IR_SER_CONJUGATED)
      }
      case "dar" => {
        form(conjugation, root(verb), endingsE)
      }
      case "reir" => {
        form(conjugation, "r", endingsE)
      }
      case "estar" | "andar" => {
        form(conjugation, root(verb) + "uvi", ERA_ENDINGS)
      }
      case IrregularRootMatch(iSuffix) => {
        val (before, after) = verb.splitAt(iSuffix)
        val modifiedEndings = after match {
          case "decir" | "traer" => ERA_ENDINGS
          case _ => endings
        }
        form(
          conjugation,
          before + IRREGULAR_ROOT_MAP(after),
          modifiedEndings
        )
      }
      case IrregularPreteriteStemMatch(withStemChange) => {
        form(conjugation, root(withStemChange), endings)
      }
      case IrregularYMatch(newRoot) => {
        form(conjugation, newRoot + "y", ERA_ENDINGS)
      }
      case IrregularUirMatch(newRoot) => {
        form(conjugation, newRoot, ERA_ENDINGS)
      }
      case IrregularUcirMatch(newRoot) => {
        form(conjugation, newRoot, ERA_ENDINGS)
      }
      case _ => {
        form(conjugation, root(verb), endings)
      }
    }
  }
}
