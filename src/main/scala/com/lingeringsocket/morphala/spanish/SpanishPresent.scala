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

import scala.collection._

import SpanishVerbConjugator._
import SpanishUtils._

object SpanishPresent
{
  val YO_CHANGE_MAP =
    MorphalaJson.wordMap("YO_CHANGE_MAP")

  object YoChangeMatch extends IrregularMap(YO_CHANGE_MAP)
}

abstract class SpanishPresent(
  initEndingsA : Array[String],
  initEndingsE : Array[String] = Array.empty,
  initEndingsI : Array[String] = Array.empty
) extends SpanishVerbConjugator(initEndingsA, initEndingsE, initEndingsI)
{
  protected def changedForm(
    input : ConjugationInput,
    stemChangedRoot : String,
    root : String,
    ends : Array[String]
  ) : String =
  {
    val chosenRoot = input.pn match {
      case 3 | 4 => root
      case _ => stemChangedRoot
    }
    form(input, chosenRoot, ends)
  }
}

object SpanishPresentSubjunctiveOrImperative
{
  val IRREGULAR_MAP =
    MorphalaJson.wordListMap("PRESENT_SUBJUNCTIVE_IRREGULAR_MAP")
}

abstract class SpanishPresentSubjunctiveOrImperative extends SpanishPresent(
  Array("e", "es", "e", "emos", "éis", "en"),
  Array("a", "as", "a", "amos", "áis", "an")
)
{
  import SpanishPresentSubjunctiveOrImperative._

  override protected def irregularMap
      : Map[String, List[String]] = IRREGULAR_MAP

  def carGarZar(infinitive : String) : String =
  {
    IrregularCarGarZarMatch.unapply(infinitive).getOrElse(root(infinitive))
  }
}
