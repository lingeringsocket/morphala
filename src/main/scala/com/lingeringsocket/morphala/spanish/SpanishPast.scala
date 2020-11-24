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

object SpanishPast
{
  val IRREGULAR_MAP =
    MorphalaJson.wordListMap("PRETERITE_IRREGULAR_MAP")

  object IrregularMatch extends IrregularArrayMap(IRREGULAR_MAP)

  val IRREGULAR_ROOT_MAP =
    MorphalaJson.wordMap("PRETERITE_IRREGULAR_ROOT_MAP")

  object IrregularRootMatch extends IrregularMap(IRREGULAR_ROOT_MAP)
}

abstract class SpanishPast(
  initEndingsA : Array[String],
  initEndingsE : Array[String] = Array.empty,
  initEndingsI : Array[String] = Array.empty
) extends SpanishVerbConjugator(initEndingsA, initEndingsE, initEndingsI)
{
}
