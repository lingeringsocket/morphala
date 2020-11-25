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

import scala.collection._

object SpanishUtils
{
  private val UNACCENTED_TO_ACCENTED = Map(
    'a' -> 'á',
    'e' -> 'é',
    'i' -> 'í',
    'o' -> 'ó',
    'u' -> 'ú'
  )

  private val ACCENTED_TO_UNACCENTED = UNACCENTED_TO_ACCENTED.map(_.swap)

  def root(infinitive : String) : String =
  {
    infinitive.dropRight(2)
  }

  def endVowel(infinitive : String) : Char =
  {
    ending(infinitive).head
  }

  def ending(infinitive : String) : String =
  {
    infinitive.takeRight(2)
  }

  def fixOrthography(word : String) : String =
  {
    word.replace("ñie", "ñe").replace("ñié", "ñé").replace("ñió", "ñó").
      replace("llie", "lle").replace("llié", "llé").replace("llió", "lló")
  }

  def accentFirstVowel(root : String) : String =
  {
    root.indexWhere(c => ("aeiou".indexOf(c) > -1)) match {
      case -1 => root
      case i => {
        root.patch(i, UNACCENTED_TO_ACCENTED(root(i)).toString, 1)
      }
    }
  }

  def isUnaccentedVowel(letter : Char) : Boolean =
  {
    UNACCENTED_TO_ACCENTED.contains(letter)
  }

  def isAccentedVowel(letter : Char) : Boolean =
  {
    ACCENTED_TO_UNACCENTED.contains(letter)
  }

  def unaccentedVowel(letter : Char) : Char =
  {
    ACCENTED_TO_UNACCENTED.get(letter).getOrElse(letter)
  }
}
