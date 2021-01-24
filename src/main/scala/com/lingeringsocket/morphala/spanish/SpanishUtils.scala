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

  def isVowel(letter : Char) : Boolean =
  {
    isAccentedVowel(letter) || isUnaccentedVowel(letter)
  }

  def unaccentedVowel(letter : Char) : Char =
  {
    ACCENTED_TO_UNACCENTED.get(letter).getOrElse(letter)
  }

  def accentedVowel(letter : Char) : Char =
  {
    UNACCENTED_TO_ACCENTED.get(letter).getOrElse(letter)
  }

  private def nextToVowel(word : String, i : Int) : Boolean =
  {
    val before = {
      if (i == 0) {
        false
      } else {
        isVowel(word(i - 1))
      }
    }
    val after = {
      if (i == (word.size - 1)) {
        false
      } else {
        isVowel(word(i + 1))
      }
    }
    before || after
  }

  def analyzeStress(
    word : String
  ) : (Seq[Int], Int, Int) =
  {
    val noThongs = word.replace("uái", "XáX").
      replace("uiái", "XXaX").replace("iái", "XaX").
      replace("iéi", "XéX").replace("ai", "aX").
      replace("ei", "eX").replace("oi", "oX").
      replace("ui", "Xi").replace("au", "aX").
      replace("eu", "eX").replace("ia", "Xa").
      replace("ie", "Xe").replace("io", "Xo").
      replace("iu", "iX").replace("ua", "Xa").
      replace("ue", "Xe").replace("uo", "Xo")
    val vowelPos = word.indices.filter(i => {
      val letter = noThongs(i)
      isVowel(letter)
    })
    if (vowelPos.isEmpty) {
      (vowelPos, -1, -1)
    } else {
      val accentedVowelPos = vowelPos.filter(i => isAccentedVowel(word(i))).
        headOption.getOrElse(-1)
      val last = word.last
      val naturalPos = {
        if (isVowel(last) || (last == 'n') || (last == 's')) {
          vowelPos.takeRight(2).head
        } else {
          vowelPos.last
        }
      }
      (vowelPos, accentedVowelPos, naturalPos)
    }
  }

  def adjustStress(
    before : String,
    after : String
  ) : String =
  {
    val (vowelPosBefore, accentedVowelPosBefore, naturalPosBefore) =
      analyzeStress(before)
    val (vowelPosAfter, accentedVowelPosAfter, naturalPosAfter) =
      analyzeStress(after)
    if (vowelPosBefore.isEmpty || vowelPosAfter.isEmpty) {
      return after
    }
    if (accentedVowelPosBefore == -1) {
      if (accentedVowelPosAfter == -1) {
        if (naturalPosBefore == naturalPosAfter) {
          // natural stress is preserved
          after
        } else {
          if (after.size > naturalPosBefore) {
            // preserve original stress
            after.patch(
              naturalPosBefore,
              accentedVowel(after(naturalPosBefore)).toString,
              1)
          } else {
            after
          }
        }
      } else {
        // already explicitly accented
        after
      }
    } else {
      if (accentedVowelPosBefore == naturalPosAfter) {
        after(accentedVowelPosBefore) match {
          case 'í' | 'ú' if (nextToVowel(after, accentedVowelPosBefore)) => {
            // preserve accent to avoid creating a diphthong
            after
          }
          case _ => {
            // accent has become redundant
            after.patch(
              accentedVowelPosBefore,
              unaccentedVowel(after(accentedVowelPosBefore)).toString,
              1)
          }
        }
      } else {
        // already explicitly accented
        after
      }
    }
  }
}
