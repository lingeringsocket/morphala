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

import SpanishUtils._

object SpanishTense
{
  val STEM_CHANGE_E_TO_IE = MorphalaJson.wordSet("STEM_CHANGE_E_TO_IE")

  val STEM_CHANGE_E_TO_I = MorphalaJson.wordSet("STEM_CHANGE_E_TO_I")

  val STEM_CHANGE_O_TO_UE = MorphalaJson.wordSet("STEM_CHANGE_O_TO_UE")

  val IRREGULAR_REQUIRE_FULL = MorphalaJson.wordSet("IRREGULAR_REQUIRE_FULL")

  val STEM_CHANGE_REQUIRE_FULL = MorphalaJson.wordSet("STEM_CHANGE_REQUIRE_FULL")

  val IAR_IRREGULAR = MorphalaJson.wordSet("IAR_IRREGULAR")

  def replaceLastInRoot(
    verb : String, letter : Char, replacement : String) : String =
  {
    root(verb).lastIndexOf(letter) match {
      case -1 => verb
      case i => {
        val modifiedReplacement = {
          if ((i > 0) && (replacement == "ue") && (verb(i - 1) == 'g')) {
            "üe"
          } else {
            replacement
          }
        }
        verb.patch(i, modifiedReplacement, 1)
      }
    }
  }

  def isStemChangeE(verb : String) : Boolean =
  {
    val requireFull = STEM_CHANGE_REQUIRE_FULL
    (
      (findSuffix(verb, STEM_CHANGE_E_TO_I, requireFull) >= 0) ||
        (findSuffix(verb, STEM_CHANGE_E_TO_IE, requireFull) >= 0)
    )
  }

  def isStemChangeO(verb : String) : Boolean =
  {
    val requireFull = STEM_CHANGE_REQUIRE_FULL
    findSuffix(verb, STEM_CHANGE_O_TO_UE, requireFull) >= 0
  }

  def changeStem(
    verb : String,
    eToIe : String = "ie",
    oToUe : String = "ue") : String =
  {
    val requireFull = STEM_CHANGE_REQUIRE_FULL
    if (findSuffix(verb, STEM_CHANGE_E_TO_I, requireFull) >= 0) {
      replaceLastInRoot(verb, 'e', "i")
    } else if (findSuffix(verb, STEM_CHANGE_E_TO_IE, requireFull) >= 0) {
      replaceLastInRoot(verb, 'e', eToIe)
    } else if (findSuffix(verb, STEM_CHANGE_O_TO_UE, requireFull) >= 0) {
      if (verb.equals("jugar")) {
        replaceLastInRoot(verb, 'u', oToUe)
      } else {
        replaceLastInRoot(verb, 'o', oToUe)
      }
    } else {
      verb
    }
  }

  def findSuffix(
    verb : String,
    set : Set[String],
    requireFull : Set[String]) : Int =
  {
    val i = verb.indices.indexWhere(i => set.contains(verb.substring(i)))
    if (i == -1) {
      i
    } else {
      if (
        verb.indices.indexWhere(
          j => requireFull.contains(verb.substring(j)), i
        ) > -1
      ) {
        if (i > 0) {
          -1
        } else {
          i
        }
      } else {
        i
      }
    }
  }

  def substZC(verb : String) : String =
  {
    val base = verb.dropRight(3)
    val pre = base.last
    if ((pre == 'n') || (pre == 'r')) {
      return base + "z";
    } else {
      return base + "zc";
    }
  }

  def endReflexive(conjugation : Conjugation, root : String) : String =
  {
    if (conjugation.toBeReflexive(0).isEmpty) {
      root
    } else {
      accentFirstVowel(root)
    }
  }

  def form(
    conjugation : Conjugation,
    root : String) : String =
  {
    val pn = conjugation.pn
    conjugation.toBeReflexive(pn) + root
  }

  def form(
    conjugation : Conjugation,
    root : String,
    ends : Array[String]) : String =
  {
    val pn = conjugation.pn
    conjugation.toBeReflexive(pn) + root + ends(pn)
  }
}

abstract class SpanishTense(
  initEndingsA : Array[String],
  initEndingsE : Array[String] = Array.empty,
  initEndingsI : Array[String] = Array.empty
)
{
  protected val endingsA = initEndingsA

  protected val endingsE = fillEndings(initEndingsE, endingsA)

  protected val endingsI = fillEndings(initEndingsI, endingsE)

  private def fillEndings(
    endings : Array[String], fallback : Array[String]) : Array[String] =
  {
    if (endings.isEmpty) {
      fallback
    } else {
      endings
    }
  }

  protected[spanish] def conjugate(
    conjugation : Conjugation) : String

  protected def endingsAEI(verb : String) : Array[String] =
  {
    endVowel(verb) match {
      case 'a' => endingsA
      case 'e' => endingsE
      case _ => endingsI
    }
  }

  protected def irregularMap : Map[String, List[String]] = Map.empty

  protected def irregularEndings(infinitive : String) : Array[String] =
  {
    irregularMap(infinitive).toArray
  }
}
