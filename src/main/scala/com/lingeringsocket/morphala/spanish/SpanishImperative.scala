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

import SpanishTense._
import SpanishUtils._

object SpanishImperative extends SpanishPresentSubjunctiveOrImperative
{
  import SpanishPresent._

  object IrregularMatch extends IrregularArrayMap(
    MorphalaJson.wordListMap("IMPERATIVE_IRREGULAR_MAP")
  )

  object ReflexiveIrregularMatch extends IrregularArrayMap(
    MorphalaJson.wordListMap("IMPERATIVE_REFLEXIVE_IRREGULAR_MAP")
  )

  object YoIrregularMatch extends IrregularExactMap(
    MorphalaJson.wordMap("IMPERATIVE_YO_IRREGULAR_MAP")
  )

  override protected[spanish] def conjugate(
    conjugation : Conjugation) : String =
  {
    val verb = conjugation.verb
    def withStemChange = changeStem(verb)

    verb match {
      case ReflexiveIrregularMatch(conjugated) if (
        conjugation.toBeReflexive(0).nonEmpty
      ) => {
        conjugated(conjugation.pn-1)
      }
      case IrregularMatch(conjugated) => {
        form(conjugation, conjugated(conjugation.pn-1))
      }
      case YoChangeMatch(iSuffix) if (
        !((conjugation.pn < 2) && verb.contains("decir") && !verb.equals("decir"))
      ) => {
        val (before, after) = verb.splitAt(iSuffix)
        yoForm(conjugation, before, after)
      }
      case IrregularCerCirMatch(_) => {
        val subst = substZC(verb)
        impForm(conjugation, subst, subst, verb, subst)
      }
      case IrregularUirMatch(newRoot) => {
        impForm(conjugation, newRoot, verb, verb, newRoot)
      }
      case IrregularUarIarMatch(newRoot) => {
        impForm(conjugation, newRoot, verb, verb, root(verb))
      }
      case _ => {
        val truncatedGerund = SpanishGerund.truncated(verb)
        impForm(
          conjugation,
          carGarZar(withStemChange),
          carGarZar(verb),
          verb,
          carGarZar(truncatedGerund + ending(verb)))
      }
    }
  }

  private def impForm(
    conjugation : Conjugation, withStemChange : String, verb : String,
    originalVerb : String, withSmallChange : String) : String =
  {
    def endings = endingsAEI(originalVerb)

    if (conjugation.pn == 1) {
      changeStem(originalVerb) match {
        case IrregularUirMatch(newRoot) => {
          form(conjugation, newRoot + "e")
        }
        case IrregularUarIarMatch(newRoot) => {
          form(conjugation, newRoot + "a")
        }
        case IrregularGuarMatch(newRoot) => {
          form(conjugation, newRoot + "ua")
        }
        case _ => {
          val vowel = {
            if (endVowel(originalVerb) == 'a') {
              'a'
            } else {
              'e'
            }
          }
          endReflexive(conjugation, root(changeStem(originalVerb))) + vowel + {
            conjugation.toBeReflexive(conjugation.pn)
          }
        }
      }
    } else if (
      ((conjugation.pn == 2) || (conjugation.pn == 5)) &&
        originalVerb.endsWith("guar")
    ) {
      form(
        conjugation,
        changeStem(originalVerb).take(originalVerb.size - 3),
        irregularEndings("guar"))
    } else if (conjugation.pn == 3) {
      if (conjugation.toBeReflexive.head.nonEmpty) {
        if (endings.head.equals("e")) {
          withSmallChange + "émonos"
        } else {
          withSmallChange + "ámonos"
        }
      } else {
        form(conjugation, withSmallChange, endings)
      }
    } else if (conjugation.pn == 4) {
      if (conjugation.toBeReflexive.head.nonEmpty) {
        root(conjugation.original) + "íos"
      } else {
        form(
          conjugation,
          endReflexive(conjugation, root(originalVerb)) +
            endVowel(originalVerb) + "d")
      }
    } else {
      endReflexive(conjugation, withStemChange) + endings(conjugation.pn) + {
        conjugation.toBeReflexive(conjugation.pn)
      }
    }
  }

  private def yoForm(
    conjugation : Conjugation, before : String, verb : String) : String =
  {
    def endings = endingsAEI(verb)

    val withChangedStem = YO_CHANGE_MAP(verb)
    if (conjugation.pn == 1) {
      verb match {
        case YoIrregularMatch(base) => {
          val rebase = {
            if (!before.isEmpty && base.endsWith("n")) {
              accentFirstVowel(base)
            } else {
              base
            }
          }
          endReflexive(conjugation, before) + rebase + {
            conjugation.toBeReflexive(conjugation.pn)
          }
        }
        case _ => {
          val vowel = {
            if (endVowel(verb) == 'a') {
              "a"
            } else {
              "e"
            }
          }
          endReflexive(conjugation, before + root(changeStem(verb))) + {
            vowel + conjugation.toBeReflexive(conjugation.pn)
          }
        }
      }
    } else if ((conjugation.pn == 3) &&
      conjugation.toBeReflexive.head.nonEmpty
    ) {
      before + withChangedStem.dropRight(1) + {
        if (endings.head == "e") {
          "émonos"
        } else {
          "ámonos"
        }
      }
    } else if (conjugation.pn == 4) {
      if (conjugation.toBeReflexive.head.nonEmpty) {
        root(conjugation.original) + "íos"
      } else {
        form(conjugation, conjugation.original.dropRight(1) + "d")
      }
    } else {
      endReflexive(conjugation, before + withChangedStem.dropRight(1)) + {
        endings(conjugation.pn) + conjugation.toBeReflexive(conjugation.pn)
      }
    }
  }
}
