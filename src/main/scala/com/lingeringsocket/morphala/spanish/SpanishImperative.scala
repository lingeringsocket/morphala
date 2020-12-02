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
        conjugated(conjugation.pn - 1)
      }
      case IrregularMatch(conjugated) => {
        conjugated(conjugation.pn - 1)
      }
      case YoChangeMatch(iSuffix) if (
        !(
          (conjugation.pn < 2) &&
            verb.contains("decir") &&
            !verb.equals("decir")
        )
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
          suffixedForm(newRoot + "e", conjugation)
        }
        case IrregularUarIarMatch(newRoot) => {
          suffixedForm(newRoot + "a", conjugation)
        }
        case IrregularGuarMatch(newRoot) => {
          suffixedForm(newRoot + "ua", conjugation)
        }
        case _ => {
          val vowel = {
            if (endVowel(originalVerb) == 'a') {
              'a'
            } else {
              'e'
            }
          }
          suffixedForm(root(changeStem(originalVerb)) + vowel, conjugation)
        }
      }
    } else if (
      ((conjugation.pn == 2) || (conjugation.pn == 5)) &&
        originalVerb.endsWith("guar")
    ) {
      suffixedForm(
        changeStem(originalVerb).take(originalVerb.size - 3) +
          irregularEndings("guar")(conjugation.pn),
        conjugation)
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
      vosotrosForm(originalVerb, conjugation)
    } else {
      suffixedForm(withStemChange + endings(conjugation.pn), conjugation)
    }
  }

  private def suffixedForm(
    conjugated : String, conjugation : Conjugation) : String =
  {
    if (conjugation.toBeReflexive.head.nonEmpty) {
      val suffixed =
        conjugated + conjugation.toBeReflexive(conjugation.pn).stripSuffix(" ")
      adjustStress(conjugated, suffixed)
    } else {
      conjugated
    }
  }

  private def vosotrosForm(verb : String, conjugation : Conjugation) : String =
  {
    if (conjugation.toBeReflexive.head.nonEmpty) {
      val ending = {
        val ev = endVowel(verb)
        if (ev == 'i') {
          "íos"
        } else if (verb == "mover") {
          // according to Fred Jehle, although spanishdict.com disagrees
          "íos"
        } else {
          s"${ev}os"
        }
      }
      root(conjugation.original) + ending
    } else {
      form(conjugation, conjugation.original.dropRight(1) + "d")
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
          suffixedForm(before + rebase, conjugation)
        }
        case _ => {
          val vowel = {
            if (endVowel(verb) == 'a') {
              "a"
            } else {
              "e"
            }
          }
          suffixedForm(before + root(changeStem(verb)) + vowel, conjugation)
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
      vosotrosForm(verb, conjugation)
    } else {
      suffixedForm(
        before + withChangedStem.dropRight(1) + endings(conjugation.pn),
        conjugation)
    }
  }
}
