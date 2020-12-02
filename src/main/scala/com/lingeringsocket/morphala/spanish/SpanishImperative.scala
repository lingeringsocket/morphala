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
    input : ConjugationInput) : String =
  {
    val verb = input.verb
    def withStemChange = changeStem(verb)

    verb match {
      case ReflexiveIrregularMatch(conjugated) if (
        input.isReflexive
      ) => {
        conjugated(input.pn - 1)
      }
      case IrregularMatch(conjugated) => {
        conjugated(input.pn - 1)
      }
      case YoChangeMatch(iSuffix) if (
        !(
          (input.pn < 2) &&
            verb.contains("decir") &&
            !verb.equals("decir")
        )
      ) => {
        val (before, after) = verb.splitAt(iSuffix)
        yoForm(input, before, after)
      }
      case IrregularCerCirMatch(_) => {
        val subst = substZC(verb)
        impForm(input, subst, subst, verb, subst)
      }
      case IrregularUirMatch(newRoot) => {
        impForm(input, newRoot, verb, verb, newRoot)
      }
      case IrregularUarIarMatch(newRoot) => {
        impForm(input, newRoot, verb, verb, root(verb))
      }
      case _ => {
        val truncatedGerund = SpanishGerund.truncated(verb)
        impForm(
          input,
          carGarZar(withStemChange),
          carGarZar(verb),
          verb,
          carGarZar(truncatedGerund + ending(verb)))
      }
    }
  }

  private def impForm(
    input : ConjugationInput, withStemChange : String, verb : String,
    originalVerb : String, withSmallChange : String) : String =
  {
    def endings = endingsAEI(originalVerb)

    if (input.pn == 1) {
      changeStem(originalVerb) match {
        case IrregularUirMatch(newRoot) => {
          suffixedForm(newRoot + "e", input)
        }
        case IrregularUarIarMatch(newRoot) => {
          suffixedForm(newRoot + "a", input)
        }
        case IrregularGuarMatch(newRoot) => {
          suffixedForm(newRoot + "ua", input)
        }
        case _ => {
          val vowel = {
            if (endVowel(originalVerb) == 'a') {
              'a'
            } else {
              'e'
            }
          }
          suffixedForm(root(changeStem(originalVerb)) + vowel, input)
        }
      }
    } else if (
      ((input.pn == 2) || (input.pn == 5)) &&
        originalVerb.endsWith("guar")
    ) {
      suffixedForm(
        changeStem(originalVerb).take(originalVerb.size - 3) +
          irregularEndings("guar")(input.pn),
        input)
    } else if (input.pn == 3) {
      if (input.isReflexive) {
        if (endings.head.equals("e")) {
          withSmallChange + "émonos"
        } else {
          withSmallChange + "ámonos"
        }
      } else {
        form(input, withSmallChange, endings)
      }
    } else if (input.pn == 4) {
      vosotrosForm(originalVerb, input)
    } else {
      suffixedForm(withStemChange + endings(input.pn), input)
    }
  }

  private def suffixedForm(
    conjugated : String, input : ConjugationInput) : String =
  {
    if (input.isReflexive) {
      val suffixed = conjugated + input.reflexivePronoun
      adjustStress(conjugated, suffixed)
    } else {
      conjugated
    }
  }

  private def vosotrosForm(verb : String, input : ConjugationInput) : String =
  {
    if (input.isReflexive) {
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
      root(input.originalVerb) + ending
    } else {
      form(input, input.originalVerb.dropRight(1) + "d")
    }
  }

  private def yoForm(
    input : ConjugationInput, before : String, verb : String) : String =
  {
    def endings = endingsAEI(verb)

    val withChangedStem = YO_CHANGE_MAP(verb)
    if (input.pn == 1) {
      verb match {
        case YoIrregularMatch(base) => {
          val rebase = {
            if (!before.isEmpty && base.endsWith("n")) {
              accentFirstVowel(base)
            } else {
              base
            }
          }
          suffixedForm(before + rebase, input)
        }
        case _ => {
          val vowel = {
            if (endVowel(verb) == 'a') {
              "a"
            } else {
              "e"
            }
          }
          suffixedForm(before + root(changeStem(verb)) + vowel, input)
        }
      }
    } else if ((input.pn == 3) && input.isReflexive) {
      before + withChangedStem.dropRight(1) + {
        if (endings.head == "e") {
          "émonos"
        } else {
          "ámonos"
        }
      }
    } else if (input.pn == 4) {
      vosotrosForm(verb, input)
    } else {
      suffixedForm(
        before + withChangedStem.dropRight(1) + endings(input.pn),
        input)
    }
  }
}
