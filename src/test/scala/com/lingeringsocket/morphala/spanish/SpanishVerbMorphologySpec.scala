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

import org.specs2.mutable._

import SpanishMorphology._
import SpanishParticiple._
import SpanishGerund._

class SpanishVerbMorphologySpec extends Specification
{
  "SpanishMorphology with SpanishPresentIndicative" should
  {
    "conjugate regular verbs" in
    {
      conjugateVerb(
        "hablar", SpanishPresentIndicative, 0, false
      ) must be equalTo(
        "hablo"
      )
      conjugateVerb(
        "andar", SpanishPresentIndicative, 2, true
      ) must be equalTo(
        "andan"
      )
      conjugateVerb(
        "averiguar", SpanishPresentIndicative, 0, false
      ) must be equalTo(
        "averiguo"
      )
    }

    "conjugate unsystematic irregular verbs" in
    {
      conjugateVerb(
        "estar", SpanishPresentIndicative, 0, false
      ) must be equalTo(
        "estoy"
      )
      conjugateVerb(
        "ser", SpanishPresentIndicative, 2, true
      ) must be equalTo(
        "son"
      )
    }

    "conjugate yo irregular verbs" in
    {
      conjugateVerb(
        "hacer", SpanishPresentIndicative, 0, false
      ) must be equalTo(
        "hago"
      )
      conjugateVerb(
        "hacer", SpanishPresentIndicative, 1, false
      ) must be equalTo(
        "haces"
      )
    }

    "conjugate stem changing verbs" in
    {
      conjugateVerb(
        "tender", SpanishPresentIndicative, 2, false
      ) must be equalTo(
        "tiende"
      )
      conjugateVerb(
        "pedir", SpanishPresentIndicative, 0, false
      ) must be equalTo(
        "pido"
      )
      conjugateVerb(
        "dormir", SpanishPresentIndicative, 2, true
      ) must be equalTo(
        "duermen"
      )
      conjugateVerb(
        "dormir", SpanishPresentIndicative, 0, true
      ) must be equalTo(
        "dormimos"
      )
      conjugateVerb(
        "seguir", SpanishPresentIndicative, 0, false
      ) must be equalTo(
        "sigo"
      )
      conjugateVerb(
        "seguir", SpanishPresentIndicative, 1, false
      ) must be equalTo(
        "sigues"
      )
    }

    "conjugate cer and cir verbs" in
    {
      conjugateVerb(
        "conocer", SpanishPresentIndicative, 0, false
      ) must be equalTo(
        "conozco"
      )
      conjugateVerb(
        "conocer", SpanishPresentIndicative, 1, false
      ) must be equalTo(
        "conoces"
      )
    }

    "conjugate uir verbs" in
    {
      conjugateVerb(
        "contribuir", SpanishPresentIndicative, 0, false
      ) must be equalTo(
        "contribuyo"
      )
      conjugateVerb(
        "contribuir", SpanishPresentIndicative, 0, true
      ) must be equalTo(
        "contribuimos"
      )
    }
  }

  "SpanishMorphology with SpanishPreterite" should
  {
    "conjugate regular verbs" in
    {
      conjugateVerb(
        "hablar", SpanishPreterite, 0, false
      ) must be equalTo(
        "hablé"
      )
      conjugateVerb(
        "mandar", SpanishPreterite, 0, false
      ) must be equalTo(
        "mandé"
      )
    }

    "conjugate unsystematic irregular verbs" in
    {
      conjugateVerb(
        "andar", SpanishPreterite, 2, true
      ) must be equalTo(
        "anduvieron"
      )
    }

    "conjugate stem changing verbs" in
    {
      conjugateVerb(
        "advertir", SpanishPreterite, 2, false
      ) must be equalTo(
        "advirtió"
      )
      conjugateVerb(
        "dormir", SpanishPreterite, 2, false
      ) must be equalTo(
        "durmió"
      )
    }

    "conjugate uir verbs" in
    {
      conjugateVerb(
        "constituir", SpanishPreterite, 2, false
      ) must be equalTo(
        "constituyó"
      )
    }
  }

  "SpanishMorphology with SpanishImperfectIndicative" should
  {
    "conjugate regular verbs" in
    {
      conjugateVerb(
        "hablar", SpanishImperfectIndicative, 0, false
      ) must be equalTo(
        "hablaba"
      )
    }

    "conjugate unsystematic irregular verbs" in
    {
      conjugateVerb(
        "ser", SpanishImperfectIndicative, 2, true
      ) must be equalTo(
        "eran"
      )
    }
  }

  "SpanishMorphology with SpanishFutureIndicative" should
  {
    "conjugate regular verbs" in
    {
      conjugateVerb(
        "hablar", SpanishFutureIndicative, 0, false
      ) must be equalTo(
        "hablaré"
      )
    }

    "conjugate unsystematic irregular verbs" in
    {
      conjugateVerb(
        "saber", SpanishFutureIndicative, 2, true
      ) must be equalTo(
        "sabrán"
      )
    }
  }

  "SpanishMorphology with SpanishConditional" should
  {
    "conjugate regular verbs" in
    {
      conjugateVerb(
        "hablar", SpanishConditional, 0, false
      ) must be equalTo(
        "hablaría"
      )
    }

    "conjugate unsystematic irregular verbs" in
    {
      conjugateVerb(
        "saber", SpanishConditional, 2, true
      ) must be equalTo(
        "sabrían"
      )
    }
  }

  "SpanishMorphology with SpanishPresentSubjunctive" should
  {
    "conjugate regular verbs" in
    {
      conjugateVerb(
        "hablar", SpanishPresentSubjunctive, 0, false
      ) must be equalTo(
        "hable"
      )
      conjugateVerb(
        "poder", SpanishPresentSubjunctive, 0, true
      ) must be equalTo(
        "podamos"
      )
    }

    "conjugate unsystematic irregular verbs" in
    {
      conjugateVerb(
        "ir", SpanishPresentSubjunctive, 2, true
      ) must be equalTo(
        "vayan"
      )
    }

    "conjugate zar verbs" in
    {
      conjugateVerb(
        "abrazar", SpanishPresentSubjunctive, 2, true
      ) must be equalTo(
        "abracen"
      )
    }

    "conjugate uir verbs" in
    {
      conjugateVerb(
        "constituir", SpanishPresentSubjunctive, 0, true
      ) must be equalTo(
        "constituyamos"
      )
    }
  }

  "SpanishMorphology with SpanishImperfectSubjunctive" should
  {
    "conjugate regular verbs" in
    {
      conjugateVerb(
        "hablar", SpanishImperfectSubjunctive, 0, false
      ) must be equalTo(
        "hablara"
      )
    }

    "conjugate stem changing verbs" in
    {
      conjugateVerb(
        "dormir", SpanishImperfectSubjunctive, 0, false
      ) must be equalTo(
        "durmiera"
      )
    }

    "conjugate unsystematic irregular verbs" in
    {
      conjugateVerb(
        "ir", SpanishImperfectSubjunctive, 2, true
      ) must be equalTo(
        "fueran"
      )
      conjugateVerb(
        "sobreseer", SpanishImperfectSubjunctive, 2, true
      ) must be equalTo(
        "sobreseyeran"
      )
    }
  }

  "SpanishMorphology with SpanishImperative" should
  {
    "conjugate regular verbs" in
    {
      conjugateVerb(
        "hablar", SpanishImperative, 1, true
      ) must be equalTo(
        "hablad"
      )
      conjugateVerb(
        "hablar", SpanishImperative, 2, false
      ) must be equalTo(
        "hable"
      )
    }

    "conjugate unsystematic irregular verbs" in
    {
      conjugateVerb(
        "ir", SpanishImperative, 1, false
      ) must be equalTo(
        "ve"
      )
      conjugateVerb(
        "oír", SpanishImperative, 1, false
      ) must be equalTo(
        "oye"
      )
    }

    "conjugate guar verbs" in
    {
      conjugateVerb(
        "averiguar", SpanishImperative, 1, false
      ) must be equalTo(
        "averigua"
      )
      conjugateVerb(
        "averiguar", SpanishImperative, 2, false
      ) must be equalTo(
        "averigüe"
      )
    }
  }

  "SpanishParticiple" should
  {
    "conjugate regular past participle" in
    {
      pastParticiple(
        "hablar"
      ) must be equalTo(
        "hablado"
      )
    }

    "conjugate unsystematic irregular past participle" in
    {
      pastParticiple(
        "ver"
      ) must be equalTo(
        "visto"
      )
    }
  }

  "SpanishGerund" should
  {
    "conjugate regular gerund" in
    {
      gerund(
        "hablar"
      ) must be equalTo(
        "hablando"
      )
    }

    "conjugate unsystematic irregular gerund" in
    {
      gerund(
        "ir"
      ) must be equalTo(
        "yendo"
      )
    }

    "conjugate aer to ayendo gerund" in
    {
      gerund(
        "atraer"
      ) must be equalTo(
        "atrayendo"
      )
    }

    "conjugate stem changing gerund" in
    {
      gerund(
        "dormir"
      ) must be equalTo(
        "durmiendo"
      )
    }
  }
}
