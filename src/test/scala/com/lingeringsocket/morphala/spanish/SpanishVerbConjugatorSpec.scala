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

class SpanishVerbConjugatorSpec extends Specification
{
  "SpanishVerbConjugator with SpanishPresentIndicative" should
  {
    "conjugate regular verbs" in
    {
      SpanishVerbConjugator.conjugate(
        "hablar", SpanishPresentIndicative, 0, false
      ) must be equalTo(
        "hablo"
      )
      SpanishVerbConjugator.conjugate(
        "andar", SpanishPresentIndicative, 2, true
      ) must be equalTo(
        "andan"
      )
      SpanishVerbConjugator.conjugate(
        "averiguar", SpanishPresentIndicative, 0, false
      ) must be equalTo(
        "averiguo"
      )
    }

    "conjugate unsystematic irregular verbs" in
    {
      SpanishVerbConjugator.conjugate(
        "estar", SpanishPresentIndicative, 0, false
      ) must be equalTo(
        "estoy"
      )
      SpanishVerbConjugator.conjugate(
        "ser", SpanishPresentIndicative, 2, true
      ) must be equalTo(
        "son"
      )
    }

    "conjugate yo irregular verbs" in
    {
      SpanishVerbConjugator.conjugate(
        "hacer", SpanishPresentIndicative, 0, false
      ) must be equalTo(
        "hago"
      )
      SpanishVerbConjugator.conjugate(
        "hacer", SpanishPresentIndicative, 1, false
      ) must be equalTo(
        "haces"
      )
    }

    "conjugate stem changing verbs" in
    {
      SpanishVerbConjugator.conjugate(
        "tender", SpanishPresentIndicative, 2, false
      ) must be equalTo(
        "tiende"
      )
      SpanishVerbConjugator.conjugate(
        "pedir", SpanishPresentIndicative, 0, false
      ) must be equalTo(
        "pido"
      )
      SpanishVerbConjugator.conjugate(
        "dormir", SpanishPresentIndicative, 2, true
      ) must be equalTo(
        "duermen"
      )
      SpanishVerbConjugator.conjugate(
        "dormir", SpanishPresentIndicative, 0, true
      ) must be equalTo(
        "dormimos"
      )
      SpanishVerbConjugator.conjugate(
        "seguir", SpanishPresentIndicative, 0, false
      ) must be equalTo(
        "sigo"
      )
      SpanishVerbConjugator.conjugate(
        "seguir", SpanishPresentIndicative, 1, false
      ) must be equalTo(
        "sigues"
      )
    }

    "conjugate cer and cir verbs" in
    {
      SpanishVerbConjugator.conjugate(
        "conocer", SpanishPresentIndicative, 0, false
      ) must be equalTo(
        "conozco"
      )
      SpanishVerbConjugator.conjugate(
        "conocer", SpanishPresentIndicative, 1, false
      ) must be equalTo(
        "conoces"
      )
    }

    "conjugate uir verbs" in
    {
      SpanishVerbConjugator.conjugate(
        "contribuir", SpanishPresentIndicative, 0, false
      ) must be equalTo(
        "contribuyo"
      )
      SpanishVerbConjugator.conjugate(
        "contribuir", SpanishPresentIndicative, 0, true
      ) must be equalTo(
        "contribuimos"
      )
    }
  }

  "SpanishVerbConjugator with SpanishPreterite" should
  {
    "conjugate regular verbs" in
    {
      SpanishVerbConjugator.conjugate(
        "hablar", SpanishPreterite, 0, false
      ) must be equalTo(
        "hablé"
      )
      SpanishVerbConjugator.conjugate(
        "mandar", SpanishPreterite, 0, false
      ) must be equalTo(
        "mandé"
      )
    }

    "conjugate unsystematic irregular verbs" in
    {
      SpanishVerbConjugator.conjugate(
        "andar", SpanishPreterite, 2, true
      ) must be equalTo(
        "anduvieron"
      )
    }

    "conjugate stem changing verbs" in
    {
      SpanishVerbConjugator.conjugate(
        "advertir", SpanishPreterite, 2, false
      ) must be equalTo(
        "advirtió"
      )
      SpanishVerbConjugator.conjugate(
        "dormir", SpanishPreterite, 2, false
      ) must be equalTo(
        "durmió"
      )
    }

    "conjugate uir verbs" in
    {
      SpanishVerbConjugator.conjugate(
        "constituir", SpanishPreterite, 2, false
      ) must be equalTo(
        "constituyó"
      )
    }
  }

  "SpanishVerbConjugator with SpanishImperfectIndicative" should
  {
    "conjugate regular verbs" in
    {
      SpanishVerbConjugator.conjugate(
        "hablar", SpanishImperfectIndicative, 0, false
      ) must be equalTo(
        "hablaba"
      )
    }

    "conjugate unsystematic irregular verbs" in
    {
      SpanishVerbConjugator.conjugate(
        "ser", SpanishImperfectIndicative, 2, true
      ) must be equalTo(
        "eran"
      )
    }
  }

  "SpanishVerbConjugator with SpanishFutureIndicative" should
  {
    "conjugate regular verbs" in
    {
      SpanishVerbConjugator.conjugate(
        "hablar", SpanishFutureIndicative, 0, false
      ) must be equalTo(
        "hablaré"
      )
    }

    "conjugate unsystematic irregular verbs" in
    {
      SpanishVerbConjugator.conjugate(
        "saber", SpanishFutureIndicative, 2, true
      ) must be equalTo(
        "sabrán"
      )
    }
  }

  "SpanishVerbConjugator with SpanishConditional" should
  {
    "conjugate regular verbs" in
    {
      SpanishVerbConjugator.conjugate(
        "hablar", SpanishConditional, 0, false
      ) must be equalTo(
        "hablaría"
      )
    }

    "conjugate unsystematic irregular verbs" in
    {
      SpanishVerbConjugator.conjugate(
        "saber", SpanishConditional, 2, true
      ) must be equalTo(
        "sabrían"
      )
    }
  }

  "SpanishVerbConjugator with SpanishPresentSubjunctive" should
  {
    "conjugate regular verbs" in
    {
      SpanishVerbConjugator.conjugate(
        "hablar", SpanishPresentSubjunctive, 0, false
      ) must be equalTo(
        "hable"
      )
      SpanishVerbConjugator.conjugate(
        "poder", SpanishPresentSubjunctive, 0, true
      ) must be equalTo(
        "podamos"
      )
    }

    "conjugate unsystematic irregular verbs" in
    {
      SpanishVerbConjugator.conjugate(
        "ir", SpanishPresentSubjunctive, 2, true
      ) must be equalTo(
        "vayan"
      )
    }

    "conjugate zar verbs" in
    {
      SpanishVerbConjugator.conjugate(
        "abrazar", SpanishPresentSubjunctive, 2, true
      ) must be equalTo(
        "abracen"
      )
    }

    "conjugate uir verbs" in
    {
      SpanishVerbConjugator.conjugate(
        "constituir", SpanishPresentSubjunctive, 0, true
      ) must be equalTo(
        "constituyamos"
      )
    }
  }

  "SpanishVerbConjugator with SpanishImperfectSubjunctive" should
  {
    "conjugate regular verbs" in
    {
      SpanishVerbConjugator.conjugate(
        "hablar", SpanishImperfectSubjunctive, 0, false
      ) must be equalTo(
        "hablara"
      )
    }

    "conjugate stem changing verbs" in
    {
      SpanishVerbConjugator.conjugate(
        "dormir", SpanishImperfectSubjunctive, 0, false
      ) must be equalTo(
        "durmiera"
      )
    }

    "conjugate unsystematic irregular verbs" in
    {
      SpanishVerbConjugator.conjugate(
        "ir", SpanishImperfectSubjunctive, 2, true
      ) must be equalTo(
        "fueran"
      )
      SpanishVerbConjugator.conjugate(
        "sobreseer", SpanishImperfectSubjunctive, 2, true
      ) must be equalTo(
        "sobreseyeran"
      )
    }
  }

  "SpanishVerbConjugator with SpanishImperative" should
  {
    "conjugate regular verbs" in
    {
      SpanishVerbConjugator.conjugate(
        "hablar", SpanishImperative, 1, true
      ) must be equalTo(
        "hablad"
      )
      SpanishVerbConjugator.conjugate(
        "hablar", SpanishImperative, 2, false
      ) must be equalTo(
        "hable"
      )
    }

    "conjugate unsystematic irregular verbs" in
    {
      SpanishVerbConjugator.conjugate(
        "ir", SpanishImperative, 1, false
      ) must be equalTo(
        "ve"
      )
      SpanishVerbConjugator.conjugate(
        "oír", SpanishImperative, 1, false
      ) must be equalTo(
        "oye"
      )
    }

    "conjugate guar verbs" in
    {
      SpanishVerbConjugator.conjugate(
        "averiguar", SpanishImperative, 1, false
      ) must be equalTo(
        "averigua"
      )
      SpanishVerbConjugator.conjugate(
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
      SpanishParticiple.pastParticiple(
        "hablar"
      ) must be equalTo(
        "hablado"
      )
    }

    "conjugate unsystematic irregular past participle" in
    {
      SpanishParticiple.pastParticiple(
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
      SpanishGerund.gerund(
        "hablar"
      ) must be equalTo(
        "hablando"
      )
    }

    "conjugate unsystematic irregular gerund" in
    {
      SpanishGerund.gerund(
        "ir"
      ) must be equalTo(
        "yendo"
      )
    }

    "conjugate aer to ayendo gerund" in
    {
      SpanishGerund.gerund(
        "atraer"
      ) must be equalTo(
        "atrayendo"
      )
    }

    "conjugate stem changing gerund" in
    {
      SpanishGerund.gerund(
        "dormir"
      ) must be equalTo(
        "durmiendo"
      )
    }
  }
}
