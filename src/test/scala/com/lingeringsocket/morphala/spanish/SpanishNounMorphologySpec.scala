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

class SpanishNounMorphologySpec extends Specification
{
  "SpanishMorphology" should
  {
    "pluralize nouns" in
    {
      pluralizeNoun("perro") must be equalTo "perros"
      pluralizeNoun("casa") must be equalTo "casas"
      pluralizeNoun("color") must be equalTo "colores"
      pluralizeNoun("avión") must be equalTo "aviones"
      pluralizeNoun("actriz") must be equalTo "actrices"
      pluralizeNoun("frac") must be equalTo "fraques"
      pluralizeNoun("zigzag") must be equalTo "zigzagues"
      pluralizeNoun("lunes") must be equalTo "lunes"
      pluralizeNoun("tórax") must be equalTo "tórax"
      pluralizeNoun("inglés") must be equalTo "ingleses"
      pluralizeNoun("sofá") must be equalTo "sofás"
      pluralizeNoun("esquí") must be equalTo "esquíes"

      // in certain cases, stress position changes
      pluralizeNoun("carácter") must be equalTo "caracteres"
      pluralizeNoun("espécimen") must be equalTo "especímenes"
      pluralizeNoun("régimen") must be equalTo "regímenes"

      // otherwise, preserve stress position
      // FIXME:  not working yet

      // pluralizeNoun("imagen") must be equalTo "imágenes"
    }
  }
}
