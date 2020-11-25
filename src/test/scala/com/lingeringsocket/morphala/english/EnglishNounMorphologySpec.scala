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
package com.lingeringsocket.morphala.english

import org.specs2.mutable._

import EnglishMorphology._

class EnglishNounMorphologySpec extends Specification
{
  "EnglishMorphology" should
  {
    "pluralize nouns" in
    {
      pluralizeNoun("dog") must be equalTo "dogs"
      pluralizeNoun("man") must be equalTo "men"
      pluralizeNoun("kite") must be equalTo "kites"
      pluralizeNoun("mouse") must be equalTo "mice"
      pluralizeNoun("bus") must be equalTo "buses"
      pluralizeNoun("leaf") must be equalTo "leaves"
      pluralizeNoun("roof") must be equalTo "roofs"
      pluralizeNoun("puppy") must be equalTo "puppies"
      pluralizeNoun("potato") must be equalTo "potatoes"
      pluralizeNoun("analysis") must be equalTo "analyses"
      pluralizeNoun("phenomenon") must be equalTo "phenomena"
    }

    "handle unusual pluralizations" in
    {
      skipped("not working yet")

      pluralizeNoun("moose") must be equalTo "moose"
      pluralizeNoun("cactus") must be equalTo "cacti"
    }
  }
}
