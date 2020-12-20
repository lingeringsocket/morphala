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
package com.lingeringsocket.morphala

import net.sf.extjwnl.dictionary._

import scala.util._

object ExtjwnlDictionaries
{
  val ENGLISH_DICT = Dictionary.getDefaultResourceInstance

  val SPANISH_DICT = Using.resource(
    getClass.getClassLoader.getResourceAsStream(
      "net/sf/extjwnl/data/mcr30/spa/res_properties.xml")
  ) {
    stream => Dictionary.getInstance(stream)
  }
}