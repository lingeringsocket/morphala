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

import SpanishVerbConjugator._
import SpanishUtils._

class IrregularMap(
  map : Map[String, String]
){
  def unapply(infinitive : String) : Option[Int] =
  {
    findSuffix(infinitive, map.keySet, IRREGULAR_REQUIRE_FULL) match {
      case -1 => None
      case i => Some(i)
    }
  }
}

class IrregularExactMap(
  map : Map[String, String]
){
  def unapply(infinitive : String) : Option[String] =
  {
    map.get(infinitive)
  }
}

class IrregularArrayMap(
  map : Map[String, List[String]]
){
  def unapply(infinitive : String) : Option[Array[String]] =
  {
    map.get(infinitive).map(_.toArray)
  }
}

object IrregularCerCirMatch
{
  val SUFFIXES = Set("cer", "cir")

  def unapply(infinitive : String) : Option[Array[String]] =
  {
    val suffix = infinitive.takeRight(3)
    if (SUFFIXES.contains(suffix)) {
      Some(Array(
        substZC(infinitive).substring(infinitive.size - 3) + "o",
        "ces", "ce", "cemos", "céis", "cen"
      ))
    } else {
      None
    }
  }
}

object IrregularUirMatch
{
  def unapply(infinitive : String) : Option[String] =
  {
    if (infinitive.endsWith("uir")) {
      Some(root(infinitive) + "y")
    } else {
      None
    }
  }
}

object IrregularUarIarMatch
{
  def unapply(infinitive : String) : Option[String] =
  {
    if (
      infinitive.endsWith("uar") &&
        !"cg".contains(infinitive.takeRight(4).head)
    ) {
      Some(infinitive.dropRight(3) + "ú")
    } else if (IAR_IRREGULAR.contains(infinitive)) {
      Some(infinitive.dropRight(3) + "í")
    } else {
      None
    }
  }
}

object IrregularUcirMatch
{
  def unapply(infinitive : String) : Option[String] =
  {
    if (infinitive.endsWith("ucir")) {
      Some(infinitive.dropRight(3) + "j")
    } else {
      None
    }
  }
}

object IrregularPreteriteStemMatch
{
  def unapply(infinitive : String) : Option[String] =
  {
    if (endVowel(infinitive) == 'i') {
      val withStemChange = changeStem(infinitive, "i", "u")
      if (withStemChange == infinitive) {
        None
      } else {
        Some(withStemChange)
      }
    } else {
      None
    }
  }
}

object IrregularCarGarZarMatch
{
  def unapply(infinitive : String) : Option[String] =
  {
    if (infinitive.endsWith("car")) {
      Some(infinitive.dropRight(3) + "qu")
    } else if (infinitive.endsWith("gar")) {
      Some(infinitive.dropRight(3) + "gu")
    } else if (infinitive.endsWith("zar")) {
      Some(infinitive.dropRight(3) + "c")
    } else {
      None
    }
  }
}

object IrregularYMatch
{
  def unapply(infinitive : String) : Option[String] =
  {
    if ("aeio".contains(infinitive.takeRight(3).head) &&
      (endVowel(infinitive) != 'a')
    ) {
      Some(root(infinitive))
    } else {
      None
    }
  }
}

object IrregularUirUerMatch
{
  def unapply(infinitive : String) : Option[String] =
  {
    if ((infinitive.takeRight(3).head == 'u') &&
      (endVowel(infinitive) != 'a')
    ) {
      Some(root(infinitive))
    } else {
      None
    }
  }
}

object IrregularGuarMatch
{
  def unapply(infinitive : String) : Option[String] =
  {
    if (infinitive.endsWith("guar")) {
      Some(infinitive.dropRight(3))
    } else {
      None
    }
  }
}
