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

import net.sf.extjwnl.data._

import org.specs2.mutable._

import scala.util._
import scala.collection._
import scala.jdk.CollectionConverters._

import java.io._

import MorphalaUtils._

class SpanishVerbExternalSpec extends Specification
{
  import SpanishVerbExternalSpec._
  import WordnetDictionaries._

  private def parseLine(line : String) : Seq[String] =
  {
    line.split("\",\"").toIndexedSeq.map(_.stripPrefix("\"").stripSuffix("\""))
  }

  private def readJehleLines : Seq[String] =
  {
    val lines = Using.resource(resourceAsSource("jehle_verb_database.csv")) {
      _.getLines().toSeq
    }
    val header = lines.head
    val headings = parseLine(header)
    headings must be equalTo Seq(
      "infinitive",
      "infinitive_english",
      "mood",
      "mood_english",
      "tense",
      "tense_english",
      "verb_english",
      "form_1s",
      "form_2s",
      "form_3s",
      "form_1p",
      "form_2p",
      "form_3p",
      "gerund",
      "gerund_english",
      "pastparticiple",
      "pastparticiple_english"
    )
    lines.tail
  }

  private def parseInfinitive(entry : String) : String =
  {
    entry.stripSuffix("(se)") match {
      case "vomit" => "vomitar"
      case x => x
    }
  }

  private def parseTam(row : Seq[String]) : Option[SpanishVerbConjugator] =
  {
    val time = row(4)
    row(2) match {
      case "Indicativo" => {
        time match {
          case "Futuro" => Some(SpanishFutureIndicative)
          case "Pretérito" => Some(SpanishPreterite)
          case "Presente" => Some(SpanishPresentIndicative)
          case "Imperfecto" => Some(SpanishImperfectIndicative)
          case "Condicional" => Some(SpanishConditional)
          case _ => None
        }
      }
      case "Subjuntivo" => {
        time match {
          case "Imperfecto" => Some(SpanishImperfectSubjunctive)
          case "Presente" => Some(SpanishPresentSubjunctive)
          case _ => None
        }
      }
      case "Imperativo Afirmativo" => {
        Some(SpanishImperative)
      }
      case _ => None
    }
  }

  private def extractForms(
    seq : Seq[String], conjugator : SpanishVerbConjugator) : Seq[String] =
  {
    assert(seq.size == 6)
    if (conjugator == SpanishImperative) {
      // the database uses a different row order for imperative
      Seq(seq(0), seq(1), seq(4), seq(3), seq(2), seq(5))
    } else {
      seq
    }
  }

  private def extractParticiple(entry : String) : String =
  {
    entry.split(", ").head
  }

  private def lastWord(s : String) : String =
  {
    s.split(" ").last
  }

  "SpanishMorphology" should
  {
    "conjugate verb database" in
    {
      val DEBUG = false

      val data = readJehleLines
      var count = 0
      data.foreach(line => {
        if (DEBUG) {
          println(line)
          println()
        }
        val row = parseLine(line)
        row.size must be equalTo 17
        val infinitive = parseInfinitive(row(0))
        val conjugatorOpt = parseTam(row)
        if (conjugatorOpt.nonEmpty) {
          val conjugator = conjugatorOpt.get
          val formGerund = row(13)
          val formParticiple = extractParticiple(row(15))
          val conjugatedGerund =
            SpanishGerund.gerund(infinitive)
          val conjugatedParticiple =
            SpanishParticiple.pastParticiple(infinitive)
          conjugatedGerund must be equalTo formGerund
          conjugatedParticiple must be equalTo formParticiple
          val coords = Seq(false, true).flatMap(plural => {
            Seq.range(0, 3).map(person => {
              (plural, person)
            })
          })
          val forms = extractForms(row.slice(7, 13), conjugator)
          forms.zip(coords).foreach {
            case (form, (plural, person)) => {
              if (DEBUG) {
                println("TAM = " + conjugator)
                println("PERSON (0-based) = " + person)
                println("PLURAL = " + plural)
                println()
              }
              if ((person == 0) && (conjugator == SpanishImperative)) {
                form must beEmpty
              } else if (
                (infinitive == "secarse") &&
                  (conjugator == SpanishImperative)
              ) {
                // skip this one since the database has an error
              } else {
                val conjugated = SpanishMorphology.conjugateVerb(
                  infinitive,
                  conjugator,
                  person,
                  plural
                )
                val normalized = {
                  if (conjugated.startsWith("os ") && !form.startsWith("os ")) {
                    // ignore some inconsistencies in the verb database
                    lastWord(conjugated)
                  } else {
                    conjugated
                  }
                }
                val normalizedForm = {
                  form.stripPrefix("no te ")
                }
                if (form.nonEmpty) {
                  normalized must be equalTo(normalizedForm)
                }
              }
            }
          }
        }
        count += 1
      })
      count must be equalTo JEHLE_LINE_COUNT
    }

    "verify irregular forms" in
    {
      val infinitives = Using.resource(
        resourceAsSource("verbos-espanol.txt")
      ) {
        _.getLines().map(_.stripSuffix("se")).toSeq
      }

      val conjugations = Using.resource(
        resourceAsSource("verbos-espanol-conjugaciones.txt")
      ) {
        _.getLines().map(
          _.replace("ándome", "ando").replace("éndome", "endo")
        ).toSet
      } ++ Set(
        "invirtéramos"
      )

      val coords = SUPPORTED_TAMS.flatMap(conjugator => {
        Seq(false, true).flatMap(plural => {
          Seq.range(0, 3).map(person => {
            (conjugator, plural, person)
          })
        })
      }).filter {
        case (SpanishImperative, _, 2) => {
          true
        }
        case (SpanishImperative, _, _) => {
          false
        }
        case _ => {
          true
        }
      }

      val ignored = UNTRIAGED ++ OBSCURE

      var errors = 0
      val newIrregulars = new mutable.TreeMap[String, String]

      def checkConjugation(
        form : String, infinitive : String, conjugated : String) : Unit =
      {
        if (!conjugations.contains(conjugated)) {
          println(s"INCORRECT $form $infinitive $conjugated")
          errors += 1
        } else if (hasVerbSenses(infinitive)) {
          val bases = SPANISH_DICT.getMorphologicalProcessor.lookupAllBaseForms(
            POS.VERB, conjugated).asScala.toSet
          if (!bases.contains(infinitive)) {
            newIrregulars.put(conjugated, infinitive)
          }
        }
      }

      infinitives.filterNot(ignored.contains).foreach(infinitive => {
        coords.foreach {
          case coord @ (conjugator, plural, person) => {
            val conjugated = SpanishMorphology.conjugateVerb(
              infinitive,
              conjugator,
              person,
              plural
            )
            checkConjugation(s"$coord", infinitive, conjugated)
          }
        }

        val progressive = SpanishGerund.gerund(infinitive)
        checkConjugation("PROGRESSIVE", infinitive, progressive)

        val participle = SpanishParticiple.pastParticiple(infinitive)
        checkConjugation("PARTICIPLE", infinitive, participle)
      })

      errors must be equalTo 0
      if (newIrregulars.nonEmpty) {
        val file = "irregular_verb_forms.txt"
        Using.resource(new PrintWriter(new FileWriter(file))) {
          pw => {
            newIrregulars.foreach {
              case (conjugated, infinitive) => {
                pw.println(s"v#$conjugated -addexc $infinitive")
              }
            }
          }
        }
        println(s"New irregular forms written to $file")
        println("Please review and merge into")
        println("extjwnl/supplemental_spa.txt")
      }
      newIrregulars.size must be equalTo 0
    }
  }
}

object SpanishVerbExternalSpec
{
  import WordnetDictionaries._

  private val JEHLE_LINE_COUNT = 11466

  private val SUPPORTED_TAMS = Seq(
    SpanishPresentIndicative,
    SpanishPresentSubjunctive,
    SpanishPreterite,
    SpanishImperfectIndicative,
    SpanishImperfectSubjunctive,
    SpanishConditional,
    SpanishFutureIndicative,
    SpanishImperative
  )

  def hasVerbSenses(lemma : String) : Boolean =
  {
    Option(SPANISH_DICT.getIndexWord(POS.VERB, lemma)) match {
      case Some(indexWord) => !indexWord.getSenses.isEmpty
      case _ => false
    }
  }

  private val UNTRIAGED = Set(
    "desleír",
    "desoír",
    "desvaír",
    "embaír",
    "engreír",
    "entreoír",
    "trasoír",
    "freír",
    "sofreír",
    "sonreír",
    "refreír",
    "reír",
    "abar",
    "abducir",
    "abocanar",
    "abolir",
    "acaecer",
    "acantalear",
    "achichiguar",
    "acontecer",
    "acrecentar",
    "acupear",
    "adherir",
    "adir",
    "adormir",
    "adquirir",
    "aducir",
    "adulcir",
    "agüitar",
    "ahijar",
    "ahilar",
    "ahincar",
    "ahitar",
    "ahuchar",
    "ahumar",
    "ahusar",
    "airar",
    "aislar",
    "alborecer",
    "algaracear",
    "aloquecer",
    "amelarchiar",
    "amohinar",
    "antever",
    "anticuar",
    "antojar",
    "apirgüinar",
    "arcaizar",
    "argüendear",
    "argüir",
    "arrempujar",
    "asentir",
    "atardecer",
    "ataucar",
    "atañer",
    "aterir",
    "atraillar",
    "aullar",
    "aunar",
    "aupar",
    "autoinducir",
    "avergonzar",
    "balbucir",
    "baraustar",
    "brisar",
    "cabrahigar",
    "caler",
    "cascarrinar",
    "cellisquear",
    "cercear",
    "cernir",
    "chaparrear",
    "chiar",
    "chipiar",
    "ciar",
    "circunferir",
    "clarecer",
    "clorformar",
    "cocer",
    "coextender",
    "cohibir",
    "coitar",
    "colidir",
    "compungir",
    "concernir",
    "concordar",
    "condecir",
    "conferir",
    "conseguir",
    "consentir",
    "consolar",
    "consonar",
    "contradecir",
    "contrahacer",
    "controvertir",
    "contumeriar",
    "coproducir",
    "correntiar",
    "criar",
    "deducir",
    "deferir",
    "degollar",
    "delinquir",
    "denegar",
    "derrenegar",
    "desabrir",
    "desadvertir",
    "desahijar",
    "desahitar",
    "desahumar",
    "desainar",
    "desaislar",
    "desalentar",
    "desandar",
    "desarrendar",
    "desasentar",
    "desasir",
    "desatender",
    "desatentar",
    "desaterrar",
    "desatraillar",
    "descafeinar",
    "descerrar",
    "desceñir",
    "descimentar",
    "descomedir",
    "desconsentir",
    "desconsolar",
    "desdar",
    "desdecir",
    "desdentar",
    "desembaular",
    "desempedrar",
    "desencerrar",
    "desenraizar",
    "desentender",
    "desenterrar",
    "desfacer",
    "desfruncir",
    "deshacer",
    "deshelar",
    "desinvertir",
    "deslendrar",
    "deslucir",
    "desmedir",
    "desmelar",
    "desmembrar",
    "desmentir",
    "descocer",
    "desnegar",
    "desosar",
    "despedrar",
    "despernar",
    "despezar",
    "desplegar",
    "desraizar",
    "desterrar",
    "desteñir",
    "desuncir",
    "desventar",
    "desvergonzar",
    "desvestir",
    "diferir",
    "digerir",
    "diluviar",
    "discernir",
    "disentir",
    "distinguir",
    "educir",
    "embaular",
    "embestir",
    "empecer",
    "empedernir",
    "empedrar",
    "empigüelar",
    "empleitar",
    "encabrahigar",
    "encelajar",
    "encerrar",
    "enchagüitar",
    "encomendar",
    "endentar",
    "enfuriar",
    "engüerar",
    "enhestar",
    "enlenzar",
    "enlucir",
    "enmelar",
    "enmendar",
    "enraizar",
    "ensangrentar",
    "ensarmentar",
    "enterrar",
    "entrecerrar",
    "entredecir",
    "entredormir",
    "entrelucir",
    "entrepernar",
    "entreuntar",
    "entrever",
    "envelar",
    "enzainar",
    "erguir",
    "escarmentar",
    "escocer",
    "esparcir",
    "estajar",
    "estarcir",
    "estatuar",
    "estreñir",
    "europeizar",
    "evanescer",
    "expedir",
    "extinguir",
    "fiar",
    "fluir",
    "fosforescer",
    "fruir",
    "fruncir",
    "garantir",
    "garuar",
    "granizar",
    "gruir",
    "guiar",
    "hacer",
    "harinear",
    "hebraizar",
    "hendir",
    "herventar",
    "herver",
    "huir",
    "inferir",
    "ingerir",
    "injerir",
    "inquirir",
    "inserir",
    "inteligenciar",
    "interdecir",
    "interferir",
    "judaizar",
    "juncir",
    "juñir",
    "langüetear",
    "lengüetear",
    "liar",
    "lucir",
    "luir",
    "magiar",
    "malentender",
    "malherir",
    "manferir",
    "marcir",
    "maullar",
    "mecer",
    "melengüelear",
    "miar",
    "mollinear",
    "molliznar",
    "molliznear",
    "muir",
    "neblinear",
    "nerviar",
    "neviscar",
    "oblicuar",
    "obstar",
    "orvallar",
    "parahusar",
    "paramar",
    "paramear",
    "peneirar",
    "perniquebrar",
    "perseguir",
    "pervertir",
    "piar",
    "pintear",
    "podrir",
    "preconcebir",
    "predecir",
    "preelegir",
    "prelucir",
    "premorir",
    "presentir",
    "preterir",
    "prever",
    "proferir",
    "prohijar",
    "proseguir",
    "puar",
    "pubescer",
    "raer",
    "raizar",
    "raspahilar",
    "reargüir",
    "reaventar",
    "recalentar",
    "recentar",
    "receñir",
    "recocer",
    "recolegir",
    "reconducir",
    "reconvertir",
    "redargüir",
    "redecir",
    "reelegir",
    "reexpedir",
    "referir",
    "refregar",
    "regimentar",
    "regoldar",
    "rehacer",
    "rehenchir",
    "reherir",
    "rehervir",
    "rehilar",
    "rehuir",
    "rehundir",
    "rehurtar",
    "reilar",
    "relucir",
    "remecer",
    "remedir",
    "remendar",
    "renegar",
    "repensar",
    "replegar",
    "repodrir",
    "reproducir",
    "requebrar",
    "resaber",
    "resarcir",
    "reseguir",
    "resentir",
    "respahilar",
    "resquebrar",
    "restregar",
    "retentar",
    "reteñir",
    "retraducir",
    "reundir",
    "reunir",
    "reuntar",
    "reventar",
    "rever",
    "revertir",
    "revestir",
    "rezurcir",
    "ruar",
    "ruñir",
    "sahumar",
    "sainar",
    "satisfacer",
    "seducir",
    "sementar",
    "sobrecalentar",
    "sobreentender",
    "sobrehilar",
    "sobrentender",
    "sobresolar",
    "sobrevestir",
    "soler",
    "sonar",
    "sorripiar",
    "subdistinguir",
    "subentender",
    "subseguir",
    "subvertir",
    "superentender",
    "tardecer",
    "terapiar",
    "traillar",
    "transferir",
    "translucir",
    "trasferir",
    "traslucir",
    "trasver",
    "triar",
    "tropezar",
    "tumultuar",
    "uncir",
    "usucapir",
    "ventiscar",
    "ventisquear",
    "zaherir",
    "zaracear",
    "zurcir"
  )

  private val OBSCURE = Set(
    "chiviar",
    "embutiar",
    "zurriar",
    "contorcer",
    "arrecir",
    "inhestar",
    "rocear"
  )
}
