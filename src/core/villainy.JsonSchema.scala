/*
    Villainy, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package villainy

import anticipation.*
import contingency.*
import fulminate.*
import inimitable.*
import jacinta.*
import kaleidoscope.*
import merino.*
import nettlesome.*
import polyvinyl.*
import rudiments.*
import vacuous.*

import scala.compiletime.*

import strategies.throwUnsafely

object JsonSchema:
  case class Property
     (`type`:     String,
      properties: Optional[Map[String, Json]],
      items:      Optional[Map[String, Json]],
      required:   Optional[Set[String]],
      minimum:    Optional[Int],
      maximum:    Optional[Int],
      format:     Optional[String],
      pattern:    Optional[String]):

    def requiredFields: Set[String] = required.or(Set())

    def arrayFields =
      items.let(_.map: (key, value) =>
        key -> value.as[Property].field(requiredFields.contains(key)))

      . or:
          panic(m"Some items were missing")

    def objectFields =
      properties.let(_.map: (key, value) =>
        key -> value.as[Property].field(requiredFields.contains(key)))

      . or:
          panic(m"Some properties were missing")

    def field(required: Boolean): RecordField = `type` match
      case "array"  => RecordField.Record(if required then "array" else "array?", arrayFields)
      case "object" => RecordField.Record(if required then "object" else "object?", objectFields)

      case "string" =>
        val suffix = if required then "" else "?"

        pattern.let(RecordField.Value("pattern"+suffix, _)).or:
          RecordField.Value(format.or("string")+suffix)

      case "integer" =>
        val end = if minimum.absent && maximum.absent then (if required then "" else "?") else "!"

        RecordField.Value
         ("integer"+end, minimum.let(_.toString).or(""), maximum.let(_.toString).or(""))

      case other =>
        RecordField.Value(if required then other else other+"?")

abstract class JsonSchema(val doc: JsonSchemaDoc) extends Schema[Optional[Json], JsonRecord]:
  def access(name: String, json: Optional[Json]): Optional[Json] = json.let: json =>
    json.as[Map[String, Json]].get(name).getOrElse(Unset)

  def make(data: Optional[Json], access: String => Optional[Json] => Any): JsonRecord =
    JsonRecord(data, access)

  def fields: Map[String, RecordField] = unsafely(doc.fields)
