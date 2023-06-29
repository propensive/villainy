/*
    Villainy, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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

import polyvinyl.*
import rudiments.*
import digression.*
import jacinta.*
import spectacular.*
import merino.*

import scala.compiletime.*

import unsafeExceptions.canThrowAny

object JsonRecord:
  given ValueAccessor[JsonRecord, JsonAst, "boolean", Boolean] = _.boolean
  given ValueAccessor[JsonRecord, JsonAst, "string", Text] = _.string
  given ValueAccessor[JsonRecord, JsonAst, "integer", Int] = _.long.toInt
  
  given ValueAccessor[JsonRecord, JsonAst, "number", Double] = _.asMatchable match
    case long: Long          => long.toDouble
    case decimal: BigDecimal => decimal.toDouble
    case double: Double      => double
    case _                   => throw JsonSchemaError()

  given RecordAccessor[JsonRecord, JsonAst, "array", IArray] = _.array.map(_)
  given RecordAccessor[JsonRecord, JsonAst, "object", [T] =>> T] = (value, make) => make(value)
  
class JsonRecord(data: JsonAst, access: String => JsonAst => Any)
extends Record[JsonAst](data, access)

case class JsonSchemaDoc
    (`$schema`: Text, `$id`: Text, title: Text, `type`: Text,
        properties: Map[String, JsonSchema.Property]):
  def fields: Map[String, RecordField] = properties.view.mapValues(_.field).to(Map)

object JsonSchema:
  case class Property
      (`type`: String, properties: Option[Map[String, Json]], items: Option[Map[String, Json]]):
    
    def arrayFields = items.get.view.mapValues(_.as[Property].field).to(Map)
    def objectFields = properties.get.view.mapValues(_.as[Property].field).to(Map)
    
    def field: RecordField = `type` match
      case "array"  => RecordField.Record("array", arrayFields)
      case "object" => RecordField.Record("object", objectFields)
      case other    => RecordField.Value(other)

abstract class JsonSchema(val doc: JsonSchemaDoc) extends Schema[JsonAst, JsonRecord]:
  def access(name: String, json: JsonAst): JsonAst = json.obj match
    case (keys: IArray[String], values: IArray[JsonAst]) => values(keys.indexOf(name))

  def make(data: JsonAst, access: String => JsonAst => Any): JsonRecord = JsonRecord(data, access)
  def fields: Map[String, RecordField] = unsafely(doc.fields)

case class JsonSchemaError() extends Error(err"there was an error in the JSON schema")