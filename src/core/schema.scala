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
  given boolean: ValueAccessor[JsonRecord, Maybe[JsonAst], "boolean", Boolean] = _.avow.boolean
  given string: ValueAccessor[JsonRecord, Maybe[JsonAst], "string", Text] = _.avow.string
  given integer: ValueAccessor[JsonRecord, Maybe[JsonAst], "integer", Int] = _.avow.long.toInt
  
  given number: ValueAccessor[JsonRecord, Maybe[JsonAst], "number", Double] = _.avow.asMatchable match
    case long: Long          => long.toDouble
    case decimal: BigDecimal => decimal.toDouble
    case double: Double      => double
    case _                   => throw JsonSchemaError()

  given array: RecordAccessor[JsonRecord, Maybe[JsonAst], "array", IArray] = _.avow.array.map(_)
  given obj: RecordAccessor[JsonRecord, Maybe[JsonAst], "object", [T] =>> T] = (value, make) => make(value.avow)
  
  given maybeBoolean: ValueAccessor[JsonRecord, Maybe[JsonAst], "boolean?", Maybe[Boolean]] = _.mm(_.boolean)
  given maybeString: ValueAccessor[JsonRecord, Maybe[JsonAst], "string?", Maybe[Text]] = _.mm(_.string)
  given maybeInteger: ValueAccessor[JsonRecord, Maybe[JsonAst], "integer?", Maybe[Int]] = _.mm(_.long.toInt)
  
  given maybeNumber: ValueAccessor[JsonRecord, Maybe[JsonAst], "number?", Maybe[Double]] = _.mm: value =>
    value.asMatchable match
      case long: Long          => long.toDouble
      case decimal: BigDecimal => decimal.toDouble
      case double: Double      => double
      case _                   => throw JsonSchemaError()

  given maybeArray: RecordAccessor[JsonRecord, Maybe[JsonAst], "array?", [T] =>> Maybe[IArray[T]]] = (value, make) =>
    value.mm(_.array.map(make))
  
  given maybeObject: RecordAccessor[JsonRecord, Maybe[JsonAst], "object?", [T] =>> Maybe[T]] = (value, make) =>
    value.mm(make(_))

class JsonRecord(data: Maybe[JsonAst], access: String => Maybe[JsonAst] => Any)
extends Record[Maybe[JsonAst]](data, access)

case class JsonSchemaDoc
    (`$schema`: Text, `$id`: Text, title: Text, `type`: Text,
        properties: Map[String, JsonSchema.Property], required: Maybe[Set[String]]):
  lazy val requiredFields: Set[String] = required.or(Set())
  def fields: Map[String, RecordField] = properties.map { (k, v) => k -> v.field(requiredFields.contains(k)) }

object JsonSchema:
  case class Property
      (`type`: String, properties: Maybe[Map[String, Json]], items: Maybe[Map[String, Json]], required: Maybe[Set[String]]):
    def requiredFields: Set[String] = required.or(Set())
    
    def arrayFields = items.mm(_.map { (k, v) => k -> v.as[Property].field(requiredFields.contains(k)) }).or(throw Mistake("missing items"))
    def objectFields = properties.mm(_.map { (k, v) => k -> v.as[Property].field(requiredFields.contains(k)) }).or(throw Mistake("missing properties"))
    
    def field(required: Boolean): RecordField = `type` match
      case "array"  => RecordField.Record(if required then "array" else "array?", arrayFields)
      case "object" => RecordField.Record(if required then "object" else "object?", objectFields)
      case other    => RecordField.Value(if required then other else other+"?")

abstract class JsonSchema(val doc: JsonSchemaDoc) extends Schema[Maybe[JsonAst], JsonRecord]:
  def access(name: String, json: Maybe[JsonAst]): Maybe[JsonAst] = json.mm: json =>
    json.obj match
      case (keys: IArray[String], values: IArray[Maybe[JsonAst]]) => keys.indexOf(name) match
        case -1    => Unset
        case index => values(index)

  def make(data: Maybe[JsonAst], access: String => Maybe[JsonAst] => Any): JsonRecord = JsonRecord(data, access)
  def fields: Map[String, RecordField] = unsafely(doc.fields)

case class JsonSchemaError() extends Error(err"there was an error in the JSON schema")
