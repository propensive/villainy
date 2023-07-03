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

object IntRangeError:
  def range(minimum: Maybe[Int], maximum: Maybe[Int]): Text =
    Text(s"${minimum.mm { n => s"$n ≤ " }.or("")}x${minimum.mm { n => s" ≤ $n" }.or("")}")

case class IntRangeError(value: Int, minimum: Maybe[Int], maximum: Maybe[Int])
extends Error(err"the integer $value is not in the range ${IntRangeError.range(minimum, maximum)}")


trait JsonValueAccessor[NameType <: Label, ValueType]
extends ValueAccessor[JsonRecord, Maybe[JsonAst], NameType, ValueType]:
  def access(value: JsonAst): ValueType
    
  def transform(value: Maybe[JsonAst], params: String*): ValueType =
    value.mm(access).or(throw JsonSchemaError())

object JsonRecord:

  given boolean: JsonValueAccessor["boolean", Boolean] = _.boolean
  given string: JsonValueAccessor["string", Text] = _.string
  given integer: JsonValueAccessor["integer", Int] = _.long.toInt
  given number: JsonValueAccessor["number", Double] = _.double

  given array: RecordAccessor[JsonRecord, Maybe[JsonAst], "array", IArray] = _.avow.array.map(_)
  
  given obj: RecordAccessor[JsonRecord, Maybe[JsonAst], "object", [T] =>> T] = (value, make) =>
    make(value.avow)
  
  given maybeBoolean: ValueAccessor[JsonRecord, Maybe[JsonAst], "boolean?", Maybe[Boolean]] =
    (value, params) => value.mm(_.boolean)
  
  given maybeString: ValueAccessor[JsonRecord, Maybe[JsonAst], "string?", Maybe[Text]] =
    (value, params) => value.mm(_.string)
  
  given maybeInteger: ValueAccessor[JsonRecord, Maybe[JsonAst], "integer?", Maybe[Int]] =
    (value, params) => value.mm(_.long.toInt)

  given boundedInteger
      : ValueAccessor[JsonRecord, Maybe[JsonAst], "integer!", Int throws IntRangeError] =
    new ValueAccessor[JsonRecord, Maybe[JsonAst], "integer!", Int throws IntRangeError]:
      def transform(json: Maybe[JsonAst], params: String*): Int throws IntRangeError =
        val int = json.avow.long.toInt
        
        (params.map(Text(_)).to(List): @unchecked) match
          case As[Int](min) :: As[Int](max) :: Nil =>
            if int < min || int > max then throw IntRangeError(int, min, max) else int
          
          case As[Int](min) :: _ :: Nil =>
            if int < min then throw IntRangeError(int, min, Unset) else int
          
          case _ :: As[Int](max) :: Nil =>
            if int > max then throw IntRangeError(int, Unset, max) else int
  
  given maybeNumber: ValueAccessor[JsonRecord, Maybe[JsonAst], "number?", Maybe[Double]] =
    (value, params) => value.mm: value =>
      value.asMatchable match
        case long: Long          => long.toDouble
        case decimal: BigDecimal => decimal.toDouble
        case double: Double      => double
        case _                   => throw JsonSchemaError()

  given maybeArray: RecordAccessor[JsonRecord, Maybe[JsonAst], "array?", [T] =>> Maybe[IArray[T]]] =
    (value, make) => value.mm(_.array.map(make))
  
  given maybeObject: RecordAccessor[JsonRecord, Maybe[JsonAst], "object?", [T] =>> Maybe[T]] =
    (value, make) => value.mm(make(_))

class JsonRecord(data: Maybe[JsonAst], access: String => Maybe[JsonAst] => Any)
extends Record[Maybe[JsonAst]](data, access)

case class JsonSchemaDoc
    (`$schema`: Text, `$id`: Text, title: Text, `type`: Text,
        properties: Map[String, JsonSchema.Property], required: Maybe[Set[String]]):
  lazy val requiredFields: Set[String] = required.or(Set())
  
  def fields: Map[String, RecordField] =
    properties.map { (key, value) => key -> value.field(requiredFields.contains(key)) }

object JsonSchema:
  case class Property
      (`type`: String, properties: Maybe[Map[String, Json]], items: Maybe[Map[String, Json]],
          required: Maybe[Set[String]], minimum: Maybe[Int], maximum: Maybe[Int],
          format: Maybe[String], pattern: Maybe[String]):
    def requiredFields: Set[String] = required.or(Set())
    
    def arrayFields =
      items.mm(_.map: (key, value) =>
        key -> value.as[Property].field(requiredFields.contains(key))
      ).or(throw Mistake("missing items"))
    
    def objectFields =
      properties.mm(_.map: (key, value) =>
        key -> value.as[Property].field(requiredFields.contains(key))
      ).or(throw Mistake("missing properties"))
    
    def field(required: Boolean): RecordField = `type` match
      case "array" =>
        RecordField.Record(if required then "array" else "array?", arrayFields)
      
      case "object" =>
        RecordField.Record(if required then "object" else "object?", objectFields)
      
      case "string" =>
        RecordField.Value(format.or("string"))
      
      case "integer" => 
        val suffix = if minimum.unset && maximum.unset then (if required then "" else "?") else "!"
        RecordField.Value("integer"+suffix, minimum.mm(_.toString).or(""), maximum.mm(_.toString).or(""))
      
      case other =>
        RecordField.Value(if required then other else other+"?")

abstract class JsonSchema(val doc: JsonSchemaDoc) extends Schema[Maybe[JsonAst], JsonRecord]:
  def access(name: String, json: Maybe[JsonAst]): Maybe[JsonAst] = json.mm: json =>
    json.obj match
      case (keys: IArray[String], values: IArray[Maybe[JsonAst]]) => keys.indexOf(name) match
        case -1    => Unset
        case index => values(index)

  def make(data: Maybe[JsonAst], access: String => Maybe[JsonAst] => Any): JsonRecord =
    JsonRecord(data, access)
  
  def fields: Map[String, RecordField] = unsafely(doc.fields)

case class JsonSchemaError() extends Error(err"there was an error in the JSON schema")
