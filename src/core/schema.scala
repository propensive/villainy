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
import kaleidoscope.*
import anticipation.*
import nettlesome.*
import merino.*

import scala.compiletime.*

import unsafeExceptions.canThrowAny

object IntRangeError:
  def range(minimum: Maybe[Int], maximum: Maybe[Int]): Text =
    Text(s"${minimum.mm { n => s"$n ≤ " }.or("")}x${minimum.mm { n => s" ≤ $n" }.or("")}")

case class IntRangeError(value: Int, minimum: Maybe[Int], maximum: Maybe[Int])
extends Error(msg"the integer $value is not in the range ${IntRangeError.range(minimum, maximum)}")

object JsonValidationError:
  enum Issue:
    case JsonType(expected: JsonPrimitive, found: JsonPrimitive)
    case MissingValue
    case IntOutOfRange(value: Int, minimum: Maybe[Int], maximum: Maybe[Int])
    case PatternMismatch(value: Text, pattern: Regex)

  object Issue:
    given AsMessage[Issue] =
      case JsonType(expected, found) =>
        msg"expected JSON type $expected, but found $found"
      
      case MissingValue =>
        msg"the value was missing"
      
      case IntOutOfRange(value, minimum, maximum) =>
        if minimum.unset then msg"the value was greater than the maximum, ${maximum.or(0)}"
        else if maximum.unset then msg"the value was less than the minimum, ${minimum.or(0)}"
        else msg"the value was not between ${minimum.or(0)} and ${maximum.or(0)}"
      
      case PatternMismatch(value, pattern) =>
        msg"the value did not conform to the regular expression ${pattern.pattern}"

case class JsonValidationError(issue: JsonValidationError.Issue)
extends Error(msg"the JSON was not valid according to the schema")

trait JsonValueAccessor[NameType <: Label, ValueType]
extends ValueAccessor[JsonRecord, Maybe[JsonAst], NameType, ValueType]:
  def access(value: JsonAst): ValueType
    
  def transform(value: Maybe[JsonAst], params: List[String]): ValueType =
    value.mm(access(_)).or(throw JsonValidationError(JsonValidationError.Issue.MissingValue))

object JsonRecord:

  given boolean: JsonValueAccessor["boolean", Boolean] = _.boolean
  given string: JsonValueAccessor["string", Text] = _.string
  given integer: JsonValueAccessor["integer", Int] = _.long.toInt
  given number: JsonValueAccessor["number", Double] = _.double
  given dateTime: JsonValueAccessor["date-time", Text] = _.string // Use Anticipation/Aviation
  given date: JsonValueAccessor["date", Text] = _.string          // Use Anticipation/Aviation
  given time: JsonValueAccessor["time", Text] = _.string          // Use Anticipation/Aviation
  given duration: JsonValueAccessor["duration", Text] = _.string  // Use Anticipation/Aviation
  given email: JsonValueAccessor["email", Text] = _.string
  given idnEmail: JsonValueAccessor["idn-email", Text] = _.string
  given hostname: JsonValueAccessor["hostname", Text] = _.string
  given idnHostname: JsonValueAccessor["idn-hustname", Text] = _.string
  given ipv4: JsonValueAccessor["ipv4", Ipv4 throws IpAddressError] with
    def access(value: JsonAst): Ipv4 throws IpAddressError = Ipv4.parse(value.string)
  
  given ipv6: JsonValueAccessor["ipv6", Ipv6 throws IpAddressError] with
    def access(value: JsonAst): Ipv6 throws IpAddressError = Ipv6.parse(value.string)
  
  given uri[UrlType: GenericUrl]: JsonValueAccessor["uri", UrlType] =
    value => makeUrl[UrlType](value.string.s)
  
  given uriReference: JsonValueAccessor["uri-reference", Text] = _.string
  
  given iri[UrlType: GenericUrl]: JsonValueAccessor["iri", UrlType] =
    value => makeUrl[UrlType](value.string.s)

  given iriReference: JsonValueAccessor["iri-reference", Text] = _.string
  given uuid: JsonValueAccessor["uuid", Text] = _.string
  given uriTemplate: JsonValueAccessor["uri-template", Text] = _.string
  given jsonPointer: JsonValueAccessor["json-pointer", Text] = _.string
  given relativeJsonPointer: JsonValueAccessor["relative-json-pointer", Text] = _.string
  
  // given maybeRegex
  //     : ValueAccessor[JsonRecord, Maybe[JsonAst], "regex?", Maybe[Regex] throws InvalidRegexError]
  //     with
    
  //   def transform
  //       (value: Maybe[JsonAst], params: List[String])
  //       : Maybe[Regex] throws InvalidRegexError =
  //     (erased invalidRegex: CanThrow[InvalidRegexError]) ?=>
  //       value.mm(_.string).mm: pattern =>
  //         Regex(pattern)

  given regex: JsonValueAccessor["regex", Regex throws InvalidRegexError] with
    def access(value: JsonAst): Regex throws InvalidRegexError = Regex(value.string)

  given array: RecordAccessor[JsonRecord, Maybe[JsonAst], "array", IArray] = _.avow(using Unsafe).array.map(_)
  
  given obj: RecordAccessor[JsonRecord, Maybe[JsonAst], "object", [T] =>> T] = (value, make) =>
    make(value.avow(using Unsafe))
  
  given maybeBoolean: ValueAccessor[JsonRecord, Maybe[JsonAst], "boolean?", Maybe[Boolean]] =
    (value, params) => value.mm(_.boolean)
  
  given maybeString: ValueAccessor[JsonRecord, Maybe[JsonAst], "string?", Maybe[Text]] =
    (value, params) => value.mm(_.string)

  given pattern
      : ValueAccessor[JsonRecord, Maybe[JsonAst], "pattern", Text throws JsonValidationError] with
    def transform(value: Maybe[JsonAst], params: List[String]): Text throws JsonValidationError =
      value.mm: value =>
        (params: @unchecked) match
          case List(pattern: String) =>
            val regex = Regex(Text(pattern))
            if regex.matches(value.string) then value.string
            else throw JsonValidationError(JsonValidationError.Issue.PatternMismatch(value.string,
                regex))
      .or(throw JsonValidationError(JsonValidationError.Issue.MissingValue))

  given maybePattern: ValueAccessor[JsonRecord, Maybe[JsonAst], "pattern?", Maybe[Text] throws
      JsonValidationError] with
    def transform
        (value: Maybe[JsonAst], params: List[String] = Nil)
        : Maybe[Text] throws JsonValidationError =
      value.mm: value =>
        (params: @unchecked) match
          case pattern :: Nil =>
            val regex = Regex(Text(pattern))
            if regex.matches(value.string) then value.string
            else throw JsonValidationError(JsonValidationError.Issue.PatternMismatch(value.string, regex))
  
  given maybeInteger: ValueAccessor[JsonRecord, Maybe[JsonAst], "integer?", Maybe[Int]] =
    (value, params) => value.mm(_.long.toInt)

  given boundedInteger
      : ValueAccessor[JsonRecord, Maybe[JsonAst], "integer!", Int throws IntRangeError] =
    new ValueAccessor[JsonRecord, Maybe[JsonAst], "integer!", Int throws IntRangeError]:
      def transform(json: Maybe[JsonAst], params: List[String] = Nil): Int throws IntRangeError =
        val int = json.avow(using Unsafe).long.toInt
        
        (params.map(Text(_)): @unchecked) match
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
        case _                   => throw JsonValidationError(JsonValidationError.Issue.JsonType(JsonPrimitive.Number, value.primitive))

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
        val suffix = if required then "" else "?"
        
        pattern.mm: pattern =>
          RecordField.Value("pattern"+suffix, pattern)
        .or(RecordField.Value(format.or("string")+suffix))
      
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
