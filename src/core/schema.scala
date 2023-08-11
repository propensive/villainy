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
import perforate.*
import fulminate.*
import jacinta.*
import kaleidoscope.*
import anticipation.*
import nettlesome.*
import merino.*

import scala.compiletime.*

import errorHandlers.throwUnsafely

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

import JsonValidationError.Issue, Issue.*

case class JsonValidationError(issue: Issue)
extends Error(msg"the JSON was not valid according to the schema because $issue")

trait JsonValueAccessor[NameType <: Label, ValueType]
extends ValueAccessor[JsonRecord, Maybe[Json], NameType, ValueType]:
  def access(value: Json): ValueType
    
  def transform(value: Maybe[Json], params: List[String]): ValueType =
    value.mm(access(_)).or(abort(JsonValidationError(MissingValue)))

object JsonRecord:

  given boolean: JsonValueAccessor["boolean", Boolean] = _.as[Boolean]
  given string: JsonValueAccessor["string", Text] = _.as[Text]
  given integer: JsonValueAccessor["integer", Int] = _.as[Int]
  given number: JsonValueAccessor["number", Double] = _.as[Double]
  given dateTime: JsonValueAccessor["date-time", Text] = _.as[Text] // Use Anticipation/Aviation
  given date: JsonValueAccessor["date", Text] = _.as[Text]          // Use Anticipation/Aviation
  given time: JsonValueAccessor["time", Text] = _.as[Text]          // Use Anticipation/Aviation
  given duration: JsonValueAccessor["duration", Text] = _.as[Text]  // Use Anticipation/Aviation
  given email: JsonValueAccessor["email", Text] = _.as[Text]
  given idnEmail: JsonValueAccessor["idn-email", Text] = _.as[Text]
  given hostname: JsonValueAccessor["hostname", Text] = _.as[Text]
  given idnHostname: JsonValueAccessor["idn-hustname", Text] = _.as[Text]
  given ipv4: JsonValueAccessor["ipv4", Ipv4 raises IpAddressError] with
    def access(value: Json): Ipv4 raises IpAddressError = Ipv4.parse(value.as[Text])
  
  given ipv6: JsonValueAccessor["ipv6", Ipv6 raises IpAddressError] with
    def access(value: Json): Ipv6 raises IpAddressError = Ipv6.parse(value.as[Text])
  
  given uri[UrlType: GenericUrl]: JsonValueAccessor["uri", UrlType] =
    value => GenericUrl[UrlType](value.as[Text])
  
  given uriReference: JsonValueAccessor["uri-reference", Text] = _.as[Text]
  
  given iri[UrlType: GenericUrl]: JsonValueAccessor["iri", UrlType] =
    value => GenericUrl[UrlType](value.as[Text])

  given iriReference: JsonValueAccessor["iri-reference", Text] = _.as[Text]
  given uuid: JsonValueAccessor["uuid", Text] = _.as[Text]
  given uriTemplate: JsonValueAccessor["uri-template", Text] = _.as[Text]
  given jsonPointer: JsonValueAccessor["json-pointer", Text] = _.as[Text]
  given relativeJsonPointer: JsonValueAccessor["relative-json-pointer", Text] = _.as[Text]
  
  // given maybeRegex
  //     : ValueAccessor[JsonRecord, Maybe[Json], "regex?", Maybe[Regex] raises RegexError]
  //     with
    
  //   def transform
  //       (value: Maybe[Json], params: List[String])
  //       : Maybe[Regex] raises RegexError =
  //     (erased invalidRegex: Raises[RegexError]) ?=>
  //       value.mm(_.as[Text]).mm: pattern =>
  //         Regex(pattern)

  given regex: JsonValueAccessor["regex", Regex raises RegexError] with
    def access(value: Json): Regex raises RegexError = Regex(value.as[Text])

  given array: RecordAccessor[JsonRecord, Maybe[Json], "array", List] =
    _.avow(using Unsafe).as[List[Json]].map(_)
  
  given obj: RecordAccessor[JsonRecord, Maybe[Json], "object", [T] =>> T] = (value, make) =>
    make(value.avow(using Unsafe))
  
  given maybeBoolean: ValueAccessor[JsonRecord, Maybe[Json], "boolean?", Maybe[Boolean]] =
    (value, params) => value.mm(_.as[Boolean])
  
  given maybeString: ValueAccessor[JsonRecord, Maybe[Json], "string?", Maybe[Text]] =
    (value, params) => value.mm(_.as[Text])

  given pattern
      : ValueAccessor[JsonRecord, Maybe[Json], "pattern", Text] with
    def transform(value: Maybe[Json], params: List[String]): Text =
      value.mm: value =>
        (params: @unchecked) match
          case List(pattern: String) =>
            val regex = Regex(Text(pattern))
            if regex.matches(value.as[Text]) then value.as[Text]
            else abort(JsonValidationError(PatternMismatch(value.as[Text], regex)))
      .or(abort(JsonValidationError(MissingValue)))

  given maybePattern: ValueAccessor[JsonRecord, Maybe[Json], "pattern?", Maybe[Text]] with
    def transform
        (value: Maybe[Json], params: List[String] = Nil)
        : Maybe[Text] =
      value.mm: value =>
        (params: @unchecked) match
          case pattern :: Nil =>
            val regex = Regex(Text(pattern))
            if regex.matches(value.as[Text]) then value.as[Text]
            else abort(JsonValidationError(PatternMismatch(value.as[Text], regex)))
  
  given maybeInteger: ValueAccessor[JsonRecord, Maybe[Json], "integer?", Maybe[Int]] =
    (value, params) => value.mm(_.as[Int])

  given boundedInteger
      : ValueAccessor[JsonRecord, Maybe[Json], "integer!", Int raises IntRangeError] =
    new ValueAccessor[JsonRecord, Maybe[Json], "integer!", Int raises IntRangeError]:
      def transform(json: Maybe[Json], params: List[String] = Nil): Int raises IntRangeError =
        val int = json.avow(using Unsafe).as[Int]
        
        (params.map(Text(_)): @unchecked) match
          case As[Int](min) :: As[Int](max) :: Nil =>
            if int < min || int > max then abort(IntRangeError(int, min, max)) else int
          
          case As[Int](min) :: _ :: Nil =>
            if int < min then abort(IntRangeError(int, min, Unset)) else int
          
          case _ :: As[Int](max) :: Nil =>
            if int > max then abort(IntRangeError(int, Unset, max)) else int
  
  given maybeNumber: ValueAccessor[JsonRecord, Maybe[Json], "number?", Maybe[Double]] =
    (value, params) => value.mm(_.as[Double])

  given maybeArray: RecordAccessor[JsonRecord, Maybe[Json], "array?", [T] =>> Maybe[List[T]]] =
    (value, make) => value.mm(_.as[List[Json]].map(make))
  
  given maybeObject: RecordAccessor[JsonRecord, Maybe[Json], "object?", [T] =>> Maybe[T]] =
    (value, make) => value.mm(make(_))

class JsonRecord(data: Maybe[Json], access: String => Maybe[Json] => Any)
extends Record[Maybe[Json]](data, access)

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
      ).or(throw Mistake(msg"Some items were missing"))
    
    def objectFields =
      properties.mm(_.map: (key, value) =>
        key -> value.as[Property].field(requiredFields.contains(key))
      ).or(throw Mistake(msg"Some properties were missing"))
    
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

abstract class JsonSchema(val doc: JsonSchemaDoc) extends Schema[Maybe[Json], JsonRecord]:
  def access(name: String, json: Maybe[Json]): Maybe[Json] = json.mm: json =>
    json.as[Map[String, Json]].get(name).getOrElse(Unset)

  def make(data: Maybe[Json], access: String => Maybe[Json] => Any): JsonRecord =
    JsonRecord(data, access)
  
  def fields: Map[String, RecordField] = unsafely(doc.fields)
