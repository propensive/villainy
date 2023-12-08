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
import inimitable.*
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
  def range(minimum: Optional[Int], maximum: Optional[Int]): Text =
    Text(s"${minimum.let { n => s"$n ≤ " }.or("")}x${minimum.let { n => s" ≤ $n" }.or("")}")

case class IntRangeError(value: Int, minimum: Optional[Int], maximum: Optional[Int])
extends Error(msg"the integer $value is not in the range ${IntRangeError.range(minimum, maximum)}")

object JsonValidationError:
  enum Reason:
    case JsonType(expected: JsonPrimitive, found: JsonPrimitive)
    case MissingValue
    case IntOutOfRange(value: Int, minimum: Optional[Int], maximum: Optional[Int])
    case PatternMismatch(value: Text, pattern: Regex)

  object Reason:
    given Communicable[Reason] =
      case JsonType(expected, found) =>
        msg"expected JSON type $expected, but found $found"
      
      case MissingValue =>
        msg"the value was missing"
      
      case IntOutOfRange(value, minimum, maximum) =>
        if minimum.absent then msg"the value was greater than the maximum, ${maximum.or(0)}"
        else if maximum.absent then msg"the value was less than the minimum, ${minimum.or(0)}"
        else msg"the value was not between ${minimum.or(0)} and ${maximum.or(0)}"
      
      case PatternMismatch(value, pattern) =>
        msg"the value did not conform to the regular expression ${pattern.pattern}"

import JsonValidationError.Reason, Reason.*

case class JsonValidationError(reason: Reason)
extends Error(msg"the JSON was not valid according to the schema because $reason")

trait JsonValueAccessor[NameType <: Label, ValueType]
extends ValueAccessor[JsonRecord, Optional[Json], NameType, ValueType]:
  def access(value: Json): ValueType
    
  def transform(value: Optional[Json], params: List[String]): ValueType =
    value.let(access(_)).or(abort(JsonValidationError(MissingValue)))

object JsonRecord:

  given boolean: JsonValueAccessor["boolean", Boolean] = _.as[Boolean]
  given string: JsonValueAccessor["string", Text] = _.as[Text]
  given integer: JsonValueAccessor["integer", Int] = _.as[Int]
  given number: JsonValueAccessor["number", Double] = _.as[Double]
  given dateTime: JsonValueAccessor["date-time", Text] = _.as[Text] // Use Anticipation/Aviation
  given date: JsonValueAccessor["date", Text] = _.as[Text]          // Use Anticipation/Aviation
  given time: JsonValueAccessor["time", Text] = _.as[Text]          // Use Anticipation/Aviation
  given duration: JsonValueAccessor["duration", Text] = _.as[Text]  // Use Anticipation/Aviation
  
  given email: JsonValueAccessor["email", EmailAddress raises EmailAddressError] with
    def access(value: Json): EmailAddress raises EmailAddressError = EmailAddress.parse(value.as[Text])
  
  given idnEmail: JsonValueAccessor["idn-email", EmailAddress raises EmailAddressError] with
    def access(value: Json): EmailAddress raises EmailAddressError = EmailAddress.parse(value.as[Text])
  
  given hostname: JsonValueAccessor["hostname", Hostname raises HostnameError] with
    def access(value: Json): Hostname raises HostnameError = Hostname.parse(value.as[Text])
  
  given idnHostname: JsonValueAccessor["idn-hostname", Hostname raises HostnameError] with
    def access(value: Json): Hostname raises HostnameError = Hostname.parse(value.as[Text])
  
  given ipv4: JsonValueAccessor["ipv4", Ipv4 raises IpAddressError] with
    def access(value: Json): Ipv4 raises IpAddressError = Ipv4.parse(value.as[Text])
  
  given ipv6: JsonValueAccessor["ipv6", Ipv6 raises IpAddressError] with
    def access(value: Json): Ipv6 raises IpAddressError = Ipv6.parse(value.as[Text])
  
  given uri[UrlType: SpecificUrl]: JsonValueAccessor["uri", UrlType] =
    value => SpecificUrl[UrlType](value.as[Text])
  
  given uriReference: JsonValueAccessor["uri-reference", Text] = _.as[Text]
  
  given iri[UrlType: SpecificUrl]: JsonValueAccessor["iri", UrlType] =
    value => SpecificUrl[UrlType](value.as[Text])

  given iriReference: JsonValueAccessor["iri-reference", Text] = _.as[Text]
  
  given uuid: JsonValueAccessor["uuid", Uuid raises UuidError] with
    def access(value: Json): Uuid raises UuidError = Uuid.parse(value.as[Text])

  given uriTemplate: JsonValueAccessor["uri-template", Text] = _.as[Text]
  given jsonPointer: JsonValueAccessor["json-pointer", Text] = _.as[Text]
  given relativeJsonPointer: JsonValueAccessor["relative-json-pointer", Text] = _.as[Text]
  
  // given maybeRegex
  //     : ValueAccessor[JsonRecord, Optional[Json], "regex?", Optional[Regex] raises RegexError]
  //     with
    
  //   def transform
  //       (value: Optional[Json], params: List[String])
  //       : Optional[Regex] raises RegexError =
  //     (erased invalidRegex: Raises[RegexError]) ?=>
  //       value.let(_.as[Text]).let: pattern =>
  //         Regex(pattern)

  given regex: JsonValueAccessor["regex", Regex raises RegexError] with
    def access(value: Json): Regex raises RegexError = Regex(value.as[Text])

  given array: RecordAccessor[JsonRecord, Optional[Json], "array", List] =
    _.vouch(using Unsafe).as[List[Json]].map(_)
  
  given obj: RecordAccessor[JsonRecord, Optional[Json], "object", [T] =>> T] = (value, make) =>
    make(value.vouch(using Unsafe))
  
  given maybeBoolean: ValueAccessor[JsonRecord, Optional[Json], "boolean?", Optional[Boolean]] =
    (value, params) => value.let(_.as[Boolean])
  
  given maybeString: ValueAccessor[JsonRecord, Optional[Json], "string?", Optional[Text]] =
    (value, params) => value.let(_.as[Text])

  given pattern
      : ValueAccessor[JsonRecord, Optional[Json], "pattern", Text] with
    def transform(value: Optional[Json], params: List[String]): Text =
      value.let: value =>
        (params: @unchecked) match
          case List(pattern: String) =>
            val regex = Regex(Text(pattern))
            if regex.matches(value.as[Text]) then value.as[Text]
            else abort(JsonValidationError(PatternMismatch(value.as[Text], regex)))
      .or(abort(JsonValidationError(MissingValue)))

  given maybePattern: ValueAccessor[JsonRecord, Optional[Json], "pattern?", Optional[Text]] with
    def transform
        (value: Optional[Json], params: List[String] = Nil)
        : Optional[Text] =
      value.let: value =>
        (params: @unchecked) match
          case pattern :: Nil =>
            val regex = Regex(Text(pattern))
            if regex.matches(value.as[Text]) then value.as[Text]
            else abort(JsonValidationError(PatternMismatch(value.as[Text], regex)))
  
  given maybeInteger: ValueAccessor[JsonRecord, Optional[Json], "integer?", Optional[Int]] =
    (value, params) => value.let(_.as[Int])

  given boundedInteger
      : ValueAccessor[JsonRecord, Optional[Json], "integer!", Int raises IntRangeError] =
    new ValueAccessor[JsonRecord, Optional[Json], "integer!", Int raises IntRangeError]:
      def transform(json: Optional[Json], params: List[String] = Nil): Int raises IntRangeError =
        val int = json.vouch(using Unsafe).as[Int]
        
        (params.map(Text(_)): @unchecked) match
          case As[Int](min) :: As[Int](max) :: Nil =>
            if int < min || int > max then abort(IntRangeError(int, min, max)) else int
          
          case As[Int](min) :: _ :: Nil =>
            if int < min then abort(IntRangeError(int, min, Unset)) else int
          
          case _ :: As[Int](max) :: Nil =>
            if int > max then abort(IntRangeError(int, Unset, max)) else int
  
  given maybeNumber: ValueAccessor[JsonRecord, Optional[Json], "number?", Optional[Double]] =
    (value, params) => value.let(_.as[Double])

  given maybeArray: RecordAccessor[JsonRecord, Optional[Json], "array?", [T] =>> Optional[List[T]]] =
    (value, make) => value.let(_.as[List[Json]].map(make))
  
  given maybeObject: RecordAccessor[JsonRecord, Optional[Json], "object?", [T] =>> Optional[T]] =
    (value, make) => value.let(make(_))

class JsonRecord(data: Optional[Json], access: String => Optional[Json] => Any)
extends Record[Optional[Json]](data, access)

case class JsonSchemaDoc
    (`$schema`: Text, `$id`: Text, title: Text, `type`: Text,
        properties: Map[String, JsonSchema.Property], required: Optional[Set[String]]):
  lazy val requiredFields: Set[String] = required.or(Set())
  
  def fields: Map[String, RecordField] =
    properties.map { (key, value) => key -> value.field(requiredFields.contains(key)) }

object JsonSchema:
  case class Property
      (`type`: String, properties: Optional[Map[String, Json]], items: Optional[Map[String, Json]],
          required: Optional[Set[String]], minimum: Optional[Int], maximum: Optional[Int],
          format: Optional[String], pattern: Optional[String]):
    def requiredFields: Set[String] = required.or(Set())
    
    def arrayFields =
      items.let(_.map: (key, value) =>
        key -> value.as[Property].field(requiredFields.contains(key))
      ).or(throw Mistake(msg"Some items were missing"))
    
    def objectFields =
      properties.let(_.map: (key, value) =>
        key -> value.as[Property].field(requiredFields.contains(key))
      ).or(throw Mistake(msg"Some properties were missing"))
    
    def field(required: Boolean): RecordField = `type` match
      case "array" =>
        RecordField.Record(if required then "array" else "array?", arrayFields)
      
      case "object" =>
        RecordField.Record(if required then "object" else "object?", objectFields)
      
      case "string" =>
        val suffix = if required then "" else "?"
        
        pattern.let: pattern =>
          RecordField.Value("pattern"+suffix, pattern)
        .or(RecordField.Value(format.or("string")+suffix))
      
      case "integer" => 
        val suffix = if minimum.absent && maximum.absent then (if required then "" else "?") else "!"
        RecordField.Value("integer"+suffix, minimum.let(_.toString).or(""), maximum.let(_.toString).or(""))
      
      case other =>
        RecordField.Value(if required then other else other+"?")

abstract class JsonSchema(val doc: JsonSchemaDoc) extends Schema[Optional[Json], JsonRecord]:
  def access(name: String, json: Optional[Json]): Optional[Json] = json.let: json =>
    json.as[Map[String, Json]].get(name).getOrElse(Unset)

  def make(data: Optional[Json], access: String => Optional[Json] => Any): JsonRecord =
    JsonRecord(data, access)
  
  def fields: Map[String, RecordField] = unsafely(doc.fields)
