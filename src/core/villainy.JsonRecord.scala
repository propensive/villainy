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

object JsonRecord:
  given boolean: JsonSchematic["boolean", Boolean] = _.as[Boolean]
  given string: JsonSchematic["string", Text] = _.as[Text]
  given integer: JsonSchematic["integer", Int] = _.as[Int]
  given number: JsonSchematic["number", Double] = _.as[Double]
  given dateTime: JsonSchematic["date-time", Text] = _.as[Text] // Use Anticipation/Aviation
  given date: JsonSchematic["date", Text] = _.as[Text]          // Use Anticipation/Aviation
  given time: JsonSchematic["time", Text] = _.as[Text]          // Use Anticipation/Aviation
  given duration: JsonSchematic["duration", Text] = _.as[Text]  // Use Anticipation/Aviation

  given email: JsonSchematic["email", EmailAddress raises EmailAddressError] with
    def access(value: Json)
            : EmailAddress raises EmailAddressError = EmailAddress.parse(value.as[Text])

  given idnEmail: JsonSchematic["idn-email", EmailAddress raises EmailAddressError] with
    def access(value: Json)
            : EmailAddress raises EmailAddressError = EmailAddress.parse(value.as[Text])

  given hostname: JsonSchematic["hostname", Hostname raises HostnameError] with
    def access(value: Json): Hostname raises HostnameError = Hostname.parse(value.as[Text])

  given idnHostname: JsonSchematic["idn-hostname", Hostname raises HostnameError] with
    def access(value: Json): Hostname raises HostnameError = Hostname.parse(value.as[Text])

  given ipv4: JsonSchematic["ipv4", Ipv4 raises IpAddressError] with
    def access(value: Json): Ipv4 raises IpAddressError = Ipv4.parse(value.as[Text])

  given ipv6: JsonSchematic["ipv6", Ipv6 raises IpAddressError] with
    def access(value: Json): Ipv6 raises IpAddressError = Ipv6.parse(value.as[Text])

  given uri[UrlType: SpecificUrl]: JsonSchematic["uri", UrlType] =
    value => SpecificUrl[UrlType](value.as[Text])

  given uriReference: JsonSchematic["uri-reference", Text] = _.as[Text]

  given iri[UrlType: SpecificUrl]: JsonSchematic["iri", UrlType] =
    value => SpecificUrl[UrlType](value.as[Text])

  given iriReference: JsonSchematic["iri-reference", Text] = _.as[Text]

  given uuid: JsonSchematic["uuid", Uuid raises UuidError] with
    def access(value: Json): Uuid raises UuidError = Uuid.parse(value.as[Text])

  given uriTemplate: JsonSchematic["uri-template", Text] = _.as[Text]
  given jsonPointer: JsonSchematic["json-pointer", Text] = _.as[Text]
  given relativeJsonPointer: JsonSchematic["relative-json-pointer", Text] = _.as[Text]

  // given maybeRegex
  //     : Schematic[JsonRecord, Optional[Json], "regex?", Optional[Regex] raises RegexError]
  //     with

  //   def transform
  //       (value: Optional[Json], params: List[String])
  //       : Optional[Regex] raises RegexError =
  //     (erased invalidRegex: Tactic[RegexError]) ?=>
  //       value.let(_.as[Text]).let: pattern =>
  //         Regex(pattern)

  given regex: JsonSchematic["regex", Regex raises RegexError] with
    def access(value: Json): Regex raises RegexError = Regex(value.as[Text])

  given array: RecordAccessor[JsonRecord, Optional[Json], "array", List] =
    _.vouch(using Unsafe).as[List[Json]].map(_)

  given obj: RecordAccessor[JsonRecord, Optional[Json], "object", [Type] =>> Type] =
    (value, make) => make(value.vouch(using Unsafe))

  given maybeBoolean: Schematic[JsonRecord, Optional[Json], "boolean?", Optional[Boolean]] =
    (value, params) => value.let(_.as[Boolean])

  given maybeString: Schematic[JsonRecord, Optional[Json], "string?", Optional[Text]] =
    (value, params) => value.let(_.as[Text])

  given pattern: Schematic[JsonRecord, Optional[Json], "pattern", Text] with
    def transform(value: Optional[Json], params: List[String]): Text =
      value.let: value =>
        (params: @unchecked) match
          case List(pattern: String) =>
            val regex = Regex(Text(pattern))
            if regex.matches(value.as[Text]) then value.as[Text]
            else abort(JsonSchemaError(PatternMismatch(value.as[Text], regex)))

      . lest(JsonSchemaError(MissingValue))

  given maybePattern: Schematic[JsonRecord, Optional[Json], "pattern?", Optional[Text]] with
    def transform(value: Optional[Json], params: List[String] = Nil): Optional[Text] =
      value.let: value =>
        (params: @unchecked) match
          case pattern :: Nil =>
            val regex = Regex(Text(pattern))
            if regex.matches(value.as[Text]) then value.as[Text]
            else abort(JsonSchemaError(PatternMismatch(value.as[Text], regex)))

  given maybeInteger: Schematic[JsonRecord, Optional[Json], "integer?", Optional[Int]] =
    (value, params) => value.let(_.as[Int])

  given boundedInteger
          : Schematic[JsonRecord, Optional[Json], "integer!", Int raises IntRangeError] =
    new Schematic[JsonRecord, Optional[Json], "integer!", Int raises IntRangeError]:
      def transform(json: Optional[Json], params: List[String] = Nil): Int raises IntRangeError =
        val int = json.vouch(using Unsafe).as[Int]

        (params.map(_.tt): @unchecked) match
          case As[Int](min) :: As[Int](max) :: Nil =>
            if int < min || int > max then abort(IntRangeError(int, min, max)) else int

          case As[Int](min) :: _ :: Nil =>
            if int < min then abort(IntRangeError(int, min, Unset)) else int

          case _ :: As[Int](max) :: Nil =>
            if int > max then abort(IntRangeError(int, Unset, max)) else int

  given maybeNumber: Schematic[JsonRecord, Optional[Json], "number?", Optional[Double]] =
    (value, params) => value.let(_.as[Double])

  given maybeArray
          : RecordAccessor[JsonRecord, Optional[Json], "array?", [T] =>> Optional[List[T]]] =
    (value, make) => value.let(_.as[List[Json]].map(make))

  given maybeObject: RecordAccessor[JsonRecord, Optional[Json], "object?", [T] =>> Optional[T]] =
    (value, make) => value.let(make(_))

class JsonRecord(data0: Optional[Json], access0: String => Optional[Json] => Any) extends Record:
  type Format = Optional[Json]
  val data: Optional[Json] = data0
  def access: String => Optional[Json] => Any = access0
