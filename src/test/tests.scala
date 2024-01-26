/*
    Villainy, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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

import probably.*
import polyvinyl.*
import gossamer.*
import rudiments.*
import jacinta.*
import turbulence.*
import hieroglyph.*, charEncoders.utf8
import spectacular.*
import perforate.*
import vacuous.*
import kaleidoscope.*

import unsafeExceptions.canThrowAny
import errorHandlers.throwUnsafely

object Tests extends Suite(t"Villainy tests"):
  def run(): Unit =
    val record = test(t"Construct a new record"):
      ExampleSchema.record(Json.parse(
        t"""{
          "name": "Jim",
          "sub": { "date": "11/12/20" },
          "children": [
            {"height": 100, "weight": 0.8, "color": "green" },
            {"height": 9, "weight": 30.0, "color": "#ff0000"}
          ],
          "pattern": "a.b",
          "domain": "example.com"
        }"""
      ))
    .check()

    test(t"Get a text value"):
      record.name
    .assert(_ == t"Jim")

    test(t"Get an integer value"):
      record.age
    .assert(_ == Unset)

    test(t"Get an array value"):
      record.children
    .assert()
    
    test(t"Get the head of an array"):
      record.children.head
    .assert()
    
    test(t"Get a nested value"):
      record.children.head.weight
    .assert(_ == 0.8)
    
    test(t"A bad pattern-checked value throws an exceptions"):
      // FIXME: This should use `capture` to grab the error, but it doesn't seem to work, perhaps because
      // `throwUnsafely` has higher precedence.
      try
        val result = record.children.head.color
        throw UnexpectedSuccessError(result)
      catch case error: JsonValidationError => error
    .assert(_ == JsonValidationError(JsonValidationError.Reason.PatternMismatch(t"green", r"#[0-9a-f]{6}")))
    
    test(t"A bad pattern-checked value throws an exceptions"):
      record.children(1).color
    .assert(_ == t"#ff0000")
    
    test(t"Get a nested item value"):
      record.sub.date
    .assert(_ == t"11/12/20")
    
    test(t"Get a regex value"):
      record.pattern
    .assert(_ == Regex(t"a.b"))
    
    test(t"Get some values in a list"):
      capture:
        record.children.map { elem => elem.height }.to(List)
    .assert(_ == IntRangeError(100, 1, 99))
