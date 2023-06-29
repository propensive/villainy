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

import probably.*
import polyvinyl.*
import gossamer.*
import rudiments.*
import jacinta.*
import turbulence.*
import hieroglyph.*, charEncoders.utf8
import spectacular.*
import digression.*

import unsafeExceptions.canThrowAny

object Tests extends Suite(t"Villainy tests"):
  def run(): Unit =
    val record = test(t"Construct a new record"):
      ExampleSchema.record(Json.parse(t"""{"name": "Jon", "age": 38, "children": [{"height": 3.3, "weight": 2.9 }, {"height": 1.0, "weight": 30.0}] }""").root)
    .check()

    test(t"Get a text value"):
      record.name
    .assert(_ == "Jon")

    test(t"Get an integer value"):
      record.age
    .assert(_ == 38)

    test(t"Get a nested value"):
      record.children.head.weight
    .assert(_ == 2.9)