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

import jacinta.*
import turbulence.*
import gossamer.*
import digression.*
import polyvinyl.*
import merino.*
import hieroglyph.*, charEncoders.utf8

object ExampleSchema extends JsonSchema(unsafely(Json.parse(t"""{
  "$$id": "abc",
  "$$schema": "schema",
  "title": "Title",
  "description": "desc",
  "type": "object",
  "required": ["name", "sub", "children"],
  "properties": {
    "name": { "type": "string" },
    "age": { "type": "integer" },
    "sub": {
      "type": "object",
      "properties": {
        "date": { "type": "string", "description": "Some sub value" }
      }
    },
    "children": {
      "description": "Children",
      "type": "array",
      "items": {
        "height": { "type": "integer", "description": "Height", "minimum": 1, "maximum": 99 },
        "weight": { "type": "number", "description": "Weight" },
        "color": { "type": "string", "description": "Colour" }
      }
    }
  }
}""").as[JsonSchemaDoc])):
  import RecordField.*
  
  transparent inline def record(json: JsonAst): JsonRecord = ${build('json)}

