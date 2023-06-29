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
        "height": { "type": "integer", "description": "Height" },
        "weight": { "type": "number", "description": "Weight" },
        "color": { "type": "string", "description": "Colour" }
      }
    }
  }
}""").as[JsonSchemaDoc])):
  import RecordField.*
  
  transparent inline def record(json: JsonAst): JsonRecord = ${build('json)}
