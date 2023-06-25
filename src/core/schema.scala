package villainy

import polyvinyl.*
import rudiments.*
import digression.*
import gossamer.*
import turbulence.*
import hieroglyph.*, charEncoders.utf8
import jacinta.*
import spectacular.*
import merino.*

import scala.compiletime.*

import unsafeExceptions.canThrowAny

object JsonRecord:
  given ValueAccessor[JsonRecord, "boolean", Boolean] = _.asInstanceOf[Boolean]
  given ValueAccessor[JsonRecord, "string", String] = _.asInstanceOf[String]
  given ValueAccessor[JsonRecord, "integer", Int] = _.asInstanceOf[Long].toInt
  given ValueAccessor[JsonRecord, "number", Double] = _.asInstanceOf[Double]
  
  given RecordAccessor[JsonRecord, "array", [ElemType] =>> IArray[ElemType]] =
    (value, make) => value.asInstanceOf[IArray[Any]].map(make)
  
  given RecordAccessor[JsonRecord, "object", [T] =>> T] = (value, make) => make(value)
  
class JsonRecord(access: String => Any) extends Record(access)

case class JsonSchemaDoc
    (`$schema`: Text, `$id`: Text, title: Text, description: Text, `type`: Text,
        properties: Map[String, JsonSchema.Property]):
  def fields: Map[String, RecordField] = properties.view.mapValues(_.field).to(Map)

object JsonSchema:
  case class Property
      (description: Text, `type`: String, properties: Option[Map[String, Json]],
          items: Option[Map[String, Json]]):
    
    def arrayFields = items.get.view.mapValues(_.as[Property].field).to(Map)
    def objectFields = properties.get.view.mapValues(_.as[Property].field).to(Map)
    
    def field: RecordField = `type` match
      case "array"  => RecordField.Record("array", arrayFields)
      case "object" => RecordField.Record("object", objectFields)
      case other    => RecordField.Value(other)

abstract class JsonSchema(val doc: JsonSchemaDoc) extends Schema[JsonRecord]:
  def access(name: String, value: Any): Any =
    println(s"access($name, ${value.getClass})")
    val (keys, values) = value.asInstanceOf[JsonAst].obj
    values(keys.indexOf(name))

  def make(access: String => Any): JsonRecord = JsonRecord(access)
  def fields: Map[String, RecordField] = unsafely(doc.fields)

case class JsonSchemaError() extends Error(err"there was an error in the JSON schema")

object ExampleSchema extends JsonSchema(unsafely(Json.parse(t"""{
  "$$id": "abc",
  "$$schema": "schema",
  "title": "Title",
  "description": "desc",
  "type": "object",
  "properties": {
    "name": { "description": "Name", "type": "string" },
    "age": { "description": "Age", "type": "integer" },
    "children": {
      "description": "Children",
      "type": "array",
      "items": {
        "height": { "type": "number", "description": "Height" },
        "weight": { "type": "number", "description": "Weight" }
      }
    }
  }
}""").as[JsonSchemaDoc])):
  import RecordField.*
  
  transparent inline def record(value: Any): JsonRecord = ${build('value)}
