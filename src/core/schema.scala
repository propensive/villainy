package villainy

import polyvinyl.*
import rudiments.*
import digression.*
import gossamer.*
import turbulence.*
import hieroglyph.*, charEncoders.utf8
import jacinta.*
import merino.*

import scala.compiletime.*

import unsafeExceptions.canThrowAny

object JsonRecord:
  erased given ValueCast[JsonRecord, "boolean", Boolean] = ###
  erased given ValueCast[JsonRecord, "string", String] = ###
  erased given ValueCast[JsonRecord, "integer", Int] = ###
  erased given ValueCast[JsonRecord, "number", Double] = ###
  erased given ValueCast[JsonRecord, "null", Null] = ###
  erased given ValueCast[JsonRecord, "array", List[JsonRecord]] = ###
  erased given ValueCast[JsonRecord, "object", JsonRecord] = ###

  
class JsonRecord(value: Map[String, Any]) extends Record:
  def access(name: String): Any = value(name)

case class JsonSchemaDoc
    (`$schema`: Text, `$id`: Text, title: Text, description: Text, `type`: Text,
        properties: Map[String, JsonSchemaProperty]):
  def fields: Map[String, RecordField] = properties.view.mapValues(_.field).to(Map)

case class JsonSchemaProperty
    (description: Text, `type`: String, properties: Option[Map[String, Json]],
        items: Option[Map[String, Json]]):
  
  def field: RecordField = `type` match
    case "array"  => RecordField.Record("array", items.get.view.mapValues(_.as[JsonSchemaProperty].field).to(Map))
    case "object" => RecordField.Record("object", properties.get.view.mapValues(_.as[JsonSchemaProperty].field).to(Map))
    case other    => RecordField.Value(other)

abstract class JsonSchema(val doc: JsonSchemaDoc) extends Schema[JsonRecord]:
  def make(value: Any): JsonRecord = JsonRecord:
    value.asInstanceOf[Json].as[Map[String, Json]].view.mapValues: value =>
      if value.root.isObject then make(value) else value.root
    .to(Map)
  
  def fields: Map[String, RecordField] = unsafely:
    try doc.fields catch case err: JsonAccessError => throw JsonSchemaError()

case class JsonSchemaError() extends Error(err"there was an error in the JSON schema")

object ExampleSchema extends JsonSchema(unsafely(Json.parse(t"""{
  "$$id": "abc",
  "$$schema": "schema",
  "title": "Title",
  "description": "desc",
  "type": "object",
  "properties": {
    "name": { "description": "Name", "type": "string" },
    "age": { "description": "Age", "type": "integer" }
  }
}""").as[JsonSchemaDoc])):
  import RecordField.*
  
  transparent inline def record(inline value: Any): JsonRecord = ${build('value)}
