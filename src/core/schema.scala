package villainy

import polyvinyl.*
import rudiments.*
import jacinta.*

object JsonRecord:
  erased given ValueTyper[JsonRecord, "boolean", Boolean] = ###
  erased given ValueTyper[JsonRecord, "string", String] = ###
  erased given ValueTyper[JsonRecord, "integer", Int] = ###
  erased given ValueTyper[JsonRecord, "number", Double] = ###
  erased given ValueTyper[JsonRecord, "array", List[JsonRecord]] = ###
  erased given ValueTyper[JsonRecord, "object", JsonRecord] = ###

  
class JsonRecord(value: Map[String, Any]) extends Record:
  def access(name: String): Any = value(name)

abstract class JsonSchema() extends Schema[JsonRecord]:
  def make(value: Any): JsonRecord = JsonRecord:
    value.asInstanceOf[Map[String, Any]].view.mapValues: value =>
      value.asMatchable match
        case record: Map[?, ?] => make(record)
        case other             => other
    .to(Map)

object ExampleSchema extends JsonSchema():
  import RecordField.*
  
  val fields: Map[String, RecordField] = Map(
    "age"  -> Value("integer"),
    "name" -> Value("string"),
    "male" -> Value("boolean"),
    "data" -> Record("object", Map(
      "color" -> Value("string"),
      "size"  -> Value("integer")
    ))
  )

  transparent inline def record(inline value: Any): JsonRecord = ${build('value)}
