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