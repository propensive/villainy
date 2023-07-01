A JSON Schema is a JSON document which defines invariants of the structure of other documents, most notably
the set of names of properties that are to be expected in the top-level JSON object; the JSON type of each
one; and recursively, the same for every nested object and the elements of each array.

Here is an example, modified from [json-schema.org](https://json-schema.org/learn/miscellaneous-examples.html):
```json
{
  "$id": "https://example.com/person.schema.json",
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "title": "Person",
  "type": "object",
  "properties": {
    "name" {
      "type": "object",
      "properties": {
        "first": { "type": "string" },
        "last": { "type": "string" }
      }
    },
    "age": { "type": "integer" }
  }
}
```

In Scala, it might be natural to implement this data structure with case classes,
```scala
case class Person(name: Name, age: Int)
case class Name(first: String, last: String)
```
but for many cases, it may be sufficient to use this structural type,
```scala
type Person = Any:
  def name: Any:
    def first: String
    def last: String
  def age: Int
```
which does not require a corresponding class file to exist for the compiler to use it. (In fact, an instance
of the case class `Person` would conform to the structural type `Person` anyway.)

Given a `JsonSchema` object instance corresponding to our particular schema, say, `PersonSchema`, _Villainy_
makes it possible to construct a new record instance from an untyped `Json` value with a structural type
derived from the JSON schema above, *without* needing to define the type for that schema:
```scala
import jacinta.*
import villainy.*

val json: Json = Json.parse(t"""{ "name": { "first": "Douglas", "last": "Richards" }, "age": 22 }""")
val person = PersonSchema.record(json)
```

We can then safely access fields such as, `person.name.first` (a `String`) and `person.age` (an `Int`), but
attempts to access fields not defined in the schema, such as `person.lastName`, will be compile errors.
