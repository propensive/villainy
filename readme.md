[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/villainy/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/villainy/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Villainy

__Scheming typesafe Scala record types for JSON__

_Villainy_ makes it possible to construct safe record types from a JSON Schema,
without any explicit Scala definitions of those types. This makes it safer and
easier to work with JSON values that conform to a schema. Record types are
generated by [Polyvinyl](https://github.com/propensive/polyvinyl) and JSON
integration is provided by [Jacinta](https://github.com/propensive/jacinta)

## Features

- reads a JSON Schema at compiletime and generates corresponding Scala structural record types
- record types are usable and fully typesafe in downstream compilations
- JSON Schema may originate from a file, URL, classpath or any source
- modifications to the schema can change types in subsequent compilations


## Availability Plan

Villainy has not yet been published. The medium-term plan is to build Villainy
with [Fury](https://github.com/propensive/fury) and to publish it as a source build on
[Vent](https://github.com/propensive/vent). This will enable ordinary users to write and build
software which depends on Villainy.

Subsequently, Villainy will also be made available as a binary in the Maven
Central repository. This will enable users of other build tools to use it.

For the overeager, curious and impatient, see [building](#building).

## Getting Started

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
type Person = {
  def name: { def first: String; def last: String }
  def age: Int
}
```
which does not require a corresponding class file to exist for the compiler to use it. (In fact, an instance
of the case class `Person` would conform to the structural type `Person` anyway.)

### Creating a schema object

We need to create an object, say, `PersonSchema`, which represents our schema. This _must_ be a singleton
object (and not a `val`), and should extend `JsonSchema`, which requires a single constructor parameter.

This constructor parameter may be any value that can be read as `Bytes`, so the following options would
all work, using appropriate [Turbulence](https://github.com/propensive/turbulence) `Readable` instances:
```scala
val schema: Text = t"""{...}"""
object PersonSchema extends JsonSchema(schema)
```
or, using [Ambience](https://github.com/propensive/ambience/) and [Galilei](https://github.com/propensive/galilei),
```scala
object PersonSchema extends JsonSchema(env.pwd / p"data" / p"schema.json")
```
or, using [Telekinesis](https://github.com/propensive/telekinesis/),
```scala
object PersonSchema extends JsonSchema(url"https://example.com/schemata/person.jsons")
```

This object must be compiled before any code which uses it to create records. It should be sufficient to put it into
a separate source file, but it can also be compiled in a separate build module.

This is because the compiler needs to instantiate it at the time it compiles the code which constructs
records. It will therefore also need to execute the code which provides the JSON schema data, so in the file-based
example above, the file `schema.json` must be in the right place relative to the PWD *at compiletime*.

Furthermore, any exceptions that may be thrown during construction must be neutralized.

### Constructing the record

Given a `JsonSchema` object instance corresponding to our particular schema, _Villainy_
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




## Status

Villainy is classified as __embryonic__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Villainy is designed to be _small_. Its entire source code currently consists
of 275 lines of code.

## Building

Villainy will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Villainy?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Villainy's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Villainy and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `villainy`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Villainy's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Villainy are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/villainy/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Villainy
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Villainy was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

Schema is homophonous with _schemer_, that is, someone who formulates devious plans; presumably an act of _villainy_.

In general, Scala One project names are always chosen with some rationale,
however it is usually frivolous. Each name is chosen for more for its
_uniqueness_ and _intrigue_ than its concision or catchiness, and there is no
bias towards names with positive or "nice" meanings—since many of the libraries
perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it
should be noted how willingly English adopts foreign words. Names are generally
of Greek or Latin origin, and have often arrived in English via a romance
language.

## Logo

The logo shows a pair of glowing, villainous eyes.

## License

Villainy is copyright &copy; 2024 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).

