[⬅ Back to highlights](../README.md)

# ReScript API reference

## Table of contents

- [Table of contents](#table-of-contents)
- [Install](#install)
- [Basic usage](#basic-usage)
- [Real-world examples](#real-world-examples)
- [API reference](#api-reference)
  - [`string`](#string)
    - [String formats](#string-formats)
    - [Custom error messages](#custom-error-messages)
    - [ISO datetimes](#iso-datetimes)
  - [`int`](#int)
  - [`float`](#float)
  - [`option`](#option)
  - [`Option.getOr`](#optiongetor)
  - [`Option.getOrWith`](#optiongetorwith)
  - [`null`](#null)
  - [`nullAsOption`](#nullasoption)
  - [`nullable`](#nullable)
  - [`nullableAsOption`](#nullableasoption)
  - [`literal`](#literal)
  - [`object`](#object)
    - [Transform object field names](#transform-object-field-names)
    - [Transform to a structurally typed object](#transform-to-a-structurally-typed-object)
    - [Transform to a tuple](#transform-to-a-tuple)
    - [Transform to a variant](#transform-to-a-variant)
    - [`s.flatten`](#sflatten)
    - [`s.nested`](#snested)
    - [Object destructuring](#object-destructuring)
  - [`strict`](#strict)
  - [`strip`](#strip)
  - [`deepStrict` & `deepStrip`](#deepstrict-deepstrip)
  - [`schema`](#schema)
  - [`shape`](#shape)
  - [`union`](#union)
    - [Enums](#enums)
    - [Converting to / from a union](#converting-to-from-a-union)
  - [`list`](#list)
  - [`compactColumns`](#compactcolumns)
  - [`tuple`](#tuple)
  - [`tuple1` - `tuple3`](#tuple1---tuple3)
  - [`dict`](#dict)
  - [`date`](#date)
  - [`isoDateTime`](#isodatetime)
  - [`instance`](#instance)
  - [`blob`](#blob)
  - [`file`](#file)
  - [`json`](#json)
  - [`jsonString`](#jsonstring)
  - [Content](#content)
  - [`meta`](#meta)
  - [`recursive`](#recursive)
- [Custom schema](#custom-schema)
- [Refinements](#refinements)
  - [`refine`](#refine)
    - [Custom error message](#custom-error-message)
    - [Custom error path](#custom-error-path)
    - [Chaining refinements](#chaining-refinements)
- [Transforms](#transforms)
  - [`to` with `~custom`](#to-with-custom)
- [Functions on schema](#functions-on-schema)
  - [At a glance](#at-a-glance)
  - [Pipelines](#pipelines)
  - [Built-in operations](#built-in-operations)
  - [`reverse`](#reverse)
  - [`to`](#to)
  - [`name`](#name)
  - [`inputExpression`](#inputexpression)
  - [`outputExpression`](#outputexpression)
  - [`toString`](#tostring)
  - [`noValidation`](#novalidation)
- [Standard Schema](#standard-schema)
- [Error handling](#error-handling)
- [Global config](#global-config)
  - [`defaultAdditionalItems`](#defaultadditionalitems)
  - [`disableNanNumberValidation`](#disablenannumbervalidation)

## Install

```sh
npm install sury
```

Then add `sury` to `bs-dependencies` in your `rescript.json`:

```diff
{
  ...
+ "bs-dependencies": ["sury"],
}
```

## Basic usage

```rescript
// 1. Define a type
type rating =
  | @as("G") GeneralAudiences
  | @as("PG") ParentalGuidanceSuggested
  | @as("PG13") ParentalStronglyCautioned
  | @as("R") Restricted
type film = {
  id: float,
  title: string,
  tags: array<string>,
  rating: rating,
  deprecatedAgeRestriction: option<int>,
}

// 2. Create a schema
let filmSchema = S.object(s => {
  id: s.field("Id", S.float),
  title: s.field("Title", S.string),
  tags: s.fieldOr("Tags", S.array(S.string), []),
  rating: s.field(
    "Rating",
    S.union([
      S.literal(GeneralAudiences),
      S.literal(ParentalGuidanceSuggested),
      S.literal(ParentalStronglyCautioned),
      S.literal(Restricted),
    ]),
  ),
  deprecatedAgeRestriction: s.field("Age", S.option(S.int)->S.meta({deprecated: true})),
})

// 3. Parse data using the schema
// The data is validated and transformed to a convenient format
{
  "Id": 1,
  "Title": "My first film",
  "Rating": "R",
  "Age": 17
}->S.parseOrThrow(~to=filmSchema)
// {
//   id: 1.,
//   title: "My first film",
//   tags: [],
//   rating: Restricted,
//   deprecatedAgeRestriction: Some(17),
// }

// 4. Convert data back using the same schema
{
  id: 2.,
  tags: ["Loved"],
  title: "Sad & sed",
  rating: ParentalStronglyCautioned,
  deprecatedAgeRestriction: None,
}->S.convertOrThrow(~from=filmSchema, ~to=S.unknown)
// {
//   "Id": 2,
//   "Title": "Sad & sed",
//   "Rating": "PG13",
//   "Tags": ["Loved"],
//   "Age": undefined,
// }

// 5. Build a value in code, checked by the same schema
let makeFilm = S.compileMakeOrThrow(~schema=filmSchema)
makeFilm({
  id: 3.,
  title: "Shorts",
  tags: [],
  rating: GeneralAudiences,
  deprecatedAgeRestriction: None,
})
// the record itself, validated

// 6. Convert the schema to a JSON schema
let filmJSONSchema = filmSchema->S.inputJSONSchema
```

> 🧠 Schemas compile to JavaScript via `eval`. Print the type they describe with [`inputExpression`](#inputexpression).

## Real-world examples

- [Reliable API layer](https://github.com/Nicolas1st/net-cli-rock-paper-scissors/blob/main/apps/client/src/Api.res)
- [Creating CLI utility](https://github.com/DZakh/rescript-stdlib-cli/blob/main/src/interactors/RunCli.res)
- [Safely accessing environment variables](https://github.com/Nicolas1st/net-cli-rock-paper-scissors/blob/main/apps/client/src/Env.res)

## API reference

The obvious ones, at a glance:

| Schema | Type | |
| --- | --- | --- |
| `S.string` | `S.t<string>` | [refinements ↓](#string) |
| `S.bool` | `S.t<bool>` | |
| `S.int` | `S.t<int>` | [refinements ↓](#int) |
| `S.integer` | `S.t<float>` | integer without `int`'s range [↓](#int) |
| `S.float` | `S.t<float>` | [refinements ↓](#float) |
| `S.bigint` | `S.t<bigint>` | |
| `S.symbol` | `S.t<Symbol.t>` | |
| `S.unit` | `S.t<unit>` | shorthand for `S.literal()` |
| `S.nullAsUnit` | `S.t<unit>` | shorthand for `S.literal(Null.null)->S.to(S.unit)` |
| `S.unknown` | `S.t<unknown>` | accepts any data |
| `S.never` | `S.t<S.never>` | fails on every value |
| `S.array(S.string)` | `S.t<array<string>>` | |

The rest have their own sections below.

### **`string`**

`S.t<string>`

```rescript
let schema = S.string

"Hello World!"->S.parseOrThrow(~to=schema)
// "Hello World!"
```

The `S.string` schema represents a data that is a string. It can be further constrainted with the following utility methods.

**Sury** includes a handful of string-specific refinements and transforms:

```rescript
S.string->S.maxLength(5) // Expected string.length <= 5
S.string->S.minLength(5) // Expected string.length >= 5
S.string->S.length(5) // Expected string.length == 5
S.string->S.nonEmpty // Expected string.length >= 1
S.string->S.pattern(%re(`/[0-9]/`)) // Invalid pattern

S.string->S.trim // trim whitespaces
```

For format-specific validation, use the standalone schemas — see [String formats](#string-formats) below.

> For ISO 8601 UTC datetime strings use the dedicated standalone `S.isoDateTime` schema — see [ISO datetimes](#iso-datetimes) below.

> ⚠️ Validating email addresses is nearly impossible with just code. Different clients and servers accept different things and many diverge from the various specs defining "valid" emails. The ONLY real way to validate an email address is to send a verification email to it and check that the user got it. With that in mind, Sury picks a relatively simple regex that does not cover all cases.

#### String formats

The JSON Schema string format vocabulary, as standalone schemas:

```rescript
S.email // Email address
S.idnEmail // Internationalized email address
S.uuid // UUID
S.cuid // CUID
S.uri // URI — a scheme is required
S.uriReference // URI or relative reference
S.uriTemplate // URI Template
S.iri // IRI — a URI with Unicode allowed
S.iriReference // IRI or relative reference
S.hostname // Host name
S.idnHostname // Internationalized host name
S.ipv4 // IPv4 address
S.ipv6 // IPv6 address
S.isoDate // Calendar date
S.isoTime // Time of day
S.isoDateTime // UTC timestamp
S.duration // Duration
S.jsonPointer // JSON Pointer
S.relativeJsonPointer // Relative JSON Pointer
S.base64 // Base64, standard alphabet with canonical padding
S.base64url // Base64url, URL-safe alphabet, no padding
```

Each survives a round trip through `S.inputJSONSchema` and `S.fromJSONSchema`.

Every format schema has its own type, so a format survives in the type system
instead of collapsing back into `string`:

```rescript
@unboxed type email = Email(string) // S.email : S.t<S.email>

let email = "dzakh.dev@gmail.com"->S.parseOrThrow(~to=S.email) // Email("dzakh.dev@gmail.com")
```

The constructor is erased at runtime — the value is the string it wraps — and
the coercion operator takes you back to the payload without one:

```rescript
let asString = (email :> string)
```

`S.integer`, `S.port` and `S.jsonString` work the same way (`Integer(float)`,
`Port(int)`, `JsonString(string)`), and `S.nonEmpty` wraps whatever it
constrains:

```rescript
@unboxed type nonEmpty<'value> = NonEmpty('value)

S.array(S.string)->S.nonEmpty // S.t<S.nonEmpty<array<string>>>
```

Two schemas of different formats no longer unify, which is the point — an
`S.email` can't be passed where an `S.uuid` is expected — but it also means a
union of two formats needs one common value type to hold its members.

**A format checks syntax, not safety.** Every one is exactly as strict as its
spec, so a well-formed value passes even when it isn't one you want to accept:

```rescript
"javascript:alert(1)"->S.assertOrThrow(~to=S.uri) // passes — a valid URI
"169.254.169.254"->S.assertOrThrow(~to=S.hostname) // passes — a valid host name
"//evil.com"->S.assertOrThrow(~to=S.uriReference) // passes — a valid reference
```

When you want a security decision rather than a syntax check, compose one. The
extra constraint rides along into the JSON Schema, so it stays honest:

```rescript
let httpsOnly = S.uri->S.pattern(%re(`/^https:\/\//`))
// { type: "string", format: "uri", pattern: "^https:\\/\\/" }
```

Two worth knowing before you pick one:

- **`S.url` is not `S.uri`.** `S.url` is an instance of the JS `URL` class, the
  way `S.date` is a `Date` — use it when you want the parsed object and its
  `.host` / `.pathname`. `S.uri` validates a string and leaves it a string.
- **`S.uriReference` is usually the one you want for a link field.** `S.uri`
  requires a scheme, so it rejects `/dashboard`.

#### Custom error messages

Built-in refinements accept an optional `~message` argument for a custom error message:

```rescript
S.string->S.nonEmpty(~message="String can't be empty")
S.string->S.length(5, ~message="SMS code should be 5 digits long")
S.string->S.pattern(%re(`/^\d+$/`), ~message="Must be numeric")
```

For standalone schemas or more control, use `S.meta` with the `errorMessage` field:

```rescript
// Override a specific constraint message
S.email->S.meta({errorMessage: {format: "Must be a valid email"}})

// Use catchAll as a fallback for any constraint
S.email->S.meta({errorMessage: {catchAll: "Invalid input"}})

// Reset error messages (removes all overrides)
schema->S.meta({errorMessage: {}})
```

Available fields: `format`, `type_`, `minimum`, `maximum`, `minLength`, `maxLength`, `minItems`, `maxItems`, `minSize`, `maxSize`, `pattern`, `catchAll` (encoded as `_`).

#### ISO datetimes

`S.isoDateTime` is a **standalone** string schema (`S.t<string>`) that validates ISO 8601 UTC datetime strings: no timezone offsets allowed, with arbitrary sub-second decimal precision.

```rescript
let schema = S.isoDateTime
// schema has the type S.t<string>

"2020-01-01T00:00:00Z"->S.parseOrThrow(~to=schema) // pass
"2020-01-01T00:00:00.123Z"->S.parseOrThrow(~to=schema) // pass
"2020-01-01T00:00:00.123456Z"->S.parseOrThrow(~to=schema) // pass (arbitrary precision)
"2020-01-01T00:00:00+02:00"->S.parseOrThrow(~to=schema) // fail (no offsets allowed)
```

To decode an ISO datetime string into a `Date.t`, combine it with `S.to(S.date)`:

```rescript
let schema = S.string->S.to(S.date)
// schema has the type S.t<Date.t>
```

### **`int`**

`S.t<int>`

The `S.int` schema represents a data that is an integer.

**Sury** includes some of int-specific refinements:

```rescript
S.int->S.lte(5) // Expected int32 <= 5
S.int->S.gte(5) // Expected int32 >= 5
S.int->S.lt(5) // Expected int32 < 5
S.int->S.gt(5) // Expected int32 > 5
S.int->S.multipleOf(2) // Expected int32 % 2
S.port // Standalone port schema
```

They all work on `S.float` and `S.bigint` too. A numeric format carries its
own range, so a bound outside it fails where it's written rather than building
a schema nothing satisfies:

```rescript
S.int->S.gte(3000000000)
// int32 >= 3000000000 contradicts int32 <= 2147483647
```

`S.integer` is an integer without that range, typed `S.t<float>` since one can
exceed ReScript's `int`:

```rescript
S.integer->S.gte(5.) // Expected integer >= 5
```

### **`float`**

`S.t<float>`

The `S.float` schema represents a data that is a number.

**Sury** includes some of float-specific refinements:

```rescript
S.float->S.lte(5.) // Expected number <= 5
S.float->S.gte(5.) // Expected number >= 5
S.float->S.lt(5.) // Expected number < 5
S.float->S.gt(5.) // Expected number > 5
```

### **`option`**

`S.t<'value> => S.t<option<'value>>`

```rescript
let schema = S.option(S.string)

"Hello World!"->S.parseOrThrow(~to=schema)
// Some("Hello World!")
%raw(`undefined`)->S.parseOrThrow(~to=schema)
// None
```

The `S.option` schema represents a data of a specific type that might be undefined.

### **`Option.getOr`**

`(S.t<option<'value>>, 'value) => S.t<'value>`

```rescript
let schema = S.option(S.string)->S.Option.getOr("Hello World!")

%raw(`undefined`)->S.parseOrThrow(~to=schema)
// "Hello World!"
"Goodbye World!"->S.parseOrThrow(~to=schema)
// "Goodbye World!"
```

The `Option.getOr` augments a schema to add transformation logic for default values, which are applied when the input is undefined.

> 🧠 If you want to set a default value for an object field, there's a more convenient `fieldOr` method on `Object.s` type.

### **`Option.getOrWith`**

`(S.t<option<'value>>, () => 'value) => S.t<'value>`

```rescript
let schema = S.option(S.array(S.string))->S.Option.getOrWith(() => ["Hello World!"])

%raw(`undefined`)->S.parseOrThrow(~to=schema)
// ["Hello World!"]
["Goodbye World!"]->S.parseOrThrow(~to=schema)
// ["Goodbye World!"]
```

Also you can use `Option.getOrWith` for lazy evaluation of the default value.

### **`null`**

`S.t<'value> => S.t<null<'value>>`

```rescript
let schema = S.null(S.string)

"Hello World!"->S.parseOrThrow(~to=schema)
// Value("Hello World!")
%raw(`null`)->S.parseOrThrow(~to=schema)
// Null
```

The `S.null` schema represents a data of a specific type that might be null.

### **`nullAsOption`**

`S.t<'value> => S.t<option<'value>>`

```rescript
let schema = S.nullAsOption(S.string)

"Hello World!"->S.parseOrThrow(~to=schema)
// Some("Hello World!")
%raw(`null`)->S.parseOrThrow(~to=schema)
// None
```

The `S.nullAsOption` schema represents a data of a specific type that might be null.

> 🧠 Since `S.nullAsOption` transforms value into `option` type, you can use `Option.getOr`/`Option.getOrWith` for it as well.

### **`nullable`**

`S.t<'value> => S.t<Nullable.t<'value>>`

```rescript
let schema = S.nullable(S.string)

"Hello World!"->S.parseOrThrow(~to=schema)
// Some("Hello World!")
%raw(`null`)->S.parseOrThrow(~to=schema)
// Null
%raw(`undefined`)->S.parseOrThrow(~to=schema)
// Undefined
```

The `S.nullable` schema represents a data of `Nullable.t` that might be null or undefined.

### **`nullableAsOption`**

`S.t<'value> => S.t<option<'value>>`

The same as `S.nullable`, but returns `option` type instead of `Nullable.t`. When encoding, it will return `undefined` for `None` values.

### **`literal`**

`'value => S.t<'value>`

```rescript
let tunaSchema = S.literal("Tuna")
let twelveSchema = S.literal(12)
let importantTimestampSchema = S.literal(1652628345865.)
let truSchema = S.literal(true)
let nullSchema = S.literal(Null.null) // Or use S.nullAsUnit
let undefinedSchema = S.literal() // Or use S.unit

// Uses Number.isNaN to match NaN literals
let nanSchema = S.literal(Float.Constants.nan)->S.shape(_ => ()) // For NaN literals I recomment adding S.shape to transform it to unit. It's better than having it as a float type

// Supports symbols and BigInt
let symbolSchema = S.literal(Symbol.asyncIterator)
let twobigSchema = S.literal(BigInt.fromInt(2))

// Supports variants and polymorphic variants
let appleSchema = S.literal(#apple)
let noneSchema = S.literal(None)

// Does a deep check for plain objects and arrays
let cliArgsSchema = S.literal(("help", "lint"))

// Supports functions and literally any Js values matching them with the === operator
let fn = () => "foo"
let fnSchema = S.literal(fn)
let weakMap = WeakMap.make()
let weakMapSchema = S.literal(weakMap)
```

The `S.literal` schema enforces that a data matches an exact value during parsing and encoding.

### **`object`**

`(S.Object.s => 'value) => S.t<'value>`

```rescript
type point = {
  x: int,
  y: int,
}

// The pointSchema will have the S.t<point> type
let pointSchema = S.object(s => {
  x: s.field("x", S.int),
  y: s.field("y", S.int),
})

// It can be used both for parsing and encoding
{"x": 1, "y": -4}->S.parseOrThrow(~to=pointSchema)
{x: 1, y: -4}->S.convertOrThrow(~from=pointSchema, ~to=S.unknown)
```

The `object` schema represents an object value, that can be transformed into any ReScript value. Here are some examples:

#### Transform object field names

```rescript
type user = {
  id: int,
  name: string,
}
// It will have the S.t<user> type
let schema = S.object(s => {
  id: s.field("USER_ID", S.int),
  name: s.field("USER_NAME", S.string),
})

{
  "USER_ID": 1,
  "USER_NAME": "John",
}->S.parseOrThrow(~to=schema)
// {id: 1, name: "John"}
{id: 1, name: "John"}->S.convertOrThrow(~from=schema, ~to=S.unknown)
// {"USER_ID": 1, "USER_NAME": "John"}
```

#### Transform to a structurally typed object

```rescript
// It will have the S.t<{"key1":string,"key2":string}> type
let schema = S.object(s => {
  "key1": s.field("key1", S.string),
  "key2": s.field("key2", S.string),
})
```

#### Transform to a tuple

```rescript
// It will have the S.t<(int, string)> type
let schema = S.object(s => (s.field("USER_ID", S.int), s.field("USER_NAME", S.string)))

{"USER_ID":1,"USER_NAME":"John"}->S.parseOrThrow(~to=schema)
// (1, "John")
```

The same schema also works for encoding:

```rescript
(1, "John")->S.convertOrThrow(~from=schema, ~to=S.unknown)
// {"USER_ID":1,"USER_NAME":"John"}
```

#### Transform to a variant

```rescript
type shape = Circle({radius: float}) | Square({x: float}) | Triangle({x: float, y: float})

// It will have the S.t<shape> type
let schema = S.object(s => {
  s.tag("kind", "circle")
  Circle({
    radius: s.field("radius", S.float),
  })
})

{
  "kind": "circle",
  "radius": 1,
}->S.parseOrThrow(~to=schema)
// Circle({radius: 1})
```

For values whose runtime representation matches your schema, you can use the less verbose `S.schema`. Under the hood, it'll create the same `S.object` schema from the example above.

```rescript
@tag("kind")
type shape =
  | @as("circle") Circle({radius: float})
  | @as("square") Square({x: float})
  | @as("triangle") Triangle({x: float, y: float})

let schema = S.schema(s => Circle({
  radius: s.matches(S.float),
}))
```

You can use the schema for parsing as well as encoding:

```rescript
Circle({radius: 1})->S.convertOrThrow(~from=schema, ~to=S.unknown)
// {
//   "kind": "circle",
//   "radius": 1,
// }
```

#### `s.flatten`

It's possible to spread/flatten an object schema in another object schema, allowing you to reuse schemas in a more powerful way.

```rescript
type entityData = {
  name: option<string>,
  age: int,
}
type entity = {
  id: string,
  ...entityData,
}

let entityDataSchema = S.object(s => {
  name: s.fieldOr("name", S.string, "Unknown"),
  age: s.field("age", S.int),
})
let entitySchema = S.object(s => {
  let {name, age} = s.flatten(entityDataSchema)
  {
    id: s.field("id", S.string),
    name,
    age,
  }
})
```

#### `s.nested`

A nice way to parse nested fields:

```rescript
let schema = S.object(s => {
  {
    id: s.field("id", S.string),
    name: s.nested("data").fieldOr("name", S.string, "Unknown")
    age: s.nested("data").field("age", S.int),
  }
})
```

The `s.nested` returns a complete `S.Object.s` context of the nested object, which you can use to define nested schema without any limitations.

#### Object destructuring

It's possible to destructure object field schemas inside of definition, as in the `s.flatten` example above.

```rescript
let entitySchema = S.object(s => {
  let {name, age} = s.field("data", entityDataSchema)
  {
    id: s.field("id", S.string),
    name,
    age,
  }
})
```

> 🧠 While the example with `s.flatten` expect an object with the type `{id: string, name: option<string>, age: int}`, the example above as well as for `s.nested` will expect an object with the type `{id: string, data: {name: option<string>, age: int}}`.

### **`strict`**

`S.t<'value> => S.t<'value>`

```rescript
// Represents an object without fields
let schema = S.object(_ => ())->S.strict

{
  "someField": "value",
}->S.parseOrThrow(~to=schema)
// throws S.error with the message: `Unrecognized key  "unknownKey"`
```

By default **Sury** silently strips unrecognized keys when parsing objects. You can change the behaviour to disallow unrecognized keys with the `S.strict` function.

If you want to change it for all schemas in your app, you can use `S.global` function:

```rescript
S.global({
  defaultAdditionalItems: Strict,
})
```

### **`strip`**

`S.t<'value> => S.t<'value>`

```rescript
// Represents an object with any fields
let schema = S.object(_ => ())->S.strip

{
  "someField": "value",
}->S.parseOrThrow(~to=schema)
// ()
```

You can use the `S.strip` function to reset a object schema to the default behavior (stripping unrecognized keys).

### **`deepStrict` & `deepStrip`**

Both `S.strict` and `S.strip` are applied for the first level of the object schema. If you want to apply it for all nested schemas, you can use `S.deepStrict` and `S.deepStrip` functions.

```rescript
let schema = S.schema(s =>
  {
    "bar": {
      "baz": s.matches(S.string),
    }
  }
)

schema->S.strict // {"baz": string} will still allow unknown keys
schema->S.deepStrict // {"baz": string} will not allow unknown keys
```

### **`schema`**

`(S.Schema.s => 'value) => S.t<'value>`

It's a helper built on `S.literal`, `S.object`, and `S.tuple` to create schemas for runtime representation of ReScript types conveniently.

```rescript
@unboxed
type answer =
  | Text(string)
  | MultiSelect(array<string>)
  | Other({value: string, @as("description") maybeDescription: option<string>})

let textSchema = S.schema(s => Text(s.matches(S.string)))
// It's going to be the same as:
// S.string->S.shape(string => Text(string))

let multySelectSchema = S.schema(s => MultiSelect(s.matches(S.array(S.string))))
// The same as:
// S.array(S.string)->S.shape(array => MultiSelect(array))

let otherSchema = S.schema(s => Other({
  value: s.matches(S.string),
  maybeDescription: s.matches(S.option(S.string)),
}))
// Creates the schema under the hood:
// S.object(s => Other({
//   value: s.field("value", S.string),
//   maybeDescription: s.field("description", S.option(S.string)),
// }))
//       Notice how the field name /|\ is taken from the type's @as attribute

let tupleExampleSchema = S.schema(s => (#id, s.matches(S.string)))
// The same as:
// S.tuple(s => (s.item(0, S.literal(#id)), s.item(1, S.string)))
```

> 🧠 Note that `S.schema` relies on the runtime representation of your type, while `S.object`/`S.tuple` are more flexible and require you to describe the schema explicitly.

### **`shape`**

`(S.t<'value>, 'value => 'shape) => S.t<'shape>`

The `S.shape` schema is a helper function that allows you to transform the value to a desired shape. It'll statically derive required data transformations to perform the change in the most optimal way.

> ⚠️ Even though it looks like you operate with a real value, it's actually a dummy proxy object. So conditions or any other runtime logic won't work. Please use `S.to` with `~custom` codecs for such cases.

```rescript
type shape = Circle({radius: float}) | Square({x: float}) | Triangle({x: float, y: float})

// It will have the S.t<shape> type
let schema = S.float->S.shape(radius => Circle({radius: radius}))

1->S.parseOrThrow(~to=schema)
// Circle({radius: 1.})
```

The same schema also works for encoding:

```rescript
Circle({radius: 1})->S.convertOrThrow(~from=schema, ~to=S.unknown)
// 1
```

### **`union`**

`array<S.t<'value>> => S.t<'value>`

An union represents a logical OR relationship. You can apply this concept to your schemas with `S.union`. This is the best API to use for variants and polymorphic variants.

On validation, the `S.union` schema returns the result of the first item that was successfully validated.

> 🧠 Members are matched in the order they are passed to `S.union` — the first one that fits the value wins.

It's also available as `S.anyOf`, matching the JSON Schema keyword it maps to.

```rescript
// TypeScript type for reference:
// type Shape =
// | { kind: "circle"; radius: number }
// | { kind: "square"; x: number }
// | { kind: "triangle"; x: number; y: number };
type shape = Circle({radius: float}) | Square({x: float}) | Triangle({x: float, y: float})

let shapeSchema = S.union([
  S.object(s => {
    s.tag("kind", "circle")
    Circle({
      radius: s.field("radius", S.float),
    })
  }),
  S.object(s => {
    s.tag("kind", "square")
    Square({
      x: s.field("x", S.float),
    })
  }),
  S.object(s => {
    s.tag("kind", "triangle")
    Triangle({
      x: s.field("x", S.float),
      y: s.field("y", S.float),
    })
  }),
])
```

```rescript
{
  "kind": "circle",
  "radius": 1,
}->S.parseOrThrow(~to=shapeSchema)
// Circle({radius: 1.})
```

```rescript
Square({x: 2.})->S.convertOrThrow(~from=shapeSchema, ~to=S.unknown)
// {
//   "kind": "square",
//   "x": 2,
// }
```

#### Enums

Also, you can describe a schema for a enum-like variant using `S.union` together with `S.literal`.

```rescript
type outcome = | @as("win") Win | @as("draw") Draw | @as("loss") Loss

let schema = S.union([
  S.literal(Win),
  S.literal(Draw),
  S.literal(Loss),
])

"draw"->S.parseOrThrow(~to=schema)
// Draw
```

Also, you can use `S.enum` as a shorthand for the use case above.

```rescript
let schema = S.enum([Win, Draw, Loss])
```

#### Converting to / from a union

`S.to` works with unions on either side of the conversion. There are three
cases.

**Single type → union.** Members are tried in the order you wrote them; the
first one that accepts the value wins:

```rescript
let schema = S.json->S.to(S.union([S.bigint->S.castToUnknown, S.string->S.castToUnknown]))

"123"->S.parseOrThrow(~to=schema) // 123n — the bigint member comes first
"abc"->S.parseOrThrow(~to=schema) // "abc" — not a valid bigint, so the string member takes it
true->S.parseOrThrow(~to=schema) // raises — no member accepts a bool
```

Notice that `true` wasn't converted to `"true"`, even though bool → string is
a supported conversion. A value is only converted into a member type the
source can't produce itself: JSON has no bigints, so strings are offered to
`S.bigint` — but JSON already has strings, so the `S.string` member only
accepts actual strings.

**Union → single type.** The mirror image — each member converts to the target
the same way it would with a direct `S.to`:

```rescript
let schema =
  S.union([S.bigint->S.castToUnknown, S.bool->S.castToUnknown])->S.to(S.string)

123n->S.parseOrThrow(~to=schema) // "123"
true->S.parseOrThrow(~to=schema) // "true"
```

**Union → union.** Values pass through to the member of the same type on the
other side — nothing is converted, so every member needs a counterpart. The one
exception: with no counterpart of its own, `S.option`'s `undefined` may pair
with `S.null`'s `null` on the other side, and vice versa:

```rescript
S.union([S.string->S.castToUnknown, S.float->S.castToUnknown])
->S.to(S.union([S.float->S.castToUnknown, S.string->S.castToUnknown])) // ✅ both pass through
S.option(S.string)->S.to(S.null(S.string)) // ✅ None <-> null
S.option(S.string)->S.to(S.null(S.bool)) // ❌ string has no counterpart
```

Good to know:

- Formats count as distinct types: `S.int` won't match a plain `S.float`
  member, and `S.json` won't match `S.string`.
- Nested unions are treated as one flat union: `S.union([S.string,
  S.union([S.float, S.bool])])` has three members.
- When a value fails a member — wrong type, failed refinement, or an error
  raised inside it — the next member gets a try. Only when all members fail
  does the union raise, listing each member's reason.

#### When a conversion is rejected

Some conversions have more than one reasonable meaning, and some have none.
Rather than guess, Sury rejects those with an `Invalid operation` error right
at the operation compilation — not later, on each value — and the error
suggests a rewrite that says what you mean.

**Ambiguous.** Given `"123"` — should it stay a string, or become a float?
Both readings are sensible, so Sury makes you pick:

```rescript
S.string->S.to(S.union([S.float->S.castToUnknown, S.string->S.castToUnknown]))
// Invalid operation: can't convert string to number | string — string has the same
// type as the source and the others don't.

// Convert to a float when possible, keep the string otherwise:
let asFloat = S.string->S.to(
  S.union([S.string->S.to(S.float)->S.castToUnknown, S.string->S.castToUnknown]),
)
"123"->S.parseOrThrow(~to=asFloat) // 123.
"abc"->S.parseOrThrow(~to=asFloat) // "abc"

// Or pass strings through, never producing a float:
let asString = S.string->S.to(
  S.union([S.never->S.to(S.float)->S.castToUnknown, S.string->S.castToUnknown]),
)
"123"->S.parseOrThrow(~to=asString) // "123"
"abc"->S.parseOrThrow(~to=asString) // "abc"
```

**The two unions don't cover each other.** Union-to-union converts nothing, so
a member with no same-type counterpart has nowhere to go:

```rescript
S.union([S.string->S.castToUnknown, S.float->S.castToUnknown])->S.to(
  S.union([
    S.float->S.castToUnknown,
    S.string->S.castToUnknown,
    S.bool->S.castToUnknown,
  ]),
)
// Invalid operation: ... boolean has no same-type variant on the other side.
S.option(S.string)->S.to(S.null(S.bool)) // ❌ string doesn't match boolean
S.option(S.string)->S.to(S.null(S.string->S.to(S.bool))) // ✅
```

**No conversion exists.** If a conversion between two types isn't supported
outside a union, putting it inside one doesn't change that. Use `S.never` to
mark a member as unreachable:

```rescript
S.bool->S.to(S.union([S.string->S.castToUnknown, S.symbol->S.castToUnknown]))
// ❌ bool -> symbol isn't supported
S.union([S.bool->S.castToUnknown, S.symbol->S.castToUnknown])->S.to(S.string)
// ❌ symbol -> string isn't supported
S.bool->S.to(
  S.union([S.string->S.castToUnknown, S.never->S.to(S.symbol)->S.castToUnknown]),
) // ✅ symbol marked unreachable
```

> 🧠 Union conversion always validates every member, so transformed unions stay
> consistent across decode and encode.

### **`list`**

`S.t<'value> => S.t<list<'value>>`

```rescript
let schema = S.list(S.string)

["Hello", "World"]->S.parseOrThrow(~to=schema)
// list{"Hello", "World"}
```

The `S.list` schema represents an array of data of a specific type which is transformed to ReScript's list data-structure.

### **`compactColumns`**

`S.t<'value> => S.t<array<array<'value>>>`

```rescript
let schema = S.compactColumns(S.schema(s => {
  id: s.matches(S.string),
  name: s.matches(S.nullAsOption(S.string)),
  deleted: s.matches(S.bool),
}))

[{id: "0", name: Some("Hello"), deleted: false}, {id: "1", name: None, deleted: true}]->S.convertOrThrow(~from=schema, ~to=S.unknown)
// [["0", "1"], ["Hello", null], [false, true]]
```

It flattens a nested array of objects into arrays of values by field — the layout described in [Boosting Postgres INSERT Performance by 2x With UNNEST](https://www.timescale.com/blog/boosting-postgres-insert-performance).

<details>

<summary>
Checkout the compiled code yourself:
</summary>

```javascript
(i) => {
  let v1 = [new Array(i.length), new Array(i.length), new Array(i.length)];
  for (let v0 = 0; v0 < i.length; ++v0) {
    let v3 = i[v0];
    try {
      let v4 = v3["name"];
      if (v4 === void 0) {
        v4 = null;
      }
      v1[0][v0] = v3["id"];
      v1[1][v0] = v4;
      v1[2][v0] = v3["deleted"];
    } catch (v2) {
      if (v2 && v2.s === s) {
        v2.path = [v0, ...v2.path];
      }
      throw v2;
    }
  }
  return v1;
};
```

</details>

### **`tuple`**

`(S.Tuple.s => 'value) => S.t<'value>`

```rescript
type point = {
  x: int,
  y: int,
}

// The pointSchema will have the S.t<point> type
let pointSchema = S.tuple(s => {
  s.tag(0, "point")
  {
    x: s.item(1, S.int),
    y: s.item(2, S.int),
  }
})

// It can be used both for parsing and encoding
["point", 1, -4]->S.parseOrThrow(~to=pointSchema)
{ x: 1, y: -4 }->S.convertOrThrow(~from=pointSchema, ~to=S.unknown)
```

The `S.tuple` schema represents that a data is an array of a specific length with values each of a specific type.

For short tuples without the need for transformation, there are wrappers over `S.tuple`:

### **`tuple1` - `tuple3`**

`(S.t<'v0>, S.t<'v1>, S.t<'v2>) => S.t<('v0, 'v1, 'v2)>`

```rescript
let schema = S.tuple3(S.string, S.int, S.bool)

%raw(`["a", 1, true]`)->S.parseOrThrow(~to=schema)
// ("a", 1, true)
```

### **`dict`**

`S.t<'value> => S.t<dict<'value>>`

```rescript
let schema = S.dict(S.string)

{
  "foo": "bar",
  "baz": "qux",
}->S.parseOrThrow(~to=schema)
// dict{foo: "bar", baz: "qux"}
```

The `dict` schema represents a dictionary of data of a specific type.

### **`date`**

`S.t<Js.Date.t>`

```rescript
let schema = S.date

Date.fromString("2024-01-01T00:00:00Z")->S.parseOrThrow(~to=schema) // passes
%raw(`new Date("invalid")`)->S.parseOrThrow(~to=schema) // throws - Invalid Date
%raw(`"2024-01-01"`)->S.parseOrThrow(~to=schema) // throws - not a Date instance
```

The `S.date` schema validates that the input is a `Date` instance and rejects Invalid Date.

> Unlike `S.isoDateTime` (which validates ISO datetime strings) and `S.string->S.to(S.date)` (which decodes ISO strings into Date objects), `S.date` validates existing Date instances directly.

You can use `S.to` to decode between strings and dates:

```rescript
// Decode ISO string to Date
let schema = S.string->S.to(S.date)
"2024-01-01T00:00:00.000Z"->S.parseOrThrow(~to=schema) // Date

// Encode Date to ISO string
Date.fromString("2024-01-01T00:00:00.000Z")->S.convertOrThrow(~from=schema, ~to=S.unknown) // "2024-01-01T00:00:00.000Z"
```

### **`isoDateTime`**

`S.t<string>`

```rescript
let schema = S.isoDateTime

"2020-01-01T00:00:00Z"->S.parseOrThrow(~to=schema) // "2020-01-01T00:00:00Z"
"not-a-date"->S.parseOrThrow(~to=schema) // throws
```

Standalone string schema that validates ISO 8601 UTC datetime strings. See also [ISO datetimes](#iso-datetimes) under Strings for more details and examples.

### **`instance`**

`S.t<instance>`

```rescript
let schema: S.t<Set.t<string>> = S.instance(%raw(`Set`))->Obj.magic;
```

The `S.instance` schema represents an instance of a class. Requires some type casting to make it work, but better than `S.unknown` as a building block for more complex schemas.

### **`blob`**

`S.t<Js.Blob.t>`

```rescript
S.blob // Expected Blob
S.blob->S.maxSize(1_000_000) // Expected Blob.size <= 1000000
S.blob->S.minSize(1) // Expected Blob.size >= 1
S.blob->S.size(2) // Expected Blob.size == 2
S.blob->S.maxSize(1_000_000, ~message="Too large")
```

`S.minSize`, `S.maxSize` and `S.size` bound the size in bytes. They work on any
`S.instance` schema with a `.size`, counting entries rather than bytes.

> Strings and arrays use `S.minLength`/`S.maxLength`/`S.length` instead.
> A lower bound of `0` is dropped; a negative one is an error.

### **`file`**

`S.t<Js.File.t>`

```rescript
let schema = S.file->S.maxSize(1_000_000)

%raw(`new File(["hi"], "a.txt")`)->S.parseOrThrow(~to=schema) // passes
%raw(`new Blob(["hi"])`)->S.parseOrThrow(~to=schema) // throws - Expected File, received Blob
```

A `File` is a `Blob`, so it also satisfies [`S.blob`](#blob) — not the other way
round. It takes the same size bounds.

### **`json`**

`S.t<JSON.t>`

```rescript
let schema = S.json

`"abc"`->S.parseOrThrow(~to=schema)
// "abc" of type JSON.t
```

The `S.json` schema represents a data that is compatible with JSON.

### **`jsonString`**

`S.t<string>`

```rescript
let schema = S.jsonString->S.to(S.int)

"123"->S.parseOrThrow(~to=schema)
// 123
```

The `S.jsonString` schema represents JSON string.

There's also `S.jsonStringWithSpace` to configure space in the JSON string during encoding.

### **Content**

Bytes in JSON become base64. They are not mangled as UTF-8.

#### Bytes in a JSON field

A field of bytes is written as base64. You do not pass Pack or Unpack.

```rescript
{
  payload: %raw(`new Uint8Array([137, 80, 78, 71])`),
}->S.convertOrThrow(
  ~from=S.schema(s => {payload: s.matches(S.uint8Array)}),
  ~to=S.jsonString,
)
// `{"payload":"iVBORw=="}`
```

#### A JWT segment

JWT segments are base64url. Parse the text as JSON, then as the object.

```rescript
"eyJzdWIiOiJhIn0"->S.parseOrThrow(
  ~to=S.base64url->S.to(
    S.jsonString->S.to(S.schema(s => {sub: s.matches(S.string)})),
  ),
)
// {sub: "a"}
```

#### Switch base64 alphabets

`S.base64url` is URL-safe and has no padding.

```rescript
S.base64 // standard alphabet, canonical padding
S.base64url // URL-safe alphabet, no padding

"iVBORw=="->S.parseOrThrow(~to=S.base64->S.to(S.base64url))
// "iVBORw"
```

#### The bytes are JSON text

```rescript
S.uint8Array->S.to(S.jsonString, ~custom={decode: Unpack, encode: Pack})
// decode unpack, encode pack
```

#### The JSON string holds the bytes

```rescript
S.uint8Array->S.to(S.jsonString, ~custom={decode: Pack, encode: Unpack})
// decode pack, encode unpack
```

#### If you omit Pack or Unpack

Sury does not guess when both conversions exist.

```rescript
S.uint8Array->S.to(S.jsonString)
// Ambiguous conversion from Uint8Array to JSON string.
// Use S.to(from, to, "unpack" | "pack")
```

#### UTF-8, the same bytes, parse, or widen

```rescript
S.uint8Array->S.to(S.string) // UTF-8
S.base64->S.to(S.uint8Array) // the same bytes
S.jsonString->S.to(S.string) // parses
S.base64->S.to(S.string) // widens
```

### **`meta`**

`(S.t<'value>, S.meta) => S.t<'value>`

Use `S.meta` to add a metadata to the resulting schema.

```rescript
let documentedStringSchema = S.string
  ->S.meta({description: "A useful bit of text, if you know what to do with it."})

(documentedStringSchema->S.untag).description // A useful bit of text...
```

This can be useful for documenting fields, generating JSON, etc.

```rescript
schema->S.inputJSONSchema
// {
//   "type": "string",
//   "description": "A useful bit of text, if you know what to do with it."
// }
```

`S.outputJSONSchema` describes the other side — what the schema produces rather than what it accepts.

### **`recursive`**

`(string, t<'value> => t<'value>) => t<'value>`

You can define a recursive schema in **Sury**.

```rescript
type rec node = {
  id: string,
  children: array<node>,
}

let nodeSchema = S.recursive("Node", nodeSchema => {
  S.object(s => {
    id: s.field("Id", S.string),
    children: s.field("Children", S.array(nodeSchema)),
  })
})
```

```rescript
{
  "Id": "1",
  "Children": [
    {"Id": "2", "Children": []},
    {"Id": "3", "Children": [{"Id": "4", "Children": []}]},
  ],
}->S.parseOrThrow(~to=nodeSchema)
// {
//   id: "1",
//   children: [{id: "2", children: []}, {id: "3", children: [{id: "4", children: []}]}],
// }
```

The same schema works for encoding:

```rescript
{
  id: "1",
  children: [{id: "2", children: []}, {id: "3", children: [{id: "4", children: []}]}],
}->S.convertOrThrow(~from=nodeSchema, ~to=S.unknown)
// {
//   "Id": "1",
//   "Children": [
//     {"Id": "2", "Children": []},
//     {"Id": "3", "Children": [{"Id": "4", "Children": []}]},
//   ],
// }
```

You can also use asynchronous parser:

```rescript
let paramsSchema = S.schema(s => {name: s.matches(S.string)})

let nodeSchema = S.recursive("Node", nodeSchema => {
  S.object(s => {
    params: s.field(
      "Id",
      S.string->S.to(paramsSchema, ~custom={decode: Async(id => loadParams(~id)), encode: Never}),
    ),
    children: s.field("Children", S.array(nodeSchema)),
  })
})
```

One great aspect of the example above is that it uses parallelism to make four requests to check for the existence of nodes.

> 🧠 Despite supporting recursive schema, passing cyclical data will cause an infinite loop.

## Custom schema

**Sury** might not have many built-in schemas for your use case. In this case you can create a custom schema for any TypeScript type.

1. Choose a base schema which is the closest to your type. Most likely it'll be `S.instance`.
2. Use `S.to` with `~custom` codecs to add a custom decoder and encoder.
3. Optionally, use `S.meta` to add customize the name of the schema and additional metadata.

```rescript
let mySet = itemSchema => {
  S.instance(%raw(`Set`))
  ->S.to(
    // The escape hatch earns its keep here: no schema carries `Set.t<'item>`,
    // and `S.any` is what lets `mySet` return `S.t<Set.t<'item>>`. Reach for a
    // real target everywhere it exists — see `to` with `~custom`.
    S.any,
    ~custom={
      decode: Sync(
        input => {
          let output = Set.make()
          input
          ->Obj.magic
          ->Set.forEach(
            item => {
              output->Set.add(S.parseOrThrow(item, ~to=itemSchema))
            },
          )
          output
        },
      ),
      encode: Never,
    },
  )
  ->S.meta({name: `Set.t<${S.inputExpression(itemSchema)}>`})
}

let intSetSchema = mySet(S.int)

S.parseOrThrow(%raw(`new Set([1, 2, 3])`), ~to=intSetSchema) // passes
S.parseOrThrow(%raw(`new Set([1, 2, "3"])`), ~to=intSetSchema) // throws S.Error: Expected int32, received "3"
S.parseOrThrow(%raw(`[1, 2, 3]`), ~to=intSetSchema) // throws S.Error: Expected Set.t<int32>, received [1, 2, 3]
```

## Refinements

**Sury** lets you provide custom validation logic via refinements. Refinements let you define checks that are not expressible in the type system alone — for example, checking that a number is positive or that a string is a valid email address.

### **`refine`**

`(S.t<'value>, 'value => bool, ~error: string=?, ~path: S.Path.t=?) => S.t<'value>`

```rescript
let positiveNumberSchema = S.int->S.refine(value => value > 0)
```

Refinement functions should return `true` to indicate success or `false` to signal failure. By default, a failed refinement throws with the message `"Refinement failed"`.

#### Custom error message

Provide a custom error message via the `~error` labeled argument:

```rescript
let shortStringSchema = S.string->S.refine(
  value => value->String.length <= 255,
  ~error="String can't be more than 255 characters",
)
```

#### Custom error path

When refining an object schema, you can use the `~path` labeled argument to attach the error to a specific field. It is an `S.Path.t`, the same array `error.path` carries: `String` segments for keys and `Number` ones for array indices, so `[String("items"), Number(0.)]` reports `Failed at items[0]`:

```rescript
let passwordFormSchema = S.object(s => {
  "password": s.field("password", S.string),
  "confirm": s.field("confirm", S.string),
})->S.refine(
  data => data["password"] === data["confirm"],
  ~error="Passwords don't match",
  ~path=S.Path.fromArray(["confirm"]),
)
```

#### Chaining refinements

Refinements can be chained. Each refinement is applied in order:

```rescript
let evenPositiveSchema = S.int
  ->S.refine(value => value > 0, ~error="Must be positive")
  ->S.refine(value => mod(value, 2) === 0, ~error="Must be even")
```

The refine function is applied for both parsing and encoding.

## Transforms

**Sury** allows to augment a conversion with custom logic, letting you transform the value during parsing and encoding. This is most commonly used for mapping the value to more convenient data-structures.

<a id="transform"></a>

### **`to` with `~custom`**

`(S.t<'from>, S.t<'to>, ~custom: S.codecs<'from, 'to>=?) => S.t<'to>`

When no built-in conversion fits, pass your own coders:

```rescript
let intToString = schema =>
  schema->S.to(
    S.string,
    ~custom={
      decode: Sync(int => int->Int.toString),
      encode: Sync(
        string =>
          switch string->Int.fromString {
          | Some(int) => int
          | None => JsError.make("Can't convert string to int")->JsError.throw
          },
      ),
    },
  )
```

Each direction is one of:

```rescript
Sync(fn)   // a coder
Async(fn)  // a coder returning a promise, run with parseAsyncOrThrow
Auto       // keep the built-in conversion for this direction
Never      // this direction is impossible, fail when an operation needs it
Pack       // store this direction's source as a value the target holds
Unpack     // open this direction's source and hand its payload over
```

See [Content](#content). A pair is always opposites.

```rescript
S.uint8Array->S.to(S.jsonString, ~custom={decode: Pack, encode: Unpack})
// decode pack, encode unpack
```

```rescript
// Trim on decode, built-in validation on encode
S.string->S.to(S.string, ~custom={decode: Sync(String.trim), encode: Auto})

// Load a user by id
let userSchema = S.schema(s => {id: s.matches(S.uuid), name: s.matches(S.string)})

S.uuid->S.to(
  userSchema,
  ~custom={decode: Async(userId => loadUser(~userId)), encode: Sync(user => user.id)},
)
```

Describe what you decode into. The target is what validates the coder's result,
types the output, and exports to JSON Schema.

> 🧠 `S.any` accepts anything, so it checks nothing about what a coder returns.
> It's the escape hatch for a value no schema can describe — reach for it last,
> not first.

A coder fails by throwing, and the path it was reached through is prepended:

```rescript
"abc"->S.convertOrThrow(~from=S.int->intToString, ~to=S.unknown)
// Can't convert string to int
```

Any exception works, a ReScript one (`throw(Failure("..."))`) included, but only
a JS error carries a message, so anything else is reported by its structure.
To name a path or the schemas involved, build the error and throw that:

```rescript
S.Error.make(
  InvalidInput({
    reason: "Can't convert string to int",
    path: S.Path.empty,
    expected: S.unknown,
    received: S.unknown,
  }),
)->S.Error.throw
```

## Functions on schema

### At a glance

`S.t<'value>` names the output type, so operations on that side carry no prefix; the ones that look at the input side say so in their name.

|           | Input side                             | Output side                              | Crosses both                                    |
| --------- | -------------------------------------- | ---------------------------------------- | ----------------------------------------------- |
| Convert   |                                        |                                          | `parseOrThrow`, `decodeOrThrow`, `parser`, `decoder` |
| Construct |                                        | `constructor`, `asyncConstructor`        |                                                 |
| Assert    | `assertOrThrow`                        |                                          |                                                 |
| Describe  | `inputJSONSchema`, `inputExpression`   | `outputJSONSchema`, `outputExpression`   |                                                 |

### Pipelines

Conversion targets are schemas, not dedicated functions: `S.json`, `S.jsonString`, `S.unknown`, `S.date`, and `S.uint8Array` are ordinary schemas usable at any position in a chain. Describe the shape of the data at each stage with `~from` and `~to`, and Sury compiles the whole pipeline into a single function via `new Function`.

```rescript
// Validate any input value.
data->S.parseOrThrow(~to=userSchema)

// Parse a JSON string, then validate.
rawString->S.convertOrThrow(~from=S.jsonString, ~to=userSchema)

// Encode a domain value all the way out to a JSON string.
user->S.convertOrThrow(~from=userSchema, ~to=S.jsonString)

// Pre-compile pipelines once, call them many times.
let parseJsonUser = S.compileConvertOrThrow(~from=S.jsonString, ~to=userSchema)
let stringifyUser = S.compileConvertOrThrow(~from=userSchema, ~to=S.jsonString)
```

The **same pipeline idea works inside schemas** via [`S.to`](#to). A field, an array element, a tuple slot — any nested schema can be its own multi-stage chain:

```rescript
let apiUserSchema = S.schema(s =>
  {
    // Arrives as a JSON string, which is parsed and validated as an array of addresses.
    "addresses": s.field("addresses", S.jsonString->S.to(S.array(addressSchema))),

    // Arrives as bytes, decoded as UTF-8, mapped to a Date.
    "createdAt": s.field("createdAt", S.uint8Array->S.to(S.string)->S.to(S.date)),

    // Element-level transforms work the same way.
    "ids": s.field("ids", S.array(S.string->S.to(S.bigint))),
  }
)
```

`S.to` is the same compiler as `S.compileConvertOrThrow` and `S.convertOrThrow`, just used at a single point in a larger schema. The whole tree — top-level operation plus every nested `S.to` — still folds into one generated function, so deep pipelines stay free of runtime overhead.

> 🧠 `S.parseOrThrow` and `S.assertOrThrow` aren't separate primitives — they're just specializations of `S.convertOrThrow` with `S.unknown` on the input side. `data->S.parseOrThrow(~to=schema)` is `data->S.convertOrThrow(~from=S.unknown, ~to=schema)`. `data->S.assertOrThrow(~to=schema)` runs a decoder from `S.unknown` through the schema to `S.literal()->S.noValidation(true)` — the target is a no-op constant with validation disabled, so the compiler emits the schema's validation but no output-construction code at all. That's why `assertOrThrow` is 2–3× faster than `parseOrThrow`.

### Built-in operations

Every operation is named `[compile]` + verb + `[Async]` + `[OrThrow]`.

`parse`, `convert` and `make` come in both flavors: the bare name returns a `result<'value, S.error>`, and the `OrThrow` name throws `S.Exn`. Asserting has only the throwing form, `assertOrThrow`. `validate` is the non-throwing counterpart to it and answers with a `bool` rather than a `result`, since there is no value to hand back either way.

The `compile` prefix returns the operation as a function to call repeatedly — the fastest way to run one schema many times. Only the throwing operations compile: a compiled operation is for the hot path, where the `result` allocation per call is the cost you are avoiding, so wrap the compiled function yourself if you want a `result` there. `compileValidate` is the exception, since its answer is already a bool.

| Verb         | One-shot                     | Compiled                     | Async                                                  |
| ------------ | ---------------------------- | ---------------------------- | ------------------------------------------------------ |
| **parse**    | `parse`, `parseOrThrow`      | `compileParseOrThrow`        | `parseAsync`, `parseAsyncOrThrow`, `compileParseAsyncOrThrow`     |
| **convert**  | `convert`, `convertOrThrow`  | `compileConvertOrThrow`      | `convertAsync`, `convertAsyncOrThrow`, `compileConvertAsyncOrThrow` |
| **assert**   | `assertOrThrow`              |                              | `assertAsyncOrThrow`                                   |
| **validate** | `validate`                   | `compileValidate`            |                                                        |
| **make**     | `make`, `makeOrThrow`        | `compileMakeOrThrow`         | `makeAsync`, `makeAsyncOrThrow`, `compileMakeAsyncOrThrow`        |

**Parsing** validates the input value against the schema and transforms it to the expected output type:

```
S.parseOrThrow: ('any, ~to: S.t<'value>) => 'value
S.parse: ('any, ~to: S.t<'value>) => result<'value, S.error>
S.compileParseOrThrow: (~to: S.t<'value>) => 'any => 'value
```

```rescript
let parse = S.compileParseOrThrow(~to=S.string)
parse("Hello world!") // "Hello world!"

switch data->S.parse(~to=schema) {
| Ok(value) => Console.log(value)
| Error(error) => Console.log(error.message)
}
```

**Converting** transforms a value from one schema's output type to another's. The input isn't validated — `~from` is trusted, and the type is derived from it. Pass `~via` to route through an intermediate schema:

```
S.convertOrThrow: ('from, ~from: S.t<'from>, ~via: S.t<'via>=?, ~to: S.t<'to>) => 'to
S.convert: ('from, ~from: S.t<'from>, ~via: S.t<'via>=?, ~to: S.t<'to>) => result<'to, S.error>
S.compileConvertOrThrow: (~from: S.t<'from>, ~via: S.t<'via>=?, ~to: S.t<'to>) => 'from => 'to
```

```rescript
// Parse JSON value
data->S.convertOrThrow(~from=S.json, ~to=schema)

// Parse JSON string
data->S.convertOrThrow(~from=S.jsonString, ~to=schema)

// Parse JSON string, validating it as JSON on the way
data->S.convertOrThrow(~from=S.jsonString, ~via=S.json, ~to=schema)

// Encode to unknown
data->S.convertOrThrow(~from=schema, ~to=S.unknown)

// Encode to JSON string with space
data->S.convertOrThrow(~from=schema, ~to=S.jsonStringWithSpace(2))

// Compile once, run many times
let toJsonString = S.compileConvertOrThrow(~from=schema, ~to=S.jsonString)
```

`~via` runs that schema's own validation, so `~via=S.json` rejects a JSON string that parses to something `S.json` doesn't accept.

Also, you can use `S.noValidation` helper to turn off type validations for the schema even when it's used with a parse operation.

**Asserting** validates the input value without returning a transformed result. Since no output is constructed, it's 2-3 times faster than `parseOrThrow` depending on the schema:

```
S.assertOrThrow: ('any, ~to: S.t<'value>) => ()
S.assertAsyncOrThrow: ('any, ~to: S.t<'value>) => promise<()>
```

**Validating** is the non-throwing assert. `assert` is a ReScript keyword, so the boolean-returning flavor is spelled `validate`:

```
S.validate: ('any, ~to: S.t<'value>) => bool
S.compileValidate: (~to: S.t<'value>) => 'any => bool
```

**Making** checks a value you built in code rather than received from the wire. Every check the schema carries runs — types, the conversion, refinements — and the value itself comes back, not a decoded copy, so an entity the schema has no way to encode fails at construction rather than at the point it's sent. `S.t<'value>` names the output type, so this is the JS `outputConstructor`:

```
S.makeOrThrow: ('value, ~schema: S.t<'value>) => 'value
S.make: ('value, ~schema: S.t<'value>) => result<'value, S.error>
S.compileMakeOrThrow: (~schema: S.t<'value>) => 'value => 'value
```

```rescript
let userSchema = S.object(s => {
  id: s.field("id", S.string),
  email: s.field("email", S.email),
})
let makeUser = S.compileMakeOrThrow(~schema=userSchema)

makeUser({id: "1", email: "billie@example.com"})
// returns the very record it was given

makeUser({id: "1", email: "not-an-address"})
// throws S.Exn: Failed at email: Expected email, received "not-an-address"
```

The `OrThrow` operations throw an exception which you can catch with `try/catch` block:

```rescript
try true->S.parseOrThrow(~to=schema) catch {
| S.Exn(error) => Console.log(error.message)
}
```

The bare names give the same failure as a `result`. Only a Sury failure becomes `Error`; any other exception propagates. Use `S.Error.classify` to match on the error's details.

### **`reverse`**

`(S.t<'value>) => S.t<'value>`

```rescript
S.nullAsOption(S.string)->S.reverse
// S.option(S.string)
```

```rescript
let schema = S.object(s => s.field("foo", S.string))

{"foo": "bar"}->S.parseOrThrow(~to=schema)
// "bar"

let reversed = schema->S.reverse

"bar"->S.parseOrThrow(~to=reversed)
// {"foo": "bar"}

123->S.parseOrThrow(~to=reversed)
// throws S.error with the message: `Expected string, received 123`
```

Reverses the schema. This gets especially magical for schemas with transformations 🪄

### **`to`**

`(S.t<'from>, S.t<'to>) => S.t<'to>`

This very powerful API allows you to coerce another data type in a declarative way. Let's say you receive a number that is passed to your system as a string. For this `S.to` is the best fit:

```rescript
let schema = S.string->S.to(S.float)

"123"->S.parseOrThrow(~to=schema) //? 123.
"abc"->S.parseOrThrow(~to=schema) //? throws: Expected number, received "abc"

// Reverse works correctly as well 🔥
123.->S.convertOrThrow(~from=schema, ~to=S.unknown) //? "123"
```

### **`name`**

```rescript
let schema = S.literal({"abc": 123})->S.meta({name: "Abc"})

(schema->S.untag).name // "Abc"
```

Used internally for readable error messages.

### **`inputExpression`**

`(S.t<'value>) => string`

```rescript
S.literal({"abc": 123})->S.inputExpression
// "{ "abc": 123 }"

S.string->S.meta({name: "Address"})->S.inputExpression
// "Address"
```

Used internally for readable error messages.

> 🧠 The format is subject to change

### **`outputExpression`**

`(S.t<'value>) => string`

```rescript
let schema = S.string->S.to(S.int)

schema->S.inputExpression
// "string"

schema->S.outputExpression
// "int32"
```

The same expression for the schema's output type.

> 🧠 The format is subject to change

### **`toString`**

`(unit) => string` on the untagged schema

```rescript
(S.string->S.untag).toString()
// "Schema<string>"

(S.string->S.to(S.int)->S.untag).toString()
// "Schema<string, int32>"
```

Both sides at once, in the order the type declares them — `Schema<TInput, TOutput>` — with the second parameter dropped when the two sides match.

`Console.log(schema)` deliberately still shows the internal schema shape, which is usually what you want when you're inspecting one. Call `toString` when you want the expression.

The output side is derived through [`reverse`](#reverse), so nested transforms are reported correctly:

```rescript
(S.array(S.string->S.to(S.int))->S.untag).toString()
// "Schema<string[], int32[]>"
```

> 🧠 The format is subject to change

### **`noValidation`**

`(S.t<'value>, bool) => S.t<'value>`

```rescript
let schema = S.object(s => s.field("abc", S.int))->S.noValidation(true)

{
  "abc": 123,
}->S.parseOrThrow(~to=schema) // This doesn't have `if (typeof i !== "object" || !i) {` check. But field types are still validated.
// 123
```

Removes validation for the provided schema. Nested schemas are not affected.

This can be useful to optimise `S.object` parsing when you construct the input data yourself.

## Standard Schema

Every schema implements the [Standard Schema](https://standardschema.dev/) spec (and its [JSON Schema](https://standardschema.dev/json-schema) extension) via `~standard`, typed by the `StandardSchema` module and readable through `S.untag`:

```rescript
let standard = (S.string->S.untag).standard

standard.validate("abc") // {value: "abc"}

S.enableStandardJSONSchema() // Once, to opt into jsonSchema (keeps the conversion tree-shakeable otherwise)
(standard.jsonSchema->Option.getUnsafe).input({target: StandardSchema.JsonSchema.Draft07})
// {type: "string", $schema: "http://json-schema.org/draft-07/schema#"}
```

## Error handling

**Sury** throws `S.error` error containing detailed information about the validation problems.

```rescript
let schema = S.literal(false)

true->S.parseOrThrow(~to=schema)
// throws S.error with the message: `Expected false, received true`
```

If you want to handle the error, the best way to use `try/catch` block:

```rescript
try true->S.parseOrThrow(~to=schema) catch {
| S.Exn(error) => Console.log(error.message)
}
```

## Global config

**Sury** has a global config that can be changed to customize the behavior of the library.

### `defaultAdditionalItems`

`defaultAdditionalItems` is an option that controls how unknown keys are handled when parsing objects. The default value is `Strip`, but you can globally change it to `Strict` to enforce strict object parsing.

```rescript
S.global({
  defaultAdditionalItems: Strict,
})
```

### `disableNanNumberValidation`

`disableNanNumberValidation` is an option that controls whether the library should check for NaN values when parsing numbers. The default value is `false`, but you can globally change it to `true` to allow NaN values. If you parse many numbers which are guaranteed to be non-NaN, you can set it to `true` to improve performance ~10%, depending on the case.

```rescript
S.global({
  disableNanNumberValidation: true,
})
```
