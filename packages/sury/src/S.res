@@uncurried
@@warning("-30")

type never

module Path = {
  type t

  external toString: t => string = "%identity"

  let empty: t = %raw(`""`)
  let dynamic: t = %raw(`"[]"`)

  @module("sury") external toArray: t => array<string> = "$pathToArray"
  @module("sury") external fromArray: array<string> => t = "$pathFromArray"
  @module("sury") external fromLocation: string => t = "$pathFromLocation"
  @module("sury") external concat: (t, t) => t = "$pathConcat"
}


type tag =
  | @as("string") String
  | @as("number") Number
  | @as("bigint") BigInt
  | @as("boolean") Boolean
  | @as("symbol") Symbol
  | @as("null") Null
  | @as("undefined") Undefined
  | @as("nan") NaN
  | @as("function") Function
  | @as("instance") Instance
  | @as("array") Array
  | @as("object") Object
  | @as("anyOf") AnyOf
  | @as("never") Never
  | @as("unknown") Unknown
  | @as("ref") Ref


type numberFormat = | @as("int32") Int32 | @as("port") Port
type stringFormat =
  | @as("json") JSON
  | @as("date-time") DateTime
  | @as("email") Email
  | @as("uuid") Uuid
  | @as("cuid") Cuid
  | @as("url") Url
type arrayFormat = | @as("compactColumns") CompactColumns

type format = | ...numberFormat | ...stringFormat | ...arrayFormat

@unboxed
type additionalItemsMode = | @as("strip") Strip | @as("strict") Strict

/**
A type definition that exists at runtime. `S.t<'value>` describes the
ReScript value the schema produces; the JavaScript input shape lives in the
schema itself, so one definition parses and encodes:

```rescript
let filmSchema = S.object(s => {
  "id": s.field("Id", S.float),
  "title": s.field("Title", S.string),
})

%raw(`{"Id": 1, "Title": "My first film"}`)->S.parseOrThrow(~to=filmSchema)
// {"id": 1., "title": "My first film"}
```

The runtime representation is JSON Schema-shaped and readable as-is — match
on the variant to inspect it.
*/
@tag("type")
type rec t<'value> =
  private
  | @as("never")
  Never({
      name?: string,
      title?: string,
      description?: string,
      deprecated?: bool,
      errorMessage?: schemaErrorMessage,
    })
  | @as("unknown")
  Unknown({
      name?: string,
      description?: string,
      title?: string,
      deprecated?: bool,
      examples?: array<unknown>,
      default?: unknown,
      errorMessage?: schemaErrorMessage,
    })
  | @as("string")
  String({
      const?: string,
      format?: stringFormat,
      name?: string,
      title?: string,
      description?: string,
      deprecated?: bool,
      examples?: array<string>,
      default?: string,
      minLength?: int,
      maxLength?: int,
      pattern?: RegExp.t,
      errorMessage?: schemaErrorMessage,
    })
  | @as("number")
  Number({
      const?: float,
      format?: numberFormat,
      name?: string,
      title?: string,
      description?: string,
      deprecated?: bool,
      examples?: array<float>,
      default?: float,
      minimum?: float,
      maximum?: float,
      exclusiveMinimum?: float,
      exclusiveMaximum?: float,
      errorMessage?: schemaErrorMessage,
    })
  | @as("bigint")
  BigInt({
      const?: bigint,
      name?: string,
      title?: string,
      description?: string,
      deprecated?: bool,
      examples?: array<bigint>,
      default?: bigint,
      minimum?: bigint,
      maximum?: bigint,
      exclusiveMinimum?: bigint,
      exclusiveMaximum?: bigint,
      errorMessage?: schemaErrorMessage,
    })
  | @as("boolean")
  Boolean({
      const?: bool,
      name?: string,
      title?: string,
      description?: string,
      deprecated?: bool,
      examples?: array<bool>,
      default?: bool,
      errorMessage?: schemaErrorMessage,
    })
  | @as("symbol")
  Symbol({
      const?: Symbol.t,
      name?: string,
      title?: string,
      description?: string,
      deprecated?: bool,
      examples?: array<Symbol.t>,
      default?: Symbol.t,
      errorMessage?: schemaErrorMessage,
    })
  | @as("null")
  Null({
      const: null<unit>,
      name?: string,
      title?: string,
      description?: string,
      deprecated?: bool,
      errorMessage?: schemaErrorMessage,
    })
  | @as("undefined")
  Undefined({
      const: unit,
      name?: string,
      title?: string,
      description?: string,
      deprecated?: bool,
      errorMessage?: schemaErrorMessage,
    })
  | @as("nan")
  NaN({
      const: float,
      name?: string,
      title?: string,
      description?: string,
      deprecated?: bool,
      errorMessage?: schemaErrorMessage,
    })
  | @as("function")
  Function({
      const?: Type.Classify.function,
      name?: string,
      title?: string,
      description?: string,
      deprecated?: bool,
      examples?: array<Type.Classify.function>,
      default?: Type.Classify.function,
      errorMessage?: schemaErrorMessage,
    })
  | @as("instance")
  Instance({
      class: unknown,
      const?: Type.Classify.object,
      name?: string,
      title?: string,
      description?: string,
      deprecated?: bool,
      examples?: array<Type.Classify.object>,
      default?: Type.Classify.object,
      errorMessage?: schemaErrorMessage,
    })
  | @as("array")
  Array({
      items: array<t<unknown>>,
      additionalItems: additionalItems,
      format?: arrayFormat,
      name?: string,
      title?: string,
      description?: string,
      deprecated?: bool,
      examples?: array<array<unknown>>,
      default?: array<unknown>,
      minItems?: int,
      maxItems?: int,
      errorMessage?: schemaErrorMessage,
    })
  | @as("object")
  Object({
      properties: dict<t<unknown>>,
      additionalItems: additionalItems,
      required?: array<string>,
      name?: string,
      title?: string,
      description?: string,
      deprecated?: bool,
      examples?: array<dict<unknown>>,
      default?: dict<unknown>,
      errorMessage?: schemaErrorMessage,
    })
  | @as("anyOf")
  AnyOf({
      anyOf: array<t<unknown>>,
      has: has,
      name?: string,
      title?: string,
      description?: string,
      deprecated?: bool,
      examples?: array<unknown>,
      default?: unknown,
      errorMessage?: schemaErrorMessage,
    })
  | @as("ref")
  Ref({
      @as("$ref")
      ref: string,
      errorMessage?: schemaErrorMessage,
    })
@unboxed and additionalItems = | ...additionalItemsMode | Schema(t<unknown>)
and schema<'a> = t<'a>
and schemaErrorMessage = {
  @as("_")
  catchAll?: string,
  format?: string,
  @as("type")
  type_?: string,
  minimum?: string,
  maximum?: string,
  exclusiveMinimum?: string,
  exclusiveMaximum?: string,
  minLength?: string,
  maxLength?: string,
  minItems?: string,
  maxItems?: string,
  pattern?: string,
}
and meta<'value> = {
  name?: string,
  title?: string,
  description?: string,
  deprecated?: bool,
  examples?: array<'value>,
  errorMessage?: schemaErrorMessage,
}
and untagged = private {
  @as("type")
  tag: tag,
  seq: float,
  @as("$ref")
  ref?: string,
  @as("$defs")
  defs?: dict<t<unknown>>,
  const?: unknown,
  class?: unknown,
  format?: format,
  name?: string,
  title?: string,
  description?: string,
  deprecated?: bool,
  examples?: array<unknown>,
  default?: unknown,
  noValidation?: bool,
  items?: array<t<unknown>>,
  required?: array<string>,
  properties?: dict<t<unknown>>,
  additionalItems?: additionalItems,
  anyOf?: array<t<unknown>>,
  has?: dict<bool>,
  to?: t<unknown>,
  @as("~standard")
  standard: StandardSchema.props<unknown, unknown>,
  // Inherited from the schema prototype rather than an own property, so it is
  // always present even though nothing in the record literal sets it.
  toString: unit => string,
}
and has = {
  string?: bool,
  number?: bool,
  never?: bool,
  unknown?: bool,
  bigint?: bool,
  boolean?: bool,
  symbol?: bool,
  null?: bool,
  undefined?: bool,
  nan?: bool,
  function?: bool,
  instance?: bool,
  array?: bool,
  object?: bool,
}
and error = private {
  message: string,
  reason: string,
  path: Path.t,
}
@tag("code")
and errorDetails =
  // When received input doesn't match the expected schema
  | @as("invalid_input")
  InvalidInput({
      path: Path.t,
      reason: string,
      expected: schema<unknown>,
      received: schema<unknown>,
      input?: unknown,
      unionErrors?: array<error>,
    })
  // When an operation fails, because it's impossible or called incorrectly
  | @as("invalid_operation") InvalidOperation({path: Path.t, reason: string})
  // When the value decoding between two schemas is not supported
  | @as("unsupported_decode")
  UnsupportedDecode({
      path: Path.t,
      reason: string,
      from: schema<unknown>,
      to: schema<unknown>,
    })
  // When a decoder/encoder fails
  | @as("invalid_conversion")
  InvalidConversion({
      path: Path.t,
      reason: string,
      from: schema<unknown>,
      to: schema<unknown>,
      cause?: exn,
    })
  | @as("unrecognized_keys") UnrecognizedKeys({path: Path.t, reason: string, keys: array<string>})

/**
Everything Sury throws. `error.message` includes the failure path; match on
`S.Error.classify(error)` for structured detail.

```rescript
switch %raw(`123`)->S.parseOrThrow(~to=S.string) {
| _ => ()
| exception S.Exn(error) => Console.log(error.message)
}
```
*/
type exn += private Exn(error)

// =============================================================================
// Bindings to the TypeScript core
// =============================================================================
//
// Sury's implementation lives in src/*.ts, bundled into the package
// entry by scripts/pack.ts (see src/entry.ts). This module is the ReScript
// face of it: the public types above, plus `@module("sury") external`
// bindings below, resolved through the package root "." conditional export
// (import -> the ESM S.mjs, require -> the published CJS S.js). That's what
// makes the bindings work for consumers compiling to either module format —
// a plain relative `@module("./S.mjs")` would break under a "commonjs"
// package-spec (require()-ing an ESM file throws).

external castToUnknown: t<'any> => t<unknown> = "%identity"
external castToAny: t<'value> => t<'any> = "%identity"
external untag: t<'any> => untagged = "%identity"

// ReScript's `catch { | Exn(e) => }` compiles to a `RE_EXN_ID === Exn`
// identity test against the constructor id synthesized right here by the
// `type exn +=` declaration above. The throwing side lives in core.ts, so
// hand it that identity once at module load — SuryError's RE_EXN_ID getter
// returns it. `%raw` because a private exn constructor can't be referenced
// as a value from ReScript code, only from spliced JS.
%%private(@module("sury") external __setExnId: unknown => unit = "$setExnId")
let () = __setExnId(%raw(`Exn`))

module Flag = {
  type t
  let none: t = %raw(`0`)
  let async: t = %raw(`1`)
  external with: (t, t) => t = "%orint"
}
type flag = Flag.t

module Error = {
  type class

  @module("sury") external class: class = "Error"

  @module("sury") @new external make: errorDetails => error = "Error"

  external classify: error => errorDetails = "%identity"

  external throw: error => 'a = "%raise"
}

// Primitive schema values — the same eager, PURE-annotated instances the JS
// entry exports (see src/entry.ts), so both surfaces share one object per
// primitive. Some (string, bool, ...) shadow stdlib names on purpose.
/** Fails on every value. Useful to forbid a field, or with `S.to` to mark a union member unreachable. */
@module("sury") external never: t<never> = "never"
/** Accepts any value as-is. */
@module("sury") external unknown: t<unknown> = "unknown"
/** Matches `undefined` — shorthand for `S.literal()`. */
@module("sury") external unit: t<unit> = "$unit"
/** Matches `null` and parses it to `()` — shorthand for `S.literal(Null.null)->S.to(S.unit)`. */
@module("sury") external nullAsUnit: t<unit> = "$nullAsUnit"
@module("sury") external string: t<string> = "string"
@module("sury") external bool: t<bool> = "bool"
/** A 32-bit integer — `"type": "integer"` in JSON Schema terms. */
@module("sury") external int: t<int> = "int"
@module("sury") external float: t<float> = "float"
@module("sury") external bigint: t<bigint> = "bigint"
@module("sury") external symbol: t<Symbol.t> = "symbol"
/** Matches `NaN`. Consider `S.nan->S.shape(_ => ())` to expose it as `unit` instead of `float`. */
@module("sury") external nan: t<float> = "nan"
/**
A `Date` instance that isn't Invalid Date. Validates existing objects — for
"ISO string -> Date" use `S.string->S.to(S.date)`.
*/
@module("sury") external date: t<Date.t> = "date"
/** Any JSON value. Also a pipeline stage: `schema->S.to(S.json)` describes "whatever this is on the wire, as JSON". */
@module("sury") external json: t<JSON.t> = "json"
/**
A string containing valid JSON. Chain it to parse and validate in one
generated function — no `JSON.parse` in your own code:

```rescript
let schema = S.jsonString->S.to(S.float)

"123"->S.parseOrThrow(~to=schema) // 123.
123.->S.decodeOrThrow(~from=schema, ~to=S.unknown) // "123"
```
*/
@module("sury") external jsonString: t<string> = "jsonString"
/** `S.jsonString` that pretty-prints with the given indentation when encoding. */
@module("sury") external jsonStringWithSpace: int => t<string> = "jsonStringWithSpace"
/** A `Uint8Array` instance. Chain `S.uint8Array->S.to(S.string)` to decode a UTF-8 byte payload. */
@module("sury") external uint8Array: t<Uint8Array.t> = "uint8Array"
/**
An ISO 8601 UTC datetime string — no timezone offsets, arbitrary sub-second
precision. To decode into a `Date.t`, use `S.string->S.to(S.date)` instead.
*/
@module("sury") external isoDateTime: t<string> = "isoDateTime"
/** A valid TCP port number. */
@module("sury") external port: t<int> = "port"
/**
An email address, by a deliberately simple regex — the only real way to
validate an email is to send something to it.
*/
@module("sury") external email: t<string> = "email"
@module("sury") external uuid: t<string> = "uuid"
@module("sury") external cuid: t<string> = "cuid"
@module("sury") external url: t<string> = "url"

/**
Matches an exact value during parsing and encoding — any value works,
including variants, and plain objects and arrays are deep-checked:

```rescript
S.literal("Tuna")
S.literal(#polymorphicVariant)
S.literal(("help", "lint")) // tuple literal, checked deeply
```
*/
@module("sury") external literal: 'value => t<'value> = "literal"
@module("sury") external array: t<'value> => t<array<'value>> = "array"
/**
Rows laid out as one column-array per field — pair with `S.to` to move
between the two layouts in both directions:

```rescript
type row = {id: string, deleted: bool}

let schema = S.compactColumns(
  S.schema(s => {id: s.matches(S.string), deleted: s.matches(S.bool)}),
)

[{id: "0", deleted: false}]->S.decodeOrThrow(~from=schema, ~to=S.unknown)
// [["0"], [false]]
```
*/
@module("sury") external compactColumns: t<'value> => t<array<array<'value>>> = "compactColumns"
/** An array on the JavaScript side, a `list` on yours. */
@module("sury") external list: t<'value> => t<list<'value>> = "list"
/**
Validates `data instanceof class_` — the base for custom schemas around
JavaScript classes; add decode/encode logic with `S.to` or `S.transform`.

```rescript
let blobSchema = S.instance(%raw(`Blob`))
```
*/
@module("sury") external instance: unknown => t<unknown> = "instance"
/** `dict<'value>` — validates the values, keeps the keys. */
@module("sury") external dict: t<'value> => t<dict<'value>> = "dict"
/**
A value that might be `undefined`:

```rescript
"Hi"->S.parseOrThrow(~to=S.option(S.string)) // Some("Hi")
```

Add a default with `S.Option.getOr` — or `fieldOr` for object fields.
*/
@module("sury") external option: t<'value> => t<option<'value>> = "$option"
// The public JS `nullable` called without a default is exactly
// `union([item, literal(null)])` — what ReScript calls `S.null`.
/** A value that might be `null`, kept as `null<'value>`. Prefer `S.nullAsOption` to work with `option` instead. */
@module("sury") external null: t<'value> => t<null<'value>> = "nullable"
/** A value that might be `null`, parsed to `option<'value>` — `None` encodes back to `null`. */
@module("sury") external nullAsOption: t<'value> => t<option<'value>> = "$nullAsOption"
/** A value that might be `null` or `undefined`, kept as `nullable<'value>`. */
@module("sury") external nullable: t<'value> => t<nullable<'value>> = "nullish"
/** A value that might be `null` or `undefined`, parsed to `option<'value>` — `None` encodes back to `undefined`. */
@module("sury") external nullableAsOption: t<'value> => t<option<'value>> = "$nullableAsOption"
/**
Logical OR: members are matched in the order they're passed and the first
fit wins.

```rescript
type shape = Circle({radius: float}) | Square({x: float})

let shapeSchema = S.union([
  S.schema(s => Circle({radius: s.matches(S.float)})),
  S.schema(s => Square({x: s.matches(S.float)})),
])
```
*/
@module("sury") external union: array<t<'value>> => t<'value> = "union"
/** Alias of `S.union`, matching the JSON Schema keyword it maps to. */
@module("sury") external anyOf: array<t<'value>> => t<'value> = "anyOf"
/** Shorthand for a union of literals: `S.enum([#GBP, #USD])` is `S.union([S.literal(#GBP), S.literal(#USD)])`. */
@module("sury") external enum: array<'value> => t<'value> = "enum"

/**
A copy of the schema with metadata attached — it surfaces in
`S.toJSONSchema` output and in error messages (`name`, `errorMessage`).

```rescript
S.string->S.meta({description: "User-visible label"})
S.email->S.meta({errorMessage: {format: "Must be a valid email"}})
```
*/
@module("sury") external meta: (t<'value>, meta<'value>) => t<'value> = "meta"

type transformDefinition<'input, 'output> = {
  @as("p")
  parser?: 'input => 'output,
  @as("a")
  asyncParser?: 'input => promise<'output>,
  @as("s")
  serializer?: 'output => 'input,
}
/**
Custom parse/encode logic when `S.to` and `S.shape` can't express it. Provide
`parser` (or `asyncParser`) and `serializer`; omitting a side makes that
direction fail:

```rescript
let trimmedSchema = S.string->S.transform(() => {
  parser: value => value->String.trim,
  serializer: value => value,
})
```
*/
@module("sury")
external transform: (t<'input>, unit => transformDefinition<'input, 'output>) => t<'output> =
  "$transform"

// The public JS `refine` takes an options object; build it here from the
// ReScript labeled args.
type refineOptions = {error?: string, path?: array<string>}
@module("sury")
external refine: (t<'value>, 'value => bool, refineOptions) => t<'value> = "refine"
/**
Custom validation for checks the type system can't express. Return `false`
to fail; the check runs on both parse and encode.

```rescript
let positiveSchema = S.float->S.refine(value => value > 0., ~error="Must be positive")
```

Use `~path` to attach the error to a specific field.
*/
let refine = (schema, refiner, ~error=?, ~path=?) => refine(schema, refiner, {?error, ?path})

/**
Declarative restructuring: the callback receives a proxy, not the value —
property accesses are recorded and compiled to direct assignments, and the
change reverses for encoding. No conditions or other runtime logic inside;
reach for `S.transform` when you need that.

```rescript
type circle = Circle({radius: float})

let circleSchema = S.float->S.shape(radius => Circle({radius: radius}))

1.->S.parseOrThrow(~to=circleSchema) // Circle({radius: 1.})
```
*/
@module("sury") external shape: (t<'value>, 'value => 'shape) => t<'shape> = "shape"

/**
Converts to another schema, inferring the coercion — and its reverse — from
the two types. Works at the top level or inside any field, and the whole
chain compiles into one generated function.

```rescript
let schema = S.string->S.to(S.float)

"123"->S.parseOrThrow(~to=schema) // 123.
123.->S.decodeOrThrow(~from=schema, ~to=S.unknown) // "123"
```
*/
@module("sury") external to: (t<'from>, t<'to>) => t<'to> = "to"

/**
The same schema with input and output swapped — validation and
transformations run backwards. The static type becomes `t<unknown>` since
the input side has no ReScript type.
*/
@module("sury") external reverse: t<'value> => t<unknown> = "reverse"

/**
Compiles the schema into a function that validates any input and returns
the typed output — the fastest way to parse repeatedly. One-off calls read
better as `data->S.parseOrThrow(~to=schema)`.

```rescript
let parse = S.parser(~to=S.string)

parse("Hello world!") // "Hello world!"
```
*/
@module("sury") external parser: (~to: t<'value>) => 'any => 'value = "parser"
/** `S.parser` for schemas with async transformations — the returned function resolves to the output. */
@module("sury") external asyncParser: (~to: t<'value>) => 'any => promise<'value> = "asyncParser"
// The public JS `decoder` compiles from a schema's Input space; the ReScript
// flavor decodes FROM a schema's Output space, so reverse `from` first.
@module("sury") external decoder: (t<unknown>, t<'to>) => 'from => 'to = "decoder"
@module("sury")
external asyncDecoder: (t<unknown>, t<'to>) => 'from => promise<'to> = "asyncDecoder"
/**
Compiles a conversion between two schemas, fused into one generated
function. The input is trusted to match `~from` — its type checks are
skipped, everything downstream still runs. For untrusted input use
`S.parser`.

```rescript
let userSchema = S.schema(s => {"id": s.matches(S.string)})

// JSON text -> validated user, in one pass
let parseUser = S.decoder(~from=S.jsonString, ~to=userSchema)
parseUser(`{"id":"1"}`) // {"id": "1"}

// Encode by decoding from the schema to S.unknown or S.jsonString
let encodeUser = S.decoder(~from=userSchema, ~to=S.unknown)
encodeUser({"id": "1"}) // {"id": "1"}
```
*/
let decoder = (~from: t<'from>, ~to) => decoder(reverse(from), to)
/** `S.decoder` for schemas with async transformations — the returned function resolves to the output. */
let asyncDecoder = (~from: t<'from>, ~to) => asyncDecoder(reverse(from), to)
// Single-schema (Input -> Output) flavors — the public JS `decoder` /
// `asyncDecoder` called with one argument.
/** Compiles a conversion from the schema's JavaScript input shape to its output — `S.decoder` for a single schema's own two sides. */
@module("sury") external decoder1: t<'value> => unknown => 'value = "decoder"
/** `S.decoder1` for schemas with async transformations. */
@module("sury") external asyncDecoder1: t<'value> => unknown => promise<'value> = "asyncDecoder"

/**
Validates any input against the schema and returns the typed output, or
throws `S.Exn`. Compiles on first use — for hot paths pre-compile with
`S.parser`.

```rescript
%raw(`{"x": 1}`)->S.parseOrThrow(~to=S.schema(s => {"x": s.matches(S.int)}))
// {"x": 1}
```
*/
let parseOrThrow = (any, ~to) => parser(~to)(any)
/** `S.parseOrThrow` for schemas with async transformations. */
let parseAsyncOrThrow = (any, ~to) => asyncParser(~to)(any)
/**
Validates without building an output — 2–3× faster than `S.parseOrThrow`.
Throws `S.Exn` on invalid input.
*/
@module("sury") external assertOrThrow: ('any, ~to: t<'value>) => unit = "assert"
/** `S.assertOrThrow` for schemas with async transformations. */
@module("sury")
external assertAsyncOrThrow: ('any, ~to: t<'value>) => promise<unit> = "$assertAsyncOrThrow"
/**
Converts a value between two schemas — `S.decoder` applied immediately.
Encoding is decoding from your schema into `S.unknown` (or `S.jsonString`):

```rescript
let schema = S.schema(s => {"id": s.matches(S.string->S.to(S.bigint))})

{"id": 1n}->S.decodeOrThrow(~from=schema, ~to=S.unknown)
// {"id": "1"}
```
*/
let decodeOrThrow = (any, ~from, ~to) => decoder(~from, ~to)(any)
/** `S.decodeOrThrow` for schemas with async transformations. */
let decodeAsyncOrThrow = (any, ~from, ~to) => asyncDecoder(~from, ~to)(any)

/** Whether the schema contains async logic, so parsing needs the `async`/`Async` operations. */
@module("sury") external isAsync: t<'value> => bool = "isAsync"

/**
A schema that references itself. The identifier names the schema in errors
and `$defs`.

```rescript
type rec node = {id: string, children: array<node>}

let nodeSchema = S.recursive("Node", nodeSchema =>
  S.object(s => {
    id: s.field("id", S.string),
    children: s.field("children", S.array(nodeSchema)),
  })
)
```
*/
@module("sury") external recursive: (string, t<'value> => t<'value>) => t<'value> = "recursive"

/**
Turns off the schema's own type checks in parse operations — transforms and
refinements still run. For trusted data where you only want the conversion.
*/
@module("sury") external noValidation: (t<'value>, bool) => t<'value> = "noValidation"

/**
Human-readable expression of the schema's input type, e.g. `"{ x: int32; }"`
— what error messages print. The format is subject to change.
*/
@module("sury") external inputExpression: t<'value> => string = "inputExpression"

/** `S.inputExpression` for the schema's output type. */
@module("sury") external outputExpression: t<'value> => string = "outputExpression"

module Schema = {
  type s = {@as("m") matches: 'value. t<'value> => 'value}
}
/**
Turns a definition into a schema: plain values become deep-checked literals,
and `s.matches` embeds a schema at any position — objects, tuples and
variants included. Reach for `S.object` only when field names need
transformation.

```rescript
@tag("kind")
type shape = | @as("circle") Circle({radius: float})

let shapeSchema = S.schema(s => Circle({radius: s.matches(S.float)}))
let pairSchema = S.schema(s => (s.matches(S.string), 42))
```
*/
@module("sury") external schema: (Schema.s => 'value) => t<'value> = "$schema"

module Object = {
  type rec s = {
    @as("f") field: 'value. (string, t<'value>) => 'value,
    fieldOr: 'value. (string, t<'value>, 'value) => 'value,
    tag: 'value. (string, 'value) => unit,
    nested: string => s,
    flatten: 'value. t<'value> => 'value,
  }
}

/**
An object that transforms to any ReScript value — rename fields, apply
defaults, tag variants — with zero runtime overhead, and the same schema
encodes it back:

```rescript
type user = {id: int, name: string}

let userSchema = S.object(s => {
  id: s.field("USER_ID", S.int),
  name: s.fieldOr("USER_NAME", S.string, "Unknown"),
})

%raw(`{"USER_ID": 1, "USER_NAME": "John"}`)->S.parseOrThrow(~to=userSchema)
// {id: 1, name: "John"}
```

The context also offers `tag` for discriminants, `flatten` to reuse another
object schema's fields, and `nested` for reaching into child objects. When
field names match your type, prefer the lighter `S.schema`.
*/
@module("sury") external object: (Object.s => 'value) => t<'value> = "object"

/** Restores the default policy of silently stripping unknown object keys (top level only). */
@module("sury") external strip: t<'value> => t<'value> = "strip"
/** `S.strip` applied to every nested object schema as well. */
@module("sury") external deepStrip: t<'value> => t<'value> = "deepStrip"
/**
Fails on unknown object keys instead of stripping them (top level only —
see `S.deepStrict`). To make this the default for every schema, use
`S.global({defaultAdditionalItems: Strict})`.
*/
@module("sury") external strict: t<'value> => t<'value> = "strict"
/** `S.strict` applied to every nested object schema as well. */
@module("sury") external deepStrict: t<'value> => t<'value> = "deepStrict"

module Tuple = {
  type s = {
    item: 'value. (int, t<'value>) => 'value,
    tag: 'value. (int, 'value) => unit,
  }
}

/**
A fixed-length array that transforms to any ReScript value:

```rescript
type point = {x: int, y: int}

let pointSchema = S.tuple(s => {
  s.tag(0, "point")
  {x: s.item(1, S.int), y: s.item(2, S.int)}
})
```

For plain tuples, `S.schema(s => (s.matches(S.string), s.matches(S.int)))`
or `S.tuple2`/`S.tuple3` are lighter.
*/
@module("sury") external tuple: (Tuple.s => 'value) => t<'value> = "tuple"
/** A single-item tuple: `[item]` on the JavaScript side, the bare item value on yours. */
let tuple1 = v0 => tuple(s => s.item(0, v0))
@module("sury") external tuple2: array<t<unknown>> => t<'value> = "schema"
let tuple2 = (v1, v2) => tuple2([castToUnknown(v1), castToUnknown(v2)])
@module("sury") external tuple3: array<t<unknown>> => t<'value> = "schema"
let tuple3 = (v1, v2, v3) => tuple3([castToUnknown(v1), castToUnknown(v2), castToUnknown(v3)])

module Option = {
  /**
  Replaces `None` with a default on parse:

  ```rescript
  let schema = S.option(S.string)->S.Option.getOr("Unknown")

  %raw(`undefined`)->S.parseOrThrow(~to=schema) // "Unknown"
  ```

  For object fields, `s.fieldOr` is more convenient.
  */
  @module("sury")
  external getOr: (t<option<'value>>, 'value) => t<'value> = "$Option_getOr"
  /** `S.Option.getOr` with the default computed on each parse. */
  @module("sury")
  external getOrWith: (t<option<'value>>, unit => 'value) => t<'value> = "$Option_getOrWith"
}

module Metadata = {
  module Id = {
    type t<'metadata>
    @module("sury")
    external make: (~namespace: string, ~name: string) => t<'metadata> = "$Metadata_Id_make"
  }

  @module("sury")
  external get: (t<'value>, ~id: Id.t<'metadata>) => option<'metadata> = "$Metadata_get"

  @module("sury")
  external set: (t<'value>, ~id: Id.t<'metadata>, 'metadata) => t<'value> = "$Metadata_set"
}

// =============
// Built-in refinements
// =============

// The bound is typed as the schema's own value, so one external serves int,
// float and bigint. It admits nonsense the JS side has to catch — a bound on a
// `t<string>`, say — which is why gt/gte/lt/lte validate both the schema tag
// and the bound's runtime type before building anything.
/**
Requires the value to be greater than the bound. Works on `S.int`, `S.float`
and `S.bigint`; an int format's own range takes part, so a bound outside it
fails where it's written. All built-in refinements take `~message`:

```rescript
S.float->S.gt(0., ~message="Must be positive")
```
*/
@module("sury") external gt: (t<'value>, 'value, ~message: string=?) => t<'value> = "gt"
/** Requires the value to be greater than or equal to the bound — see `S.gt`. */
@module("sury") external gte: (t<'value>, 'value, ~message: string=?) => t<'value> = "gte"
/** Requires the value to be less than the bound — see `S.gt`. */
@module("sury") external lt: (t<'value>, 'value, ~message: string=?) => t<'value> = "lt"
/** Requires the value to be less than or equal to the bound — see `S.gt`. */
@module("sury") external lte: (t<'value>, 'value, ~message: string=?) => t<'value> = "lte"

/**
Requires `length >= n` — works on strings and arrays, like the rest of the
length refinements:

```rescript
S.string->S.minLength(5, ~message="Too short")
S.array(S.string)->S.minLength(1)
```
*/
@module("sury") external minLength: (t<'value>, int, ~message: string=?) => t<'value> = "minLength"
/** Requires `length <= n` — see `S.minLength`. */
@module("sury") external maxLength: (t<'value>, int, ~message: string=?) => t<'value> = "maxLength"
/** Requires `length == n` — see `S.minLength`. */
@module("sury") external length: (t<'value>, int, ~message: string=?) => t<'value> = "length"
/** Requires an empty string or array — see `S.minLength`. */
@module("sury") external empty: (t<'value>, ~message: string=?) => t<'value> = "empty"
/** Requires a non-empty string or array — see `S.minLength`. */
@module("sury") external nonEmpty: (t<'value>, ~message: string=?) => t<'value> = "nonEmpty"

/**
Requires the string to match the regex:

```rescript
S.string->S.pattern(%re(`/^\d+$/`), ~message="Must be numeric")
```
*/
@module("sury")
external pattern: (t<string>, RegExp.t, ~message: string=?) => t<string> = "pattern"
/** Trims surrounding whitespace on parse. */
@module("sury") external trim: t<string> => t<string> = "trim"

type toJSONSchemaOptions = {target?: StandardSchema.JsonSchema.target}
/**
Emits `draft-07` (the default), `draft-2020-12`, or `openapi-3.0`.
Properties and examples come out in the schema's input format.

```rescript
S.string->S.toJSONSchema
// {"type": "string"}
```
*/
@module("sury")
external toJSONSchema: (t<'value>, ~options: toJSONSchemaOptions=?) => JSONSchema.t = "toJSONSchema"
/** Builds a schema from a JSON Schema document — the reverse of `S.toJSONSchema`. */
@module("sury") external fromJSONSchema: JSONSchema.t => t<JSON.t> = "fromJSONSchema"
/** Attaches raw JSON Schema keywords that merge into `S.toJSONSchema` output. */
@module("sury")
external extendJSONSchema: (t<'value>, JSONSchema.t) => t<'value> = "extendJSONSchema"
// Enables `~standard.jsonSchema`; its input/output throw before this is called.
@module("sury") external enableStandardJSONSchema: unit => unit = "enableStandardJSONSchema"

type globalConfigOverride = {
  defaultAdditionalItems?: additionalItemsMode,
  disableNanNumberValidation?: bool,
}

/**
Overrides library-wide defaults — call once at app startup, before
operations are compiled:

```rescript
S.global({defaultAdditionalItems: Strict})
```
*/
@module("sury") external global: globalConfigOverride => unit = "global"

