@@uncurried
@@warning("-30")

type never

/** Where inside a value a failure happened — `["items"]["0"]["id"]`, and
    `S.Path.empty` at the root. */
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


type numberFormat = | @as("int32") Int32 | @as("port") Port | @as("integer") Integer
type stringFormat =
  | @as("json") JSON
  | @as("date-time") DateTime
  | @as("email") Email
  | @as("uuid") Uuid
  | @as("cuid") Cuid
  | @as("uri") Uri
  | @as("date") Date
  | @as("time") Time
  | @as("duration") Duration
  | @as("hostname") Hostname
  | @as("idn-hostname") IdnHostname
  | @as("ipv4") Ipv4
  | @as("ipv6") Ipv6
  | @as("uri-reference") UriReference
  | @as("uri-template") UriTemplate
  | @as("iri") Iri
  | @as("iri-reference") IriReference
  | @as("idn-email") IdnEmail
  | @as("json-pointer") JsonPointer
  | @as("relative-json-pointer") RelativeJsonPointer
type arrayFormat = | @as("compactColumns") CompactColumns

type format = | ...numberFormat | ...stringFormat | ...arrayFormat

@unboxed
type additionalItemsMode = | @as("strip") Strip | @as("strict") Strict

/**
A schema for `'value`: one value that knows how to parse, encode and document
that type.

The variant mirrors JSON Schema, so a schema reads as data — matching on it is
how you inspect one:

```rescript
switch S.string->S.castToUnknown {
| String(_) => "a string"
| _ => "something else"
}
```

Constructors are `private`, so build schemas with the factories below and reach
the fields every schema shares (`seq`, `to`, `~standard`, …) through
`S.untag`.
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
      multipleOf?: float,
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
      multipleOf?: bigint,
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
      minSize?: int,
      maxSize?: int,
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
  multipleOf?: string,
  minLength?: string,
  maxLength?: string,
  minItems?: string,
  maxItems?: string,
  minSize?: string,
  maxSize?: string,
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
/** Carried by `S.Exn`. `message` is formatted for display, `reason` is the same
    without the path; `S.Error.classify` opens up the rest. */
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
What every operation throws.

```rescript
try data->S.parseOrThrow(~to=schema) catch {
| S.Exn(error) => Console.log(error.message)
}
```
*/
type exn += private Exn(error)

// =============================================================================
// Bindings to the TypeScript core
// =============================================================================
//
// This module is the ReScript face of Sury: the public types above, plus the
// `@module("sury") external` bindings below, resolved through the package root
// "." conditional export (import -> the ESM entry, require -> the CJS one).
// That's what makes them work whichever module format you compile to — a plain
// relative `@module("./index.mjs")` would break under a "commonjs"
// package-spec (require()-ing an ESM file throws).

/** Forgets what a schema parses to, so schemas of different types can share an
    array. Free — it's the same value. */
external castToUnknown: t<'any> => t<unknown> = "%identity"
/** `castToUnknown` in the other direction: unchecked, so the type you name is
    the type you're asserting. */
external castToAny: t<'value> => t<'any> = "%identity"
/** The schema as a flat record, for the fields every variant shares — `to`,
    `~standard`, `toString`, `$defs`. */
external untag: t<'any> => untagged = "%identity"

// ReScript's `catch { | Exn(e) => }` compiles to a `RE_EXN_ID === Exn`
// identity test against the constructor id synthesized right here by the
// `type exn +=` declaration above. The runtime that throws needs the same
// identity, so hand it over once at module load — SuryError's RE_EXN_ID getter
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

/** Building and reading Sury errors. Throw one from a `S.transform` when the
    default (a plain JS error wrapped as `InvalidConversion`) doesn't say
    enough:

    ```rescript
    S.Error.make(
      InvalidInput({
        reason: "Can't convert string to int",
        path: S.Path.empty,
        expected: S.unknown,
        received: S.unknown,
      }),
    )->S.Error.throw
    ``` */
module Error = {
  type class

  @module("sury") external class: class = "Error"

  @module("sury") @new external make: errorDetails => error = "Error"

  /** Opens up an `error` into the variant carrying its per-code fields. */
  external classify: error => errorDetails = "%identity"

  external throw: error => 'a = "%raise"
}

// Primitive schema values — the very instances the JS entry exports, so both
// surfaces share one object per primitive. Some (string, bool, ...) shadow
// stdlib names on purpose.
/** Accepts nothing — every value fails. Inside a union it marks a member as
    unreachable rather than unsupported, which is how you tell `S.to` that a
    conversion is deliberately not offered. */
@module("sury") external never: t<never> = "never"
/** Accepts anything, validating nothing — the source side of every
    `S.parseOrThrow`. */
@module("sury") external unknown: t<unknown> = "unknown"
/** `undefined`, as `unit`. Shorthand for `S.literal()`. */
@module("sury") external unit: t<unit> = "$unit"
/** `null`, as `unit` — `S.literal(Null.null)->S.to(S.unit)`. */
@module("sury") external nullAsUnit: t<unit> = "$nullAsUnit"
@module("sury") external string: t<string> = "string"
@module("sury") external bool: t<bool> = "bool"
/** A number inside ReScript's `int` (int32) range, emitted as `format: "int32"`.
    The range is a real bound, so `S.int->S.gte(3000000000)` is a contradiction
    that throws where it's written. Use `S.integer` beyond it. */
@module("sury") external int: t<int> = "int"
// `t<float>`, not `t<int>`: ReScript's `int` is int32, and a JS integer
// (JSON Schema's unbounded `integer`) can exceed that range.
/** A whole number of any magnitude, emitted as `format: "integer"`. */
@module("sury") external integer: t<float> = "integer"
/** Any JS number. `NaN` is rejected, unless `S.global` turns that check off. */
@module("sury") external float: t<float> = "float"
@module("sury") external bigint: t<bigint> = "bigint"
@module("sury") external symbol: t<Symbol.t> = "symbol"
/** `NaN`, matched with `Number.isNaN`. Usually better mapped away:
    `S.nan->S.shape(_ => ())`. */
@module("sury") external nan: t<float> = "nan"
/** A `Date` instance; Invalid Date is rejected. `S.string->S.to(S.date)` decodes
    a timestamp string into one, `S.isoDateTime` validates one that stays a
    string. */
@module("sury") external date: t<Date.t> = "date"
/** Any JSON value, checked all the way down. A good source for `S.to` when the
    shape is decided elsewhere: `S.json->S.to(userSchema)`. */
@module("sury") external json: t<JSON.t> = "json"
/**
A string holding valid JSON. Chain it to say what the JSON must contain:

```rescript
S.jsonString->S.to(userSchema) // parse and validate in one pass
S.float->S.to(S.jsonString) // stringify
```

Encoding to it generates a dedicated stringifier instead of calling
`JSON.stringify`, usually 1.3-2x faster.
*/
@module("sury") external jsonString: t<string> = "jsonString"
/** `S.jsonString` that indents by `space` when encoding. */
@module("sury") external jsonStringWithSpace: int => t<string> = "jsonStringWithSpace"
/** A `Uint8Array`. `S.uint8Array->S.to(S.string)` decodes it as UTF-8, and the
    reverse encodes. */
@module("sury") external uint8Array: t<Uint8Array.t> = "uint8Array"
// `Js.Blob.t`/`Js.File.t` rather than a pair of abstract types declared here:
// the stdlib has no Blob or File module, and these two are the compiler's own
// builtin abstract types — the ones untagged variants match on — so a value
// from any other binding unifies with these.
/** A `Blob`, sized in bytes with `S.minSize`/`S.maxSize`/`S.size`. */
@module("sury") external blob: t<Js.Blob.t> = "blob"
/** A `File`. Every `File` satisfies `S.blob`, not the other way round. */
@module("sury") external file: t<Js.File.t> = "file"
/**
RFC 3339 timestamp, **UTC only** — an offset like `+02:00` is rejected, which is
narrower than the `date-time` format it emits. Calendar-aware, and sub-second
precision is unbounded.

```rescript
"1963-06-19T08:30:06.283185Z"->S.parseOrThrow(~to=S.isoDateTime)
```
*/
@module("sury") external isoDateTime: t<string> = "isoDateTime"
/** TCP/UDP port: an integer in 0-65535. */
@module("sury") external port: t<int> = "port"
/** Email address, ASCII only — `"joe.bloggs@example.com"`. Practical rather than
    exhaustive: it wants a dot-TLD domain, so `a@localhost` is rejected. */
@module("sury") external email: t<string> = "email"
/** UUID in canonical 8-4-4-4-12 hex form, any version —
    `"f81d4fae-7dec-11d0-a765-00a0c91e6bf6"`. */
@module("sury") external uuid: t<string> = "uuid"
/** CUID — `"cjld2cjxh0000qzrmn831i7rn"`. Not a JSON Schema format, so
    `S.toJSONSchema` emits a plain string for it. */
@module("sury") external cuid: t<string> = "cuid"
/**
URI string, RFC 3986 — a scheme is required, so `/dashboard` needs
`S.uriReference` instead. `S.url` is the parsed `URL` object version.

Syntax only: **any** scheme parses, `javascript:` included. Narrow it by
composing, and the emitted JSON Schema keeps both constraints:

```rescript
S.uri->S.pattern(%re(`/^https:\/\//`))
```
*/
@module("sury") external uri: t<string> = "uri"
/** An instance of the JS `URL` class. ReScript has no stdlib binding for it,
    so this is an abstract type standing for one. */
type url
/** A `URL` instance, parsed by the WHATWG URL Standard — `S.string->S.to(S.url)`
    parses one and encodes back through `.href`. Use `S.uri` when a string
    should stay a string. */
@module("sury") external url: t<url> = "url"
/** RFC 3339 full-date — `"1963-06-19"`. Calendar-aware, century leap rule
    included. */
@module("sury") external isoDate: t<string> = "isoDate"
/** RFC 3339 full-time — `"08:30:06Z"`. An offset is required. */
@module("sury") external isoTime: t<string> = "isoTime"
/** RFC 3339 duration — `"P4DT12H30M5S"`. Units nest, so `P1Y2D` and `PT1H2S`
    are invalid; `PT1M` is a minute and `P1M` a month. */
@module("sury") external duration: t<string> = "duration"
/** RFC 1123 hostname — `"www.example.com"`. Syntax only and **not** a security
    boundary: `localhost` and `169.254.169.254` are valid host names. */
@module("sury") external hostname: t<string> = "hostname"
/** Internationalized hostname — `"실례.테스트"`. `S.hostname`'s shape with the
    character repertoire left open; the IDNA2008 rules are not applied. */
@module("sury") external idnHostname: t<string> = "idnHostname"
/** Dotted-quad IPv4 — `"192.168.0.1"`. Rejects the `inet_aton` shorthands
    (`127.1`, `0x7f000001`), but private and link-local ranges are valid
    addresses, so this is no SSRF defense. */
@module("sury") external ipv4: t<string> = "ipv4"
/** IPv6 in any RFC 4291 form, IPv4-mapped included — `"::1"`. A zone id
    (`fe80::a%eth1`) is not part of the format. */
@module("sury") external ipv6: t<string> = "ipv6"
/** URI reference, RFC 3986 — scheme and path both optional, so `"/abc"` passes.
    Usually the one you want for a link field. Very permissive: `""` and
    `"//evil.com"` are references too. */
@module("sury") external uriReference: t<string> = "uriReference"
/** RFC 6570 URI template — a URL *pattern*, not a URL:
    `"http://example.com/dictionary/{term}"`. */
@module("sury") external uriTemplate: t<string> = "uriTemplate"
/** IRI, RFC 3987 — `S.uri` with unescaped non-ASCII allowed:
    `"http://ƒøø.ßår/?∂éœ=πîx"`. */
@module("sury") external iri: t<string> = "iri"
/** IRI reference — `S.uriReference` with unescaped non-ASCII allowed. */
@module("sury") external iriReference: t<string> = "iriReference"
/** Internationalized email, RFC 6531 — `"실례@실례.테스트"`. Much looser than
    `S.email`: `a@localhost` passes. */
@module("sury") external idnEmail: t<string> = "idnEmail"
/** RFC 6901 JSON Pointer — `"/foo/bar~0/baz~1"`, and `""` for the whole
    document. It addresses a location, it doesn't make one safe to follow. */
@module("sury") external jsonPointer: t<string> = "jsonPointer"
/** RFC 6901 relative JSON Pointer — `"2/0/baz"`, a leading integer meaning "go
    up N levels". */
@module("sury") external relativeJsonPointer: t<string> = "relativeJsonPointer"

/**
Matches one exact value — with `===`, except that plain objects and arrays are
compared deeply and `NaN` via `Number.isNaN`.

```rescript
S.literal("Tuna")
S.literal(#apple) // polymorphic variants and variants
S.literal(BigInt.fromInt(2))
S.literal(("help", "lint")) // deep check for tuples, objects, arrays
```
*/
@module("sury") external literal: 'value => t<'value> = "literal"
/** An array of one element type. `S.list` gives a ReScript list, `S.tuple` a
    fixed-length one. */
@module("sury") external array: t<'value> => t<array<'value>> = "array"
/**
Columnar form of an array of rows — one array per field, in field order. Worth
it for bulk payloads, like a `UNNEST`-style INSERT.

```rescript
let schema = S.compactColumns(S.json)->S.to(S.array(rowSchema))
```
*/
@module("sury") external compactColumns: t<'value> => t<array<array<'value>>> = "compactColumns"
/** A JS array decoded into a ReScript list. Prefer `S.array` on a hot path —
    building the list costs an allocation per item. */
@module("sury") external list: t<'value> => t<list<'value>> = "list"
/**
`data instanceof class`, for a type Sury has no schema for. The usual base for
a custom schema: add `S.transform` for the conversion and `S.meta` for a name.

```rescript
S.instance(%raw(`Set`))
```
*/
@module("sury") external instance: unknown => t<unknown> = "instance"
/** `dict<'value>` — every value checked, no key required or forbidden. */
@module("sury") external dict: t<'value> => t<dict<'value>> = "dict"
/** Accepts `undefined` as `None`. `S.Option.getOr` fills the missing case; for
    an object field `s.fieldOr` is shorter. */
@module("sury") external option: t<'value> => t<option<'value>> = "$option"
// The public JS `nullable` called without a default is exactly
// `union([item, literal(null)])` — what ReScript calls `S.null`.
/** Accepts `null`, keeping it as `Null.t`. `S.nullAsOption` is the same schema
    decoded into an `option` instead. */
@module("sury") external null: t<'value> => t<null<'value>> = "nullable"
/** Accepts `null` as `None` — and encodes `None` back to `null`. */
@module("sury") external nullAsOption: t<'value> => t<option<'value>> = "$nullAsOption"
/** Accepts both `null` and `undefined`, keeping which one arrived. */
@module("sury") external nullable: t<'value> => t<nullable<'value>> = "nullish"
/** Accepts both `null` and `undefined` as `None`; `None` encodes to
    `undefined`, so the distinction is lost on the way back. */
@module("sury") external nullableAsOption: t<'value> => t<option<'value>> = "$nullableAsOption"
/**
The first member that fits the value wins, so order is the tie-breaker. This is
the API for variants and polymorphic variants.

```rescript
S.union([S.literal(Circle), S.literal(Square)])
```

A nested union counts as one flat union. `S.anyOf` is the same thing under the
JSON Schema keyword it emits.
*/
@module("sury") external union: array<t<'value>> => t<'value> = "union"
/** `S.union`, under the JSON Schema keyword. */
@module("sury") external anyOf: array<t<'value>> => t<'value> = "anyOf"
/** `S.union` over literals — `S.enum([#red, #green])` is
    `S.union([S.literal(#red), S.literal(#green)])`. */
@module("sury") external enum: array<'value> => t<'value> = "enum"

/**
Documents a schema. It all reaches `S.toJSONSchema`, and `name` is what error
messages call the schema.

```rescript
S.string->S.meta({description: "A useful bit of text", examples: ["hello"]})
S.email->S.meta({errorMessage: {format: "Must be a valid email"}})
```

Metadata is per-schema and not inherited, so attach it where the constraint is.
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
Custom parse and encode logic. Give a `parser` (or `asyncParser`) and a
`serializer`; leave one out and that direction fails.

```rescript
let intToString = schema =>
  schema->S.transform(() => {
    parser: int => int->Int.toString,
    serializer: string =>
      switch string->Int.fromString {
      | Some(int) => int
      | None => JsError.make("Can't convert string to int")->JsError.throw
      },
  })
```

Failing means throwing: it surfaces as `InvalidConversion` with the original as
`cause` and the path prepended. Prefer `S.shape` for a pure restructuring and
`S.to` for a conversion Sury already knows.
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
A check of your own, run on parse and encode alike. Return `false` to fail.

```rescript
S.int->S.refine(i => i > 0, ~error="Expected a positive number")
```

`~path` attaches the failure to a field of the value instead of the value
itself — useful when refining an object. A refinement is opaque to
`S.toJSONSchema`, so prefer a built-in constraint (`S.pattern`, `S.minLength`, …)
when one says the same thing.
*/
let refine = (schema, refiner, ~error=?, ~path=?) => refine(schema, refiner, {?error, ?path})

/**
Restructures a value, deriving the way back from the shape you return.

```rescript
let schema = S.float->S.shape(radius => Circle({radius: radius}))

1->S.parseOrThrow(~to=schema) // Circle({radius: 1.})
Circle({radius: 1.})->S.decodeOrThrow(~from=schema, ~to=S.unknown) // 1
```

The argument is a proxy standing in for the value, not the value — branching on
it won't do what it looks like. Use `S.transform` when you need real logic.
*/
@module("sury") external shape: (t<'value>, 'value => 'shape) => t<'shape> = "shape"

/**
Converts to another type, in both directions.

```rescript
let schema = S.string->S.to(S.float)

"123"->S.parseOrThrow(~to=schema) // 123.
123.->S.decodeOrThrow(~from=schema, ~to=S.unknown) // "123"
```

Any schema is a valid target — `S.json`, `S.jsonString`, `S.date`,
`S.uint8Array`, a union, an object schema — and nested conversions fold into
the same generated function, so a deep pipeline costs no more than a shallow
one. A conversion Sury can't derive, or one with more than one reasonable
meaning, is rejected when the operation is built, with the rewrite in the
message.
*/
@module("sury") external to: (t<'from>, t<'to>) => t<'to> = "to"

/** Swaps a schema's input and output, transformations and all. Unlike encoding
    with `~to=S.unknown`, the reversed schema validates on the way back. */
@module("sury") external reverse: t<'value> => t<unknown> = "reverse"

/**
Compiles a validating parse function. Compile once, call many times — this is
the fast path for repeated parsing.

```rescript
let parseUser = S.parser(~to=userSchema)
parseUser(data)
```
*/
@module("sury") external parser: (~to: t<'value>) => 'any => 'value = "parser"
/** `S.parser` for a schema carrying an async transform. */
@module("sury") external asyncParser: (~to: t<'value>) => 'any => promise<'value> = "asyncParser"
// The public JS `decoder` compiles from a schema's Input space; the ReScript
// flavor decodes FROM a schema's Output space, so reverse `from` first.
@module("sury") external decoder: (t<unknown>, t<'to>) => 'from => 'to = "decoder"
@module("sury")
external asyncDecoder: (t<unknown>, t<'to>) => 'from => promise<'to> = "asyncDecoder"
/**
Compiles a conversion between two schemas, skipping the type validation
`S.parser` performs — refinements and transforms still run.

```rescript
let decodeJson = S.decoder(~from=S.json, ~to=userSchema)
let encodeUser = S.decoder(~from=userSchema, ~to=S.unknown)
let toJsonString = S.decoder(~from=userSchema, ~to=S.jsonString)
```

The whole chain fuses into one generated function.
*/
let decoder = (~from: t<'from>, ~to) => decoder(reverse(from), to)
/** `S.decoder` for a schema carrying an async transform. */
let asyncDecoder = (~from: t<'from>, ~to) => asyncDecoder(reverse(from), to)
// Single-schema (Input -> Output) flavors — the public JS `decoder` /
// `asyncDecoder` called with one argument.
/**
Compiles the schema's own input -> output conversion, for a schema whose
transforms are internal.

```rescript
let decode = S.decoder1(S.array(S.nullAsOption(S.string)))
decode(%raw(`["foo", null]`)) // [Some("foo"), None]
```
*/
@module("sury") external decoder1: t<'value> => unknown => 'value = "decoder"
/** `S.decoder1` for a schema carrying an async transform. */
@module("sury") external asyncDecoder1: t<'value> => unknown => promise<'value> = "asyncDecoder"

/** Validates and parses one value. Compiling once with `S.parser` is faster
    when the same schema runs repeatedly. */
let parseOrThrow = (any, ~to) => parser(~to)(any)
/** `S.parseOrThrow` for a schema carrying an async transform. */
let parseAsyncOrThrow = (any, ~to) => asyncParser(~to)(any)
/** Validates without building an output — 2-3x faster than `S.parseOrThrow`
    when the verdict is all you need. */
@module("sury") external assertOrThrow: ('any, ~to: t<'value>) => unit = "assert"
/** `S.assertOrThrow` for a schema carrying an async transform. */
@module("sury")
external assertAsyncOrThrow: ('any, ~to: t<'value>) => promise<unit> = "$assertAsyncOrThrow"
/** Converts one value between two schemas. `~to=S.unknown` is how you encode:
    `value->S.decodeOrThrow(~from=schema, ~to=S.unknown)`. */
let decodeOrThrow = (any, ~from, ~to) => decoder(~from, ~to)(any)
/** `S.decodeOrThrow` for a schema carrying an async transform. */
let decodeAsyncOrThrow = (any, ~from, ~to) => asyncDecoder(~from, ~to)(any)

/** Whether the schema has an async transform — that is, whether it needs the
    `Async` flavor of an operation. */
@module("sury") external isAsync: t<'value> => bool = "isAsync"

/**
A schema that refers to itself.

```rescript
let nodeSchema = S.recursive("Node", nodeSchema =>
  S.object(s => {
    id: s.field("Id", S.string),
    children: s.field("Children", S.array(nodeSchema)),
  })
)
```

The name identifies it in errors and in the emitted `$defs`. Cyclical *data*
still loops forever.
*/
@module("sury") external recursive: (string, t<'value> => t<'value>) => t<'value> = "recursive"

/** Drops this schema's own type check — its fields and items are still checked.
    Worth it for data you built yourself and are only reshaping. */
@module("sury") external noValidation: (t<'value>, bool) => t<'value> = "noValidation"

/** The schema's input type as the expression error messages use — or its
    `S.meta` name, when it has one. Handy for naming a custom schema after its
    argument. The exact format may change between releases. */
@module("sury") external inputExpression: t<'value> => string = "inputExpression"

/** `S.inputExpression` for the output side. */
@module("sury") external outputExpression: t<'value> => string = "outputExpression"

module Schema = {
  type s = {@as("m") matches: 'value. t<'value> => 'value}
}
/**
Builds a schema from a type's runtime representation, so a variant or a record
literal describes itself.

```rescript
let textSchema = S.schema(s => Text(s.matches(S.string)))
// same as S.string->S.shape(string => Text(string))

let tupleSchema = S.schema(s => (#id, s.matches(S.string)))
```

It follows the runtime shape, `@as` field names included. Reach for `S.object`
or `S.tuple` when the external shape differs from the ReScript one.
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
An object schema, mapping external field names onto your own type at no runtime
cost — and giving you the encode direction with it.

```rescript
let pointSchema = S.object(s => {
  x: s.field("X", S.int),
  y: s.fieldOr("Y", S.int, 0),
})
```

`s.tag(name, value)` pins a constant field (a discriminant), `s.nested(name)`
gives the same context one level down, and `s.flatten(schema)` inlines another
object schema's fields here.

Undeclared keys are dropped; `S.strict` rejects them instead.
*/
@module("sury") external object: (Object.s => 'value) => t<'value> = "object"

/** Back to the default: undeclared keys are dropped. Undoes `S.strict`. */
@module("sury") external strip: t<'value> => t<'value> = "strip"
/** `S.strip` applied to nested object schemas too. */
@module("sury") external deepStrip: t<'value> => t<'value> = "deepStrip"
/** Fails on a key the schema doesn't declare. This object only — `S.deepStrict`
    covers the nested ones, `S.global` the whole app. */
@module("sury") external strict: t<'value> => t<'value> = "strict"
/** `S.strict` applied to nested object schemas too. */
@module("sury") external deepStrict: t<'value> => t<'value> = "deepStrict"

module Tuple = {
  type s = {
    item: 'value. (int, t<'value>) => 'value,
    tag: 'value. (int, 'value) => unit,
  }
}

/**
A fixed-length array, mapped onto your own type.

```rescript
let pointSchema = S.tuple(s => {
  s.tag(0, "point")
  {x: s.item(1, S.int), y: s.item(2, S.int)}
})
```

`S.tuple1`-`S.tuple3` are the shorthands when no reshaping is needed.
*/
@module("sury") external tuple: (Tuple.s => 'value) => t<'value> = "tuple"
/** A one-element tuple — `S.tuple1(S.string)`. */
let tuple1 = v0 => tuple(s => s.item(0, v0))
@module("sury") external tuple2: array<t<unknown>> => t<'value> = "schema"
/** A two-element tuple — `S.tuple2(S.string, S.int)`. */
let tuple2 = (v1, v2) => tuple2([castToUnknown(v1), castToUnknown(v2)])
@module("sury") external tuple3: array<t<unknown>> => t<'value> = "schema"
/** A three-element tuple — `S.tuple3(S.string, S.int, S.bool)`. */
let tuple3 = (v1, v2, v3) => tuple3([castToUnknown(v1), castToUnknown(v2), castToUnknown(v3)])

module Option = {
  /** Replaces the missing case, dropping the `option` from the type.

      ```rescript
      S.option(S.string)->S.Option.getOr("Hello World!")
      ```

      For an object field, `s.fieldOr` says the same thing in one call. */
  @module("sury")
  external getOr: (t<option<'value>>, 'value) => t<'value> = "$Option_getOr"
  /** `S.Option.getOr` with the default built per value — for an allocation, or
      anything that shouldn't be shared between parses. */
  @module("sury")
  external getOrWith: (t<option<'value>>, unit => 'value) => t<'value> = "$Option_getOrWith"
}

/** Arbitrary typed data attached to a schema, keyed by an id you own. Nothing
    reads it but you — unlike `S.meta`, it isn't part of the schema's
    documentation — so it's the place for information a library of yours needs
    to carry alongside a schema. */
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
Exclusive lower bound, on any numeric schema — `S.int`, `S.integer`, `S.float`,
`S.port`, `S.bigint`.

```rescript
S.float->S.gt(0.)
S.float->S.lte(5., ~message="this👏is👏too👏big")
```

Bounds are compared against each other and against the format's own range, so a
contradiction (`S.int->S.gte(3000000000)`) throws where it's written instead of
building a schema nothing satisfies.
*/
@module("sury") external gt: (t<'value>, 'value, ~message: string=?) => t<'value> = "gt"
/** Inclusive lower bound. See `S.gt`. */
@module("sury") external gte: (t<'value>, 'value, ~message: string=?) => t<'value> = "gte"
/** Exclusive upper bound. See `S.gt`. */
@module("sury") external lt: (t<'value>, 'value, ~message: string=?) => t<'value> = "lt"
/** Inclusive upper bound. See `S.gt`. */
@module("sury") external lte: (t<'value>, 'value, ~message: string=?) => t<'value> = "lte"
/** Divisible by the value. See `S.gt` for the schemas it applies to. */
@module("sury")
external multipleOf: (t<'value>, 'value, ~message: string=?) => t<'value> = "multipleOf"

/** Lower bound on a string's or array's length. For a `Blob`, a `File` or a
    `Set`, bound `.size` instead — `S.minSize`. */
@module("sury") external minLength: (t<'value>, int, ~message: string=?) => t<'value> = "minLength"
/** Upper bound on a string's or array's length. */
@module("sury") external maxLength: (t<'value>, int, ~message: string=?) => t<'value> = "maxLength"
/** Exact length — `S.string->S.length(5, ~message="SMS code should be 5 digits long")`. */
@module("sury") external length: (t<'value>, int, ~message: string=?) => t<'value> = "length"
/** `length >= 1`. */
@module("sury") external nonEmpty: (t<'value>, ~message: string=?) => t<'value> = "nonEmpty"

/** Lower bound on `.size` — bytes for `S.blob` and `S.file`, entries for
    something like `S.instance(%raw("Set"))`. A bound of `0` is dropped, a
    negative one is an error. */
@module("sury") external minSize: (t<'value>, int, ~message: string=?) => t<'value> = "minSize"
/** Upper bound on `.size`. See `S.minSize`. */
@module("sury") external maxSize: (t<'value>, int, ~message: string=?) => t<'value> = "maxSize"
/** Exact `.size`. See `S.minSize`. */
@module("sury") external size: (t<'value>, int, ~message: string=?) => t<'value> = "size"

/** The string must match the regex, which rides along into the emitted JSON
    Schema — so narrowing a format stays honest about what it accepts:
    `S.uri->S.pattern(%re("/^https:\/\//"))`. */
@module("sury")
external pattern: (t<string>, RegExp.t, ~message: string=?) => t<string> = "pattern"
/** Trims surrounding whitespace — a transformation, not a check, applied in both
    directions. */
@module("sury") external trim: t<string> => t<string> = "trim"

type toJSONSchemaOptions = {target?: StandardSchema.JsonSchema.target}
/** Emits `draft-07` by default, or the dialect `~options` names. Keywords and
    examples describe the **input** side; pass `schema->S.reverse` for the
    output side. */
@module("sury")
external toJSONSchema: (t<'value>, ~options: toJSONSchemaOptions=?) => JSONSchema.t = "toJSONSchema"
@module("sury")
external fromJSONSchemaDefinition: JSONSchema.definition => t<JSON.t> = "fromJSONSchema"
/** Builds a schema from a JSON Schema document at runtime, typed `S.t<JSON.t>`
    — follow it with `S.to` for a ReScript type. A `$ref` into the same document
    is followed (recursive ones included); one leading outside it throws, so
    bundle first. */
let fromJSONSchema = jsonSchema => fromJSONSchemaDefinition(JSONSchema.Schema(jsonSchema))
/** Merges extra keywords into what `S.toJSONSchema` emits for this schema —
    vendor extensions, a `$id`, a hand-written description. Validation is
    untouched, so keep the two in agreement yourself. */
@module("sury")
external extendJSONSchema: (t<'value>, JSONSchema.t) => t<'value> = "extendJSONSchema"
/** Enables `~standard.jsonSchema`; its input/output throw before this is called.
    Opt-in so `S.toJSONSchema` stays tree-shakeable for everyone else. */
@module("sury") external enableStandardJSONSchema: unit => unit = "enableStandardJSONSchema"

type globalConfigOverride = {
  /** `Strict` makes every object schema reject undeclared keys. Default `Strip`. */
  defaultAdditionalItems?: additionalItemsMode,
  /** Skips the `NaN` check in every number schema — ~10% faster when your
      numbers can't be `NaN`. */
  disableNanNumberValidation?: bool,
}

/** Sets library-wide defaults. Call it once at startup, before the schemas that
    should follow them are built. */
@module("sury") external global: globalConfigOverride => unit = "global"
