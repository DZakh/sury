@@uncurried
@@warning("-30")

type never

module Path = {
  // Standard Schema's `PropertyKey` minus `symbol`, which Sury never emits.
  @unboxed
  type propertyKey = String(string) | Number(float)
  type t = array<propertyKey>

  let empty: t = []
  external fromArray: array<string> => t = "%identity"
  @module("sury") external toText: t => string = "pathToText"
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
  | @as("base64") Base64
  | @as("base64url") Base64url
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

external castToUnknown: t<'any> => t<unknown> = "%identity"
external castToAny: t<'value> => t<'any> = "%identity"
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

module Error = {
  type class

  @module("sury") external class: class = "Error"

  @module("sury") @new external make: errorDetails => error = "Error"

  external classify: error => errorDetails = "%identity"

  external throw: error => 'a = "%raise"
}

// Primitive schema values — the very instances the JS entry exports, so both
// surfaces share one object per primitive. Some (string, bool, ...) shadow
// stdlib names on purpose.
@module("sury") external never: t<never> = "never"
@module("sury") external unknown: t<unknown> = "unknown"
@module("sury") external any: t<'any> = "any"
@module("sury") external unit: t<unit> = "$unit"
@module("sury") external nullAsUnit: t<unit> = "$nullAsUnit"
@module("sury") external string: t<string> = "string"
@module("sury") external bool: t<bool> = "bool"
@module("sury") external int: t<int> = "int"
// `t<float>`, not `t<int>`: ReScript's `int` is int32, and a JS integer
// (JSON Schema's unbounded `integer`) can exceed that range.
@module("sury") external integer: t<float> = "integer"
@module("sury") external float: t<float> = "float"
@module("sury") external bigint: t<bigint> = "bigint"
@module("sury") external symbol: t<Symbol.t> = "symbol"
@module("sury") external nan: t<float> = "nan"
@module("sury") external date: t<Date.t> = "date"
@module("sury") external json: t<JSON.t> = "json"
@module("sury") external jsonString: t<string> = "jsonString"
@module("sury") external jsonStringWithSpace: int => t<string> = "jsonStringWithSpace"
@module("sury") external uint8Array: t<Uint8Array.t> = "uint8Array"
// `Js.Blob.t`/`Js.File.t` rather than a pair of abstract types declared here:
// the stdlib has no Blob or File module, and these two are the compiler's own
// builtin abstract types — the ones untagged variants match on — so a value
// from any other binding unifies with these.
@module("sury") external blob: t<Js.Blob.t> = "blob"
@module("sury") external file: t<Js.File.t> = "file"
@module("sury") external isoDateTime: t<string> = "isoDateTime"
@module("sury") external port: t<int> = "port"
@module("sury") external email: t<string> = "email"
@module("sury") external uuid: t<string> = "uuid"
@module("sury") external cuid: t<string> = "cuid"
@module("sury") external base64: t<string> = "base64"
@module("sury") external base64url: t<string> = "base64url"
@module("sury") external uri: t<string> = "uri"
/** An instance of the JS `URL` class. ReScript has no stdlib binding for it,
    so this is an abstract type standing for one. */
type url
@module("sury") external url: t<url> = "url"
@module("sury") external isoDate: t<string> = "isoDate"
@module("sury") external isoTime: t<string> = "isoTime"
@module("sury") external duration: t<string> = "duration"
@module("sury") external hostname: t<string> = "hostname"
@module("sury") external idnHostname: t<string> = "idnHostname"
@module("sury") external ipv4: t<string> = "ipv4"
@module("sury") external ipv6: t<string> = "ipv6"
@module("sury") external uriReference: t<string> = "uriReference"
@module("sury") external uriTemplate: t<string> = "uriTemplate"
@module("sury") external iri: t<string> = "iri"
@module("sury") external iriReference: t<string> = "iriReference"
@module("sury") external idnEmail: t<string> = "idnEmail"
@module("sury") external jsonPointer: t<string> = "jsonPointer"
@module("sury") external relativeJsonPointer: t<string> = "relativeJsonPointer"

@module("sury") external literal: 'value => t<'value> = "literal"
@module("sury") external array: t<'value> => t<array<'value>> = "array"
@module("sury") external compactColumns: t<'value> => t<array<array<'value>>> = "compactColumns"
@module("sury") external list: t<'value> => t<list<'value>> = "list"
@module("sury") external instance: unknown => t<unknown> = "instance"
@module("sury") external dict: t<'value> => t<dict<'value>> = "dict"
@module("sury") external option: t<'value> => t<option<'value>> = "$option"
// The public JS `nullable` called without a default is exactly
// `union([item, literal(null)])` — what ReScript calls `S.null`.
@module("sury") external null: t<'value> => t<null<'value>> = "nullable"
@module("sury") external nullAsOption: t<'value> => t<option<'value>> = "$nullAsOption"
@module("sury") external nullable: t<'value> => t<nullable<'value>> = "nullish"
@module("sury") external nullableAsOption: t<'value> => t<option<'value>> = "$nullableAsOption"
@module("sury") external union: array<t<'value>> => t<'value> = "union"
@module("sury") external anyOf: array<t<'value>> => t<'value> = "anyOf"
@module("sury") external enum: array<'value> => t<'value> = "enum"

@module("sury") external meta: (t<'value>, meta<'value>) => t<'value> = "meta"

// The public JS `refine` takes an options object; build it here from the
// ReScript labeled args.
type refineOptions = {error?: string, path?: array<string>}
@module("sury")
external refine: (t<'value>, 'value => bool, refineOptions) => t<'value> = "refine"
let refine = (schema, refiner, ~error=?, ~path=?) => refine(schema, refiner, {?error, ?path})

@module("sury") external shape: (t<'value>, 'value => 'shape) => t<'shape> = "shape"

type conversion<'i, 'o> =
  | @as("auto") Auto
  | @as("never") Never
  // The two readings of a content link (CONTENT_CODEC_SPEC.md rule 1). They
  // carry no payload, so they erase to their strings the way Auto/Never do —
  // and they have to be here, because the ambiguity this axis reports names
  // them as the remedy.
  | @as("pack") Pack
  | @as("unpack") Unpack
  | Sync('i => 'o)
  | Async('i => promise<'o>)

type codecs<'from, 'to> = {
  decode: conversion<'from, 'to>,
  encode: conversion<'to, 'from>,
}

@module("sury") external to: (t<'from>, t<'to>) => t<'to> = "to"
%%private(
  @module("sury")
  external toCustom: (
    t<'from>,
    t<'to>,
    {"decodeToOutput": conversion<'from, 'to>, "encodeFromOutput": conversion<'to, 'from>},
  ) => t<'to> = "to"
)
// Auto/Never already erase to the exact "auto"/"never" strings via @as, while
// Sync/Async keep the default variant representation the JS side doesn't
// understand, so each slot unwraps to the JS `f` / `{async: f}` forms.
// The slots are the toOutput ones: `t<'to>` exposes only the target's output
// type, so the coder can't be typed against the target's input the way the JS
// `{decode, encode}` surface is. Nothing is lost, because the compiler
// already checks the coder's signature.
%%private(
  let unwrapConversion = (conversion: conversion<'i, 'o>): conversion<'i, 'o> =>
    switch conversion {
    | Sync(fn) => fn->Obj.magic
    | Async(fn) => {"async": fn}->Obj.magic
    | erased => erased
    }
)
let to = (from, target, ~custom=?) =>
  switch custom {
  | None => to(from, target)
  | Some({decode, encode}) =>
    toCustom(
      from,
      target,
      {
        "decodeToOutput": unwrapConversion(decode),
        "encodeFromOutput": unwrapConversion(encode),
      },
    )
  }

@module("sury") external noValidation: (t<'value>, bool) => t<'value> = "noValidation"

@module("sury") external reverse: t<'value> => t<unknown> = "reverse"

%%private(
  // The public JS `decoder` compiles from a schema's Input space; the ReScript
  // flavor converts FROM a schema's Output space, so `from` is reversed first.
  // Arity-specific bindings, since a labeled optional `~via` can't spread.
  @module("sury") external decoder2: (t<unknown>, t<'to>) => 'any => 'to = "decoder"
  @module("sury")
  external decoder3: (t<unknown>, t<unknown>, t<'to>) => 'any => 'to = "decoder"
  @module("sury")
  external asyncDecoder2: (t<unknown>, t<'to>) => 'any => promise<'to> = "asyncDecoder"
  @module("sury")
  external asyncDecoder3: (t<unknown>, t<unknown>, t<'to>) => 'any => promise<'to> =
    "asyncDecoder"

  // Only a Sury failure becomes `Error`; anything else propagates, the way
  // the JS `safe` rethrows what isn't a Sury error.
  let safe = (fn): result<'value, error> =>
    try Ok(fn()) catch {
    | Exn(error) => Error(error)
    }
  // A compiled async operation throws synchronously when the input fails
  // its type check before anything is awaited, so the call sits inside the
  // `try` as well.
  let safeAsync = (fn: unit => promise<'value>): promise<result<'value, error>> =>
    try {
      fn()
      ->Promise.then(value => Promise.resolve(Ok(value)))
      ->Promise.catch(exn =>
        switch exn {
        | Exn(error) => Promise.resolve(Error(error))
        | _ => throw(exn)
        }
      )
    } catch {
    | Exn(error) => Promise.resolve(Error(error))
    }
)

@module("sury") external compileParseOrThrow: (~to: t<'value>) => 'any => 'value = "parser"
@module("sury")
external compileParseAsyncOrThrow: (~to: t<'value>) => 'any => promise<'value> = "asyncParser"
let compileParse = (~to) => {
  let fn = compileParseOrThrow(~to)
  any => safe(() => fn(any))
}
let compileParseAsync = (~to) => {
  let fn = compileParseAsyncOrThrow(~to)
  any => safeAsync(() => fn(any))
}

let compileConvertOrThrow = (~from: t<'from>, ~via=?, ~to) =>
  switch via {
  | None => decoder2(reverse(from), to)
  | Some(via) => decoder3(reverse(from), castToUnknown(via), to)
  }
let compileConvertAsyncOrThrow = (~from: t<'from>, ~via=?, ~to) =>
  switch via {
  | None => asyncDecoder2(reverse(from), to)
  | Some(via) => asyncDecoder3(reverse(from), castToUnknown(via), to)
  }
let compileConvert = (~from, ~via=?, ~to) => {
  let fn = compileConvertOrThrow(~from, ~via?, ~to)
  any => safe(() => fn(any))
}
let compileConvertAsync = (~from, ~via=?, ~to) => {
  let fn = compileConvertAsyncOrThrow(~from, ~via?, ~to)
  any => safeAsync(() => fn(any))
}

// The compiled assert with a boolean answer. `assert` is a ReScript keyword,
// so the non-throwing assert is spelled `validate`.
@module("sury") external compileValidate: (~to: t<'value>) => 'any => bool = "inputValidator"

// `t<'value>` names the output type, so the output-side constructor is THE
// constructor here; the input side has no type to hand back.
@module("sury")
external compileMakeOrThrow: (~schema: t<'value>) => 'value => 'value = "outputConstructor"
@module("sury")
external compileMakeAsyncOrThrow: (~schema: t<'value>) => 'value => promise<'value> =
  "asyncOutputConstructor"
let compileMake = (~schema) => {
  let fn = compileMakeOrThrow(~schema)
  value => safe(() => fn(value))
}
let compileMakeAsync = (~schema) => {
  let fn = compileMakeAsyncOrThrow(~schema)
  value => safeAsync(() => fn(value))
}

let parseOrThrow = (any, ~to) => compileParseOrThrow(~to)(any)
let parseAsyncOrThrow = (any, ~to) => compileParseAsyncOrThrow(~to)(any)
let parse = (any, ~to) => safe(() => parseOrThrow(any, ~to))
let parseAsync = (any, ~to) => safeAsync(() => parseAsyncOrThrow(any, ~to))
@module("sury") external assertOrThrow: ('any, ~to: t<'value>) => unit = "assertInput"
@module("sury")
external assertAsyncOrThrow: ('any, ~to: t<'value>) => promise<unit> = "asyncAssertInput"
let validate = (any, ~to) => compileValidate(~to)(any)
let convertOrThrow = (any, ~from, ~via=?, ~to) => compileConvertOrThrow(~from, ~via?, ~to)(any)
let convertAsyncOrThrow = (any, ~from, ~via=?, ~to) =>
  compileConvertAsyncOrThrow(~from, ~via?, ~to)(any)
let convert = (any, ~from, ~via=?, ~to) => safe(() => convertOrThrow(any, ~from, ~via?, ~to))
let convertAsync = (any, ~from, ~via=?, ~to) =>
  safeAsync(() => convertAsyncOrThrow(any, ~from, ~via?, ~to))
let makeOrThrow = (value, ~schema) => compileMakeOrThrow(~schema)(value)
let makeAsyncOrThrow = (value, ~schema) => compileMakeAsyncOrThrow(~schema)(value)
let make = (value, ~schema) => safe(() => makeOrThrow(value, ~schema))
let makeAsync = (value, ~schema) => safeAsync(() => makeAsyncOrThrow(value, ~schema))

@module("sury") external recursive: (string, t<'value> => t<'value>) => t<'value> = "recursive"


@module("sury") external inputExpression: t<'value> => string = "inputExpression"

@module("sury") external outputExpression: t<'value> => string = "outputExpression"

module Schema = {
  type s = {@as("m") matches: 'value. t<'value> => 'value}
}
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

@module("sury") external object: (Object.s => 'value) => t<'value> = "object"

@module("sury") external strip: t<'value> => t<'value> = "strip"
@module("sury") external deepStrip: t<'value> => t<'value> = "deepStrip"
@module("sury") external strict: t<'value> => t<'value> = "strict"
@module("sury") external deepStrict: t<'value> => t<'value> = "deepStrict"

module Tuple = {
  type s = {
    item: 'value. (int, t<'value>) => 'value,
    tag: 'value. (int, 'value) => unit,
  }
}

@module("sury") external tuple: (Tuple.s => 'value) => t<'value> = "tuple"
let tuple1 = v0 => tuple(s => s.item(0, v0))
@module("sury") external tuple2: array<t<unknown>> => t<'value> = "schema"
let tuple2 = (v1, v2) => tuple2([castToUnknown(v1), castToUnknown(v2)])
@module("sury") external tuple3: array<t<unknown>> => t<'value> = "schema"
let tuple3 = (v1, v2, v3) => tuple3([castToUnknown(v1), castToUnknown(v2), castToUnknown(v3)])

module Option = {
  @module("sury")
  external getOr: (t<option<'value>>, 'value) => t<'value> = "$Option_getOr"
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
@module("sury") external gt: (t<'value>, 'value, ~message: string=?) => t<'value> = "gt"
@module("sury") external gte: (t<'value>, 'value, ~message: string=?) => t<'value> = "gte"
@module("sury") external lt: (t<'value>, 'value, ~message: string=?) => t<'value> = "lt"
@module("sury") external lte: (t<'value>, 'value, ~message: string=?) => t<'value> = "lte"
@module("sury")
external multipleOf: (t<'value>, 'value, ~message: string=?) => t<'value> = "multipleOf"

@module("sury") external minLength: (t<'value>, int, ~message: string=?) => t<'value> = "minLength"
@module("sury") external maxLength: (t<'value>, int, ~message: string=?) => t<'value> = "maxLength"
@module("sury") external length: (t<'value>, int, ~message: string=?) => t<'value> = "length"
@module("sury") external nonEmpty: (t<'value>, ~message: string=?) => t<'value> = "nonEmpty"

@module("sury") external minSize: (t<'value>, int, ~message: string=?) => t<'value> = "minSize"
@module("sury") external maxSize: (t<'value>, int, ~message: string=?) => t<'value> = "maxSize"
@module("sury") external size: (t<'value>, int, ~message: string=?) => t<'value> = "size"

@module("sury")
external pattern: (t<string>, RegExp.t, ~message: string=?) => t<string> = "pattern"
@module("sury") external trim: t<string> => t<string> = "trim"

type jsonSchemaOptions = {target?: StandardSchema.JsonSchema.target}
@module("sury")
external inputJSONSchema: (t<'value>, ~options: jsonSchemaOptions=?) => JSONSchema.t =
  "inputJSONSchema"
@module("sury")
external outputJSONSchema: (t<'value>, ~options: jsonSchemaOptions=?) => JSONSchema.t =
  "outputJSONSchema"
@module("sury")
external fromJSONSchemaDefinition: JSONSchema.definition => t<JSON.t> = "fromJSONSchema"
let fromJSONSchema = jsonSchema => fromJSONSchemaDefinition(JSONSchema.Schema(jsonSchema))
@module("sury")
external extendJSONSchema: (t<'value>, JSONSchema.t) => t<'value> = "extendJSONSchema"
// Enables `~standard.jsonSchema`; its input/output throw before this is called.
@module("sury") external enableStandardJSONSchema: unit => unit = "enableStandardJSONSchema"

type globalConfigOverride = {
  defaultAdditionalItems?: additionalItemsMode,
  disableNanNumberValidation?: bool,
}

@module("sury") external global: globalConfigOverride => unit = "global"
