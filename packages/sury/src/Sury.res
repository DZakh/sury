@@uncurried
@@warning("-30")

type never

module Path = {
  type t = string

  external toString: t => string = "%identity"

  @inline
  let empty = ""

  @inline
  let dynamic = "[]"

  @module("sury/core") external toArray: t => array<string> = "pathToArray"
  @module("sury/core") external fromArray: array<string> => t = "pathFromArray"
  @module("sury/core") external fromLocation: string => t = "pathFromLocation"
  @module("sury/core") external concat: (t, t) => t = "pathConcat"
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
  | @as("union") Union
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
  | @as("union")
  Union({
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
and flag = int
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

@tag("success")
type jsResult<'value>

type exn += private Exn(error)

// =============================================================================
// Bindings to the TypeScript core
// =============================================================================
//
// Sury's implementation lives in src/core.ts (see its header for the port
// story). This module is the ReScript face of it: the public types above,
// plus `@module("sury/core") external` bindings below. "sury/core" is a
// conditional package export (import -> core.mjs, require -> core.cjs), so
// these bindings work for consumers compiling to either module format —
// a plain relative `@module("./core.mjs")` would break under a "commonjs"
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
@module("sury/core") external __setExnId: unknown => unit = "__setExnId"
let () = __setExnId(%raw(`Exn`))

module Flag = {
  let none: flag = 0
  let async: flag = 1

  external with: (flag, flag) => flag = "%orint"
}

type s<'value> = {fail: 'a. (string, ~path: Path.t=?) => 'a}

module Error = {
  type class

  @module("sury/core") external class: class = "errorClass"

  @module("sury/core") @new external make: errorDetails => error = "errorClass"

  external classify: error => errorDetails = "%identity"
}

// Primitive factories. Some (string, bool, ...) shadow stdlib names on
// purpose, exactly like the implementation did.
@module("sury/core") external nullAsUnit: unit => t<unit> = "nullAsUnit"
@module("sury/core") external never_: unit => t<never> = "never_"
@module("sury/core") external unknown: t<unknown> = "unknown"
@module("sury/core") external unit: unit => t<unit> = "unit"
@module("sury/core") external nullLiteral: unit => t<unit> = "nullLiteral"
@module("sury/core") external nan: unit => t<float> = "nan"
@module("sury/core") external string: unit => t<string> = "string"
@module("sury/core") external bool: unit => t<bool> = "bool"
@module("sury/core") external int: unit => t<int> = "int"
@module("sury/core") external float: unit => t<float> = "float"
@module("sury/core") external bigint: unit => t<bigint> = "bigint"
@module("sury/core") external symbol: unit => t<Symbol.t> = "symbol"
@module("sury/core") external date: unit => t<Date.t> = "date"
@module("sury/core") external json: unit => t<JSON.t> = "json"
@module("sury/core") external jsonString: unit => t<string> = "jsonString"
@module("sury/core") external jsonStringWithSpace: int => t<string> = "jsonStringWithSpace"
@module("sury/core") external uint8Array: unit => t<Uint8Array.t> = "uint8Array"
@module("sury/core") external isoDateTime: unit => t<string> = "isoDateTime"
@module("sury/core") external port: unit => t<int> = "port"
@module("sury/core") external email: unit => t<string> = "email"
@module("sury/core") external uuid: unit => t<string> = "uuid"
@module("sury/core") external cuid: unit => t<string> = "cuid"
@module("sury/core") external url: unit => t<string> = "url"

@module("sury/core") external literal: 'value => t<'value> = "literal"
@module("sury/core") external array: t<'value> => t<array<'value>> = "array"
@module("sury/core") external compactColumns: t<'value> => t<array<array<'value>>> = "compactColumns"
@module("sury/core") external list: t<'value> => t<list<'value>> = "list"
@module("sury/core") external instance: unknown => t<unknown> = "instance"
@module("sury/core") external dict: t<'value> => t<dict<'value>> = "dict"
@module("sury/core") external option: t<'value> => t<option<'value>> = "option"
@module("sury/core") external null: t<'value> => t<null<'value>> = "null_"
@module("sury/core") external nullAsOption: t<'value> => t<option<'value>> = "nullAsOption"
@module("sury/core") external nullable: t<'value> => t<nullable<'value>> = "nullable"
@module("sury/core") external nullableAsOption: t<'value> => t<option<'value>> = "nullableAsOption"
@module("sury/core") external union: array<t<'value>> => t<'value> = "union"
@module("sury/core") external enum: array<'value> => t<'value> = "enum"

@module("sury/core") external meta: (t<'value>, meta<'value>) => t<'value> = "meta"

type transformDefinition<'input, 'output> = {
  @as("p")
  parser?: 'input => 'output,
  @as("a")
  asyncParser?: 'input => promise<'output>,
  @as("s")
  serializer?: 'output => 'input,
}
@module("sury/core")
external transform: (t<'input>, s<'output> => transformDefinition<'input, 'output>) => t<'output> =
  "transform"

@module("sury/core")
external refine: (t<'value>, 'value => bool, ~error: string=?, ~path: array<string>=?) => t<'value> =
  "refine"

@module("sury/core") external shape: (t<'value>, 'value => 'shape) => t<'shape> = "shape"

@module("sury/core") external to: (t<'from>, t<'to>) => t<'to> = "to"

@module("sury/core") external parser: (~to: t<'value>) => 'any => 'value = "parser"
@module("sury/core") external asyncParser: (~to: t<'value>) => 'any => promise<'value> = "asyncParser"
@module("sury/core") external decoder: (~from: t<'from>, ~to: t<'to>) => 'from => 'to = "decoder"
@module("sury/core")
external asyncDecoder: (~from: t<'from>, ~to: t<'to>) => 'from => promise<'to> = "asyncDecoder"
@module("sury/core") external decoder1: t<'value> => unknown => 'value = "decoder1"
@module("sury/core") external asyncDecoder1: t<'value> => unknown => promise<'value> = "asyncDecoder1"

@module("sury/core") external parseOrThrow: ('any, ~to: t<'value>) => 'value = "parseOrThrow"
@module("sury/core")
external parseAsyncOrThrow: ('any, ~to: t<'value>) => promise<'value> = "parseAsyncOrThrow"
@module("sury/core") external assertOrThrow: ('any, ~to: t<'value>) => unit = "assertOrThrow"
@module("sury/core")
external assertAsyncOrThrow: ('any, ~to: t<'value>) => promise<unit> = "assertAsyncOrThrow"
@module("sury/core")
external decodeOrThrow: ('from, ~from: t<'from>, ~to: t<'to>) => 'to = "decodeOrThrow"
@module("sury/core")
external decodeAsyncOrThrow: ('from, ~from: t<'from>, ~to: t<'to>) => promise<'to> =
  "decodeAsyncOrThrow"

@module("sury/core") external isAsync: t<'value> => bool = "isAsync"

@module("sury/core") external recursive: (string, t<'value> => t<'value>) => t<'value> = "recursive"

@module("sury/core") external noValidation: (t<'value>, bool) => t<'value> = "noValidation"

@module("sury/core") external toExpression: t<'value> => string = "toExpression"

module Schema = {
  type s = {@as("m") matches: 'value. t<'value> => 'value}
}
@module("sury/core") external schema: (Schema.s => 'value) => t<'value> = "schema"

module Object = {
  type rec s = {
    @as("f") field: 'value. (string, t<'value>) => 'value,
    fieldOr: 'value. (string, t<'value>, 'value) => 'value,
    tag: 'value. (string, 'value) => unit,
    nested: string => s,
    flatten: 'value. t<'value> => 'value,
  }
}

@module("sury/core") external object: (Object.s => 'value) => t<'value> = "object"

@module("sury/core") external strip: t<'value> => t<'value> = "strip"
@module("sury/core") external deepStrip: t<'value> => t<'value> = "deepStrip"
@module("sury/core") external strict: t<'value> => t<'value> = "strict"
@module("sury/core") external deepStrict: t<'value> => t<'value> = "deepStrict"

module Tuple = {
  type s = {
    item: 'value. (int, t<'value>) => 'value,
    tag: 'value. (int, 'value) => unit,
  }
}

@module("sury/core") external tuple: (Tuple.s => 'value) => t<'value> = "tuple"
@module("sury/core") external tuple1: t<'value> => t<'value> = "tuple1"
@module("sury/core") external tuple2: (t<'v1>, t<'v2>) => t<('v1, 'v2)> = "tuple2"
@module("sury/core") external tuple3: (t<'v1>, t<'v2>, t<'v3>) => t<('v1, 'v2, 'v3)> = "tuple3"

module Option = {
  @module("sury/core")
  external getOr: (t<option<'value>>, 'value) => t<'value> = "Option_getOr"
  @module("sury/core")
  external getOrWith: (t<option<'value>>, unit => 'value) => t<'value> = "Option_getOrWith"
}

module Metadata = {
  module Id = {
    type t<'metadata>
    @module("sury/core")
    external make: (~namespace: string, ~name: string) => t<'metadata> = "Metadata_Id_make"
  }

  @module("sury/core")
  external get: (t<'value>, ~id: Id.t<'metadata>) => option<'metadata> = "Metadata_get"

  @module("sury/core")
  external set: (t<'value>, ~id: Id.t<'metadata>, 'metadata) => t<'value> = "Metadata_set"
}

@module("sury/core") external reverse: t<'value> => t<unknown> = "reverse"

// =============
// Built-in refinements
// =============

@module("sury/core") external min: (t<'value>, int, ~message: string=?) => t<'value> = "min"
@module("sury/core") external floatMin: (t<float>, float, ~message: string=?) => t<float> = "floatMin"

@module("sury/core") external max: (t<'value>, int, ~message: string=?) => t<'value> = "max"
@module("sury/core") external floatMax: (t<float>, float, ~message: string=?) => t<float> = "floatMax"

@module("sury/core") external length: (t<'value>, int, ~message: string=?) => t<'value> = "length"

@module("sury/core")
external pattern: (t<string>, RegExp.t, ~message: string=?) => t<string> = "pattern"
@module("sury/core") external trim: t<string> => t<string> = "trim"

type toJSONSchemaOptions = {target?: StandardSchema.JsonSchema.target}
@module("sury/core")
external toJSONSchema: (t<'value>, ~options: toJSONSchemaOptions=?) => JSONSchema.t = "toJSONSchema"
@module("sury/core") external fromJSONSchema: JSONSchema.t => t<JSON.t> = "fromJSONSchema"
@module("sury/core")
external extendJSONSchema: (t<'value>, JSONSchema.t) => t<'value> = "extendJSONSchema"
// Enables `~standard.jsonSchema`; its input/output throw before this is called.
@module("sury/core") external enableStandardJSONSchema: unit => unit = "enableStandardJSONSchema"

type globalConfigOverride = {
  defaultAdditionalItems?: additionalItemsMode,
  disableNanNumberValidation?: bool,
}

@module("sury/core") external global: globalConfigOverride => unit = "global"

// =============
// JS/TS API
// =============

@module("sury/core") external brand: (t<'value>, string) => t<'value> = "brand"

@module("sury/core") external js_parser: t<unknown> => unknown => unknown = "js_parser"
@module("sury/core") external js_asyncParser: t<unknown> => unknown => unknown = "js_asyncParser"
@module("sury/core") external getDecoder: (~s1: t<unknown>, ~flag: flag=?) => 'from => 'to = "getDecoder"
@module("sury/core") external js_asyncDecoder: t<unknown> => unknown => unknown = "js_asyncDecoder"
@module("sury/core") external js_encoder: t<unknown> => unknown => unknown = "js_encoder"
@module("sury/core") external js_asyncEncoder: t<unknown> => unknown => unknown = "js_asyncEncoder"
@module("sury/core") external js_assert: (unknown, unknown) => unit = "js_assert"
@module("sury/core") external js_is: (unknown, unknown) => bool = "js_is"

@module("sury/core") external js_safe: (unit => 'v) => jsResult<'v> = "js_safe"
@module("sury/core")
external js_safeAsync: (unit => promise<'v>) => promise<jsResult<'v>> = "js_safeAsync"

@module("sury/core") external js_union: array<unknown> => t<'value> = "js_union"

@module("sury/core") external js_optional: (t<'v>, option<unknown>) => t<option<'v>> = "js_optional"
@module("sury/core") external js_nullable: (t<'v>, option<unknown>) => t<option<'v>> = "js_nullable"

@module("sury/core")
external js_asyncDecoderAssert: (t<'output>, 'output => promise<unit>) => t<'output> =
  "js_asyncDecoderAssert"
@module("sury/core")
external js_refine: (t<'output>, 'output => bool, option<{..}>) => t<'output> = "js_refine"

@module("sury/core")
external js_to: (
  t<'value>,
  t<'target>,
  ~decoder: 'value => 'target=?,
  ~encoder: 'target => 'value=?,
) => t<'target> = "js_to"

@module("sury/core") external js_schema: unknown => t<unknown> = "js_schema"

@module("sury/core") external js_merge: (t<unknown>, t<unknown>) => t<unknown> = "js_merge"
