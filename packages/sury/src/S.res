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

  @module("sury") external toArray: t => array<string> = "pathToArray"
  @module("sury") external fromArray: array<string> => t = "pathFromArray"
  @module("sury") external fromLocation: string => t = "pathFromLocation"
  @module("sury") external concat: (t, t) => t = "pathConcat"
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

type exn += private Exn(error)

// =============================================================================
// Bindings to the TypeScript core
// =============================================================================
//
// Sury's implementation lives in src/core/*.ts, bundled into the package
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
@module("sury") external __setExnId: unknown => unit = "__setExnId"
let () = __setExnId(%raw(`Exn`))

module Flag = {
  let none: flag = 0
  let async: flag = 1

  external with: (flag, flag) => flag = "%orint"
}

type s<'value> = {fail: 'a. (string, ~path: Path.t=?) => 'a}

module Error = {
  type class

  @module("sury") external class: class = "errorClass"

  @module("sury") @new external make: errorDetails => error = "errorClass"

  external classify: error => errorDetails = "%identity"
}

// Primitive schema values — the same eager, PURE-annotated instances the JS
// entry exports (see src/S.ts), so both surfaces share one object per
// primitive. Some (string, bool, ...) shadow stdlib names on purpose.
@module("sury") external never: t<never> = "never"
@module("sury") external unknown: t<unknown> = "unknown"
@module("sury") external unit: t<unit> = "unit"
@module("sury") external nullAsUnit: t<unit> = "nullAsUnit"
@module("sury") external string: t<string> = "string"
@module("sury") external bool: t<bool> = "bool"
@module("sury") external int: t<int> = "int"
@module("sury") external float: t<float> = "float"
@module("sury") external bigint: t<bigint> = "bigint"
@module("sury") external symbol: t<Symbol.t> = "symbol"
@module("sury") external nan: t<float> = "nan"
@module("sury") external date: t<Date.t> = "date"
@module("sury") external json: t<JSON.t> = "json"
@module("sury") external jsonString: t<string> = "jsonString"
@module("sury") external jsonStringWithSpace: int => t<string> = "jsonStringWithSpace"
@module("sury") external uint8Array: t<Uint8Array.t> = "uint8Array"
@module("sury") external isoDateTime: t<string> = "isoDateTime"
@module("sury") external port: t<int> = "port"
@module("sury") external email: t<string> = "email"
@module("sury") external uuid: t<string> = "uuid"
@module("sury") external cuid: t<string> = "cuid"
@module("sury") external url: t<string> = "url"

@module("sury") external literal: 'value => t<'value> = "literal"
@module("sury") external array: t<'value> => t<array<'value>> = "array"
@module("sury") external compactColumns: t<'value> => t<array<array<'value>>> = "compactColumns"
@module("sury") external list: t<'value> => t<list<'value>> = "list"
@module("sury") external instance: unknown => t<unknown> = "instance"
@module("sury") external dict: t<'value> => t<dict<'value>> = "dict"
@module("sury") external option: t<'value> => t<option<'value>> = "option"
@module("sury") external null: t<'value> => t<null<'value>> = "null_"
@module("sury") external nullAsOption: t<'value> => t<option<'value>> = "nullAsOption"
@module("sury") external nullable: t<'value> => t<nullable<'value>> = "nullish"
@module("sury") external nullableAsOption: t<'value> => t<option<'value>> = "nullableAsOption"
@module("sury") external union: array<t<'value>> => t<'value> = "union"
@module("sury") external enum: array<'value> => t<'value> = "enum"

@module("sury") external meta: (t<'value>, meta<'value>) => t<'value> = "meta"

type transformDefinition<'input, 'output> = {
  @as("p")
  parser?: 'input => 'output,
  @as("a")
  asyncParser?: 'input => promise<'output>,
  @as("s")
  serializer?: 'output => 'input,
}
@module("sury")
external transform: (t<'input>, s<'output> => transformDefinition<'input, 'output>) => t<'output> =
  "transform"

@module("sury")
external refine: (t<'value>, 'value => bool, ~error: string=?, ~path: array<string>=?) => t<'value> =
  "res_refine"

@module("sury") external shape: (t<'value>, 'value => 'shape) => t<'shape> = "shape"

@module("sury") external to: (t<'from>, t<'to>) => t<'to> = "res_to"

@module("sury") external parser: (~to: t<'value>) => 'any => 'value = "parser"
@module("sury") external asyncParser: (~to: t<'value>) => 'any => promise<'value> = "asyncParser"
@module("sury") external decoder: (~from: t<'from>, ~to: t<'to>) => 'from => 'to = "res_decoder"
@module("sury")
external asyncDecoder: (~from: t<'from>, ~to: t<'to>) => 'from => promise<'to> = "res_asyncDecoder"
@module("sury") external decoder1: t<'value> => unknown => 'value = "decoder1"
@module("sury") external asyncDecoder1: t<'value> => unknown => promise<'value> = "asyncDecoder1"

@module("sury") external parseOrThrow: ('any, ~to: t<'value>) => 'value = "parseOrThrow"
@module("sury")
external parseAsyncOrThrow: ('any, ~to: t<'value>) => promise<'value> = "parseAsyncOrThrow"
@module("sury") external assertOrThrow: ('any, ~to: t<'value>) => unit = "assertOrThrow"
@module("sury")
external assertAsyncOrThrow: ('any, ~to: t<'value>) => promise<unit> = "assertAsyncOrThrow"
@module("sury")
external decodeOrThrow: ('from, ~from: t<'from>, ~to: t<'to>) => 'to = "decodeOrThrow"
@module("sury")
external decodeAsyncOrThrow: ('from, ~from: t<'from>, ~to: t<'to>) => promise<'to> =
  "decodeAsyncOrThrow"

@module("sury") external isAsync: t<'value> => bool = "isAsync"

@module("sury") external recursive: (string, t<'value> => t<'value>) => t<'value> = "recursive"

@module("sury") external noValidation: (t<'value>, bool) => t<'value> = "noValidation"

@module("sury") external toExpression: t<'value> => string = "toExpression"

module Schema = {
  type s = {@as("m") matches: 'value. t<'value> => 'value}
}
@module("sury") external schema: (Schema.s => 'value) => t<'value> = "res_schema"

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
@module("sury") external tuple1: t<'value> => t<'value> = "tuple1"
@module("sury") external tuple2: (t<'v1>, t<'v2>) => t<('v1, 'v2)> = "tuple2"
@module("sury") external tuple3: (t<'v1>, t<'v2>, t<'v3>) => t<('v1, 'v2, 'v3)> = "tuple3"

module Option = {
  @module("sury")
  external getOr: (t<option<'value>>, 'value) => t<'value> = "Option_getOr"
  @module("sury")
  external getOrWith: (t<option<'value>>, unit => 'value) => t<'value> = "Option_getOrWith"
}

module Metadata = {
  module Id = {
    type t<'metadata>
    @module("sury")
    external make: (~namespace: string, ~name: string) => t<'metadata> = "Metadata_Id_make"
  }

  @module("sury")
  external get: (t<'value>, ~id: Id.t<'metadata>) => option<'metadata> = "Metadata_get"

  @module("sury")
  external set: (t<'value>, ~id: Id.t<'metadata>, 'metadata) => t<'value> = "Metadata_set"
}

@module("sury") external reverse: t<'value> => t<unknown> = "reverse"

// =============
// Built-in refinements
// =============

@module("sury") external min: (t<'value>, int, ~message: string=?) => t<'value> = "min"
@module("sury") external floatMin: (t<float>, float, ~message: string=?) => t<float> = "floatMin"

@module("sury") external max: (t<'value>, int, ~message: string=?) => t<'value> = "max"
@module("sury") external floatMax: (t<float>, float, ~message: string=?) => t<float> = "floatMax"

@module("sury") external length: (t<'value>, int, ~message: string=?) => t<'value> = "length"

@module("sury")
external pattern: (t<string>, RegExp.t, ~message: string=?) => t<string> = "pattern"
@module("sury") external trim: t<string> => t<string> = "trim"

type toJSONSchemaOptions = {target?: StandardSchema.JsonSchema.target}
@module("sury")
external toJSONSchema: (t<'value>, ~options: toJSONSchemaOptions=?) => JSONSchema.t = "toJSONSchema"
@module("sury") external fromJSONSchema: JSONSchema.t => t<JSON.t> = "fromJSONSchema"
@module("sury")
external extendJSONSchema: (t<'value>, JSONSchema.t) => t<'value> = "extendJSONSchema"
// Enables `~standard.jsonSchema`; its input/output throw before this is called.
@module("sury") external enableStandardJSONSchema: unit => unit = "enableStandardJSONSchema"

type globalConfigOverride = {
  defaultAdditionalItems?: additionalItemsMode,
  disableNanNumberValidation?: bool,
}

@module("sury") external global: globalConfigOverride => unit = "global"

