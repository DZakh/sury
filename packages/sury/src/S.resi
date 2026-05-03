@@uncurried
@@warning("-30")

type never

module Path: {
  type t

  let empty: t
  let dynamic: t

  external toString: t => string = "%identity"

  let toArray: t => array<string>

  let fromArray: array<string> => t

  let fromLocation: string => t

  let concat: (t, t) => t
}

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

@tag("type")
type rec t<'value> =
  private
  | @as("never") Never({name?: string, title?: string, description?: string, deprecated?: bool, errorMessage?: schemaErrorMessage})
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
      pattern?: Js.Re.t,
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
      const?: Js.Types.symbol,
      name?: string,
      title?: string,
      description?: string,
      deprecated?: bool,
      examples?: array<Js.Types.symbol>,
      default?: Js.Types.symbol,
      errorMessage?: schemaErrorMessage,
    })
  | @as("null")
  Null({
      const: Js.Types.null_val,
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
      const?: Js.Types.function_val,
      name?: string,
      title?: string,
      description?: string,
      deprecated?: bool,
      examples?: array<Js.Types.function_val>,
      default?: Js.Types.function_val,
      errorMessage?: schemaErrorMessage,
    })
  | @as("instance")
  Instance({
      class: unknown,
      const?: Js.Types.obj_val,
      name?: string,
      title?: string,
      description?: string,
      deprecated?: bool,
      examples?: array<Js.Types.obj_val>,
      default?: Js.Types.obj_val,
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
  | @as("ref") Ref({@as("$ref") ref: string, errorMessage?: schemaErrorMessage})
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
}
@unboxed and additionalItems = | ...additionalItemsMode | Schema(t<unknown>)
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
  | @as("custom") Custom({path: Path.t, reason: string})
and flag

module Flag: {
  let none: flag
  let async: flag
  external with: (flag, flag) => flag = "%orint"
}

type exn += private Exn(error)


type s<'value> = {fail: 'a. (string, ~path: Path.t=?) => 'a}

let never: t<never>
let unknown: t<unknown>
let unit: t<unit>
let nullAsUnit: t<unit>
let string: t<string>
let bool: t<bool>
let int: t<int>
let float: t<float>
let bigint: t<bigint>
let symbol: t<Js.Types.symbol>
let nan: t<float>
let date: t<Js.Date.t>
let json: t<Js.Json.t>
let jsonString: t<string>
let jsonStringWithSpace: int => t<string>
let uint8Array: t<Uint8Array.t>
let isoDateTime: t<string>
let port: t<int>
let email: t<string>
let uuid: t<string>
let cuid: t<string>
let url: t<string>

let literal: 'value => t<'value>
let array: t<'value> => t<array<'value>>
let compactColumns: t<'value> => t<array<array<'value>>>
let list: t<'value> => t<list<'value>>
let instance: unknown => t<unknown>
let dict: t<'value> => t<dict<'value>>
let option: t<'value> => t<option<'value>>
let null: t<'value> => t<null<'value>>
let nullAsOption: t<'value> => t<option<'value>>
let nullable: t<'value> => t<nullable<'value>>
let nullableAsOption: t<'value> => t<option<'value>>
let union: array<t<'value>> => t<'value>
let enum: array<'value> => t<'value>

let meta: (t<'value>, meta<'value>) => t<'value>

type transformDefinition<'input, 'output> = {
  @as("p")
  parser?: 'input => 'output,
  @as("a")
  asyncParser?: 'input => promise<'output>,
  @as("s")
  serializer?: 'output => 'input,
}
let transform: (t<'input>, s<'output> => transformDefinition<'input, 'output>) => t<'output>

let refine: (t<'value>, 'value => bool, ~error: string=?, ~path: array<string>=?) => t<'value>

let shape: (t<'value>, 'value => 'shape) => t<'shape>

let to: (t<'from>, t<'to>) => t<'to>

let parser: (~to: t<'value>) => 'any => 'value
let asyncParser: (~to: t<'value>) => 'any => promise<'value>
let decoder: (~from: t<'from>, ~to: t<'to>) => 'from => 'to
let asyncDecoder: (~from: t<'from>, ~to: t<'to>) => 'from => promise<'to>
let decoder1: t<'value> => unknown => 'value
let asyncDecoder1: t<'value> => unknown => promise<'value>

let parseOrThrow: ('any, ~to: t<'value>) => 'value
let parseAsyncOrThrow: ('any, ~to: t<'value>) => promise<'value>
let assertOrThrow: ('any, ~to: t<'value>) => unit
let assertAsyncOrThrow: ('any, ~to: t<'value>) => promise<unit>
let decodeOrThrow: ('from, ~from: t<'from>, ~to: t<'to>) => 'to
let decodeAsyncOrThrow: ('from, ~from: t<'from>, ~to: t<'to>) => promise<'to>

let isAsync: t<'value> => bool

let recursive: (string, t<'value> => t<'value>) => t<'value>

let noValidation: (t<'value>, bool) => t<'value>

let toExpression: t<'value> => string

external castToUnknown: t<'value> => t<unknown> = "%identity"
external castToAny: t<'value> => t<'any> = "%identity"
external untag: t<'value> => untagged = "%identity"

module Schema: {
  type s = {@as("m") matches: 'value. t<'value> => 'value}
}
let schema: (Schema.s => 'value) => t<'value>

module Object: {
  type rec s = {
    @as("f") field: 'value. (string, t<'value>) => 'value,
    fieldOr: 'value. (string, t<'value>, 'value) => 'value,
    tag: 'value. (string, 'value) => unit,
    nested: string => s,
    flatten: 'value. t<'value> => 'value,
  }
}

let object: (Object.s => 'value) => t<'value>

let strip: t<'value> => t<'value>
let deepStrip: t<'value> => t<'value>
let strict: t<'value> => t<'value>
let deepStrict: t<'value> => t<'value>

module Tuple: {
  type s = {
    item: 'value. (int, t<'value>) => 'value,
    tag: 'value. (int, 'value) => unit,
  }
}

let tuple: (Tuple.s => 'value) => t<'value>
let tuple1: t<'value> => t<'value>
let tuple2: (t<'v1>, t<'v2>) => t<('v1, 'v2)>
let tuple3: (t<'v1>, t<'v2>, t<'v3>) => t<('v1, 'v2, 'v3)>

module Option: {
  let getOr: (t<option<'value>>, 'value) => t<'value>
  let getOrWith: (t<option<'value>>, unit => 'value) => t<'value>
}

module Metadata: {
  module Id: {
    type t<'metadata>
    let make: (~namespace: string, ~name: string) => t<'metadata>
  }

  let get: (t<'value>, ~id: Id.t<'metadata>) => option<'metadata>

  let set: (t<'value>, ~id: Id.t<'metadata>, 'metadata) => t<'value>
}

let reverse: t<'value> => t<unknown>

let min: (t<'value>, int, ~message: string=?) => t<'value>
let floatMin: (t<float>, float, ~message: string=?) => t<float>

let max: (t<'value>, int, ~message: string=?) => t<'value>
let floatMax: (t<float>, float, ~message: string=?) => t<float>

let length: (t<'value>, int, ~message: string=?) => t<'value>

let pattern: (t<string>, Js.Re.t, ~message: string=?) => t<string>
let trim: t<string> => t<string>

let toJSONSchema: t<'value> => JSONSchema.t
let fromJSONSchema: JSONSchema.t => t<Js.Json.t>
let extendJSONSchema: (t<'value>, JSONSchema.t) => t<'value>

type globalConfigOverride = {
  defaultAdditionalItems?: additionalItemsMode,
  disableNanNumberValidation?: bool,
}

let global: globalConfigOverride => unit

module Error: {
  type class

  let class: class

  let make: errorDetails => error

  external classify: error => errorDetails = "%identity"
}
