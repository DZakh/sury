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

type numberFormat = | @as("int32") Int32

@tag("type")
type rec t<'value> =
  private
  | @as("never") Never({})
  | @as("unknown") Unknown({})
  | @as("string") String({const: string})
  | @as("number") Number({const: float, format?: numberFormat})
  | @as("bigint") BigInt({const: bigint})
  | @as("boolean") Boolean({const: bool})
  | @as("symbol") Symbol({const: Js.Types.symbol})
  | @as("null") Null({const: Js.Types.null_val})
  | @as("undefined") Undefined({const: unit})
  | @as("nan") NaN({const: float})
  | @as("function") Function({const?: Js.Types.function_val})
  | @as("instance") Instance({const?: Js.Types.obj_val})
  | @as("array") Array({item: t<unknown>})
  | @as("dict") Dict({item: t<unknown>})
  | @as("tuple") Tuple({items: array<item>})
  | @as("object") Object({unknownKeys: unknownKeys, items: array<item>, fields: dict<item>})
  | @as("union") Union({anyOf: array<t<unknown>>, optional?: optional})
  | @as("json") JSON({})
and schema<'a> = t<'a>
and optional = {
  item: t<unknown>,
  hasUndefined?: bool,
  hasNull?: bool,
}
and item = {
  schema: t<unknown>,
  location: string,
  inlinedLocation: string,
}
and flag
and error = private {flag: flag, code: errorCode, path: Path.t}
and errorCode =
  | OperationFailed(string)
  | InvalidOperation({description: string})
  | InvalidType({expected: schema<unknown>, received: unknown})
  | ExcessField(string)
  | InvalidUnion(array<error>)
  | UnexpectedAsync
  | InvalidJsonSchema(schema<unknown>)
and unknownKeys = | @as("strip") Strip | @as("strict") Strict

type exn += private Raised(error)

type s<'value> = {
  schema: t<'value>,
  fail: 'a. (string, ~path: Path.t=?) => 'a,
}

module Flag: {
  @inline let none: flag
  @inline let typeValidation: flag
  @inline let async: flag
  @inline let assertOutput: flag
  @inline let jsonableOutput: flag
  @inline let jsonStringOutput: flag
  @inline let reverse: flag

  external with: (flag, flag) => flag = "%orint"
  let has: (flag, flag) => bool
}

module Error: {
  type class

  let class: class

  let make: (~code: errorCode, ~flag: flag, ~path: Path.t) => error

  let raise: error => 'a

  let message: error => string

  let reason: error => string
}

let never: t<never>
let unknown: t<unknown>
let unit: t<unit>
let string: t<string>
let bool: t<bool>
let int: t<int>
let float: t<float>
let bigint: t<bigint>

let json: (~validate: bool) => t<Js.Json.t>

let literal: 'value => t<'value>
let array: t<'value> => t<array<'value>>
let unnest: t<'value> => t<array<'value>>
let list: t<'value> => t<list<'value>>
let dict: t<'value> => t<dict<'value>>
let option: t<'value> => t<option<'value>>
let null: t<'value> => t<option<'value>>
let nullable: t<'value> => t<Js.Null.t<'value>>
let nullish: t<'value> => t<option<'value>>
let jsonString: (t<'value>, ~space: int=?) => t<'value>
let union: array<t<'value>> => t<'value>
let enum: array<'value> => t<'value>

module Catch: {
  type s<'value> = {
    @as("e") error: error,
    @as("i") input: unknown,
    @as("s") schema: t<'value>,
    @as("f") fail: 'a. (string, ~path: Path.t=?) => 'a,
  }
}
let catch: (t<'value>, Catch.s<'value> => 'value) => t<'value>

let describe: (t<'value>, string) => t<'value>
let description: t<'value> => option<string>

let deprecate: (t<'value>, string) => t<'value>
let deprecation: t<'value> => option<string>

type transformDefinition<'input, 'output> = {
  @as("p")
  parser?: 'input => 'output,
  @as("a")
  asyncParser?: 'input => promise<'output>,
  @as("s")
  serializer?: 'output => 'input,
}
let transform: (t<'input>, s<'output> => transformDefinition<'input, 'output>) => t<'output>

type preprocessDefinition<'input, 'output> = {
  @as("p")
  parser?: unknown => 'output,
  @as("a")
  asyncParser?: unknown => promise<'output>,
  @as("s")
  serializer?: unknown => 'input,
}
let preprocess: (t<'value>, s<'value> => preprocessDefinition<'input, 'output>) => t<'value>

type customDefinition<'input, 'output> = {
  @as("p")
  parser?: unknown => 'output,
  @as("a")
  asyncParser?: unknown => promise<'output>,
  @as("s")
  serializer?: 'output => 'input,
}
let custom: (string, s<'output> => customDefinition<'input, 'output>) => t<'output>

let refine: (t<'value>, s<'value> => 'value => unit) => t<'value>

let shape: (t<'value>, 'value => 'shape) => t<'shape>

let coerce: (t<'from>, t<'to>) => t<'to>

type rec input<'value, 'computed> =
  | @as("Output") Value: input<'value, 'value>
  | @as("Input") Unknown: input<'value, unknown>
  | Any: input<'value, 'any>
  | Json: input<'value, Js.Json.t>
  | JsonString: input<'value, string>
type rec output<'value, 'computed> =
  | @as("Output") Value: output<'value, 'value>
  | @as("Input") Unknown: output<'value, unknown>
  | Assert: output<'value, unit>
  | Json: output<'value, Js.Json.t>
  | JsonString: output<'value, string>
type rec mode<'output, 'computed> =
  | Sync: mode<'output, 'output>
  | Async: mode<'output, promise<'output>>

let compile: (
  t<'value>,
  ~input: input<'value, 'input>,
  ~output: output<'value, 'transformedOutput>,
  ~mode: mode<'transformedOutput, 'output>,
  ~typeValidation: bool=?,
) => 'input => 'output

let parseOrThrow: ('any, t<'value>) => 'value
let parseJsonOrThrow: (Js.Json.t, t<'value>) => 'value
let parseJsonStringOrThrow: (string, t<'value>) => 'value
let parseAsyncOrThrow: ('any, t<'value>) => promise<'value>

let convertOrThrow: ('any, t<'value>) => 'value
let convertToJsonOrThrow: ('any, t<'value>) => Js.Json.t
let convertToJsonStringOrThrow: ('any, t<'value>) => string
let convertAsyncOrThrow: ('any, t<'value>) => promise<'value>

let reverseConvertOrThrow: ('value, t<'value>) => unknown
let reverseConvertToJsonOrThrow: ('value, t<'value>) => Js.Json.t
let reverseConvertToJsonStringOrThrow: ('value, t<'value>, ~space: int=?) => string

let assertOrThrow: ('any, t<'value>) => unit

let isAsync: t<'value> => bool

let recursive: (t<'value> => t<'value>) => t<'value>

let name: t<'value> => string

let setName: (t<'value>, string) => t<'value>

let removeTypeValidation: t<'value> => t<'value>

external toUnknown: t<'any> => t<unknown> = "%identity"

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
  type default = Value(unknown) | Callback(unit => unknown)

  let default: t<'value> => option<default>

  let getOr: (t<option<'value>>, 'value) => t<'value>
  let getOrWith: (t<option<'value>>, unit => 'value) => t<'value>
}

module String: {
  module Refinement: {
    type kind =
      | Min({length: int})
      | Max({length: int})
      | Length({length: int})
      | Email
      | Uuid
      | Cuid
      | Url
      | Pattern({re: Js.Re.t})
      | Datetime
    type t = {
      kind: kind,
      message: string,
    }
  }

  let refinements: t<'value> => array<Refinement.t>
}

module Int: {
  module Refinement: {
    type kind =
      | Min({value: int})
      | Max({value: int})
      | Port
    type t = {
      kind: kind,
      message: string,
    }
  }

  let refinements: t<'value> => array<Refinement.t>
}

module Float: {
  module Refinement: {
    type kind =
      | Min({value: float})
      | Max({value: float})
    type t = {
      kind: kind,
      message: string,
    }
  }

  let refinements: t<'value> => array<Refinement.t>
}

module Array: {
  module Refinement: {
    type kind =
      | Min({length: int})
      | Max({length: int})
      | Length({length: int})
    type t = {
      kind: kind,
      message: string,
    }
  }

  let refinements: t<'value> => array<Refinement.t>
}

module Metadata: {
  module Id: {
    type t<'metadata>
    let make: (~namespace: string, ~name: string) => t<'metadata>
  }

  let get: (t<'value>, ~id: Id.t<'metadata>) => option<'metadata>

  let set: (t<'value>, ~id: Id.t<'metadata>, 'metadata) => t<'value>
}

let inline: t<'value> => string

let reverse: t<'value> => t<unknown>

// =============
// Built-in refinements
// =============

let intMin: (t<int>, int, ~message: string=?) => t<int>
let intMax: (t<int>, int, ~message: string=?) => t<int>
let port: (t<int>, ~message: string=?) => t<int>

let floatMin: (t<float>, float, ~message: string=?) => t<float>
let floatMax: (t<float>, float, ~message: string=?) => t<float>

let arrayMinLength: (t<array<'a>>, int, ~message: string=?) => t<array<'a>>
let arrayMaxLength: (t<array<'a>>, int, ~message: string=?) => t<array<'a>>
let arrayLength: (t<array<'a>>, int, ~message: string=?) => t<array<'a>>

let stringMinLength: (t<string>, int, ~message: string=?) => t<string>
let stringMaxLength: (t<string>, int, ~message: string=?) => t<string>
let stringLength: (t<string>, int, ~message: string=?) => t<string>
let email: (t<string>, ~message: string=?) => t<string>
let uuid: (t<string>, ~message: string=?) => t<string>
let cuid: (t<string>, ~message: string=?) => t<string>
let url: (t<string>, ~message: string=?) => t<string>
let pattern: (t<string>, Js.Re.t, ~message: string=?) => t<string>
let datetime: (t<string>, ~message: string=?) => t<Js.Date.t>
let trim: t<string> => t<string>

// =============
// JS/TS API
// =============

@tag("success")
type jsResult<'value>

let js_safe: (unit => 'v) => jsResult<'v>
let js_safeAsync: (unit => promise<'v>) => promise<jsResult<'v>>

let js_union: array<unknown> => t<'value>

let js_optional: (t<'v>, option<unknown>) => t<option<'v>>

let js_custom: (
  ~name: string,
  ~parser: (unknown, s<'output>) => 'output=?,
  ~serializer: ('output, s<'output>) => 'input=?,
  unit,
) => t<'output>

let js_asyncParserRefine: (t<'output>, ('output, s<'output>) => promise<unit>) => t<'output>
let js_refine: (t<'output>, ('output, s<'output>) => unit) => t<'output>

let js_transform: (
  t<'output>,
  ~parser: ('output, s<'transformed>) => 'transformed=?,
  ~serializer: ('transformed, s<'transformed>) => 'output=?,
) => t<'transformed>

let js_schema: unknown => t<unknown>

let js_merge: (t<unknown>, t<unknown>) => t<unknown>

type globalConfigOverride = {
  defaultUnknownKeys?: unknownKeys,
  disableNanNumberValidation?: bool,
}

let setGlobalConfig: globalConfigOverride => unit
