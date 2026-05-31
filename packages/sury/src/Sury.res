@@uncurried
@@warning("-30")

type never

external castAnyToUnknown: 'any => unknown = "%identity"

module Obj = {
  external magic: 'a => 'b = "%identity"
}

module X = {
  module Proxy = {
    type traps<'a> = {get?: (~target: 'a, ~prop: unknown) => unknown}

    @new
    external make: ('a, traps<'a>) => 'a = "Proxy"
  }

  module Option = {
    external getUnsafe: option<'a> => 'a = "%identity"

    external unsafeToBool: option<'a> => bool = "%identity"
  }

  module Promise = {
    type t<+'a> = promise<'a>

    @send
    external thenResolveWithCatch: (t<'a>, 'a => 'b, exn => 'b) => t<'b> = "then"

    @send
    external thenResolve: (t<'a>, 'a => 'b) => t<'b> = "then"

    @val @scope("Promise")
    external resolve: 'a => t<'a> = "resolve"
  }

  module Object = {
    let immutableEmpty = %raw(`{}`)

    @val external internalClass: Type.Classify.object => string = "Object.prototype.toString.call"

    // Define a type for the property descriptor
    type propertyDescriptor<'a> = {
      configurable?: bool,
      enumerable?: bool,
      writable?: bool,
      value?: 'a,
      get?: unit => 'a,
      set?: 'a => unit,
    }

    external defineProperty: ('obj, string, propertyDescriptor<'a>) => 'obj = "d"
  }

  module Array = {
    let immutableEmpty = %raw(`[]`)

    @send
    external append: (array<'a>, 'a) => array<'a> = "concat"

    @send
    external pushWithLength: (array<'a>, 'a) => int = "push"

    @get_index
    external getUnsafeOptionByString: (array<'a>, string) => option<'a> = ""

    @get_index
    external getUnsafeOption: (array<'a>, int) => option<'a> = ""

    @inline
    let has = (array, idx) => {
      array->Array.getUnsafe(idx)->(Obj.magic: 'a => bool)
    }

    let isArray = Array.isArray

    @send
    external map: (array<'a>, 'a => 'b) => array<'b> = "map"

    @val external fromArguments: array<'a> => array<'a> = "Array.from"
  }

  module Exn = {
    type error

    @new
    external makeError: string => error = "Error"

    let throwAny = (any: 'any): 'a => any->Obj.magic->throw

    let throwError: error => 'a = throwAny
  }

  module Int = {
    @inline
    let plus = (int1: int, int2: int): int => {
      (int1->Int.toFloat +. int2->Int.toFloat)->(Obj.magic: float => int)
    }

    external unsafeToString: int => string = "%identity"
    external unsafeToBool: int => bool = "%identity"
  }

  module String = {
    external unsafeToBool: string => bool = "%identity"

    @get_index
    external getUnsafe: (string, int) => string = ""
  }

  module Dict = {
    let copy: dict<'a> => dict<'a> = %raw(`(d) => ({...d})`)

    @val
    external mixin: (dict<'a>, dict<'a>) => dict<'a> = "Object.assign"

    @get_index
    external getUnsafeOption: (dict<'a>, string) => option<'a> = ""

    // @set_index
    // external setByInt: (dict<'a>, int, 'a) => unit = ""

    // @get_index
    // external getUnsafeOptionByInt: (dict<'a>, int) => option<'a> = ""

    @get_index
    external getUnsafeOptionBySymbol: (dict<'a>, Symbol.t) => option<'a> = ""
  }

  module Float = {
    external unsafeToString: float => string = "%identity"
  }

  module Set = {
    type t<'a>

    @new
    external make: unit => t<'a> = "Set"

    // @new
    // external fromArray: array<'a> => t<'a> = "Set"

    @send external add: (t<'a>, 'a) => unit = "add"

    // @send external has: (t<'a>, 'a) => bool = "has"

    @val external toArray: t<'a> => array<'a> = "Array.from"
  }

  module Function = {
    @variadic @new
    external _make: array<string> => 'function = "Function"

    @inline
    let make2 = (~ctxVarName1, ~ctxVarValue1, ~ctxVarName2, ~ctxVarValue2, ~inlinedFunction) => {
      _make([ctxVarName1, ctxVarName2, `return ${inlinedFunction}`])(ctxVarValue1, ctxVarValue2)
    }

    external toExpression: 'a => 'a = "%unsafe_to_method"
  }

  module Symbol = {
    type t = Symbol.t

    @val external make: string => t = "Symbol"
  }

  module Inlined = {
    module Value = {
      let fromString = (string: string): string => {
        let rec loop = idx => {
          switch string->String.getUnsafe(idx)->(Obj.magic: string => option<string>) {
          | None => `"${string}"`
          | Some("\"") | Some("\n") => string->JSON.stringifyAny->Obj.magic
          | Some(_) => loop(idx + 1)
          }
        }
        loop(0)
      }
    }
  }
}

module Path = {
  type t = string

  external toString: t => string = "%identity"

  @inline
  let empty = ""

  @inline
  let dynamic = "[]"

  let toArray = path => {
    switch path {
    | "" => []
    | _ =>
      path
      ->String.split(`"]["`)
      ->Array.join(`","`)
      ->JSON.parseOrThrow
      ->(Obj.magic: JSON.t => array<string>)
    }
  }

  @inline
  let fromInlinedLocation = inlinedLocation => `[${inlinedLocation}]`

  @inline
  let fromLocation = location => `[${location->X.Inlined.Value.fromString}]`

  let fromArray = array => {
    switch array {
    | [] => ""
    | [location] => fromLocation(location)
    | _ => array->Array.map(fromLocation)->Array.join("")
    }
  }

  let concat = (path, concatedPath) => path ++ concatedPath
}

let vendor = "sury"
// Internal symbol to easily identify SuryError
let s = X.Symbol.make(vendor)
// Internal symbol to identify item proxy
let itemSymbol = X.Symbol.make(vendor ++ ":item")

// A hacky way to prevent prepending path when error is caught.
// Can be removed after we remove effectCtx
// and there's not way to throw outside of the operation context.
@inline
let shouldPrependPathKey = "p"

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

// Use variables to reduce bundle size with min+gzip
// Also as a good practice (ignore that we have tag variant 😅)
let stringTag: tag = %raw(`"string"`)
let numberTag: tag = %raw(`"number"`)
let bigintTag: tag = %raw(`"bigint"`)
let booleanTag: tag = %raw(`"boolean"`)
let symbolTag: tag = %raw(`"symbol"`)
let nullTag: tag = %raw(`"null"`)
let undefinedTag: tag = %raw(`"undefined"`)
let nanTag: tag = %raw(`"nan"`)
let functionTag: tag = %raw(`"function"`)
let instanceTag: tag = %raw(`"instance"`)
let arrayTag: tag = %raw(`"array"`)
let objectTag: tag = %raw(`"object"`)
let unionTag: tag = %raw(`"union"`)
let neverTag: tag = %raw(`"never"`)
let unknownTag: tag = %raw(`"unknown"`)
let refTag: tag = %raw(`"ref"`)
external typeof: 'a => tag = "%typeof"

type standard = {
  version: int,
  vendor: string,
  validate: 'any 'value. 'any => {"value": 'value},
}

type internalDefault = {}

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
and internal = {
  @as("type")
  mutable tag: tag,
  // A serial number for the schema
  // to use for caching operations
  mutable seq?: float,
  // Builder for transforming to the "to" schema
  // If missing, should apply coercion logic
  mutable parser?: builder,
  // A field on the "to" schema,
  // to turn it into "parser", when reversing
  mutable serializer?: builder,
  // Logic for built-in decoding to the schema type
  mutable decoder: builder,
  // Logic for built-in encoding from the schema type
  mutable encoder?: encoder,
  // Custom validations on input (before decoder)
  mutable inputRefiner?: (~input: val) => array<check>,
  // Custom validations on output (after decoder)
  mutable refiner?: (~input: val) => array<check>,
  // A schema we transform to
  mutable to?: internal,
  // When transforming with changing shape,
  // store from which path it came from
  // For S.object, S.tuple, and S.shape
  mutable from?: array<string>,
  // The index of the flattened schema
  // reshaping is happening from
  mutable fromFlattened?: int,
  mutable flattened?: array<internal>,
  mutable const?: char, // use char to avoid Caml_option.some
  mutable class?: char, // use char to avoid Caml_option.some
  mutable name?: string,
  mutable title?: string,
  mutable description?: string,
  mutable deprecated?: bool,
  mutable examples?: array<unknown>,
  mutable default?: internalDefault,
  mutable fromDefault?: internalDefault,
  mutable format?: format,
  mutable has?: dict<bool>,
  mutable anyOf?: array<internal>,
  mutable additionalItems?: additionalItems,
  mutable items?: array<internal>,
  mutable required?: array<string>,
  mutable properties?: dict<internal>,
  mutable noValidation?: bool,
  mutable minimum?: float,
  mutable maximum?: float,
  mutable minLength?: int,
  mutable maxLength?: int,
  mutable minItems?: int,
  mutable maxItems?: int,
  mutable pattern?: RegExp.t,
  mutable errorMessage?: schemaErrorMessage,
  mutable space?: int,
  @as("$ref")
  mutable ref?: string,
  @as("$defs")
  mutable defs?: dict<internal>,
  mutable isAsync?: bool, // Optional value means that it's not lazily computed yet.
  mutable hasTransform?: bool, // Optional value means that it's not lazily computed yet.
  @as("~standard")
  mutable standard?: standard, // This is optional for convenience. The object added on make call
}
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
and builder = (~input: val) => val
and encoder = (~input: val, ~target: internal) => val
and val = {
  // We might have the same value, but different instances of the val object
  // Use the bond field, to connect the var call
  @as("b")
  mutable bond?: val,
  @as("p")
  mutable parent?: val,
  @as("v")
  mutable var: unit => string,
  @as("i")
  mutable inline: string,
  // The schema of the value that is being parsed
  @as("s")
  mutable schema: internal,
  // Whether the val is at output part of expected schema
  // Needed for schemas like S.array(S.nullAsOption) where child schemas might be transformed
  @as("io")
  mutable isOutput?: bool,
  // The schema of the value that we expect to parse into
  @as("e")
  mutable expected: internal,
  mutable prev?: val,
  @as("f")
  mutable flag: flag,
  @as("d")
  mutable vals?: dict<val>,
  @as("fv")
  mutable flattenedVals?: array<val>,
  @as("cp")
  mutable codeFromPrev: string,
  @as("l")
  mutable varsAllocation: string,
  @as("a")
  mutable allocate: string => unit,
  // Invariant: absent iff no checks. Never stored as `Some([])` so callers
  // can test presence with `->unsafeToBool` instead of length.
  @as("vc")
  mutable checks?: array<check>,
  @as("u")
  mutable isUnion?: bool,
  // Whether the chain starting from the root prev has a transformation
  @as("t")
  mutable hasTransform?: bool,
  mutable path: Path.t,
  @as("g")
  global: bGlobal,
  // This is to mark an object field as optional
  // Fields like this should be skipped when the value is undefined
  @as("o")
  mutable optional?: bool,
}
and bGlobal = {
  @as("v")
  mutable varCounter: int,
  @as("o")
  mutable flag: int,
  @as("e")
  embeded: array<unknown>,
  @as("d")
  mutable defs?: dict<internal>,
}
// Adjacent checks sharing `fail` by reference equality are fused with `&&`
// in `emitChecks`, so pass the same helper (e.g. `B.failInvalidType`) to
// every check on a val if you want them to emit as one `||`-throw line.
and check = {
  @as("c")
  cond: (~inputVar: string) => string,
  @as("f")
  fail: (~input: val) => unknown => errorDetails,
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
and jsResult<'value> = | @as(true) Success({value: 'value}) | @as(false) Failure({error: error})

type exn += private Exn(error)

external castToUnknown: t<'any> => t<unknown> = "%identity"
external castToAny: t<'value> => t<'any> = "%identity"
external castToInternal: t<'any> => internal = "%identity"
external castToPublic: internal => t<'any> = "%identity"
external untag: t<'any> => untagged = "%identity"

// This is dirty
@inline
let isSchemaObject = obj => (obj->Obj.magic).standard->Obj.magic

let constField = "const"
let isLiteral = (schema: internal) => schema->Obj.magic->Dict.has(constField)

let isOptional = schema => {
  schema.tag === undefinedTag ||
    (schema.tag === unionTag &&
      schema.has->X.Option.getUnsafe->Dict.has((undefinedTag: tag :> string)))
}

module ValFlag = {
  @inline let none = 0
  @inline let async = 1
}

module Flag = {
  @inline let none = 0
  @inline let async = 1
  @inline let disableNanNumberValidation = 2
  // @inline let flatten = 64

  external with: (flag, flag) => flag = "%orint"
  // @inline
  // let without = (flags, flag) => flags->with(flag)->Int.bitwiseXor(flag)

  let unsafeHas = (acc: flag, flag) => acc->Int.bitwiseAnd(flag)->(Obj.magic: int => bool)
  let has = (acc: flag, flag) => acc->Int.bitwiseAnd(flag) !== 0
}

module TagFlag = {
  @inline let unknown = 1
  @inline let string = 2
  @inline let number = 4
  @inline let boolean = 8
  @inline let undefined = 16
  @inline let null = 32
  @inline let object = 64
  @inline let array = 128
  @inline let union = 256
  @inline let ref = 512
  @inline let bigint = 1024
  @inline let nan = 2048
  @inline let function = 4096
  @inline let instance = 8192
  @inline let symbol = 16384
  @inline let _never = 32768

  let flags = %raw(`{
    [unknownTag]: 1,
    [stringTag]: 2,
    [numberTag]: 4,
    [booleanTag]: 8,
    [undefinedTag]: 16,
    [nullTag]: 32,
    [objectTag]: 64,
    [arrayTag]: 128,
    [unionTag]: 256,
    [refTag]: 512,
    [bigintTag]: 1024,
    [nanTag]: 2048,
    ["function"]: 4096,
    [instanceTag]: 8192,
    [neverTag]: 32768,
    [symbolTag]: 16384,
  }`)

  @inline
  let get = (tag: tag) => flags->Dict.getUnsafe((tag :> string))
}

let rec stringify = unknown => {
  let tagFlag = unknown->typeof->TagFlag.get

  if tagFlag->Flag.unsafeHas(TagFlag.undefined) {
    (undefinedTag :> string)
  } else if tagFlag->Flag.unsafeHas(TagFlag.object) {
    if unknown === %raw(`null`) {
      (nullTag :> string)
    } else if unknown->X.Array.isArray {
      let array = unknown->(Obj.magic: unknown => array<unknown>)
      let string = ref("[")
      for i in 0 to array->Array.length - 1 {
        if i !== 0 {
          string := string.contents ++ ", "
        }
        string := string.contents ++ array->Array.getUnsafe(i)->stringify
      }
      string.contents ++ "]"
    } else if (
      (unknown->(Obj.magic: 'a => {"constructor": unknown}))["constructor"] === %raw("Object")
    ) {
      let dict = unknown->(Obj.magic: unknown => dict<unknown>)
      let keys = Dict.keysToArray(dict)
      let string = ref("{ ")
      for i in 0 to keys->Array.length - 1 {
        let key = keys->Array.getUnsafe(i)
        let value = dict->Dict.getUnsafe(key)
        string := `${string.contents}${key}: ${stringify(value)}; `
      }
      string.contents ++ "}"
    } else {
      unknown->Obj.magic->X.Object.internalClass
    }
  } else if tagFlag->Flag.unsafeHas(TagFlag.string) {
    let string: string = unknown->Obj.magic
    `"${string}"`
  } else if tagFlag->Flag.unsafeHas(TagFlag.bigint) {
    `${unknown->Obj.magic}n`
  } else if tagFlag->Flag.unsafeHas(TagFlag.function) {
    `Function`
  } else {
    (unknown->Obj.magic)["toString"]()
  }
}

let rec toExpression = schema => {
  let schema = schema->castToInternal
  switch schema {
  | {name} => name
  | {const} => const->Obj.magic->stringify

  | {anyOf} =>
    anyOf
    ->(Obj.magic: array<internal> => array<t<'a>>)
    ->Array.map(toExpression)
    ->Array.join(" | ")
  | {format: ?Some(CompactColumns), ?to, ?additionalItems} =>
    // For compactColumns, show the column types if we have properties from .to
    switch to {
    | Some(toSchema) =>
      switch toSchema.properties {
      | Some(props) =>
        let keys = props->Dict.keysToArray
        `[${keys
          ->Array.map(key => {
            let propSchema = props->Dict.getUnsafe(key)->castToPublic
            `${propSchema->toExpression}[]`
          })
          ->Array.join(", ")}]`
      | None => "unknown[][]"
      }
    | None =>
      // No S.to applied, reuse the array expression logic
      switch additionalItems {
      | Some(Schema(innerArraySchema)) =>
        let innerArraySchemaTyped: t<'a> = innerArraySchema->Obj.magic
        `${innerArraySchemaTyped->toExpression}[]`
      | _ => "unknown[][]"
      }
    }
  | {format} => (format :> string)
  | {tag: Object, ?properties, ?additionalItems} =>
    let properties = properties->X.Option.getUnsafe
    let locations = properties->Dict.keysToArray
    if locations->Array.length === 0 {
      if additionalItems->typeof === objectTag {
        let additionalItems: internal = additionalItems->Obj.magic
        `{ [key: string]: ${additionalItems->castToPublic->toExpression}; }`
      } else {
        `{}`
      }
    } else {
      `{ ${locations
        ->Array.map(location => {
          `${location}: ${properties->Dict.getUnsafe(location)->castToPublic->toExpression};`
        })
        ->Array.join(" ")} }`
    }

  | {tag: NaN} => "NaN"
  // Case for val
  | {tag} if %raw(`schema.b`) => (tag :> string)
  | {tag: Array, ?items, ?additionalItems} =>
    let items = items->X.Option.getUnsafe
    if additionalItems->typeof === objectTag {
      let additionalItems: internal = additionalItems->Obj.magic
      let itemName = additionalItems->castToPublic->toExpression
      if (additionalItems.tag :> string) === (unionTag :> string) {
        `(${itemName})`
      } else {
        itemName
      } ++ "[]"
    } else {
      `[${items
        ->Array.map(schema => schema->castToPublic->toExpression)
        ->Array.join(", ")}]`
    }
  | {tag: Instance, ?class} => (class->Obj.magic)["name"]
  | {tag} => (tag :> string)
  }
}

module InternalError = {
  %%raw(`
class SuryError extends Error {
  constructor(params) {
    super();
    for (let key in params) {
      this[key] = params[key];
    }
  }
}

var d = Object.defineProperty, p = SuryError.prototype;
d(p, 'message', {
  get() {
      return message(this);
  },
})
d(p, 'name', {value: 'SuryError'})
d(p, 's', {value: s})
d(p, '_1', {
  get() {
    return this
  },
});
d(p, 'RE_EXN_ID', {
  value: Exn,
});

var seq = 1;
var Schema = function() {}, sp = Object.create(null);
d(sp, 'with', {
  get() {
    return (fn, ...args) => fn(this, ...args)
  },
});
// Also has ~standard below
Schema.prototype = sp;
`)

  @new
  external make: errorDetails => error = "SuryError"

  let getOrRethrow = (exn: exn) => {
    if %raw("exn&&exn.s===s") {
      exn->(Obj.magic: exn => error)
    } else {
      throw(exn)
    }
  }

  // TODO: Throw S.Error
  @inline
  let panic = message => X.Exn.throwError(X.Exn.makeError(`[Sury] ${message}`))

  let message = (error: error) => {
    `${switch error.path {
      | "" => ""
      | nonEmptyPath => `Failed at ${nonEmptyPath}: `
      }}${error.reason}`
  }
}

type globalConfig = {
  @as("m")
  message: error => string,
  @as("d")
  mutable defsAccumulator: option<dict<internal>>,
  @as("a")
  mutable defaultAdditionalItems: additionalItems,
  @as("f")
  mutable defaultFlag: flag,
}

type globalConfigOverride = {
  defaultAdditionalItems?: additionalItemsMode,
  disableNanNumberValidation?: bool,
}

let initialOnAdditionalItems: additionalItemsMode = Strip
let initialDefaultFlag = Flag.none
let globalConfig: globalConfig = {
  message: InternalError.message,
  defsAccumulator: None,
  defaultAdditionalItems: (initialOnAdditionalItems :> additionalItems),
  defaultFlag: initialDefaultFlag,
}

let valueOptions = dict{}
let configurableValueOptions = %raw(`{configurable: true}`)
let valKey = "value"
let reversedKey = "r"

@new
external base: unit => internal = "Schema"
let base = (tag, ~selfReverse) => {
  let s = base()
  s.tag = tag
  s.seq = %raw(`seq++`)
  if selfReverse {
    valueOptions->Dict.set(valKey, s->Obj.magic)
    let _ = X.Object.defineProperty(s, reversedKey, valueOptions->Obj.magic)
  }
  s
}

let noopDecoder = (~input) => input

let factoryCache: dict<internal> = dict{}

let cached = (key: string, tag: tag, init: internal => unit): internal => {
  if factoryCache->X.Dict.getUnsafeOption(key)->X.Option.unsafeToBool {
    factoryCache->Dict.getUnsafe(key)
  } else {
    let s = base(tag, ~selfReverse=true)
    init(s)
    factoryCache->Dict.set(key, s)
    s
  }
}

let unknown = base(unknownTag, ~selfReverse=true)
unknown.decoder = noopDecoder

type s<'value> = {fail: 'a. (string, ~path: Path.t=?) => 'a}

let copySchema: internal => internal = %raw(`(schema) => {
  let c = new Schema()
  for (let k in schema) {
    c[k] = schema[k]
  }
  c.seq = seq++
  return c
}`)
let updateOutput = (schema: internal, fn): t<'value> => {
  let root = schema->copySchema
  let mut = ref(root)
  while mut.contents.to->Obj.magic {
    let next = mut.contents.to->X.Option.getUnsafe->copySchema
    mut.contents.to = Some(next)
    mut := next
  }
  // This should be the Output schema
  fn(mut.contents)
  root->castToPublic
}

module Error = {
  type class

  let class: class = %raw("SuryError")

  let make = InternalError.make

  external classify: error => errorDetails = "%identity"
}

module Builder = {
  type t = builder

  let make = (Obj.magic: ((~input: val) => val) => t)
  let encoder = (Obj.magic: ((~input: val, ~target: internal) => val) => encoder)

  module B = {
    let embed = (b: val, value) => {
      let e = b.global.embeded
      let l = e->Array.length
      e->Array.setUnsafe(l, value->castAnyToUnknown)
      `e[${l->(Obj.magic: int => string)}]`
    }

    let inlineConst = (b, schema) => {
      let tagFlag = schema.tag->TagFlag.get
      let const = schema.const
      if tagFlag->Flag.unsafeHas(TagFlag.undefined) {
        "void 0"
      } else if tagFlag->Flag.unsafeHas(TagFlag.string) {
        const->Obj.magic->X.Inlined.Value.fromString
      } else if tagFlag->Flag.unsafeHas(TagFlag.bigint) {
        const->Obj.magic ++ "n"
      } else if (
        tagFlag->Flag.unsafeHas(
          TagFlag.symbol->Flag.with(TagFlag.function)->Flag.with(TagFlag.instance),
        )
      ) {
        b->embed(schema.const->Obj.magic)
      } else {
        const->Obj.magic
      }
    }

    // Escape it once per compiled operation.
    // Use bGlobal as cache, so we don't allocate another object + it's garbage collected.
    let inlineLocation = (global: bGlobal, location) => {
      let key = `"${location}"`
      switch global->(Obj.magic: bGlobal => dict<string>)->X.Dict.getUnsafeOption(key) {
      | Some(i) => i
      | None => {
          let inlinedLocation = location->X.Inlined.Value.fromString
          global->(Obj.magic: bGlobal => dict<string>)->Dict.set(key, inlinedLocation)
          inlinedLocation
        }
      }
    }

    let secondAllocate = v => {
      let b = %raw(`this`)
      b.varsAllocation = b.varsAllocation ++ "," ++ v
    }

    let initialAllocate = v => {
      let b = %raw(`this`)
      b.varsAllocation = v
      b.allocate = secondAllocate
    }

    let _var = () => (%raw(`this`)).inline

    let _bondVar = () => {
      let val = %raw(`this`)
      let bond = val.bond->X.Option.getUnsafe
      bond.var()
    }

    let _prevVar = () => {
      let val = %raw(`this`)
      let prev = val.prev->X.Option.getUnsafe
      prev.var()
    }

    let varWithoutAllocation = (global: bGlobal) => {
      let newCounter = global.varCounter->X.Int.plus(1)
      global.varCounter = newCounter
      `v${newCounter->X.Int.unsafeToString}`
    }

    let _notVarBeforeValidation = () => {
      let val = %raw(`this`)
      let v = val.global->varWithoutAllocation
      val.codeFromPrev = `let ${v}=${val.inline};`
      val.inline = v
      val.var = _var
      v
    }

    let _notVarAtParent = () => {
      let val = %raw(`this`)
      // FIXME: The parent's allocate is removed during merge. When this val is
      // accessed via a cached bond after the parent has been finalized,
      // fall back to inlining instead of allocating a new variable on the
      // (now finalized) parent. See https://github.com/DZakh/sury/issues/240
      if !((val.parent->X.Option.getUnsafe).allocate->Obj.magic) {
        val.var = _var
        val.inline
      } else {
        let v = val.global->varWithoutAllocation
        (val.parent->X.Option.getUnsafe).allocate(`${v}=${val.inline}`)
        val.var = _var
        val.inline = v
        v
      }
    }

    let _notVar = () => {
      let val: val = %raw(`this`)
      let v = val.global->varWithoutAllocation
      let target = switch val.prev {
      | Some(from) => from
      | None => val // FIXME: Validate that this never happens
      }
      switch val.inline {
      | "" => target.allocate(v)
      | i =>
        if val.codeFromPrev !== "" {
          target.allocate(v)
          val.codeFromPrev = `${val.codeFromPrev}${v}=${i};`
        } else {
          target.allocate(`${v}=${i}`)
        }
      }
      val.var = _var
      val.inline = v
      v
    }

    @inline
    let operationArgVar = "i"

    let operationArg = (~schema, ~expected, ~flag, ~defs): val => {
      {
        codeFromPrev: "",
        var: _var,
        inline: operationArgVar,
        allocate: initialAllocate,
        flag: ValFlag.none,
        schema,
        expected,
        varsAllocation: "",
        // TODO: Add global varsAllocation here
        // Set all the vars to the varsAllocation
        // Measure performance
        // TODO: Also try setting values to embed without allocation
        // (Is it memory leak?)
        path: Path.empty,
        global: {
          ?defs,
          flag,
          embeded: [],
          varCounter: -1,
        },
      }
    }

    let throw = errorDetails => {
      X.Exn.throwAny(InternalError.make(errorDetails))
    }

    let failWithArg = (b: val, fn: 'arg => errorDetails, arg) => {
      `${b->embed(arg => {
          throw(fn(arg))
        })}(${arg})`
    }

    let makeInvalidConversionDetails = (~input, ~to, ~cause) => {
      if %raw("cause&&cause.s===s") {
        let error: error = cause->Obj.magic

        // Read about this in shouldPrependPathKey comment.
        if !(cause->Obj.magic->Dict.getUnsafe(shouldPrependPathKey)) {
          (cause->Obj.magic)["path"] = input.path->Path.concat(error.path)
        }
        error->Error.classify
      } else {
        InvalidConversion({
          from: input.schema->castToPublic,
          to: to->castToPublic,
          cause,
          path: input.path,
          reason: {
            if %raw(`cause instanceof Error`) {
              let text = %raw(`"" + cause`)
              if text->String.startsWith("Error: ") {
                text->String.slice(~start=7)
              } else {
                text
              }
            } else {
              cause->Obj.magic->stringify
            }
          },
        })
      }
    }

    // Checks run against `prev.var()`, so the runtime type at check time
    // is `prev.schema`, not the post-narrowing schema on the current val.
    @inline
    let receivedSchema = (val: val) =>
      switch val.prev {
      | Some(p) => p.schema->castToPublic
      | None => val.schema->castToPublic
      }

    let makeInvalidInputDetails = (
      ~expected,
      ~received,
      ~path,
      ~input,
      ~includeInput,
      ~unionErrors=?,
      ~reasonOverride=?,
    ) => {
      let reasonRef = ref(
        switch reasonOverride {
        | Some(r) => r
        | None =>
          `Expected ${expected
            ->castToPublic
            ->toExpression}, received ${if includeInput {
              input->stringify
            } else {
              received->toExpression
            }}`
        },
      )
      switch unionErrors {
      | Some(caseErrors) => {
          let reasonsDict = dict{}
          for idx in 0 to caseErrors->Array.length - 1 {
            let caseError = caseErrors->Array.getUnsafe(idx)
            let caseReason = caseError.reason->Stdlib.String.split("\n")->Array.join("\n  ")
            let location = switch caseError.path {
            | "" => ""
            | nonEmptyPath => `At ${nonEmptyPath}: `
            }
            let line = `\n- ${location}${caseReason}`
            if reasonsDict->Dict.getUnsafe(line)->X.Int.unsafeToBool->not {
              reasonsDict->Dict.set(line, 1)
              reasonRef := reasonRef.contents ++ line
            }
          }
        }
      | None => ()
      }

      let details = InvalidInput({
        expected: expected->castToPublic,
        received,
        path,
        reason: reasonRef.contents,
        ?unionErrors,
      })
      if includeInput {
        (details->Obj.magic)["input"] = input
      }
      details
    }

    // Drop-in `check.fail` builder for InvalidInput failures. The returned
    // `(~input) => value => details` closure snapshots expected/received/path
    // so it does not retain the val (otherwise the embed array would pin the
    // whole val chain). Pass directly as `check.fail` to skip the wrapper.
    let invalidInputBuilder = (
      ~expected=?,
      ~extraPath=Path.empty,
      ~reasonOverride=?,
      ~includeInput=true,
    ) => (~input: val) => {
      let expected = switch expected {
      | Some(e) => e
      | None => input.expected
      }
      let received = input->receivedSchema
      let path = if extraPath === Path.empty {
        input.path
      } else {
        input.path->Path.concat(extraPath)
      }
      value =>
        makeInvalidInputDetails(
          ~expected,
          ~received,
          ~path,
          ~input=value,
          ~includeInput,
          ~reasonOverride?,
        )
    }

    // Pass this as `fail` on every check that wants "expected X, received Y"
    // error semantics. Stable reference → adjacent checks fuse.
    let failInvalidType = (~input: val) => {
      let override = switch input.expected.errorMessage {
      | Some(em) =>
        let d: dict<string> = em->Obj.magic
        switch d->X.Dict.getUnsafeOption("type") {
        | Some(m) => Some(m)
        | None => d->X.Dict.getUnsafeOption("_")
        }
      | None => None
      }
      invalidInputBuilder(~reasonOverride=?override)(~input)
    }

    let failWithErrorMessage = (key, ~defaultMessage=?) => {
      (~input: val) => {
        let override = switch input.expected.errorMessage {
        | Some(em) =>
          let d: dict<string> = em->Obj.magic
          switch d->X.Dict.getUnsafeOption(key) {
          | Some(m) => Some(m)
          | None => d->X.Dict.getUnsafeOption("_")
          }
        | None => None
        }
        switch (override, defaultMessage) {
        | (Some(m), _) | (None, Some(m)) => invalidInputBuilder(~reasonOverride=m)(~input)
        | (None, None) => failInvalidType(~input)
        }
      }
    }

    // Inline variant: emits the throw expression directly. Used by decoders
    // that splice errors into custom JS (e.g. `catch(_){${embedInvalidInput}}`),
    // not via the `check` pipeline.
    let embedInvalidInput = (~input: val, ~expected=input.expected) => {
      input->failWithArg(invalidInputBuilder(~expected)(~input), input.var())
    }

    // Caller must verify `val.checks->unsafeToBool` and
    // `val.expected.noValidation !== Some(true)` first — the unwrap below
    // is unchecked. `inputVar` is usually `val.prev.var()`.
    let emitChecks = (val: val, ~inputVar: string): string => {
      let checks = val.checks->X.Option.getUnsafe
      let len = checks->Array.length
      if len === 1 {
        let check = checks->Array.getUnsafe(0)
        `${check.cond(~inputVar)}||${val->failWithArg(check.fail(~input=val), inputVar)};`
      } else {
        let out = ref("")
        let i = ref(0)
        while i.contents < len {
          let head = checks->Array.getUnsafe(i.contents)
          let fail = head.fail
          let cond = ref(head.cond(~inputVar))
          i := i.contents + 1
          // Extend the fused cond while the next check shares this `fail`.
          while i.contents < len && (checks->Array.getUnsafe(i.contents)).fail === fail {
            cond :=
              cond.contents ++ "&&" ++ (checks->Array.getUnsafe(i.contents)).cond(~inputVar)
            i := i.contents + 1
          }
          out :=
            out.contents ++ `${cond.contents}||${val->failWithArg(fail(~input=val), inputVar)};`
        }
        out.contents
      }
    }

    // Walks the val.prev chain and assembles generated code. When
    // `~hoistCond` is provided (union codegen), type-narrow checks
    // (fail === failInvalidType) lift into that ref as a dispatch
    // discriminant instead of being emitted; constraint refines still
    // emit inline so their case-specific error message survives. All
    // other callers pass no `~hoistCond` and get the plain merge:
    // every non-`noValidation` check is emitted inline.
    let merge = (val: val, ~hoistCond: option<ref<string>>=?): string => {
      let current = ref(Some(val))
      let code = ref("")

      while current.contents !== None {
        let val = current.contents->X.Option.getUnsafe
        current := val.prev

        let currentCode = ref("")

        if val.checks->X.Option.unsafeToBool {
          let isHoistable =
            hoistCond !== None && (
                val.hasTransform === Some(true)
                  ? (val.prev->X.Option.getUnsafe).hasTransform !== Some(true) &&
                      val.codeFromPrev === ""
                  : true
              )
          if isHoistable {
            // Partition: route type-narrows to hoistCond, emit refines inline.
            // `noValidation` is intentionally bypassed for the hoisted part —
            // the cond routes between union cases, it doesn't reject, so
            // suppressing would break dispatch.
            let prev = current.contents->X.Option.getUnsafe
            let inputVar = prev.var()
            let allChecks = val.checks->X.Option.getUnsafe
            let localHoist = ref("")
            for i in 0 to allChecks->Array.length - 1 {
              let check = allChecks->Array.getUnsafe(i)
              let condCode = check.cond(~inputVar)
              if check.fail === failInvalidType {
                if localHoist.contents->X.String.unsafeToBool {
                  localHoist := `${localHoist.contents}&&${condCode}`
                } else {
                  localHoist := condCode
                }
              } else if val.expected.noValidation !== Some(true) {
                currentCode :=
                  currentCode.contents ++
                  `${condCode}||${val->failWithArg(check.fail(~input=val), inputVar)};`
              }
            }
            if localHoist.contents->X.String.unsafeToBool {
              let cond = hoistCond->X.Option.getUnsafe
              if cond.contents->X.String.unsafeToBool {
                cond := `${localHoist.contents}&&${cond.contents}`
              } else {
                cond := localHoist.contents
              }
            }
          } else if val.expected.noValidation !== Some(true) {
            let prev = current.contents->X.Option.getUnsafe
            currentCode := val->emitChecks(~inputVar=prev.var())
          }
        }

        if val.varsAllocation !== "" {
          currentCode := currentCode.contents ++ `let ${val.varsAllocation};`
        }

        // Delete allocate,
        // this is used to handle Val.var
        // linked to allocated scopes
        let _ = %raw(`delete val$1.a`)

        currentCode := val.codeFromPrev ++ currentCode.contents

        code := currentCode.contents ++ code.contents
      }

      code.contents
    }

    let next = (prev: val, initial: string, ~schema, ~expected=prev.expected): val => {
      {
        // FIXME: vals and other object.val fields should be copied
        prev,
        var: _notVar,
        inline: initial,
        flag: ValFlag.none,
        schema,
        expected,
        codeFromPrev: "",
        varsAllocation: "",
        allocate: initialAllocate,
        path: prev.path,
        global: prev.global,
        hasTransform: true,
        vals: ?prev.vals,
      }
    }

    // Pass a non-empty `~checks` or omit it. Never pass `~checks=[]` —
    // that would break the val.checks "absent iff no checks" invariant.
    let refine = (val: val, ~schema=val.schema, ~checks=?, ~expected=val.expected) => {
      let shouldLink = val.var !== _var
      let nextVal = {
        prev: val,
        inline: val.inline,
        var: shouldLink ? _prevVar : _var,
        flag: val.flag,
        schema,
        expected,
        codeFromPrev: "",
        varsAllocation: "",
        allocate: initialAllocate,
        ?checks,
        path: val.path,
        global: val.global,
        hasTransform: ?val.hasTransform,
        vals: ?val.vals,
      }
      if shouldLink {
        let valVar: unit => string = %raw(`val.v.bind(val)`)
        val.var = () => {
          let v = valVar()
          nextVal.inline = v
          nextVal.var = _var
          v
        }
      }
      nextVal
    }

    // Lazy-allocate helper for mutating an existing val (as opposed to
    // building a local array and passing it through `refine`).
    let pushCheck = (val: val, check: check) => {
      switch val.checks {
      | Some(arr) => arr->Array.push(check)->ignore
      | None => val.checks = Some([check])
      }
    }

    // Applies both refiners. Input checks push onto valInput.checks
    // (emit at pre-transform slot); output checks wrap val via refine.
    // When valInput.prev is None, input checks fold into the output
    // wrap so emit has a prev.var(). Sets isOutput on the result.
    // TODO: async output refiner must run inside .then(), not on the Promise.
    let markOutput = (val: val, ~valInput: val) => {
      let deferredInputChecks = switch valInput.expected.inputRefiner {
      | Some(fn) => {
          let checks = fn(~input=valInput)
          if checks->Array.length > 0 {
            switch valInput.prev {
            | Some(_) => {
                for i in 0 to checks->Array.length - 1 {
                  valInput->pushCheck(checks->Array.getUnsafe(i))
                }
                None
              }
            | None => Some(checks)
            }
          } else {
            None
          }
        }
      | None => None
      }

      let outputChecks = switch val.expected.refiner {
      | Some(fn) => {
          let checks = fn(~input=val)
          checks->Array.length > 0 ? Some(checks) : None
        }
      | None => None
      }

      let val = switch (deferredInputChecks, outputChecks) {
      | (Some(ic), Some(oc)) => val->refine(~checks=ic->Array.concat(oc))
      | (Some(checks), None) | (None, Some(checks)) => val->refine(~checks)
      | (None, None) => val
      }
      val.isOutput = Some(true)
      val
    }

    // Used in union codegen: splice a literal child's checks into the parent
    // as dispatch discriminants. Each cond's `inputVar` is rewritten to
    // `parent[key]`; `fail` stays shared so lifted checks fuse with the
    // parent's own type guard. No-op if the child has no checks.
    let hoistChildChecks = (parent: val, ~child: val, ~key: string) => {
      if child.checks->X.Option.unsafeToBool {
        let pathAppend = parent.global->inlineLocation(key)->Path.fromInlinedLocation
        child.checks
        ->X.Option.getUnsafe
        ->Array.forEach(check => {
          parent->pushCheck({
            cond: (~inputVar) => check.cond(~inputVar=inputVar ++ pathAppend),
            fail: check.fail,
          })
        })
        child.checks = None
      }
    }

    let dynamicScope = (from: val, ~locationVar): val => {
      {
        var: _notVarBeforeValidation,
        inline: `${from.var()}[${locationVar}]`,
        flag: from.flag,
        schema: from.schema.additionalItems->(Obj.magic: option<additionalItems> => internal),
        expected: from.expected.additionalItems->(Obj.magic: option<additionalItems> => internal),
        codeFromPrev: "",
        varsAllocation: "",
        parent: from,
        allocate: initialAllocate,
        path: Path.empty,
        global: from.global,
      }
    }

    let nextConst = (from: val, ~schema, ~expected=?): val => {
      from->next(from->inlineConst(schema), ~schema, ~expected?)
    }

    let asyncVal = (from: val, initial: string): val => {
      let v = from->next(initial, ~schema=from.schema)
      v.flag = ValFlag.async
      v
    }

    module Val = {
      module Object = {
        type t = {
          ...val,
        }

        let add = (objectVal: t, ~location, val: val) => {
          if objectVal.schema.tag === arrayTag {
            objectVal.schema.items->X.Option.getUnsafe->Stdlib.Array.push(val.schema)
          } else {
            if !(val.optional->X.Option.getUnsafe) {
              objectVal.schema.required->X.Option.getUnsafe->Stdlib.Array.push(location)->ignore
            }
            objectVal.schema.properties->X.Option.getUnsafe->Stdlib.Dict.set(location, val.schema)
          }

          // Async field values must be reachable as a plain identifier so
          // the accumulator in completeObjectVal can use val.inline as a
          // destructuring/reference target. For e.g. array-of-async, the
          // asyncVal's inline is a Promise.all(...) expression, not a var.
          // This has to happen before val->merge, which deletes .allocate
          // from the prev chain and locks the emitted code.
          if val.flag->Flag.unsafeHas(ValFlag.async) {
            let _ = val.var()
          }
          objectVal.codeFromPrev = objectVal.codeFromPrev ++ val->merge
          objectVal.vals->X.Option.getUnsafe->Dict.set(location, val)
        }

        let merge = (target: t, vals: dict<val>) => {
          let locations = vals->Dict.keysToArray
          for idx in 0 to locations->Array.length - 1 {
            let location = locations->Array.getUnsafe(idx)
            target->add(~location, vals->Dict.getUnsafe(location))
          }
        }
      }

      @inline
      let var = (val: val) => {
        val.var()
      }

      let addKey = (objVal: val, key, value: val) => {
        `${objVal.var()}[${key}]=${value.inline}`
      }

      let scope = (val: val) => {
        let shouldLink = val.var !== _var

        // TODO: Simplify bond
        let nextVal = {
          inline: val.inline,
          schema: val.schema,
          expected: val.expected,
          flag: Flag.none,
          path: val.path,
          global: val.global,
          var: shouldLink ? _bondVar : _var,
          bond: val,
          codeFromPrev: "",
          isUnion: false,
          varsAllocation: "",
          hasTransform: false,
          allocate: initialAllocate,
          isOutput: ?val.isOutput,
          vals: ?val.vals, // TODO: Is this correct?
        }
        if shouldLink {
          let valVar: unit => string = %raw(`val.v.bind(val)`)
          val.var = () => {
            let v = valVar()
            nextVal.inline = v
            nextVal.var = _var
            v
          }
        }
        nextVal
      }

      let get = (parent: val, location) => {
        let vals = switch parent.vals {
        | Some(d) => d
        | None => {
            let d = dict{}
            parent.vals = Some(d)
            d
          }
        }

        switch vals->X.Dict.getUnsafeOption(location) {
        | Some(v) => v->scope
        | None => {
            let locationSchema = if parent.schema.tag === objectTag {
              parent.schema.properties->X.Option.getUnsafe->X.Dict.getUnsafeOption(location)
            } else {
              parent.schema.items
              ->X.Option.getUnsafe
              ->X.Array.getUnsafeOptionByString(location)
            }
            let schema = switch locationSchema {
            | Some(s) => s
            | None =>
              switch parent.schema.additionalItems->X.Option.getUnsafe {
              | Schema(s) => s->castToInternal
              | _ => InternalError.panic("The schema doesn't have additional items")
              }
            }

            let pathAppend = Path.fromInlinedLocation(parent.global->inlineLocation(location))

            let item = {
              // FIXME: vals and other object.val fields should be copied
              var: _notVarAtParent,
              inline: if schema->isLiteral {
                parent->inlineConst(schema)
              } else {
                `${parent->var}${pathAppend}`
              },
              flag: ValFlag.none,
              schema,
              expected: schema,
              codeFromPrev: "",
              varsAllocation: "",
              allocate: initialAllocate,
              path: parent.path->Path.concat(pathAppend),
              global: parent.global,
              parent,
            }
            vals->Dict.set(location, item)
            item
          }
        }
      }
    }

    let embedTransformation = (~input: val, ~fn: 'input => 'output, ~isAsync) => {
      let outputVar = input.global->varWithoutAllocation
      input.allocate(outputVar)
      let output =
        input->next(outputVar, ~schema=unknown, ~expected=input.expected.to->Option.getUnsafe)
      output.var = _var
      if isAsync {
        if !(input.global.flag->Flag.unsafeHas(Flag.async)) {
          throw(
            InvalidOperation({
              path: Path.empty,
              reason: "Encountered unexpected async transform or refine. Use parseAsyncOrThrow operation instead",
            }),
          )
        }
        output.flag = output.flag->Flag.with(ValFlag.async)
      }
      let embededFn = input->embed(fn)
      let failure = `${output->failWithArg(
          e => makeInvalidConversionDetails(~input, ~to=unknown, ~cause=e),
          `x`,
        )}`
      output.codeFromPrev = `try{${outputVar}=${embededFn}(${input.inline})${isAsync
          ? `.catch(x=>${failure})`
          : ""}}catch(x){${failure}}`
      output
    }

    let effectCtx = (input: val) => {
      fail: (message, ~path=Path.empty) => {
        let error = InternalError.make(
          invalidInputBuilder(~extraPath=path, ~reasonOverride=message, ~includeInput=false)(
            ~input,
          )(%raw("void 0")),
        )
        // Read about this in shouldPrependPathKey comment.
        error->Obj.magic->Dict.set(shouldPrependPathKey, 1)
        Stdlib.JsExn.throw(error)
      },
    }

    let invalidOperation = (val: val, ~description) => {
      throw(InvalidOperation({reason: description, path: val.path}))
    }

    let mergeWithCatch = (val: val, ~catch, ~appendSafe=?) => {
      let valCode = val->merge
      if (
        valCode === "" &&
          // FIXME: Instead of this wrap all S.transform in a try/catch
          !(val.flag->Flag.unsafeHas(ValFlag.async))
      ) {
        valCode ++
        switch appendSafe {
        | Some(append) => append()
        | None => ""
        }
      } else {
        let errorVar = val.global->varWithoutAllocation

        let catchCode = `${catch(~errorVar)};throw ${errorVar}`

        if val.flag->Flag.unsafeHas(ValFlag.async) {
          val.inline = `${val.inline}.catch(${errorVar}=>{${catchCode}})`
        }
        `try{${valCode}${switch appendSafe {
          | Some(append) => append()
          | None => ""
          }}}catch(${errorVar}){${catchCode}}`
      }
    }

    let mergeWithPathPrepend = (val: val, ~parent, ~locationVar=?, ~appendSafe=?) => {
      if val.path === Path.empty && locationVar === None {
        val->merge
      } else {
        val->mergeWithCatch(~appendSafe?, ~catch=(~errorVar) => {
          `${errorVar}.path=${switch parent {
            | {path: ""} => ""
            | {path} => `${path->X.Inlined.Value.fromString}+`
            }}${switch locationVar {
            | Some(var) => `'["'+${var}+'"]'+`
            | _ => ""
            }}${errorVar}.path`
        })
      }
    }

    let unsupportedDecode = (b, ~from: internal, ~target: internal) => {
      throw(
        UnsupportedDecode({
          from: from->castToPublic,
          to: target->castToPublic,
          reason: `Can't decode ${from->castToPublic->toExpression} to ${target
            ->castToPublic
            ->toExpression}. Use S.to to define a custom decoder`,
          path: b.path,
        }),
      )
    }
  }

  let noopOperation = i => i->Obj.magic
  (noopOperation->Obj.magic)["embedded"] = X.Array.immutableEmpty
}
// TODO: Split validation code and transformation code
module B = Builder.B

let int32FormatValidation = (~inputVar) => {
  `${inputVar}<=2147483647&&${inputVar}>=-2147483648&&${inputVar}%1===0`
}

let numberDecoder = Builder.make((~input) => {
  let inputTagFlag = input.schema.tag->TagFlag.get
  if inputTagFlag->Flag.unsafeHas(TagFlag.unknown) {
    let checks = [
      {
        cond: (~inputVar) => `typeof ${inputVar}==="${(numberTag :> string)}"`,
        fail: B.failInvalidType,
      },
    ]
    switch input.expected.format {
    | Some(Int32) =>
      checks
      ->Array.push({
        cond: (~inputVar) => int32FormatValidation(~inputVar),
        fail: B.failInvalidType,
      })
      ->ignore
    | _ =>
      if !(input.global.flag->Flag.unsafeHas(Flag.disableNanNumberValidation)) {
        checks
        ->Array.push({
          cond: (~inputVar) => `!Number.isNaN(${inputVar})`,
          fail: B.failInvalidType,
        })
        ->ignore
      }
    }
    input->B.refine(~schema=input.expected, ~checks)
  } else if inputTagFlag->Flag.unsafeHas(TagFlag.string) {
    let outputVar = input.global->B.varWithoutAllocation
    input.allocate(`${outputVar}=+${input.var()}`)

    let output = input->B.next(outputVar, ~schema=input.expected)
    output.var = B._var

    output.checks = Some([
      {
        cond: (~inputVar as _) =>
          switch input.expected.format {
          | Some(Int32) => int32FormatValidation(~inputVar=outputVar)
          | _ => `!Number.isNaN(${outputVar})`
          },
        fail: B.failInvalidType,
      },
    ])
    output
  } else if !(inputTagFlag->Flag.unsafeHas(TagFlag.number)) {
    input->B.unsupportedDecode(~from=input.schema, ~target=input.expected)
  } else if input.schema.format !== input.expected.format && input.expected.format === Some(Int32) {
    input->B.refine(
      ~schema=input.expected,
      ~checks=[
        {
          cond: (~inputVar) => int32FormatValidation(~inputVar),
          fail: B.failInvalidType,
        },
      ],
    )
  } else {
    input
  }
})

let float = () =>
  cached((numberTag :> string), numberTag, s => {
    s.decoder = numberDecoder
  })

let int = () =>
  cached("i", numberTag, s => {
    s.format = Some(Int32)
    s.decoder = numberDecoder
  })

let rec inputToString = (input: val) => {
  input->B.next(`""+${input.inline}`, ~schema=string())
}
and stringDecoderFn = (~input) => {
  let inputTagFlag = input.schema.tag->TagFlag.get
  if inputTagFlag->Flag.unsafeHas(TagFlag.unknown) {
    input->B.refine(
      ~schema=input.expected,
      ~checks=[
        {
          cond: (~inputVar) => `typeof ${inputVar}==="${(stringTag :> string)}"`,
          fail: B.failInvalidType,
        },
      ],
    )
  } else if (
    inputTagFlag->Flag.unsafeHas(
      TagFlag.boolean->Flag.with(
        TagFlag.number->Flag.with(
          TagFlag.bigint->Flag.with(
            TagFlag.undefined->Flag.with(TagFlag.null->Flag.with(TagFlag.nan)),
          ),
        ),
      ),
    ) && input.schema->isLiteral
  ) {
    let const = %raw(`""+input.s.const`)
    let schema = base(stringTag, ~selfReverse=false)
    schema.const = const->Obj.magic
    input->B.next(`"${const}"`, ~schema)
  } else if (
    inputTagFlag->Flag.unsafeHas(
      TagFlag.boolean->Flag.with(TagFlag.number->Flag.with(TagFlag.bigint)),
    )
  ) {
    input->inputToString
  } else if !(inputTagFlag->Flag.unsafeHas(TagFlag.string)) {
    input->B.unsupportedDecode(~from=input.schema, ~target=input.expected)
  } else {
    input
  }
}
and string = () =>
  cached((stringTag :> string), stringTag, s => {
    s.decoder = Builder.make(stringDecoderFn)
  })

let booleanDecoder = Builder.make((~input) => {
  let inputTagFlag = input.schema.tag->TagFlag.get
  if inputTagFlag->Flag.unsafeHas(TagFlag.unknown) {
    input->B.refine(
      ~schema=input.expected,
      ~checks=[
        {
          cond: (~inputVar) => `typeof ${inputVar}==="${(booleanTag :> string)}"`,
          fail: B.failInvalidType,
        },
      ],
    )
  } else if inputTagFlag->Flag.unsafeHas(TagFlag.string) {
    let outputVar = input.global->B.varWithoutAllocation
    input.allocate(outputVar)

    let output = input->B.next(outputVar, ~schema=input.expected)
    output.var = B._var

    let inputVar = input.var()
    output.codeFromPrev = `(${output.inline}=${inputVar}==="true")||${inputVar}==="false"||${B.embedInvalidInput(
        ~input,
      )};`
    output
  } else if !(inputTagFlag->Flag.unsafeHas(TagFlag.boolean)) {
    input->B.unsupportedDecode(~from=input.schema, ~target=input.expected)
  } else {
    input
  }
})

let bool = () =>
  cached((booleanTag :> string), booleanTag, s => {
    s.decoder = booleanDecoder
  })

let bigintDecoder = Builder.make((~input) => {
  let inputTagFlag = input.schema.tag->TagFlag.get

  if inputTagFlag->Flag.unsafeHas(TagFlag.unknown) {
    input->B.refine(
      ~schema=input.expected,
      ~checks=[
        {
          cond: (~inputVar) => `typeof ${inputVar}==="${(bigintTag :> string)}"`,
          fail: B.failInvalidType,
        },
      ],
    )
  } // TODO: Skip formats which 100% don't match
  else if inputTagFlag->Flag.unsafeHas(TagFlag.string) {
    let outputVar = input.global->B.varWithoutAllocation
    input.allocate(outputVar)
    let output = input->B.next(outputVar, ~schema=input.expected)
    output.var = B._var
    output.codeFromPrev = `try{${outputVar}=BigInt(${input.var()})}catch(_){${B.embedInvalidInput(
        ~input,
      )}}`
    output
  } else if inputTagFlag->Flag.unsafeHas(TagFlag.number) {
    input->B.next(`BigInt(${input.inline})`, ~schema=input.expected)
  } else if !(inputTagFlag->Flag.unsafeHas(TagFlag.bigint)) {
    input->B.unsupportedDecode(~from=input.schema, ~target=input.expected)
  } else {
    input
  }
})

let bigint = () =>
  cached((bigintTag :> string), bigintTag, s => {
    s.decoder = bigintDecoder
  })

let symbolDecoder = Builder.make((~input) => {
  let inputTagFlag = input.schema.tag->TagFlag.get
  if inputTagFlag->Flag.unsafeHas(TagFlag.unknown) {
    input->B.refine(
      ~schema=input.expected,
      ~checks=[
        {
          cond: (~inputVar) => `typeof ${inputVar}==="${(symbolTag :> string)}"`,
          fail: B.failInvalidType,
        },
      ],
    )
  } else if !(inputTagFlag->Flag.unsafeHas(TagFlag.symbol)) {
    input->B.unsupportedDecode(~from=input.schema, ~target=input.expected)
  } else {
    input
  }
})

let symbol = () =>
  cached((symbolTag :> string), symbolTag, s => {
    s.decoder = symbolDecoder
  })

let setHas = (has, tag: tag) => {
  has->Dict.set(
    tag->TagFlag.get->Flag.unsafeHas(TagFlag.union->Flag.with(TagFlag.ref))
      ? (unknownTag: tag :> string)
      : (tag: tag :> string),
    true,
  )
}

let jsonName = `JSON`

let literalDecoder = Builder.make((~input) => {
  let expectedSchema = input.expected
  if expectedSchema.noValidation->X.Option.getUnsafe && !(input.isUnion->X.Option.getUnsafe) {
    input->B.nextConst(~schema=expectedSchema)
  } else if input.schema->isLiteral {
    if input.schema.const === expectedSchema.const {
      input
    } else {
      input->B.nextConst(~schema=expectedSchema)
    }
  } else {
    let schemaTagFlag = expectedSchema.tag->TagFlag.get

    if (
      input.schema.tag->TagFlag.get->Flag.unsafeHas(TagFlag.string) &&
        schemaTagFlag->Flag.unsafeHas(
          TagFlag.boolean->Flag.with(
            TagFlag.number->Flag.with(
              TagFlag.bigint->Flag.with(
                TagFlag.undefined->Flag.with(TagFlag.null->Flag.with(TagFlag.nan)),
              ),
            ),
          ),
        )
    ) {
      let stringConstSchema = base(stringTag, ~selfReverse=false)
      stringConstSchema.const = %raw(`"" + expectedSchema.const`)

      let stringConstVal =
        input->B.nextConst(~schema=stringConstSchema, ~expected=stringConstSchema)

      stringConstVal.checks = Some([
        {
          cond: (~inputVar) => `${inputVar}==="${stringConstSchema.const->Obj.magic}"`,
          fail: B.failInvalidType,
        },
      ])

      stringConstVal->B.nextConst(~schema=expectedSchema, ~expected=expectedSchema)
    } else if schemaTagFlag->Flag.unsafeHas(TagFlag.nan) {
      input->B.refine(
        ~schema=expectedSchema,
        ~checks=[
          {
            cond: (~inputVar) => `Number.isNaN(${inputVar})`,
            fail: B.failInvalidType,
          },
        ],
      )
    } else {
      input->B.refine(
        ~schema=expectedSchema,
        ~checks=[
          {
            cond: (~inputVar) => `${inputVar}===${input->B.inlineConst(expectedSchema)}`,
            fail: B.failInvalidType,
          },
        ],
      )
    }
  }
})

let unit = () =>
  cached((undefinedTag :> string), undefinedTag, s => {
    s.const = %raw(`void 0`)
    s.decoder = literalDecoder
  })

let nullLiteral = () =>
  cached((nullTag :> string), nullTag, s => {
    s.const = %raw(`null`)
    s.decoder = literalDecoder
  })

let nan = () =>
  cached((nanTag :> string), nanTag, s => {
    s.const = %raw(`NaN`)
    s.decoder = literalDecoder
  })

module Literal = {
  let parse = (value): internal => {
    let value = value->castAnyToUnknown
    if value === %raw(`null`) {
      nullLiteral()
    } else {
      switch value->typeof {
      | tag if tag === undefinedTag => unit()
      | tag if tag === numberTag && value->(Obj.magic: unknown => float)->Float.isNaN => nan()
      | tag if tag === objectTag => {
          let s = base(instanceTag, ~selfReverse=true)
          s.class = (value->Obj.magic)["constructor"]
          s.const = value->Obj.magic
          s.decoder = literalDecoder
          s
        }
      | tag => {
          let s = base(tag, ~selfReverse=true)
          s.const = value->Obj.magic
          s.decoder = literalDecoder
          s
        }
      }
    }
  }
}

let rec parse = (input: val) => {
  let valRef = ref(input)
  let appliedEncoderRef = ref(None)
  let loopCount = ref(0)
  while (
    {
      !(valRef.contents.isOutput->Option.getUnsafe) || valRef.contents.expected.to->Obj.magic
    }
  ) {
    let appliedEncoder = appliedEncoderRef.contents
    appliedEncoderRef := None
    let loopInput = valRef.contents

    loopCount := loopCount.contents + 1

    // Console.log(loopInput)
    if loopCount.contents > 50 {
      let error = %raw(`new Error("Loop count exceeded 100")`)
      X.Exn.throwAny(error)
    }

    if loopInput.expected.defs->Obj.magic {
      if loopInput.global.defs->Obj.magic {
        let _ =
          loopInput.global.defs
          ->Stdlib.Option.getUnsafe
          ->Stdlib.Dict.assign(loopInput.expected.defs->Stdlib.Option.getUnsafe)
      } else {
        loopInput.global.defs = loopInput.expected.defs
      }
    }

    if (
      loopInput.flag->Flag.unsafeHas(
        ValFlag.async,
      ) /* FIXME: why was it needed? && step.contents !== #convert */
    ) {
      let operationInputVar = loopInput.var()

      let operationInput = loopInput->B.Val.scope
      let operationOutput = operationInput->parse
      let operationCode = operationOutput->B.merge
      if operationInput.inline !== operationOutput.inline || operationCode !== "" {
        valRef :=
          loopInput->B.next(
            `${operationInputVar}.then(${operationInputVar}=>{${operationCode}return ${operationOutput.inline}})`,
            ~schema=operationOutput.schema,
            ~expected=operationOutput.expected,
          )
      } else {
        valRef :=
          loopInput->B.refine(~schema=operationOutput.schema, ~expected=operationOutput.expected)
      }
      valRef.contents.flag = valRef.contents.flag->Flag.with(ValFlag.async)
      valRef.contents.isOutput = Some(true)
    } else if loopInput.isOutput->Option.getUnsafe {
      // It's guaranteed that to is not None, because it's checked in the while condition
      let to = loopInput.expected.to->Option.getUnsafe
      switch loopInput.expected {
      | {parser} => valRef := parser(~input=loopInput)
      | _ => valRef := valRef.contents->B.refine(~expected=to)
      }
    } else {
      let maybeEncoder = loopInput.schema.encoder
      if (
        maybeEncoder->Obj.magic &&
        maybeEncoder->Obj.magic !== appliedEncoder &&
        loopInput.schema !== loopInput.expected &&
        loopInput.expected.tag !== unknownTag
      ) {
        valRef := (maybeEncoder->Option.getUnsafe)(~input=loopInput, ~target=loopInput.expected)
      }

      // If encoder didn't change the value, we can decode it,
      // otherwise let's start the loop from the beginning
      if loopInput !== valRef.contents {
        appliedEncoderRef := Some(maybeEncoder->Option.getUnsafe)
      } else {
        valRef := loopInput.expected.decoder(~input=loopInput)

        // Primitive decoder (no internal transforms): apply refiners here.
        // Advanced decoders set isOutput themselves and own refiner application.
        if !(valRef.contents.isOutput->Option.getUnsafe) {
          valRef := valRef.contents->B.markOutput(~valInput=valRef.contents)
        }
      }
    }
  }

  valRef.contents
}
and parseDynamic = input => {
  try input->parse catch {
  | _ =>
    let error = %raw(`exn`)->InternalError.getOrRethrow
    (error->Obj.magic)["path"] = {
      // For the case parent must always be present
      switch input.parent {
      | Some(p) => p.path
      | None => Path.empty
      }->Path.concat(input.path->Path.concat(Path.dynamic)->Path.concat(error.path))
    }

    X.Exn.throwAny(error)
  }
}

and isAsyncInternal = (schema, ~defs) => {
  try {
    let input = B.operationArg(~flag=Flag.async, ~defs, ~schema=unknown, ~expected=schema)
    let output = input->parse
    let isAsync = output.flag->Flag.has(ValFlag.async)
    schema.isAsync = Some(isAsync)
    isAsync
  } catch {
  | _ => {
      let _ = %raw(`exn`)->InternalError.getOrRethrow
      false
    }
  }
}
and compileDecoder = (~schema, ~expected, ~flag, ~defs) => {
  let input = B.operationArg(
    ~flag,
    ~defs,
    ~schema=if schema->isLiteral {
      unknown
    } else {
      schema
    },
    ~expected,
  )

  let output = input->parse
  let code = output->B.merge

  let isAsync = output.flag->Flag.has(ValFlag.async)
  expected.isAsync = Some(isAsync)
  let hasTransform = output.hasTransform === Some(true)
  expected.hasTransform = Some(hasTransform)

  if (
    code === "" &&
    (output === input || output.inline === input.inline) &&
    !(flag->Flag.unsafeHas(Flag.async))
  ) {
    Builder.noopOperation
  } else {
    let inlinedOutput = ref(output.inline)
    if flag->Flag.unsafeHas(Flag.async) && !isAsync && !(defs->Obj.magic) {
      inlinedOutput := `Promise.resolve(${inlinedOutput.contents})`
    }

    let inlinedFunction = `${B.operationArgVar}=>{${code}return ${inlinedOutput.contents}}`

    // Console.log(inlinedFunction)

    let fn = X.Function.make2(
      ~ctxVarName1="e",
      ~ctxVarValue1=input.global.embeded,
      ~ctxVarName2="s",
      ~ctxVarValue2=s,
      ~inlinedFunction,
    )
    (fn->Obj.magic)["embedded"] = input.global.embeded
    fn
  }
}
and getOutputSchema = (schema: internal) => {
  switch schema.to {
  | Some(to) => getOutputSchema(to)
  | None => schema
  }
}
// FIXME: Define it as a schema property
and reverse = (schema: internal) => {
  if schema->Obj.magic->Stdlib.Dict.has(reversedKey)->Obj.magic {
    schema->Obj.magic->Stdlib.Dict.getUnsafe(reversedKey)->Obj.magic
  } else {
    let reversedHead = ref(None)
    let current = ref(Some(schema))

    while current.contents->Obj.magic {
      let mut = current.contents->X.Option.getUnsafe->copySchema
      let next = mut.to
      switch reversedHead.contents {
      | None => %raw(`delete mut.to`)
      | Some(to) => mut.to = Some(to)
      }
      let parser = mut.parser
      switch mut.serializer {
      | Some(serializer) => mut.parser = Some(serializer)
      | None => %raw(`delete mut.parser`)
      }
      switch parser {
      | Some(parser) => mut.serializer = Some(parser)
      | None => %raw(`delete mut.serializer`)
      }
      // Swap inputRefiner and refiner
      let refiner = mut.refiner
      switch mut.inputRefiner {
      | Some(inputRefiner) => mut.refiner = Some(inputRefiner)
      | None => %raw(`delete mut.refiner`)
      }
      switch refiner {
      | Some(refiner) => mut.inputRefiner = Some(refiner)
      | None => %raw(`delete mut.inputRefiner`)
      }
      let fromDefault = mut.fromDefault
      switch mut.default {
      | Some(default) => mut.fromDefault = Some(default)
      | None => %raw(`delete mut.fromDefault`)
      }
      switch fromDefault {
      | Some(fromDefault) => mut.default = Some(fromDefault)
      | None => %raw(`delete mut.default`)
      }
      switch mut.items {
      | Some(items) =>
        mut.items = Some(items->Array.map(reverse))

      | None => ()
      }
      switch mut.properties {
      | Some(properties) => {
          let newProperties = dict{}
          let keys = properties->Dict.keysToArray
          for idx in 0 to keys->Array.length - 1 {
            let key = keys->Array.getUnsafe(idx)
            newProperties->Dict.set(key, properties->Dict.getUnsafe(key)->reverse)
          }
          mut.properties = Some(newProperties)
        }
      // Skip tuple
      | None => ()
      }
      if mut.additionalItems->typeof === objectTag {
        mut.additionalItems = Some(
          Schema(
            mut.additionalItems
            ->(Obj.magic: option<additionalItems> => internal)
            ->reverse
            ->castToPublic,
          ),
        )
      }
      switch mut.anyOf {
      | Some(anyOf) =>
        let has = dict{}
        let newAnyOf = []
        for idx in 0 to anyOf->Array.length - 1 {
          let s = anyOf->Array.getUnsafe(idx)
          let reversed = s->reverse
          newAnyOf->Array.push(reversed)->ignore
          has->setHas(reversed.tag)
        }
        mut.has = Some(has)
        mut.anyOf = Some(newAnyOf)
      | None => ()
      }
      switch mut.defs {
      | Some(defs) => {
          let reversedDefs = dict{}
          for idx in 0 to defs->Dict.keysToArray->Array.length - 1 {
            let key = defs->Dict.keysToArray->Array.getUnsafe(idx)
            reversedDefs->Dict.set(key, defs->Dict.getUnsafe(key)->reverse)
          }
          mut.defs = Some(reversedDefs)
        }
      | None => ()
      }
      reversedHead := Some(mut)
      current := next
    }

    // Use defineProperty even though it's slower
    // but it improves logging experience a lot
    // for some reason Wallaby still shows the property
    let r = reversedHead.contents->X.Option.getUnsafe
    valueOptions->Dict.set(valKey, r->Obj.magic)
    let _ = X.Object.defineProperty(schema, reversedKey, valueOptions->Obj.magic)
    valueOptions->Dict.set(valKey, schema->Obj.magic)
    let _ = X.Object.defineProperty(r, reversedKey, valueOptions->Obj.magic)
    r
  }
}

let getDecoder = (~s1 as _, ~flag as _=?) => {
  let args = %raw(`arguments`)
  let idx = ref(0)
  let flag = ref(None)
  let keyRef = ref("")
  let maxSeq = ref(0.)
  let cacheTarget = ref(None)

  while flag.contents === None {
    let arg = args->Array.getUnsafe(idx.contents)
    if !(arg->Obj.magic) {
      let f = globalConfig.defaultFlag
      flag := Some(f)
      keyRef := keyRef.contents ++ "-" ++ f->X.Int.unsafeToString
    } else if typeof(arg->Obj.magic) === numberTag {
      let f = arg->Obj.magic->Flag.with(globalConfig.defaultFlag)
      flag := Some(f)
      keyRef := keyRef.contents ++ "-" ++ f->X.Int.unsafeToString
    } else {
      let schema: internal = arg->Obj.magic
      let seq: float = schema.seq->Obj.magic
      if seq > maxSeq.contents {
        maxSeq := seq
        cacheTarget := Some(schema)
      }
      keyRef := keyRef.contents ++ seq->Obj.magic ++ "-"
      idx := idx.contents + 1
    }
  }

  switch cacheTarget.contents {
  | None => InternalError.panic("No schema provided for decoder.")
  | Some(cacheTarget) => {
      let key = keyRef.contents
      if cacheTarget->Obj.magic->Stdlib.Dict.has(key) {
        cacheTarget->Obj.magic->Stdlib.Dict.getUnsafe(key)->Obj.magic
      } else {
        let schema = ref(args->Array.getUnsafe(idx.contents - 1))
        for i in idx.contents - 2 downto 0 {
          let to = schema.contents
          schema :=
            args
            ->Array.getUnsafe(i)
            ->updateOutput(mut => {
              mut.to = Some(to)
            })
            ->castToInternal
        }
        let f = compileDecoder(
          ~schema=schema.contents,
          ~expected=schema.contents,
          ~flag=flag.contents->X.Option.getUnsafe,
          ~defs=%raw(`0`),
        )
        // Reusing the same object makes it a little bit faster
        valueOptions->Dict.set(valKey, f)
        // Use defineProperty, so the cache keys are not enumerable
        let _ = X.Object.defineProperty(cacheTarget, key, valueOptions->Obj.magic)
        f->(Obj.magic: (unknown => unknown) => 'from => 'to)
      }
    }
  }
}

@val
external getDecoder2: (~s1: internal, ~s2: internal, ~flag: flag=?) => 'a => 'b = "getDecoder"

@val
external getDecoder3: (~s1: internal, ~s2: internal, ~s3: internal, ~flag: flag=?) => 'a => 'b =
  "getDecoder"

let rec makeObjectVal = (prev: val, ~schema): B.Val.Object.t => {
  {
    prev,
    var: B._notVar,
    inline: "",
    flag: ValFlag.none,
    schema: schema.tag === arrayTag
      ? {
          tag: arrayTag,
          items: [],
          additionalItems: Strict,
          decoder: arrayDecoder,
        }
      : {
          {
            tag: objectTag,
            required: [],
            properties: dict{},
            additionalItems: Strict,
            decoder: objectDecoder,
          }
        },
    expected: prev.expected,
    vals: dict{},
    hasTransform: true,
    codeFromPrev: "",
    varsAllocation: "",
    allocate: B.initialAllocate,
    path: prev.path,
    global: prev.global,
  }
}
and completeObjectVal = (objectVal: B.Val.Object.t) => {
  let isArray = objectVal.schema.tag === arrayTag
  let inline = ref("")
  let promiseAllContent = ref("")
  let optionalSettingCode = ref(None)

  let keys = objectVal.vals->X.Option.getUnsafe->Dict.keysToArray

  for idx in 0 to keys->Array.length - 1 {
    let key = keys->Array.getUnsafe(idx)
    let val = objectVal.vals->X.Option.getUnsafe->Dict.getUnsafe(key)
    if val.flag->Flag.unsafeHas(ValFlag.async) {
      promiseAllContent := promiseAllContent.contents ++ val.inline ++ ","
    }
    if val.optional->X.Option.getUnsafe {
      let existingFn = optionalSettingCode.contents
      optionalSettingCode :=
        Some(
          (~objectVar) => {
            switch existingFn {
            | None => ""
            | Some(fn) => fn(~objectVar)
            } ++
            `if(${val.var()}!==void 0){${objectVar}[${objectVal.global->B.inlineLocation(
                key,
              )}]=${val.inline}}`
          },
        )
    } else {
      inline :=
        inline.contents ++
        (isArray
          ? `${val.inline}`
          : `${objectVal.global->B.inlineLocation(key)}:${val.inline}`) ++ ","
    }
  }

  objectVal.inline = isArray ? "[" ++ inline.contents ++ "]" : "{" ++ inline.contents ++ "}"

  // FIXME: Test whether it's needed
  // objectVal.additionalItems = Some(Strict)
  let valWithRequired = (objectVal :> val)

  if promiseAllContent.contents->X.String.unsafeToBool {
    // FIXME: Test how it works with optional and fix it
    let operationInput = valWithRequired->B.Val.scope
    operationInput.isOutput = Some(true)
    let operationOutput = operationInput->parse
    let operationCode = operationOutput->B.merge

    if operationCode === "" && promiseAllContent.contents === `${operationOutput.inline},` {
      valWithRequired.inline = operationOutput.inline
    } else {
      valWithRequired.inline = `Promise.all([${promiseAllContent.contents}]).then(([${promiseAllContent.contents}])=>{${operationCode}return ${operationOutput.inline}})`
    }
    valWithRequired.flag = valWithRequired.flag->Flag.with(ValFlag.async)
    valWithRequired.schema = operationOutput.schema
    valWithRequired.expected = operationOutput.expected
    valWithRequired.isOutput = Some(true)
    valWithRequired
  } else {
    switch optionalSettingCode.contents {
    | None => valWithRequired
    | Some(fn) => {
        let code = fn(~objectVar=valWithRequired.var())
        let output = valWithRequired->B.refine
        output.codeFromPrev = output.codeFromPrev ++ code
        output
      }
    }
  }
}
and array = item => {
  let itemInternal = item->castToInternal
  let mut = base(
    arrayTag,
    ~selfReverse=itemInternal->Obj.magic->Stdlib.Dict.getUnsafe(reversedKey) ===
      itemInternal->Obj.magic,
  )
  mut.additionalItems = Some(Schema(itemInternal->castToPublic))
  mut.items = Some(X.Array.immutableEmpty)
  mut.decoder = arrayDecoder
  mut->castToPublic
}
and arrayDecoder: builder = (~input as unknownInput) => {
  let isUnion = unknownInput.isUnion->X.Option.getUnsafe
  let expectedSchema = unknownInput.expected
  let unknownInputTagFlag = unknownInput.schema.tag->TagFlag.get
  let expectedItems = expectedSchema.items->X.Option.getUnsafe
  let expectedLength = expectedItems->Array.length

  let input = if unknownInputTagFlag->Flag.unsafeHas(TagFlag.unknown->Flag.with(TagFlag.array)) {
    let isArrayInput = unknownInputTagFlag->Flag.unsafeHas(TagFlag.array)
    let schema = if !isArrayInput {
      array(unknown->castToPublic)->castToInternal
    } else {
      unknownInput.schema
    }
    let checks: array<check> = []
    if !isArrayInput {
      checks
      ->Array.push({
        cond: (~inputVar) => `Array.isArray(${inputVar})`,
        fail: B.failInvalidType,
      })
      ->ignore
    }

    let isExactSize = switch schema.additionalItems->X.Option.getUnsafe {
    | Schema(_) => false
    | _ => schema.items->X.Option.getUnsafe->Array.length === expectedLength
    }

    if !isExactSize {
      switch expectedSchema.additionalItems->X.Option.getUnsafe {
      | Strict =>
        checks
        ->Array.push({
          cond: (~inputVar) => `${inputVar}.length===${expectedLength->X.Int.unsafeToString}`,
          fail: B.failInvalidType,
        })
        ->ignore
      | Strip =>
        checks
        ->Array.push({
          cond: (~inputVar) => `${inputVar}.length>=${expectedLength->X.Int.unsafeToString}`,
          fail: B.failInvalidType,
        })
        ->ignore

      | _ => ()
      }
    }

    // Apply refine also when there are no checks,
    // so literals for union cases don't mutate input
    // FIXME: This should be removed and validation be attached to output
    if checks->Array.length > 0 {
      unknownInput->B.refine(~schema, ~checks)
    } else {
      unknownInput->B.refine(~schema)
    }
  } else {
    unknownInput->B.unsupportedDecode(~from=unknownInput.schema, ~target=expectedSchema)
  }

  let output = switch expectedSchema.additionalItems->X.Option.getUnsafe {
  | Schema(itemSchema) => {
      let itemSchema = itemSchema->castToInternal
      if itemSchema === unknown {
        input
      } else {
        let inputVar = input->B.Val.var
        let iteratorVar = input.global->B.varWithoutAllocation

        let itemInput = input->B.dynamicScope(~locationVar=iteratorVar)
        let itemOutput = itemInput->parseDynamic
        let hasTransform = itemOutput.hasTransform->X.Option.getUnsafe
        let output = hasTransform
          ? input->B.next(`new Array(${inputVar}.length)`, ~schema=expectedSchema) // FIXME: schema here should be input.expected output
          : input->B.refine(~schema=expectedSchema)

        let itemCode =
          itemOutput->B.mergeWithPathPrepend(
            ~parent=input,
            ~locationVar=iteratorVar,
            ~appendSafe=?hasTransform
              ? Some(() => output->B.Val.addKey(iteratorVar, itemOutput))
              : None,
          )

        if hasTransform || itemCode !== "" {
          output.codeFromPrev =
            output.codeFromPrev ++
            `for(let ${iteratorVar}=${expectedLength->X.Int.unsafeToString};${iteratorVar}<${inputVar}.length;++${iteratorVar}){${itemCode}}`
        }

        if itemOutput.flag->Flag.unsafeHas(ValFlag.async) {
          output->B.asyncVal(`Promise.all(${output.inline})`)
        } else {
          output
        }
      }
    }
  | _ =>
    let objectVal = input->makeObjectVal(~schema=expectedSchema)
    let shouldRecreateInput = ref(
      switch expectedSchema.additionalItems->X.Option.getUnsafe {
      // Since we have a check validating the exact properties existence
      | Strict => false
      | Strip =>
        switch input.schema.additionalItems->X.Option.getUnsafe {
        | Schema(_) => true
        | _ => input.schema.items->X.Option.getUnsafe->Array.length !== expectedLength
        }
      | _ => true
      },
    )

    for idx in 0 to expectedLength - 1 {
      let schema = expectedItems->Array.getUnsafe(idx)
      let key = idx->Int.toString
      let itemInput = input->B.Val.get(key)
      itemInput.expected = schema
      itemInput.isOutput = Some(false)
      itemInput.isUnion = Some(isUnion) // We want to controll validation on the decoder side
      let itemOutput = itemInput->parse

      if isUnion && schema->isLiteral {
        input->B.hoistChildChecks(~child=itemOutput, ~key)
      }

      objectVal->B.Val.Object.add(~location=key, itemOutput)
      if !shouldRecreateInput.contents {
        shouldRecreateInput := itemOutput.hasTransform->X.Option.getUnsafe
      }
    }

    // After input.schema was used, set it to selfSchema
    // so it has a more accurate name in error messages
    if shouldRecreateInput.contents {
      objectVal->completeObjectVal
    } else {
      let o = input->B.refine
      o.codeFromPrev = objectVal.codeFromPrev
      o.vals = objectVal.vals
      o
    }
  }
  output->B.markOutput(~valInput=input)
}
and objectDecoder: Builder.t = (~input as unknownInput) => {
  let isUnion = unknownInput.isUnion->X.Option.getUnsafe
  let expectedSchema = unknownInput.expected

  let unknownInputTagFlag = unknownInput.schema.tag->TagFlag.get

  let input = if unknownInputTagFlag->Flag.unsafeHas(TagFlag.unknown->Flag.with(TagFlag.object)) {
    let isObjectInput = unknownInputTagFlag->Flag.unsafeHas(TagFlag.object)
    let schema = if !isObjectInput {
      // TODO: Use dictFactory here
      let mut = base(objectTag, ~selfReverse=false)
      mut.properties = Some(X.Object.immutableEmpty)
      mut.additionalItems = Some(Schema(unknown->castToPublic))
      mut
    } else {
      unknownInput.schema
    }
    let checks: array<check> = []
    if !isObjectInput {
      checks
      ->Array.push({
        cond: (~inputVar) => `typeof ${inputVar}==="${(objectTag :> string)}"&&${inputVar}`,
        fail: B.failInvalidType,
      })
      ->ignore
      if expectedSchema.additionalItems->X.Option.getUnsafe !== Strip {
        // For strip case we recreate the value
        // For other cases we might optimize it,
        // this is why the check is a must have
        checks
        ->Array.push({
          cond: (~inputVar) => `!Array.isArray(${inputVar})`,
          fail: B.failInvalidType,
        })
        ->ignore
      }
    }

    // Apply refine also when there are no checks,
    // so literals for union cases don't mutate input
    if checks->Array.length > 0 {
      unknownInput->B.refine(~schema, ~checks)
    } else {
      unknownInput->B.refine(~schema)
    }
  } else {
    unknownInput->B.unsupportedDecode(~from=unknownInput.schema, ~target=expectedSchema)
  }

  let output = switch expectedSchema.additionalItems->X.Option.getUnsafe {
  | Schema(itemSchema) => {
      let itemSchema = itemSchema->castToInternal
      if itemSchema === unknown {
        input
      } else {
        let inputVar = input.var()
        let keyVar = input.global->B.varWithoutAllocation
        let itemInput = input->B.dynamicScope(~locationVar=keyVar)
        let itemOutput = itemInput->parseDynamic

        let hasTransform = itemOutput.hasTransform->X.Option.getUnsafe
        let output = hasTransform
        // FIXME: schema should be expectedSchema output
          ? input->B.next("{}", ~schema=expectedSchema)
          : input->B.refine(~schema=expectedSchema)

        let itemCode =
          itemOutput->B.mergeWithPathPrepend(
            ~parent=input,
            ~locationVar=keyVar,
            ~appendSafe=?hasTransform ? Some(() => output->B.Val.addKey(keyVar, itemOutput)) : None,
          )

        if hasTransform || itemCode !== "" {
          output.codeFromPrev =
            output.codeFromPrev ++ `for(let ${keyVar} in ${inputVar}){${itemCode}}`
        }

        if itemOutput.flag->Flag.unsafeHas(ValFlag.async) {
          let resolveVar = output.global->B.varWithoutAllocation
          let rejectVar = output.global->B.varWithoutAllocation
          let asyncParseResultVar = output.global->B.varWithoutAllocation
          let counterVar = output.global->B.varWithoutAllocation
          let outputVar = B.Val.var(output)
          output->B.asyncVal(
            `new Promise((${resolveVar},${rejectVar})=>{let ${counterVar}=Object.keys(${outputVar}).length;for(let ${keyVar} in ${outputVar}){${outputVar}[${keyVar}].then(${asyncParseResultVar}=>{${outputVar}[${keyVar}]=${asyncParseResultVar};if(${counterVar}--===1){${resolveVar}(${outputVar})}},${rejectVar})}})`,
          )
        } else {
          output
        }
      }
    }
  | _ => {
      let properties = expectedSchema.properties->X.Option.getUnsafe
      let keys = Dict.keysToArray(properties)
      let keysCount = keys->Array.length

      let objectVal = input->makeObjectVal(~schema=expectedSchema)
      let shouldRecreateInput = ref(
        switch expectedSchema.additionalItems->X.Option.getUnsafe {
        // Since we have a check validating the exact properties existence
        | Strict => false
        | Strip =>
          switch input.schema.additionalItems->X.Option.getUnsafe {
          | Schema(_) => true
          | _ =>
            input.schema.properties->X.Option.getUnsafe->Dict.keysToArray->Array.length !==
              keysCount
          }
        | _ => true
        },
      )

      // FIXME: hack — detect "JSON-sourced object" via additionalItems=json
      // (set by jsonEncoderFn) and patch the field read inline to coalesce
      // `??null`. The proper fix is for the JSON pipeline to treat missing
      // object keys as the option's empty sentinel, instead of leaving
      // objectDecoder to sniff the source and rewrite codegen by hand:
      //   - jsonEncoderFn rewrites the option arm from `v===void 0` to
      //     `v===null` because JSON has no undefined,
      //   - but `i[key]` for a missing key returns undefined, so the
      //     rewritten arm rejects `{}` for `{foo: option<...>}`.
      // Detection is fragile (string-compares the schema name) and only
      // covers the union-with-undefined shape; fold this into a shared
      // JSON option representation post-release.
      let isJsonParent = switch input.schema.additionalItems->X.Option.getUnsafe {
      | Schema(s) => (s->castToInternal).name === Some(jsonName)
      | _ => false
      }

      for idx in 0 to keysCount - 1 {
        let key = keys->Array.getUnsafe(idx)
        let schema = properties->Dict.getUnsafe(key)

        let itemInput = input->B.Val.get(key)
        itemInput.expected = schema
        itemInput.isOutput = Some(false)
        itemInput.isUnion = Some(isUnion) // We want to controll validation on the decoder side
        if (
          isJsonParent &&
          schema.tag === unionTag &&
            schema.has->X.Option.getUnsafe->Dict.getUnsafe((undefinedTag :> string))
        ) {
          itemInput.inline = `(${itemInput.inline}??null)`
        }
        let itemOutput = itemInput->parse

        if isUnion && schema->isLiteral {
          input->B.hoistChildChecks(~child=itemOutput, ~key)
        }

        objectVal->B.Val.Object.add(~location=key, itemOutput)
        if !shouldRecreateInput.contents {
          shouldRecreateInput := itemOutput.hasTransform->X.Option.getUnsafe
        }
      }

      if (
        expectedSchema.additionalItems === Some(Strict) &&
          switch input.schema.additionalItems->X.Option.getUnsafe {
          | Schema(_) => true
          | _ => false
          }
      ) {
        let keyVar = objectVal.global->B.varWithoutAllocation
        input.allocate(keyVar)
        objectVal.codeFromPrev = objectVal.codeFromPrev ++ `for(${keyVar} in ${input.var()}){if(`
        switch keys {
        | [] => objectVal.codeFromPrev = objectVal.codeFromPrev ++ "true"
        | _ =>
          for idx in 0 to keys->Array.length - 1 {
            let key = keys->Array.getUnsafe(idx)
            if idx !== 0 {
              objectVal.codeFromPrev = objectVal.codeFromPrev ++ "&&"
            }
            objectVal.codeFromPrev =
              objectVal.codeFromPrev ++ `${keyVar}!==${input.global->B.inlineLocation(key)}`
          }
        }
        objectVal.codeFromPrev =
          objectVal.codeFromPrev ++ `){${input->B.failWithArg(exccessFieldName => UnrecognizedKeys({
              path: objectVal.path,
              reason: `Unrecognized key "${exccessFieldName}"`,
              keys: [exccessFieldName],
            }), keyVar)}}}`
      }

      // After input.schema was used, set it to selfSchema
      // so it has a more accurate name in error messages
      if shouldRecreateInput.contents {
        objectVal->completeObjectVal
      } else {
        let o = input->B.refine
        o.codeFromPrev = objectVal.codeFromPrev
        o.vals = objectVal.vals
        o
      }
    }
  }
  output->B.markOutput(~valInput=input)
}

let recursiveDecoder = Builder.make((~input) => {
  let expectedSchema = input.expected

  let schemaRef = expectedSchema.ref->X.Option.getUnsafe
  let defs = input.global.defs->X.Option.getUnsafe
  // Ignore #/$defs/
  let identifier = schemaRef->String.slice(~start=8)
  let def = defs->Dict.getUnsafe(identifier)
  let flag = input.global.flag

  let inputSchema = if input.schema.seq === expectedSchema.seq {
    def
  } else {
    input.schema
  }

  let key = `${inputSchema.seq->Obj.magic}-${def.seq->Obj.magic}--${flag->Obj.magic}`
  let recOperation = ref("")

  switch def->Obj.magic->X.Dict.getUnsafeOption(key) {
  | Some(fn) =>
    // Circular reference (fn === 0) or already compiled
    recOperation := if fn === %raw(`0`) {
        input->B.embed(def) ++ `["${key}"]`
      } else {
        input->B.embed(fn)
      }
  | None => {
      // Optimistic compilation with recompile if assumptions were wrong
      let assumedHasTransform = ref(def.hasTransform->Option.getOr(false))
      let assumedIsAsync = ref(def.isAsync->Option.getOr(false))
      let compileNeeded = ref(true)
      let finalFn = ref(Obj.magic(0))

      while compileNeeded.contents {
        compileNeeded := false

        // Set optimistic values on def before compiling (if not already set)
        // Inner circular references will read these values
        if def.hasTransform === None {
          def.hasTransform = Some(assumedHasTransform.contents)
        }
        if def.isAsync === None {
          def.isAsync = Some(assumedIsAsync.contents)
        }

        // Mark as in-progress
        configurableValueOptions->Dict.set(valKey, 0->Obj.magic)
        let _ = X.Object.defineProperty(def, key, configurableValueOptions->Obj.magic)

        // Compile
        let fn = compileDecoder(~schema=inputSchema, ~expected=def, ~flag, ~defs=Some(defs))

        // Cache result
        valueOptions->Dict.set(valKey, fn)
        let _ = X.Object.defineProperty(def, key, valueOptions->Obj.magic)

        finalFn := fn

        // Check if actual values differ from assumed
        let actualHasTransform = def.hasTransform->X.Option.getUnsafe
        let actualIsAsync = def.isAsync->X.Option.getUnsafe

        if (
          actualHasTransform !== assumedHasTransform.contents ||
            actualIsAsync !== assumedIsAsync.contents
        ) {
          // Wrong assumption - update and recompile
          assumedHasTransform := actualHasTransform
          assumedIsAsync := actualIsAsync
          // Delete cached function to force recompilation
          let _ = %raw(`delete def[key]`)
          compileNeeded := true
        }
      }

      // Embed only the final compiled function to avoid wasting embed slots on recompiles
      recOperation := input->B.embed(finalFn.contents)
    }
  }

  let hasTransform = def.hasTransform === Some(true)
  let isAsync = def.isAsync->X.Option.getUnsafe

  let output = if hasTransform || isAsync {
    let outputVar = input.global->B.varWithoutAllocation
    input.allocate(outputVar)

    let output = input->B.next(outputVar, ~schema=expectedSchema, ~expected=expectedSchema)
    output.var = B._var

    output.codeFromPrev = `${outputVar}=${recOperation.contents}(${input.inline});`

    if isAsync {
      output.flag = output.flag->Flag.with(ValFlag.async)
    }
    output
  } else {
    // No transform: call for validation but don't capture result
    let output = input->B.refine(~schema=expectedSchema, ~expected=expectedSchema)
    output.codeFromPrev = `${recOperation.contents}(${input.inline});`
    output
  }

  output.prev = None
  output.codeFromPrev = output->B.mergeWithPathPrepend(~parent=input)

  // Restore allocate after merge deleted it, since this val may be reused as
  // input to a subsequent parser (e.g. S.transform on a recursive schema).
  output.allocate = B.initialAllocate
  output.prev = Some(input)

  output
})

let instanceDecoder = Builder.make((~input) => {
  let inputTagFlag = input.schema.tag->TagFlag.get
  if inputTagFlag->Flag.unsafeHas(TagFlag.unknown) {
    input->B.refine(
      ~schema=input.expected,
      ~checks=[
        {
          cond: (~inputVar) => `${inputVar} instanceof ${input->B.embed(input.expected.class)}`,
          fail: B.failInvalidType,
        },
      ],
    )
  } else if (
    inputTagFlag->Flag.unsafeHas(TagFlag.instance) && input.schema.class === input.expected.class
  ) {
    input
  } else {
    input->B.unsupportedDecode(~from=input.schema, ~target=input.expected)
  }
})

let instance = class_ => {
  let mut = base(instanceTag, ~selfReverse=true)
  mut.class = class_->Obj.magic
  mut.decoder = instanceDecoder
  mut->castToPublic
}

X.Object.defineProperty(
  %raw(`sp`),
  "~standard",
  {
    get: (
      () => {
        let schema = %raw(`this`)
        {
          version: 1,
          vendor,
          validate: input => {
            try {
              {
                "value": getDecoder2(~s1=unknown, ~s2=schema)(input->Obj.magic)->Obj.magic,
              }
            } catch {
            | _ => {
                let error = %raw(`exn`)->InternalError.getOrRethrow
                {
                  "issues": [
                    {
                      "message": error.reason,
                      "path": error.path === Path.empty ? None : Some(error.path->Path.toArray),
                    },
                  ],
                }->Obj.magic
              }
            }
          },
        }
      }
    )->X.Function.toExpression,
  },
)

// =============
// Builder functions
// =============

let parser = (~to as schema) => {
  getDecoder2(~s1=unknown, ~s2=schema->castToInternal)
}

let asyncParser = (~to as schema) => {
  getDecoder2(~s1=unknown, ~s2=schema->castToInternal, ~flag=Flag.async)
}

let decoder = (type from to, ~from: t<from>, ~to: t<to>): (from => to) => {
  getDecoder2(~s1=from->castToInternal->reverse, ~s2=to->castToInternal)
}

let asyncDecoder = (type from to, ~from: t<from>, ~to: t<to>): (from => promise<to>) => {
  getDecoder2(~s1=from->castToInternal->reverse, ~s2=to->castToInternal, ~flag=Flag.async)
}

let decoder1 = (type value, schema: t<value>): (unknown => value) => {
  getDecoder(~s1=schema->castToInternal)
}

let asyncDecoder1 = (type value, schema: t<value>): (unknown => promise<value>) => {
  getDecoder(~s1=schema->castToInternal, ~flag=Flag.async)
}

// =============
// Operations
// =============

let getAssertResult = () =>
  cached("a", undefinedTag, s => {
    s.const = %raw(`void 0`)
    s.decoder = literalDecoder
    s.noValidation = Some(true)
  })

@inline
let parseOrThrow = (any, ~to as schema) => {
  getDecoder2(~s1=unknown, ~s2=schema->castToInternal)(any)
}

let parseAsyncOrThrow = (any, ~to as schema) => {
  getDecoder2(~s1=unknown, ~s2=schema->castToInternal, ~flag=Flag.async)(any)
}

let assertOrThrow = (any, ~to as schema) => {
  getDecoder3(~s1=unknown, ~s2=schema->castToInternal, ~s3=getAssertResult())(any)
}

let assertAsyncOrThrow = (any, ~to as schema) => {
  getDecoder3(~s1=unknown, ~s2=schema->castToInternal, ~s3=getAssertResult(), ~flag=Flag.async)(any)
}

let decodeOrThrow = (any, ~from, ~to) => {
  getDecoder2(~s1=from->castToInternal->reverse, ~s2=to->castToInternal)(any)
}

let decodeAsyncOrThrow = (any, ~from, ~to) => {
  getDecoder2(~s1=from->castToInternal->reverse, ~s2=to->castToInternal, ~flag=Flag.async)(any)
}

let isAsync = schema => {
  let schema = schema->castToInternal
  switch schema.isAsync {
  | None => schema->isAsyncInternal(~defs=%raw(`0`))
  | Some(v) => v
  }
}

let wrapExnToFailure = exn => {
  if %raw("exn&&exn.s===s") {
    Failure({error: exn->(Obj.magic: exn => error)})
  } else {
    throw(exn)
  }
}

let js_safe = fn => {
  try {
    Success({
      value: fn(),
    })
  } catch {
  | _ => wrapExnToFailure(%raw(`exn`))
  }
}

let js_safeAsync = fn => {
  try {
    fn()->X.Promise.thenResolveWithCatch(value => Success({value: value}), wrapExnToFailure)
  } catch {
  | _ => X.Promise.resolve(wrapExnToFailure(%raw(`exn`)))
  }
}

module Metadata = {
  module Id: {
    type t<'metadata>
    let make: (~namespace: string, ~name: string) => t<'metadata>
    let internal: string => t<'metadata>
    external toKey: t<'metadata> => string = "%identity"
  } = {
    type t<'metadata> = string

    let make = (~namespace, ~name) => {
      `m:${namespace}:${name}`
    }

    let internal = name => {
      `m:${name}`
    }

    external toKey: t<'metadata> => string = "%identity"
  }

  let get = (schema, ~id: Id.t<'metadata>) => {
    schema->(Obj.magic: t<'a> => dict<option<'metadata>>)->Dict.getUnsafe(id->Id.toKey)
  }

  @inline
  let setInPlace = (schema, ~id: Id.t<'metadata>, metadata: 'metadata) => {
    schema->(Obj.magic: internal => dict<'metadata>)->Dict.set(id->Id.toKey, metadata)
  }

  let set = (schema, ~id: Id.t<'metadata>, metadata: 'metadata) => {
    let schema = schema->castToInternal
    let mut = schema->copySchema
    mut->setInPlace(~id, metadata)
    mut->castToPublic
  }
}

let defsPath = `#/$defs/`
let recursive = (name, fn) => {
  let ref = `${defsPath}${name}`
  let refSchema = base(refTag, ~selfReverse=false)
  refSchema.ref = Some(ref)
  refSchema.name = Some(name)
  refSchema.decoder = recursiveDecoder

  // This is for mutual recursion
  let isNestedRec = globalConfig.defsAccumulator->Obj.magic
  if !isNestedRec {
    globalConfig.defsAccumulator = Some(dict{})
  }
  let def = fn(refSchema->castToPublic)->castToInternal
  if def.name->Obj.magic {
    refSchema.name = def.name
  }
  globalConfig.defsAccumulator
  ->X.Option.getUnsafe
  ->Dict.set(name, def)

  if isNestedRec {
    refSchema->castToPublic
  } else {
    let schema = base(refTag, ~selfReverse=false)
    schema.name = refSchema.name
    schema.ref = Some(ref)
    schema.defs = globalConfig.defsAccumulator
    schema.decoder = recursiveDecoder

    globalConfig.defsAccumulator = None

    schema->castToPublic
  }
}

let noValidation = (schema, value) => {
  let schema = schema->castToInternal
  let mut = schema->copySchema

  // TODO: Test for discriminant literal
  // TODO: Better test reverse
  mut.noValidation = Some(value)
  mut->castToPublic
}

let internalRefine = (schema, makeRefiner) => {
  let schema = schema->castToInternal
  updateOutput(schema, mut => {
    let refiner = makeRefiner(mut)
    switch mut.refiner {
    | Some(existingRefiner) =>
      mut.refiner = Some(
        (~input) => {
          let arr = existingRefiner(~input)
          let next = refiner(~input)
          for i in 0 to next->Array.length - 1 {
            arr->Array.push(next->Array.getUnsafe(i))->ignore
          }
          arr
        },
      )
    | None => mut.refiner = Some(refiner)
    }
  })
}

let refine: (t<'value>, 'value => bool, ~error: string=?, ~path: array<string>=?) => t<'value> = (
  schema,
  refineCheck,
  ~error=?,
  ~path=?,
) => {
  let message = switch error {
  | Some(e) => e
  | None => "Refinement failed"
  }
  let extraPath = switch path {
  | Some(p) => Path.fromArray(p)
  | None => Path.empty
  }
  schema->internalRefine(_ =>
    (~input) => {
      let embeddedCheck = input->B.embed(refineCheck)
      [
        {
          cond: (~inputVar) => `${embeddedCheck}(${inputVar})`,
          fail: B.invalidInputBuilder(~extraPath, ~reasonOverride=message),
        },
      ]
    }
  )
}

let getMutErrorMessage = (~mut: internal): dict<string> => {
  let em: dict<string> =
    mut.errorMessage->X.Option.unsafeToBool
      ? mut.errorMessage
        ->X.Option.getUnsafe
        ->(Obj.magic: schemaErrorMessage => dict<string>)
        ->X.Dict.copy
      : dict{}
  mut.errorMessage = Some(em->Obj.magic)
  em
}

type transformDefinition<'input, 'output> = {
  @as("p")
  parser?: 'input => 'output,
  @as("a")
  asyncParser?: 'input => promise<'output>,
  @as("s")
  serializer?: 'output => 'input,
}
let transform: (t<'input>, s<'output> => transformDefinition<'input, 'output>) => t<'output> = (
  schema,
  transformer,
) => {
  let schema = schema->castToInternal
  updateOutput(schema, mut => {
    mut.parser = Some(
      Builder.make((~input) => {
        switch transformer(input->B.effectCtx) {
        | {parser, asyncParser: ?None} => B.embedTransformation(~input, ~fn=parser, ~isAsync=false)
        | {parser: ?None, asyncParser} =>
          B.embedTransformation(~input, ~fn=asyncParser, ~isAsync=true)
        | {parser: ?None, asyncParser: ?None, serializer: ?None} =>
          input->B.refine(~expected=input.expected.to->Option.getUnsafe)
        | {parser: ?None, asyncParser: ?None, serializer: _} =>
          input->B.invalidOperation(~description=`The S.transform parser is missing`)
        | {parser: _, asyncParser: _} =>
          input->B.invalidOperation(
            ~description=`The S.transform doesn't allow parser and asyncParser at the same time. Remove parser in favor of asyncParser`,
          )
        }
      }),
    )
    mut.to = Some({
      let to = unknown->copySchema
      to.serializer = Some(
        (~input) => {
          switch transformer(input->B.effectCtx) {
          | {serializer} => B.embedTransformation(~input, ~fn=serializer, ~isAsync=false)
          | {parser: ?None, asyncParser: ?None, serializer: ?None} =>
            input->B.refine(~expected=input.expected.to->Option.getUnsafe)
          | {serializer: ?None, asyncParser: ?Some(_)}
          | {serializer: ?None, parser: ?Some(_)} =>
            input->B.invalidOperation(~description=`The S.transform serializer is missing`)
          }
        },
      )
      to
    })
    let _ = %raw(`delete mut.isAsync`)
  })
}

let nullAsUnit = () => {
  let s = nullLiteral()->copySchema
  s.to = Some(unit())
  s
}

let rec neverBuilderFn = (~input) => {
  let output = input->B.refine(~expected=never_())
  output.codeFromPrev = B.embedInvalidInput(~input) ++ ";"
  output
}
and never_ = () =>
  cached((neverTag :> string), neverTag, s => {
    s.decoder = Builder.make(neverBuilderFn)
  })

let nestedLoc = "BS_PRIVATE_NESTED_SOME_NONE"

module DictSchema = {
  let factory = item => {
    let item = item->castToInternal
    let mut = base(
      objectTag,
      ~selfReverse=item->Obj.magic->Stdlib.Dict.getUnsafe(reversedKey) === item->Obj.magic,
    )
    mut.properties = Some(X.Object.immutableEmpty)
    mut.additionalItems = Some(Schema(item->castToPublic))
    mut.decoder = objectDecoder
    mut->castToPublic
  }
}

module Union = {
  @unboxed
  type itemCode = Single(string) | Multiple(array<string>)

  let toKey = (schema: internal): string =>
    schema.tag->TagFlag.get->Flag.unsafeHas(TagFlag.instance)
      ? (schema.class->Obj.magic)["name"]
      : (schema.tag :> string)

  let isPriority = (tagFlag, byKey: dict<array<unknown>>) => {
    (tagFlag->Flag.unsafeHas(TagFlag.array->Flag.with(TagFlag.instance)) &&
      byKey->Stdlib.Dict.has((objectTag: tag :> string))) ||
      (tagFlag->Flag.unsafeHas(TagFlag.nan) && byKey->Stdlib.Dict.has((numberTag: tag :> string)))
  }

  let isWiderUnionSchema = (~schemaAnyOf, ~inputAnyOf) => {
    inputAnyOf->Array.everyWithIndex((inputSchema, idx) => {
      switch schemaAnyOf->X.Array.getUnsafeOption(idx) {
      | Some(schema) =>
        !(
          inputSchema.tag
          ->TagFlag.get
          ->Flag.unsafeHas(
            TagFlag.array
            ->Flag.with(TagFlag.instance)
            ->Flag.with(TagFlag.ref)
            ->Flag.with(TagFlag.union)
            ->Flag.with(TagFlag.array)
            ->Flag.with(TagFlag.object),
          )
        ) &&
        inputSchema.tag === schema.tag &&
        inputSchema.const === schema.const &&
        inputSchema.to === None
      | None => false
      }
    })
  }

  let rec unionDecoder: Builder.t = (~input) => {
    let selfSchema = input.expected
    let schemas = selfSchema.anyOf->X.Option.getUnsafe
    let initialInputTagFlag = input.schema.tag->TagFlag.get

    let toPerCase = switch selfSchema {
    | {parser: ?None, to} => Some(to)
    | _ => None
    }

    if (
      (initialInputTagFlag->Flag.unsafeHas(TagFlag.union) &&
      isWiderUnionSchema(
        ~schemaAnyOf=schemas,
        ~inputAnyOf=input.schema.anyOf->X.Option.getUnsafe,
      ) &&
      toPerCase === None) || (input.isOutput->Option.getUnsafe && input.expected === input.schema)
    ) {
      input
    } else {
      if (
        input.schema.encoder === None &&
          initialInputTagFlag->Flag.unsafeHas(TagFlag.union->Flag.with(TagFlag.ref))
      ) {
        input.schema = unknown
      }

      let activeKey = ref("")
      if (
        !(
          initialInputTagFlag->Flag.unsafeHas(
            TagFlag.union->Flag.with(TagFlag.ref)->Flag.with(TagFlag.unknown),
          )
        )
      ) {
        let sourceKey = toKey(input.schema)
        let hasNull = ref(false)
        let hasUndefined = ref(false)
        let len = schemas->Array.length
        let i = ref(0)
        while activeKey.contents === "" && i.contents < len {
          let s = schemas->Array.getUnsafe(i.contents)
          if toKey(s) === sourceKey {
            activeKey := sourceKey
          } else if s.tag === nullTag {
            hasNull := true
          } else if s.tag === undefinedTag {
            hasUndefined := true
          }
          i := i.contents + 1
        }
        if activeKey.contents === "" {
          if initialInputTagFlag->Flag.unsafeHas(TagFlag.undefined) && hasNull.contents {
            activeKey := (nullTag :> string)
          } else if initialInputTagFlag->Flag.unsafeHas(TagFlag.null) && hasUndefined.contents {
            activeKey := (undefinedTag :> string)
          }
        }
      }
      let activeKey = activeKey.contents

      let initialInline = input.inline

      let fail = caught => {
        `${input->B.embed(
            (
              _ => {
                let args = %raw(`arguments`)
                B.throw(
                  B.makeInvalidInputDetails(
                    ~path=input.path,
                    ~expected=selfSchema,
                    ~received=unknown->castToPublic,
                    ~input=args->Array.getUnsafe(0),
                    ~includeInput=true,
                    ~unionErrors=?args->Array.length > 1
                      ? Some(args->X.Array.fromArguments->Array.slice(~start=1))
                      : None,
                  ),
                )
              }
            )->X.Function.toExpression,
          )}(${input.var()}${caught})`
      }

      // Create a copy of the input val, so we can mutate it
      // It's still the same value though, until mutated
      let output = input->B.refine
      let outputAnyOf = []

      let getArrItemsCode = (arr: array<unknown>, ~isDeopt) => {
        let typeValidationInput = arr->Array.getUnsafe(0)->(Obj.magic: unknown => val)
        let typeValidationOutput = arr->Array.getUnsafe(1)->(Obj.magic: unknown => val)

        let itemStart = ref("")
        let itemEnd = ref("")
        let itemNextElse = ref(false)
        let itemNoop = ref("")
        let caught = ref("")

        // Accumulate schemas code by refinement (discriminant)
        // so if we have two schemas with the same discriminant
        // We can generate a single switch statement
        // with try/catch blocks for each item
        // If we come across an item without a discriminant
        // we need to dump all accumulated schemas in try block
        // and have the item without discriminant as catch all
        // If we come across an item without a discriminant
        // and without any code, it means that this item is always valid
        // and we should exit early
        let byDiscriminant = ref(dict{})

        let preItems = 2
        let itemIdx = ref(preItems)
        let lastIdx = arr->Array.length - 1
        while itemIdx.contents <= lastIdx {
          // Copy it one more time, since every case decoder
          // might mutate the input
          let input = typeValidationOutput->B.Val.scope
          input.isUnion = Some(true)
          input.hasTransform = typeValidationOutput.hasTransform
          input.isOutput = Some(false)
          input.expected =
            arr->Stdlib.Array.getUnsafe(itemIdx.contents)->(Obj.magic: unknown => internal)

          let isLast = itemIdx.contents === lastIdx
          let isFirst = itemIdx.contents === preItems
          let withExhaustiveCheck = ref(!(isFirst && isLast))

          let itemCode = ref("")
          let itemCond = ref("")
          try {
            let itemOutput = input->parse
            outputAnyOf->Array.push(itemOutput.schema->castToPublic)->ignore

            itemCode := itemOutput->B.merge(~hoistCond=itemCond)

            if itemOutput.hasTransform->X.Option.getUnsafe {
              output.hasTransform = Some(true)
              if itemOutput.flag->Flag.unsafeHas(ValFlag.async) {
                output.flag = output.flag->Flag.with(ValFlag.async)
              }
              itemCode :=
                itemCode.contents ++
                // Need to allocate a var here, so we don't mutate the input object field
                `${typeValidationInput.var()}=${itemOutput.inline}`
            }
          } catch {
          | _ => {
              let errorVar = input->B.embed(%raw(`exn`)->InternalError.getOrRethrow)
              if isLast {
                // FIXME:
                withExhaustiveCheck := false
              }
              itemCode := (
                  isLast && !isDeopt
                    ? {
                        withExhaustiveCheck := false
                        fail(`,${errorVar}`)
                      }
                    : "throw " ++ errorVar
                )
            }
          }
          let itemCond = itemCond.contents
          let itemCode = itemCode.contents

          // Accumulate item parser when it has a discriminant
          if itemCond->X.String.unsafeToBool {
            if itemCode->X.String.unsafeToBool {
              switch byDiscriminant.contents->X.Dict.getUnsafeOption(itemCond) {
              | Some(Multiple(arr)) => arr->Array.push(itemCode)->ignore
              | Some(Single(code)) =>
                byDiscriminant.contents->Dict.set(itemCond, Multiple([code, itemCode]))
              | None => byDiscriminant.contents->Dict.set(itemCond, Single(itemCode))
              }
            } else {
              // We have a condition but without additional parsing logic
              // So we accumulate it in case it's needed for a refinement later
              itemNoop := (
                  itemNoop.contents->X.String.unsafeToBool
                    ? `${itemNoop.contents}||${itemCond}`
                    : itemCond
                )
            }
          }

          // Allocate all accumulated discriminants
          // If we have an item without a discriminant
          // and need to deopt. Or we are at the last item
          if itemCond->X.String.unsafeToBool->not || isLast {
            let accedDiscriminants = byDiscriminant.contents->Dict.keysToArray
            for idx in 0 to accedDiscriminants->Array.length - 1 {
              let discrim = accedDiscriminants->Array.getUnsafe(idx)
              let if_ = itemNextElse.contents ? "else if" : "if"
              itemStart := itemStart.contents ++ if_ ++ `(${discrim}){`
              switch byDiscriminant.contents->Dict.getUnsafe(discrim) {
              | Single(code) => itemStart := itemStart.contents ++ code ++ "}"
              | Multiple(arr) =>
                let caught = ref("")
                for idx in 0 to arr->Array.length - 1 {
                  let code = arr->Array.getUnsafe(idx)
                  let errorVar = `e` ++ idx->X.Int.unsafeToString
                  itemStart := itemStart.contents ++ `try{${code}}catch(${errorVar}){`
                  caught := `${caught.contents},${errorVar}`
                }
                itemStart :=
                  itemStart.contents ++
                  fail(caught.contents) ++
                  String.repeat("}", arr->Array.length) ++ "}"
              }
              itemNextElse := true
            }
            byDiscriminant.contents = dict{}
          }

          if itemCond->X.String.unsafeToBool->not {
            if itemCode->X.String.unsafeToBool->not {
              // If we don't have a condition (discriminant)
              // and additional parsing logic,
              // it means that this item is always passes
              // so we can remove preceding accumulated refinements
              // and exit early even if there are other items
              itemNoop := ""
              itemIdx := lastIdx
              withExhaustiveCheck := false
            } else {
              // The item without refinement should switch to deopt mode
              // Since there might be validation in the body
              if itemNoop.contents->X.String.unsafeToBool {
                let if_ = itemNextElse.contents ? "else if" : "if"
                itemStart := itemStart.contents ++ if_ ++ `(!(${itemNoop.contents})){`
                itemEnd := "}" ++ itemEnd.contents
                itemNoop := ""
                itemNextElse := false
              }
              if isLast && (isDeopt || !withExhaustiveCheck.contents || isFirst) {
                // For the last item don't add try/catch
                itemStart :=
                  itemStart.contents ++ `${itemNextElse.contents ? "else{" : ""}${itemCode}`
                itemEnd := (itemNextElse.contents ? "}" : "") ++ itemEnd.contents
              } else {
                let errorVar = `e` ++ (itemIdx.contents - preItems)->X.Int.unsafeToString
                itemStart :=
                  itemStart.contents ++
                  `${itemNextElse.contents ? "else{" : ""}try{${itemCode}}catch(${errorVar}){`
                itemEnd := (itemNextElse.contents ? "}" : "") ++ "}" ++ itemEnd.contents
                caught := `${caught.contents},${errorVar}`
                itemNextElse := false
              }
            }
          }
          if isLast {
            if itemNoop.contents->X.String.unsafeToBool {
              if itemStart.contents->X.String.unsafeToBool {
                let if_ = itemNextElse.contents ? "else if" : "if"
                itemStart :=
                  itemStart.contents ++ if_ ++ `(!(${itemNoop.contents})){${fail(caught.contents)}}`
              } else {
                typeValidationOutput->B.pushCheck({
                  cond: (~inputVar as _) => `(${itemNoop.contents})`,
                  fail: B.failInvalidType,
                })
              }
            } else if withExhaustiveCheck.contents {
              let errorCode = fail(caught.contents)
              itemStart :=
                itemStart.contents ++ (itemNextElse.contents ? `else{${errorCode}}` : errorCode)
            }
          }

          itemIdx := itemIdx.contents->X.Int.plus(1)
        }

        itemStart.contents ++ itemEnd.contents
      }

      let start = ref("")
      let end = ref("")
      let caught = ref("")
      // If we got a case which always passes,
      // we can exit early
      let exit = ref(false)

      let lastIdx = schemas->Array.length - 1
      let byKey: ref<dict<array<unknown>>> = ref(dict{})
      let keys = ref([])
      let updatedSchemas = []

      // FIXME: minimal fix — applies the union's refiner/inputRefiner per
      // surviving case (previously dropped when the union has `.to`). The
      // emit shape isn't ideal; fold this into the shared refiner pipeline
      // post-release.
      let appendUnionRefiners = {
        let unionRefiner = selfSchema.refiner
        let unionInputRefiner = selfSchema.inputRefiner
        // Call each source refiner at most once so its predicate is embedded
        // in `input.global.embeded` once and every case references the same
        // `e[N]`. `B.embed` is append-only, so a per-case call would duplicate.
        let cachedRefinerChecks = ref(None)
        let cachedInputRefinerChecks = ref(None)
        let attach = (current, source, cache) =>
          switch source {
          | None => current
          | Some(fn) =>
            let getCached = (~input) =>
              switch cache.contents {
              | Some(checks) => checks
              | None =>
                let checks = fn(~input)
                cache := Some(checks)
                checks
              }
            switch current {
            | None => Some(getCached)
            | Some(existing) =>
              Some(
                (~input) => {
                  let arr = existing(~input)
                  let next = getCached(~input)
                  for i in 0 to next->Array.length - 1 {
                    arr->Array.push(next->Array.getUnsafe(i))->ignore
                  }
                  arr
                },
              )
            }
          }
        (mut: internal) => {
          switch attach(mut.refiner, unionRefiner, cachedRefinerChecks) {
          | Some(r) => mut.refiner = Some(r)
          | None => ()
          }
          switch attach(mut.inputRefiner, unionInputRefiner, cachedInputRefinerChecks) {
          | Some(r) => mut.inputRefiner = Some(r)
          | None => ()
          }
        }
      }

      for idx in 0 to lastIdx {
        let schema = switch toPerCase {
        | Some(target) =>
          updateOutput(schemas->Array.getUnsafe(idx), mut => {
            appendUnionRefiners(mut)
            mut.to = Some(target)
          })->castToInternal
        | _ => schemas->Array.getUnsafe(idx)
        }
        updatedSchemas->Array.push(schema)->ignore
        let tag = schema.tag
        let tagFlag = TagFlag.get(tag)
        let key = toKey(schema)

        if activeKey !== "" && activeKey !== key {
          // not in active tier — skip
          ()
        } else if (
          tagFlag->Flag.unsafeHas(TagFlag.undefined) &&
            selfSchema->Obj.magic->Stdlib.Dict.has("fromDefault")
        ) {
          // skip it
          ()
        } else {
          let initialArr = byKey.contents->X.Dict.getUnsafeOption(key)
          switch initialArr {
          | Some(arr) =>
            if (
              tagFlag->Flag.unsafeHas(TagFlag.object) &&
                schema.properties->X.Option.getUnsafe->Stdlib.Dict.has(nestedLoc)
            ) {
              // This is a special case for https://github.com/DZakh/sury/issues/150
              // When nested option goes together with an empty object schema
              // Since we put None case check second, we need to change priority here.
              arr
              ->Stdlib.Array.splice(
                ~start=arr->Stdlib.Array.length - 1,
                ~remove=0,
                ~insert=[schema->(Obj.magic: internal => unknown)],
              )
              ->ignore
            } else if (
              // TODO: Is this check needed?
              // There can only be one valid. Dedupe
              !(
                tagFlag->Flag.unsafeHas(
                  TagFlag.undefined->Flag.with(TagFlag.null)->Flag.with(TagFlag.nan),
                )
              )
            ) {
              arr->Array.push(schema->(Obj.magic: internal => unknown))->ignore
            }
          | None =>
            // Recreate input val for every schema
            // since we will mutate it
            let typeValidationInput = input->B.Val.scope
            typeValidationInput.expected = if tagFlag->Flag.unsafeHas(TagFlag.null) {
              nullLiteral()
            } else if tagFlag->Flag.unsafeHas(TagFlag.undefined) {
              unit()
            } else if tagFlag->Flag.unsafeHas(TagFlag.object) {
              DictSchema.factory(unknown->castToPublic)->castToInternal
            } else if tagFlag->Flag.unsafeHas(TagFlag.array) {
              array(unknown->castToPublic)->castToInternal
            } else if tagFlag->Flag.unsafeHas(TagFlag.instance) {
              instance(schema.class)->castToInternal
            } else if tagFlag->Flag.unsafeHas(TagFlag.nan) {
              nan()
            } else if tagFlag->Flag.unsafeHas(TagFlag.string) {
              string()
            } else if tagFlag->Flag.unsafeHas(TagFlag.number) {
              float()
            } else if tagFlag->Flag.unsafeHas(TagFlag.boolean) {
              bool()
            } else if tagFlag->Flag.unsafeHas(TagFlag.bigint) {
              bigint()
            } else if tagFlag->Flag.unsafeHas(TagFlag.symbol) {
              symbol()
            } else {
              unknown
            }

            let typeValidationOutput = try {
              typeValidationInput->parse
            } catch {
            | _ => {
                // Discard any checks parse managed to push before throwing,
                // so the deopt path doesn't see leftover partial state.
                typeValidationInput.checks = None
                typeValidationInput
              }
            }

            if isPriority(tagFlag, byKey.contents) {
              // Not the fastest way, but it's the simplest way
              // to make sure NaN is checked before number
              // And instance and array checked before object
              keys.contents->Array.unshift(key)->ignore
            } else {
              keys.contents->Array.push(key)->ignore
            }
            byKey.contents->Dict.set(
              key,
              [
                typeValidationInput->(Obj.magic: val => unknown),
                typeValidationOutput->(Obj.magic: val => unknown),
                schema->(Obj.magic: internal => unknown),
              ],
            )

            let shouldDeopt = ref(true)
            let valRef = ref(Some(typeValidationOutput))
            while valRef.contents !== None && shouldDeopt.contents {
              let v = valRef.contents->X.Option.getUnsafe
              valRef := v.prev
              shouldDeopt :=
                !(
                  v.checks->X.Option.unsafeToBool && (
                      v.hasTransform === Some(true)
                        ? (v.prev->X.Option.getUnsafe).hasTransform !== Some(true) &&
                            v.codeFromPrev === ""
                        : true
                    )
                )
            }

            if shouldDeopt.contents {
              for keyIdx in 0 to keys.contents->Stdlib.Array.length - 1 {
                let key = keys.contents->Stdlib.Array.getUnsafe(keyIdx)
                if !exit.contents {
                  let arr = byKey.contents->Stdlib.Dict.getUnsafe(key)
                  let typeValidationOutput =
                    arr->Stdlib.Array.getUnsafe(1)->(Obj.magic: unknown => val)
                  let itemsCode = getArrItemsCode(arr, ~isDeopt=true)
                  let blockCode = typeValidationOutput->B.merge ++ itemsCode

                  if blockCode->X.String.unsafeToBool {
                    let errorVar = `e` ++ (idx + keyIdx)->X.Int.unsafeToString
                    start := start.contents ++ `try{${blockCode}}catch(${errorVar}){`
                    end := "}" ++ end.contents
                    caught := `${caught.contents},${errorVar}`
                  } else {
                    exit := true
                  }
                }
              }

              byKey := dict{}
              keys := []
            }
          }
        }
      }

      let byKey = byKey.contents
      let keys = keys.contents

      if !exit.contents {
        let nextElse = ref(false)
        let noop = ref("")

        for idx in 0 to keys->Array.length - 1 {
          let arr = byKey->Dict.getUnsafe(keys->Array.getUnsafe(idx))
          let typeValidationOutput = arr->Stdlib.Array.getUnsafe(1)->(Obj.magic: unknown => val)
          let firstSchema = arr->Stdlib.Array.getUnsafe(2)->(Obj.magic: unknown => internal)

          let itemsCode = getArrItemsCode(arr, ~isDeopt=false)

          let blockCond = ref("")
          let blockCode = typeValidationOutput->B.merge(~hoistCond=blockCond) ++ itemsCode
          let blockCond = blockCond.contents

          if blockCode->X.String.unsafeToBool || isPriority(firstSchema.tag->TagFlag.get, byKey) {
            let if_ = nextElse.contents ? "else if" : "if"
            start := start.contents ++ if_ ++ `(${blockCond}){${blockCode}}`
            nextElse := true
          } else {
            noop := (
                noop.contents->X.String.unsafeToBool ? `${noop.contents}||${blockCond}` : blockCond
              )
          }
        }

        let errorCode = fail(caught.contents)
        start :=
          start.contents ++ if noop.contents->X.String.unsafeToBool {
            let if_ = nextElse.contents ? "else if" : "if"
            if_ ++ `(!(${noop.contents})){${errorCode}}`
          } else if nextElse.contents {
            `else{${errorCode}}`
          } else {
            errorCode
          }
      }

      output.codeFromPrev = output.codeFromPrev ++ start.contents ++ end.contents

      // In case if input.var was called, but output.var wasn't
      if input.inline !== output.inline {
        output.inline = input.inline
      }

      let o = if output.flag->Flag.unsafeHas(ValFlag.async) {
        output.inline = `Promise.resolve(${output.inline})`
        output.var = B._notVar
        output
      } else if output.var === B._var {
        // TODO: Think how to make it more robust
        // Recreate to not break the logic to determine
        // whether the output is changed

        // Use output.b instead of b because of mergeWithCatch
        // Should refactor mergeWithCatch to make it simpler
        // All of this is a hack to make mergeWithCatch think that there are no changes. eg S.array(S.option(item))
        if (
          input.codeFromPrev === "" &&
          output.codeFromPrev === "" &&
          (output.varsAllocation === `${output.inline}=${initialInline}` || initialInline === "i")
        ) {
          // FIXME: Might not be not needed
          input.varsAllocation = ""
          input.allocate = B.initialAllocate
          input.var = B._notVar
          input.inline = initialInline
          input
        } else {
          output
        }
      } else {
        output
      }

      // Build the output schema from collected case output schemas
      o.schema = if outputAnyOf->Stdlib.Array.length->X.Int.unsafeToBool {
        factory(outputAnyOf)->castToInternal
      } else {
        never_()
      }
      o.expected = switch toPerCase {
      | Some(to) => {
          o.isOutput = Some(true)
          to->getOutputSchema
        }
      | _ => selfSchema
      }

      o
    }
  }
  and factory = schemas => {
    let schemas: array<internal> = schemas->Obj.magic
    // TODO:
    // 1. Fitler out items without parser
    // 2. Remove duplicate schemas
    // 3. Spread Union and JSON if they are not transformed
    // 4. Provide correct `has` value for Union and JSON
    switch schemas {
    | [] => InternalError.panic("S.union requires at least one item")
    | [schema] => schema->castToPublic
    | _ =>
      let has = dict{}
      let anyOf = X.Set.make()

      for idx in 0 to schemas->Array.length - 1 {
        let schema = schemas->Array.getUnsafe(idx)

        // Check if the union is not transformed
        if schema.tag === unionTag && schema.to === None {
          schema.anyOf
          ->X.Option.getUnsafe
          ->Array.forEach(item => {
            anyOf->X.Set.add(item)
          })
          let _ = has->X.Dict.mixin(schema.has->X.Option.getUnsafe)
        } else {
          anyOf->X.Set.add(schema)
          has->setHas(schema.tag)
        }
      }
      let mut = base(unionTag, ~selfReverse=false)
      mut.anyOf = Some(anyOf->X.Set.toArray)
      mut.decoder = unionDecoder
      mut.has = Some(has)
      mut->castToPublic
    }
  }
}

module Option = {
  type default = Value(unknown) | Callback(unit => unknown)

  let nestedOption = {
    let nestedNone = () => {
      let itemSchema = Literal.parse(0)
      // FIXME: dict{}
      let properties = dict{}
      properties->Dict.set(nestedLoc, itemSchema)
      {
        tag: objectTag,
        required: [nestedLoc],
        properties,
        additionalItems: Strip,
        decoder: objectDecoder,
        // TODO: Support this as a default coercion
        serializer: Builder.make((~input) => {
          let nextSchema = input.expected.to->X.Option.getUnsafe
          input->B.nextConst(~schema=nextSchema, ~expected=nextSchema)
          // FIXME: Need to set isOutput?
        }),
      }
    }

    let parser = Builder.make((~input) => {
      let nextSchema = input.expected.to->X.Option.getUnsafe
      input->B.next(
        `{${nestedLoc}:${(
            (input.expected->getOutputSchema).properties
            ->X.Option.getUnsafe
            ->Dict.getUnsafe(nestedLoc)
          ).const->Obj.magic}}`,
        ~schema=nextSchema,
        ~expected=nextSchema,
      )
    })

    item => {
      item
      ->updateOutput(mut => {
        mut.to = Some(nestedNone())
        mut.parser = Some(parser)
      })
      ->castToInternal
    }
  }

  let factory = (item, ~unit=unit()->castToPublic) => {
    let item = item->castToInternal

    switch item->getOutputSchema {
    | {tag: Undefined} => Union.factory([unit->castToUnknown, item->nestedOption->castToPublic])
    | {tag: Union, ?anyOf, ?has} =>
      item->updateOutput(mut => {
        let schemas = anyOf->X.Option.getUnsafe
        let mutHas = has->X.Option.getUnsafe->X.Dict.copy

        let newAnyOf = []
        for idx in 0 to schemas->Stdlib.Array.length - 1 {
          let schema = schemas->Array.getUnsafe(idx)
          newAnyOf
          ->Array.push(
            switch schema->getOutputSchema {
            | {tag: Undefined} => {
                mutHas->Dict.set(((unit->castToInternal).tag: tag :> string), true)
                newAnyOf->Array.push(unit->castToInternal)->ignore
                schema->nestedOption
              }
            | {properties} =>
              switch properties->X.Dict.getUnsafeOption(nestedLoc) {
              | Some(nestedSchema) =>
                schema
                ->updateOutput(mut => {
                  // FIXME: dict{}
                  let properties = dict{}
                  properties->Dict.set(
                    nestedLoc,
                    {
                      ...nestedSchema,
                      const: nestedSchema.const->Obj.magic->X.Int.plus(1)->Obj.magic,
                    },
                  )
                  mut.properties = Some(properties)
                })
                ->castToInternal
              | None => schema
              }
            | _ => schema
            },
          )
          ->ignore
        }

        if newAnyOf->Array.length === schemas->Array.length {
          mutHas->Dict.set(((unit->castToInternal).tag: tag :> string), true)
          newAnyOf->Array.push(unit->castToInternal)->ignore
        }

        mut.anyOf = Some(newAnyOf)
        mut.has = Some(mutHas)
      })
    | _ => Union.factory([item->castToPublic, unit->castToUnknown])
    }
  }

  let getWithDefault = (schema: t<option<'value>>, default) => {
    schema
    ->castToInternal
    ->updateOutput(mut => {
      switch mut.anyOf {
      | Some(anyOf) => {
          let outputItems = []
          // FIXME: drop `originalItems` once unionDecoder can reverse member
          // `.to` chains — then mut.default + the serializer can both run
          // through `schema->reverse` directly.
          let originalItems = []

          for idx in 0 to anyOf->Stdlib.Array.length - 1 {
            let schema = anyOf->Stdlib.Array.getUnsafe(idx)
            let outputSchema = schema->getOutputSchema
            switch outputSchema.tag {
            | Undefined => ()
            | _ =>
              outputItems->Array.push(outputSchema)
              originalItems->Array.push(schema)
            }
          }

          let item = switch outputItems {
          | [] => InternalError.panic(`Can't set default for ${mut->castToPublic->toExpression}`)
          | [single] => single
          | multiple => Union.factory(multiple->Obj.magic)->castToInternal
          }
          let originalItem = switch originalItems {
          | [single] => single
          | _ => Union.factory(originalItems->Obj.magic)->castToInternal
          }

          switch default {
          | Value(v) =>
            // Full unknown -> item decode so primitive item types still get type-checked.
            try {
              let _ = getDecoder2(~s1=unknown, ~s2=item)(v)
            } catch {
            | _ =>
              let error = %raw(`exn`)->InternalError.getOrRethrow
              InternalError.panic(
                `Invalid default for ${mut->castToPublic
                  ->toExpression}: ${(error->Obj.magic)["message"]}`,
              )
            }
            // Best-effort input form for JSON Schema metadata.
            // FIXME: running a decoder at schema-creation time isn't a goal —
            // it compiles + executes a fresh decode pipeline per default. Replace
            // with something cheaper (or move to lazy/JSON-Schema-export time)
            // before the official v11 release.
            try mut.default =
              getDecoder(~s1=originalItem->reverse)(v)->(
                Obj.magic: unknown => option<internalDefault>
              ) catch {
            | _ => ()
            }
          | Callback(_) => ()
          }

          mut.parser = Some(
            Builder.make((~input) => {
              let nextSchema = input.expected.to->X.Option.getUnsafe
              let inputVar = input.var()
              input->B.next(
                `${inputVar}===void 0?${switch default {
                  | Value(v) => input->B.inlineConst(Literal.parse(v))
                  | Callback(cb) => `${input->B.embed(cb)}()`
                  }}:${inputVar}`,
                ~schema=nextSchema,
                ~expected=nextSchema,
              )
            }),
          )
          let to = item->copySchema

          let originalDecoder = to.decoder
          to.serializer = Some(
            Builder.make((~input) => {
              let nextSchema = originalItem->reverse
              originalDecoder(~input)->B.refine(~schema=nextSchema, ~expected=nextSchema)
            }),
          )

          // FIXME: This looks wrong, but this is how it was with prev architecture
          to.decoder = noopDecoder

          mut.to = Some(to)
        }
      | None => InternalError.panic(`Can't set default for ${mut->castToPublic->toExpression}`)
      }
    })
  }

  let getOr = (schema, defalutValue) =>
    schema->getWithDefault(Value(defalutValue->castAnyToUnknown))
  let getOrWith = (schema, defalutCb) =>
    schema->getWithDefault(Callback(defalutCb->(Obj.magic: (unit => 'a) => unit => unknown)))
}

module Object = {
  type rec s = {
    @as("f") field: 'value. (string, t<'value>) => 'value,
    fieldOr: 'value. (string, t<'value>, 'value) => 'value,
    tag: 'value. (string, 'value) => unit,
    nested: string => s,
    flatten: 'value. t<'value> => 'value,
  }

  let rec setAdditionalItems = (schema, additionalItems, ~deep) => {
    let schema = schema->castToInternal
    switch schema {
    | {additionalItems: currentAdditionalItems}
      if currentAdditionalItems !== additionalItems &&
        currentAdditionalItems->typeof !== objectTag => {
        let mut = schema->copySchema
        mut.additionalItems = Some(additionalItems)
        if deep {
          switch schema.items {
          | Some(items) => {
              let newItems = []
              for idx in 0 to items->Array.length - 1 {
                let s = items->Array.getUnsafe(idx)
                newItems
                ->Array.push(
                  s->castToPublic->setAdditionalItems(additionalItems, ~deep)->castToInternal,
                )
                ->ignore
              }
              mut.items = Some(newItems)
            }
          | None => ()
          }

          switch schema.properties {
          | Some(properties) => {
              let newProperties = dict{}
              let keys = properties->Dict.keysToArray
              for idx in 0 to keys->Array.length - 1 {
                let key = keys->Array.getUnsafe(idx)
                newProperties->Dict.set(
                  key,
                  properties
                  ->Dict.getUnsafe(key)
                  ->castToPublic
                  ->setAdditionalItems(additionalItems, ~deep)
                  ->castToInternal,
                )
              }
              mut.properties = Some(newProperties)
            }
          | None => ()
          }
        }
        mut->castToPublic
      }
    | _ => schema->castToPublic
    }
  }
}

let strip = schema => {
  schema->Object.setAdditionalItems(Strip, ~deep=false)
}

let deepStrip = schema => {
  schema->Object.setAdditionalItems(Strip, ~deep=true)
}

let strict = schema => {
  schema->Object.setAdditionalItems(Strict, ~deep=false)
}

let deepStrict = schema => {
  schema->Object.setAdditionalItems(Strict, ~deep=true)
}

module Tuple = {
  type s = {
    item: 'value. (int, t<'value>) => 'value,
    tag: 'value. (int, 'value) => unit,
  }
}

let rec jsonEncoderFn = (~input, ~target) => {
  let toTagFlag = target.tag->TagFlag.get

  if (
    toTagFlag->Flag.unsafeHas(
      TagFlag.string
      ->Flag.with(TagFlag.boolean)
      ->Flag.with(TagFlag.number)
      ->Flag.with(TagFlag.null),
    )
  ) {
    input->B.refine(~schema=unknown, ~expected=target)->parse
  } else if toTagFlag->Flag.unsafeHas(TagFlag.undefined->Flag.with(TagFlag.nan)) {
    let jsonExpected = nullLiteral()->copySchema
    jsonExpected.to = Some(target)
    input->B.refine(~schema=unknown, ~expected=jsonExpected)->parse
  } else if toTagFlag->Flag.unsafeHas(TagFlag.array) {
    // Validate that the input is an array
    // and then update the schema to be an array of json instead of array of unknown
    let jsonExpected = array(unknown->castToPublic)->castToInternal
    let output = input->B.refine(~schema=unknown, ~expected=jsonExpected)->parse
    output.schema.additionalItems = Some(Schema(json()->castToPublic))
    output.expected = target
    output.isOutput = Some(false)
    output
  } else if toTagFlag->Flag.unsafeHas(TagFlag.object) {
    // Validate that the input is an object
    // and then update the schema to be an object of json instead of object of unknown
    let jsonExpected = DictSchema.factory(unknown->castToPublic)->castToInternal
    let output = input->B.refine(~schema=unknown, ~expected=jsonExpected)->parse
    output.schema.additionalItems = Some(Schema(json()->castToPublic))
    output.expected = target
    output.isOutput = Some(false)
    output
  } else if toTagFlag->Flag.unsafeHas(TagFlag.union->Flag.with(TagFlag.ref)) {
    input
  } else {
    // For non-JSON types (bigint, instance, etc.), decode through string
    let jsonExpected = string()->copySchema
    jsonExpected.to = Some(target)
    input->B.refine(~schema=unknown, ~expected=jsonExpected)->parse
  }
}

and isJsonable = schema => {
  let tagFlag = schema.tag->TagFlag.get
  tagFlag->Flag.unsafeHas(
    TagFlag.string
    ->Flag.with(TagFlag.number)
    ->Flag.with(TagFlag.boolean)
    ->Flag.with(TagFlag.null),
  ) ||
  schema.ref === json().ref ||
  tagFlag->Flag.unsafeHas(TagFlag.union) &&
    schema.anyOf->X.Option.getUnsafe->Array.every(isJsonable) ||
  tagFlag->Flag.unsafeHas(TagFlag.array) &&
  switch schema.additionalItems->X.Option.getUnsafe {
  | Schema(s) => s->castToInternal->isJsonable
  | _ => true
  } &&
  schema.items->X.Option.getUnsafe->Array.every(isJsonable) ||
  (tagFlag->Flag.unsafeHas(TagFlag.object) &&
  switch schema.additionalItems->X.Option.getUnsafe {
  | Schema(s) => s->castToInternal->isJsonable
  | _ => true
  } &&
  schema.properties->X.Option.getUnsafe->Stdlib.Dict.valuesToArray->Array.every(isJsonable))
}

and jsonDecoderFn = (~input) => {
  let inputTagFlag = input.schema.tag->TagFlag.get

  if input.schema->isJsonable {
    input
  } else if inputTagFlag->Flag.unsafeHas(TagFlag.undefined->Flag.with(TagFlag.nan)) {
    input->B.nextConst(~schema=nullLiteral())
  } else if inputTagFlag->Flag.unsafeHas(TagFlag.array) {
    let expected = base(arrayTag, ~selfReverse=false)
    expected.items = Some(
      input.schema.items
      ->X.Option.getUnsafe
      ->Array.map(_ => json()),
    )
    expected.decoder = arrayDecoder
    expected.additionalItems = Some(
      switch input.schema.additionalItems->X.Option.getUnsafe {
      | Schema(_) => Schema(json()->castToPublic)
      | v => v
      },
    )
    expected.to = input.expected.to
    input->B.refine(~expected)->parse
  } else if inputTagFlag->Flag.unsafeHas(TagFlag.object) {
    switch input.schema.additionalItems->X.Option.getUnsafe {
    | Schema(_) => {
        let expected = DictSchema.factory(json()->castToPublic)->castToInternal
        expected.to = input.expected.to
        input->B.refine(~expected)->parse
      }
    | _ => {
        let jsonVal = makeObjectVal(input, ~schema=input.schema)
        jsonVal.expected = json()
        if input.expected.to->Obj.magic {
          jsonVal.expected = jsonVal.expected->copySchema
          jsonVal.expected.to = input.expected.to
        }

        let keys = input.schema.properties->X.Option.getUnsafe->Dict.keysToArray
        for idx in 0 to keys->Array.length - 1 {
          let key = keys->Array.getUnsafe(idx)
          let itemVal = input->B.Val.get(key)
          itemVal.isOutput = Some(false)

          if (
            itemVal.schema.tag === unionTag &&
              itemVal.schema.has->X.Option.getUnsafe->Dict.getUnsafe((undefinedTag :> string))
          ) {
            itemVal.expected =
              Union.factory([unit()->castToPublic, json()->castToPublic])->castToInternal
            let itemOutput = itemVal->parse
            itemOutput.optional = Some(true)
            jsonVal->B.Val.Object.add(~location=key, itemOutput)
          } else {
            itemVal.expected = json()
            jsonVal->B.Val.Object.add(~location=key, itemVal->parse)
          }
        }

        jsonVal->completeObjectVal
      }
    }
  } else if inputTagFlag->Flag.unsafeHas(TagFlag.ref) {
    // FIXME: Should be a unified solution for ref inputs
    recursiveDecoder(~input)
  } else if inputTagFlag->Flag.unsafeHas(TagFlag.unknown) {
    let to = input.expected.to->X.Option.getUnsafe
    // Whether we can optimize encoding during decoding
    let preEncode: bool = to->Obj.magic && !(input.expected.parser->Obj.magic) // && !(selfSchema.refiner->Obj.magic) FIXME:
    if preEncode {
      input.schema = json()
      jsonEncoderFn(~input, ~target=input.expected)
    } else if input.expected.noValidation->X.Option.getUnsafe {
      input.schema = json()
      input
    } else {
      recursiveDecoder(~input)
    }
  } else {
    try {
      let expected = string()->copySchema
      expected.to = Some(input.expected)
      input.expected = expected
      input->parse
    } catch {
    | _ => input->B.unsupportedDecode(~from=input.schema, ~target=json())
    }
  }
}

and json = () =>
  cached(jsonName, refTag, s => {
    let jsonRef = base(refTag, ~selfReverse=true)
    jsonRef.ref = Some(`${defsPath}${jsonName}`)
    jsonRef.name = Some(jsonName)

    jsonRef.decoder = jsonDecoderFn
    let jsonEncoder = Builder.encoder(jsonEncoderFn)
    jsonRef.encoder = Some(jsonEncoder)

    s.ref = jsonRef.ref
    s.name = Some(jsonName)
    s.decoder = jsonDecoderFn
    s.encoder = Some(jsonEncoder)

    let anyOf = [
      string(),
      bool(),
      float(),
      nullLiteral(),
      DictSchema.factory(jsonRef->castToPublic)->castToInternal,
      array(jsonRef->castToPublic)->castToInternal,
    ]
    let has = dict{}
    anyOf->Array.forEach(schema => {
      has->Dict.set((schema.tag :> string), true)
    })

    let jsonDef = base(unionTag, ~selfReverse=true)
    jsonDef.anyOf = Some(anyOf)
    jsonDef.has = Some(has)
    jsonDef.decoder = Union.unionDecoder
    jsonDef.name = Some(jsonName)
    jsonDef.tag = unionTag

    let defs = dict{}
    defs->Dict.set(jsonName, jsonDef)
    s.defs = Some(defs)
  })

let jsonString = {
  let inlineJsonString = (input, ~schema) => {
    let tagFlag = schema.tag->TagFlag.get
    let const = schema.const
    if tagFlag->Flag.unsafeHas(TagFlag.undefined->Flag.with(TagFlag.null)) {
      `"null"`
    } else if tagFlag->Flag.unsafeHas(TagFlag.string) {
      const->Obj.magic->X.Inlined.Value.fromString->JSON.stringifyAny->Obj.magic
    } else if tagFlag->Flag.unsafeHas(TagFlag.bigint) {
      `"\\"${const->Obj.magic}\\""`
    } else if tagFlag->Flag.unsafeHas(TagFlag.number->Flag.with(TagFlag.boolean)) {
      `"${const->Obj.magic}"`
    } else {
      input->B.unsupportedDecode(~from=schema, ~target=input.expected)
    }
  }

  let constSchemaToJsonStringConst = (input, ~target) => {
    let tagFlag = target.tag->TagFlag.get
    let const = target.const
    if tagFlag->Flag.unsafeHas(TagFlag.undefined->Flag.with(TagFlag.null)) {
      `null`
    } else if tagFlag->Flag.unsafeHas(TagFlag.string) {
      const->Obj.magic->X.Inlined.Value.fromString->Obj.magic
    } else if tagFlag->Flag.unsafeHas(TagFlag.bigint) {
      `"${const->Obj.magic}"`
    } else if tagFlag->Flag.unsafeHas(TagFlag.number->Flag.with(TagFlag.boolean)) {
      %raw(`""+$$const`)
    } else {
      input->B.unsupportedDecode(~from=input.schema, ~target)
    }
  }

  let jsonStringEncoder = Builder.encoder((~input, ~target) => {
    if target.format !== Some(JSON) {
      if target->isLiteral {
        let jsonStringConstSchema = base(stringTag, ~selfReverse=true)
        jsonStringConstSchema.const = input->constSchemaToJsonStringConst(~target)->Obj.magic
        jsonStringConstSchema.to = Some(target)
        jsonStringConstSchema.decoder = literalDecoder
        input->B.refine(~expected=jsonStringConstSchema)
      } else {
        let outputVar = input.global->B.varWithoutAllocation
        input.allocate(outputVar)

        let nextSchema = json()->copySchema
        nextSchema.to = Some(target)

        let output = input->B.next(outputVar, ~schema=nextSchema, ~expected=nextSchema)
        output.isOutput = Some(true)
        output.var = B._var

        let inputVar = input.var()
        output.codeFromPrev = `try{${outputVar}=JSON.parse(${inputVar})}catch(t){${B.embedInvalidInput(
            ~input,
            ~expected=input.schema,
          )}}`

        output
      }
    } else {
      input
    }
  })

  let jsonStringDecoder = Builder.make((~input) => {
    let inputTagFlag = input.schema.tag->TagFlag.get
    let expectedSchema = input.expected

    if inputTagFlag->Flag.unsafeHas(TagFlag.unknown) {
      let to = expectedSchema.to->X.Option.getUnsafe
      // Whether we can optimize encoding during decoding
      let preEncode: bool =
        to->Obj.magic &&
        to.tag !== unknownTag &&
        !(expectedSchema.parser->Obj.magic) &&
        !(expectedSchema.refiner->Obj.magic)

      let stringVal = stringDecoderFn(~input)
      stringVal.schema = expectedSchema
      stringVal.expected = expectedSchema

      if preEncode {
        jsonStringEncoder(~input=stringVal, ~target=to)
      } else {
        let stringVar = stringVal.var()
        let output = stringVal->B.refine(~schema=expectedSchema)
        output.codeFromPrev = `try{JSON.parse(${stringVar})}catch(t){${B.embedInvalidInput(
            ~input=stringVal,
          )}}`
        output
      }
    } else if input.schema.format === Some(JSON) {
      input
    } else if input.schema->isLiteral {
      input->B.next(input->inlineJsonString(~schema=input.schema), ~schema=expectedSchema)
    } else if inputTagFlag->Flag.unsafeHas(TagFlag.string) {
      input->B.next(`JSON.stringify(${input.inline})`, ~schema=expectedSchema)
    } else if inputTagFlag->Flag.unsafeHas(TagFlag.number->Flag.with(TagFlag.boolean)) {
      let output = input->inputToString
      output.schema = expectedSchema
      output
    } else if inputTagFlag->Flag.unsafeHas(TagFlag.bigint) {
      input->B.next(`"\\""+${input.inline}+"\\""`, ~schema=expectedSchema)
    } else if inputTagFlag->Flag.unsafeHas(TagFlag.object->Flag.with(TagFlag.array)) {
      let jsonVal = input->B.refine(~expected=json())->parse
      jsonVal->B.next(
        `JSON.stringify(${jsonVal.inline}${switch expectedSchema.space {
          | Some(0)
          | None => ""
          | Some(v) => `,null,${v->X.Int.unsafeToString}`
          }})`,
        ~schema=expectedSchema,
        ~expected=expectedSchema,
      )
    } else {
      input->B.unsupportedDecode(~from=input.schema, ~target=expectedSchema)
    }
  })

  () =>
    cached((JSON :> string), stringTag, s => {
      s.format = Some(JSON)
      s.name = Some(`${jsonName} string`)
      s.encoder = Some(jsonStringEncoder)
      s.decoder = jsonStringDecoder
    })
}

let jsonStringWithSpace = (space: int) => {
  let mut = jsonString()->copySchema
  mut.space = Some(space)
  mut->castToPublic
}

let uint8Array = () =>
  cached("u", instanceTag, s => {
    s.class = %raw(`Uint8Array`)
    s.decoder = Builder.make((~input as inputArg) => {
      let inputTagFlag = inputArg.schema.tag->TagFlag.get
      let input = ref(inputArg)

      if inputTagFlag->Flag.unsafeHas(TagFlag.string) {
        input :=
          input.contents->B.next(
            `${input.contents->B.embed(
                %raw(`new TextEncoder()`),
              )}.encode(${input.contents.inline})`,
            ~schema=s,
          )
      } else if inputTagFlag->Flag.unsafeHas(TagFlag.unknown->Flag.with(TagFlag.instance)) {
        input := instanceDecoder(~input=input.contents)
      }

      switch inputArg.expected {
      | {to, parser: ?None} => {
          let toTagFlag = to.tag->TagFlag.get
          if toTagFlag->Flag.unsafeHas(TagFlag.string) {
            input :=
              input.contents->B.next(
                `${input.contents->B.embed(
                    %raw(`new TextDecoder()`),
                  )}.decode(${input.contents.inline})`,
                ~schema=string(),
              )
          }
          input.contents
        }
      | _ => input.contents
      }
    })
  })

let isoDateTime = () => {
  cached((DateTime :> string), stringTag, s => {
    let datetimeRe = /^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(\.\d+)?Z$/
    s.decoder = stringDecoderFn
    s.format = Some(DateTime)
    s.refiner = Some(
      (~input) => {
        [
          {
            cond: (~inputVar) => `${input->B.embed(datetimeRe)}.test(${inputVar})`,
            fail: B.failWithErrorMessage(
              "format",
              ~defaultMessage="Invalid datetime string! Expected UTC",
            ),
          },
        ]
      },
    )
  })
}

let port = () => {
  cached((Port :> string), numberTag, s => {
    s.decoder = numberDecoder
    s.format = Some(Port)
    s.refiner = Some(
      (~input as _) => {
        [
          {
            cond: (~inputVar) => `${inputVar}>0&&${inputVar}<65536&&${inputVar}%1===0`,
            fail: B.failWithErrorMessage("format"),
          },
        ]
      },
    )
  })
}

let email = () => {
  cached((Email :> string), stringTag, s => {
    let emailRegex = /^(?!\.)(?!.*\.\.)([A-Z0-9_'+\-\.]*)[A-Z0-9_+-]@([A-Z0-9][A-Z0-9\-]*\.)+[A-Z]{2,}$/i
    s.decoder = stringDecoderFn
    s.format = Some(Email)
    s.refiner = Some(
      (~input) => {
        [
          {
            cond: (~inputVar) => `${input->B.embed(emailRegex)}.test(${inputVar})`,
            fail: B.failWithErrorMessage("format"),
          },
        ]
      },
    )
  })
}

let uuid = () => {
  cached((Uuid :> string), stringTag, s => {
    let uuidRegex = /^[0-9a-fA-F]{8}\b-[0-9a-fA-F]{4}\b-[0-9a-fA-F]{4}\b-[0-9a-fA-F]{4}\b-[0-9a-fA-F]{12}$/i
    s.decoder = stringDecoderFn
    s.format = Some(Uuid)
    s.refiner = Some(
      (~input) => {
        [
          {
            cond: (~inputVar) => `${input->B.embed(uuidRegex)}.test(${inputVar})`,
            fail: B.failWithErrorMessage("format"),
          },
        ]
      },
    )
  })
}

let cuid = () => {
  cached((Cuid :> string), stringTag, s => {
    let cuidRegex = /^c[^\s-]{8,}$/i
    s.decoder = stringDecoderFn
    s.format = Some(Cuid)
    s.refiner = Some(
      (~input) => {
        [
          {
            cond: (~inputVar) => `${input->B.embed(cuidRegex)}.test(${inputVar})`,
            fail: B.failWithErrorMessage("format"),
          },
        ]
      },
    )
  })
}

let url = () => {
  cached((Url :> string), stringTag, s => {
    let urlValidator: unknown = %raw(`s=>{try{new URL(s);return true}catch(_){return false}}`)
    s.decoder = stringDecoderFn
    s.format = Some(Url)
    s.refiner = Some(
      (~input) => {
        [
          {
            cond: (~inputVar) => `${input->B.embed(urlValidator)}(${inputVar})`,
            fail: B.failWithErrorMessage("format"),
          },
        ]
      },
    )
  })
}

let invalidDateRefine = (input: val) =>
  input->B.refine(
    ~schema=input.expected,
    ~checks=[
      {
        cond: (~inputVar) => `!Number.isNaN(${inputVar}.getTime())`,
        fail: B.failInvalidType,
      },
    ],
  )

let date = () =>
  cached((instanceTag :> string), instanceTag, s => {
    s.class = %raw(`Date`)
    s.decoder = Builder.make((~input) => {
      let inputTagFlag = input.schema.tag->TagFlag.get
      if inputTagFlag->Flag.unsafeHas(TagFlag.string) {
        input
        ->B.next(`new Date(${input.inline})`, ~schema=s->castToPublic->castToInternal)
        ->invalidDateRefine
      } else if inputTagFlag->Flag.unsafeHas(TagFlag.unknown) {
        instanceDecoder(~input)->invalidDateRefine
      } else if inputTagFlag->Flag.unsafeHas(TagFlag.instance) && input.schema.class === s.class {
        input
      } else {
        input->B.unsupportedDecode(~from=input.schema, ~target=input.expected)
      }
    })

    // Encoder: Date → string (via toISOString) when target is string
    s.encoder = Some(
      Builder.encoder((~input, ~target) => {
        let toTagFlag = target.tag->TagFlag.get
        if toTagFlag->Flag.unsafeHas(TagFlag.string) {
          let dateTimeString = base(stringTag, ~selfReverse=false)
          dateTimeString.format = Some(DateTime)
          input
          ->B.next(`${input.inline}.toISOString()`, ~schema=dateTimeString, ~expected=target)
          ->parse
        } else {
          input
        }
      }),
    )
  })

let to = (from, target) => {
  let from = from->castToInternal
  let target = target->castToInternal

  // It makes sense, since S.to quite often will be used
  // inside of a framework where we don't control what's the to argument
  if from === target {
    from->castToPublic
  } else {
    updateOutput(from, mut => {
      mut.to = Some(target)
      // A tricky part about parser is that we don't know the input type in ReScript
      // so we need to directly parse to output instead of input
      // switch parser {
      // | Some(p) =>
      //   mut.parser = Some(
      //     Builder.make((b, ~input, , ~path as _) => {
      //       // TODO: Support async, reverse, nested parsing
      //       b->B.embedSyncOperation(~input, ~fn=p)
      //     }),
      //   )
      // | None => ()
      // }
    })
  }
}

let list = schema => {
  schema
  ->array
  ->transform(_ => {
    parser: array => array->List.fromArray,
    serializer: list => list->List.toArray,
  })
}

// TODO: Better test reverse
let meta = (schema: t<'value>, data: meta<'value>) => {
  let schema = schema->castToInternal
  let mut = schema->copySchema
  switch data.name {
  | Some("") => mut.name = None
  | Some(name) => mut.name = Some(name)
  | None => ()
  }
  switch data.title {
  | Some("") => mut.title = None
  | Some(title) => mut.title = Some(title)
  | None => ()
  }
  switch data.description {
  | Some("") => mut.description = None
  | Some(description) => mut.description = Some(description)
  | None => ()
  }
  switch data.deprecated {
  | Some(deprecated) => mut.deprecated = Some(deprecated)
  | None => ()
  }
  switch data.examples {
  | Some([]) => mut.examples = None // FIXME: Delete instead of None
  | Some(examples) => mut.examples = Some(examples->X.Array.map(getDecoder(~s1=schema->reverse)))
  | None => ()
  }
  switch data.errorMessage {
  | Some(em) =>
    let emDict: dict<string> = em->Obj.magic
    if emDict->Dict.keysToArray->Array.length === 0 {
      mut.errorMessage = None
    } else {
      mut.errorMessage = Some(em)
    }
  | None => ()
  }
  mut->castToPublic
}

let brand = (schema: t<'value>, id: string) => {
  let schema = schema->castToInternal
  let mut = schema->copySchema
  mut.name = Some(id)
  mut->castToPublic
}

module Schema = {
  type rec shapedSerializerAcc = {
    mutable val?: val,
    mutable properties?: dict<shapedSerializerAcc>,
    mutable flattened?: array<shapedSerializerAcc>,
  }

  type s = {@as("m") matches: 'value. t<'value> => 'value}

  let inputFrom = X.Array.immutableEmpty

  type advancedObjectCtx = {
    // Public API for JS/TS users.
    // It shouldn't be used from ReScript and
    // needed only because we use @as for field to reduce bundle-size
    // of ReScript compiled code
    @as("field") _jsField: 'value. (string, schema<'value>) => 'value,
    // Public API for ReScript users
    ...Object.s,
  }

  module Definition = {
    type t<'embeded>

    @inline
    let isNode = (definition: 'any) =>
      definition->typeof === objectTag && definition !== %raw(`null`)

    @inline
    let toEmbededItem = (definition: t<'embeded>): option<'embeded> =>
      definition->Obj.magic->X.Dict.getUnsafeOptionBySymbol(itemSymbol)
  }

  let rec proxifyShapedSchema = (schema: internal, ~from, ~fromFlattened=?): 'a => {
    let mut = schema->getOutputSchema->copySchema
    mut.from = Some(from)
    switch fromFlattened {
    | Some(index) => mut.fromFlattened = Some(index)
    | None => ()
    }
    mut
    ->X.Proxy.make({
      get: (~target, ~prop) => {
        if prop === itemSymbol->Obj.magic {
          target->Obj.magic
        } else {
          let location = prop->(Obj.magic: unknown => string)

          {
            let maybeField = switch target {
            | {properties} => properties->X.Dict.getUnsafeOption(location)
            // If there are no properties, then it must be Tuple
            | {items} => items->X.Array.getUnsafeOptionByString(location)
            | _ => None
            }
            if maybeField === None {
              InternalError.panic(
                `Cannot read property "${location}" of ${target
                  ->castToPublic
                  ->toExpression}`,
              )
            }
            maybeField->X.Option.getUnsafe
          }
          ->proxifyShapedSchema(
            ~from=target.from->X.Option.getUnsafe->X.Array.append(location),
            ~fromFlattened=?target.fromFlattened,
          )
          ->Obj.magic
        }
      },
    })
    ->Obj.magic
  }

  let rec shape = {
    (schema: t<'value>, definer: 'value => 'variant): t<'variant> => {
      let schema = schema->castToInternal
      schema->updateOutput(mut => {
        let fromProxy = mut->proxifyShapedSchema(~from=inputFrom)
        let definition: unknown = definer(fromProxy)->Obj.magic
        if definition === fromProxy {
          ()
        } else {
          mut.parser = Some(shapedParser)
          mut.to = Some(definitionToShapedSchema(definition))
        }
      })
    }
  }
  and nested = fieldName => {
    let parentCtx = %raw(`this`) // TODO: Add a check that it's binded?
    let cacheId = `~${fieldName}`

    switch parentCtx->X.Dict.getUnsafeOption(cacheId) {
    | Some(ctx) => ctx
    | None => {
        let properties = dict{}
        let required = []
        let schema = {
          let schema = base(objectTag, ~selfReverse=false)
          schema.required = Some(required)
          schema.properties = Some(properties)
          schema.additionalItems = Some(globalConfig.defaultAdditionalItems)
          schema.decoder = objectDecoder
          schema->castToPublic
        }

        let parentSchema = (
          parentCtx.field(fieldName, schema)
          ->Definition.toEmbededItem
          ->X.Option.getUnsafe: internal
        )

        let field:
          type value. (string, schema<value>) => value =
          (fieldName, schema) => {
            let schema = schema->castToInternal
            let inlinedLocation = fieldName->X.Inlined.Value.fromString
            if properties->Stdlib.Dict.has(fieldName) {
              InternalError.panic(`The field ${inlinedLocation} defined twice`)
            }
            required->Array.push(fieldName)->ignore
            properties->Dict.set(fieldName, schema)
            schema->proxifyShapedSchema(
              ~from=parentSchema.from->X.Option.getUnsafe->X.Array.append(fieldName),
              ~fromFlattened=?parentSchema.fromFlattened,
            )
          }

        let tag = (tag, asValue) => {
          let _ = field(tag, definitionToSchema(asValue->Obj.magic)->castToPublic)
        }

        let fieldOr = (fieldName, schema, or) => {
          field(fieldName, Option.factory(schema)->Option.getOr(or))
        }

        let flatten = schema => {
          let schema = schema->castToInternal
          switch schema {
          | {tag: Object, properties: ?flattenedProperties, ?to} => {
              if to->Obj.magic {
                InternalError.panic(
                  `Unsupported nested flatten for transformed object schema ${schema
                    ->castToPublic
                    ->toExpression}`,
                )
              }
              let flattenedProperties = flattenedProperties->X.Option.getUnsafe
              let flattenedKeys = flattenedProperties->Dict.keysToArray
              let result = dict{}
              for idx in 0 to flattenedKeys->Array.length - 1 {
                let key = flattenedKeys->Array.getUnsafe(idx)
                result->Dict.set(
                  key,
                  field(key, flattenedProperties->Dict.getUnsafe(key)->castToPublic),
                )
              }
              result->Obj.magic
            }
          | _ => InternalError.panic(`Can't flatten ${schema->castToPublic->toExpression} schema`)
          }
        }

        let ctx: advancedObjectCtx = {
          // js/ts methods
          _jsField: field,
          // methods
          field,
          fieldOr,
          tag,
          nested,
          flatten,
        }

        parentCtx->Dict.set(cacheId, ctx)

        (ctx :> Object.s)
      }
    }
  }
  and object:
    type value. (Object.s => value) => schema<value> =
    definer => {
      let flattened: option<array<internal>> = %raw(`void 0`)
      let properties = dict{}

      let flatten = schema => {
        let schema = schema->castToInternal
        switch schema {
        | {tag: Object, properties: ?flattenedProperties} => {
            let flattenedProperties = flattenedProperties->X.Option.getUnsafe
            let flattenedKeys = flattenedProperties->Dict.keysToArray
            for idx in 0 to flattenedKeys->Array.length - 1 {
              let key = flattenedKeys->Array.getUnsafe(idx)
              let flattenedSchema = flattenedProperties->Dict.getUnsafe(key)
              switch properties->X.Dict.getUnsafeOption(key) {
              | Some(schema) if schema === flattenedSchema => ()
              | Some(_) =>
                InternalError.panic(`The field "${key}" defined twice with incompatible schemas`)
              | None => properties->Dict.set(key, flattenedSchema)
              }
            }
            let f = %raw(`flattened || (flattened = [])`)
            schema->proxifyShapedSchema(
              ~from=inputFrom,
              ~fromFlattened=f->X.Array.pushWithLength(schema) - 1,
            )
          }
        | _ =>
          InternalError.panic(
            `The '${schema->castToPublic->toExpression}' schema can't be flattened`,
          )
        }
      }

      let field:
        type value. (string, schema<value>) => value =
        (fieldName, schema) => {
          let schema = schema->castToInternal

          if properties->Stdlib.Dict.has(fieldName) {
            InternalError.panic(`The field "${fieldName}" defined twice with incompatible schemas`)
          }
          properties->Dict.set(fieldName, schema)
          schema->proxifyShapedSchema(~from=[fieldName])
        }

      let tag = (tag, asValue) => {
        let _ = field(tag, definitionToSchema(asValue->Obj.magic)->castToPublic)
      }

      let fieldOr = (fieldName, schema, or) => {
        field(fieldName, Option.factory(schema)->Option.getOr(or))
      }

      let ctx = {
        // js/ts methods
        _jsField: field,
        // methods
        field,
        fieldOr,
        tag,
        nested,
        flatten,
      }

      let definition = definer((ctx :> Object.s))->(Obj.magic: value => unknown)

      let mut = base(objectTag, ~selfReverse=false)
      mut.required = Some(properties->Dict.keysToArray)
      mut.properties = Some(properties)
      mut.additionalItems = Some(globalConfig.defaultAdditionalItems)
      mut.decoder = objectDecoder
      mut.parser = Some(shapedParser)
      mut.to = Some(definitionToShapedSchema(definition))
      if flattened !== None {
        mut.flattened = flattened
      }
      mut->castToPublic
    }
  and tuple = definer => {
    let items = []

    let ctx: Tuple.s = {
      let item:
        type value. (int, schema<value>) => value =
        (idx, schema) => {
          let schema = schema->castToInternal
          let location = idx->Int.toString
          if items->X.Array.has(idx) {
            InternalError.panic(`The item [${location}] is defined multiple times`)
          } else {
            items->Array.setUnsafe(idx, schema)
            schema->proxifyShapedSchema(~from=[idx->Int.toString])
          }
        }

      let tag = (idx, asValue) => {
        let _ = item(idx, definitionToSchema(asValue->Obj.magic)->castToPublic)
      }

      {
        item,
        tag,
      }
    }
    let definition = definer(ctx)->(Obj.magic: 'any => unknown)

    for idx in 0 to items->Array.length - 1 {
      if items->Array.getUnsafe(idx)->Obj.magic->not {
        items->Array.setUnsafe(idx, unit())
      }
    }

    let mut = base(arrayTag, ~selfReverse=false)
    mut.items = Some(items)
    mut.additionalItems = Some(Strict)
    mut.decoder = arrayDecoder
    mut.parser = Some(shapedParser)
    mut.to = Some(definitionToShapedSchema(definition))
    mut->castToPublic
  }
  and getValByFrom = (~input, ~from, ~idx) => {
    // FIXME: TODO: something with flattened
    switch from->X.Array.getUnsafeOption(idx) {
    | Some(key) =>
      getValByFrom(
        ~input=input.vals->X.Option.getUnsafe->Dict.getUnsafe(key),
        ~from,
        ~idx=idx + 1,
      )
    | None => input
    }
  }
  and getShapedParserOutput = (~input, ~targetSchema) => {
    let v = switch targetSchema {
    | {fromFlattened} =>
      getValByFrom(
        ~input=input.flattenedVals->X.Option.getUnsafe->Array.getUnsafe(fromFlattened),
        ~from=targetSchema.from->X.Option.getUnsafe,
        ~idx=0,
      )->B.Val.scope
    | {from} => getValByFrom(~input, ~from, ~idx=0)->B.Val.scope
    | _ =>
      if targetSchema->isLiteral {
        input->B.nextConst(~schema=targetSchema)
      } else {
        let output = makeObjectVal(input, ~schema=targetSchema)
        output.isOutput = Some(true)
        switch targetSchema {
        | {items} =>
          for idx in 0 to items->Array.length - 1 {
            let location = idx->Int.toString
            output->B.Val.Object.add(
              ~location,
              getShapedParserOutput(~input, ~targetSchema=items->Array.getUnsafe(idx)),
            )
          }
        | {properties} => {
            let keys = properties->Dict.keysToArray
            for idx in 0 to keys->Array.length - 1 {
              let location = keys->Array.getUnsafe(idx)
              output->B.Val.Object.add(
                ~location,
                getShapedParserOutput(
                  ~input,
                  ~targetSchema=properties->Dict.getUnsafe(location),
                ),
              )
            }
          }
        | _ =>
          // FIXME: Use a path
          InternalError.panic(
            `Don't know where the value is coming from: ${targetSchema
              ->castToPublic
              ->toExpression}`,
          )
        }
        output->completeObjectVal
      }
    }
    v.prev = None
    v.expected = targetSchema
    v
  }
  and shapedParser = (~input) => {
    switch input.expected.flattened {
    | Some(flattened) =>
      let flattenedVals = []
      for idx in 0 to flattened->Array.length - 1 {
        let flattenedSchema = flattened->Array.getUnsafe(idx)
        let flattenedInput = input->B.Val.scope
        flattenedInput.expected = flattenedSchema
        flattenedInput.isOutput = Some(false)
        let flattenedVal = flattenedInput->parse
        flattenedVals->Array.push(flattenedVal)->ignore
        input.codeFromPrev = input.codeFromPrev ++ flattenedVal->B.merge
      }
      input.flattenedVals = Some(flattenedVals)
    | None => ()
    }

    let targetSchema = input.expected.to->X.Option.getUnsafe
    let output = getShapedParserOutput(~input, ~targetSchema)
    output.hasTransform = Some(true)
    output.prev = Some(input)
    output->B.markOutput(~valInput=input)
  }

  and prepareShapedSerializerAcc = (~acc: shapedSerializerAcc, ~input: val) => {
    switch input {
    | {expected: {from, ?fromFlattened}} =>
      let accAtFrom = ref(
        switch fromFlattened {
        | Some(idx) => {
            if acc.flattened === None {
              acc.flattened = Some([])
            }
            switch acc.flattened->X.Option.getUnsafe->X.Array.getUnsafeOption(idx) {
            | None => {
                let newAcc: shapedSerializerAcc = {}
                acc.flattened->X.Option.getUnsafe->Array.setUnsafe(idx, newAcc)
                newAcc
              }
            | Some(acc) => acc
            }
          }
        | None => acc
        },
      )
      for idx in 0 to from->Array.length - 1 {
        let key = from->Array.getUnsafe(idx)
        let p = switch accAtFrom.contents.properties {
        | Some(p) => p
        | None => {
            let p = dict{}

            accAtFrom.contents.properties = Some(p)
            p
          }
        }
        accAtFrom :=
          switch p->X.Dict.getUnsafeOption(key) {
          | Some(acc) => acc
          | None => {
              let newAcc: shapedSerializerAcc = {}
              p->Dict.set(key, newAcc)
              newAcc
            }
          }
      }
      accAtFrom.contents.val = Some(input)
    | {vals} => {
        let keys = vals->Dict.keysToArray
        for idx in 0 to keys->Array.length - 1 {
          prepareShapedSerializerAcc(
            ~acc,
            ~input=vals->Dict.getUnsafe(keys->Array.getUnsafe(idx)),
          )
        }
      }
    | _ => ()
    }
  }
  and getShapedSerializerOutput = (
    ~input,
    ~acc: option<shapedSerializerAcc>,
    ~targetSchema: internal,
    ~path,
  ) => {
    switch acc {
    | Some({val}) => {
        let v = val->B.Val.scope
        v.hasTransform = Some(true)
        v.schema = targetSchema
        v.expected = targetSchema
        v->parse
      }
    | _ =>
      if targetSchema->isLiteral {
        let v = input->B.nextConst(~schema=targetSchema, ~expected=targetSchema)
        v.prev = None
        v.parent = Some(input)
        v.var = B._notVarAtParent
        v.isOutput = Some(true)
        v->parse
      } else {
        // When acc is None (discriminant field with no input), follow the to chain
        // to get the actual output schema properties (e.g., for reversed transformed objects)
        let resolvedTargetSchema = if acc === None {
          targetSchema->getOutputSchema
        } else {
          targetSchema
        }
        let v = makeObjectVal(input, ~schema=resolvedTargetSchema)
        v.expected = resolvedTargetSchema
        v.isOutput = Some(true)
        v.prev = None
        v.parent = Some(input)
        v.var = B._notVarAtParent

        switch resolvedTargetSchema {
        | {items}
          if !(
            acc === None &&
              resolvedTargetSchema.additionalItems->typeof === objectTag
          ) =>
          for idx in 0 to items->Array.length - 1 {
            let location = idx->Int.toString
            v->B.Val.Object.add(
              ~location,
              getShapedSerializerOutput(
                ~input,
                ~acc=switch acc {
                | Some({properties}) => properties->X.Dict.getUnsafeOption(location)
                | _ => None
                },
                ~targetSchema=items->Array.getUnsafe(idx),
                ~path=path->Path.concat(
                  Path.fromInlinedLocation(input.global->B.inlineLocation(location)),
                ),
              ),
            )
          }
        | {properties, ?flattened}
          if !(
            acc === None &&
              resolvedTargetSchema.additionalItems->typeof === objectTag
          ) => {
            switch (flattened, acc) {
            | (Some(flattenedSchemas), Some({flattened: flattenedAcc})) =>
              flattenedAcc->Array.forEachWithIndex((acc, idx) => {
                let flattenedOutput = getShapedSerializerOutput(
                  ~input,
                  ~acc=Some(acc),
                  ~targetSchema=flattenedSchemas->Array.getUnsafe(idx)->reverse,
                  ~path,
                )
                v->B.Val.Object.merge(flattenedOutput.vals->X.Option.getUnsafe)
              })
            | _ => ()
            }

            let keys = properties->Dict.keysToArray
            for idx in 0 to keys->Array.length - 1 {
              let location = keys->Array.getUnsafe(idx)

              // Skip fields added by flattened
              if !(v.vals->X.Option.getUnsafe->Stdlib.Dict.has(location)) {
                v->B.Val.Object.add(
                  ~location,
                  getShapedSerializerOutput(
                    ~input,
                    ~acc=switch acc {
                    | Some({properties}) => properties->X.Dict.getUnsafeOption(location)
                    | _ => None
                    },
                    ~targetSchema=properties->Dict.getUnsafe(location),
                    ~path=path->Path.concat(
                      Path.fromInlinedLocation(input.global->B.inlineLocation(location)),
                    ),
                  ),
                )
              }
            }
          }
        | _ =>
          let path = switch targetSchema.from {
          | Some(from) => path ++ from->Array.map(item => `["${item}"]`)->Array.join("")
          | None => path
          }
          input->B.invalidOperation(
            ~description={
              `Missing input for ${targetSchema->castToPublic->toExpression}` ++
              switch path {
              | "" => ""
              | _ => ` at ${path}`
              }
            },
          )
        }

        v->completeObjectVal
      }
    }
  }
  and shapedSerializer = (~input) => {
    let acc: shapedSerializerAcc = {}
    prepareShapedSerializerAcc(~acc, ~input)

    let targetSchema = input.expected.to->X.Option.getUnsafe
    let output = getShapedSerializerOutput(~input, ~acc=Some(acc), ~targetSchema, ~path=Path.empty)
    output.hasTransform = Some(true)
    output.prev = Some(input)
    output
  }

  and definitionToShapedSchema = definition => {
    let s =
      definition
      ->traverseDefinition(
        ~onNode=Definition.toEmbededItem->(
          Obj.magic: (Definition.t<internal> => option<internal>) => unknown => option<internal>
        ),
      )
      ->copySchema
    s.serializer = Some(shapedSerializer)
    s
  }
  and definitionToSchema = definition =>
    definition->traverseDefinition(~onNode=node => {
      if node->isSchemaObject {
        node->(Obj.magic: unknown => option<internal>)
      } else {
        None
      }
    })
  and traverseDefinition = (definition: unknown, ~onNode): internal => {
    if definition->Definition.isNode {
      switch onNode(definition) {
      | Some(s) => s
      | None =>
        if definition->X.Array.isArray {
          let node = definition->(Obj.magic: unknown => array<unknown>)
          for idx in 0 to node->Array.length - 1 {
            let schema = node->Array.getUnsafe(idx)->traverseDefinition(~onNode)
            node->Array.setUnsafe(idx, schema->(Obj.magic: internal => unknown))
          }
          let items = node->(Obj.magic: array<unknown> => array<internal>)

          let mut = base(arrayTag, ~selfReverse=false)
          mut.items = Some(items)
          mut.additionalItems = Some(Strict)
          mut.decoder = arrayDecoder
          mut
        } else {
          let cnstr = (definition->Obj.magic)["constructor"]
          if cnstr->Obj.magic && cnstr !== %raw(`Object`) {
            let mut = base(instanceTag, ~selfReverse=true)
            mut.class = cnstr
            mut.const = definition->Obj.magic
            mut.decoder = literalDecoder
            mut
          } else {
            let node = definition->(Obj.magic: unknown => dict<unknown>)
            let fieldNames = node->Dict.keysToArray
            let length = fieldNames->Array.length
            for idx in 0 to length - 1 {
              let location = fieldNames->Array.getUnsafe(idx)
              let schema = node->Dict.getUnsafe(location)->traverseDefinition(~onNode)
              node->Dict.set(location, schema->(Obj.magic: internal => unknown))
            }
            let mut = base(objectTag, ~selfReverse=false)
            mut.required = Some(fieldNames)
            mut.properties = Some(node->(Obj.magic: dict<unknown> => dict<internal>))
            mut.additionalItems = Some(globalConfig.defaultAdditionalItems)
            mut.decoder = objectDecoder
            mut
          }
        }
      }
    } else {
      Literal.parse(definition)
    }
  }

  let matches:
    type value. schema<value> => value =
    schema => schema->(Obj.magic: schema<value> => value)
  let ctx = {
    matches: matches,
  }
  let factory = definer => {
    definer(ctx->(Obj.magic: s => 'value))
    ->(Obj.magic: 'definition => unknown)
    ->definitionToSchema
    ->castToPublic
  }
}

let schema = Schema.factory

let js_schema = definition => definition->Obj.magic->Schema.definitionToSchema->castToPublic
let literal = js_schema

let enum = values => Union.factory(values->Array.map(literal))

let compactColumnsDecoder = (~input) => {
  let selfSchema = input.expected
  let isUnknownInput = input.schema.tag->TagFlag.get->Flag.unsafeHas(TagFlag.unknown)

  // Find the object schema whose properties define the columns.
  // Forward (columnar → rows): props come from selfSchema.to.additionalItems.
  // Reverse (rows → columnar): props come from input.schema.additionalItems (the
  // object schema left over after the preceding parse pipeline step).
  let forwardProps = switch selfSchema.to {
  | Some({additionalItems: ?Some(Schema(item))}) => (item->castToInternal).properties
  | _ => None
  }
  let isForwardDirection = forwardProps->Obj.magic
  let maybeProperties = if isForwardDirection {
    forwardProps
  } else {
    switch input.schema.additionalItems {
    | Some(Schema(item)) => (item->castToInternal).properties
    | _ => None
    }
  }

  switch maybeProperties {
  | None =>
    InternalError.panic(
      "S.compactColumns supports only object schemas. Use S.compactColumns(S.unknown)->S.to(S.array(objectSchema)).",
    )
  | Some(properties) => {
      let keys = properties->Dict.keysToArray
      let keysLen = keys->Array.length

      // Forward: output already matches selfSchema.to, reuse it so
      // markOutput picks up its refiner. selfSchema.to is Some here —
      // isForwardDirection reads through it above.
      // Reverse: runtime shape differs (array of arrays of unknown),
      // so build fresh and propagate .to for downstream steps.
      let outputSchema = if isForwardDirection {
        selfSchema.to->X.Option.getUnsafe
      } else {
        let s = array(array(unknown->castToPublic))->castToInternal
        s.to = selfSchema.to
        s
      }

      if keysLen === 0 {
        let input = if isUnknownInput {
          input->B.refine(
            ~checks=[
              {
                cond: (~inputVar) => `Array.isArray(${inputVar})&&${inputVar}.length===0`,
                fail: B.failInvalidType,
              },
            ],
          )
        } else {
          input
        }
        let output = input->B.next("[]", ~schema=outputSchema, ~expected=outputSchema)
        output->B.markOutput(~valInput=input)
      } else if isForwardDirection {
        // Forward direction: columnar → rows
        let input = if isUnknownInput {
          input->B.refine(
            ~checks=[
              {
                cond: (~inputVar) => {
                  let check = ref(
                    `Array.isArray(${inputVar})&&${inputVar}.length===${keysLen->X.Int.unsafeToString}`,
                  )
                  for idx in 0 to keysLen - 1 {
                    check :=
                      check.contents ++ `&&Array.isArray(${inputVar}[${idx->X.Int.unsafeToString}])`
                  }
                  check.contents
                },
                fail: B.failInvalidType,
              },
            ],
          )
        } else {
          input
        }

        let inputVar = input.var()
        let iteratorVar = input.global->B.varWithoutAllocation
        let outputVar = input.global->B.varWithoutAllocation

        // Declared source item type from selfSchema (the compactColumns schema).
        let declaredItemSchema: internal = {
          let innerArray: internal = selfSchema.additionalItems->Obj.magic
          innerArray.additionalItems->Obj.magic
        }

        // Actual runtime item type: unknown for top-level parser, or
        // the typed source when the caller passed already-typed data.
        let runtimeItemSchema: internal = if isUnknownInput {
          unknown
        } else {
          let innerArray: internal = input.schema.additionalItems->Obj.magic
          innerArray.additionalItems->Obj.magic
        }

        let lengthCode = ref("")
        let itemBuildCode = ref("")
        let itemParseCode = ref("")
        let asyncInlines = ref("")
        let hasAsync = ref(false)
        for idx in 0 to keysLen - 1 {
          let key = keys->Array.getUnsafe(idx)
          let idxStr = idx->X.Int.unsafeToString
          let rawValueCode = `${inputVar}[${idxStr}][${iteratorVar}]`

          let fieldSchema = properties->Dict.getUnsafe(key)

          // When the declared source differs from the runtime type
          // (e.g. runtime=unknown, declared=json), chain through the
          // declared type first so parse validates the value matches
          // the source schema before converting to the field type.
          let itemExpected = if declaredItemSchema !== runtimeItemSchema {
            let chained = declaredItemSchema->copySchema
            chained.to = Some(fieldSchema)
            chained
          } else {
            fieldSchema
          }

          let itemInput = input->B.Val.scope
          itemInput.inline = rawValueCode
          itemInput.schema = runtimeItemSchema
          itemInput.expected = itemExpected
          itemInput.var = B._notVarBeforeValidation
          itemInput.isOutput = Some(false)

          // Path like ["bar"] so validation errors carry the field location.
          itemInput.path = Path.fromInlinedLocation(input.global->B.inlineLocation(key))

          let itemOutput = itemInput->parse
          if itemOutput.flag->Flag.unsafeHas(ValFlag.async) {
            hasAsync := true
          }

          itemParseCode := itemParseCode.contents ++ itemOutput->B.merge
          lengthCode := lengthCode.contents ++ `${inputVar}[${idxStr}].length,`
          asyncInlines := asyncInlines.contents ++ `${itemOutput.inline},`
          itemBuildCode :=
            itemBuildCode.contents ++ `${key->X.Inlined.Value.fromString}:${itemOutput.inline},`
        }

        input.allocate(`${outputVar}=new Array(Math.max(${lengthCode.contents}))`)

        let output = input->B.next(outputVar, ~schema=outputSchema, ~expected=outputSchema)
        output.var = B._var

        // Wrap the row body in a single try/catch that prepends the row index to
        // any thrown error — giving paths like ["0"]["bar"]. A single wrapper is
        // used (rather than per-field) so that `let` variables declared while
        // parsing one field remain in scope for the object construction.
        let rowAssign = if hasAsync.contents {
          // For async fields, each row becomes a promise that awaits all field values
          // via Promise.all, and the final output is Promise.all of all row promises.
          let rowResultVar = input.global->B.varWithoutAllocation
          let asyncBuildCode = ref("")
          for idx in 0 to keysLen - 1 {
            let key = keys->Array.getUnsafe(idx)
            asyncBuildCode :=
              asyncBuildCode.contents ++
              `${key->X.Inlined.Value.fromString}:${rowResultVar}[${idx->X.Int.unsafeToString}],`
          }
          `${outputVar}[${iteratorVar}]=Promise.all([${asyncInlines.contents}]).then(${rowResultVar}=>({${asyncBuildCode.contents}}));`
        } else {
          `${outputVar}[${iteratorVar}]={${itemBuildCode.contents}};`
        }

        let rowBody = itemParseCode.contents ++ rowAssign
        let wrappedBody = if itemParseCode.contents === "" {
          rowBody
        } else {
          let errorVar = input.global->B.varWithoutAllocation
          `try{${rowBody}}catch(${errorVar}){${errorVar}.path='["'+${iteratorVar}+'"]'+${errorVar}.path;throw ${errorVar}}`
        }
        output.codeFromPrev =
          output.codeFromPrev ++
          `for(let ${iteratorVar}=0;${iteratorVar}<${outputVar}.length;++${iteratorVar}){${wrappedBody}}`

        let output = if hasAsync.contents {
          output->B.asyncVal(`Promise.all(${outputVar})`)
        } else {
          output
        }
        output->B.markOutput(~valInput=input)
      } else {
        // Reverse direction: rows → columnar
        // When the declared source type is unknown, field values have
        // already been transformed by the object schema's reverse parse
        // and can be copied directly. When it differs (e.g. json), we
        // need per-field parse to convert values back to the source type
        // (e.g. bigint→string for json compatibility).
        let inputVar = input->B.Val.var
        let iteratorVar = input.global->B.varWithoutAllocation
        let outputVar = input.global->B.varWithoutAllocation

        let declaredItemSchema: internal = {
          let innerArray: internal = selfSchema.additionalItems->Obj.magic
          innerArray.additionalItems->Obj.magic
        }
        let needsPerFieldTransform = declaredItemSchema !== unknown

        let initialArraysCode = ref("")
        let settingCode = ref("")
        let perFieldCode = ref("")
        for idx in 0 to keysLen - 1 {
          let key = keys->Array.getUnsafe(idx)
          initialArraysCode := initialArraysCode.contents ++ `new Array(${inputVar}.length),`

          if needsPerFieldTransform {
            let fieldSchema = properties->Dict.getUnsafe(key)
            let rawValueCode = `${inputVar}[${iteratorVar}][${key->X.Inlined.Value.fromString}]`

            let itemInput = input->B.Val.scope
            itemInput.inline = rawValueCode
            itemInput.schema = fieldSchema
            itemInput.expected = declaredItemSchema
            itemInput.var = B._notVarBeforeValidation
            itemInput.isOutput = Some(false)
            itemInput.path = Path.fromInlinedLocation(input.global->B.inlineLocation(key))

            let itemOutput = itemInput->parse
            perFieldCode := perFieldCode.contents ++ itemOutput->B.merge
            settingCode :=
              settingCode.contents ++
              `${outputVar}[${idx->X.Int.unsafeToString}][${iteratorVar}]=${itemOutput.inline};`
          } else {
            settingCode :=
              settingCode.contents ++
              `${outputVar}[${idx->X.Int.unsafeToString}][${iteratorVar}]=${inputVar}[${iteratorVar}][${key->X.Inlined.Value.fromString}];`
          }
        }

        input.allocate(`${outputVar}=[${initialArraysCode.contents}]`)

        let output = input->B.next(outputVar, ~schema=outputSchema, ~expected=outputSchema)
        output.var = B._var
        let loopBody = perFieldCode.contents ++ settingCode.contents
        let wrappedBody = if needsPerFieldTransform && perFieldCode.contents !== "" {
          let errorVar = input.global->B.varWithoutAllocation
          `try{${loopBody}}catch(${errorVar}){${errorVar}.path='["'+${iteratorVar}+'"]'+${errorVar}.path;throw ${errorVar}}`
        } else {
          loopBody
        }
        output.codeFromPrev =
          output.codeFromPrev ++
          `for(let ${iteratorVar}=0;${iteratorVar}<${inputVar}.length;++${iteratorVar}){${wrappedBody}}`
        output->B.markOutput(~valInput=input)
      }
    }
  }
}

let compactColumns = inputSchema => {
  let innerArray = array(inputSchema)
  let mut = array(innerArray)->castToInternal
  mut.format = Some(CompactColumns)
  mut.decoder = compactColumnsDecoder
  mut->castToPublic
}

// let inline = {
//   let rec internalInline = (schema, ~variant as maybeVariant=?, ()) => {
//     let mut = schema->castToInternal->copy

//     let inlinedSchema = switch mut {
//     | {?const} if isLiteral(mut) => `S.literal(%raw(\`${literal->Literal.toString}\`))`
//     | {anyOf} => {
//         let variantNamesCounter = dict{}
//         `S.union([${anyOf
//           ->Array.map(s => {
//             let variantName = s.name()
//             let numberOfVariantNames = switch variantNamesCounter->Dict.get(variantName) {
//             | Some(n) => n
//             | None => 0
//             }
//             variantNamesCounter->Dict.set(variantName, numberOfVariantNames->X.Int.plus(1))
//             let variantName = switch numberOfVariantNames {
//             | 0 => variantName
//             | _ =>
//               variantName ++ numberOfVariantNames->X.Int.plus(1)->X.Int.unsafeToString
//             }
//             let inlinedVariant = `#${variantName->X.Inlined.Value.fromString}`
//             s->internalInline(~variant=inlinedVariant, ())
//           })
//           ->Array.join(", ")}])`
//       }
//     | {tag: JSON} => `S.json(~validate=${validated->(Obj.magic: bool => string)})`
//     | {tag: TupleTuple({items: [s0]}) => `S.tuple1(${s0.schema->internalInline()})`
//     | Tuple({items: [s0, s1]}) =>
//       `S.tuple2(${s0.schema->internalInline()}, ${s1.schema->internalInline()})`
//     | Tuple({items: [s0, s1, s2]}) =>
//       `S.tuple3(${s0.schema->internalInline()}, ${s1.schema->internalInline()}, ${s2.schema->internalInline()})`
//     | Tuple({items}) =>
//       `S.tuple(s => (${items
//         ->Array.mapWithIndex((schema, idx) =>
//           `s.item(${idx->X.Int.unsafeToString}, ${schema.schema->internalInline()})`
//         )
//         ->Array.join(", ")}))`
//     | Object({items: []}) => `S.object(_ => ())`
//     | Object({items}) =>
//       `S.object(s =>
//   {
//     ${items
//         ->Array.map(item => {
//           `${item.inlinedLocation}: s.field(${item.inlinedLocation}, ${item.schema->internalInline()})`
//         })
//         ->Array.join(",\n    ")},
//   }
// )`
//     | String => `S.string`
//     | Int => `S.int`
//     | Float => `S.float`
//     | BigInt => `S.bigint`
//     | Bool => `S.bool`
//     | Option(schema) => `S.option(${schema->internalInline()})`
//     | Null(schema) => `S.nullAsOption(${schema->internalInline()})`
//     | Never => `S.never`
//     | Unknown => `S.unknown`
//     | Array(schema) => `S.array(${schema->internalInline()})`
//     | Dict(schema) => `S.dict(${schema->internalInline()})`
//     }

//     let inlinedSchema = switch schema->Option.default {
//     | Some(default) => {
//         metadataMap->X.Dict.deleteInPlace(Option.defaultMetadataId->Metadata.Id.toKey)
//         switch default {
//         | Value(defaultValue) =>
//           inlinedSchema ++
//           `->S.Option.getOr(%raw(\`${defaultValue->X.Inlined.Value.stringify}\`))`
//         | Callback(defaultCb) =>
//           inlinedSchema ++
//           `->S.Option.getOrWith(() => %raw(\`${defaultCb()->X.Inlined.Value.stringify}\`))`
//         }
//       }

//     | None => inlinedSchema
//     }

//     let inlinedSchema = switch schema->deprecation {
//     | Some(message) => {
//         metadataMap->X.Dict.deleteInPlace(deprecationMetadataId->Metadata.Id.toKey)
//         inlinedSchema ++ `->S.deprecate(${message->X.Inlined.Value.fromString})`
//       }

//     | None => inlinedSchema
//     }

//     let inlinedSchema = switch schema->description {
//     | Some(message) => {
//         metadataMap->X.Dict.deleteInPlace(descriptionMetadataId->Metadata.Id.toKey)
//         inlinedSchema ++ `->S.describe(${message->X.Inlined.Value.stringify})`
//       }

//     | None => inlinedSchema
//     }

//     let inlinedSchema = switch schema->classify {
//     | Object({additionalItems: Strict}) => inlinedSchema ++ `->S.strict`
//     | _ => inlinedSchema
//     }

//     let inlinedSchema = switch schema->classify {
//     | String
//     | Literal(String(_)) =>
//       switch schema->String.refinements {
//       | [] => inlinedSchema
//       | refinements =>
//         metadataMap->X.Dict.deleteInPlace(String.Refinement.metadataId->Metadata.Id.toKey)
//         inlinedSchema ++
//         refinements
//         ->Array.map(refinement => {
//           switch refinement {
//           | {kind: Email, message} =>
//             `->S.email(~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Url, message} => `->S.url(~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Uuid, message} =>
//             `->S.uuid(~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Cuid, message} =>
//             `->S.cuid(~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Min({length}), message} =>
//             `->S.stringMinLength(${length->X.Int.unsafeToString}, ~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Max({length}), message} =>
//             `->S.stringMaxLength(${length->X.Int.unsafeToString}, ~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Length({length}), message} =>
//             `->S.stringLength(${length->X.Int.unsafeToString}, ~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Pattern({re}), message} =>
//             `->S.pattern(%re(${re
//               ->X.Re.toString
//               ->X.Inlined.Value.fromString}), ~message=${message->X.Inlined.Value.fromString})`
//           }
//         })
//         ->Array.join("")
//       }
//     | Int =>
//       // | Literal(Int(_)) ???
//       switch schema->Int.refinements {
//       | [] => inlinedSchema
//       | refinements =>
//         metadataMap->X.Dict.deleteInPlace(Int.Refinement.metadataId->Metadata.Id.toKey)
//         inlinedSchema ++
//         refinements
//         ->Array.map(refinement => {
//           switch refinement {
//           | {kind: Max({value}), message} =>
//             `->S.intMax(${value->X.Int.unsafeToString}, ~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Min({value}), message} =>
//             `->S.intMin(${value->X.Int.unsafeToString}, ~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Port, message} =>
//             `->S.port(~message=${message->X.Inlined.Value.fromString})`
//           }
//         })
//         ->Array.join("")
//       }
//     | Float =>
//       // | Literal(Float(_)) ???
//       switch schema->Float.refinements {
//       | [] => inlinedSchema
//       | refinements =>
//         metadataMap->X.Dict.deleteInPlace(Float.Refinement.metadataId->Metadata.Id.toKey)
//         inlinedSchema ++
//         refinements
//         ->Array.map(refinement => {
//           switch refinement {
//           | {kind: Max({value}), message} =>
//             `->S.floatMax(${value->X.Inlined.Float.toRescript}, ~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Min({value}), message} =>
//             `->S.floatMin(${value->X.Inlined.Float.toRescript}, ~message=${message->X.Inlined.Value.fromString})`
//           }
//         })
//         ->Array.join("")
//       }

//     | Array(_) =>
//       switch schema->Array.refinements {
//       | [] => inlinedSchema
//       | refinements =>
//         metadataMap->X.Dict.deleteInPlace(Array.Refinement.metadataId->Metadata.Id.toKey)
//         inlinedSchema ++
//         refinements
//         ->Array.map(refinement => {
//           switch refinement {
//           | {kind: Max({length}), message} =>
//             `->S.arrayMaxLength(${length->X.Int.unsafeToString}, ~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Min({length}), message} =>
//             `->S.arrayMinLength(${length->X.Int.unsafeToString}, ~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Length({length}), message} =>
//             `->S.arrayLength(${length->X.Int.unsafeToString}, ~message=${message->X.Inlined.Value.fromString})`
//           }
//         })
//         ->Array.join("")
//       }

//     | _ => inlinedSchema
//     }

//     let inlinedSchema = if metadataMap->Dict.keysToArray->Array.length !== 0 {
//       `{
//   let s = ${inlinedSchema}
//   let _ = %raw(\`s.m = ${metadataMap->JSON.stringifyAny->Option.getUnsafe}\`)
//   s
// }`
//     } else {
//       inlinedSchema
//     }

//     let inlinedSchema = switch maybeVariant {
//     | Some(variant) => inlinedSchema ++ `->S.shape(v => ${variant}(v))`
//     | None => inlinedSchema
//     }

//     inlinedSchema
//   }

//   schema => {
//     schema->castToUnknown->internalInline()
//   }
// }

let object = Schema.object
let nullAsOption = item => Option.factory(item, ~unit=nullAsUnit()->castToPublic)
let null = item => Union.factory([item->castToUnknown, nullLiteral()->castToPublic])
let option = item => item->Option.factory(~unit=unit()->castToPublic)
let array = array
let dict = DictSchema.factory
let shape = Schema.shape
let tuple = Schema.tuple
let tuple1 = v0 => tuple(s => s.item(0, v0))
let tuple2 = (v0, v1) =>
  Schema.definitionToSchema([v0->castToUnknown, v1->castToUnknown]->Obj.magic)->castToPublic
let tuple3 = (v0, v1, v2) =>
  Schema.definitionToSchema(
    [v0->castToUnknown, v1->castToUnknown, v2->castToUnknown]->Obj.magic,
  )->castToPublic
let union = Union.factory

// =============
// Built-in refinements
// =============

let assertNumber: (string, 'a) => unit = (fnName, n) =>
  if typeof(n->Obj.magic) !== numberTag || %raw(`Number.isNaN(n)`) {
    X.Exn.throwAny(
      InternalError.make(
        InvalidOperation({
          path: Path.empty,
          reason: `[S.${fnName}] Expected number, received ${n->Obj.magic->stringify}`,
        }),
      ),
    )
  }

let intMin = (schema, minValue, ~message as maybeMessage=?) => {
  assertNumber("min", minValue)
  let message = switch maybeMessage {
  | Some(m) => m
  | None => `Number must be greater than or equal to ${minValue->X.Int.unsafeToString}`
  }
  schema->internalRefine(mut => {
    mut.minimum = Some(minValue->Int.toFloat)
    getMutErrorMessage(~mut)->Dict.set("minimum", message)
    (~input as _) => {
      [
        {
          cond: (~inputVar) => `${inputVar}>${(minValue - 1)->X.Int.unsafeToString}`,
          fail: B.failWithErrorMessage("minimum", ~defaultMessage=message),
        },
      ]
    }
  })
}

let intMax = (schema, maxValue, ~message as maybeMessage=?) => {
  assertNumber("max", maxValue)
  let message = switch maybeMessage {
  | Some(m) => m
  | None => `Number must be lower than or equal to ${maxValue->X.Int.unsafeToString}`
  }
  schema->internalRefine(mut => {
    mut.maximum = Some(maxValue->Int.toFloat)
    getMutErrorMessage(~mut)->Dict.set("maximum", message)
    (~input as _) => {
      [
        {
          cond: (~inputVar) => `${inputVar}<${(maxValue + 1)->X.Int.unsafeToString}`,
          fail: B.failWithErrorMessage("maximum", ~defaultMessage=message),
        },
      ]
    }
  })
}

let floatMin = (schema, minValue, ~message as maybeMessage=?) => {
  assertNumber("min", minValue)
  let message = switch maybeMessage {
  | Some(m) => m
  | None => `Number must be greater than or equal to ${minValue->X.Float.unsafeToString}`
  }
  schema->internalRefine(mut => {
    mut.minimum = Some(minValue)
    getMutErrorMessage(~mut)->Dict.set("minimum", message)
    (~input) => {
      [
        {
          cond: (~inputVar) => `${inputVar}>=${input->B.embed(minValue)}`,
          fail: B.failWithErrorMessage("minimum", ~defaultMessage=message),
        },
      ]
    }
  })
}

let floatMax = (schema, maxValue, ~message as maybeMessage=?) => {
  assertNumber("max", maxValue)
  let message = switch maybeMessage {
  | Some(m) => m
  | None => `Number must be lower than or equal to ${maxValue->X.Float.unsafeToString}`
  }
  schema->internalRefine(mut => {
    mut.maximum = Some(maxValue)
    getMutErrorMessage(~mut)->Dict.set("maximum", message)
    (~input) => {
      [
        {
          cond: (~inputVar) => `${inputVar}<=${input->B.embed(maxValue)}`,
          fail: B.failWithErrorMessage("maximum", ~defaultMessage=message),
        },
      ]
    }
  })
}

let arrayMinLength = (schema, length, ~message as maybeMessage=?) => {
  assertNumber("min", length)
  let message = switch maybeMessage {
  | Some(m) => m
  | None => `Array must be ${length->X.Int.unsafeToString} or more items long`
  }
  schema->internalRefine(mut => {
    mut.minItems = Some(length)
    getMutErrorMessage(~mut)->Dict.set("minItems", message)
    (~input as _) => {
      [
        {
          cond: (~inputVar) => `${inputVar}.length>${(length - 1)->X.Int.unsafeToString}`,
          fail: B.failWithErrorMessage("minItems", ~defaultMessage=message),
        },
      ]
    }
  })
}

let arrayMaxLength = (schema, length, ~message as maybeMessage=?) => {
  assertNumber("max", length)
  let message = switch maybeMessage {
  | Some(m) => m
  | None => `Array must be ${length->X.Int.unsafeToString} or fewer items long`
  }
  schema->internalRefine(mut => {
    mut.maxItems = Some(length)
    getMutErrorMessage(~mut)->Dict.set("maxItems", message)
    (~input as _) => {
      [
        {
          cond: (~inputVar) => `${inputVar}.length<${(length + 1)->X.Int.unsafeToString}`,
          fail: B.failWithErrorMessage("maxItems", ~defaultMessage=message),
        },
      ]
    }
  })
}

let arrayLength = (schema, length, ~message as maybeMessage=?) => {
  assertNumber("length", length)
  let message = switch maybeMessage {
  | Some(m) => m
  | None => `Array must be exactly ${length->X.Int.unsafeToString} items long`
  }
  schema->internalRefine(mut => {
    mut.minItems = Some(length)
    mut.maxItems = Some(length)
    let em = getMutErrorMessage(~mut)
    em->Dict.set("minItems", message)
    em->Dict.set("maxItems", message)
    (~input as _) => {
      [
        {
          cond: (~inputVar) => `${inputVar}.length===${length->X.Int.unsafeToString}`,
          fail: B.failWithErrorMessage("minItems", ~defaultMessage=message),
        },
      ]
    }
  })
}

let stringMinLength = (schema, length, ~message as maybeMessage=?) => {
  assertNumber("min", length)
  let message = switch maybeMessage {
  | Some(m) => m
  | None => `String must be ${length->X.Int.unsafeToString} or more characters long`
  }
  schema->internalRefine(mut => {
    mut.minLength = Some(length)
    getMutErrorMessage(~mut)->Dict.set("minLength", message)
    (~input as _) => {
      [
        {
          cond: (~inputVar) => `${inputVar}.length>${(length - 1)->X.Int.unsafeToString}`,
          fail: B.failWithErrorMessage("minLength", ~defaultMessage=message),
        },
      ]
    }
  })
}

let stringMaxLength = (schema, length, ~message as maybeMessage=?) => {
  assertNumber("max", length)
  let message = switch maybeMessage {
  | Some(m) => m
  | None => `String must be ${length->X.Int.unsafeToString} or fewer characters long`
  }
  schema->internalRefine(mut => {
    mut.maxLength = Some(length)
    getMutErrorMessage(~mut)->Dict.set("maxLength", message)
    (~input as _) => {
      [
        {
          cond: (~inputVar) => `${inputVar}.length<${(length + 1)->X.Int.unsafeToString}`,
          fail: B.failWithErrorMessage("maxLength", ~defaultMessage=message),
        },
      ]
    }
  })
}

let stringLength = (schema, length, ~message as maybeMessage=?) => {
  assertNumber("length", length)
  let message = switch maybeMessage {
  | Some(m) => m
  | None => `String must be exactly ${length->X.Int.unsafeToString} characters long`
  }
  schema->internalRefine(mut => {
    mut.minLength = Some(length)
    mut.maxLength = Some(length)
    let em = getMutErrorMessage(~mut)
    em->Dict.set("minLength", message)
    em->Dict.set("maxLength", message)
    (~input as _) => {
      [
        {
          cond: (~inputVar) => `${inputVar}.length===${length->X.Int.unsafeToString}`,
          fail: B.failWithErrorMessage("minLength", ~defaultMessage=message),
        },
      ]
    }
  })
}

let pattern = (schema, re, ~message=`Invalid pattern`) => {
  schema->internalRefine(mut => {
    mut.pattern = Some(re)
    getMutErrorMessage(~mut)->Dict.set("pattern", message)
    (~input) => {
      let embededRe = input->B.embed(re)
      [
        {
          cond: (~inputVar) =>
            if re->RegExp.global {
              `(${embededRe}.lastIndex=0,${embededRe}.test(${inputVar}))`
            } else {
              `${embededRe}.test(${inputVar})`
            },
          fail: B.failWithErrorMessage("pattern", ~defaultMessage=message),
        },
      ]
    }
  })
}

let trim = schema => {
  let transformer = string => string->String.trim
  schema->transform(_ => {parser: transformer, serializer: transformer})
}

let nullable = schema => {
  Union.factory([schema->castToUnknown, unit()->castToPublic, nullLiteral()->castToPublic])
}

let nullableAsOption = schema => {
  Union.factory([
    schema->castToUnknown,
    unit()->castToPublic,
    nullAsUnit()->castToPublic->castToUnknown,
  ])
}

// =============
// JS/TS API
// =============

let js_parser = %raw(`(...args) => getDecoder(unknown, ...args)`)

let js_asyncParser = %raw(`(...args) => getDecoder(unknown, ...args, 1)`)

let js_asyncDecoder = %raw(`(...args) => getDecoder(...args, 1)`)

let js_encoder = %raw(`(...args) => getDecoder(...args.map(reverse))`)

let js_asyncEncoder = %raw(`(...args) => getDecoder(...args.map(reverse), 1)`)

let js_assert = (schema, data) => {
  getDecoder3(~s1=unknown, ~s2=schema->castToInternal, ~s3=getAssertResult())(data)
}

let js_union = values =>
  Union.factory(
    values->Array.map(Schema.definitionToSchema)->(Obj.magic: array<internal> => array<'a>),
  )

let js_to = {
  // FIXME: Test how it'll work if we have async var as input
  // FIXME: Might not work well with object targets
  let customBuilder = (~fn) => {
    Builder.make((~input) => {
      let target = input.expected.to->X.Option.getUnsafe
      let outputVar = input.global->B.varWithoutAllocation
      input.allocate(outputVar)
      let output = input->B.next(outputVar, ~schema=target, ~expected=target)
      output.var = B._var
      output.codeFromPrev = `try{${output.inline}=${input->B.embed(
          fn,
        )}(${input.inline})}catch(x){${output->B.failWithArg(
          e => B.makeInvalidConversionDetails(~input, ~to=target, ~cause=e),
          `x`,
        )}}`
      output
    })
  }

  (
    schema,
    target,
    ~decoder as maybeDecoder: option<'value => 'target>=?,
    ~encoder as maybeEncoder: option<'target => 'value>=?,
  ) => {
    updateOutput(schema->castToInternal, mut => {
      let target = target->castToInternal
      switch maybeEncoder {
      | Some(fn) =>
        let targetMut = target->copySchema
        targetMut.serializer = Some(customBuilder(~fn))
        mut.to = Some(targetMut)
      | None => mut.to = Some(target)
      }
      switch maybeDecoder {
      | Some(fn) => mut.parser = Some(customBuilder(~fn))
      | None => ()
      }
    })
  }
}

let js_refine = (schema, refineCheck, refineOptions) => {
  let message = switch refineOptions {
  | Some(options) =>
    switch (options->Obj.magic)["error"] {
    | Some(e) => e
    | None => "Refinement failed"
    }
  | None => "Refinement failed"
  }
  let extraPath = switch refineOptions {
  | Some(options) =>
    switch (options->Obj.magic)["path"] {
    | Some(p) => Path.fromArray(p)
    | None => Path.empty
    }
  | None => Path.empty
  }
  schema->internalRefine(_ =>
    (~input) => {
      let embeddedCheck = input->B.embed(refineCheck)
      [
        {
          cond: (~inputVar) => `${embeddedCheck}(${inputVar})`,
          fail: B.invalidInputBuilder(~extraPath, ~reasonOverride=message),
        },
      ]
    }
  )
}

let noop = a => a
let js_asyncDecoderAssert = (schema, assertFn) => {
  schema->transform(_ => {
    {
      asyncParser: v => assertFn(v)->X.Promise.thenResolve(() => v),
      serializer: noop,
    }
  })
}

let js_optional = (schema, maybeOr) => {
  // TODO: maybeOr should be part of the unit schema
  let schema = Union.factory([schema->castToUnknown, unit()->castToPublic])
  switch maybeOr {
  | Some(or) if typeof(or) === functionTag => schema->Option.getOrWith(or->Obj.magic)->Obj.magic
  | Some(or) => schema->Option.getOr(or->Obj.magic)->Obj.magic
  | None => schema
  }
}

let js_nullable = (schema, maybeOr) => {
  // TODO: maybeOr should be part of the unit schema
  switch maybeOr {
  | Some(or) =>
    let schema = Union.factory([schema->castToUnknown, nullAsUnit()->castToPublic->castToUnknown])
    if typeof(or) === functionTag {
      schema->Option.getOrWith(or->Obj.magic)->Obj.magic
    } else {
      schema->Option.getOr(or->Obj.magic)->Obj.magic
    }
  | None => Union.factory([schema->castToUnknown, nullLiteral()->castToPublic->castToUnknown])
  }
}

let js_merge = (s1, s2) => {
  switch switch (s1, s2) {
  | (
      Object({properties: properties1, additionalItems: additionalItems1}),
      Object({properties: properties2, additionalItems: additionalItems2}),
    )
    // Filter out S.record schemas
    if additionalItems1->typeof === stringTag &&
    additionalItems2->typeof === stringTag &&
    !((s1->castToInternal).to->Obj.magic) &&
    !((s2->castToInternal).to->Obj.magic) =>
    let properties = properties1->X.Dict.copy
    let keys2 = properties2->Dict.keysToArray

    for idx in 0 to keys2->Array.length - 1 {
      let key = keys2->Array.getUnsafe(idx)
      properties->Dict.set(key, properties2->Dict.getUnsafe(key))
    }

    let mut = base(objectTag, ~selfReverse=false)

    // TODO: Merge to required fields
    mut.required = Some(properties->(Obj.magic: dict<t<unknown>> => dict<internal>)->Dict.keysToArray)
    mut.properties = Some(properties->(Obj.magic: dict<t<unknown>> => dict<internal>))
    mut.additionalItems = Some(additionalItems1)
    mut.decoder = objectDecoder
    Some(mut->castToPublic)
  | _ => None
  } {
  | Some(s) => s
  | None =>
    InternalError.panic("The merge supports only structured object schemas without transformations")
  }
}

let global = override => {
  globalConfig.defaultAdditionalItems = (switch override.defaultAdditionalItems {
  | Some(defaultAdditionalItems) => defaultAdditionalItems
  | None => initialOnAdditionalItems
  } :> additionalItems)
  globalConfig.defaultFlag = switch override.disableNanNumberValidation {
  | Some(true) => Flag.disableNanNumberValidation
  | _ => initialDefaultFlag
  }
}

let reverse = reverse->Obj.magic

module RescriptJSONSchema = {
  include JSONSchema

  let jsonSchemaMetadataId: Metadata.Id.t<t> = Metadata.Id.internal("JSONSchema")

  @val
  external merge: (@as(json`{}`) _, t, t) => t = "Object.assign"

  let applyMetadataOverlay = (jsonSchema: Mutable.t, schema: schema<unknown>, ~defs): unit => {
    switch schema->untag {
    | {description: m} => jsonSchema.description = Some(m)
    | _ => ()
    }
    switch schema->untag {
    | {title: m} => jsonSchema.title = Some(m)
    | _ => ()
    }
    switch schema->untag {
    | {deprecated} => jsonSchema.deprecated = Some(deprecated)
    | _ => ()
    }
    switch schema->untag {
    | {examples} =>
      jsonSchema.examples = Some(
        examples->(
          Obj.magic: // If a schema is Jsonable,
          // then examples are Jsonable too.
          array<unknown> => array<JSON.t>
        ),
      )
    | _ => ()
    }
    switch schema->untag {
    | {defs: schemaDefs} =>
      let _ = defs->X.Dict.mixin(schemaDefs)
    | _ => ()
    }
    switch schema->Metadata.get(~id=jsonSchemaMetadataId) {
    | Some(metadataRawSchema) => jsonSchema->Mutable.mixin(metadataRawSchema)
    | None => ()
    }
  }

  let rec encodeToJsonSchema = (schema: schema<unknown>, ~path, ~defs, ~parent): option<
    JSONSchema.t,
  > => {
    let schemaInternal = schema->castToInternal
    let reversed = schemaInternal->reverse
    let input = B.operationArg(
      ~flag=Flag.none,
      ~defs=%raw(`0`),
      ~schema=unknown,
      ~expected=reversed,
    )
    try {
      let output = input->parse
      // The parse produces a val whose .schema reflects the
      // JSON-compatible transformed structure.
      Some(internalToJSONSchema(output.schema->castToPublic, ~path, ~defs, ~parent))
    } catch {
    | _ => {
        let _ = %raw(`exn`)->InternalError.getOrRethrow

        // Parse failed — caller falls through to normal tag-based logic.
        None
      }
    }
  }
  and internalToJSONSchema = (schema: schema<unknown>, ~path, ~defs, ~parent): JSONSchema.t => {
    let schemaInternal = schema->castToInternal
    // When a schema has `.to`, we can try to encode-reverse it to get a more
    // precise JSON schema (e.g. `format: "date-time"` for `S.string->S.to(S.date)`).
    // For a user-applied `.to` on a union (no `parser`) the encode-reverse output
    // is the schema produced by the union decoder, already shrunk to the
    // surviving variants — exactly what a downstream JSON Schema should describe.
    // Unions with a `parser` come from the option machinery (S.option,
    // Option.getOrWith, ...) where the union's anyOf is the input format we want
    // to keep describing. Object/array still need their nested item metadata, so
    // they keep using the base path.
    let tagFlag = schemaInternal.tag->TagFlag.get
    let hasUserTo =
      schemaInternal.to->Obj.magic &&
      !(tagFlag->Flag.unsafeHas(TagFlag.object->Flag.with(TagFlag.array))) &&
      !(tagFlag->Flag.unsafeHas(TagFlag.union) && schemaInternal.parser->Obj.magic)
    let encoded = if hasUserTo {
      encodeToJsonSchema(schema, ~path, ~defs, ~parent)
    } else {
      None
    }
    switch encoded {
    | Some(encodedJsonSchema) =>
      let mutableJs = encodedJsonSchema->Mutable.fromReadOnly
      mutableJs->applyMetadataOverlay(schema, ~defs)
      mutableJs->Mutable.toReadOnly
    | None => internalToJSONSchemaBase(schema, ~path, ~defs, ~parent)
    }
  }
  and internalToJSONSchemaBase = (schema: schema<unknown>, ~path, ~defs, ~parent): JSONSchema.t => {
    let jsonSchema: Mutable.t = {}
    switch schema {
    | String({?const, ?format}) => {
        jsonSchema.type_ = Some(Arrayable.single(#string))
        switch format {
        | Some(DateTime) => jsonSchema.format = Some("date-time")
        | Some(Email) => jsonSchema.format = Some("email")
        | Some(Uuid) => jsonSchema.format = Some("uuid")
        | Some(Url) => jsonSchema.format = Some("uri")
        | Some(Cuid) | Some(JSON) | None => ()
        }
        let internal = schema->castToInternal
        switch internal.minLength {
        | Some(v) => jsonSchema.minLength = Some(v)
        | None => ()
        }
        switch internal.maxLength {
        | Some(v) => jsonSchema.maxLength = Some(v)
        | None => ()
        }
        switch internal.pattern {
        | Some(re) => jsonSchema.pattern = Some((re->(Obj.magic: RegExp.t => {..}))["source"])
        | None => ()
        }
        switch const {
        | Some(value) => jsonSchema.const = Some(JSON.Encode.string(value))
        | None => ()
        }
      }
    | Number({?format, ?const}) => {
        let internal = schema->castToInternal
        switch format {
        | Some(Int32) => {
            jsonSchema.type_ = Some(Arrayable.single(#integer))
            jsonSchema.minimum = Some(-2147483648.)
            jsonSchema.maximum = Some(2147483647.)
          }
        | Some(Port) => {
            jsonSchema.type_ = Some(Arrayable.single(#integer))
            jsonSchema.minimum = Some(0.)
            jsonSchema.maximum = Some(65535.)
          }
        | None => jsonSchema.type_ = Some(Arrayable.single(#number))
        }
        switch internal.minimum {
        | Some(v) => jsonSchema.minimum = Some(v)
        | None => ()
        }
        switch internal.maximum {
        | Some(v) => jsonSchema.maximum = Some(v)
        | None => ()
        }
        switch const {
        | Some(value) => jsonSchema.const = Some(JSON.Encode.float(value))
        | None => ()
        }
      }
    | Boolean({?const}) => {
        jsonSchema.type_ = Some(Arrayable.single(#boolean))
        switch const {
        | Some(value) => jsonSchema.const = Some(JSON.Encode.bool(value))
        | None => ()
        }
      }
    | Array({additionalItems, items}) =>
      switch additionalItems {
      | Schema(childSchema) =>
        jsonSchema.items = Some(
          Arrayable.single(
            Schema(
              internalToJSONSchema(
                childSchema,
                ~parent=schema,
                ~path=path->Path.concat(Path.dynamic),
                ~defs,
              ),
            ),
          ),
        )
        jsonSchema.type_ = Some(Arrayable.single(#array))
        let internal = schema->castToInternal
        switch internal.minItems {
        | Some(v) => jsonSchema.minItems = Some(v)
        | None => ()
        }
        switch internal.maxItems {
        | Some(v) => jsonSchema.maxItems = Some(v)
        | None => ()
        }
      | _ => {
          let items = items->Array.mapWithIndex((itemSchema, idx) => {
            Schema(
              internalToJSONSchema(
                itemSchema,
                ~parent=schema,
                ~path=path->Path.concat(Path.fromLocation(idx->Int.toString)),
                ~defs,
              ),
            )
          })
          let itemsNumber = items->Array.length

          jsonSchema.items = Some(Arrayable.array(items))
          jsonSchema.type_ = Some(Arrayable.single(#array))
          jsonSchema.minItems = Some(itemsNumber)
          jsonSchema.maxItems = Some(itemsNumber)
        }
      }

    | Union({anyOf}) => {
        let literals = []
        let items = []

        anyOf->Array.forEach(childSchema => {
          switch childSchema {
          // Filter out undefined to support optional fields
          | Undefined(_) if (parent->castToInternal).tag === objectTag => ()
          | _ => {
              items
              ->Array.push(
                Schema(internalToJSONSchema(childSchema, ~parent=schema, ~path, ~defs)),
              )
              ->ignore
              switch childSchema->castToInternal->isLiteral {
              | true =>
                literals
                ->Array.push(
                  (childSchema->castToInternal).const->(Obj.magic: option<char> => JSON.t),
                )
                ->ignore
              | false => ()
              }
            }
          }
        })

        let itemsNumber = items->Array.length

        switch (schema->untag).default {
        | Some(default) => jsonSchema.default = Some(default->(Obj.magic: unknown => JSON.t))
        | None => ()
        }

        // TODO: Write a breaking test with itemsNumber === 0
        if itemsNumber === 1 {
          jsonSchema->Mutable.mixin(items->Array.getUnsafe(0)->Obj.magic)
        } else if literals->Array.length === itemsNumber {
          jsonSchema.enum = Some(literals)
        } else {
          jsonSchema.anyOf = Some(items)
        }
      }
    | Object({properties, additionalItems}) =>
      switch additionalItems {
      | Schema(childSchema) => {
          jsonSchema.type_ = Some(Arrayable.single(#object))
          let childJsonSchema = internalToJSONSchema(
            childSchema,
            ~path=path->Path.concat(Path.dynamic),
            ~defs,
            ~parent=schema,
          )
          jsonSchema.additionalProperties = Some(
            if (childJsonSchema->Obj.magic: dict<'a>)->Dict.keysToArray->Array.length === 0 {
              JSONSchema.Any
            } else {
              Schema(childJsonSchema)
            },
          )
        }
      | _ => {
          let required = []
          let keys = properties->Dict.keysToArray
          let jsonProperties = dict{}

          for idx in 0 to keys->Array.length - 1 {
            let key = keys->Array.getUnsafe(idx)
            let itemSchema = properties->Dict.getUnsafe(key)
            let fieldSchema = internalToJSONSchema(
              itemSchema,
              ~path=path->Path.concat(Path.fromLocation(key)),
              ~defs,
              ~parent=schema,
            )
            if itemSchema->castToInternal->isOptional->not {
              required->Array.push(key)->ignore
            }
            jsonProperties->Dict.set(key, Schema(fieldSchema))
          }

          jsonSchema.type_ = Some(Arrayable.single(#object))
          jsonSchema.properties = Some(jsonProperties)
          switch additionalItems {
          | Strict => jsonSchema.additionalProperties = Some(JSONSchema.Never)
          | Strip
          | Schema(_) => ()
          }
          switch required {
          | [] => ()
          | required => jsonSchema.required = Some(required)
          }
        }
      }
    | Ref({ref}) if ref === `${defsPath}${jsonName}` => () // S.json → empty {}
    | Ref({ref}) => jsonSchema.ref = Some(ref)
    | Null(_) => jsonSchema.type_ = Some(Arrayable.single(#null))
    | Never(_) => jsonSchema.not = Some(Schema({}))

    | _ =>
      X.Exn.throwAny(
        InternalError.make(
          B.makeInvalidInputDetails(
            ~received=if (parent->castToInternal).tag->TagFlag.get->Flag.unsafeHas(TagFlag.union) {
              parent
            } else {
              schema
            },
            ~expected=json(),
            ~path,
            ~input=%raw(`0`),
            ~includeInput=false,
          ),
        ),
      )
    }

    jsonSchema->applyMetadataOverlay(schema, ~defs)

    jsonSchema->Mutable.toReadOnly
  }
}

let toJSONSchema = schema => {
  let target = schema->castToInternal
  let defs = dict{}
  let jsonSchema =
    target
    ->castToPublic
    ->RescriptJSONSchema.internalToJSONSchema(~path=Path.empty, ~parent=target->castToPublic, ~defs)
  let _ = %raw(`delete defs.JSON`)
  let defsKeys = defs->Dict.keysToArray
  if defsKeys->Array.length->X.Int.unsafeToBool {
    // Reuse the same object to prevent allocations
    // Nothing critical, just because we can
    let jsonSchemDefs = defs->(Obj.magic: dict<t<unknown>> => dict<JSONSchema.definition>)
    defsKeys->Array.forEach(key => {
      let schema = defs->Dict.getUnsafe(key)
      jsonSchemDefs->Dict.set(
        key,
        schema
        ->RescriptJSONSchema.internalToJSONSchema(
          ~parent=schema,
          ~path=Path.empty,
          // It's not possible to have nested recursive schema.
          // It should be grouped to a single $defs of the most top-level schema.
          ~defs=%raw(`0`),
        )
        ->Schema,
      )
    })
    (jsonSchema->JSONSchema.Mutable.fromReadOnly).defs = Some(jsonSchemDefs)
  }
  jsonSchema
}

let extendJSONSchema = (schema, jsonSchema) => {
  schema->Metadata.set(
    ~id=RescriptJSONSchema.jsonSchemaMetadataId,
    switch schema->Metadata.get(~id=RescriptJSONSchema.jsonSchemaMetadataId) {
    | Some(existingSchemaExtend) => RescriptJSONSchema.merge(existingSchemaExtend, jsonSchema)
    | None => jsonSchema
    },
  )
}

let castAnySchemaToJsonableS = (Obj.magic: schema<'any> => schema<JSON.t>)
let rec fromJSONSchema: RescriptJSONSchema.t => t<JSON.t> = {
  @inline
  let primitiveToSchema = primitive => {
    Literal.parse(primitive)->castToPublic->castAnySchemaToJsonableS
  }

  let toIntSchema = (jsonSchema: JSONSchema.t) => {
    let schema = int()->castToPublic
    // TODO: Support jsonSchema.multipleOf when it's in rescript-schema
    // if (typeof jsonSchema.multipleOf === "number" && jsonSchema.multipleOf !== 1) {
    //  r += `.multipleOf(${jsonSchema.multipleOf})`;
    // }
    let schema = switch jsonSchema {
    | {minimum} => schema->intMin(minimum->Float.toInt)
    | {exclusiveMinimum} => schema->intMin((exclusiveMinimum +. 1.)->Float.toInt)
    | _ => schema
    }
    let schema = switch jsonSchema {
    | {maximum} => schema->intMax(maximum->Float.toInt)
    | {exclusiveMinimum} => schema->intMax((exclusiveMinimum -. 1.)->Float.toInt)
    | _ => schema
    }
    schema->castAnySchemaToJsonableS
  }

  let definitionToDefaultValue = (definition: JSONSchema.definition) =>
    switch definition {
    | Schema(s) => s.default
    | _ => None
    }

  (jsonSchema: JSONSchema.t) => {
    let anySchema = json()->castToPublic

    let definitionToSchema = (definition: JSONSchema.definition) =>
      switch definition {
      | Schema(s) => s->fromJSONSchema
      | Any => anySchema
      | Never => never_()->castToPublic->castToAny
      }

    let schema = switch jsonSchema {
    | _ if (jsonSchema->(Obj.magic: JSONSchema.t => {..}))["nullable"] =>
      null(
        jsonSchema
        ->RescriptJSONSchema.merge({"nullable": false}->(Obj.magic: {..} => JSONSchema.t))
        ->fromJSONSchema,
      )->castAnySchemaToJsonableS
    | {type_} if type_ === JSONSchema.Arrayable.single(#object) =>
      let schema = switch jsonSchema.properties {
      | Some(properties) =>
        let schema =
          {
            let obj = dict{}
            properties
            ->Dict.keysToArray
            ->Array.forEach(key => {
              let property = properties->Dict.getUnsafe(key)
              let propertySchema = property->definitionToSchema
              let propertySchema = switch jsonSchema.required {
              | Some(r) if r->Array.includes(key) => propertySchema
              | _ =>
                switch property->definitionToDefaultValue {
                | Some(defaultValue) =>
                  propertySchema->option->Option.getOr(defaultValue)->castAnySchemaToJsonableS
                | None => propertySchema->option->castAnySchemaToJsonableS
                }
              }
              Dict.set(obj, key, propertySchema)
            })
            obj->(Obj.magic: dict<schema<JSON.t>> => unknown)
          }
          ->Schema.definitionToSchema
          ->castToPublic
        let schema = switch jsonSchema {
        | {additionalProperties} if additionalProperties === Never => schema->strict
        | _ => schema
        }
        schema->castAnySchemaToJsonableS
      | None =>
        switch jsonSchema.additionalProperties {
        | Some(additionalProperties) =>
          switch additionalProperties {
          | Any => dict(anySchema)->castAnySchemaToJsonableS
          | Never => object(_ => ())->strict->castAnySchemaToJsonableS
          | Schema(s) => dict(s->fromJSONSchema)->castAnySchemaToJsonableS
          }
        | None => Schema.factory(_ => ())->castAnySchemaToJsonableS
        }
      }

      // TODO: jsonSchema.anyOf and jsonSchema.oneOf support
      schema
    | {type_} if type_ === JSONSchema.Arrayable.single(#array) => {
        let schema = switch jsonSchema.items {
        | Some(items) =>
          switch items->JSONSchema.Arrayable.classify {
          | Single(single) => array(single->definitionToSchema)
          | Array(array) =>
            tuple(s => array->Array.mapWithIndex((d, idx) => s.item(idx, d->definitionToSchema)))
          }
        | None => array(anySchema)
        }
        let schema = switch jsonSchema.minItems {
        | Some(min) => schema->arrayMinLength(min)
        | _ => schema
        }
        let schema = switch jsonSchema.maxItems {
        | Some(max) => schema->arrayMaxLength(max)
        | _ => schema
        }
        schema->castAnySchemaToJsonableS
      }
    | {anyOf: []} => anySchema
    | {anyOf: [d]} => d->definitionToSchema
    | {anyOf: definitions} => union(definitions->Array.map(definitionToSchema))
    | {allOf: []} => anySchema
    | {allOf: [d]} => d->definitionToSchema
    | {allOf: definitions} => anySchema->refine(data => {
        definitions->Array.every(d => {
          try {
            let _ = data->assertOrThrow(~to=d->definitionToSchema)
            true
          } catch {
          | _ => false
          }
        })
      }, ~error="Should pass for all schemas of the allOf property.")
    | {oneOf: []} => anySchema
    | {oneOf: [d]} => d->definitionToSchema
    | {oneOf: definitions} => anySchema->refine(data => {
        let validCount = ref(0)
        definitions->Array.forEach(d => {
          try {
            let _ = data->assertOrThrow(~to=d->definitionToSchema)
            validCount := validCount.contents + 1
          } catch {
          | _ => ()
          }
        })
        validCount.contents === 1
      }, ~error="Should pass exactly one schema according to the oneOf property.")
    | {not} => anySchema->refine(data => {
        try {
          let _ = data->assertOrThrow(~to=not->definitionToSchema)
          false
        } catch {
        | _ => true
        }
      }, ~error="Should NOT be valid against schema in the not property.")
    // needs to come before primitives
    | {enum: []} => anySchema
    | {enum: [p]} => p->primitiveToSchema
    | {enum: primitives} =>
      union(primitives->Array.map(primitiveToSchema))->castAnySchemaToJsonableS
    | {const} => const->primitiveToSchema
    | {type_} if type_->JSONSchema.Arrayable.isArray =>
      let types = type_->(Obj.magic: JSONSchema.Arrayable.t<'a> => array<'a>)
      union(
        types->Array.map(type_ => {
          jsonSchema
          ->RescriptJSONSchema.merge({type_: JSONSchema.Arrayable.single(type_)})
          ->fromJSONSchema
        }),
      )
    | {type_} if type_ === JSONSchema.Arrayable.single(#string) =>
      let schema = switch jsonSchema {
      | {format: "email"} => email()->castToPublic
      | {format: "uri"} => url()->castToPublic
      | {format: "uuid"} => uuid()->castToPublic
      | {format: "date-time"} => isoDateTime()->castToPublic
      | _ => string()->castToPublic
      }
      let schema = switch jsonSchema {
      | {pattern: p} => schema->pattern(RegExp.fromString(p))
      | _ => schema
      }
      let schema = switch jsonSchema {
      | {minLength} => schema->stringMinLength(minLength)
      | _ => schema
      }
      let schema = switch jsonSchema {
      | {maxLength} => schema->stringMaxLength(maxLength)
      | _ => schema
      }
      schema->castAnySchemaToJsonableS

    | {type_} if type_ === JSONSchema.Arrayable.single(#integer) => jsonSchema->toIntSchema
    | {type_, format: "int64"} if type_ === JSONSchema.Arrayable.single(#number) =>
      jsonSchema->toIntSchema
    | {type_, multipleOf: 1.} if type_ === JSONSchema.Arrayable.single(#number) =>
      jsonSchema->toIntSchema
    | {type_} if type_ === JSONSchema.Arrayable.single(#number) => {
        let schema = float()->castToPublic
        let schema = switch jsonSchema {
        | {minimum} => schema->floatMin(minimum)
        | {exclusiveMinimum} => schema->floatMin(exclusiveMinimum +. 1.)
        | _ => schema
        }
        let schema = switch jsonSchema {
        | {maximum} => schema->floatMax(maximum)
        | {exclusiveMinimum} => schema->floatMax(exclusiveMinimum -. 1.)
        | _ => schema
        }
        schema->castAnySchemaToJsonableS
      }
    | {type_} if type_ === JSONSchema.Arrayable.single(#boolean) =>
      bool()->castToPublic->castAnySchemaToJsonableS
    | {type_} if type_ === JSONSchema.Arrayable.single(#null) =>
      literal(%raw(`null`))->castAnySchemaToJsonableS
    | {if_, then, else_} => {
        let ifSchema = if_->definitionToSchema
        let thenSchema = then->definitionToSchema
        let elseSchema = else_->definitionToSchema
        anySchema->refine(data => {
          let passed = try {
            let _ = data->assertOrThrow(~to=ifSchema)
            true
          } catch {
          | _ => false
          }
          try {
            if passed {
              let _ = data->assertOrThrow(~to=thenSchema)
            } else {
              data->assertOrThrow(~to=elseSchema)
            }
            true
          } catch {
          | _ => false
          }
        }, ~error="Should pass the if/then/else schema validation.")
      }
    | _ if jsonSchema.type_ !== None =>
      InternalError.panic(`Unknown JSON Schema type: ${(jsonSchema.type_->Obj.magic: string)}`)
    | _ => anySchema
    }

    let schema = switch jsonSchema {
    | {description: _} | {deprecated: _} | {examples: _} | {title: _} =>
      schema->meta({
        title: ?jsonSchema.title,
        description: ?jsonSchema.description,
        deprecated: ?jsonSchema.deprecated,
        examples: ?jsonSchema.examples,
      })
    | _ => schema
    }

    schema
  }
}

let min = (schema, minValue, ~message as maybeMessage=?) => {
  switch schema {
  | String(_) => schema->stringMinLength(minValue, ~message=?maybeMessage)
  | Array(_) => schema->arrayMinLength(minValue, ~message=?maybeMessage)
  | Number({format: Int32 | Port}) => schema->intMin(minValue, ~message=?maybeMessage)
  | Number(_) => schema->floatMin(minValue->Obj.magic, ~message=?maybeMessage)
  | _ =>
    InternalError.panic(
      `S.min is not supported for ${schema->toExpression} schema. Coerce the schema to string, number or array using S.to first.`,
    )
  }
}

let max = (schema, maxValue, ~message as maybeMessage=?) => {
  switch schema {
  | String(_) => schema->stringMaxLength(maxValue, ~message=?maybeMessage)
  | Array(_) => schema->arrayMaxLength(maxValue, ~message=?maybeMessage)
  | Number({format: Int32 | Port}) => schema->intMax(maxValue, ~message=?maybeMessage)
  | Number(_) => schema->floatMax(maxValue->Obj.magic, ~message=?maybeMessage)
  | _ =>
    InternalError.panic(
      `S.max is not supported for ${schema->toExpression} schema. Coerce the schema to string, number or array using S.to first.`,
    )
  }
}

let length = (schema, length, ~message as maybeMessage=?) => {
  switch schema {
  | String(_) => schema->stringLength(length, ~message=?maybeMessage)
  | Array(_) => schema->arrayLength(length, ~message=?maybeMessage)
  | _ =>
    InternalError.panic(
      `S.length is not supported for ${schema->toExpression} schema. Coerce the schema to string or array using S.to first.`,
    )
  }
}

let nullAsUnit = nullAsUnit->(Obj.magic: (unit => internal) => unit => t<unit>)
let never_ = never_->(Obj.magic: (unit => internal) => unit => t<never>)
let unknown: t<unknown> = unknown->castToPublic
let unit = unit->(Obj.magic: (unit => internal) => unit => t<unit>)
let nullLiteral = nullLiteral->(Obj.magic: (unit => internal) => unit => t<unit>)
let nan = nan->(Obj.magic: (unit => internal) => unit => t<float>)
let string = string->(Obj.magic: (unit => internal) => unit => t<string>)
let bool = bool->(Obj.magic: (unit => internal) => unit => t<bool>)
let int = int->(Obj.magic: (unit => internal) => unit => t<int>)
let float = float->(Obj.magic: (unit => internal) => unit => t<float>)
let bigint = bigint->(Obj.magic: (unit => internal) => unit => t<bigint>)
let symbol = symbol->(Obj.magic: (unit => internal) => unit => t<Symbol.t>)
let date = date->(Obj.magic: (unit => internal) => unit => t<Date.t>)
let json = json->(Obj.magic: (unit => internal) => unit => t<JSON.t>)
let jsonString = jsonString->(Obj.magic: (unit => internal) => unit => t<string>)
let jsonStringWithSpace = jsonStringWithSpace->(Obj.magic: (int => t<'a>) => int => t<string>)
let uint8Array = uint8Array->(Obj.magic: (unit => internal) => unit => t<Uint8Array.t>)
let isoDateTime = isoDateTime->(Obj.magic: (unit => internal) => unit => t<string>)
let port = port->(Obj.magic: (unit => internal) => unit => t<int>)
let email = email->(Obj.magic: (unit => internal) => unit => t<string>)
let uuid = uuid->(Obj.magic: (unit => internal) => unit => t<string>)
let cuid = cuid->(Obj.magic: (unit => internal) => unit => t<string>)
let url = url->(Obj.magic: (unit => internal) => unit => t<string>)
