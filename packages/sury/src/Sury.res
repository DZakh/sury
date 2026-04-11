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

    // external unsafeToBool: option<'a> => bool = "%identity"
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

    @val external internalClass: Js.Types.obj_val => string = "Object.prototype.toString.call"

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

    @get_index
    external getUnsafeOptionByString: (array<'a>, string) => option<'a> = ""

    @get_index
    external getUnsafeOption: (array<'a>, int) => option<'a> = ""

    @inline
    let has = (array, idx) => {
      array->Js.Array2.unsafe_get(idx)->(Obj.magic: 'a => bool)
    }

    let isArray = Js.Array2.isArray

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
      (int1->Js.Int.toFloat +. int2->Js.Int.toFloat)->(Obj.magic: float => int)
    }

    external unsafeToString: int => string = "%identity"
    external unsafeToBool: int => bool = "%identity"
  }

  module String = {
    let capitalize = (string: string): string => {
      string->Js.String2.slice(~from=0, ~to_=1)->Js.String2.toUpperCase ++
        string->Js.String2.sliceToEnd(~from=1)
    }

    external unsafeToBool: string => bool = "%identity"
  }

  module Dict = {
    let copy: dict<'a> => dict<'a> = %raw(`(d) => ({...d})`)

    @val
    external mixin: (dict<'a>, dict<'a>) => dict<'a> = "Object.assign"

    @get_index
    external getUnsafeOption: (dict<'a>, string) => option<'a> = ""

    @set_index
    external setByInt: (dict<'a>, int, 'a) => unit = ""

    @get_index
    external getUnsafeOptionByInt: (dict<'a>, int) => option<'a> = ""

    @get_index
    external getUnsafeOptionBySymbol: (dict<'a>, Js.Types.symbol) => option<'a> = ""
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
    type t = Js.Types.symbol

    @val external make: string => t = "Symbol"
  }

  module Inlined = {
    module Value = {
      let fromString = (string: string): string => {
        let rec loop = idx => {
          switch string->Js.String2.get(idx)->(Obj.magic: string => option<string>) {
          | None => `"${string}"`
          | Some("\"") | Some("\n") => string->Js.Json.stringifyAny->Obj.magic
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
      ->Js.String2.split(`"]["`)
      ->Js.Array2.joinWith(`","`)
      ->Js.Json.parseExn
      ->(Obj.magic: Js.Json.t => array<string>)
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
    | _ => array->Js.Array2.map(fromLocation)->Js.Array2.joinWith("")
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
// let functionTag: tag = %raw(`"function"`)
let instanceTag: tag = %raw(`"instance"`)
let arrayTag: tag = %raw(`"array"`)
let objectTag: tag = %raw(`"object"`)
let unionTag: tag = %raw(`"union"`)
let neverTag: tag = %raw(`"never"`)
let unknownTag: tag = %raw(`"unknown"`)
let refTag: tag = %raw(`"ref"`)

type standard = {
  version: int,
  vendor: string,
  validate: 'any 'value. 'any => {"value": 'value},
}

type internalDefault = {}

type numberFormat = | @as("int32") Int32 | @as("port") Port
type stringFormat = | @as("json") JSON | @as("date-time") DateTime
type arrayFormat = | @as("compactColumns") CompactColumns

type format = | ...numberFormat | ...stringFormat | ...arrayFormat

@unboxed
type additionalItemsMode = | @as("strip") Strip | @as("strict") Strict

@tag("type")
type rec t<'value> =
  private
  | @as("never") Never({name?: string, title?: string, description?: string, deprecated?: bool})
  | @as("unknown")
  Unknown({
      name?: string,
      description?: string,
      title?: string,
      deprecated?: bool,
      examples?: array<unknown>,
      default?: unknown,
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
    })
  | @as("null")
  Null({
      const: Js.Types.null_val,
      name?: string,
      title?: string,
      description?: string,
      deprecated?: bool,
    })
  | @as("undefined")
  Undefined({
      const: unit,
      name?: string,
      title?: string,
      description?: string,
      deprecated?: bool,
    })
  | @as("nan")
  NaN({
      const: float,
      name?: string,
      title?: string,
      description?: string,
      deprecated?: bool,
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
    }) // TODO: Add const for Object and Tuple
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
    })
  | @as("ref")
  Ref({
      @as("$ref")
      ref: string,
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
  mutable inputRefiner?: (~input: val) => string,
  // Custom validations on output (after decoder)
  mutable refiner?: (~input: val) => string,
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
and meta<'value> = {
  name?: string,
  title?: string,
  description?: string,
  deprecated?: bool,
  examples?: array<'value>,
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
  // Whether the val is at input part of expected schema
  // This means that when decoding we need to do it from input to output instead of unknown to output
  @as("ii")
  mutable isInput?: bool,
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
  mutable validation: option<(~inputVar: string) => string>,
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
  // When the value conversion between two schemas is not supported
  | @as("unsupported_conversion")
  UnsupportedConversion({
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
  @inline
  let without = (flags, flag) => flags->with(flag)->Int.bitwiseXor(flag)

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
  @inline let never = 16384
  @inline let symbol = 32768

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
    [neverTag]: 16384,
    [symbolTag]: 32768,
  }`)

  @inline
  let get = (tag: tag) => flags->Js.Dict.unsafeGet((tag :> string))
}

let rec stringify = unknown => {
  let tagFlag = unknown->Type.typeof->(Obj.magic: Type.t => tag)->TagFlag.get

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
        string := string.contents ++ array->Js.Array2.unsafe_get(i)->stringify
      }
      string.contents ++ "]"
    } else if (
      (unknown->(Obj.magic: 'a => {"constructor": unknown}))["constructor"] === %raw("Object")
    ) {
      let dict = unknown->(Obj.magic: unknown => dict<unknown>)
      let keys = Js.Dict.keys(dict)
      let string = ref("{ ")
      for i in 0 to keys->Array.length - 1 {
        let key = keys->Js.Array2.unsafe_get(i)
        let value = dict->Js.Dict.unsafeGet(key)
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
    ->Js.Array2.map(toExpression)
    ->Js.Array2.joinWith(" | ")
  | {format: ?Some(CompactColumns), ?to, ?additionalItems} =>
    // For compactColumns, show the column types if we have properties from .to
    switch to {
    | Some(toSchema) =>
      switch toSchema.properties {
      | Some(props) =>
        let keys = props->Js.Dict.keys
        `[${keys
          ->Js.Array2.map(key => {
            let propSchema = props->Js.Dict.unsafeGet(key)->castToPublic
            `${propSchema->toExpression}[]`
          })
          ->Js.Array2.joinWith(", ")}]`
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
    let locations = properties->Js.Dict.keys
    if locations->Js.Array2.length === 0 {
      if additionalItems->Js.typeof === (objectTag :> string) {
        let additionalItems: internal = additionalItems->Obj.magic
        `{ [key: string]: ${additionalItems->castToPublic->toExpression}; }`
      } else {
        `{}`
      }
    } else {
      `{ ${locations
        ->Js.Array2.map(location => {
          `${location}: ${properties->Js.Dict.unsafeGet(location)->castToPublic->toExpression};`
        })
        ->Js.Array2.joinWith(" ")} }`
    }

  | {tag: NaN} => "NaN"
  // Case for val
  | {tag} if %raw(`schema.b`) => (tag :> string)
  | {tag: Array, ?items, ?additionalItems} =>
    let items = items->X.Option.getUnsafe
    if additionalItems->Js.typeof === (objectTag :> string) {
      let additionalItems: internal = additionalItems->Obj.magic
      let itemName = additionalItems->castToPublic->toExpression
      if (additionalItems.tag :> string) === (unionTag :> string) {
        `(${itemName})`
      } else {
        itemName
      } ++ "[]"
    } else {
      `[${items
        ->Js.Array2.map(schema => schema->castToPublic->toExpression)
        ->Js.Array2.joinWith(", ")}]`
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

let valueOptions = Js.Dict.empty()
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
    valueOptions->Js.Dict.set(valKey, s->Obj.magic)
    let _ = X.Object.defineProperty(s, reversedKey, valueOptions->Obj.magic)
  }
  s
}

let shakenRef = "as"

let shakenTraps: X.Proxy.traps<internal> = {
  get: (~target, ~prop) => {
    switch target->Obj.magic->X.Dict.getUnsafeOption(shakenRef) {
    | Some(l) if prop !== shakenRef->Obj.magic =>
      InternalError.panic(
        `Schema S.${l} is not enabled. To start using it, add S.enable${l->X.String.capitalize}() at the project root.`,
      )
    | _ => target->Obj.magic->Js.Dict.unsafeGet(prop->Obj.magic)
    }
  },
}

let shaken = (apiName: string) => {
  let mut = base(neverTag, ~selfReverse=true)
  mut->Obj.magic->Js.Dict.set(shakenRef, apiName)
  mut->X.Proxy.make(shakenTraps)
}

let noopDecoder = (~input) => input

let unknown = base(unknownTag, ~selfReverse=true)
unknown.decoder = noopDecoder
let bool = base(booleanTag, ~selfReverse=true)
let symbol = base(symbolTag, ~selfReverse=true)
let string = base(stringTag, ~selfReverse=true)
let int = base(numberTag, ~selfReverse=true)
int.format = Some(Int32)
let float = base(numberTag, ~selfReverse=true)
let bigint = base(bigintTag, ~selfReverse=true)
let unit = base(undefinedTag, ~selfReverse=true)
unit.const = %raw(`void 0`)
let nullLiteral = base(nullTag, ~selfReverse=true)
nullLiteral.const = %raw(`null`)
let nan = base(nanTag, ~selfReverse=true)
nan.const = %raw(`NaN`)

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
      let l = e->Js.Array2.length
      e->Js.Array2.unsafe_set(l, value->castAnyToUnknown)
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
          global->(Obj.magic: bGlobal => dict<string>)->Js.Dict.set(key, inlinedLocation)
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
      let v = val.global->varWithoutAllocation
      (val.parent->X.Option.getUnsafe).allocate(`${v}=${val.inline}`)
      val.var = _var
      val.inline = v
      v
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
        validation: None,
        path: Path.empty,
        global: {
          ?defs,
          flag,
          embeded: [],
          varCounter: -1,
        },
      }
    }

    let throw = (b: val, errorDetails) => {
      X.Exn.throwAny(InternalError.make(errorDetails))
    }

    let failWithArg = (b: val, fn: 'arg => errorDetails, arg) => {
      `${b->embed(arg => {
          b->throw(fn(arg))
        })}(${arg})`
    }

    let makeInvalidConversionDetails = (~input, ~to, ~cause) => {
      if %raw("cause&&cause.s===s") {
        let error: error = cause->Obj.magic

        // Read about this in shouldPrependPathKey comment.
        if !(cause->Obj.magic->Js.Dict.unsafeGet(shouldPrependPathKey)) {
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

    let makeInvalidInputDetails = (
      ~expected,
      ~received,
      ~path,
      ~input,
      ~includeInput,
      ~unionErrors=?,
    ) => {
      let reasonRef = ref(
        `Expected ${expected
          ->castToPublic
          ->toExpression}, received ${if includeInput {
            input->stringify
          } else {
            received->toExpression
          }}`,
      )
      switch unionErrors {
      | Some(caseErrors) => {
          let reasonsDict = Js.Dict.empty()
          for idx in 0 to caseErrors->Js.Array2.length - 1 {
            let caseError = caseErrors->Js.Array2.unsafe_get(idx)
            let caseReason = caseError.reason->Stdlib.String.split("\n")->Js.Array2.joinWith("\n  ")
            let location = switch caseError.path {
            | "" => ""
            | nonEmptyPath => `At ${nonEmptyPath}: `
            }
            let line = `\n- ${location}${caseReason}`
            if reasonsDict->Js.Dict.unsafeGet(line)->X.Int.unsafeToBool->not {
              reasonsDict->Js.Dict.set(line, 1)
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

    let embedInvalidInput = (~input: val, ~expected=input.expected) => {
      let received = input.schema->castToPublic

      input->failWithArg(
        value =>
          makeInvalidInputDetails(
            ~expected,
            ~received,
            ~path=input.path,
            ~input=value,
            ~includeInput=true,
          ),
        input.var(),
      )
    }

    let merge = (val: val): string => {
      let current = ref(Some(val))
      let code = ref("")

      while current.contents !== None {
        let val = current.contents->X.Option.getUnsafe
        current := val.prev

        let currentCode = ref("")

        switch val.validation {
        | Some(validation) if val.expected.noValidation !== Some(true) => {
            // Validation must be used only when there's a prev value
            let input = current.contents->X.Option.getUnsafe
            let inputVar = input.var()
            let validationCode = validation(~inputVar)
            currentCode :=
              `${validationCode}||${embedInvalidInput(~input, ~expected=val.expected)};`
          }
        | _ => ()
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

    let appendValidation = (validation1, validation2) => {
      Some(
        (~inputVar) => {
          {
            switch validation1 {
            | Some(prevValidation) => prevValidation(~inputVar) ++ "&&"
            | None => ""
            }
          } ++
          validation2(~inputVar)
        },
      )
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
        validation: None,
        path: prev.path,
        global: prev.global,
        hasTransform: true,
        vals: ?prev.vals,
      }
    }

    let refine = (val: val, ~schema=val.schema, ~validation=?, ~expected=val.expected) => {
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
        validation,
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
        validation: None,
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

          objectVal.codeFromPrev = objectVal.codeFromPrev ++ val->merge
          objectVal.vals->X.Option.getUnsafe->Js.Dict.set(location, val)
        }

        let merge = (target: t, vals: dict<val>) => {
          let locations = vals->Js.Dict.keys
          for idx in 0 to locations->Js.Array2.length - 1 {
            let location = locations->Js.Array2.unsafe_get(idx)
            target->add(~location, vals->Js.Dict.unsafeGet(location))
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
          validation: None,
          isInput: ?val.isInput,
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
            let d = Js.Dict.empty()
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
              validation: None,
              path: parent.path->Path.concat(pathAppend),
              global: parent.global,
              parent,
            }
            vals->Js.Dict.set(location, item)
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
          input->throw(
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

    let fail = (b: val, ~message) => {
      `${b->embed(() => {
          b->throw(Custom({reason: message, path: b.path}))
        })}()`
    }

    let effectCtx = (input: val) => {
      fail: (message, ~path=Path.empty) => {
        let error = InternalError.make(
          Custom({
            reason: message,
            path: input.path->Path.concat(path),
          }),
        )
        // Read about this in shouldPrependPathKey comment.
        error->Obj.magic->Js.Dict.set(shouldPrependPathKey, 1)
        Stdlib.JsExn.throw(error)
      },
    }

    let invalidOperation = (val: val, ~description) => {
      val->throw(InvalidOperation({reason: description, path: val.path}))
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

    let withPathPrepend = (
      ~input,
      ~dynamicLocationVar as maybeDynamicLocationVar=?,
      ~appendSafe=?,
      fn,
    ) => {
      if input.path === Path.empty && maybeDynamicLocationVar === None {
        fn(~input)
      } else {
        fn(~input)

        // try b->mergeWithCatch(
        //   ~path=Path.empty,
        //   ~input,
        //   ~catch=(b, ~errorVar) => {
        // b.codeAfterValidation = `${errorVar}.path=${b.path->X.Inlined.Value.fromString}+${switch maybeDynamicLocationVar {
        //   | Some(var) => `'["'+${var}+'"]'+`
        //   | _ => ""
        //   }}${errorVar}.path`
        //     None
        //   },
        //   ~appendSafe?,
        //   b => {
        //     fn(b, ~input)
        //   },
        // ) catch {
        // | _ =>
        //   let error = %raw(`exn`)->InternalError.getOrRethrow
        //   X.Exn.throwAny(
        //     InternalError.make(
        //       ~path=b.path->Path.concat(Path.dynamic)->Path.concat(error.path),
        //       ~code=error.codeAfterValidation,
        //       ~flag=error.flag,
        //     ),
        //   )
        // }
      }
    }

    let unsupportedConversion = (b, ~from: internal, ~target: internal) => {
      b->throw(
        UnsupportedConversion({
          from: from->castToPublic,
          to: target->castToPublic,
          reason: `Unsupported conversion from ${from->castToPublic->toExpression} to ${target
            ->castToPublic
            ->toExpression}`,
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

let inputToString = (input: val) => {
  input->B.next(`""+${input.inline}`, ~schema=string)
}

let int32FormatValidation = (~inputVar) => {
  `${inputVar}<=2147483647&&${inputVar}>=-2147483648&&${inputVar}%1===0`
}

let numberDecoder = Builder.make((~input) => {
  let inputTagFlag = input.schema.tag->TagFlag.get
  if inputTagFlag->Flag.unsafeHas(TagFlag.unknown) {
    input->B.refine(~schema=input.expected, ~validation=(~inputVar) => {
      `typeof ${inputVar}==="${(numberTag :> string)}"` ++
      {
        switch input.expected.format {
        | Some(Int32) => `&&${int32FormatValidation(~inputVar)}`

        | _ =>
          if input.global.flag->Flag.unsafeHas(Flag.disableNanNumberValidation) {
            ""
          } else {
            `&&!Number.isNaN(${inputVar})`
          }
        }
      }
    })
  } else if inputTagFlag->Flag.unsafeHas(TagFlag.string) {
    let outputVar = input.global->B.varWithoutAllocation
    input.allocate(`${outputVar}=+${input.var()}`)

    let output = input->B.next(outputVar, ~schema=input.expected)
    output.var = B._var

    output.validation = Some(
      (~inputVar as _) => {
        switch input.expected.format {
        | Some(Int32) => int32FormatValidation(~inputVar=outputVar)
        | _ => `!Number.isNaN(${outputVar})`
        }
      },
    )
    output
  } else if !(inputTagFlag->Flag.unsafeHas(TagFlag.number)) {
    input->B.unsupportedConversion(~from=input.schema, ~target=input.expected)
  } else if input.schema.format !== input.expected.format && input.expected.format === Some(Int32) {
    input->B.refine(~schema=input.expected, ~validation=(~inputVar) => {
      int32FormatValidation(~inputVar)
    })
  } else {
    input
  }
})

float.decoder = numberDecoder
int.decoder = numberDecoder

let stringDecoder = Builder.make((~input) => {
  let inputTagFlag = input.schema.tag->TagFlag.get
  if inputTagFlag->Flag.unsafeHas(TagFlag.unknown) {
    input->B.refine(~schema=input.expected, ~validation=(~inputVar) => {
      `typeof ${inputVar}==="${(stringTag :> string)}"`
    })
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
    input->B.unsupportedConversion(~from=input.schema, ~target=input.expected)
  } else {
    input
  }
})

string.decoder = stringDecoder

let booleanDecoder = Builder.make((~input) => {
  let inputTagFlag = input.schema.tag->TagFlag.get
  if inputTagFlag->Flag.unsafeHas(TagFlag.unknown) {
    input->B.refine(~schema=input.expected, ~validation=(~inputVar) => {
      `typeof ${inputVar}==="${(booleanTag :> string)}"`
    })
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
    input->B.unsupportedConversion(~from=input.schema, ~target=input.expected)
  } else {
    input
  }
})

bool.decoder = booleanDecoder

let bigintDecoder = Builder.make((~input) => {
  let inputTagFlag = input.schema.tag->TagFlag.get

  if inputTagFlag->Flag.unsafeHas(TagFlag.unknown) {
    input->B.refine(~schema=input.expected, ~validation=(~inputVar) => {
      `typeof ${inputVar}==="${(bigintTag :> string)}"`
    })
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
    input->B.unsupportedConversion(~from=input.schema, ~target=input.expected)
  } else {
    input
  }
})

bigint.decoder = bigintDecoder

let symbolDecoder = Builder.make((~input) => {
  let inputTagFlag = input.schema.tag->TagFlag.get
  if inputTagFlag->Flag.unsafeHas(TagFlag.unknown) {
    input->B.refine(~schema=input.expected, ~validation=(~inputVar) => {
      `typeof ${inputVar}==="${(symbolTag :> string)}"`
    })
  } else if !(inputTagFlag->Flag.unsafeHas(TagFlag.symbol)) {
    input->B.unsupportedConversion(~from=input.schema, ~target=input.expected)
  } else {
    input
  }
})

symbol.decoder = symbolDecoder

let setHas = (has, tag: tag) => {
  has->Js.Dict.set(
    tag->TagFlag.get->Flag.unsafeHas(TagFlag.union->Flag.with(TagFlag.ref))
      ? (unknownTag: tag :> string)
      : (tag: tag :> string),
    true,
  )
}

let jsonName = `JSON`

let jsonString = shaken("jsonString")

let jsonStringWithSpace = (space: int) => {
  let mut = jsonString->copySchema
  mut.space = Some(space)
  mut->castToPublic
}

let json = shaken("json")

module Literal = {
  let literalDecoder = Builder.make((~input) => {
    let expectedSchema = input.expected
    if expectedSchema.noValidation->X.Option.getUnsafe {
      input->B.nextConst(~schema=expectedSchema)
    } else if input.schema->isLiteral {
      // FIXME: test NaN case
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
        // This is to have a nicer error message
        let stringConstSchema = base(stringTag, ~selfReverse=false)
        stringConstSchema.const = %raw(`"" + expectedSchema.const`)

        let stringConstVal =
          input->B.nextConst(~schema=stringConstSchema, ~expected=stringConstSchema)

        // FIXME: Test, that when from item has a refinement
        // and we need to keep existing validation
        // S.string->S.check->S.to(S.literal(false))
        stringConstVal.validation = Some(
          (~inputVar) => {
            `${inputVar}==="${stringConstSchema.const->Obj.magic}"`
          },
        )

        stringConstVal->B.nextConst(~schema=expectedSchema, ~expected=expectedSchema)
      } else if schemaTagFlag->Flag.unsafeHas(TagFlag.nan) {
        input->B.refine(~schema=expectedSchema, ~validation=(~inputVar) => {
          `Number.isNaN(${inputVar})`
        })
      } else {
        // TODO: Determine impossible cases during compilation
        input->B.refine(~schema=expectedSchema, ~validation=(~inputVar) => {
          `${inputVar}===${input->B.inlineConst(expectedSchema)}`
        })
      }
    }
  })

  nullLiteral.decoder = literalDecoder
  unit.decoder = literalDecoder
  nan.decoder = literalDecoder

  let parse = (value): internal => {
    let value = value->castAnyToUnknown
    if value === %raw(`null`) {
      nullLiteral
    } else {
      switch value->Type.typeof {
      | #undefined => unit
      | #number if value->(Obj.magic: unknown => float)->Js.Float.isNaN => nan
      | #object => {
          let s = base(instanceTag, ~selfReverse=true)
          s.class = (value->Obj.magic)["constructor"]
          s.const = value->Obj.magic
          s.decoder = literalDecoder
          s
        }
      | typeof => {
          let s = base(typeof->(Obj.magic: Type.t => tag), ~selfReverse=true)
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

    // Js.log(loopInput)
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
        !(loopInput.isInput->Option.getUnsafe) &&
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

        // If the decoder's return value is not marked as isOutput,
        // we treat it as a primitive decoder with no internal transformations.
        // Otherwise, we assume internal transformations are present,
        // and expect the decoder itself to handle refinements and manage isInput/isOutput flags.
        if !(valRef.contents.isOutput->Option.getUnsafe) {
          valRef.contents.isInput = Some(true)
          valRef.contents.isOutput = Some(true)

          // FIXME: This is not well-thought + we also have custom merge logic in union decoder which should be updated
          if valRef.contents.expected.inputRefiner->Obj.magic {
            valRef.contents.codeFromPrev =
              valRef.contents.codeFromPrev ++
              (valRef.contents.expected.inputRefiner->X.Option.getUnsafe)(~input=valRef.contents)
          }
          if valRef.contents.expected.refiner->Obj.magic {
            valRef.contents.codeFromPrev =
              valRef.contents.codeFromPrev ++
              (valRef.contents.expected.refiner->X.Option.getUnsafe)(~input=valRef.contents)
          }
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

    // Js.log(inlinedFunction)

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
        let newItems = Belt.Array.makeUninitializedUnsafe(items->Js.Array2.length)
        for idx in 0 to items->Js.Array2.length - 1 {
          newItems->Js.Array2.unsafe_set(idx, items->Js.Array2.unsafe_get(idx)->reverse)
        }
        mut.items = Some(newItems)

      | None => ()
      }
      switch mut.properties {
      | Some(properties) => {
          let newProperties = Js.Dict.empty()
          let keys = properties->Js.Dict.keys
          for idx in 0 to keys->Js.Array2.length - 1 {
            let key = keys->Js.Array2.unsafe_get(idx)
            newProperties->Js.Dict.set(key, properties->Js.Dict.unsafeGet(key)->reverse)
          }
          mut.properties = Some(newProperties)
        }
      // Skip tuple
      | None => ()
      }
      if mut.additionalItems->Type.typeof === #object {
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
        let has = Js.Dict.empty()
        let newAnyOf = []
        for idx in 0 to anyOf->Js.Array2.length - 1 {
          let s = anyOf->Js.Array2.unsafe_get(idx)
          let reversed = s->reverse
          newAnyOf->Js.Array2.push(reversed)->ignore
          has->setHas(reversed.tag)
        }
        mut.has = Some(has)
        mut.anyOf = Some(newAnyOf)
      | None => ()
      }
      switch mut.defs {
      | Some(defs) => {
          let reversedDefs = Js.Dict.empty()
          for idx in 0 to defs->Js.Dict.keys->Js.Array2.length - 1 {
            let key = defs->Js.Dict.keys->Js.Array2.unsafe_get(idx)
            reversedDefs->Js.Dict.set(key, defs->Js.Dict.unsafeGet(key)->reverse)
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
    valueOptions->Js.Dict.set(valKey, r->Obj.magic)
    let _ = X.Object.defineProperty(schema, reversedKey, valueOptions->Obj.magic)
    valueOptions->Js.Dict.set(valKey, schema->Obj.magic)
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
    let arg = args->Js.Array2.unsafe_get(idx.contents)
    if !(arg->Obj.magic) {
      let f = globalConfig.defaultFlag
      flag := Some(f)
      keyRef := keyRef.contents ++ "-" ++ f->X.Int.unsafeToString
    } else if Js.typeof(arg->Obj.magic) === "number" {
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
        let schema = ref(args->Js.Array2.unsafe_get(idx.contents - 1))
        for i in idx.contents - 2 downto 0 {
          let to = schema.contents
          schema :=
            args
            ->Js.Array2.unsafe_get(i)
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
        valueOptions->Js.Dict.set(valKey, f)
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
            properties: Js.Dict.empty(),
            additionalItems: Strict,
            decoder: objectDecoder,
          }
        },
    expected: prev.expected,
    vals: Js.Dict.empty(),
    hasTransform: true,
    codeFromPrev: "",
    varsAllocation: "",
    allocate: B.initialAllocate,
    validation: None,
    path: prev.path,
    global: prev.global,
  }
}
and completeObjectVal = (objectVal: B.Val.Object.t) => {
  let isArray = objectVal.schema.tag === arrayTag
  let inline = ref("")
  let promiseAllContent = ref("")
  let optionalSettingCode = ref(None)

  let keys = objectVal.vals->X.Option.getUnsafe->Js.Dict.keys

  for idx in 0 to keys->Js.Array2.length - 1 {
    let key = keys->Js.Array2.unsafe_get(idx)
    let val = objectVal.vals->X.Option.getUnsafe->Js.Dict.unsafeGet(key)
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
  let expectedLength = expectedItems->Js.Array2.length

  let input = if unknownInputTagFlag->Flag.unsafeHas(TagFlag.unknown->Flag.with(TagFlag.array)) {
    let validation = ref(None)
    let isArrayInput = unknownInputTagFlag->Flag.unsafeHas(TagFlag.array)
    let schema = if !isArrayInput {
      validation := Some((~inputVar) => `Array.isArray(${inputVar})`)
      array(unknown->castToPublic)->castToInternal
    } else {
      unknownInput.schema
    }

    let isExactSize = switch schema.additionalItems->X.Option.getUnsafe {
    | Schema(_) => false
    | _ => schema.items->X.Option.getUnsafe->Js.Array2.length === expectedLength
    }

    if !isExactSize {
      switch expectedSchema.additionalItems->X.Option.getUnsafe {
      | Strict =>
        validation :=
          validation.contents->B.appendValidation((~inputVar) =>
            `${inputVar}.length===${expectedLength->X.Int.unsafeToString}`
          )
      | Strip =>
        validation :=
          validation.contents->B.appendValidation((~inputVar) =>
            `${inputVar}.length>=${expectedLength->X.Int.unsafeToString}`
          )

      | _ => ()
      }
    }
    switch validation.contents {
    | Some(validation) => unknownInput->B.refine(~schema, ~validation)
    // Apply refine also here,
    // so literals for union cases don't mutate input
    // FIXME: This should be removed and validation be attached to output
    | None => unknownInput->B.refine
    }
  } else {
    unknownInput->B.unsupportedConversion(~from=unknownInput.schema, ~target=expectedSchema)
  }

  switch expectedSchema.additionalItems->X.Option.getUnsafe {
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
        | _ => input.schema.items->X.Option.getUnsafe->Js.Array2.length !== expectedLength
        }
      | _ => true
      },
    )

    for idx in 0 to expectedLength - 1 {
      let schema = expectedItems->Js.Array2.unsafe_get(idx)
      let key = idx->Js.Int.toString
      let itemInput = input->B.Val.get(key)
      itemInput.expected = schema
      itemInput.isInput = Some(false)
      itemInput.isOutput = Some(false)
      itemInput.isUnion = Some(isUnion) // We want to controll validation on the decoder side
      let itemOutput = itemInput->parse

      switch itemOutput.validation {
      | Some(validation) if isUnion && schema->isLiteral =>
        input.validation =
          input.validation->B.appendValidation((~inputVar) => {
            validation(
              ~inputVar=inputVar ++ input.global->B.inlineLocation(key)->Path.fromInlinedLocation,
            )
          })
        itemOutput.validation = None
      | _ => ()
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
}
and objectDecoder: Builder.t = (~input as unknownInput) => {
  let isUnion = unknownInput.isUnion->X.Option.getUnsafe
  let expectedSchema = unknownInput.expected

  let unknownInputTagFlag = unknownInput.schema.tag->TagFlag.get

  let input = if unknownInputTagFlag->Flag.unsafeHas(TagFlag.unknown->Flag.with(TagFlag.object)) {
    let validation = ref(None)
    let isObjectInput = unknownInputTagFlag->Flag.unsafeHas(TagFlag.object)
    let schema = if !isObjectInput {
      // TODO: Use dictFactory here
      validation :=
        Some(
          (~inputVar) =>
            `typeof ${inputVar}==="${(objectTag :> string)}"&&${inputVar}`,
        )
      let mut = base(objectTag, ~selfReverse=false)
      mut.properties = Some(X.Object.immutableEmpty)
      mut.additionalItems = Some(Schema(unknown->castToPublic))
      mut
    } else {
      unknownInput.schema
    }

    if !isObjectInput && expectedSchema.additionalItems->X.Option.getUnsafe !== Strip {
      // For strip case we recreate the value
      // For other cases we might optimize it,
      // this is why the check is a must have
      validation :=
        validation.contents->B.appendValidation((~inputVar) =>
          `!Array.isArray(${inputVar})`
        )
    }

    switch validation.contents {
    | Some(validation) => unknownInput->B.refine(~schema, ~validation)
    // Apply refine also here,
    // so literals for union cases don't mutate input
    | None => unknownInput->B.refine
    }
  } else {
    unknownInput->B.unsupportedConversion(~from=unknownInput.schema, ~target=expectedSchema)
  }

  switch expectedSchema.additionalItems->X.Option.getUnsafe {
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
      let keys = Js.Dict.keys(properties)
      let keysCount = keys->Js.Array2.length

      let objectVal = input->makeObjectVal(~schema=expectedSchema)
      let shouldRecreateInput = ref(
        switch expectedSchema.additionalItems->X.Option.getUnsafe {
        // Since we have a check validating the exact properties existence
        | Strict => false
        | Strip =>
          switch input.schema.additionalItems->X.Option.getUnsafe {
          | Schema(_) => true
          | _ =>
            input.schema.properties->X.Option.getUnsafe->Js.Dict.keys->Js.Array2.length !==
              keysCount
          }
        | _ => true
        },
      )

      for idx in 0 to keysCount - 1 {
        let key = keys->Js.Array2.unsafe_get(idx)
        let schema = properties->Js.Dict.unsafeGet(key)

        let itemInput = input->B.Val.get(key)
        itemInput.expected = schema
        itemInput.isInput = Some(false)
        itemInput.isOutput = Some(false)
        itemInput.isUnion = Some(isUnion) // We want to controll validation on the decoder side
        let itemOutput = itemInput->parse

        switch itemOutput.validation {
        | Some(validation) if isUnion && schema->isLiteral =>
          input.validation =
            input.validation->B.appendValidation((~inputVar) => {
              validation(
                ~inputVar=inputVar ++ input.global->B.inlineLocation(key)->Path.fromInlinedLocation,
              )
            })
          itemOutput.validation = None
        | _ => ()
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
          for idx in 0 to keys->Js.Array2.length - 1 {
            let key = keys->Js.Array2.unsafe_get(idx)
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
}

let recursiveDecoder = Builder.make((~input) => {
  let expectedSchema = input.expected

  let schemaRef = expectedSchema.ref->X.Option.getUnsafe
  let defs = input.global.defs->X.Option.getUnsafe
  // Ignore #/$defs/
  let identifier = schemaRef->Js.String2.sliceToEnd(~from=8)
  let def = defs->Js.Dict.unsafeGet(identifier)
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
        configurableValueOptions->Js.Dict.set(valKey, 0->Obj.magic)
        let _ = X.Object.defineProperty(def, key, configurableValueOptions->Obj.magic)

        // Compile
        let fn = compileDecoder(~schema=inputSchema, ~expected=def, ~flag, ~defs=Some(defs))

        // Cache result
        valueOptions->Js.Dict.set(valKey, fn)
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
  output.prev = Some(input)

  output
})

let instanceDecoder = Builder.make((~input) => {
  let inputTagFlag = input.schema.tag->TagFlag.get
  if inputTagFlag->Flag.unsafeHas(TagFlag.unknown) {
    input->B.refine(~schema=input.expected, ~validation=(~inputVar) => {
      `${inputVar} instanceof ${input->B.embed(input.expected.class)}`
    })
  } else if (
    inputTagFlag->Flag.unsafeHas(TagFlag.instance) && input.schema.class === input.expected.class
  ) {
    input
  } else {
    input->B.unsupportedConversion(~from=input.schema, ~target=input.expected)
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

let makeConvertOrThrow = (type from to, from: t<from>, to: t<to>, ~flag=?): (from => to) => {
  getDecoder2(~s1=from->castToInternal->reverse, ~s2=to->castToInternal, ~flag?)
}
let makeAsyncConvertOrThrow = (type from to, from: t<from>, to: t<to>, ~flag=Flag.none): (
  from => promise<to>
) =>
  getDecoder2(
    ~s1=from->castToInternal->reverse,
    ~s2=to->castToInternal,
    ~flag=flag->Flag.with(Flag.async),
  )

// =============
// Operations
// =============

@inline
let parseOrThrow = (any, schema) => {
  getDecoder2(~s1=unknown, ~s2=schema->castToInternal)(any)
}

let parseJsonOrThrow = (any, schema) => {
  getDecoder2(~s1=json, ~s2=schema->castToInternal)(any)
}

let parseJsonStringOrThrow = (any, schema) => {
  getDecoder2(~s1=jsonString, ~s2=schema->castToInternal)(any)
}

let parseAsyncOrThrow = (any, schema) => {
  getDecoder2(~s1=unknown, ~s2=schema->castToInternal, ~flag=Flag.async)(any)
}

let convertOrThrow = (input, schema) => {
  getDecoder(~s1=schema->castToInternal)(input)
}

let convertToJsonOrThrow = (input, schema) => {
  getDecoder2(~s1=schema->castToInternal, ~s2=json)(input)
}

let convertToJsonStringOrThrow = (input, schema) => {
  getDecoder2(~s1=schema->castToInternal, ~s2=jsonString)(input)
}

let convertAsyncOrThrow = (input, schema) => {
  getDecoder(~s1=schema->castToInternal, ~flag=Flag.async)(input)
}

let reverseConvertOrThrow = (value, schema) => {
  getDecoder(~s1=schema->castToInternal->reverse)(value)
}

let reverseConvertToJsonOrThrow = (value, schema) => {
  getDecoder2(~s1=schema->castToInternal->reverse, ~s2=json)(value)
}

let reverseConvertToJsonStringOrThrow = (value: 'value, schema: t<'value>, ~space=?): string => {
  getDecoder2(
    ~s1=schema->castToInternal->reverse,
    ~s2=switch space {
    | None
    | Some(0) => jsonString
    | Some(v) => jsonStringWithSpace(v)->castToInternal
    },
  )(value)
}

let assertResult = unit->copySchema
assertResult.noValidation = Some(true)

let assertOrThrow = (any, schema) => {
  getDecoder3(~s1=unknown, ~s2=schema->castToInternal, ~s3=assertResult)(any)
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
    schema->(Obj.magic: t<'a> => dict<option<'metadata>>)->Js.Dict.unsafeGet(id->Id.toKey)
  }

  @inline
  let setInPlace = (schema, ~id: Id.t<'metadata>, metadata: 'metadata) => {
    schema->(Obj.magic: internal => dict<'metadata>)->Js.Dict.set(id->Id.toKey, metadata)
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
    globalConfig.defsAccumulator = Some(Js.Dict.empty())
  }
  let def = fn(refSchema->castToPublic)->castToInternal
  if def.name->Obj.magic {
    refSchema.name = def.name
  } else {
    def.name = Some(name)
  }
  globalConfig.defsAccumulator
  ->X.Option.getUnsafe
  ->Js.Dict.set(name, def)

  if isNestedRec {
    refSchema->castToPublic
  } else {
    let schema = base(refTag, ~selfReverse=false)
    schema.name = def.name
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
          existingRefiner(~input) ++ refiner(~input)
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
      let failCode = if extraPath === Path.empty {
        input->B.fail(~message)
      } else {
        `${input->B.embed(() => {
            input->B.throw(Custom({reason: message, path: input.path->Path.concat(extraPath)}))
          })}()`
      }
      `if(!${input->B.embed(refineCheck)}(${input.var()})){${failCode}}`
    }
  )
}

let addRefinement = (schema, ~metadataId, ~refinement, ~refiner) => {
  schema->internalRefine(mut => {
    mut->Metadata.setInPlace(
      ~id=metadataId,
      switch schema->Metadata.get(~id=metadataId) {
      | Some(refinements) => refinements->X.Array.append(refinement)
      | None => [refinement]
      },
    )

    refiner
  })
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
      let to = base(unknownTag, ~selfReverse=false)
      to.decoder = noopDecoder
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

let nullAsUnit = base(nullTag, ~selfReverse=false)
nullAsUnit.const = %raw(`null`)
nullAsUnit.to = Some(unit)
nullAsUnit.decoder = Literal.literalDecoder
let nullAsUnit = nullAsUnit->castToPublic

let never = base(neverTag, ~selfReverse=true)
let neverBuilder = Builder.make((~input) => {
  let output = input->B.refine(~expected=never)
  output.codeFromPrev = B.embedInvalidInput(~input) ++ ";"
  output
})
never.decoder = neverBuilder
let never: t<never> = never->castToPublic

let nestedLoc = "BS_PRIVATE_NESTED_SOME_NONE"

module Dict = {
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

module Array = {
  module Refinement = {
    type kind =
      | Min({length: int})
      | Max({length: int})
      | Length({length: int})
    type t = {
      kind: kind,
      message: string,
    }

    let metadataId: Metadata.Id.t<array<t>> = Metadata.Id.internal("Array.refinements")
  }

  let refinements = schema => {
    switch schema->Metadata.get(~id=Refinement.metadataId) {
    | Some(m) => m
    | None => []
    }
  }
}

module Union = {
  @unboxed
  type itemCode = Single(string) | Multiple(array<string>)

  let isPriority = (tagFlag, byKey: dict<array<unknown>>) => {
    (tagFlag->Flag.unsafeHas(TagFlag.array->Flag.with(TagFlag.instance)) &&
      byKey->Stdlib.Dict.has((objectTag: tag :> string))) ||
      (tagFlag->Flag.unsafeHas(TagFlag.nan) && byKey->Stdlib.Dict.has((numberTag: tag :> string)))
  }

  let isWiderUnionSchema = (~schemaAnyOf, ~inputAnyOf) => {
    inputAnyOf->Js.Array2.everyi((inputSchema, idx) => {
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

      let initialInline = input.inline

      let fail = caught => {
        `${input->B.embed(
            (
              _ => {
                let args = %raw(`arguments`)
                input->B.throw(
                  B.makeInvalidInputDetails(
                    ~path=input.path,
                    ~expected=selfSchema,
                    ~received=unknown->castToPublic,
                    ~input=args->Js.Array2.unsafe_get(0),
                    ~includeInput=true,
                    ~unionErrors=?args->Js.Array2.length > 1
                      ? Some(args->X.Array.fromArguments->Js.Array2.sliceFrom(1))
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
        let typeValidationInput = arr->Js.Array2.unsafe_get(0)->(Obj.magic: unknown => val)
        let typeValidationOutput = arr->Js.Array2.unsafe_get(1)->(Obj.magic: unknown => val)

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
        let byDiscriminant = ref(Js.Dict.empty())

        let preItems = 2
        let itemIdx = ref(preItems)
        let lastIdx = arr->Js.Array2.length - 1
        while itemIdx.contents <= lastIdx {
          // Copy it one more time, since every case decoder
          // might mutate the input
          let input = typeValidationOutput->B.Val.scope
          input.isUnion = Some(true)
          input.hasTransform = typeValidationOutput.hasTransform
          input.isInput = Some(false)
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
            outputAnyOf->Js.Array2.push(itemOutput.schema->castToPublic)->ignore

            // This is a copy of the S.merge function
            let current = ref(Some(itemOutput))

            while current.contents !== None {
              let val = current.contents->X.Option.getUnsafe
              current := val.prev

              let currentCode = ref("")

              switch val.validation {
              | Some(validation) =>
                if (
                  val.hasTransform === Some(true)
                    ? (val.prev->X.Option.getUnsafe).hasTransform !== Some(true) &&
                        val.codeFromPrev === ""
                    : true
                ) {
                  // Validation must be used only when there's a prev value
                  let input = current.contents->X.Option.getUnsafe
                  let inputVar = input.var()
                  let condCode = validation(~inputVar)
                  if itemCond.contents->X.String.unsafeToBool {
                    itemCond := `${condCode}&&${itemCond.contents}`
                  } else {
                    itemCond := condCode
                  }
                } else if val.expected.noValidation !== Some(true) {
                  // Validation must be used only when there's a prev value
                  let input = current.contents->X.Option.getUnsafe
                  let inputVar = input.var()
                  let validationCode = validation(~inputVar)
                  currentCode :=
                    `${validationCode}||${B.embedInvalidInput(
                        ~input=val,
                        ~expected=val.expected,
                      )};`
                } else {
                  ()
                }
              | _ => ()
              }

              if val.varsAllocation !== "" {
                currentCode := currentCode.contents ++ `let ${val.varsAllocation};`
              }

              // Delete allocate,
              // this is used to handle Val.var
              // linked to allocated scopes
              let _ = %raw(`delete val.a`)

              currentCode := val.codeFromPrev ++ currentCode.contents

              itemCode := currentCode.contents ++ itemCode.contents
            }

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
              | Some(Multiple(arr)) => arr->Js.Array2.push(itemCode)->ignore
              | Some(Single(code)) =>
                byDiscriminant.contents->Js.Dict.set(itemCond, Multiple([code, itemCode]))
              | None => byDiscriminant.contents->Js.Dict.set(itemCond, Single(itemCode))
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
            let accedDiscriminants = byDiscriminant.contents->Js.Dict.keys
            for idx in 0 to accedDiscriminants->Js.Array2.length - 1 {
              let discrim = accedDiscriminants->Js.Array2.unsafe_get(idx)
              let if_ = itemNextElse.contents ? "else if" : "if"
              itemStart := itemStart.contents ++ if_ ++ `(${discrim}){`
              switch byDiscriminant.contents->Js.Dict.unsafeGet(discrim) {
              | Single(code) => itemStart := itemStart.contents ++ code ++ "}"
              | Multiple(arr) =>
                let caught = ref("")
                for idx in 0 to arr->Js.Array2.length - 1 {
                  let code = arr->Js.Array2.unsafe_get(idx)
                  let errorVar = `e` ++ idx->X.Int.unsafeToString
                  itemStart := itemStart.contents ++ `try{${code}}catch(${errorVar}){`
                  caught := `${caught.contents},${errorVar}`
                }
                itemStart :=
                  itemStart.contents ++
                  fail(caught.contents) ++
                  Js.String2.repeat("}", arr->Js.Array2.length) ++ "}"
              }
              itemNextElse := true
            }
            byDiscriminant.contents = Js.Dict.empty()
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
                typeValidationOutput.validation =
                  typeValidationOutput.validation->B.appendValidation((
                    ~inputVar as _,
                  ) => {
                    `(${itemNoop.contents})`
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

      let lastIdx = schemas->Js.Array2.length - 1
      let byKey: ref<dict<array<unknown>>> = ref(Js.Dict.empty())
      let keys = ref([])
      let updatedSchemas = []
      for idx in 0 to lastIdx {
        let schema = switch toPerCase {
        | Some(target) =>
          updateOutput(schemas->Js.Array2.unsafe_get(idx), mut => {
            // switch selfSchema.refiner {
            // | Some(refiner) => mut.refiner = Some(appendRefiner(mut.refiner, refiner))
            // | None => ()
            // }
            mut.to = Some(target)
          })->castToInternal
        | _ => schemas->Js.Array2.unsafe_get(idx)
        }
        updatedSchemas->Js.Array2.push(schema)->ignore
        let tag = schema.tag
        let tagFlag = TagFlag.get(tag)
        let key =
          tagFlag->Flag.unsafeHas(TagFlag.instance)
            ? (schema.class->Obj.magic)["name"]
            : (tag :> string)

        if (
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
              arr->Js.Array2.push(schema->(Obj.magic: internal => unknown))->ignore
            }
          | None =>
            // Recreate input val for every schema
            // since we will mutate it
            let typeValidationInput = input->B.Val.scope
            typeValidationInput.expected = if tagFlag->Flag.unsafeHas(TagFlag.null) {
              nullLiteral
            } else if tagFlag->Flag.unsafeHas(TagFlag.undefined) {
              unit
            } else if tagFlag->Flag.unsafeHas(TagFlag.object) {
              Dict.factory(unknown->castToPublic)->castToInternal
            } else if tagFlag->Flag.unsafeHas(TagFlag.array) {
              array(unknown->castToPublic)->castToInternal
            } else if tagFlag->Flag.unsafeHas(TagFlag.instance) {
              instance(schema.class)->castToInternal
            } else if tagFlag->Flag.unsafeHas(TagFlag.nan) {
              nan
            } else if tagFlag->Flag.unsafeHas(TagFlag.string) {
              string
            } else if tagFlag->Flag.unsafeHas(TagFlag.number) {
              float
            } else if tagFlag->Flag.unsafeHas(TagFlag.boolean) {
              bool
            } else if tagFlag->Flag.unsafeHas(TagFlag.bigint) {
              bigint
            } else if tagFlag->Flag.unsafeHas(TagFlag.symbol) {
              symbol
            } else {
              unknown
            }

            let typeValidationOutput = try {
              typeValidationInput->parse
            } catch {
            | _ => {
                typeValidationInput.validation = None
                typeValidationInput
              }
            }

            if isPriority(tagFlag, byKey.contents) {
              // Not the fastest way, but it's the simplest way
              // to make sure NaN is checked before number
              // And instance and array checked before object
              keys.contents->Js.Array2.unshift(key)->ignore
            } else {
              keys.contents->Js.Array2.push(key)->ignore
            }
            byKey.contents->Js.Dict.set(
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
                  v.validation !== None && (
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

              byKey := Js.Dict.empty()
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

        for idx in 0 to keys->Js.Array2.length - 1 {
          let arr = byKey->Js.Dict.unsafeGet(keys->Js.Array2.unsafe_get(idx))
          let typeValidationOutput = arr->Stdlib.Array.getUnsafe(1)->(Obj.magic: unknown => val)
          let firstSchema = arr->Stdlib.Array.getUnsafe(2)->(Obj.magic: unknown => internal)

          let itemsCode = getArrItemsCode(arr, ~isDeopt=false)

          let blockCode = ref("")
          let blockCond = ref("")

          // This is a copy of the S.merge function
          let current = ref(Some(typeValidationOutput))

          while current.contents !== None {
            let val = current.contents->X.Option.getUnsafe
            current := val.prev

            let currentCode = ref("")

            switch val.validation {
            | Some(validation) =>
              if (
                val.hasTransform === Some(true)
                  ? (val.prev->X.Option.getUnsafe).hasTransform !== Some(true) &&
                      val.codeFromPrev === ""
                  : true
              ) {
                // Validation must be used only when there's a prev value
                let input = current.contents->X.Option.getUnsafe
                let inputVar = input.var()
                let condCode = validation(~inputVar)
                if blockCond.contents->X.String.unsafeToBool {
                  blockCond := `${condCode}&&${blockCond.contents}`
                } else {
                  blockCond := condCode
                }
              } else if val.expected.noValidation !== Some(true) {
                // Validation must be used only when there's a prev value
                let input = current.contents->X.Option.getUnsafe
                let inputVar = input.var()
                let validationCode = validation(~inputVar)
                currentCode :=
                  `${validationCode}||${B.embedInvalidInput(
                      ~input=val,
                      ~expected=val.expected,
                    )};`
              } else {
                ()
              }
            | _ => ()
            }

            if val.varsAllocation !== "" {
              currentCode := currentCode.contents ++ `let ${val.varsAllocation};`
            }

            // Delete allocate,
            // this is used to handle Val.var
            // linked to allocated scopes
            let _ = %raw(`delete val.a`)

            currentCode := val.codeFromPrev ++ currentCode.contents

            blockCode := currentCode.contents ++ blockCode.contents
          }

          let blockCode = blockCode.contents ++ itemsCode
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
        never->castToInternal
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
      let has = Js.Dict.empty()
      let anyOf = X.Set.make()

      for idx in 0 to schemas->Js.Array2.length - 1 {
        let schema = schemas->Js.Array2.unsafe_get(idx)

        // Check if the union is not transformed
        if schema.tag === unionTag && schema.to === None {
          schema.anyOf
          ->X.Option.getUnsafe
          ->Js.Array2.forEach(item => {
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
      let properties = Js.Dict.empty()
      properties->Js.Dict.set(nestedLoc, itemSchema)
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
            ->Js.Dict.unsafeGet(nestedLoc)
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

  let factory = (item, ~unit=unit->castToPublic) => {
    let item = item->castToInternal

    switch item->getOutputSchema {
    | {tag: Undefined} => Union.factory([unit->castToUnknown, item->nestedOption->castToPublic])
    | {tag: Union, ?anyOf, ?has} =>
      item->updateOutput(mut => {
        let schemas = anyOf->X.Option.getUnsafe
        let mutHas = has->X.Option.getUnsafe->X.Dict.copy

        let newAnyOf = []
        for idx in 0 to schemas->Stdlib.Array.length - 1 {
          let schema = schemas->Js.Array2.unsafe_get(idx)
          newAnyOf
          ->Js.Array2.push(
            switch schema->getOutputSchema {
            | {tag: Undefined} => {
                mutHas->Js.Dict.set(((unit->castToInternal).tag: tag :> string), true)
                newAnyOf->Js.Array2.push(unit->castToInternal)->ignore
                schema->nestedOption
              }
            | {properties} =>
              switch properties->X.Dict.getUnsafeOption(nestedLoc) {
              | Some(nestedSchema) =>
                schema
                ->updateOutput(mut => {
                  // FIXME: dict{}
                  let properties = Js.Dict.empty()
                  properties->Js.Dict.set(
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

        if newAnyOf->Js.Array2.length === schemas->Js.Array2.length {
          mutHas->Js.Dict.set(((unit->castToInternal).tag: tag :> string), true)
          newAnyOf->Js.Array2.push(unit->castToInternal)->ignore
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
          let item = ref(None)
          let itemOutputSchema = ref(None)

          for idx in 0 to anyOf->Stdlib.Array.length - 1 {
            let schema = anyOf->Stdlib.Array.getUnsafe(idx)
            let outputSchema = schema->getOutputSchema
            switch outputSchema.tag {
            | Undefined => ()
            | _ =>
              switch item.contents {
              | None => {
                  item := Some(schema)
                  itemOutputSchema := Some(outputSchema)
                }
              | Some(_) =>
                InternalError.panic(`Can't set default for ${mut->castToPublic->toExpression}`)
              }
            }
          }

          let item = switch item.contents {
          | None => InternalError.panic(`Can't set default for ${mut->castToPublic->toExpression}`)
          | Some(s) => s
          }

          // FIXME: Ensure that default has the same type as the item
          // Or maybe not, but need to make it properly with JSON Schema

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
          let to = itemOutputSchema.contents->X.Option.getUnsafe->copySchema

          let originalDecoder = to.decoder
          to.serializer = Some(
            Builder.make((~input) => {
              let nextSchema = item->reverse
              originalDecoder(~input)->B.refine(~schema=nextSchema, ~expected=nextSchema)
            }),
          )

          // FIXME: This looks wrong, but this is how it was with prev architecture
          to.decoder = noopDecoder

          mut.to = Some(to)

          switch default {
          | Value(v) =>
            try mut.default =
              getDecoder(~s1=item->reverse)(v)->(
                Obj.magic: unknown => option<internalDefault>
              ) catch {
            | _ => ()
            }
          | Callback(_) => ()
          }
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
        currentAdditionalItems->Js.typeof !== (objectTag :> string) => {
        let mut = schema->copySchema
        mut.additionalItems = Some(additionalItems)
        if deep {
          switch schema.items {
          | Some(items) => {
              let newItems = []
              for idx in 0 to items->Js.Array2.length - 1 {
                let s = items->Js.Array2.unsafe_get(idx)
                newItems
                ->Js.Array2.push(
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
              let newProperties = Js.Dict.empty()
              let keys = properties->Js.Dict.keys
              for idx in 0 to keys->Js.Array2.length - 1 {
                let key = keys->Js.Array2.unsafe_get(idx)
                newProperties->Js.Dict.set(
                  key,
                  properties
                  ->Js.Dict.unsafeGet(key)
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

module String = {
  module Refinement = {
    type kind =
      | Min({length: int})
      | Max({length: int})
      | Length({length: int})
      | Email
      | Uuid
      | Cuid
      | Url
      | Pattern({re: Js.Re.t})
    type t = {
      kind: kind,
      message: string,
    }

    let metadataId: Metadata.Id.t<array<t>> = Metadata.Id.internal("String.refinements")
  }

  let refinements = schema => {
    switch schema->Metadata.get(~id=Refinement.metadataId) {
    | Some(m) => m
    | None => []
    }
  }

  let cuidRegex = /^c[^\s-]{8,}$/i
  let uuidRegex = /^[0-9a-fA-F]{8}\b-[0-9a-fA-F]{4}\b-[0-9a-fA-F]{4}\b-[0-9a-fA-F]{4}\b-[0-9a-fA-F]{12}$/i
  // Adapted from https://stackoverflow.com/a/46181/1550155
  let emailRegex = /^(?!\.)(?!.*\.\.)([A-Z0-9_'+\-\.]*)[A-Z0-9_+-]@([A-Z0-9][A-Z0-9\-]*\.)+[A-Z]{2,}$/i
}

let jsonEncoder = Builder.encoder((~input, ~target) => {
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
  } else if toTagFlag->Flag.unsafeHas(TagFlag.bigint) {
    let jsonExpected = string->copySchema
    jsonExpected.to = Some(target)
    input->B.refine(~schema=unknown, ~expected=jsonExpected)->parse
  } else if toTagFlag->Flag.unsafeHas(TagFlag.undefined->Flag.with(TagFlag.nan)) {
    let jsonExpected = nullLiteral->copySchema
    jsonExpected.to = Some(target)
    input->B.refine(~schema=unknown, ~expected=jsonExpected)->parse
  } else if toTagFlag->Flag.unsafeHas(TagFlag.array) {
    // Validate that the input is an array
    // and then update the schema to be an array of json instead of array of unknown
    let jsonExpected = array(unknown->castToPublic)->castToInternal
    let output = input->B.refine(~schema=unknown, ~expected=jsonExpected)->parse
    output.schema.additionalItems = Some(Schema(json->castToPublic))
    output.expected = target
    output.isInput = Some(false)
    output.isOutput = Some(false)
    output
  } else if toTagFlag->Flag.unsafeHas(TagFlag.object) {
    // Validate that the input is an object
    // and then update the schema to be an object of json instead of object of unknown
    let jsonExpected = Dict.factory(unknown->castToPublic)->castToInternal
    let output = input->B.refine(~schema=unknown, ~expected=jsonExpected)->parse
    output.schema.additionalItems = Some(Schema(json->castToPublic))
    output.expected = target
    output.isInput = Some(false)
    output.isOutput = Some(false)
    output
  } else {
    input
  }
})

let rec isJsonable = schema => {
  let tagFlag = schema.tag->TagFlag.get
  tagFlag->Flag.unsafeHas(
    TagFlag.string
    ->Flag.with(TagFlag.number)
    ->Flag.with(TagFlag.boolean)
    ->Flag.with(TagFlag.null),
  ) ||
  schema.ref === json.ref ||
  tagFlag->Flag.unsafeHas(TagFlag.union) &&
    schema.anyOf->X.Option.getUnsafe->Js.Array2.every(isJsonable) ||
  tagFlag->Flag.unsafeHas(TagFlag.array) &&
  switch schema.additionalItems->X.Option.getUnsafe {
  | Schema(s) => s->castToInternal->isJsonable
  | _ => true
  } &&
  schema.items->X.Option.getUnsafe->Js.Array2.every(isJsonable) ||
  (tagFlag->Flag.unsafeHas(TagFlag.object) &&
  switch schema.additionalItems->X.Option.getUnsafe {
  | Schema(s) => s->castToInternal->isJsonable
  | _ => true
  } &&
  schema.properties->X.Option.getUnsafe->Stdlib.Dict.valuesToArray->Js.Array2.every(isJsonable))
}

let jsonDecoder = (~input) => {
  let inputTagFlag = input.schema.tag->TagFlag.get

  if input.schema->isJsonable {
    input
  } else if inputTagFlag->Flag.unsafeHas(TagFlag.undefined->Flag.with(TagFlag.nan)) {
    input->B.nextConst(~schema=nullLiteral)
  } else if inputTagFlag->Flag.unsafeHas(TagFlag.bigint) {
    // FIXME: Support number here
    input->inputToString
  } else if inputTagFlag->Flag.unsafeHas(TagFlag.array) {
    let expected = base(arrayTag, ~selfReverse=false)
    expected.items = Some(
      input.schema.items
      ->X.Option.getUnsafe
      ->Js.Array2.map(_ => json),
    )
    expected.decoder = arrayDecoder
    expected.additionalItems = Some(
      switch input.schema.additionalItems->X.Option.getUnsafe {
      | Schema(_) => Schema(json->castToPublic)
      | v => v
      },
    )
    expected.to = input.expected.to
    input->B.refine(~expected)->parse
  } else if inputTagFlag->Flag.unsafeHas(TagFlag.object) {
    switch input.schema.additionalItems->X.Option.getUnsafe {
    | Schema(_) => {
        let expected = Dict.factory(json->castToPublic)->castToInternal
        expected.to = input.expected.to
        input->B.refine(~expected)->parse
      }
    | _ => {
        let jsonVal = makeObjectVal(input, ~schema=input.schema)
        jsonVal.expected = json
        if input.expected.to->Obj.magic {
          jsonVal.expected = jsonVal.expected->copySchema
          jsonVal.expected.to = input.expected.to
        }

        let keys = input.schema.properties->X.Option.getUnsafe->Js.Dict.keys
        for idx in 0 to keys->Js.Array2.length - 1 {
          let key = keys->Js.Array2.unsafe_get(idx)
          let itemVal = input->B.Val.get(key)
          itemVal.isInput = Some(false)
          itemVal.isOutput = Some(false)

          if (
            itemVal.schema.tag === unionTag &&
              itemVal.schema.has->X.Option.getUnsafe->Js.Dict.unsafeGet((undefinedTag :> string))
          ) {
            itemVal.expected =
              Union.factory([unit->castToPublic, json->castToPublic])->castToInternal
            let itemOutput = itemVal->parse
            itemOutput.optional = Some(true)
            jsonVal->B.Val.Object.add(~location=key, itemOutput)
          } else {
            itemVal.expected = json
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
      input.schema = json
      jsonEncoder(~input, ~target=input.expected)
    } else if input.expected.noValidation->X.Option.getUnsafe {
      input.schema = json
      input
    } else {
      recursiveDecoder(~input)
    }
  } else {
    input->B.unsupportedConversion(~from=input.schema, ~target=input.expected)
  }
}

let enableJson = () => {
  if json->Obj.magic->Js.Dict.unsafeGet(shakenRef)->Obj.magic {
    let _ = %raw(`delete json.as`)
    let jsonRef = base(refTag, ~selfReverse=true)
    jsonRef.ref = Some(`${defsPath}${jsonName}`)
    jsonRef.name = Some(jsonName)

    // FIXME: Validate whether dcoders needs to be here
    jsonRef.decoder = jsonDecoder
    jsonRef.encoder = Some(jsonEncoder)
    json.tag = jsonRef.tag
    json.ref = jsonRef.ref
    json.name = Some(jsonName)
    json.decoder = jsonDecoder
    json.encoder = Some(jsonEncoder)

    let anyOf = [
      string,
      bool,
      float,
      nullLiteral,
      Dict.factory(jsonRef->castToPublic)->castToInternal,
      array(jsonRef->castToPublic)->castToInternal,
    ]
    let has = Js.Dict.empty()
    anyOf->Js.Array2.forEach(schema => {
      has->Js.Dict.set((schema.tag :> string), true)
    })

    let jsonDef = base(unionTag, ~selfReverse=true)
    jsonDef.anyOf = Some(anyOf)
    jsonDef.has = Some(has)
    jsonDef.decoder = Union.unionDecoder
    jsonDef.name = Some(jsonName)
    jsonDef.tag = unionTag

    let defs = Js.Dict.empty()
    defs->Js.Dict.set(jsonName, jsonDef)
    json.defs = Some(defs)
  }
}

let enableJsonString = {
  let inlineJsonString = (input, ~schema) => {
    let tagFlag = schema.tag->TagFlag.get
    let const = schema.const
    if tagFlag->Flag.unsafeHas(TagFlag.undefined->Flag.with(TagFlag.null)) {
      `"null"`
    } else if tagFlag->Flag.unsafeHas(TagFlag.string) {
      const->Obj.magic->X.Inlined.Value.fromString->Js.Json.stringifyAny->Obj.magic
    } else if tagFlag->Flag.unsafeHas(TagFlag.bigint) {
      `"\\"${const->Obj.magic}\\""`
    } else if tagFlag->Flag.unsafeHas(TagFlag.number->Flag.with(TagFlag.boolean)) {
      `"${const->Obj.magic}"`
    } else {
      input->B.unsupportedConversion(~from=schema, ~target=input.expected)
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
      input->B.unsupportedConversion(~from=input.schema, ~target)
    }
  }

  let jsonStringEncoder = Builder.encoder((~input, ~target) => {
    if target.format !== Some(JSON) {
      if target->isLiteral {
        let jsonStringConstSchema = base(stringTag, ~selfReverse=true)
        jsonStringConstSchema.const = input->constSchemaToJsonStringConst(~target)->Obj.magic
        jsonStringConstSchema.to = Some(target)
        jsonStringConstSchema.decoder = Literal.literalDecoder
        input->B.refine(~expected=jsonStringConstSchema)
      } else {
        let outputVar = input.global->B.varWithoutAllocation
        input.allocate(outputVar)

        let nextSchema = json->copySchema
        nextSchema.to = Some(target)

        let output = input->B.next(outputVar, ~schema=nextSchema, ~expected=nextSchema)
        output.isInput = Some(true)
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

      let stringVal = stringDecoder(~input)
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
      let jsonVal = input->B.refine(~expected=json)->parse
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
      input->B.unsupportedConversion(~from=input.schema, ~target=expectedSchema)
    }
  })

  () => {
    if jsonString->Obj.magic->Js.Dict.unsafeGet(shakenRef)->Obj.magic {
      let _ = %raw(`delete jsonString.as`)
      enableJson()

      jsonString.tag = stringTag
      jsonString.format = Some(JSON)
      jsonString.name = Some(`${jsonName} string`)
      jsonString.encoder = Some(jsonStringEncoder)
      jsonString.decoder = jsonStringDecoder
    }
  }
}

let uint8Array = shaken("uint8Array")

let enableUint8Array = () => {
  if uint8Array->Obj.magic->Js.Dict.unsafeGet(shakenRef)->Obj.magic {
    let _ = %raw(`delete uint8Array.as`)
    uint8Array.tag = instanceTag
    uint8Array.class = %raw(`Uint8Array`)
    uint8Array.decoder = Builder.make((~input as inputArg) => {
      let inputTagFlag = inputArg.schema.tag->TagFlag.get
      let input = ref(inputArg)

      if inputTagFlag->Flag.unsafeHas(TagFlag.string) {
        input :=
          input.contents->B.next(
            `${input.contents->B.embed(
                %raw(`new TextEncoder()`),
              )}.encode(${input.contents.inline})`,
            ~schema=uint8Array,
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
                ~schema=string,
              )
          }
          input.contents
        }
      | _ => input.contents
      }
    })
  }
}

let isoDateTime = shaken("isoDateTime")

let enableIsoDateTime = () => {
  if isoDateTime->Obj.magic->Js.Dict.unsafeGet(shakenRef)->Obj.magic {
    let _ = %raw(`delete isoDateTime.as`)
    // Adapted from https://stackoverflow.com/a/3143231
    // Kept inline so the regex is only pulled into the bundle when
    // `enableIsoDateTime` is imported; unused otherwise it's tree-shaken away.
    let datetimeRe = /^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(\.\d+)?Z$/
    isoDateTime.tag = stringTag
    isoDateTime.decoder = string.decoder
    isoDateTime.format = Some(DateTime)
    isoDateTime.refiner = Some(
      (~input) => {
        `if(!${input->B.embed(
            datetimeRe,
          )}.test(${input.var()})){${input->B.fail(
            ~message="Invalid datetime string! Expected UTC",
          )}}`
      },
    )
  }
}

let invalidDateRefine = (input: val) =>
  input->B.refine(~schema=input.expected, ~validation=(~inputVar) => {
    `!Number.isNaN(${inputVar}.getTime())`
  })

let date = {
  let mut = base(instanceTag, ~selfReverse=true)
  mut.class = %raw(`Date`)
  mut.decoder = Builder.make((~input) => {
    let inputTagFlag = input.schema.tag->TagFlag.get
    if inputTagFlag->Flag.unsafeHas(TagFlag.string) {
      input
      ->B.next(`new Date(${input.inline})`, ~schema=mut->castToPublic->castToInternal)
      ->invalidDateRefine
    } else if inputTagFlag->Flag.unsafeHas(TagFlag.unknown) {
      instanceDecoder(~input)->invalidDateRefine
    } else if inputTagFlag->Flag.unsafeHas(TagFlag.instance) && input.schema.class === mut.class {
      input
    } else {
      input->B.unsupportedConversion(~from=input.schema, ~target=input.expected)
    }
  })

  // Encoder: Date → string (via toISOString) when target is string
  mut.encoder = Some(
    Builder.encoder((~input, ~target) => {
      let toTagFlag = target.tag->TagFlag.get
      if toTagFlag->Flag.unsafeHas(TagFlag.string) {
        let dateTimeString = string->copySchema
        dateTimeString.format = Some(DateTime)
        input
        ->B.next(
          `${input.inline}.toISOString()`,
          ~schema=dateTimeString,
          ~expected=target,
        )
        ->parse
      } else {
        input
      }
    }),
  )
  mut->castToPublic
}

module Int = {
  module Refinement = {
    type kind =
      | Min({value: int})
      | Max({value: int})

    type t = {
      kind: kind,
      message: string,
    }

    let metadataId: Metadata.Id.t<array<t>> = Metadata.Id.internal("Int.refinements")
  }

  let refinements = schema => {
    switch schema->Metadata.get(~id=Refinement.metadataId) {
    | Some(m) => m
    | None => []
    }
  }
}

module Float = {
  module Refinement = {
    type kind =
      | Min({value: float})
      | Max({value: float})
    type t = {
      kind: kind,
      message: string,
    }

    let metadataId: Metadata.Id.t<array<t>> = Metadata.Id.internal("Float.refinements")
  }

  let refinements = schema => {
    switch schema->Metadata.get(~id=Refinement.metadataId) {
    | Some(m) => m
    | None => []
    }
  }
}

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
    parser: array => array->Belt.List.fromArray,
    serializer: list => list->Belt.List.toArray,
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
      definition->Type.typeof === #object && definition !== %raw(`null`)

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
        let properties = Js.Dict.empty()
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
            required->Js.Array2.push(fieldName)->ignore
            properties->Js.Dict.set(fieldName, schema)
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
              let flattenedKeys = flattenedProperties->Js.Dict.keys
              let result = Js.Dict.empty()
              for idx in 0 to flattenedKeys->Js.Array2.length - 1 {
                let key = flattenedKeys->Js.Array2.unsafe_get(idx)
                result->Js.Dict.set(
                  key,
                  field(key, flattenedProperties->Js.Dict.unsafeGet(key)->castToPublic),
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

        parentCtx->Js.Dict.set(cacheId, ctx)

        (ctx :> Object.s)
      }
    }
  }
  and object:
    type value. (Object.s => value) => schema<value> =
    definer => {
      let flattened: option<array<internal>> = %raw(`void 0`)
      let properties = Js.Dict.empty()

      let flatten = schema => {
        let schema = schema->castToInternal
        switch schema {
        | {tag: Object, properties: ?flattenedProperties} => {
            let flattenedProperties = flattenedProperties->X.Option.getUnsafe
            let flattenedKeys = flattenedProperties->Js.Dict.keys
            for idx in 0 to flattenedKeys->Js.Array2.length - 1 {
              let key = flattenedKeys->Js.Array2.unsafe_get(idx)
              let flattenedSchema = flattenedProperties->Js.Dict.unsafeGet(key)
              switch properties->X.Dict.getUnsafeOption(key) {
              | Some(schema) if schema === flattenedSchema => ()
              | Some(_) =>
                InternalError.panic(`The field "${key}" defined twice with incompatible schemas`)
              | None => properties->Js.Dict.set(key, flattenedSchema)
              }
            }
            let f = %raw(`flattened || (flattened = [])`)
            schema->proxifyShapedSchema(
              ~from=inputFrom,
              ~fromFlattened=f->Js.Array2.push(schema) - 1,
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
          properties->Js.Dict.set(fieldName, schema)
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
      mut.required = Some(properties->Js.Dict.keys)
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
          let location = idx->Js.Int.toString
          if items->X.Array.has(idx) {
            InternalError.panic(`The item [${location}] is defined multiple times`)
          } else {
            items->Js.Array2.unsafe_set(idx, schema)
            schema->proxifyShapedSchema(~from=[idx->Js.Int.toString])
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

    for idx in 0 to items->Js.Array2.length - 1 {
      if items->Js.Array2.unsafe_get(idx)->Obj.magic->not {
        items->Js.Array2.unsafe_set(idx, unit)
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
        ~input=input.vals->X.Option.getUnsafe->Js.Dict.unsafeGet(key),
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
        ~input=input.flattenedVals->X.Option.getUnsafe->Js.Array2.unsafe_get(fromFlattened),
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
          for idx in 0 to items->Js.Array2.length - 1 {
            let location = idx->Js.Int.toString
            output->B.Val.Object.add(
              ~location,
              getShapedParserOutput(~input, ~targetSchema=items->Js.Array2.unsafe_get(idx)),
            )
          }
        | {properties} => {
            let keys = properties->Js.Dict.keys
            for idx in 0 to keys->Js.Array2.length - 1 {
              let location = keys->Js.Array2.unsafe_get(idx)
              output->B.Val.Object.add(
                ~location,
                getShapedParserOutput(
                  ~input,
                  ~targetSchema=properties->Js.Dict.unsafeGet(location),
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
      for idx in 0 to flattened->Js.Array2.length - 1 {
        let flattenedSchema = flattened->Js.Array2.unsafe_get(idx)
        let flattenedInput = input->B.Val.scope
        flattenedInput.expected = flattenedSchema
        flattenedInput.isOutput = Some(false)
        flattenedInput.isInput = Some(false)
        let flattenedVal = flattenedInput->parse
        flattenedVals->Js.Array2.push(flattenedVal)->ignore
        input.codeFromPrev = input.codeFromPrev ++ flattenedVal->B.merge
      }
      input.flattenedVals = Some(flattenedVals)
    | None => ()
    }

    let targetSchema = input.expected.to->X.Option.getUnsafe
    let output = getShapedParserOutput(~input, ~targetSchema)
    output.hasTransform = Some(true)
    output.prev = Some(input)
    output
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
                acc.flattened->X.Option.getUnsafe->Js.Array2.unsafe_set(idx, newAcc)
                newAcc
              }
            | Some(acc) => acc
            }
          }
        | None => acc
        },
      )
      for idx in 0 to from->Js.Array2.length - 1 {
        let key = from->Js.Array2.unsafe_get(idx)
        let p = switch accAtFrom.contents.properties {
        | Some(p) => p
        | None => {
            let p = Js.Dict.empty()

            accAtFrom.contents.properties = Some(p)
            p
          }
        }
        accAtFrom :=
          switch p->X.Dict.getUnsafeOption(key) {
          | Some(acc) => acc
          | None => {
              let newAcc: shapedSerializerAcc = {}
              p->Js.Dict.set(key, newAcc)
              newAcc
            }
          }
      }
      accAtFrom.contents.val = Some(input)
    | {vals} => {
        let keys = vals->Js.Dict.keys
        for idx in 0 to keys->Js.Array2.length - 1 {
          prepareShapedSerializerAcc(
            ~acc,
            ~input=vals->Js.Dict.unsafeGet(keys->Js.Array2.unsafe_get(idx)),
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
              resolvedTargetSchema.additionalItems->Js.typeof === (objectTag :> string)
          ) =>
          for idx in 0 to items->Js.Array2.length - 1 {
            let location = idx->Js.Int.toString
            v->B.Val.Object.add(
              ~location,
              getShapedSerializerOutput(
                ~input,
                ~acc=switch acc {
                | Some({properties}) => properties->X.Dict.getUnsafeOption(location)
                | _ => None
                },
                ~targetSchema=items->Js.Array2.unsafe_get(idx),
                ~path=path->Path.concat(
                  Path.fromInlinedLocation(input.global->B.inlineLocation(location)),
                ),
              ),
            )
          }
        | {properties, ?flattened}
          if !(
            acc === None &&
              resolvedTargetSchema.additionalItems->Js.typeof === (objectTag :> string)
          ) => {
            switch (flattened, acc) {
            | (Some(flattenedSchemas), Some({flattened: flattenedAcc})) =>
              flattenedAcc->Js.Array2.forEachi((acc, idx) => {
                let flattenedOutput = getShapedSerializerOutput(
                  ~input,
                  ~acc=Some(acc),
                  ~targetSchema=flattenedSchemas->Js.Array2.unsafe_get(idx)->reverse,
                  ~path,
                )
                v->B.Val.Object.merge(flattenedOutput.vals->X.Option.getUnsafe)
              })
            | _ => ()
            }

            let keys = properties->Js.Dict.keys
            for idx in 0 to keys->Js.Array2.length - 1 {
              let location = keys->Js.Array2.unsafe_get(idx)

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
                    ~targetSchema=properties->Js.Dict.unsafeGet(location),
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
          | Some(from) => path ++ from->Js.Array2.map(item => `["${item}"]`)->Js.Array2.joinWith("")
          | None => path
          }
          input->B.invalidOperation(
            ~description={
              `Missing input for ${targetSchema->reverse->castToPublic->toExpression}` ++
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
          for idx in 0 to node->Js.Array2.length - 1 {
            let schema = node->Js.Array2.unsafe_get(idx)->traverseDefinition(~onNode)
            node->Js.Array2.unsafe_set(idx, schema->(Obj.magic: internal => unknown))
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
            mut.decoder = Literal.literalDecoder
            mut
          } else {
            let node = definition->(Obj.magic: unknown => dict<unknown>)
            let fieldNames = node->Js.Dict.keys
            let length = fieldNames->Js.Array2.length
            for idx in 0 to length - 1 {
              let location = fieldNames->Js.Array2.unsafe_get(idx)
              let schema = node->Js.Dict.unsafeGet(location)->traverseDefinition(~onNode)
              node->Js.Dict.set(location, schema->(Obj.magic: internal => unknown))
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

let enum = values => Union.factory(values->Js.Array2.map(literal))

let compactColumnsEncoder = Builder.encoder((~input, ~target) => {
  // target is the compactColumns schema
  // target.to = S.array(objectSchema), so properties are in target.to.additionalItems.properties
  let maybeProperties = switch target.to {
  | Some({additionalItems: ?Some(Schema(itemSchema))}) =>
    let itemInternal = itemSchema->castToInternal
    itemInternal.properties
  | _ => None
  }

  switch maybeProperties {
  | Some(properties) => {
      let keys = properties->Js.Dict.keys
      let inputVar = input->B.Val.var
      let iteratorVar = input.global->B.varWithoutAllocation
      let outputVar = input.global->B.varWithoutAllocation

      let initialArraysCode = ref("")
      let settingCode = ref("")
      for idx in 0 to keys->Js.Array2.length - 1 {
        let key = keys->Js.Array2.unsafe_get(idx)
        let rawValueCode = `${inputVar}[${iteratorVar}][${key->X.Inlined.Value.fromString}]`
        let fieldSchema = properties->Js.Dict.unsafeGet(key)
        // Check if this field converts undefined to null (reversed nullAsOption)
        let hasUndefinedToNullTransform = switch fieldSchema.anyOf {
        | Some(anyOf) =>
          anyOf->Js.Array2.some(item => {
            let itemTagFlag = item.tag->TagFlag.get
            if itemTagFlag->Flag.unsafeHas(TagFlag.undefined) {
              switch item.to {
              | Some(toSchema) =>
                let toTagFlag = toSchema.tag->TagFlag.get
                toTagFlag->Flag.unsafeHas(TagFlag.null)
              | None => false
              }
            } else {
              false
            }
          })
        | None => false
        }
        let fieldValueCode = if hasUndefinedToNullTransform {
          `${rawValueCode}===void 0?null:${rawValueCode}`
        } else {
          rawValueCode
        }
        initialArraysCode := initialArraysCode.contents ++ `new Array(${inputVar}.length),`
        settingCode :=
          settingCode.contents ++
          `${outputVar}[${idx->X.Int.unsafeToString}][${iteratorVar}]=${fieldValueCode};`
      }

      input.allocate(`${outputVar}=[${initialArraysCode.contents}]`)

      let output = input->B.next(outputVar, ~schema=base(arrayTag, ~selfReverse=false))
      output.var = B._var
      output.codeFromPrev =
        output.codeFromPrev ++
        `for(let ${iteratorVar}=0;${iteratorVar}<${inputVar}.length;++${iteratorVar}){${settingCode.contents}}`
      output
    }
  | None => input->B.unsupportedConversion(~from=input.schema, ~target)
  }
})

let compactColumnsDecoder = (~input) => {
  let selfSchema = input.expected
  let inputTagFlag = input.schema.tag->TagFlag.get
  let isUnknownInput = inputTagFlag->Flag.unsafeHas(TagFlag.unknown)

  // Get properties and determine direction
  // Forward: properties come from selfSchema.to.additionalItems (S.array(objectSchema))
  // Reverse: properties come from input.schema.additionalItems (array of objects)
  let (maybeProperties, isForwardDirection) = switch selfSchema.to {
  | Some({additionalItems: ?Some(Schema(itemSchema))}) =>
    let itemInternal = itemSchema->castToInternal
    switch itemInternal.properties {
    | Some(_) as p => (p, true)
    | None => (None, true)
    }
  | _ => (None, true)
  }
  // If forward didn't find properties, try reverse direction
  let (maybeProperties, isForwardDirection) = switch maybeProperties {
  | Some(_) => (maybeProperties, isForwardDirection)
  | None =>
    switch input.schema.additionalItems {
    | Some(Schema(itemSchema)) =>
      let itemInternal = itemSchema->castToInternal
      switch itemInternal.properties {
      | Some(_) as p => (p, false)
      | None => (None, true)
      }
    | _ => (None, true)
    }
  }

  switch maybeProperties {
  | Some(properties) => {
      let keys = properties->Js.Dict.keys

      if keys->Js.Array2.length === 0 {
        if isUnknownInput {
          input.validation = Some(
            (~inputVar) => {
              `Array.isArray(${inputVar})&&${inputVar}.length===0`
            },
          )
        }
        let outputSchema = base(arrayTag, ~selfReverse=false)
        let output = input->B.next("[]", ~schema=outputSchema, ~expected=outputSchema)
        output.isOutput = Some(true)
        output
      } else if isForwardDirection {
        // Forward direction: columnar → rows
        if isUnknownInput {
          input.validation = Some(
            (~inputVar) => {
              `Array.isArray(${inputVar})&&${inputVar}.length===${keys
                ->Js.Array2.length
                ->X.Int.unsafeToString}` ++
              `${keys
                ->Js.Array2.mapi((_, idx) => {
                  `&&Array.isArray(${inputVar}[${idx->X.Int.unsafeToString}])`
                })
                ->Js.Array2.joinWith("")}`
            },
          )
        }

        let inputVar = input.var()
        let iteratorVar = input.global->B.varWithoutAllocation
        let outputVar = input.global->B.varWithoutAllocation

        let lengthCode = ref("")
        let itemBuildCode = ref("")
        for idx in 0 to keys->Js.Array2.length - 1 {
          let key = keys->Js.Array2.unsafe_get(idx)
          let rawValueCode = `${inputVar}[${idx->X.Int.unsafeToString}][${iteratorVar}]`
          let fieldSchema = properties->Js.Dict.unsafeGet(key)
          // Check if this field has a null variant (e.g., nullAsOption)
          let hasNullVariant = switch fieldSchema.has {
          | Some(has) =>
            switch has->X.Dict.getUnsafeOption((nullTag :> string)) {
            | Some(_) => true
            | None => false
            }
          | None => false
          }
          let fieldValueCode = if hasNullVariant {
            `${rawValueCode}===null?void 0:${rawValueCode}`
          } else {
            rawValueCode
          }
          lengthCode := lengthCode.contents ++ `${inputVar}[${idx->X.Int.unsafeToString}].length,`
          itemBuildCode :=
            itemBuildCode.contents ++ `${key->X.Inlined.Value.fromString}:${fieldValueCode},`
        }

        input.allocate(`${outputVar}=new Array(Math.max(${lengthCode.contents}))`)

        let outputSchema = base(arrayTag, ~selfReverse=false)
        let output = input->B.next(outputVar, ~schema=outputSchema, ~expected=outputSchema)
        output.var = B._var
        output.isOutput = Some(true)
        output.codeFromPrev =
          output.codeFromPrev ++
          `for(let ${iteratorVar}=0;${iteratorVar}<${outputVar}.length;++${iteratorVar}){${outputVar}[${iteratorVar}]={${itemBuildCode.contents}};}`
        output
      } else {
        // Reverse direction: rows → columnar
        let inputVar = input->B.Val.var
        let iteratorVar = input.global->B.varWithoutAllocation
        let outputVar = input.global->B.varWithoutAllocation

        let initialArraysCode = ref("")
        let settingCode = ref("")
        for idx in 0 to keys->Js.Array2.length - 1 {
          let key = keys->Js.Array2.unsafe_get(idx)
          let rawValueCode = `${inputVar}[${iteratorVar}][${key->X.Inlined.Value.fromString}]`
          let fieldSchema = properties->Js.Dict.unsafeGet(key)
          // Check if this field converts undefined to null (reversed nullAsOption)
          let hasUndefinedToNullTransform = switch fieldSchema.anyOf {
          | Some(anyOf) =>
            anyOf->Js.Array2.some(item => {
              let itemTagFlag = item.tag->TagFlag.get
              if itemTagFlag->Flag.unsafeHas(TagFlag.undefined) {
                switch item.to {
                | Some(toSchema) =>
                  let toTagFlag = toSchema.tag->TagFlag.get
                  toTagFlag->Flag.unsafeHas(TagFlag.null)
                | None => false
                }
              } else {
                false
              }
            })
          | None => false
          }
          let fieldValueCode = if hasUndefinedToNullTransform {
            `${rawValueCode}===void 0?null:${rawValueCode}`
          } else {
            rawValueCode
          }
          initialArraysCode := initialArraysCode.contents ++ `new Array(${inputVar}.length),`
          settingCode :=
            settingCode.contents ++
            `${outputVar}[${idx->X.Int.unsafeToString}][${iteratorVar}]=${fieldValueCode};`
        }

        input.allocate(`${outputVar}=[${initialArraysCode.contents}]`)

        let outputSchema = base(arrayTag, ~selfReverse=false)
        let output = input->B.next(outputVar, ~schema=outputSchema, ~expected=outputSchema)
        output.var = B._var
        output.isOutput = Some(true)
        output.codeFromPrev =
          output.codeFromPrev ++
          `for(let ${iteratorVar}=0;${iteratorVar}<${inputVar}.length;++${iteratorVar}){${settingCode.contents}}`
        output
      }
    }
  | None =>
    InternalError.panic(
      "S.compactColumns supports only object schemas. Use S.compactColumns(S.unknown)->S.to(S.array(objectSchema)).",
    )
  }
}

let compactColumns = inputSchema => {
  let innerArray = array(inputSchema)
  let mut = array(innerArray)->castToInternal
  mut.format = Some(CompactColumns)
  mut.decoder = compactColumnsDecoder
  mut.encoder = Some(compactColumnsEncoder)
  mut->castToPublic
}

// let inline = {
//   let rec internalInline = (schema, ~variant as maybeVariant=?, ()) => {
//     let mut = schema->castToInternal->copy

//     let inlinedSchema = switch mut {
//     | {?const} if isLiteral(mut) => `S.literal(%raw(\`${literal->Literal.toString}\`))`
//     | {anyOf} => {
//         let variantNamesCounter = Js.Dict.empty()
//         `S.union([${anyOf
//           ->Js.Array2.map(s => {
//             let variantName = s.name()
//             let numberOfVariantNames = switch variantNamesCounter->Js.Dict.get(variantName) {
//             | Some(n) => n
//             | None => 0
//             }
//             variantNamesCounter->Js.Dict.set(variantName, numberOfVariantNames->X.Int.plus(1))
//             let variantName = switch numberOfVariantNames {
//             | 0 => variantName
//             | _ =>
//               variantName ++ numberOfVariantNames->X.Int.plus(1)->X.Int.unsafeToString
//             }
//             let inlinedVariant = `#${variantName->X.Inlined.Value.fromString}`
//             s->internalInline(~variant=inlinedVariant, ())
//           })
//           ->Js.Array2.joinWith(", ")}])`
//       }
//     | {tag: JSON} => `S.json(~validate=${validated->(Obj.magic: bool => string)})`
//     | {tag: TupleTuple({items: [s0]}) => `S.tuple1(${s0.schema->internalInline()})`
//     | Tuple({items: [s0, s1]}) =>
//       `S.tuple2(${s0.schema->internalInline()}, ${s1.schema->internalInline()})`
//     | Tuple({items: [s0, s1, s2]}) =>
//       `S.tuple3(${s0.schema->internalInline()}, ${s1.schema->internalInline()}, ${s2.schema->internalInline()})`
//     | Tuple({items}) =>
//       `S.tuple(s => (${items
//         ->Js.Array2.mapi((schema, idx) =>
//           `s.item(${idx->X.Int.unsafeToString}, ${schema.schema->internalInline()})`
//         )
//         ->Js.Array2.joinWith(", ")}))`
//     | Object({items: []}) => `S.object(_ => ())`
//     | Object({items}) =>
//       `S.object(s =>
//   {
//     ${items
//         ->Js.Array2.map(item => {
//           `${item.inlinedLocation}: s.field(${item.inlinedLocation}, ${item.schema->internalInline()})`
//         })
//         ->Js.Array2.joinWith(",\n    ")},
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
//         ->Js.Array2.map(refinement => {
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
//         ->Js.Array2.joinWith("")
//       }
//     | Int =>
//       // | Literal(Int(_)) ???
//       switch schema->Int.refinements {
//       | [] => inlinedSchema
//       | refinements =>
//         metadataMap->X.Dict.deleteInPlace(Int.Refinement.metadataId->Metadata.Id.toKey)
//         inlinedSchema ++
//         refinements
//         ->Js.Array2.map(refinement => {
//           switch refinement {
//           | {kind: Max({value}), message} =>
//             `->S.intMax(${value->X.Int.unsafeToString}, ~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Min({value}), message} =>
//             `->S.intMin(${value->X.Int.unsafeToString}, ~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Port, message} =>
//             `->S.port(~message=${message->X.Inlined.Value.fromString})`
//           }
//         })
//         ->Js.Array2.joinWith("")
//       }
//     | Float =>
//       // | Literal(Float(_)) ???
//       switch schema->Float.refinements {
//       | [] => inlinedSchema
//       | refinements =>
//         metadataMap->X.Dict.deleteInPlace(Float.Refinement.metadataId->Metadata.Id.toKey)
//         inlinedSchema ++
//         refinements
//         ->Js.Array2.map(refinement => {
//           switch refinement {
//           | {kind: Max({value}), message} =>
//             `->S.floatMax(${value->X.Inlined.Float.toRescript}, ~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Min({value}), message} =>
//             `->S.floatMin(${value->X.Inlined.Float.toRescript}, ~message=${message->X.Inlined.Value.fromString})`
//           }
//         })
//         ->Js.Array2.joinWith("")
//       }

//     | Array(_) =>
//       switch schema->Array.refinements {
//       | [] => inlinedSchema
//       | refinements =>
//         metadataMap->X.Dict.deleteInPlace(Array.Refinement.metadataId->Metadata.Id.toKey)
//         inlinedSchema ++
//         refinements
//         ->Js.Array2.map(refinement => {
//           switch refinement {
//           | {kind: Max({length}), message} =>
//             `->S.arrayMaxLength(${length->X.Int.unsafeToString}, ~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Min({length}), message} =>
//             `->S.arrayMinLength(${length->X.Int.unsafeToString}, ~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Length({length}), message} =>
//             `->S.arrayLength(${length->X.Int.unsafeToString}, ~message=${message->X.Inlined.Value.fromString})`
//           }
//         })
//         ->Js.Array2.joinWith("")
//       }

//     | _ => inlinedSchema
//     }

//     let inlinedSchema = if metadataMap->Js.Dict.keys->Js.Array2.length !== 0 {
//       `{
//   let s = ${inlinedSchema}
//   let _ = %raw(\`s.m = ${metadataMap->Js.Json.stringifyAny->Belt.Option.getUnsafe}\`)
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
let nullAsOption = item => Option.factory(item, ~unit=nullAsUnit)
let null = item => Union.factory([item->castToUnknown, nullLiteral->castToPublic])
let option = item => item->Option.factory(~unit=unit->castToPublic)
let array = array
let dict = Dict.factory
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

let intMin = (schema, minValue, ~message as maybeMessage=?) => {
  let message = switch maybeMessage {
  | Some(m) => m
  | None => `Number must be greater than or equal to ${minValue->X.Int.unsafeToString}`
  }
  schema->addRefinement(
    ~metadataId=Int.Refinement.metadataId,
    ~refiner=(~input) => {
      `if(${input.var()}<${input->B.embed(minValue)}){${input->B.fail(~message)}}`
    },
    ~refinement={
      kind: Min({value: minValue}),
      message,
    },
  )
}

let intMax = (schema, maxValue, ~message as maybeMessage=?) => {
  let message = switch maybeMessage {
  | Some(m) => m
  | None => `Number must be lower than or equal to ${maxValue->X.Int.unsafeToString}`
  }
  schema->addRefinement(
    ~metadataId=Int.Refinement.metadataId,
    ~refiner=(~input) => {
      `if(${input.var()}>${input->B.embed(maxValue)}){${input->B.fail(~message)}}`
    },
    ~refinement={
      kind: Max({value: maxValue}),
      message,
    },
  )
}

let port = (schema, ~message=?) => {
  schema->internalRefine(mut => {
    mut.format = Some(Port)
    (~input) => {
      let inputVar = input.var()
      `${inputVar}>0&&${inputVar}<65536&&${inputVar}%1===0||${switch message {
        | Some(m) => input->B.fail(~message=m)
        | None => B.embedInvalidInput(~input)
        }};`
    }
  })
}

let floatMin = (schema, minValue, ~message as maybeMessage=?) => {
  let message = switch maybeMessage {
  | Some(m) => m
  | None => `Number must be greater than or equal to ${minValue->X.Float.unsafeToString}`
  }
  schema->addRefinement(
    ~metadataId=Float.Refinement.metadataId,
    ~refiner=(~input) => {
      `if(${input.var()}<${input->B.embed(minValue)}){${input->B.fail(~message)}}`
    },
    ~refinement={
      kind: Min({value: minValue}),
      message,
    },
  )
}

let floatMax = (schema, maxValue, ~message as maybeMessage=?) => {
  let message = switch maybeMessage {
  | Some(m) => m
  | None => `Number must be lower than or equal to ${maxValue->X.Float.unsafeToString}`
  }
  schema->addRefinement(
    ~metadataId=Float.Refinement.metadataId,
    ~refiner=(~input) => {
      `if(${input.var()}>${input->B.embed(maxValue)}){${input->B.fail(~message)}}`
    },
    ~refinement={
      kind: Max({value: maxValue}),
      message,
    },
  )
}

let arrayMinLength = (schema, length, ~message as maybeMessage=?) => {
  let message = switch maybeMessage {
  | Some(m) => m
  | None => `Array must be ${length->X.Int.unsafeToString} or more items long`
  }
  schema->addRefinement(
    ~metadataId=Array.Refinement.metadataId,
    ~refiner=(~input) => {
      `if(${input.var()}.length<${input->B.embed(length)}){${input->B.fail(~message)}}`
    },
    ~refinement={
      kind: Min({length: length}),
      message,
    },
  )
}

let arrayMaxLength = (schema, length, ~message as maybeMessage=?) => {
  let message = switch maybeMessage {
  | Some(m) => m
  | None => `Array must be ${length->X.Int.unsafeToString} or fewer items long`
  }
  schema->addRefinement(
    ~metadataId=Array.Refinement.metadataId,
    ~refiner=(~input) => {
      `if(${input.var()}.length>${input->B.embed(length)}){${input->B.fail(~message)}}`
    },
    ~refinement={
      kind: Max({length: length}),
      message,
    },
  )
}

let arrayLength = (schema, length, ~message as maybeMessage=?) => {
  let message = switch maybeMessage {
  | Some(m) => m
  | None => `Array must be exactly ${length->X.Int.unsafeToString} items long`
  }
  schema->addRefinement(
    ~metadataId=Array.Refinement.metadataId,
    ~refiner=(~input) => {
      `if(${input.var()}.length!==${input->B.embed(length)}){${input->B.fail(~message)}}`
    },
    ~refinement={
      kind: Length({length: length}),
      message,
    },
  )
}

let stringMinLength = (schema, length, ~message as maybeMessage=?) => {
  let message = switch maybeMessage {
  | Some(m) => m
  | None => `String must be ${length->X.Int.unsafeToString} or more characters long`
  }
  schema->addRefinement(
    ~metadataId=String.Refinement.metadataId,
    ~refiner=(~input) => {
      `if(${input.var()}.length<${input->B.embed(length)}){${input->B.fail(~message)}}`
    },
    ~refinement={
      kind: Min({length: length}),
      message,
    },
  )
}

let stringMaxLength = (schema, length, ~message as maybeMessage=?) => {
  let message = switch maybeMessage {
  | Some(m) => m
  | None => `String must be ${length->X.Int.unsafeToString} or fewer characters long`
  }
  schema->addRefinement(
    ~metadataId=String.Refinement.metadataId,
    ~refiner=(~input) => {
      `if(${input.var()}.length>${input->B.embed(length)}){${input->B.fail(~message)}}`
    },
    ~refinement={
      kind: Max({length: length}),
      message,
    },
  )
}

let stringLength = (schema, length, ~message as maybeMessage=?) => {
  let message = switch maybeMessage {
  | Some(m) => m
  | None => `String must be exactly ${length->X.Int.unsafeToString} characters long`
  }
  schema->addRefinement(
    ~metadataId=String.Refinement.metadataId,
    ~refiner=(~input) => {
      `if(${input.var()}.length!==${input->B.embed(length)}){${input->B.fail(~message)}}`
    },
    ~refinement={
      kind: Length({length: length}),
      message,
    },
  )
}

let email = (schema, ~message=`Invalid email address`) => {
  schema->addRefinement(
    ~metadataId=String.Refinement.metadataId,
    ~refiner=(~input) => {
      `if(!${input->B.embed(String.emailRegex)}.test(${input.var()})){${input->B.fail(~message)}}`
    },
    ~refinement={
      kind: Email,
      message,
    },
  )
}

let uuid = (schema, ~message=`Invalid UUID`) => {
  schema->addRefinement(
    ~metadataId=String.Refinement.metadataId,
    ~refiner=(~input) => {
      `if(!${input->B.embed(String.uuidRegex)}.test(${input.var()})){${input->B.fail(~message)}}`
    },
    ~refinement={
      kind: Uuid,
      message,
    },
  )
}

let cuid = (schema, ~message=`Invalid CUID`) => {
  schema->addRefinement(
    ~metadataId=String.Refinement.metadataId,
    ~refiner=(~input) => {
      `if(!${input->B.embed(String.cuidRegex)}.test(${input.var()})){${input->B.fail(~message)}}`
    },
    ~refinement={
      kind: Cuid,
      message,
    },
  )
}

let url = (schema, ~message=`Invalid url`) => {
  schema->addRefinement(
    ~metadataId=String.Refinement.metadataId,
    ~refiner=(~input) => {
      `try{new URL(${input.var()})}catch(_){${input->B.fail(~message)}}`
    },
    ~refinement={
      kind: Url,
      message,
    },
  )
}

let pattern = (schema, re, ~message=`Invalid pattern`) => {
  schema->addRefinement(
    ~metadataId=String.Refinement.metadataId,
    ~refiner=(~input) => {
      let embededRe = input->B.embed(re)
      if re->Js.Re.global {
        // TODO Write a regression test when it's needed
        `${embededRe}.lastIndex=0;`
      } else {
        ""
      } ++
      `if(!${embededRe}.test(${input.var()})){${input->B.fail(~message)}}`
    },
    ~refinement={
      kind: Pattern({re: re}),
      message,
    },
  )
}

let trim = schema => {
  let transformer = string => string->Js.String2.trim
  schema->transform(_ => {parser: transformer, serializer: transformer})
}

let nullable = schema => {
  Union.factory([schema->castToUnknown, unit->castToPublic, nullLiteral->castToPublic])
}

let nullableAsOption = schema => {
  Union.factory([schema->castToUnknown, unit->castToPublic, nullAsUnit->castToUnknown])
}

// =============
// JS/TS API
// =============

let parser = %raw(`(...args) => getDecoder(unknown, ...args)`)

let asyncParser = %raw(`(...args) => getDecoder(unknown, ...args, 1)`)

let asyncDecoder = %raw(`(...args) => getDecoder(...args, 1)`)

let encoder = %raw(`(...args) => getDecoder(...args.map(reverse))`)

let asyncEncoder = %raw(`(...args) => getDecoder(...args.map(reverse), 1)`)

let js_assert = (schema, data) => {
  getDecoder3(~s1=unknown, ~s2=schema->castToInternal, ~s3=assertResult)(data)
}

let js_union = values =>
  Union.factory(
    values->Js.Array2.map(Schema.definitionToSchema)->(Obj.magic: array<internal> => array<'a>),
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
      let failCode = if extraPath === Path.empty {
        input->B.fail(~message)
      } else {
        `${input->B.embed(() => {
            input->B.throw(Custom({reason: message, path: input.path->Path.concat(extraPath)}))
          })}()`
      }
      `if(!${input->B.embed(refineCheck)}(${input.var()})){${failCode}}`
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
  let schema = Union.factory([schema->castToUnknown, unit->castToPublic])
  switch maybeOr {
  | Some(or) if Js.typeof(or) === "function" => schema->Option.getOrWith(or->Obj.magic)->Obj.magic
  | Some(or) => schema->Option.getOr(or->Obj.magic)->Obj.magic
  | None => schema
  }
}

let js_nullable = (schema, maybeOr) => {
  // TODO: maybeOr should be part of the unit schema
  let schema = Union.factory([schema->castToUnknown, nullAsUnit->castToUnknown])
  switch maybeOr {
  | Some(or) if Js.typeof(or) === "function" => schema->Option.getOrWith(or->Obj.magic)->Obj.magic
  | Some(or) => schema->Option.getOr(or->Obj.magic)->Obj.magic
  | None => schema
  }
}

let js_merge = (s1, s2) => {
  switch switch (s1, s2) {
  | (
      Object({properties: properties1, additionalItems: additionalItems1}),
      Object({properties: properties2, additionalItems: additionalItems2}),
    )
    // Filter out S.record schemas
    if additionalItems1->Type.typeof === #string &&
    additionalItems2->Type.typeof === #string &&
    !((s1->castToInternal).to->Obj.magic) &&
    !((s2->castToInternal).to->Obj.magic) =>
    let properties = properties1->X.Dict.copy
    let keys2 = properties2->Js.Dict.keys

    for idx in 0 to keys2->Js.Array2.length - 1 {
      let key = keys2->Js.Array2.unsafe_get(idx)
      properties->Js.Dict.set(key, properties2->Js.Dict.unsafeGet(key))
    }

    let mut = base(objectTag, ~selfReverse=false)

    // TODO: Merge to required fields
    mut.required = Some(properties->(Obj.magic: dict<t<unknown>> => dict<internal>)->Js.Dict.keys)
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
          array<unknown> => array<Js.Json.t>
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
    // If the schema has a user-applied .to transform, try the encode path
    // first: reverse the chain and use the resulting JSON-compatible schema.
    // Fall through to the normal tag-based logic when encoding isn't possible.
    //
    // `S.object`, `S.tuple`/`S.schema` (arrayTag) and the union definer
    // (unionTag) all set `.to` internally as part of their shaping machinery
    // (see `definitionToShapedSchema`). For those tags a truthy `.to` does NOT
    // signal a user-applied transform — the normal tag-based logic in
    // `internalToJSONSchemaBase` already recurses into properties/items/variants
    // and handles any nested `.to` on children. Only "leaf-ish" tags have `.to`
    // set solely by user-facing `S.to(target)` calls.
    //
    // IMPORTANT: if a new tag is introduced whose factory sets `.to` internally
    // (for shaping, flattening, etc.), add its TagFlag to the mask below so
    // `toJSONSchema` doesn't accidentally try to encode-reverse its internals.
    let hasUserTo =
      schemaInternal.to->Obj.magic &&
        !(
          schemaInternal.tag
          ->TagFlag.get
          ->Flag.unsafeHas(
            TagFlag.object->Flag.with(TagFlag.array)->Flag.with(TagFlag.union),
          )
        )
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
  and internalToJSONSchemaBase = (
    schema: schema<unknown>,
    ~path,
    ~defs,
    ~parent,
  ): JSONSchema.t => {
    let jsonSchema: Mutable.t = {}
    switch schema {
    | String({?const, ?format}) => {
        jsonSchema.type_ = Some(Arrayable.single(#string))
        switch format {
        | Some(DateTime) => jsonSchema.format = Some("date-time")
        | Some(JSON) | None => ()
        }
        schema
        ->String.refinements
        ->Js.Array2.forEach(refinement => {
          switch refinement {
          | {kind: Email} => jsonSchema.format = Some("email")
          | {kind: Url} => jsonSchema.format = Some("uri")
          | {kind: Uuid} => jsonSchema.format = Some("uuid")
          | {kind: Cuid} => ()
          | {kind: Length({length})} => {
              jsonSchema.minLength = Some(length)
              jsonSchema.maxLength = Some(length)
            }
          | {kind: Max({length})} => jsonSchema.maxLength = Some(length)
          | {kind: Min({length})} => jsonSchema.minLength = Some(length)
          | {kind: Pattern({re})} =>
            jsonSchema.pattern = Some((re->(Obj.magic: Js.Re.t => {..}))["source"])
          }
        })
        switch const {
        | Some(value) => jsonSchema.const = Some(Js.Json.string(value))
        | None => ()
        }
      }
    | Number({?format, ?const}) =>
      switch format {
      | Some(Int32) =>
        jsonSchema.type_ = Some(Arrayable.single(#integer))
        schema
        ->Int.refinements
        ->Js.Array2.forEach(refinement => {
          switch refinement {
          | {kind: Max({value})} => jsonSchema.maximum = Some(value->Js.Int.toFloat)
          | {kind: Min({value})} => jsonSchema.minimum = Some(value->Js.Int.toFloat)
          }
        })
      | Some(Port) => {
          jsonSchema.type_ = Some(Arrayable.single(#integer))
          jsonSchema.maximum = Some(65535.)
          jsonSchema.minimum = Some(0.)
        }
      | None =>
        jsonSchema.type_ = Some(Arrayable.single(#number))
        schema
        ->Float.refinements
        ->Js.Array2.forEach(refinement => {
          switch refinement {
          | {kind: Max({value})} => jsonSchema.maximum = Some(value)
          | {kind: Min({value})} => jsonSchema.minimum = Some(value)
          }
        })
      }
      switch const {
      | Some(value) => jsonSchema.const = Some(Js.Json.number(value))
      | None => ()
      }
    | Boolean({?const}) => {
        jsonSchema.type_ = Some(Arrayable.single(#boolean))
        switch const {
        | Some(value) => jsonSchema.const = Some(Js.Json.boolean(value))
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
        schema
        ->Array.refinements
        ->Js.Array2.forEach(refinement => {
          switch refinement {
          | {kind: Max({length})} => jsonSchema.maxItems = Some(length)
          | {kind: Min({length})} => jsonSchema.minItems = Some(length)
          | {kind: Length({length})} => {
              jsonSchema.maxItems = Some(length)
              jsonSchema.minItems = Some(length)
            }
          }
        })
      | _ => {
          let items = items->Js.Array2.mapi((itemSchema, idx) => {
            Schema(
              internalToJSONSchema(
                itemSchema,
                ~parent=schema,
                ~path=path->Path.concat(Path.fromLocation(idx->Js.Int.toString)),
                ~defs,
              ),
            )
          })
          let itemsNumber = items->Js.Array2.length

          jsonSchema.items = Some(Arrayable.array(items))
          jsonSchema.type_ = Some(Arrayable.single(#array))
          jsonSchema.minItems = Some(itemsNumber)
          jsonSchema.maxItems = Some(itemsNumber)
        }
      }

    | Union({anyOf}) => {
        let literals = []
        let items = []

        anyOf->Js.Array2.forEach(childSchema => {
          switch childSchema {
          // Filter out undefined to support optional fields
          | Undefined(_) if (parent->castToInternal).tag === objectTag => ()
          | _ => {
              items
              ->Js.Array2.push(
                Schema(internalToJSONSchema(childSchema, ~parent=schema, ~path, ~defs)),
              )
              ->ignore
              switch childSchema->castToInternal->isLiteral {
              | true =>
                literals
                ->Js.Array2.push(
                  (childSchema->castToInternal).const->(Obj.magic: option<char> => Js.Json.t),
                )
                ->ignore
              | false => ()
              }
            }
          }
        })

        let itemsNumber = items->Js.Array2.length

        switch (schema->untag).default {
        | Some(default) => jsonSchema.default = Some(default->(Obj.magic: unknown => Js.Json.t))
        | None => ()
        }

        // TODO: Write a breaking test with itemsNumber === 0
        if itemsNumber === 1 {
          jsonSchema->Mutable.mixin(items->Js.Array2.unsafe_get(0)->Obj.magic)
        } else if literals->Js.Array2.length === itemsNumber {
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
            if (childJsonSchema->Obj.magic: dict<'a>)->Js.Dict.keys->Js.Array2.length === 0 {
              JSONSchema.Any
            } else {
              Schema(childJsonSchema)
            },
          )
        }
      | _ => {
          let required = []
          let keys = properties->Js.Dict.keys
          let jsonProperties = Js.Dict.empty()

          for idx in 0 to keys->Js.Array2.length - 1 {
            let key = keys->Js.Array2.unsafe_get(idx)
            let itemSchema = properties->Js.Dict.unsafeGet(key)
            let fieldSchema = internalToJSONSchema(
              itemSchema,
              ~path=path->Path.concat(Path.fromLocation(key)),
              ~defs,
              ~parent=schema,
            )
            if itemSchema->castToInternal->isOptional->not {
              required->Js.Array2.push(key)->ignore
            }
            jsonProperties->Js.Dict.set(key, Schema(fieldSchema))
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
            ~expected=json,
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
  let defs = Js.Dict.empty()
  let jsonSchema =
    target
    ->castToPublic
    ->RescriptJSONSchema.internalToJSONSchema(~path=Path.empty, ~parent=target->castToPublic, ~defs)
  let _ = %raw(`delete defs.JSON`)
  let defsKeys = defs->Js.Dict.keys
  if defsKeys->Js.Array2.length->X.Int.unsafeToBool {
    // Reuse the same object to prevent allocations
    // Nothing critical, just because we can
    let jsonSchemDefs = defs->(Obj.magic: dict<t<unknown>> => dict<JSONSchema.definition>)
    defsKeys->Js.Array2.forEach(key => {
      let schema = defs->Js.Dict.unsafeGet(key)
      jsonSchemDefs->Js.Dict.set(
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

let castAnySchemaToJsonableS = (Obj.magic: schema<'any> => schema<Js.Json.t>)
let rec fromJSONSchema: RescriptJSONSchema.t => t<Js.Json.t> = {
  @inline
  let primitiveToSchema = primitive => {
    Literal.parse(primitive)->castToPublic->castAnySchemaToJsonableS
  }

  let toIntSchema = (jsonSchema: JSONSchema.t) => {
    let schema = int->castToPublic
    // TODO: Support jsonSchema.multipleOf when it's in rescript-schema
    // if (typeof jsonSchema.multipleOf === "number" && jsonSchema.multipleOf !== 1) {
    //  r += `.multipleOf(${jsonSchema.multipleOf})`;
    // }
    let schema = switch jsonSchema {
    | {minimum} => schema->intMin(minimum->Belt.Float.toInt)
    | {exclusiveMinimum} => schema->intMin((exclusiveMinimum +. 1.)->Belt.Float.toInt)
    | _ => schema
    }
    let schema = switch jsonSchema {
    | {maximum} => schema->intMax(maximum->Belt.Float.toInt)
    | {exclusiveMinimum} => schema->intMax((exclusiveMinimum -. 1.)->Belt.Float.toInt)
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
    let anySchema = json->castToPublic

    let definitionToSchema = (definition: JSONSchema.definition) =>
      switch definition {
      | Schema(s) => s->fromJSONSchema
      | Any => anySchema
      | Never => never->castToAny
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
            let obj = Js.Dict.empty()
            properties
            ->Js.Dict.keys
            ->Js.Array2.forEach(key => {
              let property = properties->Js.Dict.unsafeGet(key)
              let propertySchema = property->definitionToSchema
              let propertySchema = switch jsonSchema.required {
              | Some(r) if r->Js.Array2.includes(key) => propertySchema
              | _ =>
                switch property->definitionToDefaultValue {
                | Some(defaultValue) =>
                  propertySchema->option->Option.getOr(defaultValue)->castAnySchemaToJsonableS
                | None => propertySchema->option->castAnySchemaToJsonableS
                }
              }
              Js.Dict.set(obj, key, propertySchema)
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
            tuple(s => array->Js.Array2.mapi((d, idx) => s.item(idx, d->definitionToSchema)))
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
    | {anyOf: definitions} => union(definitions->Js.Array2.map(definitionToSchema))
    | {allOf: []} => anySchema
    | {allOf: [d]} => d->definitionToSchema
    | {allOf: definitions} => anySchema->refine(data => {
        definitions->Js.Array2.every(d => {
          try {
            data->assertOrThrow(d->definitionToSchema)
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
        definitions->Js.Array2.forEach(d => {
          try {
            let _ = data->assertOrThrow(d->definitionToSchema)
            validCount := validCount.contents + 1
          } catch {
          | _ => ()
          }
        })
        validCount.contents === 1
      }, ~error="Should pass exactly one schema according to the oneOf property.")
    | {not} => anySchema->refine(data => {
        try {
          let _ = data->assertOrThrow(not->definitionToSchema)
          false
        } catch {
        | _ => true
        }
      }, ~error="Should NOT be valid against schema in the not property.")
    // needs to come before primitives
    | {enum: []} => anySchema
    | {enum: [p]} => p->primitiveToSchema
    | {enum: primitives} =>
      union(primitives->Js.Array2.map(primitiveToSchema))->castAnySchemaToJsonableS
    | {const} => const->primitiveToSchema
    | {type_} if type_->JSONSchema.Arrayable.isArray =>
      let types = type_->(Obj.magic: JSONSchema.Arrayable.t<'a> => array<'a>)
      union(
        types->Js.Array2.map(type_ => {
          jsonSchema
          ->RescriptJSONSchema.merge({type_: JSONSchema.Arrayable.single(type_)})
          ->fromJSONSchema
        }),
      )
    | {type_} if type_ === JSONSchema.Arrayable.single(#string) =>
      let schema = string->castToPublic
      let schema = switch jsonSchema {
      | {pattern: p} => schema->pattern(Js.Re.fromString(p))
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
      switch jsonSchema {
      | {format: "email"} => schema->email->castAnySchemaToJsonableS
      | {format: "uri"} => schema->url->castAnySchemaToJsonableS
      | {format: "uuid"} => schema->uuid->castAnySchemaToJsonableS
      | {format: "date-time"} =>
        enableIsoDateTime()
        isoDateTime->castToPublic->castAnySchemaToJsonableS
      | _ => schema->castAnySchemaToJsonableS
      }

    | {type_} if type_ === JSONSchema.Arrayable.single(#integer) => jsonSchema->toIntSchema
    | {type_, format: "int64"} if type_ === JSONSchema.Arrayable.single(#number) =>
      jsonSchema->toIntSchema
    | {type_, multipleOf: 1.} if type_ === JSONSchema.Arrayable.single(#number) =>
      jsonSchema->toIntSchema
    | {type_} if type_ === JSONSchema.Arrayable.single(#number) => {
        let schema = float->castToPublic
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
      bool->castToPublic->castAnySchemaToJsonableS
    | {type_} if type_ === JSONSchema.Arrayable.single(#null) =>
      literal(%raw(`null`))->castAnySchemaToJsonableS
    | {if_, then, else_} => {
        let ifSchema = if_->definitionToSchema
        let thenSchema = then->definitionToSchema
        let elseSchema = else_->definitionToSchema
        anySchema->refine(data => {
          let passed = try {
            let _ = data->assertOrThrow(ifSchema)
            true
          } catch {
          | _ => false
          }
          try {
            if passed {
              data->assertOrThrow(thenSchema)
            } else {
              data->assertOrThrow(elseSchema)
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

let unknown: t<unknown> = unknown->castToPublic
let json: t<Js.Json.t> = json->castToPublic
let jsonString: t<string> = jsonString->castToPublic
let uint8Array: t<Uint8Array.t> = uint8Array->castToPublic
let isoDateTime: t<string> = isoDateTime->castToPublic
let bool: t<bool> = bool->castToPublic
let symbol: t<Js.Types.symbol> = symbol->castToPublic
let string: t<string> = string->castToPublic
let int: t<int> = int->castToPublic
let float: t<float> = float->castToPublic
let bigint: t<bigint> = bigint->castToPublic
let unit: t<unit> = unit->castToPublic
