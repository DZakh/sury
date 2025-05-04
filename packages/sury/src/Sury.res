@@uncurried
@@warning("-30")

type never

external castAnyToUnknown: 'any => unknown = "%identity"

module Obj = {
  external magic: 'a => 'b = "%identity"
}

module Stdlib = {
  module Proxy = {
    type traps<'a> = {get?: (~target: 'a, ~prop: unknown) => unknown}

    @new
    external make: ('a, traps<'a>) => 'a = "Proxy"
  }

  module Fn = {
    @send
    external bind: ('a => 'b, ~this: 'c) => 'a => 'b = "bind"
  }

  module Option = {
    external unsafeUnwrap: option<'a> => 'a = "%identity"
  }

  module Type = {
    type t = [#undefined | #object | #boolean | #number | #bigint | #string | #symbol | #function]

    external typeof: 'a => t = "#typeof"
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
  }

  module Array = {
    let immutableEmpty = %raw(`[]`)

    @send
    external append: (array<'a>, 'a) => array<'a> = "concat"

    @get_index
    external unsafeGetOptionByString: (array<'a>, string) => option<'a> = ""

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

    let raiseAny = (any: 'any): 'a => any->Obj.magic->raise

    let raiseError: error => 'a = raiseAny
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
    external unsafeToBool: string => bool = "%identity"
  }

  module Dict = {
    @val
    external copy: (@as(json`{}`) _, dict<'a>) => dict<'a> = "Object.assign"

    @val
    external mixin: (dict<'a>, dict<'a>) => dict<'a> = "Object.assign"

    @get_index
    external unsafeGetOption: (dict<'a>, string) => option<'a> = ""

    @get_index
    external unsafeGetOptionBySymbol: (dict<'a>, Js.Types.symbol) => option<'a> = ""

    @inline
    let has = (dict, key) => {
      dict->Js.Dict.unsafeGet(key)->(Obj.magic: 'a => bool)
    }
  }

  module Float = {
    external unsafeToString: float => string = "%identity"
  }

  module Set = {
    type t<'a>

    @new
    external make: unit => t<'a> = "Set"

    @new
    external fromArray: array<'a> => t<'a> = "Set"

    @send external add: (t<'a>, 'a) => unit = "add"

    @send external has: (t<'a>, 'a) => bool = "has"

    @val external toArray: t<'a> => array<'a> = "Array.from"
  }

  module Function = {
    @variadic @new
    external _make: array<string> => 'function = "Function"

    @inline
    let make2 = (~ctxVarName1, ~ctxVarValue1, ~ctxVarName2, ~ctxVarValue2, ~inlinedFunction) => {
      _make([ctxVarName1, ctxVarName2, `return ${inlinedFunction}`])(ctxVarValue1, ctxVarValue2)
    }
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
          | Some("\"") => string->Js.Json.stringifyAny->Obj.magic
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
  let fromLocation = location => `[${location->Stdlib.Inlined.Value.fromString}]`

  let fromArray = array => {
    switch array {
    | [] => ""
    | [location] => fromLocation(location)
    | _ =>
      "[" ++ array->Js.Array2.map(Stdlib.Inlined.Value.fromString)->Js.Array2.joinWith("][") ++ "]"
    }
  }

  let concat = (path, concatedPath) => path ++ concatedPath
}

let vendor = "sury"
let symbol = Stdlib.Symbol.make("schema")
let itemSymbol = Stdlib.Symbol.make("schema:item")

type tag =
  | @as("string") String
  | @as("number") Number
  | @as("never") Never
  | @as("unknown") Unknown
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
  | @as("json") JSON

type standard = {
  version: int,
  vendor: string,
  validate: 'any 'value. 'any => {"value": 'value},
}

type numberFormat = | @as("int32") Int32

type internalFormat = numberFormat

@unboxed
type additionalItemsMode = | @as("strip") Strip | @as("strict") Strict

@tag("type")
type rec t<'value> =
  private
  | @as("never") Never({name?: string, description?: string, deprecated?: bool})
  | @as("unknown")
  Unknown({
      name?: string,
      description?: string,
      deprecated?: bool,
      examples?: array<unknown>,
    })
  | @as("string")
  String({
      const?: string,
      name?: string,
      description?: string,
      deprecated?: bool,
      examples?: array<string>,
    })
  | @as("number")
  Number({
      const?: float,
      format?: numberFormat,
      name?: string,
      description?: string,
      deprecated?: bool,
      examples?: array<float>,
    })
  | @as("bigint")
  BigInt({
      const?: bigint,
      name?: string,
      description?: string,
      deprecated?: bool,
      examples?: array<bigint>,
    })
  | @as("boolean")
  Boolean({
      const?: bool,
      name?: string,
      description?: string,
      deprecated?: bool,
      examples?: array<bool>,
    })
  | @as("symbol")
  Symbol({
      const?: Js.Types.symbol,
      name?: string,
      description?: string,
      deprecated?: bool,
      examples?: array<Js.Types.symbol>,
    })
  | @as("null")
  Null({
      const: Js.Types.null_val,
      name?: string,
      description?: string,
      deprecated?: bool,
    })
  | @as("undefined")
  Undefined({
      const: unit,
      name?: string,
      description?: string,
      deprecated?: bool,
    })
  | @as("nan") NaN({const: float, name?: string, description?: string, deprecated?: bool})
  | @as("function")
  Function({
      const?: Js.Types.function_val,
      name?: string,
      description?: string,
      deprecated?: bool,
      examples?: array<Js.Types.function_val>,
    })
  | @as("instance")
  Instance({
      class: unknown,
      const?: Js.Types.obj_val,
      name?: string,
      description?: string,
      deprecated?: bool,
      examples?: array<Js.Types.obj_val>,
    })
  | @as("array")
  Array({
      items: array<item>,
      additionalItems: additionalItems,
      unnest?: bool,
      name?: string,
      description?: string,
      deprecated?: bool,
      examples?: array<array<unknown>>,
    })
  | @as("object")
  Object({
      items: array<item>,
      fields: dict<item>,
      additionalItems: additionalItems,
      name?: string,
      description?: string,
      deprecated?: bool,
      examples?: array<dict<unknown>>,
    }) // TODO: Add const for Object and Tuple
  | @as("union")
  Union({
      anyOf: array<t<unknown>>,
      has: has,
      name?: string,
      description?: string,
      deprecated?: bool,
      examples?: array<unknown>,
    })
  | @as("json")
  JSON({
      name?: string,
      description?: string,
      deprecated?: bool,
      examples?: array<Js.Json.t>,
    }) // FIXME: Remove it in favor of Union
@unboxed and additionalItems = | ...additionalItemsMode | Schema(t<unknown>)
// FIXME: Add recursive
and schema<'a> = t<'a>
and internal = {
  @as("type")
  mutable tag: tag,
  @as("b")
  mutable builder: builder,
  mutable const?: char, // use char to avoid Caml_option.some
  mutable class?: char, // use char to avoid Caml_option.some
  mutable name?: string,
  mutable description?: string,
  mutable deprecated?: bool,
  mutable examples?: array<unknown>,
  format?: internalFormat,
  mutable has?: dict<bool>,
  advanced?: bool, // TODO: Rename/remove it when have a chance
  mutable anyOf?: array<internal>,
  mutable additionalItems?: additionalItems,
  mutable items?: array<item>,
  mutable some?: internal, // This is for S.option
  mutable none?: internal, // This is for S.option
  mutable fields?: dict<item>,
  mutable noValidation?: bool,
  mutable catch?: bool,
  mutable unnest?: bool,
  // This can also be an `internal` itself, but because of the bug https://github.com/rescript-lang/rescript/issues/7314 handle it unsafely
  mutable output?: unit => internal, // Optional value means that it either should reverse to self or it's already a reversed schema
  mutable isAsync?: bool, // Optional value means that it's not lazily computed yet.
  // FIXME: Shouldn't be cloned?
  @as("~standard")
  mutable standard?: standard, // This is optional for convenience. The object added on make call
}
and meta<'value> = {
  name?: string,
  description?: string,
  deprecated?: bool,
  examples?: array<'value>,
}
and untagged = private {
  @as("type")
  tag: tag,
  const?: unknown,
  class?: unknown,
  name?: string,
  description?: string,
  deprecated?: bool,
  examples?: array<unknown>,
  unnest?: bool,
  noValidation?: bool,
  items?: array<item>,
  fields?: dict<item>,
  additionalItems?: additionalItems,
  anyOf?: array<t<unknown>>,
  has?: dict<bool>,
}
and item = {
  schema: t<unknown>,
  location: string,
  inlinedLocation: string,
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
and builder = (b, ~input: val, ~selfSchema: internal, ~path: Path.t) => val
and val = {
  @as("b")
  mutable b: b,
  @as("v")
  mutable var: b => string,
  @as("i")
  mutable inline: string,
  @as("a")
  mutable isAsync: bool,
}
and b = {
  @as("c")
  mutable code: string,
  @as("l")
  mutable varsAllocation: string,
  @as("a")
  mutable allocate: string => unit,
  @as("g")
  global: bGlobal,
}
and bGlobal = {
  @as("c")
  mutable code: string,
  @as("l")
  mutable varsAllocation: string,
  @as("a")
  mutable allocate: string => unit,
  @as("v")
  mutable varCounter: int,
  @as("o")
  mutable flag: int,
  @as("e")
  embeded: array<unknown>,
}
and flag = int
and error = private {
  message: string,
  reason: string,
  path: Path.t,
  code: errorCode,
  flag: flag,
}
and errorCode =
  | OperationFailed(string)
  | InvalidOperation({description: string})
  | InvalidType({expected: schema<unknown>, received: unknown, unionErrors?: array<error>})
  | ExcessField(string)
  | UnexpectedAsync
  | InvalidJsonSchema(schema<unknown>)
@tag("success")
and jsResult<'value> = | @as(true) Success({value: 'value}) | @as(false) Failure({error: error})

type exn += private Error(error)

external toUnknown: t<'any> => t<unknown> = "%identity"
external untag: t<'any> => untagged = "%identity"
external toInternal: t<'any> => internal = "%identity"
external fromInternal: internal => t<'any> = "%identity"

// This is dirty
@inline
let isSchemaObject = obj => (obj->Obj.magic).standard->Obj.magic

// TODO: Can be improved after ReScript supports `in` (https://github.com/rescript-lang/rescript/issues/7313)
let isLiteral: internal => bool = %raw(`s => "const" in s`)

let isOptional = schema => {
  switch schema.tag {
  | Undefined => true
  | Union => schema.has->Stdlib.Option.unsafeUnwrap->Stdlib.Dict.has((Undefined: tag :> string))
  | _ => false
  }
}

type globalConfig = {
  @as("r")
  mutable recCounter: int,
  @as("a")
  mutable defaultAdditionalItems: additionalItems,
  @as("n")
  mutable disableNanNumberValidation: bool,
}

type globalConfigOverride = {
  defaultAdditionalItems?: additionalItemsMode,
  disableNanNumberValidation?: bool,
}

let initialOnAdditionalItems: additionalItemsMode = Strip
let initialDisableNanNumberProtection = false
let globalConfig = {
  recCounter: 0,
  defaultAdditionalItems: (initialOnAdditionalItems :> additionalItems),
  disableNanNumberValidation: initialDisableNanNumberProtection,
}

module InternalError = {
  %%raw(`
class SuryError extends Error {
  constructor(code, flag, path) {
    super();
    this.flag = flag;
    this.code = code;
    this.path = path;
  }
}

var d = Object.defineProperty, p = SuryError.prototype;
d(p, 'message', {
  get() {
      return message(this);
  }
})
d(p, 'reason', {
  get() {
      return reason(this);
  }
})
d(p, 'name', {value: 'SuryError'})
d(p, 's', {value: symbol})
d(p, '_1', {
  get() {
    return this
  },
});
d(p, 'RE_EXN_ID', {
  value: $$Error,
});

function w(fn, ...args) {
  return fn(this, ...args)
}
`)

  @new
  external make: (~code: errorCode, ~flag: int, ~path: Path.t) => error = "SuryError"

  let getOrRethrow = (exn: exn) => {
    if %raw("exn&&exn.s===symbol") {
      exn->(Obj.magic: exn => error)
    } else {
      raise(exn)
    }
  }

  // TODO: Throw S.Error
  @inline
  let panic = message => Stdlib.Exn.raiseError(Stdlib.Exn.makeError(`[Schema] ${message}`))
}

type s<'value> = {
  schema: t<'value>,
  fail: 'a. (string, ~path: Path.t=?) => 'a,
}

module Flag = {
  @inline let none = 0
  @inline let typeValidation = 1
  @inline let async = 2
  @inline let assertOutput = 4
  @inline let jsonableOutput = 8
  @inline let jsonStringOutput = 16
  @inline let reverse = 32
  @inline let flatten = 64

  external with: (flag, flag) => flag = "%orint"
  @inline
  let without = (flags, flag) => flags->with(flag)->lxor(flag)

  let unsafeHas = (acc: flag, flag) => acc->land(flag)->(Obj.magic: int => bool)
  let has = (acc: flag, flag) => acc->land(flag) !== 0
}

// Need to copy without operations cache
// which use flag as a key.
// > "a" is hacky way to skip all numbers
// Should actually benchmark whether it's faster
// FIXME: If output (reverse) is populated on the schema,
// it'll stay on the copied one, which will cause issues
let copy: internal => internal = %raw(`(schema) => {
  let c = {}
  for (let k in schema) {
    if (k > "a") {
      c[k] = schema[k]
    }
  }
  return c
}`)
let mergeInPlace: (internal, internal) => unit = %raw(`(target, schema) => {
  for (let k in schema) {
    if (k > "a") {
      target[k] = schema[k]
    }
  }
}`)
let resetOperationsCache: internal => unit = %raw(`(schema) => {
  for (let k in schema) {
    if (+k) {
      delete schema[k];
    }
  }
}`)

let rec stringify = unknown => {
  let typeOfValue = unknown->Stdlib.Type.typeof
  switch typeOfValue {
  | #undefined => "undefined"
  | #object if unknown === %raw(`null`) => "null"
  | #object if unknown->Stdlib.Array.isArray => {
      let array = unknown->(Obj.magic: unknown => array<unknown>)
      let string = ref("[")
      for i in 0 to array->Array.length - 1 {
        if i !== 0 {
          string := string.contents ++ ", "
        }
        string := string.contents ++ array->Js.Array2.unsafe_get(i)->stringify
      }
      string.contents ++ "]"
    }
  | #object
    if (unknown->(Obj.magic: 'a => {"constructor": unknown}))["constructor"] === %raw("Object") =>
    let dict = unknown->(Obj.magic: unknown => dict<unknown>)
    let keys = Js.Dict.keys(dict)
    let string = ref("{ ")
    for i in 0 to keys->Array.length - 1 {
      let key = keys->Js.Array2.unsafe_get(i)
      let value = dict->Js.Dict.unsafeGet(key)
      string := `${string.contents}${key}: ${stringify(value)}; `
    }
    string.contents ++ "}"
  | #object => unknown->Obj.magic->Stdlib.Object.internalClass
  | #string => `"${unknown->Obj.magic}"`
  | #number
  | #function
  | #boolean
  | #symbol =>
    (unknown->Obj.magic)["toString"]()
  | #bigint => `${unknown->Obj.magic}n`
  }
}

let rec toExpression = schema => {
  let schema = schema->toInternal
  switch schema {
  | {name} => name
  | {const} => const->Obj.magic->stringify
  | {anyOf} =>
    anyOf
    ->(Obj.magic: array<internal> => array<t<'a>>)
    ->Js.Array2.map(toExpression)
    ->Js.Array2.joinWith(" | ")
  | {format} => (format :> string)
  | {tag: Object, ?items, ?additionalItems} =>
    let items = items->Stdlib.Option.unsafeUnwrap
    if items->Js.Array2.length === 0 {
      if additionalItems->Js.typeof === "object" {
        let additionalItems: internal = additionalItems->Obj.magic
        `{ [key: string]: ${additionalItems->fromInternal->toExpression}; }`
      } else {
        `{}`
      }
    } else {
      `{ ${items
        ->Js.Array2.map(item => {
          `${item.location}: ${item.schema->toInternal->fromInternal->toExpression};`
        })
        ->Js.Array2.joinWith(" ")} }`
    }
  | {tag: Array, ?items, ?additionalItems} =>
    let items = items->Stdlib.Option.unsafeUnwrap
    if additionalItems->Js.typeof === "object" {
      let additionalItems: internal = additionalItems->Obj.magic
      let itemName = additionalItems->fromInternal->toExpression
      if additionalItems.tag === Union {
        `(${itemName})`
      } else {
        itemName
      } ++ "[]"
    } else {
      `[${items
        ->Js.Array2.map(item => item.schema->toInternal->fromInternal->toExpression)
        ->Js.Array2.joinWith(", ")}]`
    }

  | {tag: NaN} => "NaN"
  | {tag: Instance, ?class} => (class->Obj.magic)["name"]
  | {tag} => (tag :> string)
  }
}

module ErrorClass = {
  type t

  let value: t = %raw("SuryError")

  let constructor = InternalError.make

  let rec reason = (error: error, ~nestedLevel=0) => {
    switch error.code {
    | OperationFailed(reason) => reason
    | InvalidOperation({description}) => description
    | UnexpectedAsync => "Encountered unexpected async transform or refine. Use parseAsyncOrThrow operation instead"
    | ExcessField(fieldName) => `Unrecognized key "${fieldName}"`
    | InvalidType({expected: schema, received, ?unionErrors}) =>
      let m = ref(`Expected ${schema->toExpression}, received ${received->stringify}`)
      switch unionErrors {
      | Some(errors) => {
          let lineBreak = `\n${" "->Js.String2.repeat(nestedLevel * 2)}`
          let reasonsDict = Js.Dict.empty()
          for idx in 0 to errors->Js.Array2.length - 1 {
            let error = errors->Js.Array2.unsafe_get(idx)
            let reason = error->reason(~nestedLevel=nestedLevel->Stdlib.Int.plus(1))
            let location = switch error.path {
            | "" => ""
            | nonEmptyPath => `At ${nonEmptyPath}: `
            }
            let line = `- ${location}${reason}`
            if reasonsDict->Js.Dict.unsafeGet(line)->Stdlib.Int.unsafeToBool->not {
              reasonsDict->Js.Dict.set(line, 1)
              m := m.contents ++ lineBreak ++ line
            }
          }
        }
      | None => ()
      }
      m.contents
    | InvalidJsonSchema(schema) => `${schema->toExpression} is not valid JSON`
    }
  }

  let reason = error => reason(error)

  let message = (error: error) => {
    let op = error.flag

    let text = ref("Failed ")
    if op->Flag.unsafeHas(Flag.async) {
      text := text.contents ++ "async "
    }

    text :=
      text.contents ++ if op->Flag.unsafeHas(Flag.typeValidation) {
        if op->Flag.unsafeHas(Flag.assertOutput) {
          "asserting"
        } else {
          "parsing"
        }
      } else {
        "converting"
      }

    if op->Flag.unsafeHas(Flag.jsonableOutput) {
      text :=
        text.contents ++ " to JSON" ++ (op->Flag.unsafeHas(Flag.jsonStringOutput) ? " string" : "")
    }

    `${text.contents}${switch error.path {
      | "" => ""
      | nonEmptyPath => ` at ${nonEmptyPath}`
      }}: ${error->reason}`
  }
}

module Builder = {
  type t = builder

  let make = (Obj.magic: ((b, ~input: val, ~selfSchema: internal, ~path: Path.t) => val) => t)

  module B = {
    let embed = (b: b, value) => {
      let e = b.global.embeded
      let l = e->Js.Array2.length
      e->Js.Array2.unsafe_set(l, value->castAnyToUnknown)
      `e[${l->(Obj.magic: int => string)}]`
    }

    let inlineConst = (b, schema) => {
      switch schema {
      | {tag: Symbol | Instance | Function, ?const} => b->embed(const->Obj.magic)
      | {tag: Undefined} => "void 0"
      | {tag: String, ?const} => const->Obj.magic->Stdlib.Inlined.Value.fromString
      | {tag: BigInt, ?const} => const->Obj.magic ++ "n"
      | {?const} => const->Obj.magic
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

    let rootScope = (~flag) => {
      let global = {
        code: "",
        allocate: initialAllocate,
        varsAllocation: "",
        // TODO: Add global varsAllocation here
        // Set all the vars to the varsAllocation
        // Measure performance
        // TODO: Also try setting values to embed without allocation
        // (Is it memory leak?)
        varCounter: -1,
        embeded: [],
        flag,
      }
      (global->Obj.magic)["g"] = global
      global->(Obj.magic: bGlobal => b)
    }

    @inline
    let scope = (b: b): b => {
      {
        allocate: initialAllocate,
        global: b.global,
        code: "",
        varsAllocation: "",
      }
    }

    let allocateScope = (b: b): string => {
      // Delete allocate,
      // this is used to handle Val.var
      // linked to allocated scopes
      let _ = %raw(`delete b.a`)
      let varsAllocation = b.varsAllocation
      varsAllocation === "" ? b.code : `let ${varsAllocation};${b.code}`
    }

    let varWithoutAllocation = (global: bGlobal) => {
      let newCounter = global.varCounter->Stdlib.Int.plus(1)
      global.varCounter = newCounter
      `v${newCounter->Stdlib.Int.unsafeToString}`
    }

    let _var = _b => (%raw(`this`)).inline
    let _notVar = b => {
      let val = %raw(`this`)
      let v = b.global->varWithoutAllocation
      switch val.inline {
      | "" => val.b.allocate(v)
      | i if val.b.allocate !== %raw(`void 0`) => val.b.allocate(`${v}=${i}`)
      | i =>
        b.code = b.code ++ `${v}=${i};`
        b.global.allocate(v)
      }
      val.var = _var
      val.inline = v
      v
    }

    let allocateVal = (b: b): val => {
      let v = b.global->varWithoutAllocation
      b.allocate(v)
      {b, var: _var, isAsync: false, inline: v}
    }

    @inline
    let val = (b: b, initial: string): val => {
      {b, var: _notVar, inline: initial, isAsync: false}
    }

    @inline
    let embedVal = (b: b, value): val => {
      {b, var: _var, inline: b->embed(value), isAsync: false}
    }

    @inline
    let asyncVal = (b: b, initial: string): val => {
      {b, var: _notVar, inline: initial, isAsync: true}
    }

    module Val = {
      module Object = {
        type t = {
          ...val,
          @as("j")
          mutable join: (string, string) => string,
          @as("c")
          mutable asyncCount: int,
          @as("p")
          mutable promiseAllContent: string,
        }

        let objectJoin = (inlinedLocation, value) => {
          `${inlinedLocation}:${value},`
        }

        let arrayJoin = (_inlinedLocation, value) => {
          value ++ ","
        }

        let make = (b: b, ~isArray): t => {
          {
            b,
            var: _notVar,
            inline: "",
            isAsync: false,
            join: isArray ? arrayJoin : objectJoin,
            asyncCount: 0,
            promiseAllContent: "",
          }
        }

        let add = (objectVal, inlinedLocation, val: val) => {
          // inlinedLocation is either an int or a quoted string, so it's safe to store it directly on val
          objectVal->(Obj.magic: t => dict<val>)->Js.Dict.set(inlinedLocation, val)
          if val.isAsync {
            objectVal.promiseAllContent = objectVal.promiseAllContent ++ val.inline ++ ","
            objectVal.inline =
              objectVal.inline ++ objectVal.join(inlinedLocation, `a[${%raw(`objectVal.c++`)}]`)
          } else {
            objectVal.inline = objectVal.inline ++ objectVal.join(inlinedLocation, val.inline)
          }
        }

        let merge = (target, subObjectVal) => {
          let inlinedLocations = subObjectVal->Obj.magic->Js.Dict.keys
          // Start from 7 to skip all normal fields which are not inlined locations
          for idx in 7 to inlinedLocations->Js.Array2.length - 1 {
            let inlinedLocation = inlinedLocations->Js.Array2.unsafe_get(idx)
            target->add(
              inlinedLocation,
              subObjectVal->Obj.magic->Js.Dict.unsafeGet(inlinedLocation),
            )
          }
        }

        let complete = (objectVal, ~isArray) => {
          objectVal.inline = isArray
            ? "[" ++ objectVal.inline ++ "]"
            : "{" ++ objectVal.inline ++ "}"
          if objectVal.asyncCount->Obj.magic {
            objectVal.isAsync = true
            objectVal.inline = `Promise.all([${objectVal.promiseAllContent}]).then(a=>(${objectVal.inline}))`
          }
          (objectVal :> val)
        }
      }

      @inline
      let var = (b: b, val: val) => {
        val.var(b)
      }

      let addKey = (b: b, input: val, key, val: val) => {
        `${b->var(input)}[${key}]=${val.inline}`
      }

      let set = (b: b, input: val, val) => {
        if input === val {
          ""
        } else {
          let inputVar = b->var(input)
          switch (input, val) {
          | ({isAsync: false}, {isAsync: true}) => {
              input.isAsync = true
              `${inputVar}=${val.inline}`
            }
          | ({isAsync: false}, {isAsync: false})
          | ({isAsync: true}, {isAsync: true}) =>
            `${inputVar}=${val.inline}`
          | ({isAsync: true}, {isAsync: false}) => `${inputVar}=Promise.resolve(${val.inline})`
          }
        }
      }

      let get = (b, targetVal: val, inlinedLocation) => {
        switch targetVal
        ->(Obj.magic: val => dict<val>)
        ->Stdlib.Dict.unsafeGetOption(inlinedLocation) {
        | Some(val) => val
        | None => b->val(`${b->var(targetVal)}${Path.fromInlinedLocation(inlinedLocation)}`)
        }
      }

      let setInlined = (b: b, input: val, inlined) => {
        `${b->var(input)}=${inlined}`
      }

      let map = (inlinedFn, input: val) => {
        {b: input.b, var: _notVar, inline: `${inlinedFn}(${input.inline})`, isAsync: false}
      }
    }

    @inline
    let isInternalError = (_b: b, var) => {
      `${var}&&${var}.s===s`
    }

    let transform = (b: b, ~input: val, operation) => {
      if input.isAsync {
        let bb = b->scope
        let operationInput: val = {
          b,
          var: _var,
          inline: bb.global->varWithoutAllocation,
          isAsync: false,
        }
        let operationOutputVal = operation(bb, ~input=operationInput)
        let operationCode = bb->allocateScope

        input.b->asyncVal(
          `${input.inline}.then(${b->Val.var(
              operationInput,
            )}=>{${operationCode}return ${operationOutputVal.inline}})`,
        )
      } else {
        operation(b, ~input)
      }
    }

    let raise = (b: b, ~code, ~path) => {
      Stdlib.Exn.raiseAny(InternalError.make(~code, ~flag=b.global.flag, ~path))
    }

    let embedSyncOperation = (b: b, ~input: val, ~fn: 'input => 'output) => {
      if input.isAsync {
        input.b->asyncVal(`${input.inline}.then(${b->embed(fn)})`)
      } else {
        Val.map(b->embed(fn), input)
      }
    }

    let embedAsyncOperation = (b: b, ~input, ~fn: 'input => promise<'output>) => {
      if !(b.global.flag->Flag.unsafeHas(Flag.async)) {
        b->raise(~code=UnexpectedAsync, ~path=Path.empty)
      }
      let val = b->embedSyncOperation(~input, ~fn)
      val.isAsync = true
      val
    }

    let failWithArg = (b: b, ~path, fn: 'arg => errorCode, arg) => {
      `${b->embed(arg => {
          b->raise(~path, ~code=fn(arg))
        })}(${arg})`
    }

    let fail = (b: b, ~message, ~path) => {
      `${b->embed(() => {
          b->raise(~path, ~code=OperationFailed(message))
        })}()`
    }

    let effectCtx = (b, ~selfSchema, ~path) => {
      schema: selfSchema->fromInternal,
      fail: (message, ~path as customPath=Path.empty) => {
        b->raise(~path=path->Path.concat(customPath), ~code=OperationFailed(message))
      },
    }

    let invalidOperation = (b: b, ~path, ~description) => {
      b->raise(~path, ~code=InvalidOperation({description: description}))
    }

    // TODO: Refactor
    let withCatch = (b: b, ~input, ~catch, ~appendSafe=?, fn) => {
      let prevCode = b.code

      b.code = ""
      let errorVar = b.global->varWithoutAllocation
      let maybeResolveVal = catch(b, ~errorVar)
      let catchCode = `if(${b->isInternalError(errorVar)}){${b.code}`
      b.code = ""

      let bb = b->scope
      let fnOutput = fn(bb)
      b.code = b.code ++ bb->allocateScope

      let isNoop = fnOutput.inline === input.inline && b.code === ""

      switch appendSafe {
      | Some(append) => append(b, ~output=fnOutput)
      | None => ()
      }

      if isNoop {
        fnOutput
      } else {
        let isAsync = fnOutput.isAsync
        let output =
          input === fnOutput
            ? input
            : switch appendSafe {
              | Some(_) => fnOutput
              | None => {b, var: _notVar, inline: "", isAsync}
              }

        let catchCode = switch maybeResolveVal {
        | None => _ => `${catchCode}}throw ${errorVar}`
        | Some(resolveVal) =>
          catchLocation =>
            catchCode ++
            switch catchLocation {
            | #0 => b->Val.set(output, resolveVal)
            | #1 => `return ${resolveVal.inline}`
            } ++
            `}else{throw ${errorVar}}`
        }

        b.code =
          prevCode ++
          `try{${b.code}${switch isAsync {
            | true =>
              b->Val.setInlined(output, `${fnOutput.inline}.catch(${errorVar}=>{${catchCode(#1)}})`)
            | false => b->Val.set(output, fnOutput)
            }}}catch(${errorVar}){${catchCode(#0)}}`

        output
      }
    }

    let withPathPrepend = (
      b: b,
      ~input,
      ~path,
      ~dynamicLocationVar as maybeDynamicLocationVar=?,
      ~appendSafe=?,
      fn,
    ) => {
      if path === Path.empty && maybeDynamicLocationVar === None {
        fn(b, ~input, ~path)
      } else {
        try b->withCatch(
          ~input,
          ~catch=(b, ~errorVar) => {
            b.code = `${errorVar}.path=${path->Stdlib.Inlined.Value.fromString}+${switch maybeDynamicLocationVar {
              | Some(var) => `'["'+${var}+'"]'+`
              | _ => ""
              }}${errorVar}.path`
            None
          },
          ~appendSafe?,
          b => fn(b, ~input, ~path=Path.empty),
        ) catch {
        | _ =>
          let error = %raw(`exn`)->InternalError.getOrRethrow
          Stdlib.Exn.raiseAny(
            InternalError.make(
              ~path=path->Path.concat(Path.dynamic)->Path.concat(error.path),
              ~code=error.code,
              ~flag=error.flag,
            ),
          )
        }
      }
    }

    let rec validation = (b, ~inputVar, ~schema, ~negative) => {
      let eq = negative ? "!==" : "==="
      let and_ = negative ? "||" : "&&"
      let exp = negative ? "!" : ""
      switch schema {
      | {tag: Undefined} => `${inputVar}${eq}void 0`
      | {tag: Null} => `${inputVar}${eq}null`
      | {tag: NaN} => exp ++ `Number.isNaN(${inputVar})`
      | {const: _} => `${inputVar}${eq}${inlineConst(b, schema)}`
      | {tag: Instance, ?class} => {
          let c = `${inputVar} instanceof ${b->embed(class)}`
          negative ? `!(${c})` : c
        }
      | {tag: Number as tag} => `typeof ${inputVar}${eq}"${(tag :> string)}"`
      | {tag: Array} => `${exp}Array.isArray(${inputVar})`
      | {tag: Object as tag} =>
        `typeof ${inputVar}${eq}"${(tag :> string)}"${and_}${exp}${inputVar}`
      | {tag, const: ?_} => `typeof ${inputVar}${eq}"${(tag :> string)}"`
      }
    }
    and refinement = (b, ~inputVar, ~schema, ~negative) => {
      let eq = negative ? "!==" : "==="
      let and_ = negative ? "||" : "&&"
      let not_ = negative ? "" : "!"
      let lt = negative ? ">" : "<"
      let gt = negative ? "<" : ">"

      switch schema {
      | {const: _} => ""
      | {format: Int32} =>
        `${and_}${inputVar}${lt}2147483647${and_}${inputVar}${gt}-2147483648${and_}${inputVar}%1${eq}0`
      | {tag: Number} =>
        if globalConfig.disableNanNumberValidation {
          ""
        } else {
          `${and_}${not_}Number.isNaN(${inputVar})`
        }
      | {tag: Object as tag | Array as tag, ?additionalItems, ?items} => {
          let additionalItems = additionalItems->Stdlib.Option.unsafeUnwrap
          let items = items->Stdlib.Option.unsafeUnwrap

          let length = items->Js.Array2.length

          let code = ref(
            if tag === Array {
              switch additionalItems {
              | Strict => `${and_}${inputVar}.length${eq}${length->Stdlib.Int.unsafeToString}`
              | Strip => `${and_}${inputVar}.length${gt}${length->Stdlib.Int.unsafeToString}`
              | Schema(_) => ""
              }
            } else if additionalItems === Strip {
              ""
            } else {
              `${and_}${not_}Array.isArray(${inputVar})`
            },
          )
          for idx in 0 to items->Js.Array2.length - 1 {
            let {schema: item, inlinedLocation} = items->Js.Array2.unsafe_get(idx)
            let item = item->toInternal
            let itemCode = if (
              (item->isLiteral && !(item.catch->Stdlib.Option.unsafeUnwrap)) ||
                schema.unnest->Stdlib.Option.unsafeUnwrap
            ) {
              b->validation(
                ~inputVar=Path.concat(inputVar, Path.fromInlinedLocation(inlinedLocation)),
                ~schema=item,
                ~negative,
              )
            } else if item.tag === Object {
              let inputVar = Path.concat(inputVar, Path.fromInlinedLocation(inlinedLocation))
              // TODO: Support noValidation
              b->validation(~inputVar, ~schema=item, ~negative) ++
                b->refinement(~inputVar, ~schema=item, ~negative)
            } else {
              ""
            }
            if itemCode !== "" {
              code.contents = code.contents ++ and_ ++ itemCode
            }
          }
          code.contents
        }
      | _ => ""
      }
    }

    let typeFilterCode = (b: b, ~schema, ~input, ~path) => {
      switch schema {
      | {tag: Unknown | Union | JSON | Never} | {noValidation: true} => ""
      | _ => {
          let inputVar = b->Val.var(input)

          `if(${b->validation(~inputVar, ~schema, ~negative=true)}${b->refinement(
              ~schema,
              ~inputVar,
              ~negative=true,
            )}){${b->failWithArg(
              ~path,
              input => InvalidType({
                expected: schema->fromInternal,
                received: input,
              }),
              inputVar,
            )}}`
        }
      }
    }

    @inline
    let parse = (b: b, ~schema, ~input, ~path) => {
      schema.builder(b, ~input, ~selfSchema=schema, ~path)
    }

    let parseWithTypeValidation = (b: b, ~schema, ~input, ~path) => {
      if b.global.flag->Flag.unsafeHas(Flag.typeValidation) || schema->isLiteral {
        b.code = b.code ++ b->typeFilterCode(~schema, ~input, ~path)
      }
      b->parse(~schema, ~input, ~path)
    }
  }

  let noop = make((_b, ~input, ~selfSchema as _, ~path as _) => input)

  let noopOperation = i => i->Obj.magic

  @inline
  let intitialInputVar = "i"
}
// TODO: Split validation code and transformation code
module B = Builder.B

let nonJsonableTags = Stdlib.Set.fromArray([
  (Unknown: tag),
  NaN,
  BigInt,
  Function,
  Instance,
  Symbol,
])

let rec internalCompile = (builder: builder, ~schema, ~flag) => {
  let b = B.rootScope(~flag)

  if flag->Flag.unsafeHas(Flag.jsonableOutput) {
    let output = schema->reverse
    jsonableValidation(~output, ~parent=output, ~path=Path.empty, ~flag, ~recSet=None)
  }

  let input = {b, var: B._var, isAsync: false, inline: Builder.intitialInputVar}

  let output = builder(b, ~input, ~selfSchema=schema, ~path=Path.empty)
  schema.isAsync = Some(output.isAsync)

  if b.varsAllocation !== "" {
    b.code = `let ${b.varsAllocation};${b.code}`
  }

  if flag->Flag.unsafeHas(Flag.typeValidation) || schema->isLiteral {
    b.code = b->B.typeFilterCode(~schema, ~input, ~path=Path.empty) ++ b.code
  }

  if (
    b.code === "" &&
    output === input &&
    !(
      flag->Flag.unsafeHas(
        Flag.assertOutput
        ->Flag.with(Flag.async)
        ->Flag.with(Flag.jsonStringOutput),
      )
    )
  ) {
    Builder.noopOperation
  } else {
    let inlinedOutput = ref(
      if flag->Flag.unsafeHas(Flag.assertOutput) {
        `void 0`
      } else {
        output.inline
      },
    )
    if flag->Flag.unsafeHas(Flag.jsonStringOutput) {
      inlinedOutput := `JSON.stringify(${inlinedOutput.contents})`
    }
    if flag->Flag.unsafeHas(Flag.async) && !output.isAsync {
      inlinedOutput := `Promise.resolve(${inlinedOutput.contents})`
    }

    let inlinedFunction = `${Builder.intitialInputVar}=>{${b.code}return ${inlinedOutput.contents}}`

    // Js.log(inlinedFunction)

    Stdlib.Function.make2(
      ~ctxVarName1="e",
      ~ctxVarValue1=b.global.embeded,
      ~ctxVarName2="s",
      ~ctxVarValue2=symbol,
      ~inlinedFunction,
    )
  }
}
and operationFn = (s, o) => {
  let s = s->toInternal
  if %raw(`o in s`) {
    %raw(`s[o]`)
  } else {
    let ss = o->Flag.unsafeHas(Flag.reverse) ? s->reverse : s
    let f = ss.builder->internalCompile(~schema=ss, ~flag=o)
    let _ = %raw(`s[o] = f`)
    f
  }
}
and reverse = (schema: internal) => {
  switch schema.output {
  | None => schema
  | Some(fn) =>
    if Js.typeof(fn) === "object" {
      fn->Obj.magic
    } else {
      let reversed = (fn->Obj.magic)["call"](schema)
      // If the reversed schema is reversing to self,
      // it's mostlikely a primitive, which we can't mutate,
      // so we can just copy it
      let reversed = if reversed.output === None {
        reversed->copy
      } else {
        reversed
      }
      if reversed.standard === None {
        let _ = reversed->toStandard
      }

      schema.output = reversed->Obj.magic
      reversed.output = schema->Obj.magic
      reversed
    }
  }
}
and toStandard = (schema: internal) => {
  (schema->Obj.magic)["with"] = %raw(`w`)
  schema.standard = Some({
    version: 1,
    vendor,
    validate: input => {
      try {
        {
          "value": (schema->fromInternal->operationFn(Flag.typeValidation))(
            input->Obj.magic,
          )->Obj.magic,
        }
      } catch {
      | _ => {
          let error = %raw(`exn`)->InternalError.getOrRethrow
          {
            "issues": [
              {
                "message": error->ErrorClass.message,
                "path": error.path === Path.empty ? None : Some(error.path->Path.toArray),
              },
            ],
          }->Obj.magic
        }
      }
    },
  })
  schema->(Obj.magic: internal => t<'any>)
}
and jsonableValidation = (~output, ~parent, ~path, ~flag, ~recSet) => {
  let tag = output.tag
  if (tag === Undefined && parent.tag !== Object) || nonJsonableTags->Stdlib.Set.has(tag) {
    Stdlib.Exn.raiseAny(
      InternalError.make(~code=InvalidJsonSchema(parent->fromInternal), ~flag, ~path),
    )
  }
  switch output {
  | {tag: Union | Array | Object} =>
    let recSet = switch recSet {
    | None => Stdlib.Set.make()
    | Some(set) => set
    }
    if recSet->Stdlib.Set.has(output) {
      ()
    } else {
      recSet->Stdlib.Set.add(output)
      let recSet = Some(recSet)
      if tag === Union {
        output.anyOf
        ->Stdlib.Option.unsafeUnwrap
        ->Js.Array2.forEach(s => jsonableValidation(~output=s, ~parent, ~path, ~flag, ~recSet))
      } else {
        switch output {
        | {items, ?additionalItems} => {
            switch additionalItems->Stdlib.Option.unsafeUnwrap {
            | Schema(additionalItems) =>
              jsonableValidation(
                ~output=additionalItems->toInternal,
                ~parent,
                ~path,
                ~flag,
                ~recSet,
              )

            | _ => ()
            }
            items->Js.Array2.forEach(item => {
              jsonableValidation(
                ~output=item.schema->toInternal,
                ~parent=output,
                ~path=path->Path.concat(Path.fromInlinedLocation(item.inlinedLocation)),
                ~flag,
                ~recSet,
              )
            })
          }
        | _ => ()
        }
      }
    }

  | _ => ()
  }
}

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

@@warning("-37")
type internalInput =
  | Output
  | Input
  | Any
  | Json
  | JsonString
type internalOutput =
  | Output
  | Input
  | Assert
  | Json
  | JsonString
type internalMode =
  | Sync
  | Async
@@warning("+37")

let compile = (
  schema: t<'value>,
  ~input: input<'value, 'input>,
  ~output: output<'value, 'transformedOutput>,
  ~mode: mode<'transformedOutput, 'output>,
  ~typeValidation=true,
): ('input => 'output) => {
  let output = output->(Obj.magic: output<'value, 'transformedOutput> => internalOutput)
  let input = input->(Obj.magic: input<'schemaInput, 'input> => internalInput)
  let mode = mode->(Obj.magic: mode<'transformedOutput, 'output> => internalMode)

  let schema = schema->toUnknown

  let flag = ref(Flag.none)
  switch output {
  | Output
  | Input => {
      if output === input->Obj.magic {
        InternalError.panic(`Can't compile operation to converting value to self`)
      }
      ()
    }
  | Assert => flag := flag.contents->Flag.with(Flag.assertOutput)
  | Json => flag := flag.contents->Flag.with(Flag.jsonableOutput)
  | JsonString =>
    flag := flag.contents->Flag.with(Flag.jsonableOutput->Flag.with(Flag.jsonStringOutput))
  }
  switch mode {
  | Sync => ()
  | Async => flag := flag.contents->Flag.with(Flag.async)
  }
  if typeValidation {
    flag := flag.contents->Flag.with(Flag.typeValidation)
  }
  if input === Output {
    flag := flag.contents->Flag.with(Flag.reverse)
  }
  let fn = schema->operationFn(flag.contents)->Obj.magic

  switch input {
  | JsonString =>
    let flag = flag.contents
    jsonString => {
      try jsonString->Obj.magic->Js.Json.parseExn->fn catch {
      | _ =>
        Stdlib.Exn.raiseAny(
          InternalError.make(~code=OperationFailed(%raw(`exn.message`)), ~flag, ~path=Path.empty),
        )
      }
    }
  | _ => fn
  }
}

module Output = {
  let item = (~factory, ~item) => {
    () => {
      let reversed = item->reverse
      if reversed === item {
        %raw(`this`)
      } else {
        factory(reversed->fromInternal)->toInternal
      }
    }
  }
}

// =============
// Operations
// =============

@inline
let parseOrThrow = (any, schema) => {
  (schema->operationFn(Flag.typeValidation))(any)
}

let parseJsonOrThrow = parseOrThrow->Obj.magic

let parseJsonStringOrThrow = (jsonString: string, schema: t<'value>): 'value => {
  try {
    jsonString->Js.Json.parseExn
  } catch {
  | _ =>
    Stdlib.Exn.raiseAny(
      InternalError.make(
        ~code=OperationFailed(%raw(`exn.message`)),
        ~flag=Flag.typeValidation,
        ~path=Path.empty,
      ),
    )
  }->parseOrThrow(schema)
}

let parseAsyncOrThrow = (any, schema) => {
  (schema->operationFn(Flag.async->Flag.with(Flag.typeValidation)))(any)
}

let convertOrThrow = (input, schema) => {
  (schema->operationFn(Flag.none))(input)
}

let convertToJsonOrThrow = (any, schema) => {
  (schema->operationFn(Flag.jsonableOutput))(any)
}

let convertToJsonStringOrThrow = (input, schema) => {
  (schema->operationFn(Flag.jsonableOutput->Flag.with(Flag.jsonStringOutput)))(input)
}

let convertAsyncOrThrow = (any, schema) => {
  (schema->operationFn(Flag.async))(any)
}

let reverseConvertOrThrow = (value, schema) => {
  (schema->operationFn(Flag.reverse))(value)
}

@inline
let reverseConvertToJsonOrThrow = (value, schema) => {
  (schema->operationFn(Flag.jsonableOutput->Flag.with(Flag.reverse)))(value)
}

let reverseConvertToJsonStringOrThrow = (value: 'value, schema: t<'value>, ~space=0): string => {
  value->reverseConvertToJsonOrThrow(schema)->Js.Json.stringifyWithSpace(space)
}

let assertOrThrow = (any, schema) => {
  (schema->operationFn(Flag.typeValidation->Flag.with(Flag.assertOutput)))(any)
}

module Literal = {
  open Stdlib

  let undefined = {
    tag: Undefined,
    const: %raw(`void 0`),
    builder: Builder.noop,
  }

  let null = {
    tag: Null,
    const: %raw(`null`),
    builder: Builder.noop,
  }

  @inline
  let make = tag => {
    {
      tag,
      builder: Builder.noop,
    }
  }

  let parse = (value): internal => {
    let value = value->castAnyToUnknown
    if value === %raw(`null`) {
      null
    } else {
      let schema = switch value->Type.typeof {
      | #undefined => undefined
      | #number if value->(Obj.magic: unknown => float)->Js.Float.isNaN => make(NaN)
      | #object => {
          let i = make(Instance)
          i.class = (value->Obj.magic)["constructor"]
          i
        }
      | typeof => make(typeof->(Obj.magic: Stdlib.Type.t => tag))
      }
      schema.const = Some(value->Obj.magic)
      schema
    }
  }
}

let isAsync = schema => {
  let schema = schema->toInternal
  switch schema.isAsync {
  | None =>
    try {
      let b = B.rootScope(~flag=Flag.async)
      let input = {
        b,
        var: B._var,
        isAsync: false,
        inline: Builder.intitialInputVar,
      }
      let output = schema.builder(b, ~input, ~selfSchema=schema, ~path=Path.empty)
      schema.isAsync = Some(output.isAsync)
      output.isAsync
    } catch {
    | _ => {
        let _ = %raw(`exn`)->InternalError.getOrRethrow
        false
      }
    }
  | Some(v) => v
  }
}

let wrapExnToFailure = exn => {
  if %raw("exn&&exn.s===symbol") {
    Failure({error: exn->(Obj.magic: exn => error)})
  } else {
    raise(exn)
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
    fn()->Stdlib.Promise.thenResolveWithCatch(value => Success({value: value}), wrapExnToFailure)
  } catch {
  | _ => Stdlib.Promise.resolve(wrapExnToFailure(%raw(`exn`)))
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
    let schema = schema->toInternal
    let mut = schema->copy
    mut->setInPlace(~id, metadata)
    mut->toStandard
  }
}

let recursive = fn => {
  let r = "r" ++ globalConfig.recCounter->Stdlib.Int.unsafeToString
  globalConfig.recCounter = globalConfig.recCounter + 1

  let builder = Builder.make((b, ~input, ~selfSchema as _, ~path as _) => {
    b->B.transform(~input, (_b, ~input) => {
      B.Val.map(r, input)
    })
  })
  let output = () => {
    tag: Unknown,
    builder: Builder.make((_b, ~input, ~selfSchema as _, ~path as _) => {
      B.Val.map(r, input)
    }),
  }

  let placeholder: internal = {
    tag: Unknown,
    builder,
    output,
    name: "Self",
  }
  let schema = fn(placeholder->fromInternal)->toInternal

  mergeInPlace(placeholder, schema)
  placeholder.name = Some(schema->fromInternal->toExpression)
  placeholder.builder = builder
  placeholder.output = Some(output)

  let initialParseOperationBuilder = schema.builder
  schema.builder = Builder.make((b, ~input, ~selfSchema, ~path) => {
    let inputVar = b->B.Val.var(input)
    let bb = b->B.scope
    let opOutput = initialParseOperationBuilder(bb, ~input, ~selfSchema, ~path=Path.empty)
    let opBodyCode = bb->B.allocateScope ++ `return ${opOutput.inline}`
    b.code = b.code ++ `let ${r}=${inputVar}=>{${opBodyCode}};`
    b->B.withPathPrepend(~input, ~path, (b, ~input, ~path as _) => {
      b->B.transform(
        ~input,
        (_b, ~input) => {
          let output = B.Val.map(r, input)
          if opOutput.isAsync {
            output.isAsync = true
            placeholder.builder = Builder.make(
              (b, ~input, ~selfSchema as _, ~path as _) => {
                b->B.transform(
                  ~input,
                  (_b, ~input) => {
                    let output = B.Val.map(r, input)
                    output.isAsync = true
                    output
                  },
                )
              },
            )
          }
          output
        },
      )
    })
  })

  let initialReverse = schema.output->Obj.magic->Stdlib.Fn.bind(~this=schema)
  schema.output = Some(
    () => {
      let initialReversed = initialReverse()
      let mut = initialReversed->copy
      mut.output = schema->Obj.magic
      schema.output = mut->Obj.magic
      mut.builder = Builder.make((b, ~input, ~selfSchema, ~path) => {
        let inputVar = b->B.Val.var(input)
        let bb = b->B.scope
        let initialInput = {
          ...input,
          b: bb,
        }
        let opOutput = initialReversed.builder(
          bb,
          ~input=initialInput,
          ~selfSchema,
          ~path=Path.empty,
        )
        let opBodyCode = bb->B.allocateScope ++ `return ${opOutput.inline}`
        b.code = b.code ++ `let ${r}=${inputVar}=>{${opBodyCode}};`
        b->B.withPathPrepend(~input, ~path, (_b, ~input, ~path as _) => B.Val.map(r, input))
      })
      mut
    },
  )

  schema->toStandard
}

let noValidation = (schema, value) => {
  let schema = schema->toInternal
  let mut = schema->copy

  // FIXME: Test for discriminant literal
  mut.noValidation = Some(value) // TODO: Better test reverse
  mut->toStandard
}

let internalRefine = (schema, refiner) => {
  let schema = schema->toInternal
  let mut = schema->copy
  mut.builder = Builder.make((b, ~input, ~selfSchema, ~path) => {
    b->B.transform(~input=b->B.parse(~schema, ~input, ~path), (b, ~input) => {
      let bb = b->B.scope
      let rCode = refiner(bb, ~inputVar=bb->B.Val.var(input), ~selfSchema, ~path)
      b.code = b.code ++ bb->B.allocateScope ++ rCode
      input
    })
  })
  mut.output = Some(
    () => {
      let schema = schema->reverse
      let mut = schema->copy
      mut.builder = (b, ~input, ~selfSchema, ~path) => {
        b->B.parse(
          ~schema,
          ~input=b->B.transform(~input, (b, ~input) => {
            b.code = b.code ++ refiner(b, ~inputVar=b->B.Val.var(input), ~selfSchema, ~path)
            input
          }),
          ~path,
        )
      }
      mut
    },
  )
  mut->toStandard
}

let refine: (t<'value>, s<'value> => 'value => unit) => t<'value> = (schema, refiner) => {
  schema->internalRefine((b, ~inputVar, ~selfSchema, ~path) => {
    `${b->B.embed(refiner(b->B.effectCtx(~selfSchema, ~path)))}(${inputVar});`
  })
}

let addRefinement = (schema, ~metadataId, ~refinement, ~refiner) => {
  schema
  ->Metadata.set(
    ~id=metadataId,
    switch schema->Metadata.get(~id=metadataId) {
    | Some(refinements) => refinements->Stdlib.Array.append(refinement)
    | None => [refinement]
    },
  )
  ->internalRefine(refiner)
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
  let schema = schema->toInternal
  let mut = schema->copy
  mut.builder = Builder.make((b, ~input, ~selfSchema, ~path) => {
    let input = b->B.parse(~schema, ~input, ~path)

    switch transformer(b->B.effectCtx(~selfSchema, ~path)) {
    | {parser, asyncParser: ?None} => b->B.embedSyncOperation(~input, ~fn=parser)
    | {parser: ?None, asyncParser} => b->B.embedAsyncOperation(~input, ~fn=asyncParser)
    | {parser: ?None, asyncParser: ?None, serializer: ?None} => input
    | {parser: ?None, asyncParser: ?None, serializer: _} =>
      b->B.invalidOperation(~path, ~description=`The S.transform parser is missing`)
    | {parser: _, asyncParser: _} =>
      b->B.invalidOperation(
        ~path,
        ~description=`The S.transform doesn't allow parser and asyncParser at the same time. Remove parser in favor of asyncParser`,
      )
    }
  })
  mut.output = Some(
    () => {
      let schema = schema->reverse
      {
        tag: Unknown,
        builder: (b, ~input, ~selfSchema, ~path) => {
          switch transformer(b->B.effectCtx(~selfSchema, ~path)) {
          | {serializer} =>
            b->B.parse(~schema, ~input=b->B.embedSyncOperation(~input, ~fn=serializer), ~path)
          | {parser: ?None, asyncParser: ?None, serializer: ?None} =>
            b->B.parse(~schema, ~input, ~path)
          | {serializer: ?None, asyncParser: ?Some(_)}
          | {serializer: ?None, parser: ?Some(_)} =>
            b->B.invalidOperation(~path, ~description=`The S.transform serializer is missing`)
          }
        },
      }
    },
  )
  mut.isAsync = None
  mut->toStandard
}

type customDefinition<'input, 'output> = {
  @as("p")
  parser?: unknown => 'output,
  @as("a")
  asyncParser?: unknown => promise<'output>,
  @as("s")
  serializer?: 'output => 'input,
}
let custom = (name, definer) =>
  {
    name,
    tag: Unknown,
    builder: Builder.make((b, ~input, ~selfSchema, ~path) => {
      switch definer(b->B.effectCtx(~selfSchema, ~path)) {
      | {parser, asyncParser: ?None} => b->B.embedSyncOperation(~input, ~fn=parser)
      | {parser: ?None, asyncParser} => b->B.embedAsyncOperation(~input, ~fn=asyncParser)
      | {parser: ?None, asyncParser: ?None, serializer: ?None} => input
      | {parser: ?None, asyncParser: ?None, serializer: _} =>
        b->B.invalidOperation(~path, ~description=`The S.custom parser is missing`)
      | {parser: _, asyncParser: _} =>
        b->B.invalidOperation(
          ~path,
          ~description=`The S.custom doesn't allow parser and asyncParser at the same time. Remove parser in favor of asyncParser`,
        )
      }
    }),
    output: () => {
      tag: Unknown,
      builder: Builder.make((b, ~input, ~selfSchema, ~path) => {
        switch definer(b->B.effectCtx(~selfSchema, ~path)) {
        | {serializer} => b->B.embedSyncOperation(~input, ~fn=serializer)
        | {parser: ?None, asyncParser: ?None, serializer: ?None} => input
        | {serializer: ?None, asyncParser: ?Some(_)}
        | {serializer: ?None, parser: ?Some(_)} =>
          b->B.invalidOperation(~path, ~description=`The S.custom serializer is missing`)
        }
      }),
    },
  }->toStandard

let unit = Literal.undefined->toStandard

let nullAsUnit = {
  let output = () => {
    {
      tag: Undefined,
      const: %raw(`void 0`),
      builder: Builder.make((b, ~input as _, ~selfSchema as _, ~path as _) => {
        b->B.val("null")
      }),
    }
  }
  {
    tag: Null,
    const: %raw(`null`),
    builder: Builder.make((b, ~input as _, ~selfSchema as _, ~path as _) => {
      b->B.val("void 0")
    }),
    output,
  }->toStandard
}

let unknown = {
  tag: Unknown,
  builder: Builder.noop,
}->toStandard

module Never = {
  let builder = Builder.make((b, ~input, ~selfSchema, ~path) => {
    b.code =
      b.code ++
      b->B.failWithArg(
        ~path,
        input => InvalidType({
          expected: selfSchema->fromInternal,
          received: input,
        }),
        input.inline,
      ) ++ ";"
    input
  })

  let schema = {
    tag: Never,
    builder,
  }->toStandard
}

module Union = {
  let getItemCode = (b, ~schema, ~input, ~output: val, ~deopt, ~path) => {
    try {
      let bb = b->B.scope
      if deopt {
        bb.code = bb.code ++ bb->B.typeFilterCode(~schema, ~input, ~path)
      }
      let itemOutput = bb->B.parse(~schema, ~input, ~path)

      if itemOutput !== input {
        itemOutput.b = bb
        if schema.tag === Unknown {
          let reversed = schema->reverse
          bb.code = bb.code ++ bb->B.typeFilterCode(~schema=reversed, ~input=itemOutput, ~path)
        }

        if itemOutput.isAsync {
          output.isAsync = true
        }
        bb.code =
          bb.code ++
          // Need to allocate a var here, so we don't mutate the input object field
          `${output.var(b)}=${itemOutput.inline}`
      }

      bb->B.allocateScope
    } catch {
    | _ => "throw " ++ b->B.embed(%raw(`exn`)->InternalError.getOrRethrow)
    }
  }

  let isPriority = (tag: string, byTag: dict<array<internal>>) => {
    ((tag === (Array: tag :> string) || tag === (Instance: tag :> string)) &&
      byTag->Stdlib.Dict.has((Object: tag :> string))) ||
      (tag === (NaN: tag :> string) && byTag->Stdlib.Dict.has((Number: tag :> string)))
  }

  let builder = Builder.make((b, ~input, ~selfSchema, ~path) => {
    let fail = caught => {
      `${b->B.embed(_ => {
          let args = %raw(`arguments`)
          b->B.raise(
            ~path,
            ~code=InvalidType({
              expected: selfSchema->fromInternal,
              received: args->Js.Array2.unsafe_get(0),
              unionErrors: ?(
                args->Js.Array2.length > 1
                  ? Some(args->Stdlib.Array.fromArguments->Js.Array2.sliceFrom(1))
                  : None
              ),
            }),
          )
        })}(${input.var(b)}${caught})`
    }

    let schemas = selfSchema.anyOf->Stdlib.Option.unsafeUnwrap
    let typeValidation = b.global.flag->Flag.unsafeHas(Flag.typeValidation)

    // FIXME: Test with async
    let output = input
    let initialInline = input.inline

    let deoptIdx = ref(-1)
    let lastIdx = schemas->Js.Array2.length - 1
    let byTag = ref(Js.Dict.empty())
    let tags = ref([])
    for idx in 0 to lastIdx {
      let schema = schemas->Js.Array2.unsafe_get(idx)

      switch schema.tag {
      | Union
      | JSON
      | Unknown
      | Never =>
        deoptIdx := idx
        byTag := Js.Dict.empty()
        tags := []

      | tag =>
        switch byTag.contents->Stdlib.Dict.unsafeGetOption((tag :> string)) {
        | Some(arr) =>
          // There can only be one valid. Dedupe
          if tag !== Undefined && tag !== Null && tag !== NaN {
            arr->Js.Array2.push(schema)->ignore
          }
        | None => {
            if isPriority((tag :> string), byTag.contents) {
              // Not the fastest way, but it's the simplest way
              // to make sure NaN is checked before number
              // And instance and array checked before object
              tags.contents->Js.Array2.unshift((tag :> string))->ignore
            } else {
              tags.contents->Js.Array2.push((tag :> string))->ignore
            }
            byTag.contents->Js.Dict.set((tag :> string), [schema])
          }
        }
      }
    }
    let deoptIdx = deoptIdx.contents
    let byTag = byTag.contents
    let tags = tags.contents

    let start = ref("")
    let end = ref("")
    let caught = ref("")

    if deoptIdx !== -1 {
      for idx in 0 to deoptIdx {
        let schema = schemas->Js.Array2.unsafe_get(idx)
        let itemCode = b->getItemCode(~schema, ~input, ~output, ~deopt=true, ~path)
        if itemCode->Stdlib.String.unsafeToBool {
          let errorVar = `e` ++ idx->Stdlib.Int.unsafeToString
          start := start.contents ++ `try{${itemCode}}catch(${errorVar}){`
          end := "}" ++ end.contents
          caught := `${caught.contents},${errorVar}`
        }
      }
    }

    let nextElse = ref(false)
    let noop = ref("")

    for idx in 0 to tags->Js.Array2.length - 1 {
      let tag = tags->Js.Array2.unsafe_get(idx)
      let schemas = byTag->Js.Dict.unsafeGet(tag)
      let inputVar = input.var(b)

      let isMultiple = schemas->Js.Array2.length > 1
      let firstSchema = schemas->Js.Array2.unsafe_get(0)

      let cond = ref("")

      let body = if isMultiple {
        let itemStart = ref("")
        let itemEnd = ref("")
        let itemNextElse = ref(false)
        let itemNoop = ref("")
        let caught = ref("")

        let itemIdx = ref(0)
        let lastIdx = schemas->Js.Array2.length - 1
        while itemIdx.contents <= lastIdx {
          let schema = schemas->Js.Array2.unsafe_get(itemIdx.contents)
          let itemCond =
            (schema->isLiteral ? b->B.validation(~inputVar, ~schema, ~negative=false) : "") ++
            b->B.refinement(~inputVar, ~schema, ~negative=false)->Js.String2.sliceToEnd(~from=2)
          let itemCode = b->getItemCode(~schema, ~input, ~output, ~deopt=false, ~path)

          if itemCond->Stdlib.String.unsafeToBool && !(itemCode->Stdlib.String.unsafeToBool) {
            itemNoop := (
                itemNoop.contents->Stdlib.String.unsafeToBool
                  ? `${itemNoop.contents}||${itemCond}`
                  : itemCond
              )
          } else if itemNoop.contents->Stdlib.String.unsafeToBool {
            let if_ = itemNextElse.contents ? "else if" : "if"
            itemStart := itemStart.contents ++ if_ ++ `(!(${itemNoop.contents})){`
            itemEnd := "}" ++ itemEnd.contents
            itemNoop := ""
            itemNextElse := false
          }

          // Have a refinement and can handle the specific case
          if itemCond->Stdlib.String.unsafeToBool {
            if itemCode->Stdlib.String.unsafeToBool {
              let if_ = itemNextElse.contents ? "else if" : "if"
              itemStart := itemStart.contents ++ if_ ++ `(${itemCond}){${itemCode}}`
              itemNextElse := true
            }
          } // The item without refinement should switch to deopt mode
          // Since there might be validation in the body
          else if itemCode->Stdlib.String.unsafeToBool {
            let errorVar = `e` ++ itemIdx.contents->Stdlib.Int.unsafeToString
            itemStart :=
              itemStart.contents ++
              `${itemNextElse.contents ? "else{" : ""}try{${itemCode}}catch(${errorVar}){`
            itemEnd := (itemNextElse.contents ? "}" : "") ++ "}" ++ itemEnd.contents
            caught := `${caught.contents},${errorVar}`
            itemNextElse := false
          } else {
            // If there's no body, we immideately finish.
            // Even if there might be other items, this case is always valid
            itemIdx := lastIdx
          }
          itemIdx := itemIdx.contents->Stdlib.Int.plus(1)
        }

        cond :=
          b->B.validation(
            ~inputVar,
            ~schema={tag: firstSchema.tag, builder: %raw(`0`)},
            ~negative=false,
          )

        if itemNoop.contents->Stdlib.String.unsafeToBool {
          if itemStart.contents->Stdlib.String.unsafeToBool {
            if typeValidation {
              let if_ = itemNextElse.contents ? "else if" : "if"
              itemStart :=
                itemStart.contents ++ if_ ++ `(!(${itemNoop.contents})){${fail(caught.contents)}}`
            }
          } else {
            cond := cond.contents ++ `&&(${itemNoop.contents})`
          }
        } else if typeValidation && itemStart.contents->Stdlib.String.unsafeToBool {
          let errorCode = fail(caught.contents)
          itemStart :=
            itemStart.contents ++ (itemNextElse.contents ? `else{${errorCode}}` : errorCode)
        }

        itemStart.contents ++ itemEnd.contents
      } else {
        cond :=
          b->B.validation(~inputVar, ~schema=firstSchema, ~negative=false) ++
            b->B.refinement(~inputVar, ~schema=firstSchema, ~negative=false)
        b->getItemCode(~schema=firstSchema, ~input, ~output, ~deopt=false, ~path)
      }
      let cond = cond.contents

      if body->Stdlib.String.unsafeToBool || isPriority((tag :> string), byTag) {
        let if_ = nextElse.contents ? "else if" : "if"
        start := start.contents ++ if_ ++ `(${cond}){${body}}`
        nextElse := true
      } else {
        noop := (noop.contents->Stdlib.String.unsafeToBool ? `${noop.contents}||${cond}` : cond)
      }
    }

    if typeValidation || deoptIdx === lastIdx {
      let errorCode = fail(caught.contents)
      start :=
        start.contents ++ if noop.contents->Stdlib.String.unsafeToBool {
          let if_ = nextElse.contents ? "else if" : "if"
          if_ ++ `(!(${noop.contents})){${errorCode}}`
        } else if nextElse.contents {
          `else{${errorCode}}`
        } else {
          errorCode
        }
    }

    b.code = b.code ++ start.contents ++ end.contents

    if output.isAsync {
      b->B.asyncVal(`Promise.resolve(${output.inline})`)
    } else if output.var === B._var {
      // TODO: Think how to make it more robust
      // Recreate to not break the logic to determine
      // whether the output is changed

      // Use output.b instead of b because of withCatch
      // Should refactor withCatch to make it simpler
      // All of this is a hack to make withCatch think that there are no changes. eg S.array(S.option(item))
      if (
        b.code === "" &&
        output.b.code === "" &&
        (output.b.varsAllocation === `${output.inline}=${initialInline}` || initialInline === "i")
      ) {
        output.b.varsAllocation = ""
        output.b.allocate = B.initialAllocate
        output.var = B._notVar
        output.inline = initialInline
        output
      } else {
        {
          ...output,
          b: output.b,
        }
      }
    } else {
      output
    }
  })

  let rec factory = schemas => {
    let schemas: array<internal> = schemas->Obj.magic
    // TODO:
    // 1. Fitler out items without parser
    // 2. Remove duplicate schemas
    // 3. Spread Union and JSON if they are not transformed
    // 4. Provide correct `has` value for Union and JSON
    switch schemas {
    | [] => InternalError.panic("S.union requires at least one item")
    | [schema] => schema->fromInternal
    | _ =>
      let has = Js.Dict.empty()
      let anyOf = Stdlib.Set.make()

      for idx in 0 to schemas->Js.Array2.length - 1 {
        let schema = schemas->Js.Array2.unsafe_get(idx)

        // Check if the union is not transformed
        if schema.tag === Union && schema.builder === builder {
          schema.anyOf
          ->Stdlib.Option.unsafeUnwrap
          ->Js.Array2.forEach(item => {
            anyOf->Stdlib.Set.add(item)
          })
          let _ = has->Stdlib.Dict.mixin(schema.has->Stdlib.Option.unsafeUnwrap)
        } else {
          anyOf->Stdlib.Set.add(schema)
          has->Js.Dict.set(
            (switch schema.tag {
            | Union
            | JSON =>
              Unknown
            | v => v
            }: tag :> string),
            true,
          )
        }
      }
      {
        tag: Union,
        has,
        anyOf: anyOf->Stdlib.Set.toArray,
        builder,
        output,
      }->toStandard
    }
  }
  and output = () => {
    let schemas = (%raw(`this`): internal).anyOf->Stdlib.Option.unsafeUnwrap
    let items = []
    let toSelf = ref(true)
    for idx in 0 to schemas->Js.Array2.length - 1 {
      let schema = schemas->Js.Array2.unsafe_get(idx)
      let reversed = schema->reverse
      items->Js.Array2.unsafe_set(idx, reversed->fromInternal)
      toSelf := toSelf.contents && schema === reversed
    }
    if toSelf.contents {
      %raw(`this`)
    } else {
      factory(items)->toInternal
    }
  }
}

module Option = {
  type default = Value(unknown) | Callback(unit => unknown)

  let defaultMetadataId: Metadata.Id.t<default> = Metadata.Id.internal("Option.default")

  let default = schema => schema->Metadata.get(~id=defaultMetadataId)

  let nestedLoc = "BS_PRIVATE_NESTED_SOME_NONE"
  let nestedOption = {
    let inLoc = `"${nestedLoc}"`
    let nestedNone = () => {
      let item: item = {
        schema: Literal.parse(0)->fromInternal,
        location: nestedLoc,
        inlinedLocation: inLoc,
      }
      let fields = Js.Dict.empty()
      fields->Js.Dict.set(nestedLoc, item)

      {
        tag: Object,
        fields,
        items: [item],
        additionalItems: Strip,
        builder: Builder.make((b, ~input as _, ~selfSchema, ~path as _) => {
          b->B.val(b->B.inlineConst(selfSchema->reverse))
        }),
      }
    }

    let builder = Builder.make((b, ~input as _, ~selfSchema, ~path as _) => {
      b->B.val(
        `{${inLoc}:${(
            (
              (selfSchema->reverse).items->Stdlib.Option.unsafeUnwrap->Js.Array2.unsafe_get(0)
            ).schema->toInternal
          ).const->Obj.magic}}`,
      )
    })

    item => {
      let mut = item->copy

      mut.output = Some(nestedNone)
      mut.builder = builder

      mut
    }
  }

  let factory = (item, ~unit=unit) => {
    let item = item->toInternal

    switch item->reverse {
    | {tag: Undefined} => Union.factory([unit->toUnknown, item->nestedOption->fromInternal])
    | {tag: Union, ?has} as reversed
      if has->Stdlib.Option.unsafeUnwrap->Stdlib.Dict.has((Undefined: tag :> string)) => {
        let mut = reversed->copy
        let schemas = mut.anyOf->Stdlib.Option.unsafeUnwrap
        let has = mut.has->Stdlib.Option.unsafeUnwrap

        let anyOf = []
        for idx in 0 to schemas->Array.length - 1 {
          let schema = schemas->Js.Array2.unsafe_get(idx)
          anyOf
          ->Js.Array2.push(
            switch schema {
            | {tag: Undefined} => {
                if !(has->Stdlib.Dict.has((Object: tag :> string))) {
                  // TODO: Replace with dict{} in ReScript v12
                  let d = Js.Dict.empty()
                  d->Js.Dict.set((Object: tag :> string), true)
                  mut.has = Some(d->Stdlib.Dict.mixin(has))
                }
                anyOf->Js.Array2.push(unit->toInternal->reverse)->ignore
                schema->reverse->nestedOption->reverse
              }
            | {fields} =>
              switch fields->Stdlib.Dict.unsafeGetOption(nestedLoc) {
              | Some(item) => {
                  let fSchema = item.schema->toInternal
                  let newItem = {
                    ...item,
                    schema: {
                      tag: fSchema.tag,
                      builder: fSchema.builder,
                      const: fSchema.const->Obj.magic->Stdlib.Int.plus(1)->Obj.magic,
                    }->fromInternal,
                  }
                  let mut = schema->copy
                  let fields = Js.Dict.empty()
                  fields->Js.Dict.set(nestedLoc, newItem)
                  mut.items = Some([newItem])
                  mut.fields = Some(fields)
                  (mut->reverse).output = mut->Obj.magic
                  mut
                }
              | None => schema
              }
            | _ => schema
            },
          )
          ->ignore
        }

        mut.anyOf = Some(anyOf)
        mut.output = Some(Union.output) // FIXME: Shouldn't manually update output
        mut->reverse->fromInternal
      }
    | _ => Union.factory([item->fromInternal, unit->toUnknown])
    }
  }

  let getWithDefault = (schema: t<option<'value>>, default) => {
    let schema = schema->toInternal
    let mut = schema->copy
    mut->Metadata.setInPlace(~id=defaultMetadataId, default)
    mut.builder = Builder.make((b, ~input, ~selfSchema as _, ~path) => {
      b->B.transform(~input=b->B.parse(~schema, ~input, ~path), (b, ~input) => {
        let inputVar = b->B.Val.var(input)
        b->B.val(
          `${inputVar}===void 0?${switch default {
            | Value(v) => b->B.embed(v)
            | Callback(cb) => `${b->B.embed(cb)}()`
            }}:${inputVar}`,
        )
      })
    })
    mut.output = Some(
      () => {
        let reversed = schema->reverse
        switch reversed {
        | {anyOf} =>
          // FIXME: What if the union is transformed
          // FIXME: Looks broken
          Union.factory(
            anyOf
            ->Js.Array2.filter(s => s->isOptional->not)
            ->(Obj.magic: array<internal> => array<t<unknown>>),
          )->toInternal
        | _ => reversed
        }
      },
    )
    mut->toStandard
  }

  let getOr = (schema, defalutValue) =>
    schema->getWithDefault(Value(defalutValue->castAnyToUnknown))
  let getOrWith = (schema, defalutCb) =>
    schema->getWithDefault(Callback(defalutCb->(Obj.magic: (unit => 'a) => unit => unknown)))
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

  let rec factory = item => {
    let item = item->toInternal
    {
      tag: Array,
      additionalItems: Schema(item->fromInternal),
      items: Stdlib.Array.immutableEmpty,
      builder: Builder.make((b, ~input, ~selfSchema as _, ~path) => {
        let inputVar = b->B.Val.var(input)
        let iteratorVar = b.global->B.varWithoutAllocation

        let bb = b->B.scope
        let itemInput = bb->B.val(`${inputVar}[${iteratorVar}]`)
        let itemOutput =
          bb->B.withPathPrepend(~input=itemInput, ~path, ~dynamicLocationVar=iteratorVar, (
            b,
            ~input,
            ~path,
          ) => b->B.parseWithTypeValidation(~schema=item, ~input, ~path))
        let itemCode = bb->B.allocateScope
        let isTransformed = itemInput !== itemOutput
        let output = isTransformed ? b->B.val(`new Array(${inputVar}.length)`) : input

        if isTransformed || itemCode !== "" {
          b.code =
            b.code ++
            `for(let ${iteratorVar}=0;${iteratorVar}<${inputVar}.length;++${iteratorVar}){${itemCode}${isTransformed
                ? b->B.Val.addKey(output, iteratorVar, itemOutput)
                : ""}}`
        }

        if itemOutput.isAsync {
          output.b->B.asyncVal(`Promise.all(${output.inline})`)
        } else {
          output
        }
      }),
      output: Output.item(~factory, ~item),
    }->toStandard
  }
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
    let schema = schema->toInternal
    switch schema {
    | {additionalItems: currentAdditionalItems, ?items}
      if currentAdditionalItems !== additionalItems &&
        currentAdditionalItems->Js.typeof !== "object" =>
      let mut = schema->copy
      mut.additionalItems = Some(additionalItems)
      if deep {
        let items = items->Stdlib.Option.unsafeUnwrap

        let newItems = []
        let newFields = Js.Dict.empty()
        for idx in 0 to items->Js.Array2.length - 1 {
          let item = items->Js.Array2.unsafe_get(idx)
          let newSchema =
            setAdditionalItems(
              item.schema->(Obj.magic: t<unknown> => t<'a>),
              additionalItems,
              ~deep,
            )->toUnknown
          let newItem = newSchema === item.schema ? item : {...item, schema: newSchema}
          newFields->Js.Dict.set(item.location, newItem)
          newItems->Js.Array2.push(newItem)->ignore
        }
        mut.items = Some(newItems)
        mut.fields = Some(newFields)
      }
      mut->toStandard
    | _ => schema->fromInternal
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

module Dict = {
  let rec factory = item => {
    let item = item->toInternal
    {
      tag: Object,
      fields: Stdlib.Object.immutableEmpty,
      items: Stdlib.Array.immutableEmpty,
      additionalItems: Schema(item->fromInternal),
      builder: Builder.make((b, ~input, ~selfSchema as _, ~path) => {
        let inputVar = b->B.Val.var(input)
        let keyVar = b.global->B.varWithoutAllocation

        let bb = b->B.scope
        let itemInput = bb->B.val(`${inputVar}[${keyVar}]`)
        let itemOutput =
          bb->B.withPathPrepend(~path, ~input=itemInput, ~dynamicLocationVar=keyVar, (
            b,
            ~input,
            ~path,
          ) => b->B.parseWithTypeValidation(~schema=item, ~input, ~path))
        let itemCode = bb->B.allocateScope
        let isTransformed = itemInput !== itemOutput
        let output = isTransformed ? b->B.val("{}") : input

        if isTransformed || itemCode !== "" {
          b.code =
            b.code ++
            `for(let ${keyVar} in ${inputVar}){${itemCode}${isTransformed
                ? b->B.Val.addKey(output, keyVar, itemOutput)
                : ""}}`
        }

        if itemOutput.isAsync {
          let resolveVar = b.global->B.varWithoutAllocation
          let rejectVar = b.global->B.varWithoutAllocation
          let asyncParseResultVar = b.global->B.varWithoutAllocation
          let counterVar = b.global->B.varWithoutAllocation
          let outputVar = b->B.Val.var(output)
          b->B.asyncVal(
            `new Promise((${resolveVar},${rejectVar})=>{let ${counterVar}=Object.keys(${outputVar}).length;for(let ${keyVar} in ${outputVar}){${outputVar}[${keyVar}].then(${asyncParseResultVar}=>{${outputVar}[${keyVar}]=${asyncParseResultVar};if(${counterVar}--===1){${resolveVar}(${outputVar})}},${rejectVar})}})`,
          )
        } else {
          output
        }
      }),
      output: Output.item(~factory, ~item),
    }->toStandard
  }
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
      | Datetime
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

  let cuidRegex = %re(`/^c[^\s-]{8,}$/i`)
  let uuidRegex = %re(`/^[0-9a-fA-F]{8}\b-[0-9a-fA-F]{4}\b-[0-9a-fA-F]{4}\b-[0-9a-fA-F]{4}\b-[0-9a-fA-F]{12}$/i`)
  // Adapted from https://stackoverflow.com/a/46181/1550155
  let emailRegex = %re(`/^(?!\.)(?!.*\.\.)([A-Z0-9_'+\-\.]*)[A-Z0-9_+-]@([A-Z0-9][A-Z0-9\-]*\.)+[A-Z]{2,}$/i`)
  // Adapted from https://stackoverflow.com/a/3143231
  let datetimeRe = %re(`/^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(\.\d+)?Z$/`)

  let schema = {
    tag: String,
    builder: Builder.noop,
  }->toStandard
}

module JsonString = {
  let factory = (item, ~space=0) => {
    let item = item->toInternal
    {
      tag: String,
      builder: Builder.make((b, ~input, ~selfSchema as _, ~path) => {
        let jsonVal = b->B.allocateVal

        b.code =
          b.code ++
          `try{${jsonVal.inline}=JSON.parse(${input.inline})}catch(t){${b->B.failWithArg(
              ~path,
              message => OperationFailed(message),
              "t.message",
            )}}`

        b->B.parseWithTypeValidation(~schema=item, ~input=jsonVal, ~path)
      }),
      output: () => {
        let reversed = item->reverse
        let mut = reversed->copy
        mut.builder = Builder.make((b, ~input, ~selfSchema as _, ~path) => {
          let prevFlag = b.global.flag
          b.global.flag = prevFlag->Flag.with(Flag.jsonableOutput)

          jsonableValidation(
            ~output=reversed,
            ~parent=reversed,
            ~path=Path.empty,
            ~flag=b.global.flag,
            ~recSet=None,
          )

          let output =
            b->B.val(
              `JSON.stringify(${(b->B.parse(~schema=reversed, ~input, ~path)).inline}${space > 0
                  ? `,null,${space->Stdlib.Int.unsafeToString}`
                  : ""})`,
            )
          b.global.flag = prevFlag
          output
        })
        mut
      },
    }->toStandard
  }
}

module Bool = {
  let schema = {
    tag: Boolean,
    builder: Builder.noop,
  }->toStandard
}

module Int = {
  module Refinement = {
    type kind =
      | Min({value: int})
      | Max({value: int})
      | Port
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

  let schema = {
    tag: Number,
    format: Int32,
    builder: Builder.noop,
  }->toStandard
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

  let schema = {
    tag: Number,
    builder: Builder.noop,
  }->toStandard
}

module BigInt = {
  let schema = {
    tag: BigInt,
    builder: Builder.noop,
  }->toStandard
}

let rec to = (from, target) => {
  let from = from->toInternal
  let target = target->toInternal

  // It makes sense, since S.to quite often will be used
  // inside of a framework where we don't control what's the to argument
  if from === target {
    from->fromInternal
  } else {
    switch target {
    | {anyOf} =>
      Union.factory(anyOf->Js.Array2.map(target => to(from->fromInternal, target->fromInternal)))
    | _ => {
        let extendCoercion = %raw(`0`)
        let shrinkCoercion = %raw(`1`)

        let fromOutput = from->reverse
        let isFromLiteral = from->isLiteral
        let isTargetLiteral = target->isLiteral

        let coercion = switch (fromOutput, target) {
        | (_, _) if isFromLiteral && isTargetLiteral =>
          (b, ~inputVar as _, ~failCoercion as _) => {
            b->B.val(b->B.inlineConst(target))
          }
        | ({tag: fromTag}, {tag: targetTag})
          if fromTag === targetTag && isFromLiteral && !isTargetLiteral => extendCoercion
        | (_, {tag: Unknown}) => extendCoercion
        | ({tag: Unknown}, _) => shrinkCoercion
        | ({tag: String}, {tag: String, const: _}) => shrinkCoercion
        | ({tag: String}, {tag: String}) // FIXME: validate that refinements match
        | ({tag: Number, format: Int32}, {tag: Number, format: ?None}) => extendCoercion
        | ({tag: Boolean | Number | BigInt | Undefined | Null | NaN, ?const}, {tag: String})
          if isFromLiteral =>
          (b, ~inputVar as _, ~failCoercion as _) => b->B.val(`"${const->Obj.magic}"`)

        | ({tag: Boolean | Number | BigInt}, {tag: String}) =>
          (b, ~inputVar, ~failCoercion as _) => b->B.val(`""+${inputVar}`)
        | ({tag: String}, {tag: Boolean | Number | BigInt | Undefined | Null | NaN, ?const})
          if isTargetLiteral =>
          (b, ~inputVar, ~failCoercion) => {
            b.code = b.code ++ `${inputVar}==="${const->Obj.magic}"||${failCoercion};`
            b->B.val(b->B.inlineConst(target))
          }
        | ({tag: String}, {tag: Boolean}) =>
          (b, ~inputVar, ~failCoercion) => {
            let output = b->B.allocateVal
            b.code =
              b.code ++
              `(${output.inline}=${inputVar}==="true")||${inputVar}==="false"||${failCoercion};`
            output
          }

        | ({tag: String}, {tag: Number, ?format}) =>
          (b, ~inputVar, ~failCoercion) => {
            let output = b->B.val(`+${inputVar}`)
            let outputVar = output.var(b)
            b.code =
              b.code ++
              switch format {
              | None => `Number.isNaN(${outputVar})`
              | Some(Int32) =>
                `(${b
                  ->B.refinement(~inputVar=outputVar, ~schema=target, ~negative=true)
                  ->Js.String2.sliceToEnd(~from=2)})`
              } ++
              `&&${failCoercion};`
            output
          }
        | ({tag: String}, {tag: BigInt}) =>
          (b, ~inputVar, ~failCoercion) => {
            let output = b->B.allocateVal
            b.code = b.code ++ `try{${output.inline}=BigInt(${inputVar})}catch(_){${failCoercion}}`
            output
          }

        | _ =>
          InternalError.panic(
            `S.to from ${fromOutput->fromInternal->toExpression} to ${target
              ->fromInternal
              ->toExpression} is not supported`,
          )
        }

        let mut = from->copy
        mut.builder = Builder.make((b, ~input, ~selfSchema as _, ~path) => {
          let input = b->B.parse(~schema=from, ~input, ~path)

          if coercion === extendCoercion {
            b->B.parse(~schema=target, ~input, ~path)
          } else if coercion === shrinkCoercion {
            b->B.parseWithTypeValidation(~schema=target, ~input, ~path)
          } else {
            let bb = b->B.scope
            let inputVar = input.var(bb)
            let output = bb->B.parse(
              ~schema=target,
              ~input=bb->coercion(
                ~inputVar,
                ~failCoercion=bb->B.failWithArg(
                  ~path,
                  input => InvalidType({
                    expected: target->fromInternal,
                    received: input,
                  }),
                  inputVar,
                ),
              ),
              ~path,
            )
            b.code = b.code ++ bb->B.allocateScope
            output
          }
        })

        mut.output = Some(
          () => {
            to(target->reverse->fromInternal, fromOutput->fromInternal)->toInternal
          },
        )

        mut->toStandard
      }
    }
  }
}

let list = schema => {
  schema
  ->Array.factory
  ->transform(_ => {
    parser: array => array->Belt.List.fromArray,
    serializer: list => list->Belt.List.toArray,
  })
}

let instance = class_ => {
  {
    tag: Instance,
    class: class_->Obj.magic,
    builder: Builder.noop,
  }->toStandard
}

let rec json = (~validate) =>
  {
    tag: JSON, // FIXME: Store validate on schema
    builder: validate
      ? Builder.make((b, ~input, ~selfSchema, ~path) => {
          let rec parse = (input, ~path=path) => {
            switch input->Stdlib.Type.typeof {
            | #number if Js.Float.isNaN(input->(Obj.magic: unknown => float))->not =>
              input->(Obj.magic: unknown => Js.Json.t)
            | #object =>
              if input === %raw(`null`) {
                input->(Obj.magic: unknown => Js.Json.t)
              } else if input->Stdlib.Array.isArray {
                let input = input->(Obj.magic: unknown => array<unknown>)
                let output = []
                for idx in 0 to input->Js.Array2.length - 1 {
                  let inputItem = input->Js.Array2.unsafe_get(idx)
                  output
                  ->Js.Array2.push(
                    inputItem->parse(
                      ~path=path->Path.concat(Path.fromLocation(idx->Js.Int.toString)),
                    ),
                  )
                  ->ignore
                }
                output->Js.Json.array
              } else {
                let input = input->(Obj.magic: unknown => dict<unknown>)
                let keys = input->Js.Dict.keys
                let output = Js.Dict.empty()
                for idx in 0 to keys->Js.Array2.length - 1 {
                  let key = keys->Js.Array2.unsafe_get(idx)
                  let field = input->Js.Dict.unsafeGet(key)
                  output->Js.Dict.set(
                    key,
                    field->parse(~path=path->Path.concat(Path.fromLocation(key))),
                  )
                }
                output->Js.Json.object_
              }

            | #string
            | #boolean =>
              input->(Obj.magic: unknown => Js.Json.t)

            | _ =>
              b->B.raise(
                ~path,
                ~code=InvalidType({
                  expected: selfSchema->fromInternal,
                  received: input,
                }),
              )
            }
          }

          B.Val.map(b->B.embed(parse), input)
        })
      : Builder.noop,
    output: () => validate ? json(~validate=false)->toInternal : %raw(`this`),
  }->toStandard

module Catch = {
  type s<'value> = {
    @as("e") error: error,
    @as("i") input: unknown,
    @as("s") schema: t<'value>,
    @as("f") fail: 'a. (string, ~path: Path.t=?) => 'a,
  }
}
let catch = (schema, getFallbackValue) => {
  let schema = schema->toInternal
  let mut = schema->copy
  mut.builder = Builder.make((b, ~input, ~selfSchema, ~path) => {
    let inputVar = b->B.Val.var(input)

    b->B.withCatch(
      ~input,
      ~catch=(b, ~errorVar) => Some(
        b->B.val(
          `${b->B.embed((input, internalError) =>
              getFallbackValue({
                Catch.input,
                error: internalError,
                schema: selfSchema->fromInternal,
                fail: (message, ~path as customPath=Path.empty) => {
                  b->B.raise(~path=path->Path.concat(customPath), ~code=OperationFailed(message))
                },
              })
            )}(${inputVar},${errorVar})`,
        ),
      ),
      b => {
        b->B.parseWithTypeValidation(~schema, ~input, ~path)
      },
    )
  })
  mut.noValidation = Some(true)
  mut.catch = Some(true)
  mut->toStandard
}

// TODO: Better test reverse
let meta = (schema: t<'value>, data: meta<'value>) => {
  let schema = schema->toInternal
  let mut = schema->copy
  switch data.name {
  | Some("") => mut.name = None
  | Some(name) => mut.name = Some(name)
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
  | Some([]) => mut.examples = None
  | Some(examples) =>
    mut.examples = Some(examples->Stdlib.Array.map(schema->fromInternal->operationFn(Flag.reverse)))
  | None => ()
  }
  mut->toStandard
}

module Schema = {
  type s = {@as("m") matches: 'value. t<'value> => 'value}

  // Definition item
  @tag("k")
  type rec ditem =
    | @as(0) Item({schema: internal, inlinedLocation: string, location: string}) // Needed only for ditemToItem
    | @as(1)
    ItemField({
        inlinedLocation: string,
        location: string,
        schema?: internal,
        @as("of")
        target: ditem,
        @as("p")
        path: string,
      })
    | @as(2)
    Root({
        schema: internal,
        @as("p")
        path: string,
        @as("i")
        idx: int,
      })
  // Like ditem but for reversed schema
  @tag("k")
  type ritem =
    | @as(0) Registred({@as("p") path: Path.t, @as("i") item: ditem, @as("s") reversed: internal})
    | @as(1) Discriminant({@as("p") path: Path.t, @as("s") reversed: internal})
    | @as(2) Node({@as("p") path: Path.t, @as("s") reversed: internal, @as("a") isArray: bool})

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
    type node<'embeded> = dict<t<'embeded>>

    @inline
    let isNode = (definition: 'any) =>
      definition->Stdlib.Type.typeof === #object && definition !== %raw(`null`)

    let toConstant = (Obj.magic: t<'embeded> => unknown)
    let toNode = (Obj.magic: t<'embeded> => node<'embeded>)

    @inline
    let toEmbededItem = (definition: t<'embeded>): option<ditem> =>
      definition->Obj.magic->Stdlib.Dict.unsafeGetOptionBySymbol(itemSymbol)
  }

  @inline
  let getRitemReversed = (ritem: ritem): internal => (ritem->Obj.magic)["s"]
  @inline
  let getRitemPath = (ritem: ritem): string => (ritem->Obj.magic)["p"]

  external ditemToItem: ditem => item = "%identity"
  external itemToDitem: item => ditem = "%identity"

  let rec getFullDitemPath = (ditem: ditem) => {
    switch ditem {
    | ItemField({target, path}) => Path.concat(target->getFullDitemPath, path)
    | Item({inlinedLocation}) => inlinedLocation->Path.fromInlinedLocation
    | Root({path}) => path
    }
  }

  @inline
  let setItemRitem = (item: ditem, ritem: ritem) => (item->Obj.magic)["r"] = ritem
  @inline
  let getItemRitem = (item: ditem): option<ritem> => (item->Obj.magic)["r"]

  @inline
  let getUnsafeDitemSchema = (item: ditem) => (item->Obj.magic)["schema"]
  @inline
  let getUnsafeDitemIndex = (item: ditem): string => (item->Obj.magic)["i"]

  let rec getItemReversed = item => {
    switch item {
    | ItemField({schema})
    | Root({schema})
    | Item({schema}) =>
      schema->reverse
    | ItemField({target, location, inlinedLocation}) => {
        let targetReversed = target->getItemReversed
        let maybeReversedItem = switch targetReversed {
        | {fields} => fields->Stdlib.Dict.unsafeGetOption(location)
        // If there are no fields, then it must be Tuple
        | {?items} =>
          items->Stdlib.Option.unsafeUnwrap->Stdlib.Array.unsafeGetOptionByString(location)
        }
        if maybeReversedItem === None {
          InternalError.panic(
            `Impossible to reverse the ${inlinedLocation} access of '${targetReversed
              ->fromInternal
              ->toExpression}' schema`,
          )
        }
        (maybeReversedItem->Stdlib.Option.unsafeUnwrap).schema->toInternal
      }
    }
  }

  let rec definitionToOutput = (b, ~definition: Definition.t<ditem>, ~getItemOutput) => {
    if definition->Definition.isNode {
      switch definition->Definition.toEmbededItem {
      | Some(item) => item->getItemOutput
      | None => {
          let node = definition->Definition.toNode
          let isArray = Stdlib.Array.isArray(node)
          let keys = node->Js.Dict.keys

          let objectVal = b->B.Val.Object.make(~isArray)

          for idx in 0 to keys->Js.Array2.length - 1 {
            let key = keys->Js.Array2.unsafe_get(idx)
            objectVal->B.Val.Object.add(
              isArray ? `"${key}"` : key->Stdlib.Inlined.Value.fromString,
              b->definitionToOutput(~definition=node->Js.Dict.unsafeGet(key), ~getItemOutput),
            )
          }
          objectVal->B.Val.Object.complete(~isArray)
        }
      }
    } else {
      b->B.embedVal(definition->Definition.toConstant)
    }
  }

  let objectStrictModeCheck = (b, ~input, ~items, ~selfSchema: internal, ~path) => {
    if (
      selfSchema.tag === Object &&
      selfSchema.additionalItems === Some(Strict) &&
      b.global.flag->Flag.unsafeHas(Flag.typeValidation)
    ) {
      let key = b->B.allocateVal
      let keyVar = key.inline
      b.code = b.code ++ `for(${keyVar} in ${input.inline}){if(`
      switch items {
      | [] => b.code = b.code ++ "true"
      | _ =>
        for idx in 0 to items->Js.Array2.length - 1 {
          let {inlinedLocation} = items->Js.Array2.unsafe_get(idx)
          if idx !== 0 {
            b.code = b.code ++ "&&"
          }
          b.code = b.code ++ `${keyVar}!==${inlinedLocation}`
        }
      }
      b.code =
        b.code ++
        `){${b->B.failWithArg(~path, exccessFieldName => ExcessField(exccessFieldName), keyVar)}}}`
    }
  }

  let rec proxify = (item: ditem): 'a =>
    Stdlib.Object.immutableEmpty->Stdlib.Proxy.make({
      get: (~target as _, ~prop) => {
        if prop === itemSymbol->Obj.magic {
          item->Obj.magic
        } else {
          let location = prop->(Obj.magic: unknown => string)
          let inlinedLocation = location->Stdlib.Inlined.Value.fromString
          ItemField({
            inlinedLocation,
            location,
            target: item,
            path: Path.fromInlinedLocation(inlinedLocation),
          })
          ->proxify
          ->Obj.magic
        }
      },
    })

  let rec builder = (parentB, ~input, ~selfSchema, ~path) => {
    let additionalItems = selfSchema.additionalItems
    let items = selfSchema.items->Stdlib.Option.unsafeUnwrap
    let isArray = selfSchema.tag === Array

    if parentB.global.flag->Flag.unsafeHas(Flag.flatten) {
      let objectVal = parentB->B.Val.Object.make(~isArray)
      for idx in 0 to items->Js.Array2.length - 1 {
        let {inlinedLocation} = items->Js.Array2.unsafe_get(idx)
        objectVal->B.Val.Object.add(
          inlinedLocation,
          input->Obj.magic->Js.Dict.unsafeGet(inlinedLocation),
        )
      }
      objectVal->B.Val.Object.complete(~isArray)
    } else {
      let b = parentB->B.scope // TODO: Remove the scope by grouping all typeFilters together

      let objectVal = b->B.Val.Object.make(~isArray)

      for idx in 0 to items->Js.Array2.length - 1 {
        let {schema, inlinedLocation} = items->Js.Array2.unsafe_get(idx)
        let schema = schema->toInternal
        let itemPath = inlinedLocation->Path.fromInlinedLocation

        let itemInput = b->B.Val.get(input, inlinedLocation)
        let path = path->Path.concat(itemPath)

        if (
          b.global.flag->Flag.unsafeHas(Flag.typeValidation) &&
          !(schema->isLiteral) &&
          schema.tag !== Object
        ) {
          b.code = b.code ++ b->B.typeFilterCode(~schema, ~input=itemInput, ~path)
        }

        objectVal->B.Val.Object.add(inlinedLocation, b->B.parse(~schema, ~input=itemInput, ~path))
      }

      b->objectStrictModeCheck(~input, ~items, ~selfSchema, ~path)

      parentB.code = parentB.code ++ b->B.allocateScope

      if (
        (additionalItems !== Some(Strip) || b.global.flag->Flag.unsafeHas(Flag.reverse)) &&
          selfSchema === selfSchema->reverse
      ) {
        objectVal.var = input.var
        objectVal.inline = input.inline
        objectVal.isAsync = input.isAsync
        (objectVal :> val)
      } else {
        objectVal->B.Val.Object.complete(~isArray)
      }
    }
  }

  and output = () => {
    let items = (%raw(`this`): internal).items->Stdlib.Option.unsafeUnwrap
    let reversedFields = Js.Dict.empty()
    let reversedItems = []

    let isTransformed = ref(false)
    for idx in 0 to items->Js.Array2.length - 1 {
      let {schema, location, inlinedLocation} = items->Js.Array2.unsafe_get(idx)
      let schema = schema->toInternal
      let reversed = schema->reverse
      let item = {
        location,
        inlinedLocation,
        schema: reversed->fromInternal,
      }
      reversedFields->Js.Dict.set(location, item)
      reversedItems->Js.Array2.push(item)->ignore
      if schema !== reversed {
        isTransformed.contents = true
      }
    }
    if isTransformed.contents {
      {
        tag: Object,
        items: reversedItems,
        fields: reversedFields,
        additionalItems: globalConfig.defaultAdditionalItems,
        builder,
      }
    } else {
      %raw(`this`)
    }
  }

  and advancedBuilder = (~definition, ~flattened: option<array<ditem>>=?) => (
    parentB,
    ~input,
    ~selfSchema,
    ~path,
  ) => {
    let isFlatten = parentB.global.flag->Flag.unsafeHas(Flag.flatten)
    let outputs = isFlatten ? input->Obj.magic : Js.Dict.empty()

    let b = parentB->B.scope

    if !isFlatten {
      let items = selfSchema.items->Stdlib.Option.unsafeUnwrap

      let inputVar = b->B.Val.var(input)

      for idx in 0 to items->Js.Array2.length - 1 {
        let {schema, inlinedLocation} = items->Js.Array2.unsafe_get(idx)
        let schema = schema->toInternal

        let itemPath = inlinedLocation->Path.fromInlinedLocation

        let itemInput = b->B.val(`${inputVar}${itemPath}`)
        let path = path->Path.concat(itemPath)

        if (
          b.global.flag->Flag.unsafeHas(Flag.typeValidation) &&
          !(schema->isLiteral) &&
          schema.tag !== Object
        ) {
          b.code = b.code ++ b->B.typeFilterCode(~schema, ~input=itemInput, ~path)
        }

        outputs->Js.Dict.set(inlinedLocation, b->B.parse(~schema, ~input=itemInput, ~path))
      }

      b->objectStrictModeCheck(~input, ~items, ~selfSchema, ~path)
    }

    switch flattened {
    | None => ()
    | Some(rootItems) =>
      let prevFlag = b.global.flag
      b.global.flag = prevFlag->Flag.with(Flag.flatten)
      for idx in 0 to rootItems->Js.Array2.length - 1 {
        let item = rootItems->Js.Array2.unsafe_get(idx)
        outputs
        ->Js.Dict.set(
          item->getUnsafeDitemIndex,
          b->B.parse(~schema=item->getUnsafeDitemSchema, ~input=outputs->Obj.magic, ~path),
        )
        ->ignore
      }
      b.global.flag = prevFlag
    }

    let rec getItemOutput = item => {
      switch item {
      | ItemField({target: item, inlinedLocation}) =>
        b->B.Val.get(item->getItemOutput, inlinedLocation)
      | Item({inlinedLocation}) => outputs->Js.Dict.unsafeGet(inlinedLocation)
      | Root({idx}) => outputs->Js.Dict.unsafeGet(idx->Stdlib.Int.unsafeToString)
      }
    }

    let output =
      b->definitionToOutput(
        ~definition=definition->(Obj.magic: unknown => Definition.t<ditem>),
        ~getItemOutput,
      )

    parentB.code = parentB.code ++ b->B.allocateScope

    output
  }
  and advancedReverse = (~definition, ~to=?, ~flattened=?) => () => {
    let originalSchema = %raw(`this`)

    let definition = definition->(Obj.magic: unknown => Definition.t<ditem>)

    let ritemsByItemPath = Js.Dict.empty()
    let ritems = []
    let ritem = definition->definitionToRitem(~path=Path.empty, ~ritems, ~ritemsByItemPath)

    let mut = switch ritem {
    | Registred(_)
    | Discriminant(_) =>
      // Need to copy the schema here, because we're going to override the builder
      ritem->getRitemReversed->copy
    | Node(_) => ritem->getRitemReversed
    }

    mut.builder = Builder.make((b, ~input, ~selfSchema, ~path) => {
      let getRitemInput = ritem => {
        ritem->getRitemPath === Path.empty
          ? input
          : b->B.val(`${b->B.Val.var(input)}${ritem->getRitemPath}`)
      }

      let rec reversedToInput = (reversed, ~originalPath) => {
        if reversed->isLiteral {
          b->B.embedVal(reversed.const)
        } else {
          switch reversed {
          | {items, tag, ?additionalItems}
            // Ignore S.dict and S.array
            if additionalItems->Obj.magic->Js.typeof === "string" => {
              let isArray = tag === Array
              let objectVal = b->B.Val.Object.make(~isArray)
              for idx in 0 to items->Js.Array2.length - 1 {
                let item = items->Js.Array2.unsafe_get(idx)
                let itemPath =
                  originalPath->Path.concat(Path.fromInlinedLocation(item.inlinedLocation))
                let itemInput = switch ritemsByItemPath->Stdlib.Dict.unsafeGetOption(itemPath) {
                | Some(ritem) => ritem->getRitemInput
                | None => item.schema->toInternal->reversedToInput(~originalPath=itemPath)
                }
                objectVal->B.Val.Object.add(item.inlinedLocation, itemInput)
              }
              objectVal->B.Val.Object.complete(~isArray)
            }
          | _ =>
            b->B.invalidOperation(
              ~path,
              ~description={
                switch originalPath {
                | "" => `Schema isn't registered`
                | _ => `Schema for ${originalPath} isn't registered`
                }
              },
            )
          }
        }
      }

      let getItemOutput = (item, ~itemPath) => {
        switch item->getItemRitem {
        | Some(ritem) => {
            let reversed = ritem->getRitemReversed
            let itemInput = ritem->getRitemInput
            let path = path->Path.concat(ritem->getRitemPath)
            if (
              ritem->getRitemPath !== Path.empty &&
              b.global.flag->Flag.unsafeHas(Flag.typeValidation) &&
              !(reversed->isLiteral) &&
              reversed.tag !== Object
            ) {
              b.code = b.code ++ b->B.typeFilterCode(~schema=reversed, ~input=itemInput, ~path)
            }
            b->B.parse(~schema=reversed, ~input=itemInput, ~path)
          }
        | None =>
          // It's fine to use getUnsafeDitemSchema here, because this will never be called on ItemField
          let reversed = item->getUnsafeDitemSchema->reverse
          let input = reversedToInput(reversed, ~originalPath=itemPath)

          let prevFlag = b.global.flag

          // TODO: Should refactor to use Flag.flatten
          b.global.flag = prevFlag->Flag.without(Flag.typeValidation)
          let output = b->B.parse(~schema=reversed, ~input, ~path)
          b.global.flag = prevFlag
          output
        }
      }

      switch to {
      | Some(ditem) => ditem->getItemOutput(~itemPath=Path.empty)
      | None => {
          b->objectStrictModeCheck(
            ~input,
            ~items=selfSchema.items->Stdlib.Option.unsafeUnwrap,
            ~selfSchema,
            ~path,
          )

          let isArray = (originalSchema: internal).tag === Array
          let items = originalSchema.items->Stdlib.Option.unsafeUnwrap
          let objectVal = b->B.Val.Object.make(~isArray)
          switch flattened {
          | None => ()
          | Some(rootItems) =>
            for idx in 0 to rootItems->Js.Array2.length - 1 {
              objectVal->B.Val.Object.merge(
                rootItems->Js.Array2.unsafe_get(idx)->getItemOutput(~itemPath=Path.empty),
              )
            }
          }

          for idx in 0 to items->Js.Array2.length - 1 {
            let item: item = items->Js.Array2.unsafe_get(idx)

            // TODO: Improve a hack to ignore items belonging to a flattened schema
            if !(objectVal->Obj.magic->Stdlib.Dict.has(item.inlinedLocation)) {
              objectVal->B.Val.Object.add(
                item.inlinedLocation,
                item
                ->itemToDitem
                ->getItemOutput(~itemPath=item.inlinedLocation->Path.fromInlinedLocation),
              )
            }
          }

          objectVal->B.Val.Object.complete(~isArray)
        }
      }
    })

    mut
  }
  and shape = {
    (schema: t<'value>, definer: 'value => 'variant): t<'variant> => {
      let schema = schema->toInternal
      let mut = schema->copy

      let item: ditem = Root({
        schema,
        path: Path.empty,
        idx: 0,
      })
      let definition: unknown = definer(item->proxify)->Obj.magic

      mut.builder = Builder.make((b, ~input, ~selfSchema as _, ~path) => {
        let itemOutput = b->B.parse(~schema, ~input, ~path)

        let bb = b->B.scope
        let rec getItemOutput = item => {
          switch item {
          | ItemField({target: item, inlinedLocation}) =>
            bb->B.Val.get(item->getItemOutput, inlinedLocation)
          | _ => itemOutput
          }
        }
        let output =
          bb->definitionToOutput(
            ~definition=definition->(Obj.magic: unknown => Definition.t<ditem>),
            ~getItemOutput,
          )
        b.code = b.code ++ bb->B.allocateScope

        output
      })
      mut.output = Some(advancedReverse(~definition, ~to=item))

      mut->toStandard
    }
  }
  and nested = fieldName => {
    let parentCtx = %raw(`this`) // TODO: Add a check that it's binded?
    let cacheId = `~${fieldName}`

    switch parentCtx->Stdlib.Dict.unsafeGetOption(cacheId) {
    | Some(ctx) => ctx
    | None => {
        let schemas = []

        let fields = Js.Dict.empty()
        let items = []

        let schema = {
          tag: Object,
          items,
          fields,
          additionalItems: globalConfig.defaultAdditionalItems,
          builder,
          output,
        }->toStandard

        let target =
          parentCtx.field(fieldName, schema)
          ->Definition.toEmbededItem
          ->Stdlib.Option.unsafeUnwrap

        let field:
          type value. (string, schema<value>) => value =
          (fieldName, schema) => {
            let schema = schema->toInternal
            let inlinedLocation = fieldName->Stdlib.Inlined.Value.fromString
            if fields->Stdlib.Dict.has(fieldName) {
              InternalError.panic(`The field ${inlinedLocation} defined twice`)
            }
            let ditem: ditem = ItemField({
              target,
              schema,
              location: fieldName,
              inlinedLocation,
              path: Path.fromInlinedLocation(inlinedLocation),
            })
            let item = ditem->ditemToItem
            fields->Js.Dict.set(fieldName, item)
            items->Js.Array2.push(item)->ignore
            schemas->Js.Array2.push(schema)->ignore
            ditem->proxify
          }

        let tag = (tag, asValue) => {
          let _ = field(tag, definitionToSchema(asValue->Obj.magic)->fromInternal)
        }

        let fieldOr = (fieldName, schema, or) => {
          field(fieldName, Option.factory(schema)->Option.getOr(or))
        }

        let flatten = schema => {
          let schema = schema->toInternal
          switch schema {
          | {tag: Object, items: ?flattenedItems, ?advanced} => {
              if advanced->Stdlib.Option.unsafeUnwrap {
                InternalError.panic(
                  `Unsupported nested flatten for advanced object schema '${schema
                    ->fromInternal
                    ->toExpression}'`,
                )
              }
              switch schema->reverse {
              | {tag: Object, ?advanced} if advanced !== Some(true) =>
                let flattenedItems = flattenedItems->Stdlib.Option.unsafeUnwrap
                let result = Js.Dict.empty()
                for idx in 0 to flattenedItems->Js.Array2.length - 1 {
                  let item = flattenedItems->Js.Array2.unsafe_get(idx)
                  result->Js.Dict.set(item.location, field(item.location, item.schema))
                }
                result->Obj.magic
              | _ =>
                InternalError.panic(
                  `Unsupported nested flatten for transformed schema '${schema
                    ->fromInternal
                    ->toExpression}'`,
                )
              }
            }
          | _ =>
            InternalError.panic(
              `The '${schema->fromInternal->toExpression}' schema can't be flattened`,
            )
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
      let flattened = %raw(`void 0`)
      let items = []
      let fields = Js.Dict.empty()

      let flatten = schema => {
        let schema = schema->toInternal
        switch schema {
        | {tag: Object, items: ?flattenedItems} => {
            let flattenedItems = flattenedItems->Stdlib.Option.unsafeUnwrap
            for idx in 0 to flattenedItems->Js.Array2.length - 1 {
              let {location, inlinedLocation, schema: flattenedSchema} =
                flattenedItems->Js.Array2.unsafe_get(idx)
              switch fields->Stdlib.Dict.unsafeGetOption(location) {
              | Some(item: item) if item.schema === flattenedSchema => ()
              | Some(_) =>
                InternalError.panic(
                  `The field ${inlinedLocation} defined twice with incompatible schemas`,
                )
              | None =>
                let item = Item({
                  schema: flattenedSchema->toInternal,
                  location,
                  inlinedLocation,
                })->ditemToItem
                items->Js.Array2.push(item)->ignore
                fields->Js.Dict.set(location, item)
              }
            }
            let f = %raw(`flattened || (flattened = [])`)
            let item = Root({
              schema,
              path: Path.empty,
              idx: f->Js.Array2.length,
            })
            f->Js.Array2.push(item)->ignore
            item->proxify
          }
        | _ =>
          InternalError.panic(
            `The '${schema->fromInternal->toExpression}' schema can't be flattened`,
          )
        }
      }

      let field:
        type value. (string, schema<value>) => value =
        (fieldName, schema) => {
          let schema = schema->toInternal
          let inlinedLocation = fieldName->Stdlib.Inlined.Value.fromString
          if fields->Stdlib.Dict.has(fieldName) {
            InternalError.panic(
              `The field ${inlinedLocation} defined twice with incompatible schemas`,
            )
          }
          let ditem: ditem = Item({
            schema,
            inlinedLocation,
            location: fieldName,
          })
          let item = ditem->ditemToItem
          fields->Js.Dict.set(fieldName, item)
          items->Js.Array2.push(item)->ignore
          ditem->proxify
        }

      let tag = (tag, asValue) => {
        let _ = field(tag, definitionToSchema(asValue->Obj.magic)->fromInternal)
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

      {
        tag: Object,
        items,
        fields,
        additionalItems: globalConfig.defaultAdditionalItems,
        advanced: true,
        builder: advancedBuilder(~definition, ~flattened),
        output: advancedReverse(~definition, ~flattened),
      }->toStandard
    }
  and tuple = definer => {
    let items = []

    let ctx: Tuple.s = {
      let item:
        type value. (int, schema<value>) => value =
        (idx, schema) => {
          let schema = schema->toInternal
          let location = idx->Js.Int.toString
          let inlinedLocation = `"${location}"`
          if items->Stdlib.Array.has(idx) {
            InternalError.panic(`The item [${inlinedLocation}] is defined multiple times`)
          } else {
            let ditem = Item({
              schema,
              location,
              inlinedLocation,
            })
            items->Js.Array2.unsafe_set(idx, ditem->ditemToItem)
            ditem->proxify
          }
        }

      let tag = (idx, asValue) => {
        let _ = item(idx, definitionToSchema(asValue->Obj.magic)->fromInternal)
      }

      {
        item,
        tag,
      }
    }
    let definition = definer(ctx)->(Obj.magic: 'any => unknown)

    for idx in 0 to items->Js.Array2.length - 1 {
      if items->Js.Array2.unsafe_get(idx)->Obj.magic->not {
        let location = idx->Js.Int.toString
        let inlinedLocation = `"${location}"`
        let ditem = {
          location,
          inlinedLocation,
          schema: unit->toUnknown,
        }

        items->Js.Array2.unsafe_set(idx, ditem)
      }
    }

    {
      tag: Array,
      items,
      additionalItems: Strict,
      builder: advancedBuilder(~definition),
      output: advancedReverse(~definition),
    }->toStandard
  }
  and definitionToRitem = (definition: Definition.t<ditem>, ~path, ~ritems, ~ritemsByItemPath) => {
    if definition->Definition.isNode {
      switch definition->Definition.toEmbededItem {
      | Some(item) =>
        let ritem = Registred({
          path,
          item,
          reversed: item->getItemReversed,
        })
        item->setItemRitem(ritem)
        ritemsByItemPath->Js.Dict.set(item->getFullDitemPath, ritem)
        ritem
      | None => {
          let node = definition->Definition.toNode
          if node->Stdlib.Array.isArray {
            let node = node->(Obj.magic: Definition.node<ditem> => array<Definition.t<ditem>>)
            let items = []
            for idx in 0 to node->Js.Array2.length - 1 {
              let location = idx->Js.Int.toString
              let inlinedLocation = `"${location}"`
              let ritem = definitionToRitem(
                node->Js.Array2.unsafe_get(idx),
                ~path=path->Path.concat(Path.fromInlinedLocation(inlinedLocation)),
                ~ritems,
                ~ritemsByItemPath,
              )
              ritems->Js.Array2.push(ritem)->ignore
              let item = {
                location,
                inlinedLocation,
                schema: ritem->getRitemReversed->fromInternal,
              }
              items->Js.Array2.unsafe_set(idx, item)
            }
            Node({
              path,
              isArray: true,
              reversed: {
                tag: Array,
                items,
                additionalItems: Strict,
                builder: Never.builder,
                output,
              },
            })
          } else {
            let fieldNames = node->Js.Dict.keys
            let node = node->(Obj.magic: Definition.node<ditem> => dict<Definition.t<ditem>>)

            let fields = Js.Dict.empty()
            let items = []
            for idx in 0 to fieldNames->Js.Array2.length - 1 {
              let location = fieldNames->Js.Array2.unsafe_get(idx)
              let inlinedLocation = location->Stdlib.Inlined.Value.fromString
              let ritem = definitionToRitem(
                node->Js.Dict.unsafeGet(location),
                ~path=path->Path.concat(Path.fromInlinedLocation(inlinedLocation)),
                ~ritems,
                ~ritemsByItemPath,
              )
              ritems->Js.Array2.push(ritem)->ignore
              let item = {
                location,
                inlinedLocation,
                schema: ritem->getRitemReversed->fromInternal,
              }
              items->Js.Array2.unsafe_set(idx, item)
              fields->Js.Dict.set(location, item)
            }

            Node({
              path,
              isArray: false,
              reversed: {
                tag: Object,
                items,
                fields,
                additionalItems: globalConfig.defaultAdditionalItems,
                advanced: true,
                builder: Never.builder,
                output,
              },
            })
          }
        }
      }
    } else {
      Discriminant({
        path,
        reversed: Literal.parse(definition->Definition.toConstant),
      })
    }
  }
  and definitionToSchema = (definition: unknown): internal => {
    if definition->Definition.isNode {
      if definition->isSchemaObject {
        definition->(Obj.magic: unknown => internal)
      } else if definition->Stdlib.Array.isArray {
        let node = definition->(Obj.magic: unknown => array<unknown>)
        let reversedItems = []
        let isTransformed = ref(false)
        for idx in 0 to node->Js.Array2.length - 1 {
          let schema = node->Js.Array2.unsafe_get(idx)->definitionToSchema
          let reversed = schema->reverse
          let location = idx->Js.Int.toString
          let inlinedLocation = `"${location}"`
          node->Js.Array2.unsafe_set(
            idx,
            {
              location,
              inlinedLocation,
              schema: schema->fromInternal,
            }->(Obj.magic: item => unknown),
          )
          reversedItems->Js.Array2.unsafe_set(
            idx,
            {
              location,
              inlinedLocation,
              schema: reversed->fromInternal,
            },
          )

          if schema !== reversed {
            isTransformed := true
          }
        }
        let items = node->(Obj.magic: array<unknown> => array<item>)
        {
          tag: Array,
          items,
          additionalItems: Strict,
          builder,
          output: ?(
            isTransformed.contents
              ? Some(
                  () => {
                    tag: Array,
                    items: reversedItems,
                    additionalItems: Strict,
                    builder,
                  },
                )
              : None
          ),
        }
      } else {
        let node = definition->(Obj.magic: unknown => dict<unknown>)
        let fieldNames = node->Js.Dict.keys
        let length = fieldNames->Js.Array2.length
        let items = []
        for idx in 0 to length - 1 {
          let location = fieldNames->Js.Array2.unsafe_get(idx)
          let inlinedLocation = location->Stdlib.Inlined.Value.fromString
          let schema = node->Js.Dict.unsafeGet(location)->definitionToSchema
          let item = {
            schema: schema->fromInternal,
            location,
            inlinedLocation,
          }
          node->Js.Dict.set(location, item->(Obj.magic: item => unknown))
          items->Js.Array2.unsafe_set(idx, item)
        }
        {
          tag: Object,
          items,
          fields: node->(Obj.magic: dict<unknown> => dict<item>),
          additionalItems: globalConfig.defaultAdditionalItems,
          builder,
          output,
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
    ->toStandard
  }
}

module Null = {
  let factory = item => {
    Option.factory(item, ~unit=nullAsUnit)
  }
}

let schema = Schema.factory

let js_schema = definition => definition->Obj.magic->Schema.definitionToSchema->toStandard
let literal = js_schema

let enum = values => Union.factory(values->Js.Array2.map(literal))

let unnest = schema => {
  switch schema {
  | Object({items}) =>
    if items->Js.Array2.length === 0 {
      InternalError.panic("Invalid empty object for S.unnest schema.")
    }
    let schema = schema->toInternal
    {
      tag: Array,
      items: items->Js.Array2.mapi((item, idx) => {
        let location = idx->Js.Int.toString
        {
          schema: Array.factory(item.schema),
          inlinedLocation: `"${location}"`,
          location,
        }
      }),
      additionalItems: Strict,
      builder: Builder.make((b, ~input, ~selfSchema as _, ~path) => {
        let inputVar = b->B.Val.var(input)
        let iteratorVar = b.global->B.varWithoutAllocation

        let bb = b->B.scope
        let itemInput = bb->B.Val.Object.make(~isArray=false)
        let lengthCode = ref("")
        for idx in 0 to items->Js.Array2.length - 1 {
          let item = items->Js.Array2.unsafe_get(idx)
          itemInput->B.Val.Object.add(
            item.inlinedLocation,
            bb->B.val(`${inputVar}[${idx->Stdlib.Int.unsafeToString}][${iteratorVar}]`),
          )
          lengthCode :=
            lengthCode.contents ++ `${inputVar}[${idx->Stdlib.Int.unsafeToString}].length,`
        }

        let output = b->B.val(`new Array(Math.max(${lengthCode.contents}))`)
        let outputVar = b->B.Val.var(output)

        let itemOutput = bb->B.withPathPrepend(
          ~input=itemInput->B.Val.Object.complete(~isArray=false),
          ~path,
          ~dynamicLocationVar=iteratorVar,
          ~appendSafe=(bb, ~output as itemOutput) => {
            bb.code = bb.code ++ bb->B.Val.addKey(output, iteratorVar, itemOutput) ++ ";"
          },
          (b, ~input, ~path) => {
            b->B.parse(~schema, ~input, ~path)
          },
        )
        let itemCode = bb->B.allocateScope

        b.code =
          b.code ++
          `for(let ${iteratorVar}=0;${iteratorVar}<${outputVar}.length;++${iteratorVar}){${itemCode}}`

        if itemOutput.isAsync {
          output.b->B.asyncVal(`Promise.all(${output.inline})`)
        } else {
          output
        }
      }),
      output: () => {
        let schema = schema->reverse
        {
          tag: Array,
          items: Stdlib.Array.immutableEmpty,
          additionalItems: Schema(schema->fromInternal),
          builder: Builder.make((b, ~input, ~selfSchema as _, ~path) => {
            let inputVar = b->B.Val.var(input)
            let iteratorVar = b.global->B.varWithoutAllocation
            let outputVar = b.global->B.varWithoutAllocation

            let bb = b->B.scope
            let itemInput = bb->B.val(`${inputVar}[${iteratorVar}]`)
            let itemOutput = bb->B.withPathPrepend(
              ~input=itemInput,
              ~path,
              ~dynamicLocationVar=iteratorVar,
              ~appendSafe=(bb, ~output) => {
                let initialArraysCode = ref("")
                let settingCode = ref("")
                for idx in 0 to items->Js.Array2.length - 1 {
                  let item = items->Js.Array2.unsafe_get(idx)
                  initialArraysCode :=
                    initialArraysCode.contents ++ `new Array(${inputVar}.length),`
                  settingCode :=
                    settingCode.contents ++
                    `${outputVar}[${idx->Stdlib.Int.unsafeToString}][${iteratorVar}]=${(
                        b->B.Val.get(output, item.inlinedLocation)
                      ).inline};`
                }
                b.allocate(`${outputVar}=[${initialArraysCode.contents}]`)
                bb.code = bb.code ++ settingCode.contents
              },
              (b, ~input, ~path) => b->B.parseWithTypeValidation(~schema, ~input, ~path),
            )
            let itemCode = bb->B.allocateScope

            b.code =
              b.code ++
              `for(let ${iteratorVar}=0;${iteratorVar}<${inputVar}.length;++${iteratorVar}){${itemCode}}`

            if itemOutput.isAsync {
              {
                b,
                var: B._notVar,
                inline: `Promise.all(${outputVar})`,
                isAsync: true,
              }
            } else {
              {
                b,
                var: B._var,
                inline: outputVar,
                isAsync: false,
              }
            }
          }),
        }
      },
      unnest: true,
    }->toStandard
  | _ => InternalError.panic("S.unnest supports only object schemas.")
  }
}

// let inline = {
//   let rec internalInline = (schema, ~variant as maybeVariant=?, ()) => {
//     let mut = schema->toInternal->copy

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
//             variantNamesCounter->Js.Dict.set(variantName, numberOfVariantNames->Stdlib.Int.plus(1))
//             let variantName = switch numberOfVariantNames {
//             | 0 => variantName
//             | _ =>
//               variantName ++ numberOfVariantNames->Stdlib.Int.plus(1)->Stdlib.Int.unsafeToString
//             }
//             let inlinedVariant = `#${variantName->Stdlib.Inlined.Value.fromString}`
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
//           `s.item(${idx->Stdlib.Int.unsafeToString}, ${schema.schema->internalInline()})`
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
//     | Null(schema) => `S.null(${schema->internalInline()})`
//     | Never => `S.never`
//     | Unknown => `S.unknown`
//     | Array(schema) => `S.array(${schema->internalInline()})`
//     | Dict(schema) => `S.dict(${schema->internalInline()})`
//     }

//     let inlinedSchema = switch schema->Option.default {
//     | Some(default) => {
//         metadataMap->Stdlib.Dict.deleteInPlace(Option.defaultMetadataId->Metadata.Id.toKey)
//         switch default {
//         | Value(defaultValue) =>
//           inlinedSchema ++
//           `->S.Option.getOr(%raw(\`${defaultValue->Stdlib.Inlined.Value.stringify}\`))`
//         | Callback(defaultCb) =>
//           inlinedSchema ++
//           `->S.Option.getOrWith(() => %raw(\`${defaultCb()->Stdlib.Inlined.Value.stringify}\`))`
//         }
//       }

//     | None => inlinedSchema
//     }

//     let inlinedSchema = switch schema->deprecation {
//     | Some(message) => {
//         metadataMap->Stdlib.Dict.deleteInPlace(deprecationMetadataId->Metadata.Id.toKey)
//         inlinedSchema ++ `->S.deprecate(${message->Stdlib.Inlined.Value.fromString})`
//       }

//     | None => inlinedSchema
//     }

//     let inlinedSchema = switch schema->description {
//     | Some(message) => {
//         metadataMap->Stdlib.Dict.deleteInPlace(descriptionMetadataId->Metadata.Id.toKey)
//         inlinedSchema ++ `->S.describe(${message->Stdlib.Inlined.Value.stringify})`
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
//         metadataMap->Stdlib.Dict.deleteInPlace(String.Refinement.metadataId->Metadata.Id.toKey)
//         inlinedSchema ++
//         refinements
//         ->Js.Array2.map(refinement => {
//           switch refinement {
//           | {kind: Email, message} =>
//             `->S.email(~message=${message->Stdlib.Inlined.Value.fromString})`
//           | {kind: Url, message} => `->S.url(~message=${message->Stdlib.Inlined.Value.fromString})`
//           | {kind: Uuid, message} =>
//             `->S.uuid(~message=${message->Stdlib.Inlined.Value.fromString})`
//           | {kind: Cuid, message} =>
//             `->S.cuid(~message=${message->Stdlib.Inlined.Value.fromString})`
//           | {kind: Min({length}), message} =>
//             `->S.stringMinLength(${length->Stdlib.Int.unsafeToString}, ~message=${message->Stdlib.Inlined.Value.fromString})`
//           | {kind: Max({length}), message} =>
//             `->S.stringMaxLength(${length->Stdlib.Int.unsafeToString}, ~message=${message->Stdlib.Inlined.Value.fromString})`
//           | {kind: Length({length}), message} =>
//             `->S.stringLength(${length->Stdlib.Int.unsafeToString}, ~message=${message->Stdlib.Inlined.Value.fromString})`
//           | {kind: Pattern({re}), message} =>
//             `->S.pattern(%re(${re
//               ->Stdlib.Re.toString
//               ->Stdlib.Inlined.Value.fromString}), ~message=${message->Stdlib.Inlined.Value.fromString})`
//           | {kind: Datetime, message} =>
//             `->S.datetime(~message=${message->Stdlib.Inlined.Value.fromString})`
//           }
//         })
//         ->Js.Array2.joinWith("")
//       }
//     | Int =>
//       // | Literal(Int(_)) ???
//       switch schema->Int.refinements {
//       | [] => inlinedSchema
//       | refinements =>
//         metadataMap->Stdlib.Dict.deleteInPlace(Int.Refinement.metadataId->Metadata.Id.toKey)
//         inlinedSchema ++
//         refinements
//         ->Js.Array2.map(refinement => {
//           switch refinement {
//           | {kind: Max({value}), message} =>
//             `->S.intMax(${value->Stdlib.Int.unsafeToString}, ~message=${message->Stdlib.Inlined.Value.fromString})`
//           | {kind: Min({value}), message} =>
//             `->S.intMin(${value->Stdlib.Int.unsafeToString}, ~message=${message->Stdlib.Inlined.Value.fromString})`
//           | {kind: Port, message} =>
//             `->S.port(~message=${message->Stdlib.Inlined.Value.fromString})`
//           }
//         })
//         ->Js.Array2.joinWith("")
//       }
//     | Float =>
//       // | Literal(Float(_)) ???
//       switch schema->Float.refinements {
//       | [] => inlinedSchema
//       | refinements =>
//         metadataMap->Stdlib.Dict.deleteInPlace(Float.Refinement.metadataId->Metadata.Id.toKey)
//         inlinedSchema ++
//         refinements
//         ->Js.Array2.map(refinement => {
//           switch refinement {
//           | {kind: Max({value}), message} =>
//             `->S.floatMax(${value->Stdlib.Inlined.Float.toRescript}, ~message=${message->Stdlib.Inlined.Value.fromString})`
//           | {kind: Min({value}), message} =>
//             `->S.floatMin(${value->Stdlib.Inlined.Float.toRescript}, ~message=${message->Stdlib.Inlined.Value.fromString})`
//           }
//         })
//         ->Js.Array2.joinWith("")
//       }

//     | Array(_) =>
//       switch schema->Array.refinements {
//       | [] => inlinedSchema
//       | refinements =>
//         metadataMap->Stdlib.Dict.deleteInPlace(Array.Refinement.metadataId->Metadata.Id.toKey)
//         inlinedSchema ++
//         refinements
//         ->Js.Array2.map(refinement => {
//           switch refinement {
//           | {kind: Max({length}), message} =>
//             `->S.arrayMaxLength(${length->Stdlib.Int.unsafeToString}, ~message=${message->Stdlib.Inlined.Value.fromString})`
//           | {kind: Min({length}), message} =>
//             `->S.arrayMinLength(${length->Stdlib.Int.unsafeToString}, ~message=${message->Stdlib.Inlined.Value.fromString})`
//           | {kind: Length({length}), message} =>
//             `->S.arrayLength(${length->Stdlib.Int.unsafeToString}, ~message=${message->Stdlib.Inlined.Value.fromString})`
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
//     schema->toUnknown->internalInline()
//   }
// }

let object = Schema.object
let never = Never.schema
let string = String.schema
let bool = Bool.schema
let int = Int.schema
let float = Float.schema
let bigint = BigInt.schema
let null = Null.factory
let option = Option.factory->Obj.magic
let array = Array.factory
let dict = Dict.factory
let shape = Schema.shape
let tuple = Schema.tuple
let tuple1 = v0 => tuple(s => s.item(0, v0))
let tuple2 = (v0, v1) =>
  Schema.definitionToSchema([v0->toUnknown, v1->toUnknown]->Obj.magic)->toStandard
let tuple3 = (v0, v1, v2) =>
  Schema.definitionToSchema([v0->toUnknown, v1->toUnknown, v2->toUnknown]->Obj.magic)->toStandard
let union = Union.factory
let jsonString = JsonString.factory

// =============
// Built-in refinements
// =============

let intMin = (schema, minValue, ~message as maybeMessage=?) => {
  let message = switch maybeMessage {
  | Some(m) => m
  | None => `Number must be greater than or equal to ${minValue->Stdlib.Int.unsafeToString}`
  }
  schema->addRefinement(
    ~metadataId=Int.Refinement.metadataId,
    ~refiner=(b, ~inputVar, ~selfSchema as _, ~path) => {
      `if(${inputVar}<${b->B.embed(minValue)}){${b->B.fail(~message, ~path)}}`
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
  | None => `Number must be lower than or equal to ${maxValue->Stdlib.Int.unsafeToString}`
  }
  schema->addRefinement(
    ~metadataId=Int.Refinement.metadataId,
    ~refiner=(b, ~inputVar, ~selfSchema as _, ~path) => {
      `if(${inputVar}>${b->B.embed(maxValue)}){${b->B.fail(~message, ~path)}}`
    },
    ~refinement={
      kind: Max({value: maxValue}),
      message,
    },
  )
}

let port = (schema, ~message="Invalid port") => {
  schema->addRefinement(
    ~metadataId=Int.Refinement.metadataId,
    ~refiner=(b, ~inputVar, ~selfSchema as _, ~path) => {
      `if(${inputVar}<1||${inputVar}>65535){${b->B.fail(~message, ~path)}}`
    },
    ~refinement={
      kind: Port,
      message,
    },
  )
}

let floatMin = (schema, minValue, ~message as maybeMessage=?) => {
  let message = switch maybeMessage {
  | Some(m) => m
  | None => `Number must be greater than or equal to ${minValue->Stdlib.Float.unsafeToString}`
  }
  schema->addRefinement(
    ~metadataId=Float.Refinement.metadataId,
    ~refiner=(b, ~inputVar, ~selfSchema as _, ~path) => {
      `if(${inputVar}<${b->B.embed(minValue)}){${b->B.fail(~message, ~path)}}`
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
  | None => `Number must be lower than or equal to ${maxValue->Stdlib.Float.unsafeToString}`
  }
  schema->addRefinement(
    ~metadataId=Float.Refinement.metadataId,
    ~refiner=(b, ~inputVar, ~selfSchema as _, ~path) => {
      `if(${inputVar}>${b->B.embed(maxValue)}){${b->B.fail(~message, ~path)}}`
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
  | None => `Array must be ${length->Stdlib.Int.unsafeToString} or more items long`
  }
  schema->addRefinement(
    ~metadataId=Array.Refinement.metadataId,
    ~refiner=(b, ~inputVar, ~selfSchema as _, ~path) => {
      `if(${inputVar}.length<${b->B.embed(length)}){${b->B.fail(~message, ~path)}}`
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
  | None => `Array must be ${length->Stdlib.Int.unsafeToString} or fewer items long`
  }
  schema->addRefinement(
    ~metadataId=Array.Refinement.metadataId,
    ~refiner=(b, ~inputVar, ~selfSchema as _, ~path) => {
      `if(${inputVar}.length>${b->B.embed(length)}){${b->B.fail(~message, ~path)}}`
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
  | None => `Array must be exactly ${length->Stdlib.Int.unsafeToString} items long`
  }
  schema->addRefinement(
    ~metadataId=Array.Refinement.metadataId,
    ~refiner=(b, ~inputVar, ~selfSchema as _, ~path) => {
      `if(${inputVar}.length!==${b->B.embed(length)}){${b->B.fail(~message, ~path)}}`
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
  | None => `String must be ${length->Stdlib.Int.unsafeToString} or more characters long`
  }
  schema->addRefinement(
    ~metadataId=String.Refinement.metadataId,
    ~refiner=(b, ~inputVar, ~selfSchema as _, ~path) => {
      `if(${inputVar}.length<${b->B.embed(length)}){${b->B.fail(~message, ~path)}}`
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
  | None => `String must be ${length->Stdlib.Int.unsafeToString} or fewer characters long`
  }
  schema->addRefinement(
    ~metadataId=String.Refinement.metadataId,
    ~refiner=(b, ~inputVar, ~selfSchema as _, ~path) => {
      `if(${inputVar}.length>${b->B.embed(length)}){${b->B.fail(~message, ~path)}}`
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
  | None => `String must be exactly ${length->Stdlib.Int.unsafeToString} characters long`
  }
  schema->addRefinement(
    ~metadataId=String.Refinement.metadataId,
    ~refiner=(b, ~inputVar, ~selfSchema as _, ~path) => {
      `if(${inputVar}.length!==${b->B.embed(length)}){${b->B.fail(~message, ~path)}}`
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
    ~refiner=(b, ~inputVar, ~selfSchema as _, ~path) => {
      `if(!${b->B.embed(String.emailRegex)}.test(${inputVar})){${b->B.fail(~message, ~path)}}`
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
    ~refiner=(b, ~inputVar, ~selfSchema as _, ~path) => {
      `if(!${b->B.embed(String.uuidRegex)}.test(${inputVar})){${b->B.fail(~message, ~path)}}`
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
    ~refiner=(b, ~inputVar, ~selfSchema as _, ~path) => {
      `if(!${b->B.embed(String.cuidRegex)}.test(${inputVar})){${b->B.fail(~message, ~path)}}`
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
    ~refiner=(b, ~inputVar, ~selfSchema as _, ~path) => {
      `try{new URL(${inputVar})}catch(_){${b->B.fail(~message, ~path)}}`
    },
    ~refinement={
      kind: Url,
      message,
    },
  )
}

let pattern = (schema, re, ~message=`Invalid`) => {
  schema->addRefinement(
    ~metadataId=String.Refinement.metadataId,
    ~refiner=(b, ~inputVar, ~selfSchema as _, ~path) => {
      let reVal = b->B.embedVal(re)
      `${reVal.inline}.lastIndex=0;if(!${reVal.inline}.test(${inputVar})){${b->B.fail(
          ~message,
          ~path,
        )}}`
    },
    ~refinement={
      kind: Pattern({re: re}),
      message,
    },
  )
}

let datetime = (schema, ~message=`Invalid datetime string! Expected UTC`) => {
  let refinement = {
    String.Refinement.kind: Datetime,
    message,
  }
  schema
  ->Metadata.set(
    ~id=String.Refinement.metadataId,
    {
      switch schema->Metadata.get(~id=String.Refinement.metadataId) {
      | Some(refinements) => refinements->Stdlib.Array.append(refinement)
      | None => [refinement]
      }
    },
  )
  ->transform(s => {
    parser: string => {
      if String.datetimeRe->Js.Re.test_(string)->not {
        s.fail(message)
      }
      Js.Date.fromString(string)
    },
    serializer: date => date->Js.Date.toISOString,
  })
}

let trim = schema => {
  let transformer = string => string->Js.String2.trim
  schema->transform(_ => {parser: transformer, serializer: transformer})
}

let nullish = schema => {
  Union.factory([schema->toUnknown, unit->toUnknown, Literal.null->fromInternal])
}

// =============
// JS/TS API
// =============

let js_union = values =>
  Union.factory(
    values->Js.Array2.map(Schema.definitionToSchema)->(Obj.magic: array<internal> => array<'a>),
  )

let js_transform = (schema, ~parser as maybeParser=?, ~serializer as maybeSerializer=?) => {
  schema->transform(s => {
    {
      parser: ?switch maybeParser {
      | Some(parser) => Some(v => parser(v, s))
      | None => None
      },
      serializer: ?switch maybeSerializer {
      | Some(serializer) => Some(v => serializer(v, s))
      | None => None
      },
    }
  })
}

let js_refine = (schema, refiner) => {
  schema->refine(s => {
    v => refiner(v, s)
  })
}

let noop = a => a
let js_asyncParserRefine = (schema, refine) => {
  schema->transform(s => {
    {
      asyncParser: v => refine(v, s)->Stdlib.Promise.thenResolve(() => v),
      serializer: noop,
    }
  })
}

let js_optional = (schema, maybeOr) => {
  // TODO: maybeOr should be part of the unit schema
  let schema = Union.factory([schema->toUnknown, unit->toUnknown])
  switch maybeOr {
  | Some(or) if Js.typeof(or) === "function" => schema->Option.getOrWith(or->Obj.magic)->Obj.magic
  | Some(or) => schema->Option.getOr(or->Obj.magic)->Obj.magic
  | None => schema
  }
}

let js_nullable = (schema, maybeOr) => {
  // TODO: maybeOr should be part of the unit schema
  let schema = Union.factory([schema->toUnknown, nullAsUnit->toUnknown])
  switch maybeOr {
  | Some(or) if Js.typeof(or) === "function" => schema->Option.getOrWith(or->Obj.magic)->Obj.magic
  | Some(or) => schema->Option.getOr(or->Obj.magic)->Obj.magic
  | None => schema
  }
}

let js_custom = (~name, ~parser as maybeParser=?, ~serializer as maybeSerializer=?, ()) => {
  custom(name, s => {
    {
      parser: ?switch maybeParser {
      | Some(parser) => Some(v => parser(v, s))
      | None => None
      },
      serializer: ?switch maybeSerializer {
      | Some(serializer) => Some(v => serializer(v, s))
      | None => None
      },
    }
  })
}

let js_merge = (s1, s2) => {
  switch (s1, s2) {
  | (Object({items: items1, fields: fields1}), Object({items: items2, additionalItems})) =>
    let s1 = s1->toInternal
    let s2 = s2->toInternal

    let items = []->Js.Array2.concat(items1)
    let fields = fields1->Stdlib.Dict.copy
    for idx in 0 to items2->Js.Array2.length - 1 {
      let item = items2->Js.Array2.unsafe_get(idx)
      if fields->Stdlib.Dict.has(item.location) {
        InternalError.panic(`The field ${item.inlinedLocation} is defined multiple times`)
      }
      items->Js.Array2.push(item)->ignore
      fields->Js.Dict.set(item.location, item)
    }
    {
      tag: Object,
      items,
      fields,
      additionalItems,
      advanced: true,
      builder: Builder.make((b, ~input, ~selfSchema as _, ~path) => {
        let s1Result = b->B.parse(~schema=s1, ~input, ~path)
        let s2Result = b->B.parse(~schema=s2, ~input, ~path)
        // TODO: Check that these are objects
        b->B.val(`{...${s1Result.inline}, ...${s2Result.inline}}`)
      }),
      output: () => {
        tag: Unknown,
        builder: Builder.make((b, ~input as _, ~selfSchema as _, ~path) => {
          b->B.invalidOperation(~path, ~description=`The S.merge serializing is not supported yet`)
        }),
      },
    }->toStandard

  | _ => InternalError.panic("The merge supports only Object schemas")
  }
}

let global = override => {
  globalConfig.recCounter = 0
  globalConfig.defaultAdditionalItems = (switch override.defaultAdditionalItems {
  | Some(defaultAdditionalItems) => defaultAdditionalItems
  | None => initialOnAdditionalItems
  } :> additionalItems)
  let prevDisableNanNumberCheck = globalConfig.disableNanNumberValidation
  globalConfig.disableNanNumberValidation = switch override.disableNanNumberValidation {
  | Some(disableNanNumberValidation) => disableNanNumberValidation
  | None => initialDisableNanNumberProtection
  }
  if prevDisableNanNumberCheck != globalConfig.disableNanNumberValidation {
    resetOperationsCache(float->toInternal)
  }
}

let reverse = reverse->Obj.magic

module RescriptJSONSchema = {
  include JSONSchema

  let jsonSchemaMetadataId: Metadata.Id.t<t> = Metadata.Id.internal("JSONSchema")

  @val
  external merge: (@as(json`{}`) _, t, t) => t = "Object.assign"

  let rec internalToJSONSchema = (schema: schema<unknown>): JSONSchema.t => {
    let jsonSchema: Mutable.t = {}
    switch schema {
    | String({?const}) =>
      jsonSchema.type_ = Some(Arrayable.single(#string))
      schema
      ->String.refinements
      ->Js.Array2.forEach(refinement => {
        switch refinement {
        | {kind: Email} => jsonSchema.format = Some("email")
        | {kind: Url} => jsonSchema.format = Some("uri")
        | {kind: Uuid} => jsonSchema.format = Some("uuid")
        | {kind: Datetime} => jsonSchema.format = Some("date-time")
        | {kind: Cuid} => ()
        | {kind: Length({length})} => {
            jsonSchema.minLength = Some(length)
            jsonSchema.maxLength = Some(length)
          }
        | {kind: Max({length})} => jsonSchema.maxLength = Some(length)
        | {kind: Min({length})} => jsonSchema.minLength = Some(length)
        | {kind: Pattern({re})} => jsonSchema.pattern = Some(re->Js.String2.make)
        }
      })
      switch const {
      | Some(value) => jsonSchema.const = Some(Js.Json.string(value))
      | None => ()
      }
    | Number({?format, ?const}) =>
      switch format {
      | Some(Int32) =>
        jsonSchema.type_ = Some(Arrayable.single(#integer))
        schema
        ->Int.refinements
        ->Js.Array2.forEach(refinement => {
          switch refinement {
          | {kind: Port} => ()
          | {kind: Max({value})} => jsonSchema.maximum = Some(value->Js.Int.toFloat)
          | {kind: Min({value})} => jsonSchema.minimum = Some(value->Js.Int.toFloat)
          }
        })
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
          Arrayable.single(Definition.schema(internalToJSONSchema(childSchema))),
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
          let items = items->Js.Array2.map(item => {
            Definition.schema(internalToJSONSchema(item.schema))
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
          | Undefined(_) => ()
          | _ => {
              items->Js.Array2.push(Definition.schema(internalToJSONSchema(childSchema)))->ignore
              switch childSchema->toInternal->isLiteral {
              | true =>
                literals
                ->Js.Array2.push(
                  (childSchema->toInternal).const->(Obj.magic: option<char> => Js.Json.t),
                )
                ->ignore
              | false => ()
              }
            }
          }
        })

        let itemsNumber = items->Js.Array2.length

        switch schema->Option.default {
        | Some(default) =>
          let serialize = schema->operationFn(Flag.reverse)
          jsonSchema.default = Some(
            switch default {
            | Value(v) => v
            | Callback(cb) => cb()
            }
            ->serialize
            ->(Obj.magic: unknown => Js.Json.t),
          )
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

    // | S.Option(childSchema) => {
    //     if childSchema->isOptionalSchema {
    //       Error.raise(UnsupportedNestedOptional)
    //     }

    //     let childJsonSchema = fromRescriptSchema(childSchema)
    //     jsonSchema->Mutable.mixin(childJsonSchema)

    // FIXME: Default
    //     switch schema->S.Option.default {
    //     | Some(default) =>
    //       let defaultValue = switch default {
    //       | Value(v) => v
    //       | Callback(cb) => cb()
    //       }
    //       jsonSchema.default = Some(
    //         try Some(defaultValue)
    //         ->(magic: option<unknown> => unknown)
    //         ->S.reverseConvertToJsonOrThrow(childSchema) catch {
    //         | S.Raised(destructingError) =>
    //           Error.raise(
    //             DefaultDestructingFailed({
    //               destructingErrorMessage: destructingError->S.Error.message,
    //             }),
    //           )
    //         },
    //       )
    //     | None => ()
    //     }
    //   }
    | Object({items, additionalItems}) =>
      switch additionalItems {
      | Schema(childSchema) => {
          jsonSchema.type_ = Some(Arrayable.single(#object))
          jsonSchema.additionalProperties = Some(
            Definition.schema(internalToJSONSchema(childSchema)),
          )
        }
      | _ => {
          let properties = Js.Dict.empty()
          let required = []
          items->Js.Array2.forEach(item => {
            let fieldSchema = internalToJSONSchema(item.schema)
            if item.schema->toInternal->isOptional->not {
              required->Js.Array2.push(item.location)->ignore
            }
            properties->Js.Dict.set(item.location, Definition.schema(fieldSchema))
          })
          let additionalProperties = switch additionalItems {
          | Strict => false
          | Strip => true
          | Schema(_) => true
          }

          jsonSchema.type_ = Some(Arrayable.single(#object))
          jsonSchema.properties = Some(properties)
          jsonSchema.additionalProperties = Some(Definition.boolean(additionalProperties))
          switch required {
          | [] => ()
          | required => jsonSchema.required = Some(required)
          }
        }
      }
    | JSON(_)
    | Unknown(_) => ()
    | Null(_) => jsonSchema.type_ = Some(Arrayable.single(#null))
    | Never(_) => jsonSchema.not = Some(Definition.schema({}))
    // This case should never happen,
    // since we have jsonableValidate in the toJSONSchema function
    | _ => InternalError.panic("Unexpected schema type")
    }

    switch schema->untag {
    | {description: m} => jsonSchema.description = Some(m)
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

    switch schema->Metadata.get(~id=jsonSchemaMetadataId) {
    | Some(metadataRawSchema) => jsonSchema->Mutable.mixin(metadataRawSchema)
    | None => ()
    }

    jsonSchema->Mutable.toReadOnly
  }

  // let castAnySchemaToJsonableS = (magic: S.t<'any> => S.t<Js.Json.t>)

  // @inline
  // let primitiveToSchema = primitive => {
  //   S.literal(primitive)->castAnySchemaToJsonableS
  // }

  // let toIntSchema = (jsonSchema: t) => {
  //   let schema = S.int
  //   // TODO: Support jsonSchema.multipleOf when it's in rescript-schema
  //   // if (typeof jsonSchema.multipleOf === "number" && jsonSchema.multipleOf !== 1) {
  //   //  r += `.multipleOf(${jsonSchema.multipleOf})`;
  //   // }
  //   let schema = switch jsonSchema {
  //   | {minimum} => schema->S.intMin(minimum->Belt.Float.toInt)
  //   | {exclusiveMinimum} => schema->S.intMin((exclusiveMinimum +. 1.)->Belt.Float.toInt)
  //   | _ => schema
  //   }
  //   let schema = switch jsonSchema {
  //   | {maximum} => schema->S.intMax(maximum->Belt.Float.toInt)
  //   | {exclusiveMinimum} => schema->S.intMax((exclusiveMinimum -. 1.)->Belt.Float.toInt)
  //   | _ => schema
  //   }
  //   schema->castAnySchemaToJsonableS
  // }

  // let definitionToDefaultValue = definition =>
  //   switch definition->Definition.classify {
  //   | Schema(s) => s.default
  //   | Boolean(_) => None
  //   }

  // let rec toRescriptSchema = (jsonSchema: t) => {
  //   let anySchema = S.json(~validate=false)

  //   let definitionToSchema = definition =>
  //     switch definition->Definition.classify {
  //     | Schema(s) => s->toRescriptSchema
  //     | Boolean(_) => anySchema
  //     }

  //   let schema = switch jsonSchema {
  //   | _ if (jsonSchema->(magic: t => {..}))["nullable"] =>
  //     S.null(
  //       jsonSchema->merge({"nullable": false}->(magic: {..} => t))->toRescriptSchema,
  //     )->castAnySchemaToJsonableS
  //   | {type_} if type_ === Arrayable.single(#object) =>
  //     let schema = switch jsonSchema.properties {
  //     | Some(properties) =>
  //       let schema = S.object(s =>
  //         properties
  //         ->Js.Dict.entries
  //         ->Js.Array2.map(((key, property)) => {
  //           let propertySchema = property->definitionToSchema
  //           let propertySchema = switch jsonSchema.required {
  //           | Some(r) if r->Js.Array2.includes(key) => propertySchema
  //           | _ =>
  //             switch property->definitionToDefaultValue {
  //             | Some(defaultValue) =>
  //               propertySchema->S.option->S.Option.getOr(defaultValue)->castAnySchemaToJsonableS
  //             | None => propertySchema->S.option->castAnySchemaToJsonableS
  //             }
  //           }
  //           (key, s.field(key, propertySchema))
  //         })
  //         ->Js.Dict.fromArray
  //       )
  //       let schema = switch jsonSchema {
  //       | {additionalProperties} if additionalProperties === Definition.boolean(false) =>
  //         schema->S.strict
  //       | _ => schema
  //       }
  //       schema->castAnySchemaToJsonableS
  //     | None =>
  //       switch jsonSchema.additionalProperties {
  //       | Some(additionalProperties) =>
  //         switch additionalProperties->Definition.classify {
  //         | Boolean(true) => S.dict(anySchema)->castAnySchemaToJsonableS
  //         | Boolean(false) => S.object(_ => ())->S.strict->castAnySchemaToJsonableS
  //         | Schema(s) => S.dict(s->toRescriptSchema)->castAnySchemaToJsonableS
  //         }
  //       | None => S.object(_ => ())->castAnySchemaToJsonableS
  //       }
  //     }

  //     // TODO: jsonSchema.anyOf and jsonSchema.oneOf support
  //     schema
  //   | {type_} if type_ === Arrayable.single(#array) => {
  //       let schema = switch jsonSchema.items {
  //       | Some(items) =>
  //         switch items->Arrayable.classify {
  //         | Single(single) => S.array(single->definitionToSchema)
  //         | Array(array) =>
  //           S.tuple(s => array->Js.Array2.mapi((d, idx) => s.item(idx, d->definitionToSchema)))
  //         }
  //       | None => S.array(anySchema)
  //       }
  //       let schema = switch jsonSchema.minItems {
  //       | Some(min) => schema->S.arrayMinLength(min)
  //       | _ => schema
  //       }
  //       let schema = switch jsonSchema.maxItems {
  //       | Some(max) => schema->S.arrayMaxLength(max)
  //       | _ => schema
  //       }
  //       schema->castAnySchemaToJsonableS
  //     }
  //   | {anyOf: []} => anySchema
  //   | {anyOf: [d]} => d->definitionToSchema
  //   | {anyOf: definitions} => S.union(definitions->Js.Array2.map(definitionToSchema))
  //   | {allOf: []} => anySchema
  //   | {allOf: [d]} => d->definitionToSchema
  //   | {allOf: definitions} =>
  //     anySchema->S.refine(s => data => {
  //       definitions->Js.Array2.forEach(d => {
  //         try data->S.assertOrThrow(d->definitionToSchema) catch {
  //         | _ => s.fail("Should pass for all schemas of the allOf property.")
  //         }
  //       })
  //     })
  //   | {oneOf: []} => anySchema
  //   | {oneOf: [d]} => d->definitionToSchema
  //   | {oneOf: definitions} =>
  //     anySchema->S.refine(s => data => {
  //       let hasOneValidRef = ref(false)
  //       definitions->Js.Array2.forEach(d => {
  //         let passed = try {
  //           data->S.assertOrThrow(d->definitionToSchema)
  //           true
  //         } catch {
  //         | _ => false
  //         }
  //         if passed {
  //           if hasOneValidRef.contents {
  //             s.fail("Should pass single schema according to the oneOf property.")
  //           }
  //           hasOneValidRef.contents = true
  //         }
  //       })
  //       if hasOneValidRef.contents->not {
  //         s.fail("Should pass at least one schema according to the oneOf property.")
  //       }
  //     })
  //   | {not} =>
  //     anySchema->S.refine(s => data => {
  //       let passed = try {
  //         data->S.assertOrThrow(not->definitionToSchema)
  //         true
  //       } catch {
  //       | _ => false
  //       }
  //       if passed {
  //         s.fail("Should NOT be valid against schema in the not property.")
  //       }
  //     })
  //   // needs to come before primitives
  //   | {enum: []} => anySchema
  //   | {enum: [p]} => p->primitiveToSchema
  //   | {enum: primitives} =>
  //     S.union(primitives->Js.Array2.map(primitiveToSchema))->castAnySchemaToJsonableS
  //   | {const} => const->primitiveToSchema
  //   | {type_} if type_->Arrayable.isArray =>
  //     let types = type_->(magic: Arrayable.t<'a> => array<'a>)
  //     S.union(
  //       types->Js.Array2.map(type_ => {
  //         jsonSchema->merge({type_: Arrayable.single(type_)})->toRescriptSchema
  //       }),
  //     )
  //   | {type_} if type_ === Arrayable.single(#string) =>
  //     let schema = S.string
  //     let schema = switch jsonSchema {
  //     | {pattern} => schema->S.pattern(Js.Re.fromString(pattern))
  //     | _ => schema
  //     }

  //     let schema = switch jsonSchema {
  //     | {minLength} => schema->S.stringMinLength(minLength)
  //     | _ => schema
  //     }
  //     let schema = switch jsonSchema {
  //     | {maxLength} => schema->S.stringMaxLength(maxLength)
  //     | _ => schema
  //     }
  //     switch jsonSchema {
  //     | {format: "email"} => schema->S.email->castAnySchemaToJsonableS
  //     | {format: "uri"} => schema->S.url->castAnySchemaToJsonableS
  //     | {format: "uuid"} => schema->S.uuid->castAnySchemaToJsonableS
  //     | {format: "date-time"} => schema->S.datetime->castAnySchemaToJsonableS
  //     | _ => schema->castAnySchemaToJsonableS
  //     }

  //   | {type_} if type_ === Arrayable.single(#integer) => jsonSchema->toIntSchema
  //   | {type_, format: "int64"} if type_ === Arrayable.single(#number) => jsonSchema->toIntSchema
  //   | {type_, multipleOf: 1.} if type_ === Arrayable.single(#number) => jsonSchema->toIntSchema
  //   | {type_} if type_ === Arrayable.single(#number) => {
  //       let schema = S.float
  //       let schema = switch jsonSchema {
  //       | {minimum} => schema->S.floatMin(minimum)
  //       | {exclusiveMinimum} => schema->S.floatMin(exclusiveMinimum +. 1.)
  //       | _ => schema
  //       }
  //       let schema = switch jsonSchema {
  //       | {maximum} => schema->S.floatMax(maximum)
  //       | {exclusiveMinimum} => schema->S.floatMax(exclusiveMinimum -. 1.)
  //       | _ => schema
  //       }
  //       schema->castAnySchemaToJsonableS
  //     }
  //   | {type_} if type_ === Arrayable.single(#boolean) => S.bool->castAnySchemaToJsonableS
  //   | {type_} if type_ === Arrayable.single(#null) =>
  //     S.literal(%raw(`null`))->castAnySchemaToJsonableS
  //   | {if_, then, else_} => {
  //       let ifSchema = if_->definitionToSchema
  //       let thenSchema = then->definitionToSchema
  //       let elseSchema = else_->definitionToSchema
  //       anySchema->S.refine(_ => data => {
  //         let passed = try {
  //           data->S.assertOrThrow(ifSchema)
  //           true
  //         } catch {
  //         | _ => false
  //         }
  //         if passed {
  //           data->S.assertOrThrow(thenSchema)
  //         } else {
  //           data->S.assertOrThrow(elseSchema)
  //         }
  //       })
  //     }
  //   | _ => anySchema
  //   }

  //   let schema = switch jsonSchema {
  //   | {description} => schema->S.describe(description)
  //   | _ => schema
  //   }

  //   let schema = switch jsonSchema {
  //   | {description} => schema->S.describe(description)
  //   | _ => schema
  //   }

  //   schema
  // }
}

let toJSONSchema = schema => {
  let target = schema->toInternal
  jsonableValidation(
    ~output=target,
    ~parent=target,
    ~path=Path.empty,
    ~flag=Flag.jsonableOutput,
    ~recSet=None,
  )
  target->fromInternal->RescriptJSONSchema.internalToJSONSchema
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

let min = (schema, minValue, ~message as maybeMessage=?) => {
  switch schema {
  | String(_) => schema->stringMinLength(minValue, ~message=?maybeMessage)
  | Array(_) => schema->arrayMinLength(minValue, ~message=?maybeMessage)
  | Number({format: Int32}) => schema->intMin(minValue, ~message=?maybeMessage)
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
  | Number({format: Int32}) => schema->intMax(maxValue, ~message=?maybeMessage)
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
