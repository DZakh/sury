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
    let copy: dict<'a> => dict<'a> = %raw(`(d) => ({...d})`)

    @val
    external mixin: (dict<'a>, dict<'a>) => dict<'a> = "Object.assign"

    @get_index
    external unsafeGetOption: (dict<'a>, string) => option<'a> = ""

    @set_index
    external setByInt: (dict<'a>, int, 'a) => unit = ""

    @get_index
    external unsafeGetOptionByInt: (dict<'a>, int) => option<'a> = ""

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
  let fromLocation = location => `[${location->X.Inlined.Value.fromString}]`

  let fromArray = array => {
    switch array {
    | [] => ""
    | [location] => fromLocation(location)
    | _ => "[" ++ array->Js.Array2.map(X.Inlined.Value.fromString)->Js.Array2.joinWith("][") ++ "]"
    }
  }

  let concat = (path, concatedPath) => path ++ concatedPath
}

let vendor = "sury"
// Internal symbol to easily identify the error
let s = X.Symbol.make(vendor)
// Internal symbol to identify item proxy
let itemSymbol = X.Symbol.make(vendor ++ ":item")

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
  | @as("ref") Ref

type standard = {
  version: int,
  vendor: string,
  validate: 'any 'value. 'any => {"value": 'value},
}

type numberFormat = | @as("int32") Int32 | @as("port") Port

type format = numberFormat

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
      items: array<item>,
      additionalItems: additionalItems,
      unnest?: bool,
      name?: string,
      title?: string,
      description?: string,
      deprecated?: bool,
      examples?: array<array<unknown>>,
      default?: array<unknown>,
    })
  | @as("object")
  Object({
      items: array<item>,
      properties: dict<t<unknown>>,
      additionalItems: additionalItems,
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
  // Builder for transforming to the "to" schema
  // If missing, should apply coercion logic
  mutable parser?: builder,
  // A field on the "to" schema,
  // to turn it into "parser", when reversing
  mutable serializer?: builder,
  // Builder refine that the value matches the schema
  // Applies for both parsing and serializing
  mutable refiner?: builder,
  // A schema we transform to
  mutable to?: internal,
  mutable const?: char, // use char to avoid Caml_option.some
  mutable class?: char, // use char to avoid Caml_option.some
  mutable name?: string,
  mutable title?: string,
  mutable description?: string,
  mutable deprecated?: bool,
  mutable examples?: array<unknown>,
  mutable default?: unknown,
  mutable format?: format,
  mutable has?: dict<bool>,
  mutable anyOf?: array<internal>,
  mutable additionalItems?: additionalItems,
  mutable items?: array<item>,
  mutable properties?: dict<internal>,
  mutable noValidation?: bool,
  mutable unnest?: bool,
  @as("$ref")
  mutable ref?: string,
  @as("$defs")
  mutable defs?: dict<internal>,
  mutable isAsync?: bool, // Optional value means that it's not lazily computed yet.
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
  unnest?: bool,
  noValidation?: bool,
  items?: array<item>,
  properties?: dict<t<unknown>>,
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
  @as("f")
  mutable flag: flag,
}
and b = {
  @as("c")
  mutable code: string,
  @as("l")
  mutable varsAllocation: string,
  @as("a")
  mutable allocate: string => unit,
  @as("f")
  mutable filterCode: string,
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
  @as("f")
  mutable filterCode: string,
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

external castToUnknown: t<'any> => t<unknown> = "%identity"
external castToAny: t<'value> => t<'any> = "%identity"
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
  | Union => schema.has->X.Option.getUnsafe->X.Dict.has((Undefined: tag :> string))
  | _ => false
  }
}

type globalConfig = {
  @as("d")
  mutable defsAccumulator: option<dict<internal>>,
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
let globalConfig: globalConfig = {
  defsAccumulator: None,
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
d(p, 's', {value: s})
d(p, '_1', {
  get() {
    return this
  },
});
d(p, 'RE_EXN_ID', {
  value: $$Error,
});

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
  external make: (~code: errorCode, ~flag: int, ~path: Path.t) => error = "SuryError"

  let getOrRethrow = (exn: exn) => {
    if %raw("exn&&exn.s===s") {
      exn->(Obj.magic: exn => error)
    } else {
      raise(exn)
    }
  }

  // TODO: Throw S.Error
  @inline
  let panic = message => X.Exn.raiseError(X.Exn.makeError(`[Sury] ${message}`))
}

@new
external base: unit => internal = "Schema"

type s<'value> = {
  schema: t<'value>,
  fail: 'a. (string, ~path: Path.t=?) => 'a,
}

module ValFlag = {
  @inline let none = 0
  @inline let valid = 1
  @inline let async = 2
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
// !+k[0] is hacky way to skip all numbers
// Should actually benchmark whether it's faster
let copyWithoutCache: internal => internal = %raw(`(schema) => {
  let c = new Schema()
  for (let k in schema) {
    if (!+k[0]) {
      c[k] = schema[k]
    }
  }
  return c
}`)
let updateOutput = (schema: internal, fn): t<'value> => {
  let root = schema->copyWithoutCache
  let mut = ref(root)
  while mut.contents.to->Obj.magic {
    let next = mut.contents.to->X.Option.getUnsafe->copyWithoutCache
    mut.contents.to = Some(next)
    mut := next
  }
  // This should be the Output schema
  fn(mut.contents)
  root->fromInternal
}
let resetOperationsCache: internal => unit = %raw(`(schema) => {
  for (let k in schema) {
    if (+k[0]) {
      delete schema[k];
    }
  }
}`)

let rec stringify = unknown => {
  let typeOfValue = unknown->X.Type.typeof
  switch typeOfValue {
  | #undefined => "undefined"
  | #object if unknown === %raw(`null`) => "null"
  | #object if unknown->X.Array.isArray => {
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
  | #object => unknown->Obj.magic->X.Object.internalClass
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
    let items = items->X.Option.getUnsafe
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
    let items = items->X.Option.getUnsafe
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
            let reason = error->reason(~nestedLevel=nestedLevel->X.Int.plus(1))
            let location = switch error.path {
            | "" => ""
            | nonEmptyPath => `At ${nonEmptyPath}: `
            }
            let line = `- ${location}${reason}`
            if reasonsDict->Js.Dict.unsafeGet(line)->X.Int.unsafeToBool->not {
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
      | {tag: String, ?const} => const->Obj.magic->X.Inlined.Value.fromString
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

    let rootScope = (~flag, ~defs) => {
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
        filterCode: "",
        ?defs,
      }
      (global->Obj.magic)["g"] = global
      global->(Obj.magic: bGlobal => b)
    }

    @inline
    let scope = (b: b): b => {
      {
        allocate: initialAllocate,
        global: b.global,
        filterCode: "",
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
      varsAllocation === ""
        ? b.filterCode ++ b.code
        : `${b.filterCode}let ${varsAllocation};${b.code}`
    }

    let varWithoutAllocation = (global: bGlobal) => {
      let newCounter = global.varCounter->X.Int.plus(1)
      global.varCounter = newCounter
      `v${newCounter->X.Int.unsafeToString}`
    }

    let _var = _b => (%raw(`this`)).inline
    let _notVar = b => {
      let val = %raw(`this`)
      let v = b.global->varWithoutAllocation
      switch val.inline {
      | "" => val.b.allocate(v)
      | i if b.allocate !== %raw(`void 0`) => b.allocate(`${v}=${i}`)
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
      {b, var: _var, inline: v, flag: ValFlag.valid}
    }

    @inline
    let val = (b: b, initial: string): val => {
      {b, var: _notVar, inline: initial, flag: ValFlag.valid}
    }

    @inline
    let notValidVal = (b: b, initial: string): val => {
      {b, var: _notVar, inline: initial, flag: ValFlag.none}
    }

    @inline
    let embedVal = (b: b, value): val => {
      {b, var: _var, inline: b->embed(value), flag: ValFlag.valid}
    }

    @inline
    let asyncVal = (b: b, initial: string): val => {
      {b, var: _notVar, inline: initial, flag: ValFlag.async->Flag.with(ValFlag.valid)}
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
            flag: ValFlag.none,
            join: isArray ? arrayJoin : objectJoin,
            asyncCount: 0,
            promiseAllContent: "",
          }
        }

        let add = (objectVal, inlinedLocation, val: val) => {
          // inlinedLocation is either an int or a quoted string, so it's safe to store it directly on val
          objectVal->(Obj.magic: t => dict<val>)->Js.Dict.set(inlinedLocation, val)
          if val.flag->Flag.unsafeHas(ValFlag.async) {
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
          // or from 4 if it's a normal val instead of Val.Object
          for idx in %raw(`subObjectVal.j ? 7 : 4`) to inlinedLocations->Js.Array2.length - 1 {
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
            objectVal.flag = objectVal.flag->Flag.with(ValFlag.async)
            objectVal.inline = `Promise.all([${objectVal.promiseAllContent}]).then(a=>(${objectVal.inline}))`
          }
          objectVal.flag = objectVal.flag->Flag.with(ValFlag.valid)
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
          // FIXME: Remove original ValFlag
          let inputVar = b->var(input)
          switch (
            input.flag->Flag.unsafeHas(ValFlag.async),
            val.flag->Flag.unsafeHas(ValFlag.async),
          ) {
          | (false, true) => {
              input.flag = input.flag->Flag.with(ValFlag.async)
              `${inputVar}=${val.inline}`
            }
          | (false, false)
          | (true, true) =>
            `${inputVar}=${val.inline}`
          | (true, false) => `${inputVar}=Promise.resolve(${val.inline})`
          }
        }
      }

      let get = (b, targetVal: val, inlinedLocation) => {
        switch targetVal
        ->(Obj.magic: val => dict<val>)
        ->X.Dict.unsafeGetOption(inlinedLocation) {
        | Some(val) => val
        | None => {
            let val = {
              b,
              var: _notVar,
              inline: `${b->var(targetVal)}${Path.fromInlinedLocation(inlinedLocation)}`,
              flag: ValFlag.none,
            }
            targetVal->(Obj.magic: val => dict<val>)->Js.Dict.set(inlinedLocation, val)
            val
          }
        }
      }

      let setInlined = (b: b, input: val, inlined) => {
        `${b->var(input)}=${inlined}`
      }

      let map = (inlinedFn, input: val) => {
        {b: input.b, var: _notVar, inline: `${inlinedFn}(${input.inline})`, flag: ValFlag.none}
      }
    }

    @inline
    let isInternalError = (_b: b, var) => {
      `${var}&&${var}.s===s`
    }

    let transform = (b: b, ~input: val, operation) => {
      if input.flag->Flag.unsafeHas(ValFlag.async) {
        let bb = b->scope
        let operationInput: val = {
          b,
          var: _var,
          inline: bb.global->varWithoutAllocation,
          flag: ValFlag.none,
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
      X.Exn.raiseAny(InternalError.make(~code, ~flag=b.global.flag, ~path))
    }

    let embedSyncOperation = (b: b, ~input: val, ~fn: 'input => 'output) => {
      if input.flag->Flag.unsafeHas(ValFlag.async) {
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
      val.flag = val.flag->Flag.with(ValFlag.async)
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
        let isAsync = fnOutput.flag->Flag.unsafeHas(ValFlag.async)
        let output =
          input === fnOutput
            ? input
            : switch appendSafe {
              | Some(_) => fnOutput
              | None => {b, var: _notVar, inline: "", flag: isAsync ? ValFlag.async : ValFlag.none}
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
            b.code = `${errorVar}.path=${path->X.Inlined.Value.fromString}+${switch maybeDynamicLocationVar {
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
          X.Exn.raiseAny(
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
          let additionalItems = additionalItems->X.Option.getUnsafe
          let items = items->X.Option.getUnsafe

          let length = items->Js.Array2.length

          let code = ref(
            if tag === Array {
              switch additionalItems {
              | Strict => `${and_}${inputVar}.length${eq}${length->X.Int.unsafeToString}`
              | Strip => `${and_}${inputVar}.length${gt}${length->X.Int.unsafeToString}`
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
            let itemCode = if item->isLiteral || schema.unnest->X.Option.getUnsafe {
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
      | {tag: Unknown | Union | Ref | Never} => ""
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

    let withCoerceScope = (b, ~input, ~path, ~target, coercion) => {
      let inputVar = input.var(b)
      b->coercion(
        ~inputVar,
        ~failCoercion=b->failWithArg(
          ~path,
          input => InvalidType({
            expected: target->fromInternal,
            received: input,
          }),
          inputVar,
        ),
      )
    }

    let typeValidation = (b: b, ~schema, ~input, ~path) => {
      if b.global.flag->Flag.unsafeHas(Flag.typeValidation) || schema->isLiteral {
        b.code = b.code ++ b->typeFilterCode(~schema, ~input, ~path)
      }
    }
  }

  let noopOperation = i => i->Obj.magic

  @inline
  let intitialInputVar = "i"
}
// TODO: Split validation code and transformation code
module B = Builder.B

// FIXME: Recursive
let nonJsonableTags = X.Set.fromArray([(Unknown: tag), NaN, BigInt, Function, Instance, Symbol])

let unknown = base()
unknown.tag = Unknown
let unknown: t<unknown> = unknown->fromInternal

let setHas = (has, tag: tag) => {
  has->Js.Dict.set(
    tag === Union || tag === Ref ? (Unknown: tag :> string) : (tag: tag :> string),
    true,
  )
}

let rec parse = (prevB: b, ~schema, ~input as inputArg, ~path) => {
  let b = B.scope(prevB)

  if schema.defs->Obj.magic {
    b.global.defs = schema.defs
  }

  if (
    !(inputArg.flag->Flag.unsafeHas(ValFlag.valid)) &&
    (b.global.flag->Flag.unsafeHas(Flag.typeValidation) || schema->isLiteral)
  ) {
    if !(schema.noValidation->X.Option.getUnsafe) {
      b.filterCode = prevB->B.typeFilterCode(~schema, ~input=inputArg, ~path)
    }
    inputArg.flag = inputArg.flag->Flag.with(ValFlag.valid)
  }

  let input = ref(inputArg)

  switch schema.ref {
  | Some(ref) =>
    let defs = b.global.defs->X.Option.getUnsafe
    // Ignore #/$defs/
    let identifier = ref->Js.String2.sliceToEnd(~from=8)
    let def = defs->Js.Dict.unsafeGet(identifier)
    let flag = if schema.noValidation->X.Option.getUnsafe {
      b.global.flag->Flag.without(Flag.typeValidation)
    } else {
      b.global.flag
    }
    let recOperation = switch def->Obj.magic->X.Dict.unsafeGetOptionByInt(flag) {
    | Some(fn) =>
      // A hacky way to prevent infinite recursion
      if fn === %raw(`0`) {
        b->B.embed(def) ++ `[${flag->X.Int.unsafeToString}]`
      } else {
        b->B.embed(fn)
      }
    | None => {
        def
        ->Obj.magic
        ->X.Dict.setByInt(flag, 0)
        let fn = internalCompile(~schema=def, ~flag, ~defs=b.global.defs)
        def
        ->Obj.magic
        ->X.Dict.setByInt(flag, fn)
        b->B.embed(fn)
      }
    }
    input :=
      b->B.withPathPrepend(~input=input.contents, ~path, (_, ~input, ~path as _) => {
        let output = B.Val.map(recOperation, input)
        if def.isAsync === None {
          let defsMut = defs->X.Dict.copy
          defsMut->Js.Dict.set(identifier, unknown->toInternal)
          let _ = def->isAsyncInternal(~defs=Some(defsMut))
        }
        if def.isAsync->X.Option.getUnsafe {
          output.flag = output.flag->Flag.with(ValFlag.async)
        }
        output
      })
    // Force rec function execution
    // for the case when the value is not used
    let _ = input.contents.var(b)
  | None => ()
  }

  switch schema.refiner {
  | Some(refiner) =>
    // Some refiners like union might return a new
    // instance of value. This is used for an assumption
    // that it's transformed.
    input := refiner(b, ~input=input.contents, ~selfSchema=schema, ~path)
  | _ => ()
  }
  switch schema.to {
  | Some(to) =>
    switch schema.parser {
    | Some(parser) => input := parser(b, ~input=input.contents, ~selfSchema=schema, ~path)
    | None => {
        let target = schema.to->X.Option.getUnsafe

        let isFromLiteral = schema->isLiteral
        let isTargetLiteral = target->isLiteral

        switch (schema, target) {
        | (_, _) if isFromLiteral && isTargetLiteral => input := b->B.val(b->B.inlineConst(target))
        | ({tag: fromTag}, {tag: targetTag})
          if fromTag === targetTag && isFromLiteral && !isTargetLiteral => ()
        | (_, {tag: Unknown}) => ()
        | ({tag: Unknown}, _) | ({tag: String}, {tag: String, const: _}) =>
          input.contents.flag = input.contents.flag->Flag.without(ValFlag.valid)
        | ({tag: String}, {tag: String}) // FIXME: validate that refinements match
        | ({tag: Number, format: Int32}, {tag: Number, format: ?None}) => ()
        | ({tag: Boolean | Number | BigInt | Undefined | Null | NaN, ?const}, {tag: String})
          if isFromLiteral =>
          input := b->B.val(`"${const->Obj.magic}"`)

        | ({tag: Boolean | Number | BigInt}, {tag: String}) =>
          input := b->B.val(`""+${input.contents.inline}`)
        | ({tag: String}, {tag: Boolean | Number | BigInt | Undefined | Null | NaN, ?const})
          if isTargetLiteral =>
          input :=
            b->B.withCoerceScope(~input=input.contents, ~path, ~target, (
              b,
              ~inputVar,
              ~failCoercion,
            ) => {
              b.code = b.code ++ `${inputVar}==="${const->Obj.magic}"||${failCoercion};`
              b->B.val(b->B.inlineConst(target))
            })

        | ({tag: String}, {tag: Boolean}) =>
          input :=
            b->B.withCoerceScope(~input=input.contents, ~path, ~target, (
              b,
              ~inputVar,
              ~failCoercion,
            ) => {
              let output = b->B.allocateVal
              b.code =
                b.code ++
                `(${output.inline}=${inputVar}==="true")||${inputVar}==="false"||${failCoercion};`
              output
            })

        | ({tag: String}, {tag: Number, ?format}) =>
          input :=
            b->B.withCoerceScope(~input=input.contents, ~path, ~target, (
              b,
              ~inputVar,
              ~failCoercion,
            ) => {
              let output = b->B.val(`+${inputVar}`)
              let outputVar = output.var(b)
              b.code =
                b.code ++
                switch format {
                | None => `Number.isNaN(${outputVar})`
                | Some(_) =>
                  `(${b
                    ->B.refinement(~inputVar=outputVar, ~schema=target, ~negative=true)
                    ->Js.String2.sliceToEnd(~from=2)})`
                } ++
                `&&${failCoercion};`
              output
            })
        | ({tag: String}, {tag: BigInt}) =>
          input :=
            b->B.withCoerceScope(~input=input.contents, ~path, ~target, (
              b,
              ~inputVar,
              ~failCoercion,
            ) => {
              let output = b->B.allocateVal
              b.code =
                b.code ++ `try{${output.inline}=BigInt(${inputVar})}catch(_){${failCoercion}}`
              output
            })

        | _ =>
          InternalError.panic(
            `Coercion from ${schema->fromInternal->toExpression} to ${target
              ->fromInternal
              ->toExpression} is not supported`,
          )
        }
      }
    }

    input := b->parse(~schema=to, ~input=input.contents, ~path)
  | None => ()
  }

  prevB.code = prevB.code ++ b->B.allocateScope
  input.contents
}
and isAsyncInternal = (schema, ~defs) => {
  try {
    let b = B.rootScope(~flag=Flag.async, ~defs)
    let input = {
      b,
      var: B._var,
      flag: ValFlag.none,
      inline: Builder.intitialInputVar,
    }
    let output = parse(b, ~schema, ~input, ~path=Path.empty)
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
and internalCompile = (~schema, ~flag, ~defs) => {
  let b = B.rootScope(~flag, ~defs)

  if flag->Flag.unsafeHas(Flag.jsonableOutput) {
    let output = schema->reverse
    jsonableValidation(~output, ~parent=output, ~path=Path.empty, ~flag)
  }

  let input = {
    b,
    var: B._var,
    inline: Builder.intitialInputVar,
    flag: flag->Flag.has(Flag.typeValidation) || schema->isLiteral ? ValFlag.none : ValFlag.valid,
  }

  let output = parse(b, ~schema, ~input, ~path=Path.empty)

  let code = b->B.allocateScope

  let isAsync = output.flag->Flag.has(ValFlag.async)
  schema.isAsync = Some(isAsync)

  if (
    code === "" &&
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
    if flag->Flag.unsafeHas(Flag.async) && !isAsync && !(defs->Obj.magic) {
      inlinedOutput := `Promise.resolve(${inlinedOutput.contents})`
    }

    let inlinedFunction = `${Builder.intitialInputVar}=>{${code}return ${inlinedOutput.contents}}`

    // Js.log(inlinedFunction)

    X.Function.make2(
      ~ctxVarName1="e",
      ~ctxVarValue1=b.global.embeded,
      ~ctxVarName2="s",
      ~ctxVarValue2=s,
      ~inlinedFunction,
    )
  }
}
and operationFn = (s, o) => {
  let s = s->toInternal
  if %raw(`o in s`) {
    %raw(`s[o]`)
  } else {
    let f = internalCompile(
      ~schema=o->Flag.unsafeHas(Flag.reverse) ? s->reverse : s,
      ~flag=o,
      ~defs=%raw(`0`),
    )
    let _ = %raw(`s[o] = f`)
    f
  }
}
and getOutputSchema = (schema: internal) => {
  switch schema.to {
  | Some(to) => getOutputSchema(to)
  | None => schema
  }
}
and reverse = (schema: internal) => {
  let reversedHead = ref(None)
  let current = ref(Some(schema))

  while current.contents->Obj.magic {
    let mut = current.contents->X.Option.getUnsafe->copyWithoutCache
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
    switch mut.items {
    | Some(items) =>
      let properties = Js.Dict.empty()
      let newItems = Belt.Array.makeUninitializedUnsafe(items->Js.Array2.length)
      for idx in 0 to items->Js.Array2.length - 1 {
        let item = items->Js.Array2.unsafe_get(idx)
        let reversed = {
          ...item,
          schema: item.schema->toInternal->reverse->fromInternal,
        }

        // Keep ritem if it's present. Super unsafe and might break
        // TODO: Test double reverse
        if (item->Obj.magic)["r"] {
          (reversed->Obj.magic)["r"] = (item->Obj.magic)["r"]
        }
        properties->Js.Dict.set(item.location, reversed.schema->toInternal)
        newItems->Js.Array2.unsafe_set(idx, reversed)
      }
      mut.items = Some(newItems)
      switch mut.properties {
      | Some(_) => mut.properties = Some(properties)
      // Skip tuple
      | None => ()
      }
    | None => ()
    }
    if mut.additionalItems->X.Type.typeof === #object {
      mut.additionalItems = Some(
        Schema(
          mut.additionalItems
          ->(Obj.magic: option<additionalItems> => internal)
          ->reverse
          ->fromInternal,
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

  reversedHead.contents->X.Option.getUnsafe
}
and jsonableValidation = (~output, ~parent, ~path, ~flag) => {
  let tag = output.tag
  if (tag === Undefined && parent.tag !== Object) || nonJsonableTags->X.Set.has(tag) {
    X.Exn.raiseAny(InternalError.make(~code=InvalidJsonSchema(parent->fromInternal), ~flag, ~path))
  }
  switch output {
  | {tag: Union | Array | Object} =>
    if tag === Union {
      output.anyOf
      ->X.Option.getUnsafe
      ->Js.Array2.forEach(s => jsonableValidation(~output=s, ~parent, ~path, ~flag))
    } else {
      switch output {
      | {items, ?additionalItems} => {
          switch additionalItems->X.Option.getUnsafe {
          | Schema(additionalItems) =>
            jsonableValidation(~output=additionalItems->toInternal, ~parent, ~path, ~flag)

          | _ => ()
          }
          items->Js.Array2.forEach(item => {
            jsonableValidation(
              ~output=item.schema->toInternal,
              ~parent=output,
              ~path=path->Path.concat(Path.fromInlinedLocation(item.inlinedLocation)),
              ~flag,
            )
          })
        }
      | _ => ()
      }
    }
  | _ => ()
  }
}

X.Object.defineProperty(
  %raw(`sp`),
  "~standard",
  {
    get: () => {
      let schema = %raw(`this`)
      {
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
      }
    },
  },
)

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

  let schema = schema->castToUnknown

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
        X.Exn.raiseAny(
          InternalError.make(~code=OperationFailed(%raw(`exn.message`)), ~flag, ~path=Path.empty),
        )
      }
    }
  | _ => fn
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
    X.Exn.raiseAny(
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
  open X

  let undefined = base()
  undefined.tag = Undefined
  undefined.const = %raw(`void 0`)

  let null = base()
  null.tag = Null
  null.const = %raw(`null`)

  @inline
  let make = tag => {
    let mut = base()
    mut.tag = tag
    mut
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
      | typeof => make(typeof->(Obj.magic: X.Type.t => tag))
      }
      schema.const = Some(value->Obj.magic)
      schema
    }
  }
}

let isAsync = schema => {
  let schema = schema->toInternal
  switch schema.isAsync {
  | None => schema->isAsyncInternal(~defs=%raw(`0`))
  | Some(v) => v
  }
}

let wrapExnToFailure = exn => {
  if %raw("exn&&exn.s===s") {
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
    let schema = schema->toInternal
    let mut = schema->copyWithoutCache
    mut->setInPlace(~id, metadata)
    mut->fromInternal
  }
}

let defsPath = `#/$defs/`
let recursive = (name, fn) => {
  let ref = `${defsPath}${name}`
  let refSchema = base()
  refSchema.tag = Ref
  refSchema.ref = Some(ref)
  refSchema.name = Some(name)

  // This is for mutual recursion
  let isNestedRec = globalConfig.defsAccumulator->Obj.magic
  if !isNestedRec {
    globalConfig.defsAccumulator = Some(Js.Dict.empty())
  }
  let def = fn(refSchema->fromInternal)->toInternal
  if def.name->Obj.magic {
    refSchema.name = def.name
  } else {
    def.name = Some(name)
  }
  globalConfig.defsAccumulator
  ->X.Option.getUnsafe
  ->Js.Dict.set(name, def)

  if isNestedRec {
    refSchema->fromInternal
  } else {
    let schema = base()
    schema.tag = Ref
    schema.name = def.name
    schema.ref = Some(ref)
    schema.defs = globalConfig.defsAccumulator

    globalConfig.defsAccumulator = None

    schema->fromInternal
  }
}

let noValidation = (schema, value) => {
  let schema = schema->toInternal
  let mut = schema->copyWithoutCache

  // TODO: Test for discriminant literal
  // TODO: Better test reverse
  mut.noValidation = Some(value)
  mut->fromInternal
}

let internalRefine = (schema, refiner) => {
  let schema = schema->toInternal
  updateOutput(schema, mut => {
    let prevRefiner = mut.refiner
    mut.refiner = Some(
      Builder.make((b, ~input, ~selfSchema, ~path) => {
        // FIXME: Should it be applied in more places?
        b->B.transform(
          ~input=switch prevRefiner {
          | Some(prevRefiner) => prevRefiner(b, ~input, ~selfSchema, ~path)
          | None => input
          },
          (b, ~input) => {
            let rCode = refiner(b, ~inputVar=b->B.Val.var(input), ~selfSchema, ~path)
            b.code = b.code ++ rCode
            input
          },
        )
      }),
    )
  })
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
    | Some(refinements) => refinements->X.Array.append(refinement)
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
  updateOutput(schema, mut => {
    mut.parser = Some(
      Builder.make((b, ~input, ~selfSchema, ~path) => {
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
      }),
    )
    mut.to = Some({
      let to = base()
      to.tag = Unknown
      to.serializer = Some(
        (b, ~input, ~selfSchema, ~path) => {
          switch transformer(b->B.effectCtx(~selfSchema, ~path)) {
          | {serializer} => b->B.embedSyncOperation(~input, ~fn=serializer)
          | {parser: ?None, asyncParser: ?None, serializer: ?None} => input
          | {serializer: ?None, asyncParser: ?Some(_)}
          | {serializer: ?None, parser: ?Some(_)} =>
            b->B.invalidOperation(~path, ~description=`The S.transform serializer is missing`)
          }
        },
      )
      to
    })
    let _ = %raw(`delete mut.isAsync`)
  })
}

let unit: t<unit> = Literal.undefined->fromInternal

let nullAsUnit = base()
nullAsUnit.tag = Null
nullAsUnit.const = %raw(`null`)
nullAsUnit.to = Some(unit->toInternal)
let nullAsUnit = nullAsUnit->fromInternal

let neverBuilder = Builder.make((b, ~input, ~selfSchema, ~path) => {
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

let never = base()
never.tag = Never
never.refiner = Some(neverBuilder)
let never: t<never> = never->fromInternal

module Union = {
  @unboxed
  type itemCode = Single(string) | Multiple(array<string>)

  let getItemCode = (b, ~schema, ~input, ~output: val, ~deopt, ~path) => {
    try {
      let bb = b->B.scope
      if deopt {
        let filterCode = bb->B.typeFilterCode(~schema, ~input, ~path)
        bb.code = bb.code ++ filterCode
      }
      let input = {
        ...input,
        flag: input.flag->Flag.with(ValFlag.valid),
      }
      let itemOutput = bb->parse(~schema, ~input, ~path)

      if itemOutput !== input {
        itemOutput.b = bb
        if schema.tag === Unknown {
          let reversed = schema->reverse
          bb.code = bb.code ++ bb->B.typeFilterCode(~schema=reversed, ~input=itemOutput, ~path)
        }

        if itemOutput.flag->Flag.unsafeHas(ValFlag.async) {
          output.flag = output.flag->Flag.with(ValFlag.async)
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

  let isPriority = (tag: string, byKey: dict<array<internal>>) => {
    ((tag === (Array: tag :> string) || tag === (Instance: tag :> string)) &&
      byKey->X.Dict.has((Object: tag :> string))) ||
      (tag === (NaN: tag :> string) && byKey->X.Dict.has((Number: tag :> string)))
  }

  let refiner = Builder.make((b, ~input, ~selfSchema, ~path) => {
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
                  ? Some(args->X.Array.fromArguments->Js.Array2.sliceFrom(1))
                  : None
              ),
            }),
          )
        })}(${input.var(b)}${caught})`
    }

    let schemas = selfSchema.anyOf->X.Option.getUnsafe
    let typeValidation = b.global.flag->Flag.unsafeHas(Flag.typeValidation)

    let output = input
    let initialInline = input.inline

    let deoptIdx = ref(-1)
    let lastIdx = schemas->Js.Array2.length - 1
    let byKey = ref(Js.Dict.empty())
    let keys = ref([])
    for idx in 0 to lastIdx {
      let schema = schemas->Js.Array2.unsafe_get(idx)

      switch schema.tag {
      // The tags without a determined refinement,
      // for which we can't apply optimizations.
      // So we run them and everything before them in a deopt mode.
      | Union
      | Ref
      | Unknown
      | Never =>
        deoptIdx := idx
        byKey := Js.Dict.empty()
        keys := []

      | tag =>
        let key = tag === Instance ? (schema.class->Obj.magic)["name"] : (tag :> string)
        switch byKey.contents->X.Dict.unsafeGetOption(key) {
        | Some(arr) =>
          // There can only be one valid. Dedupe
          if tag !== Undefined && tag !== Null && tag !== NaN {
            arr->Js.Array2.push(schema)->ignore
          }
        | None => {
            if isPriority((tag :> string), byKey.contents) {
              // Not the fastest way, but it's the simplest way
              // to make sure NaN is checked before number
              // And instance and array checked before object
              keys.contents->Js.Array2.unshift(key)->ignore
            } else {
              keys.contents->Js.Array2.push(key)->ignore
            }
            byKey.contents->Js.Dict.set(key, [schema])
          }
        }
      }
    }
    let deoptIdx = deoptIdx.contents
    let byKey = byKey.contents
    let keys = keys.contents

    let start = ref("")
    let end = ref("")
    let caught = ref("")

    // If we got a case which always passes,
    // we can exit early
    let exit = ref(false)

    if deoptIdx !== -1 {
      for idx in 0 to deoptIdx {
        if !exit.contents {
          let schema = schemas->Js.Array2.unsafe_get(idx)
          let itemCode = b->getItemCode(~schema, ~input, ~output, ~deopt=true, ~path)
          if itemCode->X.String.unsafeToBool {
            let errorVar = `e` ++ idx->X.Int.unsafeToString
            start := start.contents ++ `try{${itemCode}}catch(${errorVar}){`
            end := "}" ++ end.contents
            caught := `${caught.contents},${errorVar}`
          } else {
            exit := true
          }
        }
      }
    }

    if !exit.contents {
      let nextElse = ref(false)
      let noop = ref("")

      for idx in 0 to keys->Js.Array2.length - 1 {
        let schemas = byKey->Js.Dict.unsafeGet(keys->Js.Array2.unsafe_get(idx))

        let isMultiple = schemas->Js.Array2.length > 1
        let firstSchema = schemas->Js.Array2.unsafe_get(0)

        // Make cond as a weird callback, to prevent input.var call until it's needed
        let cond = ref(%raw(`0`))

        let body = if isMultiple {
          let inputVar = input.var(b)

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

          let itemIdx = ref(0)
          let lastIdx = schemas->Js.Array2.length - 1
          while itemIdx.contents <= lastIdx {
            let schema = schemas->Js.Array2.unsafe_get(itemIdx.contents)
            let itemCond =
              (schema->isLiteral ? b->B.validation(~inputVar, ~schema, ~negative=false) : "") ++
              b->B.refinement(~inputVar, ~schema, ~negative=false)->Js.String2.sliceToEnd(~from=2)
            let itemCode = b->getItemCode(~schema, ~input, ~output, ~deopt=false, ~path)

            // Accumulate item parser when it has a discriminant
            if itemCond->X.String.unsafeToBool {
              if itemCode->X.String.unsafeToBool {
                switch byDiscriminant.contents->X.Dict.unsafeGetOption(itemCond) {
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
            if itemCond->X.String.unsafeToBool->not || itemIdx.contents === lastIdx {
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
              // If we don't have a condition (discriminant)
              // and additional parsing logic,
              // it means that this item is always passes
              // so we can remove preceding accumulated refinements
              // and exit early even if there are other items
              if itemCode->X.String.unsafeToBool->not {
                itemNoop := ""
                itemIdx := lastIdx
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
                let errorVar = `e` ++ itemIdx.contents->X.Int.unsafeToString
                itemStart :=
                  itemStart.contents ++
                  `${itemNextElse.contents ? "else{" : ""}try{${itemCode}}catch(${errorVar}){`
                itemEnd := (itemNextElse.contents ? "}" : "") ++ "}" ++ itemEnd.contents
                caught := `${caught.contents},${errorVar}`
                itemNextElse := false
              }
            }

            itemIdx := itemIdx.contents->X.Int.plus(1)
          }

          cond :=
            (
              (~inputVar) =>
                b->B.validation(
                  ~inputVar,
                  ~schema={tag: firstSchema.tag, parser: %raw(`0`)},
                  ~negative=false,
                )
            )

          if itemNoop.contents->X.String.unsafeToBool {
            if itemStart.contents->X.String.unsafeToBool {
              if typeValidation {
                let if_ = itemNextElse.contents ? "else if" : "if"
                itemStart :=
                  itemStart.contents ++ if_ ++ `(!(${itemNoop.contents})){${fail(caught.contents)}}`
              }
            } else {
              let condBefore = cond.contents
              cond := ((~inputVar) => condBefore(~inputVar) ++ `&&(${itemNoop.contents})`)
            }
          } else if typeValidation && itemStart.contents->X.String.unsafeToBool {
            let errorCode = fail(caught.contents)
            itemStart :=
              itemStart.contents ++ (itemNextElse.contents ? `else{${errorCode}}` : errorCode)
          }

          itemStart.contents ++ itemEnd.contents
        } else {
          cond :=
            (
              (~inputVar) => {
                b->B.validation(~inputVar, ~schema=firstSchema, ~negative=false) ++
                  b->B.refinement(~inputVar, ~schema=firstSchema, ~negative=false)
              }
            )

          b->getItemCode(~schema=firstSchema, ~input, ~output, ~deopt=false, ~path)
        }

        if body->X.String.unsafeToBool || isPriority((firstSchema.tag :> string), byKey) {
          let if_ = nextElse.contents ? "else if" : "if"
          start := start.contents ++ if_ ++ `(${cond.contents(~inputVar=input.var(b))}){${body}}`
          nextElse := true
        } else if typeValidation {
          let cond = cond.contents(~inputVar=input.var(b))
          noop := (noop.contents->X.String.unsafeToBool ? `${noop.contents}||${cond}` : cond)
        }
      }

      if typeValidation || deoptIdx === lastIdx {
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
    }

    b.code = b.code ++ start.contents ++ end.contents

    if output.flag->Flag.unsafeHas(ValFlag.async) {
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

  let factory = schemas => {
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
      let anyOf = X.Set.make()

      for idx in 0 to schemas->Js.Array2.length - 1 {
        let schema = schemas->Js.Array2.unsafe_get(idx)

        // Check if the union is not transformed
        if schema.tag === Union && schema.to === None {
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
      let mut = base()
      mut.tag = Union
      mut.anyOf = Some(anyOf->X.Set.toArray)
      mut.refiner = Some(refiner)
      mut.has = Some(has)
      mut->fromInternal
    }
  }
}

module Option = {
  type default = Value(unknown) | Callback(unit => unknown)

  let nestedLoc = "BS_PRIVATE_NESTED_SOME_NONE"
  let inlinedNestedLoc = `"${nestedLoc}"`
  let nestedOption = {
    let nestedNone = () => {
      let itemSchema = Literal.parse(0)
      let item: item = {
        schema: itemSchema->fromInternal,
        location: nestedLoc,
        inlinedLocation: inlinedNestedLoc,
      }
      // FIXME: dict{}
      let properties = Js.Dict.empty()
      properties->Js.Dict.set(nestedLoc, itemSchema)
      {
        tag: Object,
        properties,
        items: [item],
        additionalItems: Strip,
        // TODO: Support this as a default coercion
        serializer: Builder.make((b, ~input as _, ~selfSchema, ~path as _) => {
          b->B.val(b->B.inlineConst(selfSchema.to->X.Option.getUnsafe))
        }),
      }
    }

    let parser = Builder.make((b, ~input as _, ~selfSchema, ~path as _) => {
      b->B.val(
        `{${inlinedNestedLoc}:${(
            (
              (selfSchema->getOutputSchema).items
              ->X.Option.getUnsafe
              ->Js.Array2.unsafe_get(0)
            ).schema->toInternal
          ).const->Obj.magic}}`,
      )
    })

    item => {
      item
      ->updateOutput(mut => {
        mut.to = Some(nestedNone())
        mut.parser = Some(parser)
      })
      ->toInternal
    }
  }

  let factory = (item, ~unit=unit) => {
    let item = item->toInternal

    switch item->getOutputSchema {
    | {tag: Undefined} => Union.factory([unit->castToUnknown, item->nestedOption->fromInternal])
    | {tag: Union, ?anyOf, ?has} =>
      item->updateOutput(mut => {
        let schemas = anyOf->X.Option.getUnsafe
        let mutHas = has->X.Option.getUnsafe->X.Dict.copy

        let newAnyOf = []
        for idx in 0 to schemas->Array.length - 1 {
          let schema = schemas->Js.Array2.unsafe_get(idx)
          newAnyOf
          ->Js.Array2.push(
            switch schema->getOutputSchema {
            | {tag: Undefined} => {
                mutHas->Js.Dict.set(((unit->toInternal).tag: tag :> string), true)
                newAnyOf->Js.Array2.push(unit->toInternal)->ignore
                schema->nestedOption
              }
            | {properties} =>
              switch properties->X.Dict.unsafeGetOption(nestedLoc) {
              | Some(nestedSchema) =>
                schema
                ->updateOutput(mut => {
                  let newItem = {
                    location: nestedLoc,
                    inlinedLocation: inlinedNestedLoc,
                    schema: {
                      tag: nestedSchema.tag,
                      parser: ?nestedSchema.parser,
                      const: nestedSchema.const->Obj.magic->X.Int.plus(1)->Obj.magic,
                    }->fromInternal,
                  }

                  // FIXME: dict{}
                  let properties = Js.Dict.empty()
                  properties->Js.Dict.set(nestedLoc, newItem.schema->toInternal)
                  mut.items = Some([newItem])
                  mut.properties = Some(properties)
                })
                ->toInternal
              | None => schema
              }
            | _ => schema
            },
          )
          ->ignore
        }

        if newAnyOf->Js.Array2.length === schemas->Js.Array2.length {
          mutHas->Js.Dict.set(((unit->toInternal).tag: tag :> string), true)
          newAnyOf->Js.Array2.push(unit->toInternal)->ignore
        }

        mut.anyOf = Some(newAnyOf)
        mut.has = Some(mutHas)
      })
    | _ => Union.factory([item->fromInternal, unit->castToUnknown])
    }
  }

  let getWithDefault = (schema: t<option<'value>>, default) => {
    schema
    ->toInternal
    ->updateOutput(mut => {
      switch mut.anyOf {
      | Some(anyOf) => {
          let newAnyOf = []
          let newHas = Js.Dict.empty()
          for idx in 0 to anyOf->Js.Array2.length - 1 {
            let item = anyOf->Js.Array2.unsafe_get(idx)
            let itemOutput = item->getOutputSchema
            if itemOutput.tag !== Undefined {
              newAnyOf->Js.Array2.push(itemOutput)->ignore
              newHas->setHas(itemOutput.tag)
            }
            // FIXME: Should delete schema.default on reverse?
            // FIXME: Should delete schema.unnest on reverse?
            // FIXME: Ensure that default has the same type as the item
            // Or maybe not, but need to make it properly with JSON Schema
          }

          if newAnyOf->Js.Array2.length === anyOf->Js.Array2.length {
            InternalError.panic(
              `${mut->fromInternal->toExpression} doesn't have undefined case for default`,
            )
          }

          mut.parser = Some(
            Builder.make((b, ~input, ~selfSchema as _, ~path as _) => {
              b->B.transform(
                ~input,
                (b, ~input) => {
                  let inputVar = input.var(b)
                  b->B.val(
                    `${inputVar}===void 0?${switch default {
                      | Value(v) => b->B.inlineConst(Literal.parse(v))
                      | Callback(cb) => `${b->B.embed(cb)}()`
                      }}:${inputVar}`,
                  )
                },
              )
            }),
          )
          mut.to = Some({
            tag: Union,
            anyOf: newAnyOf,
            has: newHas,
            serializer: Union.refiner,
          })

          switch default {
          | Value(v) => mut.default = Some(v)
          | Callback(_) => ()
          }
        }
      | None => InternalError.panic(`Can't set default for ${mut->fromInternal->toExpression}`)
      }
    })
  }

  //  let getWithDefault = (schema: t<option<'value>>, default) => {
  //   let schema = schema->toInternal
  //   let mut = schema->copy
  //   mut->Metadata.setInPlace(~id=defaultMetadataId, default)
  //   mut.builder = Some(
  //     Builder.make((b, ~input, ~selfSchema as _, ~path) => {
  //       b->B.transform(~input=b->B.parse(~schema, ~input, ~path), (b, ~input) => {
  //         let inputVar = b->B.Val.var(input)
  // b->B.val(
  //   `${inputVar}===void 0?${switch default {
  //     | Value(v) => b->B.embed(v)
  //     | Callback(cb) => `${b->B.embed(cb)}()`
  //     }}:${inputVar}`,
  // )
  //       })
  //     }),
  //   )
  //   mut.output = Some(
  //     () => {
  //       let reversed = schema->reverse
  //       switch reversed {
  //       | {anyOf} =>
  //         // FIXME: What if the union is transformed
  //         // FIXME: Looks broken
  //         Union.factory(
  //           anyOf
  //           ->Js.Array2.filter(s => s->isOptional->not)
  //           ->(Obj.magic: array<internal> => array<t<unknown>>),
  //         )->toInternal
  //       | _ => reversed
  //       }
  //     },
  //   )
  //   mut->fromInternal
  // }

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

  let arrayRefiner = Builder.make((b, ~input, ~selfSchema, ~path) => {
    let item = selfSchema.additionalItems->(Obj.magic: option<additionalItems> => internal)

    let inputVar = b->B.Val.var(input)
    let iteratorVar = b.global->B.varWithoutAllocation

    let bb = b->B.scope
    let itemInput = bb->B.notValidVal(`${inputVar}[${iteratorVar}]`)
    let itemOutput =
      bb->B.withPathPrepend(~input=itemInput, ~path, ~dynamicLocationVar=iteratorVar, (
        b,
        ~input,
        ~path,
      ) => b->parse(~schema=item, ~input, ~path))
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

    if itemOutput.flag->Flag.unsafeHas(ValFlag.async) {
      output.b->B.asyncVal(`Promise.all(${output.inline})`)
    } else {
      output
    }
  })

  let factory = item => {
    let mut = base()
    mut.tag = Array
    mut.additionalItems = Some(Schema(item->toInternal->fromInternal))
    mut.items = Some(X.Array.immutableEmpty)
    mut.refiner = Some(arrayRefiner)
    mut->fromInternal
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
      let mut = schema->copyWithoutCache
      mut.additionalItems = Some(additionalItems)
      if deep {
        let items = items->X.Option.getUnsafe

        let newItems = []
        let newProperties = Js.Dict.empty()
        for idx in 0 to items->Js.Array2.length - 1 {
          let item = items->Js.Array2.unsafe_get(idx)
          let newSchema =
            setAdditionalItems(
              item.schema->(Obj.magic: t<unknown> => t<'a>),
              additionalItems,
              ~deep,
            )->castToUnknown
          let newItem = newSchema === item.schema ? item : {...item, schema: newSchema}
          newProperties->Js.Dict.set(item.location, newSchema->toInternal)
          newItems->Js.Array2.push(newItem)->ignore
        }
        mut.items = Some(newItems)
        mut.properties = Some(newProperties)
      }
      mut->fromInternal
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
  let dictRefiner = Builder.make((b, ~input, ~selfSchema, ~path) => {
    let item = selfSchema.additionalItems->(Obj.magic: option<additionalItems> => internal)

    let inputVar = b->B.Val.var(input)
    let keyVar = b.global->B.varWithoutAllocation

    let bb = b->B.scope
    let itemInput = bb->B.notValidVal(`${inputVar}[${keyVar}]`)
    let itemOutput =
      bb->B.withPathPrepend(~path, ~input=itemInput, ~dynamicLocationVar=keyVar, (
        b,
        ~input,
        ~path,
      ) => b->parse(~schema=item, ~input, ~path))
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

    if itemOutput.flag->Flag.unsafeHas(ValFlag.async) {
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
  })

  let factory = item => {
    let item = item->toInternal
    let mut = base()
    mut.tag = Object
    mut.properties = Some(X.Object.immutableEmpty)
    mut.items = Some(X.Array.immutableEmpty)
    mut.additionalItems = Some(Schema(item->fromInternal))
    mut.refiner = Some(dictRefiner)
    mut->fromInternal
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

  let schema = base()
  schema.tag = String
  let schema: t<string> = schema->fromInternal
}

module JsonString = {
  let factory = (item, ~space=0) => {
    let item = item->toInternal
    let mut = base()
    mut.tag = String
    mut.parser = Some(
      Builder.make((b, ~input, ~selfSchema as _, ~path) => {
        let jsonVal = b->B.allocateVal
        b.code =
          b.code ++
          `try{${jsonVal.inline}=JSON.parse(${input.inline})}catch(t){${b->B.failWithArg(
              ~path,
              message => OperationFailed(message),
              "t.message",
            )}}`
        b->B.typeValidation(~schema=item, ~input=jsonVal, ~path)
        jsonVal
      }),
    )
    let to = item->copyWithoutCache
    to.serializer = Some(
      Builder.make((b, ~input, ~selfSchema, ~path as _) => {
        // let prevFlag = b.global.flag
        // b.global.flag = prevFlag->Flag.with(Flag.jsonableOutput)

        jsonableValidation(
          ~output=selfSchema,
          ~parent=selfSchema,
          ~path=Path.empty,
          ~flag=b.global.flag,
        )

        b->B.val(
          `JSON.stringify(${input.inline}${space > 0
              ? `,null,${space->X.Int.unsafeToString}`
              : ""})`,
        )
      }),
    )
    mut.to = Some(to)

    mut->fromInternal
  }
}

let bool = base()
bool.tag = Boolean
let bool: t<bool> = bool->fromInternal

let symbol = base()
symbol.tag = Symbol
let symbol: t<Js.Types.symbol> = symbol->fromInternal

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

  let schema = base()
  schema.tag = Number
  schema.format = Some(Int32)
  let schema: schema<int> = schema->fromInternal
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

  let schema = base()
  schema.tag = Number
  let schema: schema<float> = schema->fromInternal
}

module BigInt = {
  let schema = base()
  schema.tag = BigInt
  let schema: schema<bigint> = schema->fromInternal
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
    | _ =>
      updateOutput(from, mut => {
        mut.to = Some(target)
        // A tricky part about parser is that we don't know the input type in ReScript
        // so we need to directly parse to output instead of input
        // switch parser {
        // | Some(p) =>
        //   mut.parser = Some(
        //     Builder.make((b, ~input, ~selfSchema as _, ~path as _) => {
        //       // TODO: Support async, reverse, nested parsing
        //       b->B.embedSyncOperation(~input, ~fn=p)
        //     }),
        //   )
        // | None => ()
        // }
      })

    // mut.parser = Some(
    //   Builder.make((b, ~input, ~selfSchema as _, ~path) => {
    //     let input = b->parse(~schema=from, ~input, ~path)

    // if coercion === extendCoercion {
    //   b->parse(~schema=target, ~input, ~path)
    // } else if coercion === shrinkCoercion {
    //   b->parse(~schema=target, ~input, ~path)
    // } else {
    //   let bb = b->B.scope
    //   let inputVar = input.var(bb)
    //   let output = bb->parse(
    //     ~schema=target,
    //     ~input=bb->coercion(
    //       ~inputVar,
    //       ~failCoercion=bb->B.failWithArg(
    //         ~path,
    //         input => InvalidType({
    //           expected: target->fromInternal,
    //           received: input,
    //         }),
    //         inputVar,
    //       ),
    //     ),
    //     ~path,
    //   )
    //   b.code = b.code ++ bb->B.allocateScope
    //   output
    // }
    //   }),
    // )
    // mut.to = Some(target)

    // mut.output = Some(
    //   () => {
    //     to(target->reverse->fromInternal, fromOutput->fromInternal)->toInternal
    //   },
    // )

    // mut->fromInternal
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
  let mut = base()
  mut.tag = Instance
  mut.class = class_->Obj.magic
  mut->fromInternal
}

// TODO: Better test reverse
let meta = (schema: t<'value>, data: meta<'value>) => {
  let schema = schema->toInternal
  let mut = schema->copyWithoutCache
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
  | Some([]) => mut.examples = None
  | Some(examples) =>
    mut.examples = Some(examples->X.Array.map(schema->fromInternal->operationFn(Flag.reverse)))
  | None => ()
  }
  mut->fromInternal
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
        schema: internal,
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
    | @as(0) Registred({@as("p") path: Path.t, @as("s") schema: internal})
    | @as(1) Discriminant({@as("p") path: Path.t, @as("s") schema: internal})
    | @as(2) Node({@as("p") path: Path.t, @as("s") schema: internal})

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
      definition->X.Type.typeof === #object && definition !== %raw(`null`)

    let toConstant = (Obj.magic: t<'embeded> => unknown)
    let toNode = (Obj.magic: t<'embeded> => node<'embeded>)

    @inline
    let toEmbededItem = (definition: t<'embeded>): option<ditem> =>
      definition->Obj.magic->X.Dict.unsafeGetOptionBySymbol(itemSymbol)
  }

  @inline
  let getRitemSchema = (ritem: ritem): internal => (ritem->Obj.magic)["s"]
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

  // Can probably only keep the target path
  @inline
  let setItemRitem = (item: ditem, ritem: ritem) => (item->Obj.magic)["r"] = ritem
  @inline
  let getItemRitem = (item: ditem): option<ritem> => (item->Obj.magic)["r"]

  @inline
  let getDitemSchema = (item: ditem): internal => (item->Obj.magic)["schema"]
  @inline
  let getUnsafeDitemIndex = (item: ditem): string => (item->Obj.magic)["i"]

  let rec definitionToOutput = (b, ~definition: Definition.t<ditem>, ~getItemOutput) => {
    if definition->Definition.isNode {
      switch definition->Definition.toEmbededItem {
      | Some(item) => item->getItemOutput
      | None => {
          let node = definition->Definition.toNode
          let isArray = X.Array.isArray(node)
          let keys = node->Js.Dict.keys

          let objectVal = b->B.Val.Object.make(~isArray)

          for idx in 0 to keys->Js.Array2.length - 1 {
            let key = keys->Js.Array2.unsafe_get(idx)
            objectVal->B.Val.Object.add(
              isArray ? `"${key}"` : key->X.Inlined.Value.fromString,
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
      b.code = b.code ++ `for(${keyVar} in ${b->B.Val.var(input)}){if(`
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
    X.Object.immutableEmpty->X.Proxy.make({
      get: (~target as _, ~prop) => {
        if prop === itemSymbol->Obj.magic {
          item->Obj.magic
        } else {
          let location = prop->(Obj.magic: unknown => string)
          let inlinedLocation = location->X.Inlined.Value.fromString
          ItemField({
            schema: {
              let targetReversed = item->getDitemSchema->getOutputSchema
              let maybeField = switch targetReversed {
              | {properties} => properties->X.Dict.unsafeGetOption(location)
              // If there are no properties, then it must be Tuple
              | {items} =>
                switch items->X.Array.unsafeGetOptionByString(location) {
                | Some(i) => Some(i.schema->toInternal)
                | None => None
                }
              | _ => None
              }
              if maybeField === None {
                InternalError.panic(
                  `Cannot read property ${inlinedLocation} of ${targetReversed
                    ->fromInternal
                    ->toExpression}`,
                )
              }
              maybeField->X.Option.getUnsafe
            },
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

  let rec schemaRefiner = (b, ~input, ~selfSchema, ~path) => {
    let additionalItems = selfSchema.additionalItems
    let items = selfSchema.items->X.Option.getUnsafe
    let isArray = selfSchema.tag === Array

    if b.global.flag->Flag.unsafeHas(Flag.flatten) {
      let objectVal = b->B.Val.Object.make(~isArray)
      for idx in 0 to items->Js.Array2.length - 1 {
        let {inlinedLocation} = items->Js.Array2.unsafe_get(idx)
        objectVal->B.Val.Object.add(
          inlinedLocation,
          input->Obj.magic->Js.Dict.unsafeGet(inlinedLocation),
        )
      }
      objectVal->B.Val.Object.complete(~isArray)
    } else {
      let objectVal = b->B.Val.Object.make(~isArray)

      for idx in 0 to items->Js.Array2.length - 1 {
        let {schema, inlinedLocation} = items->Js.Array2.unsafe_get(idx)
        let schema = schema->toInternal

        let itemInput = b->B.Val.get(input, inlinedLocation)
        let path = path->Path.concat(inlinedLocation->Path.fromInlinedLocation)

        if (
          input.flag->Flag.unsafeHas(ValFlag.valid) && (schema->isLiteral || schema.tag === Object)
        ) {
          itemInput.flag = itemInput.flag->Flag.with(Flag.typeValidation)
        }

        objectVal->B.Val.Object.add(inlinedLocation, b->parse(~schema, ~input=itemInput, ~path))
      }

      b->objectStrictModeCheck(~input, ~items, ~selfSchema, ~path)

      if (
        (additionalItems !== Some(Strip) || b.global.flag->Flag.unsafeHas(Flag.reverse)) &&
          // A hacky way to detect that the schema is not transformed
          // If we don't Strip or perform a reverse operation, return the original
          // instance of Val, so other code also think that the schema value is not transformed
          items->Js.Array2.every(item => {
            objectVal
            ->(Obj.magic: B.Val.Object.t => dict<val>)
            ->Js.Dict.unsafeGet(item.inlinedLocation) ===
              input
              ->(Obj.magic: val => dict<val>)
              ->Js.Dict.unsafeGet(item.inlinedLocation)
          })
      ) {
        input
      } else {
        objectVal->B.Val.Object.complete(~isArray)
      }
    }
  }

  and advancedBuilder = (~definition, ~flattened: option<array<ditem>>=?) => (
    b,
    ~input,
    ~selfSchema,
    ~path,
  ) => {
    let isFlatten = b.global.flag->Flag.unsafeHas(Flag.flatten)
    let outputs = isFlatten ? input->Obj.magic : Js.Dict.empty()

    if !isFlatten {
      let items = selfSchema.items->X.Option.getUnsafe

      for idx in 0 to items->Js.Array2.length - 1 {
        let {schema, inlinedLocation} = items->Js.Array2.unsafe_get(idx)
        let schema = schema->toInternal

        let itemInput = b->B.Val.get(input, inlinedLocation)
        let path = path->Path.concat(inlinedLocation->Path.fromInlinedLocation)

        if (
          input.flag->Flag.unsafeHas(ValFlag.valid) && (schema->isLiteral || schema.tag === Object)
        ) {
          itemInput.flag = itemInput.flag->Flag.with(ValFlag.valid)
        }

        outputs->Js.Dict.set(inlinedLocation, b->parse(~schema, ~input=itemInput, ~path))
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
          b->parse(~schema=item->getDitemSchema, ~input, ~path),
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
      | Root({idx}) => outputs->Js.Dict.unsafeGet(idx->X.Int.unsafeToString)
      }
    }

    let output =
      b->definitionToOutput(
        ~definition=definition->(Obj.magic: unknown => Definition.t<ditem>),
        ~getItemOutput,
      )

    output
  }
  and definitionToTarget = (~definition, ~to=?, ~flattened=?) => {
    let definition = definition->(Obj.magic: unknown => Definition.t<ditem>)

    let ritemsByItemPath = Js.Dict.empty()

    let ritem = definition->definitionToRitem(~path=Path.empty, ~ritemsByItemPath)
    let mut = ritem->getRitemSchema

    // This should be done in the parser/serializer
    let _ = %raw(`delete mut.refiner`)

    mut.serializer = Some(
      Builder.make((b, ~input, ~selfSchema, ~path) => {
        let getRitemInput = ritem => {
          ritem->getRitemPath === Path.empty
            ? input
            : {
                {
                  b,
                  var: B._notVar,
                  inline: `${b->B.Val.var(input)}${ritem->getRitemPath}`,
                  // For S.object and S.tuple the value should be already validated,
                  // but not for S.shape
                  flag: if to->Obj.magic {
                    ValFlag.none
                  } else {
                    ValFlag.valid
                  },
                }
              }
        }

        let rec schemaToOutput = (schema, ~originalPath) => {
          let outputSchema = schema->getOutputSchema
          if outputSchema->isLiteral {
            b->B.val(b->B.inlineConst(outputSchema))
          } else if schema->isLiteral {
            b->parse(~schema, ~input=b->B.val(b->B.inlineConst(schema)), ~path)
          } else {
            switch outputSchema {
            | {items, tag, ?additionalItems}
              // Ignore S.dict and S.array
              if additionalItems->Obj.magic->Js.typeof === "string" => {
                let isArray = tag === Array
                let objectVal = b->B.Val.Object.make(~isArray)
                for idx in 0 to items->Js.Array2.length - 1 {
                  let item = items->Js.Array2.unsafe_get(idx)
                  let itemPath =
                    originalPath->Path.concat(Path.fromInlinedLocation(item.inlinedLocation))
                  let itemInput = switch ritemsByItemPath->X.Dict.unsafeGetOption(itemPath) {
                  | Some(ritem) =>
                    b->parse(
                      ~schema=item.schema->toInternal,
                      ~input=ritem->getRitemInput,
                      ~path=ritem->getRitemPath,
                    )
                  | None => item.schema->toInternal->schemaToOutput(~originalPath=itemPath)
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

        let getItemOutput = (item, ~itemPath, ~shouldReverse) => {
          switch item->getItemRitem {
          | Some(ritem) => {
              // If item is transformed to root, then we
              // don't want to apply the whole parse chain,
              // but only to the output schema.
              // Because it'll be parsed later anyways.
              let targetSchema = if shouldReverse {
                item->getDitemSchema->reverse
              } else if itemPath === Path.empty {
                item->getDitemSchema->getOutputSchema
              } else {
                item->getDitemSchema
              }

              let itemInput = ritem->getRitemInput

              let path = path->Path.concat(ritem->getRitemPath)
              if (
                ritem->getRitemPath !== Path.empty &&
                b.global.flag->Flag.unsafeHas(Flag.typeValidation) &&
                !(targetSchema->isLiteral) &&
                targetSchema.tag !== Object
              ) {
                b.code =
                  b.code ++ b->B.typeFilterCode(~schema=targetSchema, ~input=itemInput, ~path)
              }
              b->parse(~schema=targetSchema, ~input=itemInput, ~path)
            }
          | None => schemaToOutput(item->getDitemSchema, ~originalPath=itemPath)
          }
        }

        switch to {
        | Some(ditem) => ditem->getItemOutput(~itemPath=Path.empty, ~shouldReverse=false)
        | None => {
            let originalSchema = selfSchema.to->X.Option.getUnsafe

            b->objectStrictModeCheck(
              ~input,
              ~items=selfSchema.items->X.Option.getUnsafe,
              ~selfSchema,
              ~path,
            )

            let isArray = (originalSchema: internal).tag === Array
            let items = originalSchema.items->X.Option.getUnsafe
            let objectVal = b->B.Val.Object.make(~isArray)
            switch flattened {
            | None => ()
            | Some(rootItems) =>
              for idx in 0 to rootItems->Js.Array2.length - 1 {
                objectVal->B.Val.Object.merge(
                  rootItems
                  ->Js.Array2.unsafe_get(idx)
                  ->getItemOutput(~itemPath=Path.empty, ~shouldReverse=true),
                )
              }
            }

            for idx in 0 to items->Js.Array2.length - 1 {
              let item: item = items->Js.Array2.unsafe_get(idx)

              // TODO: Improve the hack to ignore items belonging to a flattened schema
              if !(objectVal->Obj.magic->X.Dict.has(item.inlinedLocation)) {
                objectVal->B.Val.Object.add(
                  item.inlinedLocation,
                  item
                  ->itemToDitem
                  ->getItemOutput(
                    ~itemPath=item.inlinedLocation->Path.fromInlinedLocation,
                    ~shouldReverse=false,
                  ),
                )
              }
            }

            objectVal->B.Val.Object.complete(~isArray)
          }
        }
      }),
    )

    mut
  }

  and shape = {
    (schema: t<'value>, definer: 'value => 'variant): t<'variant> => {
      let schema = schema->toInternal
      schema->updateOutput(mut => {
        let ditem: ditem = Root({
          schema,
          path: Path.empty,
          idx: 0,
        })
        let definition: unknown = definer(ditem->proxify)->Obj.magic

        mut.parser = Some(
          Builder.make((b, ~input, ~selfSchema as _, ~path as _) => {
            let rec getItemOutput = item => {
              switch item {
              | ItemField({target: item, inlinedLocation}) =>
                b->B.Val.get(item->getItemOutput, inlinedLocation)
              | _ => input
              }
            }
            let output =
              b->definitionToOutput(
                ~definition=definition->(Obj.magic: unknown => Definition.t<ditem>),
                ~getItemOutput,
              )

            output
          }),
        )
        mut.to = Some(definitionToTarget(~definition, ~to=ditem))
      })
    }
  }
  and nested = fieldName => {
    let parentCtx = %raw(`this`) // TODO: Add a check that it's binded?
    let cacheId = `~${fieldName}`

    switch parentCtx->X.Dict.unsafeGetOption(cacheId) {
    | Some(ctx) => ctx
    | None => {
        let schemas = []

        let properties = Js.Dict.empty()
        let items = []

        let schema = {
          let schema = base()
          schema.tag = Object
          schema.items = Some(items)
          schema.properties = Some(properties)
          schema.additionalItems = Some(globalConfig.defaultAdditionalItems)
          schema.refiner = Some(schemaRefiner)
          schema->fromInternal
        }

        let target =
          parentCtx.field(fieldName, schema)
          ->Definition.toEmbededItem
          ->X.Option.getUnsafe

        let field:
          type value. (string, schema<value>) => value =
          (fieldName, schema) => {
            let schema = schema->toInternal
            let inlinedLocation = fieldName->X.Inlined.Value.fromString
            if properties->X.Dict.has(fieldName) {
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
            properties->Js.Dict.set(fieldName, schema)
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
          | {tag: Object, items: ?flattenedItems, ?to} => {
              if to->Obj.magic {
                InternalError.panic(
                  `Unsupported nested flatten for transformed object schema ${schema
                    ->fromInternal
                    ->toExpression}`,
                )
              }
              let flattenedItems = flattenedItems->X.Option.getUnsafe
              let result = Js.Dict.empty()
              for idx in 0 to flattenedItems->Js.Array2.length - 1 {
                let item = flattenedItems->Js.Array2.unsafe_get(idx)
                result->Js.Dict.set(item.location, field(item.location, item.schema))
              }
              result->Obj.magic
            }
          | _ => InternalError.panic(`Can't flatten ${schema->fromInternal->toExpression} schema`)
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
      let properties = Js.Dict.empty()

      let flatten = schema => {
        let schema = schema->toInternal
        switch schema {
        | {tag: Object, items: ?flattenedItems} => {
            let flattenedItems = flattenedItems->X.Option.getUnsafe
            for idx in 0 to flattenedItems->Js.Array2.length - 1 {
              let {location, inlinedLocation, schema: flattenedSchema} =
                flattenedItems->Js.Array2.unsafe_get(idx)
              let flattenedSchema = flattenedSchema->toInternal
              switch properties->X.Dict.unsafeGetOption(location) {
              | Some(schema) if schema === flattenedSchema => ()
              | Some(_) =>
                InternalError.panic(
                  `The field ${inlinedLocation} defined twice with incompatible schemas`,
                )
              | None =>
                let item = Item({
                  schema: flattenedSchema,
                  location,
                  inlinedLocation,
                })->ditemToItem
                items->Js.Array2.push(item)->ignore
                properties->Js.Dict.set(location, flattenedSchema)
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
          let inlinedLocation = fieldName->X.Inlined.Value.fromString
          if properties->X.Dict.has(fieldName) {
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
          properties->Js.Dict.set(fieldName, schema)
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

      let mut = base()
      mut.tag = Object
      mut.items = Some(items)
      mut.properties = Some(properties)
      mut.additionalItems = Some(globalConfig.defaultAdditionalItems)
      mut.parser = Some(advancedBuilder(~definition, ~flattened))
      mut.to = Some(definitionToTarget(~definition, ~flattened))
      mut->fromInternal
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
          if items->X.Array.has(idx) {
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
          schema: unit->castToUnknown,
        }

        items->Js.Array2.unsafe_set(idx, ditem)
      }
    }

    let mut = base()
    mut.tag = Array
    mut.items = Some(items)
    mut.additionalItems = Some(Strict)
    mut.parser = Some(advancedBuilder(~definition))
    mut.to = Some(definitionToTarget(~definition))
    mut->fromInternal
  }
  and definitionToRitem = (definition: Definition.t<ditem>, ~path, ~ritemsByItemPath) => {
    if definition->Definition.isNode {
      switch definition->Definition.toEmbededItem {
      | Some(item) =>
        let ritemSchema = item->getDitemSchema->getOutputSchema->copyWithoutCache
        let _ = %raw(`delete ritemSchema.serializer`)
        let ritem = Registred({
          path,
          schema: ritemSchema,
        })
        item->setItemRitem(ritem)
        ritemsByItemPath->Js.Dict.set(item->getFullDitemPath, ritem)
        ritem
      | None => {
          let node = definition->Definition.toNode
          if node->X.Array.isArray {
            let node = node->(Obj.magic: Definition.node<ditem> => array<Definition.t<ditem>>)
            let items = []
            for idx in 0 to node->Js.Array2.length - 1 {
              let location = idx->Js.Int.toString
              let inlinedLocation = `"${location}"`
              let ritem = definitionToRitem(
                node->Js.Array2.unsafe_get(idx),
                ~path=path->Path.concat(Path.fromInlinedLocation(inlinedLocation)),
                ~ritemsByItemPath,
              )
              let item = {
                location,
                inlinedLocation,
                schema: ritem->getRitemSchema->fromInternal,
              }
              items->Js.Array2.unsafe_set(idx, item)
            }
            Node({
              path,
              schema: {
                let mut = base()
                mut.tag = Array
                mut.items = Some(items)
                mut.additionalItems = Some(Strict)
                mut.serializer = Some(neverBuilder)
                mut
              },
            })
          } else {
            let fieldNames = node->Js.Dict.keys
            let node = node->(Obj.magic: Definition.node<ditem> => dict<Definition.t<ditem>>)

            let properties = Js.Dict.empty()
            let items = []
            for idx in 0 to fieldNames->Js.Array2.length - 1 {
              let location = fieldNames->Js.Array2.unsafe_get(idx)
              let inlinedLocation = location->X.Inlined.Value.fromString
              let ritem = definitionToRitem(
                node->Js.Dict.unsafeGet(location),
                ~path=path->Path.concat(Path.fromInlinedLocation(inlinedLocation)),
                ~ritemsByItemPath,
              )
              let item = {
                location,
                inlinedLocation,
                schema: ritem->getRitemSchema->fromInternal,
              }
              items->Js.Array2.unsafe_set(idx, item)
              properties->Js.Dict.set(location, item.schema->toInternal)
            }

            Node({
              path,
              schema: {
                let mut = base()
                mut.tag = Object
                mut.items = Some(items)
                mut.properties = Some(properties)
                mut.additionalItems = Some(globalConfig.defaultAdditionalItems)
                mut.serializer = Some(neverBuilder)
                mut
              },
            })
          }
        }
      }
    } else {
      Discriminant({
        path,
        schema: Literal.parse(definition->Definition.toConstant)->copyWithoutCache,
      })
    }
  }
  and definitionToSchema = (definition: unknown): internal => {
    if definition->Definition.isNode {
      if definition->isSchemaObject {
        definition->(Obj.magic: unknown => internal)
      } else if definition->X.Array.isArray {
        let node = definition->(Obj.magic: unknown => array<unknown>)
        for idx in 0 to node->Js.Array2.length - 1 {
          let schema = node->Js.Array2.unsafe_get(idx)->definitionToSchema
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
        }
        let items = node->(Obj.magic: array<unknown> => array<item>)

        let mut = base()
        mut.tag = Array
        mut.items = Some(items)
        mut.additionalItems = Some(Strict)
        mut.refiner = Some(schemaRefiner)
        mut
      } else {
        let cnstr = (definition->Obj.magic)["constructor"]
        if cnstr->Obj.magic && cnstr !== %raw(`Object`) {
          {
            tag: Instance,
            class: cnstr,
            const: definition->Obj.magic,
          }
        } else {
          let node = definition->(Obj.magic: unknown => dict<unknown>)
          let fieldNames = node->Js.Dict.keys
          let length = fieldNames->Js.Array2.length
          let items = []
          for idx in 0 to length - 1 {
            let location = fieldNames->Js.Array2.unsafe_get(idx)
            let inlinedLocation = location->X.Inlined.Value.fromString
            let schema = node->Js.Dict.unsafeGet(location)->definitionToSchema
            let item = {
              schema: schema->fromInternal,
              location,
              inlinedLocation,
            }
            node->Js.Dict.set(location, schema->(Obj.magic: internal => unknown))
            items->Js.Array2.unsafe_set(idx, item)
          }
          let mut = base()
          mut.tag = Object
          mut.items = Some(items)
          mut.properties = Some(node->(Obj.magic: dict<unknown> => dict<internal>))
          mut.additionalItems = Some(globalConfig.defaultAdditionalItems)
          mut.refiner = Some(schemaRefiner)
          mut
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
    ->fromInternal
  }
}

module Null = {
  let factory = item => {
    Option.factory(item, ~unit=nullAsUnit)
  }
}

let schema = Schema.factory

let js_schema = definition => definition->Obj.magic->Schema.definitionToSchema->fromInternal
let literal = js_schema

let enum = values => Union.factory(values->Js.Array2.map(literal))

let unnestSerializer = Builder.make((b, ~input, ~selfSchema, ~path) => {
  let schema = selfSchema.additionalItems->(Obj.magic: option<additionalItems> => internal)
  let items = schema.items->X.Option.getUnsafe

  let inputVar = b->B.Val.var(input)
  let iteratorVar = b.global->B.varWithoutAllocation
  let outputVar = b.global->B.varWithoutAllocation

  let bb = b->B.scope
  let itemInput = bb->B.notValidVal(`${inputVar}[${iteratorVar}]`)
  let itemOutput = bb->B.withPathPrepend(
    ~input=itemInput,
    ~path,
    ~dynamicLocationVar=iteratorVar,
    ~appendSafe=(bb, ~output) => {
      let initialArraysCode = ref("")
      let settingCode = ref("")
      for idx in 0 to items->Js.Array2.length - 1 {
        let toItem = items->Js.Array2.unsafe_get(idx)
        initialArraysCode := initialArraysCode.contents ++ `new Array(${inputVar}.length),`
        settingCode :=
          settingCode.contents ++
          `${outputVar}[${idx->X.Int.unsafeToString}][${iteratorVar}]=${(
              b->B.Val.get(output, toItem.inlinedLocation)
            ).inline};`
      }
      b.allocate(`${outputVar}=[${initialArraysCode.contents}]`)
      bb.code = bb.code ++ settingCode.contents
    },
    (b, ~input, ~path) => b->parse(~schema, ~input, ~path),
  )
  let itemCode = bb->B.allocateScope

  b.code =
    b.code ++
    `for(let ${iteratorVar}=0;${iteratorVar}<${inputVar}.length;++${iteratorVar}){${itemCode}}`

  if itemOutput.flag->Flag.unsafeHas(ValFlag.async) {
    {
      b,
      var: B._notVar,
      inline: `Promise.all(${outputVar})`,
      flag: ValFlag.async,
    }
  } else {
    {
      b,
      var: B._var,
      inline: outputVar,
      flag: ValFlag.none,
    }
  }
})

let unnest = schema => {
  switch schema {
  | Object({items}) =>
    if items->Js.Array2.length === 0 {
      InternalError.panic("Invalid empty object for S.unnest schema.")
    }
    let schema = schema->toInternal
    let mut = base()
    mut.tag = Array
    mut.items = Some(
      items->Js.Array2.mapi((item, idx) => {
        let location = idx->Js.Int.toString
        {
          schema: Array.factory(item.schema),
          inlinedLocation: `"${location}"`,
          location,
        }
      }),
    )
    mut.additionalItems = Some(Strict)
    mut.parser = Some(
      Builder.make((b, ~input, ~selfSchema as _, ~path) => {
        let inputVar = b->B.Val.var(input)
        let iteratorVar = b.global->B.varWithoutAllocation

        let bb = b->B.scope
        let itemInput = bb->B.Val.Object.make(~isArray=false)
        let lengthCode = ref("")
        for idx in 0 to items->Js.Array2.length - 1 {
          let item = items->Js.Array2.unsafe_get(idx)
          itemInput->B.Val.Object.add(
            item.inlinedLocation,
            bb->B.notValidVal(`${inputVar}[${idx->X.Int.unsafeToString}][${iteratorVar}]`),
          )
          lengthCode := lengthCode.contents ++ `${inputVar}[${idx->X.Int.unsafeToString}].length,`
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
            b->parse(~schema, ~input, ~path)
          },
        )
        let itemCode = bb->B.allocateScope

        b.code =
          b.code ++
          `for(let ${iteratorVar}=0;${iteratorVar}<${outputVar}.length;++${iteratorVar}){${itemCode}}`

        if itemOutput.flag->Flag.unsafeHas(ValFlag.async) {
          output.b->B.asyncVal(`Promise.all(${output.inline})`)
        } else {
          output
        }
      }),
    )

    let to = base()
    to.tag = Array
    to.items = Some(X.Array.immutableEmpty)
    to.additionalItems = Some(Schema(schema->fromInternal))
    to.serializer = Some(unnestSerializer)

    mut.unnest = Some(true)
    mut.to = Some(to)

    mut->fromInternal
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
//     | Null(schema) => `S.null(${schema->internalInline()})`
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
//           | {kind: Datetime, message} =>
//             `->S.datetime(~message=${message->X.Inlined.Value.fromString})`
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
let string = String.schema
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
  Schema.definitionToSchema([v0->castToUnknown, v1->castToUnknown]->Obj.magic)->fromInternal
let tuple3 = (v0, v1, v2) =>
  Schema.definitionToSchema(
    [v0->castToUnknown, v1->castToUnknown, v2->castToUnknown]->Obj.magic,
  )->fromInternal
let union = Union.factory
let jsonString = JsonString.factory

let jsonName = `JSON`
let json = {
  let jsonRef = base()
  jsonRef.tag = Ref
  jsonRef.ref = Some(`${defsPath}${jsonName}`)
  jsonRef.name = Some(jsonName)
  let json = base()
  json.tag = jsonRef.tag
  json.ref = jsonRef.ref
  json.name = Some(jsonName)
  let defs = Js.Dict.empty()
  defs->Js.Dict.set(
    jsonName,
    {
      name: jsonName,
      tag: Union,
      anyOf: [
        String.schema->toInternal,
        bool->toInternal,
        float->toInternal,
        Literal.null,
        Dict.factory(jsonRef->fromInternal)->toInternal,
        Array.factory(jsonRef->fromInternal)->toInternal,
      ],
      // FIXME: use dict{} in V12
      has: %raw(`{
        string: true,
        boolean: true,
        number: true,
        null: true,
        object: true,
        array: true,
      }`),
      refiner: Union.refiner,
    },
  )
  json.defs = Some(defs)
  json->fromInternal
}

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
  | None => `Number must be lower than or equal to ${maxValue->X.Int.unsafeToString}`
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

let port = (schema, ~message=?) => {
  let mutStandard =
    schema
    ->internalRefine((b, ~inputVar, ~selfSchema, ~path) => {
      `${inputVar}>0&&${inputVar}<65536&&${inputVar}%1===0||${switch message {
        | Some(m) => b->B.fail(~message=m, ~path)
        | None =>
          b->B.failWithArg(
            ~path,
            input => InvalidType({
              expected: selfSchema->fromInternal,
              received: input,
            }),
            inputVar,
          )
        }};`
    })
    ->toInternal

  mutStandard.format = Some(Port)
  (mutStandard->reverse).format = Some(Port)

  mutStandard->fromInternal
}

let floatMin = (schema, minValue, ~message as maybeMessage=?) => {
  let message = switch maybeMessage {
  | Some(m) => m
  | None => `Number must be greater than or equal to ${minValue->X.Float.unsafeToString}`
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
  | None => `Number must be lower than or equal to ${maxValue->X.Float.unsafeToString}`
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
  | None => `Array must be ${length->X.Int.unsafeToString} or more items long`
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
  | None => `Array must be ${length->X.Int.unsafeToString} or fewer items long`
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
  | None => `Array must be exactly ${length->X.Int.unsafeToString} items long`
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
  | None => `String must be ${length->X.Int.unsafeToString} or more characters long`
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
  | None => `String must be ${length->X.Int.unsafeToString} or fewer characters long`
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
  | None => `String must be exactly ${length->X.Int.unsafeToString} characters long`
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
      | Some(refinements) => refinements->X.Array.append(refinement)
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

let nullable = schema => {
  Union.factory([schema->castToUnknown, unit->castToUnknown, Literal.null->fromInternal])
}

let nullableAsOption = schema => {
  Union.factory([schema->castToUnknown, unit->castToUnknown, nullAsUnit->castToUnknown])
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
      asyncParser: v => refine(v, s)->X.Promise.thenResolve(() => v),
      serializer: noop,
    }
  })
}

let js_optional = (schema, maybeOr) => {
  // TODO: maybeOr should be part of the unit schema
  let schema = Union.factory([schema->castToUnknown, unit->castToUnknown])
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
      Object({items: items1, additionalItems: additionalItems1}),
      Object({items: items2, additionalItems: additionalItems2}),
    )
    // Filter out S.record schemas
    if additionalItems1->X.Type.typeof === #string &&
    additionalItems2->X.Type.typeof === #string &&
    !((s1->toInternal).to->Obj.magic) &&
    !((s2->toInternal).to->Obj.magic) =>
    let properties = Js.Dict.empty()
    let locations = []
    let inlinedLocations = []
    let items = []
    for idx in 0 to items1->Js.Array2.length - 1 {
      let item = items1->Js.Array2.unsafe_get(idx)
      locations->Js.Array2.push(item.location)->ignore
      inlinedLocations->Js.Array2.push(item.inlinedLocation)->ignore
      properties->Js.Dict.set(item.location, item.schema->toInternal)
    }
    for idx in 0 to items2->Js.Array2.length - 1 {
      let item = items2->Js.Array2.unsafe_get(idx)
      if !(properties->X.Dict.has(item.location)) {
        locations->Js.Array2.push(item.location)->ignore
        inlinedLocations->Js.Array2.push(item.inlinedLocation)->ignore
      }
      properties->Js.Dict.set(item.location, item.schema->toInternal)
    }
    for idx in 0 to locations->Js.Array2.length - 1 {
      let location = locations->Js.Array2.unsafe_get(idx)
      items
      ->Js.Array2.push({
        location,
        inlinedLocation: inlinedLocations->Js.Array2.unsafe_get(idx),
        schema: properties->Js.Dict.unsafeGet(location)->fromInternal,
      })
      ->ignore
    }

    let mut = base()
    mut.tag = Object
    mut.items = Some(items)
    mut.properties = Some(properties)
    mut.additionalItems = Some(additionalItems1)
    mut.refiner = Some(Schema.schemaRefiner)
    Some(mut->fromInternal)
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
    | Unknown(_) => ()
    | Ref({ref}) if ref === `${defsPath}${jsonName}` => ()
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

    switch schema->Metadata.get(~id=jsonSchemaMetadataId) {
    | Some(metadataRawSchema) => jsonSchema->Mutable.mixin(metadataRawSchema)
    | None => ()
    }

    jsonSchema->Mutable.toReadOnly
  }
}

let toJSONSchema = schema => {
  let target = schema->toInternal
  jsonableValidation(~output=target, ~parent=target, ~path=Path.empty, ~flag=Flag.jsonableOutput)
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

let castAnySchemaToJsonableS = (Obj.magic: schema<'any> => schema<Js.Json.t>)
let rec fromJSONSchema = {
  @inline
  let primitiveToSchema = primitive => {
    Literal.parse(primitive)->fromInternal->castAnySchemaToJsonableS
  }

  let toIntSchema = (jsonSchema: JSONSchema.t) => {
    let schema = int
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

  let definitionToDefaultValue = definition =>
    switch definition->JSONSchema.Definition.classify {
    | Schema(s) => s.default
    | Boolean(_) => None
    }

  (jsonSchema: JSONSchema.t) => {
    let anySchema = json

    let definitionToSchema = definition =>
      switch definition->JSONSchema.Definition.classify {
      | Schema(s) => s->fromJSONSchema
      | Boolean(_) => anySchema
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
        let schema = object(s => {
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
            Js.Dict.set(obj, key, s.field(key, propertySchema))
          })
          obj
        })
        let schema = switch jsonSchema {
        | {additionalProperties} if additionalProperties === JSONSchema.Definition.boolean(false) =>
          schema->strict
        | _ => schema
        }
        schema->castAnySchemaToJsonableS
      | None =>
        switch jsonSchema.additionalProperties {
        | Some(additionalProperties) =>
          switch additionalProperties->JSONSchema.Definition.classify {
          | Boolean(true) => dict(anySchema)->castAnySchemaToJsonableS
          | Boolean(false) => object(_ => ())->strict->castAnySchemaToJsonableS
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
    | {allOf: definitions} =>
      anySchema->refine(s => data => {
        definitions->Js.Array2.forEach(d => {
          try data->assertOrThrow(d->definitionToSchema) catch {
          | _ => s.fail("Should pass for all schemas of the allOf property.")
          }
        })
      })
    | {oneOf: []} => anySchema
    | {oneOf: [d]} => d->definitionToSchema
    | {oneOf: definitions} =>
      anySchema->refine(s => data => {
        let hasOneValidRef = ref(false)
        definitions->Js.Array2.forEach(d => {
          let passed = try {
            let _ = data->assertOrThrow(d->definitionToSchema)
            true
          } catch {
          | _ => false
          }
          if passed {
            if hasOneValidRef.contents {
              s.fail("Should pass single schema according to the oneOf property.")
            }
            hasOneValidRef.contents = true
          }
        })
        if hasOneValidRef.contents->not {
          s.fail("Should pass at least one schema according to the oneOf property.")
        }
      })
    | {not} =>
      anySchema->refine(s => data => {
        let passed = try {
          let _ = data->assertOrThrow(not->definitionToSchema)
          true
        } catch {
        | _ => false
        }
        if passed {
          s.fail("Should NOT be valid against schema in the not property.")
        }
      })
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
      let schema = string
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
      | {format: "date-time"} => schema->datetime->castAnySchemaToJsonableS
      | _ => schema->castAnySchemaToJsonableS
      }

    | {type_} if type_ === JSONSchema.Arrayable.single(#integer) => jsonSchema->toIntSchema
    | {type_, format: "int64"} if type_ === JSONSchema.Arrayable.single(#number) =>
      jsonSchema->toIntSchema
    | {type_, multipleOf: 1.} if type_ === JSONSchema.Arrayable.single(#number) =>
      jsonSchema->toIntSchema
    | {type_} if type_ === JSONSchema.Arrayable.single(#number) => {
        let schema = float
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
    | {type_} if type_ === JSONSchema.Arrayable.single(#boolean) => bool->castAnySchemaToJsonableS
    | {type_} if type_ === JSONSchema.Arrayable.single(#null) =>
      literal(%raw(`null`))->castAnySchemaToJsonableS
    | {if_, then, else_} => {
        let ifSchema = if_->definitionToSchema
        let thenSchema = then->definitionToSchema
        let elseSchema = else_->definitionToSchema
        anySchema->refine(_ => data => {
          let passed = try {
            let _ = data->assertOrThrow(ifSchema)
            true
          } catch {
          | _ => false
          }
          if passed {
            data->assertOrThrow(thenSchema)
          } else {
            data->assertOrThrow(elseSchema)
          }
        })
      }
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
