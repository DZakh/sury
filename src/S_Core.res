@@uncurried
@@warning("-30")

type never

external castAnyToUnknown: 'any => unknown = "%identity"
external castUnknownToAny: unknown => 'any = "%identity"

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

    @inline
    let isSome = x =>
      switch x {
      | None => false
      | Some(_) => true
      }
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

  module Re = {
    @send
    external toString: Js.Re.t => string = "toString"
  }

  module Object = {
    let immutableEmpty = %raw(`{}`)

    @val external internalClass: Js.Types.obj_val => string = "Object.prototype.toString.call"
  }

  module Array = {
    @send
    external append: (array<'a>, 'a) => array<'a> = "concat"

    @get_index
    external unsafeGetOptionByString: (array<'a>, string) => option<'a> = ""

    @inline
    let has = (array, idx) => {
      array->Js.Array2.unsafe_get(idx)->(Obj.magic: 'a => bool)
    }

    let isArray = Js.Array2.isArray
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
  }

  module Dict = {
    @val
    external copy: (@as(json`{}`) _, dict<'a>) => dict<'a> = "Object.assign"

    @get_index
    external unsafeGetOption: (dict<'a>, string) => option<'a> = ""

    @get_index
    external unsafeGetOptionBySymbol: (dict<'a>, Js.Types.symbol) => option<'a> = ""

    @inline
    let has = (dict, key) => {
      dict->Js.Dict.unsafeGet(key)->(Obj.magic: 'a => bool)
    }

    @inline
    let deleteInPlace = (dict, key) => {
      Js.Dict.unsafeDeleteKey(dict->(Obj.magic: dict<'a> => dict<string>), key)
    }
  }

  module Float = {
    external unsafeToString: float => string = "%identity"
  }

  module BigInt = {
    @send external toString: bigint => string = "toString"
    @inline
    let toString = bigint => bigint->toString ++ "n"
  }

  module Function = {
    @variadic @new
    external _make: array<string> => 'function = "Function"

    @inline
    let make2 = (~ctxVarName1, ~ctxVarValue1, ~ctxVarName2, ~ctxVarValue2, ~inlinedFunction) => {
      _make([ctxVarName1, ctxVarName2, `return ${inlinedFunction}`])(ctxVarValue1, ctxVarValue2)
    }

    @send external toString: Js.Types.function_val => string = "toString"
  }

  module Symbol = {
    type t = Js.Types.symbol

    @val external make: string => t = "Symbol"

    @send external toString: t => string = "toString"
  }

  module Inlined = {
    module Value = {
      @inline
      let stringify = any => {
        if any === %raw("void 0") {
          "undefined"
        } else {
          any->Js.Json.stringifyAny->Obj.magic
        }
      }

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

    module Float = {
      @inline
      let toRescript = float => float->Js.Float.toString ++ (mod_float(float, 1.) === 0. ? "." : "")
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

let vendor = "rescript-schema"
let symbol = Stdlib.Symbol.make(vendor)
let itemSymbol = Stdlib.Symbol.make("item")

type unknownKeys = | @as("strip") Strip | @as("strict") Strict

type typeTag =
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
  | @as("dict") Dict
  | @as("tuple") Tuple
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
  | @as("json") JSON({}) // FIXME: Remove it in favor of Union ?
// FIXME: Add recursive
// FIXME: deprecate literal
and schema<'a> = t<'a>
and optional = {
  item: t<unknown>,
  hasUndefined?: bool,
  hasNull?: bool,
}
and internal = {
  @as("type")
  tag: typeTag,
  mutable const?: char, // use char to avoid Caml_option.some
  optional?: optional,
  item?: internal,
  format?: internalFormat,
  advanced?: bool, // FIXME: Should rename it?
  mutable anyOf?: array<internal>,
  mutable unknownKeys?: unknownKeys,
  mutable items?: array<item>,
  mutable fields?: dict<item>,
  mutable name?: string,
  mutable output?: unit => internal, // Optional value means that it either should reverse to self or it's already a reversed schema
  // This can also be an `internal` itself, but because of the bug https://github.com/rescript-lang/rescript/issues/7314 handle it unsafely
  @as("b")
  mutable builder: builder,
  @as("f")
  mutable typeFilter?: (b, ~inputVar: string) => string,
  mutable isAsync?: bool, // Optional value means that it's not lazily computed yet.
  @as("~standard")
  mutable standard?: standard, // This is optional for convenience. The object added on make call
}

// and tagged =
//   | @as("never") Never
//   | @as("unknown") Unknown
//   | @as("string") String
//   | @as("int32") Int
//   | @as("number") Float
//   | @as("bigint") BigInt
//   | @as("boolean") Bool
//   | @as("literal") Literal(literal)
//   | @as("option") Option(t<unknown>)
//   | @as("null") Null(t<unknown>)
//   | @as("array") Array(t<unknown>)
//   | @as("object")
//   Object({
// items: array<item>,
// fields: dict<item>,
//       unknownKeys: unknownKeys,
//       advanced: bool,
//     })
//   | @as("tuple") Tuple({items: array<item>})
//   | @as("union") Union(array<t<unknown>>)
//   | @as("dict") Dict(t<unknown>)
//   | @as("JSON") JSON({validated: bool})
and item = {
  schema: t<unknown>,
  location: string,
  inlinedLocation: string,
}
and builder = (b, ~input: val, ~selfSchema: internal, ~path: Path.t) => val
and val = {
  @as("b")
  b: b,
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
and error = private {flag: flag, code: errorCode, path: Path.t}
and errorCode =
  | OperationFailed(string)
  | InvalidOperation({description: string})
  | InvalidType({expected: schema<unknown>, received: unknown})
  | ExcessField(string)
  | InvalidUnion(array<error>)
  | UnexpectedAsync
  | InvalidJsonSchema(schema<unknown>)
@tag("success")
and jsResult<'value> = | @as(true) Success({value: 'value}) | @as(false) Failure({error: error})

type exn += private Raised(error)

external toUnknown: t<'any> => t<unknown> = "%identity"
external toInternal: t<'any> => internal = "%identity"
external fromInternal: internal => t<'any> = "%identity"

// A dirty check that this is rescript-schema object
@inline
let isSchemaObject = obj => (obj->Obj.magic).standard->Obj.magic

// TODO: Can be improved after ReScript supports `in` (https://github.com/rescript-lang/rescript/issues/7313)
let isLiteral: internal => bool = %raw(`s => "const" in s`)

type globalConfig = {
  @as("r")
  mutable recCounter: int,
  @as("u")
  mutable defaultUnknownKeys: unknownKeys,
  @as("n")
  mutable disableNanNumberValidation: bool,
}

type globalConfigOverride = {
  defaultUnknownKeys?: unknownKeys,
  disableNanNumberValidation?: bool,
}

let initialDefaultUnknownKeys = Strip
let initialDisableNanNumberProtection = false
let globalConfig = {
  recCounter: 0,
  defaultUnknownKeys: initialDefaultUnknownKeys,
  disableNanNumberValidation: initialDisableNanNumberProtection,
}

module InternalError = {
  %%raw(`
    class RescriptSchemaError extends Error {
      constructor(code, flag, path) {
        super();
        this.flag = flag;
        this.code = code;
        this.path = path;
        this.s = symbol;
        this.RE_EXN_ID = Raised;
        this._1 = this;
        this.Error = this;
        this.name = "RescriptSchemaError";
      }
      get message() {
        return message(this);
      }
      get reason() {
        return reason(this);
      }
    }
  `)

  @new
  external make: (~code: errorCode, ~flag: int, ~path: Path.t) => error = "RescriptSchemaError"

  let getOrRethrow = (exn: exn) => {
    if %raw("exn&&exn.s===symbol") {
      exn->(Obj.magic: exn => error)
    } else {
      raise(exn)
    }
  }

  @inline
  let panic = message => Stdlib.Exn.raiseError(Stdlib.Exn.makeError(`[${vendor}] ${message}`))
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
let copy: internal => internal = %raw(`(schema) => {
  let c = {}
  for (let k in schema) {
    if (k > "a") {
      c[k] = schema[k]
    }
  }
  return c
}`)
let mixingMetadata: (internal, ~from: internal) => unit = %raw(`(schema, from) => {
  for (let k in from) {
    if (k[0] === "m") {
      schema[k] = from[k]
    }
  }
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

let reverse = (schema: internal) => {
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
      schema.output = reversed->Obj.magic
      reversed.output = schema->Obj.magic
      reversed
    }
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
      let isEmbed = (val: val) => val.var === _var && val.inline->Js.String2.get(0) === "e"

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

    let registerInvalidJson = (b, ~selfSchema, ~path) => {
      if b.global.flag->Flag.unsafeHas(Flag.jsonableOutput) {
        b->raise(~path, ~code=InvalidJsonSchema(selfSchema->fromInternal))
      }
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

    let typeFilterCode = (b: b, ~schema, ~input, ~path) => {
      let inputVar = b->Val.var(input)
      `if(${b->(schema.typeFilter->Stdlib.Option.unsafeUnwrap)(~inputVar)}){${b->failWithArg(
          ~path,
          input => InvalidType({
            expected: schema->fromInternal,
            received: input,
          }),
          inputVar,
        )}}`
    }

    @inline
    let parse = (b: b, ~schema, ~input, ~path) => {
      schema.builder(b, ~input, ~selfSchema=schema, ~path)
    }

    let parseWithTypeValidation = (b: b, ~schema, ~input, ~path) => {
      if (
        schema.typeFilter->Stdlib.Option.isSome &&
          (b.global.flag->Flag.unsafeHas(Flag.typeValidation) || schema->isLiteral)
      ) {
        b.code = b.code ++ b->typeFilterCode(~schema, ~input, ~path)
      }
      b->parse(~schema, ~input, ~path)
    }
  }

  let noop = make((_b, ~input, ~selfSchema as _, ~path as _) => input)

  let invalidJson = make((b, ~input, ~selfSchema, ~path) => {
    b->B.registerInvalidJson(~selfSchema, ~path)
    input
  })

  let noopOperation = i => i->Obj.magic

  @inline
  let intitialInputVar = "i"

  let compile = (builder: builder, ~schema, ~flag) => {
    if (
      flag->Flag.unsafeHas(Flag.jsonableOutput) &&
        switch schema->reverse {
        | {optional: {hasUndefined: true}}
        | {tag: Undefined} => true
        | _ => false
        }
    ) {
      Stdlib.Exn.raiseAny(
        InternalError.make(~code=InvalidJsonSchema(schema->fromInternal), ~flag, ~path=Path.empty),
      )
    }

    let b = B.rootScope(~flag)
    let input = {b, var: B._var, isAsync: false, inline: intitialInputVar}

    let output = builder(b, ~input, ~selfSchema=schema, ~path=Path.empty)
    schema.isAsync = Some(output.isAsync)

    if b.varsAllocation !== "" {
      b.code = `let ${b.varsAllocation};${b.code}`
    }

    if (
      schema.typeFilter->Stdlib.Option.isSome &&
        (flag->Flag.unsafeHas(Flag.typeValidation) || schema->isLiteral)
    ) {
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
      noopOperation
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

      let inlinedFunction = `${intitialInputVar}=>{${b.code}return ${inlinedOutput.contents}}`

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
}
// TODO: Split validation code and transformation code
module B = Builder.B

let operationFn = (s, o) => {
  let s = s->toInternal
  if %raw(`o in s`) {
    %raw(`s[o]`)
  } else {
    let ss = o->Flag.unsafeHas(Flag.reverse) ? s->reverse : s
    let f = ss.builder->Builder.compile(~schema=ss, ~flag=o)
    let _ = %raw(`s[o] = f`)
    f
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
    let string = ref("{")
    for i in 0 to keys->Array.length - 1 {
      let key = keys->Js.Array2.unsafe_get(i)
      let value = dict->Js.Dict.unsafeGet(key)
      if i !== 0 {
        string := string.contents ++ ", "
      }
      string := `${string.contents}"${key}": ${stringify(value)}`
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

// FIXME: Support `like example` for root schema name
// FIXME: For recursive name use less human-readable format
let rec name = schema => {
  let schema = schema->toInternal
  switch schema {
  | {name} => name
  | {const} => const->Obj.magic->stringify
  | {anyOf} =>
    anyOf
    ->(Obj.magic: array<internal> => array<t<'a>>)
    ->Js.Array2.map(name)
    ->Js.Array2.joinWith(" | ")
  | {format} => (format :> string)
  | {tag, item} =>
    `${(tag :> string)}<${item
      ->fromInternal
      ->name}>`
  | {tag: Object, ?items} =>
    let items = items->Stdlib.Option.unsafeUnwrap
    if items->Js.Array2.length === 0 {
      `{}`
    } else {
      `{ ${items
        ->Js.Array2.map(item => {
          `${item.location}: ${item.schema->toInternal->fromInternal->name};`
        })
        ->Js.Array2.joinWith(" ")} }`
    }
  | {tag: Tuple, ?items} =>
    let items = items->Stdlib.Option.unsafeUnwrap
    `[${items
      ->Js.Array2.map(item => item.schema->toInternal->fromInternal->name)
      ->Js.Array2.joinWith(", ")}]`
  | {tag: NaN} => "NaN"
  | {tag} => (tag :> string)
  }
}

module Error = {
  type class
  let class: class = %raw("RescriptSchemaError")

  let make = InternalError.make

  let raise = (error: error) => error->Stdlib.Exn.raiseAny

  let rec reason = (error: error, ~nestedLevel=0) => {
    switch error.code {
    | OperationFailed(reason) => reason
    | InvalidOperation({description}) => description
    | UnexpectedAsync => "Encountered unexpected async transform or refine. Use ParseAsync operation instead"
    | ExcessField(fieldName) =>
      `Encountered disallowed excess key ${fieldName->Stdlib.Inlined.Value.fromString} on an object`
    | InvalidType({expected, received}) => `Must be ${expected->name} (was ${received->stringify})`
    | InvalidJsonSchema(schema) => `The '${schema->name}' schema cannot be converted to JSON`
    | InvalidUnion(errors) => {
        let lineBreak = `\n${" "->Js.String2.repeat(nestedLevel * 2)}`
        let reasonsDict = Js.Dict.empty()

        errors->Js.Array2.forEach(error => {
          let reason = error->reason(~nestedLevel=nestedLevel->Stdlib.Int.plus(1))
          let location = switch error.path {
          | "" => ""
          | nonEmptyPath => `Failed at ${nonEmptyPath}. `
          }
          reasonsDict->Js.Dict.set(`- ${location}${reason}`, ())
        })
        let uniqueReasons = reasonsDict->Js.Dict.keys

        `Invalid union with following errors${lineBreak}${uniqueReasons->Js.Array2.joinWith(
            lineBreak,
          )}`
      }
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

    let pathText = switch error.path {
    | "" => "root"
    | nonEmptyPath => nonEmptyPath
    }
    `${text.contents} at ${pathText}. Reason: ${error->reason}`
  }
}

let toStandard = (schema: internal) => {
  schema.standard = Some({
    version: 1,
    vendor,
    validate: input => {
      try {
        {"value": parseOrThrow(input, schema->fromInternal)}
      } catch {
      | _ => {
          let error = %raw(`exn`)->InternalError.getOrRethrow
          {
            "issues": [
              {
                "message": error->Error.message,
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

module Literal = {
  open Stdlib

  @inline
  let unsafeInline = schema => {
    switch schema {
    | {tag: String, const} => const->Obj.magic->Stdlib.Inlined.Value.fromString
    | {tag: BigInt, const} => const->Obj.magic ++ "n"
    | {?const} => const->Obj.magic
    }
  }

  let inlinedTypeFilter = (_b, ~inputVar) => {
    `${inputVar}!==${(%raw(`this`): internal)->unsafeInline}`
  }

  let strictEqualTypeFilter = (b, ~inputVar) => {
    `${inputVar}!==${b->B.embed((%raw(`this`): internal).const)}`
  }

  let undefined = {
    tag: Undefined,
    const: %raw(`void 0`),
    builder: Builder.invalidJson,
    typeFilter: inlinedTypeFilter,
  }

  let null = {
    tag: Null,
    const: %raw(`null`),
    builder: Builder.noop,
    typeFilter: inlinedTypeFilter,
  }

  let nan = {
    tag: NaN,
    builder: Builder.invalidJson,
    typeFilter: (_b, ~inputVar) => {
      `!Number.isNaN(${inputVar})`
    },
  }

  let jsonable = tag => {
    {
      tag,
      builder: Builder.noop,
      typeFilter: inlinedTypeFilter,
    }
  }

  let nonJsonable = tag => {
    {
      tag,
      builder: Builder.invalidJson,
      typeFilter: strictEqualTypeFilter,
    }
  }

  let parse = (value): internal => {
    let value = value->castAnyToUnknown
    if value === %raw(`null`) {
      null
    } else {
      let schema = switch value->Type.typeof {
      | #string => jsonable(String)
      | #boolean => jsonable(Boolean)
      | #undefined => undefined
      | #number if value->(Obj.magic: unknown => float)->Js.Float.isNaN => nan
      | #number => jsonable(Number)
      | #bigint => nonJsonable(BigInt)
      | #symbol => nonJsonable(Symbol)
      | #object => nonJsonable(Instance)
      | #function => nonJsonable(Function)
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
    external toKey: t<'metadata> => string = "%identity"
  } = {
    type t<'metadata> = string

    let make = (~namespace, ~name) => {
      `metadata:${namespace}:${name}`
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
    // FIXME: Also apply for reversed?
    let schema = schema->toInternal
    let mut = schema->copy
    mut->setInPlace(~id, metadata)
    mut->toStandard
    // let metadataMap = schema.metadataMap->Map.set(~id, metadata)
    // makeSchema(
    //   ~name=schema.name,
    //   ~builder=schema.builder,
    //   ~tagged=schema.tagged,
    //   ~typeFilter=schema.typeFilter,
    //   ~metadataMap,
    //   ~reverse=() => {
    //     let schema = schema.output()
    //     makeReverseSchema(
    //       ~name=schema.name,
    //       ~builder=schema.builder,
    //       ~tagged=schema.tagged,
    //       ~typeFilter=schema.typeFilter,
    //       ~metadataMap,
    //     )
    //   },
    // )
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
    tag: Unknown, // FIXME: Use recursive
    builder,
    output,
  }
  let schema = fn(placeholder->fromInternal)->toInternal

  mergeInPlace(placeholder, schema)
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

let setName = (schema, name) => {
  let schema = schema->toInternal
  let mut = schema->copy
  mut.name = Some(name) // TODO: Better test reverse
  mut->toStandard
}

let removeTypeValidation = schema => {
  let schema = schema->toInternal
  let mut = schema->copy
  mut.typeFilter = None // TODO: Better test reverse
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
      b->B.registerInvalidJson(~selfSchema, ~path)
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
      name,
      tag: Unknown,
      builder: Builder.make((b, ~input, ~selfSchema, ~path) => {
        b->B.registerInvalidJson(~selfSchema, ~path)
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

let unknown = {
  tag: Unknown,
  builder: Builder.invalidJson,
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
  let parse = (b, ~schemas, ~path, ~input, ~output) => {
    let isMultiple = schemas->Js.Array2.length > 1
    let rec loop = (idx, errorCodes) => {
      if idx === schemas->Js.Array2.length {
        b->B.failWithArg(
          ~path,
          internalErrors => {
            InvalidUnion(internalErrors)
          },
          `[${errorCodes}]`,
        )
      } else {
        let schema = schemas->Js.Array2.unsafe_get(idx)
        let parserCode = try {
          let bb = b->B.scope
          let itemOutput = bb->B.parse(~schema, ~input, ~path=Path.empty)

          if itemOutput !== input {
            bb.code = bb.code ++ bb->B.Val.set(output, itemOutput)
          }

          bb->B.allocateScope
        } catch {
        | _ => "throw " ++ b->B.embed(%raw(`exn`)->InternalError.getOrRethrow)
        }
        if isMultiple {
          let errorVar = `e` ++ idx->Stdlib.Int.unsafeToString
          `try{${parserCode}}catch(${errorVar}){` ++
          loop(idx + 1, errorCodes ++ errorVar ++ ",") ++ "}"
        } else {
          parserCode
        }
      }
    }
    loop(0, "")
  }

  let builder = Builder.make((b, ~input, ~selfSchema, ~path) => {
    let schemas = selfSchema.anyOf->Stdlib.Option.unsafeUnwrap
    let inputVar = b->B.Val.var(input)
    let output = {b, var: B._notVar, inline: inputVar, isAsync: false}

    let byTypeFilter = Js.Dict.empty()
    let typeFilters = []
    for idx in 0 to schemas->Js.Array2.length - 1 {
      let schema = schemas->Js.Array2.unsafe_get(idx)
      let typeFilterCode = if schema.typeFilter->Stdlib.Option.isSome {
        b->(schema.typeFilter->Stdlib.Option.unsafeUnwrap)(~inputVar)
      } else {
        ""
      }

      switch byTypeFilter->Js.Dict.get(typeFilterCode) {
      | Some(schemas) => schemas->Js.Array2.push(schema)->ignore
      | None => {
          typeFilters->Js.Array2.push(typeFilterCode)->ignore
          byTypeFilter->Js.Dict.set(typeFilterCode, [schema])
        }
      }
    }

    let rec loopTypeFilters = (idx, maybeUnknownParser) => {
      if idx === typeFilters->Js.Array2.length {
        switch maybeUnknownParser {
        | None =>
          b->B.failWithArg(
            ~path,
            received => InvalidType({
              expected: selfSchema->fromInternal,
              received,
            }),
            inputVar,
          )
        | Some(unknownParserCode) => unknownParserCode
        }
      } else {
        let typeFilterCode = typeFilters->Js.Array2.unsafe_get(idx)
        let schemas = byTypeFilter->Js.Dict.unsafeGet(typeFilterCode)

        let parserCode = parse(b, ~schemas, ~path, ~input, ~output)

        switch typeFilterCode {
        | "" => loopTypeFilters(idx + 1, Some(parserCode))
        | _ =>
          `if(${typeFilterCode}){` ++
          loopTypeFilters(idx + 1, maybeUnknownParser) ++
          switch parserCode {
          | "" => ""
          | _ => `}else{` ++ parserCode
          } ++ `}`
        }
      }
    }
    b.code = b.code ++ loopTypeFilters(0, None)

    if output.isAsync {
      b->B.asyncVal(`Promise.resolve(${output.inline})`)
    } else {
      output
    }
  })

  let rec factory = schemas => {
    let schemas: array<internal> = schemas->Obj.magic

    switch schemas {
    | [] => InternalError.panic("S.union requires at least one item")
    | [schema] => schema
    | _ => {
        tag: Union,
        anyOf: schemas, // FIXME: Handle optional
        builder,
        output,
      }
    }->toStandard
  }
  and output = () => {
    let schemas = (%raw(`this`): internal).anyOf->Stdlib.Option.unsafeUnwrap
    factory(schemas->Js.Array2.map(s => s->reverse->fromInternal))->toInternal
  }
}

module Option = {
  type default = Value(unknown) | Callback(unit => unknown)

  let defaultMetadataId: Metadata.Id.t<default> = Metadata.Id.make(
    ~namespace="rescript-schema",
    ~name="Option.default",
  )

  let default = schema => schema->Metadata.get(~id=defaultMetadataId)

  let makeBuilder = (~isNullInput, ~isNullOutput) =>
    Builder.make((b, ~input, ~selfSchema, ~path) => {
      let {item} = selfSchema.optional->Stdlib.Option.unsafeUnwrap
      let item = item->toInternal

      let bb = b->B.scope
      let itemInput = if (
        !(b.global.flag->Flag.unsafeHas(Flag.typeValidation)) &&
        (item.tag === Unknown ||
        item.tag === Undefined ||
        switch item.optional {
        | Some({hasUndefined: true}) => true
        | _ => false
        })
      ) {
        bb->B.val(`${bb->B.embed(%raw("Caml_option.valFromOption"))}(${b->B.Val.var(input)})`)
      } else {
        input
      }

      let itemOutput = bb->B.parse(~schema=item, ~input=itemInput, ~path)
      let itemCode = bb->B.allocateScope

      let inputLiteral = isNullInput ? "null" : "void 0"
      let ouputLiteral = isNullOutput ? "null" : "void 0"

      let isTransformed = inputLiteral !== ouputLiteral || itemOutput !== input

      let output = isTransformed
        ? {b, var: B._notVar, isAsync: itemOutput.isAsync, inline: ""}
        : input

      if itemCode !== "" || isTransformed {
        b.code =
          b.code ++
          `if(${b->B.Val.var(input)}!==${inputLiteral}){${itemCode}${b->B.Val.set(
              output,
              itemOutput,
            )}}${inputLiteral !== ouputLiteral || output.isAsync
              ? `else{${b->B.Val.set(output, b->B.val(ouputLiteral))}}`
              : ""}`
      }

      output
    })

  let typeFilter = (~schema, ~inlinedNoneValue) => {
    if schema.typeFilter->Stdlib.Option.isSome {
      Some(
        (b, ~inputVar) => {
          `${inputVar}!==${inlinedNoneValue}&&(${b->(schema.typeFilter->Stdlib.Option.unsafeUnwrap)(
              ~inputVar,
            )})`
        },
      )
    } else {
      None
    }
  }

  let rec factory = item => {
    let item = item->toInternal
    {
      tag: Union,
      anyOf: [item, unit->toInternal], // FIXME:
      optional: {
        item: item->fromInternal->toUnknown,
        hasUndefined: true,
      },
      builder: makeBuilder(~isNullInput=false, ~isNullOutput=false),
      typeFilter: ?typeFilter(~schema=item, ~inlinedNoneValue="void 0"),
      output: Output.item(~factory, ~item),
    }->toStandard
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
        | {optional: {item}} => item->toInternal
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

// module Null = {
//   let factory = schema => {
//     let schema = schema->toUnknown
//     makeSchema(
//       ~name=() => `${schema.name()} | null`,
//       ~tagged=Null(schema),
//       ~builder=Option.makeBuilder(~isNullInput=true, ~isNullOutput=false),
//       ~typeFilter=Option.typeFilter(~schema, ~inlinedNoneValue="null"),
//       ~reverse=() => {
//         let child = schema.output()
//         makeReverseSchema(
//           ~name=Option.name,
//           ~tagged=Option(child),
//           ~builder=Option.makeBuilder(~isNullInput=false, ~isNullOutput=true),
//           ~typeFilter=Option.typeFilter(~schema, ~inlinedNoneValue="void 0"),
//         )
//       },
//     )
//   }
// }

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

    let metadataId: Metadata.Id.t<array<t>> = Metadata.Id.make(
      ~namespace="rescript-schema",
      ~name="Array.refinements",
    )
  }

  let refinements = schema => {
    switch schema->Metadata.get(~id=Refinement.metadataId) {
    | Some(m) => m
    | None => []
    }
  }

  let typeFilter = (_b, ~inputVar) => `!Array.isArray(${inputVar})`

  let rec factory = item => {
    let item = item->toInternal
    {
      tag: Array,
      item,
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
      typeFilter,
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

  let typeFilter = (b, ~inputVar) => {
    let {?unknownKeys, ?items}: internal = %raw(`this`)
    let items = items->Stdlib.Option.unsafeUnwrap
    let code = ref(
      `typeof ${inputVar}!=="object"||!${inputVar}` ++ (
        unknownKeys === Some(Strict) ? `||Array.isArray(${inputVar})` : ""
      ),
    )
    for idx in 0 to items->Js.Array2.length - 1 {
      let {schema, inlinedLocation} = items->Js.Array2.unsafe_get(idx)
      let item = schema->toInternal
      if item->isLiteral {
        code :=
          code.contents ++
          "||" ++
          b->(item.typeFilter->Stdlib.Option.unsafeUnwrap)(
            ~inputVar=Path.concat(inputVar, Path.fromInlinedLocation(inlinedLocation)),
          )
      }
    }
    code.contents
  }

  let rec setUnknownKeys = (schema, unknownKeys, ~deep) => {
    let schema = schema->toInternal
    switch schema {
    | {unknownKeys: schemaUnknownKeys, ?items} if schemaUnknownKeys !== unknownKeys =>
      let mut = schema->copy
      mut.unknownKeys = Some(unknownKeys)
      if deep {
        let items = items->Stdlib.Option.unsafeUnwrap

        let newItems = []
        let newFields = Js.Dict.empty()
        for idx in 0 to items->Js.Array2.length - 1 {
          let item = items->Js.Array2.unsafe_get(idx)
          let newSchema =
            setUnknownKeys(
              item.schema->(Obj.magic: t<unknown> => t<'a>),
              unknownKeys,
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
  schema->Object.setUnknownKeys(Strip, ~deep=false)
}

let deepStrip = schema => {
  schema->Object.setUnknownKeys(Strip, ~deep=true)
}

let strict = schema => {
  schema->Object.setUnknownKeys(Strict, ~deep=false)
}

let deepStrict = schema => {
  schema->Object.setUnknownKeys(Strict, ~deep=true)
}

module Tuple = {
  type s = {
    item: 'value. (int, t<'value>) => 'value,
    tag: 'value. (int, 'value) => unit,
  }

  let typeFilter = (b, ~inputVar) => {
    let items = (%raw(`this`): internal).items->Stdlib.Option.unsafeUnwrap
    let length = items->Js.Array2.length
    let code = ref(
      b->Array.typeFilter(~inputVar) ++
        `||${inputVar}.length!==${length->Stdlib.Int.unsafeToString}`,
    )
    for idx in 0 to length - 1 {
      let {schema, inlinedLocation} = items->Js.Array2.unsafe_get(idx)
      let item = schema->toInternal
      if item->isLiteral {
        code :=
          code.contents ++
          "||" ++
          b->(item.typeFilter->Stdlib.Option.unsafeUnwrap)(
            ~inputVar=Path.concat(inputVar, Path.fromInlinedLocation(inlinedLocation)),
          )
      }
    }
    code.contents
  }
}

module Dict = {
  let typeFilter = (_b, ~inputVar) =>
    `typeof ${inputVar}!=="object"||!${inputVar}||Array.isArray(${inputVar})`

  let rec factory = item => {
    let item = item->toInternal
    {
      tag: Dict,
      item,
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
      typeFilter,
      output: Output.item(~factory, ~item),
    }->toStandard
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

    let metadataId: Metadata.Id.t<array<t>> = Metadata.Id.make(
      ~namespace="rescript-schema",
      ~name="String.refinements",
    )
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

  let typeFilter = (_b, ~inputVar) => `typeof ${inputVar}!=="string"`

  let schema = {
    tag: String,
    builder: Builder.noop,
    typeFilter,
  }->toStandard
}

module JsonString = {
  let factory = (item, ~space=0) => {
    let item = item->toInternal
    {
      tag: String,
      item,
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
      typeFilter: String.typeFilter,
      output: () => {
        let reversed = item->reverse
        let mut = reversed->copy
        mut.builder = Builder.make((b, ~input, ~selfSchema as _, ~path) => {
          let prevFlag = b.global.flag
          b.global.flag = prevFlag->Flag.with(Flag.jsonableOutput)
          if (
            switch reversed {
            | {optional: {hasUndefined: true}}
            | {tag: Undefined} => true
            | _ => false
            }
          ) {
            b->B.raise(~code=InvalidJsonSchema(reversed->fromInternal), ~path=Path.empty)
          }
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
  let typeFilter = (_b, ~inputVar) => `typeof ${inputVar}!=="boolean"`

  let schema = {
    tag: Boolean,
    builder: Builder.noop,
    typeFilter,
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

    let metadataId: Metadata.Id.t<array<t>> = Metadata.Id.make(
      ~namespace="rescript-schema",
      ~name="Int.refinements",
    )
  }

  let refinements = schema => {
    switch schema->Metadata.get(~id=Refinement.metadataId) {
    | Some(m) => m
    | None => []
    }
  }

  let refinement = (~inputVar) =>
    `${inputVar}>2147483647||${inputVar}<-2147483648||${inputVar}%1!==0`

  let typeFilter = (_b, ~inputVar) => `typeof ${inputVar}!=="number"||${refinement(~inputVar)}`

  let schema = {
    tag: Number,
    format: Int32,
    builder: Builder.noop,
    typeFilter,
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

    let metadataId: Metadata.Id.t<array<t>> = Metadata.Id.make(
      ~namespace="rescript-schema",
      ~name="Float.refinements",
    )
  }

  let refinements = schema => {
    switch schema->Metadata.get(~id=Refinement.metadataId) {
    | Some(m) => m
    | None => []
    }
  }

  let typeFilter = (_b, ~inputVar) =>
    `typeof ${inputVar}!=="number"` ++ if globalConfig.disableNanNumberValidation {
      ""
    } else {
      `||Number.isNaN(${inputVar})`
    }

  let schema = {
    tag: Number,
    builder: Builder.noop,
    typeFilter,
  }->toStandard
}

module BigInt = {
  let typeFilter = (_b, ~inputVar) => `typeof ${inputVar}!=="bigint"`

  let schema = {
    tag: BigInt,
    builder: Builder.invalidJson,
    typeFilter,
  }->toStandard
}

type preprocessDefinition<'input, 'output> = {
  @as("p")
  parser?: unknown => 'output,
  @as("a")
  asyncParser?: unknown => promise<'output>,
  @as("s")
  serializer?: unknown => 'input,
}
let rec preprocess = (schema, transformer) => {
  let schema = schema->toInternal
  let mut = schema->copy
  switch schema {
  | {anyOf} =>
    mut.anyOf = Some(
      anyOf->Js.Array2.map(unionSchema =>
        unionSchema->fromInternal->preprocess(transformer)->toInternal
      ),
    )
  | _ =>
    mut.builder = Builder.make((b, ~input, ~selfSchema, ~path) => {
      switch transformer(b->B.effectCtx(~selfSchema, ~path)) {
      | {parser, asyncParser: ?None} =>
        b->B.parseWithTypeValidation(
          ~schema,
          ~input=b->B.embedSyncOperation(~input, ~fn=parser),
          ~path,
        )
      | {parser: ?None, asyncParser} =>
        b->B.transform(~input=b->B.embedAsyncOperation(~input, ~fn=asyncParser), (b, ~input) => {
          b->B.parseWithTypeValidation(~schema, ~input, ~path)
        })
      | {parser: ?None, asyncParser: ?None} => b->B.parseWithTypeValidation(~schema, ~input, ~path)
      | {parser: _, asyncParser: _} =>
        b->B.invalidOperation(
          ~path,
          ~description=`The S.preprocess doesn't allow parser and asyncParser at the same time. Remove parser in favor of asyncParser`,
        )
      }
    })
    mut.typeFilter = None
    mut.output = Some(
      () => {
        let reversed = schema->reverse
        let mut = reversed->copy
        mut.builder = (b, ~input, ~selfSchema as _, ~path) => {
          let input = b->B.parse(~schema=reversed, ~input, ~path)
          switch transformer(b->B.effectCtx(~selfSchema=schema, ~path)) {
          | {serializer} => b->B.embedSyncOperation(~input, ~fn=serializer)
          | {serializer: ?None} => input
          }
        }
        mut
      },
    )
  }
  mut->toStandard
}

let list = schema => {
  schema
  ->Array.factory
  ->transform(_ => {
    parser: array => array->Belt.List.fromArray,
    serializer: list => list->Belt.List.toArray,
  })
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
let passingTypeFilter = (_b, ~inputVar as _) => "false"
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
  mut.typeFilter = switch schema->isLiteral {
  // Literal schema always expects to have a typeFilter
  | true => Some(passingTypeFilter)
  | false => None
  }
  mut->toStandard
}

let deprecationMetadataId: Metadata.Id.t<string> = Metadata.Id.make(
  ~namespace="rescript-schema",
  ~name="deprecation",
)

let deprecate = (schema, message) => {
  schema->Metadata.set(~id=deprecationMetadataId, message)
}

let deprecation = schema => schema->Metadata.get(~id=deprecationMetadataId)

let descriptionMetadataId: Metadata.Id.t<string> = Metadata.Id.make(
  ~namespace="rescript-schema",
  ~name="description",
)

let describe = (schema, description) => {
  schema->Metadata.set(~id=descriptionMetadataId, description)
}

let description = schema => schema->Metadata.get(~id=descriptionMetadataId)

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
              ->name}' schema`,
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

  let objectStrictModeCheck = (b, ~input, ~items, ~unknownKeys, ~path) => {
    if unknownKeys === Some(Strict) && b.global.flag->Flag.unsafeHas(Flag.typeValidation) {
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
    let unknownKeys = selfSchema.unknownKeys
    let items = selfSchema.items->Stdlib.Option.unsafeUnwrap
    let isArray = selfSchema.tag === Tuple

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
          schema.typeFilter->Stdlib.Option.isSome && (
              b.global.flag->Flag.unsafeHas(Flag.typeValidation)
                ? !(schema->isLiteral)
                : schema->isLiteral && !(itemInput->B.Val.isEmbed)
            )
        ) {
          b.code = b.code ++ b->B.typeFilterCode(~schema, ~input=itemInput, ~path)
        }

        objectVal->B.Val.Object.add(inlinedLocation, b->B.parse(~schema, ~input=itemInput, ~path))
      }

      b->objectStrictModeCheck(~input, ~items, ~unknownKeys, ~path)

      parentB.code = parentB.code ++ b->B.allocateScope

      if (
        (unknownKeys !== Some(Strip) || b.global.flag->Flag.unsafeHas(Flag.reverse)) &&
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
        unknownKeys: globalConfig.defaultUnknownKeys,
        typeFilter: Object.typeFilter,
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
      let unknownKeys = selfSchema.unknownKeys
      let items = selfSchema.items->Stdlib.Option.unsafeUnwrap

      let inputVar = b->B.Val.var(input)

      for idx in 0 to items->Js.Array2.length - 1 {
        let {schema, inlinedLocation} = items->Js.Array2.unsafe_get(idx)
        let schema = schema->toInternal

        let itemPath = inlinedLocation->Path.fromInlinedLocation

        let itemInput = b->B.val(`${inputVar}${itemPath}`)
        let path = path->Path.concat(itemPath)

        if (
          schema.typeFilter->Stdlib.Option.isSome && (
              b.global.flag->Flag.unsafeHas(Flag.typeValidation)
                ? !(schema->isLiteral)
                : schema->isLiteral
            )
        ) {
          b.code = b.code ++ b->B.typeFilterCode(~schema, ~input=itemInput, ~path)
        }

        outputs->Js.Dict.set(inlinedLocation, b->B.parse(~schema, ~input=itemInput, ~path))
      }

      b->objectStrictModeCheck(~input, ~items, ~unknownKeys, ~path)
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
    | Registred({reversed}) =>
      // Need to copy the schema here, because we're going to override the builder
      reversed->copy
    | _ => ritem->getRitemReversed
    }

    mut.builder = Builder.make((b, ~input, ~selfSchema, ~path) => {
      let hasTypeValidation = b.global.flag->Flag.unsafeHas(Flag.typeValidation)

      // TODO: Optimise the for loop
      for idx in 0 to ritems->Js.Array2.length - 1 {
        switch ritems->Js.Array2.unsafe_get(idx) {
        | Node(_) if hasTypeValidation =>
          b->B.invalidOperation(~path, ~description="Type validation mode is not supported")
        // typeFilters :=
        //   typeFilters.contents ++
        //   b->B.typeFilterCode(
        //     ~schema=reversed,
        //     ~typeFilter=isArray ? Array.typeFilter : typeFilter,
        //     ~input=b->B.val(`${inputVar}${rpath}`),
        //     ~path,
        //   )
        | Discriminant({reversed, path: rpath}) if !hasTypeValidation => {
            let itemInput = b->B.val(`${b->B.Val.var(input)}${rpath}`)
            let path = path->Path.concat(rpath)

            // Discriminant should always have a typeFilter, so don't check for it
            b.code = b.code ++ b->B.typeFilterCode(~schema=reversed, ~input=itemInput, ~path)
          }
        | _ => ()
        }
      }

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
          | {items, tag} => {
              let isArray = tag === Tuple
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
              reversed.typeFilter->Stdlib.Option.isSome && (
                hasTypeValidation ? !(reversed->isLiteral) : reversed->isLiteral
              )
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
          if selfSchema.unknownKeys === Some(Strict) {
            b->objectStrictModeCheck(
              ~input,
              ~items=selfSchema.items->Stdlib.Option.unsafeUnwrap,
              ~unknownKeys=Some(Strict),
              ~path,
            )
          }

          let isArray = (originalSchema: internal).tag === Tuple
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
          unknownKeys: globalConfig.defaultUnknownKeys,
          builder,
          typeFilter: Object.typeFilter,
          output,
        }->fromInternal

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
          // FIXME: Pattern match on the public schema?
          | {tag: Object, items: ?flattenedItems, ?advanced} => {
              if advanced->Stdlib.Option.unsafeUnwrap {
                InternalError.panic(
                  `Unsupported nested flatten for advanced object schema '${schema
                    ->fromInternal
                    ->name}'`,
                )
              }
              switch schema->reverse {
              | {tag: Object, ?advanced} if advanced === Some(false) =>
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
                    ->name}'`,
                )
              }
            }
          | _ =>
            InternalError.panic(`The '${schema->fromInternal->name}' schema can't be flattened`)
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
        | _ => InternalError.panic(`The '${schema->fromInternal->name}' schema can't be flattened`)
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
        unknownKeys: globalConfig.defaultUnknownKeys,
        advanced: true,
        typeFilter: Object.typeFilter,
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
        // FIXME: Throw when user passed a schema
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
      tag: Tuple,
      items,
      typeFilter: Tuple.typeFilter,
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
                tag: Tuple,
                items,
                typeFilter: Tuple.typeFilter,
                builder: Never.builder,
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
                unknownKeys: globalConfig.defaultUnknownKeys,
                advanced: true,
                typeFilter: Object.typeFilter,
                builder: Never.builder,
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
          tag: Tuple,
          items,
          builder,
          typeFilter: Tuple.typeFilter,
          output: ?(
            isTransformed.contents
              ? Some(
                  () => {
                    tag: Tuple,
                    items: reversedItems,
                    builder,
                    typeFilter: Tuple.typeFilter,
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
          unknownKeys: globalConfig.defaultUnknownKeys,
          builder,
          typeFilter: Object.typeFilter,
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

let enum = values =>
  Union.factory(values->Js.Array2.map(Literal.parse)->(Obj.magic: array<internal> => array<'a>))

let schema = Schema.factory

let js_schema = definition => definition->Obj.magic->Schema.definitionToSchema->toStandard
let literal = js_schema

let unnest = {
  let typeFilter = (b, ~inputVar) => {
    let items = (%raw(`this`): internal).items->Stdlib.Option.unsafeUnwrap
    let length = items->Js.Array2.length
    let code = ref(
      b->Array.typeFilter(~inputVar) ++
        `||${inputVar}.length!==${length->Stdlib.Int.unsafeToString}`,
    )
    for idx in 0 to length - 1 {
      let {schema, inlinedLocation} = items->Js.Array2.unsafe_get(idx)
      let schema = schema->toInternal
      code :=
        code.contents ++
        "||" ++
        b->(schema.typeFilter->Stdlib.Option.unsafeUnwrap)(
          ~inputVar=Path.concat(inputVar, Path.fromInlinedLocation(inlinedLocation)),
        )
    }
    code.contents
  }

  schema => {
    switch schema {
    | Object({items}) =>
      if items->Js.Array2.length === 0 {
        InternalError.panic("Invalid empty object for S.unnest schema.")
      }
      let schema = schema->toInternal
      {
        tag: Tuple,
        items: items->Js.Array2.mapi((item, idx) => {
          let location = idx->Js.Int.toString
          {
            schema: Array.factory(item.schema),
            inlinedLocation: `"${location}"`,
            location,
          }
        }),
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
        typeFilter,
        output: () => {
          let schema = schema->reverse
          {
            tag: Array,
            item: schema,
            typeFilter: Array.typeFilter,
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
      }->toStandard
    | _ => InternalError.panic("S.unnest supports only object schemas.")
    }
  }
}

let inline = Obj.magic
// let inline = {
//   let rec internalInline = (schema, ~variant as maybeVariant=?, ()) => {
//     let schema = schema->toInternal->copy

//     let inlinedSchema = switch schema->classify {
//     | Literal(literal) => `S.literal(%raw(\`${literal->Literal.toString}\`))`
//     | Union(unionSchemas) => {
//         let variantNamesCounter = Js.Dict.empty()
//         `S.union([${unionSchemas
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
//     | JSON({validated}) => `S.json(~validate=${validated->(Obj.magic: bool => string)})`
//     | Tuple({items: [s0]}) => `S.tuple1(${s0.schema->internalInline()})`
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
//     | Object({unknownKeys: Strict}) => inlinedSchema ++ `->S.strict`
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
let null = Obj.magic
// let null = Null.factory
let option = Option.factory
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

let datetime = (schema, ~message=`Invalid datetime string! Must be UTC`) => {
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

let rec coerce = (from, to) => {
  let from = from->toInternal
  let to = to->toInternal

  // It makes sense, since S.coerce quite often will be used
  // inside of a framework where we don't control what's the to argument
  if from === to {
    from->fromInternal
  } else {
    let extendCoercion = %raw(`0`)
    let shrinkCoercion = %raw(`1`)

    let toConst = to.const
    // Related https://github.com/rescript-lang/rescript/issues/7313
    let hasToConst = to.const !== %raw(`void 0`) || to.tag === Undefined

    let coercion = switch (from->reverse, to) {
    | ({const: _} | {tag: Undefined}, _) if hasToConst =>
      (b, ~inputVar as _, ~failCoercion as _) => {
        b->B.embedVal(toConst)
        // FIXME: Test
        // FIXME: Inline when possible
      }
    | ({tag: String}, {tag: String, const: _}) => shrinkCoercion
    | ({tag: String}, {tag: String}) // FIXME: validate that refinements match
    | ({tag: Number, format: Int32}, {tag: Number, format: ?None}) => extendCoercion
    | ({tag: Boolean | Number | BigInt | Undefined | Null | NaN, const}, {tag: String}) =>
      (b, ~inputVar as _, ~failCoercion as _) => b->B.val(const->Obj.magic)

    | ({tag: Boolean | Number | BigInt}, {tag: String}) =>
      // FIXME: Test literals
      (b, ~inputVar, ~failCoercion as _) => b->B.val(`""+${inputVar}`) // TODO: This looks like the fastest option. Benchmark vs `${inputVar}?"true":"false"` vs `"{value}"` vs .toString and store the results somewhere
    | ({tag: String}, {tag: Boolean | Number | BigInt | Undefined | Null | NaN, const}) =>
      (b, ~inputVar, ~failCoercion) => {
        b.code = b.code ++ `${inputVar}==="${const->Obj.magic}"||${failCoercion};`
        b->B.val(to->Literal.unsafeInline)
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
        b.code =
          b.code ++
          `Number.isNaN(${output.var(b)})${{
              switch format {
              | None => ``
              | Some(Int32) => `||${Int.refinement(~inputVar)}`
              // | Some(format) =>
              //   Js.Exn.raiseError(`Unsupported coercion to number format ${(format :> string)}`)
              }
            }}&&${failCoercion};`
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
        `S.coerce from ${from->reverse->fromInternal->name} to ${to
          ->fromInternal
          ->name} is not supported`,
      )
    }

    let mut = from->copy
    mut.builder = Builder.make((b, ~input, ~selfSchema as _, ~path) => {
      let input = b->B.parse(~schema=from, ~input, ~path)

      if coercion === extendCoercion {
        b->B.parse(~schema=to, ~input, ~path)
      } else if coercion === shrinkCoercion {
        b->B.parseWithTypeValidation(~schema=to, ~input, ~path)
      } else {
        let bb = b->B.scope
        let inputVar = input.var(bb)
        let output = bb->B.parse(
          ~schema=to,
          ~input=bb->coercion(
            ~inputVar,
            ~failCoercion=bb->B.failWithArg(
              ~path,
              input => InvalidType({
                expected: to->fromInternal,
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
        coerce(to->reverse->fromInternal, from->reverse->fromInternal)->toInternal
      },
    )

    mut->mixingMetadata(~from=to)

    mut->toStandard
  }
}

let nullish = schema => {
  Union.factory([schema->toUnknown, Literal.null->fromInternal, unit->toUnknown])
}

let nullable = schema => {
  Union.factory([schema->toUnknown, coerce(Literal.null->fromInternal, unit->toUnknown)])
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
  let schema = option(schema)
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
  | (Object({items: items1, fields: fields1}), Object({items: items2, unknownKeys})) =>
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
      unknownKeys,
      advanced: true,
      builder: Builder.make((b, ~input, ~selfSchema as _, ~path) => {
        let s1Result = b->B.parse(~schema=s1, ~input, ~path)
        let s2Result = b->B.parse(~schema=s2, ~input, ~path)
        // TODO: Check that these are objects
        b->B.val(`{...${s1Result.inline}, ...${s2Result.inline}}`)
      }),
      typeFilter: Object.typeFilter,
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

let setGlobalConfig = override => {
  globalConfig.recCounter = 0
  globalConfig.defaultUnknownKeys = switch override.defaultUnknownKeys {
  | Some(unknownKeys) => unknownKeys
  | None => initialDefaultUnknownKeys
  }
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
