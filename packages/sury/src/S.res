@@uncurried
@@warning("-30")

type never

module Path = {
  // Standard Schema's `PropertyKey` minus `symbol`: an untagged variant can't
  // carry a symbol case, so one written into a refine's `path` from JS reads
  // as neither. Codegen itself never emits one.
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
  | @as("cuid2") Cuid2
  | @as("ulid") Ulid
  | @as("ksuid") Ksuid
  | @as("xid") Xid
  | @as("nanoid") Nanoid
  | @as("uuidv4") Uuidv4
  | @as("uuidv6") Uuidv6
  | @as("uuidv7") Uuidv7
  | @as("e164") E164
  | @as("mac") Mac
  | @as("hex") Hex
  | @as("cidrv4") Cidrv4
  | @as("cidrv6") Cidrv6
  | @as("http-url") HttpUrl
  | @as("env") Env
  | @as("queryString") QueryString
type arrayFormat = | @as("compactColumns") CompactColumns

// A closed polymorphic variant: the constructors are the wire type names, so
// `#sint32` needs no `@as` to reach JS and does not collide with the `String`
// and `Int32` that `tag` and `numberFormat` already spell.
type protobufType = [
  | #double
  | #float
  | #int32
  | #int64
  | #uint32
  | #uint64
  | #sint32
  | #sint64
  | #fixed32
  | #fixed64
  | #sfixed32
  | #sfixed64
  | #bool
  | #string
  | #bytes
  | #enum
  | #message
]

// A Google well-known type, printed as an import. See `ProtobufWellKnownType`
// in index.d.ts for the value each takes.
type protobufWellKnownType = [
  | #"google.protobuf.Any"
  | #"google.protobuf.Timestamp"
  | #"google.protobuf.Duration"
  | #"google.protobuf.Value"
  | #"google.protobuf.Struct"
  | #"google.protobuf.ListValue"
  | #"google.protobuf.FieldMask"
  | #"google.protobuf.Empty"
  | #"google.protobuf.DoubleValue"
  | #"google.protobuf.FloatValue"
  | #"google.protobuf.Int64Value"
  | #"google.protobuf.UInt64Value"
  | #"google.protobuf.Int32Value"
  | #"google.protobuf.UInt32Value"
  | #"google.protobuf.BoolValue"
  | #"google.protobuf.StringValue"
  | #"google.protobuf.BytesValue"
]

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
  | @as("unrecognized_key") UnrecognizedKey({path: Path.t, reason: string, key: string})

type exn += private Exn(error)

// =============================================================================
// Bindings to the TypeScript core
// =============================================================================
//
// This module is the ReScript face of Sury: the public types above, plus the
// `@module("sury") external` bindings below, resolved through the package root
// "." conditional export (import -> the ESM entry, require -> the CJS one).
// That's what makes them work whichever module format you compile to - a plain
// relative `@module("./index.mjs")` would break under a "commonjs"
// package-spec (require()-ing an ESM file throws).

external castToUnknown: t<'any> => t<unknown> = "%identity"
external castToAny: t<'value> => t<'any> = "%identity"
external untag: t<'any> => untagged = "%identity"

// ReScript's `catch { | Exn(e) => }` compiles to a `RE_EXN_ID === Exn`
// identity test against the constructor id synthesized right here by the
// `type exn +=` declaration above. The runtime that throws needs the same
// identity, so hand it over once at module load - SuryError's RE_EXN_ID getter
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

// Primitive schema values - the very instances the JS entry exports, so both
// surfaces share one object per primitive. Some (string, bool, ...) shadow
// stdlib names on purpose.
@module("sury") external never: t<never> = "never"
@module("sury") external unknown: t<unknown> = "unknown"
@module("sury") external any: t<'any> = "any"
@module("sury") external unit: t<unit> = "$unit"
@module("sury") external nullAsUnit: t<unit> = "$nullAsUnit"
@module("sury") external string: t<string> = "string"
@module("sury") external bool: t<bool> = "boolean"
@module("sury") external int: t<int> = "int32"
// Every format schema carries its own `@unboxed` type, so a format survives in
// the type system instead of collapsing back into `string`/`float`. Unboxed
// means the constructor is erased at runtime - the value is the payload, and
// the JS side never sees the difference.
// `float`, not `int`: ReScript's `int` is int32, and a JS integer (JSON
// Schema's unbounded `integer`) can exceed that range.
@unboxed type integer = Integer(float)
@module("sury") external integer: t<integer> = "integer"
@module("sury") external float: t<float> = "number"
@module("sury") external bigint: t<bigint> = "bigint"
@module("sury") external symbol: t<Symbol.t> = "symbol"
@module("sury") external nan: t<float> = "nan"
/** The stdlib `Date.t`, aliased so every schema's value type is reachable as
    `S.<name>` - including from the ppx, which resolves those names. */
type date = Date.t
@module("sury") external date: t<date> = "date"
type json = JSON.t
@module("sury") external json: t<json> = "json"
@unboxed type jsonString = JsonString(string)
@module("sury") external jsonString: t<jsonString> = "jsonString"
@module("sury") external jsonStringWithSpace: int => t<jsonString> = "jsonStringWithSpace"
@module("sury") external uint8Array: t<Uint8Array.t> = "uint8Array"
@module("sury") external arrayBuffer: t<ArrayBuffer.t> = "arrayBuffer"
/** The Protocol Buffers binary wire format. */
@module("sury") external protobuf: t<Uint8Array.t> = "protobuf"

type protobufAny = {typeUrl: string, value: Uint8Array.t}
/** A `google.protobuf.Any`. Convert it with `S.to` to a message `S.protobufTypeName` names, or to an `S.union` of them. */
@module("sury") external protobufAny: t<protobufAny> = "protobufAny"
type protoOptions = {name?: string, package?: string}
@module("sury") external toProtoOrThrow_: (t<'value>, protoOptions) => string = "toProtoOrThrow"
let toProtoOrThrow = (schema, ~name=?, ~package=?) => toProtoOrThrow_(schema, {?name, ?package})
// `Js.Blob.t`/`Js.File.t` rather than a pair of abstract types declared here:
// the stdlib has no Blob or File module, and these two are the compiler's own
// builtin abstract types - the ones untagged variants match on - so a value
// from any other binding unifies with these.
type blob = Js.Blob.t
@module("sury") external blob: t<blob> = "blob"
type file = Js.File.t
@module("sury") external file: t<file> = "file"
// The stdlib has no FormData module, so the type is declared here, abstract:
// a value from `%raw`, a fetch binding or a form event unifies with it only
// through a cast, the way an external's own abstract types do.
type formData
@module("sury") external formData: t<formData> = "formData"
// Unboxed so `S.dict(S.env)` is not `dict<string>`: the codec, not `S.string`,
// is what reads `"42"` as a number.
@unboxed type env = Env(string)
@module("sury") external env: t<env> = "env"
type urlSearchParams
@module("sury") external urlSearchParams: t<urlSearchParams> = "urlSearchParams"
@unboxed type queryString = QueryString(string)
@module("sury") external queryString: t<queryString> = "queryString"
@unboxed type isoDateTime = IsoDateTime(string)
@module("sury") external isoDateTime: t<isoDateTime> = "isoDateTime"
@unboxed type utcDateTime = UtcDateTime(string)
@module("sury") external utcDateTime: t<utcDateTime> = "utcDateTime"
@unboxed type port = Port(int)
@module("sury") external port: t<port> = "port"
@unboxed type email = Email(string)
@module("sury") external email: t<email> = "email"
@unboxed type uuid = Uuid(string)
@module("sury") external uuid: t<uuid> = "uuid"
@unboxed type uuidv4 = Uuidv4(string)
@module("sury") external uuidv4: t<uuidv4> = "uuidv4"
@unboxed type uuidv6 = Uuidv6(string)
@module("sury") external uuidv6: t<uuidv6> = "uuidv6"
@unboxed type uuidv7 = Uuidv7(string)
@module("sury") external uuidv7: t<uuidv7> = "uuidv7"
@unboxed type cuid = Cuid(string)
@module("sury") external cuid: t<cuid> = "cuid"
@unboxed type cuid2 = Cuid2(string)
@module("sury") external cuid2: t<cuid2> = "cuid2"
@unboxed type ulid = Ulid(string)
@module("sury") external ulid: t<ulid> = "ulid"
@unboxed type ksuid = Ksuid(string)
@module("sury") external ksuid: t<ksuid> = "ksuid"
@unboxed type xid = Xid(string)
@module("sury") external xid: t<xid> = "xid"
@unboxed type nanoid = Nanoid(string)
@module("sury") external nanoid: t<nanoid> = "nanoid"
@unboxed type e164 = E164(string)
@module("sury") external e164: t<e164> = "e164"
@unboxed type mac = Mac(string)
@module("sury") external mac: t<mac> = "mac"
@unboxed type hex = Hex(string)
@module("sury") external hex: t<hex> = "hex"
@unboxed type base64 = Base64(string)
@module("sury") external base64: t<base64> = "base64"
@unboxed type base64url = Base64url(string)
@module("sury") external base64url: t<base64url> = "base64url"
@unboxed type uri = Uri(string)
@module("sury") external uri: t<uri> = "uri"
@unboxed type httpUrl = HttpUrl(string)
@module("sury") external httpUrl: t<httpUrl> = "httpUrl"
/** An instance of the JS `URL` class. ReScript has no stdlib binding for it,
    so this is an abstract type standing for one. */
type url
@module("sury") external url: t<url> = "url"
@unboxed type isoDate = IsoDate(string)
@module("sury") external isoDate: t<isoDate> = "isoDate"
@unboxed type isoTime = IsoTime(string)
@module("sury") external isoTime: t<isoTime> = "isoTime"
@unboxed type duration = Duration(string)
@module("sury") external duration: t<duration> = "duration"
@unboxed type hostname = Hostname(string)
@module("sury") external hostname: t<hostname> = "hostname"
@unboxed type idnHostname = IdnHostname(string)
@module("sury") external idnHostname: t<idnHostname> = "idnHostname"
@unboxed type ipv4 = Ipv4(string)
@module("sury") external ipv4: t<ipv4> = "ipv4"
@unboxed type ipv6 = Ipv6(string)
@module("sury") external ipv6: t<ipv6> = "ipv6"
@unboxed type cidrv4 = Cidrv4(string)
@module("sury") external cidrv4: t<cidrv4> = "cidrv4"
@unboxed type cidrv6 = Cidrv6(string)
@module("sury") external cidrv6: t<cidrv6> = "cidrv6"
@unboxed type uriReference = UriReference(string)
@module("sury") external uriReference: t<uriReference> = "uriReference"
@unboxed type uriTemplate = UriTemplate(string)
@module("sury") external uriTemplate: t<uriTemplate> = "uriTemplate"
@unboxed type iri = Iri(string)
@module("sury") external iri: t<iri> = "iri"
@unboxed type iriReference = IriReference(string)
@module("sury") external iriReference: t<iriReference> = "iriReference"
@unboxed type idnEmail = IdnEmail(string)
@module("sury") external idnEmail: t<idnEmail> = "idnEmail"
@unboxed type jsonPointer = JsonPointer(string)
@module("sury") external jsonPointer: t<jsonPointer> = "jsonPointer"
@unboxed type relativeJsonPointer = RelativeJsonPointer(string)
@module("sury") external relativeJsonPointer: t<relativeJsonPointer> = "relativeJsonPointer"

@module("sury") external literal: 'value => t<'value> = "literal"
@module("sury") external array: t<'value> => t<array<'value>> = "array"
@module("sury") external compactColumns: t<'value> => t<array<array<'value>>> = "compactColumns"
@module("sury") external list: t<'value> => t<list<'value>> = "list"
@module("sury") external instance: unknown => t<unknown> = "instance"
@module("sury") external dict: t<'value> => t<dict<'value>> = "record"
@module("sury") external option: t<'value> => t<option<'value>> = "$option"
// The public JS `nullable` called without a default is exactly
// `union([item, literal(null)])` - what ReScript calls `S.null`.
@module("sury") external null: t<'value> => t<null<'value>> = "nullable"
@module("sury") external nullAsOption: t<'value> => t<option<'value>> = "$nullAsOption"
@module("sury") external nullable: t<'value> => t<nullable<'value>> = "nullish"
@module("sury") external nullableAsOption: t<'value> => t<option<'value>> = "$nullableAsOption"
@module("sury") external union: array<t<'value>> => t<'value> = "union"
@module("sury") external anyOf: array<t<'value>> => t<'value> = "anyOf"
@module("sury") external enum: array<'value> => t<'value> = "enum"

@module("sury") external meta: (t<'value>, meta<'value>) => t<'value> = "meta"

// Every option the JS `S.protobufField` takes, all optional but the number:
// leaving `type_` off is what asks for the wire type the schema implies, and
// a binding that demanded it would put a ReScript caller in the business of
// restating `S.string` as `String`. `key` is the K of a `map<K, V>`, for an
// `S.dict` field.
type protobufFieldOptions = {
  number: int,
  @as("type") type_?: [protobufType | protobufWellKnownType],
  packed?: bool,
  key?: protobufType,
  oneof?: string,
}
@module("sury")
external protobufField_: (t<'value>, protobufFieldOptions) => t<'value> = "protobufField"
let protobufField = (schema, number, ~type_=?, ~packed=?, ~key=?, ~oneof=?) =>
  protobufField_(schema, {number, ?type_, ?packed, ?key, ?oneof})
@module("sury") external protobufTypeName: (t<'value>, string) => t<'value> = "protobufTypeName"

// The public JS `refine` takes an options object; build it here from the
// ReScript labeled args.
type refineOptions = {error?: string, path?: Path.t}
@module("sury")
external refine: (t<'value>, 'value => bool, refineOptions) => t<'value> = "refine"
let refine = (schema, refiner, ~error=?, ~path=?) => refine(schema, refiner, {?error, ?path})

@module("sury") external shape: (t<'value>, 'value => 'shape) => t<'shape> = "shape"

type conversion<'i, 'o> =
  | @as("auto") Auto
  | @as("never") Never
  // The two readings of a content link (CONTENT_CODEC_SPEC.md rule 1). They
  // carry no payload, so they erase to their strings the way Auto/Never do -
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
    codecs<'from, 'to>,
  ) => t<'to> = "$to"
)
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
        decode: unwrapConversion(decode),
        encode: unwrapConversion(encode),
      },
    )
  }

@module("sury") external noValidation: (t<'value>, bool) => t<'value> = "noValidation"

@module("sury") external reverse: t<'value> => t<unknown> = "reverse"

%%private(
  // Every operation has two JS call shapes - `op(schemas)` compiles, `op(data,
  // schemas)` runs - and ReScript has no overloads, so each is its own
  // external over the same import. Binding both beats applying the compiled
  // one: `compileX(~to)(any)` would ship a wrapper in S.res.mjs and pay a
  // second call for what the JS dispatch already does in one.
  //
  // The ReScript convert runs FROM a schema's Output space, which is exactly
  // what the JS `encode*` operations compile: they reverse the first schema
  // only and run the rest of the chain forward. Arity-specific bindings, since
  // a labeled optional `~via` can't spread into a rest param - and an absent
  // `~via=?` would otherwise compile to a literal `undefined` in the middle of
  // the argument list, which the JS dispatch rejects as a hole.
  @module("sury") external compileConvert2: (t<'from>, t<'to>) => 'from => 'to = "encodeOrThrow"
  @module("sury")
  external compileConvert3: (t<'from>, t<unknown>, t<'to>) => 'from => 'to = "encodeOrThrow"
  @module("sury") external convert2: ('from, t<'from>, t<'to>) => 'to = "encodeOrThrow"
  @module("sury") external convert3: ('from, t<'from>, t<unknown>, t<'to>) => 'to = "encodeOrThrow"
  @module("sury")
  external compileConvertAsync2: (t<'from>, t<'to>) => 'from => promise<'to> =
    "encodeAsPromiseOrReject"
  @module("sury")
  external compileConvertAsync3: (t<'from>, t<unknown>, t<'to>) => 'from => promise<'to> =
    "encodeAsPromiseOrReject"
  @module("sury")
  external convertAsync2: ('from, t<'from>, t<'to>) => promise<'to> = "encodeAsPromiseOrReject"
  @module("sury")
  external convertAsync3: ('from, t<'from>, t<unknown>, t<'to>) => promise<'to> =
    "encodeAsPromiseOrReject"
  @module("sury")
  external compileConvertResult2: (t<'from>, t<'to>) => 'from => result<'to, error> =
    "$encodeAsResult"
  @module("sury")
  external compileConvertResult3: (t<'from>, t<unknown>, t<'to>) => 'from => result<'to, error> =
    "$encodeAsResult"
  @module("sury")
  external convertResult2: ('from, t<'from>, t<'to>) => result<'to, error> = "$encodeAsResult"
  @module("sury")
  external convertResult3: ('from, t<'from>, t<unknown>, t<'to>) => result<'to, error> =
    "$encodeAsResult"
  @module("sury")
  external compileConvertResultPromise2: (t<'from>, t<'to>) => 'from => promise<result<'to, error>> =
    "$encodeAsResultPromise"
  @module("sury")
  external compileConvertResultPromise3: (
    t<'from>,
    t<unknown>,
    t<'to>,
  ) => 'from => promise<result<'to, error>> = "$encodeAsResultPromise"
  @module("sury")
  external convertResultPromise2: ('from, t<'from>, t<'to>) => promise<result<'to, error>> =
    "$encodeAsResultPromise"
  @module("sury")
  external convertResultPromise3: (
    'from,
    t<'from>,
    t<unknown>,
    t<'to>,
  ) => promise<result<'to, error>> = "$encodeAsResultPromise"
  @module("sury")
  external compileConvertStandard2: (t<'from>, t<'to>) => 'from => StandardSchema.Result.t<'to> =
    "encodeAsResult"
  @module("sury")
  external compileConvertStandard3: (
    t<'from>,
    t<unknown>,
    t<'to>,
  ) => 'from => StandardSchema.Result.t<'to> = "encodeAsResult"
  @module("sury")
  external convertStandard2: ('from, t<'from>, t<'to>) => StandardSchema.Result.t<'to> =
    "encodeAsResult"
  @module("sury")
  external convertStandard3: ('from, t<'from>, t<unknown>, t<'to>) => StandardSchema.Result.t<'to> =
    "encodeAsResult"
  @module("sury")
  external compileConvertStandardPromise2: (
    t<'from>,
    t<'to>,
  ) => 'from => promise<StandardSchema.Result.t<'to>> = "encodeAsResultPromise"
  @module("sury")
  external compileConvertStandardPromise3: (
    t<'from>,
    t<unknown>,
    t<'to>,
  ) => 'from => promise<StandardSchema.Result.t<'to>> = "encodeAsResultPromise"
  @module("sury")
  external convertStandardPromise2: (
    'from,
    t<'from>,
    t<'to>,
  ) => promise<StandardSchema.Result.t<'to>> = "encodeAsResultPromise"
  @module("sury")
  external convertStandardPromise3: (
    'from,
    t<'from>,
    t<unknown>,
    t<'to>,
  ) => promise<StandardSchema.Result.t<'to>> = "encodeAsResultPromise"
)

// The compiled (data-last) forms.
@module("sury") external compileParseOrThrow: (~to: t<'value>) => 'any => 'value = "parseOrThrow"
@module("sury")
external compileParseAsPromiseOrReject: (~to: t<'value>) => 'any => promise<'value> =
  "parseAsPromiseOrReject"
// The `$`-prefixed bindings are the ReScript result shape (`{TAG, _0}`), which
// the JS surface has no equivalent for: one compiler, a second tail.
@module("sury")
external compileParseAsResult: (~to: t<'value>) => 'any => result<'value, error> = "$parseAsResult"
@module("sury")
external compileParseAsResultPromise: (~to: t<'value>) => 'any => promise<result<'value, error>> =
  "$parseAsResultPromise"
// The JS Result is a Standard Schema result, so the standard outcomes are the
// JS operations themselves: every issue, not only the first.
@module("sury")
external compileParseAsStandardResult: (~to: t<'value>) => 'any => StandardSchema.Result.t<'value> =
  "parseAsResult"
@module("sury")
external compileParseAsStandardResultPromise: (
  ~to: t<'value>,
) => 'any => promise<StandardSchema.Result.t<'value>> = "parseAsResultPromise"

// One dispatch on `~via` for the four convert outcomes, over the arity-specific
// externals above.
//
// `@inline` is load-bearing, not a hint: an external is not a value, so passing
// one as an argument eta-expands it into a closure at every call site - each
// convert would allocate two of them per call, to reach a function it could
// have called directly. Inlined, both helpers disappear from S.res.mjs and each
// operation compiles to the branch it would have been written as.
%%private(
  @inline
  let withVia = (~from, ~via, ~to, two, three) =>
    switch via {
    | None => two(from, to)
    | Some(via) => three(from, castToUnknown(via), to)
    }
  @inline
  let withViaData = (any, ~from, ~via, ~to, two, three) =>
    switch via {
    | None => two(any, from, to)
    | Some(via) => three(any, from, castToUnknown(via), to)
    }
)
let compileConvertOrThrow = (~from, ~via=?, ~to) =>
  withVia(~from, ~via, ~to, compileConvert2, compileConvert3)
let compileConvertAsPromiseOrReject = (~from, ~via=?, ~to) =>
  withVia(~from, ~via, ~to, compileConvertAsync2, compileConvertAsync3)
let compileConvertAsResult = (~from, ~via=?, ~to) =>
  withVia(~from, ~via, ~to, compileConvertResult2, compileConvertResult3)
let compileConvertAsResultPromise = (~from, ~via=?, ~to) =>
  withVia(~from, ~via, ~to, compileConvertResultPromise2, compileConvertResultPromise3)
let compileConvertAsStandardResult = (~from, ~via=?, ~to) =>
  withVia(~from, ~via, ~to, compileConvertStandard2, compileConvertStandard3)
let compileConvertAsStandardResultPromise = (~from, ~via=?, ~to) =>
  withVia(~from, ~via, ~to, compileConvertStandardPromise2, compileConvertStandardPromise3)

// `assert` is a ReScript keyword, so the boolean-answering check keeps the JS
// name: `isInput` asks of the wire side, `isOutput` of the value side.
//
// These take `~schema`, not `~to`: nothing is converted into it. The value is
// checked AGAINST the schema and either handed back as it stands (`make*`) or
// answered about (`is*`, `assert*`).
@module("sury") external compileIsInput: (~schema: t<'value>) => 'any => bool = "isInput"
@module("sury") external compileIsOutput: (~schema: t<'value>) => 'any => bool = "isOutput"

// `t<'value>` names the OUTPUT type, so the output-side make is THE make here;
// the input side has no type to hand back. Same reason `convert*` is the name
// for what JS spells `encode*`.
@module("sury")
external compileMakeOrThrow: (~schema: t<'value>) => 'value => 'value = "makeOutputOrThrow"
@module("sury")
external compileMakeAsPromiseOrReject: (~schema: t<'value>) => 'value => promise<'value> =
  "makeOutputAsPromiseOrReject"
@module("sury")
external compileMakeAsResult: (~schema: t<'value>) => 'value => result<'value, error> =
  "$makeAsResult"
@module("sury")
external compileMakeAsResultPromise: (
  ~schema: t<'value>,
) => 'value => promise<result<'value, error>> = "$makeAsResultPromise"
@module("sury")
external compileMakeAsStandardResult: (
  ~schema: t<'value>,
) => 'value => StandardSchema.Result.t<'value> = "makeOutputAsResult"
@module("sury")
external compileMakeAsStandardResultPromise: (
  ~schema: t<'value>,
) => 'value => promise<StandardSchema.Result.t<'value>> = "makeOutputAsResultPromise"

// `t<'value>` names the output type, so the output side is THE equality here;
// the input side has no type to compare against. Compares two values that
// already have the type - it never validates.
@module("sury")
external compileIsEqual: (~schema: t<'value>) => ('value, 'value) => bool = "isEqualOutput"

// Same walk as `isEqual`, answering -1 | 0 | 1. 0 is exactly when `isEqual`
// would be true. `int` is the ReScript spelling of that three-value contract.
// Raises for a schema with no order: primitives, Date, URL and tuples of them
// are what can be ordered, everything else is `isEqual` only.
@module("sury")
external compileCompare: (~schema: t<'value>) => ('value, 'value) => int = "compareOutput"

// The immediate (data-first) forms. The JS dispatch reads a leading non-schema
// argument as the value, so these are the same imports at their other shape.
@module("sury") external parseOrThrow: ('any, ~to: t<'value>) => 'value = "parseOrThrow"
@module("sury")
external parseAsPromiseOrReject: ('any, ~to: t<'value>) => promise<'value> =
  "parseAsPromiseOrReject"
@module("sury")
external parseAsResult: ('any, ~to: t<'value>) => result<'value, error> = "$parseAsResult"
@module("sury")
external parseAsResultPromise: ('any, ~to: t<'value>) => promise<result<'value, error>> =
  "$parseAsResultPromise"
@module("sury")
external parseAsStandardResult: ('any, ~to: t<'value>) => StandardSchema.Result.t<'value> =
  "parseAsResult"
@module("sury")
external parseAsStandardResultPromise: (
  'any,
  ~to: t<'value>,
) => promise<StandardSchema.Result.t<'value>> = "parseAsResultPromise"

@module("sury")
external assertInputOrThrow: ('any, ~schema: t<'value>) => unit = "assertInputOrThrow"
@module("sury")
external assertInputAsPromiseOrReject: ('any, ~schema: t<'value>) => promise<unit> =
  "assertInputAsPromiseOrReject"
@module("sury")
external assertOutputOrThrow: ('any, ~schema: t<'value>) => unit = "assertOutputOrThrow"
@module("sury")
external assertOutputAsPromiseOrReject: ('any, ~schema: t<'value>) => promise<unit> =
  "assertOutputAsPromiseOrReject"

@module("sury") external isInput: ('any, ~schema: t<'value>) => bool = "isInput"
@module("sury") external isOutput: ('any, ~schema: t<'value>) => bool = "isOutput"

@module("sury") external isEqual: ('value, 'value, ~schema: t<'value>) => bool = "isEqualOutput"
@module("sury") external compare: ('value, 'value, ~schema: t<'value>) => int = "compareOutput"

@module("sury") external makeOrThrow: ('value, ~schema: t<'value>) => 'value = "makeOutputOrThrow"
@module("sury")
external makeAsPromiseOrReject: ('value, ~schema: t<'value>) => promise<'value> =
  "makeOutputAsPromiseOrReject"
@module("sury")
external makeAsResult: ('value, ~schema: t<'value>) => result<'value, error> = "$makeAsResult"
@module("sury")
external makeAsResultPromise: ('value, ~schema: t<'value>) => promise<result<'value, error>> =
  "$makeAsResultPromise"
@module("sury")
external makeAsStandardResult: ('value, ~schema: t<'value>) => StandardSchema.Result.t<'value> =
  "makeOutputAsResult"
@module("sury")
external makeAsStandardResultPromise: (
  'value,
  ~schema: t<'value>,
) => promise<StandardSchema.Result.t<'value>> = "makeOutputAsResultPromise"

let convertOrThrow = (any, ~from, ~via=?, ~to) =>
  withViaData(any, ~from, ~via, ~to, convert2, convert3)
let convertAsPromiseOrReject = (any, ~from, ~via=?, ~to) =>
  withViaData(any, ~from, ~via, ~to, convertAsync2, convertAsync3)
let convertAsResult = (any, ~from, ~via=?, ~to) =>
  withViaData(any, ~from, ~via, ~to, convertResult2, convertResult3)
let convertAsResultPromise = (any, ~from, ~via=?, ~to) =>
  withViaData(any, ~from, ~via, ~to, convertResultPromise2, convertResultPromise3)
let convertAsStandardResult = (any, ~from, ~via=?, ~to) =>
  withViaData(any, ~from, ~via, ~to, convertStandard2, convertStandard3)
let convertAsStandardResultPromise = (any, ~from, ~via=?, ~to) =>
  withViaData(any, ~from, ~via, ~to, convertStandardPromise2, convertStandardPromise3)

@module("sury") external recursive: (string, t<'value> => t<'value>) => t<'value> = "recursive"

@module("sury") external toInputExpression: t<'value> => string = "toInputExpression"

@module("sury") external toOutputExpression: t<'value> => string = "toOutputExpression"

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
// float and bigint. It admits nonsense the JS side has to catch - a bound on a
// `t<string>`, say - which is why gt/gte/lt/lte validate both the schema tag
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
@unboxed type nonEmpty<'value> = NonEmpty('value)
@module("sury")
external nonEmpty: (t<'value>, ~message: string=?) => t<nonEmpty<'value>> = "nonEmpty"

@module("sury") external minSize: (t<'value>, int, ~message: string=?) => t<'value> = "minSize"
@module("sury") external maxSize: (t<'value>, int, ~message: string=?) => t<'value> = "maxSize"
@module("sury") external size: (t<'value>, int, ~message: string=?) => t<'value> = "size"

// `t<'value>` rather than `t<string>` so the format types and `nonEmpty` stay
// chainable; like gt/gte above, this admits nonsense the JS side has to catch.
@module("sury")
external pattern: (t<'value>, RegExp.t, ~message: string=?) => t<'value> = "pattern"
@module("sury") external trim: t<'value> => t<'value> = "trim"

type jsonSchemaOptions = {target?: StandardSchema.JsonSchema.target}
@module("sury")
external toInputJSONSchemaOrThrow: (t<'value>, ~options: jsonSchemaOptions=?) => JSONSchema.t =
  "toInputJSONSchemaOrThrow"
@module("sury")
external toOutputJSONSchemaOrThrow: (t<'value>, ~options: jsonSchemaOptions=?) => JSONSchema.t =
  "toOutputJSONSchemaOrThrow"
@module("sury")
external fromJSONSchemaDefinitionOrThrow: JSONSchema.definition => t<JSON.t> =
  "fromJSONSchemaOrThrow"
let fromJSONSchemaOrThrow = jsonSchema =>
  fromJSONSchemaDefinitionOrThrow(JSONSchema.Schema(jsonSchema))
@module("sury")
external extendJSONSchema: (t<'value>, JSONSchema.t) => t<'value> = "extendJSONSchema"
// Enables `~standard.jsonSchema`; its input/output throw before this is called.
@module("sury") external enableStandardJSONSchema: unit => unit = "enableStandardJSONSchema"

type globalConfigOverride = {
  defaultAdditionalItems?: additionalItemsMode,
  disableNanNumberValidation?: bool,
}

@module("sury") external global: globalConfigOverride => unit = "global"

// @generated by protoc-gen-sury: the well-known types, bound to sury/protobuf/wkt.
/** The `google.protobuf` well-known types, each a module holding its type `t` and its `schema`. */
module Protobuf = {
  // google/protobuf/any.proto

  /**
   `Any` contains an arbitrary serialized protocol buffer message along with a
   URL that describes the type of the serialized message.

   In its binary encoding, an `Any` is an ordinary message; but in other wire
   forms like JSON, it has a special encoding. The format of the type URL is
   described on the `type_url` field.

   Protobuf APIs provide utilities to interact with `Any` values:

   - A 'pack' operation accepts a message and constructs a generic `Any` wrapper
     around it.
   - An 'unpack' operation reads the content of an `Any` message, either into an
     existing message or a new one. Unpack operations must check the type of the
     value they unpack against the declared `type_url`.
   - An 'is' operation decides whether an `Any` contains a message of the given
     type, i.e. whether it can 'unpack' that type.

   The JSON format representation of an `Any` follows one of these cases:

   - For types without special-cased JSON encodings, the JSON format
     representation of the `Any` is the same as that of the message, with an
     additional `@type` field which contains the type URL.
   - For types with special-cased JSON encodings (typically called 'well-known'
     types, listed in https://protobuf.dev/programming-guides/json/#any), the
     JSON format representation has a key `@type` which contains the type URL
     and a key `value` which contains the JSON-serialized value.

   The text format representation of an `Any` is like a message with one field
   whose name is the type URL in brackets. For example, an `Any` containing a
   `foo.Bar` message may be written `[type.googleapis.com/foo.Bar] { a: 2 }`.
   */
  module Any = {
    type t = protobufAny = {
      /**
       Identifies the type of the serialized Protobuf message with a URI reference
       consisting of a prefix ending in a slash and the fully-qualified type name.

       Example: type.googleapis.com/google.protobuf.StringValue

       This string must contain at least one `/` character, and the content after
       the last `/` must be the fully-qualified name of the type in canonical
       form, without a leading dot. Do not write a scheme on these URI references
       so that clients do not attempt to contact them.

       The prefix is arbitrary and Protobuf implementations are expected to
       simply strip off everything up to and including the last `/` to identify
       the type. `type.googleapis.com/` is a common default prefix that some
       legacy implementations require. This prefix does not indicate the origin of
       the type, and URIs containing it are not expected to respond to any
       requests.

       All type URL strings must be legal URI references with the additional
       restriction (for the text format) that the content of the reference
       must consist only of alphanumeric characters, percent-encoded escapes, and
       characters in the following set (not including the outer backticks):
       `/-.~_!$&()*+,;=`. Despite our allowing percent encodings, implementations
       should not unescape them to prevent confusion with existing parsers. For
       example, `type.googleapis.com%2FFoo` should be rejected.

       In the original design of `Any`, the possibility of launching a type
       resolution service at these type URLs was considered but Protobuf never
       implemented one and considers contacting these URLs to be problematic and
       a potential security issue. Do not attempt to contact type URLs.
       */
      typeUrl: string,
      /**
       Holds a Protobuf serialization of the type described by type_url.
       */
      value: Uint8Array.t,
    }
    @module("sury/protobuf/wkt") external schema: schema<t> = "AnySchema"
  }

  // google/protobuf/source_context.proto

  /**
   `SourceContext` represents information about the source of a
   protobuf element, like the file in which it is defined.
   */
  module SourceContext = {
    type t = {
      /**
       The path-qualified name of the .proto file that contained the associated
       protobuf element.  For example: `"google/protobuf/source_context.proto"`.
       */
      fileName: string,
    }
    @module("sury/protobuf/wkt") external schema: schema<t> = "SourceContextSchema"
  }

  // google/protobuf/type.proto

  /**
   Basic field types.
   */
  module Field_Kind = {
    type t = | @as(0) TypeUnknown | @as(1) TypeDouble | @as(2) TypeFloat | @as(3) TypeInt64 | @as(4) TypeUint64 | @as(5) TypeInt32 | @as(6) TypeFixed64 | @as(7) TypeFixed32 | @as(8) TypeBool | @as(9) TypeString | @as(10) TypeGroup | @as(11) TypeMessage | @as(12) TypeBytes | @as(13) TypeUint32 | @as(14) TypeEnum | @as(15) TypeSfixed32 | @as(16) TypeSfixed64 | @as(17) TypeSint32 | @as(18) TypeSint64
    @module("sury/protobuf/wkt") external schema: schema<t> = "Field_KindSchema"
  }

  /**
   Whether a field is optional, required, or repeated.
   */
  module Field_Cardinality = {
    type t = | @as(0) Unknown | @as(1) Optional | @as(2) Required | @as(3) Repeated
    @module("sury/protobuf/wkt") external schema: schema<t> = "Field_CardinalitySchema"
  }

  /**
   The syntax in which a protocol buffer element is defined.
   */
  module Syntax = {
    type t = | @as(0) Proto2 | @as(1) Proto3 | @as(2) Editions
    @module("sury/protobuf/wkt") external schema: schema<t> = "SyntaxSchema"
  }

  /**
   A protocol buffer option, which can be attached to a message, field,
   enumeration, etc.

   New usages of this message as an alternative to FileOptions, MessageOptions,
   FieldOptions, EnumOptions, EnumValueOptions, ServiceOptions, or MethodOptions
   are strongly discouraged.
   */
  module Option = {
    type t = {
      /**
       The option's name. For protobuf built-in options (options defined in
       descriptor.proto), this is the short name. For example, `"map_entry"`.
       For custom options, it should be the fully-qualified name. For example,
       `"google.api.http"`.
       */
      name: string,
      /**
       The option's value packed in an Any message. If the value is a primitive,
       the corresponding wrapper type defined in google/protobuf/wrappers.proto
       should be used. If the value is an enum, it should be stored as an int32
       value using the google.protobuf.Int32Value type.
       */
      value: option<Any.t>,
    }
    @module("sury/protobuf/wkt") external schema: schema<t> = "OptionSchema"
  }

  /**
   A single field of a message type.

   New usages of this message as an alternative to FieldDescriptorProto are
   strongly discouraged. This message does not reliability preserve all
   information necessary to model the schema and preserve semantics. Instead
   make use of FileDescriptorSet which preserves the necessary information.
   */
  module Field = {
    type t = {
      /**
       The field type.
       */
      kind: Field_Kind.t,
      /**
       The field cardinality.
       */
      cardinality: Field_Cardinality.t,
      /**
       The field number.
       */
      number: int,
      /**
       The field name.
       */
      name: string,
      /**
       The field type URL, without the scheme, for message or enumeration
       types. Example: `"type.googleapis.com/google.protobuf.Timestamp"`.
       */
      typeUrl: string,
      /**
       The index of the field type in `Type.oneofs`, for message or enumeration
       types. The first type has index 1; zero means the type is not in the list.
       */
      oneofIndex: int,
      /**
       Whether to use alternative packed wire representation.
       */
      packed: bool,
      /**
       The protocol buffer options.
       */
      options: array<Option.t>,
      /**
       The field JSON name.
       */
      jsonName: string,
      /**
       The string value of the default value of this field. Proto2 syntax only.
       */
      defaultValue: string,
    }
    @module("sury/protobuf/wkt") external schema: schema<t> = "FieldSchema"
  }

  /**
   A protocol buffer message type.

   New usages of this message as an alternative to DescriptorProto are strongly
   discouraged. This message does not reliability preserve all information
   necessary to model the schema and preserve semantics. Instead make use of
   FileDescriptorSet which preserves the necessary information.
   */
  module Type = {
    type t = {
      /**
       The fully qualified message name.
       */
      name: string,
      /**
       The list of fields.
       */
      fields: array<Field.t>,
      /**
       The list of types appearing in `oneof` definitions in this type.
       */
      oneofs: array<string>,
      /**
       The protocol buffer options.
       */
      options: array<Option.t>,
      /**
       The source context.
       */
      sourceContext: option<SourceContext.t>,
      /**
       The source syntax.
       */
      syntax: Syntax.t,
      /**
       The source edition string, only valid when syntax is SYNTAX_EDITIONS.
       */
      edition: string,
    }
    @module("sury/protobuf/wkt") external schema: schema<t> = "TypeSchema"
  }

  /**
   Enum value definition.

   New usages of this message as an alternative to EnumValueDescriptorProto are
   strongly discouraged. This message does not reliability preserve all
   information necessary to model the schema and preserve semantics. Instead
   make use of FileDescriptorSet which preserves the necessary information.
   */
  module EnumValue = {
    type t = {
      /**
       Enum value name.
       */
      name: string,
      /**
       Enum value number.
       */
      number: int,
      /**
       Protocol buffer options.
       */
      options: array<Option.t>,
    }
    @module("sury/protobuf/wkt") external schema: schema<t> = "EnumValueSchema"
  }

  /**
   Enum type definition.

   New usages of this message as an alternative to EnumDescriptorProto are
   strongly discouraged. This message does not reliability preserve all
   information necessary to model the schema and preserve semantics. Instead
   make use of FileDescriptorSet which preserves the necessary information.
   */
  module Enum = {
    type t = {
      /**
       Enum type name.
       */
      name: string,
      /**
       Enum value definitions.
       */
      enumvalue: array<EnumValue.t>,
      /**
       Protocol buffer options.
       */
      options: array<Option.t>,
      /**
       The source context.
       */
      sourceContext: option<SourceContext.t>,
      /**
       The source syntax.
       */
      syntax: Syntax.t,
      /**
       The source edition string, only valid when syntax is SYNTAX_EDITIONS.
       */
      edition: string,
    }
    @module("sury/protobuf/wkt") external schema: schema<t> = "EnumSchema"
  }

  // google/protobuf/api.proto

  /**
   Method represents a method of an API interface.

   New usages of this message as an alternative to MethodDescriptorProto are
   strongly discouraged. This message does not reliability preserve all
   information necessary to model the schema and preserve semantics. Instead
   make use of FileDescriptorSet which preserves the necessary information.
   */
  module Method = {
    type t = {
      /**
       The simple name of this method.
       */
      name: string,
      /**
       A URL of the input message type.
       */
      requestTypeUrl: string,
      /**
       If true, the request is streamed.
       */
      requestStreaming: bool,
      /**
       The URL of the output message type.
       */
      responseTypeUrl: string,
      /**
       If true, the response is streamed.
       */
      responseStreaming: bool,
      /**
       Any metadata attached to the method.
       */
      options: array<Option.t>,
      /**
       The source syntax of this method.

       This field should be ignored, instead the syntax should be inherited from
       Api. This is similar to Field and EnumValue.
       */
      syntax: Syntax.t,
      /**
       The source edition string, only valid when syntax is SYNTAX_EDITIONS.

       This field should be ignored, instead the edition should be inherited from
       Api. This is similar to Field and EnumValue.
       */
      edition: string,
    }
    @module("sury/protobuf/wkt") external schema: schema<t> = "MethodSchema"
  }

  /**
   Declares an API Interface to be included in this interface. The including
   interface must redeclare all the methods from the included interface, but
   documentation and options are inherited as follows:

   - If after comment and whitespace stripping, the documentation
     string of the redeclared method is empty, it will be inherited
     from the original method.

   - Each annotation belonging to the service config (http,
     visibility) which is not set in the redeclared method will be
     inherited.

   - If an http annotation is inherited, the path pattern will be
     modified as follows. Any version prefix will be replaced by the
     version of the including interface plus the [root][] path if
     specified.

   Example of a simple mixin:

       package google.acl.v1;
       service AccessControl {
         // Get the underlying ACL object.
         rpc GetAcl(GetAclRequest) returns (Acl) {
           option (google.api.http).get = "/v1/{resource=**}:getAcl";
         }
       }

       package google.storage.v2;
       service Storage {
         rpc GetAcl(GetAclRequest) returns (Acl);

         // Get a data record.
         rpc GetData(GetDataRequest) returns (Data) {
           option (google.api.http).get = "/v2/{resource=**}";
         }
       }

   Example of a mixin configuration:

       apis:
       - name: google.storage.v2.Storage
         mixins:
         - name: google.acl.v1.AccessControl

   The mixin construct implies that all methods in `AccessControl` are
   also declared with same name and request/response types in
   `Storage`. A documentation generator or annotation processor will
   see the effective `Storage.GetAcl` method after inheriting
   documentation and annotations as follows:

       service Storage {
         // Get the underlying ACL object.
         rpc GetAcl(GetAclRequest) returns (Acl) {
           option (google.api.http).get = "/v2/{resource=**}:getAcl";
         }
         ...
       }

   Note how the version in the path pattern changed from `v1` to `v2`.

   If the `root` field in the mixin is specified, it should be a
   relative path under which inherited HTTP paths are placed. Example:

       apis:
       - name: google.storage.v2.Storage
         mixins:
         - name: google.acl.v1.AccessControl
           root: acls

   This implies the following inherited HTTP annotation:

       service Storage {
         // Get the underlying ACL object.
         rpc GetAcl(GetAclRequest) returns (Acl) {
           option (google.api.http).get = "/v2/acls/{resource=**}:getAcl";
         }
         ...
       }
   */
  module Mixin = {
    type t = {
      /**
       The fully qualified name of the interface which is included.
       */
      name: string,
      /**
       If non-empty specifies a path under which inherited HTTP paths
       are rooted.
       */
      root: string,
    }
    @module("sury/protobuf/wkt") external schema: schema<t> = "MixinSchema"
  }

  /**
   Api is a light-weight descriptor for an API Interface.

   Interfaces are also described as "protocol buffer services" in some contexts,
   such as by the "service" keyword in a .proto file, but they are different
   from API Services, which represent a concrete implementation of an interface
   as opposed to simply a description of methods and bindings. They are also
   sometimes simply referred to as "APIs" in other contexts, such as the name of
   this message itself. See https://cloud.google.com/apis/design/glossary for
   detailed terminology.

   New usages of this message as an alternative to ServiceDescriptorProto are
   strongly discouraged. This message does not reliability preserve all
   information necessary to model the schema and preserve semantics. Instead
   make use of FileDescriptorSet which preserves the necessary information.
   */
  module Api = {
    type t = {
      /**
       The fully qualified name of this interface, including package name
       followed by the interface's simple name.
       */
      name: string,
      /**
       The methods of this interface, in unspecified order.
       */
      methods: array<Method.t>,
      /**
       Any metadata attached to the interface.
       */
      options: array<Option.t>,
      /**
       A version string for this interface. If specified, must have the form
       `major-version.minor-version`, as in `1.10`. If the minor version is
       omitted, it defaults to zero. If the entire version field is empty, the
       major version is derived from the package name, as outlined below. If the
       field is not empty, the version in the package name will be verified to be
       consistent with what is provided here.

       The versioning schema uses [semantic
       versioning](http://semver.org) where the major version number
       indicates a breaking change and the minor version an additive,
       non-breaking change. Both version numbers are signals to users
       what to expect from different versions, and should be carefully
       chosen based on the product plan.

       The major version is also reflected in the package name of the
       interface, which must end in `v<major-version>`, as in
       `google.feature.v1`. For major versions 0 and 1, the suffix can
       be omitted. Zero major versions must only be used for
       experimental, non-GA interfaces.

       */
      version: string,
      /**
       Source context for the protocol buffer service represented by this
       message.
       */
      sourceContext: option<SourceContext.t>,
      /**
       Included interfaces. See [Mixin][].
       */
      mixins: array<Mixin.t>,
      /**
       The source syntax of the service.
       */
      syntax: Syntax.t,
      /**
       The source edition string, only valid when syntax is SYNTAX_EDITIONS.
       */
      edition: string,
    }
    @module("sury/protobuf/wkt") external schema: schema<t> = "ApiSchema"
  }

  // google/protobuf/duration.proto

  /**
   A Duration represents a signed, fixed-length span of time represented
   as a count of seconds and fractions of seconds at nanosecond
   resolution. It is independent of any calendar and concepts like "day"
   or "month". It is related to Timestamp in that the difference between
   two Timestamp values is a Duration and it can be added or subtracted
   from a Timestamp. Range is approximately +-10,000 years.

   # Examples

   Example 1: Compute Duration from two Timestamps in pseudo code.

       Timestamp start = ...;
       Timestamp end = ...;
       Duration duration = ...;

       duration.seconds = end.seconds - start.seconds;
       duration.nanos = end.nanos - start.nanos;

       if (duration.seconds < 0 && duration.nanos > 0) {
         duration.seconds += 1;
         duration.nanos -= 1000000000;
       } else if (duration.seconds > 0 && duration.nanos < 0) {
         duration.seconds -= 1;
         duration.nanos += 1000000000;
       }

   Example 2: Compute Timestamp from Timestamp + Duration in pseudo code.

       Timestamp start = ...;
       Duration duration = ...;
       Timestamp end = ...;

       end.seconds = start.seconds + duration.seconds;
       end.nanos = start.nanos + duration.nanos;

       if (end.nanos < 0) {
         end.seconds -= 1;
         end.nanos += 1000000000;
       } else if (end.nanos >= 1000000000) {
         end.seconds += 1;
         end.nanos -= 1000000000;
       }

   Example 3: Compute Duration from datetime.timedelta in Python.

       td = datetime.timedelta(days=3, minutes=10)
       duration = Duration()
       duration.FromTimedelta(td)

   # JSON Mapping

   In JSON format, the Duration type is encoded as a string rather than an
   object, where the string ends in the suffix "s" (indicating seconds) and
   is preceded by the number of seconds, with nanoseconds expressed as
   fractional seconds. For example, 3 seconds with 0 nanoseconds should be
   encoded in JSON format as "3s", while 3 seconds and 1 nanosecond should
   be expressed in JSON format as "3.000000001s", and 3 seconds and 1
   microsecond should be expressed in JSON format as "3.000001s".

   */
  module Duration = {
    type t = {
      /**
       Signed seconds of the span of time. Must be from -315,576,000,000
       to +315,576,000,000 inclusive. Note: these bounds are computed from:
       60 sec/min * 60 min/hr * 24 hr/day * 365.25 days/year * 10000 years
       */
      seconds: bigint,
      /**
       Signed fractions of a second at nanosecond resolution of the span
       of time. Durations less than one second are represented with a 0
       `seconds` field and a positive or negative `nanos` field. For durations
       of one second or more, a non-zero value for the `nanos` field must be
       of the same sign as the `seconds` field. Must be from -999,999,999
       to +999,999,999 inclusive.
       */
      nanos: int,
    }
    @module("sury/protobuf/wkt") external schema: schema<t> = "DurationSchema"
  }

  // google/protobuf/empty.proto

  /**
   A generic empty message that you can re-use to avoid defining duplicated
   empty messages in your APIs. A typical example is to use it as the request
   or the response type of an API method. For instance:

       service Foo {
         rpc Bar(google.protobuf.Empty) returns (google.protobuf.Empty);
       }

   */
  module Empty = {
    type t = {}
    @module("sury/protobuf/wkt") external schema: schema<t> = "EmptySchema"
  }

  // google/protobuf/field_mask.proto

  /**
   `FieldMask` represents a set of symbolic field paths, for example:

       paths: "f.a"
       paths: "f.b.d"

   Here `f` represents a field in some root message, `a` and `b`
   fields in the message found in `f`, and `d` a field found in the
   message in `f.b`.

   Field masks are used to specify a subset of fields that should be
   returned by a get operation or modified by an update operation.
   Field masks also have a custom JSON encoding (see below).

   # Field Masks in Projections

   When used in the context of a projection, a response message or
   sub-message is filtered by the API to only contain those fields as
   specified in the mask. For example, if the mask in the previous
   example is applied to a response message as follows:

       f {
         a : 22
         b {
           d : 1
           x : 2
         }
         y : 13
       }
       z: 8

   The result will not contain specific values for fields x,y and z
   (their value will be set to the default, and omitted in proto text
   output):


       f {
         a : 22
         b {
           d : 1
         }
       }

   A repeated field is not allowed except at the last position of a
   paths string.

   If a FieldMask object is not present in a get operation, the
   operation applies to all fields (as if a FieldMask of all fields
   had been specified).

   Note that a field mask does not necessarily apply to the
   top-level response message. In case of a REST get operation, the
   field mask applies directly to the response, but in case of a REST
   list operation, the mask instead applies to each individual message
   in the returned resource list. In case of a REST custom method,
   other definitions may be used. Where the mask applies will be
   clearly documented together with its declaration in the API.  In
   any case, the effect on the returned resource/resources is required
   behavior for APIs.

   # Field Masks in Update Operations

   A field mask in update operations specifies which fields of the
   targeted resource are going to be updated. The API is required
   to only change the values of the fields as specified in the mask
   and leave the others untouched. If a resource is passed in to
   describe the updated values, the API ignores the values of all
   fields not covered by the mask.

   If a repeated field is specified for an update operation, new values will
   be appended to the existing repeated field in the target resource. Note that
   a repeated field is only allowed in the last position of a `paths` string.

   If a sub-message is specified in the last position of the field mask for an
   update operation, then new value will be merged into the existing sub-message
   in the target resource.

   For example, given the target message:

       f {
         b {
           d: 1
           x: 2
         }
         c: [1]
       }

   And an update message:

       f {
         b {
           d: 10
         }
         c: [2]
       }

   then if the field mask is:

    paths: ["f.b", "f.c"]

   then the result will be:

       f {
         b {
           d: 10
           x: 2
         }
         c: [1, 2]
       }

   An implementation may provide options to override this default behavior for
   repeated and message fields.

   Note that libraries which implement FieldMask resolution have various
   different behaviors in the face of empty masks or the special "*" mask.
   When implementing a service you should confirm these cases have the
   appropriate behavior in the underlying FieldMask library that you desire,
   and you may need to special case those cases in your application code if
   the underlying field mask library behavior differs from your intended
   service semantics.

   Update methods implementing https://google.aip.dev/134
   - MUST support the special value * meaning "full replace"
   - MUST treat an omitted field mask as "replace fields which are present".

   Other methods implementing https://google.aip.dev/157
   - SHOULD support the special value "*" to mean "get all".
   - MUST treat an omitted field mask to mean "get all", unless otherwise
   documented.

   ## Considerations for HTTP REST

   The HTTP kind of an update operation which uses a field mask must
   be set to PATCH instead of PUT in order to satisfy HTTP semantics
   (PUT must only be used for full updates).

   # JSON Encoding of Field Masks

   In JSON, a field mask is encoded as a single string where paths are
   separated by a comma. Fields name in each path are converted
   to/from lower-camel naming conventions.

   As an example, consider the following message declarations:

       message Profile {
         User user = 1;
         Photo photo = 2;
       }
       message User {
         string display_name = 1;
         string address = 2;
       }

   In proto a field mask for `Profile` may look as such:

       mask {
         paths: "user.display_name"
         paths: "photo"
       }

   In JSON, the same mask is represented as below:

       {
         mask: "user.displayName,photo"
       }

   # Field Masks and Oneof Fields

   Field masks treat fields in oneofs just as regular fields. Consider the
   following message:

       message SampleMessage {
         oneof test_oneof {
           string name = 4;
           SubMessage sub_message = 9;
         }
       }

   The field mask can be:

       mask {
         paths: "name"
       }

   Or:

       mask {
         paths: "sub_message"
       }

   Note that oneof type names ("test_oneof" in this case) cannot be used in
   paths.

   ## Field Mask Verification

   The implementation of any API method which has a FieldMask type field in the
   request should verify the included field paths, and return an
   `INVALID_ARGUMENT` error if any path is unmappable.
   */
  module FieldMask = {
    type t = {
      /**
       The set of field mask paths.
       */
      paths: array<string>,
    }
    @module("sury/protobuf/wkt") external schema: schema<t> = "FieldMaskSchema"
  }

  // google/protobuf/struct.proto

  /**
   Represents a JSON `null`.

   `NullValue` is a sentinel, using an enum with only one value to represent
   the null value for the `Value` type union.

   A field of type `NullValue` with any value other than `0` is considered
   invalid. Most ProtoJSON serializers will emit a Value with a `null_value` set
   as a JSON `null` regardless of the integer value, and so will round trip to
   a `0` value.
   */
  module NullValue = {
    type t = | @as(0) NullValue
    @module("sury/protobuf/wkt") external schema: schema<t> = "NullValueSchema"
  }

  type rec struct = {
    /**
     Unordered map of dynamically typed values.
     */
    fields: dict<value>,
  }
  @tag("case")
  and value_kind =
    | @as("nullValue") NullValue({value: NullValue.t})
    | @as("numberValue") NumberValue({value: float})
    | @as("stringValue") StringValue({value: string})
    | @as("boolValue") BoolValue({value: bool})
    | @as("structValue") StructValue({value: struct})
    | @as("listValue") ListValue({value: listValue})
    | @as(undefined) Unset({value?: unit})
  and value = {
    kind: value_kind,
  }
  and listValue = {
    /**
     Repeated field of dynamically typed values.
     */
    values: array<value>,
  }

  /**
   Represents a JSON object.

   An unordered key-value map, intending to perfectly capture the semantics of a
   JSON object. This enables parsing any arbitrary JSON payload as a message
   field in ProtoJSON format.

   This follows RFC 8259 guidelines for interoperable JSON: notably this type
   cannot represent large Int64 values or `NaN`/`Infinity` numbers,
   since the JSON format generally does not support those values in its number
   type.

   If you do not intend to parse arbitrary JSON into your message, a custom
   typed message should be preferred instead of using this type.
   */
  module Struct = {
    type t = struct
    @module("sury/protobuf/wkt") external schema: schema<t> = "StructSchema"
  }

  /**
   Represents a JSON value.

   `Value` represents a dynamically typed value which can be either
   null, a number, a string, a boolean, a recursive struct value, or a
   list of values. A producer of value is expected to set one of these
   variants. Absence of any variant is an invalid state.
   */
  module Value = {
    type t = value
    @module("sury/protobuf/wkt") external schema: schema<t> = "ValueSchema"
  }

  /**
   Represents a JSON array.
   */
  module ListValue = {
    type t = listValue
    @module("sury/protobuf/wkt") external schema: schema<t> = "ListValueSchema"
  }

  // google/protobuf/timestamp.proto

  /**
   A Timestamp represents a point in time independent of any time zone or local
   calendar, encoded as a count of seconds and fractions of seconds at
   nanosecond resolution. The count is relative to an epoch at UTC midnight on
   January 1, 1970, in the proleptic Gregorian calendar which extends the
   Gregorian calendar backwards to year one.

   All minutes are 60 seconds long. Leap seconds are "smeared" so that no leap
   second table is needed for interpretation, using a [24-hour linear
   smear](https://developers.google.com/time/smear).

   The range is from 0001-01-01T00:00:00Z to 9999-12-31T23:59:59.999999999Z. By
   restricting to that range, we ensure that we can convert to and from [RFC
   3339](https://www.ietf.org/rfc/rfc3339.txt) date strings.

   # Examples

   Example 1: Compute Timestamp from POSIX `time()`.

       Timestamp timestamp;
       timestamp.set_seconds(time(NULL));
       timestamp.set_nanos(0);

   Example 2: Compute Timestamp from POSIX `gettimeofday()`.

       struct timeval tv;
       gettimeofday(&tv, NULL);

       Timestamp timestamp;
       timestamp.set_seconds(tv.tv_sec);
       timestamp.set_nanos(tv.tv_usec * 1000);

   Example 3: Compute Timestamp from Win32 `GetSystemTimeAsFileTime()`.

       FILETIME ft;
       GetSystemTimeAsFileTime(&ft);
       UINT64 ticks = (((UINT64)ft.dwHighDateTime) << 32) | ft.dwLowDateTime;

       // A Windows tick is 100 nanoseconds. Windows epoch 1601-01-01T00:00:00Z
       // is 11644473600 seconds before Unix epoch 1970-01-01T00:00:00Z.
       Timestamp timestamp;
       timestamp.set_seconds((INT64) ((ticks / 10000000) - 11644473600LL));
       timestamp.set_nanos((INT32) ((ticks % 10000000) * 100));

   Example 4: Compute Timestamp from Java `System.currentTimeMillis()`.

       long millis = System.currentTimeMillis();

       Timestamp timestamp = Timestamp.newBuilder().setSeconds(millis / 1000)
           .setNanos((int) ((millis % 1000) * 1000000)).build();

   Example 5: Compute Timestamp from Java `Instant.now()`.

       Instant now = Instant.now();

       Timestamp timestamp =
           Timestamp.newBuilder().setSeconds(now.getEpochSecond())
               .setNanos(now.getNano()).build();

   Example 6: Compute Timestamp from current time in Python.

       timestamp = Timestamp()
       timestamp.GetCurrentTime()

   # JSON Mapping

   In JSON format, the Timestamp type is encoded as a string in the
   [RFC 3339](https://www.ietf.org/rfc/rfc3339.txt) format. That is, the
   format is "{year}-{month}-{day}T{hour}:{min}:{sec}[.{frac_sec}]Z"
   where {year} is always expressed using four digits while {month}, {day},
   {hour}, {min}, and {sec} are zero-padded to two digits each. The fractional
   seconds, which can go up to 9 digits (i.e. up to 1 nanosecond resolution),
   are optional. The "Z" suffix indicates the timezone ("UTC"); the timezone
   is required. A ProtoJSON serializer should always use UTC (as indicated by
   "Z") when printing the Timestamp type and a ProtoJSON parser should be
   able to accept both UTC and other timezones (as indicated by an offset).

   For example, "2017-01-15T01:30:15.01Z" encodes 15.01 seconds past
   01:30 UTC on January 15, 2017.

   In JavaScript, one can convert a Date object to this format using the
   standard
   [toISOString()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toISOString)
   method. In Python, a standard `datetime.datetime` object can be converted
   to this format using
   [`strftime`](https://docs.python.org/2/library/time.html#time.strftime) with
   the time format spec '%Y-%m-%dT%H:%M:%S.%fZ'. Likewise, in Java, one can use
   the Joda Time's [`ISODateTimeFormat.dateTime()`](
   http://joda-time.sourceforge.net/apidocs/org/joda/time/format/ISODateTimeFormat.html#dateTime()
   ) to obtain a formatter capable of generating timestamps in this format.

   */
  module Timestamp = {
    type t = {
      /**
       Represents seconds of UTC time since Unix epoch 1970-01-01T00:00:00Z. Must
       be between -62135596800 and 253402300799 inclusive (which corresponds to
       0001-01-01T00:00:00Z to 9999-12-31T23:59:59Z).
       */
      seconds: bigint,
      /**
       Non-negative fractions of a second at nanosecond resolution. This field is
       the nanosecond portion of the duration, not an alternative to seconds.
       Negative second values with fractions must still have non-negative nanos
       values that count forward in time. Must be between 0 and 999,999,999
       inclusive.
       */
      nanos: int,
    }
    @module("sury/protobuf/wkt") external schema: schema<t> = "TimestampSchema"
  }

  // google/protobuf/wrappers.proto

  /**
   Wrapper message for `double`.

   The JSON representation for `DoubleValue` is JSON number.

   Not recommended for use in new APIs, but still useful for legacy APIs and
   has no plan to be removed.
   */
  module DoubleValue = {
    type t = {
      /**
       The double value.
       */
      value: float,
    }
    @module("sury/protobuf/wkt") external schema: schema<t> = "DoubleValueSchema"
  }

  /**
   Wrapper message for `float`.

   The JSON representation for `FloatValue` is JSON number.

   Not recommended for use in new APIs, but still useful for legacy APIs and
   has no plan to be removed.
   */
  module FloatValue = {
    type t = {
      /**
       The float value.
       */
      value: float,
    }
    @module("sury/protobuf/wkt") external schema: schema<t> = "FloatValueSchema"
  }

  /**
   Wrapper message for `int64`.

   The JSON representation for `Int64Value` is JSON string.

   Not recommended for use in new APIs, but still useful for legacy APIs and
   has no plan to be removed.
   */
  module Int64Value = {
    type t = {
      /**
       The int64 value.
       */
      value: bigint,
    }
    @module("sury/protobuf/wkt") external schema: schema<t> = "Int64ValueSchema"
  }

  /**
   Wrapper message for `uint64`.

   The JSON representation for `UInt64Value` is JSON string.

   Not recommended for use in new APIs, but still useful for legacy APIs and
   has no plan to be removed.
   */
  module UInt64Value = {
    type t = {
      /**
       The uint64 value.
       */
      value: bigint,
    }
    @module("sury/protobuf/wkt") external schema: schema<t> = "UInt64ValueSchema"
  }

  /**
   Wrapper message for `int32`.

   The JSON representation for `Int32Value` is JSON number.

   Not recommended for use in new APIs, but still useful for legacy APIs and
   has no plan to be removed.
   */
  module Int32Value = {
    type t = {
      /**
       The int32 value.
       */
      value: int,
    }
    @module("sury/protobuf/wkt") external schema: schema<t> = "Int32ValueSchema"
  }

  /**
   Wrapper message for `uint32`.

   The JSON representation for `UInt32Value` is JSON number.

   Not recommended for use in new APIs, but still useful for legacy APIs and
   has no plan to be removed.
   */
  module UInt32Value = {
    type t = {
      /**
       The uint32 value.
       */
      value: integer,
    }
    @module("sury/protobuf/wkt") external schema: schema<t> = "UInt32ValueSchema"
  }

  /**
   Wrapper message for `bool`.

   The JSON representation for `BoolValue` is JSON `true` and `false`.

   Not recommended for use in new APIs, but still useful for legacy APIs and
   has no plan to be removed.
   */
  module BoolValue = {
    type t = {
      /**
       The bool value.
       */
      value: bool,
    }
    @module("sury/protobuf/wkt") external schema: schema<t> = "BoolValueSchema"
  }

  /**
   Wrapper message for `string`.

   The JSON representation for `StringValue` is JSON string.

   Not recommended for use in new APIs, but still useful for legacy APIs and
   has no plan to be removed.
   */
  module StringValue = {
    type t = {
      /**
       The string value.
       */
      value: string,
    }
    @module("sury/protobuf/wkt") external schema: schema<t> = "StringValueSchema"
  }

  /**
   Wrapper message for `bytes`.

   The JSON representation for `BytesValue` is JSON string.

   Not recommended for use in new APIs, but still useful for legacy APIs and
   has no plan to be removed.
   */
  module BytesValue = {
    type t = {
      /**
       The bytes value.
       */
      value: Uint8Array.t,
    }
    @module("sury/protobuf/wkt") external schema: schema<t> = "BytesValueSchema"
  }
}
// @generated end
