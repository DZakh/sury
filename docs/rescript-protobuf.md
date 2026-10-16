[⬅ Back to the ReScript API reference](./rescript-usage.md)

# Protocol Buffers

## Table of contents

- [Field types](#field-types)
- [Optional fields](#optional-fields)
- [Lists, maps and oneof](#lists-maps-and-oneof)
- [Oneofs as variants](#oneofs-as-variants)
- [Recursive messages](#recursive-messages)
- [Well-known types](#well-known-types)
- [Unknown fields and errors](#unknown-fields-and-errors)
- [`toProtoOrThrow`](#toprotoorthrow)
- [Generating modules from `.proto`](#generating-modules-from-proto)

`S.protobuf` encodes and decodes the [Protocol Buffers](https://protobuf.dev)
binary format. Number each field with `S.protobufField` and convert to or from
`S.protobuf`. You don't need a `.proto` file or a code generation step.

```rescript
type address = {street: string}
type user = {id: int, name: string, tags: array<string>, home: option<address>, kind: int}

let addressSchema = S.schema(s => {street: s.matches(S.string->S.protobufField(1))})

let userSchema = S.schema(s => {
  id: s.matches(S.int->S.protobufField(1)),
  name: s.matches(S.string->S.protobufField(2)),
  tags: s.matches(S.array(S.string)->S.protobufField(3)),
  home: s.matches(S.option(addressSchema)->S.protobufField(4)),
  kind: s.matches(S.enum([1, 2])->S.protobufField(5)),
})->S.meta({name: "User"})

let value = {id: 150, name: "Ada", tags: ["ml"], home: Some({street: "Main"}), kind: 2}

let bytes = value->S.convertOrThrow(~from=userSchema, ~to=S.protobuf) // 22 bytes
bytes->S.convertOrThrow(~from=S.protobuf, ~to=userSchema) // value
```

Compile the operation once when you run it many times:

```rescript
let decode = S.compileConvertOrThrow(~from=S.protobuf, ~to=userSchema)
```

To parse bytes from `unknown`, use `S.protobuf->S.to(userSchema)`:

```rescript
body->S.parseOrThrow(~to=S.protobuf->S.to(userSchema))
```

With the [PPX](https://github.com/DZakh/sury/blob/main/packages/sury-ppx/README.md), number fields with `@s.with`:

```rescript
@schema
type address = {street: @s.with(S.protobufField(_, 1)) string}
```

Build messages with `S.schema`, `S.object` or the PPX.

## Field types

The schema picks the protobuf type:

| Schema                  | Protobuf type               |
| ----------------------- | --------------------------- |
| `S.string`              | `string`                    |
| `S.bool`                | `bool`                      |
| `S.int`                 | `int32`                     |
| `S.float`               | `double`                    |
| `S.bigint`              | `int64`                     |
| `S.uint8Array`          | `bytes`                     |
| `S.enum([1, 2])`        | `enum`                      |
| `S.schema(...)`         | a nested message            |
| `S.array(t)`            | `repeated t`                |
| `S.dict(t)`             | `map<string, t>`            |
| `S.date`                | `google.protobuf.Timestamp` |
| `S.json`                | `google.protobuf.Value`     |

Pass `~type_` to choose another type. The value in your schema can differ from
the wire type:

```rescript
S.string->S.protobufField(1, ~type_=#uint32) // "150" is sent as 150
S.int->S.protobufField(2, ~type_=#sint32)
S.bigint->S.protobufField(3, ~type_=#fixed64)
S.float->S.protobufField(4, ~type_=#float)
```

## Optional fields

Proto3 skips a field that holds its default value (`0`, `""`, `false`), and
decodes a missing field as that default. Use `S.option` to tell `None` from
`Some(0)`:

```rescript
S.option(S.int)->S.protobufField(1)
```

## Lists, maps and oneof

```rescript
S.array(S.int)->S.protobufField(1) // repeated int32, packed
S.array(S.int)->S.protobufField(2, ~packed=false) // one tag per item
S.dict(S.string)->S.protobufField(3, ~key=#int64) // map<int64, string>
S.option(S.string)->S.protobufField(4, ~oneof="choice")
S.option(S.int)->S.protobufField(5, ~oneof="choice")
```

Fields with the same `~oneof` name are a `oneof`: at most one of them can be
`Some`, and encoding a value with two set throws.

## Oneofs as variants

A tagged variant is a oneof. Each constructor holds one numbered value:

```rescript
@tag("kind")
type channel =
  | @as("email") Email({email: string})
  | @as("sms") Sms({phone: string})
  | @as("none") NoChannel

let channelSchema = S.union([
  S.schema(s => Email({email: s.matches(S.string->S.protobufField(1))})),
  S.schema(s => Sms({phone: s.matches(S.string->S.protobufField(2))})),
  S.literal(NoChannel),
])

type subscriber = {channel: channel}

let subscriberSchema = S.schema(s => {channel: s.matches(channelSchema)})

{channel: Sms({phone: "1"})}->S.convertOrThrow(~from=subscriberSchema, ~to=S.protobuf)
// [18, 1, 49]
```

Leave the constructor with no value out and wrap the union in `S.option`
instead:

```rescript
S.option(
  S.union([
    S.schema(s => Email({email: s.matches(S.string->S.protobufField(1))})),
    S.schema(s => Sms({phone: s.matches(S.string->S.protobufField(2))})),
  ]),
)
// no member set: None
```

An enum reads best as a variant whose constructors carry the numbers:

```rescript
type kind = | @as(0) Unspecified | @as(1) Person | @as(2) Robot

let kindSchema = S.enum([Unspecified, Person, Robot])->S.protobufField(4, ~type_=#enum)
```

## Recursive messages

Use [`recursive`](./rescript-usage.md#recursive):

```rescript
type rec category = {name: string, children: array<category>}

let categorySchema = S.recursive("Category", categorySchema =>
  S.schema(s => {
    name: s.matches(S.string->S.protobufField(1)),
    children: s.matches(S.array(categorySchema)->S.protobufField(2)),
  })
)
```

## Well-known types

Google's `google.protobuf.*` types are supported. `S.date` and `S.json` pick
theirs automatically. For the others, pass `~type_`:

```rescript
type duration = {seconds: bigint, nanos: int}
type job = {at: Date.t, payload: JSON.t, timeout: duration, retries: option<int>}

let jobSchema = S.schema(s => {
  at: s.matches(S.date->S.protobufField(1)),
  payload: s.matches(S.json->S.protobufField(2)),
  timeout: s.matches(
    S.schema(s => {seconds: s.matches(S.bigint), nanos: s.matches(S.int)})
    ->S.protobufField(3, ~type_=#"google.protobuf.Duration"),
  ),
  retries: s.matches(S.option(S.int)->S.protobufField(4, ~type_=#"google.protobuf.Int32Value")),
})
```

| `~type_`                              | Schema                                           |
| ------------------------------------- | ------------------------------------------------ |
| `#"google.protobuf.Timestamp"`        | `S.date`, or a `{seconds: bigint, nanos: int}` record |
| `#"google.protobuf.Duration"`         | a `{seconds: bigint, nanos: int}` record          |
| `#"google.protobuf.Value"`            | `S.json`                                         |
| `#"google.protobuf.Struct"`           | `S.dict(S.json)`                                 |
| `#"google.protobuf.ListValue"`        | `S.array(S.json)`                                |
| `#"google.protobuf.FieldMask"`        | `S.array(S.string)`                              |
| `#"google.protobuf.Int32Value"`, `#"google.protobuf.StringValue"` and the other wrappers | `S.option(S.int)`, `S.option(S.string)`, ... |

A `Date.t` holds milliseconds, so use the `{seconds, nanos}` record when you
need nanoseconds.

`SuryProtobuf` holds every well-known type as an ordinary message, the way
generated code uses them: `SuryProtobuf.Timestamp.t`,
`SuryProtobuf.Timestamp.schema`.

An `Any` is a variant of the messages it may hold, each named with
`S.protobufTypeName` and tagged by a literal of your choosing:

```rescript
@tag("type")
type event =
  | @as("user") User({value: user})
  | @as("order") Order({value: order})

let eventSchema = S.union([
  S.schema(s => User({value: s.matches(userSchema->S.protobufTypeName("acme.v1.User"))})),
  S.schema(s => Order({value: s.matches(orderSchema->S.protobufTypeName("acme.v1.Order"))})),
])->S.protobufField(1)
```

A type the variant doesn't list fails the decode, unless a case holds
`S.protobufAny`, which keeps it as its type URL and bytes. The same conversion
works outside a message, on an `S.protobufAny` you already have:

```rescript
let event = S.protobufAny->S.to(eventSchema)
```

## Unknown fields and errors

Fields your schema doesn't declare are skipped, and decoding then encoding
drops them. Use `S.strict` on the message to throw on them instead.

Bad input throws an error that points to the field:

```rescript
%raw(`new Uint8Array([8, 1, 34, 3, 10, 1, 255])`)->S.convertOrThrow(
  ~from=S.protobuf,
  ~to=userSchema,
)
// throws: Failed at home.street: Protobuf string is not valid UTF-8
```

## `toProtoOrThrow`

`(S.t<'value>, ~name: string=?, ~package: string=?) => string`

Prints the `.proto` file for a message schema, so other languages can generate
code from it. The `name` meta names the message, then `~name`, then `Message`.

```rescript
userSchema->S.toProtoOrThrow(~package="acme.v1")
```

```proto
syntax = "proto3";

package acme.v1;

message User {
  message Home {
    string street = 1;
  }
  enum Kind {
    KIND_UNSPECIFIED = 0;
    KIND_1 = 1;
    KIND_2 = 2;
  }

  int32 id = 1;
  string name = 2;
  repeated string tags = 3;
  optional Home home = 4;
  Kind kind = 5;
}
```

Field names print in snake_case, the `description` meta as a comment and
`deprecated` as `[deprecated = true]`.

## Generating modules from `.proto`

`protoc-gen-sury` ships in the `sury` package. With `target=res` it writes a
ReScript file per `.proto`, and `target=ts+res` writes both:

```yaml
# buf.gen.yaml
version: v2
plugins:
  - local: protoc-gen-sury
    out: src/gen
    opt: target=res
```

A file is named after its path, `acme/v1/user.proto` becoming
`Acme_v1_user_pb.res`, since a ReScript project's modules share one namespace.
Each message and enum is a module holding `t` and `schema`:

```rescript
module User = {
  @tag("case")
  type contact =
    | @as("email") Email({value: string})
    | @as("phone") Phone({value: string})
  type t = {
    id: int,
    firstName: string,
    contact: option<contact>,
  }
  let schema: S.t<t> = S.schema(s => { /* one S.protobufField per field */ })
}
```

```rescript
let bytes = {id: 1, firstName: "Ada", contact: Some(Email({value: "a@b"}))}
  ->S.convertOrThrow(~from=Acme_v1_user_pb.User.schema, ~to=S.protobuf)
```

A field is named as in TypeScript, `firstName`, with a keyword or a `$` made a
valid label (`type_`, `constructor_`); the wire only cares about numbers. A
`uint32` or `fixed32` field is `S.integer`, since it can pass `int`'s range.
The generated files silence warning 30, which a recursive group of messages
sharing a field name raises.

See [Performance and compatibility in the JS guide](./js-protobuf.md#performance-and-compatibility) for
conformance, and
[Benchmarks: Protobuf](https://github.com/DZakh/sury/blob/main/docs/benchmarks/protobuf.md)
for speed and bundle size.
