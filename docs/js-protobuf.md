[⬅ Back to the JavaScript API reference](./js-usage.md)

# Protocol Buffers

## Table of contents

- [Field types](#field-types)
- [Optional fields](#optional-fields)
- [Lists and maps](#lists-and-maps)
- [Oneof](#oneof)
- [Recursive messages](#recursive-messages)
- [Well-known types](#well-known-types)
- [Any](#any)
- [Generating a `.proto`](#generating-a-proto)
- [Unknown fields and errors](#unknown-fields-and-errors)
- [`ArrayBuffer` output](#arraybuffer-output)
- [Generating schemas from `.proto`](#generating-schemas-from-proto)
- [Performance and compatibility](#performance-and-compatibility)

`S.protobuf` encodes and decodes the [Protocol Buffers](https://protobuf.dev)
binary format. Number each field with `S.protobufField` and use `S.protobuf` as
the other side of the operation. You don't need a `.proto` file or a code
generation step.

```ts
const User = S.schema({
  id: S.int32.with(S.protobufField, 1),
  name: S.string.with(S.protobufField, 2),
  tags: S.array(S.string).with(S.protobufField, 3),
  score: S.optional(S.number).with(S.protobufField, 4),
});

const encode = S.encodeOrThrow(User, S.protobuf);
const decode = S.decodeOrThrow(S.protobuf, User);

const bytes = encode({ id: 150, name: "Ada", tags: ["ml"] });
// Uint8Array [8, 150, 1, 18, 3, 65, 100, 97, 26, 2, 109, 108]
decode(bytes); // { id: 150, name: "Ada", tags: ["ml"] }
```

To get a single schema that parses bytes from `unknown`, use
`S.protobuf.with(S.to, User)`:

```ts
S.parseOrThrow(S.protobuf.with(S.to, User))(body);
```

## Field types

The schema picks the protobuf type:

| Schema                       | Protobuf type    |
| ---------------------------- | ---------------- |
| `S.string`                   | `string`         |
| `S.boolean`                  | `bool`           |
| `S.int32`                    | `int32`          |
| `S.number`                   | `double`         |
| `S.bigint`                   | `int64`          |
| `S.uint8Array`               | `bytes`          |
| `S.union([0, 1, 2])`         | `enum`           |
| `S.schema({ ... })`          | a nested message |
| `S.array(T)`                 | `repeated T`     |
| `S.record(T)`                | `map<string, T>` |
| `S.date`                     | `google.protobuf.Timestamp` |
| `S.json`                     | `google.protobuf.Value` |

Pass an object instead of a number to choose another type. The value in your
schema can differ from the wire type:

```ts
S.schema({
  id: S.string.with(S.protobufField, { number: 1, type: "uint32" }), // "150" is sent as 150
  delta: S.int32.with(S.protobufField, { number: 2, type: "sint32" }),
  hash: S.bigint.with(S.protobufField, { number: 3, type: "fixed64" }),
  ratio: S.number.with(S.protobufField, { number: 4, type: "float" }),
});
```

`type` is one of `double`, `float`, `int32`, `int64`, `uint32`, `uint64`,
`sint32`, `sint64`, `fixed32`, `fixed64`, `sfixed32`, `sfixed64`, `bool`,
`string`, `bytes`, `enum`, `message`, or one of the
[well-known types](#well-known-types). 64-bit integers are `bigint`s.

Three shapes the inferred types don't cover:

```ts
S.schema({
  // S.number refuses NaN, which a double holds like any other value
  ratio: S.union([S.number, S.schema(NaN)]).with(S.protobufField, { number: 1, type: "double" }),
  // a one-member enum, such as google.protobuf.NullValue
  nothing: S.schema(0).with(S.protobufField, { number: 2, type: "enum" }),
  // int64 as its digits, what [jstype = JS_STRING] asks for
  id: S.string.with(S.protobufField, { number: 3, type: "int64" }), // "150" is sent as 150
});
```

## Optional fields

Proto3 skips a field that holds its default value (`0`, `""`, `false`, empty
bytes), and decodes a missing field as that default. Wrap a field in
`S.optional` to tell "unset" from "zero":

```ts
const Counter = S.schema({
  count: S.int32.with(S.protobufField, 1),
  limit: S.optional(S.int32).with(S.protobufField, 2),
});

decode(encode({ count: 0 })); // { count: 0 }
decode(encode({ count: 0, limit: 0 })); // { count: 0, limit: 0 }
```

A nested message that isn't `S.optional` decodes to its empty value when it's
missing.

## Lists and maps

`S.array` is a repeated field. `S.record` is a map with `string` keys; set
`key` to use another key type:

```ts
S.schema({
  scores: S.array(S.int32).with(S.protobufField, 1), // repeated int32
  counts: S.record(S.int32).with(S.protobufField, 2), // map<string, int32>
  byId: S.record(User).with(S.protobufField, { number: 3, key: "int64" }), // map<int64, User>
});
```

Repeated numbers are packed. Set `packed: false` to write one tag per item.

## Oneof

A union of objects is a oneof. Each arm holds one numbered value, and a literal
names the arm:

```ts
const User = S.schema({
  id: S.int32.with(S.protobufField, 1),
  contact: S.union([
    S.schema({ type: "email", email: S.string.with(S.protobufField, 2) }),
    S.schema({ type: "phone", phone: S.bigint.with(S.protobufField, { number: 3, type: "int64" }) }),
    S.schema({ type: "none" }),
  ]),
});

S.encodeOrThrow(User, S.protobuf)({ id: 1, contact: { type: "email", email: "a@b" } });
// Uint8Array [8, 1, 18, 3, 97, 64, 98]
S.decodeOrThrow(S.protobuf, User)(new Uint8Array([8, 1]));
// { id: 1, contact: { type: "none" } }
```

protobuf-es and [generated schemas](#generating-schemas-from-proto) spell the
same oneof as `{ case, value }`:

```ts
S.optional(S.union([
  S.schema({ case: "email", value: S.string.with(S.protobufField, 2) }),
  S.schema({ case: "phone", value: S.bigint.with(S.protobufField, { number: 3, type: "int64" }) }),
]));
// no member set: undefined
```

An arm can also be a tuple, or the numbered value itself when no two arms share a
runtime type:

```ts
S.union([
  S.tuple(["email", S.string.with(S.protobufField, 2)]),
  S.tuple(["phone", S.string.with(S.protobufField, 3)]),
  S.tuple(["none"]),
]);
// ["email", "a@b"]

S.optional(S.union([
  S.string.with(S.protobufField, 2),
  S.int32.with(S.protobufField, 3),
]));
// "a@b", 7 or undefined
```

Two arms the same type need a literal beside the value: a 64-bit field held as a
string next to a string field, or two messages, are refused.

It needs an arm for no member set, or `S.optional`:

```ts
const Contact = S.schema({
  contact: S.union([
    S.schema({ type: "email", email: S.string.with(S.protobufField, 2) }),
    S.schema({ type: "phone", phone: S.string.with(S.protobufField, 3) }),
  ]),
});
S.encodeOrThrow(Contact, S.protobuf);
// throws: oneof "contact" has no arm for no member set. Add one holding only its
// literal, such as S.schema({ type: "none" }), or wrap the union in S.optional
```

A flat oneof is optional fields sharing a `oneof` name:

```ts
const Message = S.schema({
  text: S.optional(S.string).with(S.protobufField, { number: 1, oneof: "value" }),
  count: S.optional(S.int32).with(S.protobufField, { number: 2, oneof: "value" }),
});
S.encodeOrThrow(Message, S.protobuf)({ text: "a", count: 1 }); // throws: two members set
```

## Recursive messages

Use [`S.recursive`](./js-usage.md#recursive-schemas):

```ts
type Category = { name: string; children: Category[] };

const Category = S.recursive<Category>("Category", (self) =>
  S.schema({
    name: S.string.with(S.protobufField, 1),
    children: S.array(self).with(S.protobufField, 2),
  }),
);
```

## Well-known types

Google's `google.protobuf.*` types are supported. `S.date` and `S.json` pick
theirs automatically. For the others, set `type`:

```ts
const Job = S.schema({
  at: S.date.with(S.protobufField, 1),
  payload: S.json.with(S.protobufField, 2),
  timeout: S.schema({ seconds: S.bigint, nanos: S.int32 }).with(S.protobufField, {
    number: 3,
    type: "google.protobuf.Duration",
  }),
  retries: S.optional(S.int32).with(S.protobufField, {
    number: 4,
    type: "google.protobuf.Int32Value",
  }),
});
```

| `type`                          | Schema                                                         |
| ------------------------------- | -------------------------------------------------------------- |
| `google.protobuf.Timestamp`     | `S.date`, or `S.schema({ seconds: S.bigint, nanos: S.int32 })` |
| `google.protobuf.Duration`      | `S.schema({ seconds: S.bigint, nanos: S.int32 })`              |
| `google.protobuf.Value`         | `S.json`                                                       |
| `google.protobuf.Struct`        | `S.record(S.json)`                                             |
| `google.protobuf.ListValue`     | `S.array(S.json)`                                              |
| `google.protobuf.FieldMask`     | `S.array(S.string)`                                            |
| `google.protobuf.Empty`         | `S.schema({})`                                                 |
| `google.protobuf.Int32Value`, `StringValue` and the other wrappers | `S.optional(S.int32)`, `S.optional(S.string)`, ... |

A `Date` holds milliseconds, so use the `{ seconds, nanos }` form when you need
nanoseconds. Inside `S.array` or `S.record`, a wrapper's item doesn't need
`S.optional`.

## Any

`S.protobufAny` is a `google.protobuf.Any`: a message of any type, as its type
URL and its bytes. Name each message with `S.protobufTypeName`, and convert the
Any to the one it holds, or to an `S.union` of them, each tagged by a literal of
your choosing:

```ts
const User = S.schema({ id: S.int32.with(S.protobufField, 1) }).with(S.protobufTypeName, "acme.v1.User");
const Order = S.schema({ total: S.bigint.with(S.protobufField, 1) }).with(S.protobufTypeName, "acme.v1.Order");

const Payload = S.protobufAny.with(S.to, S.union([
  S.schema({ type: "user", value: User }),
  S.schema({ type: "order", value: Order }),
]));

S.decodeOrThrow(Payload)({ typeUrl: "type.googleapis.com/acme.v1.Order", value: new Uint8Array([8, 5]) });
// { type: "order", value: { total: 5n } }
S.encodeOrThrow(Payload)({ type: "user", value: { id: 1 } });
// { typeUrl: "type.googleapis.com/acme.v1.User", value: Uint8Array [8, 1] }
```

The type is the name after the URL's last `/`, so `example.com/acme.v1.Order`
reads as an order too. A type the union doesn't list fails, so the union is also
the list of types you accept. To keep the rest, add a member holding
`S.protobufAny` itself:

```ts
S.schema({ type: "other", value: S.protobufAny });
// { type: "other", value: { typeUrl: "type.googleapis.com/acme.v1.Refund", value: Uint8Array } }
```

A member can be a tuple too, or the message itself when no other member is an
object:

```ts
S.union([S.tuple(["user", User]), S.tuple(["order", Order])]);
// ["order", { total: 5n }]
```

It works anywhere an Any arrives as `{ typeUrl, value }`: a gRPC error detail,
a row of an outbox table, an `Any` from protobuf-es. Inside a message you number
what it converts to, and a union of messages is an Any without saying so. A field
that always holds one type is that message, typed as an Any:

```ts
const Event = S.schema({
  payload: S.union([
    S.schema({ type: "user", value: User }),
    S.schema({ type: "order", value: Order }),
  ]).with(S.protobufField, 1),
  author: User.with(S.protobufField, { number: 2, type: "google.protobuf.Any" }),
});
```

## Generating a `.proto`

`S.toProtoOrThrow` prints the `.proto` file for a message schema, so other
languages can generate code from it:

```ts
const Address = S.schema({
  street: S.string.with(S.protobufField, 1),
}).with(S.meta, { name: "Address" });

const Customer = S.schema({
  id: S.int32.with(S.protobufField, 1),
  homeAddress: S.optional(Address).with(S.protobufField, 2),
  kind: S.union([0, 1, 2]).with(S.protobufField, 3),
}).with(S.meta, { name: "Customer" });

S.toProtoOrThrow(Customer, { package: "acme.v1" });
```

```proto
syntax = "proto3";

package acme.v1;

message Customer {
  enum Kind {
    KIND_UNSPECIFIED = 0;
    KIND_1 = 1;
    KIND_2 = 2;
  }

  int32 id = 1;
  optional Address home_address = 2;
  Kind kind = 3;
}

message Address {
  string street = 1;
}
```

- The `name` meta names a message. Without it, the field name does.
- The `description` meta prints as a comment, and `deprecated` as
  `[deprecated = true]`.
- Field names print in snake_case, as the proto3 style guide asks. The wire
  only uses field numbers, so this changes nothing about the bytes.
- Well-known types print as imports, and so does a message or enum
  `S.protobufTypeName` names `google.protobuf.X`, as every schema in
  `sury/protobuf/wkt` is.

## Unknown fields and errors

Fields your schema doesn't declare are skipped, so a sender can add fields
without breaking you. They are not kept, so decoding and then encoding drops
them. Use `S.strict` on the message to throw on them instead.

Bad input throws an `S.Error` that points to the field, and so does a value
its field's type can't hold when encoding:

```ts
const Account = S.schema({
  id: S.int32.with(S.protobufField, 1),
  address: S.optional(Address).with(S.protobufField, 2),
  tags: S.array(S.string).with(S.protobufField, 3),
});

S.decodeOrThrow(S.protobuf, Account)(new Uint8Array([8, 1, 18, 3, 10, 1, 255]));
// S.Error: Failed at address.street: Protobuf string is not valid UTF-8

S.decodeOrThrow(S.protobuf, Account)(new Uint8Array([26, 1, 97, 26, 1, 255]));
// S.Error: Failed at tags[1]: Protobuf string is not valid UTF-8

S.decodeOrThrow(Account, S.protobuf)({ id: 2 ** 40, tags: [] });
// S.Error: Failed at id: Expected int32, received 1099511627776
```

A schema that can't be a message, such as a field without a number or two
fields with the same number, throws when you create the operation.

## `ArrayBuffer` output

Encoded bytes are a view into a shared buffer, like a Node `Buffer`. To get an
`ArrayBuffer` of its own, for example to transfer it to a worker, convert
through `S.arrayBuffer`:

```ts
S.encodeOrThrow(User, S.protobuf.with(S.to, S.arrayBuffer))(user); // ArrayBuffer
S.decodeOrThrow(S.arrayBuffer.with(S.to, S.protobuf), User)(buffer);
```

## Generating schemas from `.proto`

`protoc-gen-sury` ships in the `sury` package: a protoc plugin, run by
[`buf generate`](https://buf.build/docs/generate/overview/) or `protoc`, that
writes a TypeScript module per `.proto` file.

```yaml
# buf.gen.yaml
version: v2
plugins:
  - local: protoc-gen-sury
    out: src/gen
    opt: target=ts
```

```proto
// proto/acme/v1/user.proto
syntax = "proto3";
package acme.v1;

message User {
  int32 id = 1;
  string first_name = 2;
  oneof contact {
    string email = 3;
    string phone = 4;
  }
}
```

```ts
// src/gen/acme/v1/user_pb.ts
export type User = {
  id: number;
  firstName: string;
  contact:
    | { case: "email"; value: string }
    | { case: "phone"; value: string }
    | { case?: undefined; value?: undefined };
};

export const UserSchema = S.meta(S.schemaOf<User>()({ /* one S.protobufField per field */ }), { name: "User" });
```

```ts
import * as S from "sury";
import { UserSchema } from "./gen/acme/v1/user_pb";

const bytes = S.encodeOrThrow(UserSchema, S.protobuf)({ id: 1, firstName: "Ada", contact: { case: undefined } });
```

The names and types are protoc-gen-es's: `User` and `UserSchema`, a nested
type `User_Address`, a field's lowerCamelCase property, an enum as a const
object with its prefix dropped (`PhoneType.MOBILE`), a wrapper field as its
scalar, a Struct as a JSON object, a map with 32-bit keys still string-keyed.
A well-known type is imported from `sury/protobuf/wkt` (`TimestampSchema`,
`Timestamp`), where the generated code expects it. A field holding one keeps
protobuf-es's shape, and `S.toProtoOrThrow` prints it as an import. Every
generated message and enum carries its full name (`S.protobufTypeName`), so it
can go straight into an [Any](#any)'s union. A generated Any field holds
`AnySchema`, which is `S.protobufAny`, so it keeps protobuf-es's
`{ typeUrl, value }` until you convert it with `S.to`.

The options:

| Option | Values | Default |
| --- | --- | --- |
| `target` | `ts`, `res`, `ts+res` | `ts` |
| `import_extension` | `none`, `js`, `ts` | `none` |
| `keep_empty_files` | `true`, `false` | `false` |
| `ts_nocheck` | `true`, `false` | `false` |

`import_extension=js` is what a `"moduleResolution": "nodenext"` project
needs. A field with `[jstype = JS_STRING]` is a string.

What differs from protobuf-es:

- A message is a plain object: no `$typeName`, no `create()`. A literal spells
  every field the type requires.
- Unknown fields are skipped, not kept in `$unknown` (see above).
- Enums are closed: `PhoneType` is the union of its values, where protobuf-es
  adds `UnknownEnum`. A number the enum doesn't list still decodes as that
  number, and fails a `parse`.
- A oneof's unset arm types as `{ case?: undefined }`; protobuf-es writes
  `{ case: undefined }`. Its values are the same.
- A map keyed by a 32-bit integer is `{ [key: string]: V }`, not
  `{ [key: number]: V }`.

proto2, editions, extensions and services are refused, naming the file or
field. Every generated schema is an ordinary Sury schema: validate with it,
convert it to JSON Schema, or print it back with `S.toProtoOrThrow`.

## Performance and compatibility

`S.protobuf` passes 696 of the 698 binary proto3 cases of Google's conformance
suite, and so does the schema protoc-gen-sury generates from the suite's
`.proto`. The two it fails need unknown fields kept through a round trip. JSON and
text formats, proto2 and extensions aren't supported.

See [Benchmarks: Protobuf](https://github.com/DZakh/sury/blob/main/docs/benchmarks/protobuf.md)
for bundle size and speed against protobuf.js, protobuf-es and pbf.
