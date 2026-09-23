open Vitest

// The ReScript binding's own surface: `protobufField` adapts a positional
// number and four labelled options into the JS options object, and each one
// has to arrive. The codec itself is covered by S_protobuf_test.ts and the
// compliance suite.

type user = {
  id: int,
  name: string,
  tags: array<string>,
  nums: array<int>,
  byId: dict<string>,
  pick: option<string>,
}

let userSchema = S.object(s => {
  id: s.field("id", S.int->S.protobufField(1)),
  name: s.field("name", S.string->S.protobufField(2)),
  tags: s.field("tags", S.array(S.string)->S.protobufField(3)),
  nums: s.field("nums", S.array(S.int)->S.protobufField(4, ~packed=false)),
  byId: s.field("byId", S.dict(S.string)->S.protobufField(5, ~key=#int64)),
  pick: s.field("pick", S.option(S.string)->S.protobufField(6, ~oneof="choice")),
})

test("protobufField passes every argument through to the wire", t => {
  let value = {
    id: 150,
    name: "Ada",
    tags: ["ml"],
    nums: [1, 2],
    byId: dict{"7": "x"},
    pick: Some("p"),
  }
  let bytes = value->S.convertOrThrow(~from=userSchema, ~to=S.protobuf)
  t->Assert.deepEqual(bytes->S.convertOrThrow(~from=S.protobuf, ~to=userSchema), value)

  // `~packed=false` writes field 4 expanded, a tag per item, against one tag
  // and a length for the packed run. Three items is where that starts to
  // cost: at two the two forms are the same size.
  let packedSchema = S.object(s => s.field("nums", S.array(S.int)->S.protobufField(4)))
  let unpackedSchema = S.object(s =>
    s.field("nums", S.array(S.int)->S.protobufField(4, ~packed=false))
  )
  let sizeOf = schema =>
    (([1, 2, 3]->S.convertOrThrow(~from=schema, ~to=S.protobuf))->Obj.magic)["byteLength"]
  t->Assert.is(sizeOf(unpackedSchema), sizeOf(packedSchema) + 1)
})

test("protobufField infers the wire type when type_ is left off", t => {
  let inferred = S.string->S.protobufField(1)
  let stated = S.string->S.protobufField(1, ~type_=#string)
  let of_ = schema => S.object(s => s.field("a", schema))->S.toProtoOrThrow(~name="M")
  t->Assert.deepEqual(of_(inferred), of_(stated))
  t->Assert.deepEqual(
    of_(S.int->S.protobufField(1, ~type_=#sint32)),
    `syntax = "proto3";\n\nmessage M {\n  sint32 a = 1;\n}\n`,
  )
})

test("toProtoOrThrow names the message and the package", t => {
  t->Assert.deepEqual(
    userSchema->S.toProtoOrThrow(~name="User", ~package="acme.v1"),
    `syntax = "proto3";

package acme.v1;

message User {
  int32 id = 1;
  string name = 2;
  repeated string tags = 3;
  repeated int32 nums = 4 [packed = false];
  map<int64, string> by_id = 5;
  oneof choice {
    string pick = 6;
  }
}
`,
  )
})

// The running example of docs/rescript-usage.md's Protocol Buffers section,
// so the guide cannot drift from what the bindings do: every number and every
// line of proto printed there is asserted here.

type guideAddress = {street: string}
type guideUser = {
  id: int,
  name: string,
  tags: array<string>,
  home: option<guideAddress>,
  kind: int,
}

let guideAddressSchema = S.schema(s => {street: s.matches(S.string->S.protobufField(1))})

let guideUserSchema = S.schema(s => {
  id: s.matches(S.int->S.protobufField(1)),
  name: s.matches(S.string->S.protobufField(2)),
  tags: s.matches(S.array(S.string)->S.protobufField(3)),
  home: s.matches(S.option(guideAddressSchema)->S.protobufField(4)),
  kind: s.matches(S.enum([1, 2])->S.protobufField(5, ~type_=#enum)),
})->S.meta({name: "User"})

test("the guide's message round-trips, hoists and parses from unknown", t => {
  let value = {id: 150, name: "Ada", tags: ["ml"], home: Some({street: "Main"}), kind: 2}
  let bytes = value->S.convertOrThrow(~from=guideUserSchema, ~to=S.protobuf)
  t->Assert.is(((bytes->Obj.magic)["byteLength"]: int), 22)
  t->Assert.deepEqual(bytes->S.convertOrThrow(~from=S.protobuf, ~to=guideUserSchema), value)

  let decode = S.compileConvertOrThrow(~from=S.protobuf, ~to=guideUserSchema)
  t->Assert.deepEqual(decode(bytes), value)

  // `S.protobuf->S.to(schema)` is one codec, so parse checks the bytes are a
  // Uint8Array before it reads anything.
  t->Assert.deepEqual(
    bytes->Obj.magic->S.parseOrThrow(~to=S.protobuf->S.to(guideUserSchema)),
    value,
  )
  t->U.assertThrowsMessage(
    () => "not bytes"->S.parseOrThrow(~to=S.protobuf->S.to(guideUserSchema))->ignore,
    `Expected Uint8Array, received "not bytes"`,
  )
})

test("the guide's printed .proto", t => {
  t->Assert.deepEqual(
    guideUserSchema->S.toProtoOrThrow(~package="acme.v1"),
    `syntax = "proto3";

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
`,
  )
})

test("the guide's wire error and its S.object warning", t => {
  t->U.assertThrowsMessage(
    () =>
      %raw(`new Uint8Array([8, 1, 34, 3, 10, 1, 255])`)->S.convertOrThrow(
        ~from=S.protobuf,
        ~to=guideUserSchema,
      )->ignore,
    "protobuf string is not valid UTF-8 at home.street (field 1, wire type 2)",
  )

  // `S.object` builds the record out of named JS fields, which is a `.to`, so
  // the message sits on the far side of a conversion a nested field can't take.
  let objectAddress = S.object(s => {
    street: s.field("street", S.string->S.protobufField(1)),
  })
  let nested = S.schema(s => {
    id: s.matches(S.int->S.protobufField(1)),
    name: s.matches(S.string->S.protobufField(2)),
    tags: s.matches(S.array(S.string)->S.protobufField(3)),
    home: s.matches(S.option(objectAddress)->S.protobufField(4)),
    kind: s.matches(S.enum([1, 2])->S.protobufField(5, ~type_=#enum)),
  })
  t->Assert.throws(
    () => nested->S.toProtoOrThrow,
    ~expectations={
      message: `[Sury] S.protobuf: field "home" is a message that converts further with S.to, which a nested field can't`,
    },
  )
})

// The shapes protoc-gen-sury writes for ReScript: an enum is a variant whose
// constructors carry their proto numbers, and a oneof is a variant tagged by
// `case` under `option`, which is the `{ case, value }` object on the wire
// side and `undefined` while no member is set.
@val external bytesToArray: 'a => array<int> = "Array.from"

type kind = | @as(0) Unspecified | @as(1) Primary | @as(2) Secondary

type address = {street: string}

@tag("case")
type contact =
  | @as("email") Email({value: string})
  | @as("home") Home({value: address})

type person = {id: int, kind: kind, contact: option<contact>}

let addressSchema = S.schema(s => {
  street: s.matches(S.string->S.protobufField(1, ~type_=#string)),
})

let personSchema = S.schema(s => {
  id: s.matches(S.int->S.protobufField(1, ~type_=#int32)),
  kind: s.matches(S.enum([Unspecified, Primary, Secondary])->S.protobufField(2, ~type_=#enum)),
  contact: s.matches(
    S.option(
      S.union([
        S.schema(s => Email({value: s.matches(S.string->S.protobufField(3, ~type_=#string))})),
        S.schema(s => Home({value: s.matches(addressSchema->S.protobufField(4, ~type_=#message))})),
      ]),
    ),
  ),
})

test("a variant enum travels as its number and a tagged variant as a oneof", t => {
  let toBytes = value => value->S.convertOrThrow(~from=personSchema, ~to=S.protobuf)
  let fromBytes = bytes => bytes->S.convertOrThrow(~from=S.protobuf, ~to=personSchema)
  let email = {id: 1, kind: Secondary, contact: Some(Email({value: "a@b"}))}
  t->Assert.deepEqual(toBytes(email)->bytesToArray, [8, 1, 16, 2, 26, 3, 97, 64, 98])
  t->Assert.deepEqual(fromBytes(toBytes(email)), email)
  let home = {id: 2, kind: Unspecified, contact: Some(Home({value: {street: "x"}}))}
  t->Assert.deepEqual(fromBytes(toBytes(home)), home)
  let none = {id: 3, kind: Primary, contact: None}
  t->Assert.deepEqual(toBytes(none)->bytesToArray, [8, 3, 16, 1])
  t->Assert.deepEqual(fromBytes(toBytes(none)), none)
  t->Assert.deepEqual(
    personSchema->S.toProtoOrThrow(~name="Person"),
    `syntax = "proto3";

message Person {
  enum Kind {
    KIND_UNSPECIFIED = 0;
    KIND_1 = 1;
    KIND_2 = 2;
  }
  message Home {
    string street = 1;
  }

  int32 id = 1;
  Kind kind = 2;
  oneof contact {
    string email = 3;
    Home home = 4;
  }
}
`,
  )
})
