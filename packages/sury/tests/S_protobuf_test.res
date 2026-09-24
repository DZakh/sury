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
  kind: s.matches(S.enum([1, 2])->S.protobufField(5)),
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
    kind: s.matches(S.enum([1, 2])->S.protobufField(5)),
  })
  t->Assert.throws(
    () => nested->S.toProtoOrThrow,
    ~expectations={
      message: `[Sury] S.protobuf: field "home" is a message that converts further with S.to, which a nested field can't`,
    },
  )
})

type duration = {seconds: bigint, nanos: int}
type job = {
  at: Date.t,
  payload: JSON.t,
  timeout: duration,
  retries: option<int>,
  labels: dict<JSON.t>,
  mask: array<string>,
}

let jobSchema = S.schema(s => {
  at: s.matches(S.date->S.protobufField(1)),
  payload: s.matches(S.json->S.protobufField(2)),
  timeout: s.matches(
    S.schema(s => {seconds: s.matches(S.bigint), nanos: s.matches(S.int)})->S.protobufField(
      3,
      ~type_=#"google.protobuf.Duration",
    ),
  ),
  retries: s.matches(S.option(S.int)->S.protobufField(4, ~type_=#"google.protobuf.Int32Value")),
  labels: s.matches(S.dict(S.json)->S.protobufField(5, ~type_=#"google.protobuf.Struct")),
  mask: s.matches(S.array(S.string)->S.protobufField(6, ~type_=#"google.protobuf.FieldMask")),
})->S.meta({name: "Job"})

test("well-known types from ReScript", t => {
  let value = {
    at: Date.fromTime(1700000000123.),
    payload: JSON.Array([JSON.Number(1.), JSON.String("x"), JSON.Null]),
    timeout: {seconds: 30n, nanos: 500},
    retries: Some(0),
    labels: dict{"env": JSON.String("prod")},
    mask: ["user.name"],
  }
  let bytes = value->S.convertOrThrow(~from=jobSchema, ~to=S.protobuf)
  t->Assert.deepEqual(bytes->S.convertOrThrow(~from=S.protobuf, ~to=jobSchema), value)

  let unset = {...value, retries: None}
  let bytes = unset->S.convertOrThrow(~from=jobSchema, ~to=S.protobuf)
  t->Assert.deepEqual(bytes->S.convertOrThrow(~from=S.protobuf, ~to=jobSchema), unset)

  t->Assert.deepEqual(
    jobSchema->S.toProtoOrThrow,
    `syntax = "proto3";

import "google/protobuf/duration.proto";
import "google/protobuf/field_mask.proto";
import "google/protobuf/struct.proto";
import "google/protobuf/timestamp.proto";
import "google/protobuf/wrappers.proto";

message Job {
  google.protobuf.Timestamp at = 1;
  google.protobuf.Value payload = 2;
  google.protobuf.Duration timeout = 3;
  optional google.protobuf.Int32Value retries = 4;
  google.protobuf.Struct labels = 5;
  google.protobuf.FieldMask mask = 6;
}
`,
  )
})

type rec category = {name: string, children: array<category>}

let categorySchema = S.recursive("Category", categorySchema =>
  S.schema(s => {
    name: s.matches(S.string->S.protobufField(1)),
    children: s.matches(S.array(categorySchema)->S.protobufField(2)),
  })
)

test("the guide's recursive message", t => {
  let value = {name: "a", children: [{name: "b", children: []}]}
  let bytes = value->S.convertOrThrow(~from=categorySchema, ~to=S.protobuf)
  t->Assert.deepEqual(bytes->S.convertOrThrow(~from=S.protobuf, ~to=categorySchema), value)
  t->Assert.deepEqual(
    categorySchema->S.toProtoOrThrow,
    `syntax = "proto3";

message Category {
  string name = 1;
  repeated Category children = 2;
}
`,
  )
})
