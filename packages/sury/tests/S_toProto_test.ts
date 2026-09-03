import { test } from "vitest";

import * as S from "../index.mjs";

test("toProto prints every field shape the wire supports", (t) => {
  const Address = S.schema({
    street: S.string.with(S.protobufField, 1),
    zip: S.optional(S.string).with(S.protobufField, 2),
  }).with(S.meta, { name: "Address", description: "Postal address" });
  const User = S.schema({
    id: S.string.with(S.protobufField, { number: 1, type: "uint32" }),
    userName: S.string.with(S.protobufField, 2).with(S.meta, { description: "Display name\nfor the UI", deprecated: true }),
    tags: S.array(S.string).with(S.protobufField, 3),
    nums: S.array(S.int32).with(S.protobufField, { number: 4, packed: false }),
    home: Address.with(S.protobufField, 5),
    work: S.optional(Address).with(S.protobufField, 6),
    kind: S.union([1, 2, 3]).with(S.protobufField, { number: 7, type: "enum" }),
    mode: S.optional(S.union([0, -1])).with(S.protobufField, { number: 8, type: "enum" }),
    byId: S.record(Address).with(S.protobufField, { number: 9, key: "int64" }),
    text: S.optional(S.string).with(S.protobufField, { number: 10, oneof: "value" }),
    inline: S.schema({ n: S.int32.with(S.protobufField, { number: 1, type: "sint32" }) }).with(S.protobufField, 11),
    count: S.optional(S.int32).with(S.protobufField, { number: 12, oneof: "value" }),
    "weird-key": S.boolean.with(S.protobufField, 13),
    hash: S.bigint.with(S.protobufField, { number: 14, type: "fixed64" }),
  }).with(S.meta, { name: "User" });
  t.expect(S.toProto(User, { package: "acme.v1" })).toBe(`syntax = "proto3";

package acme.v1;

message User {
  enum Kind {
    KIND_UNSPECIFIED = 0;
    KIND_1 = 1;
    KIND_2 = 2;
    KIND_3 = 3;
  }
  enum Mode {
    MODE_UNSPECIFIED = 0;
    MODE_MINUS_1 = -1;
  }
  message Inline {
    sint32 n = 1;
  }

  uint32 id = 1;
  // Display name
  // for the UI
  string user_name = 2 [deprecated = true];
  repeated string tags = 3;
  repeated int32 nums = 4 [packed = false];
  Address home = 5;
  optional Address work = 6;
  Kind kind = 7;
  optional Mode mode = 8;
  map<int64, Address> by_id = 9;
  oneof value {
    string text = 10;
    int32 count = 12;
  }
  Inline inline = 11;
  bool weird_key = 13;
  fixed64 hash = 14;
}

// Postal address
message Address {
  string street = 1;
  optional string zip = 2;
}
`);
});

test("toProto names the root from the option, then meta, then Message", (t) => {
  const Point = S.schema({ x: S.int32.with(S.protobufField, 1) });
  t.expect(S.toProto(Point)).toBe(`syntax = "proto3";

message Message {
  int32 x = 1;
}
`);
  t.expect(S.toProto(Point.with(S.meta, { name: "point 2d" }))).toContain("message Point2d {");
  t.expect(S.toProto(Point, { name: "Pt" })).toContain("message Pt {");
  // The option is taken as given, so it names what a consumer looks up.
  t.expect(() => S.toProto(Point, { name: "user_v2.Thing" })).toThrow('[Sury] S.toProto: "user_v2.Thing" is not a message name');
});

test("toProto nests an unnamed message where it is first used and qualifies later references", (t) => {
  const Inner = S.schema({ n: S.int32.with(S.protobufField, 1) });
  const Left = S.schema({ inner: Inner.with(S.protobufField, 1) });
  const Right = S.schema({ inner: Inner.with(S.protobufField, 1) });
  const Root = S.schema({ left: Left.with(S.protobufField, 1), right: Right.with(S.protobufField, 2) });
  t.expect(S.toProto(Root)).toBe(`syntax = "proto3";

message Message {
  message Left {
    message Inner {
      int32 n = 1;
    }

    Inner inner = 1;
  }
  message Right {
    Message.Left.Inner inner = 1;
  }

  Left left = 1;
  Right right = 2;
}
`);
});

test("toProto disambiguates two named schemas that share a name", (t) => {
  const A = S.schema({ a: S.int32.with(S.protobufField, 1) }).with(S.meta, { name: "Item" });
  const B = S.schema({ b: S.int32.with(S.protobufField, 1) }).with(S.meta, { name: "Item" });
  const proto = S.toProto(S.schema({ a: A.with(S.protobufField, 1), b: B.with(S.protobufField, 2) }));
  t.expect(proto).toContain("  Item a = 1;\n  Item2 b = 2;");
  t.expect(proto).toContain("message Item2 {");
});

test("toProto rejects what the wire rejects, with the same message", (t) => {
  t.expect(() => S.toProto(S.string)).toThrow("[Sury] S.toProto: the schema is not an object");
  t.expect(() =>
    S.toProto(
      S.recursive("Node", (self) =>
        S.schema({ v: S.int32.with(S.protobufField, 1), next: S.optional(self).with(S.protobufField, { number: 2, type: "message" }) }),
      ),
    ),
  ).toThrow("[Sury] S.toProto: a recursive message can't be printed, as S.protobuf can't encode one");
  t.expect(() => S.toProto(S.schema({ a: S.string }))).toThrow(
    '[Sury] S.protobuf: field "a" has no field number. Give it one with S.protobufField',
  );
});

test("toProto keeps a nested type from shadowing a top-level one", (t) => {
  const Address = S.schema({ street: S.string.with(S.protobufField, 1) }).with(S.meta, { name: "Address" });
  const proto = S.toProto(
    S.schema({
      address: S.schema({ raw: S.string.with(S.protobufField, 1) }).with(S.protobufField, 1),
      billing: Address.with(S.protobufField, 2),
    }),
  );
  t.expect(proto).toContain("  message Address2 {\n    string raw = 1;\n  }");
  t.expect(proto).toContain("  Address2 address = 1;\n  Address billing = 2;");
  t.expect(proto).toContain("\nmessage Address {\n  string street = 1;\n}");
});

test("toProto prints the root once when a field reuses it", (t) => {
  const Root = S.schema({ n: S.int32.with(S.protobufField, 1) }).with(S.meta, { name: "Root" });
  const proto = S.toProto(
    S.schema({ a: Root.with(S.protobufField, 1), b: S.schema({ back: Root.with(S.protobufField, 1) }).with(S.protobufField, 2) }).with(
      S.meta,
      { name: "Root" },
    ),
  );
  t.expect(proto).toBe(`syntax = "proto3";

message Root {
  message B {
    Root2 back = 1;
  }

  Root2 a = 1;
  B b = 2;
}

message Root2 {
  int32 n = 1;
}
`);
});

test("toProto tells a field's description from its message's", (t) => {
  const Address = S.schema({ street: S.string.with(S.protobufField, 1) }).with(S.meta, { name: "Address", description: "Postal" });
  // Meta layered on after the field number is the field's.
  const both = S.toProto(
    S.schema({
      home: Address.with(S.protobufField, 1).with(S.meta, { description: "Home address" }),
      work: Address.with(S.protobufField, 2).with(S.meta, { description: "Work address" }),
    }),
  );
  t.expect(both).toContain("  // Home address\n  Address home = 1;\n  // Work address\n  Address work = 2;\n}\n\n// Postal\nmessage Address {");
  // Meta the schema carried before the number is the type's, wherever set.
  const before = S.toProto(S.schema({ home: Address.with(S.meta, { description: "Renamed" }).with(S.protobufField, 1) }));
  t.expect(before).toContain("{\n  Address home = 1;\n}\n\n// Renamed\nmessage Address {");
  // A use past S.optional reaches the declaration, which settles whose is whose.
  const one = S.toProto(
    S.schema({
      home: Address.with(S.protobufField, 1).with(S.meta, { description: "Home address" }),
      work: S.optional(Address).with(S.protobufField, 2),
    }),
  );
  t.expect(one).toContain("{\n  // Home address\n  Address home = 1;\n  optional Address work = 2;\n}\n\n// Postal\nmessage Address {");
  const direct = S.toProto(S.schema({ home: Address.with(S.protobufField, 1) }));
  t.expect(direct).toContain("{\n  Address home = 1;\n}\n\n// Postal\nmessage Address {");
});

test("toProto prints a deprecated message as the type's option, not its fields'", (t) => {
  const Old = S.schema({ x: S.int32.with(S.protobufField, 1) }).with(S.meta, { name: "Old", deprecated: true });
  const proto = S.toProto(
    S.schema({ a: Old.with(S.protobufField, 1), b: S.optional(Old).with(S.protobufField, 2), c: S.int32.with(S.protobufField, 3).with(S.meta, { deprecated: true }) }),
  );
  t.expect(proto).toBe(`syntax = "proto3";

message Message {
  Old a = 1;
  optional Old b = 2;
  int32 c = 3 [deprecated = true];
}

message Old {
  option deprecated = true;
  int32 x = 1;
}
`);
});

test("toProto keeps a field's name and its inline type apart", (t) => {
  const proto = S.toProto(S.schema({ Name: S.schema({ x: S.int32.with(S.protobufField, 1) }).with(S.protobufField, 1) }));
  t.expect(proto).toContain("  message Name2 {\n    int32 x = 1;\n  }\n\n  Name2 Name = 1;");
});

test("toProto gives each unnamed enum field its own type and dedupes a repeated literal", (t) => {
  const proto = S.toProto(
    S.schema({
      kind: S.union([1, 1, 2]).with(S.protobufField, { number: 1, type: "enum" }),
      status: S.union([1, 2]).with(S.protobufField, { number: 2, type: "enum" }),
    }),
  );
  t.expect(proto).toBe(`syntax = "proto3";

message Message {
  enum Kind {
    KIND_UNSPECIFIED = 0;
    KIND_1 = 1;
    KIND_2 = 2;
  }
  enum Status {
    STATUS_UNSPECIFIED = 0;
    STATUS_1 = 1;
    STATUS_2 = 2;
  }

  Kind kind = 1;
  Status status = 2;
}
`);
  const Named = S.union([0, 1]).with(S.meta, { name: "Flag" });
  const shared = S.toProto(
    S.schema({ a: Named.with(S.protobufField, { number: 1, type: "enum" }), b: S.optional(Named).with(S.protobufField, { number: 2, type: "enum" }) }),
  );
  t.expect(shared).toContain("  Flag a = 1;\n  optional Flag b = 2;\n}\n\nenum Flag {\n  FLAG_UNSPECIFIED = 0;\n  FLAG_1 = 1;\n}");
});

test("toProto prints a named schema under its name whichever copy is used first", (t) => {
  const Inner = S.schema({ n: S.int32.with(S.protobufField, 1) });
  const Named = Inner.with(S.meta, { name: "Inner" });
  // The unnamed copy is a type of its own; the name goes where it was asked for.
  const proto = S.toProto(S.schema({ a: Inner.with(S.protobufField, 1), b: Named.with(S.protobufField, 2) }));
  t.expect(proto).toContain("  message A {\n    int32 n = 1;\n  }\n\n  A a = 1;\n  Inner b = 2;\n}\n\nmessage Inner {");
});

test("toProto rejects two keys that print as one field name", (t) => {
  t.expect(() =>
    S.toProto(S.schema({ userName: S.string.with(S.protobufField, 1), user_name: S.string.with(S.protobufField, 2) })),
  ).toThrow('[Sury] S.toProto: "userName" and "user_name" of Message collide as "userName"');
  // protoc's rule: the default JSON name, not the spelling.
  t.expect(() => S.toProto(S.schema({ aB: S.string.with(S.protobufField, 1), a_B: S.string.with(S.protobufField, 2) }))).toThrow(
    '[Sury] S.toProto: "aB" and "a_B" of Message collide as "aB"',
  );
  t.expect(() =>
    S.toProto(S.schema({ value: S.string.with(S.protobufField, 1), text: S.optional(S.string).with(S.protobufField, { number: 2, oneof: "value" }) })),
  ).toThrow('[Sury] S.toProto: "value" and oneof "value" of Message collide as "value"');
  // A oneof is a symbol without a JSON name, so it only has to differ in spelling.
  t.expect(
    S.toProto(S.schema({ value: S.string.with(S.protobufField, 1), text: S.optional(S.string).with(S.protobufField, { number: 2, oneof: "va_lue" }) })),
  ).toContain("  string value = 1;\n  oneof va_lue {\n    string text = 2;\n  }");
});

test("toProto keeps a digit-leading name an identifier", (t) => {
  const proto = S.toProto(
    S.schema({ "2d": S.schema({ x: S.int32.with(S.protobufField, 1) }).with(S.protobufField, 1) }).with(S.meta, { name: "3kinds" }),
  );
  // Fields and nested types share a scope, so the type steps aside.
  t.expect(proto).toContain("message _3kinds {\n  message _2d2 {");
  t.expect(proto).toContain("  _2d2 _2d = 1;");
});


test("toProto attributes meta a schema carried before its number to the type", (t) => {
  const Old = S.schema({ street: S.string.with(S.protobufField, 1) }).with(S.meta, { name: "Old", deprecated: true, description: "Old doc" });
  t.expect(S.toProto(S.schema({ a: Old.with(S.protobufField, 1), b: Old.with(S.protobufField, 2) }))).toBe(`syntax = "proto3";

message Message {
  Old a = 1;
  Old b = 2;
}

// Old doc
message Old {
  option deprecated = true;
  string street = 1;
}
`);
  // A field's copy keeps what it layered on after the number.
  const proto = S.toProto(
    S.schema({
      old: Old.with(S.protobufField, 1).with(S.meta, { deprecated: false, description: "Legacy" }),
      one: S.optional(Old).with(S.protobufField, 2).with(S.meta, { description: "maybe" }),
    }),
  );
  t.expect(proto).toBe(`syntax = "proto3";

message Message {
  // Legacy
  Old old = 1;
  // maybe
  optional Old one = 2;
}

// Old doc
message Old {
  option deprecated = true;
  string street = 1;
}
`);
  // The only direct use, deprecated after numbering, deprecates the field alone.
  const single = S.toProto(S.schema({ legacy: Old.with(S.meta, { deprecated: false }).with(S.protobufField, 1).with(S.meta, { deprecated: true, description: "Old field" }) }));
  t.expect(single).toContain("  // Old field\n  Old legacy = 1 [deprecated = true];\n}\n\n// Old doc\nmessage Old {\n  string street = 1;");
});

test("toProto keeps a named flat optional union one enum", (t) => {
  const Flat = S.union([1, 2, S.void]).with(S.meta, { name: "Flat", description: "flat doc" });
  const proto = S.toProto(
    S.schema({
      a: Flat.with(S.protobufField, { number: 1, type: "enum" }),
      b: Flat.with(S.protobufField, { number: 2, type: "enum" }),
    }),
  );
  t.expect(proto).toBe(`syntax = "proto3";

message Message {
  optional Flat a = 1;
  optional Flat b = 2;
}

// flat doc
enum Flat {
  FLAT_UNSPECIFIED = 0;
  FLAT_1 = 1;
  FLAT_2 = 2;
}
`);
});

test("toProto prints a renamed copy as its own message", (t) => {
  const X = S.schema({ x: S.int32.with(S.protobufField, 1) }).with(S.meta, { name: "X" });
  const Y = X.with(S.meta, { name: "Y" });
  const proto = S.toProto(S.schema({ a: X.with(S.protobufField, 1), b: Y.with(S.protobufField, 2) }));
  t.expect(proto).toContain("  X a = 1;\n  Y b = 2;\n}\n\nmessage X {\n  int32 x = 1;\n}\n\nmessage Y {\n  int32 x = 1;\n}");
});

test("toProto gives two enums in one scope distinct value prefixes", (t) => {
  const proto = S.toProto(
    S.schema({
      userKind: S.union([1, 2]).with(S.protobufField, { number: 1, type: "enum" }),
      userkind: S.union([1, 2]).with(S.protobufField, { number: 2, type: "enum" }),
    }),
  );
  t.expect(proto).toContain("  enum UserKind {\n    USER_KIND_UNSPECIFIED = 0;\n    USER_KIND_1 = 1;\n    USER_KIND_2 = 2;\n  }");
  t.expect(proto).toContain("  enum Userkind {\n    USERKIND_UNSPECIFIED = 0;\n    USERKIND_1 = 1;\n    USERKIND_2 = 2;\n  }");
  const same = S.toProto(
    S.schema({
      a_b: S.union([1, 2]).with(S.protobufField, { number: 1, type: "enum" }),
      ab: S.union([1, 2]).with(S.protobufField, { number: 2, type: "enum" }),
    }),
  );
  t.expect(same).toContain("  enum AB {\n    AB_UNSPECIFIED = 0;\n    AB_1 = 1;\n    AB_2 = 2;\n  }\n  enum Ab {\n    AB2_UNSPECIFIED = 0;\n    AB2_1 = 1;\n    AB2_2 = 2;\n  }");
});

test("toProto prints an empty key as an identifier", (t) => {
  t.expect(S.toProto(S.schema({ "": S.string.with(S.protobufField, 1) }))).toContain("  string _ = 1;");
});

test("toProto leaves meta set after the number on an inline type's field", (t) => {
  const proto = S.toProto(
    S.schema({
      inline: S.schema({ n: S.int32.with(S.protobufField, 1) }).with(S.protobufField, 1).with(S.meta, { description: "Field doc" }),
      kind: S.union([1, 2]).with(S.protobufField, { number: 2, type: "enum" }).with(S.meta, { description: "Kind doc", deprecated: true }),
    }),
  );
  t.expect(proto).toBe(`syntax = "proto3";

message Message {
  message Inline {
    int32 n = 1;
  }
  enum Kind {
    KIND_UNSPECIFIED = 0;
    KIND_1 = 1;
    KIND_2 = 2;
  }

  // Field doc
  Inline inline = 1;
  // Kind doc
  Kind kind = 2 [deprecated = true];
}
`);
});

test("toProto keeps enum values clear of field names in the same scope", (t) => {
  const proto = S.toProto(
    S.schema({ kind: S.union([1, 2]).with(S.protobufField, { number: 1, type: "enum" }), KIND_1: S.string.with(S.protobufField, 2) }),
  );
  t.expect(proto).toContain("  enum Kind {\n    KIND2_UNSPECIFIED = 0;\n    KIND2_1 = 1;\n    KIND2_2 = 2;\n  }");
  t.expect(proto).toContain("  Kind kind = 1;\n  string KIND_1 = 2;");
});

test("toProto lets a field's schema keep the name Message over the root's default", (t) => {
  const Msg = S.schema({ n: S.int32.with(S.protobufField, 1) }).with(S.meta, { name: "Message" });
  t.expect(S.toProto(S.schema({ m: Msg.with(S.protobufField, 1) }))).toBe(`syntax = "proto3";

message Message2 {
  Message m = 1;
}

message Message {
  int32 n = 1;
}
`);
  t.expect(S.toProto(S.schema({ m: Msg.with(S.protobufField, 1) }), { name: "Message" })).toContain("message Message {\n  Message2 m = 1;");
});

test("toProto prints a comment without trailing whitespace", (t) => {
  const proto = S.toProto(S.schema({ a: S.string.with(S.protobufField, 1).with(S.meta, { description: "first  \r\n\nthird\n" }) }));
  t.expect(proto).toContain("  // first\n  //\n  // third\n  string a = 1;");
  t.expect(S.toProto(S.schema({ a: S.string.with(S.protobufField, 1).with(S.meta, { description: "  " }) }))).toContain("{\n  string a = 1;");
});

test("toProto reads meta set after a root message's .to", (t) => {
  const M = S.schema({ a: S.int32.with(S.protobufField, 1) })
    .with(S.to, S.schema({ a: S.int32.with(S.protobufField, 1) }))
    .with(S.meta, { name: "Named", description: "named doc", deprecated: true });
  t.expect(S.toProto(M)).toBe(`syntax = "proto3";

// named doc
message Named {
  option deprecated = true;
  int32 a = 1;
}
`);
});

test("toProto keeps a description only one use carries", (t) => {
  const A = S.schema({ x: S.int32.with(S.protobufField, 1) }).with(S.meta, { name: "A" });
  const proto = S.toProto(
    S.schema({ home: A.with(S.protobufField, 1), work: S.optional(A.with(S.meta, { description: "Doc" })).with(S.protobufField, 2) }),
  );
  t.expect(proto).toContain("  A home = 1;\n  optional A work = 2;\n}\n\n// Doc\nmessage A {");
});

test("toProto puts a shared unnamed type's declared description on the type", (t) => {
  const Inner = S.schema({ n: S.int32.with(S.protobufField, 1) }).with(S.meta, { description: "Inner doc" });
  t.expect(S.toProto(S.schema({ a: Inner.with(S.protobufField, 1), b: Inner.with(S.protobufField, 2) }))).toBe(`syntax = "proto3";

message Message {
  // Inner doc
  message A {
    int32 n = 1;
  }

  A a = 1;
  A b = 2;
}
`);
});

test("toProto prints a defaulted optional with explicit presence", (t) => {
  const Flag = S.union([0, 1]).with(S.meta, { name: "Flag" });
  const proto = S.toProto(
    S.schema({
      kind: S.optional(S.union([0, 1]), 0).with(S.protobufField, { number: 1, type: "enum" }),
      flag: S.optional(Flag, 0).with(S.protobufField, { number: 2, type: "enum" }),
      n: S.optional(S.int32, 5).with(S.protobufField, 3),
    }),
  );
  t.expect(proto).toContain("  optional Kind kind = 1;\n  optional Flag flag = 2;\n  optional int32 n = 3;");
});


test("toProto keeps a top-level enum's values clear of the root's name", (t) => {
  const Flag = S.union([0, 1]).with(S.meta, { name: "Flag" });
  const proto = S.toProto(S.schema({ a: Flag.with(S.protobufField, { number: 1, type: "enum" }) }), { name: "FLAG_1" });
  t.expect(proto).toContain("enum Flag {\n  FLAG2_UNSPECIFIED = 0;\n  FLAG2_1 = 1;\n}");
});

test("toProto names nested types per scope, so a sibling's inline types don't rename each other's", (t) => {
  const inline = () => S.schema({ n: S.int32.with(S.protobufField, 1) });
  const A = S.schema({ inner: inline().with(S.protobufField, 1), other: inline().with(S.protobufField, 2) }).with(S.meta, { name: "A" });
  const B = S.schema({ inner: inline().with(S.protobufField, 1) }).with(S.meta, { name: "B" });
  const proto = S.toProto(S.schema({ a: A.with(S.protobufField, 1), b: B.with(S.protobufField, 2) }));
  t.expect(proto).toContain("message A {\n  message Inner {\n    int32 n = 1;\n  }\n  message Other {");
  t.expect(proto).toContain("message B {\n  message Inner {\n    int32 n = 1;\n  }\n\n  Inner inner = 1;\n}");
});

test("toProto leaves the schemas it prints untouched", (t) => {
  const Flag = S.union([0, 1]).with(S.meta, { name: "Flag" });
  const proto = S.toProto(
    S.schema({
      a: S.optional(Flag).with(S.protobufField, { number: 1, type: "enum" }).with(S.meta, { description: "field doc", deprecated: true }),
    }),
  );
  t.expect(proto).toContain("  // field doc\n  optional Flag a = 1 [deprecated = true];\n}\n\nenum Flag {");
  t.expect(Flag.description).toBe(undefined);
  t.expect(Flag.deprecated).toBe(undefined);
  t.expect(S.toProto(S.schema({ b: S.optional(Flag).with(S.protobufField, { number: 1, type: "enum" }) }))).not.toContain("field doc");
});

test("toProto rejects a package that is not dot-separated identifiers", (t) => {
  const Point = S.schema({ x: S.int32.with(S.protobufField, 1) });
  t.expect(() => S.toProto(Point, { package: "bad package!" })).toThrow('[Sury] S.toProto: "bad package!" is not a package name');
  t.expect(S.toProto(Point, { package: "acme.v1_beta" })).toContain("package acme.v1_beta;");
});

test("toProto prints the first object of a .to chain, which the wire speaks", (t) => {
  const M = S.schema({ a: S.int32.with(S.protobufField, 1) }).with(S.to, S.schema({ b: S.string.with(S.protobufField, 1) }), {
    decode: (w) => ({ b: String(w.a) }),
    encode: (d) => ({ a: Number(d.b) }),
  });
  t.expect(S.toProto(M)).toContain("message Message {\n  int32 a = 1;\n}");
  // Object-first, the value converts to the object beside the wire first.
  t.expect(S.toProto(M.with(S.to, S.protobuf))).toContain("message Message {\n  string b = 1;\n}");
  // Wire-first, the object right after the wire.
  t.expect(S.toProto(S.protobuf.with(S.to, M))).toContain("message Message {\n  int32 a = 1;\n}");
  t.expect(S.toProto(S.arrayBuffer.with(S.to, S.protobuf).with(S.to, S.schema({ c: S.boolean.with(S.protobufField, 1) })))).toContain(
    "message Message {\n  bool c = 1;\n}",
  );
  t.expect(() => S.toProto(S.schema({ m: M.with(S.protobufField, 1) }))).toThrow(
    '[Sury] S.protobuf: field "m" is a message that converts further with S.to, which a nested field can\'t',
  );
  t.expect(S.toProto(S.schema({ a: S.int32.with(S.protobufField, 1) }).with(S.to, S.protobuf))).toContain("message Message {\n  int32 a = 1;\n}");
});

test("toProto prints a lone integer literal as the number it infers", (t) => {
  const Message = S.schema({ one: S.literal(1).with(S.protobufField, 1), kind: S.union([1, 2]).with(S.protobufField, 2) });
  t.expect(S.toProto(Message)).toContain("  double one = 1;\n  Kind kind = 2;");
});
