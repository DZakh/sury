[⬅ Back to the JavaScript API reference](./js-usage.md)

# rkyv

## Table of contents

- [Types](#types)
- [Integer and float widths](#integer-and-float-widths)
- [Options](#options)
- [Enums](#enums)
- [Recursive types](#recursive-types)
- [Refinements and transforms](#refinements-and-transforms)
- [Invalid bytes](#invalid-bytes)
- [What it doesn't store](#what-it-doesnt-store)
- [Compatibility](#compatibility)

`S.rkyv` reads and writes [rkyv](https://rkyv.org) archives, the zero-copy
format of Rust's `rkyv` crate. A schema describes the Rust type, with fields in
declaration order, so a value archived in Rust decodes in JavaScript, and the
other way around. There is no code generation step.

```rust
#[derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)]
struct User {
    id: i32,
    name: String,
    tags: Vec<String>,
    score: Option<f64>,
}

let bytes = rkyv::to_bytes::<rkyv::rancor::Error>(&user)?;
```

```ts
const User = S.schema({
  id: S.int32,
  name: S.string,
  tags: S.array(S.string),
  score: S.optional(S.number),
});

S.decodeOrThrow(S.rkyv, User)(bytes);
// { id: 150, name: "Ada", tags: ["ml"], score: undefined }
S.encodeOrThrow(User, S.rkyv)({ id: 150, name: "Ada", tags: ["ml"] });
// the bytes rkyv::to_bytes writes for the same value
```

The order of the keys is the order of the Rust fields. To get a single schema
that parses bytes from `unknown`, use `S.rkyv.with(S.to, User)`.

## Types

| Schema                                   | Rust type             |
| ---------------------------------------- | --------------------- |
| `S.string`                               | `String`              |
| `S.boolean`                              | `bool`                |
| `S.int32`                                | `i32`                 |
| `S.number`                               | `f64`                 |
| `S.integer`                              | `i64`                 |
| `S.port`                                 | `u16`                 |
| `S.bigint`                               | `i64`                 |
| `S.uint8Array`                           | `Vec<u8>`             |
| `S.schema({ ... })`                      | a struct              |
| `S.tuple([A, B])`                        | `(A, B)`              |
| `S.array(T)`                             | `Vec<T>`              |
| `S.optional(T)`, `S.nullable(T)`         | `Option<T>`           |
| `S.union(["a", "b"])`                    | `enum { A, B }`       |
| a union of objects with a literal tag    | an enum with fields   |
| a literal, `S.schema({})`                | `()`, an empty struct |

## Integer and float widths

`S.rkyvType` picks the Rust type where the schema implies another:

```ts
S.schema({
  flags: S.int32.with(S.rkyvType, "u8"),
  offset: S.number.with(S.rkyvType, "u32"),
  ratio: S.number.with(S.rkyvType, "f32"),
  hash: S.bigint.with(S.rkyvType, "u128"),
  initial: S.string.with(S.rkyvType, "char"),
});
```

A value out of the type's range fails to encode:

```ts
S.encodeOrThrow(Flags, S.rkyv)({ flags: 256, ... });
// throws: Failed at flags: Expected u8, received 256
```

`u64` and `i64` read into a number fail past `Number.MAX_SAFE_INTEGER`. Use
`S.bigint` to read every value:

```ts
S.schema({ blockNumber: S.number.with(S.rkyvType, "u64") }); // up to 2^53 - 1
S.schema({ blockNumber: S.bigint.with(S.rkyvType, "u64") }); // every u64
```

`S.number` refuses `NaN`, which an `f64` holds like any other value. Accept it
with a union:

```ts
S.union([S.number, S.schema(NaN)]);
```

## Options

`S.optional` reads `None` as `undefined`, and `S.nullable` as `null`. With both,
it is `Option<Option<T>>`: `None` is `undefined` and `Some(None)` is `null`.

```ts
S.optional(S.nullable(S.string)); // Option<Option<String>>
```

## Enums

A union of literals is an enum of unit variants, numbered in the order the
union lists them:

```rust
enum Color { Red, Green, Blue }
```

```ts
S.union(["red", "green", "blue"]);
```

A union of objects that share a literal key is an enum with fields. The literal
names the variant, and the other keys are its fields. A bare literal is a unit
variant:

```rust
enum Shape {
    Circle { r: f64 },
    Rect { w: u16, h: u16 },
    None,
}
```

```ts
S.union([
  S.schema({ type: "circle", r: S.number }),
  S.schema({ type: "rect", w: S.int32.with(S.rkyvType, "u16"), h: S.int32.with(S.rkyvType, "u16") }),
  "none",
]);
// { type: "rect", w: 3, h: 4 } or "none"
```

## Recursive types

A recursive type reaches itself through a `Vec`, or a `Box` that `S.rkyvType`
names:

```rust
struct Node {
    value: i32,
    kids: Vec<Node>,
    next: Option<Box<Node>>,
}
```

```ts
const Node = S.recursive("Node", (self) =>
  S.schema({
    value: S.int32,
    kids: S.array(self),
    next: S.optional(self.with(S.rkyvType, "Box")),
  }),
);
```

`Box` works on any type, and keeps the width it boxes:
`S.bigint.with(S.rkyvType, "u64").with(S.rkyvType, "Box")` is `Box<u64>`.

## Refinements and transforms

The archive holds the Rust type. A refinement checks the decoded value, and a
transform converts it, as with any other source:

```ts
const Account = S.schema({
  name: S.string.with(S.minLength, 2),
  balance: S.string.with(S.to, S.bigint), // a String in Rust
});

S.decodeOrThrow(S.rkyv, Account)(bytes);
// { name: "ok", balance: 12345678901234567890n }
```

## Invalid bytes

Decoding validates the archive the way rkyv's `access` does: every pointer
in bounds, aligned and claimed once, every `bool`, `char`, enum and `Option`
tag valid, and every string valid UTF-8. A failure names the field:

```ts
S.decodeOrThrow(S.rkyv, User)(corrupted);
// throws: Failed at tags: rkyv pointer out of bounds
```

The bytes start aligned to 16, as rkyv's `AlignedVec` is.

## What it doesn't store

- Maps (`HashMap`, `BTreeMap`). Use `S.array(S.tuple([key, value]))`, a
  `Vec<(K, V)>` in Rust.
- A list of zero-sized items, such as `Vec<()>`.
- An `S.uint8Array` root. Bytes on either side of `S.rkyv` are an archive
  already and pass as they are, like `S.protobuf`.

## Compatibility

`S.rkyv` speaks rkyv 0.8's default format: little-endian, aligned, 32-bit
relative pointers. Every release is checked against the Rust crate with
generated types: rkyv must accept what `S.rkyv` writes, write the same bytes
for the same value, and accept and refuse exactly the corrupted archives
`S.rkyv` does (`pnpm rkyv:fuzz`).
