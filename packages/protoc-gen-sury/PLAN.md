# protoc-gen-sury: phase 1 plan

Handoff document. It records the decisions already taken with the maintainer, the
facts they rest on, and the order of work. Read it with `CLAUDE.md`; every rule
there applies, and every change under `packages/sury/src` goes through the spec
skill. When phase 1 ships, this file becomes the package README or is deleted.

## Goal

`.proto` files in, Sury schemas out, for TypeScript and ReScript in the same
release. The TypeScript output matches what protobuf-es (`protoc-gen-es`)
generates closely enough that someone migrating keeps their names and their
types, and the deviations are few and deliberate (listed below).

## Settled decisions

Don't reopen these without the maintainer.

1. **Plugin, not parser.** The generator is a protoc plugin
   (`CodeGeneratorRequest` on stdin, `CodeGeneratorResponse` on stdout), run by
   `buf generate` or `protoc`. It never parses `.proto` text. A standalone
   `--descriptor_set_in=x.binpb --out=dir` entry shares the same code path.
2. **The binary ships inside `sury`.** The published artifact gets
   `"bin": { "protoc-gen-sury": "./bin/protoc-gen-sury/index.mjs" }`. Generator
   and runtime can then never skew. So the generator must have **zero
   dependencies**, and it imports `sury` through the package's own `"."` export,
   the way `S.res` does.
3. **Self-hosted input.** The generator decodes `CodeGeneratorRequest` with Sury
   schemas. `descriptor.proto` is recursive (`DescriptorProto.nested_type`), which
   `S.recursive` supports since #447.
4. **Targets: `ts` and `res`.** No `js`, no `dts`.
5. **Types first, schema checked against them.** TS emits an explicit `type` per
   message and builds the schema with `S.schemaOf<T>()`, so an emitter bug is a
   type error on the field. ReScript annotates `S.t<t>`.
6. **Every `protobufField` carries an explicit `type`**, plus `key` for maps and
   `packed: false` where the descriptor says so. Never rely on `inferType`.
7. **No renaming machinery.** The wire carries numbers only, so the property
   name is free. No `S.object`/`s.field`, no `@as` on record fields, no `name` on
   `ProtobufField`. The reprint gate compares numbers, wire types and labels,
   never names (`packages/protobuf-conformance/checkSchema.ts` already does
   this).
8. **Singular message fields are always optional** (`S.optional`, `field?: T`).
   That is proto3 presence. Since #447 it is also what keeps generated code out
   of the refused "cycle of required singular message fields". The emitter
   asserts it.
9. **Enums are closed** (`S.enum` over the known values). A decode does not
   validate, so an unknown value from a newer peer passes `decodeOrThrow` and
   fails only on `parse`. Open enums need printer work
   (`protobuf.ts`, where an enum field without a literal union prints as
   `int32`) and are out of scope.
10. **Oneofs use the protobuf-es shape**, `{ case, value }`. This needs core
    work (C1 below).

## Parity with protobuf-es

Port protobuf-es's naming helpers rather than re-deriving them: lower camel
case from the proto name, safe-property escaping, safe identifiers, and the
enum shared-prefix rule. Gate G4 proves the port.

| Thing | protobuf-es | protoc-gen-sury (TS) |
|---|---|---|
| Output file | `foo/bar_pb.ts` | same |
| Message type | `export type User = Message<"pkg.User"> & {...}` | `export type User = {...}` (no `Message<>`, see deviations) |
| Message schema | `export const UserSchema: GenMessage<User>` | `export const UserSchema` |
| Nested type | `User_Address`, `User_AddressSchema` | same |
| Property name | lowerCamelCase of the **proto name**, not `json_name` | same |
| Reserved property | `$` suffix (`constructor$`, `toString$`, ...) | same |
| Enum | TS `enum PhoneType { UNSPECIFIED = 0, ... }` + `PhoneTypeSchema`, shared `PHONE_TYPE_` prefix stripped | same |
| Implicit-presence scalar | `id: number` | same |
| proto3 `optional` | `nickname?: string` | same |
| Message field | `home?: Address` | same |
| Repeated | `T[]` | same |
| Map | `{ [key: string]: V }` | same spelling |
| bytes | `Uint8Array` | same |
| 64-bit | `bigint`; `string` under field option `jstype = JS_STRING` | same (per field, no global flag) |
| Oneof | `{ case: "a"; value: A } \| ... \| { case: undefined; value?: undefined }` | same (C1) |
| Wrapper field (`Int32Value`...) | unboxed, `field?: number` | same (C2) |
| `Struct` field / `Value` field | `JsonObject` / `JsonValue` | same (C3) |
| Other well-known types | imported from `@bufbuild/protobuf/wkt` | imported from `sury/wkt` |
| Doc comments | leading comments + `@generated from message pkg.User` / `@generated from field: int32 id = 1;` | same |
| `import_extension` | `none` default | same |
| Services | `GenService` in the same file | not in phase 1; the name `ElizaService` stays reserved for the Connect phase |

### Deliberate deviations

- **No `$typeName`, no `$unknown`.** protobuf-es needs `$typeName` to find a
  message's schema at runtime (registries, `Any`, JSON). Sury always takes the
  schema explicitly, and requiring the property would make every hand-written
  literal carry it. `$unknown` is unknown-field retention, which `S.protobuf`
  deliberately doesn't do (documented under "Unknown fields"). G4 strips both
  before comparing types.
- **No `file_*` descriptor export, no `create()`, no registry or reflection.**
  Values are plain literals. One DX difference follows: protobuf-es's `create`
  takes a partial init, while a Sury literal must spell every implicit-presence
  field. Note it in the docs.
- **proto2 `[default = x]` is not applied.** The field stays `field?: T` (the
  same type protobuf-es emits) and reads `undefined` when unset. Applying it
  with `S.optional(T, x)` would change the type and make re-encoding write the
  default explicitly, which breaks byte equality.
- **`target` defaults to `ts`.**

## ReScript target

protobuf-es has no ReScript output, so here parity means mirroring its names in
ReScript idiom. Use one module per message or enum, flattened the same way
(`User`, `User_Kind`), each holding `type t` and `let schema`:

```rescript
// Generated by protoc-gen-sury from example/v1/user.proto (package example.v1)

module User_Kind = {
  type t = | @as(0) Unspecified | @as(1) Primary
  let schema = S.enum([Unspecified, Primary])
}

module User = {
  @tag("case")
  type contact = | @as("email") Email({value: string}) | @as("phone") Phone({value: string})

  type t = {
    id: int,
    firstName: string,
    tags: array<string>,
    scores: dict<int>,
    home: option<Example_v1_address_pb.Address.t>,
    kind: User_Kind.t,
    contact: option<contact>,
  }

  let schema: S.t<t> = S.schema(s => {
    id: s.matches(S.int->S.protobufField(1, ~type_=#int32)),
    // ...
  })
}
```

- **File and module name** mangle the full proto path
  (`Example_v1_user_pb.res`), because a consumer's ReScript namespace is flat and
  two `user.proto` files would collide.
- **Enum constructors** are the stripped protobuf-es names in PascalCase
  (`LAND_LINE` becomes `LandLine`). The int representation needs C5.
- **Oneofs** are a `@tag("case")` variant with inline-record payloads, which
  compiles to the protobuf-es runtime shape `{case: "email", value}`. Unset is
  `option<contact>`, so `undefined` rather than `{case: undefined}`, which is why
  C1 must accept either unset arm.
- **Recursive components**: ReScript types don't hoist. Declare each recursive
  component as one top-level `type rec a = ... and b = ...` and alias it inside
  the module (`type t = a`).
- **Reserved field names** get a `_` suffix (`type_`), the convention `S.res`
  already uses.
- **Well-known types** are emitted on demand into the output directory
  (`Google_protobuf_timestamp_pb.res`), not shipped in `sury`. CLAUDE.md says
  `S.res` is the only ReScript module (open question Q2).
- ReScript top-level `let`s carry no pure annotation, so ReScript output shakes
  per file, not per message. Accepted.

## TypeScript output

Generated code must use the **functional forms**, never `.with(...)`. A `.with`
call can't be dropped by a bundler (CLAUDE.md, Tree-shaking), and each message
must shake individually. `S.protobufField(schema, field)`, `S.meta(schema, meta)`,
`S.shape(schema, fn)` and `S.to(...)` all exist as functions.

```ts
// @generated by protoc-gen-sury v11.x.x with parameter "target=ts"
// @generated from file example/v1/user.proto (package example.v1, syntax proto3)
/* eslint-disable */

import * as S from "sury";
import { type Address, AddressSchema } from "./address_pb";

/**
 * @generated from enum example.v1.User.Kind
 */
export enum User_Kind {
  UNSPECIFIED = 0,
  PRIMARY = 1,
}

export const User_KindSchema = S.enum([User_Kind.UNSPECIFIED, User_Kind.PRIMARY]);

/**
 * @generated from message example.v1.User
 */
export type User = {
  /**
   * @generated from field: int32 id = 1;
   */
  id: number;
  firstName: string;
  tags: string[];
  scores: { [key: string]: number };
  home?: Address;
  kind: User_Kind;
  contact:
    | { case: "email"; value: string }
    | { case: "phone"; value: string }
    | { case: undefined; value?: undefined };
};

export const UserSchema = S.meta(
  S.schemaOf<User>()({
    id: S.protobufField(S.int32, { number: 1, type: "int32" }),
    firstName: S.protobufField(S.string, { number: 2, type: "string" }),
    tags: S.protobufField(S.array(S.string), { number: 3, type: "string" }),
    scores: S.protobufField(S.record(S.int32), { number: 4, type: "int32", key: "string" }),
    home: S.protobufField(S.optional(AddressSchema), { number: 5, type: "message" }),
    kind: S.protobufField(User_KindSchema, { number: 6, type: "enum" }),
    contact: /* the form C1 settles */,
  }),
  { name: "User" },
);
```

Recursive component:

```ts
export type Node = { id: number; children: Node[]; parent?: Node };

export const NodeSchema = S.recursive<Node>("Node", (self) =>
  S.schemaOf<Node>()({
    id: S.protobufField(S.int32, { number: 1, type: "int32" }),
    children: S.protobufField(S.array(self), { number: 2, type: "message" }),
    parent: S.protobufField(S.optional(self), { number: 3, type: "message" }),
  }),
);
```

- `S.recursive` checks its definer only by assignability, so the definition is
  wrapped in `S.schemaOf` inside it for the equality check (C4).
- A mutually recursive component with several exported members emits one
  `S.recursive` root per exported member, with the other members inlined inside
  it (the shape `specs/codec-protobuf-recursive-mutual.yaml` pins). Size grows
  with the square of the component, which is 1 for nearly every real file and 3
  for `Struct`/`Value`/`ListValue`. Known: two `S.recursive` with one name
  render as one JSON Schema `$defs` entry, the later winning (a FIXME from
  #447). That's harmless for identical duplicates, but pin it with a spec.
- Declaration order: enums first (TS `enum` is a runtime object), then message
  constants in topological order, each recursive component as a unit. protoc
  rejects circular imports, so every component lives in one file.

## Core work in `packages/sury` (spec skill, before the emitters)

Each lands as a spec that stays as the regression test. Record the metric
summary each time.

- **C1. Oneof as a discriminated union.**
  - `S.shape` can't do this. Its callback runs against a build-time proxy, so it
    can't choose a member at runtime, and a whole-message `S.union` multiplies
    across several oneofs. The compiler has to support it natively, both
    directions, emitted inline.
  - Starting proposal, no new export: a field whose schema is a union of
    `{ case: <literal>, value: <numbered> }` objects plus one unset arm, marked
    with `oneof`:
    ```ts
    contact: S.protobufField(
      S.union([
        S.schema({ case: "email", value: S.protobufField(S.string, { number: 7, type: "string" }) }),
        S.schema({ case: "phone", value: S.protobufField(S.string, { number: 8, type: "string" }) }),
        S.schema({ case: undefined, value: S.optional(S.undefined) }),
      ]),
      { oneof: "contact" },
    )
    ```
    The final API is the spec work's call.
  - Requirements:
    - the unset arm may be `{ case: undefined }` (TS) or plain `undefined` (ReScript `option`)
    - decoding a member clears the others, as today
    - `toProtoOrThrow` prints `oneof contact { ... }`
    - works inside `S.recursive` (compare `codec-protobuf-recursive-oneof.yaml`)
    - `S.schemaOf` accepts it against the protobuf-es type.
  - This touches the union compiler, so run `pnpm --filter=sury fuzz:union`.
- **C2. Wrapper unboxing.**
  `S.optional(S.shape(Int32ValueSchema, (w) => w.value))` is a plain projection,
  which the proxy allows. Specs have to show two things. First, a present
  wrapper holding the zero value round-trips (on the wire that's an empty
  nested message). Second, `toProtoOrThrow` still prints the field as
  `google.protobuf.Int32Value`. Possibly no core change.
- **C3. `Struct`/`Value`/`ListValue` as `JsonObject`/`JsonValue`.**
  Spike first. `Value` is a oneof whose JSON form is untagged: decode picks the
  member by field number, encode by the value's JS type. That may be a second
  oneof form, "a union whose members carry distinct field numbers". If it
  doesn't land in phase 1, those fields fall back to the raw message types,
  listed as a parity gap.
- **C4. Type tests.**
  - `S.schemaOf` inside an `S.recursive` definer
  - `S.enum` over TS `enum` members, compared with `S.schemaOf` equality against
    the enum type
  - the oneof type from C1.
- **C5. ReScript representations on the wire.**
  - `@as(n)` constant variants through `S.enum` encode as the int
  - the `@tag("case")` inline-record variant is the C1 shape.

## Generator package

- **Source.** `packages/protoc-gen-sury`, private workspace package, TypeScript.
  `scripts/pack.ts` bundles it with esbuild into the artifact at
  `bin/protoc-gen-sury/index.mjs`. Add the bin and `sury/wkt` files to
  `artifact_test.ts`'s `FILES`, add the `bin` field and the `./wkt` export to
  the artifact `package.json`, and keep both out of `sideEffects`.
- **Input schemas.**
  - Hand-write `descriptor_pb.ts` and `plugin_pb.ts` for only the fields the
    generator reads, already in the output format and with the parity names
    (`FileDescriptorProtoSchema`, `CodeGeneratorRequestSchema`).
  - Both source files are proto2. Their `optional` is ordinary explicit presence.
    `UninterpretedOption.NamePart` has `required` fields, which the generator
    never reads.
- **proto2 subset.**
  - `optional` becomes `S.optional`
  - `required` becomes non-optional, with no check for a missing field
  - `[default]` is ignored (see deviations)
  - enums are closed
  - groups and `extend` are refused with a message naming the file and field.
- **Refused with a clear message:**
  - editions files
  - groups
  - extensions
  - anything that would emit a required singular message cycle (the assertion
    from decision 8).
- **Registry.**
  - fully-qualified name to descriptor across the whole request
  - TS and ReScript identifiers, with their own reserved words and collision
    rules
  - strongly connected components of the message graph
  - comments from `SourceCodeInfo`.
- **Options** (protobuf-es spelling where one exists):
  ```
  target=ts|res|ts+res       # default ts
  import_extension=none|js|ts  # default none, ts only
  keep_empty_files=false
  ts_nocheck=false
  ```
  64-bit handling follows `jstype` per field. There is no global `int64` flag.
- **`sury/wkt`.** At pack time the generator generates the well-known types from
  the upstream `.proto` files into the artifact, exporting protobuf-es's names
  (`TimestampSchema`, `DurationSchema`, `Int32ValueSchema`, `StructSchema`...).
  Timestamp and Duration stay messages (`{ seconds: bigint; nanos: number }`),
  exactly as in protobuf-es. Its helper functions (`timestampDate`...) are out
  of scope.

## Gates

Each gate is a script with a committed golden, run in CI.

- **G1. Compile.** `tsc --noEmit` and `rescript build` over the generated corpus.
- **G2. Type cost.** Instantiations per corpus file, committed. The
  mutual-recursion spec alone costs about 10.9k.
- **G3. Goldens.** The generated `.ts` and `.res` files are committed, and
  `update` re-baselines.
- **G4. Type parity.** Generate the corpus with `protoc-gen-es` (`target=ts`)
  too. A generated type test asserts, for every message, that protobuf-es's
  type with `$typeName`/`$unknown` stripped recursively equals ours. It also
  asserts that every protobuf-es message and enum export has a same-named Sury
  export.
- **G5. Value parity.**
  - For every corpus message: build values, protobuf-es `toBinary`, Sury decode,
    deep-equal against the protobuf-es value with `$` properties stripped, and
    the reverse.
  - This upgrades `protobuf-test-suite`'s bytes-only comparison: its README
    says the two JS shapes disagree by design, and generated schemas make them
    agree.
- **G6. Reprint.** `toProtoOrThrow` each message and compare field number, wire
  type and label with the source descriptor, reusing `checkSchema.ts`'s
  comparison.
- **G7. Fixpoint.** The generator regenerates `descriptor_pb.ts` and
  `plugin_pb.ts` from upstream byte-identically, then replaces the hand-written
  ones.
- **G8. Conformance.** Generate `TestAllTypesProto3` from upstream and run
  `packages/protobuf-conformance` against it instead of `testMessages.ts`. It
  must score at least 695/698, today's score.
- **G9. Tree-shaking.** Bundle an entry importing one schema from a
  multi-message generated file and from `sury/wkt`, and assert the other
  messages are absent. Follow `tests/treeShaking_test.ts`.

Toolchain for the gates: `@bufbuild/buf` (the npm package ships the binary) and
`@bufbuild/protoc-gen-es`, as devDependencies of the test package, never of
`sury`.

Corpus: a hand-written `kitchen_sink.proto` that covers every parity-table row,
the well-known types, `TestAllTypesProto3`, `descriptor.proto`, `plugin.proto`.

## Work order

1. Read `CLAUDE.md`, the spec skill, the Protocol Buffers sections of both usage
   guides, and the #447 commit message (it explains how recursion is compiled
   and what it refuses).
2. C4 and C5, then C1 and C2, then the C3 spike. The emitters depend on their
   answers.
3. Hand-written `descriptor_pb.ts`/`plugin_pb.ts`. Decode a real request captured
   from `buf generate`.
4. Packaging: workspace package, `pack.ts`, `bin`, `FILES`, and a smoke test
   under both `buf generate` and `protoc`.
5. Registry with the ported naming rules.
6. TS emitter, with G4 and G5 wired at the start of this step, not the end.
   They are what "parity" means.
7. ReScript emitter.
8. `sury/wkt` generation at pack time.
9. G6, G8, G9 in CI.
10. G7: the fixpoint replaces the hand-written input schemas.
11. Docs: a "Generating from `.proto`" section in `docs/js-usage.md` and
    `docs/rescript-usage.md`, including the deviations, and a note in the
    Protocol Buffers section's parity paragraph.

## Open questions for the maintainer

The defaults above hold unless answered otherwise.

- **Q1.** Omit `$typeName`/`$unknown`. Adding `$typeName` later would need core
  support for a constant field that isn't on the wire.
- **Q2.** ReScript well-known types are emitted on demand rather than shipped
  as `.res` in `sury`, because of the `S.res`-only rule.
- **Q3.** proto2 defaults are not applied.
- **Q4.** TS `enum` (parity) rather than an erasable const object. protobuf-es
  offers `erasable_syntax` behind an option, which could follow later.

## Out of scope for phase 1

- editions, extensions, groups, custom options
- open enums
- ProtoJSON as its own codec, and `json_types`
- Timestamp and other helper functions, `Any` packing
- unknown-field retention
- services
- protovalidate (phase 2)
- Connect (phase 3)
