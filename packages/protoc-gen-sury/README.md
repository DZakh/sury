# protoc-gen-sury

Generates Sury schemas from `.proto` files: a protoc plugin that `buf generate`
and `protoc` run. It ships inside the `sury` package as its `protoc-gen-sury`
bin (`scripts/pack.ts` bundles `src/cli.ts`), so the generator that wrote a
schema is always the runtime that runs it. This package is where it is built
and tested; user documentation is in `docs/js-protobuf.md` and
`docs/rescript-protobuf.md`.

```bash
pnpm protobuf:codegen                   # every gate below
pnpm protobuf:codegen update            # rewrite the goldens
SEED=7 pnpm protobuf:codegen            # another draw of values
pnpm protobuf:conformance:generated     # Google's suite over the generated schema
```

## Layout

- `src/descriptor.ts` - the parts of `descriptor.proto` and `plugin.proto` the
  generator reads, decoded by Sury. Both are proto2, which the generator
  doesn't take, so these stay hand-written.
- `src/model.ts` - descriptors resolved into messages, fields, oneofs and
  enums, named the way protobuf-es names them.
- `src/names.ts` - the exported names of a file, clashes resolved in
  protoplugin's order.
- `src/ts.ts`, `src/res.ts` - the two targets.
- `src/plugin.ts` - options, refusals, one response per request.
- `scripts/wkt.ts` - generates `sury/protobuf/wkt` from the upstream
  well-known type protos, and `S.Protobuf`, the region of `S.res` between its
  `@generated` markers that binds the same types for ReScript.
- `test/proto` - the corpus: a kitchen sink covering every field shape, a
  second file for cross-file imports, and a third whose names are the ones the
  generated code itself uses.
- `bin/protoc-gen-sury.mjs` - the plugin run from source, as the gate runs it.

## What the gates hold

`test/cli.ts`, one function each:

1. **descriptors** - `src/descriptor.ts` decodes the corpus's descriptor set
   as protobuf-es does.
2. **goldens** - the generated corpus, `packages/sury/src/protobuf/wkt` and
   the `S.Protobuf` region of `packages/sury/src/S.res`.
3. **plugin** - the bin, run by buf over stdin and on a descriptor set,
   writes what `generate()` returns, and exits 1 on an invalid option.
4. **options** - every option and every refusal, by what `generate()` answers.
5. **compile** - `tsc` over the TypeScript (where `S.schemaOf` holds every
   schema equal to its type), with every message and enum type equal to
   protoc-gen-es's after normalizing the differences the JS guide lists, and
   `rescript` over the ReScript, every warning an error.
6. **values** - values drawn from each descriptor, encoded by protobuf-es and
   decoded by the generated TypeScript and ReScript schemas, and the reverse,
   compared by value and by byte.
7. **typedAny** - an Any read as a tagged union of generated messages,
   against protobuf-es's `anyPack`.
8. **reprint** - `S.toProtoOrThrow` of every generated schema, compiled again
   by buf, declares the fields the source did.
9. **treeShaking** - Rollup 4 and esbuild bundles of one message, one
   `sury/protobuf/wkt` type and one `S.Protobuf` type (`test/res`), and a
   Rollup bundle of one ReScript module, keep none of their siblings; one
   member of a recursive group, bundled alone, encodes and decodes.

## Rules the output follows

- Every schema is built with the functional forms (`S.protobufField(schema,
  …)`), never `.with(…)`: a method call on a schema can't be dropped by a
  bundler, and one message imported must not keep the file.
- Every call that runs as a TypeScript module loads is `/* @__PURE__ */`:
  Rollup sees from Sury's own declarations that its factories are
  `@__NO_SIDE_EFFECTS__`, esbuild doesn't, and keeps an unused export along
  with the protobuf runtime it reaches. A minifier drops the comments. A call
  inside an `S.recursive` callback runs only when the call around it does, and
  takes none. ReScript can't write one, so its modules shake under Rollup
  only.
- An `S.protobufField` states `type` only where it differs from what the
  field's schema infers, and `key` only where it isn't `string`:
  `S.protobufField(S.int32, 3)`, `S.protobufField(S.int32, { number: 7, type:
  "sint32" })`. Generated code ships per message, and the inference is the
  table `docs/js-protobuf.md` documents, so stating what it infers only
  repeats it.
- A singular message field is always `S.optional`: that is proto3 presence,
  and it is what keeps a recursive message finite.
- A recursive group of messages is built once, by a function that builds
  every member, and each export is a call to it. No export reads a variable
  another one fills in: a bundler drops an export nothing imports along with
  whatever it filled in. In TypeScript the function keeps what it built for
  the next export and hands back the member asked for, since esbuild keeps a
  destructured or property-read result; the ReScript function is a named one
  returning a tuple, as ReScript inlines a function called where it is
  written. A callback that doesn't read its own schema takes no
  parameter, for `noUnusedParameters` and ReScript's warning 27.

## Not generated

proto2, editions, extensions and groups are refused, naming the file or field.
Services are skipped. Left for later: open enums (the printer can't yet keep an
enum's name on an `S.int32` field), `protovalidate` rules as refinements, and
a Connect transport over generated service descriptors.
