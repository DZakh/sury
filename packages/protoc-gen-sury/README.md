# protoc-gen-sury

Generates Sury schemas from `.proto` files: a protoc plugin that `buf generate`
and `protoc` run. It ships inside the `sury` package as its `protoc-gen-sury`
bin (`scripts/pack.ts` bundles `src/cli.ts`), so the generator that wrote a
schema is always the runtime that runs it. This package is where it is built
and tested; user documentation is in `docs/js-usage.md` and
`docs/rescript-usage.md`, under Protocol Buffers.

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
- `scripts/wkt.ts` - generates `sury/wkt` and `SuryProtobuf.res` from the
  upstream well-known type protos.
- `test/proto` - the corpus: a kitchen sink covering every field shape, and a
  second file for cross-file imports.

## What the gates hold

`test/cli.ts`, in order:

1. **Goldens** - the generated corpus, `packages/sury/src/wkt` and
   `packages/sury/src/SuryProtobuf.res`.
2. **Compile** - `tsc` over the TypeScript (where `S.schemaOf` holds every
   schema equal to its type) and `rescript` over the ReScript, warnings as
   errors.
3. **Types** - every message and enum type equal to protoc-gen-es's for the
   same file, after normalizing the differences the JS guide lists.
4. **Values** - values drawn from each descriptor, encoded by protobuf-es and
   decoded by the generated TypeScript and ReScript schemas, and the reverse,
   compared by value and by byte.
5. **Reprint** - `S.toProtoOrThrow` of every generated schema, compiled again
   by buf, declares the fields the source did.
6. **Tree-shaking** - Rollup 4 bundles of one message, one `sury/wkt` type,
   one ReScript module and one `SuryProtobuf` type keep none of their siblings.

## Rules the output follows

- Every schema is built with the functional forms (`S.protobufField(schema,
  …)`), never `.with(…)`: a method call on a schema can't be dropped by a
  bundler, and one message imported must not keep the file.
- Every `S.protobufField` states its `type`, and `key` on a map. Inference is
  for schemas written by hand.
- A singular message field is always `S.optional`: that is proto3 presence,
  and it is what keeps a recursive message finite.
- A recursive group is one `S.recursive` per exported member, the others
  inlined inside it; a binding the body doesn't read is `_`-prefixed, for
  `noUnusedParameters` and ReScript's warning 27.

## Not generated

proto2, editions, extensions and groups are refused, naming the file or field.
Services are skipped. Left for later: open enums (the printer can't yet keep an
enum's name on an `S.int32` field), `protovalidate` rules as refinements, and
a Connect transport over generated service descriptors.
