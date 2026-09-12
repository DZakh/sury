# protobuf-test-suite

Runs `S.protobuf` against [protobufjs](https://github.com/protobufjs/protobuf.js),
the JS implementation that passes Google's official Protocol Buffers conformance
suite, and against [protobuf-es](https://github.com/bufbuild/protobuf-es), and
holds the score to a committed golden.

Google's `conformance_test_runner` is a C++ process over stdin. This package
does not build that. `cases.ts` instead mirrors the binary families of
`binary_json_conformance_suite.cc` that apply to a proto3 message: the value
tables (`ValidDataScalar`, overlong and 64-bit varints, truncation to 32 bits),
`RepeatedScalarSelectsLast`, `ValidDataRepeated` in packed and expanded input
and output, `RepeatedScalarMessageMerge`, `ValidDataMap` for every key/value
pair, `ValidDataOneof`, every `PrematureEof*` position, `IllegalZeroFieldNum`,
`BadTag_*`, `UnknownWireType`, the unmatched-group family and
`RejectInvalidUtf8`, using the field numbers of `TestAllTypesProto3`, so a case
id names the conformance test it stands for. `wire.ts` holds the byte builders
of `binary_wireformat.h`. The wire-format assertions of protobuf.js's own test
suite (writer/reader vectors, packed writers, decoder bounds, map entry layout,
oneof semantics) are in the corpus too, and protobuf.js decodes what Sury
writes and vice versa on every round-trip case.

Every round-trip and decode-only case also goes through protobuf-es
(`@bufbuild/protobuf`), over the reference `.proto` and over the one
`S.toProtoOrThrow` prints. protobufjs cannot disagree with itself: a printed file its
parser reads into the shape it happened to mean, checked by its own encoder,
proves less than it looks like it does. This path shares no code with it -
`protocol-buffers-schema` parses, `descriptor.ts` builds the
FileDescriptorProto, protobuf-es decodes and re-encodes. Bytes are the
comparison, not values: the two libraries' JS shapes for a message disagree by
design, while a re-encode exercises every field of the descriptor.

protobufjs's own `toDescriptor` is not usable for this. It throws on every map
field, and writes `packed=false` onto proto3 repeated scalars that are packed
by default - both would have read as Sury failures.

Two cases are checked against protobufjs only, named in the golden's
`protobufEsWrong` with the reason in `runner.ts`: protobuf-es strips a leading
BOM from a string field, and loses a map entry keyed `__proto__`.

Google's `conformance_test_runner` itself lives in
[`packages/protobuf-conformance`](../protobuf-conformance), which runs the real
suite - cases generated inside the binary - and scores 692/698 on the binary
proto3 families. This package is the readable half: it says what broke, that
one says whether we are right. Keep both.

`google-protobuf` is Google's own JS client. It fails more than a thousand
required conformance tests, so it is not the reference here.

```bash
pnpm protobuf:compliance            # check against goldens/ (what CI runs)
pnpm protobuf:compliance update     # re-baseline after a change
pnpm protobuf:compliance report     # every case id and status
pnpm protobuf:compliance bench      # encode/decode vs protobufjs
pnpm protobuf:compliance hillclimb  # median of 7 on four workloads vs protobufjs
```

Extensions, proto2 groups as declared fields, ProtoJSON, MessageSet and
retaining unknown fields through a round trip are listed as skipped. They are
not in the public API.

`check` fails on drift in either direction. An improvement lands its golden
update in the same PR.

One `bench` run is published, with its versions and the message shapes, under
[Speed in the JS guide](../../docs/js-usage.md#speed). Re-run it there when the
numbers move enough to mislead.

`bench` runs Sury against every codec a JS project would reach for:
protobufjs reflection and `pbjs` static codegen, protobuf-es
(`@bufbuild/protobuf`) and pbf, on five workloads (tiny, typical, large,
protobuf.js's `bench/cases/common` message, and a vector-tile shaped message
dominated by packed geometry), best of 7 samples, on the same bytes and values, each
library driven the way its README shows. `hillclimb` is the frozen Sury-vs-
protobufjs ruler the perf commits on this branch quote. Neither snapshots a
number. Sury-vs-Sury encode/decode regressions are `spec check --perf`
scenarios `protobuf-encode` and `protobuf-decode`.
