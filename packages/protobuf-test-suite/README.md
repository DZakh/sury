# protobuf-test-suite

Runs `S.protobuf` against [protobufjs](https://github.com/protobufjs/protobuf.js),
the JS implementation that passes Google's official Protocol Buffers conformance
suite, and holds the score to a committed golden.

Google's `conformance_test_runner` is a C++ process over stdin. This package
does not build that. `cases.ts` instead mirrors the binary families of
`binary_json_conformance_suite.cc` that apply to a proto3 message — the value
tables (`ValidDataScalar`, overlong and 64-bit varints, truncation to 32 bits),
`RepeatedScalarSelectsLast`, `ValidDataRepeated` in packed and expanded input
and output, `RepeatedScalarMessageMerge`, `ValidDataMap` for every key/value
pair, `ValidDataOneof`, every `PrematureEof*` position, `IllegalZeroFieldNum`,
`BadTag_*`, `UnknownWireType`, the unmatched-group family and
`RejectInvalidUtf8` — using the field numbers of `TestAllTypesProto3`, so a case
id names the conformance test it stands for. `wire.ts` holds the byte builders
of `binary_wireformat.h`. The wire-format assertions of protobuf.js's own test
suite (writer/reader vectors, packed writers, decoder bounds, map entry layout,
oneof semantics) are in the corpus too, and protobuf.js decodes what Sury
writes and vice versa on every round-trip case.

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

`bench` and `hillclimb` print nanoseconds per op against protobufjs's
reflection codec (`Type#encode(...).finish()` / `Type#decode`), on the same
bytes and the same values. `hillclimb` adds protobuf.js's own `bench/cases/common`
message so the numbers compare with its published benchmark. Neither snapshots
a number. Sury-vs-Sury encode/decode regressions are `spec check --perf`
scenarios `protobuf-encode` and `protobuf-decode`.
