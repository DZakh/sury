# protobuf-test-suite

Runs `S.protobuf` against [protobufjs](https://github.com/protobufjs/protobuf.js),
the JS implementation that passes Google's official Protocol Buffers conformance
suite, and holds the score to a committed golden.

Google's `conformance_test_runner` is a C++ process over stdin. This package
does not build that. It uses protobufjs as the JS reference (it reports 100%
on that suite) and includes the encoding-guide vectors from
https://protobuf.dev/programming-guides/encoding/.

`google-protobuf` is Google's own JS client. It fails more than a thousand
required conformance tests, so it is not the reference here.

```bash
pnpm protobuf:compliance            # check against goldens/ (what CI runs)
pnpm protobuf:compliance update     # re-baseline after a change
pnpm protobuf:compliance report     # every case id and status
pnpm protobuf:compliance bench      # encode/decode vs protobufjs
```

The corpus is the wire types `S.protobuf` claims: all 15 scalars, packed and
unpacked repeated fields, nested messages, proto3 presence, unknown-field
strip, and malformed tags. Maps, oneofs, extensions, proto2 groups, and ProtoJSON
are listed as skipped. They are not in the public API.

`check` fails on drift in either direction. An improvement lands its golden
update in the same PR.

`bench` prints nanoseconds per op against protobufjs. It does not snapshot a
number. Sury-vs-Sury encode/decode regressions are `spec check --perf` scenarios
`protobuf-encode` and `protobuf-decode`.
