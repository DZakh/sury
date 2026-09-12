# protobuf-conformance-suite

Runs Google's official Protocol Buffers
[conformance suite](https://github.com/protocolbuffers/protobuf/tree/main/conformance)
against `S.protobuf` and holds the score to a committed golden.

```bash
pnpm protobuf:conformance          # check against goldens/ (what CI runs)
pnpm protobuf:conformance update   # re-baseline after a change
pnpm protobuf:conformance report   # the runner's full log, every failure named
pnpm protobuf:conformance schema   # diff testMessages.ts against the pinned .proto
```

## What this is, and what the other suite is

`packages/protobuf-test-suite` is ours: a corpus written from
`binary_json_conformance_suite.cc` and protobuf.js's own tests, checked against
two reference implementations. It runs fast, it is readable, and every case is
a file you can edit.

This package is the real thing. The cases are generated inside Google's own C++
`conformance_test_runner`, which spawns `runner.ts` and pipes length-prefixed
`ConformanceRequest`s through it. Nothing here can drift from upstream, because
nothing here decides what a case is.

Keep both. The first one tells you what broke; this one tells you whether you
are right.

## Vendoring

Two pinned sources, and the split is not obvious:

- **The corpus.** `conformance-ref.json` pins a commit of
  [bufbuild/protobuf-conformance](https://github.com/bufbuild/protobuf-conformance)
  (Apache-2.0), fetched into a gitignored `.upstream/`. Its `proto/` is what
  `testMessages.ts` is written against, and every run diffs the two field
  number by field number, so a bump that adds a field or retypes one says
  which rather than leaving a case to fail for reasons nobody can see. The six
  fields the schema cannot express are listed in `cli.ts` with the reason.
  `runner.ts` follows the harness that repo gives each implementation in
  `impl/*/runner.ts`.
- **The runner.** Google's C++ binary, from the `protobuf-conformance` npm
  package, pinned exactly - the same package bufbuild's repo depends on. The
  test cases live inside that binary rather than in any file, so its version is
  as load-bearing as the commit.

Linux and macOS only: those are the platforms the runner publishes a binary
for.

## What is counted

`S.protobuf` speaks the protobuf binary format. ProtoJSON, text format and the
proto2 message types are answered `skipped`, which the runner counts as neither
pass nor fail, and the rate is over what is left - every binary proto3 case,
none of them skipped.

`failing_tests.txt` names the cases expected to fail, each with its reason. A
case that starts failing without a line there fails the job; so does a case
that starts passing, because the golden moves. Both want the same one-line fix
and a re-baseline in the same commit.
