---
name: fuzz
description: Pick and run the Sury fuzzer that covers what you changed (union dispatch, a schema's eq/codec properties, the form codec, the content codec, escape-free formats), read its findings, and triage them against scripts/knownBugs.ts. Use after touching the union compiler, equality/compare, defaults, containers, reverse, formData, S.to links or jsonString formats, and whenever a fuzzer fails in CI.
---

# Sury fuzzers

A spec pins one schema. A fuzzer holds a property over every schema its grammar
can draw, which is where the branch no spec reaches shows up. All of them run in
CI (`.github/workflows/ci.yml`) with the exact invocation below, so a red CI run
reproduces locally with the same command. Run from `packages/sury`.

| you touched | run | time |
|---|---|---|
| union compiler, dispatch, grouping | `pnpm fuzz:union --seed=1` | ~1 min |
| equality / `compare`, defaults, containers, `reverse`, anything deciding the Output type | `pnpm fuzz:schema` | ~25s |
| `S.formData`, a field wrapper or leaf | `pnpm fuzz:formdata` | seconds |
| `S.to` links, pack/unpack, carriers | `pnpm fuzz:content` | seconds |
| a string format's regex or `escapeFree` | `pnpm fuzz:escfree` | ~90s |

When unsure, run all five. They are cheap next to a red CI cycle.

## Findings

A finding is a bug until shown otherwise. Triage each one:

1. **Fix it** (the Bug playbook in `ship`), or
2. **List it**: add an entry to `scripts/knownBugs.ts` (union/schema fuzzers) or
   to the fuzzer's own catalog (formdata/content, `scripts/fuzzKit.ts`) *and* a
   spec that reproduces it, in the same change.

Never widen an existing entry's predicate past the one root cause its summary
names. Entries match the parsed shape (`scripts/unionFuzz/shape.ts`), never a
substring of a printed id. A gate also fails on an entry it no longer reaches:
that is a fix announcing itself, so delete the entry and correct the spec's
`FIXME: known bug <id>` example.

## What each one holds

### `fuzz:union`

Compares the compiler to a sequential try of each variant's own parser/encoder
(grouping is codegen, not semantics). Exits non-zero on an `acceptance` /
`exception-kind` diff that `knownBugs.ts` doesn't list; `reasons` / `message`
are error detail. `--ref` is an optional changelog against a git commit, not the
gate. `--seed=N` widens the search.

### `fuzz:schema`

Draws a schema from the shared grammar, samples its Input side from the schema
and its Output side from its reverse, and hands both to each family in
`scripts/schemaFuzz/`. A property a single schema can be held to belongs in a
family there, not in a new runner. The default invocation is the gate;
`--only=eq,codec`, `--seed`, `--seeds` and `--cases` turn it into a narrower
search, which reports unlisted findings but not stale entries. A creation throw
is reported and the draw finishes without the default, so two builds of the
library always draw the same schemas.

- **codec**: a decode passes the schema's `isOutput` and an encode its
  `isInput`, `parse` agrees with `decode` on accepted input, `decode(encode(o))`
  is `o`, and `encode` is decode of the reverse.
- **eq**: a value equals a separately built copy of itself, `eq(a,b)` is
  `eq(b,a)`, equal to the same value means equal to each other, the answer
  matches a schema-blind structural walk, `isEqualInput(schema)` is
  `isEqualOutput(reverse(schema))`, and two inputs the Input side calls equal
  decode to two outputs the Output side calls equal. For every schema `compare`
  accepts: `compare(a,b)===0` exactly when `isEqual`, and `compare(a,b)` is
  `-compare(b,a)`. `--seeds=N` widens, `--cases=N` deepens; reach for the
  first, since the grammar branches on every draw.

The spec harness also sends every example pair it compares to `isEqual*`, so the
whole spec corpus is the comparator's test suite.

### `fuzz:formdata`

Crosses every wrapper with every leaf: a field works in both directions or is
rejected in both, an encode does not write into the value it was handed,
`decode(encode(v))` is `v`, and every entry list a client could send is either
rejected or read as a value the schema's own output type accepts (a repeated
key, a file where text belongs). It then compiles every working field into one
schema, which is where a name handed out twice or a declaration hoisted after
its reader shows. Exhaustive, no seed.

### `fuzz:content`

Crosses every source with every target and every slot: a link compiles both
directions or neither, `reverse(link)` reads the values the link writes and
writes the ones it reads, a value survives decode, encode, decode, and a slot
does what it declares (a link with two readings and no slot refuses and names
both; a link with one reads the same with `"unpack"` as without). Crashes are
always findings. Exhaustive, no seed.

formdata and content share their catalogs (`scripts/fuzzKit.ts`): each lists the
cases known not to hold with a hand-written reason, and the run fails on an
unexplained one, on a listed one that has started to hold, and on a listed one
that no longer runs. A case that stays unfixed carries a `FIXME` in both the
catalog and its spec.

### `fuzz:escfree`

Checks each format flagged `escapeFree` can never produce a character that
needs escaping in JSON, since jsonString splices those values between bare
quotes.
