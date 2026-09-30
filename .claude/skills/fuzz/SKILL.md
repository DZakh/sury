---
name: fuzz
description: Which Sury fuzzer to run for what you changed, what each checks, and its flags. Use after touching unions, equality/compare, defaults, containers, reverse, formData, S.to links, protobuf or string formats, or when a fuzzer fails in CI.
---

# Fuzzers

Run from `packages/sury` (`protobuf:fuzz` from the root). These are CI's exact
invocations, so a red CI run reproduces with the same command. A finding is a
bug until shown otherwise; triage it by CLAUDE.md's Fuzzers rules. A gate that reports a stale entry may
mean the bug is fixed or that the grammar stopped drawing its shape: run the
entry's spec, and only when it no longer reproduces delete the entry and
correct its `FIXME: known bug` example.

| you touched | run | checks |
|---|---|---|
| union dispatch, failure exits | `pnpm fuzz:union --seed=1` | the compiler against a sequential try of each member, and every answering outcome (`isInput`, `parseAsResult`, `~standard.validate`, the promise ones) against the same compile's `parseOrThrow`; fails only on `acceptance`/`exception-kind`/`outcome` diffs |
| equality, compare, defaults, containers, `reverse`, the Output type | `pnpm fuzz:schema` | **eq**: equivalence laws, `compare` antisymmetry and agreement with `isEqual`; **codec**: decode passes `isOutput`, encode passes `isInput`, round trips, encode = decode of reverse |
| `S.formData` | `pnpm fuzz:formdata` | every wrapper × leaf works both ways or neither, encode doesn't mutate, round trip, any client entry list is rejected or valid |
| `S.to` links, pack/unpack | `pnpm fuzz:content` | every source × target × slot compiles both ways or neither, reverse mirrors, round trip, an ambiguous link refuses; any crash is a finding |
| the protobuf codec | `pnpm protobuf:fuzz` | generated message graphs and mutated bytes against protobufjs and protobuf-es |
| a format's regex or `escapeFree` | `pnpm fuzz:escfree` | a flagged format never needs JSON escaping |

Unsure: run all six, a few minutes in total.

Widening a search: `fuzz:union --seed=N` (`--ref=<commit>` adds a changelog,
not a gate); `fuzz:schema --only=eq,codec --seed=N --seeds=N --cases=N` (prefer
more seeds over more cases). Narrowed runs report unlisted findings but not
stale entries. formdata and content are exhaustive and keep their known cases in catalogs in
`formDataFuzz.ts` and `contentFuzz.ts`; a catalogued bug (not a limitation)
carries a `FIXME` in both the catalog and its spec.
