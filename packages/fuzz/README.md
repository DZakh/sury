# Sury schema compiler fuzzer

This is one deterministic engine for the public, compiler-facing runtime schema API. It varies schema trees, decoder/encoder pipelines, and compilation modes; it intentionally does not spend most of its budget mutating arbitrary input values.

The engine covers:

- the complete compiler-facing schema constructor, refinement, modifier, transformation, recursion, list, compact-column, instance, merge, shape, and reverse surface (aliases share their canonical implementation);
- `parser`, `decoder`, `encoder`, and their async variants;
- one-to-three-schema compiler pipelines;
- unions, recursion, refinements, objects, tuples, records, and transformations;
- `S.to(source, target)`, `S.to(source, target, decoder)`, and `S.to(source, target, decoder, encoder)`;
- generated-function syntax, sync/async contracts, cache behavior, and a canonical witness value;
- deterministic failure replay and structural shrinking.

PPX is out of scope. It has a separate compilation surface and should get its own focused harness if it is fuzzed later. JSON Schema conversion, global configuration, and result-wrapper helpers are also outside this schema-to-generated-function engine because they do not add decoder/encoder compiler combinations.

## Run

From the repository root:

```sh
pnpm fuzz
```

The command deliberately has no tuning flags. It always runs the checked-in reliability profile: four deterministic seeds, 2,000 cases per seed, schema depth four, canonical witness execution, a one-second async timeout, and structural shrinking. Keeping this profile in source makes local and CI runs comparable and prevents important checks from being disabled accidentally.

A compiler crash, invalid generated function, unexpected runtime exception, timeout, or sync/async contract violation writes a JSON artifact under `packages/fuzz/artifacts/` and prints a replay command. `SuryError` and explicit `[Sury]` configuration errors are treated as controlled rejections because many randomly composed API combinations are intentionally incompatible.

To replay a saved failure:

```sh
pnpm fuzz -- replay packages/fuzz/artifacts/<artifact>.json
```

The report lists operation, public API, pipeline-category, and outcome counters. It explicitly reports any compiler API not reached by the run; the fixed corpus reaches the full catalog before seeded generation begins.
