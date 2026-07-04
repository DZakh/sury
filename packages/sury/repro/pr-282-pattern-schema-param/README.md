# Repro: PR #282 — missing `schema` param on `S.pattern`'s type declaration

https://github.com/DZakh/sury/pull/282

## Root cause

`S.pattern`'s runtime implementation takes the schema as its first argument,
consistent with every other string refinement:

```rescript
// packages/sury/src/Sury.res
let pattern = (schema, re, ~message=`Invalid pattern`) => { ... }
```

but its public type declaration dropped that parameter:

```ts
// packages/sury/src/S.d.ts (main, before PR #282)
export const pattern: (re: RegExp, message?: string) => Schema<string, string>;
```

This has two consequences:

1. `S.pattern` can't be called with an explicit schema in TS at all (the arity
   doesn't match the implementation).
2. Worse, when used through `.with(S.pattern, re)`, TS still picks an
   overload of `Schema.with` (the generic `A1` one) because a RegExp is
   structurally close enough — but it then hard-codes `Input` to `string`.
   Any schema whose `Input` differs from `string` (i.e. it has a transform
   via `.with(S.to, ...)`) fails to type-check, exactly as reported in the PR.

No existing test exercises this because `S_test.ts` only has a *commented
out* `S.pattern(...)` call (see the `FIXME` above it), so nothing caught the
regression.

## Reproducing

From `packages/sury`:

```sh
npx tsc --noEmit --strict --esModuleInterop --moduleResolution node \
  --target esnext --module ES2020 \
  repro/pr-282-pattern-schema-param/repro.ts
```

- **On `main`** (buggy declaration): fails with `TS2769: No overload matches
  this call` — see `repro.ts` for the exact error.
- **With PR #282's diff applied** (`schema` param restored, `Input` made
  generic): compiles cleanly.

## Fix

PR #282's diff is the correct, minimal fix:

```ts
export const pattern: <Input>(
  schema: Schema<string, Input>,
  re: RegExp,
  message?: string,
) => Schema<string, Input>;
```

Verified locally that this resolves `repro.ts`'s type error without
introducing new ones.
