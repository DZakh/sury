// Reproduction for https://github.com/DZakh/sury/pull/282
//
// `S.pattern`'s runtime implementation (Sury.res, `let pattern = (schema, re, ~message=...)`)
// takes the schema as its first argument, matching every other refinement
// (`S.min`, `S.max`, `S.length`, `S.trim`, ...). But its `.d.ts` declaration
// dropped that parameter:
//
//   export const pattern: (re: RegExp, message?: string) => Schema<string, string>;
//
// This forces `Input` to `string`, so any schema whose Input differs from its
// Output (i.e. it has a `.with(S.to, ...)` transform) fails to type-check when
// passed through `.with(S.pattern, ...)`.
//
// Run with (from packages/sury): npx tsc --noEmit --strict --esModuleInterop
//   --moduleResolution node --target esnext --module ES2020 repro/pr-282-pattern-schema-param/repro.ts
//
// On `main` (buggy `.d.ts`), this fails with:
//   error TS2769: No overload matches this call.
//     Types of parameters 're' and 'schema' are incompatible.
//       Type 'Schema<string, unknown>' is not assignable to type 'RegExp'.
//
// Applying PR #282's fix (adding the `schema` param back to `pattern`'s
// declaration) makes this file type-check cleanly.

import * as S from "../../src/S.js";

const raw = S.schema("unknown" as unknown).with(S.to, S.string, (v) => String(v));

const patterned = raw.with(S.pattern, /^\d+$/);

const check: S.Schema<string, unknown> = patterned;
