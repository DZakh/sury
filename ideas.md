# Ideas & Changelog

## Changelog

### Unreleased

#### Breaking Changes

- **`S.refine` (JS/TS API):** Changed signature from callback-based to boolean-returning.

  **Before:**

  ```ts
  S.string.with(S.refine, (value, s) => {
    if (value.length > 255) {
      s.fail("String can't be more than 255 characters");
    }
  });
  ```

  **After:**

  ```ts
  S.string.with(S.refine, (value) => value.length <= 255, {
    error: "String can't be more than 255 characters",
  });
  ```

  The refine check now returns `boolean` (`true` = valid, `false` = invalid) instead of calling `s.fail()`. An optional second argument accepts `{ error?: string, path?: string[] }`. Defaults to `"Refinement failed"` when no error message is provided.

  > **Note:** The ReScript `S.refine` API is unchanged. Only the JS/TS `S.refine` / `S.js_refine` wrapper was updated.

#### Migration Guide

1. Invert your condition: the check function should return `true` when valid.
2. Move the error message from `s.fail("...")` to the options object `{ error: "..." }`.
3. If you used `s.fail` with a path, pass `{ path: ["key1", "key2"] }` in the options.

## Ideas

- Consider adding a `message` shorthand on `.with()` (e.g., `schema.with("Must be positive")`) for inline error overrides.
- Explore composable refinement combinators (e.g., `S.and(check1, check2)`) to chain multiple boolean checks on a single schema.
