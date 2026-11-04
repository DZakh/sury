# The owner's taste

For taste calls, after CLAUDE.md's goal order. Add a line when "switch to X"
overrides the same kind of pick twice.

- Answer where the core already answers it. A parallel walk, second cache or
  one-caller flag loses even when it measures a little better (#456).
- A big rewrite is fine when it removes a hack or shrinks the core.
- Fewer concepts beat fewer bytes. Bundle growth is fine for a faster valid path
  (#459). Generated code size outranks library size.
- Refuse ambiguity at creation with an error naming the fix. Breaking is fine
  when the error names the spelling that restores the old behaviour (#454).
- At or below the noise floor is no difference. Unmeasured is not a reason.
- A bug a fuzzer missed means the fuzzer grows.
- Docs are examples; error messages carry the rest.
