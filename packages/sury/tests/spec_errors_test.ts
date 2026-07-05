// Proves the spec harness's error messages are guiding, not just pass/fail:
// for each way a spec can be wrong, snapshot the exact message(s) checkSpec
// produces and confirm they name the problem AND say what to run about it.
// Uses the same checkSpec that `pnpm spec check` calls (see cli.ts's cmdCheck)
// — these are the real messages an author or CI would see, not a re-implementation.
import { test, expect, vi } from "vitest";
import { checkSpec, listSpecFiles, readSpec, serialize, specId } from "../../spec/harness";
import type { Spec } from "../../spec/format";

// Every test here calls checkSpec, which (for a schema that still evaluates)
// runs a full recomputeGoldens — the same cold-start cost documented in
// spec_test.ts's identical vi.setConfig call.
vi.setConfig({ testTimeout: 20_000 });

// A real, valid baseline to mutate per scenario — proves each snapshot is
// triggered by exactly one introduced problem, not an unrelated existing one.
const baseline = readSpec(listSpecFiles().find((f) => specId(f) === "string")!);

const mutate = (patch: (spec: Spec) => void): Spec => {
  const spec = structuredClone(baseline);
  patch(spec);
  return spec;
};

test("stale golden (expression drifted from what the schema actually compiles to)", async () => {
  const spec = mutate((s) => {
    if (s.operations.parse !== "identity") s.operations.parse.expression = "i=>i /* stale */";
  });
  await expect(checkSpec("string", spec, serialize(spec))).resolves.toMatchInlineSnapshot(`
    [
      "goldens stale — run \`pnpm spec check string --write\` (also formats canonically; use \`pnpm spec fmt\` for a formatting-only fix)",
    ]
  `);
});

test("stale golden (recorded example output no longer matches live behavior)", async () => {
  const spec = mutate((s) => {
    if (s.operations.parse !== "identity") {
      const ex = s.operations.parse.examples.valid;
      if (ex && "output" in ex) ex.output = '"WRONG"';
    }
  });
  await expect(checkSpec("string", spec, serialize(spec))).resolves.toMatchInlineSnapshot(`
    [
      "goldens stale — run \`pnpm spec check string --write\` (also formats canonically; use \`pnpm spec fmt\` for a formatting-only fix)",
    ]
  `);
});

test("invalid _skip reason (not an enum value or todo(#...))", async () => {
  const spec = mutate((s) => {
    s.ts.bundleBytes = { _skip: "because-i-said-so" };
  });
  await expect(checkSpec("string", spec, serialize(spec))).resolves.toMatchInlineSnapshot(`
    [
      "string.ts.bundleBytes: invalid _skip reason "because-i-said-so"",
    ]
  `);
});

test("not canonical (on-disk text doesn't match the canonical form)", async () => {
  const spec = mutate(() => {});
  const scrambled = serialize(spec).replace("ts:\n", "ts:\n  # a stray comment\n");
  await expect(checkSpec("string", spec, scrambled)).resolves.toMatchInlineSnapshot(`
    [
      "not canonical — run \`pnpm spec fmt string\` (or \`pnpm spec check string --write\`, which also refreshes goldens)",
    ]
  `);
});

test("identity claimed but the operation doesn't actually compile to identity", async () => {
  const spec = mutate((s) => {
    s.ts.schema = "S.string.with(S.min, 3)"; // decode/encode are real checks now, not passthroughs
  });
  await expect(checkSpec("string", spec, serialize(spec))).resolves.toMatchInlineSnapshot(`
    [
      "operations.decode: marked \`identity\` but does not compile to identity — use a full op block with examples",
      "operations.encode: marked \`identity\` but does not compile to identity — use a full op block with examples",
      "goldens stale — resolve the identity mismatch above first, then \`pnpm spec check string --write\` can fix it (also formats canonically; use \`pnpm spec fmt\` for a formatting-only fix)",
    ]
  `);
});

test("full op block claimed but the operation actually compiles to identity", async () => {
  const spec = mutate((s) => {
    s.operations.decode = { expression: "", examples: {} }; // string's decode really is identity
  });
  await expect(checkSpec("string", spec, serialize(spec))).resolves.toMatchInlineSnapshot(`
    [
      "operations.decode: compiles to identity — use \`identity\` instead of an expression + examples",
      "goldens stale — resolve the identity mismatch above first, then \`pnpm spec check string --write\` can fix it (also formats canonically; use \`pnpm spec fmt\` for a formatting-only fix)",
    ]
  `);
});

test("format validation failure (unrecognized key)", async () => {
  const spec = mutate((s) => {
    (s as unknown as Record<string, unknown>).notAField = true;
  });
  await expect(checkSpec("string", spec, serialize(spec))).resolves.toMatchInlineSnapshot(`
    [
      "schema: Unrecognized key "notAField"",
    ]
  `);
});

test("format validation failure (wrong type for a required field)", async () => {
  const spec = mutate((s) => {
    (s.ts as unknown as Record<string, unknown>).schema = 42;
  });
  await expect(checkSpec("string", spec, serialize(spec))).resolves.toMatchInlineSnapshot(`
    [
      "schema: Failed at ["ts"]["schema"]: Expected string, received 42",
      "ts.schema evaluated but isn't a Sury schema",
    ]
  `);
});

test("schema source doesn't evaluate (syntax error)", async () => {
  const spec = mutate((s) => {
    s.ts.schema = "S.string->>>not valid js";
  });
  await expect(checkSpec("string", spec, serialize(spec))).resolves.toMatchInlineSnapshot(`
    [
      "ts.schema did not evaluate: Unexpected token '>>>'",
    ]
  `);
});

test("multiple simultaneous problems all get their own guiding message", async () => {
  const spec = mutate((s) => {
    s.ts.bundleBytes = { _skip: "nonsense-reason" };
    if (s.operations.parse !== "identity") s.operations.parse.expression = "i=>i /* stale */";
  });
  await expect(checkSpec("string", spec, serialize(spec))).resolves.toMatchInlineSnapshot(`
    [
      "string.ts.bundleBytes: invalid _skip reason "nonsense-reason"",
      "goldens stale — run \`pnpm spec check string --write\` (also formats canonically; use \`pnpm spec fmt\` for a formatting-only fix)",
    ]
  `);
});
