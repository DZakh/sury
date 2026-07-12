// Proves the spec harness's error messages are guiding, not just pass/fail:
// for each way a spec can be wrong, snapshot the exact stdout/stderr `spec
// check` prints for it. Goes through report.ts's runCheck — the same
// formatting, color, and stream routing cli.ts uses for a real invocation,
// not a re-implementation — so these are the literal bytes an author or CI
// would see.
import { test, expect, vi } from "vitest";
import { listSpecFiles, readSpec, serialize, specId } from "../../spec/harness";
import { runCheck } from "../../spec/report";
import type { Spec } from "../../spec/format";

// Every test here calls runCheck, which (for a schema that still evaluates)
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

// A baseline whose decode/encode really do compile to the same code as
// parse (unlike `string`'s, where decode skips parse's type check) — the
// only kind of spec `eq-to-parse` applies to.
const eqToParseBaseline = readSpec(listSpecFiles().find((f) => specId(f) === "never")!);

const mutateEqToParse = (patch: (spec: Spec) => void): Spec => {
  const spec = structuredClone(eqToParseBaseline);
  patch(spec);
  return spec;
};

// A baseline whose parse itself is `identity` — proves identity wins over
// eq-to-parse when both would technically hold (there's no `parse` op to
// point `eq-to-parse` at).
const identityParseBaseline = readSpec(listSpecFiles().find((f) => specId(f) === "any")!);

const mutateIdentityParse = (patch: (spec: Spec) => void): Spec => {
  const spec = structuredClone(identityParseBaseline);
  patch(spec);
  return spec;
};

test("stale golden (expression drifted from what the schema actually compiles to)", async () => {
  const spec = mutate((s) => {
    if (s.operations.parse !== "identity") s.operations.parse.expression = "i=>i /* stale */";
  });
  await expect(runCheck("string", serialize(spec))).resolves.toMatchInlineSnapshot(`
    {
      "stderr": "✗ string
        goldens stale — run \`pnpm spec check string --write\` (also formats canonically; use \`pnpm spec format\` for a formatting-only fix):
    @@ -10,7 +10,7 @@
        output: '{ type: "string" }'
      operations:
        parse:
    -     expression: i=>i /* stale */
    +     expression: i=>{typeof i==="string"||e[0](i);return i}
          examples:
            valid:
              input: '"hello"'",
      "stdout": "",
    }
  `);
});

test("stale golden (recorded example output no longer matches live behavior)", async () => {
  const spec = mutate((s) => {
    if (s.operations.parse !== "identity") {
      const ex = s.operations.parse.examples.valid;
      if (ex && "output" in ex) ex.output = '"WRONG"';
    }
  });
  await expect(runCheck("string", serialize(spec))).resolves.toMatchInlineSnapshot(`
    {
      "stderr": "✗ string
        goldens stale — run \`pnpm spec check string --write\` (also formats canonically; use \`pnpm spec format\` for a formatting-only fix):
    @@ -14,7 +14,7 @@
          examples:
            valid:
              input: '"hello"'
    -         output: '"WRONG"'
    +         output: '"hello"'
            empty:
              input: '""'
              output: '""'",
      "stdout": "",
    }
  `);
});

test("invalid _skip reason (not an enum value or todo(#...))", async () => {
  const spec = mutate((s) => {
    s.ts.bundleBytes = { _skip: "because-i-said-so" };
  });
  await expect(runCheck("string", serialize(spec))).resolves.toMatchInlineSnapshot(`
    {
      "stderr": "✗ string
        ts.bundleBytes: invalid _skip reason "because-i-said-so"",
      "stdout": "",
    }
  `);
});

test("not canonical (on-disk text doesn't match the canonical form)", async () => {
  const spec = mutate(() => {});
  const scrambled = serialize(spec).replace("ts:\n", "ts:\n  # a stray comment\n");
  await expect(runCheck("string", scrambled)).resolves.toMatchInlineSnapshot(`
    {
      "stderr": "✗ string
        not canonical — run \`pnpm spec format string\` (or \`pnpm spec check string --write\`, which also refreshes goldens):
    @@ -1,6 +1,5 @@
      # yaml-language-server: $schema=./spec.schema.json
      ts:
    -   # a stray comment
        schema: S.string
        input: string
        output: string",
      "stdout": "",
    }
  `);
});

test("identity claimed but the operation doesn't actually compile to identity", async () => {
  const spec = mutate((s) => {
    s.ts.schema = "S.string.with(S.min, 3)"; // decode/encode are real checks now, not passthroughs
  });
  await expect(runCheck("string", serialize(spec))).resolves.toMatchInlineSnapshot(`
    {
      "stderr": "✗ string
        operations.decode: marked \`identity\` but does not compile to identity — use a full op block with examples
        operations.encode: marked \`identity\` but does not compile to identity — use a full op block with examples
        goldens stale — resolve the identity mismatch above first, then \`pnpm spec check string --write\` can fix it (also formats canonically; use \`pnpm spec format\` for a formatting-only fix):
    @@ -3,21 +3,21 @@
        schema: S.string.with(S.min, 3)
        input: string
        output: string
    -   instantiations: 254
    -   bundleBytes: 3905
    +   instantiations: 5181
    +   bundleBytes: 4380
      jsonSchema:
    -   input: '{ type: "string" }'
    -   output: '{ type: "string" }'
    +   input: '{ type: "string", minLength: 3 }'
    +   output: '{ type: "string", minLength: 3 }'
      operations:
        parse:
    -     expression: i=>{typeof i==="string"||e[0](i);return i}
    +     expression: i=>{typeof i==="string"||e[1](i);i.length>2||e[0](i);return i}
          examples:
            valid:
              input: '"hello"'
              output: '"hello"'
            empty:
              input: '""'
    -         output: '""'
    +         error: String must be 3 or more characters long
            invalid-number:
              input: "42"
              error: Expected string, received 42",
      "stdout": "",
    }
  `);
});

test("full op block claimed but the operation actually compiles to identity", async () => {
  const spec = mutate((s) => {
    s.operations.decode = { expression: "", examples: {} }; // string's decode really is identity
  });
  await expect(runCheck("string", serialize(spec))).resolves.toMatchInlineSnapshot(`
    {
      "stderr": "✗ string
        operations.decode: compiles to identity — use \`identity\` instead of an expression + examples
        goldens stale — resolve the identity mismatch above first, then \`pnpm spec check string --write\` can fix it (also formats canonically; use \`pnpm spec format\` for a formatting-only fix):
    @@ -25,7 +25,10 @@
              input: "null"
              error: Expected string, received null
        decode:
    -     expression: ""
    +     expression: |-
    +       function noopOperation(i) {
    +         return i;
    +       }
          examples: {}
        encode: identity
    ",
      "stdout": "",
    }
  `);
});

test("eq-to-parse claimed but the operation doesn't actually compile to the same code as parse", async () => {
  const spec = mutateEqToParse((s) => {
    s.ts.schema = "S.string.with(S.min, 3)"; // decode drops parse's type check, so it no longer matches
  });
  await expect(runCheck("never", serialize(spec))).resolves.toMatchInlineSnapshot(`
    {
      "stderr": "✗ never
        operations.decode: marked \`eq-to-parse\` but does not compile to the same code as parse — use a full op block with examples
        operations.encode: marked \`eq-to-parse\` but does not compile to the same code as parse — use a full op block with examples
        goldens stale — resolve the identity mismatch above first, then \`pnpm spec check never --write\` can fix it (also formats canonically; use \`pnpm spec format\` for a formatting-only fix):
      # yaml-language-server: $schema=./spec.schema.json
      ts:
        schema: S.string.with(S.min, 3)
    -   input: never
    -   output: never
    -   instantiations: 254
    -   bundleBytes: 3716
    +   input: string
    +   output: string
    +   instantiations: 5181
    +   bundleBytes: 4380
      jsonSchema:
    -   input: "{ not: {} }"
    -   output: "{ not: {} }"
    +   input: '{ type: "string", minLength: 3 }'
    +   output: '{ type: "string", minLength: 3 }'
      operations:
        parse:
    -     expression: i=>{e[0](i);return i}
    +     expression: i=>{typeof i==="string"||e[1](i);i.length>2||e[0](i);return i}
          examples:
            invalid-string:
              input: '"anything"'
    -         error: Expected never, received "anything"
    +         output: '"anything"'
            invalid-undefined:
              input: undefined
    -         error: Expected never, received undefined
    +         error: Expected string, received undefined
        decode: eq-to-parse
        encode: eq-to-parse
    ",
      "stdout": "",
    }
  `);
});

test("full op block claimed but the operation actually compiles to the same code as parse", async () => {
  const spec = mutateEqToParse((s) => {
    s.operations.decode = { expression: "", examples: {} }; // never's decode really does mirror parse
  });
  await expect(runCheck("never", serialize(spec))).resolves.toMatchInlineSnapshot(`
    {
      "stderr": "✗ never
        operations.decode: compiles to the same code as parse — use \`eq-to-parse\` instead of an expression + examples
        goldens stale — resolve the identity mismatch above first, then \`pnpm spec check never --write\` can fix it (also formats canonically; use \`pnpm spec format\` for a formatting-only fix):
    @@ -19,7 +19,7 @@
              input: undefined
              error: Expected never, received undefined
        decode:
    -     expression: ""
    +     expression: i=>{e[0](i);return i}
          examples: {}
        encode: eq-to-parse
    ",
      "stdout": "",
    }
  `);
});

test("eq-to-parse claimed but parse itself is identity — identity wins", async () => {
  const spec = mutateIdentityParse((s) => {
    s.operations.decode = "eq-to-parse"; // any's decode really is identity, not merely eq-to-parse
  });
  await expect(runCheck("any", serialize(spec))).resolves.toMatchInlineSnapshot(`
    {
      "stderr": "✗ any
        operations.decode: compiles to identity — use \`identity\` instead of \`eq-to-parse\`",
      "stdout": "",
    }
  `);
});

test("format validation failure (unrecognized key)", async () => {
  const spec = mutate((s) => {
    (s as unknown as Record<string, unknown>).notAField = true;
  });
  await expect(runCheck("string", serialize(spec))).resolves.toMatchInlineSnapshot(`
    {
      "stderr": "✗ string
        schema: Unrecognized key "notAField"",
      "stdout": "",
    }
  `);
});

test("format validation failure (wrong type for a required field)", async () => {
  const spec = mutate((s) => {
    (s.ts as unknown as Record<string, unknown>).schema = 42;
  });
  await expect(runCheck("string", serialize(spec))).resolves.toMatchInlineSnapshot(`
    {
      "stderr": "✗ string
        schema: Failed at ["ts"]["schema"]: Expected string, received 42
        ts.schema evaluated but isn't a Sury schema",
      "stdout": "",
    }
  `);
});

test("schema source doesn't evaluate (syntax error)", async () => {
  const spec = mutate((s) => {
    s.ts.schema = "S.string->>>not valid js";
  });
  await expect(runCheck("string", serialize(spec))).resolves.toMatchInlineSnapshot(`
    {
      "stderr": "✗ string
        ts.schema did not evaluate: Unexpected token '>>>'",
      "stdout": "",
    }
  `);
});

test("multiple simultaneous problems all get their own guiding message", async () => {
  const spec = mutate((s) => {
    s.ts.bundleBytes = { _skip: "nonsense-reason" };
    if (s.operations.parse !== "identity") s.operations.parse.expression = "i=>i /* stale */";
  });
  await expect(runCheck("string", serialize(spec))).resolves.toMatchInlineSnapshot(`
    {
      "stderr": "✗ string
        ts.bundleBytes: invalid _skip reason "nonsense-reason"
        goldens stale — run \`pnpm spec check string --write\` (also formats canonically; use \`pnpm spec format\` for a formatting-only fix):
    @@ -11,7 +11,7 @@
        output: '{ type: "string" }'
      operations:
        parse:
    -     expression: i=>i /* stale */
    +     expression: i=>{typeof i==="string"||e[0](i);return i}
          examples:
            valid:
              input: '"hello"'",
      "stdout": "",
    }
  `);
});
