// Proves the spec harness's error messages are guiding, not just pass/fail:
// for each way a spec can be wrong, snapshot the exact stdout/stderr `spec
// check` prints for it. Goes through report.ts's runCheck — the same
// formatting, color, and stream routing cli.ts uses for a real invocation,
// not a re-implementation — so these are the literal bytes an author or CI
// would see.
import { test, expect, vi } from "vitest";
import { listSpecFiles, readSpec, serialize, specId } from "../../spec/harness";
import { runCheck } from "../../spec/report";
import { isCreationError, type Spec } from "../../spec/format";

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
    if (s.operations.parse !== "identity" && !isCreationError(s.operations.parse))
      s.operations.parse.expression = "i=>i /* stale */";
  });
  await expect(runCheck("string", serialize(spec))).resolves.toMatchInlineSnapshot(`
    {
      "stderr": "✗ string
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

test("stale golden (recorded example output no longer matches live behavior)", async () => {
  const spec = mutate((s) => {
    if (s.operations.parse !== "identity" && !isCreationError(s.operations.parse)) {
      const ex = s.operations.parse.examples.valid;
      if (ex && "output" in ex) ex.output = '"WRONG"';
    }
  });
  await expect(runCheck("string", serialize(spec))).resolves.toMatchInlineSnapshot(`
    {
      "stderr": "✗ string
        goldens stale — run \`pnpm spec check string --write\` (also formats canonically; use \`pnpm spec format\` for a formatting-only fix):
    @@ -15,7 +15,7 @@
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
    s.ts.instantiations = { _skip: "because-i-said-so" };
  });
  await expect(runCheck("string", serialize(spec))).resolves.toMatchInlineSnapshot(`
    {
      "stderr": "✗ string
        ts.instantiations: invalid _skip reason "because-i-said-so"",
      "stdout": "",
    }
  `);
});

// A baseline whose operations are rejected at creation (an unsupported `.to`),
// so every direction is a `creationError` block instead of compiled code.
const creationErrorBaseline = readSpec(
  listSpecFiles().find((f) => specId(f) === "codec-bool-number-unsupported")!,
);

test("stale creationError golden (recorded message drifted from what the schema actually throws)", async () => {
  const spec = structuredClone(creationErrorBaseline);
  if (isCreationError(spec.operations.parse)) spec.operations.parse.creationError = "stale message";
  await expect(runCheck("codec-bool-number-unsupported", serialize(spec))).resolves.toMatchInlineSnapshot(`
    {
      "stderr": "✗ codec-bool-number-unsupported
        goldens stale — run \`pnpm spec check codec-bool-number-unsupported --write\` (also formats canonically; use \`pnpm spec format\` for a formatting-only fix):
    @@ -12,7 +12,7 @@
        output: '{ type: "number" }'
      operations:
        parse:
    -     creationError: stale message
    +     creationError: "SuryError: Can't decode boolean to number. Use S.to to define a custom decoder"
        decode: eq-to-parse
        encode:
          creationError: "SuryError: Can't decode number to boolean. Use S.to to define a custom decoder"",
      "stdout": "",
    }
  `);
});

test("vs.zod overwrite form records a side that matches ts (should be omitted)", async () => {
  const spec = mutate((s) => {
    // string's ts.input/output are both `string`, and z.string() infers the
    // same — so recording either side is wrong; each matching side must be
    // omitted (its absence is what means "no divergence").
    s.vs.zod = { schema: "z.string()", divergence: "none (contrived)", input: "string", output: "string" };
  });
  await expect(runCheck("string", serialize(spec))).resolves.toMatchInlineSnapshot(`
    {
      "stderr": "✗ string
        vs.zod.input equals ts.input "string" — it matches Sury, so omit \`input\`.
        vs.zod.output equals ts.output "string" — it matches Sury, so omit \`output\`.",
      "stdout": "",
    }
  `);
});

test("vs.zod overwrite form omits both sides (records no divergence — belongs in the bare string form)", async () => {
  const spec = mutate((s) => {
    s.vs.zod = { schema: "z.string()", divergence: "none (contrived)" };
  });
  await expect(runCheck("string", serialize(spec))).resolves.toMatchInlineSnapshot(`
    {
      "stderr": "✗ string
        vs.zod: overwrite form records no divergence (input and output both omitted) — use the bare \`zod: "z.string()"\` string form instead.",
      "stdout": "",
    }
  `);
});

test("vs.zod overwrite form omits a side that actually diverges from ts (must be recorded)", async () => {
  const spec = mutate((s) => {
    // z.string().nullable() infers `string | null`, which diverges from
    // string's ts.input/output (`string`). output records the divergence;
    // input is omitted but shouldn't be.
    s.vs.zod = { schema: "z.string().nullable()", divergence: "adds | null", output: "string | null" };
  });
  await expect(runCheck("string", serialize(spec))).resolves.toMatchInlineSnapshot(`
    {
      "stderr": "✗ string
        vs.zod: input omitted (no divergence) but Zod infers "string | null" !== ts.input "string" — add \`input\` to record the divergent type.",
      "stdout": "",
    }
  `);
});

test("not canonical (on-disk text doesn't match the canonical form)", async () => {
  const spec = mutate(() => {});
  const scrambled = serialize(spec).replace("vs:\n  zod: z.string()\n", "vs: { zod: z.string() }\n");
  await expect(runCheck("string", scrambled)).resolves.toMatchInlineSnapshot(`
    {
      "stderr": "✗ string
        not canonical — run \`pnpm spec format string\` (or \`pnpm spec check string --write\`, which also refreshes goldens):
    @@ -4,7 +4,8 @@
        input: string
        output: string
        instantiations: 254
    - vs: { zod: z.string() }
    + vs:
    +   zod: z.string()
      jsonSchema:
        input: '{ type: "string" }'
        output: '{ type: "string" }'",
      "stdout": "",
    }
  `);
});

test("a compiled op block with no examples (codegen nothing ever runs)", async () => {
  const spec = mutate((s) => {
    if (s.operations.parse !== "identity" && !isCreationError(s.operations.parse))
      s.operations.parse.examples = {};
  });
  await expect(runCheck("string", serialize(spec))).resolves.toMatchInlineSnapshot(`
    {
      "stderr": "✗ string
        operations.parse: no examples — a compiled op block must run at least one input (add a named entry with just \`input\`, then \`--write\` fills the result)",
      "stdout": "",
    }
  `);
});

test("a comment that isn't a FIXME (prose the checker can't verify)", async () => {
  const spec = mutate(() => {});
  const commented = serialize(spec).replace("ts:\n", "ts:\n  # the fastest schema there is\n");
  await expect(runCheck("string", commented)).resolves.toMatchInlineSnapshot(`
    {
      "stderr": "✗ string
        ts.schema: comment "the fastest schema there is" is not allowed — prefix it with \`FIXME:\` if it flags broken behavior to address, or move it to Spec Harness Suggestions in CONTRIBUTING.md if the spec format can't express it",
      "stdout": "",
    }
  `);
});

// The canonical form is rebuilt from the parsed object, so a comment survives
// only if it's re-anchored — otherwise `--write` would silently delete the one
// marker an author leaves behind.
test("a FIXME comment is allowed and survives canonicalization", async () => {
  const spec = mutate(() => {});
  const commented = serialize(spec)
    .replace("ts:\n", "ts:\n  # FIXME: coercion is wrong here (#123)\n")
    .replace("  decode: identity\n", "  decode: identity # FIXME: should not be identity\n");
  await expect(runCheck("string", commented)).resolves.toMatchInlineSnapshot(`
    {
      "stderr": "",
      "stdout": "✓ string",
    }
  `);
});

test("identity claimed but the operation doesn't actually compile to identity", async () => {
  const spec = mutate((s) => {
    s.ts.schema = "S.string.with(S.minLength, 3)"; // decode/encode are real checks now, not passthroughs
  });
  await expect(runCheck("string", serialize(spec))).resolves.toMatchInlineSnapshot(`
    {
      "stderr": "✗ string
        operations.decode: marked \`identity\` but does not compile to identity — use a full op block with examples
        operations.encode: marked \`identity\` but does not compile to identity — use a full op block with examples
        goldens stale — resolve the identity mismatch above first, then \`pnpm spec check string --write\` can fix it (also formats canonically; use \`pnpm spec format\` for a formatting-only fix):
    @@ -3,28 +3,28 @@
        schema: S.string.with(S.minLength, 3)
        input: string
        output: string
    -   instantiations: 254
    +   instantiations: 5181
      vs:
        zod: z.string()
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
    -         error: Expected string, received 42
    +         error: Expected string >= 3, received 42
            invalid-null:
              input: "null"
    -         error: Expected string, received null
    +         error: Expected string >= 3, received null
        decode: identity
        encode: identity
    ",
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
        operations.decode: no examples — a compiled op block must run at least one input (add a named entry with just \`input\`, then \`--write\` fills the result)
        operations.decode: compiles to identity — use \`identity\` instead of an expression + examples
        goldens stale — resolve the identity mismatch above first, then \`pnpm spec check string --write\` can fix it (also formats canonically; use \`pnpm spec format\` for a formatting-only fix):
    @@ -26,7 +26,10 @@
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
    s.ts.schema = "S.string.with(S.minLength, 3)"; // decode drops parse's type check, so it no longer matches
  });
  await expect(runCheck("never", serialize(spec))).resolves.toMatchInlineSnapshot(`
    {
      "stderr": "✗ never
        operations.decode: marked \`eq-to-parse\` but does not compile to the same code as parse — use a full op block with examples
        operations.encode: marked \`eq-to-parse\` but does not compile to the same code as parse — use a full op block with examples
        goldens stale — resolve the identity mismatch above first, then \`pnpm spec check never --write\` can fix it (also formats canonically; use \`pnpm spec format\` for a formatting-only fix):
      # yaml-language-server: $schema=./spec.schema.json
      ts:
        schema: S.string.with(S.minLength, 3)
    -   input: never
    -   output: never
    -   instantiations: 254
    +   input: string
    +   output: string
    +   instantiations: 5181
      vs:
        zod: z.never()
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
    +         error: Expected string >= 3, received undefined
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
        operations.decode: no examples — a compiled op block must run at least one input (add a named entry with just \`input\`, then \`--write\` fills the result)
        operations.decode: compiles to the same code as parse — use \`eq-to-parse\` instead of an expression + examples
        goldens stale — resolve the identity mismatch above first, then \`pnpm spec check never --write\` can fix it (also formats canonically; use \`pnpm spec format\` for a formatting-only fix):
    @@ -20,7 +20,7 @@
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
    s.ts.instantiations = { _skip: "nonsense-reason" };
    if (s.operations.parse !== "identity" && !isCreationError(s.operations.parse))
      s.operations.parse.expression = "i=>i /* stale */";
  });
  await expect(runCheck("string", serialize(spec))).resolves.toMatchInlineSnapshot(`
    {
      "stderr": "✗ string
        ts.instantiations: invalid _skip reason "nonsense-reason"
        goldens stale — run \`pnpm spec check string --write\` (also formats canonically; use \`pnpm spec format\` for a formatting-only fix):
    @@ -12,7 +12,7 @@
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
