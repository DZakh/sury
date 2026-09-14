// Proves the spec harness's error messages are guiding, not just pass/fail:
// for each way a spec can be wrong, snapshot the exact stdout/stderr `spec
// check` prints for it. Goes through report.ts's runCheck - the same
// formatting, color, and stream routing cli.ts uses for a real invocation,
// not a re-implementation - so these are the literal bytes an author or CI
// would see.
import { test, expect, vi } from "vitest";
import { listSpecFiles, readSpec, serialize, specId } from "../../spec/harness";
import { runCheck } from "../../spec/report";
import { isCreationError, type Spec } from "../../spec/format";

// Every test here calls runCheck, which (for a schema that still evaluates)
// runs a full recomputeGoldens - the same cold-start cost documented in
// spec_test.ts's identical vi.setConfig call.
vi.setConfig({ testTimeout: 20_000 });

// A real, valid baseline to mutate per scenario - proves each snapshot is
// triggered by exactly one introduced problem, not an unrelated existing one.
const baseline = readSpec(listSpecFiles().find((f) => specId(f) === "string")!);

const mutate = (patch: (spec: Spec) => void): Spec => {
  const spec = structuredClone(baseline);
  patch(spec);
  return spec;
};

// A baseline whose decode/encode really do compile to the same code as
// parse (unlike `string`'s, where decode skips parse's type check) - the
// only kind of spec `eq-to-parse` applies to.
const eqToParseBaseline = readSpec(listSpecFiles().find((f) => specId(f) === "never")!);

const mutateEqToParse = (patch: (spec: Spec) => void): Spec => {
  const spec = structuredClone(eqToParseBaseline);
  patch(spec);
  return spec;
};

// A baseline whose parse itself is `identity` - proves identity wins over
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
        goldens stale - run \`pnpm spec check string --write\` (also formats canonically; use \`pnpm spec format\` for a formatting-only fix):
    @@ -14,7 +14,7 @@
        zod: z.string()
      operations:
        parse:
    -     expression: i=>i /* stale */
    +     expression: i=>{typeof i==="string"||e[0](i);return i}
          examples:
            valid:
              input: '"hello"'
        ts.aliases["S.schema(S.string)"]: operations.parse.expression differs:
    - i=>i /* stale */
    + i=>{typeof i==="string"||e[0](i);return i}",
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
        goldens stale - run \`pnpm spec check string --write\` (also formats canonically; use \`pnpm spec format\` for a formatting-only fix):
    @@ -18,7 +18,7 @@
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
        goldens stale - run \`pnpm spec check codec-bool-number-unsupported --write\` (also formats canonically; use \`pnpm spec format\` for a formatting-only fix):
    @@ -13,7 +13,7 @@
          _skip: not-applicable
      operations:
        parse:
    -     creationError: stale message
    +     creationError: "SuryError: Can't decode boolean -> number. Define custom codec with S.to"
        decode: eq-to-parse
        encode:
          creationError: "SuryError: Can't decode number -> boolean. Define custom codec with S.to"",
      "stdout": "",
    }
  `);
});

test("vs.zod overwrite form records a side that matches ts (should be omitted)", async () => {
  const spec = mutate((s) => {
    // string's ts.input/output are both `string`, and z.string() infers the
    // same - so recording either side is wrong; each matching side must be
    // omitted (its absence is what means "no divergence").
    s.vs.zod = { schema: "z.string()", divergence: "none (contrived)", input: "string", output: "string" };
  });
  await expect(runCheck("string", serialize(spec))).resolves.toMatchInlineSnapshot(`
    {
      "stderr": "✗ string
        vs.zod.input equals ts.input "string" - it matches Sury, so omit \`input\`.
        vs.zod.output equals ts.output "string" - it matches Sury, so omit \`output\`.",
      "stdout": "",
    }
  `);
});

test("vs.zod overwrite form omits both sides (records no divergence - belongs in the bare string form)", async () => {
  const spec = mutate((s) => {
    s.vs.zod = { schema: "z.string()", divergence: "none (contrived)" };
  });
  await expect(runCheck("string", serialize(spec))).resolves.toMatchInlineSnapshot(`
    {
      "stderr": "✗ string
        vs.zod: overwrite form records no divergence (input and output both omitted) - use the bare \`zod: "z.string()"\` string form instead.",
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
        vs.zod: input omitted (no divergence) but Zod infers "string | null" !== ts.input "string" - add \`input\` to record the divergent type.
        operations.parse.examples.invalid-null: parse rejects this value and the \`vs.zod\` equivalent accepts it, returning null - if the two libraries genuinely read it differently, record it with \`divergence: { reason: ..., zod: "null" }\`; if not, the equivalent is the wrong one",
      "stdout": "",
    }
  `);
});

test("jsonSchema round-trip types are omitted when they match the schema types", async () => {
  const spec = mutate((s) => {
    s.jsonSchema.fromInputType = "string";
    s.jsonSchema.fromOutputType = "string";
  });
  await expect(runCheck("string", serialize(spec))).resolves.toMatchInlineSnapshot(`
    {
      "stderr": "✗ string
        jsonSchema.fromInputType: S.fromJSONSchemaOrThrow(jsonSchema.input) matches ts.input "string" - omit \`fromInputType\`.
        jsonSchema.fromOutputType: S.fromJSONSchemaOrThrow(jsonSchema.output) matches ts.output "string" - omit \`fromOutputType\`.
        goldens stale - run \`pnpm spec check string --write\` (also formats canonically; use \`pnpm spec format\` for a formatting-only fix):
    @@ -8,9 +8,7 @@
        instantiations: 254
      jsonSchema:
        input: '{ type: "string" }'
    -   fromInputType: string
        output: '{ type: "string" }'
    -   fromOutputType: string
      isEqual: strictEqual
      vs:
        zod: z.string()",
      "stdout": "",
    }
  `);
});

test("jsonSchema round-trip types are forbidden when JSON Schema creation fails", async () => {
  const spec = readSpec(listSpecFiles().find((f) => specId(f) === "bigint")!);
  spec.jsonSchema.fromInputType = "bigint";
  spec.jsonSchema.fromOutputType = "bigint";
  await expect(runCheck("bigint", serialize(spec))).resolves.toMatchInlineSnapshot(`
    {
      "stderr": "✗ bigint
        jsonSchema.fromInputType: jsonSchema.input failed to create, so there is no round-trip type to record - omit \`fromInputType\`.
        jsonSchema.fromOutputType: jsonSchema.output failed to create, so there is no round-trip type to record - omit \`fromOutputType\`.
        goldens stale - run \`pnpm spec check bigint --write\` (also formats canonically; use \`pnpm spec format\` for a formatting-only fix):
    @@ -6,9 +6,7 @@
        instantiations: 254
      jsonSchema:
        input: Expected JSON, received bigint
    -   fromInputType: bigint
        output: Expected JSON, received bigint
    -   fromOutputType: bigint
      isEqual: strictEqual
      vs:
        zod: z.bigint()",
      "stdout": "",
    }
  `);
});

test("jsonSchema round-trip types are required when they diverge from the schema types", async () => {
  const spec = readSpec(listSpecFiles().find((f) => specId(f) === "array-minLength")!);
  delete spec.jsonSchema.fromInputType;
  delete spec.jsonSchema.fromOutputType;
  await expect(runCheck("array-minLength", serialize(spec))).resolves.toMatchInlineSnapshot(`
    {
      "stderr": "✗ array-minLength
        jsonSchema.fromInputType: omitted, but S.fromJSONSchemaOrThrow(jsonSchema.input) infers "string[]" !== ts.input "[string, string, ...string[]]" - add \`fromInputType\`.
        jsonSchema.fromOutputType: omitted, but S.fromJSONSchemaOrThrow(jsonSchema.output) infers "string[]" !== ts.output "[string, string, ...string[]]" - add \`fromOutputType\`.
        goldens stale - run \`pnpm spec check array-minLength --write\` (also formats canonically; use \`pnpm spec format\` for a formatting-only fix):
    @@ -6,7 +6,9 @@
        instantiations: 1121
      jsonSchema:
        input: '{ items: { type: "string" }, type: "array", minItems: 2 }'
    +   fromInputType: string[]
        output: '{ items: { type: "string" }, type: "array", minItems: 2 }'
    +   fromOutputType: string[]
      isEqual: (a,b)=>{if(a===b)return true;let n=a.length;if(n!==b.length)return false;for(let i=0;i<n;i++)if(!(a[i]===b[i]))return false;return true}
      vs:
        zod:",
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
        not canonical - run \`pnpm spec format string\` (or \`pnpm spec check string --write\`, which also refreshes goldens):
    @@ -10,7 +10,8 @@
        input: '{ type: "string" }'
        output: '{ type: "string" }'
      isEqual: strictEqual
    - vs: { zod: z.string() }
    + vs:
    +   zod: z.string()
      operations:
        parse:
          expression: i=>{typeof i==="string"||e[0](i);return i}",
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
        operations.parse: no examples - a compiled op block must run at least one input (add a named entry with just \`input\`, then \`--write\` fills the result)",
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
        ts.schema: comment "the fastest schema there is" is not allowed - prefix it with \`FIXME:\` if it flags broken behavior to address, or move it to Spec Harness Suggestions in CONTRIBUTING.md if the spec format can't express it",
      "stdout": "",
    }
  `);
});

// The canonical form is rebuilt from the parsed object, so a comment survives
// only if it's re-anchored - otherwise `--write` would silently delete the one
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
        operations.decode: marked \`identity\` but does not compile to identity - use a full op block with examples
        operations.encode: marked \`identity\` but does not compile to identity - use a full op block with examples
        goldens stale - resolve the identity mismatch above first, then \`pnpm spec check string --write\` can fix it (also formats canonically; use \`pnpm spec format\` for a formatting-only fix):
    @@ -5,29 +5,29 @@
          - S.schema(S.string)
        input: string
        output: string
    -   instantiations: 254
    +   instantiations: 789
      jsonSchema:
    -   input: '{ type: "string" }'
    -   output: '{ type: "string" }'
    +   input: '{ type: "string", minLength: 3 }'
    +   output: '{ type: "string", minLength: 3 }'
      isEqual: strictEqual
      vs:
        zod: z.string()
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
    +         error: Expected string.length >= 3, received ""
            invalid-number:
              input: "42"
    -         error: Expected string, received 42
    +         error: Expected string.length >= 3, received 42
            invalid-null:
              input: "null"
    -         error: Expected string, received null
    +         error: Expected string.length >= 3, received null
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
        operations.decode: no examples - a compiled op block must run at least one input (add a named entry with just \`input\`, then \`--write\` fills the result)
        operations.decode: compiles to identity - use \`identity\` instead of an expression + examples
        goldens stale - resolve the identity mismatch above first, then \`pnpm spec check string --write\` can fix it (also formats canonically; use \`pnpm spec format\` for a formatting-only fix):
    @@ -29,7 +29,7 @@
              input: "null"
              error: Expected string, received null
        decode:
    -     expression: ""
    +     expression: (i) => i
          examples: {}
        encode: identity

        ts.aliases["S.schema(S.string)"]: operations.decode compiles to identity on this alias but not on schema",
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
        operations.decode: marked \`eq-to-parse\` but does not compile to the same code as parse - use a full op block with examples
        operations.encode: marked \`eq-to-parse\` but does not compile to the same code as parse - use a full op block with examples
        goldens stale - resolve the identity mismatch above first, then \`pnpm spec check never --write\` can fix it (also formats canonically; use \`pnpm spec format\` for a formatting-only fix):
      # yaml-language-server: $schema=./spec.schema.json
      ts:
        schema: S.string.with(S.minLength, 3)
    -   input: never
    -   output: never
    -   instantiations: 254
    +   input: string
    +   output: string
    +   instantiations: 789
      jsonSchema:
    -   input: "{ not: {} }"
    -   output: "{ not: {} }"
    - isEqual: (a,b)=>a===b||e[0](a,b)
    +   input: '{ type: "string", minLength: 3 }'
    +   output: '{ type: "string", minLength: 3 }'
    + isEqual: strictEqual
      vs:
        zod: z.never()
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
    +         error: Expected string.length >= 3, received undefined
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
        operations.decode: no examples - a compiled op block must run at least one input (add a named entry with just \`input\`, then \`--write\` fills the result)
        operations.decode: compiles to the same code as parse - use \`eq-to-parse\` instead of an expression + examples
        goldens stale - resolve the identity mismatch above first, then \`pnpm spec check never --write\` can fix it (also formats canonically; use \`pnpm spec format\` for a formatting-only fix):
    @@ -21,7 +21,7 @@
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

test("eq-to-parse claimed but parse itself is identity - identity wins", async () => {
  const spec = mutateIdentityParse((s) => {
    s.operations.decode = "eq-to-parse"; // any's decode really is identity, not merely eq-to-parse
  });
  await expect(runCheck("any", serialize(spec))).resolves.toMatchInlineSnapshot(`
    {
      "stderr": "✗ any
        operations.decode: compiles to identity - use \`identity\` instead of \`eq-to-parse\`",
      "stdout": "",
    }
  `);
});

// An async direction is compiled by a different builder and its examples are
// awaited, so the marker is the author's acknowledgment that the operation's
// whole shape changed - the two directions of getting it wrong are checked
// against a spec that really is async on one side and sync on the other.
const asyncBaseline = readSpec(listSpecFiles().find((f) => specId(f) === "async-assert")!);

const mutateAsync = (patch: (spec: Spec) => void): Spec => {
  const spec = structuredClone(asyncBaseline);
  patch(spec);
  return spec;
};

test("an async operation left unmarked", async () => {
  const spec = mutateAsync((s) => {
    if (s.operations.parse !== "identity" && !isCreationError(s.operations.parse))
      delete s.operations.parse.isAsync;
  });
  await expect(runCheck("async-assert", serialize(spec))).resolves.toMatchInlineSnapshot(`
    {
      "stderr": "✗ async-assert
        operations.parse: is async (the schema has an async transform or refine) - add \`isAsync: true\`, which builds it with the AsPromiseOrReject operations and awaits every example",
      "stdout": "",
    }
  `);
});

test("`isAsync: true` on an operation that is synchronous", async () => {
  const spec = mutateAsync((s) => {
    // async-assert's encode side runs the assert's sync `s: noop` half.
    if (typeof s.operations.encode !== "string" && !isCreationError(s.operations.encode))
      s.operations.encode.isAsync = true;
  });
  await expect(runCheck("async-assert", serialize(spec))).resolves.toMatchInlineSnapshot(`
    {
      "stderr": "",
      "stdout": "✓ async-assert",
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

test("an unrepresentable example output fails the check instead of becoming an error golden", async () => {
  const spec = mutate((s) => {
    s.ts.schema =
      'S.string.with(S.to, S.unknown, { decode: () => Object.assign(Object.create({}), { x: 1 }), encode: () => "x" })';
    s.ts.output = "unknown";
    s.vs.zod = { _skip: "not-applicable" };
  });
  const result = await runCheck("string", serialize(spec));
  expect(result.stdout).toBe("");
  expect(result.stderr).toContain("cannot represent a Object instance as spec source code");
  expect(result.stderr).not.toMatch(/error: cannot represent/);
});

test("operations block omits an op the schema supports", async () => {
  const spec = mutate((s) => {
    delete (s.operations as Partial<Spec["operations"]>).encode;
  });
  await expect(runCheck("string", serialize(spec))).resolves.toMatchInlineSnapshot(`
    {
      "stderr": "✗ string
        schema: Failed at ["operations"]["encode"]: Expected "identity" | "eq-to-parse" | { isAsync: true | undefined; expression: string | { _skip: string; }; resultExpression: string | undefined; examples: { [key: string]: { input: string; output: string; divergence: { reason: string; check: true | string | undefined; ajv: true | string | undefined; zod: string | undefined; } | undefined; } | { input: string; error: string; divergence: { reason: string; check: true | string | undefined; ajv: true | string | undefined; zod: string | undefined; } | undefined; } | { input: string; errorConstructor: string; divergence: { reason: string; check: true | string | undefined; ajv: true | string | undefined; zod: string | undefined; } | undefined; }; }; } | { creationError: string; }, received undefined
        operations.encode: missing - a spec must declare parse, decode, and encode (run \`pnpm spec new\` to scaffold them, or add the block)",
      "stdout": "",
    }
  `);
});

test("_skip on an operation is rejected with a guiding message", async () => {
  const spec = mutate((s) => {
    (s.operations as Record<string, unknown>).parse = { _skip: "not-applicable" };
  });
  await expect(runCheck("string", serialize(spec))).resolves.toMatchInlineSnapshot(`
    {
      "stderr": "✗ string
        schema: Failed at ["operations"]["parse"]: Expected "identity" | { isAsync: true | undefined; expression: string | { _skip: string; }; resultExpression: string | undefined; examples: { [key: string]: { input: string; output: string; divergence: { reason: string; check: true | string | undefined; ajv: true | string | undefined; zod: string | undefined; } | undefined; } | { input: string; error: string; divergence: { reason: string; check: true | string | undefined; ajv: true | string | undefined; zod: string | undefined; } | undefined; } | { input: string; errorConstructor: string; divergence: { reason: string; check: true | string | undefined; ajv: true | string | undefined; zod: string | undefined; } | undefined; }; }; } | { creationError: string; }, received { _skip: "not-applicable"; }
    - At ["operations"]["parse"]["expression"]: Expected string | { _skip: string; }, received undefined
    - At ["operations"]["parse"]["creationError"]: Expected string, received undefined
        operations.parse: _skip is not valid on an operation - use identity, eq-to-parse, a full block with examples, or a creationError",
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
        ts.schema did not evaluate: Expression expected. - if this panic is the contract, add ts.constructionError",
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
        goldens stale - run \`pnpm spec check string --write\` (also formats canonically; use \`pnpm spec format\` for a formatting-only fix):
    @@ -15,7 +15,7 @@
        zod: z.string()
      operations:
        parse:
    -     expression: i=>i /* stale */
    +     expression: i=>{typeof i==="string"||e[0](i);return i}
          examples:
            valid:
              input: '"hello"'
        ts.aliases["S.schema(S.string)"]: operations.parse.expression differs:
    - i=>i /* stale */
    + i=>{typeof i==="string"||e[0](i);return i}",
      "stdout": "",
    }
  `);
});

// ---- the operation matrix --------------------------------------------------
//
// The golden pins one spelling of one outcome; these prove the run notices when
// another spelling answers differently, and when a recorded divergence stops
// being true.

test("a recorded divergence that is no longer true is reported", async () => {
  const spec = mutate((s) => {
    const op = s.operations.parse;
    if (op !== "identity" && !isCreationError(op))
      op.examples.valid!.divergence = { reason: "stale", check: true };
  });
  const { stderr } = await runCheck("string", serialize(spec));
  expect(stderr).toContain("divergence.check agrees with parse - remove it");
});

test("a divergence that names no verifier is reported", async () => {
  const spec = mutate((s) => {
    const op = s.operations.parse;
    if (op !== "identity" && !isCreationError(op))
      op.examples.valid!.divergence = { reason: "the verifiers all agree" };
  });
  const { stderr } = await runCheck("string", serialize(spec));
  expect(stderr).toContain("divergence names no verifier");
});

test("a divergence on a direction other than parse is reported", async () => {
  // decode and encode trust their input where every verifier validates it, so
  // a verifier disagreeing with them is by design, not a fact worth recording.
  const spec = readSpec(listSpecFiles().find((f) => specId(f) === "codec-string-number")!);
  const op = spec.operations.encode;
  if (typeof op !== "string" && !isCreationError(op))
    Object.values(op.examples)[0]!.divergence = { reason: "not parse", zod: "0" };
  const { stderr } = await runCheck("codec-string-number", serialize(spec));
  expect(stderr).toContain("divergence is `parse` only");
});

test("a divergence whose reason is blank is reported", async () => {
  // The verdict says which verifier disagrees; without the reason there is
  // nothing saying why a reader should accept that it does.
  const spec = mutate((s) => {
    const op = s.operations.parse;
    if (op !== "identity" && !isCreationError(op))
      op.examples.valid!.divergence = { reason: "  ", check: "rejected" };
  });
  const { stderr } = await runCheck("string", serialize(spec));
  expect(stderr).toContain("divergence.reason is empty");
});

// ---- the examples themselves -----------------------------------------------
//
// Each of these was previously either invisible or reported as something else -
// a golden that pinned a typo, a name with no case behind it, a staleness diff
// naming the schema. What they have in common is that the spec stayed green.

test("an example input that does not evaluate is reported, not recorded as its own error golden", async () => {
  const spec = mutate((s) => {
    if (s.operations.parse !== "identity" && !isCreationError(s.operations.parse))
      s.operations.parse.examples.typo = { input: "notDefiend", error: "notDefiend is not defined" };
  });
  const { stderr } = await runCheck("string", serialize(spec));
  expect(stderr).toContain("examples.typo: input did not evaluate: notDefiend is not defined");
  expect(stderr).toContain("pins the typo");
});

test("an example input that does not parse is reported instead of being silently repaired", async () => {
  // transpileModule REPAIRS this: `({ a: "hello"` comes back as a complete
  // object literal, so the golden recorded a passing result for an input with
  // no closing brace - and re-recorded it on every `--write`.
  const spec = mutate((s) => {
    if (s.operations.parse !== "identity" && !isCreationError(s.operations.parse))
      s.operations.parse.examples.unbalanced = { input: '{ a: "hello"', error: "x" };
  });
  const { stderr } = await runCheck("string", serialize(spec));
  expect(stderr).toContain("examples.unbalanced: input did not evaluate:");
});

test("two examples with the same input are one case with two names", async () => {
  const spec = mutate((s) => {
    if (s.operations.parse !== "identity" && !isCreationError(s.operations.parse))
      s.operations.parse.examples.alsoValid = { input: '"hello"', output: '"hello"' };
  });
  const { stderr } = await runCheck("string", serialize(spec));
  expect(stderr).toContain("examples.alsoValid: input is identical to `valid`'s");
});

test("an example whose operation answers differently each time is reported as that", async () => {
  // Otherwise it is a `--write` that rewrites the file every run and a `check`
  // that reports a staleness diff naming the schema, neither of which says the
  // golden cannot exist.
  const spec = mutate((s) => {
    s.ts.schema =
      'S.string.with(S.to, S.unknown, { decode: () => Math.random(), encode: () => "x" })';
    s.vs.zod = { _skip: "not-applicable" };
  });
  const { stderr } = await runCheck("string", serialize(spec));
  expect(stderr).toContain("is not deterministic");
  expect(stderr).toContain("so no golden can hold it");
});

test("an example input that calls S.global is refused before anything evaluates it", async () => {
  // An input is evaluated with the same `S` the schema is, so the rule has to
  // cover it too. Reported by CodeRabbit on #434.
  // Spliced into the text rather than built with `serialize`: canonicalizing
  // an example evaluates its input, so serializing this one here would run the
  // very call the test is about.
  const raw = serialize(mutate(() => {})).replace(
    "  decode: identity",
    [
      "      sneaky:",
      `        input: 'S.global({ disableNanNumberCheck: true }) ?? "x"'`,
      `        output: '"x"'`,
      "  decode: identity",
    ].join("\n"),
  );
  const { stderr } = await runCheck("string", raw);
  expect(stderr).toContain("operations.parse.examples.sneaky.input: calls S.global");
  expect(stderr).not.toContain("goldens stale");
});

test("a spec that calls S.global is refused before anything evaluates it", async () => {
  // One library instance is shared by every spec in a run, so this one would
  // change what its neighbours compile. The check reports it and stops - the
  // schema is never evaluated, which is the only way the rule can hold.
  const spec = mutate((s) => {
    s.ts.schema = "S.global({ disableNanNumberCheck: true }) ?? S.string";
  });
  const { stderr } = await runCheck("string", serialize(spec));
  expect(stderr).toContain("ts.schema: calls S.global");
  expect(stderr).toContain("the failure lands somewhere else and only sometimes");
  // Nothing past the lint ran, so no golden was recomputed against a
  // reconfigured library.
  expect(stderr).not.toContain("goldens stale");
});

// ---- the cross-checks ------------------------------------------------------

test("a JSON Schema that rejects a value parse accepts must be recorded or fixed", async () => {
  // `S.minLength` counts JS characters and `minLength` counts code points, so
  // the published document turns away the emoji the parser takes.
  const spec = readSpec(listSpecFiles().find((f) => specId(f) === "string-minLength")!);
  const op = spec.operations.parse;
  if (op !== "identity" && !isCreationError(op)) delete op.examples.emoji!.divergence;
  const { stderr } = await runCheck("string-minLength", serialize(spec));
  expect(stderr).toContain("parse accepts this value but jsonSchema.input rejects it");
  expect(stderr).toContain("record it with `divergence: { reason: ..., ajv:");
});

test("a `vs.zod` equivalent that reads a value differently must be recorded", async () => {
  const spec = readSpec(listSpecFiles().find((f) => specId(f) === "integer")!);
  const op = spec.operations.parse;
  if (op !== "identity" && !isCreationError(op))
    for (const ex of Object.values(op.examples)) delete ex.divergence;
  const { stderr } = await runCheck("integer", serialize(spec));
  expect(stderr).toContain("the `vs.zod` equivalent");
  expect(stderr).toContain("record it with `divergence: { reason: ..., zod:");
});

test("an error thrown by something other than Sury carries its class into the golden", async () => {
  // A bare message reads exactly like a Sury rejection, so a leaked TypeError
  // or a foreign exception out of user code was indistinguishable from
  // intended behaviour.
  const spec = readSpec(listSpecFiles().find((f) => specId(f) === "string-to-blob")!);
  const op = spec.operations.decode;
  if (typeof op !== "string" && !isCreationError(op)) {
    const ex = op.examples["not-a-part"]!;
    if ("error" in ex) ex.error = "boom";
  }
  const { stderr } = await runCheck("string-to-blob", serialize(spec));
  expect(stderr).toContain("-         error: boom");
  expect(stderr).toContain('+         error: "Error: boom"');
});
