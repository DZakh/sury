#!/usr/bin/env tsx
// `spec` — the AI-first test-spec harness (see the `spec` skill).
//
//   spec check   [id…]              validate + lint + assert fmt-clean + goldens match live
//   spec fmt     [id…]               rewrite specs to canonical byte-deterministic form
//   spec gen     [id…]               (re)generate tests/generated/<id>.gen_test.ts (gitignored)
//   spec update  [id…]               recompute goldens (expression, jsonSchema, examples), then fmt
//   spec new --id <id> --ts <schema> scaffold a spec; jsonSchema, operations,
//                                     and ts.input/output/instantiations are
//                                     all auto-derived from --ts (only example
//                                     inputs need manual authoring after)
//   spec schema                     (re)emit specs/spec.schema.json from the Sury format schema
//
// Infra (format validity, spec.schema.json) runs on published sury; golden
// execution runs on the dev source. See format.ts / harness.ts.
import { readFileSync, writeFileSync, mkdirSync, existsSync } from "node:fs";
import { join } from "node:path";
import { validate, schemaJson, isSkip, type Spec } from "./format";
import {
  SPECS_DIR,
  GEN_DIR,
  SCHEMA_PATH,
  listSpecFiles,
  specId,
  readSpec,
  serialize,
  recomputeGoldens,
  evalSchema,
  identityViolations,
  scaffoldJsonSchema,
  scaffoldOperations,
  deriveTypeInfo,
  generateTest,
  genPath,
  isValidSkipReason,
} from "./harness";

const args = process.argv.slice(2);
const cmd = args[0];
const rest = args.slice(1);

const targets = (): string[] =>
  rest.length
    ? rest.map((id) => join(SPECS_DIR, `${id.replace(/\.yaml$/, "")}.yaml`))
    : listSpecFiles();

const red = (s: string) => `\x1b[31m${s}\x1b[0m`;
const green = (s: string) => `\x1b[32m${s}\x1b[0m`;

function fail(msg: string): never {
  console.error(red(msg));
  process.exit(1);
}

// Walk every `_skip` in a spec and collect malformed reasons.
const lintSkips = (obj: unknown, path: string, out: string[]): void => {
  if (isSkip(obj)) {
    if (!isValidSkipReason(obj._skip))
      out.push(`${path}: invalid _skip reason ${JSON.stringify(obj._skip)}`);
    return;
  }
  if (obj && typeof obj === "object")
    for (const [k, v] of Object.entries(obj)) lintSkips(v, `${path}.${k}`, out);
};

const cmdSchema = (): void => {
  writeFileSync(SCHEMA_PATH, schemaJson());
  console.log(`wrote ${SCHEMA_PATH}`);
};

const cmdFmt = (): void => {
  for (const file of targets()) {
    writeFileSync(file, serialize(readSpec(file)));
    console.log(`fmt ${specId(file)}`);
  }
};

const cmdGen = (): void => {
  mkdirSync(GEN_DIR, { recursive: true });
  for (const file of targets()) {
    const id = specId(file);
    writeFileSync(genPath(id), generateTest(id, readSpec(file)));
    console.log(`gen ${id} -> tests/generated/${id}.gen_test.ts`);
  }
};

const cmdUpdate = (): void => {
  for (const file of targets()) {
    const id = specId(file);
    const obj = readSpec(file);
    let schema: any;
    try {
      schema = evalSchema(obj.ts.schema);
    } catch (e) {
      fail(`${id}: ts.schema did not evaluate: ${(e as Error).message}`);
    }
    const violations = identityViolations(schema, obj);
    if (violations.length) fail(`${id}:\n    ${violations.join("\n    ")}`);
    writeFileSync(file, serialize(recomputeGoldens(obj)));
    console.log(`update ${id}`);
  }
  cmdGen();
};

// Parse `--id <id> --ts <schema>`. Both required — there's nothing sensible to
// scaffold without a schema, and deriving jsonSchema/operations from it up
// front is the whole point of `new` (see harness.scaffold*).
const parseNewArgs = (argv: string[]): { id: string; ts: string } => {
  const flags: Record<string, string> = {};
  for (let i = 0; i < argv.length; i++) {
    const a = argv[i];
    if (a === "--id" || a === "--ts") {
      const val = argv[i + 1];
      if (val === undefined) fail(`${a} requires a value`);
      flags[a.slice(2)] = val;
      i++;
    }
  }
  const id = flags.id;
  const ts = flags.ts;
  if (!id || !ts) fail("usage: spec new --id <id> --ts <schema-ts-source>");
  return { id, ts };
};

const cmdNew = (): void => {
  const { id, ts } = parseNewArgs(rest);
  let schema: any;
  try {
    schema = evalSchema(ts);
  } catch (e) {
    fail(`--ts did not evaluate: ${(e as Error).message}`);
  }
  const typeInfo = deriveTypeInfo(ts);
  const spec: Spec = {
    ts: {
      schema: ts,
      input: typeInfo.input,
      output: typeInfo.output,
      instantiations: typeInfo.instantiations,
      bundleBytes: { _skip: "todo(#bundle-dimension)" },
    },
    jsonSchema: scaffoldJsonSchema(schema),
    operations: scaffoldOperations(schema),
  };
  writeFileSync(join(SPECS_DIR, `${id}.yaml`), serialize(spec));
  console.log(`new ${id} -> specs/${id}.yaml (add example inputs, then \`pnpm spec update ${id}\`)`);
};

// `check` is the CI gate: emitted JSON Schema is current, and every spec is
// format-valid, skip-lint-clean, canonical, with goldens matching live behavior.
const cmdCheck = (): void => {
  let failed = 0;

  if (existsSync(SCHEMA_PATH) && readFileSync(SCHEMA_PATH, "utf8") !== schemaJson()) {
    failed++;
    console.log(red("✗ spec.schema.json"));
    console.log("    stale — run `pnpm spec schema`");
  }

  for (const file of targets()) {
    const id = specId(file);
    const errs: string[] = [];
    const raw = readFileSync(file, "utf8");
    const obj = readSpec(file);

    const v = validate(obj);
    if (!v.ok) errs.push(`schema: ${v.error}`);

    lintSkips(obj, id, errs);

    const canon = serialize(obj);
    if (raw !== canon) errs.push(`not canonical — run \`pnpm spec fmt ${id}\``);

    let schema: any;
    try {
      schema = evalSchema(obj.ts.schema);
    } catch (e) {
      errs.push(`ts.schema did not evaluate: ${(e as Error).message}`);
    }
    if (schema) {
      // A spec that failed format validation above may not have the shape
      // identityViolations/recomputeGoldens assume (e.g. a missing
      // `examples` map) — catch so one malformed file doesn't abort the
      // whole batch; the validation error already reported above is the
      // actionable one.
      try {
        // identity invariant: noop op <-> the literal `identity`
        for (const v of identityViolations(schema, obj)) errs.push(v);
        // goldens must equal what the live schema produces (no hand-edits)
        if (serialize(recomputeGoldens(obj)) !== canon)
          errs.push(`goldens stale — run \`pnpm spec update ${id}\``);
      } catch (e) {
        errs.push(`goldens could not be computed: ${(e as Error).message}`);
      }
    }

    if (errs.length) {
      failed++;
      console.log(red(`✗ ${id}`));
      for (const e of errs) console.log(`    ${e}`);
    } else {
      console.log(green(`✓ ${id}`));
    }
  }
  if (failed) fail(`${failed} check(s) failed`);
};

switch (cmd) {
  case "check":
    cmdCheck();
    break;
  case "fmt":
    cmdFmt();
    break;
  case "gen":
    cmdGen();
    break;
  case "update":
    cmdUpdate();
    break;
  case "new":
    cmdNew();
    break;
  case "schema":
    cmdSchema();
    break;
  default:
    fail(
      "usage: spec <check|fmt|gen|update|schema> [id…] | spec new --id <id> --ts <schema>",
    );
}
