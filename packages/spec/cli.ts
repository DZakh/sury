#!/usr/bin/env tsx
// `spec` — the AI-first test-spec harness (see the `spec` skill).
//
//   spec check   [id…]   validate + lint + assert fmt-clean + goldens match live
//   spec fmt     [id…]    rewrite specs to canonical byte-deterministic form
//   spec gen     [id…]    (re)generate tests/generated/<id>.gen_test.ts (gitignored)
//   spec update  [id…]    recompute goldens (expression, jsonSchema, examples), then fmt
//   spec new     <id>     scaffold a spec with every dimension present as _skip: todo
//   spec schema          (re)emit specs/spec.schema.json from the Sury format schema
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
    writeFileSync(file, serialize(recomputeGoldens(readSpec(file))));
    console.log(`update ${specId(file)}`);
  }
  cmdGen();
};

const cmdNew = (): void => {
  const id = rest[0];
  if (!id) fail("usage: spec new <id>");
  const skeleton: Spec = {
    schema: { res: "S.unknown", ts: "S.unknown" },
    types: { _skip: "todo(#fill)" },
    jsonSchema: { _skip: "todo(#fill)" },
    instantiations: { _skip: "todo(#instantiations-dimension)" },
    bundleBytes: { _skip: "todo(#bundle-dimension)" },
    properties: { _skip: "todo(#pbt-dimension)" },
    operations: {
      parse: { _skip: "todo(#fill)" },
      decode: { _skip: "todo(#fill)" },
      encode: { _skip: "todo(#fill)" },
    },
  };
  writeFileSync(join(SPECS_DIR, `${id}.yaml`), serialize(skeleton));
  console.log(`new ${id} -> specs/${id}.yaml (fill the _skip: todo dimensions)`);
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

    try {
      if (serialize(recomputeGoldens(obj)) !== canon)
        errs.push(`goldens stale — run \`pnpm spec update ${id}\``);
    } catch (e) {
      errs.push(`schema.ts did not evaluate: ${(e as Error).message}`);
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
    fail("usage: spec <check|fmt|gen|update|new|schema> [id…]");
}
