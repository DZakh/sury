#!/usr/bin/env node
// `spec` — the AI-first test-spec harness.
//
//   spec check   [id…]   validate + lint + assert fmt-clean + generated files current
//   spec fmt     [id…]    rewrite specs to canonical byte-deterministic form
//   spec gen     [id…]    (re)generate committed tests/generated/<id>.gen_test.ts
//   spec update  [id…]    recompute goldens (expression, jsonSchema, examples), then fmt
//   spec new     <id>     scaffold a spec with every dimension present as _skip: todo
//   spec schema          (re)emit specs/spec.schema.json from the Sury format schema
//
// One spec file = one schema's full contract. See CONTRIBUTING "Specs".
import { readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import * as S from "../../src/S.js";
import { specSchema, OP_ORDER, isSkip } from "./format.mjs";
import {
  SPECS_DIR,
  SCHEMA_PATH,
  listSpecFiles,
  specId,
  readSpec,
  validate,
  serialize,
  recomputeGoldens,
  generateTest,
  genPath,
  sha256,
  isValidSkipReason,
} from "./harness.mjs";

const args = process.argv.slice(2);
const cmd = args[0];
const rest = args.slice(1);

// Resolve requested ids (or all specs) to file paths.
const targets = () =>
  rest.length
    ? rest.map((id) => join(SPECS_DIR, `${id.replace(/\.yaml$/, "")}.yaml`))
    : listSpecFiles();

const red = (s) => `\x1b[31m${s}\x1b[0m`;
const green = (s) => `\x1b[32m${s}\x1b[0m`;

// Walk every `_skip` in a spec and collect malformed reasons.
const lintSkips = (obj, path, out) => {
  if (isSkip(obj)) {
    if (!isValidSkipReason(obj._skip))
      out.push(`${path}: invalid _skip reason ${JSON.stringify(obj._skip)}`);
    return;
  }
  if (obj && typeof obj === "object")
    for (const [k, v] of Object.entries(obj)) lintSkips(v, `${path}.${k}`, out);
};

const emitSchema = () => {
  const js = S.toJSONSchema(specSchema);
  writeFileSync(SCHEMA_PATH, JSON.stringify(js, null, 2) + "\n");
  console.log(`wrote ${SCHEMA_PATH}`);
};

const cmdFmt = () => {
  for (const file of targets()) {
    const obj = readSpec(file);
    writeFileSync(file, serialize(obj));
    console.log(`fmt ${specId(file)}`);
  }
};

const cmdGen = () => {
  for (const file of targets()) {
    const id = specId(file);
    const specText = readFileSync(file, "utf8");
    const obj = readSpec(file);
    writeFileSync(genPath(id), generateTest(id, obj, specText));
    console.log(`gen ${id} -> tests/generated/${id}.gen_test.ts`);
  }
};

const cmdUpdate = () => {
  for (const file of targets()) {
    const obj = recomputeGoldens(readSpec(file));
    writeFileSync(file, serialize(obj));
    console.log(`update ${specId(file)}`);
  }
  cmdGen();
};

const cmdNew = () => {
  const id = rest[0];
  if (!id) return fail("usage: spec new <id>");
  const skeleton = {
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
  const file = join(SPECS_DIR, `${id}.yaml`);
  writeFileSync(file, serialize(skeleton));
  console.log(`new ${id} -> specs/${id}.yaml (fill the _skip: todo dimensions)`);
};

// `check` is the CI gate: format schema validity, skip-reason lint, canonical
// (fmt-clean) form, and generated files matching the current spec byte-for-byte.
const cmdCheck = () => {
  let failed = 0;
  for (const file of targets()) {
    const id = specId(file);
    const errs = [];
    const raw = readFileSync(file, "utf8");
    const obj = readSpec(file);

    const v = validate(obj);
    if (!v.ok) errs.push(`schema: ${v.error}`);

    lintSkips(obj, id, errs);

    const canon = serialize(obj);
    if (raw !== canon) errs.push(`not canonical — run \`pnpm spec fmt ${id}\``);

    // Goldens must equal what the live schema produces — no hand-edited goldens.
    try {
      if (serialize(recomputeGoldens(obj)) !== canon)
        errs.push(`goldens stale — run \`pnpm spec update ${id}\``);
    } catch (e) {
      errs.push(`schema.ts did not evaluate: ${e.message}`);
    }

    try {
      const gen = generateTest(id, obj, raw);
      const existing = readFileSync(genPath(id), "utf8");
      if (existing !== gen)
        errs.push(`generated test stale — run \`pnpm spec gen ${id}\``);
      const stamp = existing.match(/source-sha256: (\w+)/)?.[1];
      if (stamp && stamp !== sha256(raw))
        errs.push(`generated test sha mismatch — run \`pnpm spec gen ${id}\``);
    } catch {
      errs.push(`no generated test — run \`pnpm spec gen ${id}\``);
    }

    if (errs.length) {
      failed++;
      console.log(red(`✗ ${id}`));
      for (const e of errs) console.log(`    ${e}`);
    } else {
      console.log(green(`✓ ${id}`));
    }
  }
  if (failed) fail(`${failed} spec(s) failed check`);
};

function fail(msg) {
  console.error(red(msg));
  process.exit(1);
}

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
    emitSchema();
    break;
  default:
    fail(
      "usage: spec <check|fmt|gen|update|new|schema> [id…]",
    );
}
