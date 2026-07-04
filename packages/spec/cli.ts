#!/usr/bin/env tsx
// `spec` — the AI-first test-spec harness (see the `spec` skill).
//
// Infra (format validity, spec.schema.json) runs on published sury; golden
// execution runs on the dev source. See format.ts / harness.ts. Full usage: HELP below.
import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { join } from "node:path";
import { schemaJson, type Spec } from "./format";
import {
  SPECS_DIR,
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
  deriveBundleBytes,
  checkSpec,
} from "./harness";

const args = process.argv.slice(2);
const cmd = args[0];
const rest = args.slice(1);

const targets = (ids: string[] = rest): string[] =>
  ids.length
    ? ids.map((id) => join(SPECS_DIR, `${id.replace(/\.yaml$/, "")}.yaml`))
    : listSpecFiles();

const red = (s: string) => `\x1b[31m${s}\x1b[0m`;
const green = (s: string) => `\x1b[32m${s}\x1b[0m`;

function fail(msg: string): never {
  console.error(red(msg));
  process.exit(1);
}

const HELP = `spec — the AI-first Sury test-spec harness (see the \`spec\` skill)

Usage: spec <command> [args]

Commands:
  new --id <id> --ts <schema>
      Scaffold specs/<id>.yaml: derives jsonSchema, operations, and every
      ts.* dimension from --ts. Add example inputs by hand, then run
      \`check --write\`.
      e.g. spec new --id string.min --ts "S.string.with(S.min, 3)"

  check [id…] [--write]
      The CI gate. For the given spec(s) (or all): validates against the
      format schema, lints every _skip reason, asserts canonical form, and
      verifies goldens are fresh. Never mutates files by default. Pass
      --write to persist whatever's safely fixable (canonical form, stale
      goldens) — skipped for a format-invalid spec or a live identity
      mismatch, which need a human decision instead. Prints a specific,
      actionable message per remaining problem, never just pass/fail.

  fmt [id…]
      Rewrite the given spec(s) (or all) to canonical, byte-deterministic
      form — key order, formatting — without recomputing any golden. Run
      \`check --write\` if goldens themselves need refreshing.

  schema
      Re-emit specs/spec.schema.json from the format schema in format.ts.
      Run this after changing the format itself; \`check\` fails if it's stale.

  help, --help, -h
      Show this message.

[id…] accepts a bare id or a filename (e.g. "string" or "string.yaml"); omit
it to target every *.yaml file under packages/sury/specs/.
`;

const cmdHelp = (): void => {
  console.log(HELP);
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

const cmdNew = async (): Promise<void> => {
  const { id, ts } = parseNewArgs(rest);
  let schema: any;
  try {
    schema = evalSchema(ts);
  } catch (e) {
    fail(`--ts did not evaluate: ${(e as Error).message}`);
  }
  // deriveBundleBytes (genuinely async, esbuild child process) goes first so
  // it's kicked off before deriveTypeInfo's synchronous compiler work runs —
  // see the ordering note on recomputeGoldens in harness.ts.
  const [bundleBytes, typeInfo] = await Promise.all([deriveBundleBytes(ts), deriveTypeInfo(ts)]);
  const spec: Spec = {
    ts: {
      schema: ts,
      input: typeInfo.input,
      output: typeInfo.output,
      instantiations: typeInfo.instantiations,
      bundleBytes,
    },
    jsonSchema: scaffoldJsonSchema(schema),
    operations: scaffoldOperations(schema),
  };
  writeFileSync(join(SPECS_DIR, `${id}.yaml`), serialize(spec));
  console.log(`new ${id} -> specs/${id}.yaml (add example inputs, then \`pnpm spec check ${id} --write\`)`);
};

const WRITE_FLAG = "--write";

// `check` is the CI gate: emitted JSON Schema is current, and every spec is
// format-valid, skip-lint-clean, canonical, with goldens matching live
// behavior — all via harness.checkSpec, so the CLI and the guiding-error
// snapshot tests (tests/spec_errors_test.ts) exercise the exact same code.
// --write additionally persists whatever's safely fixable *before* checking —
// canonical form and stale goldens, via the same recomputeGoldens a bare
// `check` already calls internally to detect staleness. This deliberately
// does NOT require the spec to already be format-valid: a freshly-added
// example (just `input`, per the documented workflow) fails format
// validation until --write fills in output/error, so gating on validity
// would make --write unable to do the one thing it exists for. Safe because
// identityViolations/recomputeGoldens throwing (a genuinely malformed
// ts.schema, an identity mismatch) is caught below and falls through to
// checkSpec's own reporting rather than writing anything. Every file's
// (re)computation runs concurrently; results are collected and printed in
// original order once all resolve, so parallel esbuild/TS work doesn't
// interleave the report output.
const cmdCheck = async (): Promise<void> => {
  const write = rest.includes(WRITE_FLAG);
  let failed = 0;

  if (existsSync(SCHEMA_PATH) && readFileSync(SCHEMA_PATH, "utf8") !== schemaJson()) {
    failed++;
    console.log(red("✗ spec.schema.json"));
    console.log("    stale — run `pnpm spec schema`");
  }

  const results = await Promise.all(
    targets(rest.filter((a) => a !== WRITE_FLAG)).map(async (file) => {
      const id = specId(file);
      let raw = readFileSync(file, "utf8");
      let obj = readSpec(file);

      if (write) {
        let schema: any;
        try {
          schema = evalSchema(obj.ts.schema);
        } catch {
          schema = null;
        }
        if (schema) {
          try {
            if (identityViolations(schema, obj).length === 0) {
              const fresh = serialize(await recomputeGoldens(obj));
              if (fresh !== raw) {
                writeFileSync(file, fresh);
                raw = fresh;
                obj = readSpec(file);
                console.log(`wrote ${id}`);
              }
            }
          } catch {
            // Recompute failed for a reason format validation would also
            // catch (e.g. ts.schema evaluates but isn't really a schema) —
            // skip the write; checkSpec below reports the real problem.
          }
        }
      }

      const errs = await checkSpec(id, obj, raw);
      return { id, errs };
    }),
  );

  for (const { id, errs } of results) {
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

// Wrapped in an async function (instead of top-level await) so the script
// type-checks under this project's shared tsconfig.json, which targets
// module: ES2020 — too old for top-level await (matches the convention
// established in tests/bundle.bench.ts).
async function main() {
  switch (cmd) {
    case "check":
      await cmdCheck();
      break;
    case "fmt":
      cmdFmt();
      break;
    case "new":
      await cmdNew();
      break;
    case "schema":
      cmdSchema();
      break;
    case "help":
    case "--help":
    case "-h":
      cmdHelp();
      break;
    default:
      // A bare `spec` (no command at all) just needs guidance, not a scolding;
      // an actually-unrecognized command gets a clear header before the same
      // help text. Either way exits non-zero — nothing useful happened.
      if (cmd) console.error(red(`Unknown command: ${cmd}\n`));
      console.error(HELP);
      process.exit(1);
  }
}

main();
