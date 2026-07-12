#!/usr/bin/env tsx
// `spec` — the AI-first test-spec harness (see the `spec` skill).
//
// Infra (format validity, spec.schema.json) runs on published sury; golden
// execution runs on the dev source. See format.ts / harness.ts. Full usage: HELP below.
import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { join } from "node:path";
import { fileURLToPath } from "node:url";
import { schemaJson, type Spec } from "./format";
import {
  SPECS_DIR,
  SCHEMA_PATH,
  listSpecFiles,
  lintSpecsDir,
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
import { red, green, formatFailure } from "./report";

// A script, not a library — nothing here is exported. Importing it (instead
// of report.ts/harness.ts, which hold the testable logic) would silently run
// no CLI command, so fail loudly instead of doing nothing.
if (fileURLToPath(import.meta.url) !== process.argv[1])
  throw new Error("cli.ts is a script, not a library — import from report.ts or harness.ts instead");

const args = process.argv.slice(2);
const cmd = args[0];
const rest = args.slice(1);

const targets = (ids: string[] = rest): string[] =>
  ids.length
    ? ids.map((id) => {
        const file = join(SPECS_DIR, `${id.replace(/\.yaml$/, "")}.yaml`);
        if (!existsSync(file)) fail(`no such spec: ${id} (expected ${file})`);
        return file;
      })
    : listSpecFiles();

function fail(msg: string): never {
  console.error(red(msg));
  process.exit(1);
}

const HELP = `spec — the AI-first Sury test-spec harness (see the \`spec\` skill)

Usage: spec <command> [args]

Commands:
  new --id <id> --ts <schema>
      Scaffold specs/<id>.yaml from --ts (derives every dimension). Add
      example inputs, then run \`check --write\`.
      e.g. spec new --id string-min --ts "S.string.with(S.min, 3)"

  check [id…] [--write]
      Gate: format-valid, canonical, skips valid, goldens fresh. Read-only
      by default. --write persists whatever's safely fixable; a
      format-invalid spec or a live identity mismatch needs a fix first —
      resolve it, then re-run.

  format [id…]
      Rewrite to canonical form only — no golden recompute.

  schema
      Re-emit spec.schema.json from format.ts. Run after changing the
      format itself; \`check\` fails while it's stale.

  help, --help, -h
      Show this message.

[id…] is a bare id or filename (e.g. "string" or "string.yaml"); omit for every spec.
`;

const cmdHelp = (): void => {
  console.log(HELP);
};

const cmdSchema = (): void => {
  writeFileSync(SCHEMA_PATH, schemaJson());
  console.log(`wrote ${SCHEMA_PATH}`);
};

const cmdFormat = (): void => {
  for (const file of targets()) {
    writeFileSync(file, serialize(readSpec(file)));
    console.log(`format ${specId(file)}`);
  }
};

// Both --id/--ts required — there's nothing sensible to scaffold without a
// schema, and deriving jsonSchema/operations from it up front is the whole
// point of `new`.
const parseNewArgs = (argv: string[]): { id: string; ts: string } => {
  const flags: Record<string, string> = {};
  for (let i = 0; i < argv.length; i++) {
    const a = argv[i];
    if (a === "--id" || a === "--ts") {
      const val = argv[i + 1];
      if (val === undefined) fail(`${a} requires a value`);
      flags[a.slice(2)] = val;
      i++;
    } else {
      fail(`unknown argument ${JSON.stringify(a)} — usage: spec new --id <id> --ts <schema-ts-source>`);
    }
  }
  const id = flags.id;
  const ts = flags.ts;
  if (!id || !ts) fail("usage: spec new --id <id> --ts <schema-ts-source>");
  return { id, ts };
};

const cmdNew = async (): Promise<void> => {
  const { id, ts } = parseNewArgs(rest);
  const file = join(SPECS_DIR, `${id}.yaml`);
  // Overwriting would clobber the one thing the harness can't regenerate:
  // hand-authored example inputs.
  if (existsSync(file))
    fail(`spec ${id} already exists (${file}) — edit it directly, or delete it first to re-scaffold`);
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
  // scaffoldJsonSchema tolerates a schema it can't represent (records the
  // thrown message instead), but scaffoldOperations has no such fallback — a
  // --ts that evaluates without throwing to something that isn't a usable
  // schema (e.g. a typo like "S.strng" evaluating to undefined) throws a raw
  // internal Sury error here instead of this tool's own guiding message.
  let operations: Spec["operations"];
  try {
    operations = scaffoldOperations(schema);
  } catch (e) {
    fail(`--ts evaluated but isn't a usable schema: ${(e as Error).message}`);
  }
  const spec: Spec = {
    ts: {
      schema: ts,
      input: typeInfo.input,
      output: typeInfo.output,
      instantiations: typeInfo.instantiations,
      bundleBytes,
    },
    // `vs` is a required dimension but can't be derived — scaffold a skip so
    // the spec is valid out of the box, and prompt the author (below) to
    // replace it with the real Zod equivalent, or a truthful skip reason.
    vs: { zod: { _skip: "not-applicable" } },
    jsonSchema: scaffoldJsonSchema(schema),
    operations,
  };
  writeFileSync(file, serialize(spec));
  console.log(
    `new ${id} -> specs/${id}.yaml (add example inputs and a \`vs.zod\` equivalent, then \`pnpm spec check ${id} --write\`)`,
  );
};

const WRITE_FLAG = "--write";

// --write persists whatever's safely fixable (canonical form, stale goldens)
// before checking, but deliberately doesn't require the spec to already be
// format-valid: a freshly-added example (just `input`) fails validation until
// --write fills in output/error, so gating on validity up front would make
// --write unable to do the one thing it exists for. Results are collected
// before printing (not logged as each resolves) so concurrent per-file work
// doesn't interleave the report output.
const cmdCheck = async (): Promise<void> => {
  const write = rest.includes(WRITE_FLAG);
  let failed = 0;

  // Existence and freshness are checked as two separate facts, not one
  // `existsSync && readFileSync(...) !== schemaJson()` expression — that
  // would short-circuit to "no failure" for a deleted spec.schema.json
  // instead of reporting it missing.
  const schemaExists = existsSync(SCHEMA_PATH);
  if (!schemaExists || readFileSync(SCHEMA_PATH, "utf8") !== schemaJson()) {
    failed++;
    console.error(
      formatFailure("spec.schema.json", [
        schemaExists ? "stale — run `pnpm spec schema`" : "missing — run `pnpm spec schema`",
      ]),
    );
  }

  const dirErrs = lintSpecsDir();
  if (dirErrs.length) {
    failed++;
    console.error(formatFailure("specs dir", dirErrs));
  }

  const results = await Promise.all(
    targets(rest.filter((a) => a !== WRITE_FLAG)).map(async (file) => {
      const id = specId(file);
      let raw = readFileSync(file, "utf8");
      let obj = readSpec(file);
      // Set when --write's own recompute succeeds, so the checkSpec call
      // below can reuse it instead of redoing the same esbuild+TS-
      // introspection work purely to re-derive what's already known.
      let knownFresh: string | undefined;

      if (write) {
        let schema: any;
        let evaluated = false;
        try {
          schema = evalSchema(obj.ts.schema);
          evaluated = true;
        } catch {
          // fall through — checkSpec below reports the real problem
        }
        // `evaluated`, not `schema` truthiness — ts.schema could evaluate to
        // a legitimately falsy value (e.g. `0`).
        if (evaluated) {
          try {
            if (identityViolations(schema, obj).length === 0) {
              knownFresh = serialize(await recomputeGoldens(obj));
              if (knownFresh !== raw) {
                writeFileSync(file, knownFresh);
                raw = knownFresh;
                obj = readSpec(file);
                console.log(`wrote ${id}`);
              }
            }
          } catch {
            knownFresh = undefined;
            // Not a usable schema, or some other execution failure — skip
            // the write either way; checkSpec below reports the real problem.
          }
        }
      }

      const errs = await checkSpec(id, obj, raw, knownFresh);
      return { id, errs };
    }),
  );

  for (const { id, errs } of results) {
    if (errs.length) {
      failed++;
      console.error(formatFailure(id, errs));
    } else {
      console.log(green(`✓ ${id}`));
    }
  }
  if (failed) fail(`${failed} check(s) failed`);
};

// Wrapped in an async function (instead of top-level await) since this
// project's shared tsconfig.json targets module: ES2020 — too old for
// top-level await.
async function main() {
  switch (cmd) {
    case "check":
      await cmdCheck();
      break;
    case "format":
      cmdFormat();
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
      // A bare `spec` gets just the help text; an actually-unrecognized
      // command gets a header first.
      if (cmd) console.error(red(`Unknown command: ${cmd}\n`));
      console.error(HELP);
      process.exit(1);
  }
}

main();
