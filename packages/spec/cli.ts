#!/usr/bin/env tsx
// `spec` — the AI-first test-spec harness (see the `spec` skill).
//
// Infra (format validity, spec.schema.json) runs on published sury; golden
// execution runs on the dev source. See format.ts / harness.ts. Full usage: HELP below.
import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { basename, join } from "node:path";
import { fileURLToPath } from "node:url";
import { schemaJson, scenariosSchemaJson, type Spec } from "./format";
import {
  SPECS_DIR,
  SCHEMA_PATH,
  BUNDLE_SIZE_PATH,
  SCENARIOS_PATH,
  SCENARIOS_SCHEMA_PATH,
  checkScenarios,
  readScenarios,
  listSpecFiles,
  lintSpecsDir,
  specId,
  parseSpec,
  readSpec,
  serialize,
  collectComments,
  recomputeGoldens,
  evalSchema,
  identityViolations,
  scaffoldJsonSchema,
  scaffoldOperations,
  deriveTypeInfo,
  checkBundleSize,
  checkSpec,
} from "./harness";
import { red, green, formatFailure } from "./report";
import { summarize, renderPerformance, type SpecChange, type BundleSizeChange } from "./summary";
import { runPerf } from "./bench";

// A script, not a library — nothing here is exported. Importing it (instead
// of report.ts/harness.ts, which hold the testable logic) would silently run
// no CLI command, so fail loudly instead of doing nothing.
if (fileURLToPath(import.meta.url) !== process.argv[1])
  throw new Error("cli.ts is a script, not a library — import from report.ts or harness.ts instead");

const args = process.argv.slice(2);
const cmd = args[0];
const rest = args.slice(1);

// An id names a spec file or a scenario (not a file — they live in
// scenarios.yaml), so naming only spec ids selects no scenarios and vice
// versa. `scenarios` undefined = unnarrowed run = all of them.
const resolveIds = (ids: string[]): { files: string[]; scenarios?: string[] } => {
  if (!ids.length) return { files: listSpecFiles() };
  const known = new Set(Object.keys(readScenarios()));
  const files: string[] = [];
  const scenarios: string[] = [];
  for (const raw of ids) {
    const id = raw.replace(/\.yaml$/, "");
    if (known.has(id)) {
      scenarios.push(id);
      continue;
    }
    const file = join(SPECS_DIR, `${id}.yaml`);
    // Sit in the specs dir but aren't specs, so they would otherwise be
    // validated as one.
    if (file === BUNDLE_SIZE_PATH)
      fail(`${id} isn't a spec — bundleSize.yaml is checked by a full \`spec check\` (no [id…])`);
    if (file === SCENARIOS_PATH)
      fail(`${id} isn't a spec — name the scenario you want, or omit [id…] to run every one`);
    if (!existsSync(file)) fail(`no such spec or scenario: ${id} (expected ${file})`);
    files.push(file);
  }
  return { files, scenarios };
};

// A scenario has no file to format, so naming one here fails instead of
// silently no-oping.
const targets = (ids: string[] = rest): string[] => {
  const { files, scenarios } = resolveIds(ids);
  if (scenarios?.length)
    fail(`${scenarios.join(", ")}: a scenario, not a spec — only \`spec check\` runs scenarios`);
  return files;
};

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
      e.g. spec new --id string-min --ts "S.string.with(S.minLength, 3)"

  check [id…] [--write] [--perf=skip|only] [--against <ref>]
      Gate: format-valid, canonical, skips valid, goldens fresh. Read-only
      by default. --write persists whatever's safely fixable, then prints
      what moved (instantiations, generated-code length, bundle size,
      behavior) ranked by percentage; a format-invalid spec or a live
      identity mismatch needs a fix first — resolve it, then re-run.
      bundleSize.yaml is whole-package, so only a full run (no [id…])
      checks or rewrites it.

      Also measures schema creation, creation+compilation, every example,
      and every scenario in scenarios.yaml (a consumer-level call, where
      the dispatch around a compiled operation is inside the timing),
      against the same library built from a git ref — reported as a
      relative delta, never as a stored number, and never affecting the
      exit code. --perf=skip drops it (the fast loop); --perf=only runs
      it alone. --against defaults to the PR base under CI, else the
      merge-base with main. Narrow with [id…] while editing one schema.

  format [id…]
      Rewrite to canonical form only — no golden recompute.

  schema
      Re-emit spec.schema.json and scenarios.schema.json from format.ts.
      Run after changing the format itself; \`check\` fails while stale.

  help, --help, -h
      Show this message.

[id…] is a bare id or filename (e.g. "string" or "string.yaml"), or a scenario
id from scenarios.yaml; omit for every spec and every scenario.
`;

const cmdHelp = (): void => {
  console.log(HELP);
};

// One entry per emitted JSON Schema, so `schema` and the freshness gate below
// can never disagree about which files exist.
const EMITTED_SCHEMAS: [path: string, emit: () => string][] = [
  [SCHEMA_PATH, schemaJson],
  [SCENARIOS_SCHEMA_PATH, scenariosSchemaJson],
];

const cmdSchema = (): void => {
  for (const [path, emit] of EMITTED_SCHEMAS) {
    writeFileSync(path, emit());
    console.log(`wrote ${path}`);
  }
};

const cmdFormat = (): void => {
  for (const file of targets()) {
    const raw = readFileSync(file, "utf8");
    writeFileSync(file, serialize(parseSpec(raw), collectComments(raw)));
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
  const typeInfo = await deriveTypeInfo(ts);
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
    },
    // `vs` is a required dimension but can't be derived — scaffold a `todo`
    // skip (a placeholder, not a claim of no-equivalent) and prompt the author
    // (below) to replace it with the real Zod equivalent.
    vs: { zod: { _skip: "todo(#…)" } },
    jsonSchema: await scaffoldJsonSchema(schema, typeInfo),
    operations,
  };
  writeFileSync(file, serialize(spec));
  console.log(
    `new ${id} -> specs/${id}.yaml (add example inputs and a \`vs.zod\` equivalent, then \`pnpm spec check ${id} --write\`)`,
  );
};

const PERF_MODES = ["skip", "only"] as const;
type PerfMode = (typeof PERF_MODES)[number] | "with";

const parseCheckArgs = (argv: string[]): { write: boolean; perf: PerfMode; against?: string; ids: string[] } => {
  let write = false;
  let perf: PerfMode = "with";
  let against: string | undefined;
  const ids: string[] = [];
  for (let i = 0; i < argv.length; i++) {
    const a = argv[i]!;
    if (a === "--write") write = true;
    else if (a.startsWith("--perf=")) {
      const value = a.slice("--perf=".length);
      if (!(PERF_MODES as readonly string[]).includes(value))
        fail(`--perf must be ${PERF_MODES.join(" or ")} (got ${JSON.stringify(value)}); omit it to check both`);
      perf = value as PerfMode;
    } else if (a === "--against") {
      const value = argv[++i];
      if (value === undefined) fail("--against requires a value");
      against = value;
    } else if (a.startsWith("--")) {
      fail(`unknown flag ${JSON.stringify(a)} — see \`spec help\``);
    } else ids.push(a);
  }
  return { write, perf, against, ids };
};

// A regression is never a failure: wall-clock is advisory, and a gate that
// occasionally cries wolf is a gate nobody reads. A failure to *measure*
// (unresolvable ref, bundle error) is a real error and does exit non-zero.
const measurePerf = async (
  files: string[],
  against?: string,
  scenarios?: string[],
): Promise<void> => {
  try {
    console.log(`\n${renderPerformance(await runPerf(files, against, scenarios))}`);
  } catch (e) {
    fail(`performance: ${(e as Error).message}`);
  }
};

// --write persists whatever's safely fixable (canonical form, stale goldens)
// before checking, but deliberately doesn't require the spec to already be
// format-valid: a freshly-added example (just `input`) fails validation until
// --write fills in output/error, so gating on validity up front would make
// --write unable to do the one thing it exists for. Results are collected
// before printing (not logged as each resolves) so concurrent per-file work
// doesn't interleave the report output.
const cmdCheck = async (): Promise<void> => {
  const { write, perf, against, ids } = parseCheckArgs(rest);
  let failed = 0;

  // Ahead of the --perf=only split: a malformed scenario is a target the perf
  // half cannot build — deriveTargets would surface it as an unattributed
  // TypeError out of the TypeScript scanner — so it fails both paths here.
  // Only on an unnarrowed run, like bundleSize.yaml below: scenarios are
  // whole-file state a narrowed run has nothing to say about, and executing
  // every one would tax the tight `spec check <id>` loop. CI and spec_test.ts
  // run unnarrowed, so that gate still holds.
  const scenarioErrs = ids.length ? [] : checkScenarios();
  if (scenarioErrs.length) {
    failed++;
    console.error(formatFailure("scenarios.yaml", scenarioErrs));
  }

  // Splits the run in two for CI, where the goldens gate and the (advisory,
  // comment-posting) perf report want different jobs and different exit
  // semantics.
  const selected = resolveIds(ids);
  if (perf === "only") {
    if (failed) fail(`${failed} check(s) failed`);
    return measurePerf(selected.files, against, selected.scenarios);
  }

  // bundleSize.yaml measures the package's whole export surface, so it isn't a
  // spec and a narrowed run has nothing to say about it — reporting it stale
  // there would point at a fix (`--write`) the same invocation can't perform.
  // The full run (what CI and spec_test.ts do) is the gate. Kicked off first so
  // its esbuild build overlaps the sync work that follows.
  const bundleSizePromise = ids.length ? null : checkBundleSize();

  // Existence and freshness are checked as two separate facts, not one
  // `existsSync && readFileSync(...) !== schemaJson()` expression — that
  // would short-circuit to "no failure" for a deleted spec.schema.json
  // instead of reporting it missing.
  for (const [path, emit] of EMITTED_SCHEMAS) {
    const exists = existsSync(path);
    if (exists && readFileSync(path, "utf8") === emit()) continue;
    failed++;
    console.error(
      formatFailure(basename(path), [
        exists ? "stale — run `pnpm spec schema`" : "missing — run `pnpm spec schema`",
      ]),
    );
  }

  const dirErrs = lintSpecsDir();
  if (dirErrs.length) {
    failed++;
    console.error(formatFailure("specs dir", dirErrs));
  }

  let bundleSizeChange: BundleSizeChange | undefined;
  const bundleSize = await bundleSizePromise;
  if (bundleSize?.errs.length) {
    // Fully derived, so --write resolves anything the measurement could
    // produce — there's no author-owned part needing a manual fix first, as a
    // spec's goldens can have. A failed measurement has no `fresh` to write,
    // and still reports.
    if (write && bundleSize.fresh !== undefined) {
      writeFileSync(BUNDLE_SIZE_PATH, bundleSize.fresh);
      console.log("wrote bundleSize.yaml");
      bundleSizeChange = { before: bundleSize.before, after: bundleSize.after! };
    } else {
      failed++;
      console.error(formatFailure("bundleSize.yaml", bundleSize.errs));
    }
  }

  const results = await Promise.all(
    selected.files.map(async (file) => {
      const id = specId(file);
      let raw = readFileSync(file, "utf8");
      let obj = readSpec(file);
      // Set when --write's own recompute succeeds, so the checkSpec call
      // below can reuse it instead of redoing the same TS-introspection work
      // purely to re-derive what's already known.
      let knownFresh: string | undefined;
      let change: SpecChange | undefined;

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
              const recomputed = await recomputeGoldens(obj);
              knownFresh = serialize(recomputed, collectComments(raw));
              if (knownFresh !== raw) {
                writeFileSync(file, knownFresh);
                change = { id, before: obj, after: recomputed };
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
      return { id, errs, change };
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

  // What moved, ranked — so the metrics ratchet can be read off the run itself
  // instead of by opening every file that was rewritten.
  const summary = summarize(
    results.flatMap((r) => (r.change ? [r.change] : [])),
    bundleSizeChange,
  );
  if (summary) console.log(`\n${summary}`);

  // After the goldens, so the report reads bottom-up as "what changed, then
  // what it cost". Runs even when a check failed — a stale golden doesn't make
  // the timing less interesting.
  if (perf === "with") await measurePerf(selected.files, against, selected.scenarios);

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
