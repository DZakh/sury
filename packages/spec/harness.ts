// Harness SUBJECT half: canonicalize specs and (re)compute goldens by
// executing the real schema.
//
// Unlike format.ts (which runs on published sury), this half imports the
// in-development sury SOURCE (`../sury/src/S.js`), because goldens must reflect
// the code under test — that's how `spec check`/`update` catch codegen changes.
//
// There is no code-generation step: packages/sury/tests/spec_test.ts is a
// single, committed, hand-written Vitest file that dynamically loops over
// listSpecFiles()/readSpec() at test-run time and calls straight into this
// module (recomputeGoldens, in particular) — so example execution and
// jsonSchema/instantiations/bundleBytes drift are all exercised, and covered,
// by a real Vitest run without ever materializing a generated .ts file per spec.
import { readFileSync, readdirSync } from "node:fs";
import { join, basename } from "node:path";
import { fileURLToPath } from "node:url";
import { parse as parseYaml, stringify as stringifyYaml } from "yaml";
import * as S from "../sury/src/S.js";
import {
  KEY_ORDER,
  TS_KEY_ORDER,
  OP_ORDER,
  isSkip,
  validate,
  type Spec,
  type Operation,
  type Example,
  type OpName,
} from "./format";
import { deriveTypeInfo } from "./introspect";
import { deriveBundleBytes } from "./bundleSize";

const here = (rel: string) => fileURLToPath(new URL(rel, import.meta.url));
// The spec suite lives in the sury package (specs ship with it).
export const SPECS_DIR = here("../sury/specs/");
export const SCHEMA_PATH = join(SPECS_DIR, "spec.schema.json");

const HEADER = "# yaml-language-server: $schema=./spec.schema.json";

// Maps an operation to the dev-sury builder that compiles that direction.
const OP_BUILDER: Record<OpName, (schema: any) => (input: any) => any> = {
  parse: S.parser,
  decode: S.decoder,
  encode: S.encoder,
};

// The reason string on a `_skip` must be a known enum value or `todo(#…)`.
// (`identity` isn't here — it's no longer a `_skip` reason, see Operation.)
const SKIP_REASONS = new Set([
  "parser-only",
  "serializer-only",
  "lossy",
  "not-applicable",
]);
export const isValidSkipReason = (r: unknown): boolean =>
  typeof r === "string" && (SKIP_REASONS.has(r) || /^todo\(#.+\)$/.test(r));

// Walk every `_skip` in a spec and collect malformed reasons.
export const lintSkips = (obj: unknown, path: string, out: string[]): void => {
  if (isSkip(obj)) {
    if (!isValidSkipReason(obj._skip))
      out.push(`${path}: invalid _skip reason ${JSON.stringify(obj._skip)}`);
    return;
  }
  if (obj && typeof obj === "object")
    for (const [k, v] of Object.entries(obj)) lintSkips(v, `${path}.${k}`, out);
};

export const specId = (file: string): string =>
  basename(file).replace(/\.yaml$/, "");

export const listSpecFiles = (): string[] =>
  readdirSync(SPECS_DIR)
    .filter((f) => f.endsWith(".yaml"))
    .map((f) => join(SPECS_DIR, f))
    .sort();

export const readSpec = (file: string): Spec =>
  parseYaml(readFileSync(file, "utf8")) as Spec;

export const evalSchema = (tsSource: string): any =>
  new Function("S", `return (${tsSource});`)(S);

// Sury compiles a pass-through operation to this shared function — the ONLY
// signal identity detection has. If this name is ever changed in Sury's
// source, every `identity`-marked operation starts failing loudly (across
// every spec, in `identityViolations` below) rather than silently going stale.
const NOOP_OPERATION_WHICH_WILL_NEVER_CHANGE = "noopOperation";
const isNoop = (fn: Function): boolean =>
  fn.name === NOOP_OPERATION_WHICH_WILL_NEVER_CHANGE;

// Enforce the identity invariant both ways: an operation compiles to Sury's
// pass-through *iff* it is declared the literal `identity`. Returns violation
// messages ([] if ok). Requires the live (dev) schema.
export const identityViolations = (schema: any, spec: Spec): string[] => {
  const out: string[] = [];
  for (const opName of OP_ORDER) {
    const op = spec.operations[opName];
    const noop = isNoop(OP_BUILDER[opName](schema));
    if (op === "identity") {
      if (!noop)
        out.push(
          `operations.${opName}: marked \`identity\` but does not compile to identity — use a full op block with examples`,
        );
    } else if (noop) {
      out.push(
        `operations.${opName}: compiles to identity — use \`identity\` instead of an expression + examples`,
      );
    }
  }
  return out;
};

// Fully derive the jsonSchema dimension from a live schema — no example inputs
// needed, so `spec new` can fill this in immediately from `--ts`.
export const scaffoldJsonSchema = (schema: any) => ({
  input: asJson(S.toJSONSchema(schema)),
  output: asJson(S.toJSONSchema(S.reverse(schema))),
});

// Fully derive the operations dimension from a live schema: an identity op
// collapses to the literal `identity`; others get their expression golden with
// no examples yet (the author still adds example inputs by hand).
export const scaffoldOperations = (schema: any): Spec["operations"] =>
  Object.fromEntries(
    OP_ORDER.map((opName) => {
      const fn = OP_BUILDER[opName](schema);
      const op: Operation = isNoop(fn)
        ? "identity"
        : { expression: fn.toString(), examples: {} };
      return [opName, op];
    }),
  ) as Spec["operations"];

// ---- canonical form -------------------------------------------------------

const order = <T extends Record<string, unknown>>(obj: T, keys: string[]): T => {
  if (obj === null || typeof obj !== "object" || Array.isArray(obj)) return obj;
  const out: Record<string, unknown> = {};
  for (const k of keys) if (k in obj) out[k] = obj[k];
  for (const k of Object.keys(obj)) if (!(k in out)) out[k] = obj[k];
  return out as T;
};

// Individual named examples are never `_skip` — only the enclosing operation
// block is (the format schema has no `orSkip` on the examples map's values).
const canonExample = (ex: Example): Example =>
  order(ex, ["input", "output", "error", "bench"]) as Example;

const canonOp = (op: Operation): Operation => {
  if (op === "identity") return op;
  const o = order(op, ["expression", "examples"]);
  if (o.examples && typeof o.examples === "object") {
    const ex: Record<string, Example> = {};
    for (const [name, v] of Object.entries(o.examples)) ex[name] = canonExample(v);
    o.examples = ex;
  }
  return o;
};

export const canonicalize = (obj: Spec): Spec => {
  const o = order(obj, KEY_ORDER as string[]);
  if (o.ts) o.ts = order(o.ts, TS_KEY_ORDER as string[]);
  if (o.jsonSchema && !isSkip(o.jsonSchema))
    o.jsonSchema = order(o.jsonSchema as Record<string, unknown>, [
      "input",
      "output",
    ]) as Spec["jsonSchema"];
  if (o.operations) {
    const ops = order(o.operations, OP_ORDER);
    for (const name of OP_ORDER) if (ops[name]) ops[name] = canonOp(ops[name]);
    o.operations = ops;
  }
  return o;
};

export const serialize = (obj: Spec): string =>
  HEADER + "\n" + stringifyYaml(canonicalize(obj), { lineWidth: 0 });

// ---- golden recomputation (golden-master `update`) ------------------------

const inlineToValue = (codeStr: string): unknown =>
  new Function("S", `return (${codeStr});`)(S);

// `S.toJSONSchema` returns the concrete `JSONSchema7` interface, which has no
// index signature and so doesn't structurally satisfy Sury's generic `JSON`
// type — even though every JSONSchema7 value is valid JSON data. Bridge the
// two Sury-internal type declarations at this one boundary.
const asJson = (v: unknown): S.Output<typeof S.json> => v as S.Output<typeof S.json>;

// `String(5n)` prints "5" — a plain number literal when re-embedded as source,
// silently losing BigInt-ness on the next `inlineToValue` round-trip. JSON.stringify
// throws outright on a bare (or nested) bigint. Neither NaN/-0/Date/Map/Set is
// handled correctly either (see Spec Harness Suggestions); bigint is the one
// fixed here since S.bigint makes it a real, not merely theoretical, output type.
const valueToCode = (v: unknown): string =>
  typeof v === "bigint"
    ? `${v}n`
    : typeof v === "string" || (typeof v === "object" && v !== null)
      ? JSON.stringify(v)
      : String(v);

const clean = <T extends Record<string, unknown>>(o: T): T => {
  const r: Record<string, unknown> = {};
  for (const [k, v] of Object.entries(o)) if (v !== undefined) r[k] = v;
  return r as T;
};

// Recompute everything derivable from the live (dev) schema: per-op codegen
// goldens, input/output JSON Schemas, ts.input/ts.output/ts.instantiations (via
// introspect.ts), ts.bundleBytes (via bundleSize.ts), and each example's
// result by running the operation. The author owns inputs and skips (and can
// still `_skip` any of these — e.g. `ts.instantiations: { _skip:
// not-applicable }`); the harness owns the answers.
//
// The esbuild-based bundle measurement is kicked off FIRST, before any of the
// synchronous work below, so its child-process build genuinely overlaps with
// the (CPU-bound, single-threaded) TS introspection and operation execution —
// awaiting a promise you call *after* doing unrelated sync work just means you
// awaited something that was already progressing; calling it first is what
// lets the two actually run concurrently.
export const recomputeGoldens = async (obj: Spec): Promise<Spec> => {
  const next: Spec = structuredClone(obj);
  const schema = evalSchema(next.ts.schema);

  const bundleBytesPromise = isSkip(next.ts.bundleBytes)
    ? null
    : deriveBundleBytes(next.ts.schema);

  if (!isSkip(next.ts.input) || !isSkip(next.ts.output) || !isSkip(next.ts.instantiations)) {
    const info = await deriveTypeInfo(next.ts.schema);
    if (!isSkip(next.ts.input)) next.ts.input = info.input;
    if (!isSkip(next.ts.output)) next.ts.output = info.output;
    if (!isSkip(next.ts.instantiations)) next.ts.instantiations = info.instantiations;
  }

  if (!isSkip(next.jsonSchema)) {
    next.jsonSchema = {
      input: asJson(S.toJSONSchema(schema)),
      output: asJson(S.toJSONSchema(S.reverse(schema))),
    };
  }

  for (const opName of OP_ORDER) {
    const op = next.operations[opName];
    if (op === "identity") continue;
    const fn = OP_BUILDER[opName](schema);
    if (!isSkip(op.expression)) op.expression = fn.toString();
    for (const [name, ex] of Object.entries(op.examples)) {
      const bench = ex.bench;
      try {
        const out = fn(inlineToValue(ex.input));
        op.examples[name] = clean({ input: ex.input, output: valueToCode(out), bench });
      } catch (e) {
        op.examples[name] = clean({ input: ex.input, error: (e as Error).message, bench });
      }
    }
  }

  if (bundleBytesPromise) next.ts.bundleBytes = await bundleBytesPromise;
  return next;
};

// Every check `spec check` performs against a single already-parsed spec,
// returning guiding error messages ([] if the spec is fully valid and fresh).
// Never mutates a file or exits the process, so it's directly testable —
// cli.ts's cmdCheck and tests/spec_errors_test.ts's snapshot tests both call
// this same function; there's exactly one implementation of "what's wrong
// with this spec, and what should the author do about it."
export const checkSpec = async (id: string, obj: Spec, raw: string): Promise<string[]> => {
  const errs: string[] = [];

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
    // identityViolations/recomputeGoldens assume (e.g. a missing `examples`
    // map) — catch so one malformed spec reports gracefully instead of
    // throwing; the validation error already pushed above is the actionable one.
    try {
      // identity invariant: noop op <-> the literal `identity`
      for (const violation of identityViolations(schema, obj)) errs.push(violation);
      // goldens must equal what the live schema produces (no hand-edits)
      if (serialize(await recomputeGoldens(obj)) !== canon)
        errs.push(`goldens stale — run \`pnpm spec update ${id}\``);
    } catch (e) {
      errs.push(`goldens could not be computed: ${(e as Error).message}`);
    }
  }
  return errs;
};

// Re-exported so `spec new` can populate ts.input/ts.output/ts.instantiations/
// ts.bundleBytes up front too (cli.ts only imports from harness.ts/format.ts,
// never touches introspect.ts/bundleSize.ts directly).
export { deriveTypeInfo, type TypeInfo } from "./introspect";
export { deriveBundleBytes } from "./bundleSize";
