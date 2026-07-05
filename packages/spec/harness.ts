// Harness SUBJECT half: canonicalize specs and (re)compute goldens by
// executing the real schema.
//
// Unlike format.ts (which runs on published sury), this half imports the
// in-development sury SOURCE (`../sury/src/S.js`), because goldens must reflect
// the code under test — that's how `spec check` catches codegen changes.
//
// There is no code-generation step: packages/sury/tests/spec_test.ts loops
// over listSpecFiles()/readSpec() at run time and calls straight into this
// module, so drift in any dimension is exercised by a real Vitest run without
// ever materializing a generated .ts file per spec.
import { readFileSync, readdirSync } from "node:fs";
import { join, basename } from "node:path";
import { fileURLToPath } from "node:url";
import { parse as parseYaml, stringify as stringifyYaml } from "yaml";
import { diffLinesUnified } from "@vitest/utils/diff";
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

const OP_BUILDER: Record<OpName, (schema: any) => (input: any) => any> = {
  parse: S.parser,
  decode: S.decoder,
  encode: S.encoder,
};

// `identity` isn't here — it's no longer a `_skip` reason, see Operation.
const SKIP_REASONS = new Set([
  "parser-only",
  "serializer-only",
  "lossy",
  "not-applicable",
]);
export const isValidSkipReason = (r: unknown): boolean =>
  typeof r === "string" && (SKIP_REASONS.has(r) || /^todo\(#.+\)$/.test(r));

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

const VALID_ID_RE = /^[a-zA-Z0-9-]+$/;

// listSpecFiles below silently ignores anything that isn't *.yaml — this
// walks the same directory to surface exactly what that filter would
// otherwise hide: a stray non-spec file, or a spec whose id doesn't match the
// letters/digits/-only convention (see the `spec` skill). `names` is
// injectable (defaulting to the real directory listing) so tests can exercise
// the id/filename rules directly, without touching the filesystem.
export const lintSpecsDir = (names: string[] = readdirSync(SPECS_DIR)): string[] => {
  const errs: string[] = [];
  const schemaFile = basename(SCHEMA_PATH);
  for (const name of names) {
    if (name === schemaFile) continue;
    if (!name.endsWith(".yaml")) {
      errs.push(`specs dir: unexpected file ${JSON.stringify(name)} (only *.yaml and ${schemaFile} allowed)`);
      continue;
    }
    const id = name.replace(/\.yaml$/, "");
    if (!VALID_ID_RE.test(id))
      errs.push(`specs dir: invalid spec id ${JSON.stringify(id)} (only letters, digits, and - allowed)`);
  }
  return errs;
};

export const listSpecFiles = (): string[] =>
  readdirSync(SPECS_DIR)
    .filter((f) => f.endsWith(".yaml"))
    .map((f) => join(SPECS_DIR, f))
    .sort();

export const parseSpec = (raw: string): Spec => parseYaml(raw) as Spec;

export const readSpec = (file: string): Spec => parseSpec(readFileSync(file, "utf8"));

export const evalSchema = (tsSource: string): any =>
  new Function("S", `return (${tsSource});`)(S);

// Sury compiles a pass-through operation to this shared function — the ONLY
// signal identity detection has. If this name is ever changed in Sury's
// source, every `identity`-marked operation starts failing loudly (across
// every spec, in `identityViolations` below) rather than silently going stale.
const NOOP_OPERATION_WHICH_WILL_NEVER_CHANGE = "noopOperation";
const isNoop = (fn: Function): boolean =>
  fn.name === NOOP_OPERATION_WHICH_WILL_NEVER_CHANGE;

// Checks the identity invariant both ways: declared `identity` but doesn't
// compile to a pass-through, or vice versa.
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

// JSON Schema has no representation for bigint or symbol, so `S.toJSONSchema`
// throws for any schema containing one (at any nesting depth) — a real "this
// concept doesn't apply" case, not a bug to work around. Recorded per
// direction (rather than skipping the whole dimension) since the two
// directions can differ — e.g. a `.to` transform might make only one side
// representable. Shared by `scaffoldJsonSchema` (spec new) and
// `recomputeGoldens` (spec check/--write) so both degrade the same way.
const toJsonSchemaOrError = (fn: () => unknown): S.Output<typeof S.json> => {
  try {
    return asJson(fn());
  } catch (e) {
    return asJson((e as Error).message);
  }
};
const deriveJsonSchema = (schema: any): Spec["jsonSchema"] => ({
  input: toJsonSchemaOrError(() => S.toJSONSchema(schema)),
  output: toJsonSchemaOrError(() => S.toJSONSchema(S.reverse(schema))),
});

// No example inputs needed, so `spec new` can fill this in immediately from `--ts`.
export const scaffoldJsonSchema = (schema: any): Spec["jsonSchema"] => deriveJsonSchema(schema);

// Can throw if `schema` isn't actually a usable schema (e.g. `--ts` evaluated
// to `undefined` from a typo like `S.strng`) — callers decide how to report that.
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

// Reformats `input`/`output` to canonical source-text form (see valueToCode)
// by round-tripping each through eval — independent of recomputeGoldens, so
// `spec format` can normalize formatting without executing the schema at all.
// Left as-is if it no longer evaluates; that's a deeper problem the freshness
// check surfaces, not a formatting one. Individual named examples are never
// `_skip` — only the enclosing operation block is (the format schema has no
// `orSkip` on the examples map's values).
const reformatIfEvaluable = (text: string): string => {
  try {
    return valueToCode(evalSchema(text));
  } catch {
    return text;
  }
};

const canonExample = (ex: Example): Example => {
  const o = order(ex, ["input", "output", "error", "bench"]) as Example;
  o.input = reformatIfEvaluable(o.input);
  if ("output" in o) o.output = reformatIfEvaluable(o.output);
  return o;
};

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
  if (o.jsonSchema)
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

// ---- golden recomputation --------------------------------------------------

// `S.toJSONSchema` returns the concrete `JSONSchema7` interface, which has no
// index signature and so doesn't structurally satisfy Sury's generic `JSON`
// type — even though every JSONSchema7 value is valid JSON data. Bridge the
// two Sury-internal type declarations at this one boundary.
const asJson = (v: unknown): S.Output<typeof S.json> => v as S.Output<typeof S.json>;

// An object key needs quotes only when it isn't a valid identifier — matches
// how a human would hand-write the same literal.
const IDENT_RE = /^[A-Za-z_$][A-Za-z0-9_$]*$/;
const keyToCode = (k: string): string => (IDENT_RE.test(k) ? k : JSON.stringify(k));

// Recursive (not JSON.stringify) because JSON.stringify throws outright on a
// bare (or nested) bigint, and silently mangles Date (→ a plain string, not a
// Date)/Map/Set (→ "{}", dropping every entry). `Object.is` catches -0, which
// `String(-0)` prints as "0". Only a *registry* symbol (`Symbol.for(key)`)
// round-trips through source text — a bare `Symbol()` is unique per call, so
// no source expression can reproduce it.
const valueToCode = (v: unknown): string => {
  if (v === undefined) return "undefined";
  if (typeof v === "bigint") return `${v}n`;
  if (typeof v === "number") return Object.is(v, -0) ? "-0" : String(v);
  if (v === null || typeof v === "boolean" || typeof v === "string") return JSON.stringify(v);
  if (typeof v === "symbol") {
    const key = Symbol.keyFor(v);
    if (key === undefined)
      throw new Error("cannot represent a non-registry symbol (use Symbol.for(key)) as spec source code");
    return `Symbol.for(${JSON.stringify(key)})`;
  }
  if (v instanceof Date) return `new Date(${JSON.stringify(v.toISOString())})`;
  if (v instanceof RegExp) return v.toString();
  if (v instanceof Map) return `new Map(${valueToCode([...v])})`;
  if (v instanceof Set) return `new Set(${valueToCode([...v])})`;
  if (Array.isArray(v)) return `[${v.map(valueToCode).join(", ")}]`;
  if (typeof v === "object") {
    const entries = Object.entries(v);
    if (entries.length === 0) return "{}";
    return `{ ${entries.map(([k, val]) => `${keyToCode(k)}: ${valueToCode(val)}`).join(", ")} }`;
  }
  throw new Error(`cannot represent a ${typeof v} as spec source code`);
};

const clean = <T extends Record<string, unknown>>(o: T): T => {
  const r: Record<string, unknown> = {};
  for (const [k, v] of Object.entries(o)) if (v !== undefined) r[k] = v;
  return r as T;
};

// The author owns inputs and skips; the harness owns every derived answer.
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

  next.jsonSchema = deriveJsonSchema(schema);

  for (const opName of OP_ORDER) {
    const op = next.operations[opName];
    if (op === "identity") continue;
    const fn = OP_BUILDER[opName](schema);
    if (!isSkip(op.expression)) op.expression = fn.toString();
    for (const [name, ex] of Object.entries(op.examples)) {
      const bench = ex.bench;
      try {
        const out = fn(evalSchema(ex.input));
        op.examples[name] = clean({ input: ex.input, output: valueToCode(out), bench });
      } catch (e) {
        op.examples[name] = clean({ input: ex.input, error: (e as Error).message, bench });
      }
    }
  }

  if (bundleBytesPromise) next.ts.bundleBytes = await bundleBytesPromise;
  return next;
};

// `ts.schema` can evaluate without throwing to a value that still isn't a
// usable Sury schema (e.g. `ts.schema: "42"` evaluates to the number 42).
// Every Sury schema carries a Standard Schema `~standard` prop whose `vendor`
// is `"sury"` (Sury's own internals use this exact check — see `js_assert` in
// Sury.res.mjs) — a reliable, non-throwing alternative to probing with a builder.
const isUsableSchema = (schema: unknown): boolean =>
  (schema as { ["~standard"]?: { vendor?: string } } | null | undefined)?.["~standard"]?.vendor === "sury";

const identity = (s: string): string => s;

// A plain (no ANSI color, since this also renders inside inline test
// snapshots and CI logs) git-style unified diff between two spec texts, so
// "not canonical"/"goldens stale" show exactly what differs instead of just
// asserting that something does. `a` is the current text, `b` the target —
// `-`/`+` read as the edit needed to fix `a`.
const diffText = (a: string, b: string): string =>
  diffLinesUnified(a.split("\n"), b.split("\n"), {
    aColor: identity,
    bColor: identity,
    changeColor: identity,
    commonColor: identity,
    patchColor: identity,
    aIndicator: "-",
    bIndicator: "+",
    commonIndicator: " ",
    omitAnnotationLines: true,
    expand: false,
    contextLines: 3,
  });

// Never mutates a file or exits the process, so it's directly testable —
// cli.ts's cmdCheck and tests/spec_errors_test.ts both call this same
// function, so there's exactly one implementation of "what's wrong with this
// spec, and what should the author do about it."
//
// `knownFresh`, when passed, is the already-serialized result of a
// recomputeGoldens call the caller just performed (cli.ts's `--write` path,
// right after writing) — skips redoing that same esbuild+TS-introspection
// work a second time purely to re-derive what the caller already has.
export const checkSpec = async (
  id: string,
  obj: Spec,
  raw: string,
  knownFresh?: string,
): Promise<string[]> => {
  const errs: string[] = [];

  const v = validate(obj);
  if (!v.ok) errs.push(`schema: ${v.error}`);
  const spec = v.ok ? v.value : obj;

  lintSkips(spec, id, errs);

  const canon = serialize(spec);
  if (raw !== canon)
    errs.push(
      `not canonical — run \`pnpm spec format ${id}\` (or \`pnpm spec check ${id} --write\`, which also refreshes goldens):\n${diffText(raw, canon)}`,
    );

  let schema: any;
  let evaluated = false;
  try {
    schema = evalSchema(spec.ts.schema);
    evaluated = true;
  } catch (e) {
    errs.push(`ts.schema did not evaluate: ${(e as Error).message}`);
  }
  if (evaluated && !isUsableSchema(schema)) {
    errs.push(`ts.schema evaluated but isn't a Sury schema`);
  } else if (evaluated) {
    try {
      const violations = identityViolations(schema, spec);
      for (const violation of violations) errs.push(violation);
      const fresh = knownFresh ?? serialize(await recomputeGoldens(spec));
      if (fresh !== canon)
        errs.push(
          (violations.length
            ? `goldens stale — resolve the identity mismatch above first, then \`pnpm spec check ${id} --write\` can fix it (also formats canonically; use \`pnpm spec format\` for a formatting-only fix)`
            : `goldens stale — run \`pnpm spec check ${id} --write\` (also formats canonically; use \`pnpm spec format\` for a formatting-only fix)`) +
            `:\n${diffText(canon, fresh)}`,
        );
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
