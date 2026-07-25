// Harness SUBJECT half: canonicalize specs and (re)compute goldens by
// executing the real schema.
//
// Unlike format.ts (which runs on published sury), this half imports the
// in-development sury SOURCE (`../sury/src/S.mjs`), because goldens must reflect
// the code under test — that's how `spec check` catches codegen changes.
//
// There is no code-generation step: packages/sury/tests/spec_test.ts loops
// over listSpecFiles()/readSpec() at run time and calls straight into this
// module, so drift in any dimension is exercised by a real Vitest run without
// ever materializing a generated .ts file per spec.
import { existsSync, readFileSync, readdirSync } from "node:fs";
import { join, basename } from "node:path";
import { fileURLToPath } from "node:url";
import { parse as parseYaml, stringify as stringifyYaml } from "yaml";
import { diffLinesUnified } from "@vitest/utils/diff";
import ts from "typescript";
import * as S from "../sury/src/S.mjs";
import {
  KEY_ORDER,
  TS_KEY_ORDER,
  VS_KEY_ORDER,
  VS_ZOD_KEY_ORDER,
  OP_ORDER,
  BUNDLE_SIZE_KEY_ORDER,
  SKIP_REASONS,
  isSkip,
  isZodOverwrite,
  validate,
  validateBundleSize,
  type Spec,
  type Operation,
  type Example,
  type OpName,
  type BundleSize,
} from "./format";
import { deriveTypeInfo, deriveVsTypeInfo } from "./introspect";
import { deriveBundleSize } from "./bundleSize";

const here = (rel: string) => fileURLToPath(new URL(rel, import.meta.url));
// The spec suite lives in the sury package (specs ship with it).
export const SPECS_DIR = here("../sury/specs/");
export const SCHEMA_PATH = join(SPECS_DIR, "spec.schema.json");
export const BUNDLE_SIZE_PATH = join(SPECS_DIR, "bundleSize.yaml");

// Lives in the specs dir but isn't a spec: one whole-package measurement, not
// a per-schema contract. `bundleSize` is a valid spec id, so every walk of the
// directory has to exclude it by name or it gets validated as a Spec.
const NON_SPEC_FILES = new Set([basename(SCHEMA_PATH), basename(BUNDLE_SIZE_PATH)]);

const HEADER = "# yaml-language-server: $schema=./spec.schema.json";

const OP_BUILDER: Record<OpName, (schema: any) => (input: any) => any> = {
  parse: S.parser,
  decode: S.decoder,
  encode: S.encoder,
};

const SKIP_REASON_SET = new Set<string>(SKIP_REASONS);
export const isValidSkipReason = (r: unknown): boolean =>
  typeof r === "string" && (SKIP_REASON_SET.has(r) || /^todo\(#.+\)$/.test(r));

// `path` is relative to the spec root (e.g. `ts.instantiations`) — the reported
// error already sits under a `✗ <id>` header, so prefixing the id here would
// print it twice.
export const lintSkips = (obj: unknown, path: string, out: string[]): void => {
  if (isSkip(obj)) {
    if (!isValidSkipReason(obj._skip))
      out.push(`${path}: invalid _skip reason ${JSON.stringify(obj._skip)}`);
    return;
  }
  if (obj && typeof obj === "object")
    for (const [k, v] of Object.entries(obj)) lintSkips(v, path ? `${path}.${k}` : k, out);
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
  for (const name of names) {
    if (NON_SPEC_FILES.has(name)) continue;
    if (!name.endsWith(".yaml")) {
      errs.push(
        `specs dir: unexpected file ${JSON.stringify(name)} (only *.yaml and ${[...NON_SPEC_FILES].join("/")} allowed)`,
      );
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
    .filter((f) => f.endsWith(".yaml") && !NON_SPEC_FILES.has(f))
    .map((f) => join(SPECS_DIR, f))
    .sort();

export const parseSpec = (raw: string): Spec => parseYaml(raw) as Spec;

export const readSpec = (file: string): Spec => parseSpec(readFileSync(file, "utf8"));

// transpileModule (syntax-only, no type info) strips TS-only syntax like
// `as const` so aliases can use it — `new Function` only ever sees plain JS.
// The source is parenthesized before stripping (not after) so a bare object
// literal parses as an expression, not a block statement with a labeled
// statement inside — and the trailing `;\n` transpileModule always emits
// comes off since it's re-wrapped in `return … ;` below.
const stripTypes = (tsSource: string): string =>
  ts.transpileModule(`(${tsSource})`, {
    compilerOptions: { target: ts.ScriptTarget.ESNext, module: ts.ModuleKind.ESNext },
  }).outputText.trim().replace(/;$/, "");

export const evalSchema = (tsSource: string): any =>
  new Function("S", `return ${stripTypes(tsSource)};`)(S);

// Sury compiles a pass-through operation to this shared function — the ONLY
// signal identity detection has. If this name is ever changed in Sury's
// source, every `identity`-marked operation starts failing loudly (across
// every spec, in `identityViolations` below) rather than silently going stale.
const NOOP_OPERATION_WHICH_WILL_NEVER_CHANGE = "noopOperation";
const isNoop = (fn: Function): boolean =>
  fn.name === NOOP_OPERATION_WHICH_WILL_NEVER_CHANGE;

// Checks the shorthand invariants both ways: a declared `identity`/`eq-to-parse`
// that doesn't hold, or a full op block that should be a shorthand.
export const identityViolations = (schema: any, spec: Spec): string[] => {
  const out: string[] = [];
  const parseCode = OP_BUILDER.parse(schema).toString();
  for (const opName of OP_ORDER) {
    const op = spec.operations[opName];
    const fn = OP_BUILDER[opName](schema);
    const noop = isNoop(fn);
    const matchesParse = opName !== "parse" && !noop && fn.toString() === parseCode;
    if (op === "identity") {
      if (!noop)
        out.push(
          `operations.${opName}: marked \`identity\` but does not compile to identity — use a full op block with examples`,
        );
    } else if (noop) {
      out.push(
        op === "eq-to-parse"
          ? `operations.${opName}: compiles to identity — use \`identity\` instead of \`eq-to-parse\``
          : `operations.${opName}: compiles to identity — use \`identity\` instead of an expression + examples`,
      );
    } else if (op === "eq-to-parse") {
      if (!matchesParse)
        out.push(
          `operations.${opName}: marked \`eq-to-parse\` but does not compile to the same code as parse — use a full op block with examples`,
        );
    } else if (matchesParse) {
      out.push(
        `operations.${opName}: compiles to the same code as parse — use \`eq-to-parse\` instead of an expression + examples`,
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
// Always a string (source text, same formatting as example values) so the
// success case (the schema itself) and the failure case (the thrown message)
// are one uniform, one-line field — not a structural union at the YAML level.
const toJsonSchemaOrError = (fn: () => unknown): string => {
  try {
    return valueToCode(fn());
  } catch (e) {
    return (e as Error).message;
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
export const scaffoldOperations = (schema: any): Spec["operations"] => {
  const parseCode = OP_BUILDER.parse(schema).toString();
  return Object.fromEntries(
    OP_ORDER.map((opName) => {
      const fn = OP_BUILDER[opName](schema);
      const op: Operation = isNoop(fn)
        ? "identity"
        : opName !== "parse" && fn.toString() === parseCode
          ? "eq-to-parse"
          : { expression: fn.toString(), examples: {} };
      return [opName, op];
    }),
  ) as Spec["operations"];
};

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
// check surfaces, not a formatting one.
const reformatIfEvaluable = (text: string): string => {
  try {
    return valueToCode(evalSchema(text));
  } catch {
    return text;
  }
};

// Individual named examples are never `_skip` — only the enclosing operation
// block is (the format schema has no `orSkip` on the examples map's values).
const canonExample = (ex: Example): Example => {
  const o = order(ex, ["input", "output", "error", "bench"]) as Example;
  o.input = reformatIfEvaluable(o.input);
  if ("output" in o) o.output = reformatIfEvaluable(o.output);
  return o;
};

const canonOp = (op: Operation): Operation => {
  if (typeof op === "string") return op;
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
  if (o.vs) {
    o.vs = order(o.vs as Record<string, unknown>, VS_KEY_ORDER as string[]) as Spec["vs"];
    if (isZodOverwrite(o.vs.zod))
      o.vs.zod = order(o.vs.zod as Record<string, unknown>, VS_ZOD_KEY_ORDER as string[]) as typeof o.vs.zod;
  }
  if (o.jsonSchema)
    o.jsonSchema = order(o.jsonSchema as Record<string, unknown>, [
      "input",
      "output",
    ]) as Spec["jsonSchema"];
  if (o.operations) {
    const ops = order(o.operations, OP_ORDER) as Record<OpName, Operation>;
    for (const name of OP_ORDER) if (ops[name]) ops[name] = canonOp(ops[name]);
    o.operations = ops as Spec["operations"];
  }
  return o;
};

export const serialize = (obj: Spec): string =>
  HEADER + "\n" + stringifyYaml(canonicalize(obj), { lineWidth: 0 });

// ---- golden recomputation --------------------------------------------------

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
//
// Anything the emitted source would NOT evaluate back to (structurally) must
// throw rather than emit: a cyclic value would recurse forever, a class
// instance would silently flatten to a plain-object literal, and symbol keys
// would be dropped by Object.entries — each of those would record a golden
// that looks fine but doesn't equal the real output.
const valueToCode = (v: unknown, seen: WeakSet<object> = new WeakSet()): string => {
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
  if (typeof v === "object") {
    if (seen.has(v)) throw new Error("cannot represent a cyclic value as spec source code");
    seen.add(v);
  }
  if (v instanceof Date) return `new Date(${JSON.stringify(v.toISOString())})`;
  if (v instanceof RegExp) return v.toString();
  if (v instanceof Map) return `new Map(${valueToCode([...v], seen)})`;
  if (v instanceof Set) return `new Set(${valueToCode([...v], seen)})`;
  if (Array.isArray(v)) return `[${v.map((x) => valueToCode(x, seen)).join(", ")}]`;
  if (typeof v === "object") {
    const proto = Object.getPrototypeOf(v);
    if (proto !== Object.prototype && proto !== null)
      throw new Error(
        `cannot represent a ${(v as object).constructor?.name ?? "unknown-class"} instance as spec source code`,
      );
    if (Object.getOwnPropertySymbols(v).length)
      throw new Error("cannot represent an object with symbol keys as spec source code");
    const entries = Object.entries(v);
    if (entries.length === 0) return "{}";
    return `{ ${entries.map(([k, val]) => `${keyToCode(k)}: ${valueToCode(val, seen)}`).join(", ")} }`;
  }
  throw new Error(`cannot represent a ${typeof v} as spec source code`);
};

const ZOD_IMPORT = `import * as z from "zod";\n`;

const clean = <T extends Record<string, unknown>>(o: T): T => {
  const r: Record<string, unknown> = {};
  for (const [k, v] of Object.entries(o)) if (v !== undefined) r[k] = v;
  return r as T;
};

// The author owns inputs and skips; the harness owns every derived answer.
export const recomputeGoldens = async (obj: Spec): Promise<Spec> => {
  const next: Spec = structuredClone(obj);
  const schema = evalSchema(next.ts.schema);

  if (!isSkip(next.ts.input) || !isSkip(next.ts.output) || !isSkip(next.ts.instantiations)) {
    const info = await deriveTypeInfo(next.ts.schema);
    if (!isSkip(next.ts.input)) next.ts.input = info.input;
    if (!isSkip(next.ts.output)) next.ts.output = info.output;
    if (!isSkip(next.ts.instantiations)) next.ts.instantiations = info.instantiations;
  }

  // The overwrite form of `vs.zod` records Zod's inferred types as goldens for
  // the side(s) that diverge from ts; the harness owns those, so fill from the
  // live Zod schema. An omitted side matches ts and isn't recorded (checkVs
  // verifies the match). A schema that doesn't typecheck throws here and
  // surfaces via checkSpec's "goldens could not be computed" — same as any
  // other uncomputable golden.
  if (isZodOverwrite(next.vs.zod)) {
    const zi = await deriveVsTypeInfo(ZOD_IMPORT, next.vs.zod.schema);
    if (next.vs.zod.input !== undefined) next.vs.zod.input = zi.input;
    if (next.vs.zod.output !== undefined) next.vs.zod.output = zi.output;
  }

  next.jsonSchema = deriveJsonSchema(schema);

  for (const opName of OP_ORDER) {
    const op = next.operations[opName];
    if (typeof op === "string") continue;
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

  return next;
};

// `ts.schema` can evaluate without throwing to a value that still isn't a
// usable Sury schema (e.g. `ts.schema: "42"` evaluates to the number 42).
// Every Sury schema carries a Standard Schema `~standard` prop whose `vendor`
// is `"sury"` (Sury's own internals use this exact check — see `js_assert` in
// the sury entry) — a reliable, non-throwing alternative to probing with a builder.
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

// Confirms each `ts.aliases` entry evaluates to a schema equivalent to
// `ts.schema` — same ts.input/ts.output, jsonSchema, and operations —
// without giving an alias its own goldens to maintain. Compared directly
// against the (already-validated) `spec`'s recorded values rather than
// against each other, so a drifting alias is reported against the one
// spelling the author actually reads top-to-bottom.
export const checkAliases = async (spec: Spec): Promise<string[]> => {
  const aliases = spec.ts.aliases;
  if (!aliases || !aliases.length) return [];
  const errs: string[] = [];
  for (const aliasSrc of aliases) {
    const label = `ts.aliases[${JSON.stringify(aliasSrc)}]`;
    let aliasSchema: any;
    try {
      aliasSchema = evalSchema(aliasSrc);
    } catch (e) {
      errs.push(`${label}: did not evaluate: ${(e as Error).message}`);
      continue;
    }
    if (!isUsableSchema(aliasSchema)) {
      errs.push(`${label}: evaluated but isn't a Sury schema`);
      continue;
    }

    // Isolated per alias — a throw here (e.g. deriveTypeInfo failing to
    // resolve the alias's type) must not abort the remaining aliases or
    // surface as the outer, label-less "goldens could not be computed".
    try {
      if (!isSkip(spec.ts.input) || !isSkip(spec.ts.output)) {
        const info = await deriveTypeInfo(aliasSrc);
        if (!isSkip(spec.ts.input) && info.input !== spec.ts.input)
          errs.push(`${label}: ts.input ${JSON.stringify(info.input)} !== ${JSON.stringify(spec.ts.input)}`);
        if (!isSkip(spec.ts.output) && info.output !== spec.ts.output)
          errs.push(`${label}: ts.output ${JSON.stringify(info.output)} !== ${JSON.stringify(spec.ts.output)}`);
      }

      const js = deriveJsonSchema(aliasSchema);
      if (js.input !== spec.jsonSchema.input)
        errs.push(`${label}: jsonSchema.input differs:\n${diffText(spec.jsonSchema.input, js.input)}`);
      if (js.output !== spec.jsonSchema.output)
        errs.push(`${label}: jsonSchema.output differs:\n${diffText(spec.jsonSchema.output, js.output)}`);

      const aliasParseCode = OP_BUILDER.parse(aliasSchema).toString();
      for (const opName of OP_ORDER) {
        const op = spec.operations[opName];
        const fn = OP_BUILDER[opName](aliasSchema);
        const noop = isNoop(fn);
        if (op === "identity") {
          if (!noop) errs.push(`${label}: operations.${opName} is \`identity\` on schema but not on this alias`);
        } else if (noop) {
          errs.push(`${label}: operations.${opName} compiles to identity on this alias but not on schema`);
        } else if (op === "eq-to-parse") {
          if (fn.toString() !== aliasParseCode)
            errs.push(
              `${label}: operations.${opName} is \`eq-to-parse\` on schema but does not compile to the same code as parse on this alias`,
            );
        } else if (!isSkip(op.expression) && fn.toString() !== op.expression) {
          errs.push(`${label}: operations.${opName}.expression differs:\n${diffText(op.expression, fn.toString())}`);
        }
      }
    } catch (e) {
      errs.push(`${label}: could not be checked: ${(e as Error).message}`);
      continue;
    }
  }
  return errs;
};

// Cross-checks a spec's `vs` equivalent against its recorded inferred types,
// live like checkAliases (no golden of its own). Strict string equality —
// both sides printed with the same InTypeAlias formatting — so the author
// writes the `vs` source to match Sury's ordering where it differs (e.g.
// union member order).
export const checkVs = async (spec: Spec): Promise<string[]> => {
  const vs = spec.vs;
  if (!vs || isSkip(vs.zod)) return [];
  const errs: string[] = [];

  const zodSource = isZodOverwrite(vs.zod) ? vs.zod.schema : vs.zod;
  let info: { input: string; output: string };
  try {
    info = await deriveVsTypeInfo(ZOD_IMPORT, zodSource);
  } catch (e) {
    errs.push(`vs.zod: did not typecheck: ${(e as Error).message}`);
    return errs;
  }

  if (isZodOverwrite(vs.zod)) {
    // The overwrite form records a divergence, per side. A present side must
    // actually differ from ts; an omitted side means "no divergence" and must
    // actually match. If both sides are omitted, nothing diverges — the bare
    // string form (which asserts both equalities) is the right tool.
    const hasInput = vs.zod.input !== undefined;
    const hasOutput = vs.zod.output !== undefined;
    if (!hasInput && !hasOutput) {
      errs.push(
        "vs.zod: overwrite form records no divergence (input and output both omitted) — " +
          `use the bare \`zod: ${JSON.stringify(vs.zod.schema)}\` string form instead.`,
      );
      return errs;
    }
    if (!isSkip(spec.ts.input)) {
      if (!hasInput && info.input !== spec.ts.input)
        errs.push(
          `vs.zod: input omitted (no divergence) but Zod infers ${JSON.stringify(info.input)} !== ts.input ` +
            `${JSON.stringify(spec.ts.input)} — add \`input\` to record the divergent type.`,
        );
      else if (hasInput && info.input === spec.ts.input)
        errs.push(
          `vs.zod.input equals ts.input ${JSON.stringify(spec.ts.input)} — it matches Sury, so omit \`input\`.`,
        );
    }
    if (!isSkip(spec.ts.output)) {
      if (!hasOutput && info.output !== spec.ts.output)
        errs.push(
          `vs.zod: output omitted (no divergence) but Zod infers ${JSON.stringify(info.output)} !== ts.output ` +
            `${JSON.stringify(spec.ts.output)} — add \`output\` to record the divergent type.`,
        );
      else if (hasOutput && info.output === spec.ts.output)
        errs.push(
          `vs.zod.output equals ts.output ${JSON.stringify(spec.ts.output)} — it matches Sury, so omit \`output\`.`,
        );
    }
    return errs;
  }

  if (!isSkip(spec.ts.input) && info.input !== spec.ts.input)
    errs.push(`vs.zod: input type ${JSON.stringify(info.input)} !== ts.input ${JSON.stringify(spec.ts.input)}`);
  if (!isSkip(spec.ts.output) && info.output !== spec.ts.output)
    errs.push(`vs.zod: output type ${JSON.stringify(info.output)} !== ts.output ${JSON.stringify(spec.ts.output)}`);
  return errs;
};

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

  lintSkips(spec, "", errs);

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
      errs.push(...(await checkAliases(spec)));
      errs.push(...(await checkVs(spec)));
    } catch (e) {
      errs.push(`goldens could not be computed: ${(e as Error).message}`);
    }
  }
  return errs;
};

// ---- bundleSize.yaml -------------------------------------------------------

const serializeBundleSize = (obj: BundleSize): string =>
  stringifyYaml(order(obj, BUNDLE_SIZE_KEY_ORDER as string[]), { lineWidth: 0 });

const readBundleSizeRaw = (): string =>
  existsSync(BUNDLE_SIZE_PATH) ? readFileSync(BUNDLE_SIZE_PATH, "utf8") : "";

// Every row is derived, so the check is just "does the file equal what the live
// entry measures" — no author-owned part to preserve. `fresh` comes back with
// the errors so `--write` writes exactly what was compared instead of running
// the measurement a second time; it's absent when the measurement itself
// failed, which is what tells `--write` there's nothing safe to write.
//
// `raw` is injectable (defaulting to the real file) so tests can exercise the
// reporting without touching the filesystem, same as lintSpecsDir's `names`.
export const checkBundleSize = async (
  raw: string = readBundleSizeRaw(),
): Promise<{ errs: string[]; fresh?: string; before?: BundleSize; after?: BundleSize }> => {
  const errs: string[] = [];

  let after: BundleSize;
  let fresh: string;
  try {
    after = await deriveBundleSize();
    fresh = serializeBundleSize(after);
  } catch (e) {
    return { errs: [`could not be measured: ${(e as Error).message}`] };
  }

  if (!raw) return { errs: ["missing — run `pnpm spec check --write`"], fresh, after };

  // Reported alongside (not instead of) the staleness diff below: for a
  // hand-mangled file, a pointed "expected number at exports.string" is the
  // message that explains it, not a whole-file golden diff.
  let before: BundleSize | undefined;
  try {
    const v = validateBundleSize(parseYaml(raw));
    if (v.ok) before = v.value;
    else errs.push(`schema: ${v.error}`);
  } catch (e) {
    errs.push(`is not valid YAML: ${(e as Error).message}`);
  }

  if (raw !== fresh) errs.push(`stale — run \`pnpm spec check --write\`:\n${diffText(raw, fresh)}`);

  return { errs, fresh, before, after };
};

// Re-exported so `spec new` can populate ts.input/ts.output/ts.instantiations
// up front too (cli.ts only imports from harness.ts/format.ts, never touches
// introspect.ts/bundleSize.ts directly).
export { deriveTypeInfo, deriveVsTypeInfo, type TypeInfo } from "./introspect";
