// Harness SUBJECT half: canonicalize specs and (re)compute goldens by
// executing the real schema.
//
// Unlike format.ts (which runs on published sury), this half imports the
// in-development sury SOURCE (`../sury/index.mjs`), because goldens must reflect
// the code under test — that's how `spec check` catches codegen changes.
//
// There is no code-generation step: packages/sury/tests/spec_test.ts loops
// over listSpecFiles()/readSpec() at run time and calls straight into this
// module, so drift in any dimension is exercised by a real Vitest run without
// ever materializing a generated .ts file per spec.
import { existsSync, readFileSync, readdirSync } from "node:fs";
import { join, basename } from "node:path";
import { fileURLToPath } from "node:url";
import { Document, parse as parseYaml, parseDocument, stringify as stringifyYaml, isMap, isSeq } from "yaml";
import { diffLinesUnified } from "@vitest/utils/diff";
import ts from "typescript";
import * as S from "../sury/index.mjs";
import {
  KEY_ORDER,
  TS_KEY_ORDER,
  VS_KEY_ORDER,
  VS_ZOD_KEY_ORDER,
  OP_ORDER,
  OP_BLOCK_KEY_ORDER,
  BUNDLE_SIZE_KEY_ORDER,
  SKIP_REASONS,
  isSkip,
  isZodOverwrite,
  isCreationError,
  validate,
  validateBundleSize,
  validateScenarios,
  type Spec,
  type Operation,
  type Example,
  type OpName,
  type BundleSize,
  type Scenario,
  type Scenarios,
} from "./format";
import { buildScenarioRunner, type ScenarioSource } from "./scenario";
import { deriveTypeInfo, deriveVsTypeInfo } from "./introspect";
import { deriveBundleSize } from "./bundleSize";

const here = (rel: string) => fileURLToPath(new URL(rel, import.meta.url));
// The spec suite lives in the sury package (specs ship with it).
export const SPECS_DIR = here("../sury/specs/");
export const SCHEMA_PATH = join(SPECS_DIR, "spec.schema.json");
export const BUNDLE_SIZE_PATH = join(SPECS_DIR, "bundleSize.yaml");
export const SCENARIOS_PATH = join(SPECS_DIR, "scenarios.yaml");
export const SCENARIOS_SCHEMA_PATH = join(SPECS_DIR, "scenarios.schema.json");

// Live in the specs dir but aren't specs: one whole-package measurement and
// one set of consumer-level perf scenarios, neither a per-schema contract.
// `bundleSize` and `scenarios` are both valid spec ids, so every walk of the
// directory has to exclude them by name or they get validated as Specs.
const NON_SPEC_FILES = new Set([
  basename(SCHEMA_PATH),
  basename(BUNDLE_SIZE_PATH),
  basename(SCENARIOS_PATH),
  basename(SCENARIOS_SCHEMA_PATH),
]);

const HEADER = "# yaml-language-server: $schema=./spec.schema.json";

const OP_BUILDER: Record<OpName, (schema: any) => (input: any) => any> = {
  parse: S.parser,
  decode: S.decoder,
  encode: S.encoder,
};

// A schema carrying an async transform or refine compiles only through these:
// the sync builders reject it at operation creation ("Encountered unexpected
// async transform or refine"), and they wrap a sync direction in
// `Promise.resolve(...)`, so which builder an op uses is part of its codegen —
// hence a declared `isAsync`, checked against the schema, rather than a guess.
const ASYNC_OP_BUILDER: Record<OpName, (schema: any) => (input: any) => Promise<any>> = {
  parse: S.asyncParser,
  decode: S.asyncDecoder,
  encode: S.asyncEncoder,
};

// `S.isAsync` is exported by the runtime entry but declared only for ReScript
// (docs/rescript-usage.md), so index.d.ts has no signature to import.
const schemaIsAsync = (S as unknown as { isAsync: (schema: any) => boolean }).isAsync;

// Asked per direction, not per schema: `S.asyncDecoderAssert` makes the decode
// side async while the encode side stays a plain sync pass, and encode runs the
// reversed schema.
const opIsAsync = (opName: OpName, schema: any): boolean =>
  schemaIsAsync(opName === "encode" ? S.reverse(schema) : schema);

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

// A full op block is chosen over `identity`/`eq-to-parse` precisely because it
// has real codegen — and nothing ever runs that codegen until an example does,
// so an empty map snapshots an expression no test executes.
export const lintExamples = (spec: Spec, out: string[]): void => {
  const ops = spec.operations as Record<OpName, Operation>;
  for (const opName of OP_ORDER) {
    const op = ops[opName];
    if (typeof op === "string" || isCreationError(op) || Object.keys(op.examples).length) continue;
    out.push(
      `operations.${opName}: no examples — a compiled op block must run at least one input ` +
        "(add a named entry with just `input`, then `--write` fills the result)",
    );
  }
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
export const stripTypes = (tsSource: string): string =>
  ts.transpileModule(`(${tsSource})`, {
    compilerOptions: { target: ts.ScriptTarget.ESNext, module: ts.ModuleKind.ESNext },
  }).outputText.trim().replace(/;$/, "");

export const evalSchema = (tsSource: string): any =>
  new Function("S", `return ${stripTypes(tsSource)};`)(S);

// A scenario's `prepare` is statements, not an expression, so it goes through
// transpileModule directly — stripTypes' parenthesization exists only to keep
// a bare object literal from parsing as a block, which statements must not get.
const stripStatements = (tsSource: string): string =>
  ts.transpileModule(tsSource, {
    compilerOptions: { target: ts.ScriptTarget.ESNext, module: ts.ModuleKind.ESNext },
  }).outputText.trim();

export const scenarioSource = (scenario: Scenario): ScenarioSource => ({
  prepareSrc: scenario.prepare === undefined ? undefined : stripStatements(scenario.prepare),
  runSrc: stripTypes(scenario.run),
});

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
  const parseBuilt = buildOp("parse", schema);
  const parseCode = "fn" in parseBuilt ? parseBuilt.fn.toString() : undefined;
  for (const opName of OP_ORDER) {
    const op = spec.operations[opName];
    const built = opName === "parse" ? parseBuilt : buildOp(opName, schema);
    // Rejected at operation creation: no compiled form, so the shorthand
    // invariants don't apply. recomputeGoldens records/refreshes the
    // `creationError` message, and the staleness diff carries any shape
    // transition (expression↔creationError) — same as jsonSchema's
    // success↔error string flips, which aren't gated here either.
    if (!("fn" in built)) continue;
    // Was a `{creationError}` block but now compiles — likewise left to
    // recompute + staleness, not flagged as a shorthand violation.
    if (isCreationError(op)) continue;
    const fn = built.fn;
    const noop = isNoop(fn);
    const matchesParse = opName !== "parse" && !noop && parseCode !== undefined && fn.toString() === parseCode;
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

// The `isAsync` marker checked both ways, like identityViolations: an async
// direction must declare it (the operation returns a Promise — a different API
// for every consumer, and different codegen), and a declared one must hold.
// Only full op blocks carry the marker: `identity` can't be async (an async op
// never compiles to Sury's noop, so identityViolations already reports it),
// `eq-to-parse` inherits parse's block, and a `{creationError}` block has no
// compiled operation to be async.
export const asyncViolations = (schema: any, spec: Spec): string[] => {
  const out: string[] = [];
  for (const opName of OP_ORDER) {
    const op = spec.operations[opName];
    if (typeof op === "string" || isCreationError(op)) continue;
    let isAsync: boolean;
    try {
      isAsync = opIsAsync(opName, schema);
    } catch {
      // Not a usable schema — reported by checkSpec's own evaluation, and by
      // the creationError golden if the operation is what fails.
      continue;
    }
    if (isAsync && op.isAsync !== true)
      out.push(
        `operations.${opName}: is async (the schema has an async transform or refine) — add \`isAsync: true\`, ` +
          "which builds it with S.asyncParser/asyncDecoder/asyncEncoder and awaits every example",
      );
    else if (!isAsync && op.isAsync === true)
      out.push(
        `operations.${opName}: marked \`isAsync: true\` but the operation is synchronous — remove the marker ` +
          "(the async builders would only wrap the result in `Promise.resolve`)",
      );
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

// Compile an operation, capturing any creation-time throw as the golden instead
// of letting it abort — the operation analogue of toJsonSchemaOrError. The
// message is prefixed with the error class (`SuryError:` for an intended
// unsupported/ambiguous conversion, `TypeError:` etc. for an internal fault),
// so a bug stays visibly distinct in the golden — and flips back to compiled
// code once a fix turns the crash into a real operation — rather than silently
// masquerading as a normal rejection.
type BuiltOp = { fn: (input: any) => any; isAsync: boolean } | { creationError: string };
const buildOp = (opName: OpName, schema: any): BuiltOp => {
  try {
    const isAsync = opIsAsync(opName, schema);
    return { fn: (isAsync ? ASYNC_OP_BUILDER : OP_BUILDER)[opName](schema), isAsync };
  } catch (e) {
    const err = e as Error;
    return { creationError: `${err.constructor.name}: ${err.message}` };
  }
};

// Reduce a built op to its canonical form against parse:
// - rejected at creation → a `{creationError}` block, or `eq-to-parse` when a
//   non-parse direction is rejected with parse's exact message. A direction
//   that fails with different wording keeps its own block: the reverse names
//   the reverse conversion, and collapsing it would leave that wording
//   unratcheted.
// - compiles to Sury's noop → `identity`.
// - a non-parse direction compiling to parse's exact code → `eq-to-parse`.
// - otherwise a fresh `{expression, examples:{}}` block.
const opForm = (opName: OpName, built: BuiltOp, parseBuilt: BuiltOp): Operation => {
  if ("creationError" in built) {
    return opName !== "parse" &&
      "creationError" in parseBuilt &&
      parseBuilt.creationError === built.creationError
      ? "eq-to-parse"
      : { creationError: built.creationError };
  }
  const parseCode = "fn" in parseBuilt ? parseBuilt.fn.toString() : undefined;
  return isNoop(built.fn)
    ? "identity"
    : opName !== "parse" && parseCode !== undefined && built.fn.toString() === parseCode
      ? "eq-to-parse"
      : clean({ isAsync: built.isAsync ? (true as const) : undefined, expression: built.fn.toString(), examples: {} });
};

// Can throw if `schema` isn't actually a usable schema (e.g. `--ts` evaluated
// to `undefined` from a typo like `S.strng`) — callers decide how to report that.
export const scaffoldOperations = (schema: any): Spec["operations"] => {
  const parseBuilt = buildOp("parse", schema);
  return Object.fromEntries(
    OP_ORDER.map((opName) => [opName, opForm(opName, opName === "parse" ? parseBuilt : buildOp(opName, schema), parseBuilt)]),
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
  if (isCreationError(op)) return order(op, ["creationError"]);
  const o = order(op, OP_BLOCK_KEY_ORDER as string[]);
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

// ---- comments -------------------------------------------------------------

// Rebuilding the YAML from the parsed object would drop every comment, so they
// are lifted off the on-disk text and re-attached to the canonical document,
// anchored by the dotted spec path they annotate (`ts.schema`,
// `ts.aliases[0]`; `""` for a comment trailing the whole file).
type Anchor = { before?: string; trailing?: string };
export type SpecComments = ReadonlyMap<string, Anchor>;
const NO_COMMENTS: SpecComments = new Map();

// `owner` is the collection whose FIRST item this path is: yaml hangs a
// leading comment on the collection node in that one position and on the item
// itself everywhere else, though both mean "the lines above this path".
type AnchorVisitor = (path: string, before: any, trailing: any, owner?: any) => void;

const eachAnchor = (node: unknown, path: string, visit: AnchorVisitor): void => {
  if (isMap(node)) {
    node.items.forEach((pair: any, i) => {
      const p = path ? `${path}.${pair.key.value}` : String(pair.key.value);
      visit(p, pair.key, pair.value, i === 0 ? node : undefined);
      eachAnchor(pair.value, p, visit);
    });
  } else if (isSeq(node)) {
    node.items.forEach((item: any, i) => {
      const p = `${path}[${i}]`;
      visit(p, item, item, i === 0 ? node : undefined);
      eachAnchor(item, p, visit);
    });
  }
};

export const collectComments = (raw: string): SpecComments => {
  // The header is machine-owned (serialize re-emits it); parsing without it
  // keeps it from being collected as a comment on the first key.
  const doc = parseDocument(raw.startsWith(HEADER + "\n") ? raw.slice(HEADER.length + 1) : raw);
  const out = new Map<string, Anchor>();
  const add = (path: string, side: keyof Anchor, text?: string | null): void => {
    if (text == null) return;
    const at = out.get(path) ?? {};
    at[side] = at[side] === undefined ? text : `${at[side]}\n${text}`;
    out.set(path, at);
  };
  eachAnchor(doc.contents, "", (path, before, trailing, owner) => {
    add(path, "before", owner?.commentBefore);
    add(path, "before", before.commentBefore);
    add(path, "trailing", trailing?.comment);
  });
  add("", "trailing", doc.comment);
  return out;
};

const applyComments = (doc: Document, comments: SpecComments): void => {
  eachAnchor(doc.contents, "", (path, before, trailing) => {
    const at = comments.get(path);
    if (!at) return;
    if (at.before !== undefined) before.commentBefore = at.before;
    if (at.trailing !== undefined && trailing) trailing.comment = at.trailing;
  });
  const trailing = comments.get("")?.trailing;
  if (trailing !== undefined) doc.comment = trailing;
};

// A spec is machine-checked documentation: every claim about the schema is a
// dimension the harness executes, so prose the checker can't see is a claim
// nothing enforces. The one exception is `FIXME:` — a marker for behavior the
// goldens currently snapshot but shouldn't.
const FIXME = "FIXME:";

// Consecutive `#` lines arrive as one string; a blank line between them starts
// a separate comment, and only a comment's first line carries the prefix (the
// rest is continuation).
export const lintComments = (comments: SpecComments, out: string[]): void => {
  for (const [path, anchor] of comments)
    for (const text of [anchor.before, anchor.trailing]) {
      if (text === undefined) continue;
      for (const comment of text.split(/\n\s*\n/)) {
        const first = comment.split("\n")[0]!.trim();
        if (first.startsWith(FIXME)) continue;
        out.push(
          `${path ? `${path}: ` : ""}comment ${JSON.stringify(first)} is not allowed — prefix it with ` +
            `\`${FIXME}\` if it flags broken behavior to address, or move it to Spec Harness Suggestions ` +
            `in CONTRIBUTING.md if the spec format can't express it`,
        );
      }
    }
};

export const serialize = (obj: Spec, comments: SpecComments = NO_COMMENTS): string => {
  const doc = new Document(canonicalize(obj));
  if (comments.size) applyComments(doc, comments);
  return HEADER + "\n" + doc.toString({ lineWidth: 0 });
};

// ---- golden recomputation --------------------------------------------------

// An object key needs quotes only when it isn't a valid identifier — matches
// how a human would hand-write the same literal. `__proto__` must be computed
// (`["__proto__"]`): both the bare and the quoted form are prototype-setter
// syntax in an object literal, so either would read back as a different value
// (the key silently dropped) and `--write` would oscillate the golden.
const IDENT_RE = /^[A-Za-z_$][A-Za-z0-9_$]*$/;
const keyToCode = (k: string): string =>
  k === "__proto__"
    ? '["__proto__"]'
    : IDENT_RE.test(k)
      ? k
      : JSON.stringify(k);

// Recursive (not JSON.stringify) because JSON.stringify throws outright on a
// bare (or nested) bigint, and silently mangles Date (→ a plain string, not a
// Date)/Map/Set (→ "{}", dropping every entry). `Object.is` catches -0, which
// `String(-0)` prints as "0". Only a *registry* symbol (`Symbol.for(key)`)
// round-trips through source text — a bare `Symbol()` is unique per call, so
// no source expression can reproduce it.
//
// Anything the emitted source would NOT evaluate back to (structurally) must
// throw rather than emit: a cyclic value would recurse forever, and a class
// instance would silently flatten to a plain-object literal — each of those
// would record a golden that looks fine but doesn't equal the real output.
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
    // Symbol keys ride along as computed keys — only registry symbols, for the
    // same reason as symbol values above. Object.entries would drop them.
    const parts = Object.entries(v).map(([k, val]) => `${keyToCode(k)}: ${valueToCode(val, seen)}`);
    for (const sym of Object.getOwnPropertySymbols(v)) {
      if (!Object.getOwnPropertyDescriptor(v, sym)!.enumerable) continue;
      parts.push(`[${valueToCode(sym, seen)}]: ${valueToCode((v as Record<symbol, unknown>)[sym], seen)}`);
    }
    if (parts.length === 0) return "{}";
    return `{ ${parts.join(", ")} }`;
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

  const parseBuilt = buildOp("parse", schema);
  // Indexing by a `OpName` union narrows the value type to the intersection of
  // the three fields (which drops `eq-to-parse`), so reassignments below go
  // through this widened view.
  const ops = next.operations as Record<OpName, Operation>;
  for (const opName of OP_ORDER) {
    const op = next.operations[opName];
    const built = opName === "parse" ? parseBuilt : buildOp(opName, schema);
    if ("creationError" in built) {
      // Rejected at creation — take the canonical creationError form (a block,
      // or `eq-to-parse` on a co-failing direction). Any recorded
      // expression/examples are dropped (they can't run).
      ops[opName] = opForm(opName, built, parseBuilt);
      continue;
    }
    // Compiles. A prior string shorthand (identity / eq-to-parse) is left for
    // identityViolations to validate; only a creationError→compiles transition
    // and in-place expression refresh happen here.
    if (typeof op === "string") continue;
    const fn = built.fn;
    if (isCreationError(op)) {
      // Was rejected, now compiles — rewrite to the canonical block/shorthand.
      // Examples are author-owned and can't be invented, so start empty.
      ops[opName] = opForm(opName, built, parseBuilt);
      continue;
    }
    if (!isSkip(op.expression)) op.expression = fn.toString();
    for (const [name, ex] of Object.entries(op.examples)) {
      const bench = ex.bench;
      try {
        const value = evalSchema(ex.input);
        // `await` on a sync operation's result is a no-op, so both kinds run
        // through one path. An async operation can still throw synchronously
        // (the top-level type check runs before the first await), which the
        // same catch handles — a rejection and a synchronous throw are one
        // outcome to the author.
        const out = await fn(value);
        // An operation that hands its input straight back records the input's
        // own source rather than a re-derived spelling of the same value. It
        // reads better, and it's the only way a value the serializer can't
        // reproduce gets a passing example at all — a Blob or a File only
        // yields its bytes asynchronously, so `new File(["ab"], "a.txt")`
        // could be *run* but never written down as a result.
        op.examples[name] = clean({
          input: ex.input,
          output: out === value ? ex.input : valueToCode(out),
          bench,
        });
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
// is `"sury"` (Sury's own internals use this exact check — see `assert` in
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

      const aliasParseBuilt = buildOp("parse", aliasSchema);
      const aliasParseCode = "fn" in aliasParseBuilt ? aliasParseBuilt.fn.toString() : undefined;
      for (const opName of OP_ORDER) {
        const op = spec.operations[opName];
        const built = opName === "parse" ? aliasParseBuilt : buildOp(opName, aliasSchema);
        if (isCreationError(op)) {
          if ("fn" in built)
            errs.push(`${label}: operations.${opName} is a \`creationError\` on schema but compiles on this alias`);
          else if (built.creationError !== op.creationError)
            errs.push(`${label}: operations.${opName}.creationError differs:\n${diffText(op.creationError, built.creationError)}`);
          continue;
        }
        if (!("fn" in built)) {
          // Valid when the schema's op is the co-failure `eq-to-parse` and the
          // alias's parse is rejected with the same message — both fail at
          // creation the same way. Otherwise the alias diverges from a
          // compiling schema op.
          if (
            !(
              op === "eq-to-parse" &&
              !("fn" in aliasParseBuilt) &&
              aliasParseBuilt.creationError === built.creationError
            )
          )
            errs.push(
              `${label}: operations.${opName} does not fail at creation on schema but is rejected at operation creation on this alias: ${built.creationError}`,
            );
          continue;
        }
        const fn = built.fn;
        const noop = isNoop(fn);
        if (op === "identity") {
          if (!noop) errs.push(`${label}: operations.${opName} is \`identity\` on schema but not on this alias`);
        } else if (noop) {
          errs.push(`${label}: operations.${opName} compiles to identity on this alias but not on schema`);
        } else if (op === "eq-to-parse") {
          if (aliasParseCode === undefined || fn.toString() !== aliasParseCode)
            errs.push(
              `${label}: operations.${opName} is \`eq-to-parse\` on schema but does not compile to the same code as parse on this alias`,
            );
        } else if (built.isAsync !== (op.isAsync === true)) {
          // Reported instead of the expression diff below: the two are built by
          // different builders, so their code differs everywhere and the diff
          // would bury the one fact that explains it.
          errs.push(
            `${label}: operations.${opName} is ${built.isAsync ? "async on this alias but not on schema" : "async on schema but not on this alias"}`,
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
  lintExamples(spec, errs);

  // Collected before the canonical form is built (rather than dropped) so a
  // disallowed comment is reported as itself, not as a "not canonical" diff —
  // and so `--write` never silently deletes one.
  const comments = collectComments(raw);
  lintComments(comments, errs);

  const canon = serialize(spec, comments);
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
      // Not part of `violations`: a wrong `isAsync` doesn't block `--write`
      // (which builder a direction uses is derived from the schema, so the
      // recomputed goldens are right either way) — only the marker needs the
      // author's hand.
      errs.push(...asyncViolations(schema, spec));
      const fresh = knownFresh ?? serialize(await recomputeGoldens(spec), comments);
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

// Part of the compared text, so an edited or dropped header reads as stale
// like any other drift.
const BUNDLE_SIZE_HEADER = [
  "# Minified+gzipped bytes per public export of index.mjs, plus `total` for the whole entry.",
  "# Generated by `pnpm spec check --write` — every row is measured, so never hand-write one.",
].join("\n");

const serializeBundleSize = (obj: BundleSize): string =>
  BUNDLE_SIZE_HEADER + "\n" + stringifyYaml(order(obj, BUNDLE_SIZE_KEY_ORDER as string[]), { lineWidth: 0 });

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

// ---- scenarios.yaml --------------------------------------------------------

export const readScenarios = (raw: string = readScenariosRaw()): Scenarios =>
  raw ? ((parseYaml(raw) as Scenarios) ?? {}) : {};

const readScenariosRaw = (): string =>
  existsSync(SCENARIOS_PATH) ? readFileSync(SCENARIOS_PATH, "utf8") : "";

// Scenarios have no goldens, so this checks the file's shape and that each
// scenario runs. The second matters most: the perf pass reports a throwing
// scenario as "new" (indistinguishable from one the baseline predates), which
// would leave a typo quietly unmeasured forever. `raw`/`specIds` are
// injectable for tests, same as lintSpecsDir's `names`.
export const checkScenarios = (
  raw: string = readScenariosRaw(),
  specIds: string[] = listSpecFiles().map(specId),
): string[] => {
  if (!raw) return [];

  let parsed: unknown;
  try {
    parsed = parseYaml(raw);
  } catch (e) {
    return [`is not valid YAML: ${(e as Error).message}`];
  }

  const v = validateScenarios(parsed);
  if (!v.ok) return [`schema: ${v.error}`];

  const errs: string[] = [];
  const taken = new Set(specIds);
  for (const [id, scenario] of Object.entries(v.value)) {
    // `spec check --perf [id…]` resolves an id against both, so a name that
    // is both a spec and a scenario would silently select only one of them.
    if (taken.has(id)) errs.push(`${id}: id collides with a spec of the same name`);
    if (!VALID_ID_RE.test(id))
      errs.push(`${id}: invalid scenario id (only letters, digits, and - allowed)`);
    try {
      // Built exactly as benchChild.ts builds it (and buildScenarioRunner runs
      // it once), so what passes here is what the perf pass can measure.
      buildScenarioRunner(S, scenarioSource(scenario), { v: undefined });
    } catch (e) {
      errs.push(`${id}: did not run: ${(e as Error).message}`);
    }
  }
  return errs;
};

// Re-exported so `spec new` can populate ts.input/ts.output/ts.instantiations
// up front too (cli.ts only imports from harness.ts/format.ts, never touches
// introspect.ts/bundleSize.ts directly).
export { deriveTypeInfo, deriveVsTypeInfo, type TypeInfo } from "./introspect";
