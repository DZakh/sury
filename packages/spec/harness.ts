// Harness SUBJECT half: canonicalize specs and (re)compute goldens by
// executing the real schema.
//
// Unlike format.ts (which runs on published sury), this half imports the
// in-development sury SOURCE (`../sury/index.mjs`), because goldens must reflect
// the code under test - that's how `spec check` catches codegen changes.
//
// There is no code-generation step: packages/sury/tests/spec_test.ts loops
// over listSpecFiles()/readSpec() at run time and calls straight into this
// module, so drift in any dimension is exercised by a real Vitest run without
// ever materializing a generated .ts file per spec.
import { existsSync, readFileSync, readdirSync } from "node:fs";
import { isDeepStrictEqual } from "node:util";
import { join, basename } from "node:path";
import { fileURLToPath } from "node:url";
import {
  Document,
  parse as parseYaml,
  parseDocument,
  stringify as stringifyYaml,
  isMap,
  isSeq,
  visit,
  Scalar,
} from "yaml";
import { diffLinesUnified } from "@vitest/utils/diff";
import ts from "typescript";
import * as S from "../sury/index.mjs";
import {
  KEY_ORDER,
  TS_KEY_ORDER,
  VS_KEY_ORDER,
  VS_ZOD_KEY_ORDER,
  OP_ORDER,
  REQUIRED_OPS,
  OP_BLOCK_KEY_ORDER,
  BUNDLE_SIZE_KEY_ORDER,
  JSON_SCHEMA_KEY_ORDER,
  JSON_SCHEMA_DIALECT_KEY_ORDER,
  JSON_SCHEMA_TARGETS,
  IS_EQUAL_KEY_ORDER,
  type JsonSchemaDialect,
  type JsonSchemaTargetName,
  type IsEqualSides,
  SKIP_REASONS,
  isSkip,
  isZodOverwrite,
  isCreationError,
  isEqualSidesForm,
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
import {
  deriveRoundTripTypeInfo,
  deriveTypeInfo,
  deriveVsTypeInfo,
  type TypeInfo,
} from "./introspect";
import { deriveBundleSize } from "./bundleSize";
import { documentValidator, isJsonValue } from "./jsonSchemaValidator";
import * as z from "zod";

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

const SKIP_REASON_SET = new Set<string>(SKIP_REASONS);
export const isValidSkipReason = (r: unknown): boolean =>
  typeof r === "string" && (SKIP_REASON_SET.has(r) || /^todo\(#.+\)$/.test(r));

// `path` is relative to the spec root (e.g. `ts.instantiations`) - the reported
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

// Every name a compiled operation binds, so an assignment to anything else can
// be spotted. Scans the golden text rather than parsing it: the shapes Sury
// emits are `let a,b=…`, `for(let i=…`, `for(let k in o)` and `catch(x)`, and
// each one names its bindings up to the first `;`, `)` or `in`/`of` at depth 0.
const boundNames = (code: string): Set<string> => {
  // `i` is the operation's argument and `e` its embed array - the only two
  // free names generated code is allowed to read.
  const out = new Set(["i", "e"]);
  const heads = /\b(?:let|const|var)\s+|\bcatch\(/g;
  let head: RegExpExecArray | null;
  while ((head = heads.exec(code))) {
    let at = heads.lastIndex;
    for (;;) {
      let name = "";
      while (/\s/.test(code[at]!)) at++;
      while (/[\w$]/.test(code[at] ?? "")) name += code[at++];
      if (name) out.add(name);
      if (head[0] === "catch(") break;
      let depth = 0;
      for (;;) {
        const c = code[at];
        if (c === undefined) return out;
        if ("([{".includes(c)) depth++;
        else if (")]}".includes(c)) {
          if (depth === 0) break;
          depth--;
        } else if (depth === 0 && (c === ";" || c === ",")) break;
        at++;
      }
      if (code[at] !== ",") break;
      at++;
    }
  }
  return out;
};

// Generated code that assigns a name it never bound writes a *global*: Sury
// builds its functions with `new Function`, whose body is sloppy mode, so
// nothing reports it and two operations end up sharing the slot. The goldens
// are the only place the generated code is written down, so this is where it
// gets caught.
const scanAssignments = (code: string, where: string, extra: string[], out: string[]): void => {
  const bound = boundNames(code);
  for (const name of extra) bound.add(name);
  const leaked = new Set<string>();
  for (const [, name] of code.matchAll(/[({,;&|?:!= ]([A-Za-z_$][\w$]*)\s*=(?![=>])/g)) {
    if (!bound.has(name!)) leaked.add(name!);
  }
  if (leaked.size)
    out.push(
      `${where}: assigns ${[...leaked].join(", ")} without declaring ` +
        "it - generated code runs in sloppy mode, so that lands on globalThis",
    );
};

export const undeclaredAssignments = (spec: Spec, out: string[]): void => {
  const ops = spec.operations as Partial<Record<OpName, Operation>> | undefined;
  if (ops != null) {
    for (const opName of OP_ORDER) {
      const op = ops[opName];
      if (op == null || typeof op === "string" || isCreationError(op) || isSkip(op)) continue;
      const code = op.expression;
      if (typeof code !== "string") continue;
      scanAssignments(code, `operations.${opName}`, [], out);
    }
  }
  // Equality is compiled as `(a,b)=>…`, so those two are bound rather than free.
  const eq = spec.isEqual;
  if (eq == null || isSkip(eq)) return;
  for (const [side, code] of isEqualSidesForm(eq)
    ? ([["input", eq.input], ["output", eq.output]] as const)
    : ([["", eq]] as const))
    if (typeof code === "string")
      scanAssignments(code, side ? `isEqual.${side}` : "isEqual", ["a", "b"], out);
};

// A full op block is chosen over `identity`/`eq-to-parse` precisely because it
// has real codegen - and nothing ever runs that codegen until an example does,
// so an empty map snapshots an expression no test executes.
export const lintExamples = (spec: Spec, out: string[]): void => {
  const ops = spec.operations;
  if (ops == null || isSkip(ops)) return;
  for (const opName of OP_ORDER) {
    const op = ops[opName];
    if (op == null) {
      if (!(REQUIRED_OPS as readonly OpName[]).includes(opName)) continue;
      out.push(
        `operations.${opName}: missing - a spec must declare parse, decode, and encode ` +
          "(run `pnpm spec new` to scaffold them, or add the block)",
      );
      continue;
    }
    if (typeof op === "string" || isCreationError(op)) continue;
    if (isSkip(op)) {
      out.push(
        `operations.${opName}: _skip is not valid on an operation - use identity, eq-to-parse, ` +
          "a full block with examples, or a creationError",
      );
      continue;
    }
    if (op.examples && Object.keys(op.examples).length) continue;
    out.push(
      `operations.${opName}: no examples - a compiled op block must run at least one input ` +
        "(add a named entry with just `input`, then `--write` fills the result)",
    );
  }
};

// `S.global` sets process-wide configuration, and one library instance is
// shared by every spec in a run (cli.ts checks them under Promise.all, and
// spec_test.ts in one Vitest worker). A spec that calls it changes what its
// neighbours compile, in an order nothing controls - so the failure surfaces on
// some other spec, intermittently, and never on the one that caused it.
const GLOBAL_CALL = /\bS\s*\.\s*global\b/;
export const lintGlobal = (spec: Spec, out: string[]): void => {
  const sources: [string, unknown][] = [["ts.schema", spec.ts?.schema]];
  const aliases = spec.ts?.aliases;
  if (Array.isArray(aliases))
    aliases.forEach((alias, i) => sources.push([`ts.aliases[${i}]`, alias]));
  // An example's input is evaluated with the same `S` the schema is (see
  // valueEvaluator), so it reaches `global` exactly as the schema source does.
  // Covering only the schema would leave the rule with a hole the size of every
  // example in the suite.
  const ops = spec.operations as Partial<Record<OpName, Operation>> | undefined;
  for (const opName of OP_ORDER) {
    const op = ops?.[opName];
    if (op == null || typeof op === "string" || isCreationError(op) || isSkip(op)) continue;
    for (const [exName, ex] of Object.entries(op.examples ?? {}))
      sources.push([`operations.${opName}.examples.${exName}.input`, ex?.input]);
  }
  for (const [path, source] of sources)
    if (typeof source === "string" && GLOBAL_CALL.test(source))
      out.push(
        `${path}: calls S.global - it sets process-wide configuration that every other spec in the ` +
          "run then compiles against, so the failure lands somewhere else and only sometimes",
      );
};

export const specId = (file: string): string =>
  basename(file).replace(/\.yaml$/, "");

const VALID_ID_RE = /^[a-zA-Z0-9-]+$/;

// listSpecFiles below silently ignores anything that isn't *.yaml - this
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
    else if (id === "codec" || /^[a-z0-9]+-codec$/.test(id))
      errs.push(
        `specs dir: ${JSON.stringify(id)} names a codec spec backwards - use codec-<from>-<to>, not <from>-codec`,
      );
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
// `as const` so aliases can use it - `new Function` only ever sees plain JS.
//
// Diagnostics are reported and thrown, because transpiling REPAIRS what it
// cannot parse: `{ a: "hello"` (a missing brace) comes back as a complete
// object literal, and the golden then records a passing result for an input
// nobody wrote. transpileModule builds no program, so everything it reports is
// syntactic - source that does not parse, never a type error.
const TS_STRIP = {
  reportDiagnostics: true,
  compilerOptions: { target: ts.ScriptTarget.ESNext, module: ts.ModuleKind.ESNext },
};
const transpile = (tsSource: string): string => {
  const out = ts.transpileModule(tsSource, TS_STRIP);
  // The first only: one unbalanced brace cascades into four diagnostics, and
  // the rest describe the wreckage rather than the mistake.
  const [first] = out.diagnostics ?? [];
  if (first) throw new SyntaxError(ts.flattenDiagnosticMessageText(first.messageText, " "));
  return out.outputText.trim();
};
// The source is parenthesized before stripping (not after) so a bare object
// literal parses as an expression, not a block statement with a labeled
// statement inside - and the trailing `;\n` transpileModule always emits
// comes off since it's re-wrapped in `return … ;` below.
export const stripTypes = (tsSource: string): string =>
  transpile(`(${tsSource})`).replace(/;$/, "");

// A re-runnable evaluator for one source string, transpiled once. The matrix
// runs an example through a dozen spellings and hands each its OWN value: an
// operation that mutates what it was given would otherwise have every spelling
// after the first measuring the aftermath instead of the operation.
export const valueEvaluator = (tsSource: string): (() => any) => {
  const fn = new Function("S", `return ${stripTypes(tsSource)};`);
  return () => fn(S);
};

export const evalSchema = (tsSource: string): any => valueEvaluator(tsSource)();

// A scenario's `prepare` is statements, not an expression, so it goes through
// transpileModule directly - stripTypes' parenthesization exists only to keep
// a bare object literal from parsing as a block, which statements must not get.
const stripStatements = (tsSource: string): string => transpile(tsSource);

export const scenarioSource = (scenario: Scenario): ScenarioSource => ({
  prepareSrc: scenario.prepare === undefined ? undefined : stripStatements(scenario.prepare),
  runSrc: stripTypes(scenario.run),
});

// Sury compiles a pass-through operation to this shared function - the ONLY
// signal identity detection has. If this name is ever changed in Sury's
// source, every `identity`-marked operation starts failing loudly (across
// every spec, in `identityViolations` below) rather than silently going stale.
const NOOP_OPERATION_WHICH_WILL_NEVER_CHANGE = "noopOperation";
const isNoop = (fn: Function): boolean =>
  fn.name === NOOP_OPERATION_WHICH_WILL_NEVER_CHANGE;

// The comparators eq.ts hands back whole instead of compiling one. Recorded by
// name for the same reason and with the same risk as `identity` above: a
// compiled comparator comes out of `new Function` and so has no name at all,
// which is what makes the reading unambiguous, and a rename in Sury's source
// turns every spec that claims one stale rather than leaving it silently wrong.
const SHARED_COMPARATORS = ["alwaysEqual", "strictEqual", "sameValueZeroEqual"];
const comparatorForm = (fn: Function): string =>
  SHARED_COMPARATORS.includes(fn.name) ? fn.name : fn.toString();

// Checks the shorthand invariants both ways: a declared `identity`/`eq-to-parse`
// that doesn't hold, or a full op block that should be a shorthand.
export const identityViolations = (schema: any, spec: Spec, ops?: Record<OpName, BuiltOp>): string[] => {
  const out: string[] = [];
  if (isSkip(spec.operations)) return out;
  const builtOps = ops ?? buildOps(schema);
  const parseBuilt = builtOps.parse;
  const parseCode = "fn" in parseBuilt ? parseBuilt.fn.toString() : undefined;
  for (const opName of OP_ORDER) {
    const op = spec.operations[opName];
    if (op == null) continue;
    const built = builtOps[opName];
    // Rejected at operation creation: no compiled form, so the shorthand
    // invariants don't apply. recomputeGoldens records/refreshes the
    // `creationError` message, and the staleness diff carries any shape
    // transition (expression↔creationError) - same as jsonSchema's
    // success↔error string flips, which aren't gated here either.
    if (!("fn" in built)) continue;
    // Was a `{creationError}` block but now compiles - likewise left to
    // recompute + staleness, not flagged as a shorthand violation.
    if (isCreationError(op)) continue;
    const fn = built.fn;
    const noop = isNoop(fn);
    const matchesParse = opName !== "parse" && !noop && parseCode !== undefined && fn.toString() === parseCode;
    if (op === "identity") {
      if (!noop)
        out.push(
          `operations.${opName}: marked \`identity\` but does not compile to identity - use a full op block with examples`,
        );
    } else if (noop) {
      out.push(
        op === "eq-to-parse"
          ? `operations.${opName}: compiles to identity - use \`identity\` instead of \`eq-to-parse\``
          : `operations.${opName}: compiles to identity - use \`identity\` instead of an expression + examples`,
      );
    } else if (op === "eq-to-parse") {
      if (!matchesParse)
        out.push(
          `operations.${opName}: marked \`eq-to-parse\` but does not compile to the same code as parse - use a full op block with examples`,
        );
    } else if (matchesParse) {
      out.push(
        `operations.${opName}: compiles to the same code as parse - use \`eq-to-parse\` instead of an expression + examples`,
      );
    }
  }
  return out;
};

// The `isAsync` marker checked both ways, like identityViolations: an async
// direction must declare it (the operation returns a Promise - a different API
// for every consumer, and different codegen), and a declared one must hold.
// Only full op blocks carry the marker: `identity` can't be async (an async op
// never compiles to Sury's noop, so identityViolations already reports it),
// `eq-to-parse` inherits parse's block, and a `{creationError}` block has no
// compiled operation to be async.
export const asyncViolations = (schema: any, spec: Spec, ops?: Record<OpName, BuiltOp>): string[] => {
  const out: string[] = [];
  const builtOps = ops ?? buildOps(schema);
  for (const opName of OP_ORDER) {
    const op = spec.operations[opName];
    if (op == null || typeof op === "string" || isCreationError(op)) continue;
    const built = builtOps[opName];
    // Rejected at creation: reported by the creationError golden instead, and
    // an operation that doesn't compile can't be async.
    if (!("fn" in built)) continue;
    const isAsync = built.isAsync;
    if (isAsync && op.isAsync !== true)
      out.push(
        `operations.${opName}: is async (the schema has an async transform or refine) - add \`isAsync: true\`, ` +
          "which builds it with the AsPromiseOrReject operations and awaits every example",
      );
    else if (!isAsync && op.isAsync === true)
      out.push(
        `operations.${opName}: marked \`isAsync: true\` but the operation is synchronous - remove the marker ` +
          "(the async builders would only wrap the result in `Promise.resolve`)",
      );
  }
  return out;
};

// JSON Schema has no representation for bigint or symbol, so the conversion
// throws for any schema containing one (at any nesting depth) - a real "this
// concept doesn't apply" case, not a bug to work around. Recorded per
// direction (rather than skipping the whole dimension) since the two
// directions can differ - e.g. a `.to` transform might make only one side
// representable. Shared by `scaffoldJsonSchema` (spec new) and
// `recomputeGoldens` (spec check/--write) so both degrade the same way.
// Always a string (source text, same formatting as example values) so the
// success case (the schema itself) and the failure case (the thrown message)
// are one uniform, one-line field - not a structural union at the YAML level.
const toJsonSchemaOrError = (
  fn: () => unknown,
): { ok: true; source: string } | { ok: false; error: string } => {
  try {
    return { ok: true, source: valueToCode(fn()) };
  } catch (e) {
    return { ok: false, error: (e as Error).message };
  }
};
type JsonSchemaSide = { schema: string; source?: string };
type JsonSchemaSides = { input: JsonSchemaSide; output: JsonSchemaSide };

const deriveJsonSchemaSide = (fn: () => unknown): JsonSchemaSide => {
  const result = toJsonSchemaOrError(fn);
  if (!result.ok) return { schema: result.error };
  return { schema: result.source, source: result.source };
};

const deriveJsonSchemaSides = (schema: any): JsonSchemaSides => ({
  input: deriveJsonSchemaSide(() => S.toInputJSONSchemaOrThrow(schema)),
  output: deriveJsonSchemaSide(() => S.toOutputJSONSchemaOrThrow(schema)),
});

const withoutDollarSchema = (value: unknown): unknown => {
  if (value && typeof value === "object" && !Array.isArray(value) && "$schema" in (value as object)) {
    const rest = { ...(value as Record<string, unknown>) };
    delete rest.$schema;
    return rest;
  }
  return value;
};

const jsonSchemaSourceDiffers = (defaultSource: string, targetSource: string): boolean => {
  if (defaultSource === targetSource) return false;
  try {
    return (
      valueToCode(withoutDollarSchema(evalSchema(defaultSource))) !==
      valueToCode(withoutDollarSchema(evalSchema(targetSource)))
    );
  } catch {
    return true;
  }
};

const deriveJsonSchemaTarget = async (
  schema: any,
  schemaTs: string,
  types: { input: string; output: string },
  target: JsonSchemaTargetName,
  defaultSides: JsonSchemaSides,
  defaultTypes: { fromInput?: string; fromOutput?: string },
): Promise<JsonSchemaDialect | undefined> => {
  const input = deriveJsonSchemaSide(() => S.toInputJSONSchemaOrThrow(schema, { target }));
  const output = deriveJsonSchemaSide(() => S.toInputJSONSchemaOrThrow(S.reverse(schema), { target }));
  const inputDiffers = jsonSchemaSourceDiffers(defaultSides.input.schema, input.schema);
  const outputDiffers = jsonSchemaSourceDiffers(defaultSides.output.schema, output.schema);
  if (!inputDiffers && !outputDiffers) return undefined;
  const roundTrip = await deriveRoundTripTypeInfo(
    schemaTs,
    inputDiffers ? input.source : undefined,
    outputDiffers ? output.source : undefined,
  );
  const defaultFromInput = defaultTypes.fromInput ?? types.input;
  const defaultFromOutput = defaultTypes.fromOutput ?? types.output;
  const pick = (
    differs: boolean,
    schemaSrc: string,
    from: string | undefined,
    fallback: string,
    schemaKey: "input" | "output",
    fromKey: "fromInputType" | "fromOutputType",
  ) => ({
    ...(differs ? { [schemaKey]: schemaSrc } : {}),
    ...(differs && from !== undefined && from !== fallback ? { [fromKey]: from } : {}),
  });
  return {
    ...pick(inputDiffers, input.schema, roundTrip.fromInput, defaultFromInput, "input", "fromInputType"),
    ...pick(outputDiffers, output.schema, roundTrip.fromOutput, defaultFromOutput, "output", "fromOutputType"),
  };
};

const deriveJsonSchema = async (
  schema: any,
  types: { input: string; output: string },
  combinedInfo: Pick<
    TypeInfo,
    "fromInput" | "fromOutput" | "inputMatches" | "outputMatches"
  >,
  sides = deriveJsonSchemaSides(schema),
  schemaTs?: string,
): Promise<Spec["jsonSchema"]> => {
  const inputInferred = combinedInfo.fromInput;
  const outputInferred = combinedInfo.fromOutput;
  const inputMatches = combinedInfo.inputMatches ?? inputInferred === types.input;
  const outputMatches = combinedInfo.outputMatches ?? outputInferred === types.output;
  const fromType = (
    inferred: string | undefined,
    matches: boolean,
    key: "fromInputType" | "fromOutputType",
  ) => (inferred === undefined || matches ? {} : { [key]: inferred });
  const doc: Spec["jsonSchema"] = {
    input: sides.input.schema,
    ...fromType(inputInferred, inputMatches, "fromInputType"),
    output: sides.output.schema,
    ...fromType(outputInferred, outputMatches, "fromOutputType"),
  };
  if (schemaTs === undefined) return doc;
  for (const target of JSON_SCHEMA_TARGETS) {
    const block = await deriveJsonSchemaTarget(schema, schemaTs, types, target, sides, {
      fromInput: inputInferred,
      fromOutput: outputInferred,
    });
    if (block && Object.keys(block).length) doc[target] = block;
  }
  return doc;
};

// No example inputs needed, so `spec new` can fill this in immediately from `--ts`.
export const scaffoldJsonSchema = (
  schema: any,
  types: { input: string; output: string },
  schemaTs: string,
): Promise<Spec["jsonSchema"]> => {
  const sides = deriveJsonSchemaSides(schema);
  return deriveRoundTripTypeInfo(
    schemaTs,
    sides.input.source,
    sides.output.source,
  ).then((roundTripInfo) =>
    deriveJsonSchema(schema, types, roundTripInfo, sides, schemaTs),
  );
};

// Compile an operation, capturing any creation-time throw as the golden instead
// of letting it abort - the operation analogue of toJsonSchemaOrError. The
// message is prefixed with the error class (`SuryError:` for an intended
// unsupported/ambiguous conversion, `TypeError:` etc. for an internal fault),
// so a bug stays visibly distinct in the golden - and flips back to compiled
// code once a fix turns the crash into a real operation - rather than silently
// masquerading as a normal rejection.
type BuiltOp = { fn: (input: any) => any; isAsync: boolean } | { creationError: string };
const describeThrow = (e: unknown): string =>
  `${(e as Error).constructor.name}: ${(e as Error).message}`;

// A rejection Sury raises is recorded by its message alone: the message is the
// product surface, and stamping one constant class name onto every error golden
// in the suite would cost bytes on every line to say what the format already
// guarantees. Anything else is a leak - an internal TypeError, a RangeError out
// of a Date, a foreign exception thrown by user code in a custom codec - and
// carries its class, so the golden shows WHAT threw, a fix that turns a crash
// into a real rejection reads as a diff, and a message whose wording belongs to
// the platform rather than to Sury is visible as such. Same form as
// `describeThrow` above, which does this unconditionally for creation errors.
const SURY_ERROR_NAME = "SuryError";
export const describeExampleThrow = (e: unknown): string => {
  // A thrown non-Error (user code doing `throw "nope"`) is itself a leak worth
  // naming, and has no class to name it with.
  if (!(e instanceof Error)) return `${typeof e}: ${String(e)}`;
  const name = e.constructor?.name;
  return name === undefined || name === SURY_ERROR_NAME ? e.message : `${name}: ${e.message}`;
};
// Sury has no `isAsync` probe - a schema's combinations are open-ended, so no
// static answer covers them - and the sync builder rejecting is what tells a
// caller to switch. The harness does the same, per direction, which matters
// when only one direction is async. Retry on the error's `code`, not its
// wording: a non-async `invalid_operation` fails the async attempt too and is
// recorded from there, so the only cost of the broader test is one extra
// compile.
const buildOp = (opName: OpName, schema: any): BuiltOp => {
  try {
    return { fn: OP_BUILDER[opName](schema), isAsync: false };
  } catch (e) {
    if ((e as { code?: string }).code !== "invalid_operation") {
      return { creationError: describeThrow(e) };
    }
  }
  try {
    return { fn: ASYNC_OP_BUILDER[opName](schema), isAsync: true };
  } catch (e) {
    return { creationError: describeThrow(e) };
  }
};

export const buildOps = (schema: any): Record<OpName, BuiltOp> => {
  const out = {} as Record<OpName, BuiltOp>;
  for (const name of OP_ORDER) out[name] = buildOp(name, schema);
  return out;
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

// The equality dimension: one string when both sides compile to the same code,
// the pair when they don't. Shared by `spec new` and `--write`.
export const deriveIsEqual = (schema: any): string | IsEqualSides => {
  const input = comparatorForm(S.isEqualInput(schema));
  const output = comparatorForm(S.isEqualOutput(schema));
  return input === output ? input : { input, output };
};

// Can throw if `schema` isn't actually a usable schema (e.g. `--ts` evaluated
// to `undefined` from a typo like `S.strng`) - callers decide how to report that.
export const scaffoldOperations = (schema: any): Spec["operations"] => {
  const builtOps = buildOps(schema);
  const parseBuilt = builtOps.parse;
  return Object.fromEntries(
    REQUIRED_OPS.map((opName) => [opName, opForm(opName, builtOps[opName], parseBuilt)]),
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
// by round-tripping each through eval - independent of recomputeGoldens, so
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

// Individual named examples are never `_skip` - only the enclosing operation
// block is (the format schema has no `orSkip` on the examples map's values).
const canonExample = (ex: Example): Example => {
  const o = order(ex, [
    "input",
    "output",
    "error",
    "errorConstructor",
    "divergence",
  ]) as Example;
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
  if (o.jsonSchema && !isSkip(o.jsonSchema)) {
    o.jsonSchema = order(o.jsonSchema as Record<string, unknown>, JSON_SCHEMA_KEY_ORDER as string[]) as typeof o.jsonSchema;
    for (const name of JSON_SCHEMA_TARGETS) {
      const block = o.jsonSchema[name];
      if (!block) {
        delete o.jsonSchema[name];
        continue;
      }
      const ordered = order(block as Record<string, unknown>, JSON_SCHEMA_DIALECT_KEY_ORDER as string[]);
      if (Object.keys(ordered).length === 0) delete o.jsonSchema[name];
      else o.jsonSchema[name] = ordered as JsonSchemaDialect;
    }
  }
  if (isEqualSidesForm(o.isEqual))
    o.isEqual = order(o.isEqual as Record<string, unknown>, IS_EQUAL_KEY_ORDER as string[]) as typeof o.isEqual;
  if (o.operations && !isSkip(o.operations)) {
    const ops = order(o.operations, OP_ORDER) as Record<OpName, Operation>;
    for (const name of OP_ORDER) if (ops[name]) ops[name] = canonOp(ops[name]);
    o.operations = ops as typeof o.operations;
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
// nothing enforces. The one exception is `FIXME:` - a marker for behavior the
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
          `${path ? `${path}: ` : ""}comment ${JSON.stringify(first)} is not allowed - prefix it with ` +
            `\`${FIXME}\` if it flags broken behavior to address, or move it to Spec Harness Suggestions ` +
            `in CONTRIBUTING.md if the spec format can't express it`,
        );
      }
    }
};

const YAML_CONTROL = /[\u0000-\u0008\u0009\u000b\u000c\u000e-\u001f\u007f-\u009f]/;
// JSON.stringify (which yaml uses for double-quoted scalars) leaves DEL and
// the C1 block raw, and those bytes are what PyYAML and yamllint reject.
const escapeC1 = (text: string): string =>
  text.replace(/[\u007f-\u009f]/g, (ch) => `\\u${ch.charCodeAt(0).toString(16).padStart(4, "0")}`);

export const serialize = (obj: Spec, comments: SpecComments = NO_COMMENTS): string => {
  const doc = new Document(canonicalize(obj));
  if (comments.size) applyComments(doc, comments);
  visit(doc, {
    Scalar(_key, node) {
      if (typeof node.value === "string" && YAML_CONTROL.test(node.value))
        node.type = Scalar.QUOTE_DOUBLE;
    },
  });
  // The visit above forced QUOTE_DOUBLE for every scalar holding one of those
  // bytes, so the escape lands inside quotes.
  return HEADER + "\n" + escapeC1(doc.toString({ lineWidth: 0 }));
};

// ---- golden recomputation --------------------------------------------------

// An object key needs quotes only when it isn't a valid identifier - matches
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
// round-trips through source text - a bare `Symbol()` is unique per call, so
// no source expression can reproduce it.
//
// Anything the emitted source would NOT evaluate back to (structurally) must
// throw rather than emit: a cyclic value would recurse forever, and a class
// instance would silently flatten to a plain-object literal - each of those
// would record a golden that looks fine but doesn't equal the real output.
// A binary container's bytes are only readable asynchronously, so they are
// collected in one pass before rendering and handed to `valueToCode` through
// this map. Everything the writer walks is walked here too, in the same order.
type Bytes = WeakMap<object, Uint8Array>;
const readBytes = async (v: unknown, out: Bytes): Promise<void> => {
  if (v === null || typeof v !== "object") return;
  if (isBlob(v)) {
    out.set(v, new Uint8Array(await (v as Blob).arrayBuffer()));
    return;
  }
  if (isFormData(v)) {
    for (const [, entry] of (v as FormData).entries()) await readBytes(entry, out);
    return;
  }
  if (v instanceof Map) for (const pair of v) await readBytes(pair, out);
  else if (v instanceof Set || Array.isArray(v)) for (const item of v as Iterable<unknown>) await readBytes(item, out);
  else if (Object.getPrototypeOf(v) === Object.prototype) for (const item of Object.values(v)) await readBytes(item, out);
};

const globalClass = (name: string): Function | undefined =>
  (globalThis as Record<string, unknown>)[name] as Function | undefined;
const isBlob = (v: object): boolean => {
  const c = globalClass("Blob");
  return c !== undefined && v instanceof c;
};
const isFile = (v: object): boolean => {
  const c = globalClass("File");
  return c !== undefined && v instanceof c;
};
const isFormData = (v: object): boolean => {
  const c = globalClass("FormData");
  return c !== undefined && v instanceof c;
};
const isURLSearchParams = (v: object): boolean => {
  const c = globalClass("URLSearchParams");
  return c !== undefined && v instanceof c;
};

// Bytes read best as the text that produced them, which is what a spec author
// writes; anything else (and anything with a control byte, which YAML would
// have to escape) falls back to the byte array.
const TEXT_SAFE = /^[^\u0000-\u0008\u000b\u000c\u000e-\u001f\u007f-\u009f]*$/;
const bytesToCode = (bytes: Uint8Array): string => {
  const text = new TextDecoder("utf-8", { fatal: false }).decode(bytes);
  return TEXT_SAFE.test(text) && String(new TextEncoder().encode(text)) === String(bytes)
    ? escapeC1(JSON.stringify(text))
    : `new Uint8Array([${[...bytes].join(", ")}])`;
};

// `type` is rendered only when set, and `lastModified` never: it defaults to
// the moment the file is built, so recording it would rewrite the golden on
// every run.
const blobToCode = (v: object, bytes: Bytes): string => {
  const own = bytes.get(v);
  if (own === undefined) throw new Error("cannot represent a Blob whose bytes were not read");
  const type = (v as Blob).type;
  const options = type ? `, { type: ${JSON.stringify(type)} }` : "";
  return isFile(v)
    ? `new File([${bytesToCode(own)}], ${JSON.stringify((v as File).name)}${options})`
    : `new Blob([${bytesToCode(own)}]${options})`;
};

const recordToCode = (parts: string[]): string =>
  parts.length === 0 ? "{}" : `{ ${parts.join(", ")} }`;


const valueToCode = (v: unknown, seen: WeakSet<object> = new WeakSet(), bytes: Bytes = new WeakMap()): string => {
  if (v === undefined) return "undefined";
  if (typeof v === "bigint") return `${v}n`;
  if (typeof v === "number") return Object.is(v, -0) ? "-0" : String(v);
  if (v === null || typeof v === "boolean") return JSON.stringify(v);
  if (typeof v === "string") return escapeC1(JSON.stringify(v));
  if (typeof v === "symbol") {
    const key = Symbol.keyFor(v);
    if (key === undefined)
      throw new Error("cannot represent a non-registry symbol (use Symbol.for(key)) as spec source code");
    return `Symbol.for(${JSON.stringify(key)})`;
  }
  if (typeof v === "object") {
    if (seen.has(v)) throw new Error("cannot represent a cyclic value as spec source code");
    seen.add(v);
    try {
      if (v instanceof Date) return `new Date(${JSON.stringify(v.toISOString())})`;
      if (v instanceof URL) return `new URL(${JSON.stringify(v.href)})`;
      if (v instanceof RegExp) return v.toString();
      // A subclass (Node's Buffer) would read back as a plain Uint8Array, so it
      // stays unrepresentable rather than silently narrowed.
      if (Object.getPrototypeOf(v) === Uint8Array.prototype)
        return `new Uint8Array([${[...(v as Uint8Array)].join(", ")}])`;
      if (isBlob(v)) return blobToCode(v, bytes);
      // The same idiom a spec author writes for an input: `append` returns
      // nothing, so the comma expression hands the FormData back.
      if (isFormData(v)) {
        const appends = [...(v as FormData).entries()]
          .map(([k, entry]) => `f.append(${JSON.stringify(k)}, ${valueToCode(entry, seen, bytes)})`)
          .join(", ");
        return appends
          ? `((f) => (${appends}, f))(new FormData())`
          : "new FormData()";
      }
      if (isURLSearchParams(v)) {
        const appends = [...(v as URLSearchParams).entries()]
          .map(([k, entry]) => `p.append(${JSON.stringify(k)}, ${valueToCode(entry, seen, bytes)})`)
          .join(", ");
        return appends
          ? `((p) => (${appends}, p))(new URLSearchParams())`
          : "new URLSearchParams()";
      }
      if (v instanceof Map) return `new Map(${valueToCode([...v], seen, bytes)})`;
      if (v instanceof Set) return `new Set(${valueToCode([...v], seen, bytes)})`;
      if (Array.isArray(v)) return `[${v.map((x) => valueToCode(x, seen, bytes)).join(", ")}]`;
      const proto = Object.getPrototypeOf(v);
      if (proto !== Object.prototype && proto !== null)
        throw new Error(
          `cannot represent a ${(v as object).constructor?.name ?? "unknown-class"} instance as spec source code`,
        );
      // Symbol keys ride along as computed keys - only registry symbols, for the
      // same reason as symbol values above. Object.entries would drop them.
      const parts = Object.entries(v).map(([k, val]) => `${keyToCode(k)}: ${valueToCode(val, seen, bytes)}`);
      for (const sym of Object.getOwnPropertySymbols(v)) {
        if (!Object.getOwnPropertyDescriptor(v, sym)!.enumerable) continue;
        parts.push(`[${valueToCode(sym, seen, bytes)}]: ${valueToCode((v as Record<symbol, unknown>)[sym], seen, bytes)}`);
      }
      return recordToCode(parts);
    } finally {
      seen.delete(v);
    }
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
export const recomputeGoldens = async (obj: Spec, compiled?: Record<OpName, BuiltOp>): Promise<Spec> => {
  const next: Spec = structuredClone(obj);
  const schema = evalSchema(next.ts.schema);
  const jsonSchemaSides = deriveJsonSchemaSides(schema);
  const info = await deriveTypeInfo(next.ts.schema);
  const roundTripInfo = await deriveRoundTripTypeInfo(
    next.ts.schema,
    jsonSchemaSides.input.source,
    jsonSchemaSides.output.source,
  );

  if (!isSkip(next.ts.input)) next.ts.input = info.input;
  if (!isSkip(next.ts.output)) next.ts.output = info.output;
  if (!isSkip(next.ts.instantiations)) next.ts.instantiations = info.instantiations;

  // The overwrite form of `vs.zod` records Zod's inferred types as goldens for
  // the side(s) that diverge from ts; the harness owns those, so fill from the
  // live Zod schema. An omitted side matches ts and isn't recorded (checkVs
  // verifies the match). A schema that doesn't typecheck throws here and
  // surfaces via checkSpec's "goldens could not be computed" - same as any
  // other uncomputable golden.
  if (isZodOverwrite(next.vs.zod)) {
    const zi = await deriveVsTypeInfo(ZOD_IMPORT, next.vs.zod.schema);
    if (next.vs.zod.input !== undefined) next.vs.zod.input = zi.input;
    if (next.vs.zod.output !== undefined) next.vs.zod.output = zi.output;
  }

  next.jsonSchema = await deriveJsonSchema(
    schema,
    info,
    roundTripInfo,
    jsonSchemaSides,
    next.ts.schema,
  );

  if (!isSkip(next.isEqual)) next.isEqual = deriveIsEqual(schema);

  const builtOps = compiled ?? buildOps(schema);
  const parseBuilt = builtOps.parse;
  // Indexing by a `OpName` union narrows the value type to the intersection of
  // the three fields (which drops `eq-to-parse`), so reassignments below go
  // through this widened view.
  const ops = next.operations as Record<OpName, Operation>;
  for (const opName of OP_ORDER) {
    const op = next.operations[opName];
    if (op == null) continue;
    const built = builtOps[opName];
    if ("creationError" in built) {
      // Rejected at creation - take the canonical creationError form (a block,
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
      // Was rejected, now compiles - rewrite to the canonical block/shorthand.
      // Examples are author-owned and can't be invented, so start empty.
      ops[opName] = opForm(opName, built, parseBuilt);
      continue;
    }
    if (!isSkip(op.expression)) op.expression = fn.toString();
    for (const [name, ex] of Object.entries(op.examples)) {
      // Evaluated OUTSIDE the try below: an input that does not parse, or that
      // names something undefined, is an authoring mistake. Recording the
      // evaluator's own complaint as this example's `error` golden would pin
      // the typo instead of the schema - green forever, running nothing.
      // Aborting here is what keeps `--write` from minting that golden;
      // checkExamples reports the same thing pointedly, per example.
      let value: unknown;
      try {
        value = evalSchema(ex.input);
      } catch (e) {
        throw new Error(
          `operations.${opName}.examples.${name}: input did not evaluate: ${(e as Error).message}`,
        );
      }
      try {
        // `await` on a sync operation's result is a no-op, so both kinds run
        // through one path. An async operation can still throw synchronously
        // (the top-level type check runs before the first await), which the
        // same catch handles - a rejection and a synchronous throw are one
        // outcome to the author.
        const out = await fn(value);
        // Binary containers only yield their bytes asynchronously, so they are
        // read here, before the sync writer runs.
        const bytes: Bytes = new WeakMap();
        await readBytes(out, bytes);
        // An operation that hands its input straight back records the input's
        // own source rather than a re-derived spelling of the same value. It
        // reads better, and it's the only way a value the serializer can't
        // reproduce gets a passing example at all - a Blob or a File only
        // yields its bytes asynchronously, so `new File(["ab"], "a.txt")`
        // could be *run* but never written down as a result.
        op.examples[name] = clean({
          input: ex.input,
          output: out === value ? ex.input : valueToCode(out, new WeakSet(), bytes),
          ...(await refreshDivergence(opName, op.isAsync === true, schema, ex, next)),
        });
      } catch (e) {
        if (e instanceof Error && e.message.startsWith("cannot represent ")) throw e;
        op.examples[name] = clean({
          input: ex.input,
          ...("errorConstructor" in ex
            ? { errorConstructor: (e as Error).constructor.name }
            : { error: describeExampleThrow(e) }),
          ...(await refreshDivergence(opName, op.isAsync === true, schema, ex, next)),
        });
      }
    }
  }

  return next;
};

const jsonSchemaTypePresenceViolations = (spec: Spec, expected: Spec): string[] => {
  const errs: string[] = [];
  if (isSkip(spec.jsonSchema) || isSkip(expected.jsonSchema)) return errs;
  for (const { field, side } of [
    { field: "fromInputType", side: "input" },
    { field: "fromOutputType", side: "output" },
  ] as const) {
    const currentType = spec.jsonSchema[field];
    const expectedType = expected.jsonSchema[field];
    const schemaType = expected.ts[side];
    if (isSkip(schemaType)) continue;
    if (currentType !== undefined && expectedType === undefined) {
      try {
        evalSchema(expected.jsonSchema[side]);
        errs.push(
          `jsonSchema.${field}: S.fromJSONSchemaOrThrow(jsonSchema.${side}) matches ts.${side} ` +
            `${JSON.stringify(schemaType)} - omit \`${field}\`.`,
        );
      } catch {
        errs.push(
          `jsonSchema.${field}: jsonSchema.${side} failed to create, so there is no round-trip type to record - omit \`${field}\`.`,
        );
      }
    } else if (currentType === undefined && expectedType !== undefined)
      errs.push(
        `jsonSchema.${field}: omitted, but S.fromJSONSchemaOrThrow(jsonSchema.${side}) infers ` +
          `${JSON.stringify(expectedType)} !== ts.${side} ${JSON.stringify(schemaType)} - add \`${field}\`.`,
      );
  }
  return errs;
};

// `ts.schema` can evaluate without throwing to a value that still isn't a
// usable Sury schema (e.g. `ts.schema: "42"` evaluates to the number 42).
// Every Sury schema carries a Standard Schema `~standard` prop whose `vendor`
// is `"sury"` (Sury's own internals use this exact check - see `assert` in
// the sury entry) - a reliable, non-throwing alternative to probing with a builder.
const isUsableSchema = (schema: unknown): boolean =>
  (schema as { ["~standard"]?: { vendor?: string } } | null | undefined)?.["~standard"]?.vendor === "sury";

const identity = (s: string): string => s;

// A plain (no ANSI color, since this also renders inside inline test
// snapshots and CI logs) git-style unified diff between two spec texts, so
// "not canonical"/"goldens stale" show exactly what differs instead of just
// asserting that something does. `a` is the current text, `b` the target -
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
// `ts.schema` - same ts.input/ts.output, jsonSchema, and operations -
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

    // Isolated per alias - a throw here (e.g. deriveTypeInfo failing to
    // resolve the alias's type) must not abort the remaining aliases or
    // surface as the outer, label-less "goldens could not be computed".
    try {
      const jsonSchemaSides = deriveJsonSchemaSides(aliasSchema);
      const info = await deriveTypeInfo(aliasSrc);
      const roundTripInfo = await deriveRoundTripTypeInfo(
        aliasSrc,
        jsonSchemaSides.input.source,
        jsonSchemaSides.output.source,
      );
      if (!isSkip(spec.ts.input) && info.input !== spec.ts.input)
        errs.push(`${label}: ts.input ${JSON.stringify(info.input)} !== ${JSON.stringify(spec.ts.input)}`);
      if (!isSkip(spec.ts.output) && info.output !== spec.ts.output)
        errs.push(`${label}: ts.output ${JSON.stringify(info.output)} !== ${JSON.stringify(spec.ts.output)}`);

      const js = await deriveJsonSchema(
        aliasSchema,
        info,
        roundTripInfo,
        jsonSchemaSides,
        aliasSrc,
      );
      if (!isSkip(spec.jsonSchema)) {
        if (js.input !== spec.jsonSchema.input)
          errs.push(`${label}: jsonSchema.input differs:\n${diffText(spec.jsonSchema.input, js.input)}`);
        if (js.output !== spec.jsonSchema.output)
          errs.push(`${label}: jsonSchema.output differs:\n${diffText(spec.jsonSchema.output, js.output)}`);
        for (const name of JSON_SCHEMA_TARGETS) {
          if (JSON.stringify(js[name] ?? null) !== JSON.stringify(spec.jsonSchema[name] ?? null))
            errs.push(
              `${label}: jsonSchema[${JSON.stringify(name)}] differs:\n${diffText(
                JSON.stringify(spec.jsonSchema[name] ?? null),
                JSON.stringify(js[name] ?? null),
              )}`,
            );
        }
      }
      if (isSkip(spec.operations)) continue;
      const aliasOps = buildOps(aliasSchema);
      const aliasParseBuilt = aliasOps.parse;
      const aliasParseCode = "fn" in aliasParseBuilt ? aliasParseBuilt.fn.toString() : undefined;
      for (const opName of OP_ORDER) {
        const op = spec.operations[opName];
        if (op == null) continue;
        const built = aliasOps[opName];
        if (isCreationError(op)) {
          if ("fn" in built)
            errs.push(`${label}: operations.${opName} is a \`creationError\` on schema but compiles on this alias`);
          else if (built.creationError !== op.creationError)
            errs.push(`${label}: operations.${opName}.creationError differs:\n${diffText(op.creationError, built.creationError)}`);
          continue;
        }
        if (!("fn" in built)) {
          // Valid when the schema's op is the co-failure `eq-to-parse` and the
          // alias's parse is rejected with the same message - both fail at
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

// ---- operation matrix ------------------------------------------------------
//
// A spec pins ONE spelling of each direction: the compiled, data-last, throwing
// form, `S.parseOrThrow(schema)(data)`. Which spelling a caller writes is a
// property of the CALL, not of the schema, so no golden notices when
// `op(data, schema)` reads the schema as the data, or when `AsResult` reports a
// success the throwing form rejects. Every example is therefore re-run through
// every equivalent spelling and has to land on the outcome the golden records.
//
// Live, like checkAliases and checkVs: nothing new is written down, because
// there is nothing here the golden doesn't already say.

// A `Ref` is what one spelling answered: a value, or the message it failed
// with. How it failed (a throw, a rejection, a `{success: false}`) is the
// spelling's own business — that is what makes the outcomes comparable.
type Ref = { value: unknown } | { message: string } | { ctor: string };

const sameBlob = async (a: unknown, b: unknown): Promise<boolean> => {
  if (typeof Blob === "undefined" || !(a instanceof Blob) || !(b instanceof Blob)) return false;
  if (a.constructor !== b.constructor) return false;
  if (typeof File !== "undefined" && a instanceof File && b instanceof File && a.name !== b.name)
    return false;
  const [aa, bb] = await Promise.all([a.arrayBuffer(), b.arrayBuffer()]);
  return isDeepStrictEqual(new Uint8Array(aa), new Uint8Array(bb));
};

const sameValue = async (a: unknown, b: unknown): Promise<boolean> => {
  if (a === b) return true;
  if (typeof Blob !== "undefined" && a instanceof Blob) return sameBlob(a, b);
  if (typeof Blob !== "undefined" && b instanceof Blob) return false;
  if (isDeepStrictEqual(a, b)) return true;
  if (Array.isArray(a) && Array.isArray(b)) {
    if (a.length !== b.length) return false;
    for (let i = 0; i < a.length; i++) if (!(await sameValue(a[i], b[i]))) return false;
    return true;
  }
  if (
    a !== null &&
    b !== null &&
    typeof a === "object" &&
    typeof b === "object" &&
    Object.getPrototypeOf(a) === Object.prototype &&
    Object.getPrototypeOf(b) === Object.prototype
  ) {
    const ak = Object.keys(a);
    const bk = Object.keys(b);
    if (ak.length !== bk.length) return false;
    const ao = a as Record<string, unknown>;
    const bo = b as Record<string, unknown>;
    for (const k of ak) {
      if (!Object.prototype.hasOwnProperty.call(b, k)) return false;
      if (!(await sameValue(ao[k], bo[k]))) return false;
    }
    return true;
  }
  return false;
};

// Result wraps a foreign Error as `Name: message` (`B_foreignDetails`); throw
// stays `message`. Same failure, two spellings of it.
const sameMessage = (a: string, b: string): boolean => {
  if (a === b) return true;
  const [short, long] = a.length <= b.length ? [a, b] : [b, a];
  return (
    long.endsWith(short) && /^[A-Za-z][\w]*: $/.test(long.slice(0, long.length - short.length))
  );
};

const ctorMatchesMessage = (ctor: string, message: string): boolean =>
  message === ctor || message.startsWith(`${ctor}: `);

const sameOutcome = async (a: Ref, b: Ref): Promise<boolean> => {
  if ("ctor" in a) {
    if ("ctor" in b) return a.ctor === b.ctor;
    return "message" in b && ctorMatchesMessage(a.ctor, b.message);
  }
  if ("message" in a) {
    if ("message" in b) return sameMessage(a.message, b.message);
    return "ctor" in b && ctorMatchesMessage(b.ctor, a.message);
  }
  return !("message" in b) && !("ctor" in b) && (await sameValue(a.value, b.value));
};

const describeRef = (r: Ref): string =>
  "ctor" in r
    ? `failed with ${r.ctor}`
    : "message" in r
      ? `failed with ${JSON.stringify(r.message)}`
      : `returned ${valueToCodeSafe(r.value)}`;

// Whether a Blob sits anywhere in a value. The schema's own comparator reads
// one by identity - bytes come back only asynchronously, so a synchronous
// answer has nothing else to go on - while `sameBlob` above reads the bytes.
// That is a deliberate difference, so the cross-check below steps around it
// rather than reporting it on every run.
const holdsBlob = (v: unknown): boolean => {
  if (typeof Blob === "undefined" || v === null || typeof v !== "object") return false;
  if (v instanceof Blob) return true;
  if (Array.isArray(v)) return v.some(holdsBlob);
  if (v instanceof FormData) return [...v].some((entry) => holdsBlob(entry[1]));
  if (v instanceof Set) return [...v].some(holdsBlob);
  if (Object.getPrototypeOf(v) !== Object.prototype) return false;
  return Object.values(v as Record<string, unknown>).some(holdsBlob);
};

// Two runs of the same input agree only on the nose. `sameOutcome` is built for
// comparing a golden against a spelling, where `boom` and `Error: boom` are one
// failure written two ways - but an operation that alternates between raising a
// SuryError and letting a foreign one through is not deterministic, whatever
// the messages read. A value still goes through `sameOutcome`, which is what
// compares a Blob by its bytes rather than by identity.
const sameRun = async (a: Ref, b: Ref): Promise<boolean> =>
  "message" in a || "message" in b
    ? "message" in a && "message" in b && a.message === b.message
    : sameOutcome(a, b);

// One run of a built operation, reduced to the same two shapes an example
// records. `await` on a sync result is a no-op, so both kinds share the path.
const runToRef = async (fn: (input: any) => any, data: unknown): Promise<Ref> => {
  try {
    return { value: await fn(data) };
  } catch (e) {
    return { message: describeExampleThrow(e) };
  }
};

// The matrix and the `vs` equivalent both run on values a spec chose, which
// includes ones the golden serializer can't write down (a Blob's bytes are only
// readable asynchronously). Neither a failure message nor a recorded answer is
// worth aborting a check over, so a value it can't render is described by its
// type instead.
const valueToCodeSafe = (v: unknown): string => {
  try {
    return valueToCode(v);
  } catch {
    return Object.prototype.toString.call(v);
  }
};

const representable = (v: unknown): boolean => {
  try {
    valueToCode(v);
    return true;
  } catch {
    return false;
  }
};

// The schema's own comparator for the side an operation lands on, asked the
// questions the harness already answers for itself. Every `sameValue` the
// checks below run is a pair of values on one side of one schema, which is
// exactly what `isEqual*` claims to decide - so running both and requiring
// them to agree turns the whole spec corpus into the comparator's test suite,
// with an independently written oracle on the other side of every answer.
//
// Which side follows the direction: parse and decode produce the Output side,
// encode the Input side. `assert` and `is` produce a verdict rather than a
// value on either, so there is nothing to compare.
type SideEquality = {
  name: string;
  // `undefined` for a pair the comparator is not answerable for; a string when
  // it threw, which is a finding rather than an answer.
  answer: (a: unknown, b: unknown) => boolean | string | undefined;
};

const sideEquality = (schema: any, opName: OpName): SideEquality | undefined => {
  if (opName === "assert" || opName === "is") return undefined;
  const encoding = opName === "encode";
  const usable = (v: unknown): boolean =>
    // Two Blobs hold the same bytes only asynchronously, so nothing a
    // synchronous comparator can read tells them apart from identity, while
    // `sameBlob` reads the bytes. A deliberate difference, not a finding.
    !holdsBlob(v) &&
    // A value the harness can't write down has no golden to hold it, and the
    // spec already fails saying so. A second report would name the wrong thing.
    representable(v) &&
    (() => {
      // The comparator assumes what it is handed conforms, and an example is
      // free not to: a JSON Schema converted with a `$ref` it can't resolve
      // parses to a value its own Output side rejects. Asking the validator
      // first is what keeps that spec's report about the conversion.
      try {
        return encoding ? S.isInput(schema, v) : S.isOutput(schema, v);
      } catch {
        return false;
      }
    })();
  return {
    name: encoding ? "isEqualInput" : "isEqualOutput",
    answer: (a, b) => {
      if (!usable(a) || !usable(b)) return undefined;
      try {
        // Wrapped, not bare: each is three overloads, and a union of those has
        // no signature TypeScript will call.
        return encoding ? S.isEqualInput(schema, a, b) : S.isEqualOutput(schema, a, b);
      } catch (e) {
        return `threw ${JSON.stringify((e as Error).message)}`;
      }
    },
  };
};

const OUTCOME_FORMS = {
  parse: {
    OrThrow: S.parseOrThrow,
    AsResult: S.parseAsResult,
    AsPromiseOrReject: S.parseAsPromiseOrReject,
    AsResultPromise: S.parseAsResultPromise,
    AsPromisableResult: S.parseAsPromisableResult,
  },
  decode: {
    OrThrow: S.decodeOrThrow,
    AsResult: S.decodeAsResult,
    AsPromiseOrReject: S.decodeAsPromiseOrReject,
    AsResultPromise: S.decodeAsResultPromise,
    AsPromisableResult: S.decodeAsPromisableResult,
  },
  encode: {
    OrThrow: S.encodeOrThrow,
    AsResult: S.encodeAsResult,
    AsPromiseOrReject: S.encodeAsPromiseOrReject,
    AsResultPromise: S.encodeAsResultPromise,
    AsPromisableResult: S.encodeAsPromisableResult,
  },
  assert: {
    OrThrow: S.assertInputOrThrow,
    AsPromiseOrReject: S.assertInputAsPromiseOrReject,
  },
  is: {
    Sync: S.isInput,
    AsPromise: S.isInputAsPromise,
  },
} as const satisfies Record<OpName, Record<string, unknown>>;

// The golden builders: the throwing outcome of each verb, and - for a schema
// carrying an async transform or refine, which the sync builders reject at
// operation creation and which wraps a sync direction in `Promise.resolve(...)`,
// so which builder an op uses is part of its codegen, hence a declared
// `isAsync` checked against the schema - the rejecting one.
const OP_BUILDER: Record<OpName, (schema: any) => (input: any) => any> = {
  parse: OUTCOME_FORMS.parse.OrThrow,
  decode: OUTCOME_FORMS.decode.OrThrow,
  encode: OUTCOME_FORMS.encode.OrThrow,
  assert: OUTCOME_FORMS.assert.OrThrow,
  is: OUTCOME_FORMS.is.Sync,
};
const ASYNC_OP_BUILDER: Record<OpName, (schema: any) => (input: any) => Promise<any>> = {
  parse: OUTCOME_FORMS.parse.AsPromiseOrReject,
  decode: OUTCOME_FORMS.decode.AsPromiseOrReject,
  encode: OUTCOME_FORMS.encode.AsPromiseOrReject,
  assert: OUTCOME_FORMS.assert.AsPromiseOrReject,
  is: OUTCOME_FORMS.is.AsPromise,
};

// What a Result carries: a failure OF THE VALUE it was handed (index.d.ts,
// `DataError`) - a foreign exception from user code included, wrapped as
// `invalid_conversion`. Only a `DefectError` throws out of every outcome: it
// describes the schema and fails for every input.
const DATA_CODES = new Set(["invalid_input", "unrecognized_key", "invalid_conversion"]);

// Whether an outcome answers a failure with a value instead of an exception -
// which is also whether a throw out of it is itself a finding.
const RESULT_SHAPED = new Set(["AsResult", "AsResultPromise", "AsPromisableResult"]);
// An async direction compiles only through the outcomes that carry the async
// flag; the other two are rejected at operation creation, which
// `asyncViolations` already covers.
const ASYNC_OUTCOMES = new Set(["AsPromiseOrReject", "AsResultPromise", "AsPromisableResult", "AsPromise"]);

const CALL_FORMS = [
  ["op(schema)(data)", (op: any, schema: any, data: unknown) => op(schema)(data)],
  ["op(schema, data)", (op: any, schema: any, data: unknown) => op(schema, data)],
  ["op(data, schema)", (op: any, schema: any, data: unknown) => op(data, schema)],
] as const;

// The checks, which answer only "did it pass". Run against `parse` alone:
// `decode` and `encode` TRUST their input (no type validation), while
// `assert`/`is`/`make` always validate, so a decode example that is
// deliberately ill-typed diverges by design. Parse is where both sides do the
// same work, and where a divergence is a bug.
// Wrapped, not bare: each check is nine overloads, and a union of those has no
// signature TypeScript will call.
const CHECK_FORMS = {
  sync: {
    assertInputOrThrow: (schema: any, data: unknown) => S.assertInputOrThrow(schema, data),
    isInput: (schema: any, data: unknown) => S.isInput(schema, data),
    makeInputOrThrow: (schema: any, data: unknown) => S.makeInputOrThrow(schema, data),
  },
  async: {
    assertInputAsPromiseOrReject: (schema: any, data: unknown) =>
      S.assertInputAsPromiseOrReject(schema, data),
    isInputAsPromise: (schema: any, data: unknown) => S.isInputAsPromise(schema, data),
    makeInputAsPromiseOrReject: (schema: any, data: unknown) =>
      S.makeInputAsPromiseOrReject(schema, data),
  },
} as const;

// What each check answers for this value. They are meant to agree with each
// other; a disagreement has no spelling in the format and is reported instead.
const checkVerdicts = async (
  schema: any,
  nextData: () => unknown,
  isAsync: boolean,
): Promise<
  { name: string; passed: boolean; detail: string; message?: string; answer?: unknown; given?: unknown }[]
> =>
  Promise.all(
    Object.entries(CHECK_FORMS[isAsync ? "async" : "sync"]).map(async ([name, run]) => {
      const given = nextData();
      try {
        const answer = await run(schema, given);
        // `is*` answers a boolean; the others answer by not throwing.
        return {
          name,
          passed: name.startsWith("is") ? answer === true : true,
          detail: "",
          answer,
          given,
        };
      } catch (e) {
        return { name, passed: false, detail: `: ${(e as Error).message}`, message: (e as Error).message };
      }
    }),
  );

// ---- the JSON Schema a spec publishes, asked about that spec's own values ---

// `detail` is prose for an error message; `answer` is what gets recorded - the
// verifier's own words, or `true` where accepting is all it can say.
type Answer = true | string;
type Verdict = { passed: boolean; detail: string; answer: Answer };
type JsonSchemaDocument = { side: "input" | "output"; validate: (v: unknown) => string | undefined };

// The two documents a spec records in full. A side whose golden is a recorded
// conversion error rather than a document (`Expected JSON, received bigint`)
// simply has nothing to ask; one that IS a document but which Ajv cannot
// compile is a finding about the document itself, reported once for the spec
// rather than against whichever example happened to reach it first.
const jsonSchemaSides = (spec: Spec): { sides: JsonSchemaDocument[]; errs: string[] } => {
  const sides: JsonSchemaDocument[] = [];
  const errs: string[] = [];
  for (const side of ["input", "output"] as const) {
    const source = spec.jsonSchema?.[side];
    if (typeof source !== "string") continue;
    let doc: unknown;
    try {
      doc = evalSchema(source);
    } catch {
      continue;
    }
    if (typeof doc !== "object" || doc === null) continue;
    const validator = documentValidator(source, doc);
    if ("error" in validator) {
      errs.push(
        `jsonSchema.${side}: is a document no validator can compile: ${validator.error} - ` +
          "a consumer handed it gets this error instead of a verdict",
      );
      continue;
    }
    sides.push({ side, validate: validator.validate });
  }
  return { sides, errs };
};

// What the recorded documents say about one example, or `undefined` when there
// is nothing to ask - a rejected example, or a value outside JSON.
//
// ONLY an accepted example is asked. JSON Schema is allowed to describe a wider
// set than the parser does: a refinement, a coercion rule and a bound in units
// JSON Schema cannot name all narrow the parser without narrowing the document,
// so a rejected input validating is by design and carries no information. The
// other direction is a promise the library makes - a document that turns away
// data the parser itself accepts is one a consumer would use to reject good
// input - so that is the direction gated.
const jsonSchemaVerdict = (sides: JsonSchemaDocument[], ex: Example): Verdict | undefined => {
  if (!("output" in ex)) return undefined;
  const failures: string[] = [];
  const reasons: string[] = [];
  let asked = 0;
  for (const { side, validate } of sides) {
    let value: unknown;
    try {
      value = evalSchema(side === "input" ? ex.input : ex.output);
    } catch {
      continue;
    }
    if (!isJsonValue(value)) continue;
    asked++;
    const reason = validate(value);
    if (reason !== undefined) {
      failures.push(`jsonSchema.${side} rejects it (${reason})`);
      reasons.push(`${side}: ${reason}`);
    }
  }
  if (asked === 0) return undefined;
  return {
    passed: failures.length === 0,
    detail: failures.join("; "),
    answer: failures.length === 0 ? true : reasons.join("; "),
  };
};

// ---- the `vs` equivalent, asked about that spec's own values ---------------

// Built once per source: checkZodExamples and `--write`'s refresh both want the
// same schema, and a spec's examples all share it.
const zodSchemas = new Map<string, { schema: any } | { error: string }>();
const zodSchemaFor = (source: string): { schema: any } | { error: string } => {
  const hit = zodSchemas.get(source);
  if (hit) return hit;
  let built: { schema: any } | { error: string };
  try {
    built = { schema: new Function("z", `return ${stripTypes(source)};`)(z) };
  } catch (e) {
    built = { error: (e as Error).message };
  }
  zodSchemas.set(source, built);
  return built;
};

// Read through Standard Schema (`~standard`) rather than Zod's own `safeParse`,
// for the reason deriveVsTypeInfo reads the types that way: the `vs` dimension
// is about a cross-library equivalent, and every vendor it could name exposes
// this one interface. `validate` may answer a promise (an async refinement), so
// both kinds go through one await.
const zodVerdict = async (schema: any, ex: Example): Promise<Verdict | undefined> => {
  let value: unknown;
  try {
    value = valueEvaluator(ex.input)();
  } catch {
    return undefined;
  }
  try {
    const result = await schema["~standard"].validate(value);
    // Accepting, the equivalent has a VALUE to show, and it is the interesting
    // half: two libraries can both accept `""` and hand back different things.
    if (result.issues === undefined)
      return { passed: true, detail: "", answer: valueToCodeSafe(result.value) };
    const message = result.issues[0]?.message ?? "rejected";
    return { passed: false, detail: message, answer: message };
  } catch (e) {
    // A vendor may throw rather than report - still a rejection, and the
    // wording is the only thing that says why.
    const message = describeExampleThrow(e);
    return { passed: false, detail: message, answer: message };
  }
};

// What the checks answer, in the form a `divergence` records: `true` when they
// accept, else the words they reject with. `isInput*` answers a bare `false`,
// so the message comes from whichever sibling threw one.
// A recorded answer, as it reads back in a message: `true` bare, a message
// quoted, so "records true" and "records \"true\"" are not the same sentence.
const answerToCode = (answer: Answer): string =>
  answer === true ? "true" : JSON.stringify(answer);

const checksAnswer = (
  verdicts: { passed: boolean; message?: string }[],
): Answer => (verdicts[0]!.passed ? true : (verdicts.find((v) => v.message)?.message ?? "rejected"));

// `--write`'s half: a `divergence` that is already there keeps its verdicts
// fresh, the way `creationError` does. `reason` is never touched, and adding or
// removing the field stays the author's call - that is the moment a divergence
// appears or goes away, and it should be read by a person, not written by a
// tool.
//
// `parse` only: decode and encode trust their input while every verifier
// validates, so a divergence on one of those directions is a misuse the checks
// report rather than a value to refresh.
const refreshDivergence = async (
  opName: OpName,
  isAsync: boolean,
  schema: any,
  ex: Example,
  spec: Spec,
): Promise<Pick<Example, "divergence">> => {
  const was = ex.divergence;
  if (was === undefined) return {};
  const out: NonNullable<Example["divergence"]> = { reason: was.reason };
  if (was.check !== undefined) {
    out.check =
      opName === "parse"
        ? checksAnswer(await checkVerdicts(schema, valueEvaluator(ex.input), isAsync))
        : was.check;
  }
  if (was.ajv !== undefined) {
    const verdict = opName === "parse" ? jsonSchemaVerdict(jsonSchemaSides(spec).sides, ex) : undefined;
    out.ajv = verdict === undefined ? was.ajv : verdict.answer;
  }
  if (was.zod !== undefined) {
    const vs = spec.vs?.zod;
    const source = vs === undefined || isSkip(vs) ? undefined : isZodOverwrite(vs) ? vs.schema : vs;
    const built = opName === "parse" && source !== undefined ? zodSchemaFor(source) : undefined;
    const verdict = built && "schema" in built ? await zodVerdict(built.schema, ex) : undefined;
    // `zod` holds a value or a message, never `true` - the equivalent always
    // has words of its own, so there is nothing for a bare `true` to mean.
    out.zod = verdict === undefined ? was.zod : String(verdict.answer);
  }
  return { divergence: out };
};

export const checkOperationMatrix = async (spec: Spec, schema: any): Promise<string[]> => {
  const errs: string[] = [];
  for (const opName of OP_ORDER) {
    const op = spec.operations?.[opName];
    if (op == null || typeof op === "string" || isCreationError(op) || isSkip(op)) continue;
    const isAsync = op.isAsync === true;
    const eq = sideEquality(schema, opName);
    for (const [exName, ex] of Object.entries(op.examples)) {
      if (isSkip(ex)) continue;
      const where = `operations.${opName}.examples.${exName}`;
      let nextData: () => unknown;
      let given: unknown;
      let golden: Ref;
      try {
        nextData = valueEvaluator(ex.input);
        given = nextData();
        golden =
          "errorConstructor" in ex
            ? { ctor: ex.errorConstructor }
            : "error" in ex
              ? { message: ex.error }
              : {
                  value: ex.output === ex.input ? given : evalSchema(ex.output),
                };
      } catch (e) {
        if ("errorConstructor" in ex) {
          if ((e as Error).constructor.name !== ex.errorConstructor)
            errs.push(
              `${where}: input threw ${(e as Error).constructor.name}, expected ${ex.errorConstructor}`,
            );
          continue;
        }
        continue; // an unevaluatable golden is reported by the checks above
      }

      // `op(data, schema)` reads two schemas as a chain - the one call form a
      // Sury schema in the data slot can't take (see index.d.ts). Documented,
      // not a finding.
      const skipDataFirst = isUsableSchema(given);

      for (const [outcome, factory] of Object.entries(OUTCOME_FORMS[opName])) {
        // A sync direction runs all five; an async one only the outcomes that
        // carry the async flag - the rest are rejected at operation creation,
        // which `asyncViolations` already covers.
        if (isAsync && !ASYNC_OUTCOMES.has(outcome)) continue;
        // Every spelling answers the golden: the async outcomes of a sync
        // direction compile the same body and lift it into a promise.
        const expected = golden;
        for (const [form, call] of CALL_FORMS) {
          if (skipDataFirst && form === "op(data, schema)") continue;
          const spelling = `${opName}${outcome} as ${form}`;
          let actual: Ref;
          try {
            const raw = await call(factory, schema, given);
            if (RESULT_SHAPED.has(outcome)) {
              if (raw === null || typeof raw !== "object" || typeof (raw as any).success !== "boolean") {
                errs.push(`${where}: ${spelling} answered ${valueToCodeSafe(raw)}, not a Result`);
                continue;
              }
              const r = raw as { success: boolean; value: unknown; error?: { message: string } };
              actual = r.success ? { value: r.value } : { message: r.error!.message };
            } else {
              actual = { value: raw };
            }
          } catch (e) {
            // Only a defect (the schema is wired wrong, so every input fails)
            // throws out of a Result outcome; a data failure - a foreign
            // exception from user code included - is its return value.
            if (RESULT_SHAPED.has(outcome) && DATA_CODES.has((e as { code?: string }).code!)) {
              errs.push(
                `${where}: ${spelling} threw ${JSON.stringify((e as Error).message)} - a Result ` +
                  "outcome reports a failure of the value in its return type, and only a defect throws",
              );
              continue;
            }
            actual =
              "ctor" in expected
                ? { ctor: (e as Error).constructor.name }
                : { message: describeExampleThrow(e) };
          }
          if (!(await sameOutcome(expected, actual))) {
            errs.push(
              `${where}: ${spelling} ${describeRef(actual)}, but the golden ${describeRef(expected)}`,
            );
            continue;
          }
          // The same two values, put to the schema's own comparator. The pair
          // is what makes this worth the call: the golden side was evaluated
          // from the source an author wrote, the actual side was built by the
          // operation, so agreeing here is reflexivity across every recorded
          // example rather than across one code path run twice.
          //
          // Only the strict direction is reported. A comparator that answers
          // `true` for two values `sameValue` calls different is caught by the
          // cross-example pools in `checkEquality`, and reporting it here as
          // well would put two lines under one problem.
          if ("value" in expected && "value" in actual) {
            const answered = eq?.answer(expected.value, actual.value);
            if (answered !== undefined && answered !== true)
              errs.push(
                `${where}: ${spelling} returned a value the golden matches, but S.${eq!.name} ` +
                  `answered ${valueToCodeSafe(answered)} for the two - the comparator is reading ` +
                  "its own side wrong",
              );
          }
        }
      }
      // Whether the checks agree that this value passes. Only that - `assert`
      // and `is` build no output, and `make` hands the value back rather than a
      // decoded clone, so there is no output to compare. `parse` only: `decode`
      // and `encode` TRUST their input, while the checks always validate, so a
      // deliberately ill-typed decode example diverges by design.
      if (opName !== "parse") continue;
      const verdicts = await checkVerdicts(schema, nextData, isAsync);
      const disagreeing = verdicts.filter((v) => v.passed !== verdicts[0]!.passed);
      if (disagreeing.length) {
        errs.push(
          `${where}: the checks disagree with each other - ${verdicts
            .map((v) => `S.${v.name} ${v.passed ? "passed" : `failed${v.detail}`}`)
            .join(", ")}`,
        );
        continue;
      }
      const passed = verdicts[0]!.passed;
      const parseFailed = "error" in ex || "errorConstructor" in ex;
      const recorded = ex.divergence?.check;
      if (passed === !parseFailed) {
        if (recorded !== undefined)
          errs.push(`${where}: divergence.check agrees with parse - remove it`);
      } else {
        const answer = checksAnswer(verdicts);
        if (recorded === undefined)
          errs.push(
            `${where}: the checks ${passed ? "passed" : `failed${verdicts[0]!.detail}`}, but parse ` +
              `${parseFailed ? `failed with ${"error" in ex ? JSON.stringify(ex.error) : ex.errorConstructor}` : "succeeded"} - ` +
              `record it with \`divergence: { reason: ..., check: ${answerToCode(answer)} }\``,
          );
        else if (recorded !== answer)
          errs.push(
            `${where}: divergence.check records ${answerToCode(recorded)} but the checks answer ` +
              answerToCode(answer),
          );
        if (recorded !== answer) continue;
      }
      // `make` hands the value back rather than decoding it. `Object.is`, not
      // `!==`: a spec whose example is NaN is exactly the case that matters.
      const made = verdicts.find((v) => v.name.startsWith("make"));
      if (made?.passed && !Object.is(await made.answer, made.given))
        errs.push(
          `${where}: S.${made.name} returned a different value than the one it was given - ` +
            "make validates and hands the value back, it does not decode it",
        );
    }
  }
  return errs;
};

// ---- the examples themselves ----------------------------------------------

// What a `divergence` has to be before any verifier is asked about it. Here
// rather than in the checks that read its verdicts, because those run only once
// the goldens are fresh, and a field this malformed should be reported as
// itself.
const divergenceShapeViolations = (where: string, opName: OpName, ex: Example): string[] => {
  const d = ex.divergence;
  if (d === undefined) return [];
  const errs: string[] = [];
  if (opName !== "parse")
    errs.push(
      `${where}: divergence is \`parse\` only - decode and encode trust their input, so a verifier ` +
        "disagreeing with them is by design",
    );
  if (d.reason.trim() === "")
    errs.push(
      `${where}: divergence.reason is empty - the verdict beside it says which verifier disagrees, ` +
        "and only the reason says why anyone should accept that",
    );
  if (d.check === undefined && d.ajv === undefined && d.zod === undefined)
    errs.push(
      `${where}: divergence names no verifier - add the one that disagrees (\`check\`, \`ajv\` or ` +
        "`zod`), or remove the field",
    );
  return errs;
};


// What every other check assumes and none of them state: an example's input is
// source the harness can run, names a case no sibling already covers, and
// produces the same answer twice. Runs BEFORE the goldens are recomputed, so
// each of these reads as itself rather than as a staleness diff or as a
// "goldens could not be computed" with the whole spec's worth of context lost.
export const checkExamples = async (
  spec: Spec,
  schema: any,
  ops?: Record<OpName, BuiltOp>,
): Promise<string[]> => {
  const errs: string[] = [];
  for (const opName of OP_ORDER) {
    const op = spec.operations?.[opName];
    if (op == null || typeof op === "string" || isCreationError(op) || isSkip(op)) continue;
    const built = ops?.[opName] ?? buildOp(opName, schema);
    const eq = sideEquality(schema, opName);
    const byInput = new Map<string, string>();
    for (const [exName, ex] of Object.entries(op.examples)) {
      const where = `operations.${opName}.examples.${exName}`;
      errs.push(...divergenceShapeViolations(where, opName, ex));

      let nextData: () => unknown;
      try {
        nextData = valueEvaluator(ex.input);
        nextData();
      } catch (e) {
        errs.push(
          `${where}: input did not evaluate: ${(e as Error).message} - ` +
            "an input the harness cannot run would be recorded as an `error` golden that pins the " +
            "typo rather than the schema, and passes forever while running nothing",
        );
        continue;
      }

      // Byte-identical source, so the two run the same value through the same
      // operation: one of them is a name with no case behind it.
      const twin = byInput.get(ex.input);
      if (twin !== undefined)
        errs.push(
          `${where}: input is identical to \`${twin}\`'s - two names for one case, so one of them ` +
            "covers nothing (give it a different input, or delete it)",
        );
      else byInput.set(ex.input, exName);

      // Run twice on two fresh values. A golden derived from the clock, from
      // `Math.random`, or from iteration order that is not stable would be
      // rewritten by every `--write` and reported as a staleness diff on every
      // check, naming the schema instead of the reason.
      if (!("fn" in built)) continue;
      const first = await runToRef(built.fn, nextData());
      const second = await runToRef(built.fn, nextData());
      const agreed = await sameRun(first, second);
      if (!agreed)
        errs.push(
          `${where}: is not deterministic - the same input ${describeRef(first)} once and ` +
            `${describeRef(second)} the next time, so no golden can hold it`,
        );
      // The schema's own comparator, asked what `sameRun` just answered. This
      // is the pair it is built for: two values the operation PRODUCED, so both
      // conform to the side it compares, and two distinct objects, so the
      // `a === b` it opens with cannot answer for free the way comparing one
      // value to itself would.
      else if ("value" in first && "value" in second) {
        const answered = eq?.answer(first.value, second.value);
        if (answered !== undefined && answered !== true)
          errs.push(
            `${where}: S.${eq!.name} answered ${valueToCodeSafe(answered)} for two runs of this ` +
              `operation on the same input, which returned equal values ` +
              `(${valueToCodeSafe(first.value)}) - the comparator is reading its own side wrong`,
          );
      }
    }
  }
  return errs;
};

// ---- the recorded JSON Schema, against the recorded examples ---------------

// A spec pins the JSON Schema it publishes and the values its parser accepts,
// and never puts the two side by side - so a document that contradicts the
// parser is two green goldens. This asks the document, with a real validator
// (see jsonSchemaValidator.ts), about the values the same file already holds.
export const checkJsonSchemaExamples = (spec: Spec): string[] => {
  const errs: string[] = [];

  const op = spec.operations?.parse;
  if (op == null || typeof op === "string" || isCreationError(op) || isSkip(op)) return errs;
  const { sides, errs: compileErrs } = jsonSchemaSides(spec);
  errs.push(...compileErrs);

  for (const [exName, ex] of Object.entries(op.examples)) {
    const where = `operations.parse.examples.${exName}`;
    const verdict = jsonSchemaVerdict(sides, ex);
    const recorded = ex.divergence?.ajv;
    if (verdict === undefined) {
      if (recorded !== undefined)
        errs.push(
          `${where}: divergence.ajv records a disagreement nothing can check - only an accepted ` +
            "example whose values are JSON is asked, so remove it",
        );
      continue;
    }
    if (verdict.passed) {
      if (recorded !== undefined) errs.push(`${where}: divergence.ajv agrees with parse - remove it`);
      continue;
    }
    if (recorded === undefined)
      errs.push(
        `${where}: parse accepts this value but ${verdict.detail} - a consumer validating with the ` +
          "document this spec publishes would turn away input the library itself takes. Fix the " +
          `document, or record it with \`divergence: { reason: ..., ajv: ${answerToCode(verdict.answer)} }\` ` +
          "and a `FIXME:` if it is a bug",
      );
    else if (recorded !== verdict.answer)
      errs.push(
        `${where}: divergence.ajv records ${answerToCode(recorded)} but the documents answer ` +
          answerToCode(verdict.answer),
      );
  }
  return errs;
};

// ---- the `vs` equivalent, against the recorded examples --------------------

// `vs.zod` pins what the other library's TYPES say and never runs it, so two
// libraries that agree on `string` and disagree on which strings are green
// either way. Accept-or-reject only: the value each produces, the wording of a
// rejection and which of several failures is reported are Sury's own.
export const checkZodExamples = async (spec: Spec): Promise<string[]> => {
  const errs: string[] = [];

  const op = spec.operations?.parse;
  if (op == null || typeof op === "string" || isCreationError(op) || isSkip(op)) return errs;
  const vs = spec.vs?.zod;
  const source = vs === undefined || isSkip(vs) ? undefined : isZodOverwrite(vs) ? vs.schema : vs;

  if (source === undefined) {
    for (const [exName, ex] of Object.entries(op.examples))
      if (ex.divergence?.zod !== undefined)
        errs.push(
          `operations.parse.examples.${exName}: divergence.zod records what an equivalent answers, ` +
            "but `vs.zod` is skipped - there is nothing to disagree with, so remove it",
        );
    return errs;
  }

  const built = zodSchemaFor(source);
  if ("error" in built) {
    // Distinct from checkVs's "did not typecheck": this one built a TS program,
    // that one ran the source. A spec can pass the first and fail this.
    errs.push(`vs.zod: did not evaluate: ${built.error}`);
    return errs;
  }

  for (const [exName, ex] of Object.entries(op.examples)) {
    const where = `operations.parse.examples.${exName}`;
    const verdict = await zodVerdict(built.schema, ex);
    if (verdict === undefined) continue;
    // `"output" in ex`, not the absence of `error`: an `errorConstructor`
    // example carries neither, and reading it as a success would compare zod's
    // rejection against a pass this spec never claimed.
    const suryPassed = "output" in ex;
    const recorded = ex.divergence?.zod;
    if (verdict.passed === suryPassed) {
      if (recorded !== undefined) errs.push(`${where}: divergence.zod agrees with parse - remove it`);
      continue;
    }
    const answered = String(verdict.answer);
    const reads = verdict.passed
      ? `accepts it, returning ${answered}`
      : `rejects it${verdict.detail ? ` (${verdict.detail})` : ""}`;
    if (recorded === undefined)
      errs.push(
        `${where}: parse ${suryPassed ? "accepts" : "rejects"} this value and the \`vs.zod\` ` +
          `equivalent ${reads} - if the two libraries genuinely read it differently, record it ` +
          `with \`divergence: { reason: ..., zod: ${JSON.stringify(answered)} }\`; if not, the ` +
          "equivalent is the wrong one",
      );
    else if (recorded !== answered)
      errs.push(
        `${where}: divergence.zod records ${JSON.stringify(recorded)} but the \`vs.zod\` ` +
          `equivalent answers ${JSON.stringify(answered)}`,
      );
  }
  return errs;
};

// Cross-checks a spec's `vs` equivalent against its recorded inferred types,
// ---- equality --------------------------------------------------------------
//
// The `isEqual` golden is one line of source; whether it MEANS anything is
// whether the values a spec already writes down compare the way they should.
// So every one of them is run through the compiled comparator against a
// freshly built copy of itself - which is the reflexivity test worth running,
// since a fresh copy takes the structural path instead of the `a===b`
// short-circuit the identical reference would - and against every other value
// of the same side.
//
// The expected answer comes from `structurallyEqual`, an oracle written without
// reference to the schema: when the two disagree, one of them is wrong, and the
// report names the two examples whose values show it.

// SameValueZero at the leaves, so it agrees with the emit on NaN (equal to
// itself) and on -0 (equal to 0) - `isDeepStrictEqual` splits both the other
// way. An absent key and an `undefined` one are one value: a schema reads an
// optional property the same either way.
const structurallyEqual = (a: unknown, b: unknown): boolean => {
  if (a === b) return true;
  if (a !== a) return b !== b;
  if (!a || !b || typeof a !== "object" || typeof b !== "object") return false;
  // The prototype, not `.constructor` - a data key named `constructor` shadows
  // it, and `object-reserved-names` is a spec precisely because values like
  // that exist.
  const proto = Object.getPrototypeOf(a);
  if (proto !== Object.getPrototypeOf(b)) return false;
  const ao = a as Record<string, unknown>;
  const bo = b as Record<string, unknown>;
  if (Array.isArray(a) || (ArrayBuffer.isView(a) && typeof (a as unknown as ArrayLike<unknown>).length === "number")) {
    const n = (a as unknown as ArrayLike<unknown>).length;
    if (n !== (b as unknown as ArrayLike<unknown>).length) return false;
    for (let i = 0; i < n; i++) if (!structurallyEqual(ao[i], bo[i])) return false;
    return true;
  }
  // The built-ins whose value is their content rather than their identity.
  if (proto === Date.prototype) return +(a as Date) === +(b as Date);
  if (proto === URL.prototype) return `${a}` === `${b}`;
  // A Set by its members, which it already holds by SameValueZero; FormData
  // and URLSearchParams by their entries in order, since a name handed out
  // twice is two of them.
  if (proto === Set.prototype) {
    const as = a as Set<unknown>;
    const bs = b as Set<unknown>;
    return as.size === bs.size && [...as].every((v) => bs.has(v));
  }
  if (proto === FormData.prototype || proto === URLSearchParams.prototype) {
    const be = [...(b as FormData | URLSearchParams)];
    const ae = [...(a as FormData | URLSearchParams)];
    return (
      ae.length === be.length && ae.every((e, i) => e[0] === be[i]![0] && e[1] === be[i]![1])
    );
  }
  // Anything else carrying an identity of its own - a Blob, a class instance -
  // has already failed `===`, and reading its fields would invent a rule the
  // compiled comparison doesn't have either.
  if (proto !== null && proto !== Object.prototype) return false;
  // Key sets, not key counts, and not a union of both sides' keys either: a name
  // one side lacks reads `undefined` on both, so `{a: undefined}` and
  // `{b: undefined}` would pass every value comparison a lenient walk makes.
  // For a dict or an unknown position the keys ARE the content, which is the
  // rule `dictFn` and `deepEqual` follow. A declared-optional property is the
  // one place absent and `undefined` are one value, and no pair here turns on
  // it: these are values an operation produced, and an operation writes the
  // property either way.
  const keys = Object.keys(ao);
  if (keys.length !== Object.keys(bo).length) return false;
  for (const key of keys) if (!(key in bo) || !structurallyEqual(ao[key], bo[key])) return false;
  return true;
};

type EqValue = { where: string; make: () => unknown };

// Only values the SCHEMA produced, never ones an author wrote: a parse input
// may carry a key the schema strips, or a spelling it coerces, so two of them
// can be one value to the schema and two to a structural oracle - a
// disagreement about the input, not about equality. An operation's output is
// already canonical for the side it lands on, which is what makes the oracle's
// blind walk the right answer for it.
//
// Which side that is follows the direction: parse and decode produce the
// Output side, encode the Input side.
const equalityPools = (spec: Spec): Record<"input" | "output", EqValue[]> => {
  const pools: Record<"input" | "output", EqValue[]> = { input: [], output: [] };
  const seen: Record<"input" | "output", Set<string>> = { input: new Set(), output: new Set() };
  for (const opName of OP_ORDER) {
    const op = spec.operations?.[opName];
    if (op == null || typeof op === "string" || isCreationError(op) || isSkip(op)) continue;
    // An async direction answers a promise, so its recorded output is not the
    // value; the sync directions of the same schema carry the same values.
    if (op.isAsync === true) continue;
    const side = opName === "encode" ? "input" : "output";
    for (const [exName, ex] of Object.entries(op.examples)) {
      // A failed example never produced a value of either side.
      if (isSkip(ex) || !("output" in ex)) continue;
      if (seen[side].has(ex.output)) continue;
      seen[side].add(ex.output);
      let make: () => unknown;
      try {
        make = valueEvaluator(ex.output);
        make();
      } catch {
        continue; // an unevaluatable golden is reported by the checks above
      }
      pools[side].push({ where: `operations.${opName}.examples.${exName}.output`, make });
    }
  }
  return pools;
};

// Pairs are quadratic in the pool, and a spec with dozens of examples of one
// direction is measuring the same emit over and over by the tenth.
const EQ_POOL_CAP = 12;

export const checkEquality = (spec: Spec, schema: any): string[] => {
  const errs: string[] = [];
  if (isSkip(spec.isEqual)) return errs;
  const pools = equalityPools(spec);
  for (const side of ["input", "output"] as const) {
    const values = pools[side].slice(0, EQ_POOL_CAP);
    if (!values.length) continue;
    const name = side === "input" ? "isEqualInput" : "isEqualOutput";
    const direct = (side === "input" ? S.isEqualInput : S.isEqualOutput) as (
      ...args: unknown[]
    ) => boolean;
    // The comparison assumes both values match the schema, so a value that
    // doesn't is outside what it answers for - an excess key on a strict
    // object, a required field an encode example deliberately drops. Those are
    // findings about the OPERATION, which its own golden already records.
    // Wrapped, not bare: each guard is nine overloads, and a union of those has
    // no signature TypeScript will call.
    const conforms =
      side === "input"
        ? (s: any) => S.isInput(s) as unknown as (v: unknown) => boolean
        : (s: any) => S.isOutput(s) as unknown as (v: unknown) => boolean;
    let inSide: (v: unknown) => boolean;
    try {
      inSide = conforms(schema);
    } catch {
      inSide = () => true; // no validator to build: compare what we have
    }
    let compiled: (a: unknown, b: unknown) => boolean;
    try {
      compiled = direct(schema) as unknown as (a: unknown, b: unknown) => boolean;
    } catch (e) {
      errs.push(`isEqual: S.${name}(schema) threw ${JSON.stringify((e as Error).message)}`);
      continue;
    }
    const conforming = values.filter((v) => {
      try {
        return inSide(v.make());
      } catch {
        return false;
      }
    });
    for (let i = 0; i < conforming.length; i++) {
      for (let j = i; j < conforming.length; j++) {
        const left = conforming[i]!;
        const right = conforming[j]!;
        const a = left.make();
        const b = right.make();
        const want = structurallyEqual(a, b);
        const pair =
          i === j ? `${left.where} against a fresh copy of itself` : `${left.where} vs ${right.where}`;
        // Every comparison here has to reach the structural path. The emit
        // opens with `a === b`, so handing it one object twice would answer
        // true without reading a single field, and the whole check would pass
        // on a comparator that does nothing. `make()` evaluates the source
        // afresh per side, which is what keeps them distinct; a primitive is
        // the one thing that legitimately is its own twin.
        if (a === b && a !== null && typeof a === "object") {
          errs.push(
            `isEqual: ${pair}: both sides are the same object, so \`a === b\` answers before the ` +
              "comparator reads anything - the pair has to be built twice",
          );
          continue;
        }
        // Both orders, because equality is symmetric, and every call shape,
        // because the three are one dispatch that has to agree with itself.
        const spellings: [string, () => unknown][] = [
          [`S.${name}(schema)(a, b)`, () => compiled(a, b)],
          [`S.${name}(schema)(b, a)`, () => compiled(b, a)],
          [`S.${name}(schema, a, b)`, () => direct(schema, a, b)],
        ];
        // `S.op(data, ..., schema)` reads a schema in the data slot as a chain -
        // the one call shape a Sury schema as a value can't take.
        if (!isUsableSchema(a))
          spellings.push([`S.${name}(a, b, schema)`, () => direct(a, b, schema)]);
        for (const [spelling, run] of spellings) {
          let got: unknown;
          try {
            got = run();
          } catch (e) {
            errs.push(`isEqual: ${pair}: ${spelling} threw ${JSON.stringify((e as Error).message)}`);
            continue;
          }
          if (got !== want)
            errs.push(
              `isEqual: ${pair}: ${spelling} answered ${valueToCodeSafe(got)}, expected ${want}` +
                ` - a=${valueToCodeSafe(a)}, b=${valueToCodeSafe(b)}`,
            );
        }
      }
    }
  }
  return errs;
};

// live like checkAliases (no golden of its own). Strict string equality -
// both sides printed with the same InTypeAlias formatting - so the author
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
    // actually match. If both sides are omitted, nothing diverges - the bare
    // string form (which asserts both equalities) is the right tool.
    const hasInput = vs.zod.input !== undefined;
    const hasOutput = vs.zod.output !== undefined;
    if (!hasInput && !hasOutput) {
      errs.push(
        "vs.zod: overwrite form records no divergence (input and output both omitted) - " +
          `use the bare \`zod: ${JSON.stringify(vs.zod.schema)}\` string form instead.`,
      );
      return errs;
    }
    for (const side of ["input", "output"] as const) {
      if (isSkip(spec.ts[side])) continue;
      const has = vs.zod[side] !== undefined;
      const z = info[side];
      const t = spec.ts[side];
      if (!has && z !== t)
        errs.push(
          `vs.zod: ${side} omitted (no divergence) but Zod infers ${JSON.stringify(z)} !== ts.${side} ` +
            `${JSON.stringify(t)} - add \`${side}\` to record the divergent type.`,
        );
      else if (has && z === t)
        errs.push(
          `vs.zod.${side} equals ts.${side} ${JSON.stringify(t)} - it matches Sury, so omit \`${side}\`.`,
        );
    }
    return errs;
  }

  for (const side of ["input", "output"] as const) {
    if (!isSkip(spec.ts[side]) && info[side] !== spec.ts[side])
      errs.push(`vs.zod: ${side} type ${JSON.stringify(info[side])} !== ts.${side} ${JSON.stringify(spec.ts[side])}`);
  }
  return errs;
};

// Never mutates a file or exits the process, so it's directly testable -
// cli.ts's cmdCheck and tests/spec_errors_test.ts both call this same
// function, so there's exactly one implementation of "what's wrong with this
// spec, and what should the author do about it."
//
// `knownFresh`, when passed, is the already-serialized result of a
// recomputeGoldens call the caller just performed (cli.ts's `--write` path,
// right after writing) - skips redoing that same esbuild+TS-introspection
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
  undeclaredAssignments(spec, errs);

  // Collected on its own because it decides whether anything below may RUN.
  // Every check past this point evaluates `ts.schema`, and the whole point of
  // the rule is that evaluating this one would reconfigure the library for
  // every other spec in the process.
  const globalErrs: string[] = [];
  lintGlobal(spec, globalErrs);
  errs.push(...globalErrs);
  // Before ANY of the work below, `serialize` included: canonicalizing an
  // example reformats its input by evaluating it (see reformatIfEvaluable), so
  // returning any later than this would already have run the call the rule
  // exists to refuse. Such a spec gets no canonical-form or golden report,
  // which is the point - it is not processed at all.
  if (globalErrs.length) return errs;

  // Collected before the canonical form is built (rather than dropped) so a
  // disallowed comment is reported as itself, not as a "not canonical" diff -
  // and so `--write` never silently deletes one.
  const comments = collectComments(raw);
  lintComments(comments, errs);

  const canon = serialize(spec, comments);
  if (raw !== canon)
    errs.push(
      `not canonical - run \`pnpm spec format ${id}\` (or \`pnpm spec check ${id} --write\`, which also refreshes goldens):\n${diffText(raw, canon)}`,
    );

  let schema: any;
  let evaluated = false;
  try {
    schema = evalSchema(spec.ts.schema);
    evaluated = true;
  } catch (e) {
    const message = (e as Error).message;
    if (spec.ts.constructionError !== undefined) {
      if (spec.ts.constructionError !== message)
        errs.push(`ts.constructionError differs:\n${diffText(spec.ts.constructionError, message)}`);
    } else {
      errs.push(
        `ts.schema did not evaluate: ${message} - if this panic is the contract, add ts.constructionError`,
      );
    }
  }
  if (evaluated && spec.ts.constructionError !== undefined) {
    errs.push(`ts.constructionError is set but ts.schema evaluated - omit constructionError`);
  }
  if (evaluated && !isUsableSchema(schema)) {
    errs.push(`ts.schema evaluated but isn't a Sury schema`);
  } else if (
    v.ok &&
    evaluated &&
    REQUIRED_OPS.every((op) => spec.operations?.[op] != null)
  ) {
    try {
      const compiled = buildOps(schema);
      const violations = identityViolations(schema, spec, compiled);
      for (const violation of violations) errs.push(violation);
      // Not part of `violations`: a wrong `isAsync` doesn't block `--write`
      // (which builder a direction uses is derived from the schema, so the
      // recomputed goldens are right either way) - only the marker needs the
      // author's hand.
      const asyncErrs = asyncViolations(schema, spec, compiled);
      errs.push(...asyncErrs);
      // Before the recompute. An input that does not run makes every golden
      // under it uncomputable, and one that answers differently each time makes
      // them a coin flip - so without this they surface as "goldens could not be
      // computed" and as a staleness diff naming the schema, neither of which
      // says what is actually wrong.
      const exampleErrs = await checkExamples(spec, schema, compiled);
      errs.push(...exampleErrs);
      if (exampleErrs.length) return errs;
      const recomputed = knownFresh === undefined ? await recomputeGoldens(spec, compiled) : undefined;
      if (recomputed) errs.push(...jsonSchemaTypePresenceViolations(spec, recomputed));
      const fresh = knownFresh ?? serialize(recomputed!, comments);
      const stale = fresh !== canon;
      if (stale)
        errs.push(
          (violations.length
            ? `goldens stale - resolve the identity mismatch above first, then \`pnpm spec check ${id} --write\` can fix it (also formats canonically; use \`pnpm spec format\` for a formatting-only fix)`
            : `goldens stale - run \`pnpm spec check ${id} --write\` (also formats canonically; use \`pnpm spec format\` for a formatting-only fix)`) +
            `:\n${diffText(canon, fresh)}`,
        );
      errs.push(...(await checkAliases(spec)));
      errs.push(...(await checkVs(spec)));
      // Not gated on staleness: this asks what the comparator DOES with the
      // values, which a stale golden line doesn't change.
      errs.push(...checkEquality(spec, schema));
      // The matrix asks whether every spelling agrees with the golden, which is
      // not a question worth answering against a golden already known to be
      // wrong - it would report the same staleness a dozen more times. A wrong
      // `isAsync` is the same: the marker is what says which outcomes a
      // direction can even be built through.
      if (!stale && !asyncErrs.length) {
        errs.push(...(await checkOperationMatrix(spec, schema)));
        // Both read the goldens back and ask something else about them, so they
        // are gated on the same freshness the matrix is: against a stale golden
        // they would report the staleness a second and third time.
        errs.push(...checkJsonSchemaExamples(spec));
        errs.push(...(await checkZodExamples(spec)));
      }
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
  "# Generated by `pnpm spec check --write` - every row is measured, so never hand-write one.",
].join("\n");

const serializeBundleSize = (obj: BundleSize): string =>
  BUNDLE_SIZE_HEADER + "\n" + stringifyYaml(order(obj, BUNDLE_SIZE_KEY_ORDER as string[]), { lineWidth: 0 });

const readBundleSizeRaw = (): string =>
  existsSync(BUNDLE_SIZE_PATH) ? readFileSync(BUNDLE_SIZE_PATH, "utf8") : "";

// Every row is derived, so the check is just "does the file equal what the live
// entry measures" - no author-owned part to preserve. `fresh` comes back with
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

  if (!raw) return { errs: ["missing - run `pnpm spec check --write`"], fresh, after };

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

  if (raw !== fresh) errs.push(`stale - run \`pnpm spec check --write\`:\n${diffText(raw, fresh)}`);

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
    // Reported AND skipped: buildScenarioRunner runs the scenario once while
    // constructing it, so merely reporting this one would still let it
    // reconfigure the library for every scenario and spec after it - the exact
    // thing the rule exists to prevent. checkSpec stops before evaluating for
    // the same reason.
    const callsGlobal = (["prepare", "run"] as const).filter((field) => {
      const source = scenario[field];
      return typeof source === "string" && GLOBAL_CALL.test(source);
    });
    for (const field of callsGlobal)
      errs.push(
        `${id}: ${field} calls S.global - it sets process-wide configuration that every spec and ` +
          "scenario in the run then compiles against",
      );
    if (callsGlobal.length) continue;
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
