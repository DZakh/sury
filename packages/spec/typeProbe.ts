// Derives TypeScript type strings (`ts.input`/`ts.output`) and the type-
// instantiation count (`ts.instantiations`) for a schema expression, directly
// via @typescript/vfs + the TypeScript compiler API - NOT @ark/attest.
//
// @ark/attest's own instantiation-counting (bench/type.js + cache/utils.js)
// already works this way internally: an isolated @typescript/vfs environment,
// diffed against a baseline via the real (if undocumented)
// `program.getInstantiationCount()`. What makes attest itself slow for our
// purposes is `setup()`'s separate, unrelated `analyzeProjectAssertions()` -
// a full-project scan for pre-written `attest()`/`bench()` calls, built to
// support hardcoded-expected-value assertions across a whole test suite. We
// don't need that: we want a fresh value for an arbitrary expression on
// demand, so this module vendors just the isolated-environment +
// instantiation-delta + typeToString logic.
//
// Measured: ~1s cold (first schema in a worker - dominated by loading
// lib.d.ts + index.d.ts), ~50-200ms warm (every subsequent schema in the same
// worker, since the environment is memoized) - versus attest's ~15s (which
// is dominated by its whole-project assertion scan, unrelated to this cost).
import { fileURLToPath } from "node:url";
import { parentPort } from "node:worker_threads";
import ts from "typescript";
import * as tsvfs from "@typescript/vfs";
import type { TypeInfo } from "./introspect";

const SURY_DIR = fileURLToPath(new URL("../sury/", import.meta.url));
// Only ever written to the environment's in-memory file map, never to disk, so
// every worker can use the same name without clobbering another's.
const PROBE_FILE = SURY_DIR + ".type-probe.ts";
const IMPORT_LINE = `import * as S from "./index.mjs";\n`;

// Per worker. The pool in introspect.ts hands a worker one probe at a time, so
// PROBE_FILE is never rewritten while another probe still reads it.
let env: tsvfs.VirtualTypeScriptEnvironment | undefined;
let baselineCount: number | undefined;

const getEnv = (): tsvfs.VirtualTypeScriptEnvironment => {
  if (env) return env;
  const configPath = ts.findConfigFile(SURY_DIR, ts.sys.fileExists, "tsconfig.json");
  if (!configPath) throw new Error(`tsconfig.json not found under ${SURY_DIR}`);
  const configFile = ts.readConfigFile(configPath, ts.sys.readFile);
  const parsed = ts.parseJsonConfigFileContent(configFile.config, ts.sys, SURY_DIR);
  const libMap = tsvfs.createDefaultMapFromNodeModules(parsed.options);
  const system = tsvfs.createFSBackedSystem(libMap, SURY_DIR, ts);
  env = tsvfs.createVirtualTypeScriptEnvironment(system, [], ts, parsed.options);
  return env;
};

const check = (text: string) => {
  const e = getEnv();
  if (e.sys.fileExists(PROBE_FILE)) e.updateFile(PROBE_FILE, text);
  else e.createFile(PROBE_FILE, text);
  const program = e.languageService.getProgram()!;
  const file = program.getSourceFile(PROBE_FILE)!;
  // Force type checking - merely constructing the program doesn't instantiate
  // the generics; getInstantiationCount() only reflects work actually done.
  // Diagnostics are collected (not just triggered) so deriveTypeInfo can
  // surface *why* if the probe below ever fails to resolve a type.
  const diagnostics = [...program.getSemanticDiagnostics(file), ...program.getDeclarationDiagnostics(file)];
  return { program, file, diagnostics, count: program.getInstantiationCount() };
};

// Subtracted from every schema's count so each spec's `ts.instantiations` is
// isolated to what *that* schema contributes, not the cost of the bare import.
// Measured in the same environment as the probe it is subtracted from: each
// worker has its own, and the count must not depend on which one ran it.
const getBaselineCount = (): number => {
  if (baselineCount === undefined) baselineCount = check(IMPORT_LINE).count;
  return baselineCount;
};

// Prints the resolved type of every top-level `type __X = …` alias in a
// checked probe file, keyed by alias name. InTypeAlias makes the printer
// expand a type that still carries an alias symbol back to the alias itself (a
// union return type would otherwise print as the useless literal "__Output"
// instead of "string | number"). Shared by every derivation below so they all
// read the exact same way.
const extractAliases = (program: ts.Program, file: ts.SourceFile): Record<string, string> => {
  const checker = program.getTypeChecker();
  const out: Record<string, string> = {};
  ts.forEachChild(file, function visit(node) {
    if (ts.isTypeAliasDeclaration(node))
      out[node.name.text] = checker.typeToString(
        checker.getTypeAtLocation(node.name),
        undefined,
        ts.TypeFormatFlags.InTypeAlias,
      );
    ts.forEachChild(node, visit);
  });
  return out;
};

const diagnosticsText = (diagnostics: readonly ts.Diagnostic[]): string =>
  diagnostics.map((d) => ts.flattenDiagnosticMessageText(d.messageText, "\n")).join("\n");

const deriveTypeInfo = (schemaTs: string): TypeInfo => {
  const withExpr =
    IMPORT_LINE +
    `const __schema = ${schemaTs};\n` +
    `type __Output = S.Output<typeof __schema>;\n` +
    `type __Input = S.Input<typeof __schema>;\n`;
  const { program, file, diagnostics, count } = check(withExpr);
  const { __Input: input, __Output: output } = extractAliases(program, file);
  // A schema that genuinely fails to typecheck should fail loudly here, not
  // silently produce an empty ts.output/ts.input golden that then happily
  // passes `spec check` forever (byte-identical "" recomputed each time).
  if (!output || !input) {
    const msg = diagnosticsText(diagnostics);
    throw new Error(
      `deriveTypeInfo: could not resolve __Output/__Input for \`${schemaTs}\`` +
        (msg ? `:\n${msg}` : " (no compiler diagnostics - schema didn't produce the expected type alias)"),
    );
  }
  // Aliases resolving is not the same as the schema typechecking. An excess
  // argument still infers a schema, so a spec written against a removed
  // signature keeps producing goldens - with the argument silently dropped at
  // runtime, which is how the codec specs lost their encode direction.
  if (diagnostics.length) {
    throw new Error(
      `deriveTypeInfo: \`${schemaTs}\` does not typecheck:\n${diagnosticsText(diagnostics)}`,
    );
  }
  return { input, output, instantiations: count - getBaselineCount() };
};

const deriveRoundTripTypeInfo = (
  schemaTs: string,
  inputSource?: string,
  outputSource?: string,
): Pick<TypeInfo, "fromInput" | "fromOutput" | "inputMatches" | "outputMatches"> => {
  if (inputSource === undefined && outputSource === undefined) return {};
  const withExpr =
    IMPORT_LINE +
    `const __schema = ${schemaTs};\n` +
    `type __Input = S.Input<typeof __schema>;\n` +
    `type __Output = S.Output<typeof __schema>;\n` +
    (inputSource === undefined
      ? ""
      : `const __inputSchema = S.fromJSONSchemaOrThrow(${inputSource});\n` +
        `type __FromInput = S.Input<typeof __inputSchema>;\n` +
        `type __InputMatches = [__Input] extends [__FromInput] ? [__FromInput] extends [__Input] ? true : false : false;\n`) +
    (outputSource === undefined
      ? ""
      : `const __outputSchema = S.fromJSONSchemaOrThrow(${outputSource});\n` +
        `type __FromOutput = S.Output<typeof __outputSchema>;\n` +
        `type __OutputMatches = [__Output] extends [__FromOutput] ? [__FromOutput] extends [__Output] ? true : false : false;\n`);
  const { program, file, diagnostics } = check(withExpr);
  const {
    __FromInput: fromInput,
    __FromOutput: fromOutput,
    __InputMatches: inputMatches,
    __OutputMatches: outputMatches,
  } = extractAliases(program, file);
  if ((inputSource !== undefined && !fromInput) || (outputSource !== undefined && !fromOutput)) {
    throw new Error(
      "deriveRoundTripTypeInfo: could not resolve JSON Schema round-trip types" +
        (diagnostics.length === 0 ? "" : `:\n${diagnosticsText(diagnostics)}`),
    );
  }
  return {
    fromInput,
    fromOutput,
    inputMatches: inputSource === undefined ? undefined : inputMatches === "true",
    outputMatches: outputSource === undefined ? undefined : outputMatches === "true",
  };
};

const deriveVsTypeInfo = (importLine: string, expr: string): { input: string; output: string } => {
  const withExpr =
    importLine +
    `const __schema = ${expr};\n` +
    `type __Output = NonNullable<(typeof __schema)["~standard"]["types"]>["output"];\n` +
    `type __Input = NonNullable<(typeof __schema)["~standard"]["types"]>["input"];\n`;
  const { program, file, diagnostics } = check(withExpr);
  const { __Input: input, __Output: output } = extractAliases(program, file);
  if (!output || !input) {
    const msg = diagnosticsText(diagnostics);
    throw new Error(
      `deriveVsTypeInfo: could not resolve __Output/__Input for \`${expr}\`` +
        (msg ? `:\n${msg}` : " (no compiler diagnostics - is it a Standard Schema value with a `~standard` prop?)"),
    );
  }
  return { input, output };
};

export const probes = { deriveTypeInfo, deriveRoundTripTypeInfo, deriveVsTypeInfo };
export type Probe = keyof typeof probes;

// introspect.ts posts a worker's next probe only after its reply, so each reply
// answers the one probe in flight.
parentPort?.on("message", ([probe, args]: [Probe, string[]]) => {
  try {
    parentPort!.postMessage([true, (probes[probe] as (...a: string[]) => unknown)(...args)]);
  } catch (e) {
    parentPort!.postMessage([false, (e as Error).message ?? String(e)]);
  }
});
