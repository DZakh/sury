// Derives TypeScript type strings (`ts.input`/`ts.output`) and the type-
// instantiation count (`ts.instantiations`) for a schema expression, directly
// via @typescript/vfs + the TypeScript compiler API — NOT @ark/attest.
//
// @ark/attest's own instantiation-counting (bench/type.js + cache/utils.js)
// already works this way internally: an isolated @typescript/vfs environment,
// diffed against a baseline via the real (if undocumented)
// `program.getInstantiationCount()`. What makes attest itself slow for our
// purposes is `setup()`'s separate, unrelated `analyzeProjectAssertions()` —
// a full-project scan for pre-written `attest()`/`bench()` calls, built to
// support hardcoded-expected-value assertions across a whole test suite. We
// don't need that: we want a fresh value for an arbitrary expression on
// demand, so this module vendors just the isolated-environment +
// instantiation-delta + typeToString logic.
//
// Measured: ~1s cold (first schema in a process — dominated by loading
// lib.d.ts + S.d.ts), ~50-200ms warm (every subsequent schema in the same
// process, since the environment is memoized) — versus attest's ~15s (which
// is dominated by its whole-project assertion scan, unrelated to this cost).
import { fileURLToPath } from "node:url";
import ts from "typescript";
import * as tsvfs from "@typescript/vfs";

const SURY_DIR = fileURLToPath(new URL("../sury/", import.meta.url));
// Kept at the package root (not under tests/generated/) so it doesn't depend
// on that directory existing yet on a fresh checkout.
const PROBE_FILE = SURY_DIR + ".type-probe.ts";
const IMPORT_LINE = `import * as S from "./src/S.mjs";\n`;

// `env`/`baselineCount`/PROBE_FILE are process-wide mutable state shared
// across concurrent deriveTypeInfo calls (cli.ts runs specs through
// Promise.all). Safe only because `check()` below has no `await` — each call
// mutates PROBE_FILE and reads the result back synchronously before another
// can interleave. If `check()` ever gains an await (e.g. an async
// LanguageService API), concurrent calls could cross-contaminate each
// other's PROBE_FILE content — would need a mutex/queue at that point.
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
  // Force type checking — merely constructing the program doesn't instantiate
  // the generics; getInstantiationCount() only reflects work actually done.
  // Diagnostics are collected (not just triggered) so deriveTypeInfo can
  // surface *why* if the probe below ever fails to resolve a type.
  const diagnostics = [...program.getSemanticDiagnostics(file), ...program.getDeclarationDiagnostics(file)];
  return { program, file, diagnostics, count: program.getInstantiationCount() };
};

// Subtracted from every schema's count so each spec's `ts.instantiations` is
// isolated to what *that* schema contributes, not the cost of the bare import.
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

export type TypeInfo = { input: string; output: string; instantiations: number };

// Derives {input, output} type strings and the instantiation count
// contributed by declaring `schemaTs` and extracting S.Output<>/S.Input<>
// from it — the realistic combined per-schema cost, not the isolated cost of
// either half alone.
//
// Returns a Promise for a uniform async API alongside bundleSize.ts's
// genuinely-async esbuild call, so a caller can `Promise.all` the two — but
// the TS Program/checker calls inside are inherently synchronous (no async
// variant of the compiler API exists), so this doesn't itself parallelize
// across concurrent calls; it just composes cleanly with what does.
export const deriveTypeInfo = async (schemaTs: string): Promise<TypeInfo> => {
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
        (msg ? `:\n${msg}` : " (no compiler diagnostics — schema didn't produce the expected type alias)"),
    );
  }
  return { input, output, instantiations: count - getBaselineCount() };
};

// The inferred input/output type strings of a `vs` cross-library schema, read
// through the Standard Schema (`~standard`) interface rather than any one
// library's own `Infer*` helper — so the same probe works for every
// Standard-Schema vendor (Zod today, Valibot/ArkType tomorrow) and reads the
// value's *published* type contract, exactly what a downstream user gets.
// Printed with the same InTypeAlias formatting as `deriveTypeInfo`, so the
// caller can compare the two strings directly for equality. `importLine`
// brings the vendor into scope (e.g. `import * as z from "zod";`). No
// instantiation count — only Sury's own schema owns that golden.
export const deriveVsTypeInfo = async (
  importLine: string,
  expr: string,
): Promise<{ input: string; output: string }> => {
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
        (msg ? `:\n${msg}` : " (no compiler diagnostics — is it a Standard Schema value with a `~standard` prop?)"),
    );
  }
  return { input, output };
};
