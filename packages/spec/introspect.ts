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
const IMPORT_LINE = `import * as S from "./src/S.js";\n`;

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
  const checker = program.getTypeChecker();
  let output = "";
  let input = "";
  ts.forEachChild(file, function visit(node) {
    if (ts.isTypeAliasDeclaration(node)) {
      // Without InTypeAlias, typeToString prints a type's OWN alias name
      // instead of expanding it whenever the resolved type still carries an
      // alias symbol back to `__Output`/`__Input` themselves — e.g. a union
      // return type prints as the useless literal string "__Output" rather
      // than "string | number". InTypeAlias tells the printer this call IS
      // the alias's own definition, so it always expands fully.
      const str = checker.typeToString(
        checker.getTypeAtLocation(node.name),
        undefined,
        ts.TypeFormatFlags.InTypeAlias,
      );
      if (node.name.text === "__Output") output = str;
      if (node.name.text === "__Input") input = str;
    }
    ts.forEachChild(node, visit);
  });
  // A schema that genuinely fails to typecheck should fail loudly here, not
  // silently produce an empty ts.output/ts.input golden that then happily
  // passes `spec check` forever (byte-identical "" recomputed each time).
  if (!output || !input) {
    const msg = diagnostics.map((d) => ts.flattenDiagnosticMessageText(d.messageText, "\n")).join("\n");
    throw new Error(
      `deriveTypeInfo: could not resolve __Output/__Input for \`${schemaTs}\`` +
        (msg ? `:\n${msg}` : " (no compiler diagnostics — schema didn't produce the expected type alias)"),
    );
  }
  return { input, output, instantiations: count - getBaselineCount() };
};
