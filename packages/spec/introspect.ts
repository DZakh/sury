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
  program.getSemanticDiagnostics(file);
  program.getDeclarationDiagnostics(file);
  return { program, file, count: program.getInstantiationCount() };
};

// The bare import's own instantiation cost, memoized once per process and
// subtracted from every schema's count so each spec's `ts.instantiations` is
// isolated to what *that* schema contributes.
const getBaselineCount = (): number => {
  if (baselineCount === undefined) baselineCount = check(IMPORT_LINE).count;
  return baselineCount;
};

export type TypeInfo = { input: string; output: string; instantiations: number };

// Derives {input, output} type strings and the instantiation count
// contributed by declaring `schemaTs` and extracting S.Output<>/S.Input<>
// from it — the realistic combined per-schema cost (matching the "define +
// extract" measurements in tests/types.bench.ts), not the isolated cost of
// either half alone.
export const deriveTypeInfo = (schemaTs: string): TypeInfo => {
  const withExpr =
    IMPORT_LINE +
    `const __schema = ${schemaTs};\n` +
    `type __Output = S.Output<typeof __schema>;\n` +
    `type __Input = S.Input<typeof __schema>;\n`;
  const { program, file, count } = check(withExpr);
  const checker = program.getTypeChecker();
  let output = "";
  let input = "";
  ts.forEachChild(file, function visit(node) {
    if (ts.isTypeAliasDeclaration(node)) {
      const str = checker.typeToString(checker.getTypeAtLocation(node.name));
      if (node.name.text === "__Output") output = str;
      if (node.name.text === "__Input") input = str;
    }
    ts.forEachChild(node, visit);
  });
  return { input, output, instantiations: count - getBaselineCount() };
};
