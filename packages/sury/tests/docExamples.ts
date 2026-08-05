// Extraction and compilation machinery for docExamples_test.ts.
//
// Pulls fenced code blocks out of markdown docs, out of JSDoc comments in
// index.d.ts, and out of docstrings in src/S.res, then verifies they compile:
// TypeScript blocks through a single in-memory ts.Program resolving `sury` to
// the real index.d.ts, ReScript blocks through per-module `bsc` runs against
// the artifacts `rescript build` left in lib/bs/src.
//
// A ```ts block may use names declared by earlier blocks in the same document
// (docs are written as running tutorials). Each block therefore compiles with
// a context: the top-level declarations it references, pulled transitively
// from preceding blocks, with a later redeclaration shadowing an earlier one.
// A ReScript document becomes one module with each block in a submodule that
// opens its predecessors, since values shadow at the top level but types
// don't.
//
// Tag a fence ```ts skip / ```rescript skip to exclude a block that is
// intentionally not compilable on its own.

import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { execFile } from "node:child_process";
import ts from "typescript";

const projectPath = fileURLToPath(new URL("..", import.meta.url));
const repoRootPath = path.join(projectPath, "../..");

export type Block = {
  /** Path relative to the repo root, for reporting. */
  file: string;
  /** 1-based line of the first code line inside the fence. */
  line: number;
  lang: string;
  skip: boolean;
  text: string;
};

// ── Extraction ───────────────────────────────────────────────────────────────

export const extractMarkdownBlocks = (absPath: string): Block[] => {
  const file = path.relative(repoRootPath, absPath);
  const lines = fs.readFileSync(absPath, "utf8").split("\n");
  const blocks: Block[] = [];
  let open: Block | null = null;
  let buf: string[] = [];
  for (let i = 0; i < lines.length; i++) {
    const fence = /^\s*```(\S*)\s*(.*)$/.exec(lines[i]!);
    if (open) {
      if (fence && fence[1] === "") {
        open.text = buf.join("\n");
        blocks.push(open);
        open = null;
        buf = [];
      } else {
        buf.push(lines[i]!);
      }
    } else if (fence && fence[1] !== "") {
      open = {
        file,
        line: i + 2,
        lang: fence[1]!.toLowerCase(),
        skip: /\bskip\b/.test(fence[2]!),
        text: "",
      };
    }
  }
  return blocks;
};

// Fenced blocks inside /** … */ comments. Lines are unindented from the
// doc-comment `* ` prefix before fence matching.
export const extractDocCommentBlocks = (absPath: string): Block[] => {
  const file = path.relative(repoRootPath, absPath);
  const source = fs.readFileSync(absPath, "utf8");
  const blocks: Block[] = [];
  for (const match of source.matchAll(/\/\*\*[\s\S]*?\*\//g)) {
    const startLine = source.slice(0, match.index).split("\n").length;
    const lines = match[0]
      .split("\n")
      .map((l) => l.replace(/^\s*\* ?/, "").replace(/\*\/\s*$/, ""));
    let open: Block | null = null;
    let buf: string[] = [];
    for (let i = 0; i < lines.length; i++) {
      const fence = /^```(\S*)\s*(.*)$/.exec(lines[i]!);
      if (open) {
        if (fence && fence[1] === "") {
          open.text = buf.join("\n");
          blocks.push(open);
          open = null;
          buf = [];
        } else {
          buf.push(lines[i]!);
        }
      } else if (fence && fence[1] !== "") {
        open = {
          file,
          line: startLine + i + 1,
          lang: fence[1]!.toLowerCase(),
          skip: /\bskip\b/.test(fence[2]!),
          text: "",
        };
      }
    }
  }
  return blocks;
};

export const isTs = (b: Block) => (b.lang === "ts" || b.lang === "typescript") && !b.skip;
export const isRes = (b: Block) => (b.lang === "rescript" || b.lang === "res") && !b.skip;

// ── TypeScript units ─────────────────────────────────────────────────────────

type Segment = { file: string; line: number; text: string };

export type TsUnit = {
  /** Virtual file name inside the ts.Program. */
  name: string;
  segments: Segment[];
};

type RegistryEntry = {
  text: string;
  file: string;
  line: number;
  order: number;
  declared: Set<string>;
  deps: Set<RegistryEntry>;
};

const scriptTarget = ts.ScriptTarget.ESNext;

const topLevelDeclarations = (statement: ts.Statement): string[] => {
  const names: string[] = [];
  const addBinding = (name: ts.BindingName) => {
    if (ts.isIdentifier(name)) names.push(name.text);
    else
      for (const el of name.elements)
        if (ts.isBindingElement(el)) addBinding(el.name);
  };
  if (ts.isVariableStatement(statement)) {
    for (const d of statement.declarationList.declarations) addBinding(d.name);
  } else if (
    (ts.isFunctionDeclaration(statement) ||
      ts.isClassDeclaration(statement) ||
      ts.isInterfaceDeclaration(statement) ||
      ts.isTypeAliasDeclaration(statement) ||
      ts.isEnumDeclaration(statement) ||
      ts.isModuleDeclaration(statement)) &&
    statement.name &&
    ts.isIdentifier(statement.name)
  ) {
    names.push(statement.name.text);
  } else if (ts.isImportDeclaration(statement) && statement.importClause) {
    const clause = statement.importClause;
    if (clause.name) names.push(clause.name.text);
    if (clause.namedBindings) {
      if (ts.isNamespaceImport(clause.namedBindings)) {
        names.push(clause.namedBindings.name.text);
      } else {
        for (const el of clause.namedBindings.elements) names.push(el.name.text);
      }
    }
  }
  return names;
};

// Identifiers that read a name from the enclosing scope. Property names,
// member accesses and declaration names are excluded; over-collection is
// harmless because only names present in the registry matter.
const usedIdentifiers = (node: ts.Node): Set<string> => {
  const used = new Set<string>();
  const visit = (n: ts.Node): void => {
    if (ts.isIdentifier(n)) {
      const p = n.parent;
      const isNonUse =
        (ts.isPropertyAccessExpression(p) && p.name === n) ||
        (ts.isQualifiedName(p) && p.right === n) ||
        (ts.isPropertyAssignment(p) && p.name === n) ||
        ((ts.isPropertySignature(p) ||
          ts.isPropertyDeclaration(p) ||
          ts.isMethodDeclaration(p) ||
          ts.isMethodSignature(p) ||
          ts.isEnumMember(p) ||
          ts.isFunctionDeclaration(p) ||
          ts.isClassDeclaration(p) ||
          ts.isInterfaceDeclaration(p) ||
          ts.isTypeAliasDeclaration(p) ||
          ts.isEnumDeclaration(p) ||
          ts.isModuleDeclaration(p)) &&
          p.name === n) ||
        (ts.isParameter(p) && p.name === n) ||
        (ts.isVariableDeclaration(p) && p.name === n) ||
        (ts.isBindingElement(p) && p.name === n && !p.dotDotDotToken && p.propertyName !== undefined) ||
        ts.isImportClause(p) ||
        ts.isImportSpecifier(p) ||
        ts.isNamespaceImport(p);
      if (!isNonUse) used.add(n.text);
    }
    ts.forEachChild(n, visit);
  };
  visit(node);
  return used;
};

// Build one compilable unit per ts block. Blocks of a document share a
// registry of top-level declarations; each unit prepends, in original order,
// the registered statements its block references (transitively).
export const buildTsUnits = (blocks: Block[], unitPrefix: string): TsUnit[] => {
  const units: TsUnit[] = [];
  const registry = new Map<string, RegistryEntry>();
  let order = 0;
  blocks.forEach((block, blockIdx) => {
    const sf = ts.createSourceFile("block.ts", block.text, scriptTarget, true);
    const blockDeclared = new Set<string>();
    for (const statement of sf.statements)
      for (const name of topLevelDeclarations(statement)) blockDeclared.add(name);

    // Resolve context from the registry as it stood before this block.
    const needed = [...usedIdentifiers(sf)].filter(
      (name) => !blockDeclared.has(name) && registry.has(name)
    );
    const picked = new Set<RegistryEntry>();
    const resolve = (entry: RegistryEntry) => {
      if (picked.has(entry)) return;
      picked.add(entry);
      for (const dep of entry.deps) resolve(dep);
    };
    for (const name of needed) resolve(registry.get(name)!);
    // A block's own declaration shadows any context statement redeclaring it.
    const context = [...picked]
      .filter((e) => ![...e.declared].some((n) => blockDeclared.has(n)))
      .sort((a, b) => a.order - b.order);

    const segments: Segment[] = [];
    if (!blockDeclared.has("S") && !context.some((e) => e.declared.has("S"))) {
      segments.push({ file: block.file, line: block.line, text: `import * as S from "sury";` });
    }
    for (const e of context) segments.push({ file: e.file, line: e.line, text: e.text });
    segments.push({ file: block.file, line: block.line, text: block.text });
    units.push({ name: `${unitPrefix}${blockIdx}.ts`, segments });

    // Register this block's declarations for later blocks.
    const lineOf = (statement: ts.Statement) =>
      block.line + sf.getLineAndCharacterOfPosition(statement.getStart(sf)).line;
    for (const statement of sf.statements) {
      const declared = new Set(topLevelDeclarations(statement));
      if (declared.size === 0 || ts.isImportDeclaration(statement)) continue;
      const deps = new Set<RegistryEntry>();
      for (const name of usedIdentifiers(statement)) {
        if (!declared.has(name) && registry.has(name)) deps.add(registry.get(name)!);
      }
      const entry: RegistryEntry = {
        text: statement.getText(sf),
        file: block.file,
        line: lineOf(statement),
        order: order++,
        declared,
        deps,
      };
      for (const name of declared) registry.set(name, entry);
    }
  });
  return units;
};

export type CompileFailure = { file: string; line: number; message: string };

export const compileTsUnits = (units: TsUnit[]): CompileFailure[] => {
  const options: ts.CompilerOptions = {
    // Mirrors the package tsconfig — doc examples are held to the same bar.
    strict: true,
    noUncheckedIndexedAccess: true,
    esModuleInterop: true,
    skipLibCheck: true,
    target: scriptTarget,
    module: ts.ModuleKind.ESNext,
    moduleResolution: ts.ModuleResolutionKind.Bundler,
    lib: ["lib.esnext.d.ts", "lib.dom.d.ts"],
    types: [],
    noEmit: true,
    baseUrl: projectPath,
    paths: { sury: ["./index.d.ts"] },
  };
  const virtual = new Map<string, { content: string; segments: Segment[]; starts: number[] }>();
  for (const unit of units) {
    const starts: number[] = [];
    let lineCursor = 0;
    const parts: string[] = [];
    for (const segment of unit.segments) {
      starts.push(lineCursor);
      parts.push(segment.text);
      lineCursor += segment.text.split("\n").length;
    }
    virtual.set(path.join(projectPath, unit.name), {
      content: parts.join("\n"),
      segments: unit.segments,
      starts,
    });
  }

  const host = ts.createCompilerHost(options);
  const baseGetSourceFile = host.getSourceFile.bind(host);
  host.getSourceFile = (fileName, languageVersion, ...rest) => {
    const v = virtual.get(path.resolve(fileName));
    if (v) return ts.createSourceFile(fileName, v.content, languageVersion);
    return baseGetSourceFile(fileName, languageVersion, ...rest);
  };
  const baseReadFile = host.readFile.bind(host);
  host.readFile = (fileName) =>
    virtual.get(path.resolve(fileName))?.content ?? baseReadFile(fileName);
  const baseFileExists = host.fileExists.bind(host);
  host.fileExists = (fileName) =>
    virtual.has(path.resolve(fileName)) || baseFileExists(fileName);

  const program = ts.createProgram([...virtual.keys()], options, host);
  const failures: CompileFailure[] = [];
  const diagnostics = [
    ...program.getSyntacticDiagnostics(),
    ...program.getSemanticDiagnostics(),
  ];
  for (const d of diagnostics) {
    const message = ts.flattenDiagnosticMessageText(d.messageText, "\n");
    const v = d.file && virtual.get(path.resolve(d.file.fileName));
    if (!v || d.start === undefined) {
      failures.push({ file: d.file?.fileName ?? "<program>", line: 0, message });
      continue;
    }
    const { line } = d.file!.getLineAndCharacterOfPosition(d.start);
    let idx = v.starts.length - 1;
    while (idx > 0 && v.starts[idx]! > line) idx--;
    const segment = v.segments[idx]!;
    failures.push({
      file: segment.file,
      line: segment.line + (line - v.starts[idx]!),
      message,
    });
  }
  return failures;
};

// ── ReScript units ───────────────────────────────────────────────────────────

export type ResUnit = {
  moduleName: string;
  segments: Segment[];
};

// One module per document; compileResUnits wraps each block in a submodule
// opening its predecessors, so `let schema = …` redefinitions shadow while
// blocks can still build on earlier ones.
export const buildResUnit = (blocks: Block[], moduleName: string): ResUnit | null =>
  blocks.length === 0
    ? null
    : {
        moduleName,
        segments: blocks.map((b) => ({ file: b.file, line: b.line, text: b.text })),
      };

// One module per block, for interface docstrings that must stand alone.
export const buildResUnitPerBlock = (blocks: Block[], modulePrefix: string): ResUnit[] =>
  blocks.map((b, i) => ({
    moduleName: `${modulePrefix}${i}`,
    segments: [{ file: b.file, line: b.line, text: b.text }],
  }));

const bscPath = path.join(projectPath, "node_modules", ".bin", "bsc");

export const compileResUnits = async (units: ResUnit[]): Promise<CompileFailure[]> => {
  const artifacts = path.join(projectPath, "lib", "bs", "src");
  if (!fs.existsSync(path.join(artifacts, "S.cmi"))) {
    return [
      {
        file: "lib/bs/src/S.cmi",
        line: 0,
        message: "ReScript artifacts not found — run `rescript build` first (pnpm test does).",
      },
    ];
  }
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), "sury-doc-examples-"));
  try {
    const jobs = units.map((unit) => {
      // Doc examples routinely leave values unused and discard non-unit
      // results; only errors should gate, so warnings are off wholesale.
      //
      // Values shadow at the top level but type names don't, so a document's
      // blocks can't just concatenate: each goes into its own submodule that
      // `open`s all previous ones — a later definition shadows an earlier one
      // through opens, keeping the tutorial reading order.
      const starts: number[] = [];
      let lineCursor = 2;
      const parts: string[] = [`@@warning("-a")`];
      unit.segments.forEach((segment, i) => {
        if (unit.segments.length > 1) {
          parts.push(`module Block${i} = {`);
          for (let j = 0; j < i; j++) parts.push(`open Block${j}`);
          lineCursor += 1 + i;
        }
        starts.push(lineCursor);
        parts.push(segment.text);
        lineCursor += segment.text.split("\n").length;
        if (unit.segments.length > 1) {
          parts.push(`}`);
          lineCursor += 1;
        }
      });
      const filePath = path.join(dir, `${unit.moduleName}.res`);
      fs.writeFileSync(filePath, parts.join("\n"));
      return { unit, filePath, starts };
    });

    const failures: CompileFailure[] = [];
    const concurrency = Math.max(1, Math.min(8, os.cpus().length - 1));
    let next = 0;
    const worker = async () => {
      while (next < jobs.length) {
        const job = jobs[next++]!;
        const output = await new Promise<string | null>((resolvePromise) => {
          execFile(
            bscPath,
            ["-I", artifacts, "-bs-package-name", "sury", "-color", "never", job.filePath],
            { maxBuffer: 1024 * 1024 },
            (error, _stdout, stderr) => resolvePromise(error ? stderr || String(error) : null)
          );
        });
        if (output !== null) {
          // Attribute the error to the doc line via the file:line reference
          // bsc prints; fall back to the unit's first segment.
          const loc = new RegExp(
            `${job.unit.moduleName}\\.res:(\\d+)`
          ).exec(output);
          let file = job.unit.segments[0]!.file;
          let line = job.unit.segments[0]!.line;
          if (loc) {
            const errLine = Number(loc[1]);
            let idx = job.starts.length - 1;
            while (idx > 0 && job.starts[idx]! > errLine) idx--;
            const segment = job.unit.segments[idx]!;
            file = segment.file;
            line = segment.line + (errLine - job.starts[idx]!);
          }
          failures.push({ file, line, message: output.trim() });
        }
      }
    };
    await Promise.all(Array.from({ length: concurrency }, worker));
    return failures;
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
};

export const formatFailures = (failures: CompileFailure[]): string =>
  failures
    .map((f) => `${f.file}:${f.line}\n${f.message}`)
    .join("\n\n" + "-".repeat(72) + "\n\n");
