// Guards the shape of the published package. `pnpm build` assembles ./artifacts
// and publishing happens from there, so the directory listing below is the
// deliverable — anything that appears in it without appearing here is something
// a consumer would download by accident.

import { execFileSync } from "node:child_process";
import { existsSync, readdirSync, readFileSync, statSync } from "node:fs";
import { createRequire } from "node:module";
import path from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import { describe, expect, test } from "vitest";

const artifactsPath = fileURLToPath(new URL("../artifacts", import.meta.url));

const FILES = [
  "LICENSE",
  "README.md",
  "docs/js-usage.md",
  "docs/rescript-usage.md",
  "index.d.mts",
  "index.d.ts",
  "index.js",
  "index.mjs",
  "jsr.json",
  "package.json",
  "rescript.json",
  "src/JSONSchema.res",
  "src/JSONSchema.res.mjs",
  "src/OpenAPI.res",
  "src/OpenAPI.res.mjs",
  "src/S.gen.d.ts",
  "src/S.res",
  "src/S.res.js",
  "src/S.res.mjs",
  "src/StandardSchema.res",
  "src/StandardSchema.res.mjs",
];

// jsr.json configures the JSR publish; it isn't part of the npm tarball.
const PUBLISHED_FILES = FILES.filter((f) => f !== "jsr.json");

const walk = (dir: string, prefix = ""): string[] =>
  readdirSync(dir, { withFileTypes: true }).flatMap((entry) =>
    entry.isDirectory()
      ? walk(path.join(dir, entry.name), `${prefix}${entry.name}/`)
      : [`${prefix}${entry.name}`]
  );

const read = (file: string): string => readFileSync(path.join(artifactsPath, file), "utf8");

const readJson = (file: string): any => JSON.parse(read(file));

// `](x)` is a link in prose and an arrow function in a code sample, so the
// samples have to go before anything below matches. Fences are tracked line by
// line rather than by a lazy `^```[\s\S]*?^``` `: an unclosed fence has to
// swallow the rest of the file, where the regex would silently give up and
// hand the whole code block back as prose.
const prose = (markdown: string): string => {
  const lines: string[] = [];
  let fence: string | null = null;
  for (const line of markdown.split("\n")) {
    const marker = /^\s*(`{3,}|~{3,})/.exec(line)?.[1];
    if (fence === null) {
      if (marker) fence = marker;
      else lines.push(line.replace(/`[^`\n]*`/g, ""));
    } else if (marker && marker[0] === fence[0] && marker.length >= fence.length) {
      fence = null;
    }
  }
  return lines.join("\n");
};

// A relative link target: `](…)`, minus anchors, and minus anything carrying a
// URL scheme (`https:`, `mailto:`) or a title after the path.
const RELATIVE_LINK = /]\((?!\w+:)([^)#\s]+)[^)]*\)/g;

// `pnpm test` doesn't run the packer, so these only mean anything after a
// `pnpm build`. Skipping is fine locally, but in CI a missing artifacts/ means
// the build step was dropped or reordered — and a silent skip here would
// retire this whole guard without anyone noticing.
if (process.env.CI && !existsSync(artifactsPath)) {
  throw new Error("artifacts/ is missing in CI — run `pnpm build` before the tests");
}
const describeArtifact = existsSync(artifactsPath) ? describe : describe.skip;

// Loaded per test, not at collection: a half-built artifacts/ (interrupted
// pack, stale directory) should fail the test that needs the entry, not throw
// during collection and take the whole file with it.
const requireCjsEntry = (): any =>
  createRequire(import.meta.url)(path.join(artifactsPath, "index.js"));

// The "." export nests a types/default pair per condition; every string leaf
// is a file the tarball must carry.
const exportTargets = (entry: unknown): string[] =>
  typeof entry === "string" ? [entry] : Object.values(entry as object).flatMap(exportTargets);

describeArtifact("artifact", () => {
  test("contains exactly the files it ships", () => {
    // The walk is compared sorted, so a FILES entry out of order would read as
    // a phantom diff.
    expect(FILES).toEqual([...FILES].sort());
    expect(walk(artifactsPath).sort()).toEqual(FILES);
  });

  test("npm packs exactly those files", () => {
    const output = execFileSync("npm", ["pack", "--dry-run", "--json"], {
      cwd: artifactsPath,
      encoding: "utf8",
      stdio: ["ignore", "pipe", "ignore"],
    });
    const packed = JSON.parse(output)[0].files.map((f: { path: string }) => f.path);
    expect(packed.sort()).toEqual(PUBLISHED_FILES);
  });

  test("ships no TypeScript beyond the declarations", () => {
    const sources = walk(artifactsPath).filter((f) => f.endsWith(".ts") && !f.endsWith(".d.ts"));
    expect(sources).toEqual([]);
  });

  // A link that resolves against the repo root reads fine on GitHub and dangles
  // for everyone who opens the same file inside node_modules. Anything the
  // tarball doesn't carry has to be linked by absolute URL instead.
  test("every relative link in the shipped docs points at a shipped file", () => {
    const dangling: string[] = [];
    for (const file of PUBLISHED_FILES.filter((f) => f.endsWith(".md"))) {
      for (const [, target] of prose(read(file)).matchAll(RELATIVE_LINK)) {
        const resolved = path.resolve(path.dirname(path.join(artifactsPath, file)), target!);
        // A directory "resolves" too, but no doc means to link one — npm and
        // GitHub render nothing useful for it.
        if (!existsSync(resolved) || !statSync(resolved).isFile()) {
          dangling.push(`${file} -> ${target}`);
        }
      }
    }
    expect(dangling).toEqual([]);
  });

  // These stay in the repo for contributors. A public doc linking one sends a
  // reader somewhere they were never meant to end up, and an absolute URL to it
  // slips past the relative-link check above.
  test("the shipped docs don't link internal repo docs", () => {
    const internal = ["IDEAS.md", "CLAUDE.md", "CODEC_SPEC.md"];
    for (const file of PUBLISHED_FILES.filter((f) => f.endsWith(".md"))) {
      for (const doc of internal) {
        expect(read(file), `${file} links ${doc}`).not.toContain(doc);
      }
    }
  });

  // Removed API lives on in prose long after the code is gone. The ReScript
  // reference is checked by eye — its `S.` names are a different module.
  // Unlike the link checks this scans the raw markdown, code fences included:
  // the samples are exactly where stale API names live.
  test("the JS docs name only API that exists", () => {
    const api = new Set(Object.keys(requireCjsEntry()));
    for (const [, name] of read("index.d.ts").matchAll(
      /^export\s+(?:declare\s+)?(?:abstract\s+)?(?:type|interface|class|const|let|var|function|namespace|enum)\s+([A-Za-z_$][\w$]*)/gm
    )) {
      api.add(name!);
    }
    const unknown = new Set<string>();
    for (const file of ["README.md", "docs/js-usage.md"]) {
      for (const [, name] of read(file).matchAll(/\bS\.([A-Za-z_][A-Za-z0-9_]*)/g)) {
        if (!api.has(name!)) unknown.add(`${file} -> S.${name}`);
      }
    }
    expect([...unknown]).toEqual([]);
  });

  test("every exports target resolves to a shipped file", () => {
    const pkg = readJson("package.json");
    const targets = [...exportTargets(pkg.exports["."]), pkg.exports["./S.gen.js"].types];
    for (const target of [...targets, pkg.main, pkg.module, pkg.types]) {
      expect(existsSync(path.join(artifactsPath, target)), target).toBe(true);
    }
  });

  test("is publishable and carries no dev configuration", () => {
    const pkg = readJson("package.json");
    expect(pkg.private).toBe(false);
    // ReScript applications don't work with type: module set on packages
    expect(pkg.type).toBe("commonjs");
    expect(pkg.scripts).toBeUndefined();
    expect(pkg.devDependencies).toBeUndefined();
    // TypeScript only honors "types" when it comes first in its condition, and
    // each entry format needs declarations of its own flavor — the package is
    // commonjs, so index.d.ts typing the ESM entry would misreport its format.
    expect(pkg.exports["."]).toEqual({
      import: { types: "./index.d.mts", default: "./index.mjs" },
      require: { types: "./index.d.ts", default: "./index.js" },
    });
  });

  test("JSR publishes the same entry as npm", () => {
    expect(readJson("jsr.json").exports).toBe(readJson("package.json").module);
  });

  // artifacts/ is gitignored, so JSR's default is to publish nothing and the
  // exclude list is all `!` re-includes. package.json rides along (`!package.json`),
  // so every file its fields point at has to ride along too — npm's file list
  // (pinned above) says nothing about JSR's.
  test("JSR includes every file package.json points at", () => {
    const included = readJson("jsr.json")
      .exclude.filter((e: string) => e.startsWith("!"))
      .map((e: string) => e.slice(1));
    const pkg = readJson("package.json");
    const targets = [
      ...exportTargets(pkg.exports["."]),
      pkg.exports["./S.gen.js"].types,
      pkg.main,
      pkg.module,
      pkg.types,
    ];
    for (const target of targets) {
      const normalized = target.replace(/^\.\//, "");
      const covered = included.some(
        (inc: string) => normalized === inc || normalized.startsWith(`${inc}/`)
      );
      expect(covered, `${target} is not in jsr.json's include set`).toBe(true);
    }
  });

  // Two entry builds, but the schema cache and the Exn identity must not be
  // duplicated: the ReScript output resolves "sury" through the package's own
  // "." export rather than inlining a second copy of the implementation.
  test("ReScript output imports the runtime instead of inlining it", () => {
    expect(read("src/S.res.mjs")).toMatch(/from\s*["']sury["']/);
    expect(read("src/S.res.js")).toMatch(/require\(["']sury["']\)/);
    expect(read("src/S.res.mjs")).not.toContain("Generated from src/entry.ts");
  });

  test("both entries expose the public API", async () => {
    const esm = await import(pathToFileURL(path.join(artifactsPath, "index.mjs")).href);
    for (const entry of [requireCjsEntry(), esm]) {
      expect(entry.parser(entry.schema({ xp: entry.number }))({ xp: 1 })).toEqual({ xp: 1 });
      expect(typeof entry.object).toBe("function");
    }
  });
});
