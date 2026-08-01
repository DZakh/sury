// Guards the shape of the published package. `pnpm build` assembles ./artifacts
// and publishing happens from there, so the directory listing below is the
// deliverable — anything that appears in it without appearing here is something
// a consumer would download by accident.

import { execFileSync } from "node:child_process";
import { existsSync, readdirSync, readFileSync } from "node:fs";
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
  "index.js",
  "index.mjs",
  "jsr.json",
  "package.json",
  "rescript.json",
  "src/JSONSchema.res",
  "src/JSONSchema.res.mjs",
  "src/OpenAPI.res",
  "src/OpenAPI.res.mjs",
  "src/S.d.ts",
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

// `pnpm test` doesn't run the packer, so these only mean anything after a
// `pnpm build` — which CI always does first.
const describeArtifact = existsSync(artifactsPath) ? describe : describe.skip;

describeArtifact("artifact", () => {
  test("contains exactly the files it ships", () => {
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
    const sources = FILES.filter((f) => f.endsWith(".ts") && !f.endsWith(".d.ts"));
    expect(sources).toEqual([]);
  });

  test("every exports target resolves to a shipped file", () => {
    const pkg = readJson("package.json");
    const targets = [...Object.values<string>(pkg.exports["."]), pkg.exports["./S.gen.js"].types];
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
    // TypeScript only honors "types" when it precedes the runtime conditions.
    expect(Object.keys(pkg.exports["."])[0]).toBe("types");
  });

  test("JSR publishes the same entry as npm", () => {
    expect(readJson("jsr.json").exports).toBe(readJson("package.json").module);
  });

  // Two entry builds, but the schema cache and the Exn identity must not be
  // duplicated: the ReScript output resolves "sury" through the package's own
  // "." export rather than inlining a second copy of the implementation.
  test("ReScript output imports the runtime instead of inlining it", () => {
    expect(read("src/S.res.mjs")).toMatch(/from\s*["']sury["']/);
    expect(read("src/S.res.js")).toMatch(/require\(["']sury["']\)/);
    expect(read("src/S.res.mjs")).not.toContain("Generated from entry.ts");
  });

  test("both entries expose the public API", async () => {
    const cjs = createRequire(import.meta.url)(path.join(artifactsPath, "index.js"));
    const esm = await import(pathToFileURL(path.join(artifactsPath, "index.mjs")).href);
    for (const entry of [cjs, esm]) {
      expect(entry.parser(entry.schema({ xp: entry.number }))({ xp: 1 })).toEqual({ xp: 1 });
      expect(typeof entry.object).toBe("function");
    }
  });
});
