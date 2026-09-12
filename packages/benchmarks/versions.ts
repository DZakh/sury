// `name version` for each package a page was measured against, read off what
// is installed rather than a hand-kept list, so a dependency bump shows up in
// the generated page instead of silently invalidating it.
import { createRequire } from "node:module";
import { existsSync, readFileSync } from "node:fs";
import path from "node:path";

const require_ = createRequire(import.meta.url);

const read = (manifest: string): { name?: string; version?: string } =>
  JSON.parse(readFileSync(manifest, "utf8")) as { name?: string; version?: string };

const versionOf = (name: string): string => {
  // Most packages export their manifest.
  try {
    return read(require_.resolve(`${name}/package.json`)).version ?? "?";
  } catch {
    // The ones that do not (`@sinclair/typebox`, `arktype`) are found by
    // walking up from whatever their main entry resolves to, until the
    // manifest that names them.
    try {
      let dir = path.dirname(require_.resolve(name));
      for (;;) {
        const candidate = path.join(dir, "package.json");
        if (existsSync(candidate)) {
          const manifest = read(candidate);
          if (manifest.name === name) return manifest.version ?? "?";
        }
        const up = path.dirname(dir);
        if (up === dir) return "?";
        dir = up;
      }
    } catch {
      return "?";
    }
  }
};

export const versionsOf = (names: string[]): string[] => names.map((name) => `${name} ${versionOf(name)}`);
