// Typechecks the shipped declarations with `skipLibCheck` OFF.
//
// Every other typecheck in the repo runs with `skipLibCheck: true` (tsconfig.json,
// and most consumers do the same), which does not check .d.ts files at all. A
// broken reference inside index.d.ts therefore raises nothing — it resolves to an
// error type, and everything built on it degrades quietly: a dangling
// `StandardSchemaV1.Props` in `Schema["~standard"]` costs every schema its
// inferred Input and Output while the suite stays green.
//
// This bites specifically when declarations are split across files, since
// `export * from "./other.js"` re-exports names to consumers without binding
// them locally — the file still needs its own `import type`.
import { execFileSync } from "node:child_process";
import { createRequire } from "node:module";
import { fileURLToPath } from "node:url";
import { test, expect } from "vitest";

const entry = fileURLToPath(new URL("../index.d.ts", import.meta.url));
// Resolved rather than run through `npx`, whose own resolution step is slower
// than the compile and can outlast a test timeout when the suite is running it
// alongside everything else.
const tsc = createRequire(import.meta.url).resolve("typescript/bin/tsc");

// Generous: a cold tsc on a loaded machine is well past vitest's 5s default,
// and this failing on timing would say "the declarations are broken".
test("the public declarations typecheck without skipLibCheck", { timeout: 120_000 }, () => {
  let output = "";
  try {
    execFileSync(
      process.execPath,
      [
        tsc,
        "--noEmit",
        "--strict",
        "--target",
        "esnext",
        "--module",
        "ES2020",
        "--moduleResolution",
        "node",
        entry,
      ],
      { encoding: "utf8", cwd: fileURLToPath(new URL("..", import.meta.url)) },
    );
  } catch (error) {
    output = (error as { stdout?: string }).stdout ?? String(error);
  }
  expect(output).toBe("");
});
