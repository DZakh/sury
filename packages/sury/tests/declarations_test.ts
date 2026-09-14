// Typechecks the shipped declarations with `skipLibCheck` OFF.
//
// Every other typecheck in the repo runs with `skipLibCheck: true` (tsconfig.json,
// and most consumers do the same), which does not check .d.ts files at all. A
// broken reference inside index.d.ts therefore raises nothing - it resolves to an
// error type, and everything built on it degrades quietly: a dangling
// `StandardSchemaV1.Props` in `Schema["~standard"]` costs every schema its
// inferred Input and Output while the suite stays green.
//
// This bites specifically when declarations are split across files, since
// `export * from "./other.js"` re-exports names to consumers without binding
// them locally - the file still needs its own `import type`.
import { execFileSync } from "node:child_process";
import { readFileSync } from "node:fs";
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

// The declarations are hand-written and the runtime entry is generated, so
// nothing but this keeps the two surfaces the same set of names. A declaration
// with no export behind it typechecks and then throws "is not a function" at
// the call; an export with no declaration is invisible to every TS consumer.
test("every declared value has a runtime export, and vice versa", async () => {
  const runtime = new Set(Object.keys(await import("../index.mjs")));
  const source = readFileSync(entry, "utf8");
  const declared = new Set<string>();
  for (const [, name] of source.matchAll(/^export (?:declare )?(?:function|const|class) (\w+)/gm)) {
    declared.add(name!);
  }
  // A reserved word (`void`, `enum`) can't be a declaration name, so those are
  // declared under an alias and renamed in an export list.
  for (const [, list] of source.matchAll(/^export \{([^}]*)\};/gm)) {
    for (const entry of list!.split(",")) {
      const parts = entry.trim().split(/\s+as\s+/);
      if (parts[0]) declared.add(parts[1] ?? parts[0]!);
    }
  }
  // The ReScript binding surface, which index.d.ts deliberately doesn't
  // describe. `$`-prefixed is the convention (entry.ts); these three predate it
  // and are bound from S.res under their bare names - `list` builds a ReScript
  // linked list, `enum` takes the array of values `S.union` takes as a
  // definition, and `nan` is `t<float>`'s NaN literal. Declaring them would put
  // three ReScript-shaped APIs in front of every TS consumer; renaming them to
  // `$list`/`$enum`/`$nan` is the fix, and it belongs to whoever owns the
  // published surface.
  const RESCRIPT_ONLY = new Set(["list", "enum", "nan"]);
  const rescriptOnly = (name: string) => name.startsWith("$") || RESCRIPT_ONLY.has(name);
  // Excused from the runtime side only: a declaration for one of them is the
  // very thing the exemption exists to keep out.
  expect([...declared].filter(rescriptOnly).sort()).toEqual([]);
  expect([...runtime].filter((n) => !rescriptOnly(n) && !declared.has(n)).sort()).toEqual([]);
  expect([...declared].filter((n) => !runtime.has(n)).sort()).toEqual([]);
});
