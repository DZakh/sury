import { readdirSync, readFileSync } from "node:fs";
import { join } from "node:path";
import { fileURLToPath } from "node:url";
import { expect, test } from "vitest";
import { KNOWN_BUGS } from "../scripts/knownBugs";

// The registry and the specs point at each other: a bug entry is only real
// with a spec reproducing it, and a spec's FIXME is only honest while the
// entry it names exists. The fuzzers check the third side - that each entry
// still matches what their gates find.

const specsDir = fileURLToPath(new URL("../specs", import.meta.url));
const marker = /FIXME: known bug ([\w-]+)/g;

test("every known bug has a spec that marks it", () => {
  for (const entry of KNOWN_BUGS.filter((known) => known.kind === "bug")) {
    expect(entry.spec, `${entry.id} names no spec`).toBeTypeOf("string");
    const text = readFileSync(join(specsDir, `${entry.spec}.yaml`), "utf8");
    expect(text, `specs/${entry.spec}.yaml has no "FIXME: known bug ${entry.id}"`).toContain(
      `FIXME: known bug ${entry.id}`,
    );
  }
});

test("a limitation is not a bug and carries no spec", () => {
  for (const entry of KNOWN_BUGS.filter((known) => known.kind === "limitation")) {
    expect(entry.spec, `${entry.id} is a limitation with a spec`).toBeUndefined();
  }
});

test("ids are unique", () => {
  const ids = KNOWN_BUGS.map((known) => known.id);
  expect(new Set(ids).size).toBe(ids.length);
});

test("every known-bug marker in the specs names a listed bug", () => {
  const ids = new Set(KNOWN_BUGS.filter((known) => known.kind === "bug").map((known) => known.id));
  for (const file of readdirSync(specsDir).filter((name) => name.endsWith(".yaml"))) {
    for (const [, id] of readFileSync(join(specsDir, file), "utf8").matchAll(marker)) {
      expect(ids.has(id!), `specs/${file} marks "${id}", which scripts/knownBugs.ts does not list`).toBe(true);
    }
  }
});
