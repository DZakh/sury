// Harness internals: read/validate/canonicalize specs, (re)compute goldens by
// executing the real schema, and expand a spec into a committed vitest file.
import { readFileSync, writeFileSync, readdirSync } from "node:fs";
import { createHash } from "node:crypto";
import { join, basename } from "node:path";
import { parse as parseYaml, stringify as stringifyYaml } from "yaml";
import * as S from "../../src/S.js";
import {
  specSchema,
  KEY_ORDER,
  OP_ORDER,
  OP_BUILDER,
  isSkip,
} from "./format.mjs";

export const SPECS_DIR = new URL("../../specs/", import.meta.url).pathname;
export const GEN_DIR = new URL("../../tests/generated/", import.meta.url)
  .pathname;
export const SCHEMA_PATH = join(SPECS_DIR, "spec.schema.json");

const HEADER = "# yaml-language-server: $schema=./spec.schema.json";

// The reason string on a `_skip` must be a known enum value or `todo(#…)`.
// This keeps skips greppable ("what's left?" == grep for `todo`) and prevents
// an unexplained skip from being a silent default in disguise.
const SKIP_REASONS = new Set([
  "identity",
  "parser-only",
  "serializer-only",
  "lossy",
  "not-applicable",
]);
export const isValidSkipReason = (r) =>
  typeof r === "string" && (SKIP_REASONS.has(r) || /^todo\(#.+\)$/.test(r));

export const specId = (file) => basename(file).replace(/\.yaml$/, "");

export const listSpecFiles = () =>
  readdirSync(SPECS_DIR)
    .filter((f) => f.endsWith(".yaml"))
    .map((f) => join(SPECS_DIR, f));

export const readSpec = (file) => parseYaml(readFileSync(file, "utf8"));

// Run the spec object through Sury's own parser. Returns {ok} or {ok:false,error}.
export const validate = (obj) => {
  try {
    S.parser(specSchema)(obj);
    return { ok: true };
  } catch (e) {
    return { ok: false, error: e.message };
  }
};

// Evaluate the `ts` schema source into a live schema. The JS surface (`.with`)
// is valid JavaScript, so a bound `S` is all that's needed. Only `ts` is
// evaluated; `res` is checked by the ReScript compiler once res-generation lands.
export const evalSchema = (tsSource) =>
  new Function("S", `return (${tsSource});`)(S);

// ---- canonical form -------------------------------------------------------

const order = (obj, keys) => {
  if (obj === null || typeof obj !== "object" || Array.isArray(obj)) return obj;
  const out = {};
  for (const k of keys) if (k in obj) out[k] = obj[k];
  for (const k of Object.keys(obj)) if (!(k in out)) out[k] = obj[k];
  return out;
};

const canonExample = (ex) =>
  isSkip(ex) ? ex : order(ex, ["input", "output", "error", "bench"]);

const canonOp = (op) => {
  if (isSkip(op)) return op;
  const o = order(op, ["expression", "examples"]);
  if (o.examples && typeof o.examples === "object") {
    const ex = {};
    for (const [name, v] of Object.entries(o.examples)) ex[name] = canonExample(v);
    o.examples = ex;
  }
  return o;
};

// Reorder every level to the schema's fixed key order so `spec fmt` output is
// byte-deterministic and diffs show only semantic change.
export const canonicalize = (obj) => {
  const o = order(obj, KEY_ORDER);
  if (o.schema && !isSkip(o.schema)) o.schema = order(o.schema, ["res", "ts"]);
  if (o.jsonSchema && !isSkip(o.jsonSchema))
    o.jsonSchema = order(o.jsonSchema, ["input", "output"]);
  if (o.operations) {
    const ops = order(o.operations, OP_ORDER);
    for (const name of OP_ORDER) if (ops[name]) ops[name] = canonOp(ops[name]);
    o.operations = ops;
  }
  return o;
};

export const serialize = (obj) =>
  HEADER + "\n" + stringifyYaml(canonicalize(obj), { lineWidth: 0 });

// ---- golden recomputation (golden-master `update`) ------------------------

const inlineToValue = (codeStr) =>
  new Function("S", `return (${codeStr});`)(S);

// Recompute everything the harness can derive from the live schema: per-op
// codegen goldens, the input/output JSON Schemas, and — golden-master style —
// each example's result by actually running the operation on its input. The
// author owns inputs and skips; the harness owns the answers.
export const recomputeGoldens = (obj) => {
  const next = structuredCloneish(obj);
  if (isSkip(next.schema)) return next;
  const schema = evalSchema(next.schema.ts);

  if (!isSkip(next.jsonSchema)) {
    next.jsonSchema = {
      input: S.toJSONSchema(schema),
      output: S.toJSONSchema(S.reverse(schema)),
    };
  }

  for (const opName of OP_ORDER) {
    const op = next.operations[opName];
    if (isSkip(op)) continue;
    const fn = OP_BUILDER[opName](schema);
    if (!isSkip(op.expression)) op.expression = fn.toString();
    for (const [name, ex] of Object.entries(op.examples || {})) {
      if (isSkip(ex)) continue;
      const bench = ex.bench;
      try {
        const out = fn(inlineToValue(ex.input));
        op.examples[name] = clean({ input: ex.input, output: valueToCode(out), bench });
      } catch (e) {
        op.examples[name] = clean({ input: ex.input, error: e.message, bench });
      }
    }
  }
  return next;
};

const clean = (o) => {
  const r = {};
  for (const [k, v] of Object.entries(o)) if (v !== undefined) r[k] = v;
  return r;
};

// A structured clone that tolerates our plain-data specs (no functions/dates).
const structuredCloneish = (o) => JSON.parse(JSON.stringify(o));

// Render a runtime value back into source text for an example `output`.
const valueToCode = (v) =>
  typeof v === "string" || typeof v === "object" ? JSON.stringify(v) : String(v);

// ---- test-file generation -------------------------------------------------

export const sha256 = (s) => createHash("sha256").update(s).digest("hex");

const lit = (s) => JSON.stringify(s);

export const generateTest = (id, obj, specText) => {
  const L = [];
  L.push(`// @generated from specs/${id}.yaml — DO NOT EDIT.`);
  L.push(`// source-sha256: ${sha256(specText)}`);
  L.push(`// Regenerate with: pnpm spec gen`);
  L.push(`import { test, expect, expectTypeOf } from "vitest";`);
  L.push(`import * as S from "../../src/S.js";`);
  L.push(``);
  L.push(`const schema = ${obj.schema.ts};`);
  L.push(``);

  if (!isSkip(obj.types)) {
    L.push(`test(${lit(`${id} › types`)}, () => {`);
    L.push(`  expectTypeOf(schema).toEqualTypeOf<${obj.types.ts}>();`);
    L.push(`});`);
  }

  if (!isSkip(obj.jsonSchema)) {
    L.push(`test(${lit(`${id} › jsonSchema`)}, () => {`);
    L.push(
      `  expect(S.toJSONSchema(schema)).toStrictEqual(${JSON.stringify(obj.jsonSchema.input)});`,
    );
    L.push(
      `  expect(S.toJSONSchema(S.reverse(schema))).toStrictEqual(${JSON.stringify(obj.jsonSchema.output)});`,
    );
    L.push(`});`);
  }

  for (const opName of OP_ORDER) {
    const op = obj.operations[opName];
    if (isSkip(op)) continue;
    const run = `S.${opName === "parse" ? "parser" : opName === "decode" ? "decoder" : "encoder"}(schema)`;
    if (!isSkip(op.expression)) {
      L.push(`test(${lit(`${id} › ${opName} › expression`)}, () => {`);
      L.push(`  expect(${run}.toString()).toBe(${lit(op.expression)});`);
      L.push(`});`);
    }
    for (const [name, ex] of Object.entries(op.examples || {})) {
      if (isSkip(ex)) continue;
      L.push(`test(${lit(`${id} › ${opName} › ${name}`)}, () => {`);
      if ("error" in ex) {
        L.push(`  expect(() => ${run}(${ex.input})).toThrow(${lit(ex.error)});`);
      } else {
        L.push(`  expect(${run}(${ex.input})).toStrictEqual(${ex.output});`);
      }
      L.push(`});`);
    }
  }
  return L.join("\n") + "\n";
};

export const genPath = (id) => join(GEN_DIR, `${id}.gen_test.ts`);

export { S };
