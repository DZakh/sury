import { bench } from "@ark/attest";
import * as S from "../src/S.js";

// Baseline: load module instantiations once so per-bench counts isolate
// the type producer under test. Must not match any benchmark expression.
S.boolean;

// ─────────────────────────────────────────────────────────────────────────────
// S.schema on objects — exercises UnknownToOutput / UnknownToInput, which
// resolve each field once and delegate to ResolveObject / Flatten (S.d.ts:338).
// ─────────────────────────────────────────────────────────────────────────────

bench("S.schema object — 1 field", () => {
  return S.schema({ a: S.string });
}).types([217, "instantiations"]);

bench("S.schema object — 5 fields all required", () => {
  return S.schema({
    a: S.string,
    b: S.number,
    c: S.boolean,
    d: S.bigint,
    e: S.symbol,
  });
}).types([273, "instantiations"]);

bench("S.schema object — 10 fields all required", () => {
  return S.schema({
    a: S.string,
    b: S.number,
    c: S.boolean,
    d: S.bigint,
    e: S.symbol,
    f: S.string,
    g: S.number,
    h: S.boolean,
    i: S.bigint,
    j: S.symbol,
  });
}).types([343, "instantiations"]);

// Mixed required/optional makes ResolveObject take its optional/required split
// branch (S.d.ts:375) instead of the all-required fast path. Costlier than
// all-required — this is the hot spot from issue #166.
bench("S.schema object — 5 fields mixed required/optional", () => {
  return S.schema({
    a: S.string,
    b: S.optional(S.number),
    c: S.boolean,
    d: S.optional(S.bigint),
    e: S.symbol,
  });
}).types([10557, "instantiations"]);

// Recursion through UnknownToOutput compounds badly with nesting depth.
bench("S.schema object — nested 3 levels", () => {
  return S.schema({
    a: S.string,
    nested: S.schema({
      b: S.number,
      inner: S.schema({
        c: S.boolean,
        d: S.optional(S.string),
      }),
    }),
  });
}).types([20419, "instantiations"]);

// ─────────────────────────────────────────────────────────────────────────────
// S.schema on tuples — exercises UnknownArrayToOutput / _RestToOutput
// recursive accumulator (S.d.ts:404-419).
// ─────────────────────────────────────────────────────────────────────────────

bench("S.schema tuple — 2 items", () => {
  return S.schema([S.string, S.number] as const);
}).types([1195, "instantiations"]);

bench("S.schema tuple — 5 items", () => {
  return S.schema([
    S.string,
    S.number,
    S.boolean,
    S.bigint,
    S.symbol,
  ] as const);
}).types([1465, "instantiations"]);

bench("S.schema tuple — 10 items", () => {
  return S.schema([
    S.string,
    S.number,
    S.boolean,
    S.bigint,
    S.symbol,
    S.string,
    S.number,
    S.boolean,
    S.bigint,
    S.symbol,
  ] as const);
}).types([1935, "instantiations"]);

// ─────────────────────────────────────────────────────────────────────────────
// S.union — exercises UnknownToOutput | UnknownArrayToOutput[number]
// (S.d.ts:461-475).
// ─────────────────────────────────────────────────────────────────────────────

bench("S.union — 2 members", () => {
  return S.union([S.string, S.number]);
}).types([1277, "instantiations"]);

bench("S.union — 5 members", () => {
  return S.union([S.string, S.number, S.boolean, S.bigint, S.symbol]);
}).types([1500, "instantiations"]);

// Union of objects is the worst-case combinator: every member pays the full
// UnknownToOutput cost and the results are joined into an unflattened union.
bench("S.union — 5 object members", () => {
  return S.union([
    S.schema({ kind: "a" as const, a: S.string }),
    S.schema({ kind: "b" as const, b: S.number }),
    S.schema({ kind: "c" as const, c: S.boolean }),
    S.schema({ kind: "d" as const, d: S.bigint }),
    S.schema({ kind: "e" as const, e: S.symbol }),
  ]);
}).types([53173, "instantiations"]);

// ─────────────────────────────────────────────────────────────────────────────
// S.Output<T> / S.Input<T> extraction — measures the cost users actually pay
// at every `type Foo = S.Output<typeof fooSchema>` site (138 usages mentioned
// in issue #166).
// ─────────────────────────────────────────────────────────────────────────────

const flatSchema = S.schema({
  a: S.string,
  b: S.number,
  c: S.boolean,
  d: S.bigint,
  e: S.symbol,
});

bench("S.Output on 5-field object", () => {
  return {} as S.Output<typeof flatSchema>;
}).types([6951, "instantiations"]);

bench("S.Input on 5-field object", () => {
  return {} as S.Input<typeof flatSchema>;
}).types([5816, "instantiations"]);

const deepSchema = S.schema({
  a: S.string,
  nested: S.schema({
    b: S.number,
    inner: S.schema({
      c: S.boolean,
      d: S.optional(S.string),
    }),
  }),
});

bench("S.Output on 3-level nested object", () => {
  return {} as S.Output<typeof deepSchema>;
}).types([6951, "instantiations"]);

// ─────────────────────────────────────────────────────────────────────────────
// Compound: define + extract (the realistic per-schema cost).
// ─────────────────────────────────────────────────────────────────────────────

bench("S.schema + S.Output — 10-field object", () => {
  const s = S.schema({
    a: S.string,
    b: S.number,
    c: S.boolean,
    d: S.bigint,
    e: S.symbol,
    f: S.string,
    g: S.number,
    h: S.boolean,
    i: S.bigint,
    j: S.symbol,
  });
  return {} as S.Output<typeof s>;
}).types([7294, "instantiations"]);

// ─────────────────────────────────────────────────────────────────────────────
// S.merge — exercises the Omit<O1, keyof O2> & O2 intersection in the return
// type (S.d.ts:687). Issue #157: previous mapped-type form stripped optional
// modifiers; the fix uses an intersection. Bench captures the cost of the new
// shape.
// ─────────────────────────────────────────────────────────────────────────────

const mergeLeftRequired = S.schema({
  a: S.string,
  b: S.number,
  c: S.boolean,
});
const mergeRightRequired = S.schema({
  d: S.bigint,
  e: S.symbol,
});

bench("S.merge — 3+2 all required", () => {
  return S.merge(mergeLeftRequired, mergeRightRequired);
}).types([5461, "instantiations"]);

const mergeLeftOptional = S.schema({
  _id: S.optional(S.string),
  _rev: S.optional(S.string),
  _deleted: S.optional(S.boolean),
});
const mergeRightForOptional = S.schema({
  code: S.string,
  name: S.string,
});

bench("S.merge — 3 optional + 2 required (issue #157 case)", () => {
  return S.merge(mergeLeftOptional, mergeRightForOptional);
}).types([5956, "instantiations"]);

bench("S.merge — output extraction with optional preservation", () => {
  const m = S.merge(mergeLeftOptional, mergeRightForOptional);
  return {} as S.Output<typeof m>;
}).types([12426, "instantiations"]);

