// Competitor benchmark behind the README's "JSON serialization" table:
// JSON.stringify vs fast-json-stringify vs a prepared
// `S.encoder(schema, S.jsonString)`. Where the competitors can't represent a
// type (bigint, Uint8Array, Date), their timed loop includes the hand-written
// mapping pass a real consumer would need — Sury compiles that mapping into
// the encoder, so charging it to the competitors is the honest comparison.
//
//   pnpm --filter=sury bench:jsonstring
//
// Rebuild the library first (`pnpm --filter=sury build:entry`) when comparing
// local changes. Numbers are ns/op medians of ROUNDS runs; treat small deltas
// as noise and re-run before updating the README.

import fastJson from "fast-json-stringify";
import * as S from "../index.mjs";

const ROUNDS = 7;
const bench = (fn: () => unknown): number => {
  const run = (n: number): number => {
    const start = process.hrtime.bigint();
    for (let i = 0; i < n; i++) fn();
    return Number(process.hrtime.bigint() - start) / n;
  };
  run(10_000); // warmup
  const iterations = Math.max(1000, Math.min(2_000_000, Math.round(20_000_000 / run(1000))));
  const samples: number[] = [];
  for (let i = 0; i < ROUNDS; i++) samples.push(run(iterations));
  return samples.sort((a, b) => a - b)[Math.floor(ROUNDS / 2)]!;
};

type Case = {
  name: string;
  stringify: () => string;
  fastJson: () => string;
  sury: () => string;
};

const cases: Case[] = [];

// ── Flat object (7 fields) ───────────────────────────────────────────────────
{
  const data = {
    id: 42,
    name: "Anna Nachesa",
    email: "anna@example.com",
    age: 34,
    verified: true,
    score: 12.5,
    role: "admin",
  };
  const fj = fastJson({
    type: "object",
    properties: {
      id: { type: "integer" },
      name: { type: "string" },
      email: { type: "string" },
      age: { type: "integer" },
      verified: { type: "boolean" },
      score: { type: "number" },
      role: { type: "string" },
    },
    required: ["id", "name", "email", "age", "verified", "score", "role"],
  });
  const sury = S.encoder(
    S.schema({
      id: S.number,
      name: S.string,
      email: S.string,
      age: S.number,
      verified: S.boolean,
      score: S.number,
      role: S.string,
    }),
    S.jsonString,
  );
  cases.push({
    name: "API response (user profile, 7 fields)",
    stringify: () => JSON.stringify(data),
    fastJson: () => fj(data),
    sury: () => sury(data),
  });
}

// ── 100-item array of objects ────────────────────────────────────────────────
{
  const data = Array.from({ length: 100 }, (_, i) => ({
    id: i,
    name: `item-${i}`,
    active: i % 2 === 0,
  }));
  const fj = fastJson({
    type: "array",
    items: {
      type: "object",
      properties: {
        id: { type: "integer" },
        name: { type: "string" },
        active: { type: "boolean" },
      },
      required: ["id", "name", "active"],
    },
  });
  const sury = S.encoder(
    S.array(S.schema({ id: S.number, name: S.string, active: S.boolean })),
    S.jsonString,
  );
  cases.push({
    name: "List endpoint (100 rows)",
    stringify: () => JSON.stringify(data),
    fastJson: () => fj(data),
    sury: () => sury(data),
  });
}

// ── Dict with 50 dynamic keys ────────────────────────────────────────────────
{
  const data: Record<string, number> = {};
  for (let i = 0; i < 50; i++) data[`key-${i}`] = i * 1.5;
  const fj = fastJson({
    type: "object",
    additionalProperties: { type: "number" },
  });
  const sury = S.encoder(S.record(S.number), S.jsonString);
  cases.push({
    name: "Metrics dict (50 dynamic keys)",
    stringify: () => JSON.stringify(data),
    fastJson: () => fj(data),
    sury: () => sury(data),
  });
}

// ── bigint + Uint8Array + Date, mapping included ─────────────────────────────
{
  const data = {
    id: 12345678901234567890n,
    payload: new Uint8Array([104, 101, 108, 108, 111, 33, 33, 33]),
    createdAt: new Date("2026-01-15T10:30:00.000Z"),
    label: "event",
  };
  // JSON.stringify throws on bigint and mangles Uint8Array, and
  // fast-json-stringify expects pre-mapped strings — both pay a mapping pass.
  const map = (d: typeof data) => ({
    id: d.id.toString(),
    payload: Buffer.from(d.payload).toString(),
    createdAt: d.createdAt.toISOString(),
    label: d.label,
  });
  const fj = fastJson({
    type: "object",
    properties: {
      id: { type: "string" },
      payload: { type: "string" },
      createdAt: { type: "string" },
      label: { type: "string" },
    },
    required: ["id", "payload", "createdAt", "label"],
  });
  // Declared wire-side: string on the JSON side, rich type on the domain side.
  const sury = S.encoder(
    S.schema({
      id: S.to(S.string, S.bigint),
      payload: S.to(S.string, S.uint8Array),
      createdAt: S.to(S.string, S.date),
      label: S.string,
    }),
    S.jsonString,
  );
  cases.push({
    name: "Event: bigint id + binary payload + Date",
    stringify: () => JSON.stringify(map(data)),
    fastJson: () => fj(map(data)),
    sury: () => sury(data),
  });
}

const fmt = (ns: number): string =>
  ns < 1000 ? `${ns.toFixed(0)} ns` : `${(ns / 1000).toFixed(2)} µs`;

const main = () => {
  console.log(`node ${process.version}\n`);
  const rows: string[][] = [
    ["Encode to JSON string", "JSON.stringify", "fast-json-stringify", "Sury"],
  ];
  for (const c of cases) {
    // Guard against benchmarking functions that disagree on the output.
    const out = { stringify: c.stringify(), sury: c.sury() };
    if (JSON.stringify(JSON.parse(out.stringify)) !== JSON.stringify(JSON.parse(out.sury))) {
      throw new Error(`${c.name}: Sury output differs\n${out.stringify}\n${out.sury}`);
    }
    rows.push([c.name, fmt(bench(c.stringify)), fmt(bench(c.fastJson)), fmt(bench(c.sury))]);
  }
  const widths = rows[0]!.map((_, i) => Math.max(...rows.map((r) => r[i]!.length)));
  for (const r of rows) {
    console.log(r.map((cell, i) => cell.padEnd(widths[i]!)).join("  "));
  }
};
main();
