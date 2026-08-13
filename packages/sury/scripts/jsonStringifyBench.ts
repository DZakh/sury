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
import * as devalue from "devalue";
import * as E from "effect/Schema";
import * as z from "zod";
import { build } from "esbuild";
import { tmpdir } from "node:os";
import { join } from "node:path";

// Importing the generated module through tsx measures tsx's CJS interop, not
// typia: the same encoders run ~10x slower transpiled than bundled, for
// byte-identical output. Bundle to plain JS first so the number is typia's.
const typiaOut = join(tmpdir(), "suryBenchTypia.mjs");
await build({
  entryPoints: ["./scripts/typiaEncoders.generated.ts"],
  outfile: typiaOut,
  bundle: true,
  format: "esm",
  platform: "node",
  logLevel: "error",
});
const typia = await import(typiaOut);
import superjson from "superjson";
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
  // devalue/superjson take the domain value directly: they encode bigint,
  // Date and Uint8Array themselves, into their own wire format.
  devalue: () => string;
  superjson: () => string;
  // typia expands at build time, so its encoders live in a checked-in
  // generated module rather than being constructed here.
  typia: () => string;
  effect: () => string;
  // Zod encodes to a value, never to JSON text, so the stringify call a
  // consumer still has to make is part of what's timed.
  zod: () => string;
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
  const effEnc = E.encodeSync(
    E.fromJsonString(
      E.toCodecJson(
        E.Struct({ id: E.Number, name: E.String, email: E.String, age: E.Number, verified: E.Boolean, score: E.Number, role: E.String }),
      ),
    ),
  );
  const zodSchema = z.object({ id: z.number(), name: z.string(), email: z.string(), age: z.number(), verified: z.boolean(), score: z.number(), role: z.string() });
  cases.push({
    name: "API response (user profile, 7 fields)",
    stringify: () => JSON.stringify(data),
    fastJson: () => fj(data),
    sury: () => sury(data),
    devalue: () => devalue.stringify(data),
    superjson: () => superjson.stringify(data),
    typia: () => typia.encUser(data),
    effect: () => effEnc(data),
    zod: () => JSON.stringify(z.encode(zodSchema, data)),
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
  const effEnc = E.encodeSync(
    E.fromJsonString(E.toCodecJson(E.Array(E.Struct({ id: E.Number, name: E.String, active: E.Boolean })))),
  );
  const zodSchema = z.array(z.object({ id: z.number(), name: z.string(), active: z.boolean() }));
  cases.push({
    name: "List endpoint (100 rows)",
    stringify: () => JSON.stringify(data),
    fastJson: () => fj(data),
    sury: () => sury(data),
    devalue: () => devalue.stringify(data),
    superjson: () => superjson.stringify(data),
    typia: () => typia.encRows(data),
    effect: () => effEnc(data),
    zod: () => JSON.stringify(z.encode(zodSchema, data)),
  });
}

// ── Tagged union: 50 events ──────────────────────────────────────────────────
// fast-json-stringify resolves `anyOf` by running Ajv against each branch at
// serialization time; Sury compiles the discriminant into the encoder.
{
  const data = {
    events: Array.from({ length: 50 }, (_, i) =>
      i % 3 === 0
        ? ({ type: "click", x: i, y: i * 2 } as const)
        : i % 3 === 1
          ? ({ type: "view", path: `/page/${i}` } as const)
          : ({ type: "error", message: `boom ${i}`, code: 500 } as const),
    ),
  };
  const fj = fastJson({
    type: "object",
    properties: {
      events: {
        type: "array",
        items: {
          anyOf: [
            {
              type: "object",
              properties: {
                type: { const: "click" },
                x: { type: "number" },
                y: { type: "number" },
              },
              required: ["type", "x", "y"],
            },
            {
              type: "object",
              properties: { type: { const: "view" }, path: { type: "string" } },
              required: ["type", "path"],
            },
            {
              type: "object",
              properties: {
                type: { const: "error" },
                message: { type: "string" },
                code: { type: "number" },
              },
              required: ["type", "message", "code"],
            },
          ],
        },
      },
    },
    required: ["events"],
  });
  const sury = S.encoder(
    S.schema({
      events: S.array(
        S.union([
          S.schema({ type: "click", x: S.number, y: S.number }),
          S.schema({ type: "view", path: S.string }),
          S.schema({ type: "error", message: S.string, code: S.number }),
        ]),
      ),
    }),
    S.jsonString,
  );
  const effEnc = E.encodeSync(
    E.fromJsonString(
      E.toCodecJson(
      E.Struct({ events: E.Array(E.Union([
        E.Struct({ type: E.Literal("click"), x: E.Number, y: E.Number }),
        E.Struct({ type: E.Literal("view"), path: E.String }),
        E.Struct({ type: E.Literal("error"), message: E.String, code: E.Number }),
      ])) }),
      ),
    ),
  );
  const zodSchema = z.object({ events: z.array(z.discriminatedUnion("type", [
    z.object({ type: z.literal("click"), x: z.number(), y: z.number() }),
    z.object({ type: z.literal("view"), path: z.string() }),
    z.object({ type: z.literal("error"), message: z.string(), code: z.number() }),
  ])) });
  cases.push({
    name: "Event feed (50 tagged-union events)",
    stringify: () => JSON.stringify(data),
    fastJson: () => fj(data),
    sury: () => sury(data),
    devalue: () => devalue.stringify(data),
    superjson: () => superjson.stringify(data),
    typia: () => typia.encFeed(data),
    effect: () => effEnc(data),
    zod: () => JSON.stringify(z.encode(zodSchema, data)),
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
  const effEnc = E.encodeSync(E.fromJsonString(E.toCodecJson(E.Record(E.String, E.Number))));
  const zodSchema = z.record(z.string(), z.number());
  cases.push({
    name: "Metrics dict (50 number values)",
    stringify: () => JSON.stringify(data),
    fastJson: () => fj(data),
    sury: () => sury(data),
    devalue: () => devalue.stringify(data),
    superjson: () => superjson.stringify(data),
    typia: () => typia.encNumDict(data),
    effect: () => effEnc(data),
    zod: () => JSON.stringify(z.encode(zodSchema, data)),
  });
}

// ── Dict with 50 string values (falls back to native JSON.stringify) ─────────
{
  const data: Record<string, string> = {};
  for (let i = 0; i < 50; i++) data[`svc-${i}`] = `value-${i}`;
  const fj = fastJson({
    type: "object",
    additionalProperties: { type: "string" },
  });
  const sury = S.encoder(S.record(S.string), S.jsonString);
  const effEnc = E.encodeSync(E.fromJsonString(E.toCodecJson(E.Record(E.String, E.String))));
  const zodSchema = z.record(z.string(), z.string());
  cases.push({
    name: "Labels dict (50 string values)",
    stringify: () => JSON.stringify(data),
    fastJson: () => fj(data),
    sury: () => sury(data),
    devalue: () => devalue.stringify(data),
    superjson: () => superjson.stringify(data),
    typia: () => typia.encStrDict(data),
    effect: () => effEnc(data),
    zod: () => JSON.stringify(z.encode(zodSchema, data)),
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
  // toCodecJson carries bigint and Date on its own; only the payload needs the
  // hand-written pass, since Effect encodes binary as base64 and this wire
  // format is utf8.
  const effEnc = E.encodeSync(
    E.fromJsonString(E.toCodecJson(E.Struct({ id: E.BigInt, payload: E.String, createdAt: E.Date, label: E.String }))),
  );
  const zodSchema = z.object({
    id: z.codec(z.string(), z.bigint(), { decode: (v) => BigInt(v), encode: (v) => v.toString() }),
    payload: z.codec(z.string(), z.instanceof(Uint8Array), { decode: (v) => new TextEncoder().encode(v), encode: (v) => Buffer.from(v).toString() }),
    createdAt: z.codec(z.string(), z.date(), { decode: (v) => new Date(v), encode: (v) => v.toISOString() }),
    label: z.string(),
  });
  cases.push({
    name: "Event: bigint id + binary payload + Date",
    stringify: () => JSON.stringify(map(data)),
    fastJson: () => fj(map(data)),
    sury: () => sury(data),
    devalue: () => devalue.stringify(data),
    superjson: () => superjson.stringify(data),
    typia: () => typia.encWire(map(data)),
    effect: () => effEnc({ ...data, payload: Buffer.from(data.payload).toString() }),
    zod: () => JSON.stringify(z.encode(zodSchema, data)),
  });
}

const fmt = (ns: number): string =>
  ns < 1000 ? `${ns.toFixed(0)} ns` : `${(ns / 1000).toFixed(2)} µs`;

const main = () => {
  console.log(`node ${process.version}\n`);
  const rows: string[][] = [
    ["Encode to JSON string", "Sury", "JSON.stringify", "fast-json-stringify", "typia", "Effect", "Zod", "devalue", "superjson"],
  ];
  for (const c of cases) {
    // Guard against benchmarking functions that disagree on the output.
    // devalue/superjson are exempt: they encode into their own wire format.
    const expected = JSON.stringify(JSON.parse(c.stringify()));
    for (const k of ["sury", "fastJson", "typia", "effect", "zod"] as const) {
      const actual = JSON.stringify(JSON.parse(c[k]()));
      if (actual !== expected) {
        throw new Error(`${c.name}: ${k} output differs\n${expected}\n${actual}`);
      }
    }
    rows.push([
      c.name,
      fmt(bench(c.sury)),
      fmt(bench(c.stringify)),
      fmt(bench(c.fastJson)),
      fmt(bench(c.typia)),
      fmt(bench(c.effect)),
      fmt(bench(c.zod)),
      fmt(bench(c.devalue)),
      fmt(bench(c.superjson)),
    ]);
  }
  const widths = rows[0]!.map((_, i) => Math.max(...rows.map((r) => r[i]!.length)));
  for (const r of rows) {
    console.log(r.map((cell, i) => cell.padEnd(widths[i]!)).join("  "));
  }
};
main();
