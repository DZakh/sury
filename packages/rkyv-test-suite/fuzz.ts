// Holds S.rkyv to the Rust rkyv crate. Each seed generates a batch of random
// types and compiles them, as Rust, into the oracle (rust/src/main.rs). Every
// generated value must then:
//  - encode with S.rkyv to bytes rkyv's validator accepts,
//  - read back in Rust as the value that went in (by the canonical dump),
//  - be exactly the bytes rkyv's own serializer writes for it,
//  - decode with S.rkyv to the value that went in.
// Each value's bytes are then mutated, and S.rkyv must accept exactly what
// rkyv's validator accepts, reading the same value when both do.
//
//   pnpm rkyv:fuzz [--seed=N] [--seeds=N] [--types=N] [--values=N]

import { execFileSync } from "node:child_process";
import { writeFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import * as S from "sury";
import { describe, dump, genType, genValue, rng, type T, toRust, toSchema } from "./model";

const args = Object.fromEntries(
  process.argv.slice(2).map((arg) => {
    const [k, v] = arg.replace(/^--/, "").split("=");
    return [k!, Number(v)];
  }),
);
const firstSeed = args.seed ?? 1;
const seeds = args.seeds ?? 3;
const typeCount = args.types ?? 40;
const valueCount = args.values ?? 25;
const mutations = 12;

const U = undefined;
const root = dirname(fileURLToPath(import.meta.url));
const crate = join(root, "rust");

const hex = (bytes: Uint8Array) => Buffer.from(bytes).toString("hex");

type Query = { type: number; bytes: Uint8Array; expect?: { dump: string }; mutated?: boolean; value?: unknown };

const mutate = (r: ReturnType<typeof rng>, bytes: Uint8Array): Uint8Array => {
  const out = Uint8Array.from(bytes);
  switch (r.int(0, 5)) {
    case 0:
    case 1:
      if (out.length) out[r.int(0, out.length - 1)] ^= 1 << r.int(0, 7);
      return out;
    case 2:
      if (out.length) out[r.int(0, out.length - 1)] = r.pick([0, 1, 2, 0x7f, 0x80, 0xc0, 0xff, r.int(0, 255)]);
      return out;
    case 3:
      return out.slice(r.int(0, out.length));
    case 4: {
      const pad = r.pick([1, 3, 4, 8, 16]);
      const grown = new Uint8Array(out.length + pad);
      grown.set(out, pad);
      return grown;
    }
  }
  return Uint8Array.from({ length: r.int(0, 24) }, () => r.int(0, 255));
};

let failures = 0;
let checked = 0;
let agreedRejects = 0;
let agreedAccepts = 0;
let narrowed = 0;
const shown = new Map<string, number>();
const report = (message: string) => {
  failures++;
  // Two per type: one root cause tends to repeat on every value of it.
  const type = message.split("\n")[0]!.replace(/ \(.*/, "").replace(/:.*/, "");
  const count = shown.get(type) ?? 0;
  shown.set(type, count + 1);
  if (count < 2 && shown.size <= 15) console.error(message);
};

for (let seed = firstSeed; seed < firstSeed + seeds; seed++) {
  const r = rng(seed);
  const types: T[] = [];
  const defs: string[] = [];
  const roots: string[] = [];
  for (let idx = 0; idx < typeCount; idx++) {
    let t = r.chance(0.7) ? ({ k: "struct", name: `Root${seed}_${idx}`, f: Array.from({ length: r.int(1, 5) }, (_, j) => [`f${j}`, genType(r, 1)] as [string, T]) } as T) : genType(r);
    // A root Uint8Array is an archive already, as S.protobuf takes one.
    if (t.k === "bytes") t = { k: "vec", i: { k: "num", r: "u8", as: "int32" } };
    types.push(t);
    roots.push(toRust(t, defs));
  }
  writeFileSync(
    join(crate, "src/generated.rs"),
    `${defs.join("\n")}\nfn dispatch(id: usize, bytes: &[u8]) -> String {\n  match id {\n${roots.map((rt, idx) => `    ${idx} => check::<${rt}>(bytes),`).join("\n")}\n    _ => unreachable!(),\n  }\n}\n`,
  );
  const started = Date.now();
  execFileSync("cargo", ["build", "--release", "--quiet"], { cwd: crate, stdio: "inherit" });
  console.log(`seed ${seed}: ${typeCount} types compiled in ${((Date.now() - started) / 1000).toFixed(1)}s`);

  const queries: Query[] = [];
  const ops = types.map((t, idx) => {
    try {
      const schema = toSchema(t);
      return { schema, encode: S.encodeOrThrow(schema, S.rkyv), decode: S.decodeOrThrow(S.rkyv, schema) };
    } catch (error) {
      report(`seed ${seed} type ${idx}: compiling failed: ${(error as Error).message}\n  ${describe(t)}`);
      return U;
    }
  });
  types.forEach((t, idx) => {
    const op = ops[idx];
    if (op === U) return;
    for (let n = 0; n < valueCount; n++) {
      const value = genValue(r, t);
      let bytes: Uint8Array;
      try {
        bytes = op.encode(value);
      } catch (error) {
        report(`seed ${seed} type ${idx}: encoding failed: ${(error as Error).message}\n  ${describe(t)}\n  value ${String(value)}`);
        continue;
      }
      const expected = dump(t, value);
      try {
        const back = dump(t, op.decode(bytes));
        if (back !== expected) report(`seed ${seed} type ${idx}: JS round trip reads ${back}, wrote ${expected}\n  ${describe(t)}`);
      } catch (error) {
        report(`seed ${seed} type ${idx}: decoding its own bytes failed: ${(error as Error).message}\n  ${describe(t)}\n  bytes ${hex(bytes)}`);
      }
      queries.push({ type: idx, bytes, expect: { dump: expected }, value });
      for (let m = 0; m < mutations; m++) queries.push({ type: idx, bytes: mutate(r, bytes), mutated: true });
    }
  });

  const output = execFileSync(join(crate, "target/release/rkyv-oracle"), {
    input: queries.map((q) => `${q.type} ${hex(q.bytes)}`).join("\n") + "\n",
    maxBuffer: 1 << 30,
  })
    .toString()
    .trimEnd()
    .split("\n");

  queries.forEach((q, idx) => {
    checked++;
    const answer = output[idx]!;
    const t = types[q.type]!;
    const where = `seed ${seed} type ${q.type} (${roots[q.type]})\n  ${describe(t)}\n  bytes ${hex(q.bytes)}`;
    const rustOk = answer.startsWith("OK ");
    const [, rustDump, rustBytes] = rustOk ? answer.split(" ") : [];
    if (!q.mutated) {
      if (!rustOk) return report(`${where}\n  rkyv refuses what S.rkyv wrote: ${answer}`);
      if (rustDump !== q.expect!.dump) return report(`${where}\n  rkyv reads ${rustDump}\n  S.rkyv wrote ${q.expect!.dump}`);
      if (rustBytes !== hex(q.bytes)) return report(`${where}\n  rkyv serializes ${rustBytes}`);
      return;
    }
    let jsDump: string | undefined;
    let jsError: string | undefined;
    try {
      jsDump = dump(t, ops[q.type]!.decode(q.bytes));
    } catch (error) {
      jsError = (error as Error).message;
    }
    // A JS target narrower than the Rust type refuses what it can't hold: an
    // i64 past 2^53 read into a number, a NaN read into S.number.
    if (rustOk && jsDump === U && /doesn't fit a number|received NaN/.test(jsError!)) return void narrowed++;
    if (rustOk && jsDump === U) return report(`${where}\n  rkyv accepts (${rustDump}), S.rkyv refuses: ${jsError}`);
    if (!rustOk && jsDump !== U) return report(`${where}\n  S.rkyv accepts (${jsDump}), rkyv refuses: ${answer}`);
    if (rustOk) {
      agreedAccepts++;
      if (rustDump !== jsDump) return report(`${where}\n  rkyv reads ${rustDump}\n  S.rkyv reads ${jsDump}`);
    } else agreedRejects++;
  });
}

console.log(`${checked} byte strings checked against rkyv 0.8.18: ${agreedAccepts} mutated accepted by both, ${agreedRejects} rejected by both, ${narrowed} refused by a narrower JS target, ${failures} disagreements`);
if (failures) process.exit(1);
