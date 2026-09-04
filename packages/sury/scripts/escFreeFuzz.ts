// Fuzz for jsonString's escape-free splice (the `escapeFree` field in src/base.ts).
//
// An `escapeFree`-flagged format is spliced between bare quotes with no escaping, so a
// value it accepts carrying `"`, `\`, a control char or a lone surrogate would
// make the encoder emit syntactically broken JSON — a much louder bug than the
// over-escaping it replaces. The guarantee is a property of the format's
// pattern, which sits right next to the flag in refinements.ts but can still
// be widened without re-checking it. This is what keeps the two in sync.
//
//   pnpm --filter=sury fuzz:escfree
//   pnpm --filter=sury fuzz:escfree --cases=2000000 --seed=7
//
// Which formats are raw-spliced is read off the emitted code, not restated
// here, so a format flagged without a seed fails as unfuzzed rather than
// passing silently. For each one it hunts, from known-valid seeds, for an
// accepted value needing escapes: every single-character insert and replace at
// every position (exhaustive), then seeded random multi-character mutation.
//
// Generation is seeded, so a reported hit reproduces from its seed alone.

import * as S from "../index.mjs";

const arg = (name: string, fallback: number): number => {
  const hit = process.argv.find((a) => a.startsWith(`--${name}=`));
  return hit === undefined ? fallback : Number(hit.slice(name.length + 3));
};
const cases = arg("cases", 400_000);
const seed = arg("seed", 1);

// xorshift32, so a run reproduces from its seed on any platform.
let state = seed >>> 0;
const rnd = (n: number): number => {
  state ^= state << 13;
  state ^= state >>> 17;
  state ^= state << 5;
  return (state >>> 0) % n;
};

// Exactly what jsonString's escape helper escapes, and so exactly what a raw
// splice may not carry.
const needsEscape = (value: string): boolean =>
  /[\u0000-\u001f"\\\ud800-\udfff]/.test(value);

const DANGER = ['"', "\\", "\u0000", "\u0007", "\u001f", "\n", "\r", "\t", "\ud800", "\udfff"];

// Keyed by format name.
const SEEDS: Record<string, string[]> = {
  "date-time": ["2026-01-15T10:30:00.000Z", "2026-01-15t10:30:00z", "2026-12-31T23:59:60Z"],
  date: ["2026-01-15", "0000-01-01", "9999-12-31"],
  duration: ["P1Y2M3DT4H5M6S", "PT1H", "P1W", "PT0.5S"],
  uuid: ["123e4567-e89b-12d3-a456-426614174000", "00000000-0000-0000-0000-000000000000"],
  email: ["a@b.co", "first.last@example.com", "x+y@sub.domain.org"],
  hostname: ["example.com", "a-b.c", "x"],
  ipv4: ["127.0.0.1", "255.255.255.255", "0.0.0.0"],
  ipv6: ["::1", "2001:db8::8a2e:370:7334", "::ffff:127.0.0.1"],
  uri: ["https://example.com/a?b=c#d", "mailto:a@b.co", "urn:isbn:0451450523"],
  "uri-reference": ["/a/b?c#d", "https://example.com", "//host/path", "?q", "#f", ""],
  base64: ["ZGF0YQ==", "aGkh", "iVBORw==", ""],
  base64url: ["ZGF0YQ", "aGkh", "a-b_", ""],
  cuid: ["ckopqwooh000001la8mbi2im9", "c123456", "C1234567"],
  cuid2: ["tz4a98xxat96iws9zmbrgj3a", "a", "z0"],
  ulid: ["01ARZ3NDEKTSV4RRFFQ69G5FAV", "01arz3ndektsv4rrffq69g5fav", "7ZZZZZZZZZZZZZZZZZZZZZZZZZ"],
  ksuid: ["0ujtsYcgvSTl8PAuAdqWYSMnLOv", "aaaaaaaaaaaaaaaaaaaaaaaaaaa"],
  xid: ["9m4e2mr0ui3e8a215n4g", "9M4E2MR0UI3E8A215N4G"],
  nanoid: ["V1StGXR8_Z5jdHi6B-myT", "_-_-_", "a"],
  uuidv4: ["9b2f4f0e-6a1e-4c3b-8b7a-1f2e3d4c5b6a", "9b2f4f0e-6a1e-4c3b-Bb7a-1f2e3d4c5b6a"],
  uuidv6: ["1ef21d2f-1207-6ea0-8b7a-1f2e3d4c5b6a"],
  uuidv7: ["0192f0e1-2b3c-7d4e-8b7a-1f2e3d4c5b6a"],
  e164: ["+14155552671", "+1234567", "+123456789012345"],
  mac: ["00:1b:44:11:3a:b7", "00-1b-44-11-3a-b7", "001b.4411.3ab7", "001b:4411:3ab7:c8d9"],
  hex: ["deadBEEF", "0", "abc"],
  cidrv4: ["192.168.0.0/16", "0.0.0.0/0", "255.255.255.255/32"],
  cidrv6: ["2001:db8::/32", "::/0", "FE80::/10"],
  "http-url": ["https://example.com/a?b=c#d", "http://example.com", "HTTP://EXAMPLE.COM"],
};

// `S.Schema` is a union over the `type` variants, so the string arm's `format`
// is only reachable once narrowed — which the filter below does at runtime.
type StringSchema = S.Schema<string, string> & {
  type: "string";
  format: S.StringFormat;
};

// `content` is internal (it marks a bytes carrier), so it is the one field this
// script has to reach past the public type for. Named rather than an `as any`
// on the whole schema: everything else here goes through the public API, so a
// rename of it is a compile error instead of a runtime surprise.
const contentOf = (schema: StringSchema): unknown =>
  (schema as unknown as { content?: unknown }).content;

const stringFormatSchemas = Object.entries(S as Record<string, unknown>).filter(
  (entry): entry is [string, StringSchema] => {
    const v = entry[1] as StringSchema | null;
    return (
      typeof v === "object" &&
      v !== null &&
      v.type === "string" &&
      typeof v.format === "string" &&
      v.format !== "json"
    );
  },
);

// One compiled validator per schema: `inputValidator` builds an operation, and
// building it per candidate would dominate a 400k-case run.
const validators = new Map<StringSchema, (value: string) => boolean>();

// No try/catch. A validator that fails to compile, or an API that stopped
// existing, is a broken harness — it has to crash with a stack trace rather
// than read as "this format rejected the value". Swallowing that is how a
// rename left every format silently unfuzzed for nine commits.
const accepts = (schema: StringSchema, value: string): boolean => {
  let validate = validators.get(schema);
  if (!validate) {
    validate = S.inputValidator(schema);
    validators.set(schema, validate);
  }
  return validate(value);
};

const failures: string[] = [];
const rows: string[][] = [];
// Every format that reached the seeded phase, so the table below can be
// checked against the set it is supposed to mirror.
const rawSpliced = new Set<string>();

for (const [name, schema] of stringFormatSchemas) {
  const format = schema.format;
  // Ground truth: a raw-spliced format emits `"\""+i+"\""`, an escaped one a
  // call into the embedded helper.
  // A content format's link to jsonString has two readings (CONTENT_CODEC_SPEC.md)
  // and asks rather than guessing, so name the one this script is about: the
  // value spliced into a document.
  const emitted = String(
    contentOf(schema)
      ? S.decoder(S.to(schema, S.jsonString, { decode: "pack", encode: "unpack" }))
      : S.encoder(schema, S.jsonString),
  );
  if (!emitted.includes(`"\\""+`)) {
    rows.push([name, format, "escape helper", "—"]);
    continue;
  }

  rawSpliced.add(format);

  const seeds = (SEEDS[format] ?? []).filter((v) => accepts(schema, v));
  if (!seeds.length) {
    failures.push(
      `${name} (${format}): raw-spliced but this script has no valid seed for it — ` +
        `add seeds here, or clear its escFree flag in refinements.ts`,
    );
    rows.push([name, format, "RAW SPLICE", "NO SEED — unfuzzed"]);
    continue;
  }

  let hit: string | undefined;
  outer: for (const s of seeds) {
    for (const c of DANGER) {
      for (let i = 0; i <= s.length; i++) {
        for (const cand of [s.slice(0, i) + c + s.slice(i), s.slice(0, i) + c + s.slice(i + 1)]) {
          if (needsEscape(cand) && accepts(schema, cand)) {
            hit = cand;
            break outer;
          }
        }
      }
    }
  }
  for (let n = 0; n < cases && hit === undefined; n++) {
    let v = seeds[rnd(seeds.length)]!;
    for (let k = rnd(3) + 1; k--; ) {
      const c = DANGER[rnd(DANGER.length)]!;
      const i = rnd(v.length + 1);
      const mode = rnd(3);
      v =
        mode === 0
          ? v.slice(0, i) + c + v.slice(i)
          : mode === 1
            ? v.slice(0, i) + c + v.slice(i + 1)
            : c + v + c;
    }
    if (needsEscape(v) && accepts(schema, v)) hit = v;
  }

  if (hit === undefined) {
    rows.push([name, format, "raw splice", `escape-free (${seeds.length} seeds)`]);
  } else {
    failures.push(`${name} (${format}): accepts ${JSON.stringify(hit)}, which needs escaping`);
    rows.push([name, format, "RAW SPLICE", `ACCEPTS ${JSON.stringify(hit)}`]);
  }
}

// The table has to mirror the raw-spliced set exactly. A missing key is caught
// above, per format; a key for a format that stopped being raw-spliced (or
// stopped existing) is caught here, so the table can't quietly rot into a list
// of names nothing reads.
for (const format of Object.keys(SEEDS)) {
  if (!rawSpliced.has(format)) {
    failures.push(
      `${format}: seeds for a format that is not raw-spliced — drop them, or ` +
        `restore the escFree flag in refinements.ts`,
    );
  }
}

const widths = [0, 1, 2, 3].map((i) => Math.max(...rows.map((r) => r[i]!.length)));
for (const r of rows) console.log(r.map((c, i) => c.padEnd(widths[i]!)).join("  "));

if (failures.length) {
  console.error(`\n${failures.length} failure(s):`);
  for (const f of failures) console.error(`  ${f}`);
  process.exit(1);
}
console.log(`\nok — every raw-spliced format is escape-free (seed ${seed}, ${cases} random cases)`);
