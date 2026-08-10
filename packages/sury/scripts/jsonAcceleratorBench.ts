// Sury against elysiajs/json-accelerator on json-accelerator's own benchmark
// models and harness (mitata, same TypeBox models and values, taken verbatim
// from its benchmarks/{small,medium-manual,array,large}.ts).
//
//   pnpm --filter=sury bench:accelerator            # their harness, TextEncoder included
//   RAW=1 pnpm --filter=sury bench:accelerator      # string production alone
//   AUTO=1 pnpm --filter=sury bench:accelerator     # ignore per-case sanitize options
//
// Their harness times `new TextEncoder().encode(stringify(value))`, because
// json-accelerator also ships a direct-to-bytes encoder. TextEncoder costs more
// than the stringification on every machine tested so far, so it compresses
// every difference toward 1.0x — `RAW=1` is the comparison that says something
// about the compilers.
//
// The TypeBox model goes into Sury as plain JSON Schema, which is the whole
// point of the comparison: the same document both libraries consume.

import { bench, run, barplot, summary, compact } from "mitata";
import { createAccelerator } from "json-accelerator";
import fastJson from "fast-json-stringify";
import { Type as t, type TSchema } from "@sinclair/typebox";
import * as S from "../index.mjs";

// fromJSONSchema treats `allOf` as a validation-only refinement, so an object
// whose shape lives entirely in `allOf` — what TypeBox's t.Intersect emits —
// compiles to a shapeless object and encodes as `{}`. Merging the members into
// the parent is what makes the intersect cases comparable at all; drop this
// once fromJSONSchema folds object `allOf` members into the shape.
const flattenAllOf = (schema: any): any => {
  if (Array.isArray(schema)) return schema.map(flattenAllOf);
  if (!schema || typeof schema !== "object") return schema;
  const out: any = {};
  for (const [k, v] of Object.entries(schema)) {
    if (k !== "allOf") out[k] = flattenAllOf(v);
  }
  for (const member of (schema.allOf ?? []).map(flattenAllOf)) {
    out.properties = { ...out.properties, ...member.properties };
    out.required = [...(out.required ?? []), ...(member.required ?? [])];
  }
  return out;
};

type Case = {
  name: string;
  model: TSchema;
  value: unknown;
  options?: Parameters<typeof createAccelerator>[1];
};

const cases: Case[] = [
  {
    name: "small",
    model: t.Object({
      id: t.Number(),
      name: t.String(),
      bio: t.String(),
      metadata: t.Object({ alias: t.String(), country: t.String() }),
    }),
    value: {
      id: 1,
      name: "SaltyAom",
      bio: "I like train",
      metadata: { alias: "SaltyAom", country: "Thailand" },
    },
  },
  {
    name: "medium-manual",
    // `sanitize: "manual"` makes json-accelerator emit no escape check at all
    // for strings not marked `sanitize: true` — the string splices into the
    // template literal raw. Sury escapes unconditionally, so this case is a
    // correctness difference as much as a speed one; AUTO=1 drops the option
    // and puts both on escape-everything.
    options: { sanitize: "manual" } as any,
    model: t.Object({
      id: t.Number(),
      name: t.Literal("SaltyAom"),
      bio: t.String({ sanitize: true } as any),
      user: t.Object({ name: t.String(), password: t.String() }),
      playing: t.Optional(t.String()),
      games: t.Array(
        t.Object({
          name: t.String(),
          hoursPlay: t.Number({ default: 0 }),
          tags: t.Array(t.String()),
        }),
      ),
      metadata: t.Intersect([
        t.Object({ alias: t.String() }),
        t.Object({ country: t.Union([t.String(), t.Null()]) }),
      ]),
      social: t.Optional(
        t.Object({
          facebook: t.Optional(t.String()),
          twitter: t.Optional(t.String()),
          youtube: t.Optional(t.String()),
        }),
      ),
    }),
    value: {
      id: 1,
      name: "SaltyAom",
      bio: "I like train\n",
      user: { name: "SaltyAom", password: "123456" },
      games: [
        {
          name: "MiSide",
          hoursPlay: 17,
          tags: ["Psychological Horror", "Cute", "Dating Sim"],
        },
        {
          name: "Strinova",
          hoursPlay: 365,
          tags: ["Free to Play", "Anime", "Third-Person Shooter"],
        },
        {
          name: "Tom Clancy's Rainbow Six Siege",
          hoursPlay: 287,
          tags: ["FPS", "Multiplayer", "Tactical"],
        },
      ],
      metadata: { alias: "SaltyAom", country: "Thailand" },
      social: { twitter: "SaltyAom" },
    },
  },
  {
    name: "array",
    model: t.Object({
      ids: t.Array(t.Number()),
      names: t.Array(t.String()),
      games: t.Array(t.Object({ name: t.String(), tags: t.Array(t.String()) })),
    }),
    value: {
      ids: [1, 2, 3],
      names: ["SaltyAom", "SaltyAom", "SaltyAom"],
      games: [
        { name: "MiSide", tags: ["Psychological Horror", "Cute", "Dating Sim"] },
        { name: "Strinova", tags: ["Free to Play", "Anime", "Third-Person Shooter"] },
        {
          name: "Tom Clancy's Rainbow Six Siege",
          tags: ["FPS", "Multiplayer", "Tactical"],
        },
      ],
    },
  },
  {
    name: "large",
    model: t.Array(
      t.Object({
        id: t.Number(),
        name: t.String(),
        bio: t.String(),
        user: t.Object({
          name: t.String(),
          password: t.String(),
          email: t.Optional(t.String()),
          age: t.Optional(t.Number()),
          avatar: t.Optional(t.String()),
          cover: t.Optional(t.String()),
        }),
        playing: t.Optional(t.String()),
        wishlist: t.Optional(t.Array(t.Number())),
        games: t.Array(
          t.Object({
            id: t.Number(),
            name: t.String(),
            hoursPlay: t.Optional(t.Number({ default: 0 })),
            tags: t.Array(t.Object({ name: t.String(), count: t.Number() })),
          }),
        ),
        metadata: t.Intersect([
          t.Object({ alias: t.String() }),
          t.Object({
            country: t.Union([t.String(), t.Null()]),
            region: t.Optional(t.String()),
          }),
        ]),
        social: t.Optional(
          t.Object({
            facebook: t.Optional(t.String()),
            twitter: t.Optional(t.String()),
            youtube: t.Optional(t.String()),
          }),
        ),
      }),
    ),
    value: [
      {
        id: 1,
        name: "SaltyAom",
        bio: "I like train",
        user: {
          name: "SaltyAom",
          password: "123456",
          avatar: "https://avatars.githubusercontent.com/u/35027979?v=4",
          cover: "https://saltyaom.com/cosplay/pekomama.webp",
        },
        playing: "Strinova",
        wishlist: [4_154_456, 2_345_345],
        games: [
          {
            id: 4_154_456,
            name: "MiSide",
            hoursPlay: 17,
            tags: [
              { name: "Psychological Horror", count: 236_432 },
              { name: "Cute", count: 495_439 },
              { name: "Dating Sim", count: 395_532 },
            ],
          },
          {
            id: 4_356_345,
            name: "Strinova",
            hoursPlay: 365,
            tags: [
              { name: "Free to Play", count: 205_593 },
              { name: "Anime", count: 504_304 },
              { name: "Third-Person Shooter", count: 395_532 },
            ],
          },
          {
            id: 2_345_345,
            name: "Tom Clancy's Rainbow Six Siege",
            hoursPlay: 287,
            tags: [
              { name: "FPS", count: 855_324 },
              { name: "Multiplayer", count: 456_567 },
              { name: "Tactical", count: 544_467 },
            ],
          },
        ],
        metadata: { alias: "SaltyAom", country: "Thailand", region: "Asia" },
        social: { twitter: "SaltyAom" },
      },
      {
        id: 2,
        name: "VLost",
        bio: "ไม่พี่คืองี้",
        user: { name: "nattapon_kub", password: "123456" },
        games: [
          {
            id: 4_154_456,
            name: "MiSide",
            hoursPlay: 17,
            tags: [
              { name: "Psychological Horror", count: 236_432 },
              { name: "Cute", count: 495_439 },
              { name: "Dating Sim", count: 395_532 },
            ],
          },
          {
            id: 4_356_345,
            name: "Strinova",
            hoursPlay: 365,
            tags: [
              { name: "Free to Play", count: 205_593 },
              { name: "Anime", count: 504_304 },
              { name: "Third-Person Shooter", count: 395_532 },
            ],
          },
        ],
        metadata: { alias: "vlost", country: "Thailand" },
      },
      {
        id: 2,
        name: "eika",
        bio: "こんにちわ！",
        user: { name: "ei_ka", password: "123456" },
        games: [
          {
            id: 4_356_345,
            name: "Strinova",
            hoursPlay: 365,
            tags: [
              { name: "Free to Play", count: 205_593 },
              { name: "Anime", count: 504_304 },
              { name: "Third-Person Shooter", count: 395_532 },
            ],
          },
        ],
        metadata: { alias: "eika", country: "Japan" },
      },
    ],
  },
];

const enc = new TextEncoder();
const wrap = process.env.RAW
  ? (fn: () => string) => fn
  : (fn: () => string) => () => enc.encode(fn());
const only = process.argv[2];

const main = async () => {
  for (const c of cases) {
    if (only && c.name !== only) continue;

    const accelerate = createAccelerator(
      c.model,
      process.env.AUTO ? undefined : c.options,
    ) as (v: unknown) => string;
    const fjs = fastJson(c.model as any);
    const suryModel = flattenAllOf(c.model);
    // Both directions, because they are not the same function: the decode
    // direction validates the input, the encode direction trusts it — except
    // where a union or an optional forces a check to pick the branch.
    const suryDecode = S.decoder(S.fromJSONSchema(suryModel), S.jsonString);
    const suryEncode = S.encoder(S.fromJSONSchema(suryModel), S.jsonString);

    const expected = JSON.stringify(c.value);
    for (const [label, fn] of [
      ["json-accelerator", () => accelerate(c.value)],
      ["sury (decode)", () => suryDecode(c.value as never)],
      ["sury (encode)", () => suryEncode(c.value as never)],
    ] as const) {
      const got = fn();
      if (got !== expected) {
        throw new Error(`${c.name}: ${label} output differs\n  got ${got}\n  exp ${expected}`);
      }
    }

    console.log(`\n=== ${c.name} (${expected.length} bytes) ===`);
    compact(() => {
      barplot(() => {
        summary(() => {
          bench("JSON Accelerator", wrap(() => accelerate(c.value)));
          bench("Sury (decode, validating)", wrap(() => suryDecode(c.value as never)));
          bench("Sury (encode)", wrap(() => suryEncode(c.value as never)));
          bench("Fast JSON Stringify", wrap(() => fjs(c.value)));
          bench("JSON Stringify", wrap(() => JSON.stringify(c.value)));
        });
      });
    });
    await run();
  }
};

void main();
