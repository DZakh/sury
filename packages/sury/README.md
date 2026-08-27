[![CI](https://github.com/DZakh/sury/actions/workflows/ci.yml/badge.svg)](https://github.com/DZakh/sury/actions/workflows/ci.yml)
[![sury npm](https://img.shields.io/npm/dm/sury?label=Sury)](https://www.npmjs.com/package/sury)
[![rescript-schema npm](https://img.shields.io/npm/dm/rescript-schema?label=ReScript%20Schema)](https://www.npmjs.com/package/rescript-schema)
[![license](https://img.shields.io/npm/l/sury)](https://github.com/DZakh/sury/blob/main/LICENSE)

# Sury 🧬

**Next-gen schemas, faster than hand-written code.**

Declare your data model once, in TypeScript or ReScript. Decoders and encoders are pipelines of schemas - a wire schema on one side, the types you work with on the other - each JIT-specialized into a function written for exactly your shape.

```sh
npm install sury
```

## Why Sury

```ts
import * as S from "sury";

// Declare your data model - unions, constraints and metadata in one place
const eventSchema = S.union([
  {
    type: "user.created",
    id: S.bigint, // can't exist in JSON - carried as "42" on the wire
    tags: S.array({ name: S.string }).with(S.nonEmpty, "Add at least one tag"),
  },
  { type: "user.deleted", id: S.bigint },
]).with(S.meta, { description: "User lifecycle event" });

// Types you can read on hover
type Event = S.Output<typeof eventSchema>;
//   ^? { type: "user.created"; id: bigint; tags: { name: string }[] }
//      | { type: "user.deleted"; id: bigint }

// One schema, both directions
const parseEvent = S.decoder(S.jsonString, eventSchema);
parseEvent('{"type":"user.created","id":"42","tags":[{"name":"vip"}]}');
// => { type: "user.created", id: 42n, tags: [{ name: "vip" }] }

// ...and encode back out - faster than JSON.stringify (see Comparison)
S.encoder(eventSchema, S.jsonString)({ type: "user.deleted", id: 7n });
// => '{"type":"user.deleted","id":"7"}'

// Errors point into the matched variant, with your message
parseEvent('{"type":"user.created","id":"42","tags":[]}');
// => throws S.Error: Failed at ["tags"]: Add at least one tag

// Rather a result than an exception? S.safe wraps any block
const result = S.safe(() => parseEvent('{"type":"user.deleted"}'));
result.success; // => false, with result.error: Failed at ["id"]: ...

// Swap the wire, keep the model: the same event inside base64url
const b64Event = S.base64url.with(S.to, S.jsonString.with(S.to, eventSchema));
S.encoder(b64Event)({ type: "user.deleted", id: 7n });
// => "eyJ0eXBlIjoidXNlci5kZWxldGVkIiwiaWQiOiI3In0"

// Standard Schema: accepted by tRPC, Hono, TanStack and 28+ more
eventSchema["~standard"].validate({ type: "user.deleted", id: 7n });
// => { value: { type: "user.deleted", id: 7n } }

// JSON Schema out - it describes the wire, so id is { type: "string" }...
S.toJSONSchema(S.json.with(S.to, eventSchema));
// => { anyOf: [...], description: "User lifecycle event", ... }

// ...and in - fully typed, scored against the official test suite in CI
const emailSchema = S.fromJSONSchema({ type: "string", format: "email" });
S.parser(emailSchema)("hi@sury.dev"); // => "hi@sury.dev", typed as string

// Wires today: JSON, JSON string, base64, base64url, Uint8Array, File, Blob
// Coming next: env, FormData, protobuf
```

Everything above is JIT-specialized with `new Function` into the functions you'd write by hand - [see the code](#the-code-a-schema-turns-into), the [benchmarks](#comparison), and [why `new Function` is fine](#does-it-really-use-new-function) - and a schema with a parser ships in 7.9 kB min+gzip, tree-shakable, [string formats](https://github.com/DZakh/sury/blob/main/docs/js-usage.md#string-formats) included.

**Full API reference:** [JS/TS](https://github.com/DZakh/sury/blob/main/docs/js-usage.md) · [ReScript](https://github.com/DZakh/sury/blob/main/docs/rescript-usage.md) · [PPX](https://github.com/DZakh/sury/blob/main/packages/sury-ppx/README.md)

> Formerly published as **rescript-schema**. Sury is plain JavaScript - the ReScript compiler is not involved - with first-class ReScript bindings on the same package.

## Recipes

Scenarios that usually need a helper library or hand-written glue, as plain schemas.

### Decode a JWT payload

A JWT segment is base64url text holding JSON. Declare the layers and get both directions:

```ts
const claimsSchema = S.base64url.with(
  S.to,
  S.jsonString.with(S.to, S.schema({ sub: S.string, exp: S.number })),
);

S.parser(claimsSchema)("eyJzdWIiOiJhIiwiZXhwIjoxNzM1Njg2MDAwfQ");
// => { sub: "a", exp: 1735686000 }
```

### Read an uploaded config file

A `File`'s content is only readable asynchronously, so the pipeline is async as a whole - and the sync parser refuses at creation instead of failing later:

```ts
const configSchema = S.file.with(
  S.to,
  S.jsonString.with(S.to, S.schema({ theme: S.string })),
);

await S.asyncParser(configSchema)(new File(['{"theme":"dark"}'], "config.json"));
// => { theme: "dark" }

S.parser(configSchema);
// => throws: Invalid async during sync operation
```

### Type your environment variables

`process.env` is strings; your config isn't. Pipe from `S.record(S.string)` and the coercions are inferred - extra variables pass through untouched:

```ts
const envSchema = S.record(S.string).with(
  S.to,
  S.schema({
    PORT: S.port,
    DEBUG: S.string.with(S.to, S.boolean),
  }),
);

S.parser(envSchema)(process.env);
// => { PORT: 8080, DEBUG: true }

S.parser(envSchema)({ PORT: "99999", DEBUG: "true" });
// => throws S.Error: Failed at ["PORT"]: Expected port, received 99999
```

### ISO strings <-> `Date`

```ts
const at = S.string.with(S.to, S.date);

S.parser(at)("2026-08-26T12:00:00.000Z"); // => Date
S.encoder(at)(new Date("2026-08-26T12:00:00.000Z")); // => "2026-08-26T12:00:00.000Z"
```

### Columnar data to rows

`S.compactColumns` maps columnar arrays - of `S.json` values here - to rows, in both directions:

```ts
const cityRow = S.schema({ id: S.bigint, city: S.string });
const rows = S.compactColumns(S.json).with(S.to, S.array(cityRow));

S.parser(rows)([["1", "2"], ["Tbilisi", "Batumi"]]);
// => [{ id: 1n, city: "Tbilisi" }, { id: 2n, city: "Batumi" }]

S.encoder(rows)([{ id: 1n, city: "Tbilisi" }, { id: 2n, city: "Batumi" }]);
// => [["1", "2"], ["Tbilisi", "Batumi"]]
```

More building blocks in the [API reference](https://github.com/DZakh/sury/blob/main/docs/js-usage.md).

## Comparison

Sury is the fastest composable validation library in the ecosystem, because schemas are JIT-specialized with `new Function` rather than interpreted.

It's also small. Instead of a few large classes with many methods, the API and source are built from many small, independent functions, each with a single task. A bundler follows your imports and drops everything you don't use, which can cut the shipped size by up to 2× compared to [Zod](https://github.com/colinhacks/zod). (The approach is borrowed from [Valibot](https://github.com/fabian-hiller/valibot), which pioneered it.)

### The code a schema turns into

`parseEvent` from the tour above isn't an interpreter walking a schema tree. It's a function Sury specialized for exactly that shape: the union dispatches on the discriminant, the inferred `bigint` coercion is inlined as a bare `BigInt()` call, and your `nonEmpty` message is a plain length check - `S.jsonString` -> union -> fields fuse into one pass:

```js
(i) => {
  let v0;
  try {
    v0 = JSON.parse(i);
  } catch (t) {
    e[0](i);
  }
  if (typeof v0 === "object" && v0 && !Array.isArray(v0)) {
    for (;;) {
      if (v0["type"] === "user.created") {
        let v2 = v0["id"], v3 = v0["tags"];
        typeof v2 === "string" || e[2](v2);
        let v1;
        try {
          v1 = BigInt(v2);
        } catch (_) {
          e[1](v2);
        }
        Array.isArray(v3) || e[6](v3);
        // ...validates each tag, tracking the error path
        v8.length > 0 || e[5](v8); // e[5] throws your nonEmpty message
        v0 = { type: v0["type"], id: v1, tags: v8 };
        break;
      }
      if (v0["type"] === "user.deleted") {
        // ...one branch per variant, no loop over union members
      }
      e[9](v0);
    }
  } else {
    e[10](v0);
  }
  return v0;
};
```

That's why Sury tends to outrun not just other libraries, but hand-rolled validation too.

### Encoding vs `JSON.stringify`

The encoder builds the JSON text directly - no intermediate object, the literal parts baked in:

```js
(i) => {
  if (typeof i === "object" && i && !Array.isArray(i)) {
    for (;;) {
      // ...one branch per variant
      if (i["type"] === "user.deleted") {
        i = '{"type":"user.deleted","id":"' + i["id"] + '"}';
        break;
      }
    }
  }
  return i;
};
```

And the values `JSON.stringify` silently corrupts throw instead:

```ts
JSON.stringify({ price: Infinity });
// => '{"price":null}'

S.encoder(S.schema({ price: S.number }), S.jsonString)({ price: Infinity });
// => throws S.Error: Failed at ["price"]: Expected JSON, received Infinity
```

| Encode to JSON string                 | **Sury**    | `JSON.stringify` | fast-json-stringify |
| ------------------------------------- | ----------- | ---------------- | ------------------- |
| API response (user profile, 7 fields) | **305 ns**  | 590 ns           | 382 ns              |
| Event feed (50 tagged-union events)   | **5.05 µs** | 7.82 µs          | 20.26 µs            |
| `bigint` id + binary payload + `Date` | **1.17 µs** | 1.51 µs          | 1.45 µs             |

Faster than `JSON.stringify`, and 3.5× lighter than fast-json-stringify - 16.4 kB against 56.7 kB, encoder included.

Measured with [this repo's comparison benchmark](https://github.com/DZakh/sury/tree/main/packages/e2e/src/benchmark) against `sury@11.0.0-rc.1`, `zod@4.4.3`, `typebox@0.34.52`, `valibot@1.4.2`, `arktype@2.2.3`:

### Size & speed

|                                 | Sury           | Zod          | TypeBox                        | Valibot      | ArkType        |
| ------------------------------- | -------------- | ------------ | ------------------------------ | ------------ | -------------- |
| **Total size** (min + gzip)     | 31.5 kB        | 65.0 kB      | 31.3 kB                        | 15.3 kB      | 47.2 kB        |
| **Benchmark size** (min + gzip) | 8.0 kB         | 19.6 kB      | 22.6 kB                        | 1.29 kB      | 47.1 kB        |
| **Parse with the same schema**  | 210,061 ops/ms | 9,367 ops/ms | 158,185 ops/ms (no transforms) | 1,970 ops/ms | 106,520 ops/ms |
| **Create schema & parse once**  | 99 ops/ms      | 11 ops/ms    | 103 ops/ms (no transforms)     | 315 ops/ms   | 11 ops/ms      |

"Total size" is the whole library; "Benchmark size" is what the benchmark's schema and parse pull into a bundle after tree-shaking. The TypeBox numbers come from its compiled validator, which checks the value but doesn't transform or rebuild it.

Independent benchmarks and conformance suites that include Sury:

- [typescript-runtime-type-benchmarks](https://moltar.github.io/typescript-runtime-type-benchmarks/) - throughput across the ecosystem
- [schemabenchmarks.dev](https://schemabenchmarks.dev/) - per-step breakdown: download, initialization, validation, parsing, Standard Schema, codec
- [json-schema-compliance-suite](https://github.com/sinclairzx81/json-schema-compliance-suite) - JSON Schema validation, semantics, and round-trip fidelity

### Features

|                                          | Sury                                     | Zod                                       | TypeBox                   | Valibot                                                               | ArkType                   |
| ---------------------------------------- | ---------------------------------------- | ----------------------------------------- | ------------------------- | --------------------------------------------------------------------- | ------------------------- |
| **Inferred TS type** (what you hover)    | `S.Schema<{foo: string}, {foo: string}>` | `z.ZodObject<{foo: z.ZodString}, $strip>` | `TObject<{foo: TString}>` | `v.ObjectSchema<{readonly foo: v.StringSchema<undefined>}, undefined>` | `Type<{foo: string}, {}>` |
| **JSON Schema**                          | `S.toJSONSchema` + `S.fromJSONSchema`    | `z.toJSONSchema`                          | 👑                        | `@valibot/to-json-schema`                                             | `myType.toJsonSchema()`   |
| **Standard Schema**                      | ✅                                       | ✅                                        | ❌                        | ✅                                                                    | ✅                        |
| **Codegen-free** (doesn't need compiler) | ✅                                       | ✅                                        | ✅                        | ✅                                                                    | ✅                        |
| **Eval-free**                            | ❌                                       | ⭕ opt-out                                | ⭕ opt-in                 | ✅                                                                    | ⭕ opt-out                |
| **Ecosystem**                            | ⭐️⭐️                                   | ⭐️⭐️⭐️⭐️⭐️                           | ⭐️⭐️⭐️⭐️⭐️           | ⭐️⭐️⭐️                                                             | ⭐️⭐️                    |

Sury's own ecosystem is young, but implementing Standard Schema means the 32+ libraries that support the spec already work with it today.

## Integrations

Use Sury anywhere a schema is accepted:

- [tRPC](https://trpc.io/), [TanStack Form](https://tanstack.com/form), [TanStack Router](https://tanstack.com/router), [Hono](https://hono.dev/), and 28+ more via the [Standard Schema](https://standardschema.dev/) spec
- Anything that speaks [JSON Schema](https://json-schema.org/), via `S.toJSONSchema` / `S.fromJSONSchema`

## Used by

- [HyperIndex](https://github.com/enviodev/hyperindex) - Envio's blockchain indexing framework, which uses Sury to power native high-performance external calls
- [rescript-rest](https://github.com/DZakh/rescript-rest) - RPC-like client, contract, and server implementation for a pure REST API
- [rescript-envsafe](https://github.com/DZakh/rescript-envsafe) - makes sure you don't accidentally deploy apps with missing or invalid environment variables
- [rescript-stripe](https://github.com/enviodev/rescript-stripe) - describe and manage Stripe billing in a declarative way with code
- Internal form library at [Carla](https://www.carla.se/)

Building something with Sury? [Let me know](https://x.com/dzakh_dev) and I'll add it here.

## FAQ

### Does it really use `new Function`?

Yes - that's where the speed comes from. It's also how TypeBox, Zod v4 and ArkType work, and even Cloudflare Workers added support for it.

There's currently no eval-free mode, so Sury won't run where dynamic code evaluation is forbidden: pages under a strict CSP without `'unsafe-eval'`, some browser extension contexts, and a few restricted edge runtimes. If that's your environment, [Valibot](https://valibot.dev/) is the honest recommendation today.

### Why "Sury"?

It's short, it's pronounceable, and the 🧬 fits: a schema is the DNA of your data - one definition that everything else is derived from.

## Resources

- Welcome Sury - The fastest schema with next-gen DX ([Dev.to](https://dev.to/dzakh/welcome-sury-the-fastest-schema-with-next-gen-dx-5gl4))
- ReScript Schema unique features ([Dev.to](https://dev.to/dzakh/javascript-schema-library-from-the-future-5420))
- Building and consuming REST API in ReScript with rescript-rest and Fastify ([YouTube](https://youtu.be/37FY6a-zY20?si=72zT8Gecs5vmDPlD))

## Contributing

Bug reports, ideas, and pull requests are all welcome - open an [issue](https://github.com/DZakh/sury/issues) to get started.

## Sponsorship

If you're enjoying Sury and want to give back, that would be rad!

The free ways help a lot too: star the repo, write about it, or tell someone who's picking a validation library this week.

If you'd like to donate, GitHub Sponsors isn't available in my country, so **USDT** is the easiest route:

- ERC20: `0x509fCF7C24A94a776eb92B56B9DA4aA145615529`
- TRC20: `TFg5hKgkdcrFnPHNgYqfbp9yMyx25uaWrF`

Your sponsorship doesn't go towards anything specific - it's simply a wonderful way to say "thank you" and make me happy. 😁

DM me on [X/Twitter](https://x.com/dzakh_dev) if you want to be featured or just to say hi! This would mean so much to me. ✨

## License

[MIT](https://github.com/DZakh/sury/blob/main/LICENSE)
