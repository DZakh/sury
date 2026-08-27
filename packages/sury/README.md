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

**API Reference:** [TypeScript](https://github.com/DZakh/sury/blob/main/docs/js-usage.md) | [ReScript](https://github.com/DZakh/sury/blob/main/docs/rescript-usage.md) | [ReScript PPX](https://github.com/DZakh/sury/blob/main/packages/sury-ppx/README.md)

## Why Sury

Describe your data model once - unions, constraints and metadata included. Hover a schema and you can actually read the type: `S.Schema<Event>`, not `v.ObjectSchema<{readonly foo: v.StringSchema<undefined>}, undefined>`:

```ts
import * as S from "sury"; // Tree-shakable: a schema + parser starts at 8 kB gzip

const eventSchema = S.union([
  {
    type: "user.created",
    id: S.bigint,
    tags: S.array({ name: S.string }).with(S.nonEmpty, "Add at least one tag"),
  },
  { type: "user.deleted", id: S.bigint, payload: S.json },
]).with(S.meta, { description: "User lifecycle event" });

type Event = S.Output<typeof eventSchema>;
//   ^? { type: "user.created"; id: bigint; tags: { name: string }[] }
//      | { type: "user.deleted"; id: bigint; payload: S.JSON }
```

The same schema parses and encodes - no second definition. Encoding is even faster than `JSON.stringify` ([which lies to you](https://dev.to/dzakh/encode-dont-stringify-how-jsonstringify-lies-to-you-38fk)):

```ts
const parseEvent = S.decoder(S.jsonString, eventSchema);
parseEvent('{"type":"user.created","id":"42","tags":[{"name":"vip"}]}');
// => { type: "user.created", id: 42n, tags: [{ name: "vip" }] }

S.encoder(eventSchema, S.jsonString)({ type: "user.deleted", id: 7n, payload: { reason: "spam" } });
// => '{"type":"user.deleted","id":"7","payload":{"reason":"spam"}}'
```

Errors tell you exactly where to look, in wire terms - the missing `id` is a missing string. Prefer a result over an exception? Wrap the call in `S.safe`:

```ts
parseEvent('{"type":"user.created","id":"42","tags":[]}');
// => throws S.Error: Failed at ["tags"]: Add at least one tag

const result = S.safe(() => parseEvent('{"type":"user.deleted"}'));
if (!result.success) result.error.message;
// => 'Failed at ["id"]: Expected string, received undefined'
```

Need a different wire? Wrap the same model in base64url. The pipeline knows both of its ends, so `S.encoder` and `S.parser` take just the schema:

```ts
const b64Event = S.base64url.with(S.to, S.jsonString.with(S.to, eventSchema));

S.encoder(b64Event)({ type: "user.deleted", id: 7n, payload: { reason: "spam" } });
// => "eyJ0eXBlIjoidXNlci5kZWxldGVkIiwiaWQiOiI3IiwicGF5bG9hZCI6eyJyZWFzb24iOiJzcGFtIn19"

S.parser(b64Event)("eyJ0eXBlIjoidXNlci5kZWxldGVkIiwiaWQiOiI3IiwicGF5bG9hZCI6eyJyZWFzb24iOiJzcGFtIn19");
// => { type: "user.deleted", id: 7n, payload: { reason: "spam" } }
```

Thanks to [Standard Schema](https://standardschema.dev/), the schema plugs straight into tRPC, Hono, TanStack and 28+ other libraries. Its Standard JSON Schema extension describes the wire - and a `bigint` has no wire until you give it one:

```ts
S.enableStandardJSONSchema(); // Opt-in, so unused JSON Schema code tree-shakes away

eventSchema["~standard"].validate({ type: "user.deleted", id: 7n, payload: null });
// => { value: { type: "user.deleted", id: 7n, payload: null } }

eventSchema["~standard"].jsonSchema.input({ target: "draft-07" });
// => throws: Failed at ["id"]: Expected JSON, received bigint

S.json.with(S.to, eventSchema)["~standard"].jsonSchema.input({ target: "draft-07" });
// => { anyOf: [...], description: "User lifecycle event", ... } - with id: { type: "string" }
```

You can go the other way too: feed JSON Schema in and get a typed Sury schema back. 93% of the official draft-07 test suite passes in CI:

```ts
const emailSchema = S.fromJSONSchema({ type: "string", format: "email" });
//? S.Schema<string, string>

S.parser(emailSchema)("hi@sury.dev"); // => "hi@sury.dev"
```

There's also the stuff you'd normally grab one more library for. Recursive schemas:

```ts
type Node = { id: string; children: Node[] };
const nodeSchema = S.recursive<Node>("Node", (nodeSchema) =>
  S.schema({ id: S.string, children: S.array(nodeSchema) }),
);
```

Renaming fields between the wire and your code is one `S.shape` call, and the encode direction follows automatically:

```ts
const userSchema = S.schema({
  USER_ID: S.string.with(S.to, S.bigint),
  USER_NAME: S.string,
}).with(S.shape, (input) => ({
  id: input.USER_ID,
  name: input.USER_NAME,
}));

S.parser(userSchema)({ USER_ID: "0", USER_NAME: "Dmitry" });
// => { id: 0n, name: "Dmitry" }
S.encoder(userSchema)({ id: 0n, name: "Dmitry" });
// => { USER_ID: "0", USER_NAME: "Dmitry" }
```

Reading a `File` is asynchronous, so a pipeline that starts from one becomes async too:

```ts
const configSchema = S.file.with(S.to, S.jsonString.with(S.to, S.schema({ theme: S.string })));

await S.asyncParser(configSchema)(new File(['{"theme":"dark"}'], "config.json"));
// => { theme: "dark" }
```

`process.env` is strings; your config isn't. Pipe from `S.record(S.string)` and the coercions are inferred:

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
```

Some data arrives in awkward layouts - like the columnar arrays that [boost Postgres INSERT performance by 2x](https://www.timescale.com/blog/boosting-postgres-insert-performance). Describe the layout instead of writing glue code, and `S.compactColumns` turns columns into rows and back:

```ts
const rows = S.compactColumns(S.json).with(S.to, S.array({ id: S.bigint, city: S.string }));

S.decoder(rows)([["1", "2"], ["Tbilisi", "Batumi"]]);
// => [{ id: 1n, city: "Tbilisi" }, { id: 2n, city: "Batumi" }]
S.encoder(rows)([{ id: 1n, city: "Tbilisi" }, { id: 2n, city: "Batumi" }]);
// => [["1", "2"], ["Tbilisi", "Batumi"]]
```

Wires today: `S.json`, `S.jsonString`, `S.base64`, `S.base64url`, `S.uint8Array`, `S.file` and `S.blob`. Coming next: env, `FormData` and protobuf.

### The code a schema turns into

Here's what `parseEvent` from above actually runs - a function specialized for this exact shape: the union dispatches on the discriminant, the `bigint` coercion is inlined as a bare `BigInt()` call, your `nonEmpty` message is a plain length check, and `S.jsonString` -> union -> fields fuse into one pass:

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

That's why Sury tends to outrun even hand-rolled validation - see the [benchmarks](#comparison) below.

### Encoding vs `JSON.stringify`

The encoder builds the JSON text directly - no intermediate object, the structure baked in as literals, and `JSON.stringify` left only the free-form `payload`:

```js
(i) => {
  if (typeof i === "object" && i && !Array.isArray(i)) {
    for (;;) {
      // ...one branch per variant
      if (i["type"] === "user.deleted") {
        let v6 = JSON.stringify(i["payload"]);
        let v7 = '{"type":"user.deleted","id":"' + i["id"] + '"';
        if (v6 !== void 0) {
          v7 += ',"payload":' + v6;
        }
        i = v7 + "}";
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

And 3.5× lighter than fast-json-stringify - 16.4 kB against 56.7 kB, encoder included.

## Comparison

Sury has the fastest parsing and encoding in the ecosystem - the hot path. Creating a schema and using it once is the one workload where an interpreted library wins a row below.

It's also small. Instead of a few large classes with many methods, the API and source are built from many small, independent functions. A bundler follows your imports and drops everything you don't use, which can cut the shipped size by up to 2× compared to [Zod](https://github.com/colinhacks/zod). (The approach is borrowed from [Valibot](https://github.com/fabian-hiller/valibot), which pioneered it.)

### Size & speed

Measured with [this repo's comparison benchmark](https://github.com/DZakh/sury/tree/main/packages/e2e/src/benchmark) against `sury@11.0.0-rc.1`, `zod@4.4.3`, `typebox@0.34.52`, `valibot@1.4.2`, `arktype@2.2.3`.

|                                 | Sury           | Zod          | TypeBox                        | Valibot      | ArkType        |
| ------------------------------- | -------------- | ------------ | ------------------------------ | ------------ | -------------- |
| **Total size** (min + gzip)     | 31.5 kB        | 65.0 kB      | 31.3 kB                        | 15.3 kB      | 47.2 kB        |
| **Benchmark size** (min + gzip) | 8.0 kB         | 19.6 kB      | 22.6 kB                        | 1.29 kB      | 47.1 kB        |
| **Parse with the same schema**  | 210,061 ops/ms | 9,367 ops/ms | 158,185 ops/ms (no transforms) | 1,970 ops/ms | 106,520 ops/ms |
| **Create schema & parse once**  | 99 ops/ms      | 11 ops/ms    | 103 ops/ms (no transforms)     | 315 ops/ms   | 11 ops/ms      |

"Benchmark size" is what actually ships after tree-shaking for the benchmarked schema. The TypeBox numbers are validation-only - it doesn't run the transforms.

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

### Is `new Function` safe to use?

Yes. It's the same technique TypeBox, Zod v4 and ArkType use, and it's where much of the speed comes from ([the code a schema turns into](#the-code-a-schema-turns-into)). Cloudflare Workers allows `new Function` during Worker startup (the default since compatibility date 2025-06-01), so schemas and operations created at the top level work there.

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
