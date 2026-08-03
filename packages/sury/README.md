[![CI](https://github.com/DZakh/sury/actions/workflows/ci.yml/badge.svg)](https://github.com/DZakh/sury/actions/workflows/ci.yml)
[![sury npm](https://img.shields.io/npm/dm/sury?label=Sury)](https://www.npmjs.com/package/sury)
[![rescript-schema npm](https://img.shields.io/npm/dm/rescript-schema?label=ReScript%20Schema)](https://www.npmjs.com/package/rescript-schema)
[![license](https://img.shields.io/npm/l/sury)](https://github.com/DZakh/sury/blob/main/LICENSE)

# Sury 🧬

**The fastest schema with next-gen DX.**

Describe your data once, then parse it, validate it, transform it, serialize it back, and turn it into JSON Schema — all from the same definition, all compiled into a single optimized function.

> Formerly known as **ReScript Schema**. It's plain JavaScript — you don't need the ReScript compiler to use it. ReScript users, see the [ReScript docs](https://github.com/DZakh/sury/blob/main/docs/rescript-usage.md).

## Getting started

```sh
npm install sury
```

```ts
import * as S from "sury";

const playerSchema = S.schema({
  username: S.string,
  xp: S.number,
});

S.parser(playerSchema)({ username: "billie", xp: 100 });
// => { username: "billie", xp: 100 }

S.parser(playerSchema)({ username: "billie", xp: "not a number" });
// => throws S.Error: Failed at ["xp"]: Expected number, received "not a number"

type Player = S.Infer<typeof playerSchema>;
//   ^? { username: string; xp: number }
```

The API mirrors TypeScript types, so there's not much new syntax to learn.

**Full API reference:** [JS/TS](https://github.com/DZakh/sury/blob/main/docs/js-usage.md) · [ReScript](https://github.com/DZakh/sury/blob/main/docs/rescript-usage.md) · [PPX](https://github.com/DZakh/sury/blob/main/packages/sury-ppx/README.md)

## Why Sury

### Discriminated unions, decoded straight from a JSON string

Declare the union, and get parsing, narrowing, and serialization from one definition:

```ts
const eventSchema = S.union([
  { type: "user.created", id: S.bigint },
  { type: "user.renamed", id: S.bigint, name: S.string },
  { type: "user.deleted", id: S.bigint },
]);

// Chain schemas to build a pipeline — no JSON.parse in your own code
const parseEvent = S.decoder(S.jsonString, eventSchema);

const event = parseEvent('{"type":"user.renamed","id":"42","name":"Dmitry"}');
// => { type: "user.renamed", id: 42n, name: "Dmitry" }

switch (event.type) {
  case "user.renamed":
    event.name; // string — TypeScript narrows it for you
    break;
}

// The same schema serializes back out, no second definition needed
S.encoder(eventSchema, S.jsonString)(event);
// => '{"type":"user.renamed","id":"42","name":"Dmitry"}'
```

Note that you write `id: S.bigint` — the type you want to work with. A `bigint` can't exist in JSON, so **Sury** infers the `"42"` → `42n` coercion from the input side of the pipeline, in both directions. No `as const`, no coercion wrappers, no second schema for the wire format.

Errors point at the field inside the matched variant, not at the union as a whole:

```ts
parseEvent('{"type":"user.renamed","id":"42"}');
// => throws S.Error: Failed at ["name"]: Expected string, received undefined
```

### See the code it compiles

`parseEvent` above isn't an interpreter walking a schema tree at runtime — it's a function **Sury** generated for exactly this shape. The union dispatches on the discriminant, the inferred `bigint` coercion is inlined as a bare `BigInt()` call, and the whole `S.jsonString` → union → field-level pipeline is fused into one pass:

```js
(i) => {
  let v0;
  try {
    v0 = JSON.parse(i);
  } catch (t) {
    e[0](i);
  }
  if (typeof v0 === "object" && v0 && !Array.isArray(v0)) {
    if (v0["type"] === "user.created") {
      let v2 = v0["id"];
      typeof v2 === "string" || e[2](v2);
      let v1;
      try {
        v1 = BigInt(v2);
      } catch (_) {
        e[1](v2);
      }
      v0 = { type: v0["type"], id: v1 };
    } else if (v0["type"] === "user.renamed") {
      // …one branch per variant, no loop over union members
    } else {
      e[8](v0);
    }
  } else {
    e[9](v0);
  }
  return v0;
};
```

This is why **Sury** will most likely outperform not only other libraries, but also your own hand-rolled validation logic.

### Transformations that reverse themselves

Rename fields, coerce types, and reshape objects — then get the inverse for free:

```ts
const userSchema = S.schema({
  USER_ID: S.string.with(S.to, S.bigint),
  USER_NAME: S.string,
}).with(S.shape, (input) => ({
  id: input.USER_ID,
  name: input.USER_NAME,
}));
//? S.Schema<{ USER_ID: string; USER_NAME: string }, { id: bigint; name: string }>

S.parser(userSchema)({ USER_ID: "0", USER_NAME: "Dmitry" });
// => { id: 0n, name: "Dmitry" }

S.encoder(userSchema)({ id: 0n, name: "Dmitry" });
// => { USER_ID: "0", USER_NAME: "Dmitry" }
```

Every schema is reversible, so `S.reverse` hands you a full-featured schema with `Input` and `Output` swapped — usable with every operation, not just a serialize shortcut.

### Every schema is a pipeline stage

`S.jsonString` above wasn't a special "parse JSON" mode — it's an ordinary schema used as a stage. So is `S.json`, `S.uint8Array`, `S.date`, and every schema you write. Instead of a fixed menu of `parseJson` / `parseJsonString` / `convertToJson` functions, you describe the shape of the data at each step and let **Sury** compile the path between them.

Stages nest, so any field can be its own pipeline:

```ts
const apiUser = S.schema({
  // Arrives as JSON text, parsed and validated as an array of addresses
  addresses: S.jsonString.with(S.to, S.array(addressSchema)),
  // Arrives as a string, mapped to a Date
  createdAt: S.string.with(S.to, S.date),
  // Element-level transforms work the same way
  ids: S.array(S.string.with(S.to, S.bigint)),
});
```

The whole tree — top-level operation plus every nested `S.to` — still folds into one generated function, so deep pipelines cost nothing at runtime.

Once schemas are stages, layouts that usually need hand-written glue become a single definition. `S.compactColumns` maps columnar arrays to rows, in both directions:

```ts
const cityRow = S.schema({ id: S.bigint, city: S.string });
const rows = S.compactColumns(S.json).with(S.to, S.array(cityRow));

S.parser(rows)([["1", "2"], ["Tbilisi", "Batumi"]]);
// => [{ id: 1n, city: "Tbilisi" }, { id: 2n, city: "Batumi" }]

S.encoder(rows)([{ id: 1n, city: "Tbilisi" }, { id: 2n, city: "Batumi" }]);
// => [["1", "2"], ["Tbilisi", "Batumi"]]
```

### JSON Schema, through the standard interface

**Sury**'s internal representation is JSON Schema-shaped, so conversion is native rather than bolted on — and it's exposed through the [Standard JSON Schema](https://standardschema.dev/json-schema) extension of the [Standard Schema](https://standardschema.dev/) spec, so tools consume it without special-casing **Sury**.

Because **Sury** tracks Input and Output separately, it describes both sides of a transformation:

```ts
S.enableStandardJSONSchema();

const productSchema = S.schema({
  id: S.string,
  price: S.string.with(S.to, S.number),
}).with(S.meta, {
  description: "A product in the catalog",
  examples: [{ id: "p_1", price: 9.99 }],
});

productSchema["~standard"].jsonSchema.input({ target: "draft-2020-12" });
// {
//   $schema: "https://json-schema.org/draft/2020-12/schema",
//   type: "object",
//   properties: { id: { type: "string" }, price: { type: "string" } },
//   required: ["id", "price"],                    ↑ the wire format
//   description: "A product in the catalog",
//   examples: [{ id: "p_1", price: "9.99" }],
// }

productSchema["~standard"].jsonSchema.output({ target: "draft-2020-12" });
// { … properties: { id: { type: "string" }, price: { type: "number" } }, … }
//                                                   ↑ what your code receives
```

`S.meta` attaches `description`, `title`, `examples`, and `deprecated`. You write examples in the Output format you actually work with (`price: 9.99`), and they're emitted in the Input format the wire uses (`price: "9.99"`) — so a generated OpenAPI document describes the payload a client really sends.

`"draft-07"`, `"draft-2020-12"`, and `"openapi-3.0"` are all supported targets, and `S.toJSONSchema(schema, options)` is available directly if you'd rather not go through `~standard`.

It reads JSON Schema back in, too:

```ts
S.assert(S.fromJSONSchema({ type: "string", format: "email" }), "example.com");
// => throws S.Error: Expected email, received "example.com"
```

### Types you can actually read

Hover any schema and you see the data, not the library's internals:

```ts
S.schema({ foo: S.string });
//? S.Schema<{ foo: string }, { foo: string }>
```

Compare that with `v.ObjectSchema<{readonly foo: v.StringSchema<undefined>}, undefined>`. Both sides are right there, and they read in the direction the data flows — `S.Schema<TInput, TOutput>` — which is what makes a transformation's two sides obvious at a glance rather than something you reconstruct in your head.

### Errors that tell you where to look

```ts
S.parser(S.schema({ a: S.array(S.schema({ b: S.string })) }))({
  a: [{ b: "x" }, { b: 1 }],
});
// => throws S.Error: Failed at ["a"]["1"]["b"]: Expected string, received 1
```

Every error is an `S.Error` (`err instanceof S.Error`), and `S.safe` / `S.safeAsync` wrap any block into a typed result if you'd rather not catch:

```ts
const result = S.safe(() => S.parser(playerSchema)(data));
if (result.success) result.value;
else result.error;
```

## More highlights

- Works with plain JavaScript, TypeScript, and ReScript — no compiler required
- The **fastest** parsing and validation library in the JavaScript ecosystem ([benchmarks](#comparison))
- Small JS footprint & tree-shakable API
- Async transformations, recursive schemas, and custom schemas

### String formats, and what they don't promise

**Sury** covers the JSON Schema 2020-12 string format vocabulary — `S.isoDate`,
`S.isoTime`, `S.isoDateTime`, `S.duration`, `S.email`, `S.idnEmail`,
`S.hostname`, `S.idnHostname`, `S.ipv4`, `S.ipv6`, `S.uri`, `S.uriReference`,
`S.uriTemplate`, `S.iri`, `S.iriReference`, `S.uuid`, `S.jsonPointer` and
`S.relativeJsonPointer`. Each one stores its JSON Schema name verbatim, so it
survives a round trip through `S.toJSONSchema` and `S.fromJSONSchema`.

A JSON Schema format is an assertion about **syntax**, never about safety or
reachability, and Sury keeps them exactly as strict as their specs. That means:

```ts
S.assert(S.uri, "javascript:alert(1)");        // passes — a valid URI
S.assert(S.hostname, "169.254.169.254");       // passes — a valid hostname
S.assert(S.uriReference, "//evil.com");        // passes — a valid reference
S.assert(S.ipv4, "127.0.0.1");                 // passes — a valid address
```

None of those is a bug: they are well-formed instances of their formats. Making
them stricter would break the promise that the emitted JSON Schema describes the
schema — `format: "uri"` would mean one thing in Sury and another in every other
validator reading the same document.

When you want a security decision rather than a syntax check, compose one. The
extra constraint rides along in the JSON Schema, so it stays honest:

```ts
const httpsOnly = S.uri.with(S.pattern, /^https?:\/\//);
// { type: "string", format: "uri", pattern: "^https?:\\/\\/" }
// rejects javascript:alert(1) and file:///etc/passwd

const publicHost = S.hostname.with(S.refine, (h) => !isPrivateAddress(h));
```

Two more worth knowing before you pick one:

- **`S.url` is not `S.uri`.** `S.url` is an instance of the JS `URL` class, the
  way `S.date` is an instance of `Date` — use it when you want the parsed object
  and its `.host` / `.pathname`. `S.uri` validates a string as RFC 3986 and
  leaves it a string. They accept different languages: `new URL` silently
  percent-encodes spaces and quotes that RFC 3986 forbids.
- **`S.uriReference` is usually the one you want for a link field.** `S.uri`
  requires a scheme, so it rejects `/dashboard`.

If you want the *type* to record that a value was validated, brand it — a
branded type can only be produced by parsing, so it is real proof rather than a
shape that any string literal can satisfy:

```ts
const IsoDate = S.isoDate.with(S.brand, "IsoDate");
type IsoDate = S.Output<typeof IsoDate>;

const bad: IsoDate = someString;    // error
const bad2: IsoDate = "1963-06-19"; // error — looking right isn't enough
const ok: IsoDate = S.parser(IsoDate)(someString);
```

`S.hostname` and `S.idnHostname` carry one known gap: an `xn--` label is accepted
on shape alone, because rejecting one whose Punycode decodes to a character
IDNA2008 disallows would mean shipping the Unicode derived-property tables. The
gap only ever over-accepts — no valid hostname is turned away.

## Integrations

Use **Sury** anywhere a schema is accepted:

- [tRPC](https://trpc.io/), [TanStack Form](https://tanstack.com/form), [TanStack Router](https://tanstack.com/router), [Hono](https://hono.dev/), and 19+ more via the [Standard Schema](https://standardschema.dev/) spec
- Anything that speaks [JSON Schema](https://json-schema.org/), via `S.toJSONSchema` / `S.fromJSONSchema`

## Used by

- [HyperIndex](https://github.com/enviodev/hyperindex) — Envio's blockchain indexing framework, which uses **Sury** to power native high-performance external calls
- [rescript-rest](https://github.com/DZakh/rescript-rest) — RPC-like client, contract, and server implementation for a pure REST API
- [rescript-envsafe](https://github.com/DZakh/rescript-envsafe) — makes sure you don't accidentally deploy apps with missing or invalid environment variables
- [rescript-stripe](https://github.com/enviodev/rescript-stripe) — describe and manage Stripe billing in a declarative way with code
- Internal form library at [Carla](https://www.carla.se/)

Building something with **Sury**? [Let me know](https://x.com/dzakh_dev) and I'll add it here.

## Comparison

Instead of a few large classes with many methods, **Sury**'s API and source are built from many small, independent functions, each with a single task. A bundler can follow your import statements and drop everything you don't use, which can cut the shipped size by up to 2× compared to [Zod](https://github.com/colinhacks/zod). (The approach is borrowed from [Valibot](https://github.com/fabian-hiller/valibot), which pioneered it.)

At the same time **Sury** is the fastest composable validation library in the ecosystem, because schemas are compiled to specialized code with `new Function` rather than interpreted.

Measured against `sury@11.0.0-alpha.11`, `zod@4.4.3`, `typebox@0.34.52`, `valibot@1.4.2`, `arktype@2.2.3`:

### Size & speed

|                                 | Sury           | Zod          | TypeBox                        | Valibot      | ArkType       |
| ------------------------------- | -------------- | ------------ | ------------------------------ | ------------ | ------------- |
| **Total size** (min + gzip)     | 20.8 kB        | 64.7 kB      | 31.2 kB                        | 15.2 kB      | 47.9 kB       |
| **Benchmark size** (min + gzip) | 9.88 kB        | 19.6 kB      | 22.6 kB                        | 1.30 kB      | 47.8 kB       |
| **Parse with the same schema**  | 160,549 ops/ms | 8,463 ops/ms | 120,684 ops/ms (no transforms) | 1,328 ops/ms | 77,405 ops/ms |
| **Create schema & parse once**  | 54 ops/ms      | 7 ops/ms     | 82 ops/ms (no transforms)      | 198 ops/ms   | 9 ops/ms      |

Independent benchmarks and conformance suites that include **Sury**:

- [typescript-runtime-type-benchmarks](https://moltar.github.io/typescript-runtime-type-benchmarks/) — throughput across the ecosystem
- [schemabenchmarks.dev](https://schemabenchmarks.dev/) — per-step breakdown: download, initialization, validation, parsing, Standard Schema, codec
- [json-schema-compliance-suite](https://github.com/sinclairzx81/json-schema-compliance-suite) — JSON Schema validation, semantics, and round-trip fidelity

### Features

|                                          | Sury                                     | Zod                                       | TypeBox                   | Valibot                                                               | ArkType                   |
| ---------------------------------------- | ---------------------------------------- | ----------------------------------------- | ------------------------- | --------------------------------------------------------------------- | ------------------------- |
| **Inferred TS type** (what you hover)    | `S.Schema<{foo: string}, {foo: string}>` | `z.ZodObject<{foo: z.ZodString}, $strip>` | `TObject<{foo: TString}>` | `v.ObjectSchema<{readonly foo: v.StringSchema<undefined>}, undefined>` | `Type<{foo: string}, {}>` |
| **JSON Schema**                          | `S.toJSONSchema` + `S.fromJSONSchema`    | `z.toJSONSchema`                          | 👑                        | `@valibot/to-json-schema`                                             | `myType.toJsonSchema()`   |
| **Standard Schema**                      | ✅                                       | ✅                                        | ❌                        | ✅                                                                    | ✅                        |
| **Codegen-free** (doesn't need compiler) | ✅                                       | ✅                                        | ✅                        | ✅                                                                    | ✅                        |
| **Eval-free**                            | ❌                                       | ⭕ opt-out                                | ⭕ opt-in                 | ✅                                                                    | ⭕ opt-out                |
| **Ecosystem**                            | ⭐️⭐️                                   | ⭐️⭐️⭐️⭐️⭐️                           | ⭐️⭐️⭐️⭐️⭐️           | ⭐️⭐️⭐️                                                             | ⭐️⭐️                    |

**Sury**'s own ecosystem is young, but implementing Standard Schema means the 32+ libraries that support the spec already work with it today.

## FAQ

### Does it really use `new Function`?

Yes — that's where the speed comes from. The approach is battle-tested and has no known security issues; it's also how TypeBox, Zod v4, and ArkType work. Even Cloudflare Workers added support for it.

There's currently no eval-free mode, so **Sury** won't run in environments that forbid dynamic code evaluation, such as pages under a strict CSP without `'unsafe-eval'`, some browser extension contexts, and a few restricted edge runtimes. If that's your environment, [Valibot](https://valibot.dev/) is the honest recommendation today.

### Why "Sury"?

It's short, it's pronounceable, and the 🧬 fits: a schema is the DNA of your data — one definition that everything else is generated from.

## Resources

- Welcome Sury - The fastest schema with next-gen DX ([Dev.to](https://dev.to/dzakh/welcome-sury-the-fastest-schema-with-next-gen-dx-5gl4))
- ReScript Schema unique features ([Dev.to](https://dev.to/dzakh/javascript-schema-library-from-the-future-5420))
- Building and consuming REST API in ReScript with rescript-rest and Fastify ([YouTube](https://youtu.be/37FY6a-zY20?si=72zT8Gecs5vmDPlD))

## Contributing

Bug reports, ideas, and pull requests are all welcome — open an [issue](https://github.com/DZakh/sury/issues) to get started.

## Sponsorship

If you're enjoying **Sury** and want to give back, that would be rad!

The free ways help a lot too: star the repo, write about it, or tell someone who's picking a validation library this week.

If you'd like to donate, GitHub Sponsors isn't available in my country, so **USDT** is the easiest route:

- ERC20: `0x509fCF7C24A94a776eb92B56B9DA4aA145615529`
- TRC20: `TFg5hKgkdcrFnPHNgYqfbp9yMyx25uaWrF`

Your sponsorship doesn't go towards anything specific – it's simply a wonderful way to say "thank you" and make me happy. 😁

DM me on [X/Twitter](https://x.com/dzakh_dev) if you want to be featured or just to say hi! This would mean so much to me. ✨

## License

[MIT](https://github.com/DZakh/sury/blob/main/LICENSE)
