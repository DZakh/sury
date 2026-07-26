[![CI](https://github.com/DZakh/rescript-schema/actions/workflows/ci.yml/badge.svg)](https://github.com/DZakh/rescript-schema/actions/workflows/ci.yml)
[![codecov](https://codecov.io/gh/DZakh/rescript-schema/branch/main/graph/badge.svg?token=40G6YKKD6J)](https://codecov.io/gh/DZakh/rescript-schema)
[![sury npm](https://img.shields.io/npm/dm/sury?label=Sury)](https://www.npmjs.com/package/sury)
[![rescript-schema npm](https://img.shields.io/npm/dm/rescript-schema?label=ReScript%20Schema)](https://www.npmjs.com/package/rescript-schema)

# Sury (aka ReScript Schema) 🧬

The fastest schema with next-gen DX.

**Highlights:**

- Works with plain JavaScript, TypeScript, and ReScript. You don't need to use any compiler.
- The **fastest** parsing and validation library in the entire JavaScript ecosystem ([benchmark](https://moltar.github.io/typescript-runtime-type-benchmarks/))
- Small JS footprint & tree-shakable API ([Comparison with Zod and Valibot](#comparison))
- Implements the [Standard Schema](https://standardschema.dev/) spec, including the [Standard JSON Schema](https://standardschema.dev/json-schema) extension
- Built-in JSON Schema support
- Detailed and easy to understand error messages
- Declarative transformations with automatic serialization
- Immutable API with 100+ different operations
- Flexible global config

Also, you can use **Sury** as a building block for your own tools or use existing ones:

- [tRPC](https://trpc.io/), [TanStack Form](https://tanstack.com/form), [TanStack Router](https://tanstack.com/router), [Hono](https://hono.dev/) and 19+ more using [Standard Schema](https://standardschema.dev/) spec
- Anything that supports [JSON Schema](https://json-schema.org/) with `S.toJSONSchema`
- [rescript-rest](https://github.com/DZakh/rescript-rest) - RPC-like client, contract, and server implementation for a pure REST API
- [rescript-envsafe](https://github.com/DZakh/rescript-envsafe) - Makes sure you don't accidentally deploy apps with missing or invalid environment variables
- [rescript-stripe](https://github.com/enviodev/rescript-stripe) - Describe and manage Stripe billing in a declarative way with code
- Internal form library at [Carla](https://www.carla.se/)

## Documentation

- [For JS/TS users](/docs/js-usage.md)
- [For ReScript users](/docs/rescript-usage.md)
- [For PPX users](/packages/sury-ppx/README.md)

> ⚠️ Be aware that **Sury** uses `new Function` for parsing. The approach is battle tested and has no known security issues. It's also used by TypeBox, Zod@4 and ArkType. Even Cloudflare Workers recently added support for it.

## Resources

- Welcome Sury - The fastest schema with next-gen DX ([Dev.to](https://dev.to/dzakh/welcome-sury-the-fastest-schema-with-next-gen-dx-5gl4))
- ReScript Schema unique features ([Dev.to](https://dev.to/dzakh/javascript-schema-library-from-the-future-5420))
- Building and consuming REST API in ReScript with rescript-rest and Fastify ([YouTube](https://youtu.be/37FY6a-zY20?si=72zT8Gecs5vmDPlD))

## Comparison

Instead of relying on a few large functions with many methods, **Sury** follows [Valibot](https://github.com/fabian-hiller/valibot)'s approach, where API design and source code is based on many small and independent functions, each with just a single task. This modular design has several advantages.

For example, this allows a bundler to use the import statements to remove code that is not needed. This way, only the code that is actually used gets into your production build. This can reduce the bundle size by up to 2 times compared to [Zod](https://github.com/colinhacks/zod).

Besides the individual bundle size, the overall size of the library is also significantly smaller.

At the same time **Sury** is the fastest composable validation library in the entire JavaScript ecosystem. This is achieved because of the JIT approach when an ultra optimized validator is created using `new Function`.

|                                          | [Sury@11.0.0-alpha.11](https://github.com/DZakh/sury) | [Zod@4.4.3](https://zod.dev/)             | [TypeBox@0.34.52](https://github.com/sinclairzx81/typebox) | [Valibot@1.4.2](https://valibot.dev/)                                  | [ArkType@2.2.3](https://arktype.io/) |
| ---------------------------------------- | ----------------------------------------------------- | ----------------------------------------- | ---------------------------------------------------------- | ---------------------------------------------------------------------- | ------------------------------------ |
| **Total size** (min + gzip)              | 20.8 kB                                               | 64.7 kB                                   | 31.2 kB                                                    | 15.2 kB                                                                | 47.9 kB                              |
| **Benchmark size** (min + gzip)          | 9.88 kB                                               | 19.6 kB                                   | 22.6 kB                                                    | 1.30 kB                                                                | 47.8 kB                              |
| **Parse with the same schema**           | 160,549 ops/ms                                        | 8,463 ops/ms                              | 120,684 ops/ms (No transforms)                             | 1,328 ops/ms                                                           | 77,405 ops/ms                        |
| **Create schema & parse once**           | 54 ops/ms                                             | 7 ops/ms                                  | 82 ops/ms (No transforms)                                  | 198 ops/ms                                                             | 9 ops/ms                             |
| **JSON Schema**                          | `S.toJSONSchema`                                      | `z.toJSONSchema`                          | 👑                                                         | `@valibot/to-json-schema`                                              | `myType.toJsonSchema()`              |
| **Standard Schema**                      | ✅                                                    | ✅                                        | ❌                                                         | ✅                                                                     | ✅                                   |
| **Eval-free**                            | ❌                                                    | ⭕ opt-out                                | ⭕ opt-in                                                  | ✅                                                                     | ⭕ opt-out                           |
| **Codegen-free** (Doesn't need compiler) | ✅                                                    | ✅                                        | ✅                                                         | ✅                                                                     | ✅                                   |
| **Infered TS Type**                      | `S.Schema<{foo: string}, {foo: string}>`              | `z.ZodObject<{foo: z.ZodString}, $strip>` | `TObject<{foo: TString}>`                                  | `v.ObjectSchema<{readonly foo: v.StringSchema<undefined>}, undefined>` | `Type<{foo: string}, {}>`            |
| **Ecosystem**                            | ⭐️⭐️                                                | ⭐️⭐️⭐️⭐️⭐️                           | ⭐️⭐️⭐️⭐️⭐️                                            | ⭐️⭐️⭐️                                                              | ⭐️⭐️                               |

## Sponsorship

If you're enjoying **Sury** and want to give back, that would be rad!

Your sponsorship doesn't go towards anything specific – it's simply a wonderful way to say "thank you" and make me happy. 😁

Donate with **USDT**:

- ERC20: `0x509fCF7C24A94a776eb92B56B9DA4aA145615529`
- TRC20: `TFg5hKgkdcrFnPHNgYqfbp9yMyx25uaWrF`

DM me on [X/Twitter](https://x.com/dzakh_dev) if you want to be featured or just to say hi! This would mean so much to me. ✨
