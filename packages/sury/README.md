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
- Implements the [Standard Schema](https://standardschema.dev/) spec
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

|                                          | [Sury@10.0.0-rc.0](https://github.com/DZakh/sury) | [Zod@4.0.0-beta](https://v4.zod.dev/v4) | [TypeBox@0.34.33](https://github.com/sinclairzx81/typebox) | [Valibot@1.0.0](https://valibot.dev/)                                  | [ArkType@2.1.20](https://arktype.io/) |
| ---------------------------------------- | ------------------------------------------------- | --------------------------------------- | ---------------------------------------------------------- | ---------------------------------------------------------------------- | ------------------------------------- |
| **Total size** (min + gzip)              | 14.1 kB                                           | 25.9 kB                                 | 31.4 kB                                                    | 12.6 kB                                                                | 45.9 kB                               |
| **Benchmark size** (min + gzip)          | 4.27 kB                                           | 13.5 kB                                 | 22.8 kB                                                    | 1.23 kB                                                                | 45.8 kB                               |
| **Parse with the same schema**           | 94,828 ops/ms                                     | 8,437 ops/ms                            | 99,640 ops/ms (No transforms)                              | 1,721 ops/ms                                                           | 67,552 ops/ms                         |
| **Create schema & parse once**           | 166 ops/ms                                        | 6 ops/ms                                | 111 ops/ms (No transforms)                                 | 287 ops/ms                                                             | 11 ops/ms                             |
| **JSON Schema**                          | `S.toJSONSchema`                                  | `z.toJSONSchema`                        | 👑                                                         | `@valibot/to-json-schema`                                              | `T.toJsonSchema`                      |
| **Standard Schema**                      | ✅                                                | ✅                                      | ❌                                                         | ✅                                                                     | ✅                                    |
| **Eval-free**                            | ❌                                                | ⭕ opt-out                              | ⭕ opt-in                                                  | ✅                                                                     | ⭕ opt-out                            |
| **Codegen-free** (Doesn't need compiler) | ✅                                                | ✅                                      | ✅                                                         | ✅                                                                     | ✅                                    |
| **Infered TS Type**                      | `S.Schema<{foo: string}, {foo: string}>`          | `z.ZodObject<{foo: z.ZodString}, {}>`   | `TObject<{foo: TString}>`                                  | `v.ObjectSchema<{readonly foo: v.StringSchema<undefined>}, undefined>` | `Type<{foo: string}, {}>`             |
| **Ecosystem**                            | ⭐️⭐️                                            | ⭐️⭐️⭐️⭐️⭐️                         | ⭐️⭐️⭐️⭐️⭐️                                            | ⭐️⭐️⭐️                                                              | ⭐️⭐️                                |

## Sponsorship

If you're enjoying **Sury** and want to give back, that would be rad!

Your sponsorship doesn't go towards anything specific – it's simply a wonderful way to say "thank you" and make me happy. 😁

Donate with **USDT**:

- ERC20: `0x509fCF7C24A94a776eb92B56B9DA4aA145615529`
- TRC20: `TFg5hKgkdcrFnPHNgYqfbp9yMyx25uaWrF`

DM me on [X/Twitter](https://x.com/dzakh_dev) if you want to be featured or just to say hi! This would mean so much to me. ✨
