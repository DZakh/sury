# Contributing

When it comes to open source, there are different ways you can contribute, all of which are valuable. Here's few guidelines that should help you as you prepare your contribution.

## Initial steps

Before you start working on a contribution, create an issue describing what you want to build. It's possible someone else is already working on something similar, or perhaps there is a reason that feature isn't implemented. The maintainers will point you in the right direction.

## Development

The following steps will get you setup to contribute changes to this repo:

1. Fork this repo.
2. Clone your forked repo: `git clone git@github.com:{your_username}/sury.git`
3. Install [pnpm](https://pnpm.io/) if not available `npm i -g pnpm@8.14.3`
4. Run `pnpm i` to install dependencies.
5. Start playing with the code!

## PPX

### With Dune

Make sure running the below commands in `packages/sury-ppx/src`.

1. Create a sandbox with opam

```
opam switch create sury-ppx 4.12.1
```

Or

```
opam switch set sury-ppx
```

2. Install dependencies

```
opam install . --deps-only
```

3. Build

```
dune build --watch
```

4. Test

Make sure running tests

```
(run compiler for lib)
npm run res
(run compiler for tests)
npm run test:res
(run tests in watch mode)
npm run test -- --watch
```

## Make comparison

https://bundlejs.com/

`sury`

```ts
export * as S from "sury@10.0.0-rc.0";
```

```ts
import * as S from "sury@10.0.0-rc.0";

const schema = S.schema({
  number: S.number,
  negNumber: S.number,
  maxNumber: S.number,
  string: S.string,
  longString: S.string,
  boolean: S.boolean,
  deeplyNested: {
    foo: S.string,
    num: S.number,
    bool: S.boolean,
  },
});
S.parseOrThrow(data, schema);
```

valibot

```ts
export * as v from "valibot@1.0.0";
```

```ts
import * as v from "valibot@1.0.0";

const schema = v.object({
  number: v.number(),
  negNumber: v.number(),
  maxNumber: v.number(),
  string: v.string(),
  longString: v.string(),
  boolean: v.boolean(),
  deeplyNested: v.object({
    foo: v.string(),
    num: v.number(),
    bool: v.boolean(),
  }),
});
v.parse(schema, data);
```

zod

```ts
export * as z from "zod@4.0.0-beta.20250420T053007";
```

```ts
import * as z from "zod@4.0.0-beta.20250420T053007";

const schema = z.object({
  number: z.number(),
  negNumber: z.number(),
  maxNumber: z.number(),
  string: z.string(),
  longString: z.string(),
  boolean: z.boolean(),
  deeplyNested: z.object({
    foo: z.string(),
    num: z.number(),
    bool: z.boolean(),
  }),
});
schema.parse(data);
```

### TypeBox

```ts
export * from "@sinclair/typebox";
// Include Value for transforms support
export * from "@sinclair/typebox/value";
export * from "@sinclair/typebox/compiler";
```

```ts
import { Type } from "@sinclair/typebox";
import { TypeCompiler } from "@sinclair/typebox/compiler";

const schema = TypeCompiler.Compile(
  Type.Object({
    number: Type.Number(),
    negNumber: Type.Number(),
    maxNumber: Type.Number(),
    string: Type.String(),
    longString: Type.String(),
    boolean: Type.Boolean(),
    deeplyNested: Type.Object({
      foo: Type.String(),
      num: Type.Number(),
      bool: Type.Boolean(),
    }),
  })
);
if (!schema.Check(data)) {
  throw new Error(schema.Errors(data).First()?.message);
}
```

ArkType

```ts
export * from "arktype@2.1.20";
```

```ts
import { type } from "arktype@2.1.20";

const schema = type({
  number: "number",
  negNumber: "number",
  maxNumber: "number",
  string: "string",
  longString: "string",
  boolean: "boolean",
  deeplyNested: {
    foo: "string",
    num: "number",
    bool: "boolean",
  },
});
schema(data);
```

## License

By contributing your code to the rescript-schema GitHub repository, you agree to license your contribution under the MIT license.
