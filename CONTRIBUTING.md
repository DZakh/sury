# Contributing

When it comes to open source, there are different ways you can contribute, all of which are valuable. Here's few guidelines that should help you as you prepare your contribution.

## Initial steps

Before you start working on a contribution, create an issue describing what you want to build. It's possible someone else is already working on something similar, or perhaps there is a reason that feature isn't implemented. The maintainers will point you in the right direction.

## Development

The following steps will get you setup to contribute changes to this repo:

1. Fork this repo.
2. Clone your forked repo: `git clone git@github.com:{your_username}/sury.git`
3. Install [pnpm](https://pnpm.io/) if not available `npm i -g pnpm@9.0.5`
4. Run `pnpm i` to install dependencies.
5. Run `pnpm res` to run ReScript compiler
6. Run `pnpm test` for tests or use Wallaby.js

## Architecture

This section describes the internal architecture of Sury to help with understanding and contributing to the codebase.

### Core Concepts

#### Schema (internal type)

The internal representation of a type schema, containing:

- `tag`: Type identifier (e.g., `stringTag`, `objectTag`, `arrayTag`)
- `decoder`: Builder function for input validation (type checking)
- `encoder`: Builder function for converting from different schema types
- `parser`: Builder function for transformations after decoding (used by `S.shape`, `S.to`)
- `serializer`: Builder function for reverse transformations
- `to`: Target schema for transformations (set by `S.shape`, `S.to`)
- `from`: Path array indicating where this value comes from in shaped schemas
- `properties`: For object schemas, a dict of field name to schema
- `items`: For array/tuple schemas, an array of item schemas

#### Builder

A builder is a function with signature `(~input: val, ~selfSchema: internal) => val`. Builders generate JavaScript code at compile time by manipulating `val` objects. They are created using `Builder.make`:

```rescript
let myBuilder = Builder.make((~input, ~selfSchema) => {
  // Generate code and return output val
  let output = input->B.val(`someTransform(${input.var()})`, ~schema=selfSchema)
  output
})
```

#### Val (Value)

A compilation-time representation of a value being processed. Key fields:

- `inline`: The generated code expression (e.g., `i["foo"]`, `v0`)
- `var()`: Function to allocate/retrieve a variable name (use when value is referenced multiple times)
- `schema`: The schema of the current value
- `expected`: The schema we're trying to parse/convert into
- `from`: Link to the input val (for code merging)
- `code`: Generated code statements
- `validation`: Optional validation function to generate type checks
- `skipTo`: When `Some(true)`, prevents `parse` from following the `.to` chain
- `global`: Shared compilation context containing:
  - `embeded`: Array of embedded values (functions, constants) accessible as `e[n]`
  - `varCounter`: Counter for generating unique variable names

### Compilation Flow

When a schema operation is compiled (e.g., `parseOrThrow`), the following happens:

```
Input Schema
     │
     ▼
┌─────────────────────────────────────────────────────────┐
│  parse(val) function                                    │
│                                                         │
│  1. Encoder (if input.schema !== expected)              │
│     - Converts between different schema types           │
│                                                         │
│  2. Decoder (always runs)                               │
│     - Validates input type (e.g., typeof === "string")  │
│     - Generates validation code                         │
│                                                         │
│  3. Parser (if expected.parser exists)                  │
│     - Applies transformations (S.transform, S.shape)    │
│                                                         │
│  4. Recursive parse (if expected.to exists)             │
│     - Follows transformation chain                      │
│     - Skipped if val.skipTo === Some(true)              │
└─────────────────────────────────────────────────────────┘
     │
     ▼
Output Val with merged code
     │
     ▼
B.merge() → JavaScript function string
```

### Code Generation Example

For `S.object(s => s.field("foo", S.string))`:

```javascript
// Generated parse function:
(i) => {
  if (typeof i !== "object" || !i) {
    e[0](i);
  } // Object validation
  let v0 = i["foo"]; // Field access
  if (typeof v0 !== "string") {
    e[1](v0);
  } // String validation
  return v0; // Return parsed value
};
```

Where:

- `i` is the input argument
- `e` is the embedded values array (error throwers, transformers)
- `v0`, `v1`, etc. are allocated variables

### Key Functions

- `parse(val)`: Main compilation function that walks through encoder → decoder → parser → to chain
- `B.merge(val)`: Collects all generated code from the val chain into a single string
- `B.Val.cleanValFrom(val)`: Creates a clean copy of val for new code generation while preserving variable binding
- `B.embed(val, value)`: Embeds a runtime value (function, object) and returns reference like `e[0]`

### Shaped Schemas (S.shape, S.object with definer)

Shaped schemas use a proxy-based approach to track how values are used:

1. During schema definition, field accesses are tracked via `proxifyShapedSchema`
2. Each accessed field gets `from` set to its path (e.g., `["foo"]` for `s.field("foo", ...)`)
3. During parsing, `shapedParser` traverses the target structure and maps values from input
4. During serialization, `shapedSerializer` builds an accumulator (`acc`) that maps output paths to input vals, then `getShapedSerializerOutput` reconstructs the original structure

## PPX

### With Dune

Make sure running the below commands in `packages/sury-ppx/src`.

1. Create a sandbox with opam

```
opam switch create sury-ppx 5.3.0
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
