[⬅ Back to highlights](/README.md)

# JavaScript API reference

## Table of contents

- [Table of contents](#table-of-contents)
- [Install](#install)
- [Basic usage](#basic-usage)
  - [Parsing data](#parsing-data)
  - [Inferred types](#inferred-types)
  - [Serializing data](#serializing-data)
  - [Performance](#performance)
  - [JSON Schema](#json-schema)
  - [Standard Schema](#standard-schema)
- [Defining schemas](#defining-schemas)
  - [Advanced schemas](#advanced-schemas)
- [Strings](#strings)
  - [Custom error messages](#custom-error-messages)
  - [ISO datetimes](#iso-datetimes)
- [Numbers](#numbers)
- [Optionals](#optionals)
- [Nullables](#nullables)
- [Nullish](#nullish)
- [Objects](#objects)
  - [Literal fields](#literal-fields)
  - [Advanced object schema](#advanced-object-schema)
  - [`strict`](#strict)
  - [`strip`](#strip)
  - [`deepStrict` & `deepStrip`](#deepstrict--deepstrip)
  - [`merge`](#merge)
- [Arrays](#arrays)
- [Tuples](#tuples)
  - [Advanced tuple schema](#advanced-tuple-schema)
- [Unions](#unions)
  - [Discriminated unions](#discriminated-unions)
  - [Enums](#enums)
  - [Decoding into / out of a union](#decoding-into--out-of-a-union)
- [Records](#records)
- [JSON](#json)
- [JSON string](#json-string)
- [Date](#date)
- [ISO DateTime](#iso-datetime)
- [Instance](#instance)
- [Meta](#meta)
- [Custom schema](#custom-schema)
- [Recursive schemas](#recursive-schemas)
- [Refinements](#refinements)
- [Transforms](#transforms)
  - [`transform`](#transforms)
  - [`shape`](#shape)
- [Functions on schema](#functions-on-schema)
  - [The mental model: pipelines, not operations](#the-mental-model-pipelines-not-operations)
  - [Built-in operations](#built-in-operations)
  - [Chaining operations](#chaining-operations)
  - [`reverse`](#reverse)
  - [`to`](#to)
  - [`standard`](#standard)
  - [`name`](#name)
  - [`toExpression`](#toExpression)
- [Error handling](#error-handling)
- [Comparison](#comparison)
- [Global config](#global-config)
  - [`defaultAdditionalItems`](#defaultAdditionalItems)
  - [`disableNanNumberValidation`](#disablenannumbervalidation)

## Install

```sh
npm install sury
```

> 🧠 You don't need to install [ReScript](https://rescript-lang.org/) compiler for the library to work.

## Basic usage

The main building block of **Sury** is a schema. You can think of it as a type definition that exists at runtime - giving you infinite possibilities of using it.

Let's start with a simple object schema for the purpose of this guide. I use the same example as [Zod v4](https://v4.zod.dev/basics) docs so you can easily compare the two.

```ts
import * as S from "sury"; // 4.3 kB (min + gzip)

const playerSchema = S.schema({
  username: S.string,
  xp: S.number,
});
```

> 🧠 The API is very similar to TypeScript types, so you don't need to learn a new syntax.

### Parsing data

The most basic use-case for a schema is to parse unknown data. If the data is valid, the function will return a strongly-typed deep clone of the input. (With stripped fields by default)

```ts
S.parser(playerSchema)({ username: "billie", xp: 100 });
// => returns { username: "billie", xp: 100 }
```

If the data is invalid, the function will throw an error.

```ts
S.parser(playerSchema)({ username: "billie", xp: "not a number" });
// => throws S.Error: Failed at ["xp"]: Expected number, got string
```

**Sury** API explicitly tells you that it might throw an error. If you need you can catch it and perform `err instanceof S.Error` check. But **Sury** provides a convenient API which does it for you:

```ts
const result = S.safe(() =>
  S.parser(playerSchema)({ username: "billie", xp: "not a number" })
);
// Or for async operations:
const result = await S.safeAsync(() =>
  S.asyncParser(playerSchema)({ username: "billie", xp: "not a number" })
);

// The result type is a discriminated union, so you can handle both cases conveniently:
if (!result.success) {
  result.error; // handle error
} else {
  result.value; // do stuff
}
```

> 🧠 Besides `parser` there are also built-in operations to transform the data without validation, assert without allocating output, serialize back to the initial format and more. For more advanced pipelines, chain schemas with `S.decoder(input, …intermediate, output)` or `S.encoder(output, …intermediate, input)`.

### Inferred types

**Sury** automatically infers the static type from the schema definition. It has a really nice type on hover, which you can extract by using `S.Infer<typeof schema>`, `S.Output<typeof schema>`, or `S.Input<typeof schema>`.

```ts
const playerSchema = S.schema({
  username: S.string,
  xp: S.number,
});
//? S.Schema<{ username: string; xp: number }, { username: string; xp: number }>

type Player = S.Infer<typeof playerSchema>;

// Use it in your code
const player: Player = { username: "billie", xp: 100 };
```

### Serializing data

If you wonder why the schema needs an `Input` type, it's because **Sury** supports serializing data back to the initial format.

```ts
S.encoder(playerSchema)({ username: "billie", xp: 100 });
// => returns { username: "billie", xp: 100 }
```

Doesn't look like a big deal, with the example above. But if you have a more complex schema with transformations, it can be really useful.

```ts
// 1. Create some advanced schema with transformations
//    S.to - for easy & fast coercion
//    S.shape - for fields transformation
//    S.meta - with examples in Output format
const userSchema = S.schema({
  USER_ID: S.string.with(S.to, S.bigint),
  USER_NAME: S.string,
})
  .with(S.shape, (input) => ({
    id: input.USER_ID,
    name: input.USER_NAME,
  }))
  .with(S.meta, {
    description: "User entity in our system",
    examples: [
      {
        id: 0n,
        name: "Dmitry",
      },
    ],
  });
// On hover:
// S.Schema<{
//     id: bigint;
//     name: string;
// }, {
//     USER_ID: string;
//     USER_NAME: string;
// }>

// 2. You can use it for parsing Input to Output
S.parser(userSchema)({
  USER_ID: "0",
  USER_NAME: "Dmitry",
});
// { id: 0n, name: "Dmitry" }
// See how "0" is turned into 0n and fields are renamed

// 3. And reverse the schema and use it for parsing Output to Input
S.parser(S.reverse(userSchema))({
  id: 0n,
  name: "Dmitry",
});
// { USER_ID: "0", USER_NAME: "Dmitry" }
// Just use `S.reverse` and get a full-featured schema with switched `Output` and `Input` types
// Note: You can use `S.encoder(schema)(data)` if you don't need to perform validation
```

### Performance

This is not really about usage, but what you should be aware of is that **Sury** will most likely outperform not only other libraries, but also your own hand-rolled validation logic.

```ts
// This is how S.parser(userSchema)(data) is compiled
(i) => {
  if (typeof i !== "object" || !i) {
    e[3](i);
  }
  let v0 = i["USER_ID"],
    v2 = i["USER_NAME"];
  if (typeof v0 !== "string") {
    e[0](v0);
  }
  let v1;
  try {
    v1 = BigInt(v0);
  } catch (_) {
    e[1](v0);
  }
  if (typeof v2 !== "string") {
    e[2](v2);
  }
  return { id: v1, name: v2 };
};
```

```ts
// This is how S.encoder(userSchema)(data) is compiled
(i) => {
  let v0 = i["id"];
  return { USER_ID: "" + v0, USER_NAME: i["name"] };
};
```

So if you need the fastest possible parsing/serializing - **Sury** is the way to go ⭐

### JSON Schema

**Sury** internal representation is very simple and alike to JSON Schema, so you can use it directly.

```ts
console.log(
  S.schema("Hello world!").with(S.meta, { description: "Your greeting :)" })
);
// {
//   type: "string",
//   const: "Hello world!",
//   description: "Your greeting :)",
//   ...a few internal properties
// }
```

But for better interoperability, you can convert it to the official JSON Schema specification. Let's take the `User` schema from the example above and convert it:

```ts
S.toJSONSchema(userSchema);
// {
//   type: "object",
//   additionalProperties: true,
//   properties: {
//     USER_ID: {
//       type: "string",
//     },
//     USER_NAME: {
//       type: "string",
//     },
//   },
//   required: ["USER_ID", "USER_NAME"],
//   description: "User entity in our system",
//   examples: [
//     {
//       USER_ID: "0",
//       USER_NAME: "Dmitry",
//     },
//   ],
// }
```

See how all the properties and examples are in the Input format. It's just asking to put itself to Fastify or any other server with OpenAPI integration 😁

If that's not cool enough for you, you can also turn a JSON Schema into a **Sury** schema:

```ts
S.assert(
  S.fromJSONSchema({
    type: "string",
    format: "email",
  }),
  "example.com"
);
// Throws S.Error: Invalid email address
```

### Standard Schema

**Sury** implements a [Standard Schema](https://standardschema.dev/) specification which is already integrated with over 32 popular libraries.

Here's an example how you can use **Sury** to generate structured data using [xsAI](https://xsai.js.org/):

```ts
import { generateObject } from "@xsai/generate-object";
import { env } from "node:process";
import * as S from "sury";

const { object } = await generateObject({
  apiKey: env.OPENAI_API_KEY!,
  baseURL: "https://api.openai.com/v1/",
  messages: [
    {
      content: "Extract the event information.",
      role: "system",
    },
    {
      content: "Alice and Bob are going to a science fair on Friday.",
      role: "user",
    },
  ],
  model: "gpt-4o",
  schema: S.schema({
    name: S.string,
    date: S.string,
    participants: S.array(S.string),
  }),
});
```

## Defining schemas

```ts
import * as S from "sury";

// Primitive values
S.string;
S.number;
S.int32;
S.boolean;
S.bigint;
S.symbol;
S.void;

// Literal values
// Supports any JS type
// Validated using strict equal checks
S.schema("tuna");
S.schema(12);
S.schema(2n);
S.schema(true);
S.schema(undefined);
S.schema(null);
S.schema(Symbol("terrific"));

// NaN literals
// Validated using Number.isNaN
S.schema(NaN);

// Catch-all type
// Allows any value
S.unknown;
S.any;

// Never type
// Allows no values
S.never;
```

### Advanced schemas

> 🧠 Don't forget `S.to` which comes with powerful coercion logic.

```ts
// JSON type
// Allows string | boolean | number | null | Record<string, JSON> | JSON[]
S.json;

// JSON string
// Asserts that the input is a valid JSON string
S.jsonString;
S.jsonStringWithSpace(2);
// Parses JSON string and validates that it's a number
// JSON string -> number
S.jsonString.with(S.to, S.number);
// Serializes number to JSON string
S.number.with(S.to, S.jsonString);

// Asserts that the input is a Date instance and not Invalid Date
S.date;

// Asserts that the input is an instance of Uint8Array
S.uint8Array;
// Decodes Uint8Array to utf-8 string
S.uint8Array.with(S.to, S.string);
// Encodes utf-8 string to Uint8Array
S.string.with(S.to, S.uint8Array);
```

## Strings

**Sury** includes a handful of string-specific refinements and transforms:

```ts
S.max(S.string, 5); // String must be 5 or fewer characters long
S.min(S.string, 5); // String must be 5 or more characters long
S.length(S.string, 5); // String must be exactly 5 characters long
S.string.with(S.pattern, /[0-9]/); // Invalid pattern

S.trim(S.string); // trim whitespaces
```

For format-specific string validation, use the standalone schemas:

```ts
S.email; // Standalone email schema
S.url; // Standalone URL schema
S.uuid; // Standalone UUID schema
S.cuid; // Standalone CUID schema
```

> For ISO 8601 UTC datetime strings use the dedicated standalone `S.isoDateTime` schema — see [ISO datetimes](#iso-datetimes) below.

> ⚠️ Validating email addresses is nearly impossible with just code. Different clients and servers accept different things and many diverge from the various specs defining "valid" emails. The ONLY real way to validate an email address is to send a verification email to it and check that the user got it. With that in mind, Sury picks a relatively simple regex that does not cover all cases.

When using built-in refinements, you can provide a custom error message.

```ts
S.min(S.string, 1, "String can't be empty");
S.length(S.string, 5, "SMS code should be 5 digits long");
```

### Custom error messages

Built-in refinements accept an optional last argument for a custom error message:

```ts
S.min(S.string, 5, "Too short");
S.pattern(S.string, /^\d+$/, "Must be numeric");
```

For standalone schemas or more control, use `S.meta` with the `errorMessage` field:

```ts
// Override a specific constraint message
S.email.with(S.meta, { errorMessage: { format: "Must be a valid email" } });

// Use "_" as a catch-all for any constraint
S.email.with(S.meta, { errorMessage: { _: "Invalid input" } });

// Reset error messages (removes all overrides)
schema.with(S.meta, { errorMessage: {} });
```

Available keys: `format`, `type`, `minimum`, `maximum`, `minLength`, `maxLength`, `minItems`, `maxItems`, `pattern`, `_` (catch-all).

### ISO datetimes

`S.isoDateTime` is a **standalone** string schema (`S.Schema<string, string>`) that validates ISO 8601 UTC datetime strings: no timezone offsets allowed, with arbitrary sub-second decimal precision.

```ts
const schema = S.isoDateTime;
// schema has the type S.Schema<string, string>

S.parser(schema)("2020-01-01T00:00:00Z"); // pass
S.parser(schema)("2020-01-01T00:00:00.123Z"); // pass
S.parser(schema)("2020-01-01T00:00:00.123456Z"); // pass (arbitrary precision)
S.parser(schema)("2020-01-01T00:00:00+02:00"); // fail (no offsets allowed)
```

To decode an ISO datetime string into a `Date`, combine it with `S.to(S.date)`:

```ts
const schema = S.to(S.string, S.date);
// schema has the type S.Schema<Date, string>
```

## Numbers

**Sury** includes some of number-specific refinements:

```ts
S.max(S.number, 5); // Number must be lower than or equal to 5
S.min(S.number, 5); // Number must be greater than or equal to 5
```

Optionally, you can pass in a second argument to provide a custom error message.

```ts
S.max(S.number, 5, "this👏is👏too👏big");
```

## Optionals

You can make any schema optional with `S.optional`.

```ts
const schema = S.optional(S.string);

S.parser(schema)(undefined); // => returns undefined
type A = S.Infer<typeof schema>; // string | undefined
```

You can pass a default value to the second argument of `S.optional`.

```ts
const stringWithDefaultSchema = S.optional(S.string, "tuna");

S.parser(stringWithDefaultSchema)(undefined); // => returns "tuna"
type A = S.Infer<typeof stringWithDefaultSchema>; // string
```

Optionally, you can pass a function as a default value that will be re-executed whenever a default value needs to be generated:

```ts
const numberWithRandomDefault = S.optional(S.number, Math.random);

S.parser(numberWithRandomDefault)(undefined); // => 0.4413456736055323
S.parser(numberWithRandomDefault)(undefined); // => 0.1871840107401901
S.parser(numberWithRandomDefault)(undefined); // => 0.7223408162401552
```

Conceptually, this is how **Sury** processes default values:

1. If the input is `undefined`, the default value is returned
2. Otherwise, the data is parsed using the base schema

## Nullables

Similarly, you can create nullable types with `S.nullable`.

```ts
const nullableStringSchema = S.nullable(S.string);
S.parser(nullableStringSchema)("asdf"); // => "asdf"
S.parser(nullableStringSchema)(null); // => undefined
```

Notice how the `null` input transformed to `undefined`.

## Nullish

A convenience method that returns a "nullish" version of a schema. Nullish schemas will accept both `undefined` and `null`. Read more about the concept of "nullish" [in the TypeScript 3.7 release notes](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-3-7.html#nullish-coalescing).

```ts
const nullishStringSchema = S.nullish(S.string);
S.parser(nullishStringSchema)("asdf"); // => "asdf"
S.parser(nullishStringSchema)(null); // => null
S.parser(nullishStringSchema)(undefined); // => undefined
```

## Objects

```ts
// all properties are required by default
const dogSchema = S.schema({
  name: S.string,
  age: S.number,
});

// extract the inferred type like this
type Dog = S.Infer<typeof dogSchema>;

// equivalent to:
type Dog = {
  name: string;
  age: number;
};
```

### Literal fields

Besides passing schemas for values in `S.schema`, you can also pass **any** Js value and it'll be treated as a literal field.

```ts
const meSchema = S.schema({
  id: S.number,
  name: "Dmitry Zakharov",
  age: 23,
  kind: "human",
  metadata: {
    description: "What?? Even an object with NaN works! Yes 🔥",
    money: NaN,
  } ,
});
```

You can add `as const` or wrap the value with `S.schema` to adjust the schema type. The example below turns the `kind` field to be a `"human"` type instead of `string`:

```ts
S.schema({
  kind: "human" as const,
  // Or
  kind: S.schema("human"),
});
```

This is useful for discriminated unions.

### Advanced object schema

Sometimes you want to transform the data coming to your system. You can easily do it by passing a function to the `S.object` schema.

```ts
const userSchema = S.object((s) => ({
  id: s.field("USER_ID", S.number),
  name: s.field("USER_NAME", S.string),
}));

S.parser(userSchema)({
  USER_ID: 1,
  USER_NAME: "John",
});
// => returns { id: 1, name: "John" }

// Infer output TypeScript type of the userSchema
type User = S.Infer<typeof userSchema>; // { id: number; name: string }
```

Compared to using custom transformation functions, the approach has 0 performance overhead. Also, you can use the same schema to convert the parsed data back to the initial format:

```ts
S.encoder(userSchema)({
  id: 1,
  name: "John",
});
// => returns { USER_ID: 1, USER_NAME: "John" }
```

### `strict`

By default **Sury** object schema strip out unrecognized keys during parsing. You can disallow unknown keys with `S.strict` function. If there are any unknown keys in the input, **Sury** will fail with an error.

```ts
const personSchema = S.strict(
  S.schema({
    name: S.string,
  })
);

S.parser(personSchema)({
  name: "bob dylan",
  extraKey: 61,
});
// => throws S.Error
```

If you want to change it for all schemas in your app, you can use `S.global` function:

```ts
S.global({
  defaultAdditionalItems: "strict",
});
```

### `strip`

Use the `S.strip` function to reset an object schema to the default behavior (stripping unrecognized keys).

### `deepStrict` & `deepStrip`

Both `S.strict` and `S.strip` are applied for the first level of the object schema. If you want to apply it for all nested schemas, you can use `S.deepStrict` and `S.deepStrip` functions.

```ts
const schema = S.schema({
  bar: {
    baz: S.string,
  },
});

S.strict(schema); // { "baz": string } will still allow unknown keys
S.deepStrict(schema); // { "baz": string } will not allow unknown keys
```

### `merge`

You can add additional fields to an object schema with the `merge` function.

```ts
const baseTeacherSchema = S.schema({ students: S.array(S.string) });
const hasIDSchema = S.schema({ id: S.string });

const teacherSchema = S.merge(baseTeacherSchema, hasIDSchema);
type Teacher = S.Infer<typeof teacherSchema>; // => { students: string[], id: string }
```

> 🧠 The function will throw if the schemas share keys. The returned schema also inherits the "unknownKeys" policy (strip/strict) of B.

## Arrays

```ts
const stringArraySchema = S.array(S.string);
```

**Sury** includes some of array-specific refinements:

```ts
S.max(S.array(S.string), 5); // Array must be 5 or fewer items long
S.min(S.array(S.string), 5); // Array must be 5 or more items long
S.length(S.array(S.string), 5); // Array must be exactly 5 items long
```

### Compact Columns

```ts
const schema = S.compactColumns(
  S.schema({
    id: S.string,
    name: S.nullable(S.string),
    deleted: S.boolean,
  })
);

const value = S.encoder(schema)([
  { id: "0", name: "Hello", deleted: false },
  { id: "1", name: undefined, deleted: true },
]);
// [["0", "1"], ["Hello", null], [false, true]]
```

The helper function is inspired by the article [Boosting Postgres INSERT Performance by 2x With UNNEST](https://www.timescale.com/blog/boosting-postgres-insert-performance). It allows you to flatten a nested array of objects into arrays of values by field.

The main concern of the approach described in the article is usability. And **Sury** completely solves the problem, providing a simple and intuitive API that is even more performant than `S.array`.

<details>

<summary>
Checkout the compiled code yourself:
</summary>

```javascript
(i) => {
  let v1 = [new Array(i.length), new Array(i.length), new Array(i.length)];
  for (let v0 = 0; v0 < i.length; ++v0) {
    let v3 = i[v0];
    try {
      let v4 = v3["name"];
      if (v4 === void 0) {
        v4 = null;
      }
      v1[0][v0] = v3["id"];
      v1[1][v0] = v4;
      v1[2][v0] = v3["deleted"];
    } catch (v2) {
      if (v2 && v2.s === s) {
        v2.path = "" + "[\"'+v0+'\"]" + v2.path;
      }
      throw v2;
    }
  }
  return v1;
};
```

</details>

## Tuples

Unlike arrays, tuples have a fixed number of elements and each element can have a different type.

```ts
const athleteSchema = S.schema([
  S.string, // name
  S.number, // jersey number
  {
    pointsScored: S.number,
  }, // statistics
]);

type Athlete = S.Infer<typeof athleteSchema>;
// type Athlete = [string, number, { pointsScored: number }]
```

### Advanced tuple schema

Sometimes you want to transform incoming tuples to a more convenient data-structure. To do this you can pass a function to the `S.tuple` schema.

```ts
const athleteSchema = S.tuple((s) => ({
  name: s.item(0, S.string),
  jerseyNumber: s.item(1, S.number),
  statistics: s.item(
    2,
    S.schema({
      pointsScored: S.number,
    })
  ),
}));

type Athlete = S.Infer<typeof athleteSchema>;
// type Athlete = {
//   name: string;
//   jerseyNumber: number;
//   statistics: {
//     pointsScored: number;
//   };
// }
```

That looks much better than before. And the same as for advanced objects, you can use the same schema for transforming the parsed data back to the initial format. Also, it has 0 performance overhead and is as fast as parsing tuples without the transformation.

## Unions

An union represents a logical OR relationship. You can apply this concept to your schemas with `S.union`. The same api works for discriminated unions as well.

The schema function `union` creates an OR relationship between any number of schemas that you pass as the first argument in the form of an array. On validation, the schema returns the result of the first schema that was successfully validated.

> 🧠 Schemas are not guaranteed to be validated in the order they are passed to `S.union`. They are grouped by the input data type to optimise performance and improve error message. Schemas with unknown data typed validated the last.

```ts
// TypeScript type for reference:
// type Union = string | number;

const stringOrNumberSchema = S.union([S.string, S.number]);

S.parser(stringOrNumberSchema)("foo"); // passes
S.parser(stringOrNumberSchema)(14); // passes
```

### Discriminated unions

```typescript
// TypeScript type for reference:
// type Shape =
// | { kind: "circle"; radius: number }
// | { kind: "square"; x: number }
// | { kind: "triangle"; x: number; y: number };

const shapeSchema = S.union([
  {
    kind: "circle" as const,
    radius: S.number,
  },
  {
    kind: "square" as const,
    x: S.number,
  },
  {
    kind: "triangle" as const,
    x: S.number,
    y: S.number,
  },
]);
```

### Enums

Creating a schema for a enum-like union was never so easy:

```ts
const schema = S.union(["Win", "Draw", "Loss"]);

type Schema = S.Infer<typeof schema>; // "Win" | "Draw" | "Loss"
```

### Decoding into / out of a union

When you compile `source -> targetUnion` (via `S.to`, or implicitly by reversing the schema), Sury picks the target variant using a three-tier algorithm based on the source's **derived tag** — the tag known at compile time, which may be narrower than the original type (an upstream transformation can refine it). If the source is itself a union, the algorithm runs independently for each source variant.

If the source is `unknown` (no derived tag), the tag-based tiers are skipped and target variants are simply attempted in target-union order at runtime.

1. **Same-tag group.** Collect target variants sharing the source's tag. If non-empty, match only within this group: variants with a matching `const`/`format` (string literals, `Int32`, etc.) are tried first in target-union order, then any remaining catch-all same-tag variants. Variants with a different tag are never tried from here — if every branch in the group fails, the match errors.
2. **Nullish bridge.** Used only when tier 1 is empty. If the source tag is `null` or `undefined`, use the opposite nullish target variant (if present), exclusively.
3. **Fallback.** Used only when tiers 1 and 2 are both empty. Build a decoder for every target variant in target-union order. Cross-type coercions live here: `number`/`bigint` → `string` via `"" + i`, `string` → `number` via `+i`, `string` → `bigint` via `BigInt(i)`, stringified-const matches like `"null" → null`, and more.

**Worked example** — `S.union([S.bigint, S.number, null]).with(S.to, S.union([S.string, undefined]))`:

Forward:

- `123n` → `"123"` (tier 3: bigint → string)
- `123.12` → `"123.12"` (tier 3: number → string)
- `null` → `undefined` (tier 2: nullish bridge)

Reverse (via `S.encoder`):

- `"null"` → `null` (tier 3: stringified-const literal match)
- `undefined` → `null` (tier 2: nullish bridge)
- `"123"` → `123n` (tier 3: bigint attempted first by target order; parse succeeds)
- `"123.12"` → `123.12` (tier 3: bigint parse throws, falls through to number)
- `"abc"` → error (tier 3: no variant's decoder succeeds)

**Identity wins over coercion.** For `S.union([S.string, S.bigint]).with(S.to, S.union([S.number, S.string]))`:

- `"123"` → `"123"` (tier 1: `string` matches `string`, never coerced to `number` even though a `number` target exists)
- `123n` → `"123"` (tier 3: no `bigint` target, falls through to `string` via `"" + i`)

To opt into `string → number` when a `string` target also exists, write the transform into a variant explicitly:

```ts
S.union([S.string.with(S.to, S.number), S.string]);
```

The transformed variant is const/format-refined relative to the catch-all `string` and matches first within tier 1.

> 🧠 Union conversion always performs exhaustive validation now — every variant is checked, so transformed unions stay consistent across decode and encode.

## Records

Record schema is used to validate types such as `{ [k: string]: number }`.

If you want to validate the values of an object against some schema but don't care about the keys, use `S.record(valueSchema)`:

```ts
const numberCacheSchema = S.record(S.number);

type NumberCache = S.Infer<typeof numberCacheSchema>;
// => { [k: string]: number }
```

## Date

`S.date` validates that the input is a `Date` instance and rejects Invalid Date.

```ts
S.parser(S.date)(new Date()); // passes
S.parser(S.date)(new Date("2024-01-01T00:00:00Z")); // passes
S.parser(S.date)(new Date("invalid")); // throws
S.parser(S.date)("2024-01-01"); // throws - not a Date instance
```

> Unlike `S.isoDateTime` (which validates ISO datetime strings) and `S.to(S.string, S.date)` (which decodes ISO strings into Date objects), `S.date` validates existing Date instances directly.

You can use `S.decoder` with multiple arguments to decode between strings and dates:

```ts
// Decode ISO string to Date
S.decoder(S.string, S.date)("2024-01-01T00:00:00.000Z"); // Date

// Decode Date to ISO string
S.decoder(S.date, S.string)(new Date("2024-01-01T00:00:00.000Z")); // "2024-01-01T00:00:00.000Z"
```

## ISO DateTime

`S.Schema<string, string>`

```ts
const schema = S.isoDateTime;

S.parser(schema)("2020-01-01T00:00:00Z"); // "2020-01-01T00:00:00Z"
S.parser(schema)("not-a-date"); // throws
```

Standalone string schema that validates ISO 8601 UTC datetime strings. See also [ISO datetimes](#iso-datetimes) under Strings for more details and examples.

## Instance

You can use `S.instance` to check that the input is an instance of a class. This is useful to validate inputs against classes that are exported from third-party libraries.

```ts
class Test {
  name: string;
}

const testSchema = S.instance(Test);

const blob: any = "whatever";
S.parser(testSchema)(new Test()); // passes
S.parser(testSchema)(blob); // throws S.Error: Expected Test, received "whatever"
```

## Meta

Use `S.meta` to add metadata to the resulting schema.

```ts
const documentedStringSchema = S.string.with(S.meta, {
  description: "A useful bit of text, if you know what to do with it.",
});

documentedStringSchema.description; // A useful bit of text…
```

This can be useful for documenting fields, generating JSON, etc.

```ts
S.toJSONSchema(documentedStringSchema);
// {
//   "type": "string",
//   "description": "A useful bit of text, if you know what to do with it."
// }
```

## Brand

Add a type-only symbol to an existing type so that only values produced by validation satisfy it.

Use `S.brand` to attach a nominal brand to a schema's output. This is a TypeScript-only marker: it does not change runtime behavior. Combine it with `S.refine` (or any validation) so only validated values can acquire the brand.

```ts
// Brand a string as a UserId
const userIdSchema = S.string.with(S.brand, "UserId");
type UserId = S.Infer<typeof userIdSchema>; // S.Brand<string, "UserId">

const id: UserId = S.parser(userIdSchema)("u_123"); // OK
const asString: string = id; // OK: branded value is assignable to string
// @ts-expect-error - A plain string is not assignable to a branded string
const notId: UserId = "u_123";
```

You can define brands for refined constraints, like even numbers:

```ts
const evenSchema = S.number
  .with(S.refine, (value) => value % 2 === 0, {
    error: "Expected an even number",
  })
  .with(S.brand, "even");

type Even = S.Infer<typeof evenSchema>; // S.Brand<number, "even">

const good: Even = S.parser(evenSchema)(2); // OK
// @ts-expect-error - number is not assignable to brand "even"
const bad: Even = 5;
```

For more information on branding in general, check out [this excellent article](https://www.learningtypescript.com/articles/branded-types) from [Josh Goldberg](https://github.com/joshuakgoldberg).

## Custom schema

**Sury** might not have many built-in schemas for your use case. In this case you can create a custom schema for any TypeScript type.

1. Choose a base schema which is the closest to your type. Most likely it'll be `S.instance`.
2. Use `S.to` to add a custom decode and encode logic.
3. Optionally, use `S.meta` to add customize the name of the schema and additional metadata.

```ts
const mySet = <T>(itemSchema: S.Schema<T>): S.Schema<Set<T>> =>
  S.instance(Set<unknown>)
    .with(S.to, S.instance(Set<T>), (input) => {
      const output = new Set<T>();
      input.forEach((item, index) => {
        try {
          output.add(S.parser(itemSchema)(item));
        } catch (e) {
          if (e instanceof S.Error) {
            throw new Error(`At item ${index} - ${e.reason}`);
          }
          throw e;
        }
      });
      return output;
    })
    .with(S.meta, {
      name: `Set<${S.toExpression(itemSchema)}>`,
    });

const numberSetSchema = mySet(S.number);
type NumberSet = S.Infer<typeof numberSetSchema>; // Set<number>

S.parser(numberSetSchema)(new Set([1, 2, 3])); // passes
S.parser(numberSetSchema)(new Set([1, 2, "3"])); // throws S.Error: At item 3 - Expected number, received "3"
S.parser(numberSetSchema)([1, 2, 3]); // throws S.Error: Expected Set<number>, received [1, 2, 3]
```

## Recursive schemas

You can define a recursive schema in **Sury**. Unfortunately, TypeScript derives the Schema type as `unknown` so you need to explicitly specify the type and it'll start correctly typechecking.

```ts
type Node = {
  id: string;
  children: Node[];
};

const nodeSchema = S.recursive<Node, Node>("Node", (nodeSchema) =>
  S.schema({
    id: S.string,
    children: S.array(nodeSchema),
  })
);
```

> 🧠 Despite supporting recursive schema, passing cyclical data will cause an infinite loop.

## Refinements

**Sury** lets you provide custom validation logic via refinements. Refinements let you define checks that are not expressible in the type system alone — for example, checking that a number is positive or that a string is a valid URL.

```ts
const positiveNumberSchema = S.number.with(S.refine, (value) => value > 0);
```

Refinement functions should return `true` to indicate success or `false` to signal failure. By default, a failed refinement throws with the message `"Refinement failed"`.

#### Custom error message

Provide a custom error message via the `error` option:

```ts
const shortStringSchema = S.string.with(S.refine, (value) => value.length <= 255, {
  error: "String can't be more than 255 characters",
});
```

#### Custom error path

When refining an object schema, you can use the `path` option to attach the error to a specific field:

```ts
const passwordFormSchema = S.schema({
  password: S.string,
  confirm: S.string,
}).with(S.refine, (data) => data.password === data.confirm, {
  error: "Passwords don't match",
  path: ["confirm"],
});
```

#### Chaining refinements

Refinements can be chained. Each refinement is applied in order:

```ts
const evenPositiveSchema = S.number
  .with(S.refine, (val) => val > 0, { error: "Must be positive" })
  .with(S.refine, (val) => val % 2 === 0, { error: "Must be even" });
```

The refine function is applied for both parsing and serializing.

Also, you can have an asynchronous assertion (for decoder only):

```ts
const userSchema = S.schema({
  id: S.uuid.with(S.asyncDecoderAssert, async (id) => {
    const isActiveUser = await checkIsActiveUser(id);
    if (!isActiveUser) {
      throw new Error(`The user ${id} is inactive.`);
    }
  }),
  name: S.string,
});

type User = S.Infer<typeof userSchema>; // { id: string, name: string }

// Need to use asyncParser for schemas with async transformations
await S.asyncParser(userSchema)({
  id: "1",
  name: "John",
});
```

### **`shape`**

The `S.shape` schema is a helper function that allows you to transform the value to a desired shape. It'll statically derive required data transformations to perform the change in the most optimal way.

> ⚠️ Even though it looks like you operate with a real value, it's actually a dummy proxy object. So conditions or any other runtime logic won't work. Please use `S.to` for such cases.

```typescript
const circleSchema = S.number.with(S.shape, (radius) => ({
  kind: "circle",
  radius: radius,
}));

S.parser(circleSchema)(1); //? { kind: "circle", radius: 1 }

// Also works in reverse 🔄
S.encoder(circleSchema)({ kind: "circle", radius: 1 }); //? 1
```

## Functions on schema

### The mental model: pipelines, not operations

If you've used other validation libraries, you're used to a separate function for every input/output pair: `parseJson`, `parseJsonString`, `convertToJson`, `convertToJsonString`, and so on. **Sury treats those targets as schemas instead.** `S.json`, `S.jsonString`, `S.unknown`, `S.date`, `S.uint8Array` — none of them are special, they're just schemas like any other.

The two operation functions you need are:

- **`S.decoder(from, …intermediate, to)`** — compile a forward pipeline from one schema to another.
- **`S.encoder(from, …intermediate, to)`** — compile the reverse pipeline.

Every call fuses the whole chain into a single ultra-optimized function generated via `new Function`, so adding stages costs you nothing at runtime.

```ts
// Validate unknown input.
S.parser(userSchema)(data);

// Parse a JSON string, then validate.
S.decoder(S.jsonString, userSchema)(rawString);

// Encode a domain value all the way out to a JSON string.
S.encoder(userSchema, S.jsonString)(user);

// Decode a binary payload of UTF-8 JSON, then validate.
S.decoder(S.uint8Array, S.jsonString, userSchema)(bytes);
```

You're no longer picking from a fixed menu of operations — you're describing the shape of the data at each stage and letting Sury compile the path.

> 🧠 `S.parser` and `S.assert` aren't separate primitives — they're just specializations of `S.decoder` with `S.unknown` on the input side. `S.parser(schema)` is `S.decoder(S.unknown, schema)`. `S.assert(schema, data)` runs a decoder from `S.unknown` *through* the schema *to* `S.literal(true).with(S.noValidation, true)` — the target is a no-op constant with validation disabled, so the compiler emits the schema's validation but no output-construction code at all. That's why `assert` is 2–3× faster than `parser`.

### Built-in operations

The library provides a bunch of built-in operations that can be used to parse, convert, and assert values.

Parsing means that the input value is validated against the schema and transformed to the expected output type. You can use the following operations to parse values:

| Operation      | Interface                                                       | Description                                                   |
| -------------- | --------------------------------------------------------------- | ------------------------------------------------------------- |
| S.parser       | `(Schema<Output, Input>) => (data: unknown) => Output`          | Parses any value with the schema                              |
| S.asyncParser  | `(Schema<Output, Input>) => (data: unknown) => Promise<Output>` | Parses any value with the schema having async transformations |

For advanced users you can only transform to the output type without type validations. But be careful, since the input type is not checked:

| Operation       | Interface                                                | Description                                                      |
| --------------- | -------------------------------------------------------- | ---------------------------------------------------------------- |
| S.decoder       | `(Schema<Output, Input>) => (Input) => Output`           | Converts input value to the output type                          |
| S.asyncDecoder  | `(Schema<Output, Input>) => (Input) => Promise<Output>`  | Converts input value to the output type with async transforms    |

Note, that in this case only type validations are skipped. If your schema has refinements or transforms, they will be applied.

Also, you can use `S.noValidation(schema, true)` helper to turn off type validations for the schema even when it's used with a parse operation.

More often than converting input to output, you'll need to perform the reversed operation. It's usually called "serializing" or "decoding". The ReScript Schema has a unique mental model and provides an ability to reverse any schema with `S.reverse` which you can later use with all possible kinds of operations. But for convinence, there's a few helper functions that can be used to convert output values to the initial format:

| Operation       | Interface                                              | Description                                                           |
| --------------- | ------------------------------------------------------ | --------------------------------------------------------------------- |
| S.encoder       | `(Schema<Output, Input>) => (Output) => Input`         | Converts schema value to the input type                               |
| S.asyncEncoder  | `(Schema<Output, Input>) => (Output) => Promise<Input>`| Converts schema value to the input type with async transformations    |

This is literally the same as convert operations applied to the reversed schema.

For some cases you might want to simply assert the input value is valid. For this there's `S.assert` operation:

| Operation | Interface                                                      | Description                                                                                                                                    |
| --------- | -------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------- |
| S.assert  | `(Schema<Output, Input>, data: unknown) asserts data is Input` | Asserts that the input value is valid. Since the operation doesn't return a value, it's 2-3 times faster than `parser` depending on the schema |

All operations either return the output value or throw an error. For convinient error handling you can use the `S.safe` and `S.safeAsync` helpers, which would catch the error an wrap it into a `Result` type:

```ts
const result = S.safe(() => S.parser(S.string)(123));
```

### Chaining operations

`S.decoder` and `S.encoder` accept multiple schemas to build a single fused pipeline. The first schema is the input side and the last is the output side; intermediate schemas act as stages.

```ts
// Decode a JSON string into your domain type in one pass
const parseJsonString = S.decoder(S.jsonString, userSchema);
parseJsonString('{"id":"1","name":"John"}');

// Encode your domain type to a JSON string in one pass
const stringifyUser = S.encoder(userSchema, S.jsonString);
stringifyUser({ id: "1", name: "John" });
```

This covers the use cases that previously needed `S.compile` — see the [migration cheat sheet](/IDEAS.md#typescript--javascript) for the full mapping.

### **`reverse`**

```ts
S.reverse(S.nullable(S.string));
// S.optional(S.string)
```

```ts
const schema = S.object((s) => s.field("foo", S.string));

S.parser(schema)({ foo: "bar" });
// "bar"

const reversed = S.reverse(schema);

S.parser(reversed)("bar");
// {"foo": "bar"}

S.parser(reversed)(123);
// throws S.error with the message: `Expected string, received 123`
```

Reverses the schema. This gets especially magical for schemas with transformations 🪄

### **`to`**

This very powerful API allows you to coerce another data type in a declarative way. Let's say you receive a number that is passed to your system as a string. For this `S.to` is the best fit:

```ts
const schema = S.string.with(S.to, S.number);

S.parser(schema)("123"); //? 123.
S.parser(schema)("abc"); //? throws: Expected number, received "abc"

// Reverse works correctly as well 🔥
S.encoder(schema)(123); //? "123"
```

#### Custom transformations

You can also provide a custom transformation function to the `S.to` operation. This is useful when you need to perform a more complex transformation than the built-in ones.

```ts
const schema = S.string.with(
  S.to,
  S.number,
  // Custom decode function
  (string) => {
    const number = parseInt(string, 10);
    if (Number.isNaN(number)) {
      throw new Error("Invalid number");
    }
    return number;
  },
  // Custom encode function
  (number) => {
    return number.toString();
  }
);

S.parser(schema)("123"); //? 123
S.parser(schema)("abc"); //? throws: Invalid number

S.encoder(schema)(123); //? "123"
```

> 🧠 Prefer to use built-in `S.string.with(S.to, S.number)` instead of custom transformation functions when possible.

### **`name`**

```ts
const schema = S.schema({ abc: 123 }).with(S.meta, { name: "Abc" });

schema.name; // "Abc"
```

Used internally for readable error messages.

### **`toExpression`**

```ts
S.toExpression(S.schema({ abc: 123 }));
// "{ abc: 123; }"

S.toExpression(S.name(S.string, "Address"));
// "Address"
```

Used internally for readable error messages.

> 🧠 The format subject to change

## Error handling

**Sury** throws `S.Error` which is a subclass of Error class. It contains detailed information about the operation problem.

```ts
S.parser(S.schema(false))(true);
// => Throws S.Error with the following message: Expected false, received true".
```

You can catch the error using `S.safe` and `S.safeAsync` helpers:

```ts
const result = S.safe(() => S.parser(S.schema(false))(true));

if (result.success) {
  console.log(result.value);
} else {
  console.log(result.error);
}
```

Or the async version:

```ts
const result = await S.safeAsync(async () => {
  const passed = await S.asyncParser(S.boolean)(data);
  return passed ? 1 : 0;
});
```

As you can notice, you can have more logic inside of the safe function callback and still be sure that the error will be caught in a functional way.

## Global config

**Sury** has a global config that can be changed to customize the behavior of the library.

### `defaultAdditionalItems`

`defaultAdditionalItems` is an option that controls how unknown keys are handled when parsing objects. The default value is `strip`, but you can globally change it to `strict` to enforce strict object parsing.

```rescript
S.global({
  defaultAdditionalItems: "strict",
})
```

### `disableNanNumberValidation`

`disableNanNumberValidation` is an option that controls whether the library should check for NaN values when parsing numbers. The default value is `false`, but you can globally change it to `true` to allow NaN values. If you parse many numbers which are guaranteed to be non-NaN, you can set it to `true` to improve performance ~10%, depending on the case.

```rescript
S.global({
  disableNanNumberValidation: true,
})
```
