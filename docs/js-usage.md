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
- [Strings](#strings)
  - [ISO datetimes](#iso-datetimes)
- [Numbers](#numbers)
- [Optionals](#optionals)
- [Nullables](#nullables)
- [Nullish](#nullish)
- [Objects](#objects)
  - [Literal shorthand](#literal-shorthand)
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
- [Records](#records)
- [JSON](#json)
- [JSON string](#json-string)
- [Instance](#instance)
- [Meta](#meta)
- [Custom schema](#custom-schema)
- [Recursive schemas](#recursive-schemas)
- [Refinements](#refinements)
- [Transforms](#transforms)
  - [`transform`](#transforms)
  - [`shape`](#shape)
- [Functions on schema](#functions-on-schema)
  - [Built-in operations](#built-in-operations)
  - [`compile`](#compile)
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

const Player = S.schema({
  username: S.string,
  xp: S.number,
});
```

> 🧠 The API is very similar to TypeScript types, so you don't need to learn a new syntax.

### Parsing data

The most basic use-case for a schema is to parse unknown data. If the data is valid, the function will return a strongly-typed deep clone of the input. (With stripped fields by default)

```ts
S.parseOrThrow({ username: "billie", xp: 100 }, Player);
// => returns { username: "billie", xp: 100 }
```

If the data is invalid, the function will throw an error.

```ts
S.parseOrThrow({ username: "billie", xp: "not a number" }, Player);
// => throws S.Error: Failed parsing at ["xp"]: Expected number, got string
```

**Sury** API explicitly tells you that it might throw an error. If you need you can catch it and perform `err instanceof S.Error` check. But **Sury** provides a convenient API which does it for you:

```ts
const result = S.safe(() =>
  S.parseOrThrow({ username: "billie", xp: "not a number" }, Player)
);
// Or for async operations:
const result = await S.safeAsync(() =>
  S.parseAsyncOrThrow({ username: "billie", xp: "not a number" }, Player)
);

// The result type is a discriminated union, so you can handle both cases conveniently:
if (!result.success) {
  result.error; // handle error
} else {
  result.data; // do stuff
}
```

> 🧠 Besides `parseOrThrow` there are also built-in operations to transform the data without validation, assert without allocating output, serialize back to the initial format and more. If somebody is missing in built-in operations, you can use `S.compile` to create a custom one.

### Inferred types

**Sury** automatically infers the static type from the schema definition. It has a really nice type on hover, which you can extract by using `S.Output<typeof schema>` or `S.Input<typeof schema>`.

```ts
const Player = S.schema({
  username: S.string,
  xp: S.number,
});
//? S.Schema<{ username: string; xp: number }, { username: string; xp: number }>

type Player = S.Output<typeof Player>;

// Use it in your code
const player: Player = { username: "billie", xp: 100 };
```

### Serializing data

If you wonder why the schema needs an `Input` type, it's because **Sury** supports serializing data back to the initial format.

```ts
S.reverseConvertOrThrow({ username: "billie", xp: 100 }, Player);
// => returns { username: "billie", xp: 100 }
```

Doesn't look like a big deal, with the example above. But if you have a more complex schema with transformations, it can be really useful.

```ts
// 1. Create some advanced schema with transformations
//    S.to - for easy & fast coercion
//    S.shape - for fields transformation
//    S.meta - with examples in Output format
const User = S.schema({
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
S.parseOrThrow(
  {
    USER_ID: "0",
    USER_NAME: "Dmitry",
  },
  userSchema
);
// { id: 0n, name: "Dmitry" }
// See how "0" is turned into 0n and fields are renamed

// 3. And reverse the schema and use it for parsing Output to Input
S.parseOrThrow(
  {
    id: 0n,
    name: "Dmitry",
  },
  S.reverse(userSchema)
);
// { USER_ID: "0", USER_NAME: "Dmitry" }
// Just use `S.reverse` and get a full-featured schema with switched `Output` and `Input` types
// Note: You can use `S.reverseConvertOrThrow(data, schema)` if you don't need to perform validation
```

### Performance

This is not really about usage, but what you should be aware of is that **Sury** will most likely outperform not only other libraries, but also your own hand-rolled validation logic.

```ts
// This is how S.parseOrThrow(data, userSchema) is compiled
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
// This is how S.reverseConvertOrThrow(data, userSchema) is compiled
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
S.toJSONSchema(User);
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
S.assertOrThrow(
  "example.com",
  S.fromJSONSchema({
    type: "string",
    format: "email",
  })
);
// Throws S.Error: Failed asserting: Invalid email address
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

// JSON type
S.enableJson(); // ❕ Call at the project root.
// Allows string | boolean | number | null | Record<string, JSON> | JSON[]
S.json;

// JSON string
S.enableJsonString(); // ❕ Call at the project root.
// Asserts that the input is a valid JSON string
S.jsonString;
S.jsonStringWithSpace(2);
// Parses JSON string and validates that it's a number
S.jsonString.with(S.to, S.number);
// Serializes number to JSON string
S.number.with(S.to, S.jsonString);
```

## Strings

**Sury** includes a handful of string-specific refinements and transforms:

```ts
S.max(S.string, 5); // String must be 5 or fewer characters long
S.min(S.string, 5); // String must be 5 or more characters long
S.length(S.string, 5); // String must be exactly 5 characters long
S.email(S.string); // Invalid email address
S.url(S.string); // Invalid url
S.uuid(S.string); // Invalid UUID
S.cuid(S.string); // Invalid CUID
S.pattern(S.string, %re(`/[0-9]/`)); // Invalid
S.datetime(S.string); // Invalid datetime string! Expected UTC

S.trim(S.string); // trim whitespaces
```

> ⚠️ Validating email addresses is nearly impossible with just code. Different clients and servers accept different things and many diverge from the various specs defining "valid" emails. The ONLY real way to validate an email address is to send a verification email to it and check that the user got it. With that in mind, Sury picks a relatively simple regex that does not cover all cases.

When using built-in refinements, you can provide a custom error message.

```ts
S.min(S.string, 1, "String can't be empty");
S.length(S.string, 5, "SMS code should be 5 digits long");
```

### ISO datetimes

The `S.datetime(S.string)` function has following UTC validation: no timezone offsets with arbitrary sub-second decimal precision.

```ts
const datetimeSchema = S.datetime(S.string);
// The datetimeSchema has the type S.Schema<Date, string>
// String is transformed to the Date instance

S.parseOrThrow("2020-01-01T00:00:00Z", datetimeSchema); // pass
S.parseOrThrow("2020-01-01T00:00:00.123Z", datetimeSchema); // pass
S.parseOrThrow("2020-01-01T00:00:00.123456Z", datetimeSchema); // pass (arbitrary precision)
S.parseOrThrow("2020-01-01T00:00:00+02:00", datetimeSchema); // fail (no offsets allowed)
```

## Numbers

**Sury** includes some of number-specific refinements:

```ts
S.max(S.number, 5); // Number must be lower than or equal to 5
S.min(S.number 5); // Number must be greater than or equal to 5
```

Optionally, you can pass in a second argument to provide a custom error message.

```ts
S.max(S.number, 5, "this👏is👏too👏big");
```

## Optionals

You can make any schema optional with `S.optional`.

```ts
const schema = S.optional(S.string);

S.parseOrThrow(undefined, schema); // => returns undefined
type A = S.Output<typeof schema>; // string | undefined
```

You can pass a default value to the second argument of `S.optional`.

```ts
const stringWithDefaultSchema = S.optional(S.string, "tuna");

S.parseOrThrow(undefined, stringWithDefaultSchema); // => returns "tuna"
type A = S.Output<typeof stringWithDefaultSchema>; // string
```

Optionally, you can pass a function as a default value that will be re-executed whenever a default value needs to be generated:

```ts
const numberWithRandomDefault = S.optional(S.number, Math.random);

S.parseOrThrow(undefined, numberWithRandomDefault); // => 0.4413456736055323
S.parseOrThrow(undefined, numberWithRandomDefault); // => 0.1871840107401901
S.parseOrThrow(undefined, numberWithRandomDefault); // => 0.7223408162401552
```

Conceptually, this is how **Sury** processes default values:

1. If the input is `undefined`, the default value is returned
2. Otherwise, the data is parsed using the base schema

## Nullables

Similarly, you can create nullable types with `S.nullable`.

```ts
const nullableStringSchema = S.nullable(S.string);
S.parseOrThrow("asdf", nullableStringSchema); // => "asdf"
S.parseOrThrow(null, nullableStringSchema); // => undefined
```

Notice how the `null` input transformed to `undefined`.

## Nullish

A convenience method that returns a "nullish" version of a schema. Nullish schemas will accept both `undefined` and `null`. Read more about the concept of "nullish" [in the TypeScript 3.7 release notes](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-3-7.html#nullish-coalescing).

```ts
const nullishStringSchema = S.nullish(S.string);
S.parseOrThrow("asdf", nullishStringSchema); // => "asdf"
S.parseOrThrow(null, nullishStringSchema); // => null
S.parseOrThrow(undefined, nullishStringSchema); // => undefined
```

## Objects

```ts
// all properties are required by default
const dogSchema = S.schema({
  name: S.string,
  age: S.number,
});

// extract the inferred type like this
type Dog = S.Output<typeof dogSchema>;

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
  age: 23
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

S.parseOrThrow(
  {
    USER_ID: 1,
    USER_NAME: "John",
  },
  userSchema
);
// => returns { id: 1, name: "John" }

// Infer output TypeScript type of the userSchema
type User = S.Output<typeof userSchema>; // { id: number; name: string }
```

Compared to using `S.transform`, the approach has 0 performance overhead. Also, you can use the same schema to convert the parsed data back to the initial format:

```ts
S.reverseConvertOrThrow(
  {
    id: 1,
    name: "John",
  },
  userSchema
);
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

S.parseOrThrow(
  {
    name: "bob dylan",
    extraKey: 61,
  },
  personSchema
);
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
type Teacher = S.Output<typeof teacherSchema>; // => { students: string[], id: string }
```

> 🧠 The function will throw if the schemas share keys. The returned schema also inherits the "unknownKeys" policy (strip/strict) of B.

## Arrays

```ts
const stringArraySchema = S.array(S.string);
```

**Sury** includes some of array-specific refinements:

```ts
S.max(S.array(S.string), 5); // Array must be 5 or fewer items long
S.min(S.array(S.string) 5); // Array must be 5 or more items long
S.length(S.array(S.string) 5); // Array must be exactly 5 items long
```

### Unnest

```ts
const schema = S.unnest(
  S.schema({
    id: S.string,
    name: S.nullable(S.string),
    deleted: S.boolean,
  })
);

const value = S.reverseConvertOrThrow(
  [
    { id: "0", name: "Hello", deleted: false },
    { id: "1", name: undefined, deleted: true },
  ],
  schema
);
// [["0", "1"], ["Hello", null], [false, true]]
```

The helper function is inspired by the article [Boosting Postgres INSERT Performance by 2x With UNNEST](https://www.timescale.com/blog/boosting-postgres-insert-performance). It allows you to flatten a nested array of objects into arrays of values by field.

The main concern of the approach described in the article is usability. And ReScript Schema completely solves the problem, providing a simple and intuitive API that is even more performant than `S.array`.

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

type Athlete = S.Output<typeof athleteSchema>;
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

type Athlete = S.Output<typeof athleteSchema>;
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

S.parseOrThrow("foo", stringOrNumberSchema); // passes
S.parseOrThrow(14, stringOrNumberSchema); // passes
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

typeof S.Output<schema>; // Win | Draw | Loss
```

## Records

Record schema is used to validate types such as `{ [k: string]: number }`.

If you want to validate the values of an object against some schema but don't care about the keys, use `S.record(valueSchema)`:

```ts
const numberCacheSchema = S.record(S.number);

type NumberCache = S.Output<typeof numberCacheSchema>;
// => { [k: string]: number }
```

## Instance

You can use `S.instance` to check that the input is an instance of a class. This is useful to validate inputs against classes that are exported from third-party libraries.

```ts
class Test {
  name: string;
}

const TestSchema = S.instance(Test);

const blob: any = "whatever";
S.parseOrThrow(new Test(), TestSchema); // passes
S.parseOrThrow(blob, TestSchema); // throws S.Error: Failed parsing: Expected Test, received "whatever"
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

## Custom schema

**Sury** might not have many built-in schemas for your use case. In this case you can create a custom schema for any TypeScript type.

1. Choose a base schema which is the closest to your type. Most likely it'll be `S.instance`.
2. Use `S.transform` to add a custom parser and serializer.
3. Optionally, use `S.meta` to add customize the name of the schema and additional metadata.

```ts
const mySet = <T>(itemSchema: S.Schema<T>): S.Schema<Set<T>> =>
  S.instance(Set)
    .with(S.transform, (input) => {
      const output = new Set<T>();
      input.forEach((item) => {
        output.add(S.parseOrThrow(item, itemSchema));
      });
      return output;
    })
    .with(S.meta, {
      name: `Set<${S.toExpression(itemSchema)}>`,
    });

const numberSetSchema = mySet(S.number);
type NumberSet = S.Output<typeof numberSetSchema>; // Set<number>

S.parseOrThrow(new Set([1, 2, 3]), numberSetSchema); // passes
S.parseOrThrow(new Set([1, 2, "3"]), numberSetSchema); // throws S.Error: Failed parsing: Expected number, received "3"
S.parseOrThrow([1, 2, 3], numberSetSchema); // throws S.Error: Failed parsing: Expected Set<number>, received [1, 2, 3]
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

**Sury** lets you provide custom validation logic via refinements. It's useful to add checks that's not possible to cover with type system. For instance: checking that a number is an integer or that a string is a valid email address.

```ts
const shortStringSchema = S.string.with(S.refine, (value, s) => {
  if (value.length > 255) {
    s.fail("String can't be more than 255 characters");
  }
});
```

The refine function is applied for both parser and serializer.

Also, you can have an asynchronous refinement (for parser only):

```ts
const userSchema = S.schema({
  id: S.string.with(S.uuid).with(S.asyncParserRefine, async (id, s) => {
    const isActiveUser = await checkIsActiveUser(id);
    if (!isActiveUser) {
      s.fail(`The user ${id} is inactive.`);
    }
  }),
  name: S.string,
});

type User = S.Output<typeof userSchema>; // { id: string, name: string }

// Need to use parseAsync which will return a promise with S.Result
await S.parseAsyncOrThrow(
  {
    id: "1",
    name: "John",
  },
  userSchema
);
```

## Transforms

**Sury** allows to augment schema with transformation logic, letting you transform value during parsing and serializing. This is most commonly used when you need to access the value in runtime and perform some transformation logic. For cases when you only want to change the shape of the data, it's better to use `S.shape` instead.

```ts
const intToString = (schema) =>
  S.transform(
    schema,
    (int) => int.toString(),
    (string, s) => {
      const int = parseInt(string, 10);
      if (isNaN(int)) {
        s.fail("Can't convert string to int");
      }
      return int;
    }
  );
```

> 🧠 You can use `S.int32.with(S.to, S.string)` which is a better version of the above example.

### **`shape`**

The `S.shape` schema is a helper function that allows you to transform the value to a desired shape. It'll statically derive required data transformations to perform the change in the most optimal way.

> ⚠️ Even though it looks like you operate with a real value, it's actually a dummy proxy object. So conditions or any other runtime logic won't work. Please use `S.transform` for such cases.

```typescript
const circleSchema = S.number.with(S.shape, (radius) => ({
  kind: "circle",
  radius: radius,
}));

S.parseOrThrow(1, circleSchema); //? { kind: "circle", radius: 1 }

// Also works in reverse 🔄
S.reverseConvertOrThrow({ kind: "circle", radius: 1 }, circleSchema); //? 1
```

## Functions on schema

### Built-in operations

The library provides a bunch of built-in operations that can be used to parse, convert, and assert values.

Parsing means that the input value is validated against the schema and transformed to the expected output type. You can use the following operations to parse values:

| Operation                | Interface                                             | Description                                                   |
| ------------------------ | ----------------------------------------------------- | ------------------------------------------------------------- |
| S.parseOrThrow           | `(unknown, Schema<Output, Input>) => Output`          | Parses any value with the schema                              |
| S.parseJsonOrThrow       | `(Json, Schema<Output, Input>) => Output`             | Parses JSON value with the schema                             |
| S.parseJsonStringOrThrow | `(string, Schema<Output, Input>) => Output`           | Parses JSON string with the schema                            |
| S.parseAsyncOrThrow      | `(unknown, Schema<Output, Input>) => Promise<Output>` | Parses any value with the schema having async transformations |

For advanced users you can only transform to the output type without type validations. But be careful, since the input type is not checked:

| Operation                    | Interface                                  | Description                             |
| ---------------------------- | ------------------------------------------ | --------------------------------------- |
| S.convertOrThrow             | `(Input, Schema<Output, Input>) => Output` | Converts input value to the output type |
| S.convertToJsonOrThrow       | `(Input, Schema<Output, Input>) => Json`   | Converts input value to JSON            |
| S.convertToJsonStringOrThrow | `(Input, Schema<Output, Input>) => string` | Converts input value to JSON string     |

Note, that in this case only type validations are skipped. If your schema has refinements or transforms, they will be applied.

Also, you can use `S.noValidation(schema, true)` helper to turn off type validations for the schema even when it's used with a parse operation.

More often than converting input to output, you'll need to perform the reversed operation. It's usually called "serializing" or "decoding". The ReScript Schema has a unique mental model and provides an ability to reverse any schema with `S.reverse` which you can later use with all possible kinds of operations. But for convinence, there's a few helper functions that can be used to convert output values to the initial format:

| Operation                           | Interface                                           | Description                                                           |
| ----------------------------------- | --------------------------------------------------- | --------------------------------------------------------------------- |
| S.reverseConvertOrThrow             | `(Output, Schema<Output, Input>) => Input`          | Converts schema value to the output type                              |
| S.reverseConvertToJsonOrThrow       | `(Output, Schema<Output, Input>) => Json`           | Converts schema value to JSON                                         |
| S.reverseConvertToJsonStringOrThrow | `(Output, Schema<Output, Input>) => string`         | Converts schema value to JSON string                                  |
| S.reverseConvertAsyncOrThrow        | `(Output, Schema<Output, Input>) => promise<Input>` | Converts schema value to the output type having async transformations |

This is literally the same as convert operations applied to the reversed schema.

For some cases you might want to simply assert the input value is valid. For this there's `S.assertOrThrow` operation:

| Operation       | Interface                                                      | Description                                                                                                                                          |
| --------------- | -------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------- |
| S.assertOrThrow | `(data: unknown, Schema<Output, Input>) asserts data is Input` | Asserts that the input value is valid. Since the operation doesn't return a value, it's 2-3 times faster than `parseOrThrow` depending on the schema |

All operations either return the output value or throw an error. For convinient error handling you can use the `S.safe` and `S.safeAsync` helpers, which would catch the error an wrap it into a `Result` type:

```ts
const result = S.safe(() => S.parseOrThrow(123, S.string));
```

### **`compile`**

If you want to have the most possible performance, or the built-in operations doesn't cover your specific use case, you can use `compile` to create fine-tuned operation functions.

```ts
const operation = S.compile(S.string, "Any", "Assert", "Async");
typeof operation; // => (input: unknown) => Promise<void>
await operation("Hello world!");
// ()
```

For example, in the example above we've created an async assert operation, which is not available by default.

You can configure compiled function `input` with the following options:

- `Output` - accepts `Output` of `Schema<Output, Input>` and reverses the operation
- `Input` - accepts `Input` of `Schema<Output, Input>` which only affects the operation function argument type
- `Any` - accepts `unknown`
- `Json` - accepts `Json`
- `JsonString` - accepts `string` and applies `JSON.parse` before parsing

You can configure compiled function `output` with the following options:

- `Output` - returns `Output` of `Schema<Output, Input>`
- `Input` - returns `Input` of `Schema<Output, Input>`
- `Assert` - returns `void` with `asserts data is T` guard
- `Json` - validates that the schema is JSON compatible and returns `Js.Json.t`
- `JsonString` - validates that the schema is JSON compatible and converts output to JSON string

You can configure compiled function `mode` with the following options:

- `Sync` - for sync operations
- `Async` - for async operations - will wrap return value in a promise

And you can configure compiled function `typeValidation` with the following options:

- `true (default)` - performs type validation
- `false` - doesn't perform type validation and only converts data to the output format. Note that refines are still applied.

### **`reverse`**

```ts
S.reverse(S.nullable(S.string));
// S.optional(S.string)
```

```ts
const schema = S.object((s) => s.field("foo", S.string));

S.parseOrThrow({ foo: "bar" }, schema);
// "bar"

const reversed = S.reverse(schema);

S.parseOrThrow("bar", reversed);
// {"foo": "bar"}

S.parseOrThrow(123, reversed);
// throws S.error with the message: `Failed parsing: Expected string, received 123`
```

Reverses the schema. This gets especially magical for schemas with transformations 🪄

### **`to`**

This very powerful API allows you to coerce another data type in a declarative way. Let's say you receive a number that is passed to your system as a string. For this `S.to` is the best fit:

```ts
const schema = S.string.with(S.to, S.number);

S.parseOrThrow("123", schema); //? 123.
S.parseOrThrow("abc", schema); //? throws: Failed parsing: Expected number, received "abc"

// Reverse works correctly as well 🔥
S.reverseConvertOrThrow(123, schema); //? "123"
```

### **`name`**

```ts
const schema = S.schema({ abc: 123 }.with(S.meta, { name: "Abc" }));

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
S.parseOrThrow(true, S.schema(false));
// => Throws S.Error with the following message: Failed parsing: Expected false, received true".
```

You can catch the error using `S.safe` and `S.safeAsync` helpers:

```ts
const result = S.safe(() => S.parseOrThrow(true, S.schema(false)));

if (result.success) {
  console.log(result.value);
} else {
  console.log(result.error);
}
```

Or the async version:

```ts
const result = await S.safeAsync(async () => {
  const passed = await S.parseAsyncOrThrow(data, S.schema(S.boolean));
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
