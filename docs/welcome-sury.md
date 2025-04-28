# Welcome Sury: The fastest schema with next-gen DX 🚀

Recently I wrote a [comprehensive article about a JavaScript Schema library from the future](https://dev.to/dzakh/javascript-schema-library-from-the-future-5420) where I introduced [ReScript Schema](https://github.com/DZakh/sury) and demonstrated some truly groundbreaking features you won't find in any other library. I highly recommend checking it out first. While the feedback was overwhelmingly positive, I noticed that despite having TypeScript support, many developers were hesitant simply because of "[ReScript](https://rescript-lang.org/)" in the name. So after months of dedicated development, I'm excited to present you [Sury](https://github.com/DZakh/sury) - a TypeScript-first schema validation library that brings all those amazing features while staying true to its ReScript roots 🔥

## What is Sury?

It's a schema library you'll definitely want to use:

- API that's easy to read and write, similar to TS types and without noise
- Nice type inference with real types on schema hover
- The fastest parsing with explicit throws
- Convenient error handling with readable error messages
- And more after the code example ☺️

```typescript
import * as S from "sury";

const filmSchema = S.schema({
  id: S.bigint,
  title: S.string,
  tags: S.array(S.string),
  rating: S.union(["G", "PG", "PG13", "R"]),
});
// On hover: S.Schema<{ id: bigint; title: string; tags: string[]; rating: "G" | "PG" | "PG13" | "R"; }, Input>

type Film = S.Output<typeof filmSchema>;
// On hover: { id: bigint; title: string; tags: string[]; rating: "G" | "PG" | "PG13" | "R"; }

S.parseOrThrow(
  {
    id: 1n,
    title: "My first film",
    tags: ["Loved"],
    rating: "S",
  },
  filmSchema
);
// Throws S.Error with message: Failed at ["rating"]: Expected "G" | "PG" | "PG13" | "R", received "S"

// Or do it safely:
const result = S.safe(() => S.parseOrThrow(data, filmSchema));
if (result.error) {
  console.log(result.error.reason);
  // Expected "G" | "PG" | "PG13" | "R", received "S"
}
```

Honestly, I think that's already enough to make you like **Sury** and want to try it in your next project, or at least [leave a star on GitHub](https://github.com/DZakh/sury) ⭐

Well, probably that's still a lot to ask for, since I've only shown ~10% of the features so far 😅

## Ecosystem

### Standard Schema

Let's set aside the performance, transformations, and serialization features for now, even though they're some of the library's greatest strengths. These features become truly valuable once you have a robust ecosystem to use them with - which was the main focus of the Sury (ReScript Schema v10) release.

A major enhancement is native support for [Standard Schema](https://standardschema.dev/), which is already integrated with over 32 popular libraries. Previous versions required explicitly calling `S.toStandard`, but now every **Sury** schema automatically implements the [Standard Schema](https://standardschema.dev/) interface.

Here's an example from the newly released [oRPC](https://orpc.unnoq.com/) showing how to replace **Zod** with **Sury**:

```diff
import { ORPCError, os } from '@orpc/server'
- import { z } from 'zod'
+ import * as S from 'sury'

export const listPlanet = os
  .input(
-   z.object({
-     limit: z.number().int().min(1).max(100).optional(),
-     cursor: z.number().int().min(0).default(0),
-   }),
+   S.schema({
+     limit: S.optional(S.int32.with(S.min, 1).with(S.max, 100)),
+     cursor: S.optional(S.int32.with(S.min, 0), 0),
+   })
  )
  .handler(async ({ input }) => {
    // your list code here
    return [{ id: 1, name: 'name' }]
  })
```

And it just works - faster, smaller and more flexible 👍
You may notice the `.with` calls which might look unfamiliar. This is a new feature introduced in this release that I'll definitely talk about later.

### JSON Schema

But what's more important and much more widely used is JSON Schema. I used to support it only for ReScript users, but now it's time to share some gold with TypeScript users as well 🫡

```typescript
// 1. Create some advanced schema with transformations
//    S.to - for easy & fast coercion
//    S.shape - for easy & fast transformation
//    S.meta - with examples in transformed format
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

// 2. Use built-in S.toJSONSchema and see how everything in the Input format
//    It's just asking to put itself to Fastify or any other server with OpenAPI integration 😁
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

#### Offtopic: Serialization

In the example above I used several advanced features of the library. Let's take a moment to explore one of **Sury**'s most powerful capabilities - **bidirectional schemas**. Every schema can both validate and serialize data without needing separate decoders or encoders.

Let's see how this works with our previous example:

```typescript
// You can use it for normal parsing
S.parseOrThrow(
  {
    USER_ID: "0",
    USER_NAME: "Dmitry",
  },
  userSchema
);
// { id: 0n, name: "Dmitry" }
// See how "0" is turned into 0n and fields are renamed

// And reverse the schema and use it for serialization
S.parseOrThrow(
  {
    id: 0n,
    name: "Dmitry",
  },
  S.reverse(userSchema)
);
// { USER_ID: "0", USER_NAME: "Dmitry" }
// Just add `S.reverse` and get a full-featured schema with switched `Output` and `Input` types
// Note: You can use `S.reverseConvertOrThrow(data, schema)` if you don't need additional validation
```

And while we're at it, I'll quickly show why **Sury** is the fastest schema library out there. **Sury** heavily relies on JIT optimizations by inlining all validations and transformations using `new Function` API which is the fastest way to execute code in JS/TS. It's not something new and used by other libraries like `TypeBox`, `ArkType` and even `Zod@4`. But they mostly do basic validations, while **Sury** embraces transformations as it's core primitive:

```javascript
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

```javascript
// This is how S.reverseConvertOrThrow(data, userSchema) is compiled
(i) => {
  let v0 = i["id"];
  return { USER_ID: "" + v0, USER_NAME: i["name"] };
};
```

Neat, right? 🤯

### Internal Representation

Let's go back to ecosystem talk. While **Sury** provides JSON Schema integration out of the box, having a good internal representation is crucial for a schema library. It enables building custom tools and growing the ecosystem. In this version, I've made significant progress by representing every schema as a discriminated union, similar to JSON Schema.

This design allows you to easily reason about and work with the Input and Output types of your schemas in your code:

```typescript
const envSchema = (schema) => {};
```

### 🛠 API Improvements

We've made several API improvements to make it more intuitive:

```typescript
// Before
S.stringMinLength(5);
S.intMin(0);
S.arrayMinLength(1);

// After
S.min(5); // Works for strings, numbers, and arrays
```

Other notable changes:

- Renamed `S.nullable` to `S.nullish` to match JS API
- Removed `S.undefined` in favor of `S.schema(undefined)`
- Improved error messages for recursive schemas
- Better tuple handling with Strip mode
- Removed `S.preprocess` in favor of `S.to` and `S.transform`

## Why Sury v10?

While maintaining the incredible performance that made Sury stand out, v10 brings several key improvements:

1. **Better Developer Experience**

   - More intuitive API naming
   - Improved error messages
   - Better TypeScript integration
   - Unified metadata handling

2. **Enhanced Flexibility**

   - More powerful transformation capabilities
   - Better support for complex data structures
   - Improved JSON Schema support

3. **Future-Proof**
   - Better type safety with `unknown` defaults
   - More consistent API design
   - Improved error handling

## Getting Started

Ready to try Sury v10? Here's a quick example:

```typescript
import * as S from "sury";

const userSchema = S.schema({
  id: S.number,
  name: S.string,
  email: S.string,
  age: S.optional(S.number),
  tags: S.array(S.string),
  role: S.union(["admin", "user", "guest"]),
});

// Parse and validate data
const result = S.parseOrThrow(data, userSchema);

// Transform data
const transformed = S.to(result, userSchema, {
  // transformation rules
});
```

## What's Next?

We're already working on v11, which will bring even more improvements:

- Enhanced refinement capabilities
- Better support for unknown types
- Improved date handling
- More powerful transformation options

## Join the Community

We'd love to hear your feedback and see what you build with Sury. Join our community and help shape the future of schema validation!

- GitHub: [github.com/DZakh/sury](https://github.com/DZakh/sury)
- Documentation: [sury.dev](https://sury.dev)
- Discord: [Join our community](https://discord.gg/sury)

## Conclusion

Sury v10 represents a significant step forward in schema validation, combining incredible performance with an intuitive developer experience. Whether you're building a small project or a large-scale application, Sury provides the tools you need to validate and transform your data with confidence.

Try Sury v10 today and experience the future of schema validation! 🚀

---

Would you like me to expand on any particular section or add more specific technical details about certain features?
