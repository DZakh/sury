---
title: JSON Schema vs Standard JSON Schema
published: false
description: JSON Schema is JSON that describes JSON. Standard JSON Schema is a TypeScript interface that lets any tool accept a schema from any library. Same three words, completely different things.
tags: typescript, json, opensource, webdev
# cover_image: https://direct_url_to_image.jpg
# Use a ratio of 100:42 for best results.
# published_at: 2026-06-16 17:36 +0000
---

Standard JSON Schema is not "a JSON Schema that follows the standard". It's a TypeScript interface, and it solves a problem JSON Schema itself never had.

Same three words, different things. Let's fix that in three minutes.

## JSON Schema, in one example

```json
{
  "type": "object",
  "properties": {
    "id": { "type": "string" },
    "price": { "type": "number" }
  },
  "required": ["id", "price"]
}
```

JSON that describes JSON. That's the whole idea.

## You already use it

- **OpenAPI** — the spec is JSON Schema wearing a hat
- **LLM structured outputs and tool calling** — every provider takes a JSON Schema
- **MCP** — tool definitions ship their `inputSchema` as JSON Schema on the wire
- **Fastify** — validation *and* serialization, both from JSON Schema
- **Form builders** — react-jsonschema-form and friends render straight from it
- **Your editor** — autocomplete in `tsconfig.json` and `settings.json` comes from SchemaStore

It's the one schema format everything already speaks. If your tool takes a JSON Schema, it works with all of that for free.

## So why does anyone need a standard *for* it?

Because nobody writes JSON Schema by hand. You write a schema in a library, and the library converts:

```ts
// Zod
import * as z from "zod";
z.toJSONSchema(z.object({ id: z.string(), price: z.number() }));

// Valibot - needs a separate package
import * as v from "valibot";
import { toJsonSchema } from "@valibot/to-json-schema";
toJsonSchema(v.object({ id: v.string(), price: v.number() }));

// Sury
import * as S from "sury";
S.toJSONSchema(S.schema({ id: S.string, price: S.number }));
```

Three libraries, three call shapes, one of them a whole extra dependency. And others - ArkType, VineJS, TypeBox, effect - each with their own.

Now put yourself on the other side. You maintain a tool that wants "give me your schema". You either write an adapter per library and keep them all updated forever, or you pick one library and tell everyone else to convert manually. Almost everyone picked Zod.

## Before the standard, people got creative

JSON Schema's classic use case is generating OpenAPI, and tRPC needed that. There was no way to accept a schema from any library, so [tRPC's OpenAPI generator](https://trpc.io/docs/openapi) went around the runtime entirely:

> The generator statically analyses your router's TypeScript types — it never executes your code.

It reads your API shape out of the *type system*, with the compiler, instead of asking your schemas what they are. And descriptions:

> Zod `.describe()` calls and JSDoc comments on types, routers, and procedures, all become `description` fields in the spec.

So `.describe()` — a runtime call — has to survive into the TypeScript type for a static analyzer to pick it back up. That's a lot of machinery to answer a question every schema in the router could have answered directly. 👀

Phew. Ok. So:

## Standard JSON Schema

[The spec](https://standardschema.dev/json-schema) is one interface:

```ts
interface StandardJSONSchemaV1<Input = unknown, Output = Input> {
  readonly "~standard": {
    readonly version: 1;
    readonly vendor: string;
    readonly types?: { input: Input; output: Output };
    readonly jsonSchema: {
      input: (options: { target: Target }) => Record<string, unknown>;
      output: (options: { target: Target }) => Record<string, unknown>;
    };
  };
}
```

A library implements it, and every tool gets the same call:

```ts
import * as z from "zod";
import * as v from "valibot";
import { toStandardJsonSchema } from "@valibot/to-json-schema";
import * as S from "sury";

const schemas = [
  z.object({ id: z.string() }),
  toStandardJsonSchema(v.object({ id: v.string() })),
  S.schema({ id: S.string }),
];

for (const schema of schemas) {
  schema["~standard"].jsonSchema.input({ target: "draft-2020-12" });
  // => { $schema: "...", type: "object", properties: { id: { type: "string" } }, required: ["id"] }
}
```

Two things worth saying out loud, because everyone mixes them up:

It's **not** [Standard Schema](https://standardschema.dev). That's the sibling spec for *validation*, on the same `~standard` field. A library can implement either one alone, and a schema that does both gives a tool validation and JSON Schema from a single argument.

And it's not automatic. Zod v4.2+ and ArkType have it built in, Valibot needs the `toStandardJsonSchema` wrapper above, and Sury needs `S.enableStandardJSONSchema()` once at startup (it's opt-in so the converter can be tree-shaken away when you don't use it).

## If you maintain a tool that takes JSON Schema

You don't have to choose. Accept both:

```ts
const toJsonSchema = (schema, target = "draft-2020-12") =>
  "~standard" in schema
    ? schema["~standard"].jsonSchema.input({ target })
    : schema; // already a plain JSON Schema
```

That's the migration. Nobody's existing code breaks, and every Standard JSON Schema library works on day one.

## Who already accepts it

The **OpenAI SDK** shipped it in 6.49.0:

```ts
import { standardTextFormat } from "openai/helpers/standard-schema";

const response = await client.responses.parse({
  model: "gpt-5",
  input: "Extract the product",
  text: { format: standardTextFormat(schema, "product") },
});
```

`schema` there is yours - Zod, Valibot, Sury, whatever you already use. Internally the SDK calls `schema["~standard"].jsonSchema?.input({ target: "draft-07" })` and gets on with it. Compare that to `zodTextFormat`, which is the same helper with one library's name baked into it.

**MCP Apps** ([`@modelcontextprotocol/ext-apps`](https://apps.extensions.modelcontextprotocol.io/api/interfaces/app.StandardSchemaWithJSON.html)) does the both-specs-at-once trick, with a type literally called `StandardSchemaWithJSON`. Also on the list: xsAI, GQLoom, and the Restate SDK.

That's a short list. It's a young spec.

## Who's missing it

Anything today that says "pass a Zod schema, or hand-write a JSON Schema". Fastify, react-jsonschema-form, LangChain.js, most OpenAPI generators, and a pile of internal tools at your company. All of them already have the JSON Schema half working. They're maybe fifteen lines from the other one.

## Why this beats asking for a JSON Schema

**Your users pick their own library.** That's the entire point, and it's worth more to them than it costs you.

**Types come along for the ride.** `~standard.types` carries the inferred Input and Output, so your tool's return value is typed from the user's schema without a single generic of your own.

**Validation without an extra dependency.** If the schema also implements Standard Schema, you already have a validator - the same object that produced the JSON Schema can check the value that comes back:

```ts
schema["~standard"].validate({ id: "p_1", price: "9.99" });
// => { value: { id: "p_1", price: 9.99 } }
```

**The dialect is your call, not theirs.** `target` takes `draft-2020-12`, `draft-07` or `openapi-3.0`, and the library deals with the differences. No more shipping your own converter that only speaks one draft.

**And there are two schemas, not one.** This is the part plain JSON Schema genuinely can't express. A schema that transforms has an input shape and an output shape, and they're different:

```ts
const product = S.schema({
  id: S.string,
  price: S.string.with(S.to, S.number), // string on the wire, number in the app
});

product["~standard"].jsonSchema.input({ target: "draft-07" });
// price: { type: "string" }   <- what you accept

product["~standard"].jsonSchema.output({ target: "draft-07" });
// price: { type: "number" }   <- what you return
```

Hand a tool one JSON Schema and it has no idea which of those it's holding. That's why the OpenAI SDK explicitly calls `.input()` - a decision it could only make because the standard offers both.

## Wrapping up

Standard JSON Schema doesn't replace JSON Schema. It's the missing handshake in front of it: one interface, so tools stop hardcoding library names and users stop converting by hand.

If you maintain a schema library, implement it. If you maintain a tool, accept it - the snippet above is the whole change. And if you're just picking a library, check whether yours does it yet.

Hope this was useful. Check out [Sury](https://dev.to/dzakh/welcome-sury-the-fastest-schema-with-next-gen-dx-5gl4) — the most powerful schema library in the TS ecosystem. Follow me on X at [@dzakh_dev](https://x.com/dzakh_dev), and ask your questions in the comments!
