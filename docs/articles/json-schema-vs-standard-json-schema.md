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

They don't even agree on what comes out by default. Run those three as written and Zod stamps draft 2020-12, Valibot stamps draft-07, and Sury stamps no `$schema` at all. Every one of them takes an option to fix that, and it's a different option each time.

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
  // all three: { $schema: "https://json-schema.org/draft/2020-12/schema",
  //              type: "object", properties: { id: { type: "string" } },
  //              required: ["id"] }
}
```

Note that `target` isn't optional - the spec makes you pass it. That's the fix for the disagreement above: ask all three for `draft-07` and you get draft-07 from all three, ask for `openapi-3.0` and all three drop the `$schema`. The dialect stops being a per-library default you have to look up and becomes an argument you control.

Two more things worth saying out loud, because everyone mixes them up:

It's **not** [Standard Schema](https://standardschema.dev). That's the sibling spec for *validation*, on the same `~standard` field. A library can implement either one alone, and a schema that does both gives a tool validation and JSON Schema from a single argument.

And turning it on isn't the same everywhere. Zod v4.2+ and ArkType ship it enabled. Valibot needs the `toStandardJsonSchema` wrapper above. Sury needs one call at startup:

```ts
S.enableStandardJSONSchema();
```

It's a call rather than always-on so that bundlers can drop the JSON Schema converter from apps that only parse - which is most of them.

## Who already accepts it

The **OpenAI SDK** shipped it in 6.49.0:

```ts
import { standardTextFormat } from "openai/helpers/standard-schema";

const response = await client.responses.parse({
  model: "gpt-5",
  input: "Extract the product",
  text: { format: standardTextFormat(schema, "product") },
});

response.output_parsed; // { id: string, price: number }
```

`schema` there is yours. Pass a Zod one, a Valibot one or a Sury one and nothing else in that snippet changes - same request on the wire, and `output_parsed` typed from whichever you handed over. Internally the SDK just calls `schema["~standard"].jsonSchema?.input({ target: "draft-07" })`.

Compare that to `zodTextFormat`, the same helper with one library's name baked into it.

**MCP Apps** ([`@modelcontextprotocol/ext-apps`](https://apps.extensions.modelcontextprotocol.io/api/interfaces/app.StandardSchemaWithJSON.html)) does the both-specs-at-once trick, with a type literally called `StandardSchemaWithJSON`. Also on the list: xsAI, GQLoom, and the Restate SDK.

That's a short list. It's a young spec.

## If your tool is JSON Schema-native

By that I mean a tool where the user hands over a JSON Schema and the tool interprets it. API frameworks that validate requests, form renderers, OpenAPI generators, anything that defines LLM tools.

They all end up in the same place. You take JSON Schema, so you ship a validator to enforce it - [Fastify's route validation runs on Ajv](https://fastify.dev/docs/latest/Reference/Validation-and-Serialization/). Then users ask to write schemas in a real library instead of by hand, so you grow an adapter per library - Fastify's [type providers](https://fastify.dev/docs/latest/Reference/Type-Providers/), one package each.

Standard JSON Schema is the way out, and you can take it in three steps.

**Level 1 — take it as well.** Keep everything you have and add one branch:

```ts
const toJsonSchema = (schema, target = "draft-2020-12") =>
  "~standard" in schema
    ? schema["~standard"].jsonSchema.input({ target })
    : schema; // already a plain JSON Schema
```

Five lines. Nothing existing breaks, and every schema library starts working with your tool at once. For most tools this is the whole job.

**Level 2 — let the schema do the validating.** If it also implements Standard Schema, the object you were handed can check values itself:

```ts
schema["~standard"].validate({ id: "p_1", price: "9.99" });
// => { value: { id: "p_1", price: 9.99 } }
```

So don't run your own validator over it - call that and hand back the errors. Your users get messages from the library they already know, in the shape they already handle. And you stop validating the same data twice.

**Level 3 — stop vendoring a validator.** For a new tool or a major version: if schemas only ever arrive as Standard JSON Schema, Ajv leaves your dependency tree completely. Validation becomes the user's library, and JSON Schema goes back to being something you *emit* rather than something you interpret.

Level 3 has a real cost, though. JSON Schema's superpower is that it's just data - it can come from a file, a registry, an OpenAPI doc, another language. Drop the plain path and all of that stops working. If your users load schemas at runtime, Level 1 is the finish line, not a stepping stone.

## Why this beats asking for a JSON Schema

**Your users pick their own library.** That's the entire point, and it's worth more to them than it costs you.

**Types come along for the ride.** `~standard.types` carries the inferred Input and Output, so your tool's return value is typed from the user's schema without a single generic of your own.

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

If you maintain a schema library, implement it. If you maintain a tool, Level 1 is five lines and buys you every library at once. And if you're just picking a library, check whether yours does it yet.

Hope this was useful. Check out [Sury](https://dev.to/dzakh/welcome-sury-the-fastest-schema-with-next-gen-dx-5gl4) — the most powerful schema library in the TS ecosystem. Follow me on X at [@dzakh_dev](https://x.com/dzakh_dev), and ask your questions in the comments!
