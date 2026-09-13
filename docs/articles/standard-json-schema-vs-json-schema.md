---
title: Standard JSON Schema vs JSON Schema
published: true
description: JSON Schema is a standard for describing JSON. Standard JSON Schema is a standard for...
tags: typescript, json, opensource, webdev
# cover_image: https://direct_url_to_image.jpg
# Use a ratio of 100:42 for best results.
# published_at: 2026-06-16 17:36 +0000
---

Damn, I like standards! I'm so happy to live in a world where we can finally use USB-C for almost everything... Oh, the article is not about USB-C, but about JSON Schema finally getting a "Standard" in 2026!

Well, I'm kidding here... JSON Schema is a standard itself, which has existed for a long time already- multiple standards, actually: first proposal 2007, draft-04, draft-07, draft-2020-12, openapi-3.0, and they don't stop.

Describing JSON with JSON - what can be better? This is what JSON Schema is:

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

> Hey, everybody knows this! Tell us about Standard JSON Schema instead!!!

.
.
> ... Ignoring

Even if you don't know JSON Schema or have never written it yourself, it's a big part of our lives. I'm sure you've used at least one of those:

- OpenAPI
- OpenAI
- OpenTelemetry
- OpenRPC
- Ope...

Wait, I feel an Open-prefix conspiracy here. So let's stop, and I'll explain how all the words relate to JSON Schema.

[OpenAPI](https://www.openapis.org/) is a standard which uses JSON Schema to describe HTTP APIs. [OpenAI](https://openai.com/) made it here not because I have a fun mood today - which is also true. They are here because modern AI models (not only OpenAI's) support JSON Schema to force a guaranteed structured output. [OpenTelemetry](https://opentelemetry.io/docs/collector/configuration/) is a nice example of how JSON Schema can be used for configuration. Pretty much all validation and hover support in IDE config files is powered by JSON Schemas. [OpenRPC](https://www.open-rpc.org/) - I don't know; I myself just learned that something like this exists.

To continue the story, JSON Schema is used here and there. And it was especially trendy ~7 years ago when [Fastify](https://fastify.dev/) was still young and shiny - this is what I wanted to say, but look at their updated website!

There are also forms...

> Wait, but nobody in their right mind uses JSON Schema for forms!

Well, it might be interesting for declarative UI, but in terms of validation, people have massively started adopting custom schema libraries instead:

```ts
import Joi from "joi";

const schema = Joi.object({
  id: Joi.string().required(),
  price: Joi.number().required(),
});
```

Developers embraced the idea, and now we have hundreds of schema libraries. Each with its own API, logic, types, and internal representation. A representation that is never compatible with JSON Schema and many existing libraries that worked with JSON Schema before.

## We are so back now!

This is when [@colinhacks](https://x.com/colinhacks) (Zod), [@fabianhiller](https://x.com/fabianHiller) (Valibot), and [@ssalbdivad](https://x.com/ssalbdivad) (ArkType) gathered together and created [Standard Schema](https://standardschema.dev/). And a few months later, they added an extension: [Standard JSON Schema](https://standardschema.dev/json-schema).

Now hundreds of schema-libraries, each previously reinventing a wheel, got a standard for how to provide JSON Schema for other libraries to consume in an agreed way.

In other words, [Standard JSON Schema](https://standardschema.dev/json-schema) is a standard way for schema libraries like Zod to return [JSON Schema](https://json-schema.org/), so other libraries that support the standard can use the JSON Schema without needing to know it's provided by Zod.

**A standard for passing JSON Schema around. Literally.**

This sounds small, but it's actually super cool, and I really wish it gets higher adoption for the sake of the JavaScript ecosystem's future.

Why do I think it's important? Do you know how [tRPC](https://trpc.io/) currently supports OpenAPI? It infers input and output types during project compilation and uses them to create JSON Schema. This is an amazing solution that works for all existing schema libraries, but the formats and extra metadata are lost in the process. And every library that consumes some sort of schema nowadays has to reinvent the wheel.

This is solved now, and I bet tRPC will start using Standard JSON Schema very soon. At least for augmenting the JSON Schema they derived from types.

Honestly, adoption isn't big yet. But on a positive note, big projects like the OpenAI SDK recently added support for Standard JSON Schema. And my take is: if you're building a library that needs types to be known at runtime, definitely check out the [Standard JSON Schema](https://standardschema.dev/json-schema) docs.

## How to use it?

Some libraries come with it out of the box:

```ts
import type { StandardJSONSchemaV1 } from "@standard-schema/spec";
import * as z from "zod";

z.string() satisfies StandardJSONSchemaV1; // ✅
```

So you can pass it around and use in a dependency agnostic way:

```ts
// Function that accepts any compliant `StandardJSONSchemaV1`
// and converts it to a JSON Schema.
export function acceptSchema(schema: StandardJSONSchemaV1) {
  // do stuff, e.g.
  return schema["~standard"].jsonSchema.input({
    target: "draft-2020-12",
  });
}

acceptSchema(z.string());
```

For some libraries like [Valibot](https://valibot.dev/), bundle size is a priority. So instead of adding an extra 1-4KB to every user's bundle, there's either a separate function or even a package to get the standard:

```ts
import * as v from "valibot";
import { toStandardJsonSchema } from "@valibot/to-json-schema";

toStandardJsonSchema(v.string()) satisfies StandardJSONSchemaV1; // ✅
```

Another interesting example is how [Sury](https://github.com/DZakh/sury) manages the case. By default, every schema comes with Standard JSON Schema disabled. It doesn't affect the bundle in any way. And there's a tree-shakable toggle, `S.enableStandardJSONSchema()`, which automatically enables Standard JSON Schema for every existing and future schema.

```ts
import * as S from "sury";

S.enableStandardJSONSchema();

S.string satisfies StandardJSONSchemaV1; // ✅
```

## JSON Schema to Standard JSON Schema

By default, these two are different things, and if a library API accepts Standard JSON Schema, it doesn't mean it accepts JSON Schema itself.

You don't often want to do this, but I think it might happen to some of you. And there's a nice solution. For example, using [Sury](https://github.com/DZakh/sury), which I mentioned above, we can do the conversion, and what's extra cool is that it'll provide you with types:

```ts
import * as S from "sury";

const schema = S.fromJSONSchemaOrThrow({
  type: "object",
  properties: { id: { type: "string" }, role: { enum: ["admin", "user"] } },
  required: ["id"],
});
//? S.Schema<{ id: string; role?: "admin" | "user" | undefined }>

S.enableStandardJSONSchema();

schema satisfies StandardJSONSchemaV1; // ✅
```

Why might you need it? To come back to the roots and use a mature JSON Schema DSL? Or maybe convert draft-07 into openapi-3.0 - this also works because Standard JSON Schema supports different targets. As of the day of writing this article, [Sury](https://github.com/DZakh/sury) has 93.4% draft-07 compliance. This is almost as much as [TypeBox](https://github.com/sinclairzx81/typebox), but TypeBox is not Standard JSON Schema compatible and doesn't allow conversion to another JSON Schema target.

## Shipping!

I hope you liked the introduction to [Standard JSON Schema](https://standardschema.dev/json-schema). This project has huge potential, and I hope it will unify the JavaScript ecosystem over time. Not much adoption yet, but I'm sure it'll change.

One more time, kudos to [@colinhacks](https://x.com/colinhacks) (Zod), [@fabianhiller](https://x.com/fabianHiller) (Valibot), [@ssalbdivad](https://x.com/ssalbdivad) (ArkType), and the community for bringing the standard to life.

And if you want to learn more about schema libraries and everything related, follow me on [X](https://x.com/dzakh_dev) - it'll make my day 🙏
