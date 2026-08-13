---
title: Encode, don't stringify - how JSON.stringify lies to you
published: false
description: It corrupts your data, it crashes, it returns undefined. And there are zero reasons to keep using it.
tags: javascript, typescript, json, webdev
---

`JSON.stringify` is the default and seemingly only choice in basically every JavaScript project. It's built in, fast, needs no dependency, and pretends to be a safe choice.

It's also responsible for an enormous number of production bugs. Wrong money values. Timestamps that come back as strings. Fields that quietly vanish between the server and the client. Crashes on a payload that worked fine in another place.

My take: **NEVER use `JSON.stringify` in your code.**

Phew, this is hot, let me earn it.

## FAQ before we start

**"Never? Really?"**
For serializing your application data - yes, really. For a `console.log`, a cache key, or a quick debug dump, might be fine, but I still don't recommend it.

**"Is this an ad?"**
Partly, yes — I wrote [**Sury**](https://github.com/DZakh/sury), I'm proud of it and I think it solves this. But the first half of the article is plain JavaScript, and every problem in it exists whether or not you ever install anything of mine.

**"With rigged benchmarks as well?"**
I published the rows where **Sury** loses too. They're further down.

## Part 1: it's completely unsafe

### It returns `undefined`

```ts
JSON.stringify(undefined);
// => undefined

JSON.stringify(() => {});
// => undefined
```

Not the string `"undefined"`. The actual value. From a function typed as:

```ts
stringify(value: any, replacer?: ..., space?: ...): string;
```

The type says `string`, well... ok... So TypeScript happily lets you write `JSON.stringify(x).length` and hands you a Sentry alert at 3am. And no, [ts-reset](https://github.com/mattpocock/ts-reset) doesn't save you here - it patches `JSON.parse`, and ships no rule for `JSON.stringify` at all.

### It crashes

```ts
JSON.stringify({ id: 1n });
// => TypeError: Do not know how to serialize a BigInt

const user = {};
user.self = user;
JSON.stringify(user);
// => TypeError: Converting circular structure to JSON

let root = {},
  node = root;
for (let i = 0; i < 50000; i++) node = node.next = {};
JSON.stringify(root);
// => RangeError: Maximum call stack size exceeded
```

The last 2 are true edge cases and a `try/catch` handles them fine. But be honest - when did you last wrap a `JSON.stringify` in one? Especially knowing it can hand you `undefined`, a `TypeError` or a `RangeError` depending on the day.

### The errors don't tell you where

When it does throw - hide&seek starts:

```ts
JSON.stringify({ user: { orders: [{ total: 1n }] } });
// => TypeError: Do not know how to serialize a BigInt
```

Which order? Which field? You get to find out by yourself. At least there's a stack trace... oh, it only knows about the `res.json()` call - well, not that helpful.

### It silently corrupts your data

This is the part that actually costs money. No crash, no warning, just a negative balance in your bank app:

```ts
JSON.stringify({ price: Infinity }); // => '{"price":null}'
JSON.stringify({ price: NaN }); // => '{"price":null}'
// Your math overflowed. Your client renders an empty cell. Nobody was told.

JSON.stringify({ a: undefined, b: 1 }); // => '{"b":1}'      the key is gone
JSON.stringify([1, undefined, 2]); // => '[1,null,2]'   the same value, now null
JSON.stringify([1, () => {}, 2]); // => '[1,null,2]'
// One value, two corruption flavors, depending on where it sits

JSON.stringify({ m: new Map([["a", 1]]) }); // => '{"m":{}}'
JSON.stringify({ s: new Set([1, 2]) }); // => '{"s":{}}'
// Your data didn't survive. You got an empty object and no error.

JSON.stringify({ b: new Uint8Array([1, 2, 3]) });
// => '{"b":{"0":1,"1":2,"2":3}}'
// A byte array became a dictionary of indices, three times the size
```

Not one of these told you anything went wrong.

### It sends everything it finds

```ts
JSON.stringify({ login: "hello", _internalSecret: "1232" });
// => '{"login":"hello","_internalSecret":"1232"}'
```

There's no list of fields you approved - whatever sits on the object goes out on the wire. Add a column to a database model and it can be in your public API response the same day, and nothing in your code changed to tell you.

## Part 2: encode, don't stringify

Over the last five years, **"parse, don't validate"** became a genuine trend in the JavaScript world. We stopped checking whether data was fine and started converting it into a type we can trust, at the edge, once.

I think the next three years belong to the other half of that idea: **encode, don't `JSON.stringify`.**

The insight is the same. `JSON.stringify` doesn't know what your data is supposed to be, so it guesses. A schema knows. And if it already describes the data going _in_, it can describe the data going _out_, from the same definition.

That's what [**Sury**](https://github.com/DZakh/sury) has been doing since 2022, before anyone else in the JS schema world. Same schema, both directions:

```ts
import * as S from "sury";

const schema = S.schema({
  id: S.bigint,
  at: S.date,
  price: S.number,
});

const encode = S.encoder(schema, S.jsonString);

encode({
  id: 9007199254740993n,
  at: new Date("2026-01-15T10:30:00.000Z"),
  price: 9.99,
});
// => '{"id":"9007199254740993","at":"2026-01-15T10:30:00.000Z","price":9.99}'
```

The types `JSON.stringify` refuses are ordinary fields here. The values it silently corrupts throw instead - with a path. And the fields you didn't declare simply don't make it out:

```ts
encode({ id: 1n, at: new Date(), price: Infinity });
// => throws S.Error: Failed at ["price"]: Expected JSON, received Infinity
```

There's no intermediate object either. `encode` is a function generated for exactly this shape:

```js
(i) => {
  let v0 = i["price"];
  return (
    '{"id":"' +
    i["id"] +
    '","at":' +
    e[0](i["at"].toISOString()) +
    ',"price":' +
    (Number.isFinite(v0) ? v0 : e[1](v0)) +
    "}"
  );
};
```

That's the whole encoder - no, I didn't trim it for the article. The only runtime check left is the `Number.isFinite` guard, the one that turns silent corruption into an error.

### One schema, both directions

And it goes the other way for free - no second mapper to keep in sync:

```ts
const eventSchema = S.union([
  { type: "user.created", id: S.bigint },
  { type: "user.renamed", id: S.bigint, name: S.string },
  { type: "user.deleted", id: S.bigint },
]);

const parseEvent = S.decoder(S.jsonString, eventSchema);
const encodeEvent = S.encoder(eventSchema, S.jsonString);

const event = parseEvent('{"type":"user.renamed","id":"42","name":"Dmitry"}');
// => { type: "user.renamed", id: 42n, name: "Dmitry" }

switch (event.type) {
  case "user.renamed":
    event.name; // string — TypeScript narrows it
    break;
}

encodeEvent(event);
// => '{"type":"user.renamed","id":"42","name":"Dmitry"}'
```

You wrote `id: S.bigint` - the type you want in your code. A `bigint` can't exist in JSON, so **Sury** infers the `"42"` <-> `42n` conversion in both directions. No coercion wrapper, no second schema for the wire format. And no `JSON.parse`/`JSON.stringify` in your own code either - `S.jsonString` is just another schema in the pipeline.

### Encode to X

What's extra cool is that `S.jsonString` is just another schema, and you can replace it with `S.json`, `S.unknown`, or something like `S.toon`/`S.formData`/`S.protobuf` which are currently in development.

```ts
// Same logic and correctness guarantees as S.jsonString
// but not stringified
S.encoder(schema, S.json);
```

## Part 3: what else is out there

Compiling a serializer from a schema is not a new idea. Even I first started working in this direction 4 years ago 😱

**The unmaintained ones.** `compile-json-stringify` and `slow-json-stringify` have both been untouched since 2022, and `@deepkit/type` is a build-time transform with its last release in September 2025. Why that matters: `slow-json-stringify` doesn't escape quotes, so `{ name: 'he said "hi"' }` serializes to `{"name":"he said "hi""}` - a crash-causing bug sitting there for four years.

**[ElysiaJS](https://elysiajs.com/) team prototype.** [json-accelerator](https://github.com/elysiajs/json-accelerator) is an interesting prototype from the people behind one of the fastest HTTP frameworks in the JavaScript ecosystem. But it never shipped inside the framework itself, it hasn't had a release since April 2025, and it does no validation at all - it coerces whatever you hand it, so `{ price: Infinity }` comes out as `{"price":Infinity}`, which isn't even valid JSON.

**Wire formats.** [devalue](https://github.com/sveltejs/devalue) (SvelteKit) and [superjson](https://github.com/ravionhq/superjson) (tRPC) are huge - together around 20M downloads a week - and they solve a different problem:

```ts
const value = { at: new Date("2026-01-15T10:30:00Z"), price: Infinity, secret: "LEAKED" };

devalue.stringify(value);
// => [{"at":1,"price":-4,"secret":2},["Date","2026-01-15T10:30:00.000Z"],"LEAKED"]

superjson.stringify(value);
// => {"json":{"at":"2026-01-15T10:30:00.000Z","price":"Infinity","secret":"LEAKED"},
//     "meta":{"values":{"at":["Date"],"price":["number"]},"v":1}}
```

They make `bigint`, `Date`, `Map` and even circular references survive a round trip, and they do it by inventing their own wire format. Still valid JSON, just not your data anymore - so both ends have to run the same library. Great for JS to JS, not something you hand to an OpenAPI consumer or a Python service.

And a wire format is not a schema. Nothing here knows what your data was supposed to be, so `Infinity` travels happily and `secret` goes out either way. Different problem, no correctness guarantees.

**[typia](https://typia.io/)** reads your TypeScript types at build time and emits a serializer from them. `typia.json.assertStringify<T>()` validates first and points at the field - `invalid type on $input.id` - so it's genuinely safe, and the runtime cost is near zero because everything is inlined at build. The price is the build: you need `ttsc` or the unplugin wired in, it can't run from plain JavaScript, and there's no schema value at runtime to hand to anything else. `bigint` is also [prohibited outright](https://github.com/samchon/typia/issues/444) in its JSON functions, so back to a manual mapper for those.

**[Effect Schema](https://effect.website/)** has had bidirectional codecs for years, and v4 ships `fromJsonString`, so the pipeline idea is there too:

```ts
S.encodeSync(S.fromJsonString(Item))({ price: Infinity, name: "a" });
// => '{"price":null,"name":"a"}'
```

Same `null`. It reaches `JSON.stringify` at the end, and the corruption comes back with it. To be fair, `S.BigInt` into a JSON string throws instead of lying, and `BigIntFromString` bridges it properly - but the plain-number case is the one everybody writes.

**[Zod](https://zod.dev/codecs)** got codecs in 4.1, and it's the one that comes out closest:

```ts
z.encode(schema, { price: Infinity, name: "a" }); // ❌ throws
```

`z.number()` already rejects `NaN` and `Infinity`, so if you write your codecs by hand down to a JSON value, calling `JSON.stringify` on the result afterwards really is safe. It's just still on you: Zod hands you an object, not JSON text, so the last line of the pipeline is the built-in and the guarantee is yours to maintain. You also pay for the intermediate object - more on that in a second.

That leaves the one that's built specifically for this.

**[fast-json-stringify](https://github.com/fastify/fast-json-stringify)** (the Fastify one) is the most used, but it keeps some of the lies and adds a new one:

```ts
const stringify = fastJson({
  type: "object",
  properties: { price: { type: "number" }, name: { type: "string" } },
  required: ["price", "name"],
});

stringify({ price: Infinity, name: "a" }); // => '{"price":null,"name":"a"}'   still corrupts
stringify({ price: NaN, name: "a" }); // => throws: The value "NaN" cannot be converted to a number
stringify({ price: 1, name: 42 }); // => '{"price":1,"name":"42"}'     silently coerced
```

The silent coercion is not a bug - its types literally declare `StringCoercible = string | Date | RegExp` - but it took me a while to believe it wasn't my schema.

The main reason for the behavior is that the schema never reaches TypeScript:

```ts
// fast-json-stringify — <TDoc extends object = object>(doc: TDoc) => string
stringify({ totally: "unrelated", nonsense: 123 }); // ✅ compiles, strict mode

// Sury — (data: { price: number; name: string }) => string
encode({ totally: "unrelated", nonsense: 123 });
// => TS2353: 'totally' does not exist in type '{ price: number; name: string }'
```

In the AI age, ignoring the types your schema provides is a free ticket to many funny bugs. At the same time [**Sury**](https://github.com/DZakh/sury) has `S.fromJSONSchema`, which correctly infers types even from recursive JSON Schema definitions.

Anyway, the whole thing side by side:

| Encode a `{ price, name }` object | **Sury**            | fast-json-stringify    | typia               | Effect Schema       | Zod                 |
| --------------------------------- | ------------------- | ---------------------- | ------------------- | ------------------- | ------------------- |
| `Infinity`                        | ✅ throws with path | ❌ `null`              | ✅ throws with path | ❌ `null`           | ✅ throws           |
| Wrong type                        | ✅ throws with path | ❌ silently coerced    | ✅ throws with path | ✅ throws           | ✅ throws           |
| Missing field                     | ✅ throws with path | ✅ throws (field only) | ✅ throws with path | ✅ throws           | ✅ throws           |
| `bigint` / `Date` as real types   | ✅                  | ❌                     | ❌ `bigint` banned  | ✅                  | ✅ hand-written     |
| Undeclared fields                 | ✅ stripped         | ✅ stripped            | ✅ stripped         | ✅ stripped         | ✅ stripped         |
| Schema reaches TypeScript         | ✅ inferred         | ❌ any object          | ✅ it is the type   | ✅ inferred         | ✅ inferred         |
| Decodes back too                  | ✅ same schema      | ❌                     | ✅                  | ✅                  | ✅                  |
| Emits the JSON text itself        | ✅                  | ✅                     | ✅                  | ✅                  | ❌ you stringify    |
| Runs without a build step         | ✅                  | ✅                     | ❌ compiler         | ✅                  | ✅                  |
| min+gzip                          | **16.4 kB**         | 56.7 kB                | ~0, inlined         | 23.5 kB             | 19.4 kB             |

Those sizes are measured with tree-shaking. fast-json-stringify is the only one here that corrupts and coerces without telling you - and adding [Ajv](https://ajv.js.org/) in front of it fixes that for about 46 bytes, since it already depends on Ajv anyway. What you still don't get for those bytes is the second direction or the types.

## Part 4: "but isn't `JSON.stringify` hardware-accelerated?"

I got this comment a few times during development and it's fair. `JSON.stringify` is C++ inside the engine, hand-tuned for two decades. How does JavaScript beat it?

**The schema already did the work.** `JSON.stringify` has to discover your object's shape on every single call - walk the keys, branch on each value's type, escape every string. **Sury** did all of that once, when you created the encoder. What's left at runtime is string concatenation, which engines optimize just as hard.

**No intermediate object.** This is the one people miss. Even if you write the mappers by hand, you build a whole new object first - `{ id: String(id), at: at.toISOString() }` - and then hand it to `JSON.stringify`, which walks it all over again. An allocation, plus a second pass. **Sury** goes straight to the string.

**And where `JSON.stringify` wins, Sury just calls it.** Long strings, pretty-printed output, subtrees that are already plain JSON. No pride involved 😁

Here's the full benchmark, every row, including the ones I lose:

| Encode to JSON string                 | **Sury**    | `JSON.stringify` | fast-json-stringify | typia    | Effect   | Zod      | devalue / superjson |
| ------------------------------------- | ----------- | ---------------- | ------------------- | -------- | -------- | -------- | ------------------- |
| API response (user profile, 7 fields) | **227 ns**  | 385 ns           | 266 ns              | 277 ns   | 2.15 µs  | 516 ns   | 2.30 - 3.38 µs      |
| List endpoint (100 rows)              | 10.39 µs    | **10.23 µs**     | 10.95 µs            | 10.39 µs | 44.13 µs | 16.64 µs | 120 - 200 µs        |
| Event feed (50 tagged-union events)   | **3.30 µs** | 4.62 µs          | 12.65 µs            | 5.99 µs  | 25.53 µs | 9.38 µs  | 50 - 94 µs          |
| Metrics dict (50 number values)       | 8.34 µs     | **4.72 µs**      | 8.74 µs             | 19.92 µs | 12.16 µs | 14.74 µs | 24 - 34 µs          |
| Labels dict (50 string values)        | **3.67 µs** | **3.67 µs**      | 7.90 µs             | 18.74 µs | 10.69 µs | 14.09 µs | 23 - 33 µs          |
| `bigint` id + binary payload + `Date` | **1.00 µs** | 1.14 µs          | 1.13 µs             | 1.14 µs  | 4.28 µs  | 1.50 µs  | 3.36 - 5.44 µs      |

Every column encodes the same six payloads into byte-identical JSON, in one process, from the same script - `pnpm bench:jsonstring` in the repo if you want to run it yourself. Numbers are medians across three runs.

typia lands closest, which makes sense: it's compiled too, just at build time instead of runtime. Zod pays for the intermediate object it hands you before `JSON.stringify` walks it again.

Important! I'm not asking you to switch for the nanoseconds. The point is that `JSON.stringify` is unsafe, and that fixing it costs you nothing - no performance regression, and in most real shapes an improvement. Safety is the reason. Speed is just the excuse you can bring to your team lead.

## You don't have to rewrite your project

Maybe you like [Zod](https://github.com/colinhacks/zod), or [Valibot](https://valibot.dev/), or [TypeBox](https://github.com/sinclairzx81/typebox), and you're not about to migrate a codebase because a stranger on dev.to wrote a rant about `JSON.stringify`. Fair.

You don't have to. Anything that can emit JSON Schema can hand its schemas to [**Sury**](https://github.com/DZakh/sury), and **Sury** will give you back a typed, compiled, safe encoder:

```ts
const surySchema = S.fromJSONSchema(
  yourExistingSchema["~standard"].jsonSchema.input({ target: "draft-07" }),
);

S.encoder(surySchema, S.jsonString); // safe, jsonString encoder
S.encoder(surySchema, S.json); // safe, json encoder
S.assert(surySchema, data); // fast validation with error paths
```

Keep your schemas where they are. Use **Sury** as an accelerator in the infrastructure layer - the serialization boundary, the hot endpoint, the queue producer - and leave the rest of your application alone. No migration meeting required.

(If the phrase [_Standard JSON Schema_](https://standardschema.dev/json-schema) means nothing to you yet: it's the spec that makes this work across libraries without any of them knowing about each other. Follow me - that's one of the next articles.)

## Wrapping up

I don't think your app is actually broken. Plenty of software has shipped happily on `JSON.stringify` for the fifteen-plus years it exists.

But the next time you see a `null` where a number should be, a timestamp that came back as a string, or a field that vanished between two services - you'll know exactly which function did it, and that it never once warned you.

We stopped trusting unvalidated input years ago. It's time we stopped trusting unvalidated _output_.

**Encode, don't `JSON.stringify`.** - Cheers 🙏

---

**Sury** is [on GitHub](https://github.com/DZakh/sury) - a star genuinely helps, and it's the main thing that keeps me building this. If you have questions, [open an issue](https://github.com/DZakh/sury/issues) or find me on [X](https://x.com/dzakh_dev). 🧬
