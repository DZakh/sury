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

Not the string `"undefined"`. The value `undefined`. From a function whose TypeScript signature is:

```ts
stringify(value: any, replacer?: ..., space?: ...): string;
```

The type says `string`, well... ok... TypeScript will happily let you do `JSON.stringify(x).length` and hand you a Sentry alert with `TypeError: Cannot read properties of undefined` at 3am.

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

The last 2 are honestly true edge cases and wrapping in a `try/catch` will be fine. But let's be honest - how often do you properly handle your `JSON.stringify` errors, especially when `undefined`, `TypeError` and `RangeError` with different reasons are all on the table at the same time?

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

JSON.stringify({ login: "hello", _internalSecret: "1232" });
// => '{"login":"hello","_internalSecret":"1232"}'
// And a field you never meant to send is now in someone else's browser
```

Not one of these told you anything went wrong.

## Part 2: `JSON.parse` lies too

Not what you opened the article for, but bear with me 🙏

Say you work around all of the above with a custom mapper. `bigint` becomes a string, `Date` becomes an ISO string, your `Map` becomes an array of pairs. Fine.

Now read it back:

```ts
const wire = JSON.stringify({ id: "42", at: new Date().toISOString() });

const data = JSON.parse(wire);
typeof data.id; // "string"  — you wanted a bigint
typeof data.at; // "string"  — you wanted a Date
```

`JSON.parse` returns `any`, which TypeScript will cheerfully assign to whatever type you claim. So now you need a _second_ mapper for the way back, kept in sync with the first one by hand. Two functions, one contract, no compiler checking that they agree - normal life for 90% of developers.

### Kind of solution: Use ts-reset

Fair. [ts-reset](https://github.com/mattpocock/ts-reset) patches exactly this hole:

```ts
// with ts-reset
const data = JSON.parse(wire);
//    ^? unknown
```

Now instead of `any` you get `unknown`, which you later parse with one of thousands of schema libraries.

The problem is that none of the `JSON.stringify` issues are solved by `ts-reset`. It ships a `json-parse` rule and no `json-stringify` rule - the return type still says `string`, every corruption above is still in place, and you still need separate encoding and decoding logic for non-jsonable fields in your code.

## Part 3: encode, don't stringify

Over the last five years, **"parse, don't validate"** became a genuine trend in the JavaScript world. We stopped checking whether data was fine and started converting it into a type we can trust, at the edge, once.

I think the next three years belong to the other half of that idea: **encode, don't `JSON.stringify`.**

The insight is the same. `JSON.stringify` doesn't know what your data is supposed to be, so it guesses - and sometimes it guesses wrong. A schema knows exactly. And if the schema already describes the data going _in_, it can describe the data going _out_, from the same definition.

That's what [**Sury**](https://github.com/DZakh/sury) pioneers in the JS schema world. Same schema, both directions:

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

The types `JSON.stringify` refuses are ordinary fields here. And the values it silently corrupts throw instead — with a path:

```ts
encode({ id: 1n, at: new Date(), price: Infinity });
// => throws S.Error: Failed at ["price"]: Expected JSON, received Infinity

JSON.stringify({ price: Infinity });
// => '{"price":null}'
```

`Failed at ["price"]`. Not "somewhere in this request."

There's no intermediate object to stringify, either. `encode` is a function generated for exactly this shape:

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

That's the whole encoder. The keys are baked into string literals, the `bigint` is interpolated directly, the `Date` goes straight to `.toISOString()`, and the only check that survives is the one that actually matters — the `Number.isFinite` guard that turns silent corruption into an error.

### One schema, both directions

The same definition reads the data back, so there's no second mapper to keep in sync:

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

You wrote `id: S.bigint` - the type you want in your code. A `bigint` can't exist in JSON, so **Sury** infers the `"42"` <-> `42n` conversion in both directions. No coercion wrapper, no second schema for the wire format. And no `JSON.parse`/`JSON.stringify` in your own code either - `S.jsonString` is just another schema in the pipeline. One less reason to use `ts-reset` as well.

### Encode to X

What's extra cool is that `S.jsonString` is just another schema, and you can replace it with `S.json`, `S.unknown`, or something like `S.toon`/`S.formData`/`S.protobuf` which are currently in development.

```ts
// Same logic and correctness guarantees as S.jsonString
// but not stringified
S.encoder(schema, S.json);
```

## Part 4: I'm not the first

Compiling a serializer from a schema is not a new idea. Even I first started working in this direction 4 years ago 😱

So let me walk through the existing ones honestly rather than pretend they don't exist. But let me say upfront which ones I'm skipping, and why.

**I'm skipping everything that needs a compiler.** There's a whole family of libraries - [typia](https://typia.io/) is the best of them - that read your TypeScript types at build time and emit a serializer from them. They're fast, and typia's `assertStringify` is genuinely safe. But I don't want a compiler and work with runtime values instead. If you're happy to add one, typia is a good tool and I won't argue against it - it's just a different deal than the one this article is about.

**I'm also skipping the unmaintained ones.** `compile-json-stringify` and `slow-json-stringify` have both been untouched since 2022, and `@deepkit/type` is a build-time transform with its last release in September 2025. For the record on why "unmaintained serializer" should worry you: `slow-json-stringify` doesn't escape quotes, so `{ name: 'he said "hi"' }` serializes to `{"name":"he said "hi""}` - a crash-causing bug sitting there for four years.

**[ElysiaJS](https://elysiajs.com/) team prototype.** [json-accelerator](https://github.com/elysiajs/json-accelerator) is an interesting prototype from the people behind one of the fastest HTTP frameworks in the JavaScript ecosystem. But it never shipped inside the framework itself, it hasn't had a release since April 2025, and it does no validation at all - it coerces whatever you hand it, so `{ price: Infinity }` comes out as `{"price":Infinity}`, which isn't even valid JSON. Skipping it as unmaintained as well.

That leaves one library you'd actually reach for today.

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

The silent coercion is not a bug - its types literally declare `StringCoercible = string | Date | RegExp` - but definitely something I wouldn't expect.

The main reason for the behavior is that the schema never reaches TypeScript:

```ts
// fast-json-stringify — <TDoc extends object = object>(doc: TDoc) => string
stringify({ totally: "unrelated", nonsense: 123 }); // ✅ compiles, strict mode

// Sury — (data: { price: number; name: string }) => string
encode({ totally: "unrelated", nonsense: 123 });
// => TS2353: 'totally' does not exist in type '{ price: number; name: string }'
```

In the AI age, ignoring the types your schema provides is a free ticket to many funny bugs. At the same time [**Sury**](https://github.com/DZakh/sury) has `S.fromJSONSchema`, which correctly infers types even from recursive JSON Schema definitions.

| Encode a `{ price, name }` object | **Sury** (with S.fromJSONSchema) | fast-json-stringify   | + Ajv               |
| --------------------------------- | -------------------------------- | --------------------- | ------------------- |
| `Infinity`                        | ✅ throws with path              | ❌ `null`             | ✅ throws with path |
| Wrong type                        | ✅ throws with path              | ❌ silently coerced   | ✅ throws with path |
| Missing field                     | ✅ throws with path              | ✅ throws (field only) | ✅ throws with path |
| `bigint` / `Date` as real types   | ✅                               | ❌                    | ❌                  |
| Undeclared fields                 | ✅ stripped or prevented         | ✅ stripped           | ✅ stripped         |
| Schema reaches TypeScript         | ✅ inferred                      | ❌ any object         | ❌ any object       |
| Decodes back too                  | ✅ same schema                   | ❌                    | ❌                  |
| min+gzip                          | **16.4 kB**                      | 56.7 kB               | 56.7 kB             |

Those sizes are measured with tree-shaking. And `+ Ajv` is nearly free to add, since fast-json-stringify already depends on it - what you don't get for those bytes is the second direction, the types, or the ability to say `bigint`.

## Part 5: "but isn't `JSON.stringify` hardware-accelerated?"

I got the comment a few times during development and it's a fair one. `JSON.stringify` is C++ inside the engine, hand-tuned for two decades. How does JavaScript beat it?

Three things.

**1. It's still a function call.** Crossing into the engine's serializer, walking an object it knows nothing about, checking every value's type at runtime - that costs more than concatenating a few strings. And string concatenation is also one of the most aggressively optimized operations in every JS engine 😁

**2. The schema deletes the work.** `JSON.stringify` has to discover your object's shape on every single call - enumerate keys, branch on each value's type, escape every string. **Sury** can do it at encoder creation time. What's left at runtime is `'{"id":"' + i["id"] + '"'`.

**3. Where `JSON.stringify` wins, Sury calls it.** Because why not. **Sury** uses `JSON.stringify` internally - for strings past a length threshold where a manual escape scan stops paying off, for pretty-printed output, and for whole subtrees that are already plain JSON. The point of the article is not that `JSON.stringify` is slow. The point is that `JSON.stringify` is not safe. When the schema says a subtree is ordinary JSON, calling the fast built-in _is_ often indeed faster, and **Sury** does it.

Here's the full benchmark, every row, including the ones I lose:

| Encode to JSON string                 | **Sury**    | `JSON.stringify` | fast-json-stringify |
| ------------------------------------- | ----------- | ---------------- | ------------------- |
| API response (user profile, 7 fields) | **242 ns**  | 402 ns           | 303 ns              |
| List endpoint (100 rows)              | 11.71 µs    | **11.08 µs**     | 11.71 µs            |
| Event feed (50 tagged-union events)   | **3.99 µs** | 5.56 µs          | 13.90 µs            |
| Metrics dict (50 number values)       | 8.31 µs     | **4.81 µs**      | 9.67 µs             |
| Labels dict (50 string values)        | **3.94 µs** | 3.96 µs          | 8.18 µs             |
| `bigint` id + binary payload + `Date` | **1.04 µs** | 1.14 µs          | 1.14 µs             |

Important! The main point is not that **Sury** is faster, but that `JSON.stringify` is unsafe and you can solve it without performance regression and even get an improvement for many cases.

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

Keep your schemas where they are. Use **Sury** as an accelerator in the infrastructure layer - the serialization boundary, the hot endpoint, the queue producer - and leave the rest of your application alone.

(If the phrase [_Standard JSON Schema_](https://standardschema.dev/json-schema) means nothing to you yet: it's the spec that makes this work across libraries without any of them knowing about each other. Follow me - that's one of the next articles.)

## Wrapping up

I don't think your app is actually broken. Plenty of software has shipped happily on `JSON.stringify` for the fifteen-plus years it exists.

But the next time you see a `null` where a number should be, a timestamp that came back as a string, or a field that vanished between two services - you'll know exactly which function did it, and that it never once warned you.

We stopped trusting unvalidated input years ago. It's time we stopped trusting unvalidated _output_.

**Encode, don't `JSON.stringify`.** - Cheers 🙏

---

**Sury** is [on GitHub](https://github.com/DZakh/sury) - a star genuinely helps, and it's the main thing that keeps me building this. If you have questions, [open an issue](https://github.com/DZakh/sury/issues) or find me on [X](https://x.com/dzakh_dev). 🧬
