---
title: JSON.stringify lies to you
published: false
description: It crashes, it returns undefined, and it silently corrupts your data. After 15 years of shipping bugs, maybe it's time to stop calling it.
tags: javascript, typescript, json, webdev
---

`JSON.stringify` is the default choice in basically every JavaScript project. It's built in, it's fast, it needs no dependency, and everybody already knows it.

It's also responsible for an enormous number of production bugs. Wrong money values. Timestamps that come back as strings. Fields that quietly vanish between the server and the client. Crashes on a payload that worked fine yesterday.

My take: **you should never use `JSON.stringify`.**

That's a strong claim, so let me earn it.

## FAQ before we start

**"Never? Really?"**
For serializing your application data — yes, really. For a `console.log`, a cache key, or a quick debug dump, go wild. This article is about the boundary where your data leaves your process.

**"Isn't this just an ad for your library?"**
Partly, yes — I wrote **Sury** and I think it solves this. But the first half of the article is plain JavaScript, and every problem in it exists whether or not you ever install anything of mine.

**"Aren't your benchmarks rigged?"**
I published the rows where **Sury** loses too. They're further down.

## Part 1: it's completely unsafe

### It crashes

```ts
JSON.stringify({ id: 1n });
// => TypeError: Do not know how to serialize a BigInt

const user = {};
user.self = user;
JSON.stringify(user);
// => TypeError: Converting circular structure to JSON

let root = {}, node = root;
for (let i = 0; i < 50000; i++) node = node.next = {};
JSON.stringify(root);
// => RangeError: Maximum call stack size exceeded
```

Fine, you say — I'll wrap it in a `try/catch`. But *what* do you catch? Every one of these is a `TypeError` or a `RangeError` with a message about a language-level concept. None of them tells you which field of which object in which request blew up. You get a stack trace pointing at your serialization line, which you already knew.

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

The type says `string`. The runtime says otherwise. TypeScript will happily let you do `JSON.stringify(x).length` and hand you a `TypeError: Cannot read properties of undefined` at 3am. The type signature itself lies to you.

### It silently corrupts your data

This is the part that actually costs money. No crash, no warning — just different data on the other side:

```ts
JSON.stringify({ price: Infinity });   // => '{"price":null}'
JSON.stringify({ price: NaN });        // => '{"price":null}'
JSON.stringify({ n: -0 });             // => '{"n":0}'

JSON.stringify({ a: undefined, b: 1 }); // => '{"b":1}'      the field is gone
JSON.stringify([1, undefined, 2]);      // => '[1,null,2]'   ...unless it's an array
JSON.stringify([1, () => {}, 2]);       // => '[1,null,2]'

JSON.stringify({ m: new Map([["a", 1]]) });     // => '{"m":{}}'
JSON.stringify({ s: new Set([1, 2]) });         // => '{"s":{}}'
JSON.stringify({ b: new Uint8Array([1, 2, 3]) });
// => '{"b":{"0":1,"1":2,"2":3}}'

JSON.stringify({ id: 9007199254740993 });
// => '{"id":9007199254740992}'
```

Read that list again. A computation overflowed and produced `Infinity`, and your API returned `null` — which your client happily rendered as an empty cell. A `Map` became an empty object. A byte array became a dictionary of indices, three times the size. An ID lost its last digit.

And notice the inconsistency: `undefined` in an object **drops the key**, but `undefined` in an array **becomes `null`**. Same value, two different corruptions, depending on where it sits.

### The errors don't tell you where

When it does throw, compare these:

```ts
JSON.stringify({ user: { orders: [{ total: 1n }] } });
// => TypeError: Do not know how to serialize a BigInt
```

Which order? Which field? You get to find out by yourself, in production, from a stack trace that only knows about your `res.json()` call.

## Part 2: `JSON.parse` lies too

Say you work around all of the above with a custom mapper. `bigint` becomes a string, `Date` becomes an ISO string, your `Map` becomes an array of pairs. Fine.

Now read it back:

```ts
const wire = JSON.stringify({ id: "42", at: new Date().toISOString() });

const data = JSON.parse(wire);
typeof data.id; // "string"  — you wanted a bigint
typeof data.at; // "string"  — you wanted a Date
```

`JSON.parse` returns `any`, which TypeScript will cheerfully assign to whatever type you claim. So now you need a *second* mapper for the way back, kept in sync with the first one by hand. Two functions, one contract, no compiler checking that they agree.

### "Just use ts-reset"

Fair — and you should. [ts-reset](https://github.com/mattpocock/ts-reset) patches exactly this hole:

```ts
// with ts-reset
const data = JSON.parse(wire);
//    ^? unknown
```

`any` becomes `unknown`, and the compiler stops taking your word for what came off the wire. If you're not using it, go install it; it fixes several other built-ins the same way.

But look at what it changed and what it didn't. `unknown` is a *question*, not an answer. Something still has to narrow it — a hand-written type guard you maintain forever, or a schema. ts-reset doesn't remove the work; it makes the compiler finally admit the work exists. That's an argument for a schema library, not against one.

And notice which rules it ships: there's a `json-parse` rule, and no `json-stringify` rule. That's not an oversight. `JSON.stringify` is still declared to return `string` while handing you `undefined` — but everything else in Part 1 isn't a *type* problem at all. `{ price: number }` is a perfectly true type for `{ price: Infinity }`. The type was never wrong. The output was.

No `.d.ts` file can fix a function that returns the wrong data.

This is the real cost. It isn't that `JSON.stringify` has a few quirks — it's that JSON has fewer types than your program does, and the built-ins hand you that mismatch as homework, twice, in opposite directions.

## Part 3: encode, don't stringify

Over the last three years, **"parse, don't validate"** became a genuine trend in the JavaScript world. We stopped checking whether data was fine and started converting it into a type we can trust, at the edge, once.

I think the next three years belong to the other half of that idea: **encode, don't `JSON.stringify`.**

The insight is the same. `JSON.stringify` doesn't know what your data is supposed to be, so it guesses — and its guesses are the corruptions above. A schema knows. And if the schema already describes the data going *in*, it can describe the data going *out*, from the same definition.

That's what **Sury** does. Same schema, both directions, and the JSON text is compiled:

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
    '{"id":"' + i["id"] +
    '","at":' + e[0](i["at"].toISOString()) +
    ',"price":' + (Number.isFinite(v0) ? v0 : e[1](v0)) +
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

You wrote `id: S.bigint` — the type you want in your code. A `bigint` can't exist in JSON, so **Sury** infers the `"42"` ↔ `42n` conversion in both directions. No `as const`, no coercion wrapper, no second schema for the wire format. And no `JSON.parse` in your own code either — `S.jsonString` is just another schema in the pipeline.

Errors point inside the matched variant, not at the union as a whole:

```ts
parseEvent('{"type":"user.renamed","id":"42"}');
// => throws S.Error: Failed at ["name"]: Expected string, received undefined
```

## Part 4: I'm not the first to notice this

Compiling a serializer from a schema is not a new idea, and I'd rather walk through the existing ones honestly than pretend they don't exist. But let me say upfront which ones I'm skipping, and why.

**I'm skipping everything that needs a compiler.** There's a whole family of libraries — [typia](https://typia.io/) is the best of them — that read your TypeScript types at build time and emit a serializer from them. They're fast, and typia's `assertStringify` is genuinely safe. But they need a compiler transform wired into your build, they can't run from plain JavaScript, and there's no schema value at runtime to hand to anything else. I don't want a compiler. If you're happy to add one, typia is a good tool and I won't argue against it — it's just a different deal than the one this article is about.

**I'm also skipping the unmaintained ones.** `compile-json-stringify` and `slow-json-stringify` have both been untouched since 2022, and `@deepkit/type` is a build-time transform anyway with its last release in September 2025. For the record on why "unmaintained serializer" should worry you: `slow-json-stringify` doesn't escape quotes, so `{ name: 'he said "hi"' }` serializes to `{"name":"he said "hi""}` — output no JSON parser will accept, sitting there for four years.

That leaves two libraries you'd actually reach for today.

**[fast-json-stringify](https://github.com/fastify/fast-json-stringify)** (the Fastify one) keeps some of the lies and adds a new one:

```ts
const stringify = fastJson({
  type: "object",
  properties: { price: { type: "number" }, name: { type: "string" } },
  required: ["price", "name"],
});

stringify({ price: Infinity, name: "a" });  // => '{"price":null,"name":"a"}'   still corrupts
stringify({ price: NaN, name: "a" });       // => throws: The value "NaN" cannot be converted to a number
stringify({ price: 1, name: 42 });          // => '{"price":1,"name":"42"}'     silently coerced
```

`Infinity` still becomes `null`. `NaN` throws with a message that doesn't name the field. And a number where you declared a string gets **silently coerced into a string** — your consumer now receives `"42"` and has no idea it was ever a number. That one isn't even a bug: its types declare `StringCoercible = string | Date | RegExp`. It also has an [`unsafe` string format](https://github.com/fastify/fast-json-stringify) that skips escaping entirely, which is exactly the kind of footgun you don't want in a serializer.

And the schema never reaches TypeScript:

```ts
// fast-json-stringify — <TDoc extends object = object>(doc: TDoc) => string
stringify({ totally: "unrelated", nonsense: 123 }); // ✅ compiles, strict mode

// Sury — (data: { price: number; name: string }) => string
encode({ totally: "unrelated", nonsense: 123 });
// => TS2353: 'totally' does not exist in type '{ price: number; name: string }'
```

You wrote a schema that fully describes the shape, and your editor learned nothing from it.

**[json-accelerator](https://github.com/elysiajs/json-accelerator)** comes from the [ElysiaJS](https://elysiajs.com/) team — the people behind one of the fastest HTTP frameworks in the JavaScript ecosystem — so it's worth taking seriously as the state of the art in raw speed. It's faster still than fast-json-stringify, and completely honest about why: it says in its own README that it **will not** check type validity, and expects the schema to always be correct. Here's what that means in practice:

```ts
const encode = createAccelerator(t.Object({ price: t.Number(), name: t.String() }));

encode({ price: Infinity, name: "a" });      // => '{"price":Infinity,"name":"a"}'
encode({ price: 1 });                        // => '{"price":1,"name":"undefined"}'
encode({ price: 1, name: { a: 1 } });        // => '{"price":1,"name":"[object Object]"}'
```

Look at the first one closely: `{"price":Infinity}` is **not valid JSON**. `JSON.parse` throws on it. The serializer produced a string that no JSON parser in any language will accept, and told you nothing.

That isn't a bug — it's the documented deal. You're supposed to run a validator first. Which means the real comparison isn't "accelerator vs **Sury**", it's "validator + accelerator vs **Sury**":

| Encode a `{ price, name }` object | **Sury** | fast-json-stringify | json-accelerator | + TypeBox validation |
| --- | --- | --- | --- | --- |
| `Infinity` | ❌ throws with path | `null` | `Infinity` (invalid JSON!) | ❌ throws |
| Wrong type | ❌ throws with path | silently coerced | `"[object Object]"` | ❌ throws |
| Missing field | ❌ throws with path | ❌ throws | `"undefined"` | ❌ throws |
| `bigint` / `Date` as real types | ✅ | ❌ | ❌ | ❌ |
| Schema reaches TypeScript | ✅ inferred | ❌ any object | ✅ inferred | ✅ inferred |
| Decodes back too | ✅ same schema | ❌ | ❌ | ❌ |
| min+gzip | **16.2 kB** | 56.7 kB | 13.9 kB | 24.5 kB |

Those sizes are measured, not estimated — esbuild, minified, gzipped, one schema and one encoder each. **Sury**'s 16.2 kB includes validation, encoding *and* decoding. json-accelerator is smaller until you add the validator it tells you to add, at which point it's 1.5× bigger and still can't read the data back.

And notice what none of these three can do at all: give you a `bigint` or a `Date` on one side and correct JSON on the other. They serialize the types JSON already has. The mapper from Part 2 is still yours to write and still yours to keep in sync.

### Even the fast frameworks give up at the last step

Here's the thing that convinced me this article was worth writing.

json-accelerator is an experiment by the Elysia team, not something Elysia ships — it isn't in the framework's dependencies. So what does one of the fastest HTTP frameworks in the ecosystem actually do when it turns your handler's return value into a response body?

It cleans the value against your schema, using its own compiled [`exact-mirror`](https://github.com/elysiajs/exact-mirror) — a genuinely clever bit of engineering that strips fields you didn't declare, hundreds of times faster than the naive approach. It validates, if you declared a `response` schema.

And then:

```ts
// elysia/src/adapter/web-standard/handler.ts
return new Response(JSON.stringify(response), set as any);

// elysia/src/adapter/bun/handler.ts
// Response.json is faster than new Response(JSON.stringify()) in Bun
return Response.json(response, set as any);
```

The schema was *right there*. It was compiled. It was used to strip fields and check types. And at the one step that actually produces the bytes on the wire, it's thrown away and the generic serializer takes over.

I'm not picking on Elysia — it's a great framework, and every other framework I've looked at does the same thing. That's exactly my point. This is the industry-standard architecture: describe your data precisely, validate it rigorously, then hand it to a function that knows none of it. Every corruption in Part 1 still lands in the response body. Return a `bigint` from a handler and you get a raw `TypeError` with no field name. Return an `Infinity` past a `Clean` that isn't a `Check`, and your client gets `null`.

The schema already knows what the JSON should look like. Encoding is the one place nobody uses it.

## Part 5: "but isn't `JSON.stringify` hardware-accelerated?"

This is the objection I get most, and it's a fair one. `JSON.stringify` is C++ inside the engine, hand-tuned for two decades. How does JavaScript beat it?

Three things.

**1. It's still a function call.** Crossing into the engine's serializer, walking an object it knows nothing about, checking every value's type at runtime — that costs more than concatenating a few strings. And string concatenation is not some slow fallback; it's one of the most aggressively optimized operations in every JS engine. Ropes, inline caches, the works.

**2. The schema deletes the work.** `JSON.stringify` has to discover your object's shape on every single call — enumerate keys, branch on each value's type, escape every string. **Sury** did all of that once, at compile time. What's left at runtime is `'{"id":"' + i["id"] + '"'`.

**3. Where `JSON.stringify` wins, Sury calls it.** This is the honest part. **Sury** uses `JSON.stringify` internally — for strings past a length threshold where a manual escape scan stops paying off, for pretty-printed output, and for whole subtrees that are already plain JSON. The point was never that `JSON.stringify` is slow. The point is that `JSON.stringify` doesn't know what your data is. When the schema says a subtree is ordinary JSON, calling the fast built-in *is* the optimal move, and **Sury** makes it.

Here's the full benchmark, every row, including the ones I lose:

| Encode to JSON string | **Sury** | `JSON.stringify` | fast-json-stringify |
| --- | --- | --- | --- |
| API response (user profile, 7 fields) | **242 ns** | 402 ns | 303 ns |
| List endpoint (100 rows) | 11.71 µs | **11.08 µs** | 11.71 µs |
| Event feed (50 tagged-union events) | **3.99 µs** | 5.56 µs | 13.90 µs |
| Metrics dict (50 number values) | 8.31 µs | **4.81 µs** | 9.67 µs |
| Labels dict (50 string values) | **3.94 µs** | 3.96 µs | 8.18 µs |
| `bigint` id + binary payload + `Date` | **1.04 µs** | 1.14 µs | 1.14 µs |

Two rows go to `JSON.stringify`, and I'm not going to pretend otherwise. A dictionary of 50 plain numbers is the ideal case for a generic serializer and a bad case for a schema — there's no structure to specialize on, so all **Sury** adds is a validation pass you didn't have before. That's the trade: on that row you are paying, in nanoseconds, for the guarantee that a `NaN` never silently becomes `null`.

Everywhere there's actual structure — objects with known keys, tagged unions, types JSON can't represent — knowing the shape wins. And it wins against fast-json-stringify at 3.5× less bundle size, with decoding included in the number.

## You don't have to rewrite your project

Maybe you like Zod, or Valibot, or TypeBox, and you're not about to migrate a codebase because a stranger on dev.to wrote a rant about `JSON.stringify`. Fair.

You don't have to. Anything that can emit JSON Schema can hand its schemas to **Sury**, and **Sury** will give you back a typed, compiled, safe encoder:

```ts
const schema = S.fromJSONSchema(otherLibrary.toJSONSchema(yourExistingSchema));

S.encoder(schema, S.jsonString); // safe, compiled JSON output
S.assert(schema, data);          // fast validation with error paths
```

Keep your schemas where they are. Use **Sury** as an accelerator in the infrastructure layer — the serialization boundary, the hot endpoint, the queue producer — and leave the rest of your application alone.

(If the phrase *Standard JSON Schema* means nothing to you yet: it's the spec that makes this work across libraries without any of them knowing about each other. Follow me — that's the next article.)

## Wrapping up

I'm not going to tell you your app is broken. Plenty of software has shipped happily on `JSON.stringify` for fifteen years, and yours might be one of them.

But the next time you see a `null` where a number should be, a timestamp that came back as a string, or a field that vanished between two services — you'll know exactly which function did it, and that it never once warned you.

We stopped trusting unvalidated input years ago. It's time we stopped trusting unvalidated *output*.

**Encode, don't `JSON.stringify`.**

---

**Sury** is [on GitHub](https://github.com/DZakh/sury) — a star genuinely helps, and it's the main thing that keeps me building this. If you have questions, [open an issue](https://github.com/DZakh/sury/issues) or find me on [X](https://x.com/dzakh_dev). 🧬
