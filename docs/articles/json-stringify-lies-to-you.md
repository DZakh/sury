---
title: Encode, don't stringify - how JSON.stringify lies to you
published: true
description: Corrupted data, crashes, undefined for a string type. There are zero reasons to keep using it.
tags: javascript, typescript, json, webdev
---

Do you remember how 5-7 years ago everyone used [Yup](https://github.com/jquense/yup) or [Ajv](https://ajv.js.org/) and was genuinely happy about it? I have to admit half of the internet is still happy about it 🫡

Then ["Parse, don't validate"](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/) came out and became a handbook for many developers, and then made it big in the JavaScript/TypeScript world as well. Thousands of schema libraries for parsing are proof 😁

Well... this is already solved and not why you opened the article. What's not solved is:

- **"Encode, don't stringify"**

Or in other words **NEVER use `JSON.stringify` in your code.** - Yeah, this is a hot take now!

Let me prove it and make the article a turning point for the JavaScript ecosystem the way ["Parse, don't validate"](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/) was in 2019. Let's go!

## FAQ before we start

**"Never? Really?"**
Yes. It's acceptable only as an implementation detail of an encoder you shouldn't see in your code.

**Who are you?**
I'm [Dmitry](https://github.com/DZakh) - I build OSS with top-notch DX and performance, I'm a [ReScript Lang](https://rescript-lang.org/) team member and the creator of [Sury](https://github.com/DZakh/sury), which will also be mentioned in the article.

**"Is this an ad?"**
Partially. I'll show you a real problem, and I'll give you an honest overview of the available solutions. [Sury](https://github.com/DZakh/sury) is the library I've worked on for the past 4.5 years, and I'm genuinely proud that it's the best tool currently available. What's important is the concept, and as happened with "Parse, don't validate", I believe that with time there'll be more and more alternatives.

> I'll appreciate a share if you like the article 🙏 Let's make "Encode, don't stringify" big!

## Part 1: full of lies

What's wrong with `JSON.stringify`, you say? The API almost every project has. Well, let's count how many holes you have in your shoes. I think I gathered all of them during my 7 years of development. Share in the comments if `JSON.stringify` has never lied to you before - I'll be surprised 👀

### It returns `undefined`

```ts
JSON.stringify(undefined);
// => undefined

JSON.stringify(() => {});
// => undefined
```

Somebody will say ok... but look at the TypeScript return type:

```ts
stringify(value: any, replacer?: ..., space?: ...): string;
```

The `any` argument makes it especially nice. I won't explain how it can go wrong at this point.

There's a good tool [ts-reset](https://github.com/mattpocock/ts-reset), but it doesn't save you here - it patches `JSON.parse` and ships no rule for `JSON.stringify` at all.

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

The last 2 are true edge cases outside of the article topic, but I'll still add the example to the collection of unhandled errors.

### The errors don't tell you where

When it does throw, Hide & Seek starts:

```ts
JSON.stringify({ user: { orders: [{ total: 1n }] } });
// => TypeError: Do not know how to serialize a BigInt
```

You are literally blind at this point, and the stack trace just tells you `res.json()` - not that helpful.

### It silently corrupts your data

I want to highlight this part. It might actually cost real money. No crash, no warning, and very easy to miss:

```ts
JSON.stringify({ price: Infinity }); // => '{"price":null}'
JSON.stringify({ price: NaN }); // => '{"price":null}'
// Your math overflowed. Your client renders an empty cell or crashes

JSON.stringify({ a: undefined, b: 1 }); // => '{"b":1}'      the key is gone
JSON.stringify([1, undefined, 2]); // => '[1,null,2]'   the same value, now null
JSON.stringify([1, () => {}, 2]); // => '[1,null,2]'
// Maybe expected for you, but not for the T | null check on the other side

JSON.stringify({ m: new Map([["a", 1]]) }); // => '{"m":{}}'
JSON.stringify({ s: new Set([1, 2]) }); // => '{"s":{}}'
// Just lost data and no error

JSON.stringify({ b: new Uint8Array([1, 2, 3]) });
// => '{"b":{"0":1,"1":2,"2":3}}'
// A byte array became a dictionary nobody knows how to handle
```

The biggest problem is that this goes unnoticed, which sometimes even TypeScript can't protect you from.

### It sends everything it finds

```ts
JSON.stringify({ login: "hello", _internalSecret: "1232" });
// => '{"login":"hello","_internalSecret":"1232"}'
```

There's no list of fields you approved - whatever sits on the object goes out on the wire. I doubt it'll actually be a secret, but some internal state - easily.

## Part 2: encode, don't stringify

I guess you came across at least one of the `JSON.stringify` lies in the past if you're still reading the article. Happy to see you here 🤝

The solution to the problem is simple, and the insight is the same as in "Parse, don't validate". `JSON.stringify` doesn't know what your data is supposed to be, so it guesses. Instead, for every application type before stringify, we need to create an encoder that will safely turn the value into valid JSON, or into a JSON string directly:

```ocaml
type user = {
  name : string;
  age : int;
}

let user_to_json { name; age } =
  `Assoc [
    ("name", `String name);
    ("age", `Int age);
  ]

let json = user_to_json { name = "Alice"; age = 42 }

let output = Yojson.Safe.to_string json
(* {"name":"Alice","age":42} *)
```

I used OCaml here because I think it perfectly shows the idea at its root and how people have been dealing with it in "serious" languages for ages. The only detail is that in real-world scenarios, the encoders are usually produced from type definitions - like [Typia](https://typia.io/) does in the JavaScript world.

My preference is using a runtime schema, though. All modern JavaScript schema libraries are already a source of truth for our data models with type inference in place. And if it already describes the data going _in_, it can describe the data going _out_, from the same definition.

That's what [**Sury**](https://github.com/DZakh/sury) pioneers in the JavaScript schema world. Same schema, both directions:

```ts
import * as S from "sury";

const schema = S.schema({
  id: S.bigint,
  at: S.date,
  price: S.number,
});
//? Schema<{id: bigint, at: Date, price: number}, {id: bigint, at: Date, price: number}>

const encodeToJsonString = S.encoder(schema, S.jsonString);
//? (data: {id: bigint, at: Date, price: number}) => string

encodeToJsonString({
  id: 9007199254740993n,
  at: new Date("2026-01-15T10:30:00.000Z"),
  price: 9.99,
});
// => '{"id":"9007199254740993","at":"2026-01-15T10:30:00.000Z","price":9.99}'

// or
const encodeToJson = S.encoder(schema, S.json);
//? (data: {id: bigint, at: Date, price: number}) => JSON

// or (coming soon)
const encodeToBase64url = S.encoder(schema, S.base64url);
//? (data: {id: bigint, at: Date, price: number}) => string

// or safely decode it back
S.decoder(
  S.jsonString,
  schema,
)('{"id":"9007199254740993","at":"2026-01-15T10:30:00.000Z","price":9.99}');
// => {id: 9007199254740993n, at: new Date("2026-01-15T10:30:00.000Z"), price: 9.99}
```

The types `JSON.stringify` refuses are automatically coerced using the schema definition logic. The invalid values explicitly fail with a path instead of silently corrupting. The undeclared fields are stripped, and everything is easy to use, small, fast, and type-safe.

```ts
encodeToJsonString({ id: 1n, at: new Date(), price: Infinity });
// => throws SuryError: Failed at ["price"]: Expected JSON, received Infinity
```

There's no intermediate object either. `encodeToJsonString` is a function generated for exactly this shape:

```js
(i) => {
  let v0 = i["price"];
  return (
    '{"id":"' +
    i["id"] +
    '","at":"' +
    i["at"].toISOString() +
    '","price":' +
    (Number.isFinite(v0) ? v0 : e[1](v0)) +
    "}"
  );
};
```

That's the whole evaluated encoder. The only runtime check left is the `Number.isFinite` guard, which is allowed during parsing from unknown, but not when encoding to JSON.

## Part 3: what else is out there

Compiling an encoder from a schema is not a new idea. Let's go through all the currently available solutions, so you have the full picture.

**The unmaintained ones.** `compile-json-stringify` and `slow-json-stringify` have both been untouched since 2022, and `@deepkit/type` is a build-time transform with its last release in September 2025. Why that matters: `slow-json-stringify` doesn't escape quotes, so `{ name: 'he said "hi"' }` serializes to `{"name":"he said "hi""}` - a crash-causing bug sitting there for four years. I don't recommend using them.

**[ElysiaJS](https://elysiajs.com/) team prototype.** [json-accelerator](https://github.com/elysiajs/json-accelerator) is an interesting prototype from the people behind one of the fastest HTTP frameworks in the JavaScript ecosystem. But it never shipped inside the framework itself, it hasn't had a release since April 2025, and it does no validation at all - it coerces whatever you hand it, so `{ price: Infinity }` comes out as `{"price":Infinity}`, which isn't even valid JSON. The goal of the project was to make a faster `JSON.stringify` instead of fixing correctness issues - I don't recommend it either.

**Wire formats.** [devalue](https://github.com/sveltejs/devalue) (SvelteKit) and [superjson](https://github.com/ravionhq/superjson) (tRPC) are huge - together around 20M downloads a week - and they solve a different problem:

```ts
const value = {
  at: new Date("2026-01-15T10:30:00Z"),
  price: Infinity,
  secret: "LEAKED",
};

devalue.stringify(value);
// => [{"at":1,"price":-4,"secret":2},["Date","2026-01-15T10:30:00.000Z"],"LEAKED"]

superjson.stringify(value);
// => {"json":{"at":"2026-01-15T10:30:00.000Z","price":"Infinity","secret":"LEAKED"},
//     "meta":{"values":{"at":["Date"],"price":["number"]},"v":1}}
```

They make `bigint`, `Date`, `Map` and even circular references survive a round trip, and they do it by inventing their own wire format. I recommend the solution for simplicity when you control both the parsing and stringifying ends and don't control the structure sent (unknown schema). But it's a wire format that third-party consumers won't understand, and not having a schema leads to the same uncontrolled data problem as `JSON.stringify`.

**[typia](https://typia.io/)** reads your TypeScript types at build time and emits a serializer from them. `typia.json.assertStringify<T>()` validates first and points at the field - `invalid type on $input.id` - so it's genuinely safe, and the runtime cost is near zero because everything is inlined at build. The price is the build: you need `ttsc` or the unplugin wired in, it can't run from plain JavaScript, and there's no schema value at runtime to hand to anything else. `bigint` is also [prohibited outright](https://github.com/samchon/typia/issues/444) in its JSON functions, so back to a manual mapper for those. Recommended if you like compile-time tools.

**[Effect Schema](https://effect.website/)** has had bidirectional codecs for years, and v4 ships `fromJsonString`, so the pipeline idea is there too:

```ts
S.encodeSync(S.fromJsonString(Item))({ price: Infinity, name: "a" });
// => '{"price":null,"name":"a"}'
```

However, the same `null` problem is still here. It reaches `JSON.stringify` at the end, and the corruption comes back with it. You can use `BigIntFromString` to bridge the `bigint` type properly, but it pollutes your schema definition with wire logic.

Besides all the roast, I'd call Effect v4 the best TypeScript library of 2026, and Effect Schema is a no-brainer if you already use Effect.

**[Zod](https://zod.dev/codecs)** got codecs in 4.1, and it's the one that comes out closest:

```ts
z.encode(schema, { price: Infinity, name: "a" }); // ❌ throws
```

Although the case works only because `z.number()` already rejects `NaN` and `Infinity`, not because it knows that the target is JSON. And regarding advanced types like `bigint` or `Date`, you need to have hand-written codecs attached to the schema definition. This is not that bad; what's important is that it perfectly solves the problem of `JSON.stringify` unsafety. There's also no direct JSON string encoding target, but after you've performed an encode powered by a schema, `JSON.stringify` is no longer unsafe to use. If you already use Zod, I recommend switching to encoders instead of unsafe stringify.

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

The silent coercion is not a bug - its types literally declare `StringCoercible = string | Date | RegExp` - but it took me a while to figure out what was wrong.

The main reason for the behavior is that the schema never reaches TypeScript:

```ts
// fast-json-stringify — <TDoc extends object = object>(doc: TDoc) => string
stringify({ totally: "unrelated", nonsense: 123 }); // ✅ compiles, strict mode

// Sury — (data: { price: number; name: string }) => string
encode({ totally: "unrelated", nonsense: 123 });
// => TS2353: 'totally' does not exist in type '{ price: number; name: string }'
```

In the AI age, ignoring the types your schema provides is a free ticket to funny bugs. At the same time, [**Sury**](https://github.com/DZakh/sury) has `S.fromJSONSchema`, which correctly infers types even from recursive JSON Schema definitions. I wouldn't recommend `fast-json-stringify` - it adds neither performance nor correctness guarantees.

Anyway, the whole thing side by side:

| Encode a `{ price, name }` object | **Sury**         | Zod              | Effect Schema       | typia              | fast-json-stringify    |
| --------------------------------- | ---------------- | ---------------- | ------------------- | ------------------ | ---------------------- |
| `Infinity`                        | ✅ throws + path | ✅ throws + path | ❌ `null`           | ✅ throws + path   | ❌ `null`              |
| Wrong type                        | ✅ throws + path | ✅ throws + path | ✅ throws + path    | ✅ throws + path   | ❌ silently coerced    |
| Missing field                     | ✅ throws + path | ✅ throws + path | ✅ throws + path    | ✅ throws + path   | ✅ throws (field only) |
| `bigint` / `Date` as real types   | ✅               | ✅ hand-written  | ✅ explicit encoder | ❌ `bigint` banned | ❌                     |
| Undeclared fields                 | ✅ stripped      | ✅ stripped      | ✅ stripped         | ✅ stripped        | ✅ stripped            |
| Schema reaches TypeScript         | ✅ inferred      | ✅ inferred      | ✅ inferred         | ✅ it is the type  | ❌ any object          |
| Decodes back too                  | ✅ same schema   | ✅               | ✅                  | ✅                 | ❌                     |
| Runs without a build step         | ✅               | ✅               | ✅                  | ❌ compiler        | ✅                     |
| min+gzip                          | **16.4 kB**      | 19.4 kB          | 23.5 kB             | inlined            | 56.7 kB                |

Those sizes are measured with tree-shaking. fast-json-stringify is the only one here that corrupts and coerces without telling you - and adding [Ajv](https://ajv.js.org/) in front of it fixes that for about 46 bytes, since it already depends on Ajv anyway.

## Part 4: "but isn't encoding slow?"

This is the debate I used to hear often when people started bringing schemas for parsing to every project 5 years ago. It's pretty much the same situation here - you trade extra logic for correct outgoing data. But my point is that it can be a winning trade, with performance improved instead.

Here's the full benchmark taken from `fast-json-stringify` readme page:

| Encode to JSON string                 | **Sury**    | `JSON.stringify` | Zod      | Effect   | typia    | fast-json-stringify | devalue / superjson |
| ------------------------------------- | ----------- | ---------------- | -------- | -------- | -------- | ------------------- | ------------------- |
| API response (user profile, 7 fields) | **227 ns**  | 385 ns           | 516 ns   | 2.15 µs  | 277 ns   | 266 ns              | 2.30 - 3.38 µs      |
| List endpoint (100 rows)              | 10.39 µs    | **10.23 µs**     | 16.64 µs | 44.13 µs | 10.39 µs | 10.95 µs            | 120 - 200 µs        |
| Event feed (50 tagged-union events)   | **3.30 µs** | 4.62 µs          | 9.38 µs  | 25.53 µs | 5.99 µs  | 12.65 µs            | 50 - 94 µs          |
| Metrics dict (50 number values)       | 8.34 µs     | **4.72 µs**      | 14.74 µs | 12.16 µs | 19.92 µs | 8.74 µs             | 24 - 34 µs          |
| Labels dict (50 string values)        | **3.67 µs** | **3.67 µs**      | 14.09 µs | 10.69 µs | 18.74 µs | 7.90 µs             | 23 - 33 µs          |
| `bigint` id + binary payload + `Date` | **1.00 µs** | 1.14 µs          | 1.50 µs  | 4.28 µs  | 1.14 µs  | 1.13 µs             | 3.36 - 5.44 µs      |

Important! I'm not saying you need to switch to [Sury](https://github.com/DZakh/sury) for the nanoseconds. The point is that `JSON.stringify` is unsafe, and that fixing it costs you nothing - no performance regression, and in most real shapes an improvement. Safety is the main reason, and if you decide to migrate, you can bring speed as an extra candy for your team.

Other libraries are still slower than raw `JSON.stringify`, but not to the extent where it becomes a problem for the majority of real projects. My main goal is to show that it's possible to be faster, and [Sury](https://github.com/DZakh/sury) is the proof. I also think that in a few years other tools will catch up and encoding will become a standard in the JavaScript ecosystem the same way as parsing did.

## Part 5: how did I beat hardware-accelerated `JSON.stringify`?

`JSON.stringify` is C++ inside the engine, hand-tuned for two decades. Let me tell you how [Sury](https://github.com/DZakh/sury) manages to be faster than this.

**JIT.** Sury uses Just-In-Time compilation via `new Function` to generate optimized code at runtime. The approach is battle-tested and has no known security issues. It's also how TypeBox, Zod v4 and ArkType work, and even Cloudflare Workers added support for `eval` calls recently.

**The schema already did the work.** `JSON.stringify` has to discover your object's shape on every single call - walk the keys, branch on each value's type, escape every string. Sury does all of that once, when you create the encoder. What's left at runtime is string concatenation, which engines optimize just as hard.

**No intermediate object.** This is the one other solutions miss. Even if you write the mappers by hand, you build a whole new object first - `{ id: String(id), at: at.toISOString() }` - and then hand it to `JSON.stringify`, which walks it all over again. An allocation, plus a second pass. Sury goes straight to the string.

**And where `JSON.stringify` wins, Sury just calls it.** Long strings, pretty-printed output, subtrees that are already plain JSON. No pride involved 😁

## You don't have to rewrite your project

Maybe you like [Zod](https://github.com/colinhacks/zod), or [Valibot](https://valibot.dev/), or [TypeBox](https://github.com/sinclairzx81/typebox), and I doubt you're about to migrate a codebase because a stranger on dev.to wrote a rant about `JSON.stringify`...

You don't have to. Anything that can emit JSON Schema can hand its schemas to [Sury](https://github.com/DZakh/sury), and Sury will give you back a typed, compiled, safe encoder:

```ts
const surySchema = S.fromJSONSchema(
  yourExistingSchema["~standard"].jsonSchema.input({ target: "draft-07" }),
);

S.encoder(surySchema, S.jsonString); // safe, jsonString encoder
S.encoder(surySchema, S.json); // safe, json encoder
S.encoder(S.any, surySchema, S.json); // safe, json encoder with validation
S.assert(surySchema, data); // fast validation with error paths
```

Keep your schemas where they are. Use [Sury](https://github.com/DZakh/sury) as an accelerator in the infrastructure layer - the serialization boundary, the hot endpoint, the queue producer - and leave the rest of your application alone. No migration meeting required.

(If the phrase [_Standard JSON Schema_](https://standardschema.dev/json-schema) means nothing to you yet: it's the spec that makes this work across libraries without any of them knowing about each other. Follow me - that's one of the next articles.)

## Wrapping up

I don't think your app is actually broken. Plenty of software has shipped happily on `JSON.stringify` for the fifteen-plus years it exists.

But the next time you see a `null` where a number should be, a timestamp that came back as a string, or a field that vanished between two services - you'll know exactly which function did it, and that it never once warned you.

We stopped trusting unvalidated _input_ years ago. It's time we stopped trusting unvalidated _output_.

**Encode, don't `JSON.stringify`.** - Cheers 🙏

---

**Sury** is [on GitHub](https://github.com/DZakh/sury) - a star genuinely helps, and it's the main thing that keeps me building this. If you have questions, [open an issue](https://github.com/DZakh/sury/issues) or find me on [X](https://x.com/dzakh_dev). 🧬
