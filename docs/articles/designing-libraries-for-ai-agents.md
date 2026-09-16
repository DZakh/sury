---
title: I'm designing libraries professionally for 5 years
published: false
description: How agentic programming changed the way I design a public API, with four things I did in the Sury v11 release.
tags: typescript, opensource, ai, webdev
# cover_image: https://direct_url_to_image.jpg
# Use a ratio of 100:42 for best results.
# published_at: 2026-06-16 17:36 +0000
---

7 years ago I published my first library to npm [custom-border-mixin](https://www.npmjs.com/package/custom-border-mixin). Back then I decided to postpone learning JavaScript and spent a week writing an SCSS mixin for fun. If you wonder what fun might be in SCSS, just look at the helper:

```scss
@function param-get($parameters, $key) {
  $value: map-get($parameters, $key);
  @if $key ==
    "side" and
    $value !=
    "top" and
    $value !=
    "right" and
    $value !=
    "bottom" and
    $value !=
    "left"
  {
    @error 'Value #{$value} of property #{$key} must be either top, or right, or bottom, or left, or vertical, or horizontal, or all.';
  }
  @if ($key == "size" or $key == "length" or $key == "gap") and
    (type-of($value) != number or type-of($value) == number and $value < 0)
  {
    @error 'Value #{$value} of property #{$key} must be non-negative size number.';
  }
  @if $key == "color" and type-of($value) != color {
    @error 'Value #{$value} of property #{$key} must be color.';
  }
  @if $key ==
    "start" and
    $value !=
    "origin" and
    $value !=
    "center" and
    $value !=
    "opposite"
  {
    @error 'Value #{$value} of property #{$key} must be either origin, center, or opposite.';
  }
  @return $value;
}
```

Nobody wanted and nobody asked for the mixin, but I think it born some passion inside of me. Probably this is how many people come to open-source.

My name is [Dmitry](https://x.com/dzakh_dev) and in the article I'll share how my vision on libraries design and public API specificaly changed with coming of agentic programming.

I worked in a platform team, then 5 years ago created my personal open-source project [Sury](https://github.com/DZakh/sury). It's v11 and still going. Currently I work at [Envio](https://envio.dev/) where I shape and build the fasted blockchain indexing tool ([HyperIndex GitHub](https://github.com/enviodev/hyperindex)).

## How AI changed library API design?

In an ideal world there should be close to none difference between API for human-being or an AI agent. It just happened that designing libraries before, we often relied on expectation that users read the docs, have prior knowledge, or _context_. With agents this not always work, and what becomes important is to design the library the way, so the API will drive the usage.

**To fall into the pit of success** - it's never old.

Besides obvious ones like guiding error messages, here are some ideas I used when shipping [Sury](https://github.com/DZakh/sury) v11 release. Sury is a JavaScript schema library and I'll use it for examples from now on.

## 1. Prefer explicit over implicit

There's no `S.parse` in Sury. I didn't want to give a default which throws somewhere down the line and nobody handles it. So there are two names, and you have to choose:

```ts
S.parseOrThrow(userSchema, data);
// { id: "p_1" }

S.parseAsResult(userSchema, data);
// { success: true, value: { id: "p_1" } }
// { success: false, error: SuryError: Expected string, received 42 }
```

Now the choice is written down. The agent is more likely to take `parseAsResult`, and on review another agent can actually see that nobody handles the error.

Compare it with Zod, where the throwing one gets the short name:

```ts
userSchema.parse(data); // throws
userSchema.safeParse(data); // returns a result
```

Looks finished, doesn't it? Nothing in `parse` tells you that an exception is coming, and the safe version is hidden behind a name you have to know about.

Half a joke, but an important one - it matters even more now, when the review is done by an AI as well. A human at least could have a bad feeling about `parse`. 😄

### The same story, but worse

Now look at `is`. Every schema library has something like it, and on the first glance it looks completely harmless:

```ts
if (is(userSchema, data)) {
  // data is a User, right?
}
```

But if the schema transforms something, there are two answers to this question. Every library picks one for you, and, surprise, they don't pick the same one:

| | The helper | What it checks |
|---|---|---|
| [Zod](https://zod.dev/) | `z.validate(schema, data)` | Input |
| [Valibot](https://valibot.dev/) | `v.is(schema, data)` | Input |
| [ArkType](https://arktype.io/) | `schema.allows(data)` | Input |
| [TypeBox](https://github.com/sinclairzx81/typebox) | `Value.Check(schema, data)` | Input |
| [io-ts](https://github.com/gcanti/io-ts) | `codec.is(data)` | Output |
| [Effect](https://effect.website/) | `Schema.is(schema)(data)` | Output |
| [Superstruct](https://github.com/ianstormtaylor/superstruct) | `is(data, struct)` | Output |
| [Yup](https://github.com/jquense/yup) | `schema.isValidSync(data)` | converts first, so both pass |
| [Joi](https://joi.dev/) | `schema.validate(data)` | converts first, so both pass |
| [Sury](https://github.com/DZakh/sury) | `S.isInput` / `S.isOutput` | the one you picked |

Same call, opposite meaning, depending on what's in your `package.json`. And Yup with Joi convert the value first, so they just say yes to both.

You won't notice any of this until the schema gets its first `.transform()`. Russian roulette in disguise. 👀

That's why there's no `S.is`:

```ts
const priceSchema = S.string.with(S.to, S.number);

S.isInput(priceSchema, "42"); // true
S.isOutput(priceSchema, "42"); // false
```

## 2. Any arguments order

An agent which guessed the arguments order wrong spends an extra iteration on it. And why should it? So I made every order work:

```ts
S.parseOrThrow(data, userSchema);
S.parseOrThrow(userSchema, data);
S.parseOrThrow(userSchema)(data);

S.isInput(data, userSchema);
S.isInput(userSchema, data);
S.isInput(userSchema)(data);
```

All of them give the same result, so any guess is the right guess.

## 3. Force a decision where it matters

When a case can be read in more than one way, I don't want to pick a default for you. I make you choose:

```ts
S.decodeOrThrow(S.env, S.string);
// SuryError: Ambiguous "" for string. Should a blank input be rejected,
// kept, or read as absent? Choose with S.nonEmpty, S.minLength(0),
// or S.optional
```

And it throws when you build the decoder, not when the data arrives, so you see it while writing the code. An empty env var is a decision, and I don't think my library should make it for you.

## 4. Aliases for common knowledge

As much as I'd like to force my own API, there are practices the agent takes from its own knowledge. Fighting them costs an iteration, so I just made aliases:

```ts
S.union([S.literal("admin"), S.object({ role: S.literal("user") })]);
S.union([S.schema("admin"), S.schema({ role: S.schema("user") })]);
S.union(["admin", { role: "user" }]);
// all three: Schema<"admin" | { role: "user" }>
```

And if you don't like one of the spellings in your own codebase, that's a linter rule, not a decision I should make for everybody:

```js
// eslint.config.js
"no-restricted-syntax": ["error", {
  selector: "CallExpression[callee.object.name='S'][callee.property.name='object']",
  message: "Use S.schema instead of S.object",
}]
```

## Back to the pit of success

Honestly, nothing here is really about AI. A library which drives its own usage was always better for people too. It's just that agents stopped forgiving the parts we used to cover with docs.

All of this is in [Sury](https://github.com/DZakh/sury) v11, which is out now. And if you want more about schema libraries and library design, follow me on [X](https://x.com/dzakh_dev) - it'll make my day 🙏
