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

There's no `S.parse` in Sury. A default `parse` throws somewhere down the line and nobody handles it, so instead there are two names and you have to pick one:

```ts
S.parseOrThrow(userSchema, data);
// { id: "p_1" }

S.parseAsResult(userSchema, data);
// { success: true, value: { id: "p_1" } }
// { success: false, error: SuryError: Expected string, received 42 }
```

Now the choice is in the code. The agent is more likely to pick `parseAsResult`, and during review another agent can notice that an error is never handled.

## 2. Any arguments order

An agent which guessed the arguments order wrong spends an extra iteration on it. So I let every order work:

```ts
S.parseOrThrow(data, userSchema);
S.parseOrThrow(userSchema, data);
S.parseOrThrow(userSchema)(data);
```

Same result from all three. Nothing to get wrong, nothing to re-run.

## 3. Force a decision where it matters

When a case can be read in more than one way, I don't pick a default. I make you choose:

```ts
S.decodeOrThrow(S.env, S.string);
// SuryError: Ambiguous "" for string. Should a blank input be rejected,
// kept, or read as absent? Choose with S.nonEmpty, S.minLength(0),
// or S.optional
```

And it throws when you build the decoder, not when the data arrives. An empty env var is a decision, not something my library should guess for you.

## 4. Aliases for common knowledge

As much as I'd like to force my own API, there are practices the agent brings from its own knowledge. Fighting them costs an iteration, so I just alias them:

```ts
S.schema({ id: S.string });
S.object({ id: S.string });

S.union([S.literal("admin"), S.literal("user")]);
S.enum(["admin", "user"]);
```

Under the hood it's the same thing.

## Shipping!

Honestly, none of this is AI-specific. A library which drives its own usage was always better for people too. Agents just stopped forgiving the parts we used to cover with docs.

If you want to see all of it together, [Sury](https://github.com/DZakh/sury) v11 is out. And follow me on [X](https://x.com/dzakh_dev) - it'll make my day 🙏
