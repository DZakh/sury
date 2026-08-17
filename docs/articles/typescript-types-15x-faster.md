---
title: Making my TypeScript types 15.7x faster
published: true
description: What slows large TypeScript types down, how to measure it with @ark/attest, and the one-field match that made my schema library 15.7x cheaper to type-check.
tags: typescript, performance, opensource, webdev
# cover_image: https://direct_url_to_image.jpg
# Use a ratio of 100:42 for best results.
# published_at: 2026-06-16 17:36 +0000
---

If you opened this article, you probably already agree with me: sometimes TypeScript compiles *painfully* slowly.

The same thing happened to [@_chenglou](https://x.com/_chenglou) — an amazing dev who inspires me (he worked on React, Messenger, ReasonML and ReScript, and currently Midjourney, Pretext).

He's also a user of [Sury](https://github.com/DZakh/sury) — the fastest schema validation library, which I maintain — and the author of this issue: ["Large TS types cause type inference slowdown"](https://github.com/DZakh/sury/issues/166).

That's... not nice. But I knew [ArkType](https://arktype.io) exists, and that people have built wild things in TS's type system (Doom included). So the slowness was clearly solvable, not fundamental. Here's the recipe I used. You can apply it to your own library.

Before we begin, here are the results. These are type-instantiation counts, measured with [`@ark/attest`](https://github.com/arktypeio/arktype/tree/main/ark/attest):

| Operation | Before | After | Faster |
|---|--:|--:|--:|
| Define a 10-field object | 1409 | 343 | 4.1× |
| Define a 5-field object (mixed optional) | 17916 | 10557 | 1.7× |
| Define a 3-level nested object | 31013 | 20419 | 1.5× |
| Define a union of 5 objects | 67580 | 53173 | 1.3× |
| **Extract `S.Output` (5-field object)** | 7842 | 501 | **15.7×** |
| **Extract `S.Input` (5-field object)** | 6767 | 501 | **13.5×** |
| **Extract `S.Output` (nested object)** | 7732 | 501 | **15.4×** |
| **Define + extract (10-field object)** | 9446 | 844 | **11.2×** |
| Merge + extract output | 13651 | 7039 | 1.9× |

The title comes from that **15.7×** on `S.Output`. Extraction is the operation you hit most often: every `type X = S.Output<typeof schema>` in your code pays for it.

Let's begin. The recipe is three steps.

## 1. Pin it down first

If you want to change something, you first need a way to know you didn't break it. For a library like this, that means **tests for types**:

```ts
import { expectTypeOf } from "vitest";
import * as S from "sury";

const user = S.schema({ id: S.string, age: S.number });

// Pin the inferred type. If it drifts, the test fails.
expectTypeOf<S.Output<typeof user>>().toEqualTypeOf<{
  id: string;
  age: number;
}>();
```

I used to use [`ts-expect`](https://github.com/TypeStrong/ts-expect) for this, but I migrated to [Vitest](https://vitest.dev)'s [type-testing utils](https://vitest.dev/guide/testing-types) (`expectTypeOf`, above) to drop a dependency. Either way, I already had the tests, and I'll admit they really earned their keep. A type optimization can quietly turn `{ a: string }` into `{ a?: string }` and nothing throws. The tests are what catch that.

## 2. Measure before you touch anything

If you want to *improve* something, you have to **measure it first**, or you can't actually tell whether you improved anything.

For this I used **[Attest](https://github.com/arktypeio/arktype/tree/main/ark/attest)** (`@ark/attest`) from the [ArkType](https://arktype.io) creators. I'm very inspired by their TS skills, and I'll admit I'm not that strong at type-level optimization myself. Good thing I have *other* strong parts. 😄

It counts the type instantiations an expression costs and pins them as a baseline:

```ts
import { bench } from "@ark/attest";
import * as S from "sury";

bench("define a user schema", () => {
  return S.schema({ id: S.string, age: S.number });
}).types([230, "instantiations"]);
```

Bonus: bake those baselines into CI, and any future regression will fail the build.

## 3. Let Claude do the heavy lifting

The third step was the easy one: I asked Claude to find the most impactful type-instantiation optimizations.

The fix turned out to be small. To read a schema's type back out, `S.Output` / `S.Input` matched against the *entire* `Schema<…>` type. That type is huge: a big object with several `with` overloads, intersected with a union of every schema variant. So TypeScript had to expand all of it on every extraction, and the cost grew with the schema:

```ts
// Before: match the whole Schema<…> shape just to read Output back out
type Output<T> = T extends Schema<infer Output, unknown> ? Output : never;
```

But every Sury schema already carries a `~standard` field (the [Standard Schema](https://standardschema.dev) marker) that holds the resolved types. So the fix is to read that one slot directly, instead of re-deriving it from the whole shape:

```ts
// After: match only the one field that already holds the type
type Output<T> = T extends { "~standard": { types?: { output: infer Output } } }
  ? Output
  : never;
```

Now extraction costs the same no matter how big the schema is. That one change is most of the 11–16× you see above.

## Do you even need it?

It depends (my favorite answer). But feedback loop speed matters more and more as AI iterates on your code. If your codebase's TS compilation is slow, you can use the same approach to identify the slow parts and optimize them.

## Wrapping up

One caveat: 15.7× is the drop in type *instantiations*, not seconds. I didn't time the actual compile. But instantiations are the work `tsc` does while type-checking, so fewer of them is a fair proxy for what you feel in the editor. 🙂

Hope this was useful. Check out [Sury](https://dev.to/dzakh/welcome-sury-the-fastest-schema-with-next-gen-dx-5gl4) — the most powerful schema library in the TS ecosystem. Follow me on X at [@dzakh_dev](https://x.com/dzakh_dev), and ask your questions in the comments!
