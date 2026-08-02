import { test } from "vitest";

import * as S from "../index.mjs";

const rejectingAsyncMember = (message: string) =>
  S.string
    .with(S.asyncDecoderAssert, async () => {})
    .with(S.refine, () => false, { error: message });

test("disjoint async exact literals keep the allocation-free dispatch path", async (t) => {
  let calls = 0;
  const parser = S.asyncParser(
    S.union([
      S.schema(0).with(S.asyncDecoderAssert, async () => {
        calls++;
      }),
      S.schema(1),
    ]),
  );

  t.expect(parser.toString()).not.toContain("(async(");
  await t.expect(parser(0)).resolves.toBe(0);
  await t.expect(parser(1)).resolves.toBe(1);
  t.expect(calls).toBe(1);
});

test("a Promise rejection containing a Sury error falls through", async (t) => {
  let fallbackCalls = 0;
  const schema = S.union([
    S.string.with(S.asyncDecoderAssert, async () => {
      S.parser(S.number)("not a number");
    }),
    S.string.with(S.refine, () => {
      fallbackCalls++;
      return true;
    }),
  ]);

  await t.expect(S.asyncParser(schema)("value")).resolves.toBe("value");
  t.expect(fallbackCalls).toBe(1);
});

test("an async Sury rejection falls through to an overlapping member", async (t) => {
  let fallbackCalls = 0;
  const schema = S.union([
    rejectingAsyncMember("first async member rejected"),
    S.string.with(S.refine, () => {
      fallbackCalls++;
      return true;
    }),
  ]);

  await t.expect(S.asyncParser(schema)("value")).resolves.toBe("value");
  t.expect(fallbackCalls).toBe(1);
});

test("same-literal async members await and fall through inside one group", async (t) => {
  let firstCalls = 0;
  let secondCalls = 0;
  const schema = S.union([
    S.schema("value")
      .with(S.asyncDecoderAssert, async () => {
        firstCalls++;
      })
      .with(S.refine, () => false, { error: "grouped async rejection" }),
    S.schema("value").with(S.asyncDecoderAssert, async () => {
      secondCalls++;
    }),
  ]);

  await t.expect(S.asyncParser(schema)("value")).resolves.toBe("value");
  t.expect(firstCalls).toBe(1);
  t.expect(secondCalls).toBe(1);
});

test("an accepted async first match does not evaluate a same-tier fallback", async (t) => {
  const calls: string[] = [];
  const schema = S.union([
    S.string.with(S.asyncDecoderAssert, async () => {
      calls.push("first");
    }),
    S.string.with(S.asyncDecoderAssert, async () => {
      calls.push("second");
    }),
  ]);

  await t.expect(S.asyncParser(schema)("value")).resolves.toBe("value");
  t.expect(calls).toEqual(["first"]);
});

test("SameValueZero exact literals remain overlapping", async (t) => {
  let fallbackCalls = 0;
  const parser = S.asyncParser(
    S.union([
      S.schema(-0)
        .with(S.asyncDecoderAssert, async () => {})
        .with(S.refine, () => false, { error: "negative zero rejected" }),
      S.schema(0).with(S.refine, () => {
        fallbackCalls++;
        return true;
      }),
    ]),
  );

  t.expect(parser.toString()).toContain("(async(");
  await t.expect(parser(-0)).resolves.toBe(-0);
  t.expect(fallbackCalls).toBe(1);
});

test("cross-group semantic discriminators preserve overlap and disjointness", async (t) => {
  let sameFallbackCalls = 0;
  const sameKind = S.union([
    S.schema({
      kind: S.schema("same"),
      value: S.string,
    }).with(S.asyncDecoderAssert, async () => {
      S.parser(S.number)("not a number");
    }),
    S.schema({
      kind: S.schema("same"),
      value: S.string,
    }).with(S.refine, () => {
      sameFallbackCalls++;
      return true;
    }),
  ]);
  const sameValue = { kind: "same" as const, value: "value" };

  await t.expect(S.asyncParser(sameKind)(sameValue)).resolves.toEqual(sameValue);
  t.expect(sameFallbackCalls).toBe(1);

  let disjointFirstCalls = 0;
  const disjoint = S.asyncParser(
    S.union([
      S.schema({
        kind: S.schema("first"),
        value: S.string,
      }).with(S.asyncDecoderAssert, async () => {
        disjointFirstCalls++;
      }),
      S.schema({
        kind: S.schema("second"),
        value: S.string,
      }),
    ]),
  );
  const secondValue = { kind: "second" as const, value: "value" };

  t.expect(disjoint.toString()).not.toContain("(async(");
  await t.expect(disjoint(secondValue)).resolves.toEqual(secondValue);
  t.expect(disjointFirstCalls).toBe(0);
});

test("distinct symbol identities are disjoint", async (t) => {
  const first = Symbol("same description");
  const second = Symbol("same description");
  const parser = S.asyncParser(
    S.union([
      S.schema(first).with(S.asyncDecoderAssert, async () => {}),
      S.schema(second),
    ]),
  );

  t.expect(parser.toString()).not.toContain("(async(");
  await t.expect(parser(first)).resolves.toBe(first);
  await t.expect(parser(second)).resolves.toBe(second);
});

test("a broad intervening member remains an overlap barrier", async (t) => {
  let fallbackCalls = 0;
  const parser = S.asyncParser(
    S.union([
      S.schema(0)
        .with(S.asyncDecoderAssert, async () => {})
        .with(S.refine, () => false, { error: "zero rejected" }),
      S.schema(1),
      S.number.with(S.refine, () => {
        fallbackCalls++;
        return true;
      }),
    ]),
  );

  t.expect(parser.toString()).toContain("(async(");
  await t.expect(parser(0)).resolves.toBe(0);
  t.expect(fallbackCalls).toBe(1);
});

test("a non-bucketed deoptimized member remains an overlap barrier", async (t) => {
  let fallbackCalls = 0;
  const parser = S.asyncParser(
    S.union([
      S.schema(0)
        .with(S.asyncDecoderAssert, async () => {})
        .with(S.refine, () => false, { error: "zero rejected" }),
      S.unknown.with(S.refine, () => {
        fallbackCalls++;
        return true;
      }),
      S.schema(1),
    ]),
  );

  t.expect(parser.toString()).toContain("(async(");
  await t.expect(parser(0)).resolves.toBe(0);
  t.expect(fallbackCalls).toBe(1);
});

test("async all-reject errors remain flat and source ordered", async (t) => {
  const parse = S.asyncParser(
    S.union([
      rejectingAsyncMember("first async rejection"),
      rejectingAsyncMember("second async rejection"),
      rejectingAsyncMember("third async rejection"),
    ]),
  );

  try {
    await parse("value");
    t.expect.fail("every member should reject");
  } catch (error) {
    t.expect(error).toBeInstanceOf(S.Error);
    t.expect((error as Error).message).toBe(
      'Expected string | string | string, received "value"\n' +
        "- first async rejection\n" +
        "- second async rejection\n" +
        "- third async rejection",
    );
    const unionErrors = (error as { unionErrors?: unknown[] }).unionErrors;
    t.expect(unionErrors).toHaveLength(3);
  }
});

test("an async foreign rejection escapes without trying a fallback", async (t) => {
  const foreignError = new RangeError("foreign async transform rejection");
  let fallbackCalls = 0;
  const schema = S.union([
    S.string.with(S.asyncDecoderAssert, async () => {
      throw foreignError;
    }),
    S.string.with(S.refine, () => {
      fallbackCalls++;
      return true;
    }),
  ]);

  await t.expect(S.asyncParser(schema)("value")).rejects.toBe(foreignError);
  t.expect(fallbackCalls).toBe(0);
});

test("an async object member can reject into a same-discriminator fallback", async (t) => {
  const schema = S.union([
    S.schema({
      kind: S.schema("nested"),
      value: rejectingAsyncMember("nested async member rejected"),
    }),
    S.schema({
      kind: S.schema("nested"),
      value: S.string,
    }),
  ]);
  const input = { kind: "nested", value: "value" };

  await t.expect(S.asyncParser(schema)(input)).resolves.toEqual(input);
});

test("a nested async foreign rejection keeps identity and skips object fallback", async (t) => {
  const foreignError = new TypeError("nested foreign async rejection");
  let fallbackCalls = 0;
  const schema = S.union([
    S.schema({
      kind: S.schema("nested"),
      value: S.string.with(S.asyncDecoderAssert, async () => {
        throw foreignError;
      }),
    }),
    S.schema({
      kind: S.schema("nested"),
      value: S.string.with(S.refine, () => {
        fallbackCalls++;
        return true;
      }),
    }),
  ]);

  await t
    .expect(S.asyncParser(schema)({ kind: "nested", value: "value" }))
    .rejects.toBe(foreignError);
  t.expect(fallbackCalls).toBe(0);
});

test("a nested async union falls through before its containing object resolves", async (t) => {
  const schema = S.schema({
    payload: S.union([
      rejectingAsyncMember("nested union member rejected"),
      S.string,
    ]),
  });
  const input = { payload: "value" };

  await t.expect(S.asyncParser(schema)(input)).resolves.toEqual(input);
});
