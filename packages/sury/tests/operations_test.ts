import { expect, test } from "vitest";
import * as S from "sury";

// The operation surface itself: which call form an argument list resolves to,
// and the Result tail the compiler emits. Neither is expressible as a spec -
// a spec pins one schema's codegen per direction, and these are properties of
// the call, not of the schema (see CONTRIBUTING.md's Spec Harness
// Suggestions).

const user = S.schema({ id: S.string });

// ── Call forms ───────────────────────────────────────────────────────────────

test("all four call forms resolve, on every arity", () => {
  const trimmed = S.string.with(S.trim);
  const toNumber = S.string.with(S.to, S.number, { decode: Number, encode: String });

  // op(s...) - compiled
  expect(S.parseOrThrow(trimmed)(" a ")).toBe("a");
  expect(S.parseOrThrow(S.unknown, toNumber)("1")).toBe(1);
  expect(S.parseOrThrow(S.unknown, S.string, toNumber)("2")).toBe(2);

  // op(s..., data) - immediate, schema-first
  expect(S.parseOrThrow(trimmed, " a ")).toBe("a");
  expect(S.parseOrThrow(S.unknown, toNumber, "1")).toBe(1);
  expect(S.parseOrThrow(S.unknown, S.string, toNumber, "2")).toBe(2);

  // op(data, s...) - immediate, data-first
  expect(S.parseOrThrow(" a ", trimmed)).toBe("a");
  expect(S.parseOrThrow("1", S.unknown, toNumber)).toBe(1);
  expect(S.parseOrThrow("2", S.unknown, S.string, toNumber)).toBe(2);
});

test("the compiled form is told from the immediate one by argument count alone", () => {
  // `undefined` is a perfectly good value to parse, so a schema slot is never
  // probed for it: only `arguments.length` separates these two.
  expect(typeof S.parseOrThrow(S.void)).toBe("function");
  expect(S.parseOrThrow(S.void, undefined)).toBe(undefined);
  expect(S.parseAsResult(S.void, undefined)).toEqual({
    success: true,
    value: undefined,
    error: undefined,
  });
});

test("every call form shares one compiled operation", () => {
  const schema = S.schema({ id: S.string });
  const compiled = S.parseOrThrow(schema);
  expect(S.parseOrThrow(schema)).toBe(compiled);
  // The immediate forms go through the same memo, so nothing recompiles.
  S.parseOrThrow(schema, { id: "a" });
  S.parseOrThrow({ id: "a" }, schema);
  expect(S.parseOrThrow(schema)).toBe(compiled);
  // A different return mode is a different operation, keyed by its own flag.
  expect(S.parseAsResult(schema)).not.toBe(compiled);
});

test("a Sury schema reaching a data slot parses as data", () => {
  // `op(s1, s2)` always reads as a chain, so a schema is only parsed AS DATA
  // through the compiled form. Documented; no argument order makes both
  // reachable.
  const meta = S.schema({ seq: S.number });
  expect(S.parseOrThrow(meta)(S.string as unknown)).toEqual({ seq: (S.string as any).seq });
});

test("a hole and a foreign Standard Schema are named, not read as data", () => {
  const foreign = {
    "~standard": { version: 1, vendor: "other", validate: (v: unknown) => ({ value: v }) },
  } as unknown as S.Schema<string, string>;
  expect(() => S.parseOrThrow(user, undefined as any, user)).toThrow("Expected a Sury schema");
  expect(() => S.parseOrThrow(foreign, "x")).toThrow("Expected a Sury schema");
  expect(() => (S.parseOrThrow as (...a: unknown[]) => unknown)()).toThrow(
    "Expected a Sury schema",
  );
  expect(() =>
    (S.parseOrThrow as (...a: unknown[]) => unknown)(user, user, user, user, "x"),
  ).toThrow("Expected at most 3 schemas");
});

// ── Result tail ──────────────────────────────────────────────────────────────

test("the Result tail is compiled into the operation, not wrapped around it", () => {
  expect(S.parseAsResult(S.string).toString()).toMatchInlineSnapshot(
    `"i=>{try{typeof i==="string"||e[0](i);return {success:true,value:i,error:void 0}}catch(v0){return {success:false,value:void 0,error:e[1](v0)}}}"`,
  );
});

test("an operation that provably cannot throw emits no try", () => {
  // The raise counter says nothing in the body can fail, so there is no `try`
  // to pay for - the decision a `safe(() => ...)` wrapper can never make.
  expect(S.parseAsResult(S.unknown).toString()).toMatchInlineSnapshot(
    `"i=>{return {success:true,value:i,error:void 0}}"`,
  );
  // Transformation code, and still no `try`: what the body does can't fail.
  expect(
    S.parseAsResult(S.schema({ id: S.unknown }).with(S.noValidation, true)).toString(),
  ).toMatchInlineSnapshot(
    `"i=>{return {success:true,value:{id:i.id},error:void 0}}"`,
  );
});

test("the throw tail is untouched by the Result modes existing", () => {
  expect(S.parseOrThrow(S.string).toString()).toMatchInlineSnapshot(
    `"i=>{typeof i==="string"||e[0](i);return i}"`,
  );
  expect(S.parseOrThrow(S.unknown)).toBe(S.parseOrThrow(S.number.with(S.noValidation, true)));
});

test("both Result branches carry the same keys in the same order", () => {
  // One hidden class for the two branches, so a consumer's `.success`/`.value`
  // reads stay monomorphic.
  const parse = S.parseAsResult(user);
  expect(Object.keys(parse({ id: "a" }))).toEqual(["success", "value", "error"]);
  expect(Object.keys(parse({ id: 1 }))).toEqual(["success", "value", "error"]);
});

test("a Result destructures and narrows", () => {
  const { value, error } = S.parseAsResult(user, { id: "a" });
  expect(error).toBe(undefined);
  expect(value).toEqual({ id: "a" });
  const failure = S.parseAsResult(user, { id: 1 });
  expect(failure.success).toBe(false);
  expect(failure.error).toBeInstanceOf(S.Error);
  expect(failure.error?.code).toBe("invalid_input");
  expect(failure.error?.path).toEqual(["id"]);
});

test("a foreign exception from user code is a failure of the value, in the outcome's own shape", async () => {
  const boom = new TypeError("boom");
  const schema = S.string.with(S.to, S.number, {
    decode: () => {
      throw boom;
    },
    encode: String,
  });
  // Sury wraps a coder failure as `invalid_conversion` - a failure of THIS
  // value - and keeps the original as the cause.
  const result = S.parseAsResult(schema, "1");
  expect(result.error?.code).toBe("invalid_conversion");
  expect((result.error as { cause?: unknown } | undefined)?.cause).toBe(boom);

  // A refinement that throws is that refinement failing, the same way.
  const throwing = S.string.with(S.refine, () => {
    throw boom;
  });
  expect(S.parseAsResult(throwing, "1").error?.code).toBe("invalid_conversion");
  expect(() => S.parseOrThrow(throwing, "1")).toThrow(S.Error);

  // What nothing in the schema catches - a getter - still comes back as a
  // SuryError wherever the operation answers rather than throws, so a
  // consumer never tells a failure from an exception by inspecting it. The
  // shape is the outcome's: a Result, `false`, or a promise of either - never
  // a synchronous throw from a promise-returning operation.
  const evil = new Proxy({}, { get() { throw boom; } });
  const user = S.schema({ id: S.string });
  expect(S.parseAsResult(user, evil).error?.code).toBe("invalid_conversion");
  expect(S.isInput(user, evil)).toBe(false);
  expect((await S.parseAsResultPromise(user, evil)).error?.code).toBe("invalid_conversion");
  expect(await S.isInputAsPromise(user, evil)).toBe(false);
  await expect(S.parseAsPromiseOrReject(user, evil)).rejects.toBe(boom);
  // The throwing outcome is the exception itself.
  expect(() => S.parseOrThrow(user, evil)).toThrow(boom);
});

test("a defect throws instead of becoming a Result", () => {
  // A schema wired wrong fails for every input, so it is the developer's bug,
  // not an entry in someone's form validation. It is raised where the
  // operation is created - which for an immediate call form is that same call.
  const undecodable = S.boolean.with(S.to, S.number, { decode: "never", encode: "never" });
  expect(() => S.parseAsResult(undecodable, true)).toThrow(S.Error);
  expect(() => S.parseAsResult(undecodable)).toThrow(S.Error);
});

test("the promisable Result mode follows the schema's own shape", async () => {
  const sync = S.parseAsPromisableResult(user, { id: "a" });
  expect(sync).toEqual({ success: true, value: { id: "a" }, error: undefined });

  const asyncSchema = S.string.with(S.to, S.number, {
    decode: { async: async (v: string) => Number(v) },
    encode: String,
  });
  const promised = S.parseAsPromisableResult(asyncSchema, "1");
  expect(promised).toBeInstanceOf(Promise);
  expect(await promised).toEqual({ success: true, value: 1, error: undefined });

  // A failure the sync phase raises still comes back as a promise, so an async
  // operation answers in one shape whether the value died before the first
  // await or after it.
  const early = S.parseAsPromisableResult(asyncSchema, 1);
  expect(early).toBeInstanceOf(Promise);
  expect((await early).error?.code).toBe("invalid_input");
});

test("`~standard.validate` is the compiled operation, with no wrapper left", () => {
  // The Standard Schema result shape is a tail of its own (mode bit 1024), so
  // there is no wrapper translating one result shape into another. It is
  // emitted by `throwTail` rather than behind the `__setTail` hook: the
  // `~standard` prototype getter can never be tree-shaken, so registering from
  // it would drag the whole emitter into every consumer bundle.
  expect(S.parseAsResult(user).toString()).toContain("success:true");
  const schema = S.schema({ id: S.string });
  expect(schema["~standard"].validate({ id: "a" })).toEqual({ value: { id: "a" } });
  expect(schema["~standard"].validate({ id: 1 })).toEqual({
    issues: [{ message: "Expected string, received 1", path: ["id"] }],
  });
  // `path` is omitted at the root rather than sent as an empty array.
  expect(S.string["~standard"].validate(1)).toEqual({
    issues: [{ message: "Expected string, received 1" }],
  });
});

test("the public types resolve to the right call form", () => {
  // Nine arity-discriminated overloads per operation. The chain ones come
  // first, which is why the last block below can't be written any other way.
  const User = S.schema({ id: S.string, age: S.number });
  const Str = S.string.with(S.to, S.number, { decode: Number, encode: String });
  const data: unknown = { id: "a", age: 1 };

  const a = S.parseOrThrow(User)(data); a satisfies { id: string; age: number };
  const b = S.parseOrThrow(User, data); b satisfies { id: string; age: number };
  const c = S.parseOrThrow(data, User); c satisfies { id: string; age: number };
  const d = S.parseOrThrow(S.unknown, Str)("1"); d satisfies number;
  const e = S.parseOrThrow(S.unknown, S.string, Str, "1"); e satisfies number;
  const f = S.parseAsResult(User, data); if (f.success) f.value satisfies { id: string };
  const { value, error } = S.parseAsResult(User, data);
  if (error) { error.code satisfies "invalid_input" | "invalid_conversion" | "unrecognized_key"; }
  else { value satisfies { id: string; age: number }; }
  const g = S.decodeOrThrow(Str, "1"); g satisfies number;
  const h = S.encodeOrThrow(Str, 1); h satisfies string;
  const i = S.encodeOrThrow(Str)(1); i satisfies string;
  const j = S.makeInputOrThrow(User)({ id: "a", age: 1 }); j satisfies { id: string; age: number };
  const k = S.makeOutputOrThrow(User, { id: "a", age: 1 }); k satisfies { id: string; age: number };
  const l: unknown = data;
  if (S.isInput(User, l)) { l satisfies { id: string; age: number }; }
  const m: unknown = data;
  S.assertInputOrThrow(User, m); m satisfies { id: string; age: number };
  const n: unknown = data;
  S.assertInputOrThrow(n, User); n satisfies { id: string; age: number };
  const o = S.parseAsResultPromise(User, data); o satisfies Promise<S.Result<{ id: string; age: number }>>;
  const p = S.parseAsPromiseOrReject(User, data); p satisfies Promise<{ id: string; age: number }>;
  const q = S.parseAsPromisableResult(User, data);
  const r = S.isInputAsPromise(User, data); r satisfies Promise<boolean>;
  const t = S.assertInputAsPromiseOrReject(User, data); t satisfies Promise<void>;
  const u = S.isInput(User); u satisfies (x: unknown) => boolean;
  // a schema in a data slot: only via the compiled form
  const Meta = S.schema({ seq: S.number });
  const v = S.parseOrThrow(Meta)(S.string); v satisfies { seq: number };

  const chain = S.parseOrThrow(S.unknown, User); chain satisfies (d: unknown) => { id: string };
  // @ts-expect-error the two-schema form is a chain, never "parse a schema as data"
  const notValue: { id: string } = S.parseOrThrow(S.unknown, User);

  const maybe = "1" as string | undefined;
  const num = 1 as number | undefined;
  S.decodeOrThrow(S.string, "1");
  S.decodeOrThrow("1", S.string);
  S.decodeOrThrow(S.string)("1");
  S.decodeOrThrow(S.string, Str, "1");
  S.decodeAsResult(S.string, "1");
  // @ts-expect-error wider than string input
  S.decodeOrThrow(S.string, maybe);
  // @ts-expect-error wider than string input
  S.decodeOrThrow(maybe, S.string);
  // @ts-expect-error wider than string input
  S.decodeOrThrow(S.string)(maybe);
  // @ts-expect-error wider than string input
  S.decodeOrThrow(S.string, Str, maybe);
  // @ts-expect-error wider than string input
  S.decodeAsResult(S.string, maybe);

  S.encodeOrThrow(S.string, "1");
  S.encodeOrThrow(Str, 1);
  S.encodeAsResult(Str)(1);
  // @ts-expect-error wider than string output
  S.encodeOrThrow(S.string, maybe);
  // @ts-expect-error wider than number output
  S.encodeOrThrow(Str, num);
  // @ts-expect-error wider than number output
  S.encodeOrThrow(Str)(num);
  // @ts-expect-error wider than number output
  S.encodeAsResult(Str, num);

  S.makeInputOrThrow(S.string, "1");
  S.makeOutputOrThrow(Str, 1);
  // @ts-expect-error wider than string input
  S.makeInputOrThrow(S.string, maybe);
  // @ts-expect-error wider than number output
  S.makeOutputOrThrow(Str, num);

  // Both values are checked against the schema's side, not inferred from each
  // other: a pair the schema can't hold has to be rejected, or the comparator
  // would be handed values its emit never narrowed for.
  const isEqStr = S.isEqualInput(S.string); isEqStr satisfies (a: string, b: string) => boolean;
  S.isEqualInput(S.string, "1", "2") satisfies boolean;
  S.isEqualInput("1", "2", S.string) satisfies boolean;
  S.isEqualOutput(Str, 1, 2) satisfies boolean;
  // @ts-expect-error wider than string input
  S.isEqualInput(S.string, "1", maybe);
  // @ts-expect-error wider than string input
  S.isEqualInput(S.string)("1", maybe);
  // @ts-expect-error wider than string input
  S.isEqualInput("1", maybe, S.string);
  // @ts-expect-error wider than number output
  S.isEqualOutput(Str, 1, num);
  // @ts-expect-error the Output side is number, not string
  S.isEqualOutput(Str, "1", "2");
  // A brand is required rather than made: both values are already outputs, so
  // `make`'s Unbranded relaxation would only let the brand be bypassed.
  const Branded = S.uuid.with(S.brand, "UserId");
  const userId = S.makeOutputOrThrow(Branded, "f81d4fae-7dec-11d0-a765-00a0c91e6bf6");
  S.isEqualOutput(Branded, userId, userId) satisfies boolean;
  // @ts-expect-error a plain string is not the branded output
  S.isEqualOutput(Branded, "f81d4fae-7dec-11d0-a765-00a0c91e6bf6", userId);
  const _noMakeChain = () => {
    // @ts-expect-error make takes one schema
    const makeChain: (data: string) => string = S.makeOutputOrThrow(S.string, S.string);
  };
  void _noMakeChain;

  S.parseOrThrow(S.string, maybe);
  S.assertInputOrThrow(S.string, maybe);
  S.isInput(S.string, maybe);
});

// ── The other verbs ──────────────────────────────────────────────────────────

const strToNum = S.string.with(S.to, S.number, { decode: Number, encode: String });

test("decode and encode run the two directions of the same schema", () => {
  expect(S.decodeOrThrow(strToNum, "1")).toBe(1);
  expect(S.encodeOrThrow(strToNum, 1)).toBe("1");
  expect(S.decodeAsResult(strToNum, "1")).toEqual({ success: true, value: 1, error: undefined });
  expect(S.encodeAsResult(strToNum)(1)).toEqual({ success: true, value: "1", error: undefined });

  // Only the first schema is reversed, so a chain after it reads forward.
  expect(S.encodeOrThrow(strToNum, S.jsonString, 1)).toBe(`"1"`);
  expect(S.decodeOrThrow(S.reverse(strToNum), S.jsonString, 1)).toBe(`"1"`);
});

test("make validates and hands back the value it was given", () => {
  // `parse` builds a decoded clone; `make` runs the same checks and keeps the
  // value's identity.
  const value = { id: "u1" };
  expect(S.makeInputOrThrow(user, value)).toBe(value);
  expect(S.parseOrThrow(user, value)).not.toBe(value);
  expect(S.makeOutputOrThrow(user)(value)).toBe(value);
  expect(() => S.makeInputOrThrow(user, { id: 1 as unknown as string })).toThrow(S.Error);
  expect(S.makeInputAsResult(user, { id: 1 as unknown as string }).error?.code).toBe("invalid_input");
  expect(S.makeInputAsResult(user, value)).toEqual({
    success: true,
    value,
    error: undefined,
  });

  // The two directions differ where the schema converts.
  expect(S.makeInputOrThrow(strToNum, "1")).toBe("1");
  expect(S.makeOutputOrThrow(strToNum, 1)).toBe(1);
});

test("make compiles to the checks plus the value, with no wrapper", () => {
  expect(S.makeInputOrThrow(user).toString()).toMatchInlineSnapshot(
    `"i=>{typeof i==="object"&&i&&!Array.isArray(i)||e[1](i);let v0=i.id;typeof v0==="string"||e[0](v0);return i}"`,
  );
  // Nothing to check: the operation is the identity itself.
  expect(S.makeInputOrThrow(S.unknown)).toBe(S.parseOrThrow(S.unknown));
  expect(() => (S.makeOutputOrThrow as (...args: unknown[]) => unknown)(S.string, S.number)).toThrow(
    "Expected a single schema and a value",
  );
  expect(() => (S.makeInputOrThrow as (...args: unknown[]) => unknown)(S.string, S.number, "x")).toThrow(
    "Expected a single schema and a value",
  );
});

test("is answers a boolean from one compiled operation", () => {
  expect(S.isInput(user, { id: "a" })).toBe(true);
  expect(S.isInput({ id: 1 }, user)).toBe(false);
  expect(S.isOutput(strToNum, 1)).toBe(true);
  expect(S.isOutput(strToNum, "1")).toBe(false);
  expect(S.isInput(user).toString()).toMatchInlineSnapshot(
    `"i=>{try{typeof i==="object"&&i&&!Array.isArray(i)||e[1](i);let v0=i.id;typeof v0==="string"||e[0](v0);return true}catch(v1){return false}}"`,
  );
  // Nothing can fail, so there is nothing to catch.
  expect(S.isInput(S.unknown).toString()).toMatchInlineSnapshot(`"i=>{return true}"`);
  // A schema wired wrong is still the developer's bug, not a `false`.
  expect(() =>
    S.isInput(S.boolean.with(S.to, S.number, { decode: "never", encode: "never" }), true),
  ).toThrow(S.Error);
});

test("assert throws, and narrows in either argument order", () => {
  expect(S.assertInputOrThrow(user, { id: "a" })).toBe(undefined);
  expect(() => S.assertInputOrThrow(user, { id: 1 })).toThrow(S.Error);
  expect(() => S.assertInputOrThrow({ id: 1 }, user)).toThrow(S.Error);
  expect(() => S.assertOutputOrThrow(strToNum, "1")).toThrow(S.Error);
  expect(S.assertOutputOrThrow(strToNum, 1)).toBe(undefined);
});

test("the async outcomes", async () => {
  const asyncSchema = S.string.with(S.to, S.number, {
    decode: { async: async (v: string) => Number(v) },
    encode: String,
  });
  expect(await S.parseAsPromiseOrReject(asyncSchema, "1")).toBe(1);
  await expect(S.parseAsPromiseOrReject(asyncSchema, 1)).rejects.toThrow(S.Error);
  expect(await S.parseAsResultPromise(asyncSchema, "1")).toEqual({
    success: true,
    value: 1,
    error: undefined,
  });
  expect((await S.parseAsResultPromise(asyncSchema, 1)).error?.code).toBe("invalid_input");
  expect(await S.isInputAsPromise(asyncSchema, "1")).toBe(true);
  expect(await S.isInputAsPromise(asyncSchema, 1)).toBe(false);
  expect(await S.assertInputAsPromiseOrReject(asyncSchema, "1")).toBe(undefined);
  await expect(S.assertInputAsPromiseOrReject(asyncSchema, 1)).rejects.toThrow(S.Error);
  const value = "1";
  expect(await S.makeInputAsPromiseOrReject(asyncSchema, value)).toBe(value);

  // A synchronous schema still answers in the shape the name promises.
  expect(await S.parseAsPromiseOrReject(S.string, "a")).toBe("a");
  expect(await S.parseAsResultPromise(S.string, 1)).toEqual({
    success: false,
    value: undefined,
    error: expect.any(S.Error),
  });
});

test("a promise-returning operation rejects, it never throws synchronously", async () => {
  // `OrReject` is the whole story its name tells: a value that fails its type
  // check before the first await comes back as a rejection, like one that
  // fails after it.
  const asyncSchema = S.string.with(S.to, S.number, {
    decode: { async: async (v: string) => Number(v) },
    encode: String,
  });
  for (const call of [
    () => S.parseAsPromiseOrReject(asyncSchema, 1),
    () => S.parseAsPromiseOrReject(S.string, 1),
    () => S.assertInputAsPromiseOrReject(asyncSchema, 1),
    () => S.makeInputAsPromiseOrReject(asyncSchema, 1 as unknown as string),
  ]) {
    const answer = call();
    expect(answer).toBeInstanceOf(Promise);
    await expect(answer).rejects.toThrow(S.Error);
  }
  // `is*AsPromise` resolves to the answer and never rejects.
  const answered = S.isInputAsPromise(asyncSchema, 1);
  expect(answered).toBeInstanceOf(Promise);
  await expect(answered).resolves.toBe(false);
});

test("what an operation compiles to depends on its flag, never on call order", () => {
  // The tail emitter is registered globally on first use, so a mode that lived
  // in the emitter's identity rather than in the flag (or in what
  // `compileDecoder` is handed) would make an operation compile differently
  // depending on whether some other operation had been called first.
  const before = S.parseAsPromiseOrReject(S.schema({ id: S.string })).toString();
  S.parseAsResult(S.schema({ other: S.string }), { other: "x" });
  S.isInput(S.schema({ third: S.string }), {});
  const after = S.parseAsPromiseOrReject(S.schema({ id: S.string })).toString();
  expect(after).toBe(before);
  expect(before).toContain("Promise.reject");
});

test("make hands back the value it was given, even when the body rebinds it", () => {
  // A union rebinds the operation's parameter while dispatching, so `return i`
  // would answer with the encoded form rather than the value handed in.
  const toNumber = S.string.with(S.to, S.number, { decode: Number, encode: String });
  const union = S.union([toNumber, S.boolean]);
  expect(S.makeOutputOrThrow(union, 5)).toBe(5);
  expect(S.makeOutputAsResult(union, 5)).toEqual({ success: true, value: 5, error: undefined });
  expect(S.makeOutputOrThrow(union).toString()).toMatchInlineSnapshot(
    `"i=>{let v1=i;for(;;){if(typeof i==="number"&&i==i){let v0;try{v0=e[0](i)}catch(x){e[1](x)}typeof v0==="string"||e[2](v0);i=v0;break}if(typeof i==="boolean")break;e[3](i)}return v1}"`,
  );
  // Nothing to run means nothing can rebind, so the extra binding isn't there
  // and the operation still reads as the identity.
  expect(S.makeInputOrThrow(S.unknown)).toBe(S.parseOrThrow(S.unknown));
});

test("every verb takes the promisable Result, and answers in the schema's shape", async () => {
  const asyncSchema = S.string.with(S.to, S.number, {
    decode: { async: async (v: string) => Number(v) },
    encode: String,
  });
  const sync: Array<S.Result<unknown> | Promise<S.Result<unknown>>> = [
    S.parseAsPromisableResult(user, { id: "a" }),
    S.decodeAsPromisableResult(user, { id: "a" }),
    S.encodeAsPromisableResult(user, { id: "a" }),
    S.makeInputAsPromisableResult(user, { id: "a" }),
    S.makeOutputAsPromisableResult(user, { id: "a" }),
  ];
  for (const answer of sync) {
    expect(answer).not.toBeInstanceOf(Promise);
    expect(answer).toEqual({ success: true, value: { id: "a" }, error: undefined });
  }
  const asyncAnswers = [
    S.parseAsPromisableResult(asyncSchema, "1"),
    S.decodeAsPromisableResult(asyncSchema, "1"),
  ];
  for (const answer of asyncAnswers) {
    expect(answer).toBeInstanceOf(Promise);
    expect(await answer).toEqual({ success: true, value: 1, error: undefined });
  }
  // There is no promisable throwing variant, in either language.
  expect("parseAsPromisableOrThrow" in S).toBe(false);
});
