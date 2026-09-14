import { test, expectTypeOf, assertType } from "vitest";
import { format, inspect } from "node:util";

import * as S from "../index.mjs";

// `S.safe` is gone: a Result is what an operation returns, not something you
// wrap a call in. What is left here is catching a DEFECT - a schema built or
// wired wrong - which the surface deliberately does not turn into a value.
const caught = (fn: () => unknown): S.Error | undefined => {
  try {
    fn();
    return undefined;
  } catch (exn) {
    if (exn instanceof S.Error) return exn;
    throw exn;
  }
};

// FIXME: Move the test to e2e
// import { stringSchema } from "../genType/GenType.gen.js";

// FIXME: This is fails
// S.parseOrThrow(
//   S.union([
//     "bar",
//     "bas",
//     S.string.with(S.to, S.schema("unknown").with(S.noValidation, true)),
//   ])
// )

// Exact (bidirectional) type equality. expect-type's `toEqualTypeOf` can't be
// wrapped in a generic helper and still fire at call sites, so the dual
// Input+Output check is enforced via a required-argument constraint instead.
type Equal<TLeft, TRight> =
  (<T>() => T extends TLeft ? 1 : 2) extends <T>() => T extends TRight ? 1 : 2
    ? true
    : false;

const expectSchemaType = <TSchema extends S.Schema<unknown, unknown>>(
  _schema: TSchema,
) => ({
  toBe: <TInput, TOutput = TInput>(
    ..._mismatch: Equal<S.Input<TSchema>, TInput> extends true
      ? Equal<S.Output<TSchema>, TOutput> extends true
        ? []
        : [output: S.Output<TSchema>]
      : [input: S.Input<TSchema>]
  ) => {},
});

// Can use genType schema
// expectSchemaType(stringSchema).toBe<unknown, string>();

// The spec format has no pipeline operation (`operations` is parse/decode/encode
// on one schema), so the multi-argument entry points are tested here - see
// CONTRIBUTING.md's Spec Harness Suggestions.
// Bit 8 ("the source is `S.unknown`") rides through the `& 127` mask that keeps
// a nested recursive compile from inheriting the outer operation's return mode.
// These are the shapes that mask exists for; they must stay unaffected by it.
test("A return mode does not leak into a nested compile", (t) => {
  t.expect(S.isInput(S.json)(function () {})).toBe(false);
  t.expect(S.isInput(S.json)({ a: [1, "x"] })).toBe(true);
  t.expect(S.parseAsResult(S.json, function () {}).success).toBe(false);

  const Node = S.recursive("MaskNode", (self) =>
    S.schema({ id: S.string, kids: S.array(self) })
  );
  const good = { id: "a", kids: [{ id: "b", kids: [] }] };
  const bad = { id: 1, kids: [] };

  t.expect(S.isInput(Node)(good)).toBe(true);
  t.expect(S.isInput(Node)(bad)).toBe(false);
  t.expect(S.parseAsResult(Node, good).value).toEqual(good);
  t.expect(S.parseAsResult(Node, bad).success).toBe(false);
  t.expect(S.makeOutputOrThrow(Node, good)).toBe(good);
  t.expect(() => S.assertInputOrThrow(Node, bad)).toThrow();
});

// `docs/js-usage.md`: encoding is decoding the reversed schema. Nothing pinned
// that, and it is the invariant any change to how a link's direction is decided
// has to preserve - so it is the gate for moving the content axis's reading
// rules around. Verified to hold across all 434 spec schemas when written
// (395 identical codegen, 39 rejected identically, 0 asymmetric); these are the
// shapes where reversal does real work.
// Several structural decisions turn on "is this schema the whole JSON document
// rather than a rendering of one". That question used to be asked by comparing
// `name` to "JSON", which user metadata can forge.
test("Renaming a schema JSON does not change how it converts", (t) => {
  const renamed = S.meta(S.jsonString, { name: "JSON" });

  // Both are a carrier meeting a format with a different payload, so both are
  // rule 4's ambiguous pair. Only the rendered name may differ.
  const real = (): unknown => S.parseOrThrow(S.to(S.base64, S.jsonString));
  const forged = (): unknown => S.parseOrThrow(S.to(S.base64, renamed));
  t.expect(real).toThrow("Ambiguous base64 -> JSON string.");
  t.expect(forged).toThrow("Ambiguous base64 -> JSON.");

  // `S.json` genuinely has no opened form, so its pair says so instead - the
  // branch the forged name used to reach.
  t.expect(() => S.parseOrThrow(S.to(S.uint8Array, S.json))).toThrow(
    "Can't decode Uint8Array -> JSON"
  );

  // `S.recursive` builds `$ref` from its argument, so that spelling is forgeable
  // too; it must stay an ordinary recursive schema.
  const Forged = S.recursive("JSON", (self) => S.schema({ a: S.optional(self) }));
  t.expect(S.parseOrThrow(Forged)({ a: { a: undefined } })).toEqual({ a: { a: undefined } });

  // The marker rides copies, which is why identity could not replace the name:
  // a chain node that IS json is a copy of it.
  t.expect(S.parseOrThrow(S.json.with(S.to, S.string))("x")).toBe("x");
  t.expect(
    S.parseOrThrow(S.jsonString.with(S.to, S.schema({ foo: S.optional(S.string) })))("{}")
  ).toEqual({});
});

test("Encoding is decoding the reversed schema", (t) => {
  const shapes: Record<string, S.Schema<unknown, unknown>> = {
    codec: S.string.with(S.to, S.number),
    "codec with coders": S.string.with(S.to, S.number, { decode: Number, encode: String }),
    "object with a codec field": S.schema({ n: S.number.with(S.to, S.string) }),
    "array of codecs": S.array(S.string.with(S.to, S.number)),
    "refined codec": S.string.with(S.to, S.number).with(S.gt, 0),
    "codec then codec": S.string.with(S.to, S.number).with(S.to, S.string),
    union: S.union([S.string.with(S.to, S.number), S.boolean]),
    optional: S.optional(S.string.with(S.to, S.number)),
    "content payload": S.base64.with(S.to, S.jsonString.with(S.to, S.string)),
    "content reading": S.to(S.base64, S.jsonString, "unpack"),
    "bytes transfer": S.base64.with(S.to, S.uint8Array),
    "json document": S.jsonString.with(S.to, S.schema({ id: S.string })),
    "unsupported pair": S.boolean.with(S.to, S.number),
    "ambiguous pair": S.base64.with(S.to, S.jsonString),
  };
  for (const [name, schema] of Object.entries(shapes)) {
    const encode = (): string => String(S.encodeOrThrow(schema));
    const decodeReversed = (): string => String(S.decodeOrThrow(S.reverse(schema)));
    let encoded: string | undefined, encodeError: string | undefined;
    try { encoded = encode(); } catch (e) { encodeError = (e as Error).message; }
    let decoded: string | undefined, decodeError: string | undefined;
    try { decoded = decodeReversed(); } catch (e) { decodeError = (e as Error).message; }
    t.expect({ [name]: encoded, error: encodeError }).toEqual({ [name]: decoded, error: decodeError });
  }
});

test("A parse and a decode of one schema stay separate compiled operations", (t) => {
  const schema = S.schema({ id: S.string });

  t.expect(S.parseOrThrow(schema)).toBe(S.parseOrThrow(schema));
  t.expect(S.decodeOrThrow(schema)).toBe(S.decodeOrThrow(schema));
  t.expect(S.parseOrThrow(schema)).not.toBe(S.decodeOrThrow(schema));

  t.expect(() => S.parseOrThrow(schema)({ id: 1 })).toThrow();
  t.expect(S.decodeOrThrow(schema)({ id: 1 } as never)).toEqual({ id: 1 });
});

test("A slotless S.to is shared, so an inline pipeline compiles once", (t) => {
  const item = S.schema({ id: S.string });

  // Identity is the whole point: a fresh chain per call is also a fresh
  // operation-cache target, so an inline pipeline used to recompile on every
  // call (16.2us against 68ns hoisted).
  t.expect(S.to(item, S.unknown)).toBe(S.to(item, S.unknown));
  t.expect(item.with(S.to, S.jsonString)).toBe(item.with(S.to, S.jsonString));
  t.expect(S.parseOrThrow(S.jsonString.with(S.to, item))).toBe(
    S.parseOrThrow(S.jsonString.with(S.to, item))
  );

  // Direction is part of the key - both orders store on the same schema when
  // one of them is the newer of the pair.
  t.expect(S.to(item, S.unknown)).not.toBe(S.to(S.unknown, item));

  t.expect(S.parseOrThrow(S.jsonString.with(S.to, item))('{"id":"a"}')).toEqual({ id: "a" });
});

test("A reading joins the link key; a coder opts out", (t) => {
  const nodes = (schema: unknown) => {
    let n = 0;
    let node = (schema as { l?: { n?: unknown } }).l;
    while (node) (n++, (node = node.n as { n?: unknown } | undefined));
    return n;
  };
  const make = () => S.string.with(S.to, S.number, { decode: Number, encode: String });

  // A reading is a string primitive, so it is bounded by construction: at most
  // three entries per pair, and the two readings do not collide.
  t.expect(S.to(S.base64, S.jsonString, "unpack")).toBe(
    S.to(S.base64, S.jsonString, "unpack")
  );
  t.expect(S.to(S.base64, S.jsonString, "unpack")).not.toBe(
    S.to(S.base64, S.jsonString, "pack")
  );

  // A coder is a fresh object every call, so it must NOT join the key - keying
  // on it would miss every time and add a node it can never hit again, on a
  // pair of singletons that never dies.
  t.expect(make()).not.toBe(make());
  for (let i = 0; i < 20; i++) S.string.with(S.to, S.number, { decode: Number, encode: String });
  t.expect(nodes(S.number)).toBe(0);

  // `S.trim` reshapes its own result after building it, so it stays unshared
  // for a second reason - and the content marker it stamps onto its tail must
  // not reach the shared `string` singleton. `content` is internal, hence the cast.
  t.expect(S.base64.with(S.trim)).not.toBe(S.base64.with(S.trim));
  t.expect((S.string as unknown as { content?: unknown }).content).toBe(undefined);
});

test("The link cache lands on the argument that dies first", (t) => {
  const nodes = (schema: unknown) => {
    let n = 0;
    let node = (schema as { l?: { n?: unknown } }).l;
    while (node) (n++, (node = node.n as { n?: unknown } | undefined));
    return n;
  };
  // Both long-lived schemas are this test's own: asserting a node count on a
  // shared singleton couples the result to whatever else the suite linked to it.
  const longLived = S.schema({ id: S.string });
  const longLivedTarget = S.schema({ tag: S.string });

  // `seq` is monotonic, so a fresh partner is always the newer of the pair and
  // takes the node with it. Neither a long-lived source nor a long-lived target
  // accumulates anything, which is what makes an unbounded cache safe.
  for (let i = 0; i < 20; i++) S.to(longLived, S.schema({ n: S.literal(i) }));
  for (let i = 0; i < 20; i++) S.to(S.schema({ n: S.literal(i) }), longLivedTarget);
  t.expect(nodes(longLived)).toBe(0);
  t.expect(nodes(longLivedTarget)).toBe(0);

  // A repeated pair is one node, however many times it is asked for.
  for (let i = 0; i < 20; i++) S.to(longLived, S.unknown);
  t.expect(nodes(longLived)).toBe(1);

  // Invisible to everything that walks a schema: `copySchema`'s Object.assign,
  // `unionIsTransparent`'s field count, and JSON.stringify of an error.
  t.expect(Object.keys(longLived)).not.toContain("l");
  t.expect("l" in S.meta(longLived, { title: "t" })).toBe(false);
  t.expect(typeof JSON.stringify(S.to(longLived, S.unknown))).toBe("string");
});

test("Deriving from a shared link leaves the shared instance alone", (t) => {
  const item = S.schema({ id: S.string });
  const shared = item.with(S.to, S.unknown);

  const described = shared.with(S.meta, { description: "d" });
  t.expect(described).not.toBe(shared);
  t.expect(shared.description).toBe(undefined);
  t.expect(item.with(S.to, S.unknown).description).toBe(undefined);

  // The reverse is cached on the shared instance, which is a second win, not a
  // leak: it is derived from the same two arguments.
  t.expect(S.reverse(shared)).toBe(S.reverse(item.with(S.to, S.unknown)));
});

test("S.to returns the schema itself when the target is the same instance", (t) => {
  const make = () => S.string.with(S.to, S.number, (string) => string.length);
  const schema = make();

  t.expect(S.to(schema, schema)).toBe(schema);
  t.expect(schema.with(S.to, schema)).toBe(schema);
  t.expect(S.parseOrThrow(schema.with(S.to, schema))("hello")).toBe(5);

  // Without the shortcut this appends a second copy of the chain, so the
  // decoder runs twice over its own output - silently wrong, not an error.
  t.expect(S.parseOrThrow(S.to(schema, make()))("hello")).toBe(1);

  // Custom coders still mean a real conversion step, same instance or not.
  const doubled = S.to(schema, schema, (n) => String(n * 2));
  t.expect(doubled).not.toBe(schema);
  t.expect(S.parseOrThrow(doubled)("hello")).toBe(2);

  expectSchemaType(schema).toBe<string, number>();
  expectSchemaType(S.to(schema, schema)).toBe<string, number>();
});

test("Function literal schema", (t) => {
  const fn = function () {};

  const schema = S.schema(fn);

  expectSchemaType(schema).toBe<() => void, () => void>();
  if (schema.type !== "function") {
    t.expect.fail("Schema should be a function");
    return;
  }
  t.expect(schema.const).toBe(fn);

  const value = S.parseOrThrow(schema)(fn);

  t.expect(value).toEqual(fn);
  t.expect(value).not.toEqual(function () {});
});

test("Successfully parses float when NaN is provided and NaN check disabled in global config", (t) => {
  S.global({
    disableNanNumberValidation: true,
  });
  const schema = S.number;
  const value = S.parseOrThrow(schema)(NaN);
  S.global({});

  t.expect(value).toEqual(NaN);

  expectSchemaType(schema).toBe<number, number>();
  expectTypeOf(value).toEqualTypeOf<number>();
});

test("Can get a reason from an error", (t) => {
  const schema = S.never;

  const result = S.parseAsResult(schema, true);

  if (result.success) {
    t.expect.fail("Should fail");
    return;
  }
  t.expect(result.error.reason).toBe("Expected never, received true");
});

test("Parse JSON string to object with bigint and back", (t) => {
  const messageSchema = S.schema({
    type: "info",
    value: S.bigint,
  });

  const decode = S.decodeOrThrow(S.jsonString, messageSchema);
  const encode = S.decodeOrThrow(
    messageSchema,
    // Cast to string to disable json string encoder
    S.jsonString.with(S.to, S.string, (string) => string),
    S.uint8Array,
  );

  t.expect(decode(`{"type": "info", "value": "123"}`)).toEqual({
    type: "info",
    value: 123n,
  });
  t.expect(encode({ type: "info", value: 123n })).toEqual(
    new Uint8Array([
      123, 34, 116, 121, 112, 101, 34, 58, 34, 105, 110, 102, 111, 34, 44, 34,
      118, 97, 108, 117, 101, 34, 58, 34, 49, 50, 51, 34, 125,
    ]),
  );
});

test("Optional enum", (t) => {
  const statuses = S.union(["Win", "Draw", "Loss"]);
  const schema = S.optional(statuses);

  t.expect(S.parseOrThrow(schema)("Win")).toEqual("Win");
  t.expect(S.parseOrThrow(schema)(undefined)).toEqual(undefined);

  expectTypeOf(schema).toEqualTypeOf<
    S.Schema<
      "Win" | "Draw" | "Loss" | undefined,
      "Win" | "Draw" | "Loss" | undefined
    >
  >();

  const inlineOptional = S.optional(S.union(["Win", "Draw", "Loss"]));
  t.expect(S.parseOrThrow(inlineOptional)("Win")).toEqual("Win");
  t.expect(S.encodeOrThrow(inlineOptional)("Win")).toEqual("Win");
  expectTypeOf(inlineOptional).toEqualTypeOf<
    S.Schema<
      "Win" | "Draw" | "Loss" | undefined,
      "Win" | "Draw" | "Loss" | undefined
    >
  >();

  const inlineNullable = S.nullable(S.union(["Win", "Draw", "Loss"]));
  t.expect(S.parseOrThrow(inlineNullable)("Win")).toEqual("Win");
  t.expect(S.encodeOrThrow(inlineNullable)("Win")).toEqual("Win");
  expectTypeOf(inlineNullable).toEqualTypeOf<
    S.Schema<"Win" | "Draw" | "Loss" | null, "Win" | "Draw" | "Loss" | null>
  >();

  const inlineNullish = S.nullish(S.union(["Win", "Draw", "Loss"]));
  t.expect(S.parseOrThrow(inlineNullish)("Win")).toEqual("Win");
  t.expect(S.encodeOrThrow(inlineNullish)("Win")).toEqual("Win");
  expectTypeOf(inlineNullish).toEqualTypeOf<
    S.Schema<
      "Win" | "Draw" | "Loss" | null | undefined,
      "Win" | "Draw" | "Loss" | null | undefined
    >
  >();

  const inlineArray = S.array(S.union(["Win", "Draw", "Loss"]));
  t.expect(S.parseOrThrow(inlineArray)(["Win", "Loss"])).toEqual(["Win", "Loss"]);
  t.expect(S.encodeOrThrow(inlineArray)(["Win", "Loss"])).toEqual(["Win", "Loss"]);
  expectTypeOf(inlineArray).toEqualTypeOf<
    S.Schema<("Win" | "Draw" | "Loss")[], ("Win" | "Draw" | "Loss")[]>
  >();

  const inlineRecord = S.record(S.union(["Win", "Draw", "Loss"]));
  t.expect(S.parseOrThrow(inlineRecord)({ a: "Win" })).toEqual({ a: "Win" });
  t.expect(S.encodeOrThrow(inlineRecord)({ a: "Win" })).toEqual({ a: "Win" });
  expectTypeOf(inlineRecord).toEqualTypeOf<
    S.Schema<
      Record<string, "Win" | "Draw" | "Loss">,
      Record<string, "Win" | "Draw" | "Loss">
    >
  >();

  const inlineObject = S.schema({
    status: S.union(["Win", "Draw", "Loss"]),
  });
  t.expect(S.parseOrThrow(inlineObject)({ status: "Win" })).toEqual({
    status: "Win",
  });
  t.expect(S.encodeOrThrow(inlineObject)({ status: "Win" })).toEqual({
    status: "Win",
  });
  expectTypeOf(inlineObject).toEqualTypeOf<
    S.Schema<
      { status: "Win" | "Draw" | "Loss" },
      { status: "Win" | "Draw" | "Loss" }
    >
  >();

  const inlineTuple = S.schema([S.union(["Win", "Draw", "Loss"]), S.number]);
  t.expect(S.parseOrThrow(inlineTuple)(["Win", 1])).toEqual(["Win", 1]);
  t.expect(S.encodeOrThrow(inlineTuple)(["Win", 1])).toEqual(["Win", 1]);
  expectTypeOf(inlineTuple).toEqualTypeOf<
    S.Schema<
      ["Win" | "Draw" | "Loss", number],
      ["Win" | "Draw" | "Loss", number]
    >
  >();

  const nestedDeep = S.optional(
    S.array(S.nullable(S.union(["Win", "Draw", "Loss"]))),
  );
  t.expect(S.parseOrThrow(nestedDeep)(["Win", null])).toEqual(["Win", null]);
  t.expect(S.encodeOrThrow(nestedDeep)(["Win", null])).toEqual(["Win", null]);
  expectTypeOf(nestedDeep).toEqualTypeOf<
    S.Schema<
      ("Win" | "Draw" | "Loss" | null)[] | undefined,
      ("Win" | "Draw" | "Loss" | null)[] | undefined
    >
  >();
});

test("Successfully parses nullable string with dynamic default", (t) => {
  const schema = S.nullable(S.string, () => "bar");
  const value1 = S.parseOrThrow(schema)("foo");
  const value2 = S.parseOrThrow(schema)(null);

  t.expect(value1).toEqual("foo");
  t.expect(value2).toEqual("bar");

  expectTypeOf(schema).toEqualTypeOf<S.Schema<string | null, string>>();
  expectTypeOf(value1).toEqualTypeOf<string>();
});

test("Pattern match on schema", (t) => {
  const schema = S.int32;

  if (schema.type === "number") {
    t.expect(schema.format).toBe("int32");
  } else {
    t.expect.fail("Not a schema");
  }
});

test("Test extended JSON Schema", (t) => {
  const schema = S.int32
    .with(S.extendJSONSchema, {
      $ref: "Foo",
    })
    .with(S.extendJSONSchema, {
      readOnly: true,
    });

  t.expect(S.toInputJSONSchemaOrThrow(schema)).toEqual({
    $ref: "Foo",
    readOnly: true,
    type: "integer",
    minimum: -2147483648,
    maximum: 2147483647,
  });
});

test("toInputJSONSchemaOrThrow omits default additionalProperties schemas", (t) => {
  const expected = {
    type: "object",
    properties: { value: { type: "string" } },
  };
  const input = { value: "ok", extra: { nested: true } };

  for (const additionalProperties of [true, {}] as const) {
    const schema = S.fromJSONSchemaOrThrow({
      type: "object",
      properties: { value: { type: "string" } },
      additionalProperties,
    });
    t.expect(S.parseOrThrow(schema)(input)).toBe(input);
    t.expect(S.toInputJSONSchemaOrThrow(schema)).toEqual(expected);
  }

  const referencedAny = S.fromJSONSchemaOrThrow({
    type: "object",
    additionalProperties: { $ref: "#/$defs/any" },
    $defs: { any: {} },
  });
  t.expect(S.parseOrThrow(referencedAny)(input)).toBe(input);
  t.expect(S.toInputJSONSchemaOrThrow(referencedAny)).toEqual({ type: "object" });
  t.expect(S.toInputJSONSchemaOrThrow(S.record(S.json))).toEqual({ type: "object" });
});

test("S.encodeAsPromiseOrReject runs an async encode codec", async (t) => {
  const schema = S.string.with(S.to, S.number, {
    decode: (string) => string.length,
    encode: { async: (number) => Promise.resolve("x".repeat(number)) },
  });

  // The forward direction stays sync-parseable; async-ness is discovered by
  // catching the sync operation's rejection, not via a dedicated probe.
  t.expect(S.parseOrThrow(schema)("abc")).toBe(3);
  t.expect(() => S.encodeOrThrow(schema)).toThrow(
    "Invalid async during sync operation",
  );
  await t.expect(S.encodeAsPromiseOrReject(schema)(3)).resolves.toBe("xxx");
});

test("All-auto codecs behave exactly like the coder-less spelling", (t) => {
  const schema = S.string.with(S.to, S.number, (string) => string.length);

  // Same-instance target: the self-chain shortcut must apply to the
  // all-"auto" object too, not only to the omitted argument.
  t.expect(S.to(schema, schema, { decode: "auto", encode: "auto" })).toBe(schema);
});

test("Rejects unknown codec slot values at schema creation", (t) => {
  const codec = (codecs: any) => () => S.string.with(S.to, S.number, codecs);

  // The rejection names the direction the caller got wrong, not the pair.
  t.expect(codec({ decode: 1, encode: "auto" })).toThrow(
    '[Sury] Invalid decode 1. Expected a function, "auto", "never", "pack", "unpack" or {async: fn}',
  );
  t.expect(codec({ decode: "auto", encode: 1 })).toThrow("[Sury] Invalid encode 1.");
  // {async} is strict: extra keys are a misuse, not something to guess about.
  t.expect(
    codec({ decode: { async: async (v: string) => v.length, sync: 1 }, encode: "auto" }),
  ).toThrow("[Sury] Invalid decode");
  // A missing (or nulled) direction reads as the incomplete pair it is.
  t.expect(codec({ decode: null, encode: "auto" })).toThrow(
    '[Sury] Expected {decode, encode}. Use "auto" for the built-in conversion',
  );
});

test("Custom codecs type their coders against the junction, and stay assignable", (t) => {
  const schema = S.string.with(S.to, S.number.with(S.gt, 0), {
    decode: (value) => {
      expectTypeOf(value).toEqualTypeOf<string>();
      return value.length;
    },
    // `encode` receives the target's *input*, not its output - the coder sits
    // at the junction, so its result runs through the target's own pipeline.
    encode: (value) => {
      expectTypeOf(value).toEqualTypeOf<number>();
      return "x".repeat(value);
    },
  });
  expectTypeOf(schema).toEqualTypeOf<S.Schema<string, number>>();

  // The Coder bivariance hack: without it every `with` mention of Codecs makes
  // TOutput compare contravariantly, and a concrete schema stops being usable
  // where the erased one is expected.
  const erased: S.Schema<unknown, unknown> = schema;
  t.expect(S.parseOrThrow(erased)("abc")).toBe(3);
});

test("An output-seam codec rejects a target that already converts", (t) => {
  const target = S.string.with(S.to, S.number);

  // Only the ReScript adapter can reach this seam, and only it is restricted:
  // the JS pair feeds the target's chain instead of replacing it.
  t.expect(() =>
    (S.to as any)(S.string, target, {
      decodeToOutput: (value: string) => value.length,
      encodeFromOutput: (value: number) => String(value),
    }),
  ).toThrow("[Sury] The target already converts. Chain S.to instead of passing a custom codec");

  t.expect(
    S.parseOrThrow(S.string.with(S.to, target, { decode: (v) => v, encode: (v) => v }))("42"),
  ).toBe(42);
});

test("JS refine produces invalid_input error with expected/received populated", (t) => {
  const schema = S.string.with(S.refine, () => false, { error: "nope" });
  const result = S.parseAsResult(schema, "123");
  if (result.success) {
    t.expect.fail("Should have thrown");
    return;
  }
  t.expect(result.error.code).toBe("invalid_input");
  t.expect(result.error.reason).toBe("nope");
  if (result.error.code === "invalid_input") {
    t.expect(result.error.expected.type).toBe("string");
    t.expect(result.error.received.type).toBe("string");
  }
});

test("Successfully parses async schema", async (t) => {
  const schema = S.string.with(S.to, S.string, {
    decode: {
      async: async (string) => {
        expectTypeOf(string).toEqualTypeOf<string>();
        return string;
      },
    },
    encode: "auto",
  });
  const value = await S.parseAsResultPromise(schema, "123");

  t.expect(value).toEqual({ success: true, value: "123" });

  expectTypeOf(value).toEqualTypeOf<S.Result<string>>();
});

test("Fails to parses async schema", async (t) => {
  const schema = S.string.with(S.to, S.string, {
    decode: {
      async: async (): Promise<string> => {
        throw new Error("User error");
      },
    },
    encode: "auto",
  });

  const result = await S.parseAsResultPromise(schema, "123");

  if (result.success) {
    t.expect.fail("Should fail");
    return;
  }
  t.expect(result.error.message).toBe("User error");
  t.expect(result.error instanceof S.Error).toBe(true);

  expectTypeOf(result.error.code).toEqualTypeOf<
    | "invalid_input"
    | "invalid_conversion"
    | "unrecognized_key"
  >();

  t.expect(result.error.code).toBe("invalid_conversion");
});

test("Fails to parse strict object with exccess fields which created using global config override", (t) => {
  S.global({
    defaultAdditionalItems: "strict",
  });
  const schema = S.schema({
    foo: S.string,
  });
  // Reset global config back
  S.global({});

  t.expect(() => {
    const value = S.parseOrThrow(schema)({
      foo: "bar",
      bar: true,
    });
    expectTypeOf(schema).toEqualTypeOf<
      S.Schema<{ foo: string }, { foo: string }>
    >();
    expectTypeOf(value).toEqualTypeOf<{ foo: string }>();
  }).toThrow(
    t.expect.objectContaining({
      name: "SuryError",
      message: `Unrecognized key "bar"`,
    }),
  );
});

test("Fails to parse intersected objects with transform", (t) => {
  t.expect(() => {
    const schema = S.merge(
      S.schema({
        foo: S.string,
        bar: S.boolean,
      }).with(S.shape, (obj) => ({
        abc: obj.foo,
      })),
      S.schema({
        baz: S.string,
      }),
    );
  }).toThrow(
    t.expect.objectContaining({
      name: "Error",
      message: `[Sury] Can't merge transformed { foo: string; bar: boolean; }`,
    }),
  );

  // expectSchemaType(schema).toBe<
  //   Record<string, unknown>,
  //   {
  //     abc: string;
  //     baz: string;
  //   }
  // >();

  // const result = S.parseAsResult(
  //   S.parseOrThrow(
  //     {
  //       foo: "bar",
  //       bar: true,
  //     },
  //     schema
  //   )
  // );
  // if (result.success) {
  //   t.fail("Should fail");
  //   return;
  // }
  // t.is(
  //   result.error.message,
  //   `Failed at baz: Expected string, received undefined`
  // );

  // const value = S.parseOrThrow(
  //   {
  //     foo: "bar",
  //     baz: "baz",
  //     bar: true,
  //   },
  //   schema
  // );
  // t.deepEqual(value, {
  //   abc: "bar",
  //   baz: "baz",
  // });
});

test("Object with an S.never field is inferred as a required never property", (t) => {
  const schema = S.schema({
    key: S.string,
    oldKey: S.never,
  });

  // The field can never hold a value, so it stays a required `never` property:
  // the object is uninhabited, which is what you would write by hand.
  expectSchemaType(schema).toBe<{ key: string; oldKey: never }>();

  // ...and parsing always fails on the never field (see S_never_test.res).
  t.expect(() => S.parseOrThrow(schema)({ key: "value" })).toThrow(
    t.expect.objectContaining({
      name: "SuryError",
      message: `Failed at oldKey: Expected never, received undefined`,
    }),
  );
});

test("Object with an S.optional(S.never) field is inferred as optional undefined", (t) => {
  const schema = S.schema({
    key: S.string,
    oldKey: S.optional(S.never),
  });

  // The realistic deprecated-field pattern: optional collapses to `undefined`,
  // so the field is optional and the object stays inhabited.
  expectSchemaType(schema).toBe<{ key: string; oldKey?: undefined }>();

  const value = S.parseOrThrow(schema)({ key: "value" });
  t.expect(value).toEqual({ key: "value", oldKey: undefined });
});

test("S.name", (t) => {
  t.expect(S.toInputExpression(S.unknown.with(S.meta, { name: "BlaBla" }))).toBe(
    `BlaBla`,
  );
});

test("Successfully parses and returns result", (t) => {
  const schema = S.string;
  const value = S.parseAsResult(schema, "123");

  t.expect(value).toEqual({ success: true, value: "123" });

  expectTypeOf(value).toEqualTypeOf<S.Result<string>>();
  if (value.success) {
    expectTypeOf(value).toEqualTypeOf<{
      readonly success: true;
      readonly value: string;
      readonly error?: undefined;
    }>();
  } else {
    expectTypeOf(value).toEqualTypeOf<{
      readonly success: false;
      readonly error: S.DataError;
      readonly value?: undefined;
    }>();
  }
});

test("Successfully reverse converts and returns result", (t) => {
  const schema = S.string;
  const value = S.encodeAsResult(schema, "123");

  t.expect(value).toEqual({ success: true, value: "123" });

  if (value.success) {
    expectTypeOf(value).toEqualTypeOf<{
      readonly success: true;
      readonly value: string;
      readonly error?: undefined;
    }>();
  } else {
    expectTypeOf(value).toEqualTypeOf<{
      readonly success: false;
      readonly error: S.DataError;
      readonly value?: undefined;
    }>();
  }
});

test("Successfully parses undefined using the default value for transformed schema", (t) => {
  // FIXME: Test that it works correctly:
  // const schema = S.boolean.with(S.optional, false).with(S.to, S.string);
  const schema = S.boolean.with(S.to, S.string).with(S.optional, "false");

  const value = S.parseOrThrow(schema)(undefined);

  t.expect(value).toEqual("false");
  t.expect(schema.default).toEqual(false);

  expectTypeOf(schema.default).toEqualTypeOf<boolean | undefined>();
  expectSchemaType(schema).toBe<boolean | undefined, string>();
});

test("Successfully parses undefined using the default value from callback", (t) => {
  const schema = S.string.with(S.optional, () => "foo");

  const value = S.parseOrThrow(schema)(undefined);

  t.expect(value).toEqual("foo");
  t.expect(schema.default).toEqual(undefined);

  expectSchemaType(schema).toBe<string | undefined, string>();
});

test("Creates schema with description and title", (t) => {
  const undocumentedStringSchema = S.string;

  expectTypeOf(undocumentedStringSchema).toEqualTypeOf<
    S.Schema<string, string>
  >();

  const documentedStringSchema = undocumentedStringSchema.with(S.meta, {
    title: "My schema",
    description: "A useful bit of text, if you know what to do with it.",
  });

  expectTypeOf(documentedStringSchema).toEqualTypeOf<
    S.Schema<string, string>
  >();

  expectTypeOf(documentedStringSchema.title).toEqualTypeOf<
    string | undefined
  >();
  expectTypeOf(documentedStringSchema.description).toEqualTypeOf<
    string | undefined
  >();

  t.expect(undocumentedStringSchema.description).toEqual(undefined);
  t.expect(documentedStringSchema.description).toEqual(
    "A useful bit of text, if you know what to do with it.",
  );
  t.expect(undocumentedStringSchema.title).toEqual(undefined);
  t.expect(documentedStringSchema.title).toEqual("My schema");
});

test("Creates schema with deprecation", (t) => {
  const schema = S.string;

  expectTypeOf(schema).toEqualTypeOf<S.Schema<string, string>>();

  const deprecatedStringSchema = schema.with(S.meta, {
    deprecated: true,
    description: "Use number instead.",
  });

  expectTypeOf(deprecatedStringSchema).toEqualTypeOf<
    S.Schema<string, string>
  >();

  expectTypeOf(deprecatedStringSchema.deprecated).toEqualTypeOf<
    boolean | undefined
  >();
  expectTypeOf(deprecatedStringSchema.description).toEqualTypeOf<
    string | undefined
  >();

  t.expect(schema.deprecated).toEqual(undefined);
  t.expect(deprecatedStringSchema.deprecated).toEqual(true);
  t.expect(deprecatedStringSchema.description).toEqual("Use number instead.");
});

test("Tuple types", (t) => {
  const emptyTuple = S.schema([]);
  expectSchemaType(emptyTuple).toBe<[]>();

  const tuple1WithLiteral = S.schema(["foo"]);
  expectSchemaType(tuple1WithLiteral).toBe<["foo"]>();

  const tuple1WithSchema = S.schema([S.string]);
  expectSchemaType(tuple1WithSchema).toBe<[string]>();

  const tuple1WithObject = S.schema([{ foo: S.string }]);
  expectSchemaType(tuple1WithObject).toBe<[{ foo: string }]>();

  const tuple2WithLiterals = S.schema(["foo", 123]);
  expectSchemaType(tuple2WithLiterals).toBe<["foo", 123]>();

  const tuple2WithSchemas = S.schema([S.string, S.boolean]);
  expectSchemaType(tuple2WithSchemas).toBe<[string, boolean]>();

  const tuple2LiteralAndSchema = S.schema(["foo", S.boolean]);
  expectSchemaType(tuple2LiteralAndSchema).toBe<["foo", boolean]>();

  const tuple2LiteralAsCosntAndSchema = S.schema(["foo" as const, S.boolean]);
  expectSchemaType(tuple2LiteralAsCosntAndSchema).toBe<["foo", boolean]>();

  const tuple2LiteralSchemaAndSchema = S.schema([S.schema("foo"), S.boolean]);
  expectSchemaType(tuple2LiteralSchemaAndSchema).toBe<["foo", boolean]>();
});

test("Standard schema", (t) => {
  const schema = S.nullable(S.string);

  t.expect(schema["~standard"]["vendor"]).toEqual("sury");
  t.expect(schema["~standard"]["version"]).toEqual(1);
  t.expect(schema["~standard"]["validate"](undefined)).toEqual({
    issues: [
      {
        message: "Expected string | null, received undefined",
        path: undefined,
      },
    ],
  });
  t.expect(schema["~standard"]["validate"]("foo")).toEqual({
    value: "foo",
  });
  t.expect(schema["~standard"]["validate"](null)).toEqual({
    value: null,
  });

  expectTypeOf<S.StandardSchemaV1.InferInput<typeof schema>>().toEqualTypeOf<
    string | null
  >();
  expectTypeOf<S.StandardSchemaV1.InferOutput<typeof schema>>().toEqualTypeOf<
    string | null
  >();
});

// getOp answers a repeated call from a per-operation node cache on the
// schema (see OpNode in parse.ts), and
// `~standard.validate` holds its compiled decoder in a closure. Both are keyed
// on the arguments and the global flag, so anything that picks a different
// compiled operation must still get it.
test("Compiled operations stay per-operation and per-global-config", (t) => {
  const schema = S.schema({
    a: S.string.with(S.to, S.number, { decode: Number, encode: String }),
  });

  // Alternating operations on one schema must not answer each other.
  for (let i = 0; i < 3; i++) {
    t.expect(S.parseOrThrow(schema)({ a: "1" })).toEqual({ a: 1 });
    t.expect(S.encodeOrThrow(schema)({ a: 1 })).toEqual({ a: "1" });
    t.expect(S.decodeOrThrow(schema)({ a: "3" })).toEqual({ a: 3 });
    t.expect(S.isInput(schema)({ a: "1" })).toBe(true);
    t.expect(S.isInput(schema)({ a: 1 })).toBe(false);
    t.expect(schema["~standard"].validate({ a: "2" })).toEqual({
      value: { a: 2 },
    });
  }

  // A derived schema must compile its own operation, not inherit the original's.
  t.expect(S.isInput(S.number)(NaN)).toBe(false);
  const derived = S.number.with(S.meta, { title: "t" });
  t.expect(S.parseOrThrow(derived)(1)).toBe(1);
  t.expect(S.isInput(derived)(NaN)).toBe(false);

  const standard = S.number["~standard"];
  const nanRejected = {
    issues: [{ message: "Expected number, received NaN", path: undefined }],
  };
  t.expect(standard.validate(NaN)).toEqual(nanRejected);
  try {
    S.global({ disableNanNumberValidation: true });
    t.expect(S.isInput(S.number)(NaN)).toBe(true);
    t.expect(standard.validate(NaN)).toEqual({ value: NaN });
  } finally {
    S.global({});
  }
  t.expect(S.isInput(S.number)(NaN)).toBe(false);
  t.expect(standard.validate(NaN)).toEqual(nanRejected);
});

// A conversion rejected at operation creation fails for every input, so it
// isn't a fact about the value being validated - it's a bug in the schema, and
// `issues` is the channel a consumer renders to the person filling in the
// form. It throws to the developer instead, and keeps throwing: `validate`
// holds its decoder across calls, and a compile that never produced one must
// not leave the cache claiming to be current.
test("A conversion rejected at operation creation throws from validate, on every call", (t) => {
  const standard = S.boolean.with(S.to, S.number)["~standard"];
  const message = "Can't decode boolean -> number. Define custom codec with S.to";
  for (let i = 0; i < 3; i++) {
    t.expect(() => standard.validate(true)).toThrow(message);
  }

  // Only the compile is promoted: an input that fails validation is still a
  // result, not an exception.
  const schema = S.schema({ id: S.string });
  t.expect(schema["~standard"].validate({ id: "a" })).toEqual({ value: { id: "a" } });
  t.expect(schema["~standard"].validate({ id: 1 })).toEqual({
    issues: [{ message: "Expected string, received 1", path: ["id"] }],
  });
});

// The Standard Schema contract lets `validate` answer with a promise, and an
// async codec has nothing else to answer with. Discovery mirrors
// `S.parseAsPromiseOrReject`: the sync compile rejects, the async one is cached instead.
test("~standard.validate returns a promise for a schema with an async codec", async (t) => {
  const schema = S.schema({
    id: S.string.with(S.to, S.number, {
      decode: { async: (string) => Promise.resolve(string.length) },
      encode: (number) => "x".repeat(number),
    }),
  });
  const standard = schema["~standard"];
  for (let i = 0; i < 2; i++) {
    const valid = standard.validate({ id: "abc" });
    t.expect(valid).toBeInstanceOf(Promise);
    await t.expect(valid).resolves.toEqual({ value: { id: 3 } });
    await t.expect(standard.validate({ id: 1 })).resolves.toEqual({
      issues: [{ message: "Expected string, received 1", path: ["id"] }],
    });
  }

  // A sync schema keeps answering synchronously - a consumer that can't
  // await must not start getting promises.
  t.expect(S.string["~standard"].validate("a")).toEqual({ value: "a" });

  // A conversion rejected at operation creation still throws, on the async
  // retry rather than being read as an async schema.
  t.expect(() => S.boolean.with(S.to, S.number)["~standard"].validate(true)).toThrow(
    "Can't decode boolean -> number. Define custom codec with S.to"
  );
});

// A foreign exception (not a Sury error) keeps going up from `~standard.validate`
// of a sync schema, and keeps doing so after a Result operation has installed
// the operation tail emitter - which must not wrap the Standard Schema body in
// a second `try` that turns the rethrow into a rejection. A test file rather
// than a spec: what's under test is an order dependency between two
// operations, which a per-schema golden can't express.
// A foreign exception - a getter here - is a failure of THIS value, so it is
// an issue rather than a throw, and the same issue whichever operation was
// compiled first (the Result emitter is registered on first use).
test("~standard.validate answers a foreign exception as an issue regardless of which operation compiled first", (t) => {
  const foreign = new Proxy({}, { get() { throw new Error("foreign"); } });
  t.expect(S.schema({ id: S.string })["~standard"].validate(foreign)).toEqual({
    issues: [{ message: "foreign", path: undefined }],
  });
  S.parseAsResult(S.string, "a");
  t.expect(S.schema({ id: S.string, n: S.number })["~standard"].validate(foreign)).toEqual({
    issues: [{ message: "foreign", path: undefined }],
  });
});

// A symbol is a valid `PropertyKey` segment of a Standard Schema issue path,
// so one written into a refine's `path` reaches the consumer as is.
test("~standard.validate forwards a symbol path segment", (t) => {
  const tag = Symbol("tag");
  const schema = S.string.with(S.refine, () => false, { error: "User error", path: [tag] });
  t.expect(schema["~standard"].validate("a")).toEqual({
    issues: [{ message: "User error", path: [tag] }],
  });
  t.expect(S.pathToText([tag, "a"])).toBe("[Symbol(tag)].a");
});

// `S.isInput` makes the same split, for the same reason: `false` is an answer about
// the value, and a schema with no compilable operation has no answer to give.
test("A conversion rejected at operation creation throws from S.isInput, rather than reading as false", (t) => {
  const rejected = S.boolean.with(S.to, S.number);
  t.expect(() => S.isInput(rejected)).toThrow(
    "Can't decode boolean -> number. Define custom codec with S.to"
  );

  const isValid = S.isInput(S.schema({ id: S.string }));
  t.expect(isValid({ id: "a" })).toBe(true);
  t.expect(isValid({ id: 1 })).toBe(false);
  t.expect(isValid(null)).toBe(false);
  t.expect(isValid(undefined)).toBe(false);

  // A refinement that throws is that refinement failing (the same as a coder's
  // throw), so the value is not valid - and a schema wired wrong still throws.
  const boom = S.string.with(S.refine, () => {
    throw new RangeError("boom");
  });
  t.expect(S.isInput(boom)("x")).toBe(false);
  t.expect(() => S.parseOrThrow(boom, "x")).toThrow("RangeError: boom");
});

// A recursive def marks itself in-progress in the operation cache before
// compiling (OpNode `v === 0`, parse.ts). A compile that throws must unlink
// that node: left behind, a retry reads it as a live circular reference and
// builds an operation that calls 0 at runtime.
test("A failed recursive compile reports the same error on retry, not a poisoned cache node", (t) => {
  // Nested rather than top-level: a top-level call derives a fresh input
  // schema per compile, so only the nested shape keeps the def-to-def cache
  // triple stable enough for a retry to find the leftover node.
  const schema = S.schema({
    node: S.recursive<{ bad: boolean }, { bad: number }>("BrokenRec", (_) =>
      S.schema({ bad: S.boolean.with(S.to, S.number) }),
    ),
  });
  const message = "Failed at bad: Can't decode boolean -> number. Define custom codec with S.to";
  t.expect(() => S.parseOrThrow(schema)).toThrow(message);
  t.expect(() => S.parseOrThrow(schema)({ node: { bad: true } })).toThrow(message);
});

test("Standard JSON Schema interface support", (t) => {
  const schema = S.schema({ foo: S.to(S.string, S.number) });
  const standard = schema["~standard"];

  // Throws until enableStandardJSONSchema is called.
  t.expect(() => standard.jsonSchema.input({ target: "draft-07" })).toThrow(
    "~standard.jsonSchema requires S.enableStandardJSONSchema() to be called first"
  );

  S.enableStandardJSONSchema();

  // The `~standard` property now also exposes the Standard JSON Schema
  // `jsonSchema` converter. https://standardschema.dev/json-schema
  const jsonSchema: S.StandardJSONSchemaV1.Converter = standard.jsonSchema;

  const inputJsonSchema: Record<string, unknown> = jsonSchema.input({
    target: "draft-07",
  });
  const outputJsonSchema: Record<string, unknown> = jsonSchema.output({
    target: "draft-07",
  });

  // `input` returns the JSON Schema of the input type, with the `$schema` URI
  // for the requested target stamped on top of `S.toInputJSONSchemaOrThrow(schema)`.
  t.expect(inputJsonSchema).toEqual({
    $schema: "http://json-schema.org/draft-07/schema#",
    ...(S.toInputJSONSchemaOrThrow(schema) as Record<string, unknown>),
  });
  // `output` returns the JSON Schema of the output type, which differs.
  t.expect(inputJsonSchema).not.toEqual(outputJsonSchema);

  // The `draft-2020-12` target stamps a different `$schema` URI.
  t.expect(jsonSchema.input({ target: "draft-2020-12" }).$schema).toBe(
    "https://json-schema.org/draft/2020-12/schema"
  );
  // The `openapi-3.0` target omits `$schema`.
  t.expect(jsonSchema.input({ target: "openapi-3.0" }).$schema).toBe(undefined);
  // An unsupported target throws.
  t.expect(() =>
    jsonSchema.input({ target: "unsupported-target" })
  ).toThrow("Unsupported JSON Schema target: unsupported-target");
});

test("Full Set schema", (t) => {
  const mySet = <T>(itemSchema: S.Schema<unknown, T>): S.Schema<unknown, Set<T>> =>
    S.instance(Set<unknown>)
      .with(S.to, S.instance(Set<T>), (input) => {
        const output = new Set<T>();
        input.forEach((item, index) => {
          try {
            output.add(S.parseOrThrow(itemSchema)(item));
          } catch (e) {
            if (e instanceof S.Error) {
              throw new Error(`At item ${index} - ${e.reason}`);
            }
            throw e;
          }
        });
        return output;
      })
      .with(S.meta, {
        name: `Set<${S.toInputExpression(itemSchema)}>`,
      });

  const numberSetSchema = mySet(S.number);

  expectSchemaType(numberSetSchema).toBe<unknown, Set<number>>();

  t.expect(S.parseOrThrow(numberSetSchema)(new Set([1, 2, 3]))).toEqual(
    new Set([1, 2, 3]),
  );

  t.expect(() => S.parseOrThrow(numberSetSchema)([1, 2, "3"])).toThrow(
    t.expect.objectContaining({
      name: "SuryError",
      message: `Expected Set<number>, received [1, 2, "3"]`,
    }),
  );
  t.expect(() => S.parseOrThrow(numberSetSchema)(new Set([1, 2, "3"]))).toThrow(
    t.expect.objectContaining({
      name: "SuryError",
      message: `At item 3 - Expected number, received "3"`,
    }),
  );
});

test("Assert throws with invalid data", (t) => {
  const schema: S.Schema<string> = S.string;

  t.expect(() => {
    S.assertInputOrThrow(schema, 123);
  }).toThrow(
    t.expect.objectContaining({
      name: "SuryError",
      message: "Expected string, received 123",
    }),
  );
});

test("Assert passes with valid data", (t) => {
  const schema = S.string;

  const data: unknown = "abc";
  expectTypeOf(data).toEqualTypeOf<unknown>();
  S.assertInputOrThrow(schema, data);
  expectTypeOf(data).toEqualTypeOf<string>();
});

test("Assert supports both (schema, data) and (data, schema) arg orders", (t) => {
  const schema = S.string;

  // (schema, data)
  const a: unknown = "abc";
  S.assertInputOrThrow(schema, a);
  expectTypeOf(a).toEqualTypeOf<string>();

  // (data, schema)
  const b: unknown = "abc";
  S.assertInputOrThrow(b, schema);
  expectTypeOf(b).toEqualTypeOf<string>();

  // Both orders throw on invalid data
  t.expect(() => S.assertInputOrThrow(schema, 123)).toThrow();
  t.expect(() => S.assertInputOrThrow(123, schema)).toThrow();
});

test("AsyncAssertInput and AsyncAssertOutput reject instead of throwing, in either arg order", async (t) => {
  const schema = S.string.with(S.to, S.number, {
    decode: { async: async (s: string) => Number(s) },
    encode: String,
  });

  await S.assertInputAsPromiseOrReject(schema, "123");
  await S.assertInputAsPromiseOrReject("123", schema);
  await S.assertOutputAsPromiseOrReject(schema, 123);
  await S.assertOutputAsPromiseOrReject(123, schema);

  // Like every async operation, a failure in the synchronous type check
  // throws before a promise exists; `await` inside an async caller sees both.
  await t
    .expect(async () => await S.assertInputAsPromiseOrReject(schema, 123))
    .rejects.toThrow("Expected string, received 123");
  await t
    .expect(async () => await S.assertOutputAsPromiseOrReject(schema, "123"))
    .rejects.toThrow('Expected number, received "123"');
});

test("AssertOutput asserts against the output side", (t) => {
  const schema = S.string.with(S.to, S.number, { decode: Number, encode: String });

  const a: unknown = 123;
  S.assertOutputOrThrow(schema, a);
  expectTypeOf(a).toEqualTypeOf<number>();

  const b: unknown = 123;
  S.assertOutputOrThrow(b, schema);
  expectTypeOf(b).toEqualTypeOf<number>();

  // The input side is the other question, and answers it the other way.
  t.expect(() => S.assertOutputOrThrow(schema, "123")).toThrow();
  t.expect(() => S.assertInputOrThrow(schema, "123")).not.toThrow();
});

test("Constructors validate and hand back the value they were given", (t) => {
  const schema = S.schema({ id: S.string, email: S.email });
  const construct = S.makeOutputOrThrow(schema);
  const value = { id: "1", email: "a@b.com" };

  t.expect(construct(value)).toBe(value);
  t.expect(() => construct({ id: "1", email: "nope" })).toThrow(
    "Failed at email: Expected email, received \"nope\""
  );
});

test("Constructors take the side they are named for", (t) => {
  const schema = S.schema({ id: S.string.with(S.to, S.bigint) });

  t.expect(S.makeOutputOrThrow(schema)({ id: 1n })).toEqual({ id: 1n });
  t.expect(() => S.makeOutputOrThrow(schema)({ id: "1" } as never)).toThrow();

  t.expect(S.makeInputOrThrow(schema)({ id: "1" })).toEqual({ id: "1" });
  t.expect(() => S.makeInputOrThrow(schema)({ id: 1n } as never)).toThrow();
});

// The conversion runs even though its result is dropped, so an entity the
// codec has no way to represent is rejected rather than silently accepted.
test("OutputConstructor rejects a value the schema can't encode", (t) => {
  const schema = S.schema({
    n: S.string.with(S.to, S.number, { decode: Number, encode: "never" }),
  });

  t.expect(() => S.makeOutputOrThrow(schema)({ n: 1 })).toThrow(
    "Failed at n: Nothing decodes number -> string. It is marked with S.never"
  );
});

test("OutputConstructor mints a brand from an unbranded value", (t) => {
  const userId = S.string.with(S.brand, "UserId");
  const construct = S.makeOutputOrThrow(userId);

  const minted = construct("abc");
  expectTypeOf(minted).toEqualTypeOf<S.Brand<string, "UserId">>();
  t.expect(minted).toBe("abc");
  t.expect(() => construct(123 as never)).toThrow();
});

test("Async constructors await the conversion before handing the value back", async (t) => {
  const schema = S.schema({
    n: S.string.with(S.to, S.number, {
      decode: { async: async (value) => Number(value) },
      encode: String,
    }),
  });

  t.expect(await S.makeOutputAsPromiseOrReject(schema)({ n: 5 })).toEqual({ n: 5 });
  t.expect(await S.makeInputAsPromiseOrReject(schema)({ n: "5" })).toEqual({ n: "5" });
});

test("InputValidator returns a compiled type guard", (t) => {
  const isString = S.isInput(S.string);

  t.expect(isString("abc")).toBe(true);
  t.expect(isString(123)).toBe(false);
  t.expect(isString(null)).toBe(false);
  t.expect(isString(undefined)).toBe(false);

  const data: unknown = "abc";
  if (isString(data)) {
    expectTypeOf(data).toEqualTypeOf<string>();
  }
});

test("InputValidator works with advanced schemas", (t) => {
  const isValid = S.isInput(S.schema({ foo: S.string }));

  t.expect(isValid({ foo: "bar" })).toBe(true);
  t.expect(isValid({ foo: 123 })).toBe(false);
  t.expect(isValid(null)).toBe(false);
});

test("OutputValidator checks the output side", (t) => {
  const schema = S.string.with(S.to, S.number, { decode: Number, encode: String });
  const isOutput = S.isOutput(schema);

  t.expect(isOutput(123)).toBe(true);
  t.expect(isOutput("123")).toBe(false);
  // The input side is the other question, and answers it the other way.
  t.expect(S.isInput(schema)("123")).toBe(true);

  const data: unknown = 123;
  if (isOutput(data)) {
    expectTypeOf(data).toEqualTypeOf<number>();
  }
});

test("Assert throws a Sury error for null/undefined data in both arg orders", (t) => {
  const schema = S.string;

  // (schema, data)
  t.expect(() => S.assertInputOrThrow(schema, null)).toThrow(S.Error);
  t.expect(() => S.assertInputOrThrow(schema, undefined)).toThrow(S.Error);

  // (data, schema) - nullish data must throw a Sury error, not a TypeError
  t.expect(() => S.assertInputOrThrow(null, schema)).toThrow(S.Error);
  t.expect(() => S.assertInputOrThrow(undefined, schema)).toThrow(S.Error);
});

test("Assert reads a `~standard`-carrying payload as data, not as the schema", (t) => {
  const schema = S.schema({ id: S.string });
  // A JSON body can carry any key. Duck-typing the Standard Schema marker read
  // this as the schema - and the real schema as the data to validate.
  const payload = JSON.parse('{"~standard": 1, "id": "u1"}') as unknown;
  t.expect(S.assertInputOrThrow(payload, schema)).toBe(undefined);
  t.expect(() => S.assertInputOrThrow(JSON.parse('{"~standard": 1}'), schema)).toThrow(S.Error);
});

test("Assert panics when no argument is a Sury schema", (t) => {
  const foreign = {
    "~standard": { version: 1, vendor: "other", validate: (v: unknown) => ({ value: v }) },
  };
  t.expect(() => (S.assertInputOrThrow as (a: unknown, b: unknown) => void)("x", foreign)).toThrow(
    "Expected a Sury schema",
  );
  t.expect(() => (S.assertOutputOrThrow as (a: unknown, b: unknown) => void)("x", foreign)).toThrow(
    "Expected a Sury schema",
  );
});

// The three call shapes and their failure are a property of the CALL, so no
// spec pins them: every spec runs its own values through all three, but only a
// schema it already has.
test("isEqual dispatch: the schema is found first or last, and its absence panics", (t) => {
  const schema = S.schema({ id: S.string });
  const a = { id: "u1" };
  const b = { id: "u1" };

  // Arity, not an undefined check, separates the compiled form from a
  // comparison of two absent values - `S.void` admits `undefined`.
  t.expect(typeof S.isEqualOutput(schema)).toBe("function");
  t.expect(S.isEqualOutput(S.void, undefined, undefined)).toBe(true);

  t.expect(S.isEqualOutput(schema)(a, b)).toBe(true);
  t.expect(S.isEqualOutput(schema, a, b)).toBe(true);
  t.expect(S.isEqualOutput(a, b, schema)).toBe(true);
  t.expect(S.isEqualInput(a, b, schema)).toBe(true);
  t.expect(S.isEqualOutput(a, { id: "u2" }, schema)).toBe(false);

  const foreign = {
    "~standard": { version: 1, vendor: "other", validate: (v: unknown) => ({ value: v }) },
  };
  t.expect(() => (S.isEqualOutput as (a: unknown) => unknown)(foreign)).toThrow(
    "Expected a Sury schema",
  );
  t.expect(() =>
    (S.isEqualInput as (a: unknown, b: unknown, c: unknown) => unknown)("x", "x", foreign),
  ).toThrow("Expected a Sury schema");
});

// A spec can't turn a global on, so the one position that compares with
// SameValueZero has no golden: a number reaches NaN only once its validation is
// off. Reflexivity is what is at stake - NaN is the one value `===` cannot
// match - and -0 comes along with it, since SameValueZero is what the rest of
// the emit uses for a number too.
test("a number compares with SameValueZero once NaN can reach it", (t) => {
  t.expect(S.isEqualOutput(S.number, NaN, NaN)).toBe(false);
  try {
    S.global({ disableNanNumberValidation: true });
    t.expect(S.isEqualOutput(S.number, NaN, NaN)).toBe(true);
    t.expect(S.isEqualOutput(S.number, 0, -0)).toBe(true);
    t.expect(S.isEqualOutput(S.number, 1, 2)).toBe(false);
    const nested = S.schema({ n: S.number });
    t.expect(S.isEqualOutput(nested, { n: NaN }, { n: NaN })).toBe(true);
    t.expect(S.isEqualOutput(nested, { n: NaN }, { n: 1 })).toBe(false);
  } finally {
    S.global({});
  }
});

test("Every construction path keeps a schema recognizable to operation dispatch", (t) => {
  // `S.assertInputOrThrow(data, schema)` only finds the schema in the second slot when
  // the schema still has one of the two schema prototypes, so every way of
  // making a schema has to preserve it.
  type Node = { id: string; next?: Node };
  const recursed = S.recursive<Node, Node>("Node", (self) =>
    S.schema({ id: S.string, next: S.optional(self) }),
  );
  const schemas: S.Schema<any, any>[] = [
    S.string,
    S.string.with(S.meta, { description: "copied" }),
    S.string.with(S.to, S.number, { decode: Number, encode: String }),
    S.reverse(S.string.with(S.to, S.number, { decode: Number, encode: String })),
    S.union(["a", "b"]),
    recursed,
  ];
  for (const schema of schemas) {
    // Reaches the data slot only if the schema was recognized in slot two.
    t.expect(() => S.assertInputOrThrow(Symbol("not data"), schema)).toThrow(S.Error);
  }
});

test("Schema of object with empty prototype", (t) => {
  const obj = Object.create(null) as { foo: S.Schema<string> };
  obj.foo = S.string;
  const schema = S.schema(obj);

  const data = {
    foo: "bar",
  };
  t.expect(S.parseOrThrow(schema)(data)).toEqual(data);
  t.expect(S.encodeOrThrow(schema)(data)).toEqual(data);
});

test("Successfully parses recursive object", (t) => {
  type Node = {
    id: string;
    children: Node[];
  };

  // The one-arg form relies on `TOutput = TInput` - keep it compiling for
  // identity recursion even if the signature changes.
  let nodeSchema = S.recursive<Node>("Node", (nodeSchema) =>
    S.schema({
      id: S.string,
      children: S.array(nodeSchema),
    }),
  );

  expectSchemaType(nodeSchema).toBe<Node, Node>();

  t.expect(
    S.parseOrThrow(nodeSchema)({
      id: "1",
      children: [
        { id: "2", children: [] },
        { id: "3", children: [{ id: "4", children: [] }] },
      ],
    }),
  ).toEqual({
    id: "1",
    children: [
      { id: "2", children: [] },
      { id: "3", children: [{ id: "4", children: [] }] },
    ],
  });
});

test("Mutually recursive objects", (t) => {
  type User = {
    email: string;
    posts: Post[];
  };
  type Post = {
    title: string;
    author: User;
  };

  const makeUserSchema = (postSchema: S.Schema<unknown, Post>) =>
    S.schema({
      email: S.string,
      posts: S.array(postSchema),
    });
  const makePostSchema = (userSchema: S.Schema<unknown, User>) =>
    S.schema({
      Title: S.string,
      Author: userSchema,
    }).with(S.shape, (post) => ({
      title: post.Title,
      author: post.Author,
    }));

  const userSchema = S.recursive<unknown, User>("User", (userSchema) =>
    makeUserSchema(
      S.recursive<unknown, Post>("Post", (_) => makePostSchema(userSchema)),
    ),
  );
  const postSchema = S.recursive<unknown, Post>("Post", (postSchema) =>
    makePostSchema(
      S.recursive<unknown, User>("User", (_) => makeUserSchema(postSchema)),
    ),
  );

  expectSchemaType(userSchema).toBe<unknown, User>();
  expectSchemaType(postSchema).toBe<unknown, Post>();

  t.expect(
    S.parseOrThrow(userSchema)({
      email: "test@test.com",
      posts: [
        { Title: "Hello", Author: { email: "test@test.com", posts: [] } },
      ],
    }),
  ).toEqual({
    email: "test@test.com",
    posts: [{ title: "Hello", author: { email: "test@test.com", posts: [] } }],
  });

  t.expect(
    S.parseOrThrow(postSchema)({
      Title: "Hello",
      Author: { email: "test@test.com", posts: [] },
    }),
  ).toEqual({ title: "Hello", author: { email: "test@test.com", posts: [] } });
});

test("Recursive object with S.shape", (t) => {
  type Node = {
    id: string;
    children: Node[];
  };

  let nodeSchema = S.recursive<unknown, Node>("Node", (nodeSchema) =>
    S.schema({
      ID: S.string,
      CHILDREN: S.array(nodeSchema),
    }).with(S.shape, (input) => ({
      id: input.ID,
      children: input.CHILDREN,
    })),
  );

  expectSchemaType(nodeSchema).toBe<unknown, Node>();

  t.expect(
    S.parseOrThrow(nodeSchema)({
      ID: "1",
      CHILDREN: [
        { ID: "2", CHILDREN: [] },
        { ID: "3", CHILDREN: [{ ID: "4", CHILDREN: [] }] },
      ],
    }),
  ).toEqual({
    id: "1",
    children: [
      { id: "2", children: [] },
      { id: "3", children: [{ id: "4", children: [] }] },
    ],
  });
});

test("Recursive with self as transform target", (t) => {
  type Node = Node[];

  t.expect(() => {
    let nodeSchema = S.recursive<string, Node>("Node", (self) =>
      S.string.with(S.to, S.array(self)),
    );
    expectSchemaType(nodeSchema).toBe<string, Node>();

    t.expect(S.parseOrThrow(nodeSchema)(`["[]","[]"]`)).toEqual([[], []]);
  }).toThrow(
    t.expect.objectContaining({
      message:
        "Can't decode string -> Node[]. Define custom codec with S.to",
    }),
  );
});

test("Parse to literal with no validation to emulate assert", async (t) => {
  const fn = S.parseOrThrow(
    S.schema({ foo: S.string }),
    S.schema(true).with(S.noValidation, true),
  );

  expectTypeOf(fn).toEqualTypeOf<(data: unknown) => true>();
  t.expect(fn({ foo: "bar" })).toEqual(true);
  t.expect(fn.toString()).toEqual(
    `i=>{typeof i==="object"&&i&&!Array.isArray(i)||e[1](i);let v0=i.foo;typeof v0==="string"||e[0](v0);return true}`,
  );
});

test("ArkType pattern matching", async (t) => {
  const schema = S.recursive("DbJSON", (self) =>
    S.union([
      S.to(S.bigint, S.string),
      S.string,
      S.number,
      S.boolean,
      null,
      S.record(self),
    ]),
  );

  t.expect(S.parseOrThrow(schema)(`foo`)).toEqual("foo");
  t.expect(S.parseOrThrow(schema)(5n)).toEqual("5");
  t.expect(S.parseOrThrow(schema)({ nested: 5n })).toEqual({ nested: "5" });
  t.expect(S.encodeOrThrow(schema)("5")).toEqual(5n);
  t.expect(S.encodeOrThrow(schema)("foo")).toEqual("foo");
});

test("Example of transformed schema", (t) => {
  // 1. Create a schema
  //    S.to - for easy & fast coercion
  //    S.shape - for easy & fast transformation
  //    S.meta - with examples in transformed format
  const userSchema = S.schema({
    USER_ID: S.string.with(S.to, S.bigint),
    USER_NAME: S.string,
  })
    .with(S.shape, (input) => ({
      id: input.USER_ID,
      name: input.USER_NAME,
    }))
    .with(S.meta, {
      description: "User entity in our system",
      examples: [
        {
          id: 0n,
          name: "Dmitry",
        },
      ],
    });
  // On hover: S.Schema<{
  //     id: bigint;
  //     name: string;
  // }, {
  //     USER_ID: string;
  //     USER_NAME: string;
  // }>

  // 2. Infer User type
  type User = S.Output<typeof userSchema>;
  // type User = {
  //   id: bigint;
  //   name: string;
  // }

  // 3. Use examples directly
  //    See how they are in the Input format 🔥
  t.expect(userSchema.examples).toEqual([
    {
      USER_ID: "0",
      USER_NAME: "Dmitry",
    },
  ]);

  // 4. Or via JSON Schema
  t.expect(S.toInputJSONSchemaOrThrow(userSchema)).toEqual({
    type: "object",
    properties: {
      USER_ID: {
        type: "string",
      },
      USER_NAME: {
        type: "string",
      },
    },
    required: ["USER_ID", "USER_NAME"],
    description: "User entity in our system",
    examples: [
      {
        USER_ID: "0",
        USER_NAME: "Dmitry",
      },
    ],
  });

  const fromJsonSchema = S.fromJSONSchemaOrThrow(S.toInputJSONSchemaOrThrow(userSchema));
  const jsonInput = { USER_ID: "0", USER_NAME: "Dmitry" };
  t.expect(S.parseOrThrow(fromJsonSchema)(jsonInput)).toEqual(jsonInput);
});

test("Brand", (t) => {
  const schema = S.string.with(S.brand, "Foo");
  type Foo = S.Infer<typeof schema>;
  expectSchemaType(schema).toBe<string, S.Brand<string, "Foo">>();
  const result = S.parseOrThrow(schema)("hello");
  assertType<S.Brand<string, "Foo">>(result);
  t.expect(result).toEqual("hello");
  t.expect(schema.name).toEqual("Foo");

  // @ts-expect-error - Branded string is not assignable to string
  const a: Foo = "bar";
});

test("fromJSONSchemaOrThrow", (t) => {
  const emailSchema = S.fromJSONSchemaOrThrow({
    type: "string",
    format: "email",
  });
  expectSchemaType(emailSchema).toBe<string, string>();
  t.expect(caught(() => S.assertInputOrThrow(emailSchema, "example.com"))?.message).toBe(
    `Expected email, received "example.com"`,
  );
});

test("fromJSONSchemaOrThrow: takes untyped input, `satisfies` checks an inline one", (t) => {
  // A schema loaded from a file or an API is untyped, and must not need a cast.
  const loaded: unknown = JSON.parse(`{"type":"string"}`);
  expectSchemaType(S.fromJSONSchemaOrThrow(loaded)).toBe<S.JSON, S.JSON>();
  t.expect(S.parseOrThrow(S.fromJSONSchemaOrThrow(loaded))("hello")).toBe("hello");

  const asJson: S.JSON = { type: "boolean" };
  expectSchemaType(S.fromJSONSchemaOrThrow(asJson)).toBe<S.JSON, S.JSON>();
  t.expect(S.parseOrThrow(S.fromJSONSchemaOrThrow(asJson))(true)).toBe(true);

  const authored = {
    type: "object",
    properties: { id: { type: "string" } },
    required: ["id"],
    "x-internal": true,
  } satisfies S.JSONSchema;
  t.expect(S.parseOrThrow(S.fromJSONSchemaOrThrow(authored))({ id: "1" })).toEqual({
    id: "1",
  });

  const typo = {
    type: "object",
    // @ts-expect-error - an unknown keyword is still caught; only `x-` is open
    requird: ["id"],
  } satisfies S.JSONSchema;
  t.expect(typo.type).toBe("object");
});

test("fromJSONSchemaOrThrow: an inline schema infers the type it describes", (t) => {
  const userSchema = S.fromJSONSchemaOrThrow({
    type: "object",
    properties: {
      id: { type: "string" },
      role: { enum: ["admin", "user"] },
      tags: { type: "array", items: { type: "string" } },
      point: { type: "array", prefixItems: [{ type: "number" }, { type: "number" }] },
      score: { type: "number", nullable: true },
    },
    required: ["id", "role"],
  });
  expectSchemaType(userSchema).toBe<{
    id: string;
    role: "admin" | "user";
    tags?: string[] | undefined;
    point?: [number?, number?, ...S.JSON[]] | undefined;
    score?: number | null | undefined;
  }>();
  t.expect(S.parseOrThrow(userSchema)({ id: "1", role: "admin" })).toEqual({
    id: "1",
    role: "admin",
  });

  // Local $ref pointers resolve, including recursive ones. The runtime still
  // parses a $ref as plain JSON - the static type leads it here.
  const treeSchema = S.fromJSONSchemaOrThrow({
    $ref: "#/$defs/node",
    $defs: {
      node: {
        type: "object",
        properties: {
          value: { type: "string" },
          children: { type: "array", items: { $ref: "#/$defs/node" } },
        },
        required: ["value"],
      },
    },
  });
  type Tree = S.Output<typeof treeSchema>;
  const tree: Tree = { value: "root", children: [{ value: "leaf" }] };
  assertType<string | undefined>(tree.children?.[0]?.value);

  // A dialect interface isn't a literal, so it falls back to Schema<JSON, JSON>.
  expectSchemaType(
    S.fromJSONSchemaOrThrow(S.toInputJSONSchemaOrThrow(S.schema({ a: S.string }))),
  ).toBe<S.JSON, S.JSON>();
});

test("fromJSONSchemaOrThrow: assertion-only schemas preserve valid JSON", (t) => {
  const anySchema = S.fromJSONSchemaOrThrow(true);
  const noSchema = S.fromJSONSchemaOrThrow(false);
  const emptyEnum = S.fromJSONSchemaOrThrow({ enum: [] });
  expectSchemaType(anySchema).toBe<S.JSON>();
  expectSchemaType(noSchema).toBe<never>();
  expectSchemaType(emptyEnum).toBe<never>();
  t.expect(S.toInputJSONSchemaOrThrow(emptyEnum)).toEqual({ not: {} });
  t.expect(S.parseOrThrow(anySchema)({ nested: [1, true] })).toEqual({ nested: [1, true] });
  t.expect(() => S.parseOrThrow(noSchema)(null)).toThrow("Expected never");
  t.expect(() => S.parseOrThrow(emptyEnum)("anything")).toThrow("Expected never");

  const composed = S.fromJSONSchemaOrThrow({
    type: "string",
    minLength: 2,
    anyOf: [{ pattern: "^a" }, { pattern: "z$" }],
  });
  expectSchemaType(composed).toBe<string>();
  t.expect(S.parseOrThrow(composed)("ab")).toBe("ab");
  t.expect(S.parseOrThrow(composed)("zz")).toBe("zz");
  t.expect(() => S.parseOrThrow(composed)("bb")).toThrow(
    "Should pass at least one schema according to the anyOf property."
  );

  const tupleSchema = S.fromJSONSchemaOrThrow({
    type: "array",
    items: [{ type: "string" }, { type: "number" }],
    additionalItems: { type: "boolean" },
  });
  expectSchemaType(tupleSchema).toBe<[
    (string | undefined)?,
    (number | undefined)?,
    ...boolean[],
  ]>();
  for (const value of [[], ["a"], ["a", 1], ["a", 1, true, false]]) {
    t.expect(S.parseOrThrow(tupleSchema)(value)).toBe(value);
  }
  t.expect(() => S.parseOrThrow(tupleSchema)(["a", 1, 2])).toThrow(
    "Should pass the positional and additional item schemas."
  );

  const optionalTransformedTuple = S.fromJSONSchemaOrThrow({
    type: "array",
    prefixItems: [
      {
        type: "object",
        properties: { value: { type: "string", default: "fallback" } },
      },
    ],
    items: false,
  });
  expectSchemaType(optionalTransformedTuple).toBe<
    [({ value?: string | undefined } | undefined)?, ...never[]]
  >();
  const absentPrefix: [] = [];
  t.expect(S.parseOrThrow(optionalTransformedTuple)(absentPrefix)).toBe(absentPrefix);
  t.expect(absentPrefix).toHaveLength(0);
  const presentPrefix: [{ value?: string }] = [{}];
  t.expect(S.parseOrThrow(optionalTransformedTuple)(presentPrefix)).toBe(presentPrefix);
  t.expect(presentPrefix).toEqual([{}]);

  const closedTupleSchema = S.fromJSONSchemaOrThrow({
    type: "array",
    minItems: 2,
    maxItems: 2,
    items: [{ type: "string" }, { type: "number" }],
  });
  expectSchemaType(closedTupleSchema).toBe<[string, number]>();
  t.expect(S.parseOrThrow(closedTupleSchema)(["a", 1])).toEqual(["a", 1]);
  // The tuple's own arity is the length check, so `minItems`/`maxItems` add
  // nothing and the error reads like a hand-written `S.tuple`.
  t.expect(() => S.parseOrThrow(closedTupleSchema)(["a"])).toThrow(
    "Expected [string, number], received"
  );
  t.expect(() => S.parseOrThrow(closedTupleSchema)(["a", 1, true])).toThrow(
    "Expected [string, number], received"
  );

  // Bounds that cross describe an array no value can have, rather than a tuple
  // carrying two contradictory length checks.
  const emptyTupleRange = S.fromJSONSchemaOrThrow({
    type: "array",
    prefixItems: [{ type: "string" }],
    minItems: 3,
    items: false,
  });
  expectSchemaType(emptyTupleRange).toBe<never>();
  t.expect(S.toInputJSONSchemaOrThrow(emptyTupleRange)).toEqual({ not: {} });

  const objectSchema = S.fromJSONSchemaOrThrow({
    type: "object",
    properties: { value: { type: "string", default: "fallback" } },
    required: ["constructor"],
    additionalProperties: { type: "integer" },
  });
  expectSchemaType(objectSchema).toBe<
    { value?: string | undefined; constructor: number },
    { value?: string | undefined; constructor: number }
  >();
  const objectInput = { constructor: 1, value: "set", extra: 2 };
  t.expect(S.parseOrThrow(objectSchema)(objectInput)).toBe(objectInput);
  t.expect(S.parseOrThrow(objectSchema)({ constructor: 1 })).toEqual({ constructor: 1 });
  t.expect(() => S.parseOrThrow(objectSchema)({})).toThrow(
    "Should contain every required property."
  );
  t.expect(() => S.parseOrThrow(objectSchema)({ constructor: 1, extra: "no" })).toThrow(
    "Should pass the additionalProperties schema."
  );

  const nativeDefaultObject = S.fromJSONSchemaOrThrow({
    type: "object",
    properties: { value: { type: "string", default: "fallback" } },
    additionalProperties: false,
  });
  expectSchemaType(nativeDefaultObject).toBe<
    { value?: string | undefined },
    { value: string }
  >();
  t.expect(S.parseOrThrow(nativeDefaultObject)({})).toEqual({ value: "fallback" });

  const defaultRecord = S.fromJSONSchemaOrThrow({
    type: "object",
    additionalProperties: {
      type: "object",
      properties: { value: { type: "string", default: "fallback" } },
      additionalProperties: false,
    },
  });
  expectSchemaType(defaultRecord).toBe<
    { [key: string]: { value?: string | undefined } },
    { [key: string]: { value: string } }
  >();
  t.expect(S.parseOrThrow(defaultRecord)({ first: {} })).toEqual({
    first: { value: "fallback" },
  });

  const openObjectSchema = S.fromJSONSchemaOrThrow({
    type: "object",
    properties: { value: { type: "string" } },
    additionalProperties: true,
  });
  const ownProto = JSON.parse(
    '{"value":"ok","__proto__":{"polluted":true}}'
  ) as S.Input<typeof openObjectSchema>;
  const parsedOwnProto = S.parseOrThrow(openObjectSchema)(ownProto);
  t.expect(parsedOwnProto).toBe(ownProto);
  t.expect(Object.getPrototypeOf(parsedOwnProto)).toBe(Object.prototype);
  t.expect(Object.hasOwn(parsedOwnProto, "__proto__")).toBe(true);
  t.expect((parsedOwnProto as { polluted?: boolean }).polluted).toBeUndefined();

  const unicode = S.fromJSONSchemaOrThrow({
    type: "string",
    minLength: 2,
    maxLength: 2,
  });
  t.expect(() => S.parseOrThrow(unicode)("\u{10400}\u{10401}")).toThrow(
    'Expected string.length == 2, received "𐐀𐐁"',
  );
  t.expect(S.parseOrThrow(unicode)("😀")).toBe("😀");

  const legacyPattern = S.fromJSONSchemaOrThrow({
    type: "string",
    pattern: "^\\d{3}\\-\\d{4}$",
  });
  t.expect(S.parseOrThrow(legacyPattern)("123-4567")).toBe("123-4567");
  t.expect(() => S.parseOrThrow(legacyPattern)("1234567")).toThrow("Invalid pattern");
  t.expect(() => S.fromJSONSchemaOrThrow({ type: "string", pattern: "[" })).toThrow(
    'Invalid JSON Schema pattern: "["'
  );
});

test("fromJSONSchemaOrThrow: $ref siblings follow the declared dialect", (t) => {
  const target = { type: "string" } as const;
  const modern = S.fromJSONSchemaOrThrow({
    $schema: "https://json-schema.org/draft/2020-12/schema",
    $ref: "#/$defs/id",
    minLength: 3,
    anyOf: [{ pattern: "c$" }],
    $defs: { id: target },
  });
  const legacy = S.fromJSONSchemaOrThrow({
    $schema: "http://json-schema.org/draft-07/schema#",
    $ref: "#/$defs/id",
    minLength: 3,
    anyOf: [{ pattern: "never$" }],
    $defs: { id: target },
  });
  expectSchemaType(modern).toBe<string>();
  expectSchemaType(legacy).toBe<string>();
  t.expect(S.parseOrThrow(modern)("abc")).toBe("abc");
  t.expect(() => S.parseOrThrow(modern)("ab")).toThrow(
    "Should pass the keywords adjacent to the $ref."
  );
  t.expect(() => S.parseOrThrow(modern)("abd")).toThrow(
    "Should pass at least one schema according to the anyOf property."
  );
  t.expect(() => S.parseOrThrow(legacy)("ab")).toThrow(
    "Should pass at least one schema according to the anyOf property."
  );
  // The `$ref` resolved to a finite shape and inlined, so it left no `$defs`
  // entry behind - and the rendering doesn't depend on whether options were
  // passed, only on the dialect they name.
  const legacyRendering = {
    type: "string",
    anyOf: [{ pattern: "never$" }],
  };
  t.expect(S.toInputJSONSchemaOrThrow(legacy)).toEqual(legacyRendering);
  t.expect(S.toInputJSONSchemaOrThrow(legacy, { target: "openapi-3.0" })).toEqual(legacyRendering);
  t.expect(S.toInputJSONSchemaOrThrow(legacy, { target: "draft-07" })).toEqual({
    ...legacyRendering,
    $schema: "http://json-schema.org/draft-07/schema#",
  });

  const modernAlias = S.fromJSONSchemaOrThrow({
    $schema: "http://json-schema.org/draft/2020-12/schema#",
    $ref: "#/$defs/id",
    type: "number",
    $defs: { id: target },
  });
  const legacyAlias = S.fromJSONSchemaOrThrow({
    $schema: "https://json-schema.org/draft-07/schema",
    $ref: "#/$defs/id",
    type: "number",
    $defs: { id: target },
  });
  expectSchemaType(modernAlias).toBe<never>();
  expectSchemaType(legacyAlias).toBe<string>();
  t.expect(() => S.parseOrThrow(modernAlias)("abc")).toThrow(
    "Should pass the keywords adjacent to the $ref."
  );
  t.expect(S.parseOrThrow(legacyAlias)("abc")).toBe("abc");

  const customDialect = {
    $schema: "https://json-schema.org/draft/2020-12/custom",
    $ref: "#/$defs/id",
    type: "number",
    $defs: { id: target },
  } as const;
  const customDialectSchema = S.fromJSONSchemaOrThrow(customDialect);
  expectSchemaType(customDialectSchema).toBe<string>();
  t.expect(S.parseOrThrow(customDialectSchema)("abc")).toBe("abc");

  const nestedModern = S.fromJSONSchemaOrThrow({
    $schema: "https://json-schema.org/draft/2020-12/schema",
    type: "object",
    properties: { id: { $ref: "#/$defs/id" } },
    $defs: { id: target },
  });
  expectSchemaType(nestedModern).toBe<{ id?: string | undefined }>();
  t.expect(S.toInputJSONSchemaOrThrow(nestedModern)).toEqual({
    type: "object",
    properties: { id: { type: "string" } },
  });
});

test("toInputJSONSchemaOrThrow: the target picks the dialect of the result", (t) => {
  const tupleSchema = S.schema([S.string, S.number]);

  const draft07 = S.toInputJSONSchemaOrThrow(tupleSchema);
  expectTypeOf(draft07).toEqualTypeOf<S.JSONSchema7>();
  t.expect(draft07.items).toEqual([{ type: "string" }, { type: "number" }]);

  const draft2020 = S.toInputJSONSchemaOrThrow(tupleSchema, { target: "draft-2020-12" });
  expectTypeOf(draft2020).toEqualTypeOf<S.JSONSchema2020>();
  t.expect(draft2020.prefixItems).toEqual([
    { type: "string" },
    { type: "number" },
  ]);

  const openapi = S.toInputJSONSchemaOrThrow(S.nullable(S.string), {
    target: "openapi-3.0",
  });
  expectTypeOf(openapi).toEqualTypeOf<S.OpenAPISchema30>();
  t.expect(openapi.nullable).toBe(true);

  const imported2020 = S.fromJSONSchemaOrThrow({
    $schema: "https://json-schema.org/draft/2020-12/schema",
    type: "array",
    items: [{ type: "string" }, { type: "number" }],
    additionalItems: false,
    minItems: 2,
    maxItems: 2,
  });
  t.expect(S.toInputJSONSchemaOrThrow(imported2020, { target: "draft-07" })).toEqual({
    $schema: "http://json-schema.org/draft-07/schema#",
    type: "array",
    minItems: 2,
    maxItems: 2,
    items: [{ type: "string" }, { type: "number" }],
  });
  t.expect(S.toInputJSONSchemaOrThrow(imported2020, { target: "openapi-3.0" })).not.toHaveProperty(
    "$schema",
  );

  // A target held in a variable can't select a dialect, so the result widens.
  const target: S.StandardJSONSchemaV1.Target = "draft-07";
  expectTypeOf(S.toInputJSONSchemaOrThrow(tupleSchema, { target })).toEqualTypeOf<
    S.JSONSchema
  >();

  // @ts-expect-error - draft-07 spells tuples with `items`, not `prefixItems`
  draft07.prefixItems;
  // @ts-expect-error - OpenAPI 3.0 has no `const`; it uses a one-value `enum`
  openapi.const;

  // Every dialect's result feeds back in without a cast.
  t.expect(S.parseOrThrow(S.fromJSONSchemaOrThrow(draft2020))(["a", 1])).toEqual(["a", 1]);
  t.expect(S.parseOrThrow(S.fromJSONSchemaOrThrow(openapi))(null)).toBe(null);

  // Every dialect stays assignable to the wide type - the invariant that keeps
  // `extendJSONSchema(schema, toInputJSONSchemaOrThrow(other, { target }))` compiling. This
  // breaks when a shared keyword is typed incompatibly across the two (extra
  // dialect-only keywords slip through structurally - parity there is on the
  // comment in src/types/jsonschema.d.ts).
  expectTypeOf<S.JSONSchema7>().toExtend<S.JSONSchema>();
  expectTypeOf<S.JSONSchema2020>().toExtend<S.JSONSchema>();
  expectTypeOf<S.OpenAPISchema30>().toExtend<S.JSONSchema>();
});

test("fromJSONSchemaOrThrow: assertion keywords bind without an explicit `type`", (t) => {
  const parse = (js: object) => S.parseOrThrow(S.fromJSONSchemaOrThrow(js)) as (d: unknown) => unknown;

  const obj = parse({ properties: { bar: { type: "integer" } }, required: ["bar"] });
  t.expect(obj({ bar: 2 })).toEqual({ bar: 2 });
  t.expect(caught(() => obj({ bar: "x" }))).toBeDefined();
  t.expect(caught(() => obj({}))).toBeDefined();

  const min = parse({ minimum: 5 });
  t.expect(min(7)).toBe(7);
  t.expect(caught(() => min(3))).toBeDefined();
  // Vacuous off-type: `minimum` says nothing about a string.
  t.expect(min("abc")).toBe("abc");

  const minLength = parse({ minLength: 3 });
  t.expect(minLength("abcd")).toBe("abcd");
  t.expect(caught(() => minLength("a"))).toBeDefined();
  t.expect(minLength(1)).toBe(1);

  t.expect(parse({ additionalItems: false })(["anything"])).toEqual(["anything"]);
});

test("fromJSONSchemaOrThrow: annotations stay on a synthesized type union's root", (t) => {
  const schema = S.fromJSONSchemaOrThrow({
    type: ["string", "number"],
    title: "Value",
  });
  t.expect(S.toInputJSONSchemaOrThrow(schema)).toEqual({
    anyOf: [{ type: "string" }, { type: "number" }],
    title: "Value",
  });
});

test("fromJSONSchemaOrThrow: composition keywords constrain in addition to the base shape", (t) => {
  const emptyAllOf = S.fromJSONSchemaOrThrow({ allOf: [] });
  expectSchemaType(emptyAllOf).toBe<S.JSON>();
  t.expect(S.parseOrThrow(emptyAllOf)({ anything: true })).toEqual({ anything: true });

  const schema = S.fromJSONSchemaOrThrow({
    type: "object",
    properties: { bar: { type: "integer" } },
    required: ["bar"],
    allOf: [{ properties: { foo: { type: "string" } }, required: ["foo"] }],
  });
  const parse = S.parseOrThrow(schema) as (d: unknown) => unknown;

  const input = { bar: 2, foo: "x" };
  t.expect(parse(input)).toEqual({ bar: 2 });
  // Fails the base shape.
  t.expect(caught(() => parse({ bar: "no", foo: "x" }))).toBeDefined();
  // Fails only the allOf branch - the base shape alone used to win.
  t.expect(caught(() => parse({ bar: 2 }))).toBeDefined();
});

test("fromJSONSchemaOrThrow: oneOf counts matches, `not` and if/then/else layer on", (t) => {
  const one = S.parseOrThrow(
    S.fromJSONSchemaOrThrow({ oneOf: [{ type: "number" }, { type: "string" }] }),
  ) as (d: unknown) => unknown;
  t.expect(one(1)).toBe(1);
  t.expect(caught(() => one(true))).toBeDefined();

  const not = S.parseOrThrow(S.fromJSONSchemaOrThrow({ not: { type: "string" } })) as (
    d: unknown,
  ) => unknown;
  t.expect(not(1)).toBe(1);
  t.expect(caught(() => not("x"))).toBeDefined();

  // `then`/`else` are each optional and default to "always passes".
  const ite = S.parseOrThrow(
    S.fromJSONSchemaOrThrow({ if: { type: "number" }, then: { minimum: 5 } }),
  ) as (d: unknown) => unknown;
  t.expect(ite(7)).toBe(7);
  t.expect(ite("anything")).toBe("anything");
  t.expect(caught(() => ite(3))).toBeDefined();
});

test("fromJSONSchemaOrThrow: an unmodelled assertion keyword fails at creation", (t) => {
  // Ignoring it would widen the schema - the validator would accept data the
  // author wrote the keyword to reject - so this must not silently succeed.
  t.expect(
    caught(() => S.fromJSONSchemaOrThrow({ unevaluatedProperties: false }))?.message,
  ).toContain("Unsupported JSON Schema keyword: unevaluatedProperties");

  t.expect(
    caught(() => S.fromJSONSchemaOrThrow({ $dynamicRef: "#items" }))?.message,
  ).toContain("$dynamicRef");

  t.expect(
    caught(() => S.fromJSONSchemaOrThrow({ $recursiveRef: "#" }))?.message,
  ).toContain("$recursiveRef");

  t.expect(
    S.parseOrThrow(
      S.fromJSONSchemaOrThrow({
        $schema: "https://example.com/custom-meta-schema",
        type: "number",
      }),
    )(1),
  ).toBe(1);
});

test("fromJSONSchemaOrThrow: exclusiveMaximum bounds the maximum, not the minimum", (t) => {
  const parse = S.parseOrThrow(
    S.fromJSONSchemaOrThrow({ type: "integer", exclusiveMaximum: 5 }),
  ) as (d: unknown) => unknown;
  t.expect(parse(4)).toBe(4);
  t.expect(caught(() => parse(5))).toBeDefined();
  t.expect(caught(() => parse(9))).toBeDefined();
});

test("Compile types", async (t) => {
  const schema = S.union([
    S.string,
    S.schema(null).with(S.to, S.schema(undefined)),
  ]);

  const fn1 = S.decodeOrThrow(schema);
  expectTypeOf(fn1).toEqualTypeOf<
    (input: string | null) => string | undefined
  >();
  t.expect(fn1("hello")).toEqual("hello");
  t.expect(fn1(null)).toEqual(undefined);

  const fn2 = S.encodeOrThrow(schema);
  expectTypeOf(fn2).toEqualTypeOf<
    (input: string | undefined) => string | null
  >();
  t.expect(fn2("hello")).toEqual("hello");
  t.expect(fn2(undefined)).toEqual(null);

  const fn3 = S.parseOrThrow(schema);
  expectTypeOf(fn3).toEqualTypeOf<(input: unknown) => string | undefined>();
  t.expect(fn3("hello")).toEqual("hello");
  t.expect(fn3(null)).toEqual(undefined);

  const fn4 = S.decodeOrThrow(S.json, schema);
  expectTypeOf(fn4).toEqualTypeOf<(input: S.JSON) => string | undefined>();
  t.expect(fn4("hello")).toEqual("hello");
  t.expect(fn4(null)).toEqual(undefined);

  const fn5 = S.decodeOrThrow(S.jsonString, schema);
  expectTypeOf(fn5).toEqualTypeOf<(input: string) => string | undefined>();
  t.expect(fn5(`"hello"`)).toEqual("hello");
  t.expect(fn5("null")).toEqual(undefined);

  const fn6 = S.encodeOrThrow(schema, S.json);
  expectTypeOf(fn6).toEqualTypeOf<(input: string | undefined) => S.JSON>();
  t.expect(fn6("hello")).toEqual("hello");
  t.expect(fn6(undefined)).toEqual(null);

  const fn7 = S.encodeOrThrow(schema, S.jsonString);
  expectTypeOf(fn7).toEqualTypeOf<(input: string | undefined) => string>();
  t.expect(fn7("hello")).toEqual(`"hello"`);
  t.expect(fn7(undefined)).toEqual("null");

  // Only the first schema is reversed; the target runs forward from its Input.
  const stringToNumber = S.string.with(S.to, S.number);
  const fn8 = S.encodeOrThrow(S.number, stringToNumber);
  expectTypeOf(fn8).toEqualTypeOf<(input: number) => number>();
  t.expect(fn8(1)).toEqual(1);
  t.expect(S.encodeOrThrow(S.reverse(stringToNumber), stringToNumber)("1")).toEqual(1);
  const fn9 = S.encodeAsPromiseOrReject(S.number, stringToNumber);
  expectTypeOf(fn9).toEqualTypeOf<(input: number) => Promise<number>>();
  t.expect(await fn9(1)).toEqual(1);

  // FIXME:
  // const fn8 = S.compile(schema, "Output", "Assert", "Sync", true);
  // expectTypeOf(fn8).toEqualTypeOf<(input: string | undefined) => void>();
  // t.deepEqual(fn8("hello"), undefined);
  // t.deepEqual(fn8(undefined), undefined);

  // const fn9 = S.compile(schema, "Output", "JsonString", "Async");
  // expectTypeOf(fn9).toEqualTypeOf<(input: string | undefined) => Promise<string>>();
  // t.deepEqual(await fn9("hello"), `"hello"`);
  // t.deepEqual(await fn9(undefined), "null");
});

test("Preprocess nested fields", (t) => {
  const stripPrefix = <TInput>(
    schema: S.Schema<TInput, string>,
    prefix: string,
  ): S.Schema<TInput, string> =>
    S.to(schema, S.string, {
      decode: (v) => {
        if (v.startsWith(prefix)) {
          return v.slice(1);
        } else {
          throw new Error(`String must start with ${prefix}`);
        }
      },
      encode: (v) => prefix + v,
    });

  const schema = S.schema({
    nested: {
      tag: S.string.with(stripPrefix, "_").with(S.to, S.schema("foo")),
      numberTag: S.string.with(stripPrefix, "~").with(S.to, S.schema(1)),
    },
  }).with(S.shape, (_) => undefined);

  const fn = S.encodeOrThrow(schema);

  t.expect(fn.toString()).toEqual(
    // The junction seam validates each coder's result against its target.
    `i=>{i===void 0||e[6](i);let v0;try{v0=e[0]("foo")}catch(x){e[1](x)}typeof v0==="string"||e[2](v0);let v1;try{v1=e[3]("1")}catch(x){e[4](x)}typeof v1==="string"||e[5](v1);return {nested:{tag:v0,numberTag:v1}}}`,
  );

  const value = fn(undefined);
  t.expect(value).toEqual({
    nested: {
      numberTag: "~1",
      tag: "_foo",
    },
  });
});

test("Union of object keys", (t) => {
  // https://github.com/DZakh/sury/issues/128
  const allCurrencies = {
    USD: 1,
    BGP: 2,
    EUR: 3,
  };

  const schema = S.union(Object.keys(allCurrencies));
  expectSchemaType(schema).toBe<string, string>();
  t.expect(S.parseOrThrow(schema)("USD")).toEqual("USD");
  t.expect(() => S.parseOrThrow(schema)("GBP")).toThrow(
    t.expect.objectContaining({
      name: "SuryError",
      message: `Expected "USD" | "BGP" | "EUR", received "GBP"`,
    }),
  );

  const schema2 = S.union(
    Object.keys(allCurrencies) as (keyof typeof allCurrencies)[],
  );
  expectSchemaType(schema2).toBe<
    "USD" | "BGP" | "EUR",
    "USD" | "BGP" | "EUR"
  >();
  t.expect(S.parseOrThrow(schema)("USD")).toEqual("USD");
  t.expect(() => S.parseOrThrow(schema)("GBP")).toThrow(
    t.expect.objectContaining({
      name: "SuryError",
      message: `Expected "USD" | "BGP" | "EUR", received "GBP"`,
    }),
  );

  const schema3 = S.union(
    (Object.keys(allCurrencies) as (keyof typeof allCurrencies)[]).map(
      (literal) => S.schema(literal),
    ),
  );
  expectSchemaType(schema3).toBe<
    "USD" | "BGP" | "EUR",
    "USD" | "BGP" | "EUR"
  >();
  t.expect(S.parseOrThrow(schema)("USD")).toEqual("USD");
  t.expect(() => S.parseOrThrow(schema)("GBP")).toThrow(
    t.expect.objectContaining({
      name: "SuryError",
      message: `Expected "USD" | "BGP" | "EUR", received "GBP"`,
    }),
  );
});

test("Union of dynamic enum as const", (t) => {
  // https://github.com/DZakh/sury/issues/137
  const test = ["a", "b", "c"] as const;
  const schema = S.union(test);

  expectSchemaType(schema).toBe<"a" | "b" | "c", "a" | "b" | "c">();
  t.expect(S.parseOrThrow(schema)("a")).toEqual("a");
  t.expect(() => S.parseOrThrow(schema)("d")).toThrow(
    t.expect.objectContaining({
      name: "SuryError",
      message: `Expected "a" | "b" | "c", received "d"`,
    }),
  );
});

test("Overwrite error message", (t) => {
  const schema = S.string.with(S.minLength, 3, "Invalid string");

  const fieldSchema = <TInput, TOutput>(
    schema: S.Schema<TInput, TOutput>,
  ): S.Schema<TInput, TOutput> => {
    return S.any.with(S.to, schema, (v) => {
      try {
        S.assertInputOrThrow(schema, v);
        return v;
      } catch (e) {
        if (e instanceof S.Error) {
          throw new Error(e.reason);
        }
        throw e;
      }
    });
  };

  // Doesn't work starting from 11.0.0-alpha.4
  // The error is always wrapped in SuryError
  t.expect(() =>
    S.parseOrThrow(S.schema({ foo: fieldSchema(schema) }))({ foo: "hi" }),
  ).toThrow(
    t.expect.objectContaining({
      name: "SuryError",
      message: `Failed at foo: Invalid string`,
    }),
  );
});

test("Throwing one retained error instance twice doesn't accumulate the path", (t) => {
  // The path a throw is reached through is prepended to the error, so doing it
  // on the caught instance leaves the second parse reporting `a.a`.
  // Nothing stops user code from holding one error and throwing it again.
  const retained = S.parseAsResult(S.string, 1).error!;
  const schema = S.schema({
    a: S.string.with(S.to, S.number, () => {
      throw retained;
    }),
  });
  const parse = S.parseOrThrow(schema);

  for (const _ of [1, 2, 3]) {
    const error = caught(() => parse({ a: "x" }));
    t.expect(error?.message).toBe(`Failed at a: Expected string, received 1`);
    t.expect(error?.path).toEqual(["a"]);
  }
  // The instance user code holds is left as it was caught.
  t.expect(retained.path).toEqual([]);

  // Top level: nothing to prepend, so the error is passed through rather than
  // copied. Still must not pick up a path or mutate what was thrown.
  const flat = S.parseOrThrow(
    S.string.with(S.to, S.number, () => {
      throw retained;
    }),
  );
  for (const _ of [1, 2, 3]) {
    t.expect(caught(() => flat("x"))?.path).toEqual([]);
  }
  t.expect(retained.path).toEqual([]);
});

test("Formatting a path segment that isn't a string renders instead of throwing", (t) => {
  // Sury never emits a symbol segment, but `refine`'s `path` is user-supplied
  // and plain JS callers aren't held to its `string[]` type. `message` is a
  // getter, so throwing while formatting would swallow the failure being
  // reported.
  const path = ["a", Symbol("k")] as unknown as string[];
  t.expect(() =>
    S.parseOrThrow(S.string.with(S.refine, () => false, { error: "bad", path }))("x"),
  ).toThrow(
    t.expect.objectContaining({
      name: "SuryError",
      message: "Failed at a[Symbol(k)]: bad",
    }),
  );
});

test("pathToText renders a path the way error messages do", (t) => {
  t.expect(S.pathToText([])).toBe("");
  t.expect(S.pathToText(["user", "tags", 2, "my key", "3", "[]"])).toBe(
    'user.tags[2]["my key"][3][]',
  );
  const error = S.parseAsResult(S.schema({ a: S.string }), { a: 1 }).error!;
  t.expect(error.message).toBe(`Failed at ${S.pathToText(error.path)}: Expected string, received 1`);
});

test("A contradictory bound pair is rejected where it's written", (t) => {
  // The schema would compile and then reject every possible value, which only
  // surfaces in production - so it fails at construction instead. Both sides
  // render through toInputExpression, so the message is in the same syntax the
  // schema is, not the constructor names the caller happened to use.
  t.expect(() => S.number.with(S.gte, 5).with(S.lte, 1)).toThrow(
    `[Sury] number <= 1 contradicts number >= 5`,
  );
  t.expect(() => S.number.with(S.lte, 1).with(S.gte, 5)).toThrow(
    `[Sury] number >= 5 contradicts number <= 1`,
  );
  // Exclusive bounds make the touching cases empty too.
  t.expect(() => S.number.with(S.gt, 5).with(S.lte, 5)).toThrow(
    `[Sury] number <= 5 contradicts number > 5`,
  );
  t.expect(() => S.number.with(S.gte, 5).with(S.lt, 5)).toThrow(
    `[Sury] number < 5 contradicts number >= 5`,
  );
  t.expect(() => S.string.with(S.minLength, 5).with(S.maxLength, 1)).toThrow(
    `[Sury] string.length <= 1 contradicts string.length >= 5`,
  );
  t.expect(() => S.array(S.string).with(S.minLength, 5).with(S.maxLength, 1)).toThrow(
    `[Sury] string[].length <= 1 contradicts string[].length >= 5`,
  );
  // `nonEmpty` desugars to a length bound, and reports as one rather than
  // naming a constructor the caller didn't write.
  t.expect(() => S.string.with(S.minLength, 2).with(S.length, 0)).toThrow(
    `[Sury] string.length <= 0 contradicts string.length >= 2`,
  );
  // A format's range is a bound like any other, so a value outside it conflicts.
  t.expect(() => S.int32.with(S.gte, 3000000000)).toThrow(
    `[Sury] int32 >= 3000000000 contradicts int32 <= 2147483647`,
  );
  t.expect(() => S.port.with(S.lte, -1)).toThrow(
    `[Sury] port <= -1 contradicts port >= 0`,
  );
  // Combining divisors stores their LCM; an LCM past 2^53 rounds and would
  // validate the wrong set, and fractional divisors have no float LCM - both
  // refuse rather than silently drift.
  t.expect(() =>
    S.integer.with(S.multipleOf, 67108859).with(S.multipleOf, 134217689).with(S.multipleOf, 2097143)
  ).toThrow(`[Sury] multipleOf 2097143 cannot be combined with multipleOf 9007195966406851`);
  t.expect(() => S.number.with(S.multipleOf, 0.3).with(S.multipleOf, 0.2)).toThrow(
    `[Sury] multipleOf 0.2 cannot be combined with multipleOf 0.3`,
  );
  // A divisor excluded by the range is NOT a construction error, unlike a
  // pair of bounds: detecting it needs multiples-in-range arithmetic (see the
  // updateBounds comment). The schema builds and rejects everything, with the
  // divisor and the range both in the message.
  t.expect(
    caught(() =>
      S.assertInputOrThrow(S.number.with(S.gt, 0).with(S.lt, 5).with(S.multipleOf, 10), 3)
    )?.message
  ).toBe("Expected 0 < (number % 10) < 5, received 3");

  // A single point is satisfiable, so these stay legal.
  t.expect(S.toInputJSONSchemaOrThrow(S.number.with(S.gte, 5).with(S.lte, 5))).toEqual({
    type: "number",
    minimum: 5,
    maximum: 5,
  });
  t.expect(S.toInputJSONSchemaOrThrow(S.number.with(S.gt, 5).with(S.lt, 6))).toEqual({
    type: "number",
    exclusiveMinimum: 5,
    exclusiveMaximum: 6,
  });
  // A divisor larger than the range still admits 0, and a single point is a
  // point like any other.
  t.expect(S.toInputJSONSchemaOrThrow(S.int32.with(S.multipleOf, 3000000000))).toEqual({
    type: "integer",
    minimum: -2147483648,
    maximum: 2147483647,
    multipleOf: 3000000000,
  });
});

test("A superseded bound takes its message with it", (t) => {
  // The surviving check is the one the caller's message has to reach, so a
  // message written on a bound that doesn't narrow carries onto it...
  t.expect(
    caught(() => S.assertInputOrThrow(S.number.with(S.gte, 5).with(S.gte, 1, "MY MESSAGE"), 3))?.message
  ).toBe("MY MESSAGE");
  // ...and a narrowing replacement without one clears the stale text rather
  // than reporting a bound the schema no longer advertises.
  t.expect(
    caught(() => S.assertInputOrThrow(S.number.with(S.gte, 5, "A").with(S.gte, 10), 7))?.message
  ).toBe("Expected number >= 10, received 7");
  // Switching form replaces the field, so the message keyed to the old form
  // goes with it instead of lingering where nothing reads it.
  const flipped = S.number.with(S.gte, 5, "A").with(S.gt, 10);
  t.expect(flipped.errorMessage?.minimum).toBe(undefined);
  t.expect(
    caught(() => S.assertInputOrThrow(flipped, 7))?.message
  ).toBe("Expected number > 10, received 7");
});

test("An unsatisfiable JSON Schema document loads as never", (t) => {
  // Legal JSON Schema - it just describes a type nothing inhabits - so it has
  // to load rather than fail the way the hand-written equivalent does.
  for (const definition of [
    { type: "number", minimum: 5, maximum: 1 },
    { type: "integer", minimum: 5, maximum: 1 },
    { type: "number", exclusiveMinimum: 5, maximum: 5 },
    { type: "string", minLength: 5, maxLength: 1 },
    { type: "string", minLength: -1 },
    { type: "array", minItems: 5, maxItems: 1 },
    { type: "array", minItems: -1 },
    { type: "array", maxItems: -1 },
    { type: [] },
  ] as const) {
    const schema = S.fromJSONSchemaOrThrow(definition);
    t.expect(S.toInputExpression(schema)).toEqual("never");
  }
});

test("fromJSONSchemaOrThrow: literal bounds narrow the inferred type", () => {
  expectSchemaType(
    S.fromJSONSchemaOrThrow({
      type: "array",
      items: { type: "string" },
      minItems: 2,
      maxItems: 2,
    }),
  ).toBe<[string, string]>();
  expectSchemaType(
    S.fromJSONSchemaOrThrow({ type: "array", items: { type: "string" }, maxItems: 0 }),
  ).toBe<[]>();
  expectSchemaType(
    S.fromJSONSchemaOrThrow({ type: "string", minLength: 0, maxLength: 0 }),
  ).toBe<"">();
  expectSchemaType(
    S.fromJSONSchemaOrThrow({ type: "number", minimum: 5, maximum: 1 }),
  ).toBe<never>();
  expectSchemaType(
    S.fromJSONSchemaOrThrow({ type: "number", exclusiveMinimum: 5, maximum: 5 }),
  ).toBe<never>();
  expectSchemaType(
    S.fromJSONSchemaOrThrow({ type: "string", minLength: 5, maxLength: 1 }),
  ).toBe<never>();
  expectSchemaType(
    S.fromJSONSchemaOrThrow({ type: "array", minItems: 5, maxItems: 1 }),
  ).toBe<never>();
  expectSchemaType(S.fromJSONSchemaOrThrow({ type: [] })).toBe<never>();
});

test("Schema toString prints Schema<input, output>", (t) => {
  t.expect(S.string.toString()).toBe("Schema<string>");
  t.expect(S.to(S.string, S.number).toString()).toBe("Schema<string, number>");
  t.expect(`${S.schema({ a: S.string })}`).toBe("Schema<{ a: string; }>");
  t.expect(`${S.union([S.string, S.number])}`).toBe("Schema<string | number>");

  // Nested transforms only reverse correctly through S.reverse, not a .to walk.
  t.expect(`${S.schema({ a: S.to(S.string, S.number) })}`).toBe(
    "Schema<{ a: string; }, { a: number; }>",
  );

  // The apparent type supplies toString without S.d.ts declaring it.
  expectTypeOf(S.string.toString()).toEqualTypeOf<string>();
});

// The schema prototype is Object.create(null), so before there was a toString
// there was nothing to coerce through and every one of these threw
// "Cannot convert object to primitive value" rather than merely reading badly.
test("Schema survives string coercion", (t) => {
  t.expect(String(S.string)).toBe("Schema<string>");
  t.expect(S.string + "").toBe("Schema<string>");
  t.expect([S.string, S.number].join(", ")).toBe("Schema<string>, Schema<number>");
});

// util.inspect ignores toString, and no inspect hook is registered on purpose,
// so console.log still reveals the internal shape for debugging. Asserted so
// that adding a hook is a deliberate change rather than a silent one.
test("console.log shows the internal schema shape, not the expression", (t) => {
  const dump = inspect(S.string);
  t.expect(dump).not.toBe("Schema<string>");
  t.expect(dump).toContain("type: 'string'");

  // %s formats via toString, which is the opt-in path.
  t.expect(format("%s", S.to(S.string, S.number))).toBe("Schema<string, number>");
});

test("Error messages render through toInputExpression, not toString", (t) => {
  const schema = S.schema({ id: S.string });
  let error: { message: string; reason: string; expected: unknown } | undefined;
  try {
    S.parseOrThrow(schema)({ id: 1 });
  } catch (exn) {
    error = exn as typeof error;
  }

  // No "Schema<…>" wrapper: the message names the type, it does not print the
  // schema object.
  t.expect(error!.message).toBe('Failed at id: Expected string, received 1');
  t.expect(error!.reason).toBe("Expected string, received 1");
  t.expect(`${error}`).toBe(
    'SuryError: Failed at id: Expected string, received 1',
  );

  // The schema hanging off the error is where toString does help.
  t.expect(`${error!.expected}`).toBe("Schema<string>");
});

// Rendering the received value used to walk objects and arrays without a
// limit, so a cyclic input overflowed the stack inside the error formatter - a
// validation failure surfaced as a RangeError instead of a SuryError. One level
// of expansion keeps that fixed: the cycle is reached at depth 1 and named.
test("A cyclic input is reported, not a stack overflow", (t) => {
  const cyclic: Record<string, unknown> = { a: 1 };
  cyclic["self"] = cyclic;

  t.expect(() => S.parseOrThrow(S.string)(cyclic)).toThrow(
    t.expect.objectContaining({
      name: "SuryError",
      message: "Expected string, received { a: 1; self: object; }",
    }),
  );
});

test("A received value is expanded one level", (t) => {
  const reasonFor = (value: unknown): string => {
    try {
      S.parseOrThrow(S.string)(value);
      return "(accepted)";
    } catch (exn) {
      return (exn as { reason: string }).reason;
    }
  };
  const received = (value: unknown) => reasonFor(value).replace("Expected string, received ", "");

  // Primitives keep their value - `received 42` beats `received number` - and
  // bigint keeps its suffix so it stays distinguishable from a number.
  t.expect(received(42)).toBe("42");
  t.expect(received(10n)).toBe("10n");
  t.expect(received(NaN)).toBe("NaN");

  // Plain objects and arrays expand; anything else names its constructor.
  t.expect(received({ a: 1, b: "x" })).toBe('{ a: 1; b: "x"; }');
  t.expect(received([1, "a"])).toBe('[1, "a"]');
  t.expect(received({})).toBe("{}");
  t.expect(received([])).toBe("[]");
  t.expect(received(Object.create(null))).toBe("{}");
  t.expect(received(new Date(0))).toBe("Date");
  t.expect(received(new Map())).toBe("Map");
  t.expect(received(new (class Foo {})())).toBe("Foo");

  // One level only: a nested value names its type instead of recursing, and an
  // array keeps its length because against a tuple that is the diagnostic.
  t.expect(received({ a: 1, meta: { z: 9 } })).toBe("{ a: 1; meta: object; }");
  t.expect(received({ a: 1, tags: [1, 2, 3] })).toBe("{ a: 1; tags: Array(3); }");
  t.expect(received([[1, 2], { a: 1 }])).toBe("[Array(2), object]");

  // Anything without a useful constructor name is lowercase `object`, the same
  // way a primitive is named by its type - a plain object, a null prototype and
  // an anonymous class all read alike, and none of them read as `Object`.
  t.expect(received({ a: Object.create(null) })).toBe("{ a: object; }");
  t.expect(received({ a: new (class {})() })).toBe("{ a: object; }");

  // Width is capped too, or one wide input still produces a huge message.
  t.expect(received(Object.fromEntries(Array.from({ length: 40 }, (_, i) => [i, i])))).toBe(
    "{ 0: 0; 1: 1; 2: 2; 3: 3; 4: 4; ... }",
  );
  t.expect(received([1, 2, 3, 4, 5, 6, 7, 8])).toBe("[1, 2, 3, 4, 5, ...]");
});

// There is no `nan` case in toInputExpression: the sole nan schema always carries
// `const: NaN`, so the `const` branch renders it - via stringify, to the same
// string. Pinned here because removing that branch is only safe while this holds.
test("A nan schema renders as NaN without a dedicated branch", (t) => {
  t.expect(S.toInputExpression(S.schema(NaN))).toBe("NaN");
  t.expect(`${S.schema(NaN)}`).toBe("Schema<NaN>");
});

// A literal length on an array pins arity in the type (specs/array-length,
// specs/array-empty, specs/string-empty pin the direct cases). Pinned here are
// the fallbacks a spec can't express: a non-literal bound narrows nothing, and
// past 64 the tuple spelling bails to the unbounded type instead of hitting
// TS's recursion ceiling - both must stay `string[]`, not become errors.
test("Array length type pinning falls back to the unbounded type", () => {
  expectSchemaType(S.array(S.string).with(S.length, 2)).toBe<[string, string]>();
  expectSchemaType(S.length(S.array(S.boolean), 3)).toBe<[boolean, boolean, boolean]>();
  expectSchemaType(S.array(S.number).with(S.length, 0)).toBe<[]>();
  expectSchemaType(S.string.with(S.length, 0)).toBe<"">();
  // length picks up an earlier bound's subsumption unchanged
  expectSchemaType(S.array(S.string).with(S.minLength, 1).with(S.length, 2)).toBe<
    [string, string]
  >();
  // On an already-pinned arity the bound is redundant and must change nothing:
  // rebuilding a tuple from the union of its elements would widen
  // `["bar", number]` into `[number | "bar", number | "bar"]`.
  expectSchemaType(S.tuple(["bar", S.number]).with(S.length, 2)).toBe<["bar", number]>();
  expectSchemaType(S.array(S.string).with(S.length, 2).with(S.length, 2)).toBe<
    [string, string]
  >();

  const n: number = 2;
  expectSchemaType(S.array(S.string).with(S.length, n)).toBe<string[]>();
  expectSchemaType(S.array(S.string).with(S.length, 100)).toBe<string[]>();
  expectSchemaType(S.length(S.array(S.string), 1e6)).toBe<string[]>();

  // Never called, and the bound has to arrive as a parameter: `S.length` raises
  // on a bound no value can satisfy, and a `const` initialized to a literal is
  // narrowed to it, so a local would test the literal case over again.
  const _typeOnly = (union: 0 | 2) => {
    // A bound that isn't one literal resolves per member rather than letting
    // the smallest match stand for all of them.
    expectSchemaType(S.array(S.string).with(S.length, union)).toBe<[string, string] | []>();
    // A tuple built by recursing until it matched would report an unsatisfiable
    // bound as a compile error on the recursion limit, not as the runtime error
    // it already is.
    expectSchemaType(S.length(S.array(S.string), -1)).toBe<string[]>();
    expectSchemaType(S.length(S.array(S.string), 2.5)).toBe<string[]>();
  };
});

// A lower bound fixes a head and leaves the tail open. specs/array-nonEmpty and
// specs/array-minLength pin the direct cases; what needs pinning here is that it
// only has something to say while the tail *is* open - every no-op the runtime
// makes of a redundant bound the type has to make too, or the two disagree about
// a schema that compiled fine.
test("A lower bound only widens an array whose length is still open", () => {
  expectSchemaType(S.array(S.string).with(S.nonEmpty)).toBe<[string, ...string[]]>();
  expectSchemaType(S.minLength(S.array(S.number), 2)).toBe<[number, number, ...number[]]>();
  // Stacking lower bounds keeps the strictest, as the runtime does.
  expectSchemaType(S.array(S.string).with(S.minLength, 1).with(S.minLength, 3)).toBe<
    [string, string, string, ...string[]]
  >();
  // Already pinned to an arity: `narrowsSize` drops the bound outright, so the
  // type must not widen back to `[string, ...string[]]` either.
  expectSchemaType(S.array(S.string).with(S.length, 2).with(S.minLength, 1)).toBe<
    [string, string]
  >();
  // A zero lower bound is no bound at all.
  expectSchemaType(S.array(S.string).with(S.minLength, 0)).toBe<string[]>();
  expectSchemaType(S.array(S.string).with(S.minLength, 100)).toBe<string[]>();

  // TypeScript counts tuple elements, not characters, so a string keeps its type
  // under every lower bound - only the exact bound reaches `""`, the one
  // length with a literal to name it.
  expectSchemaType(S.string.with(S.nonEmpty)).toBe<string>();
  expectSchemaType(S.string.with(S.minLength, 3)).toBe<string>();
});

// The bound binds the array, and a codec's input is a different value reachable
// from it - pinning the array's arity says nothing about the string it decodes
// from, which is why the input side is rewritten only when it is the same type.
test("A length bound leaves the other side of a codec alone", () => {
  const csv = S.string.with(S.to, S.array(S.string), {
    decode: (s) => s.split(","),
    encode: (a) => a.join(","),
  });
  expectSchemaType(csv.with(S.length, 0)).toBe<string, []>();
  expectSchemaType(csv.with(S.length, 2)).toBe<string, [string, string]>();
  expectSchemaType(csv.with(S.nonEmpty)).toBe<string, [string, ...string[]]>();
  expectSchemaType(csv.with(S.minLength, 2)).toBe<string, [string, string, ...string[]]>();
});

// The runtime tags a spec cannot record: specs snapshot codegen and types,
// not the introspectable schema object itself.
test("Schema introspection tags survive on coerced and instance schemas", (t) => {
  t.expect(S.jsonString.type === "string" && S.jsonString.format === "json").toBe(true);

  const coerced = S.to(S.string, S.number);
  t.expect(coerced.to).toBe(S.number);

  const portFromString = S.string.with(S.to, S.port);
  t.expect(portFromString.type === "string" && portFromString.format === undefined).toBe(true);
  const reversedPort = S.reverse(portFromString);
  t.expect(reversedPort.type === "number" && (reversedPort as { format?: string }).format === "port").toBe(true);

  const setSchema = S.instance(Set);
  t.expect(setSchema.type === "instance" && setSchema.class === Set).toBe(true);
});

// `schemaOf` is checked by the type system, so half of its contract is what
// must NOT compile - a spec snapshots a schema that exists, and can't express
// a rejection. `object-in-object3` and `codec-string-date` carry the positive
// half as `ts.aliases`, which is what proves the definition builds the very
// same schema as the inferred spelling.
test("schemaOf: builds a schema against a type the consumer already has", (t) => {
  type User = {
    id: string;
    name: string;
    age?: number;
    createdAt: Date;
    role: "admin" | "user";
  };

  const userSchema = S.schemaOf<User>()({
    id: S.string,
    name: S.string,
    age: S.optional(S.number),
    createdAt: S.date,
    role: S.union(["admin", "user"]),
  });

  // The output side is named rather than expanded: it was checked equal to
  // `User`, so it can be reported as `User`.
  expectTypeOf(userSchema).toEqualTypeOf<S.Schema<User, User>>();

  const user = {
    id: "u1",
    name: "Bob",
    createdAt: new Date("2024-01-01T00:00:00.000Z"),
    role: "admin" as const,
  };
  t.expect(S.parseOrThrow(userSchema)(user)).toEqual({ ...user, age: undefined });

  // The encoded side is read off the definition, so a codec needs no second
  // type argument.
  type UserRow = {
    id: string;
    name: string;
    age?: number;
    createdAt: string;
    role: "admin" | "user";
  };
  const rowSchema = S.schemaOf<User>()({
    id: S.string,
    name: S.string,
    age: S.optional(S.number),
    createdAt: S.isoDateTime.with(S.to, S.date),
    role: S.union(["admin", "user"]),
  });
  expectTypeOf(rowSchema).toEqualTypeOf<S.Schema<UserRow, User>>();
  t.expect(
    S.parseOrThrow(rowSchema)({ ...user, createdAt: "2024-01-01T00:00:00.000Z" })
  ).toEqual({ ...user, age: undefined });

  // A field the type declares optional, defined by a schema that requires it.
  // The definition's output is `number` where the type reads `number |
  // undefined` - assignable, so only an equality check sees it.
  S.schemaOf<User>()({
    id: S.string,
    name: S.string,
    // @ts-expect-error - `age?: number` needs S.optional, or the schema rejects
    // a value the type calls valid
    age: S.number,
    createdAt: S.date,
    role: S.union(["admin", "user"]),
  });

  S.schemaOf<User>()({
    // @ts-expect-error - the type declares `id` a string
    id: S.number,
    name: S.string,
    age: S.optional(S.number),
    createdAt: S.date,
    role: S.union(["admin", "user"]),
  });

  // @ts-expect-error - `name`, `age` and `role` are missing
  S.schemaOf<User>()({ id: S.string, createdAt: S.date });

  S.schemaOf<User>()({
    id: S.string,
    name: S.string,
    age: S.optional(S.number),
    createdAt: S.date,
    role: S.union(["admin", "user"]),
    // @ts-expect-error - the type declares no `oops`
    oops: S.string,
  });

  S.schemaOf<{ role: "admin" | "user" }>()({
    // @ts-expect-error - "guest" is not one of the declared members
    role: S.union(["admin", "guest"]),
  });

  // Narrower than declared is a rejection for the same reason as the optional
  // case: the schema would refuse values the type admits.
  S.schemaOf<{ id: string }>()({
    // @ts-expect-error - the type declares `string`, not the one literal
    id: S.schema("fixed"),
  });
});

test("schemaOf: definitions that aren't a plain fields object", (t) => {
  // A whole schema as the definition - how a union, or anything else with no
  // fields object, is named.
  type Shape = { kind: "circle"; r: number } | { kind: "square"; side: number };
  const shape = S.schemaOf<Shape>()(
    S.union([
      S.schema({ kind: "circle", r: S.number }),
      S.schema({ kind: "square", side: S.number }),
    ])
  );
  expectTypeOf(shape).toEqualTypeOf<S.Schema<Shape, Shape>>();
  t.expect(S.parseOrThrow(shape)({ kind: "circle", r: 1 })).toEqual({
    kind: "circle",
    r: 1,
  });

  S.schemaOf<Shape>()(
    // @ts-expect-error - `r` is a number in the declared union
    S.union([
      S.schema({ kind: "circle", r: S.string }),
      S.schema({ kind: "square", side: S.number }),
    ])
  );

  // A literal field is spelled as the value itself, same as in `S.schema`.
  const tagged = S.schemaOf<{ kind: "circle"; r: number }>()({
    kind: "circle",
    r: S.number,
  });
  expectTypeOf(tagged).toEqualTypeOf<
    S.Schema<{ kind: "circle"; r: number }, { kind: "circle"; r: number }>
  >();

  // @ts-expect-error - the type declares the tag "circle"
  S.schemaOf<{ kind: "circle"; r: number }>()({ kind: "square", r: S.number });

  // A tuple definition is compared whole: `keyof` a tuple carries every array
  // method, so there is no field list to walk.
  const pair = S.schemaOf<[string, number]>()([S.string, S.number]);
  expectTypeOf(pair).toEqualTypeOf<S.Schema<[string, number], [string, number]>>();
  t.expect(S.parseOrThrow(pair)(["a", 1])).toEqual(["a", 1]);

  // @ts-expect-error - the second element is declared a number
  S.schemaOf<[string, number]>()([S.string, S.string]);

  const list = S.schemaOf<string[]>()(S.array(S.string));
  expectTypeOf(list).toEqualTypeOf<S.Schema<string[], string[]>>();

  type Node = { id: string; children: Node[] };
  const node = S.schemaOf<Node>()(
    S.recursive<Node>("Node", (node) =>
      S.schema({ id: S.string, children: S.array(node) })
    )
  );
  expectTypeOf(node).toEqualTypeOf<S.Schema<Node, Node>>();
  t.expect(S.parseOrThrow(node)({ id: "a", children: [] })).toEqual({
    id: "a",
    children: [],
  });

  // A `readonly` array or tuple has no `readonly` schema to match it, so the
  // comparison drops the modifier rather than asking for one.
  type Team = {
    name: string;
    members: { id: string }[];
    tags: Record<string, number>;
    at: readonly [number, number];
  };
  expectTypeOf(
    S.schemaOf<Team>()({
      name: S.string,
      members: S.array(S.schema({ id: S.string })),
      tags: S.record(S.number),
      at: S.tuple([S.number, S.number]),
    })
  ).toEqualTypeOf<
    S.Schema<
      {
        name: string;
        members: { id: string }[];
        tags: Record<string, number>;
        at: [number, number];
      },
      Team
    >
  >();
});

// No schema produces a `readonly` type, so a target that declares one has the
// modifier dropped before the comparison rather than being unsatisfiable. Two
// mechanisms do it between them: `Mutable` for arrays and tuples, and the
// per-field walk for properties, which reads `TOutput[K]` and so never sees a
// property modifier at all. The result names the target - `readonly` and all -
// while the encoded side, which is what the definition actually builds, stays
// mutable.
test("schemaOf: a target type that declares readonly", (t) => {
  type Tagged = { readonly id: string; readonly tags: readonly string[] };
  const tagged = S.schemaOf<Tagged>()({ id: S.string, tags: S.array(S.string) });
  expectTypeOf(tagged).toEqualTypeOf<
    S.Schema<{ id: string; tags: string[] }, Tagged>
  >();
  t.expect(S.parseOrThrow(tagged)({ id: "a", tags: ["x"] })).toEqual({
    id: "a",
    tags: ["x"],
  });

  type Wrapped = Readonly<{ id: string; nested: { readonly n: number } }>;
  expectTypeOf(
    S.schemaOf<Wrapped>()({ id: S.string, nested: { n: S.number } })
  ).toEqualTypeOf<S.Schema<{ id: string; nested: { n: number } }, Wrapped>>();

  type Deep = { outer: { items: readonly number[] } };
  expectTypeOf(
    S.schemaOf<Deep>()({ outer: { items: S.array(S.number) } })
  ).toEqualTypeOf<S.Schema<{ outer: { items: number[] } }, Deep>>();

  type Pair = readonly [string, number];
  expectTypeOf(S.schemaOf<Pair>()([S.string, S.number])).toEqualTypeOf<
    S.Schema<[string, number], Pair>
  >();

  // Dropping `readonly` doesn't drop the check underneath it.
  // @ts-expect-error - the type declares `readonly string[]`, not numbers
  S.schemaOf<Tagged>()({ id: S.string, tags: S.array(S.number) });
});

// The first call takes only a type argument, so an inferring `S.schema` call
// can't reach `schemaOf`'s checking at all. These pin that inference is
// untouched by it.
test("schemaOf: leaves inference alone", () => {
  expectTypeOf(S.schema("tuna")).toEqualTypeOf<S.Schema<"tuna", "tuna">>();
  expectTypeOf(S.schema(12)).toEqualTypeOf<S.Schema<12, 12>>();
  expectTypeOf(S.schema({ id: S.string })).toEqualTypeOf<
    S.Schema<{ id: string }, { id: string }>
  >();
  expectTypeOf(S.schema([S.string, S.number])).toEqualTypeOf<
    S.Schema<[string, number], [string, number]>
  >();
});
