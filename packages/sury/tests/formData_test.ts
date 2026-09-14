import { expect, test } from "vitest";
import * as S from "sury";
import { withoutGlobalRoutes } from "./withoutGlobal";

// The value side of `S.formData`, for what a `codec-formdata-*` spec can't
// write down: an example records an input and a result, so a rejection raised
// in *both* directions, a value read back through its own encode, and an
// encode that writes into the value it was handed all need a test. Codegen and
// everything one operation can show stay in the specs.
//
// `pnpm --filter=sury fuzz:formdata` crosses the field shapes and checks the
// same properties over all of them; a case it turns up lands here or in a spec.

const form = (...entries: [string, string | Blob][]): FormData => {
  const f = new FormData();
  for (const [key, value] of entries) f.append(key, value);
  return f;
};

const entries = (f: FormData): [string, string | File][] => [...f.entries()];

test("an encode appends one entry per field, in field order, as text", () => {
  const schema = S.formData.with(
    S.to,
    S.schema({
      name: S.string.with(S.nonEmpty),
      age: S.number,
      agree: S.boolean,
      kind: "signup",
      since: S.date,
      id: S.bigint,
      site: S.url,
    }),
  );
  const encoded = S.encodeOrThrow(schema)({
    name: "Ann",
    age: 42,
    agree: true,
    kind: "signup",
    since: new Date("2024-01-01T00:00:00.000Z"),
    id: 7n,
    site: new URL("https://sury.dev/"),
  });
  expect(encoded).toBeInstanceOf(FormData);
  expect(entries(encoded)).toEqual([
    ["name", "Ann"],
    ["age", "42"],
    ["agree", "on"],
    ["kind", "signup"],
    ["since", "2024-01-01T00:00:00.000Z"],
    ["id", "7"],
    ["site", "https://sury.dev/"],
  ]);
  // The same schema reads the entries back, coercions included.
  expect(S.decodeOrThrow(schema)(encoded)).toEqual({
    name: "Ann",
    age: 42,
    agree: true,
    kind: "signup",
    since: new Date("2024-01-01T00:00:00.000Z"),
    id: 7n,
    site: new URL("https://sury.dev/"),
  });
});

test("an absent optional is no entry, and a default fills the absent one back", () => {
  const schema = S.formData.with(
    S.to,
    S.schema({ nick: S.optional(S.string), age: S.optional(S.number, 18) }),
  );
  expect(entries(S.encodeOrThrow(schema)({ age: 18 }))).toEqual([["age", "18"]]);
  expect(entries(S.encodeOrThrow(schema)({ nick: "nn", age: 42 }))).toEqual([
    ["nick", "nn"],
    ["age", "42"],
  ]);
  expect(S.decodeOrThrow(schema)(new FormData())).toEqual({ nick: undefined, age: 18 });
  // The empty text input is the absent one.
  expect(S.decodeOrThrow(schema)(form(["nick", ""], ["age", ""]))).toEqual({ nick: undefined, age: 18 });
});

test("a boolean is a checkbox: on when set, nothing when not", () => {
  const schema = S.formData.with(S.to, S.schema({ agree: S.boolean, notify: S.optional(S.boolean) }));
  // An unchecked box sends nothing, which is all the entry list says about
  // `false` - so that is what an encode writes.
  expect(entries(S.encodeOrThrow(schema)({ agree: false }))).toEqual([]);
  expect(entries(S.encodeOrThrow(schema)({ agree: true, notify: false }))).toEqual([
    ["agree", "on"],
    // A tri-state is the one thing the wire cannot express, so `false` is
    // spelled out there to keep it apart from absent.
    ["notify", "false"],
  ]);
  expect(S.decodeOrThrow(schema)(S.encodeOrThrow(schema)({ agree: false, notify: true }))).toEqual({
    agree: false,
    notify: true,
  });
  expect(S.decodeOrThrow(schema)(form(["agree", "on"]))).toEqual({ agree: true, notify: undefined });
  expect(S.decodeOrThrow(schema)(new FormData())).toEqual({ agree: false, notify: undefined });
});

test("a checkbox reads the entries a form can carry, and only those", () => {
  const schema = S.formData.with(S.to, S.schema({ a: S.boolean }));
  // "on" is what a checked box submits; "true"/"false" is what a hidden input
  // carries.
  for (const [entry, value] of [
    ["on", true],
    ["true", true],
    ["false", false],
    // A checked box whose value is "" is indistinguishable from an unchecked
    // one on this wire.
    ["", false],
  ] as const) {
    expect(S.decodeOrThrow(schema)(form(["a", entry])), entry).toEqual({ a: value });
  }
  // Anything else is a checkbox with a `value` attribute, which is a string
  // the schema should name rather than a boolean the codec guesses at.
  expect(() => S.decodeOrThrow(schema)(form(["a", "yes"]))).toThrow(
    'Failed at a: Expected boolean, received "yes"',
  );
});

test("a nullable field reads a blank entry as null, and omits null on the way out", () => {
  const schema = S.formData.with(
    S.to,
    S.schema({ nick: S.nullable(S.string), age: S.nullable(S.number) }),
  );
  expect(S.decodeOrThrow(schema)(form(["nick", ""], ["age", ""]))).toEqual({ nick: null, age: null });
  expect(S.decodeOrThrow(schema)(new FormData())).toEqual({ nick: null, age: null });
  expect(S.decodeOrThrow(schema)(form(["nick", "nn"], ["age", "42"]))).toEqual({ nick: "nn", age: 42 });
  // `null` is not an entry, so it is omitted - and reads back as null.
  expect(entries(S.encodeOrThrow(schema)({ nick: null, age: null }))).toEqual([]);
  expect(entries(S.encodeOrThrow(schema)({ nick: "nn", age: 42 }))).toEqual([
    ["nick", "nn"],
    ["age", "42"],
  ]);
  expect(S.decodeOrThrow(schema)(S.encodeOrThrow(schema)({ nick: null, age: 7 }))).toEqual({
    nick: null,
    age: 7,
  });
  // The literal text "null" is a string, not the null a blank field means.
  expect(S.decodeOrThrow(schema)(form(["nick", "null"]))).toEqual({ nick: "null", age: null });
});

test("a blank required string must say what it means", () => {
  const ambiguous = ['Failed at f: Ambiguous "" for', "S.nonEmpty", "S.minLength(0)", "S.optional"];
  for (const schema of [S.string, S.string.with(S.maxLength, 100)]) {
    for (const fragment of ambiguous) {
      expect(() => S.decodeOrThrow(S.formData.with(S.to, S.schema({ f: schema })))).toThrow(fragment);
    }
  }
  // Every spelling the message names, plus the ones that answer on their own.
  for (const [name, schema] of [
    ["nonEmpty", S.string.with(S.nonEmpty)],
    ["minLength(0)", S.string.with(S.minLength, 0)],
    ["optional", S.optional(S.string)],
    ["nullable", S.nullable(S.string)],
    ["a format", S.email],
    ["a literal", S.schema("x")],
    ["a pattern rejecting blank", S.string.with(S.pattern, /^\d+$/)],
    ["a conversion", S.string.with(S.to, S.date)],
  ] as const) {
    expect(() => S.decodeOrThrow(S.formData.with(S.to, S.schema({ f: schema }))), name).not.toThrow();
  }
  // A pattern that matches "" says nothing about it, so it stays ambiguous.
  expect(() =>
    S.decodeOrThrow(S.formData.with(S.to, S.schema({ f: S.string.with(S.pattern, /^\d*$/) }))),
  ).toThrow('Failed at f: Ambiguous "" for');
  // Encoding never reads a blank entry, so it has nothing to be ambiguous about.
  expect(() => S.encodeOrThrow(S.formData.with(S.to, S.schema({ f: S.string })))).not.toThrow();
});

test("an array is a repeated key, and an empty array is no entry", () => {
  const schema = S.formData.with(S.to, S.schema({ tags: S.array(S.string), ids: S.array(S.number) }));
  expect(entries(S.encodeOrThrow(schema)({ tags: ["a", "b"], ids: [1, 2] }))).toEqual([
    ["tags", "a"],
    ["tags", "b"],
    ["ids", "1"],
    ["ids", "2"],
  ]);
  expect(entries(S.encodeOrThrow(schema)({ tags: [], ids: [] }))).toEqual([]);
  expect(S.decodeOrThrow(schema)(form(["ids", "1"], ["tags", "x"], ["ids", "2"]))).toEqual({
    tags: ["x"],
    ids: [1, 2],
  });
});

test("a file travels as itself, name included, and a blob becomes a file", async () => {
  const schema = S.formData.with(
    S.to,
    S.schema({ avatar: S.file, cover: S.optional(S.file), raw: S.blob }),
  );
  const avatar = new File(["a"], "a.png", { type: "image/png" });
  const encoded = S.encodeOrThrow(schema)({ avatar, raw: new Blob(["r"]) });
  const sent = entries(encoded) as [string, File][];
  const sentAvatar = sent[0]![1];
  const sentRaw = sent[1]![1];
  expect(sentAvatar.name).toBe("a.png");
  expect(sentAvatar.type).toBe("image/png");
  expect(await sentAvatar.text()).toBe("a");
  // `append` wraps a bare blob in a File, which is what `S.blob` still accepts.
  expect(sentRaw).toBeInstanceOf(File);
  expect(await sentRaw.text()).toBe("r");
  const decoded = S.decodeOrThrow(schema)(encoded);
  expect(decoded.avatar).toBe(sentAvatar);
  expect(decoded.cover).toBe(undefined);
  expect(decoded.raw).toBe(sentRaw);
});

test("a multi-file input is an array of entries, both ways", () => {
  const schema = S.formData.with(S.to, S.schema({ files: S.array(S.file) }));
  const a = new File(["a"], "a.png");
  const b = new File(["b"], "b.png");
  expect(entries(S.encodeOrThrow(schema)({ files: [a, b] }))).toEqual([
    ["files", a],
    ["files", b],
  ]);
  expect(S.decodeOrThrow(schema)(form(["files", a], ["files", b]))).toEqual({ files: [a, b] });
  expect(S.decodeOrThrow(schema)(new FormData())).toEqual({ files: [] });
  expect(() => S.decodeOrThrow(schema)(form(["files", "x"]))).toThrow(
    "Failed at files[0]: Expected File, received \"x\"",
  );
});

test("an array of union items encodes without leaking a declaration", () => {
  // The item's own `let` used to land after the loop body that reads it, so
  // the compiled encoder threw `ReferenceError` on its first item.
  const schema = S.formData.with(S.to, S.schema({ m: S.array(S.union(["a", "b"])) }));
  expect(entries(S.encodeOrThrow(schema)({ m: ["a", "b", "a"] }))).toEqual([
    ["m", "a"],
    ["m", "b"],
    ["m", "a"],
  ]);
});

test("a list carries neither a hole nor a list, in either direction", () => {
  // A repeated key is flat and positional: an item with no entry would shift
  // the ones after it, and a list of lists would run them together. Both
  // directions reject rather than silently reshape - an encoder that only ever
  // encodes would otherwise drop the holes and never hear about it.
  const holes = S.formData.with(S.to, S.schema({ m: S.array(S.optional(S.string)) }));
  const positional = (list: string) =>
    `Failed at m: Can't decode form field -> ${list}. A repeated key is positional, so every item needs an entry`;
  expect(() => S.encodeOrThrow(holes)({ m: ["a", undefined] })).toThrow(positional("(string | undefined)[]"));
  expect(() => S.decodeOrThrow(holes)(new FormData())).toThrow(positional("(string | undefined)[]"));
  const nullableItems = S.formData.with(S.to, S.schema({ m: S.array(S.nullable(S.string)) }));
  expect(() => S.encodeOrThrow(nullableItems)({ m: [null] })).toThrow(positional("(string | null)[]"));
  const slots = S.formData.with(S.to, S.schema({ m: S.schema([S.string, S.optional(S.string)]) }));
  expect(() => S.encodeOrThrow(slots)({ m: ["a", undefined] })).toThrow(positional("[string, string | undefined]"));

  const nested = S.formData.with(S.to, S.schema({ n: S.array(S.array(S.string)) }));
  const flat = "Failed at n: Can't decode form field -> string[][]. A repeated key is flat";
  expect(() => S.encodeOrThrow(nested)({ n: [["a"], ["b"]] })).toThrow(flat);
  expect(() => S.decodeOrThrow(nested)(new FormData())).toThrow(flat);
});

test("a checkbox round-trips however the field is wrapped", () => {
  for (const [name, schema] of [
    ["required", S.boolean],
    ["optional", S.optional(S.boolean)],
    ["defaulted false", S.optional(S.boolean, false)],
  ] as const) {
    const s = S.formData.with(S.to, S.schema({ a: schema }));
    for (const value of [true, false]) {
      expect(S.decodeOrThrow(s)(S.encodeOrThrow(s)({ a: value })), `${name} ${value}`).toEqual({ a: value });
    }
    // What a browser actually submits for a checked and an unchecked box.
    expect(S.decodeOrThrow(s)(form(["a", "on"])), name).toEqual({ a: true });
    expect(S.decodeOrThrow(s)(new FormData()), name).toEqual({ a: name === "optional" ? undefined : false });
  }
});

test("a boolean literal is the must-be-checked box", () => {
  // The terms-and-conditions checkbox, which submits "on" like any other and
  // reports what the browser sent when it is not ticked.
  const schema = S.formData.with(S.to, S.schema({ terms: S.schema(true) }));
  expect(S.decodeOrThrow(schema)(form(["terms", "on"]))).toEqual({ terms: true });
  expect(entries(S.encodeOrThrow(schema)({ terms: true }))).toEqual([["terms", "on"]]);
  for (const [fd, received] of [
    [new FormData(), "undefined"],
    [form(["terms", "false"]), '"false"'],
    [form(["terms", "0"]), '"0"'],
  ] as const) {
    expect(() => S.decodeOrThrow(schema)(fd)).toThrow(`Failed at terms: Expected true, received ${received}`);
  }
  // And its mirror, for a box that must stay clear.
  const clear = S.formData.with(S.to, S.schema({ spam: S.schema(false) }));
  expect(S.decodeOrThrow(clear)(new FormData())).toEqual({ spam: false });
  expect(entries(S.encodeOrThrow(clear)({ spam: false }))).toEqual([]);
  expect(() => S.decodeOrThrow(clear)(form(["spam", "on"]))).toThrow(
    'Failed at spam: Expected false, received "on"',
  );
});

test("a union arm reads by its own rule, not the field's", () => {
  // The reading belongs to the entry, so a boolean beside a number still gets
  // the checkbox spellings - the hook is consulted once per arm.
  const schema = S.formData.with(S.to, S.schema({ a: S.union([S.boolean, S.number]) }));
  const d = S.decodeOrThrow(schema);
  expect(d(form(["a", "on"]))).toEqual({ a: true });
  expect(d(form(["a", "1"]))).toEqual({ a: 1 });
  expect(d(form(["a", "false"]))).toEqual({ a: false });
  expect(d(form(["a", "42"]))).toEqual({ a: 42 });
  expect(() => d(form(["a", "x"]))).toThrow("Expected boolean | number");
});

test("a list of booleans is not something a form can send", () => {
  // A checkbox is a whole field, and a group of them submits the *value* of
  // each checked box - never `"on"` per position. Nothing a browser produces
  // reads as a boolean list, so it is refused rather than given a reading of
  // its own.
  for (const schema of [S.array(S.boolean), S.tuple([S.string, S.boolean])]) {
    expect(() => S.decodeOrThrow(S.formData.with(S.to, S.schema({ flags: schema })))).toThrow(
      "A checkbox group sends the value of each checked box, so read it as string[]",
    );
  }
  // The group a browser does send is a list of the checked values.
  const group = S.formData.with(S.to, S.schema({ tags: S.array(S.string) }));
  expect(S.decodeOrThrow(group)(form(["tags", "ts"], ["tags", "go"]))).toEqual({
    tags: ["ts", "go"],
  });
});

test("a repeated key fills a tuple, one entry per slot", () => {
  // A tuple is the fixed-length case of the same positional read, and its own
  // length check reports a form that sent the wrong number of them.
  const schema = S.formData.with(S.to, S.schema({ at: S.tuple([S.string.with(S.nonEmpty), S.number]) }));
  expect(entries(S.encodeOrThrow(schema)({ at: ["x", 42] }))).toEqual([
    ["at", "x"],
    ["at", "42"],
  ]);
  expect(S.decodeOrThrow(schema)(form(["at", "x"], ["at", "42"]))).toEqual({ at: ["x", 42] });
  expect(() => S.decodeOrThrow(schema)(form(["at", "x"]))).toThrow(
    'Failed at at: Expected [string.length >= 1, number], received ["x"]',
  );
});

test("a repeated key of a union item encodes once per item", () => {
  // The conversion merges its own chain and the loop merges the item, so
  // compiling both on the same val emitted the union's dispatch `let` twice
  // and the operation failed to build at all.
  const schema = S.formData.with(S.to, S.schema({ picks: S.array(S.union(["a", "b"])) }));
  expect(entries(S.encodeOrThrow(schema)({ picks: ["a", "b", "a"] }))).toEqual([
    ["picks", "a"],
    ["picks", "b"],
    ["picks", "a"],
  ]);
  expect(S.decodeOrThrow(schema)(form(["picks", "a"], ["picks", "b"]))).toEqual({ picks: ["a", "b"] });
});

test("a nullable checkbox reads an absent box as null, and spells its false out", () => {
  // Without the first the `null` arm would be unreachable: nothing a form
  // submits reads as null, so absence is the only thing left to carry it -
  // which is what makes the second necessary. Omitting `false` the way a
  // browser does would hand it back as `null`.
  const schema = S.formData.with(S.to, S.schema({ a: S.nullable(S.boolean) }));
  expect(S.decodeOrThrow(schema)(new FormData())).toEqual({ a: null });
  expect(S.decodeOrThrow(schema)(form(["a", "on"]))).toEqual({ a: true });
  expect(entries(S.encodeOrThrow(schema)({ a: null }))).toEqual([]);
  for (const value of [true, false] as const) {
    expect(S.decodeOrThrow(schema)(S.encodeOrThrow(schema)({ a: value }))).toEqual({ a: value });
  }
});

test("a checkbox defaulting to true cannot round-trip, because the wire disagrees", () => {
  // An absent checkbox entry means unchecked, so a default of `true` states
  // something the wire never says. The encode omits `false` like a browser
  // does, and the decode then applies that default. Documented rather than
  // worked around: the schema is what contradicts the medium.
  const schema = S.formData.with(S.to, S.schema({ a: S.optional(S.boolean, true) }));
  expect(entries(S.encodeOrThrow(schema)({ a: false }))).toEqual([]);
  expect(S.decodeOrThrow(schema)(S.encodeOrThrow(schema)({ a: false }))).toEqual({ a: true });
});

test("encoding a tuple of unions does not write into the caller's array", () => {
  const schema = S.formData.with(S.to, S.schema({ a: S.schema([S.union([S.boolean, S.number])]) }));
  const input = { a: [true] as [boolean | number] };
  S.encodeOrThrow(schema)(input);
  expect(input).toEqual({ a: [true] });
});

test("a repeated key reaching a field declared once is reported, not resolved", () => {
  // Parameter pollution: a client can send a key twice for a field the schema
  // says holds one value. `get` would answer the first and say nothing, and
  // which one that is depends on submission order - so the pair is handed over
  // and the field's own check reports it.
  const schema = S.formData.with(S.to, S.schema({ name: S.string.with(S.nonEmpty) }));
  expect(() => S.decodeOrThrow(schema)(form(["name", "first"], ["name", "second"]))).toThrow(
    'Failed at name: Expected string.length >= 1, received ["first", "second"]',
  );
  // One entry is still one value, and a list takes it as a one-item list.
  expect(S.decodeOrThrow(schema)(form(["name", "only"]))).toEqual({ name: "only" });
});

test("a file entry in a text field is reported as the file it is", () => {
  const schema = S.formData.with(S.to, S.schema({ name: S.string.with(S.nonEmpty) }));
  expect(() => S.decodeOrThrow(schema)(form(["name", new File(["x"], "a.txt")]))).toThrow(
    "Failed at name: Expected string.length >= 1, received File",
  );
});

test("what a text input reaching S.number is read as", () => {
  // A form has no number type, so every one of these is a string a user can
  // type into a field the schema calls a number. `+text` is the reading, which
  // is broader than most people expect at both ends.
  const schema = S.formData.with(S.to, S.schema({ n: S.number }));
  for (const [text, value] of [
    ["42", 42],
    ["  42  ", 42],
    ["42.00", 42],
    ["+42", 42],
    [".5", 0.5],
    ["1e5", 100000],
    ["0x10", 16],
    ["Infinity", Infinity],
  ] as const) {
    expect(S.decodeOrThrow(schema)(form(["n", text])), text).toEqual({ n: value });
  }
  for (const text of ["42abc", "1_000", "NaN", "", " "]) {
    expect(() => S.decodeOrThrow(schema)(form(["n", text])), text).toThrow("Expected number");
  }
});

test("a nested document is a JSON text field, both ways", () => {
  const schema = S.formData.with(
    S.to,
    S.schema({ prefs: S.jsonString.with(S.to, S.schema({ theme: S.string, size: S.number })) }),
  );
  const encoded = S.encodeOrThrow(schema)({ prefs: { theme: "dark", size: 2 } });
  expect(entries(encoded)).toEqual([["prefs", `{"theme":"dark","size":2}`]]);
  expect(S.decodeOrThrow(schema)(encoded)).toEqual({ prefs: { theme: "dark", size: 2 } });
});

test("the reverse is spelled the same as jsonString's", () => {
  const user = S.schema({ name: S.string.with(S.nonEmpty), age: S.number });
  const value = { name: "Ann", age: 42 };
  expect(entries(S.encodeOrThrow(user, S.formData)(value))).toEqual([
    ["name", "Ann"],
    ["age", "42"],
  ]);
  expect(S.decodeOrThrow(S.formData, user)(S.encodeOrThrow(user, S.formData)(value))).toEqual(value);
  expect(entries(S.parseOrThrow(user.with(S.to, S.formData))(value))).toEqual([
    ["name", "Ann"],
    ["age", "42"],
  ]);
});

test("a runtime without FormData says so on every route into the schema", () => {
  const message = "[Sury] S.formData is not supported in this runtime";
  expect(
    withoutGlobalRoutes("FormData", [
      `S.parseOrThrow(S.formData)`,
      `S.toInputExpression(S.formData)`,
      `S.encodeOrThrow(S.schema({ a: S.string }), S.formData)`,
      `S.decodeOrThrow(S.formData, S.schema({ a: S.string }))`,
      // And the sibling the runtime does have is untouched.
      `typeof S.parseOrThrow(S.file)`,
    ]),
  ).toEqual([message, message, message, message, "ok:function"]);
});

// A spec example is sync; an async arm inside a wrapper is what a golden
// cannot run.
test("an async conversion inside an optional or nullable field is awaited", async () => {
  const shout = S.string
    .with(S.nonEmpty)
    .with(S.to, S.string, { decode: { async: async (v) => `${v}!` }, encode: (v) => v });
  const schema = S.formData.with(
    S.to,
    S.schema({ a: S.optional(shout), b: S.nullable(shout, "d"), c: S.optional(shout, "e") }),
  );
  expect(await S.parseAsPromiseOrReject(schema)(form(["a", "x"]))).toEqual({ a: "x!", b: "d", c: "e" });
  expect(await S.parseAsPromiseOrReject(schema)(form(["b", "y"], ["c", "z"]))).toEqual({
    a: undefined,
    b: "y!",
    c: "z!",
  });
});
