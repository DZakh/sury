import { expect, test } from "vitest";
import * as S from "sury";

// What a real browser puts in a `FormData`, captured rather than imagined.
//
// SUBMISSION is the entry list headless Chromium 141 built from the form in
// FORM_HTML below, read back as `[...new FormData(form).entries()]`. It is
// frozen here because the assertions are about `S.formData`, not about the
// browser — but every row is reproducible: load FORM_HTML in any engine and
// read the entries. Each one is also a step of the HTML Standard's
// "constructing the entry list" algorithm (§4.10.22.4), noted per row.
const FORM_HTML = `<form id="f">
  <input name="name" type="text" value="Ann">
  <input name="blank" type="text" value="">
  <input name="padded" type="text" value="  spaced  ">
  <textarea name="bio">line1\nline2</textarea>
  <input name="age" type="number" value="42">
  <input name="blankNumber" type="number" value="">
  <input name="agree" type="checkbox" checked>
  <input name="unchecked" type="checkbox">
  <input name="valued" type="checkbox" value="yes" checked>
  <input name="emptyValued" type="checkbox" value="" checked>
  <input name="plan" type="radio" value="free">
  <input name="plan" type="radio" value="pro" checked>
  <select name="tags" multiple>
    <option value="a" selected>a</option>
    <option value="b" selected>b</option>
    <option value="c">c</option>
  </select>
  <select name="single"><option value="x" selected>x</option></select>
  <select name="none" multiple><option value="q">q</option></select>
  <input name="avatar" type="file">
  <input name="_charset_" type="hidden">
  <input name="disabledField" type="text" value="nope" disabled>
  <input name="dated" type="date" value="2024-01-01">
  <input name="dt" type="datetime-local" value="2024-01-01T10:30">
  <input name="rng" type="range" min="0" max="10" value="7">
  <input name="colr" type="color" value="#ff0000">
</form>`;

type Captured = [string, string | { file: true; name: string; type: string }];

const SUBMISSION: Captured[] = [
  ["name", "Ann"],
  // An empty text input is an entry with an empty value, not an absent one.
  ["blank", ""],
  // No trimming anywhere in the algorithm.
  ["padded", "  spaced  "],
  // LF stays LF in the entry list; CRLF normalization belongs to the
  // urlencoded/text-plain serializers, not to `FormData`.
  ["bio", "line1\nline2"],
  ["age", "42"],
  ["blankNumber", ""],
  // A checked box with no `value` attribute submits the string "on".
  ["agree", "on"],
  // `unchecked` contributes nothing: the algorithm skips an unchecked box.
  // A checkbox's value is whatever the attribute says…
  ["valued", "yes"],
  // …including the empty string, which is why "" cannot mean "unchecked".
  ["emptyValued", ""],
  // Only the checked radio of a group.
  ["plan", "pro"],
  // One entry per selected option; `none` selects nothing and contributes none.
  ["tags", "a"],
  ["tags", "b"],
  ["single", "x"],
  // A file input with nothing chosen still submits — an empty, unnamed file.
  ["avatar", { file: true, name: "", type: "application/octet-stream" }],
  // The browser fills `_charset_` itself; the schema never declared it.
  ["_charset_", "UTF-8"],
  // `disabledField` contributes nothing.
  ["dated", "2024-01-01"],
  // No seconds and no zone, so `new Date` reads it as local time.
  ["dt", "2024-01-01T10:30"],
  ["rng", "7"],
  ["colr", "#ff0000"],
];

const submitted = (): FormData => {
  const fd = new FormData();
  for (const [key, value] of SUBMISSION) {
    fd.append(
      key,
      typeof value === "string" ? value : new File([], value.name, { type: value.type }),
    );
  }
  return fd;
};

test("the captured submission is what the algorithm describes", () => {
  const keys = SUBMISSION.map(([key]) => key);
  expect(FORM_HTML).toContain(`name="unchecked"`);
  expect(keys).not.toContain("unchecked");
  expect(keys).not.toContain("disabledField");
  expect(keys).not.toContain("none");
  expect(keys.filter((key) => key === "tags")).toHaveLength(2);
});

test("a browser submission decodes field by field", () => {
  const schema = S.formData.with(
    S.to,
    S.schema({
      name: S.string,
      blank: S.string,
      padded: S.string,
      bio: S.string,
      age: S.number,
      agree: S.boolean,
      unchecked: S.boolean,
      plan: S.union(["free", "pro"]),
      tags: S.array(S.string),
      single: S.string,
      none: S.array(S.string),
      dated: S.string.with(S.to, S.date),
      rng: S.number,
      colr: S.string.with(S.pattern, /^#[0-9a-f]{6}$/),
    }),
  );
  expect(S.decoder(schema)(submitted())).toEqual({
    name: "Ann",
    // A required string takes the empty entry as the value it is; `S.nonEmpty`
    // is how a schema rejects it.
    blank: "",
    // Never trimmed — `S.trim` is the opt-in.
    padded: "  spaced  ",
    bio: "line1\nline2",
    age: 42,
    agree: true,
    // An unchecked box sends nothing, and nothing is `false`.
    unchecked: false,
    plan: "pro",
    tags: ["a", "b"],
    single: "x",
    none: [],
    dated: new Date("2024-01-01"),
    rng: 7,
    colr: "#ff0000",
  });
});

test("a blank entry is absent for an optional field, and its own value otherwise", () => {
  const optional = S.formData.with(
    S.to,
    S.schema({ blank: S.optional(S.string), blankNumber: S.optional(S.number, 7) }),
  );
  expect(S.decoder(optional)(submitted())).toEqual({ blank: undefined, blankNumber: 7 });

  // Required, so each target answers for itself rather than reporting absence.
  expect(() =>
    S.decoder(S.formData.with(S.to, S.schema({ blank: S.string.with(S.nonEmpty) })))(submitted()),
  ).toThrow('Failed at blank: Expected string.length >= 1, received ""');
  expect(() =>
    S.decoder(S.formData.with(S.to, S.schema({ blankNumber: S.number })))(submitted()),
  ).toThrow('Failed at blankNumber: Expected number, received ""');
});

test("a file input with nothing chosen reads as absent, not as an empty file", () => {
  // The algorithm still appends an entry: a `File` with an empty name,
  // `application/octet-stream`, and no bytes. Handing that to a schema as a
  // real upload is what every form library treats as a bug.
  const optional = S.formData.with(S.to, S.schema({ avatar: S.optional(S.file) }));
  expect(S.decoder(optional)(submitted())).toEqual({ avatar: undefined });

  const required = S.formData.with(S.to, S.schema({ avatar: S.file }));
  expect(() => S.decoder(required)(submitted())).toThrow(
    "Failed at avatar: Expected File, received undefined",
  );

  // A real upload still arrives as itself.
  const chosen = new FormData();
  const picked = new File(["hi"], "a.txt", { type: "text/plain" });
  chosen.append("avatar", picked);
  expect(S.decoder(required)(chosen)).toEqual({ avatar: picked });
});

test("a checkbox with a value attribute is not a boolean", () => {
  // "yes" is a legal checkbox value, and the boolean read does not guess at it
  // — the schema says what the value is.
  expect(() =>
    S.decoder(S.formData.with(S.to, S.schema({ valued: S.boolean })))(submitted()),
  ).toThrow('Failed at valued: Expected boolean, received "yes"');
  expect(
    S.decoder(S.formData.with(S.to, S.schema({ valued: S.union(["yes"]) })))(submitted()),
  ).toEqual({ valued: "yes" });

  // A checked box whose value is "" is indistinguishable from an unchecked one
  // on this wire, and reads as unchecked.
  expect(
    S.decoder(S.formData.with(S.to, S.schema({ emptyValued: S.boolean })))(submitted()),
  ).toEqual({ emptyValued: false });
});

test("a browser adds entries the schema never declared", () => {
  // `_charset_` is filled in by the browser, and `dirname` and an image
  // button's `name.x`/`name.y` arrive the same way — so `S.strict` cannot mean
  // "no other entries" against a real submission.
  const schema = S.formData.with(S.to, S.schema({ name: S.string }).with(S.strict));
  expect(() => S.decoder(schema)(submitted())).toThrow("Unrecognized key");
  // Even a schema that declares every control the author wrote still trips on
  // the one the browser added.
  const declared = S.formData.with(
    S.to,
    S.schema({ name: S.string, blank: S.string }).with(S.strict),
  );
  const partial = new FormData();
  partial.append("name", "Ann");
  partial.append("blank", "");
  expect(S.decoder(declared)(partial)).toEqual({ name: "Ann", blank: "" });
  partial.append("_charset_", "UTF-8");
  expect(() => S.decoder(declared)(partial)).toThrow('Unrecognized key "_charset_"');
});
