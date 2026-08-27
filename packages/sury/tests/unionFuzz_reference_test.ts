import { expect, test } from "vitest";
import * as S from "../index.mjs";
import { issue347OptionVoidLastSchema, issue347Schema } from "../scripts/unionFuzz/issue347";
import { issue392Case } from "../scripts/unionFuzz/issue392";
import { describeOutcome, show } from "../scripts/unionFuzz/outcome";
import {
  compiledEncode,
  compiledParse,
  memberEncode,
  memberParse,
  referenceEncode,
  referenceParse,
} from "../scripts/unionFuzz/reference";
import { witnessOf } from "../scripts/unionFuzz/witness";

test("show distinguishes non-finite numbers from null", () => {
  expect(show(Number.NaN)).toBe("NaN");
  expect(show(Infinity)).toBe("Infinity");
  expect(show(-Infinity)).toBe("-Infinity");
  expect(show(null)).toBe("null");
});

test("reference parse is sequential try of each member parser", () => {
  const stringMember = S.string;
  const numberMember = S.number;
  const union = S.union([stringMember, numberMember]);
  const stringWitness = "hi";
  const numberWitness = 1;
  const junk = true;

  expect(referenceParse(S, union, stringWitness)).toEqual(
    memberParse(S, stringMember, stringWitness),
  );
  expect(referenceParse(S, union, numberWitness)).toEqual(
    memberParse(S, numberMember, numberWitness),
  );
  expect(referenceParse(S, union, junk).ok).toBe(false);
  expect(memberParse(S, stringMember, junk).ok).toBe(false);
  expect(memberParse(S, numberMember, junk).ok).toBe(false);
});

test("reference encode is parse of the reversed union", () => {
  const stringMember = S.string;
  const numberMember = S.number;
  const union = S.union([stringMember, numberMember]);
  expect(referenceEncode(S, union, "hi")).toEqual(
    memberEncode(S, stringMember, "hi"),
  );
  expect(referenceEncode(S, union, 1)).toEqual(
    memberEncode(S, numberMember, 1),
  );
  expect(referenceEncode(S, union, "hi")).toEqual(
    referenceParse(S, S.reverse(union), "hi"),
  );
});

test("reference agrees with a member on a witness that member accepts", () => {
  const a = S.schema({ kind: "a", v: S.string });
  const b = S.schema({ kind: "b", v: S.number });
  const union = S.union([a, b]);
  const wa = witnessOf(a);
  const wb = witnessOf(b);
  expect(memberParse(S, a, wa).ok).toBe(true);
  expect(referenceParse(S, union, wa)).toEqual(memberParse(S, a, wa));
  expect(memberParse(S, b, wb).ok).toBe(true);
  expect(referenceParse(S, union, wb)).toEqual(memberParse(S, b, wb));
  expect(referenceParse(S, union, { kind: "z" }).ok).toBe(false);
});

test("issue 392: compiled parse matches member parser and sequential-try reference", () => {
  const { members, laterWitness, allWitnesses } = issue392Case(S);
  const union = S.union(members.map((m) => m.schema));
  const four = members[3]!.schema;

  expect(memberParse(S, four, laterWitness).ok).toBe(true);

  for (const { label, value } of allWitnesses) {
    const member = members.find((m) => m.id.includes(label))!.schema;
    const own = memberParse(S, member, value);
    const reference = referenceParse(S, union, value);
    const compiled = compiledParse(S, union, value);
    expect(own.ok, `${label} member ${describeOutcome(own)}`).toBe(true);
    expect(reference.ok, `${label} reference ${describeOutcome(reference)}`).toBe(
      true,
    );
    expect(compiled.ok, `${label} compiled ${describeOutcome(compiled)}`).toBe(
      true,
    );
    expect(describeOutcome(compiled)).toBe(describeOutcome(reference));
    expect(describeOutcome(compiled)).toBe(describeOutcome(own));
  }

  const junk = compiledParse(S, union, { TAG: "Z", _0: "x" });
  expect(junk.ok).toBe(false);

  const parser = S.parser(union);
  const reversed = S.parser(S.reverse(union));
  for (const { value } of allWitnesses) {
    expect(parser(value)).toEqual(value);
    expect(S.encoder(union)(value)).toEqual(value);
    expect(reversed(value)).toEqual(value);
  }
});

test("issue 347: encode of null through nullable(union of S.to(S.any)) is null, not TypeError", () => {
  const schema = issue347Schema(S) as S.Schema<unknown, unknown>;
  const compiled = compiledEncode(S, schema, null);
  const reference = referenceEncode(S, schema, null);
  expect(compiled.ok, describeOutcome(compiled)).toBe(true);
  expect(reference.ok, describeOutcome(reference)).toBe(true);
  expect(describeOutcome(compiled)).toBe(describeOutcome(reference));
  const encode = S.encoder(schema);
  expect(encode(null)).toBe(null);
  expect(encode({ TAG: "Tagged", _0: "abc" })).toEqual({ $ref: "abc" });
  expect(encode({ TAG: "Plain", _0: { name: "n" } })).toEqual({ name: "n" });
});

test("issue 347: option(union(custom, void)) encodes undefined without TypeError", () => {
  const schema = issue347OptionVoidLastSchema(S) as S.Schema<unknown, unknown>;
  const compiled = compiledEncode(S, schema, undefined);
  expect(compiled.ok, describeOutcome(compiled)).toBe(true);
  expect(S.encoder(schema)(undefined)).toBe(undefined);
  expect(S.encoder(schema)({ TAG: "Tagged", _0: "abc" })).toEqual({ $ref: "abc" });
});

test("object group after nested optional/null payload still reaches later members", () => {
  const four = { TAG: "Four", _0: "x" };
  const optionalUnion = S.union([
    S.schema({ TAG: "One", _0: S.string }),
    S.schema({ TAG: "Two", _0: S.string }),
    S.schema({
      TAG: "Three",
      _0: S.schema({ a: S.string, extra: S.optional(S.number) }),
    }),
    S.schema({ TAG: "Four", _0: S.string }),
  ]);
  const nullUnion = S.union([
    S.schema({ TAG: "One", _0: S.string }),
    S.schema({ TAG: "Two", _0: S.string }),
    S.schema({
      TAG: "Three",
      _0: S.schema({ a: S.string, extra: S.nullable(S.number) }),
    }),
    S.schema({ TAG: "Four", _0: S.string }),
  ]);
  expect(S.parser(optionalUnion)(four)).toEqual(four);
  expect(S.parser(nullUnion)(four)).toEqual(four);
  expect(S.parser(optionalUnion)({ TAG: "Three", _0: { a: "x" } })).toEqual({
    TAG: "Three",
    _0: { a: "x" },
  });
  expect(
    S.parser(nullUnion)({ TAG: "Three", _0: { a: "x", extra: null } }),
  ).toEqual({ TAG: "Three", _0: { a: "x", extra: null } });
});
