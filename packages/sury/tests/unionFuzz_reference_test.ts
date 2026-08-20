import { expect, test } from "vitest";
import * as S from "../index.mjs";
import { issue392Case } from "../scripts/unionFuzz/issue392";
import { describeOutcome } from "../scripts/unionFuzz/outcome";
import {
  compiledParse,
  memberEncode,
  memberParse,
  referenceEncode,
  referenceParse,
} from "../scripts/unionFuzz/reference";
import { compareValue } from "../scripts/unionFuzz/run";
import { witnessOf } from "../scripts/unionFuzz/witness";

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

test("issue 392 later member: reference accepts, compiler is compared not trusted", () => {
  const { members, laterWitness, allWitnesses } = issue392Case(S);
  const union = S.union(members.map((m) => m.schema));
  const four = members[3]!.schema;

  expect(memberParse(S, four, laterWitness).ok).toBe(true);
  const reference = referenceParse(S, union, laterWitness);
  expect(reference.ok, describeOutcome(reference)).toBe(true);

  for (const { label, value } of allWitnesses) {
    const ref = referenceParse(S, union, value);
    expect(ref.ok, `${label} ${describeOutcome(ref)}`).toBe(true);
  }

  const compiled = compiledParse(S, union, laterWitness);
  const pair = compareValue(S, union, laterWitness, "parse");
  expect(pair.reference.ok).toBe(true);
  if (compiled.ok) {
    expect(describeOutcome(compiled)).toBe(describeOutcome(reference));
  } else {
    expect(pair.compiled.ok).toBe(false);
  }
});
