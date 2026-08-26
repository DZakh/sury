import type { MemberSpec } from "./generate";
import type { Sury } from "./types";

export type Issue392Case = {
  readonly id: string;
  readonly members: MemberSpec[];
  readonly laterWitness: { TAG: string; _0: string };
  readonly allWitnesses: readonly { label: string; value: unknown }[];
};

export const issue392Case = (S: Sury): Issue392Case => {
  const payload = S.schema({
    a: S.string,
    kind: S.union([S.schema("A"), S.schema("B")]),
  });
  const members: MemberSpec[] = [
    { id: '{TAG:"One",_0:string}', schema: S.schema({ TAG: "One", _0: S.string }) },
    { id: '{TAG:"Two",_0:string}', schema: S.schema({ TAG: "Two", _0: S.string }) },
    {
      id: '{TAG:"Three",_0:{a,kind:"A"|"B"}}',
      schema: S.schema({ TAG: "Three", _0: payload }),
    },
    { id: '{TAG:"Four",_0:string}', schema: S.schema({ TAG: "Four", _0: S.string }) },
  ];
  return {
    id: "issue-392",
    members,
    laterWitness: { TAG: "Four", _0: "x" },
    allWitnesses: [
      { label: "One", value: { TAG: "One", _0: "x" } },
      { label: "Two", value: { TAG: "Two", _0: "x" } },
      { label: "Three", value: { TAG: "Three", _0: { a: "x", kind: "A" } } },
      { label: "Four", value: { TAG: "Four", _0: "x" } },
    ],
  };
};
