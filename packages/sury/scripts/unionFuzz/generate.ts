import { modifiers, schemaLeaves, wraps } from "./catalog";
import type { Sury } from "./types";

export type Rng = () => number;

export const rngFromSeed = (seed: number): Rng => {
  let state = seed | 0;
  return () => {
    state = (state + 0x6d2b79f5) | 0;
    let t = state;
    t = Math.imul(t ^ (t >>> 15), t | 1);
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
};

const pick = <T>(rng: Rng, list: readonly T[]): T =>
  list[Math.floor(rng() * list.length)]!;

export type MemberSpec = {
  readonly id: string;
  readonly schema: unknown;
};

const leafSchema = (S: Sury, rng: Rng): MemberSpec => {
  const leaves = schemaLeaves(S);
  const leaf = pick(rng, leaves);
  return { id: leaf.name, schema: leaf.schema };
};

const applyWrap = (S: Sury, rng: Rng, inner: MemberSpec): MemberSpec => {
  const [name, spec] = pick(rng, wraps());
  return { id: `${name}(${inner.id})`, schema: spec.wrap(S, inner.schema) };
};

const applyModify = (S: Sury, rng: Rng, inner: MemberSpec): MemberSpec | undefined => {
  const type = (inner.schema as { type?: string }).type;
  if (!type) return undefined;
  const matching = modifiers().filter(([, spec]) => spec.on.includes(type));
  if (!matching.length) return undefined;
  const [name, spec] = pick(rng, matching);
  try {
    return { id: `${inner.id}.with(${name})`, schema: spec.modify(S, inner.schema) };
  } catch {
    return undefined;
  }
};

const taggedKind = (S: Sury, tag: string, inner: MemberSpec): MemberSpec => ({
  id: `{kind:${tag},${inner.id}}`,
  schema: S.object({ kind: tag, v: inner.schema }),
});

const taggedRescript = (S: Sury, tag: string, inner: MemberSpec): MemberSpec => ({
  id: `{TAG:${tag},${inner.id}}`,
  schema: S.schema({ TAG: tag, _0: inner.schema }),
});

const payloadWithUnionField = (S: Sury, optional: boolean): MemberSpec => {
  const field = optional
    ? S.optional(S.string)
    : S.union([S.schema("A"), S.schema("B")]);
  return {
    id: `{a:string,kind:${optional ? "optional(string)" : '"A"|"B"'}}`,
    schema: S.schema({ a: S.string, kind: field }),
  };
};

const tupleMember = (S: Sury, rng: Rng): MemberSpec => {
  const a = leafSchema(S, rng);
  const b = leafSchema(S, rng);
  return { id: `tuple(${a.id},${b.id})`, schema: S.tuple([a.schema, b.schema]) };
};

const nestedUnion = (S: Sury, rng: Rng, depth: number): MemberSpec => {
  const a = memberAt(S, rng, depth + 1);
  const b = memberAt(S, rng, depth + 1);
  return { id: `union(${a.id},${b.id})`, schema: S.union([a.schema, b.schema]) };
};

const memberAt = (S: Sury, rng: Rng, depth: number): MemberSpec => {
  if (depth >= 2) return leafSchema(S, rng);
  const roll = rng();
  if (roll < 0.06) {
    return { id: "enum(e0,e1)", schema: S.enum(["e0", "e1"]) };
  }
  if (roll < 0.1) {
    return { id: "null", schema: S.schema(null) };
  }
  if (roll < 0.14) {
    return { id: "instance(Error)", schema: S.instance(Error) };
  }
  if (roll < 0.28) return leafSchema(S, rng);
  if (roll < 0.4) return applyWrap(S, rng, leafSchema(S, rng));
  if (roll < 0.55) {
    return taggedKind(S, `k${Math.floor(rng() * 8)}`, leafSchema(S, rng));
  }
  if (roll < 0.72) {
    return taggedRescript(S, `T${Math.floor(rng() * 8)}`, leafSchema(S, rng));
  }
  if (roll < 0.82) {
    return taggedRescript(
      S,
      `T${Math.floor(rng() * 8)}`,
      payloadWithUnionField(S, rng() < 0.5),
    );
  }
  if (roll < 0.9) return tupleMember(S, rng);
  if (roll < 0.96) {
    const modified = applyModify(S, rng, leafSchema(S, rng));
    return modified ?? leafSchema(S, rng);
  }
  return nestedUnion(S, rng, depth);
};

export const groupingBarrierMembers = (S: Sury): MemberSpec[] => [
  taggedRescript(S, "One", { id: "string", schema: S.string }),
  taggedRescript(S, "Two", { id: "string", schema: S.string }),
  taggedRescript(S, "Three", payloadWithUnionField(S, false)),
  taggedRescript(S, "Four", { id: "string", schema: S.string }),
];

export const generateMembers = (
  S: Sury,
  rng: Rng,
  size: number,
): MemberSpec[] => {
  if (rng() < 0.05) return groupingBarrierMembers(S);
  const members: MemberSpec[] = [];
  for (let i = 0; i < size; i++) members.push(memberAt(S, rng, 0));
  return members;
};
