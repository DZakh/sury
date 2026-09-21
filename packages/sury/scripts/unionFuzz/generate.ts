import { modifiers, schemaLeaves, wraps } from "./catalog";
import { NO_SAMPLE, sample, show } from "./sample";
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
  // Carries a conversion that throws information away, so `decode(encode(o))`
  // is not `o` and no property may ask it to be. Set by the one modifier that
  // is deliberately lossy and propagated by every shape built over it, since
  // a container of lossy items is lossy too.
  readonly lossy?: boolean;
};

const leafSchema = (S: Sury, rng: Rng): MemberSpec => {
  const leaves = schemaLeaves(S);
  const leaf = pick(rng, leaves);
  return { id: leaf.name, schema: leaf.schema };
};

const applyWrap = (S: Sury, rng: Rng, inner: MemberSpec): MemberSpec => {
  const [name, spec] = pick(rng, wraps());
  return { id: `${name}(${inner.id})`, schema: spec.wrap(S, inner.schema), lossy: inner.lossy };
};

// A schema that refuses to be built is a finding, and a finding needs the shape
// it is about: generation happens before the runner has an id, so the id is put
// on the error here, where it exists.
const named = (id: string, build: () => unknown): unknown => {
  try {
    return build();
  } catch (error) {
    (error as Error).message = `${id} - ${(error as Error).message}`;
    throw error;
  }
};

// An Output value of `inner`, drawn the way every other output value is: the
// sampler over the reverse. `undefined` is not one a default may be - it is the
// absence the default replaces - and a shape the sampler cannot fill has none
// to offer, so both fall back to the undefaulted wrap.
const defaultFor = (S: Sury, rng: Rng, inner: MemberSpec): unknown => {
  let value: unknown;
  try {
    value = sample(S.reverse(inner.schema), rng);
    // The sampler reads types, not refinements, so it draws values a bounded
    // or patterned schema rejects. Asking the schema settles it, and leaves a
    // creation throw below meaning what it says: a default the schema itself
    // calls an output, refused.
    if (value === undefined || value === NO_SAMPLE || S.isOutput(inner.schema)(value) !== true) {
      return NO_SAMPLE;
    }
  } catch {
    return NO_SAMPLE;
  }
  return value;
};

// An object that RESHAPES: its input has a field the output does not, so it
// converts in both directions. The grammar's other objects are all built from
// `S.schema`/`S.object` literals whose input and output are the same shape, so
// before this the only transforming members it could draw were `S.to` chains
// and the carriers - and a transform on a `.to` chain is exactly the one every
// site already reads correctly (#452).
const renamedObject = (S: Sury, rng: Rng, inner: MemberSpec): MemberSpec => ({
  id: `{TAG:R,_0:<-a:${inner.id}}`,
  schema: S.object((s: any) => ({ TAG: "R", _0: s.field("a", inner.schema) })),
  lossy: inner.lossy,
});

// A container whose ITEMS transform: the one shape whose conversion is
// invisible in its own `.to`, so every site that reads the chain tail as the
// Output type is wrong about it (#452). The general grammar reaches it only
// when three independent rolls line up, which over a sweep is a handful of
// draws - not a test. Seeded deliberately instead, the way the union grammar
// seeds its grouping barrier.
const taggedRescript = (S: Sury, tag: string, inner: MemberSpec): MemberSpec => ({
  id: `{TAG:${tag},${inner.id}}`,
  schema: S.schema({ TAG: tag, _0: inner.schema }),
  lossy: inner.lossy,
});

const transformingContainer = (S: Sury, rng: Rng): MemberSpec => {
  const [name, spec] = pick(rng, wraps());
  const leaf = leafSchema(S, rng);
  const roll = rng();
  const inner =
    roll < 0.5 ? renamedObject(S, rng, leaf) : roll < 0.75 ? applyModify(S, rng, leaf) : leaf;
  const item = inner ?? leaf;
  return { id: `${name}(${item.id})`, schema: spec.wrap(S, item.schema), lossy: item.lossy };
};

// `S.optional(x, d)` / `S.nullable(x, d)`. Deliberately NOT guarded: a default
// the sampler drew off the Output side is one the schema has to accept, so a
// throw here is the finding (#452), and the runners report it against this id.
const defaultedMember = (S: Sury, rng: Rng, inner: MemberSpec): MemberSpec => {
  const name = rng() < 0.5 ? "optional" : "nullable";
  const value = defaultFor(S, rng, inner);
  if (value === NO_SAMPLE) {
    return { id: `${name}(${inner.id})`, schema: S[name](inner.schema), lossy: inner.lossy };
  }
  const id = `${name}(${inner.id},${show(value)})`;
  return { id, schema: named(id, () => S[name](inner.schema, value)), lossy: inner.lossy };
};

// The same default reached through the object builder, which spells it as a
// field rather than a wrapper and used to compile to something else entirely.
const fieldOrMember = (S: Sury, rng: Rng, inner: MemberSpec): MemberSpec => {
  const value = defaultFor(S, rng, inner);
  return value === NO_SAMPLE
    ? {
        id: `{f:${inner.id}}`,
        schema: S.object((s: any) => ({ f: s.field("f", inner.schema) })),
        lossy: inner.lossy,
      }
    : (() => {
        const id = `{fieldOr(f,${inner.id},${show(value)})}`;
        return {
          id,
          schema: named(id, () =>
            S.object((s: any) => ({ f: s.fieldOr("f", inner.schema, value) })),
          ),
          lossy: inner.lossy,
        };
      })();
};

const applyModify = (S: Sury, rng: Rng, inner: MemberSpec): MemberSpec | undefined => {
  const type = (inner.schema as { type?: string }).type;
  if (!type) return undefined;
  const matching = modifiers().filter(([, spec]) => spec.on.includes(type));
  if (!matching.length) return undefined;
  const [name, spec] = pick(rng, matching);
  try {
    return {
      id: `${inner.id}.with(${name})`,
      schema: spec.modify(S, inner.schema),
      lossy: inner.lossy || name === "to",
    };
  } catch {
    return undefined;
  }
};

const taggedKind = (S: Sury, tag: string, inner: MemberSpec): MemberSpec => ({
  id: `{kind:${tag},${inner.id}}`,
  schema: S.object({ kind: tag, v: inner.schema }),
  lossy: inner.lossy,
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
  return {
    id: `tuple(${a.id},${b.id})`,
    schema: S.tuple([a.schema, b.schema]),
    lossy: a.lossy || b.lossy,
  };
};

const nestedUnion = (S: Sury, rng: Rng, depth: number): MemberSpec => {
  const a = memberAt(S, rng, depth + 1);
  const b = memberAt(S, rng, depth + 1);
  return {
    id: `union(${a.id},${b.id})`,
    schema: S.union([a.schema, b.schema]),
    lossy: a.lossy || b.lossy,
  };
};

// Three, not two: a default over a container whose ITEMS transform needs three
// levels to exist at all (default, container, transforming item), and that is
// the shape where a container's transform is invisible to its own `.to` (#452).
const memberAt = (S: Sury, rng: Rng, depth: number): MemberSpec => {
  if (depth >= 3) return leafSchema(S, rng);
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
  // Over a nested member, not a bare leaf: `array(string.with(to, number))` is
  // the shape where a container's transform lives in its items rather than on
  // its `.to`, and no composition of leaf-only wraps ever reaches it (#452).
  if (roll < 0.36) return applyWrap(S, rng, memberAt(S, rng, depth + 1));
  // Half of the defaulted draws go over a transforming container, which is what
  // puts a default and an items-side conversion in the same schema.
  if (roll < 0.4) {
    return defaultedMember(
      S,
      rng,
      rng() < 0.5 ? transformingContainer(S, rng) : memberAt(S, rng, depth + 1),
    );
  }
  if (roll < 0.44) {
    return fieldOrMember(
      S,
      rng,
      rng() < 0.5 ? transformingContainer(S, rng) : memberAt(S, rng, depth + 1),
    );
  }
  if (roll < 0.48) return renamedObject(S, rng, memberAt(S, rng, depth + 1));
  if (roll < 0.59) {
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

// One schema from the same grammar the union members come from, for a fuzzer
// whose subject is a schema rather than a union of them (`fuzz:eq`).
export const generateSchema = (S: Sury, rng: Rng): MemberSpec => memberAt(S, rng, 0);

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
