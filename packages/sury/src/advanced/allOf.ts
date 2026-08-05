// `S.allOf` — intersection ("must satisfy every member"), resolved at
// schema-construction time instead of a runtime check-them-all wrapper: objects
// merge property by property, same-type members chain so the parse loop fuses
// their checks under one type narrow, and a union member distributes over its
// variants so the union planner keeps owning dispatch. The result is a plain
// schema — there is no allOf tag, so the generated code carries no second
// validation pass and the reverse comes from the folded schema for free.

import {
  anyOfTag,
  baseSchema,
  type Internal,
  isLiteral,
  neverTag,
  objectTag,
  panic,
  stringTag,
  tagFlagNever,
  tagFlags,
  tagFlagUnknown,
  U,
  updateOutput,
} from "../base";
import { unionFactory } from "../union";
import { objectDecoder } from "../composites";
import { getOutputSchema, never_ } from "../parse";
import { definitionToSchema } from "../factory";

// The structural-merge shape: fixed properties, a mode string ("strip"/"strict")
// for extras, and nothing pending on the value — the same set S.merge accepts.
// A refiner disqualifies: the naive property merge would silently drop it.
const allOfPlainObject = (schema: Internal): boolean =>
  typeof schema.additionalItems === stringTag &&
  schema.to === U &&
  schema.parser === U &&
  schema.refiner === U &&
  schema.inputRefiner === U;

// An unsatisfiable variant contributes nothing to the intersection, so it drops
// before the union planner ever sees it — `(A | undefined) & A` folds to `A`.
const allOfDistribute = (
  variants: Internal[],
  merge: (member: Internal) => Internal
): Internal => {
  const members: Internal[] = [];
  for (let idx = 0; idx < variants.length; idx++) {
    const merged = merge(variants[idx]!);
    if (merged.type !== neverTag) {
      members.push(merged);
    }
  }
  return members.length ? unionFactory(members) : never_;
};

const allOfPair = (a: Internal, b: Internal): Internal => {
  if (a === b) {
    return a;
  }
  const aTag = tagFlags[a.type]!;
  const bTag = tagFlags[b.type]!;
  if (aTag & tagFlagUnknown) {
    return b;
  }
  if (bTag & tagFlagUnknown) {
    return a;
  }
  if ((aTag | bTag) & tagFlagNever) {
    return never_;
  }
  // Distribution before everything structural: (A | B) & C = (A & C) | (B & C),
  // and two unions cross-distribute by re-entering here per variant.
  if (a.type === anyOfTag && a.to === U && a.parser === U) {
    return allOfDistribute(a.anyOf!, (member) => allOfPair(member, b));
  }
  if (b.type === anyOfTag && b.to === U && b.parser === U) {
    return allOfDistribute(b.anyOf!, (member) => allOfPair(a, member));
  }
  if (a.type === objectTag && b.type === objectTag) {
    if (!(allOfPlainObject(a) && allOfPlainObject(b))) {
      return panic(
        "S.allOf supports only structured object schemas without transformations"
      );
    }
    // Null prototype for the same reason the factories use it: a merged key
    // named `__proto__` must become a property, not a reparent.
    const properties: Record<string, Internal> = Object.create(null);
    for (const key in a.properties!) {
      properties[key] = a.properties![key]!;
    }
    for (const key in b.properties!) {
      const existing = properties[key];
      const field = b.properties![key]!;
      properties[key] = existing === U ? field : allOfPair(existing, field);
    }
    const mut = baseSchema(objectTag, false);
    mut.required = Object.keys(properties);
    mut.properties = properties;
    mut.additionalItems =
      b.additionalItems === "strict" ? b.additionalItems : a.additionalItems;
    mut.decoder = objectDecoder;
    return mut;
  }
  // Two exact values can only intersect by being the same value (SameValueZero,
  // so NaN meets itself). Decided statically — chaining would emit two const
  // checks the second of which can never pass.
  if (
    isLiteral(a) &&
    isLiteral(b) &&
    !(a.const === b.const || (a.const !== a.const && b.const !== b.const))
  ) {
    return never_;
  }
  const aOut = getOutputSchema(a);
  // Same runtime type (class included — two different classes are treated as
  // disjoint, ignoring subclassing): append `b`, and the parse loop fuses the
  // chain into one type narrow carrying both members' checks.
  if (aOut.type === b.type && aOut.class === b.class) {
    return updateOutput(a, (mut) => {
      mut.to = b;
    });
  }
  return never_;
};

// @__NO_SIDE_EFFECTS__
export const allOf = (definitions: unknown[]): Internal => {
  if (definitions.length === 0) {
    return panic("S.allOf requires at least one item");
  }
  let acc = definitionToSchema(definitions[0]);
  for (let idx = 1; idx < definitions.length; idx++) {
    acc = allOfPair(acc, definitionToSchema(definitions[idx]));
  }
  return acc;
};
