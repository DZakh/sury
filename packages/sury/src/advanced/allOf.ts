// `S.allOf` — intersection ("must satisfy every member"), resolved at
// schema-construction time instead of a runtime check-them-all wrapper:
// containers merge field by field, a union member distributes over its variants
// so the union planner keeps owning dispatch, and members that only constrain
// the same runtime type chain so the parse loop fuses their checks under one
// type narrow. The result is a plain schema — there is no allOf tag, so the
// generated code carries no second validation pass and the reverse comes from
// the folded schema for free.
//
// `never` is returned only where two members are *provably* disjoint, never as a
// fallback for a combination this fold can't reason about: a schema that rejects
// every value is indistinguishable from a working one until it reaches
// production. Anything unresolvable panics at construction instead, the way
// union.ts rejects an ambiguous conversion where it is written.

import {
  type AdditionalItemsMode,
  anyOfTag,
  arrayTag,
  baseSchema,
  inputExpression,
  type Internal,
  isLiteral,
  neverTag,
  objectTag,
  panic,
  stringTag,
  tagFlagArray,
  tagFlagFunction,
  tagFlagInstance,
  tagFlagNever,
  tagFlagObject,
  tagFlagRef,
  tagFlags,
  tagFlagUnion,
  tagFlagUnknown,
  U,
  updateOutput,
} from "../base";
import { array, arrayDecoder, objectDecoder } from "../composites";
import { definitionToSchema } from "../factory";
import { getOutputSchema, never_ } from "../parse";
import { unionFactory } from "../union";

const allOfUnsupported = (a: Internal, b: Internal, why: string): never =>
  panic(
    `Can't intersect ${inputExpression(a)} with ${inputExpression(b)} — ${why}. Use S.to to chain them explicitly`
  );

// Whether the schema constrains nothing beyond its own shape — no conversion and
// no user check — so a merge that rebuilds it from its parts loses nothing.
const allOfShapeOnly = (schema: Internal): boolean =>
  schema.to === U &&
  schema.parser === U &&
  schema.refiner === U &&
  schema.inputRefiner === U;

// The structural-merge shape: fixed fields plus a mode string ("strip"/"strict")
// for extras. A dict/`S.array` element schema is excluded rather than merged
// because a container carrying both fixed fields and an element schema is
// representable but not decodable — objectDecoder takes the dict path and never
// reads `properties` — so merging into one would silently drop every fixed
// field's checks.
const allOfFixedFields = (schema: Internal): boolean =>
  typeof schema.additionalItems === stringTag && allOfShapeOnly(schema);

// A container's extras mode after merging: the stricter of the two wins, since
// the value has to satisfy both.
const allOfMode = (a: Internal, b: Internal): AdditionalItemsMode =>
  (b.additionalItems === "strict" ? b : a).additionalItems as AdditionalItemsMode;

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

// A union only distributes while it is still a plain widening. One carrying a
// conversion of its own is a pipeline, and pushing the intersection inside it
// would re-order the conversion against the checks being merged in.
const allOfSpreads = (schema: Internal): boolean =>
  schema.type === anyOfTag && schema.to === U && schema.parser === U;

// Subclassing makes two instance tags a real intersection: every TypeError is an
// Error, so the pair meets at the more derived class. Chained rather than
// collapsed to that class alone, so a refinement on the base side survives.
const allOfExtends = (derived: unknown, base: unknown): boolean =>
  typeof derived === "function" &&
  typeof base === "function" &&
  (derived as { prototype: unknown }).prototype instanceof
    (base as new () => unknown);

const allOfChain = (first: Internal, second: Internal): Internal =>
  updateOutput(first, (mut) => {
    mut.to = second;
  });

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
  if (allOfSpreads(a)) {
    return allOfDistribute(a.anyOf!, (member) => allOfPair(member, b));
  }
  if (allOfSpreads(b)) {
    return allOfDistribute(b.anyOf!, (member) => allOfPair(a, member));
  }

  // A tag with no shape to compare — a recursive `$ref`, `S.json`, a function,
  // or a union that didn't spread because it carries a conversion — is opaque to
  // this fold: it can be neither merged nor proven disjoint. This has to precede
  // the same-type chain below, which would otherwise read two *different* refs
  // as one type (both carry no `class`) and chain them into a decode the parse
  // loop can't enter.
  if ((aTag | bTag) & (tagFlagRef | tagFlagFunction | tagFlagUnion)) {
    return allOfUnsupported(a, b, "it has no shape to intersect against");
  }

  if ((aTag & tagFlagObject) && (bTag & tagFlagObject)) {
    if (!(allOfFixedFields(a) && allOfFixedFields(b))) {
      return allOfUnsupported(
        a,
        b,
        "only objects with fixed properties and no transformation can merge"
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
      // A contradictory shared key leaves `never` on that property rather than
      // collapsing the whole object: both reject every value, and the field-level
      // one reports which key is contradictory.
      properties[key] = existing === U ? field : allOfPair(existing, field);
    }
    const mut = baseSchema(objectTag, false);
    mut.required = Object.keys(properties);
    mut.properties = properties;
    mut.additionalItems = allOfMode(a, b);
    mut.decoder = objectDecoder;
    return mut;
  }

  // Arrays merge for the same reason objects do: chaining two of them would
  // walk the value twice, which is the double validation this fold exists to
  // avoid. `S.array` carries an element schema, a tuple carries fixed items;
  // only like meets like, since a tuple's own mode decides whether the element
  // schema would even reach the trailing values.
  if ((aTag & tagFlagArray) && (bTag & tagFlagArray)) {
    const aItem = a.additionalItems;
    const bItem = b.additionalItems;
    if (typeof aItem === "object" && typeof bItem === "object") {
      return array(allOfPair(aItem, bItem));
    }
    if (typeof aItem !== "object" && typeof bItem !== "object") {
      const aItems = a.items!;
      const bItems = b.items!;
      // No value has two lengths at once.
      if (aItems.length !== bItems.length) {
        return never_;
      }
      const mut = baseSchema(arrayTag, false);
      mut.items = aItems.map((item, idx) => allOfPair(item, bItems[idx]!));
      mut.additionalItems = allOfMode(a, b);
      mut.decoder = arrayDecoder;
      return mut;
    }
    return allOfUnsupported(a, b, "an element schema can't merge with fixed items");
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

  if ((aTag & tagFlagInstance) && (bTag & tagFlagInstance) && a.class !== b.class) {
    // `instanceof Derived` already implies `instanceof Base`, so the pair meets
    // at the derived class — but only when the base side adds no checks of its
    // own: instanceDecoder rejects a class-to-class chain, so there is nowhere
    // left to carry them, and dropping them silently is the one outcome worse
    // than refusing.
    const derived = allOfExtends(a.class, b.class)
      ? a
      : allOfExtends(b.class, a.class)
        ? b
        : // Unrelated classes share no instance.
          U;
    if (derived === U) {
      return never_;
    }
    const base = derived === a ? b : a;
    return allOfShapeOnly(base)
      ? derived
      : allOfUnsupported(a, b, "the base class carries checks the derived one can't chain");
  }

  const aOut = getOutputSchema(a);
  // Same runtime type: append `b`, and the parse loop fuses the chain into one
  // type narrow carrying both members' checks. Read off `a`'s *output* so a
  // member that converts still intersects against what it produces.
  if (aOut.type === b.type && aOut.class === b.class) {
    return allOfChain(a, b);
  }

  // Same tag but not the same type — two instance classes reached through a
  // conversion. Disjointness isn't provable here, so say so rather than guess.
  if (aOut.type === b.type) {
    return allOfUnsupported(a, b, "they share a tag but not a type");
  }
  // Different concrete runtime tags: no value carries both.
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
