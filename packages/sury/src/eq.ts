// `S.isEqualInput` / `S.isEqualOutput` / `S.compareInput` / `S.compareOutput`:
// a compiled pairwise function for one side of a schema.
//
// Two things make this unlike every other operation. It takes a PAIR of values,
// which `compileDecoder`'s `i=>{…}` shape can't express, and it never
// validates: both values are assumed to conform, which is what lets an object
// compile to a bare conjunction of field reads with no type narrowing at all.
// So it walks the schema's own structure the way jsonschema.ts does rather than
// driving `parse`, and borrows only the pieces that don't presuppose a decode:
// the embed array, the union type-narrow, and the operation cache.
//
// Equality and compare share that walk. They do not share emit: a runtime
// `if (ord)` inside the walker would keep both templates in every bundle that
// reaches this module. The emit is a mode object the compile entry passes in,
// so an isEqual-only graph never names a compare leaf, loop, or shared-body.
//
// `compare(a,b)===0` is `isEqual(a,b)`. The equal path stays a conjunction of
// reads; compare is a lexicographic chain of ordered tests. Reflexivity
// (`f(x,x)` is true / 0) is the emit invariant. NaN is the whole difficulty:
// it is the only value not `===` itself, so a position that can hold one uses
// SameValueZero, and a position that cannot (default number validation) keeps
// `===` / `<`/`>`.

import {
  baseSchema,
  type Flag,
  globalConfig,
  inlinedProperty,
  inputExpression,
  type Internal,
  isLiteral,
  jsonName,
  neverTag,
  noopDecoder,
  panic,
  tagFlags,
  U,
  type Val,
} from "./base";
import { B_embedPure, B_inlineConst, B_operationArg } from "./builder";
import { addOpNode, findOpNode, type OpNode, removeOpNode, typeCheckCond } from "./parse";

export type IsEqual = (a: unknown, b: unknown) => boolean;
export type Compare = (a: unknown, b: unknown) => number;

// The cache key's first slot, so an eq/cmp node can't be mistaken for a
// decoder's (getDecoder never passes this schema) or for recursiveDecoder's
// `[input, def]` pair. Never compiled, never reversed: only its identity is
// read. Two sentinels so the two compiles don't collide in the op cache.
const eqOp: Internal = /* @__PURE__ */ baseSchema(neverTag, true, noopDecoder);
const cmpOp: Internal = /* @__PURE__ */ baseSchema(neverTag, true, noopDecoder);

// Off `globalThis`, because `FormData` landed in Node 18 and a bare reference
// to it is a ReferenceError on anything older. `URLSearchParams` is older, but
// the same lookup keeps a constructor read out of every bundle: a member at
// module scope is not something esbuild drops.
const globalClass = (name: "FormData" | "URLSearchParams"): unknown =>
  (globalThis as unknown as Record<string, unknown>)[name];

// The structural fallback, for a position whose schema describes no shape
// (`S.unknown`, `S.json`, a function) or one this emit declines to unroll.
// SameValueZero at the leaves, for the reflexivity reason above.
const deepEqual = (a: unknown, b: unknown): boolean => {
  if (a === b) return true;
  if (a !== a) return b !== b;
  if (!a || !b || typeof a !== "object" || typeof b !== "object") return false;
  // The PROTOTYPE, never `.constructor`: a data key named `constructor` shadows
  // it, and every object carrying one would then be compared as a foreign class.
  const proto = Object.getPrototypeOf(a);
  if (proto !== Object.getPrototypeOf(b)) return false;
  const ao = a as Record<string, unknown>;
  const bo = b as Record<string, unknown>;
  // Arrays and typed arrays alike: both are indexed by a numeric `length`.
  // `ArrayBuffer.isView` also admits DataView, which has no such index, so the
  // `length` test is what keeps one from reading as an empty match.
  if (
    Array.isArray(a) ||
    (ArrayBuffer.isView(a) && typeof (a as unknown as ArrayLike<unknown>).length === "number")
  ) {
    const n = (a as unknown as ArrayLike<unknown>).length;
    if (n !== (b as unknown as ArrayLike<unknown>).length) return false;
    for (let i = 0; i < n; i++) if (!deepEqual(ao[i], bo[i])) return false;
    return true;
  }
  // The built-ins a schema also compares by value rather than by identity, so
  // an untyped position answers the same way a typed one would.
  if (proto === Date.prototype) return +(a as Date) === +(b as Date);
  if (proto === URL.prototype) return `${a}` === `${b}`;
  // A Set by its members. Membership is SameValueZero already, so `has` is both
  // the right test and the fast one, and a member with an identity of its own
  // compares by identity - the rule the Set used to decide it holds one copy.
  if (proto === Set.prototype) {
    const as = a as Set<unknown>;
    const bs = b as Set<unknown>;
    if (as.size !== bs.size) return false;
    for (const value of as) if (!bs.has(value)) return false;
    return true;
  }
  // FormData and URLSearchParams are ordered lists of entries, not mappings:
  // a name handed out twice is two values, and the order they arrive in is the
  // order a server reads them, so two bodies differing only in it are two bodies.
  if (
    proto === (globalClass("FormData") as typeof FormData | undefined)?.prototype ||
    proto === (globalClass("URLSearchParams") as typeof URLSearchParams | undefined)?.prototype
  ) {
    const entries = [...(b as Iterable<[unknown, unknown]>)];
    let idx = 0;
    for (const [key, value] of a as Iterable<[unknown, unknown]>) {
      const entry = entries[idx++];
      if (entry === U || entry[0] !== key || entry[1] !== value) return false;
    }
    return idx === entries.length;
  }
  // Anything else with its own identity, a Blob or a user class, has already
  // failed the `===` above, and has no readable structure to fall back to.
  if (proto !== null && proto !== Object.prototype) return false;
  // `key in bo`, for the reason dictFn gives: equal key COUNTS are not equal key
  // sets, and reading a name the other side lacks yields `undefined` on both -
  // so `{a: undefined}` and `{b: undefined}` would count 1 each and match.
  let n = 0;
  for (const key in ao) {
    if (!(key in bo) || !deepEqual(ao[key], bo[key])) return false;
    n++;
  }
  for (const _key in bo) n--;
  return !n;
};

type Walk = (schema: Internal, a: string, b: string) => string;

// The emit for one side of the walk, split by what the two sides can answer.
// Compare has no order for the shapes the equality-only fields serve, so its
// mode carries `reject` in their place and the walk narrows on it: a branch
// reaching one of those fields without rejecting first does not typecheck.
// That is what keeps the accepted set and the emit from drifting apart - they
// are the same declaration.
type SharedMode = {
  join: (left: string, right: string) => string;
  prim: (ctx: Ctx, schema: Internal, a: string, b: string) => string;
  date: (a: string, b: string) => string;
  url: (a: string, b: string) => string;
  // `undefined` means "not a closed identity union, keep walking".
  identUnion: (ctx: Ctx, schema: Internal, a: string, b: string) => string | undefined;
  nullish: (
    narrows: string[],
    restIdx: number,
    rest: string,
    a: string,
    b: string,
    present: string,
  ) => string;
  yes: string;
  wrapPrefix: string;
  always: IsEqual | Compare;
  strict: IsEqual | Compare;
  nan: IsEqual | Compare;
  // Literals, not helper calls: a top-level `ordLeaf("a","b")` is a side
  // effect esbuild will not drop, and then the whole eq.ts graph ships with
  // every export.
  strictExpr: string;
  nanExpr: string;
};

type EqMode = SharedMode & {
  reject?: undefined;
  elem: (a: string, b: string) => string;
  ident: (a: string, b: string) => string;
  index: (element: string, from: number) => string;
  dict: (walk: (a: string, b: string) => string) => string;
  union: (
    narrows: string[],
    objectTagged: number[],
    members: Internal[],
    walk: Walk,
    a: string,
    b: string,
  ) => string;
  deep: (a: unknown, b: unknown) => unknown;
};

type CmpMode = SharedMode & {
  reject: (schema: Internal) => never;
};

type Mode = EqMode | CmpMode;

type Ctx = {
  // Codegen context, for the embed array (`g.e`) and the op flag. Never
  // merged, never emitted: this operation has no Val chain.
  b: Val;
  d: Record<string, Internal> | undefined;
  // Hoisted comparators, keyed by the schema they compare so a shape reached
  // twice is compiled once.
  h: Map<Internal, string>;
  // The structural-fallback embed, reused across positions.
  q: string;
  // The schema being compiled, and the loop body to use as the operation's own
  // body when the whole comparison turns out to be that one loop.
  root: Internal;
  inline: string;
  // Cache-key sentinel, eqOp or cmpOp, so a recursive `$ref` lands on the
  // matching compile rather than the other direction's.
  op: Internal;
  k: Mode;
};

// Whether this loop is the entire comparison, in which case it is the body
// rather than something the body calls. `whole` is the caller saying nothing of
// its own is conjoined with the loop - a tuple's fixed items, an object's
// declared properties.
const isRoot = (ctx: Ctx, schema: Internal, whole: boolean): boolean =>
  whole && schema === ctx.root && !ctx.inline;

const sameValueZero = (a: string, b: string): string => `(${a}===${b}||${a}!=${a}&&${b}!=${b})`;
const eqPair = (a: string, b: string): string => `${a}===${b}`;
// Ordered counterpart of `===` for a comparable primitive. `:0` is load-bearing
// for `joinOrd`: a chain of these strips the trailing `0` and becomes one nested
// ternary.
const ordLeaf = (a: string, b: string): string => `${a}<${b}?-1:${a}>${b}?1:0`;
// Number that can be NaN: `<`/`>` are both false for NaN, so the SameValueZero
// zero-case sits in the middle and a remaining NaN-vs-number is NaN-first.
const nanOrdLeaf = (a: string, b: string): string =>
  `${a}<${b}?-1:${a}>${b}?1:${a}===${b}||${a}!=${a}&&${b}!=${b}?0:${a}!=${a}?-1:1`;
// `S.env` is a string schema that also admits `undefined`. `<` is 0 for that
// pair, which would disagree with `===`. Undefined first, then string order.
const envOrd = (a: string, b: string): string =>
  `${a}===void 0?${b}===void 0?0:-1:${b}===void 0?1:${ordLeaf(a, b)}`;

// The built-in classes an instance compares by content rather than by identity:
// 1 a Date, 2 a URL, 3 a typed array, 4 a Set, FormData or URLSearchParams,
// which the structural fallback already reads by content and so needs no emit
// of its own. 0 is everything else, a Blob or a user class, which has only its
// identity to compare, and is therefore also the one kind of instance a union
// can collapse to a bare `===`.
//
// A typed array constructor carries `BYTES_PER_ELEMENT` and is indexed by
// `length`; DataView, the other `ArrayBuffer.isView` shape, has neither.
const valueClass = (class_: unknown): number =>
  class_ === (Date as unknown)
    ? 1
    : class_ === (URL as unknown)
      ? 2
      : (class_ as { BYTES_PER_ELEMENT?: number } | undefined)?.BYTES_PER_ELEMENT !== U
        ? 3
        : class_ === (Set as unknown) ||
            (class_ !== U &&
              (class_ === globalClass("FormData") || class_ === globalClass("URLSearchParams")))
          ? 4
          : 0;

// The equality-only emit, or the refusal for a schema compare has no order for.
// Every field only `EqMode` declares is reached through this, so a branch that
// forgets the refusal does not typecheck rather than crashing at runtime on a
// field the compare mode never had.
const eqOnly = (ctx: Ctx, schema: Internal): EqMode =>
  ctx.k.reject ? ctx.k.reject(schema) : ctx.k;

// Every shape with no emit of its own ends here: `S.unknown`, a function, a
// Set, a FormData, and the unions that fall back rather than dispatch.
const deep = (ctx: Ctx, schema: Internal, a: string, b: string): string =>
  `${(ctx.q ||= B_embedPure(ctx.b, eqOnly(ctx, schema).deep))}(${a},${b})`;

const and = (left: string, right: string): string =>
  left ? (right ? `${left}&&${right}` : left) : right;

// A leaf that ends `:0` is rewritten as the next test in the same ternary
// (`a.x<b.x?-1:a.x>b.x?1:` + next). Anything else (a hoisted call, a union,
// a symbol) uses `||` because -1 and 1 are truthy and 0 is not.
const joinOrd = (left: string, right: string): string =>
  left
    ? right
      ? left.endsWith(":0")
        ? left.slice(0, -1) + right
        : `(${left})||(${right})`
      : left
    : right;

// A hoisted comparator, called. Empty when the loop became the body instead, in
// which case there is nothing left to call and nothing left to conjoin.
const call = (fn: string, a: string, b: string): string => (fn ? `${fn}(${a},${b})` : "");

// A comparator for a shape that needs statements: a loop over a length or a
// key set. Compiled once per schema into the SAME embed array the rest of the
// operation reads, so a nested comparison is one call to a compiled function
// rather than a closure allocated per element, and generated code keeps to the
// two free names (`e` and the operation's own argument) the goldens allow.
//
// The trade-off this makes: like an embedded transform or a recursive
// operation, the loop body is behind `e[N]` rather than inline in the golden.
// The examples are what hold it: a spec runs every one of them through this.
const hoist = (ctx: Ctx, schema: Internal, body: () => string): string => {
  const cached = ctx.h.get(schema);
  if (cached !== U) return cached;
  const embeds = ctx.b.g.e;
  // The slot is reserved before the body is built so a shape reached again
  // while building it lands on this one instead of compiling a second copy.
  const index = embeds.push(U) - 1;
  const ref = `e[${index}]`;
  ctx.h.set(schema, ref);
  embeds[index] = new Function("e", `return ${body()}`)(embeds);
  return ref;
};

// `undefined` element schema means the members compare with the mode's `elem`
// (typed arrays: `===` / `<`/`>`, never NaN). `from` is also how the caller
// says whether anything precedes the loop: only a tuple's fixed items start it
// anywhere but 0.
const indexedFn = (ctx: Ctx, schema: Internal, item: Internal | undefined, from: number): string => {
  // A length the schema does not fix means the comparison is a loop, and a loop
  // is not the inlined chain compare answers with.
  const k = eqOnly(ctx, schema);
  const stmts = (): string => {
    const element = item === U ? k.elem("a[i]", "b[i]") : eqExpr(ctx, item, "a[i]", "b[i]");
    return k.index(element, from);
  };
  return isRoot(ctx, schema, !from)
    ? ((ctx.inline = stmts()), "")
    : hoist(ctx, schema, () => `(a,b)=>{${stmts()}}`);
};

// Key sets must match, so the count is walked on both sides: a key present on
// one with an `undefined` value is not the same value as a key absent from the
// other, even though reading both yields `undefined`.
//
// `k in b` is not redundant with that count. Two dicts of equal size can still
// name different keys, so the count alone would read `{a: 1}` and `{b: 1}` as
// one value. It is also what makes the element comparison safe: reading a
// non-primitive through `b[k]` for a key `b` doesn't have is a read off
// `undefined`.
const dictFn = (ctx: Ctx, schema: Internal, value: Internal): string => {
  const k = eqOnly(ctx, schema);
  const stmts = (): string => k.dict((a, b) => eqExpr(ctx, value, a, b));
  return isRoot(ctx, schema, true)
    ? ((ctx.inline = stmts()), "")
    : hoist(ctx, schema, () => `(a,b)=>{${stmts()}}`);
};

const objectExpr = (ctx: Ctx, schema: Internal, a: string, b: string): string => {
  // The one rejected shape whose emit is an inlined chain like compare's own,
  // so nothing below this forces the refusal: an object's fields would compare
  // in JS property order, integer-like names first, which is not the order the
  // schema was written in.
  eqOnly(ctx, schema);
  const properties = schema.properties;
  const rest = schema.additionalItems;
  const value = typeof rest === "string" ? U : rest;
  const keys = properties !== U ? Object.keys(properties) : [];
  if (value !== U) {
    // Declared properties and a rest schema at once would need both rules
    // applied to one key set; no factory builds that today, and the structural
    // fallback stays correct if one ever does.
    if (keys.length) return deep(ctx, schema, a, b);
    return call(dictFn(ctx, schema, value), a, b);
  }
  let out = "";
  for (let idx = 0; idx < keys.length; idx++) {
    const key = keys[idx]!;
    out = ctx.k.join(
      out,
      eqExpr(ctx, properties![key]!, inlinedProperty(a, key), inlinedProperty(b, key)),
    );
  }
  return out;
};

const arrayExpr = (ctx: Ctx, schema: Internal, a: string, b: string): string => {
  const items = schema.items;
  const rest = schema.additionalItems;
  const value = typeof rest === "string" ? U : rest;
  const fixed = items !== U ? items.length : 0;
  let out = "";
  for (let idx = 0; idx < fixed; idx++) {
    // A tuple's length is fixed on both sides by conformance, so only the rest
    // form below pays for a length check.
    const at = String(idx);
    out = ctx.k.join(
      out,
      eqExpr(ctx, items![idx]!, inlinedProperty(a, at, true), inlinedProperty(b, at, true)),
    );
  }
  if (value === U) return out;
  return ctx.k.join(out, call(indexedFn(ctx, schema, value, fixed), a, b));
};

// A narrow is built ONCE, as a template with this placeholder standing in for
// the value, and the value is substituted per use. Rebuilding it per use would
// embed a member's class a second and third time, and then two members of the
// same class would read as different tests, which is exactly the ambiguity the
// distinctness check below exists to catch.
// NUL is not in `inlineUnsafeRe` (base.ts), so a string literal holding one is
// spliced raw and would read as a second placeholder. Every part built from
// user data is checked against it, and a union that can't be templated safely
// takes the structural comparison instead.
const V = "\0";
const applyNarrow = (template: string, value: string): string => template.split(V).join(value);
// `unknown`, not `string`: B_inlineConst hands back a number or boolean const
// as itself and leaves the coercion to the interpolation that splices it.
const templatable = (part: unknown): boolean => !`${part}`.includes(V);

// A NaN const is the one value `===` can't match.
const isNanConst = (schema: Internal): boolean => {
  const c = schema.const;
  return typeof c === "number" && c !== c;
};

// Every tag `typeCheckCond` has a test for. Anything else, a nested union or a
// ref or `S.unknown` or a function, has no narrow, and a union containing one
// falls back to the structural comparison rather than emitting an empty test.
const NARROWABLE = 2 | 4 | 8 | 16 | 32 | 64 | 128 | 1024 | 2048 | 8192 | 16384;

// What a union member is told apart by, or `undefined` when nothing tells it
// apart from the rest.
const narrowOf = (ctx: Ctx, member: Internal): string | undefined => {
  if (isLiteral(member)) {
    if (isNanConst(member)) return `${V}!=${V}`;
    const inlined = B_inlineConst(ctx.b, member);
    return templatable(inlined) ? `${V}===${inlined}` : U;
  }
  if (!(tagFlags[member.type]! & NARROWABLE)) return U;
  return typeCheckCond(ctx.b, member, V);
};

// A property every object member declares as a distinct literal: the tag of a
// tagged union, and the only thing that tells two object members apart.
const discriminantOf = (members: Internal[]): string | undefined => {
  const first = members[0]!.properties;
  if (first === U) return U;
  const keys = Object.keys(first);
  for (let idx = 0; idx < keys.length; idx++) {
    const key = keys[idx]!;
    const seen = new Set<unknown>();
    let ok = true;
    for (let m = 0; m < members.length; m++) {
      const property = members[m]!.properties?.[key];
      if (property === U || !isLiteral(property) || seen.has(property.const)) {
        ok = false;
        break;
      }
      seen.add(property.const);
    }
    if (ok) return key;
  }
  return U;
};

// Whether a value of this schema compares by `===` alone, asked before
// emitting a union, since a union of such members is `===` too whichever
// members the two values land in. A predicate rather than a trial emit: an emit
// whose result is discarded still leaves its embeds and hoisted functions
// behind.
const isIdentity = (ctx: Ctx, schema: Internal): boolean => {
  if (isLiteral(schema)) {
    const c = schema.const;
    // NaN is the one const `===` can't match.
    return !(typeof c === "number" && c !== c);
  }
  const tagFlag = tagFlags[schema.type]!;
  if (tagFlag & (2 | 8 | 16 | 32 | 1024 | 16384)) return true;
  if (tagFlag & 4) return !(ctx.b.g.o & 2);
  // An instance with nothing but its identity to compare is `===` too, so a
  // union of them needs no dispatch at all.
  if (tagFlag & 8192) return !valueClass(schema.class);
  if (tagFlag & 256) {
    const members = schema.anyOf!;
    for (let idx = 0; idx < members.length; idx++) {
      if (!isIdentity(ctx, members[idx]!)) return false;
    }
    return true;
  }
  return false;
};

// The comparable primitive tag every identity member shares, or `undefined`
// when they don't share one. string 2, number 4, boolean 8, bigint 1024 are
// the tags `<`/`>` order without coercing across kinds (`1` vs `"1"`).
const identityKind = (ctx: Ctx, schema: Internal): number | undefined => {
  if (isLiteral(schema)) {
    const c = schema.const;
    const t = typeof c;
    return t === "string"
      ? 2
      : t === "number"
        ? c !== c
          ? U
          : 4
        : t === "boolean"
          ? 8
          : t === "bigint"
            ? 1024
            : U;
  }
  const tagFlag = tagFlags[schema.type]!;
  // `S.env` is tagged string and admits undefined, so `<` is not a total
  // order on the values it holds.
  if (schema.format === "env") return U;
  if (tagFlag & (2 | 4 | 8 | 1024)) return tagFlag & (2 | 4 | 8 | 1024);
  if (tagFlag & 256) {
    const members = schema.anyOf!;
    let kind: number | undefined;
    for (let idx = 0; idx < members.length; idx++) {
      const next = identityKind(ctx, members[idx]!);
      if (next === U || (kind !== U && next !== kind)) return U;
      kind = next;
    }
    return kind;
  }
  return U;
};

const unionExpr = (ctx: Ctx, schema: Internal, a: string, b: string): string => {
  const members = schema.anyOf!;
  // Covers `S.optional`/`S.nullable` of a primitive, enums, and any union of
  // plain literals.
  if (isIdentity(ctx, schema)) {
    const closed = ctx.k.identUnion(ctx, schema, a, b);
    if (closed !== U) return closed;
  }

  // A narrow can embed (a class to test with `instanceof`, a symbol const), and
  // a union that then falls back has no use for what it embedded. Rolling the
  // array back keeps those slots from showing up as gaps in an emit that never
  // reads them. Safe to truncate: nothing else pushes until the arms are built,
  // which is after every fallback below.
  const embedMark = ctx.b.g.e.length;
  const abandon = (): string => (
    (ctx.b.g.e.length = embedMark), deep(ctx, schema, a, b)
  );
  const narrows: string[] = [];
  const objects: Internal[] = [];
  for (let idx = 0; idx < members.length; idx++) {
    const narrow = narrowOf(ctx, members[idx]!);
    if (narrow === U) return abandon();
    narrows.push(narrow);
    if (members[idx]!.properties !== U) objects.push(members[idx]!);
  }
  // Members sharing a narrow don't dispatch. Objects can still be separated by
  // a discriminant; anything else falls back to the structural comparison,
  // which is slower but never picks the wrong arm.
  if (new Set(narrows).size !== narrows.length) {
    const key = objects.length > 1 ? discriminantOf(objects) : U;
    if (key === U) return abandon();
    if (!templatable(key)) return abandon();
    // A null or undefined member has no property to read, and the arms test
    // the discriminant before they reach it, on either value.
    const prop = inlinedProperty("", key);
    const at = members.some((member) => tagFlags[member.type]! & 48)
      ? `${V}?${prop[0] === "." ? "" : "."}${prop}`
      : V + prop;
    for (let idx = 0; idx < members.length; idx++) {
      const property = members[idx]!.properties?.[key];
      if (property !== U && isLiteral(property)) {
        if (isNanConst(property)) {
          narrows[idx] = `${at}!=${at}`;
        } else {
          const inlined = B_inlineConst(ctx.b, property);
          if (!templatable(inlined)) return abandon();
          narrows[idx] = `${at}===${inlined}`;
        }
      }
    }
    if (new Set(narrows).size !== narrows.length) return abandon();
  }

  // `S.optional(X)`, `S.nullable(X)`, `S.nullish(X)`: nullish literals around
  // one member of substance, and the shape most schemas that reach here have.
  // Testing the LITERALS is what earns it a case of its own: `a===null` in
  // place of the four operators X's own narrow would write, and `b!=null` in
  // place of them again for `b`. Sound only here - null and undefined are the
  // two values no other narrow admits, so moving X last raises no question of
  // overlap, and "b is neither" says b is X because X is all that is left.
  //
  // undefined 16, null 32. Either tag narrows to `===void 0` / `===null`
  // whether the member is the type or the literal, so the tag is the whole test.
  let restIdx = -1;
  let rest = 0;
  let nullish = 0;
  for (let idx = 0; idx < members.length; idx++) {
    const flag = tagFlags[members[idx]!.type]! & 48;
    if (flag) {
      nullish |= flag;
    } else {
      restIdx = idx;
      rest++;
    }
  }
  if (nullish && rest === 1) {
    const present = nullish === 48 ? `${b}!=null` : `${b}!==${nullish & 32 ? "null" : "void 0"}`;
    return ctx.k.nullish(narrows, restIdx, eqExpr(ctx, members[restIdx]!, a, b), a, b, present);
  }

  // Distinct narrows are not disjoint ones, and an arm reads its member's
  // fields off BOTH values, so a narrow that admits another member is a read
  // off the wrong shape: `S.union([S.schema({a: S.string}), S.date])` tests a
  // Date with `typeof ==="object"` and then compares `.a` - undefined on both
  // sides, which reads as equal, or a crash one field deeper.
  //
  // Only this one pair overlaps. `typeof` tells the primitives apart,
  // objectTagCond excludes arrays, and `Array.isArray` is false for every
  // instance of a class that does not extend Array (none is reachable: the
  // array tag is not something `S.instance` builds). So conjoining the
  // instances' own `instanceof` tests, negated, onto the object narrow is what
  // makes the set disjoint - and it reuses their embed slots rather than
  // adding any. A bare-value conjunct is what marks a narrow as objectTagCond's:
  // a member the discriminant pass rewrote reads a property instead, which is
  // already false for an instance.
  const instanceNarrows: string[] = [];
  const objectTagged: number[] = [];
  for (let idx = 0; idx < members.length; idx++)
    if (tagFlags[members[idx]!.type]! & 8192) instanceNarrows.push(narrows[idx]!);
    else if (narrows[idx]!.split("&&").includes(V)) objectTagged.push(idx);
  const exclude = instanceNarrows.map((narrow) => `&&!(${narrow})`).join("");
  for (let at = 0; at < objectTagged.length; at++) narrows[objectTagged[at]!] += exclude;

  // A union that dispatches reads its members off a type narrow, and a narrow
  // is a branch where compare answers with a chain. The two unions compare does
  // answer for returned above, before any of this.
  return eqOnly(ctx, schema).union(
    narrows,
    objectTagged,
    members,
    (s, x, y) => eqExpr(ctx, s, x, y),
    a,
    b,
  );
};

const refExpr = (ctx: Ctx, schema: Internal, a: string, b: string): string => {
  // `S.json` describes every JSON value, which is precisely what the structural
  // comparison already covers, and unrolling its `$ref` cycle would emit a
  // comparator for each arm of it.
  //
  // A ref a compiler built carries its definition (`Internal.definition`). None
  // reaches here from the public API - a codec's declared sides are the schema
  // its author wrote - but one that did would otherwise compare as whatever the
  // record holds under its name.
  const def =
    schema.name === jsonName ? U : schema.definition || ctx.d?.[schema["$ref"]!.slice(8)];
  if (def === U) return deep(ctx, schema, a, b);
  // A def compiles to its own function and is called, not inlined.
  eqOnly(ctx, schema);
  const flag = ctx.b.g.o;
  const existing = findOpNode(def, ctx.op, def, flag);
  if (existing !== U) {
    // `v === 0` is a def still being compiled, a self-reference. The NODE is
    // embedded, so the call reaches whatever it ends up holding.
    return existing.v === 0
      ? `${B_embedPure(ctx.b, existing)}.v(${a},${b})`
      : `${B_embedPure(ctx.b, existing.v)}(${a},${b})`;
  }
  const node = addOpNode(def, [ctx.op, def], flag, 0);
  try {
    node.v = compile(def, flag, ctx.op, ctx.k) as unknown as OpNode["v"];
  } catch (exn) {
    // A node left at the sentinel would read as a live circular reference on
    // the next attempt, and calling 0 is what that would compile to.
    removeOpNode(def, node);
    throw exn;
  }
  return `${B_embedPure(ctx.b, node.v)}(${a},${b})`;
};

// "" means "always equal": a position that admits exactly one value contributes
// no test at all, which is what keeps a literal field out of the emit.
const eqExpr = (ctx: Ctx, schema: Internal, a: string, b: string): string => {
  const defs = schema["$defs"];
  if (defs !== U) ctx.d = ctx.d ? Object.assign({}, ctx.d, defs) : defs;
  // A literal, `S.nan` included: both values are the one the schema admits.
  if (isLiteral(schema)) return "";
  const tagFlag = tagFlags[schema.type]!;
  // string 2, number 4, boolean 8, undefined 16, null 32, bigint 1024, symbol 16384
  if (tagFlag & (2 | 4 | 8 | 16 | 32 | 1024 | 16384)) return ctx.k.prim(ctx, schema, a, b);
  if (tagFlag & 64) return objectExpr(ctx, schema, a, b);
  if (tagFlag & 128) return arrayExpr(ctx, schema, a, b);
  if (tagFlag & 256) return unionExpr(ctx, schema, a, b);
  if (tagFlag & 8192) {
    const kind = valueClass(schema.class);
    // `+date` is `getTime()`; an invalid one reads NaN, which the `===` the top
    // level opens with is what keeps reflexive.
    if (kind === 1) return ctx.k.date(a, b);
    if (kind === 2) return ctx.k.url(a, b);
    if (kind === 3) return call(indexedFn(ctx, schema, U, 0), a, b);
    if (kind) return deep(ctx, schema, a, b);
    // Nothing but its identity to compare: equal or not, never ordered.
    return eqOnly(ctx, schema).ident(a, b);
  }
  if (tagFlag & 512) return refExpr(ctx, schema, a, b);
  // unknown 1, nan 2048, function 4096, never 32768.
  return deep(ctx, schema, a, b);
};

const alwaysEqual: IsEqual = () => true;
const strictEqual: IsEqual = (a, b) => a === b;
const sameValueZeroEqual: IsEqual = (a, b) => a === b || (a !== a && b !== b);
const alwaysCompare: Compare = () => 0;
const strictCompare: Compare = (a, b) =>
  (a as number) < (b as number) ? -1 : (a as number) > (b as number) ? 1 : 0;
const nanCompare: Compare = (a, b) =>
  (a as number) < (b as number)
    ? -1
    : (a as number) > (b as number)
      ? 1
      : a === b || (a !== a && b !== b)
        ? 0
        : a !== a
          ? -1
          : 1;

const eqIndex = (element: string, from: number): string =>
  `let n=a.length;if(n!==b.length)return false;` +
  (element ? `for(let i=${from};i<n;i++)if(!(${element}))return false;` : ``) +
  `return true`;

const eqDict = (walk: (a: string, b: string) => string): string => {
  const element = walk("a[k]", "b[k]");
  return (
    `let n=0;for(const k in a){if(!(k in b)` +
    (element ? `||!(${element})` : ``) +
    `)return false;n++}for(const k in b)n--;return !n`
  );
};

const eqPrim = (ctx: Ctx, schema: Internal, a: string, b: string): string => {
  const tagFlag = tagFlags[schema.type]!;
  if (tagFlag & 4) return ctx.b.g.o & 2 ? sameValueZero(a, b) : eqPair(a, b);
  return eqPair(a, b);
};

const cmpPrim = (ctx: Ctx, schema: Internal, a: string, b: string): string => {
  const tagFlag = tagFlags[schema.type]!;
  if (tagFlag & 4) return ctx.b.g.o & 2 ? nanOrdLeaf(a, b) : ordLeaf(a, b);
  if (tagFlag & (16 | 32)) return "";
  // symbol 16384: `<` on two symbols is a TypeError, so this one is refused at
  // the leaf rather than by a walk branch. A symbol LITERAL is one value and
  // never reaches here.
  if (tagFlag & 16384) unorderable(schema);
  if (tagFlag & 2 && schema.format === "env") return envOrd(a, b);
  return ordLeaf(a, b);
};

const eqNullish = (
  narrows: string[],
  restIdx: number,
  rest: string,
  a: string,
  b: string,
  present: string,
): string => {
  let out = and(present, rest) || "true";
  for (let idx = narrows.length - 1; idx >= 0; idx--)
    if (idx !== restIdx)
      out = `${applyNarrow(narrows[idx]!, a)}?${applyNarrow(narrows[idx]!, b)}:${out}`;
  return `(${out})`;
};

const cmpNullish = (
  narrows: string[],
  restIdx: number,
  rest: string,
  a: string,
  b: string,
  _present: string,
): string => {
  let out = rest || "0";
  for (let idx = narrows.length - 1; idx >= 0; idx--)
    if (idx !== restIdx) {
      const na = applyNarrow(narrows[idx]!, a);
      const nb = applyNarrow(narrows[idx]!, b);
      out = `${na}?${nb}?0:-1:${nb}?1:${out}`;
    }
  return `(${out})`;
};

const eqUnion = (
  narrows: string[],
  objectTagged: number[],
  members: Internal[],
  walk: Walk,
  a: string,
  b: string,
): string => {
  let out = "";
  for (let idx = members.length - 1; idx >= 0; idx--) {
    const arm = and(applyNarrow(narrows[idx]!, b), walk(members[idx]!, a, b)) || "true";
    out = out === "" ? arm : `${applyNarrow(narrows[idx]!, a)}?${arm}:${out}`;
  }
  // A narrow on `b` sits in value position, where `&&` yields the operand that
  // failed rather than `false`. Every conjunct `typeCheckCond` writes is a
  // comparison except that same bare value, so an object member is also the one
  // - and only - narrow whose arm can answer `null` instead of a boolean.
  const coerce = objectTagged.length > 0;
  return members.length > 1 || coerce ? `${coerce ? "!!" : ""}(${out})` : out;
};

const identUnionCmp = (ctx: Ctx, schema: Internal, a: string, b: string): string | undefined => {
  const kind = identityKind(ctx, schema);
  return kind !== U ? ordLeaf(a, b) : U;
};

const eqMode: Mode = {
  join: and,
  prim: eqPrim,
  elem: eqPair,
  date: (a, b) => `+${a}===+${b}`,
  url: (a, b) => `${a}.href===${b}.href`,
  ident: eqPair,
  index: eqIndex,
  dict: eqDict,
  identUnion: (_ctx, _schema, a, b) => eqPair(a, b),
  nullish: eqNullish,
  union: eqUnion,
  yes: "true",
  wrapPrefix: "a===b||",
  always: alwaysEqual,
  strict: strictEqual,
  nan: sameValueZeroEqual,
  strictExpr: "a===b",
  nanExpr: "(a===b||a!=a&&b!=b)",
  deep: deepEqual,
};

const unorderable = (schema: Internal): never =>
  panic(
    `Can't compare ${inputExpression(schema)}. Only primitives, Date, URL and tuples ` +
      `of them are orderable. Use isEqual for equality`,
  );

const cmpMode: Mode = {
  join: joinOrd,
  prim: cmpPrim,
  // Two invalid dates are two NaNs; `+a===+b` is false, matching isEqual.
  date: (a, b) => `+${a}<+${b}?-1:+${a}>+${b}?1:+${a}===+${b}?0:1`,
  url: (a, b) => ordLeaf(`${a}.href`, `${b}.href`),
  reject: unorderable,
  identUnion: identUnionCmp,
  nullish: cmpNullish,
  yes: "0",
  wrapPrefix: "a===b?0:",
  always: alwaysCompare,
  strict: strictCompare,
  nan: nanCompare,
  strictExpr: "a<b?-1:a>b?1:0",
  nanExpr: "a<b?-1:a>b?1:a===b||a!=a&&b!=b?0:a!=a?-1:1",
};

const compile = (schema: Internal, flag: Flag, op: Internal, mode: Mode): IsEqual | Compare => {
  const b = B_operationArg(schema, schema, flag, U);
  const ctx: Ctx = { b, d: U, h: new Map(), q: "", root: schema, inline: "", op, k: mode };
  const expr = eqExpr(ctx, schema, "a", "b");
  // A root array, dict or typed array is one hoisted call and nothing else, and
  // at the root that call buys nothing: the function it points at IS the body.
  // Nested, the hoist still pays for itself, since the only other ways to put a
  // loop inside the conjunction an object compiles to are a closure per call or
  // no inlining at all.
  if (ctx.inline)
    return new Function("e", `return (a,b)=>{if(a===b)return ${mode.yes};${ctx.inline}}`)(
      b.g.e,
    ) as IsEqual | Compare;
  if (expr === "") return mode.always;
  if (expr === mode.strictExpr) return mode.strict;
  if (expr === mode.nanExpr) return mode.nan;
  return new Function("e", `return (a,b)=>${mode.wrapPrefix}${expr}`)(b.g.e) as IsEqual | Compare;
};

const compileOp = (schema: Internal, op: Internal, mode: Mode): IsEqual | Compare => {
  const flag = globalConfig.f;
  const existing = findOpNode(schema, op, schema, flag);
  if (existing !== U && existing.v !== 0) return existing.v as unknown as IsEqual | Compare;
  const node = addOpNode(schema, [op, schema], flag, 0);
  try {
    const fn = compile(schema, flag, op, mode);
    node.v = fn as unknown as OpNode["v"];
    return fn;
  } catch (exn) {
    removeOpNode(schema, node);
    throw exn;
  }
};

// @__NO_SIDE_EFFECTS__
export const compileIsEqual = (schema: Internal): IsEqual =>
  compileOp(schema, eqOp, eqMode) as IsEqual;

// @__NO_SIDE_EFFECTS__
export const compileCompare = (schema: Internal): Compare =>
  compileOp(schema, cmpOp, cmpMode) as Compare;