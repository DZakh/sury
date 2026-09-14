// `S.isEqualInput` / `S.isEqualOutput`: a compiled value-equality function for
// one side of a schema.
//
// Two things make this unlike every other operation. It takes a PAIR of values,
// which `compileDecoder`'s `i=>{…}` shape can't express, and it never
// validates: both values are assumed to conform, which is what lets an object
// compile to a bare conjunction of field reads with no type narrowing at all.
// So it walks the schema's own structure the way jsonschema.ts does rather than
// driving `parse`, and borrows only the pieces that don't presuppose a decode:
// the embed array, the union type-narrow, and the operation cache.
//
// The one invariant the emit must never break is reflexivity: `f(x, x)` is true
// for every value the side admits. NaN is the whole difficulty. It is the only
// value not `===` itself, so every position that can hold one compares with
// SameValueZero instead, and every position that cannot (the default number
// validation rejects NaN) keeps the bare `===`.

import {
  baseSchema,
  type Flag,
  globalConfig,
  inlinedProperty,
  type Internal,
  isLiteral,
  jsonName,
  neverTag,
  noopDecoder,
  tagFlags,
  U,
  type Val,
} from "./base";
import { B_embedPure, B_inlineConst, B_operationArg } from "./builder";
import { addOpNode, findOpNode, type OpNode, removeOpNode, typeCheckCond } from "./parse";

export type IsEqual = (a: unknown, b: unknown) => boolean;

// The cache key's first slot, so an eq node can't be mistaken for a decoder's
// (getDecoder never passes this schema) or for recursiveDecoder's `[input, def]`
// pair. Never compiled, never reversed: only its identity is read.
const eqOp: Internal = /* @__PURE__ */ baseSchema(neverTag, true, noopDecoder);

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
  // A Set by its members, a Map by its entries, neither by iteration order.
  // Both key their content by identity, so one value can sit on the two sides
  // under different identities: what the other side holds under the SAME
  // identity settles itself, which is the common case and the cheap one, and
  // the leftovers are matched against each other. A match is spent, since two
  // members one side calls distinct - two Dates at one instant - can both
  // equal a single one of the other's. The test is symmetric, so an entry is a
  // leftover on one side exactly when its partner is on the other, which is
  // what lets the two lists be built independently and still line up.
  if (proto === Set.prototype || proto === Map.prototype) {
    const isMap = proto === Map.prototype;
    const as = a as Set<unknown> & Map<unknown, unknown>;
    const bs = b as Set<unknown> & Map<unknown, unknown>;
    if (as.size !== bs.size) return false;
    const settled = (side: typeof as, entry: unknown): boolean => {
      if (!isMap) return side.has(entry);
      const pair = entry as [unknown, unknown];
      return side.has(pair[0]) && deepEqual(side.get(pair[0]), pair[1]);
    };
    let rest: unknown[] | undefined;
    for (const entry of as) if (!settled(bs, entry)) (rest ||= []).push(entry);
    if (rest === U) return true;
    // A Map's entry is a `[key, value]` array, which is what makes one
    // comparison serve both: the array branch above reads the pair.
    const left: unknown[] = [];
    for (const entry of bs) if (!settled(as, entry)) left.push(entry);
    for (const entry of rest) {
      let i = left.length;
      while (i--) if (deepEqual(entry, left[i])) break;
      if (i < 0) return false;
      left.splice(i, 1);
    }
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

type Ctx = {
  // Codegen context, for the embed array (`g.e`) and the op flag. Never
  // merged, never emitted: this operation has no Val chain.
  b: Val;
  d: Record<string, Internal> | undefined;
  // Hoisted comparators, keyed by the schema they compare so a shape reached
  // twice is compiled once.
  m: Map<Internal, string>;
  // The `deepEqual` embed, reused across positions.
  q: string;
  // The schema being compiled, and the loop body to use as the operation's own
  // body when the whole comparison turns out to be that one loop.
  root: Internal;
  inline: string;
};

// Whether this loop is the entire comparison, in which case it is the body
// rather than something the body calls. `whole` is the caller saying nothing of
// its own is conjoined with the loop - a tuple's fixed items, an object's
// declared properties.
const isRoot = (ctx: Ctx, schema: Internal, whole: boolean): boolean =>
  whole && schema === ctx.root && !ctx.inline;

const sameValueZero = (a: string, b: string): string => `(${a}===${b}||${a}!=${a}&&${b}!=${b})`;

// The built-in classes an instance compares by content rather than by identity:
// 1 a Date, 2 a URL, 3 a typed array, 4 a Set, a Map, FormData or
// URLSearchParams, which the structural fallback already reads by content and
// so needs no emit of its own. 0 is everything else, a Blob or a user class,
// which has only its identity to compare, and is therefore also the one kind of
// instance a union can collapse to a bare `===`.
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
            class_ === (Map as unknown) ||
            (class_ !== U &&
              (class_ === globalClass("FormData") || class_ === globalClass("URLSearchParams")))
          ? 4
          : 0;

const deep = (ctx: Ctx, a: string, b: string): string =>
  `${(ctx.q ||= B_embedPure(ctx.b, deepEqual))}(${a},${b})`;

const and = (left: string, right: string): string =>
  left ? (right ? `${left}&&${right}` : left) : right;

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
  const cached = ctx.m.get(schema);
  if (cached !== U) return cached;
  const embeds = ctx.b.g.e;
  // The slot is reserved before the body is built so a shape reached again
  // while building it lands on this one instead of compiling a second copy.
  const index = embeds.push(U) - 1;
  const ref = `e[${index}]`;
  ctx.m.set(schema, ref);
  embeds[index] = new Function("e", `return ${body()}`)(embeds);
  return ref;
};

// `undefined` element schema means the members compare with `===`: the typed
// array case, whose elements are numbers or bigints and cannot be NaN.
// `from` is also how the caller says whether anything precedes the loop: only a
// tuple's fixed items start it anywhere but 0.
const indexedFn = (ctx: Ctx, schema: Internal, item: Internal | undefined, from: number): string => {
  const stmts = (): string => {
    const element = item === U ? `a[i]===b[i]` : eqExpr(ctx, item, "a[i]", "b[i]");
    return (
      `let n=a.length;if(n!==b.length)return false;` +
      (element ? `for(let i=${from};i<n;i++)if(!(${element}))return false;` : ``) +
      `return true`
    );
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
  const stmts = (): string => {
    const element = eqExpr(ctx, value, "a[k]", "b[k]");
    return (
      `let n=0;for(const k in a){if(!(k in b)` +
      (element ? `||!(${element})` : ``) +
      `)return false;n++}for(const k in b)n--;return !n`
    );
  };
  return isRoot(ctx, schema, true)
    ? ((ctx.inline = stmts()), "")
    : hoist(ctx, schema, () => `(a,b)=>{${stmts()}}`);
};

const objectExpr = (ctx: Ctx, schema: Internal, a: string, b: string): string => {
  const properties = schema.properties;
  const rest = schema.additionalItems;
  const value = typeof rest === "string" ? U : rest;
  const keys = properties !== U ? Object.keys(properties) : [];
  if (value !== U) {
    // Declared properties and a rest schema at once would need both rules
    // applied to one key set; no factory builds that today, and the structural
    // fallback stays correct if one ever does.
    if (keys.length) return deep(ctx, a, b);
    return call(dictFn(ctx, schema, value), a, b);
  }
  let out = "";
  for (let idx = 0; idx < keys.length; idx++) {
    const key = keys[idx]!;
    out = and(
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
    out = and(out, eqExpr(ctx, items![idx]!, inlinedProperty(a, at, true), inlinedProperty(b, at, true)));
  }
  if (value === U) return out;
  return and(out, call(indexedFn(ctx, schema, value, fixed), a, b));
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

const unionExpr = (ctx: Ctx, schema: Internal, a: string, b: string): string => {
  const members = schema.anyOf!;
  // Covers `S.optional`/`S.nullable` of a primitive, enums, and any union of
  // plain literals.
  if (isIdentity(ctx, schema)) return `${a}===${b}`;

  // A narrow can embed (a class to test with `instanceof`, a symbol const), and
  // a union that then falls back has no use for what it embedded. Rolling the
  // array back keeps those slots from showing up as gaps in an emit that never
  // reads them. Safe to truncate: nothing else pushes until the arms are built,
  // which is after every fallback below.
  const embedMark = ctx.b.g.e.length;
  const abandon = (): string => ((ctx.b.g.e.length = embedMark), deep(ctx, a, b));
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
    const at = inlinedProperty(V, key);
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
    let out = and(present, eqExpr(ctx, members[restIdx]!, a, b)) || "true";
    for (let idx = members.length - 1; idx >= 0; idx--)
      if (idx !== restIdx)
        out = `${applyNarrow(narrows[idx]!, a)}?${applyNarrow(narrows[idx]!, b)}:${out}`;
    return `(${out})`;
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

  // `narrow(a) ? narrow(b) && eq : …`. The last arm still tests `b`: `a` being
  // that member says nothing about which member `b` is. Parenthesized because
  // `?:` binds looser than every operator a caller splices this into.
  let out = "";
  for (let idx = members.length - 1; idx >= 0; idx--) {
    const arm = and(applyNarrow(narrows[idx]!, b), eqExpr(ctx, members[idx]!, a, b)) || "true";
    out = out === "" ? arm : `${applyNarrow(narrows[idx]!, a)}?${arm}:${out}`;
  }
  // A narrow on `b` sits in value position, where `&&` yields the operand that
  // failed rather than `false`. Every conjunct `typeCheckCond` writes is a
  // comparison except that same bare value, so an object member is also the one
  // - and only - narrow whose arm can answer `null` instead of a boolean.
  // Coerced here rather than around the whole body, which would pay for it at
  // every union.
  const coerce = objectTagged.length > 0;
  return members.length > 1 || coerce ? `${coerce ? "!!" : ""}(${out})` : out;
};

const refExpr = (ctx: Ctx, schema: Internal, a: string, b: string): string => {
  // `S.json` describes every JSON value, which is precisely what the structural
  // comparison already covers, and unrolling its `$ref` cycle would emit a
  // comparator for each arm of it.
  const def = schema.name === jsonName ? U : ctx.d?.[schema["$ref"]!.slice(8)];
  if (def === U) return deep(ctx, a, b);
  const flag = ctx.b.g.o;
  const existing = findOpNode(def, eqOp, def, flag);
  if (existing !== U) {
    // `v === 0` is a def still being compiled, a self-reference. The NODE is
    // embedded, so the call reaches whatever it ends up holding.
    return existing.v === 0
      ? `${B_embedPure(ctx.b, existing)}.v(${a},${b})`
      : `${B_embedPure(ctx.b, existing.v)}(${a},${b})`;
  }
  const node = addOpNode(def, [eqOp, def], flag, 0);
  try {
    node.v = compile(def, flag) as unknown as OpNode["v"];
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
  // string 2, boolean 8, undefined 16, null 32, bigint 1024, symbol 16384
  if (tagFlag & (2 | 8 | 16 | 32 | 1024 | 16384)) return `${a}===${b}`;
  // number 4: NaN reaches a value only when its validation is off (flag 2).
  if (tagFlag & 4) return ctx.b.g.o & 2 ? sameValueZero(a, b) : `${a}===${b}`;
  if (tagFlag & 64) return objectExpr(ctx, schema, a, b);
  if (tagFlag & 128) return arrayExpr(ctx, schema, a, b);
  if (tagFlag & 256) return unionExpr(ctx, schema, a, b);
  if (tagFlag & 8192) {
    const kind = valueClass(schema.class);
    // `+date` is `getTime()`; an invalid one reads NaN, which the `===` the top
    // level opens with is what keeps reflexive.
    if (kind === 1) return `+${a}===+${b}`;
    if (kind === 2) return `${a}.href===${b}.href`;
    if (kind === 3) return call(indexedFn(ctx, schema, U, 0), a, b);
    return kind ? deep(ctx, a, b) : `${a}===${b}`;
  }
  if (tagFlag & 512) return refExpr(ctx, schema, a, b);
  // unknown 1, function 4096, never 32768.
  return deep(ctx, a, b);
};

// The three bodies a schema with no structure to walk compiles to, written out
// rather than evaluated: every primitive, literal and opaque instance lands on
// one of them, so this is most schemas, and `noopOperation` (builder.ts) is the
// same trade for the identity decoder. A caller sees a shared function where it
// would otherwise see a fresh one, which only shows in `===` on the operation
// itself, never in an answer.
const alwaysEqual: IsEqual = () => true;
const strictEqual: IsEqual = (a, b) => a === b;
// NaN is the one value `===` can't match, so a position that admits one needs
// SameValueZero. Reached only with number validation off (flag 2).
const sameValueZeroEqual: IsEqual = (a, b) => a === b || (a !== a && b !== b);

const compile = (schema: Internal, flag: Flag): IsEqual => {
  const b = B_operationArg(schema, schema, flag, U);
  const ctx: Ctx = { b, d: U, m: new Map(), q: "", root: schema, inline: "" };
  const expr = eqExpr(ctx, schema, "a", "b");
  // A root array, dict or typed array is one hoisted call and nothing else, and
  // at the root that call buys nothing: the function it points at IS the body.
  // Nested, the hoist still pays for itself, since the only other ways to put a
  // loop inside the conjunction an object compiles to are a closure per call or
  // no inlining at all.
  if (ctx.inline)
    return new Function("e", `return (a,b)=>{if(a===b)return true;${ctx.inline}}`)(b.g.e) as IsEqual;
  if (expr === "") return alwaysEqual;
  if (expr === "a===b") return strictEqual;
  if (expr === sameValueZero("a", "b")) return sameValueZeroEqual;
  // `a===b` first for everything with structure: it answers the identical-value
  // case in one comparison, and it is what keeps reflexivity for a value the
  // schema's own emit can't compare (a Blob, an opaque instance).
  return new Function("e", `return (a,b)=>a===b||${expr}`)(b.g.e) as IsEqual;
};

// @__NO_SIDE_EFFECTS__
export const compileIsEqual = (schema: Internal): IsEqual => {
  const flag = globalConfig.f;
  const existing = findOpNode(schema, eqOp, schema, flag);
  if (existing !== U && existing.v !== 0) return existing.v as unknown as IsEqual;
  const node = addOpNode(schema, [eqOp, schema], flag, 0);
  try {
    const fn = compile(schema, flag);
    node.v = fn as unknown as OpNode["v"];
    return fn;
  } catch (exn) {
    removeOpNode(schema, node);
    throw exn;
  }
};
