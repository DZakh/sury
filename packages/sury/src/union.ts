import {
  baseSchema,
  type Builder,
  type Check,
  type Encoder,
  flagUnsafeHas,
  getOrRethrow,
  immutableEmptyArray,
  immutableEmptyObject,
  type Internal,
  isLiteral,
  neverTag,
  nullTag,
  numberTag,
  objectTag,
  panic,
  setHas,
  type SuryErrorRecord,
  type Tag,
  tagFlagArray,
  tagFlagFunction,
  tagFlagInstance,
  tagFlagNaN,
  tagFlagNever,
  tagFlagNull,
  tagFlagObject,
  tagFlagRef,
  tagFlags,
  tagFlagUndefined,
  tagFlagUnion,
  tagFlagUnknown,
  toExpression,
  U,
  undefinedTag,
  unionTag,
  unknown,
  updateOutput,
  type Val,
  valFlagAsync,
} from "./base";
import {
  _notVar,
  _var,
  B_embed,
  B_failWithArg,
  B_invalidOperation,
  B_isHoistable,
  B_makeInvalidInputDetails,
  B_markOutput,
  B_merge,
  B_pushCheck,
  B_refine,
  B_scope,
  B_throw,
  failInvalidType,
  type HoistCond,
} from "./builder";
import { getOutputSchema, nestedLoc, never_, parse, typeCheckCond } from "./parse";

// Every tag bit set — the acceptance mask of a case that narrows nothing, and of
// a source that can hold any runtime type. Folded from `tagFlags` rather than
// written as a literal: a 17th tag added without widening a hand-kept mask would
// silently turn "accepts anything" into "anything but the new tag", and
// fallback elision would drop reachable cases.
const unionAnyTag = /* @__PURE__ */ Object.values(tagFlags).reduce(
  (acc, tagFlag) => acc | tagFlag,
  0
);
// Tags with no `typeof`-style discriminant: they can't own a shared group
// narrow, so each such variant dispatches inside its own decoded body.
const unionOpaqueTags =
  tagFlagUnknown | tagFlagUnion | tagFlagRef | tagFlagFunction | tagFlagNever;

// ── Type identity ────────────────────────────────────────────────────────────

// The spec's "same type": tags match, including the class for instances and the
// format for formatted primitives. A schema with no tag of its own to compare
// (a union treated as a normal schema, a recursive ref) matches only itself.
const unionSameType = (a: Internal, b: Internal): boolean =>
  a === b ||
  (a.type === b.type &&
    !flagUnsafeHas(tagFlags[a.type]!, tagFlagRef | tagFlagUnion) &&
    a.class === b.class &&
    a.format === b.format);

// A nested union spreads into its parent only when it carries nothing of its
// own; otherwise it stays one opaque variant that matches by reference.
const unionIsTransparent = (schema: Internal): boolean =>
  schema.type === unionTag &&
  schema.to === U &&
  schema.parser === U &&
  schema.format === U &&
  schema.refiner === U &&
  schema.inputRefiner === U;

// Grouping key: variants sharing it can share one emitted type narrow.
// Grouping key. Instance variants key by class *identity*, not `class.name`:
// two distinct classes routinely share a name (any minified bundle), and a
// shared key would put the second class under the first one's `instanceof`
// narrow — where its case decodes to nothing and every instance of it gets
// rejected by a check it can never pass. `@` can't collide with a tag name.
const unionKey = (schema: Internal, classIds: Map<unknown, number>): string => {
  if (!flagUnsafeHas(tagFlags[schema.type]!, tagFlagInstance)) {
    return schema.type;
  }
  let id = classIds.get(schema.class);
  if (id === U) {
    id = classIds.size;
    classIds.set(schema.class, id);
  }
  return "@" + id;
};

// NaN has to be tested before number, and instance/array before object — their
// narrows overlap and the more specific one has to win.
const unionIsPriority = (tagFlag: number, seen: Record<string, boolean>): boolean =>
  (flagUnsafeHas(tagFlag, tagFlagArray | tagFlagInstance) && objectTag in seen) ||
  (flagUnsafeHas(tagFlag, tagFlagNaN) && numberTag in seen);

// Whether decoding a value already known to be of the schema type is a noop —
// no transformation anywhere in the schema tree. Recursive refs are
// conservatively treated as transforming.
const unionIsNoop = (schema: Internal): boolean => {
  const additionalItems = schema.additionalItems;
  return (
    schema.to === U &&
    schema.parser === U &&
    !flagUnsafeHas(tagFlags[schema.type]!, tagFlagRef) &&
    (schema.anyOf !== U ? schema.anyOf.every(unionIsNoop) : true) &&
    (schema.items !== U ? schema.items.every(unionIsNoop) : true) &&
    (schema.properties !== U
      ? Object.values(schema.properties).every(unionIsNoop)
      : true) &&
    (additionalItems !== U && typeof additionalItems !== "string"
      ? unionIsNoop(additionalItems)
      : true)
  );
};

// Whether a union input is already narrower than this union, variant for
// variant, so dispatching would only re-check what the input already guarantees.
const unionIsWider = (variants: Internal[], inputVariants: Internal[]): boolean =>
  inputVariants.every((inputSchema, idx) => {
    const schema = variants[idx];
    return (
      schema !== U &&
      !flagUnsafeHas(
        tagFlags[inputSchema.type]!,
        tagFlagArray | tagFlagInstance | tagFlagRef | tagFlagUnion | tagFlagObject
      ) &&
      inputSchema.type === schema.type &&
      inputSchema.const === schema.const &&
      inputSchema.to === U &&
      // A paired variant with its own `.to` still transforms the value, so
      // passing the input through would skip the conversion
      schema.to === U
    );
  });

// The rejecting form of a hoisted dispatch cond, for a case that runs inside a
// `try` — the throw is what hands the value to the next case.
const unionRejectCond = (out: HoistCond): string => {
  const hoists = out.h;
  let code = "";
  for (let i = 0; i < hoists.length; i++) {
    const h = hoists[i]!;
    code = code + `${h.c}||${B_failWithArg(h.v, failInvalidType(h.v), h.i)};`;
  }
  return code;
};

// ── Emission ─────────────────────────────────────────────────────────────────

// One emitted alternative. `c` selects it, `b` runs it, `q` is its dispatch cond
// in mergeable form. `th` — the body can throw; `ft` — a later alternative could
// still accept a value this one fails on; `df` — it accepts with no code and
// nothing later would do anything different with the same value, so its
// condition can be deferred into the chain's final acceptance test.
type UnionCase = {
  c: string;
  b: string;
  q: HoistCond;
  th: boolean;
  ft: boolean;
  df: boolean;
  // The body's last statement has no `;` of its own, so it needs one when it
  // ends up unbraced at the end of the whole dispatch.
  n: boolean;
};

type UnionCtx = {
  // Error-var counter. Per decoder, so a nested union restarts at `e0` — its
  // `catch` sits inside the outer `try`, never around the outer chain's own
  // reference to `e0`, so the reused name can't shadow a live binding.
  n: number;
  // Set by `unionEmitChain` when its code ends with an unbraced, unterminated
  // statement. Read right after the call, so nesting can't confuse it.
  b: boolean;
  // The aggregated union error, given the caught per-case error vars.
  fail: (caught: string) => string;
  // `e[N]` for `getOrRethrow`, embedded on first use and shared by every case:
  // only a Sury validation error means "this variant didn't match".
  rethrow: () => string;
};

// Whether generated code can raise. Every Sury throw goes through an embedded
// helper (`e[N](…)`) or a bare `throw`, so their absence proves the body only
// computes — no `try` is needed to fall back from it.
const unionCanThrow = (code: string): boolean =>
  code.indexOf("e[") !== -1 || code.indexOf("throw") !== -1;

// Emits a linear fallback chain: every alternative that fails hands the value to
// the next one, and the last failure raises the aggregated union error. An
// alternative whose failure is provably terminal (`ft === false`) skips the
// try/catch and throws its own precise error instead.
const unionEmitChain = (cases: UnionCase[], ctx: UnionCtx): string => {
  let head = "";
  let tail = "";
  let caught = "";
  // An `if(…){…}` is open at the current nesting level, so the next alternative
  // continues it with `else`.
  let chained = false;
  // Alternatives that accept with no code of their own, deferred into one
  // disjunction tested last. Only safe when nothing later can accept them.
  let noop = "";
  let exhaustive = false;
  ctx.b = false;

  for (let idx = 0; idx < cases.length; idx++) {
    const c = cases[idx]!;

    if (c.b === "" && c.c === "") {
      // Accepts everything with no code — the chain can't fail from here on,
      // and any deferred alternative is subsumed.
      noop = "";
      exhaustive = true;
      break;
    }

    if (c.df) {
      noop = noop ? `${noop}||${c.c}` : c.c;
      continue;
    }

    if (c.b === "") {
      // Accepts with no code of its own, but a later alternative could take the
      // same value, so it has to stay in its slot. Guarding the rest of the
      // chain beats emitting an empty block.
      head = head + `${chained ? "else " : ""}if(!(${c.c})){`;
      tail = "}" + tail;
      chained = false;
      continue;
    }

    // A terminal last alternative still needs its `try` when earlier ones were
    // caught: throwing straight past them would drop their reasons from the
    // aggregated union error.
    if (c.th && (c.ft || (idx === cases.length - 1 && caught !== ""))) {
      if (noop) {
        head = head + `${chained ? "else " : ""}if(!(${noop})){`;
        tail = "}" + tail;
        noop = "";
        chained = false;
      } else if (chained) {
        head = head + "else{";
        tail = "}" + tail;
        chained = false;
      }
      const errorVar = "e" + ctx.n++;
      // A foreign exception — a `TypeError` from a buggy user predicate, say —
      // is a bug, not a variant miss. Rethrowing it beats reporting "didn't
      // match" and silently succeeding through a later catch-all.
      head =
        head +
        `try{${unionRejectCond(c.q)}${c.b}}catch(${errorVar}){${ctx.rethrow()}(${errorVar});`;
      tail = "}" + tail;
      caught = `${caught},${errorVar}`;
    } else if (c.c === "") {
      // Unconditional body: it either succeeds or throws, so nothing after it
      // can run.
      if (noop) {
        head = head + `${chained ? "else " : ""}if(!(${noop})){${c.b}}`;
        noop = "";
      } else if (chained) {
        head = head + `else{${c.b}}`;
      } else {
        head = head + c.b;
        ctx.b = c.n && tail === "";
      }
      exhaustive = true;
      break;
    } else {
      head = head + `${chained ? "else if" : "if"}(${c.c}){${c.b}}`;
      chained = true;
    }
  }

  if (!exhaustive) {
    const failCode = ctx.fail(caught);
    head =
      head +
      (noop
        ? `${chained ? "else " : ""}if(!(${noop})){${failCode}}`
        : chained
          ? `else{${failCode}}`
          : tail === ""
            ? // The bare fail call might be followed by more code, eg `return`
              failCode + ";"
            : failCode);
  }

  return head + tail;
};

// ── Group narrows ────────────────────────────────────────────────────────────

// A minimal schema standing in as the variant's runtime type, shared by every
// variant in the group. Built without a per-type factory reference so unused
// type decoders still tree-shake out of a union-using bundle — and
// `S.optional`/`S.nullable` are unions.
const unionNarrowSchema = (schema: Internal): Internal => {
  const tagFlag = tagFlags[schema.type]!;
  const narrow = baseSchema(schema.type, false);
  // Carries the variant's encoder so a pending `.to` reverse reaches it.
  narrow.encoder = schema.encoder;
  if (flagUnsafeHas(tagFlag, tagFlagInstance)) {
    narrow.class = schema.class;
  } else if (flagUnsafeHas(tagFlag, tagFlagObject)) {
    narrow.properties = immutableEmptyObject as Record<string, Internal>;
    narrow.additionalItems = unknown;
  } else if (flagUnsafeHas(tagFlag, tagFlagArray)) {
    narrow.additionalItems = unknown;
    narrow.items = immutableEmptyArray as Internal[];
  } else if (flagUnsafeHas(tagFlag, tagFlagNull | tagFlagUndefined | tagFlagNaN)) {
    // null/undefined/nan stay literals so the case body passes through.
    narrow.const = schema.const;
  }
  // Per-invocation, not hoisted: the narrow is re-decoded per variant — with the
  // union's `unknown` input (emit the discriminant), with a source that already
  // holds this tag (nothing to do), or with a value the source encoder coerced
  // into another representation (delegate to the variant's own decoder).
  narrow.decoder = (input: Val) => {
    if (flagUnsafeHas(tagFlags[input.s.type]!, tagFlagUnknown)) {
      return B_refine(input, input.e, [
        {
          c: (inputVar) => typeCheckCond(input, schema, inputVar),
          f: failInvalidType,
        },
      ]);
    }
    if (input.s.type === narrow.type && input.s.class === narrow.class) {
      // The source already holds this tag, so there is nothing to decode. A
      // structured source additionally swaps in the minimal narrow: several
      // variants share the `object` tag, and a union case validates the value
      // against its own shape rather than converting one shape into another.
      return flagUnsafeHas(tagFlag, tagFlagObject | tagFlagArray)
        ? B_refine(input, input.e)
        : input;
    }
    return schema.decoder(input);
  };
  return narrow;
};

// The tags a case's dispatch cond can be true for, read off the narrow it
// actually emitted — for a gap-filled variant that's the source's
// representation tag (a JSON string offered to `S.bigint`), not the variant's
// own. Only hoistable narrows count: a check the dispatch can't lift stays in the
// body and throws, which constrains nothing about which values reach the case.
// The earliest narrow in the chain is the one tested against the raw input, so
// keep walking to the root.
const unionAcceptMask = (attempt: Val, fallback: number): number => {
  let v: Val | undefined = attempt;
  let narrowed: Internal | undefined = U;
  while (v !== U) {
    const checks = v.vc;
    if (checks !== U && B_isHoistable(v)) {
      for (let i = 0; i < checks.length; i++) {
        if (checks[i]!.f === failInvalidType) {
          narrowed = v.s;
          break;
        }
      }
    }
    v = v.prev;
  }
  if (narrowed === U) {
    return fallback;
  }
  const tagFlag = tagFlags[narrowed.type]!;
  return flagUnsafeHas(tagFlag, unionOpaqueTags) ? fallback : unionWiden(tagFlag);
};

// Tag bits don't partition runtime values: every instance passes the object
// narrow, so two such cases are only provably disjoint after widening each to
// everything its narrow could also let through. Arrays and NaN need no widening —
// the object and number narrows exclude them explicitly.
const unionObjectish = tagFlagObject | tagFlagInstance;
// Tags whose "same type" says nothing about the value's shape.
const unionStructured =
  tagFlagObject | tagFlagArray | tagFlagInstance | tagFlagRef | tagFlagUnion;
const unionWiden = (tagFlag: number): number =>
  flagUnsafeHas(tagFlag, unionObjectish) ? unionObjectish : tagFlag;

// The tags a nested union accepts, when it compiles to nothing but a type test
// over its members. A union tag has no `typeof` discriminant of its own, so the
// dispatch would otherwise have to assume the variant might accept anything and
// learn otherwise by catching its failure — which costs a thrown exception on
// input the variant was never going to take. Returns 0 when that isn't provable,
// leaving the conservative source mask in place. Anything that would give a
// member code of its own (a transform, a refinement, a nested shape) bails: the
// mask has to describe what the emitted dispatch condition accepts, and only a
// pure type test keeps the two in step.
const unionNestedMask = (schema: Internal): number => {
  const members = schema.anyOf;
  if (members === U || schema.to !== U || schema.parser !== U) {
    return 0;
  }
  let mask = 0;
  for (let idx = 0; idx < members.length; idx++) {
    const member = members[idx]!;
    const tagFlag = tagFlags[member.type]!;
    if (flagUnsafeHas(tagFlag, tagFlagNever)) {
      continue;
    }
    if (
      flagUnsafeHas(tagFlag, unionOpaqueTags | unionStructured) ||
      !unionIsNoop(member) ||
      member.refiner !== U ||
      member.inputRefiner !== U ||
      member.noValidation !== U
    ) {
      return 0;
    }
    mask = mask | unionWiden(tagFlag);
  }
  return mask;
};

// ── Rejections ───────────────────────────────────────────────────────────────

// A source matching some but not all variants by type is ambiguous — Sury can't
// tell a pass-through from a decoding attempt. Reject where the operation is
// written, naming the spellings that resolve it.
const unionCheckPartial = (
  input: Val,
  source: Internal,
  target: Internal,
  // The union's variants, and the side of each one the other side is compared
  // against: its input under rule 2 (the union is the target), its output under
  // rule 3 (the union is the source).
  variants: Internal[],
  matchSide: (variant: Internal) => Internal,
  // What the unmatched side is called in the message — the union sits opposite
  // it, so this is also which of source/target `variants` belongs to.
  side: "source" | "target"
): void => {
  const other = side === "target" ? target : source;
  let total = 0;
  let matches = 0;
  let matched: Internal | undefined = U;
  for (let idx = 0; idx < variants.length; idx++) {
    const variant = variants[idx]!;
    if (!unionCounts(variant)) {
      continue;
    }
    total = total + 1;
    if (unionSameType(other, matchSide(variant))) {
      matches = matches + 1;
      if (matched === U) {
        matched = variant;
      }
    }
  }
  if (matches > 0 && matches < total) {
    unionInvalid(
      input,
      source,
      target,
      `${toExpression(matched!)} has the same type as the ${side} and the others don't`
    );
  }
};

const unionUncovered = (
  input: Val,
  source: Internal,
  target: Internal,
  variant: Internal
): never =>
  unionInvalid(
    input,
    source,
    target,
    `${toExpression(variant)} has no same-type variant on the other side`
  );

const unionInvalid = (input: Val, from: Internal, to: Internal, why: string): never =>
  B_invalidOperation(
    input,
    `Invalid operation: can't convert ${toExpression(from)} to ${toExpression(
      to
    )} — ${why}. Use S.to to say what you mean, or S.never to mark a variant unreachable`
  );

// ── Decoder ──────────────────────────────────────────────────────────────────

type UnionGroup = {
  // The scoped input the group's cases branch from.
  ni: Val;
  // The narrowed val, parsed once and shared.
  nv: Val;
  // Acceptance mask of the shared narrow.
  m: number;
  // Variants dispatched under this narrow, in definition order.
  items: Internal[];
  // Definition index of the last variant added, for merge legality.
  last: number;
};

export const unionDecoder: Builder = (input: Val) => {
  const self = input.e;
  // The union's own `.to` chain, applied per case during decoding. None when a
  // custom parser owns the conversion, or when the target is the `noValidation`
  // sentinel `S.assert` appends: fusing that into every case replaces each
  // member's own check with the sentinel's, which silently breaks dispatch.
  // Left alone, it converts the union's assembled output once instead.
  const toPerCase =
    self.parser === U && self.to !== U && self.to.noValidation !== true ? self.to : U;
  let variants = self.anyOf!;

  if (
    // Already validated against this exact schema.
    (input.io! && input.e === input.s) ||
    // The input val is already of the union type (trusted self-decode). Only
    // allowed when no variant transforms the value.
    (input.s === self && toPerCase === U && variants.every(unionIsNoop)) ||
    (input.s.type === unionTag &&
      toPerCase === U &&
      unionIsWider(variants, input.s.anyOf!))
  ) {
    return input;
  }

  const initialTagFlag = tagFlags[input.s.type]!;
  if (
    flagUnsafeHas(initialTagFlag, tagFlagUnion) ||
    (input.s.encoder === U && flagUnsafeHas(initialTagFlag, tagFlagRef))
  ) {
    input.s = unknown;
  }

  const source = input.s;
  // A source that can hold anything constrains nothing, so it can't prove two
  // cases disjoint.
  const sourceMask = flagUnsafeHas(
    tagFlags[source.type]!,
    tagFlagUnknown | tagFlagUnion | tagFlagRef
  )
    ? unionAnyTag
    : tagFlags[source.type]!;

  // Rule 2 — matching some but not all target variants is ambiguous: pass the
  // value through to the matching one, or attempt decoding in definition order?
  // Two sources are never ambiguous: `unknown`, which may already be any of the
  // variant types (so nothing is coerced either way), and a const the target
  // spells out exactly — that variant takes the value as it is, and no other
  // variant can produce it.
  if (
    !flagUnsafeHas(tagFlags[source.type]!, tagFlagUnknown) &&
    !(
      isLiteral(source) &&
      variants.some((v) => isLiteral(v) && v.const === source.const)
    )
  ) {
    unionCheckPartial(input, source, self, variants, (v) => v, "source");
  }

  // A union carrying its own `.to` converts per variant, so rules 3 and 4 have
  // to resolve the target before it's fused into the cases — appending the whole
  // target union instead would re-enter as an ambiguous rule-2 conversion. The
  // union's own refiners ride along on each variant for the same reason: there
  // is no single pre-conversion output val left to attach them to.
  if (toPerCase !== U) {
    const perCase = unionTargetOwns(toPerCase)
      ? variants.map((v) => (getOutputSchema(v).type === neverTag ? U : toPerCase))
      : unionResolve(input, self, variants, toPerCase);
    const attach =
      self.refiner !== U || self.inputRefiner !== U ? unionRefinerAttacher(self) : U;
    variants = variants.map((variant, idx) => {
      const to = perCase[idx];
      return to === U && attach === U
        ? variant
        : updateOutput<Internal>(variant, (mut) => {
            if (attach !== U) {
              attach(mut);
            }
            if (to !== U) {
              mut.to = to;
            }
          });
    });
  }

  // A const source the target spells out exactly reaches only those variants:
  // the value already *is* that literal, so every other variant is dead code —
  // and compiling one would ask for a decoder that has no reason to exist.
  if (isLiteral(source)) {
    const matching: Internal[] = [];
    for (let idx = 0; idx < variants.length; idx++) {
      const schema = variants[idx]!;
      if (isLiteral(schema) && schema.const === source.const) {
        matching.push(schema);
      }
    }
    if (matching.length > 0) {
      variants = matching;
    }
  }

  const skipUndefined = "fromDefault" in self;

  // ── Pass 0: grouping keys and how many variants share each ─────────────────
  // A tag carried by one variant needs no shared narrow — that variant's own
  // decode emits exactly the narrow a shared one would. Skipping it halves the
  // `parse` calls for the `X | undefined` shape every `S.optional` compiles to,
  // and it's only sound with an acceptance mask that doesn't need the narrow
  // either: an `unknown` source hands the value to each decoder untouched, so a
  // variant's own tag *is* what its cond accepts. `object` is the one tag where
  // that doesn't hold — a `strip` object rebuilds its value, so its own decoder
  // skips the `!Array.isArray` half of the narrow (composites.ts) and would let
  // an array into the case while `maskAt` claims only object/instance reach it.
  const soleTag = flagUnsafeHas(tagFlags[source.type]!, tagFlagUnknown);
  const keyAt: string[] = [];
  const tagCount: Record<string, number> = Object.create(null);
  const classIds = new Map<unknown, number>();
  for (let idx = 0; idx < variants.length; idx++) {
    const key = unionKey(variants[idx]!, classIds);
    keyAt.push(key);
    tagCount[key] = (tagCount[key] || 0) + 1;
  }

  // ── Pass 1 (forward): group variants under shared narrows ──────────────────
  const groups: UnionGroup[] = [];
  const openByKey: Record<string, UnionGroup> = Object.create(null);
  const seenKeys: Record<string, boolean> = Object.create(null);
  // Acceptance mask per definition index, for merge-legality lookups. A skipped
  // variant keeps 0, which makes it invisible to them.
  const maskAt: number[] = [];

  for (let idx = 0; idx < variants.length; idx++) {
    const variant = variants[idx]!;
    const tagFlag = tagFlags[variant.type]!;
    maskAt.push(0);
    // `S.never` marks a deliberately unreachable path: no branch, no coverage,
    // no rejection.
    if (
      flagUnsafeHas(tagFlag, tagFlagNever) ||
      (skipUndefined && flagUnsafeHas(tagFlag, tagFlagUndefined))
    ) {
      continue;
    }

    const key = keyAt[idx]!;

    const open = openByKey[key];
    if (open !== U) {
      // Merging past an intervening case is only legal while that case can't
      // accept anything this one would — else the earlier group would steal it.
      let legal = true;
      for (let j = open.last + 1; j < idx; j++) {
        if (keyAt[j] !== key && (maskAt[j]! & open.m) !== 0) {
          legal = false;
          break;
        }
      }
      if (legal) {
        maskAt[idx] = open.m;
        open.last = idx;
        const items = open.items;
        if (flagUnsafeHas(tagFlag, tagFlagObject) && nestedLoc in variant.properties!) {
          // https://github.com/DZakh/sury/issues/150 — a nested option next to
          // an empty object schema: the None case is checked second, so this
          // one has to go in front of it.
          items.splice(items.length - 1, 0, variant);
        } else if (
          !flagUnsafeHas(tagFlag, tagFlagUndefined | tagFlagNull | tagFlagNaN)
        ) {
          // Only one null/undefined/nan case can ever match — dedupe.
          items.push(variant);
        }
        continue;
      }
    }

    const narrowInput = B_scope(input);
    narrowInput.io = false;
    let narrowVal: Val;
    let mask: number;
    if (flagUnsafeHas(tagFlag, unionOpaqueTags)) {
      // unknown / union / ref / json / function have no `typeof` discriminant:
      // the variant's own decoder owns the whole branch.
      narrowInput.e = unknown;
      narrowVal = narrowInput;
      mask = sourceMask;
      if (flagUnsafeHas(tagFlag, tagFlagUnion)) {
        const nested = unionNestedMask(variant);
        if (nested !== 0) {
          mask = sourceMask & nested;
        }
      }
    } else if (
      soleTag &&
      tagCount[key] === 1 &&
      variant.to === U &&
      !flagUnsafeHas(tagFlag, tagFlagObject)
    ) {
      // A conversion still needs the narrow: it stands in as the case's clean
      // input schema, where decoding straight from the source would leave the
      // case's output val describing a `.to` that has already run (#284).
      narrowVal = narrowInput;
      mask = unionWiden(tagFlag);
    } else {
      narrowInput.e = unionNarrowSchema(variant);
      // A creation error here rejects the whole operation: the source has no way
      // to reach this variant's type, and a union never salvages that by
      // dropping the variant.
      narrowVal = parse(narrowInput);
      mask = unionAcceptMask(narrowVal, sourceMask);
    }
    maskAt[idx] = mask;

    const group: UnionGroup = {
      ni: narrowInput,
      nv: narrowVal,
      m: mask,
      items: [variant],
      last: idx,
    };
    openByKey[key] = group;
    if (unionIsPriority(tagFlag, seenKeys)) {
      groups.unshift(group);
    } else {
      groups.push(group);
    }
    seenKeys[key] = true;
  }

  // ── Pass 2 (reverse): what any later group can still accept ────────────────
  const suffix: number[] = [];
  let acc = 0;
  for (let idx = groups.length - 1; idx >= 0; idx--) {
    suffix[idx] = acc;
    acc = acc | groups[idx]!.m;
  }

  // ── Pass 3 (forward): emit ─────────────────────────────────────────────────
  let output = B_refine(input);
  const outputAnyOf: Internal[] = [];
  const initialInline = input.i;

  const salvage = self.perVariant === true;
  // Embedded creation errors of dropped variants, reported as the union error's
  // per-case reasons.
  let salvaged = "";
  let rethrow = "";

  const ctx: UnionCtx = {
    n: 0,
    b: false,
    fail: (caught: string) =>
      `${B_embed(
        input,
        // Reads `arguments`, so this must stay a `function` expression.
        function () {
          const args = arguments;
          B_throw(
            B_makeInvalidInputDetails(
              self,
              unknown,
              input.path,
              args[0],
              true,
              args.length > 1 ? (Array.from(args).slice(1) as SuryErrorRecord[]) : U
            )
          );
        }
      )}(${input.v()}${salvaged}${caught})`,
    rethrow: () => rethrow || (rethrow = B_embed(input, getOrRethrow)),
  };

  const branches: UnionCase[] = [];
  const branchMasks: number[] = [];
  for (let idx = 0; idx < groups.length; idx++) {
    const group = groups[idx]!;
    const groupFallsThrough = (group.m & suffix[idx]!) !== 0;

    const cases: UnionCase[] = [];
    const items = group.items;
    for (let i = 0; i < items.length; i++) {
      const variant = items[i]!;

      // Copy the input again: every case decoder may mutate it.
      const caseInput = B_scope(group.nv);
      caseInput.u = true;
      caseInput.t = group.nv.t;
      caseInput.io = false;
      caseInput.e = variant;

      let caseOut: Val;
      if (salvage) {
        // Sury's own possibly-absent read: a variant with no decoder to the
        // target isn't a user error to reject the operation for, it just can't
        // match. Drop it and report its reason under the union's own error.
        try {
          caseOut = parse(caseInput);
        } catch (exn) {
          salvaged = `${salvaged},${B_embed(input, getOrRethrow(exn))}`;
          continue;
        }
      } else {
        caseOut = parse(caseInput);
      }
      outputAnyOf.push(caseOut.s);

      const cond: HoistCond = { c: "", h: [] };
      let body = B_merge(caseOut, cond);
      let needsTerminator = false;

      if (caseOut.t!) {
        output.t = true;
        if (flagUnsafeHas(caseOut.f, valFlagAsync)) {
          output.f |= valFlagAsync;
        }
        const itemVar = group.ni.v();
        if (caseOut.i !== itemVar) {
          // Allocate through the shared var so the case doesn't mutate the
          // input object field it was read from.
          body = body + `${itemVar}=${caseOut.i}`;
          needsTerminator = true;
        }
      }

      cases.push({
        c: cond.c,
        b: body,
        q: cond,
        th: unionCanThrow(body),
        ft: false,
        df: false,
        n: needsTerminator,
      });

      if (cond.c === "" && body === "") {
        // Accepts everything under this narrow with no code — nothing after it is
        // reachable, so don't compile (and embed) the rest.
        break;
      }
    }

    // The shared narrow is already factored out, so what's left of a case's cond
    // is its discriminant. Distinct discriminants can't both match, making a
    // failure there terminal; identical (or absent) ones overlap and fall
    // through. The group's last case falls through to the next group instead.
    for (let i = 0; i < cases.length; i++) {
      const c = cases[i]!;
      let fallsThrough = false;
      let deferrable = c.b === "";
      for (let j = i + 1; j < cases.length; j++) {
        const other = cases[j]!;
        const overlaps = other.c === "" || other.c === c.c;
        if (overlaps) {
          fallsThrough = true;
          // Only a later case that would *do* something changes the outcome of
          // deferring this one; another pass-through leaves the value alone
          // either way.
          if (other.b !== "") {
            deferrable = false;
          }
        }
      }
      // The last case's failure is routed by the group's own branch — either its
      // try/catch or, when nothing later can accept, a direct throw — so it
      // never wraps itself.
      c.ft = i === cases.length - 1 ? false : fallsThrough;
      c.df = deferrable;
    }

    if (cases.length === 0) {
      continue;
    }

    const groupCond: HoistCond = { c: "", h: [] };
    let groupBody: string;
    let bare = false;
    if (cases.every((c) => c.b === "")) {
      // Every case accepts with no code of its own, so the group is pure
      // validation: fold the discriminants into its own narrow and let them
      // emit as one condition.
      if (!cases.some((c) => c.c === "")) {
        let fused = cases[0]!.c;
        for (let i = 1; i < cases.length; i++) {
          fused = `${fused}||${cases[i]!.c}`;
        }
        if (cases.length > 1) {
          // A disjunction needs its own parens inside the narrow's `&&` chain.
          fused = `(${fused})`;
        }
        if (group.nv.prev === U) {
          // The narrow emitted nothing, so the discriminants read the raw input
          // var and join the group's own condition directly. Going through a
          // check would need a `prev` to read that var from.
          groupCond.c = fused;
        } else {
          // Routed through a check so `B_isHoistable` decides: a narrow that
          // transformed the value keeps the discriminant in its body, below the
          // `let` the discriminant reads.
          B_pushCheck(group.nv, {
            c: (_inputVar) => fused,
            f: failInvalidType,
          });
        }
      }
      groupBody = B_merge(group.nv, groupCond);
    } else {
      const narrowCode = B_merge(group.nv, groupCond);
      const only = cases.length === 1 ? cases[0]! : U;
      if (only !== U && narrowCode === "" && only.c !== "") {
        // One case under a narrow that emitted nothing, so the two conditions are
        // really one: keep it on the branch, where the chain can continue it with
        // `else` instead of nesting the case inside a block of its own.
        groupCond.c = groupCond.c ? `${groupCond.c}&&${only.c}` : only.c;
        groupCond.h = groupCond.h.concat(only.q.h);
        groupBody = only.b;
        bare = only.n;
      } else {
        const inner = unionEmitChain(cases, ctx);
        bare = ctx.b;
        groupBody = narrowCode + inner;
      }
    }

    branches.push({
      c: groupCond.c,
      b: groupBody,
      q: groupCond,
      th: unionCanThrow(groupBody),
      ft: groupFallsThrough,
      df: false,
      n: bare,
    });
    branchMasks.push(group.m);
  }

  // Deferring a pass-through group past the rest of the chain is only safe while
  // no later group with a body of its own could take the same value.
  let bodyMask = 0;
  for (let idx = branches.length - 1; idx >= 0; idx--) {
    const branch = branches[idx]!;
    const mask = branchMasks[idx]!;
    branch.df = branch.b === "" && (mask & bodyMask) === 0;
    if (branch.b !== "") {
      bodyMask = bodyMask | mask;
    }
  }

  // A union that only validates — every branch a pass-through carrying a
  // condition of its own — has nothing to emit but its own narrow. Kept as a
  // check instead of an `if(!cond){fail}` statement it stays hoistable, so an
  // enclosing union lifts it into the dispatch and reaches the next variant with
  // an `else` rather than through a thrown exception.
  let pureNarrow = false;
  if (branches.length > 0 && branches.every((b) => b.b === "" && b.c !== "")) {
    let fused = branches[0]!.c;
    for (let idx = 1; idx < branches.length; idx++) {
      fused = `${fused}||${branches[idx]!.c}`;
    }
    if (branches.length > 1) {
      // A disjunction needs its own parens inside the `&&` chain a check emits.
      fused = `(${fused})`;
    }
    // Two vals: the inner one carries the narrow with `self` pinned as its
    // expected schema, the outer absorbs the tail's mutations below — which
    // overwrite `e` with the `.to` target and rebuild `s` from the variants'
    // outputs, either of which would otherwise rename the check's error to a
    // schema the value was never matched against.
    pureNarrow = true;
    output = B_refine(
      B_refine(output, output.s, [{ c: (_inputVar) => fused, f: failInvalidType }], self)
    );
  } else {
    const dispatch = unionEmitChain(branches, ctx);
    // The whole dispatch can collapse to one unbraced statement (a case that
    // always applies), and the caller appends `return` right after it.
    output.cp = output.cp + dispatch + (ctx.b ? ";" : "");
  }

  // In case input.var was called, but output.var wasn't
  if (input.i !== output.i) {
    output.i = input.i;
  }

  let o: Val;
  if (flagUnsafeHas(output.f, valFlagAsync)) {
    output.i = `Promise.resolve(${output.i})`;
    output.v = _notVar;
    o = output;
  } else if (
    output.v === _var &&
    input.cp === "" &&
    output.cp === "" &&
    // A pure-validation union emits no code but carries its narrow as a check,
    // and dropping `output` for `input` would drop the whole validation.
    !pureNarrow &&
    initialInline === "i"
  ) {
    // Nothing was emitted: hand back the untouched input so callers keep
    // treating the value as unchanged.
    input.hd = "";
    input.v = _notVar;
    input.i = initialInline;
    o = input;
  } else {
    o = output;
  }

  // Built from the variants' own output schemas: the next `.to` segment decodes
  // from this, so it has to describe what the value actually holds (#284).
  o.s = outputAnyOf.length ? unionFactory(outputAnyOf) : never_();
  if (toPerCase !== U) {
    o.io = true;
    o.e = getOutputSchema(toPerCase);
    return o;
  }
  o.e = self;
  return B_markOutput(o, input);
};

// Calls each source refiner at most once so its predicate is embedded once and
// every case references the same `e[N]` — `B_embed` is append-only, so a
// per-case call would duplicate it.
const unionRefinerAttacher = (self: Internal): ((mut: Internal) => void) => {
  const cache: Record<string, Check[] | undefined> = Object.create(null);
  const attach = (
    current: ((input: Val) => Check[]) | undefined,
    source: ((input: Val) => Check[]) | undefined,
    key: string
  ): ((input: Val) => Check[]) | undefined => {
    if (source === U) {
      return current;
    }
    const getCached = (input: Val): Check[] => {
      const cached = cache[key];
      if (cached !== U) {
        return cached;
      }
      const checks = source(input);
      cache[key] = checks;
      return checks;
    };
    if (current === U) {
      return getCached;
    }
    const existing = current;
    return (input: Val) => {
      const arr = existing(input);
      const next = getCached(input);
      for (let i = 0; i < next.length; i++) {
        arr.push(next[i]!);
      }
      return arr;
    };
  };
  return (mut: Internal) => {
    const r = attach(mut.refiner, self.refiner, "r");
    if (r !== U) {
      mut.refiner = r;
    }
    const ir = attach(mut.inputRefiner, self.inputRefiner, "i");
    if (ir !== U) {
      mut.inputRefiner = ir;
    }
  };
};

// ── Encoder ──────────────────────────────────────────────────────────────────

// Re-drives the source union with a per-variant target appended, so its decoder
// dispatches over the variants and each one converts independently.
export const unionRewrite = (
  input: Val,
  map: (variant: Internal, idx: number) => Internal
): Val => {
  const variants = input.s.anyOf!;
  const anyOf: Internal[] = [];
  const has: Partial<Record<Tag, boolean>> = {};
  for (let idx = 0; idx < variants.length; idx++) {
    const rewritten = map(variants[idx]!, idx);
    anyOf.push(rewritten);
    setHas(has, rewritten.type);
  }
  const mut = baseSchema(unionTag, false);
  mut.anyOf = anyOf;
  mut.has = has;
  mut.decoder = unionDecoder;
  mut.encoder = unionEncoder;
  mut.perVariant = input.s.perVariant;
  return B_refine(input, unknown, U, mut);
};

// Appends `.to(target)` to every source variant. A variant already ending in
// `never` stays as it is: it's an explicit rejection, not a path to convert.
export const unionRewriteTo = (input: Val, target: Internal): Val =>
  unionRewrite(input, (variant) =>
    getOutputSchema(variant).type === neverTag
      ? variant
      : updateOutput<Internal>(variant, (mut) => {
          mut.to = target;
        })
  );

// Whether the union should hand itself to the target untouched — recursive
// schemas and `S.json` decode a union source per variant on their own, and a
// `noValidation` target (`S.assert`'s result sentinel) discards the value, so
// converting each member into it would replace every member's check with the
// sentinel's.
const unionTargetOwns = (target: Internal): boolean =>
  target.noValidation === true ||
  flagUnsafeHas(tagFlags[getOutputSchema(target).type]!, tagFlagRef) ||
  (target.type === unionTag &&
    target.anyOf!.some((v) => flagUnsafeHas(tagFlags[v.type]!, tagFlagRef)));

// Applied by the parse loop when a union-typed val meets a different expected
// schema — rules 3 and 4.
export const unionEncoder: Encoder = (input: Val, target: Internal) => {
  if (unionTargetOwns(target)) {
    return input;
  }
  const variants = input.s.anyOf!;
  if (target.perVariant && target.anyOf!.length === variants.length) {
    // An already-resolved per-variant mapping (the JSON encoder builds one for an
    // object field): each target variant *is* its source variant plus whatever
    // the caller appended, so it replaces the variant instead of chaining onto
    // it — chaining would run the variant's own pipeline twice.
    const targets = target.anyOf!;
    return targets.every((tv, idx) => tv === variants[idx])
      ? input
      : unionRewrite(input, (_variant, idx) => targets[idx]!);
  }
  const resolved = unionResolve(input, input.s, variants, target);
  if (resolved.every((to) => to === U)) {
    // Nothing to convert — hand the union straight to the target's own decoder,
    // which can then skip re-checking what this union already guarantees.
    // Rewriting would drop the input's type down to `unknown` and force a full
    // re-validation (a second item loop over an array, #284).
    return input;
  }
  return unionRewrite(input, (variant, idx) => {
    const to = resolved[idx];
    return to === U
      ? variant
      : updateOutput<Internal>(variant, (mut) => {
          mut.to = to;
        });
  });
};

// `S.never` on either side of a variant's pipeline marks the path unreachable:
// nothing flows in, or nothing comes out. Either way type matching ignores it.
const unionCounts = (variant: Internal): boolean =>
  variant.type !== neverTag && getOutputSchema(variant).type !== neverTag;

const unionNullish = tagFlagNull | tagFlagUndefined;

const unionOpposite = (schema: Internal): Tag | undefined =>
  schema.type === undefinedTag ? nullTag : schema.type === nullTag ? undefinedTag : U;

// Per source variant, the target to append — or `U` for a pass-through, where
// the type check the dispatch already emits is the whole conversion. This is
// where rules 3 and 4 are decided, shared by the encoder and by a union that
// carries its own `.to`.
const unionResolve = (
  input: Val,
  source: Internal,
  variants: Internal[],
  target: Internal
): (Internal | undefined)[] => {
  if (source.perVariant) {
    return variants.map(() => target);
  }
  if (unionIsTransparent(target)) {
    return unionResolveToUnion(input, source, variants, target);
  }
  // Rule 3 — every source variant gets its own built-in decoder to the target.
  // Two targets are never ambiguous: `unknown`, the top type, which decodes
  // nothing; and a `noValidation` target (S.assert's result sentinel), which
  // discards the value entirely.
  if (!flagUnsafeHas(tagFlags[target.type]!, tagFlagUnknown) && !target.noValidation) {
    unionCheckPartial(input, source, target, variants, getOutputSchema, "target");
  }
  return variants.map((variant) =>
    getOutputSchema(variant).type === neverTag ? U : target
  );
};

// Rule 4 — no coercion: values pass through to the same-type target variant. The
// two unions must cover each other, with `null`/`undefined` allowed to bridge to
// the opposite nullish variant on the other side.
const unionResolveToUnion = (
  input: Val,
  source: Internal,
  variants: Internal[],
  target: Internal
): (Internal | undefined)[] => {
  const targets = target.anyOf!;
  const matches: (Internal | undefined)[] = [];
  const covered: boolean[] = [];

  for (let s = 0; s < variants.length; s++) {
    matches.push(U);
    const sourceOut = getOutputSchema(variants[s]!);
    if (!unionCounts(variants[s]!)) {
      continue;
    }
    const sameTyped: Internal[] = [];
    for (let t = 0; t < targets.length; t++) {
      const targetVariant = targets[t]!;
      if (unionCounts(targetVariant) && unionSameType(sourceOut, targetVariant)) {
        covered[t] = true;
        sameTyped.push(targetVariant);
      }
    }
    if (sameTyped.length === 1) {
      matches[s] = sameTyped[0]!;
    } else if (sameTyped.length > 1) {
      // "Same type" is tag-level, so several target variants can share it. For a
      // structured variant that's too coarse to pick by definition order — every
      // object shape is `object` — so a candidate that *is* this variant's own
      // output takes it as the pass-through rule 4 describes. Otherwise hand the
      // value to all the candidates and let their own dispatch (and fallback)
      // sort it out.
      const own = flagUnsafeHas(tagFlags[sourceOut.type]!, unionStructured)
        ? sameTyped.indexOf(sourceOut)
        : -1;
      matches[s] = own >= 0 ? sourceOut : unionFactory(sameTyped);
    }
  }

  // Nullish bridge: an unmatched null/undefined may take the opposite nullish
  // variant, even one that already has a same-type source. The same-type match
  // wins at runtime, so the bridge only ever fills a hole.
  let sourceNullish = 0;
  for (let s = 0; s < variants.length; s++) {
    const sourceOut = getOutputSchema(variants[s]!);
    sourceNullish = sourceNullish | (tagFlags[sourceOut.type]! & unionNullish);
    if (matches[s] !== U) {
      continue;
    }
    const opposite = unionOpposite(sourceOut);
    for (let t = 0; opposite !== U && t < targets.length; t++) {
      if (targets[t]!.type === opposite) {
        matches[s] = targets[t]!;
        break;
      }
    }
  }

  for (let s = 0; s < variants.length; s++) {
    if (unionCounts(variants[s]!) && matches[s] === U) {
      unionUncovered(input, source, target, getOutputSchema(variants[s]!));
    }
  }
  for (let t = 0; t < targets.length; t++) {
    const targetVariant = targets[t]!;
    // A nullish target is covered by the opposite nullish source through the
    // bridge, even without a same-type match of its own.
    if (
      unionCounts(targetVariant) &&
      !covered[t] &&
      (unionOpposite(targetVariant) === U ||
        !flagUnsafeHas(sourceNullish, tagFlags[unionOpposite(targetVariant)!]!))
    ) {
      unionUncovered(input, source, target, targetVariant);
    }
  }

  return matches.map((matched, idx) =>
    matched !== U && unionAddsNothing(matched, getOutputSchema(variants[idx]!)) ? U : matched
  );
};

// Whether the matched target adds nothing to the source variant's output: same
// type, no transformation, refinement or nested structure left to check, so the
// type check the dispatch already emits is the whole conversion. Appending it
// anyway would re-decode the value into a schema it already satisfies — and
// leave the case's output val describing a `.to` that has already run.
const unionAddsNothing = (matched: Internal, sourceOut: Internal): boolean =>
  matched === sourceOut ||
  (unionIsNoop(matched) &&
    matched.refiner === U &&
    matched.inputRefiner === U &&
    matched.noValidation === U &&
    // A target const narrows the source; only a target that constrains nothing
    // (or exactly the same value) is a pass-through.
    (matched.const === U || matched.const === sourceOut.const) &&
    !flagUnsafeHas(tagFlags[matched.type]!, unionStructured) &&
    unionSameType(matched, sourceOut));

// ── Factory ──────────────────────────────────────────────────────────────────

export const unionFactory = (schemas: Internal[]): Internal => {
  if (schemas.length === 0) {
    return panic("S.union requires at least one item");
  } else if (schemas.length === 1) {
    return schemas[0]!;
  }

  const has: Partial<Record<Tag, boolean>> = {};
  const anyOf: Internal[] = [];
  const add = (schema: Internal): void => {
    for (let idx = 0; idx < anyOf.length; idx++) {
      if (anyOf[idx] === schema) {
        return;
      }
    }
    anyOf.push(schema);
  };

  for (let idx = 0; idx < schemas.length; idx++) {
    const schema = schemas[idx]!;
    if (unionIsTransparent(schema)) {
      const nested = schema.anyOf!;
      for (let i = 0; i < nested.length; i++) {
        add(nested[i]!);
      }
      Object.assign(has, schema.has!);
    } else {
      add(schema);
      setHas(has, schema.type);
    }
  }

  const mut = baseSchema(unionTag, false);
  mut.anyOf = anyOf;
  mut.decoder = unionDecoder;
  mut.encoder = unionEncoder;
  mut.has = has;
  return mut;
};
