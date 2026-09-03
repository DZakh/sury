// `S.union` — the factory, the decoder that dispatches a value to one member,
// and the encoder that converts a union into another schema. `CODEC_SPEC.md`
// states which conversions are legal.
//
// The decoder is four stages, each taking the previous one's output:
// source flags (what the source can be) → `unionAnalyze` (one record of
// integers per variant) → `unionPlan` (an ordered list of groups, each sharing
// one type narrow) → `unionEmit` (the dispatch chain). Ordering is by
// specificity rather than source order, which is what puts an instance ahead of
// an earlier generic `object`, and an exact `NaN` ahead of `number`.
//
// Which member a value reaches is invisible in a golden until someone writes the
// spec for exactly that permutation. `pnpm --filter=sury fuzz:union` compares
// this compiler to a sequential try of each variant's own parser/encoder.

import {
  anyOfTag,
  baseSchema,
  type Builder,
  type Check,
  type Encoder,
  getOrRethrow,
  immutableEmptyArray,
  immutableEmptyObject,
  inlinedValueFromString,
  inputExpression,
  type Internal,
  isLiteral,
  nanTag,
  neverTag,
  stringTag,
  nullTag,
  numberTag,
  objectTag,
  panic,
  type Path,
  setHas,
  setContent,
  type SuryErrorRecord,
  type Tag,
  tagFlags,
  U,
  undefinedTag,
  unknown,
  updateOutput,
  type Val
} from "./base";
import {
  _notVar,
  _var,
  B_embed,
  B_inlineConst,
  B_invalidOperation,
  B_neverSlot,
  B_makeInvalidInputDetails,
  B_markOutput,
  B_merge,
  B_pushCheck,
  B_refine,
  B_scope,
  B_throw,
  failInvalidType,
  type HoistCond
} from "./builder";
import { nestedLoc, never_, parse, typeCheckCond } from "./parse";

// ── Type identity ────────────────────────────────────────────────────────────

// The spec's "same type": tags match, including the class for instances and the
// format for formatted primitives. A schema with no tag of its own to compare
// (a union treated as a normal schema, a recursive ref) matches only itself.
const unionRuntimeSame = (a: Internal, b: Internal): boolean =>
  a.type === b.type && a.class === b.class;

const unionSameType = (a: Internal, b: Internal): boolean =>
  a === b ||
  (unionRuntimeSame(a, b) &&
    !(tagFlags[a.type]! & (512 | 256)) &&
    a.format === b.format);

const unionLiteralEqual = (a: unknown, b: unknown): boolean =>
  a === b || (a !== a && b !== b);

// Union reachability stops at the first `never`: a later `.to(...)` is not
// executable because the `never` decoder rejects first, and a link whose codec
// slot is the "never" sentinel produces nothing the same way. The general
// output helper intentionally follows the whole chain for type introspection,
// so the planner needs this terminal-aware view for production and coverage.
const unionOutput = (schema: Internal): Internal => {
  let output = schema;
  while (output.type !== neverTag && output.to !== U) {
    if (output.parser === B_neverSlot) return never_;
    output = output.to;
  }
  return output;
};

// Whether the variant's compiled direction crosses a "never" codec slot. The
// distinction that matters: a chain-terminal `S.never` still *dispatches* and
// rejects per value (effect 3), while a never slot removes the variant from
// this direction entirely, so it accepts nothing, counts toward no coverage,
// and yields to its siblings.
const unionNeverLink = (schema: Internal): boolean => {
  for (let node: Internal | undefined = schema; node !== U && node.type !== neverTag; node = node.to) {
    if (node.parser === B_neverSlot) return true;
  }
  return false;
};

// A nested union spreads into its parent only when it carries nothing of its
// own; otherwise it stays one opaque variant that matches by reference.
//
// "Nothing of its own" is a field count, not a list of interesting fields, so an
// unknown field reads as "carries something" and keeps the union whole — the
// conservative direction. The 6 are exactly what `unionFactory` sets: `type` and
// `seq` from `baseSchema`, then `anyOf`, `decoder`, `encoder`, `has`. `isAsync`
// and `hasTransform` are excluded because the parse loop writes them onto a live
// schema in place. Changing `unionFactory`'s field set without changing this
// count stops every union from flattening, which the nested-union goldens catch.
const unionIsTransparent = (schema: Internal): boolean => {
  if (schema.type !== anyOfTag) return false;
  let fields = 0;
  for (const key in schema) {
    if (key !== "isAsync" && key !== "hasTransform") fields++;
  }
  return fields === 6;
};

// One bounded structural walk supplies every recursive effect fact used by
// analysis. Refs, nested unions, functions, and custom parsers stop the walk,
// so public recursive schemas terminate without eager expansion.
const unionTraits = (schema: Internal): number => {
  const tag = tagFlags[schema.type]!;
  let traits = 0;
  // Low bits: Sury failure, foreign failure, opaque boundary, change.
  if ((tag & (256 | 512 | 4096)) || schema.parser !== U) return 15;
  if (schema.refiner !== U || schema.inputRefiner !== U) {
    traits |= 3;
  } else if (tag & (64 | 128 | 8192)) {
    traits |= 2;
  }
  if (schema.format !== U || isLiteral(schema)) {
    traits |= 1;
  }
  const to = schema.to;
  if (to !== U) {
    if (
      to === schema ||
      to.parser !== U ||
      (tagFlags[to.type]! & (256 | 512 | 4096))
    ) {
      traits |= 15;
    } else if (
      !(
        to.noValidation === true ||
        (tagFlags[to.type]! & 1) ||
        unionRuntimeSame(schema, to) ||
        (to.type === anyOfTag &&
          (unionMask(to, 1, 0) & tag))
      )
    ) {
      traits |= 9;
    } else {
      traits |= unionTraits(to);
    }
  }
  const fields = (schema.items ||
    schema.properties) as unknown as Record<string, Internal> | undefined;
  for (const key in fields) {
    const field = fields[key]!;
    traits |= unionTraits(field);
  }
  if (typeof schema.additionalItems === "object") {
    traits |= unionTraits(schema.additionalItems);
  }
  return traits;
};

// Typed decode/encode may skip validation and refinement, but never a value
// transformation. Refs stay conservative so this walk terminates on cycles.
const unionIsNoop = (schema: Internal): boolean => {
  if (schema.to !== U || schema.parser !== U || tagFlags[schema.type]! & 512) {
    return false;
  }
  const fields = (schema.anyOf || schema.items || schema.properties) as
    | Record<string, Internal>
    | undefined;
  for (const key in fields) {
    if (!unionIsNoop(fields[key]!)) return false;
  }
  return typeof schema.additionalItems !== "object" || unionIsNoop(schema.additionalItems);
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
  // Body can throw (1), must be awaited by fallback dispatch (2), is a
  // grouped wrapper (4), or may fall through to an overlapping member (8).
  f: number;
};

type UnionCtx = {
  // The aggregated union error, given the lazily collected per-case errors.
  f: (caught: string) => string;
  // `e[N]` for `getOrRethrow`, embedded on first use and shared by every case:
  // only a Sury validation error means "this variant didn't match".
  r: () => string;
  // The union schema, used to recognize and flatten an inner group's synthetic
  // "none of these members matched" wrapper without string-inspecting code.
  s: () => string;
};

const unionFail = (
  schema: Internal,
  path: Path,
  input: unknown,
  ...unionErrors: SuryErrorRecord[]
): never =>
  B_throw(
    B_makeInvalidInputDetails(
      schema,
      unknown,
      path,
      input,
      unionErrors.length ? unionErrors : U
    )
  );

// Whether a stretch of emitted code can raise is read off `g.t` (see
// `B_markThrow`) by bracketing the emission, not by inspecting the string it
// produced: `e[N](…)` is the accessor for *every* embed, so a body holding
// nothing but a total transform was wrapped in a `try` it could never need, and
// a raise spelled some other way would have silently lost its fallback.

// Emits a linear fallback chain: every alternative that fails hands the value to
// the next one, and the last failure raises the aggregated union error. An
// alternative whose failure is provably terminal (`ft === false`) skips the
// try/catch and throws its own precise error instead.
const unionEmitChain = (cases: UnionCase[], ctx: UnionCtx): string => {
  if (cases.length === 1) {
    const c = cases[0]!;
    if (c.b === "" && c.c === "") return "";
    if (c.b === "") {
      return `if(!(${c.c})){${ctx.f("")}}`;
    }
    if (c.c === "") return c.b + ";";
    return `if(${c.c}){${c.b}}else{${ctx.f("")}}`;
  }

  let code = "";
  let caught = false;
  let exhaustive = false;

  // The case's code with its condition taken as given — the shared shape between
  // a lone `if(cond){…}` and one arm of a run that tests `cond` once. A `try` arm
  // hands control to whatever follows it; every other form breaks, which ends its
  // block and needs no trailing `;`.
  const attempt = (c: UnionCase, idx: number): string => {
    if (c.b === "") return "break";
    // Skip the `;` where the body already ends in one: `;;break` is a wart in
    // every golden it reaches.
    const body = c.b.endsWith(";") ? c.b : `${c.b};`;
    // A `try` is needed when the case can raise and either a later alternative
    // could still accept the value or an earlier one is already relying on the
    // chain to carry its failure forward.
    if ((c.f & 1) && ((c.f & 8) || caught)) {
      caught = true;
      const record =
        c.f & 4
          ? `x=${ctx.r()}(x);if(x.expected===${ctx.s()}){x=x.unionErrors;x&&(r||(r=[])).push(...x)}else{(r||(r=[])).push(x)}`
          : `(r||(r=[])).push(${ctx.r()}(x))`;
      // A terminal case — one only present because an *earlier* one needs the
      // chain to carry its failure — records and lets control reach the chain's
      // own fail, so the error keeps its "Expected A | B | C" framing. Every case
      // after it is guarded by a condition the value has already been proven not
      // to satisfy, so reaching them costs a few false tests.
      //
      // Unless one of them has no condition at all: that one would run, fail on a
      // type it was never offered, and add a reason the value could never have
      // matched — 4.7x on a 24-member union. Only then is the fail worth inlining
      // here, because a second spread call site in a `catch` is not free: it cost
      // 6x on the small instance-dispatch schemas when emitted unconditionally.
      return `try{${body}break}catch(x){${record}${
        !(c.f & 8) && unconditional > idx
          ? `;${ctx.f(",...(r||[])")}`
          : ""
      }}`;
    }
    return `${body}break`;
  };

  // The last case that runs whatever reaches it, so `attempt` can tell whether
  // anything after a terminal case would actually execute.
  let unconditional = -1;
  for (let idx = 0; idx < cases.length; idx++) {
    if (cases[idx]!.c === "") unconditional = idx;
  }

  // The condition of the case just emitted, and whether its block is still open
  // — i.e. ended in a `try` that hands control onward rather than a `break`.
  let last = "";
  let open = false;

  for (let idx = 0; idx < cases.length; idx++) {
    const c = cases[idx]!;
    // Members that narrow the same way — two variants of one tuple shape, two
    // objects behind the same discriminant — share the test. Only the case
    // *immediately* before qualifies: a case in between with a different
    // condition could accept a value these two also accept, and pulling the
    // later one back past it would change which member wins.
    const shared = c.c !== "" && c.c === last;
    // Behind a condition the previous case already accepted outright, this one
    // can never run. Dropping it is what removes the unreachable second
    // `if(i===void 0){…}` an `optional`-of-`optional` used to emit.
    if (shared && !open) continue;

    const arm = attempt(c, idx);
    open = arm[0] === "t"; /* `try` */
    last = c.c;

    if (shared) {
      code = `${code.slice(0, -1)}${arm}}`;
    } else if (c.c === "") {
      // Nothing left to test: this alternative accepts every value that reaches
      // it, so unless it can fail nothing after it is reachable.
      code += open ? arm : `${arm};`;
      if (!open) {
        exhaustive = true;
        break;
      }
    } else {
      // `if(cond)break;` beats `if(cond){break}` by two characters, and a case
      // that accepts without running anything is the commonest shape there is.
      code += arm === "break" ? `if(${c.c})break;` : `if(${c.c}){${arm}}`;
    }
  }

  if (!exhaustive) {
    code += ctx.f(caught ? ",...(r||[])" : "");
  }
  return `for(;;){${caught ? "let r;" : ""}${code}}`;
};

const unionOr = (cs: UnionCase[]): string => {
  const s = cs.map((c) => c.c).join("||");
  return cs.length > 1 ? `(${s})` : s;
};

// ── Group narrows ────────────────────────────────────────────────────────────

// A minimal schema standing in as the variant's runtime type, shared by every
// variant in the group. Built without a per-type factory reference so unused
// type decoders still tree-shake out of a union-using bundle — and
// `S.optional`/`S.nullable` are unions.
const unionNarrowSchema = (schema: Internal): Internal => {
  const tagFlag = tagFlags[schema.type]!;
  const container = 64 | 128;
  // The decoder closes over `narrow` itself. Passing it to the constructor is
  // still fine — nothing calls it until the schema is fully built.
  // This schema is only used by effect-compatible validation groups. It owns
  // the runtime tag check, never a member's conversion.
  const narrow: Internal = baseSchema(schema.type, false, (input: Val) => {
    if (tagFlags[input.s.type]! & 1) {
      return B_refine(input, input.e, [
        {
          c: (inputVar) => typeCheckCond(input, schema, inputVar),
          f: failInvalidType,
        },
      ]);
    }
    if (unionRuntimeSame(input.s, narrow)) {
      return tagFlag & container
        ? B_refine(input, input.e)
        : input;
    }
    return schema.decoder(input);
  });
  narrow.encoder = schema.encoder;
  // With the encoder, its marker: the narrow is what a carrier's encoder is
  // handed for this arm, and without it the arm reads as carrying no payload —
  // bytes reaching a `S.base64` variant UTF-8-decoded instead of packing.
  if (schema.content !== U) {
    setContent(narrow, schema.content);
  }
  if (tagFlag & 8192) {
    narrow.class = schema.class;
  } else if (tagFlag & container) {
    narrow.additionalItems = unknown;
    if (tagFlag & 64) {
      narrow.properties = immutableEmptyObject as Record<string, Internal>;
    } else {
      narrow.items = immutableEmptyArray as Internal[];
    }
  } else if (tagFlag & (32 | 16 | 2048)) {
    // null/undefined/nan stay literals so the case body passes through.
    narrow.const = schema.const;
  }
  return narrow;
};

// Tag bits don't partition runtime values: every instance passes the object
// narrow, so two such cases are only provably disjoint after widening each to
// everything its narrow could also let through. Arrays and NaN need no widening —
// the object and number narrows exclude them explicitly.
const unionWiden = (tagFlag: number, nan: number): number =>
  tagFlag | (tagFlag & (64 | 8192) ? 64 | 8192 : tagFlag & tagFlags[numberTag]! ? nan : 0);

// Mode 0 describes produced output, 1 a member's accepted input, and 2 the
// declared source (whose root ref may expose a bounded input tag).
// A self-describing boundary's definition: what `$ref` + `$defs` resolve to
// (`S.json`'s recursive union), or undefined for everything else.
const unionRefDef = (schema: Internal): Internal | undefined => {
  const defs = schema["$defs"], ref = schema["$ref"];
  if (defs === U || ref === U) return U;
  const resolved = defs[ref.slice(ref.lastIndexOf("/") + 1)];
  return resolved !== U && resolved !== schema ? resolved : U;
};

const unionMask = (schema: Internal, mode: number, nan: number): number => {
  if (mode === 2) {
    const resolved = unionRefDef(schema);
    if (resolved !== U) return unionMask(resolved, 1, nan);
  }
  const tagFlag = tagFlags[schema.type]!;
  if (!mode && tagFlag & 32768) return 0;
  if (mode && tagFlag & 256) {
    let mask = 0;
    const variants = schema.anyOf!;
    for (let i = 0; i < variants.length; i++) mask |= unionMask(variants[i]!, 1, nan);
    return mask;
  }
  // ~0: bitwise ops only see the low 32 bits, so this stays future-proof.
  return tagFlag & (1 | 256 | 512)
    ? ~0
    : unionWiden(tagFlag, nan);
};

// A member's effect (`UnionMember.e`), as a literal because naming these five
// costs ~40 bundle bytes that esbuild will not inline. The scale is ordered, and
// two cuts in it carry the planner. `e < 2` — "changes nothing about the value" —
// is what lets members share a group, since order among them is unobservable.
// `e === 0` is stronger: total for its type, which is what lets one member cover
// another outright.
//
//   0 identity   — accepts every value of its type and passes it straight
//                  through, so it is the only effect that can cover a member.
//   1 validation — checks the value but does not change it.
//   2 coercion   — converts, so it cannot share a group with a pass-through.
//   3 rejection  — output is `never`; executable, but accepts nothing.
//   4 opaque     — a boundary (ref, nested union, function, custom parser) the
//                  analysis will not look inside.

// Member low bits record Sury throws (1), foreign throws (2), and opacity (4).
// Case bit 1 is refined from emitted-body throw tracking, never inferred from
// the semantic Sury bit. Falls (8) and direct dispatch (16) are shared by plan.

// Bits 1, 2 and 4 of `f` mean different things on a `UnionCase` than on a
// `UnionGroup`, and are deliberately left as literals (naming them costs ~30
// bundle bytes because they don't get inlined):
//
//   case 1  — the emitted body can raise, read off throw tracking (`g.t`).
//   case 2  — the body yields a promise the dispatch must await before it can
//             tell whether the member matched.
//   case 4  — the body is a nested chain, so its failure arrives as the
//             synthetic "none of these matched" wrapper, not a member's error.
//   group 2 — some value reaching this group could still be accepted by a later
//             one, so a failure here has to be caught rather than raised.
//   group 32 — later same runtime type; emit ANDs this group's discriminants
//             into the outer condition so a disjoint TAG is not eaten (#392).
//
// Bits 8 (falls) and 16 (direct dispatch) mean the same on both.

type UnionDiscriminator = [key: string, value: unknown];

type UnionMember = {
  i: number;
  s: Internal;
  m: number;
  // Whether the member produces a value at all: `mode 0` masks are zero for
  // exactly one reason, a `never` output, and nothing reads more than that.
  o: boolean;
  e: number;
  f: number;
  p: number;
  k: unknown;
  r: number;
  d?: UnionDiscriminator;
};

type UnionGroup = {
  m: number;
  a: UnionMember[];
  f: number;
  n?: Internal;
  // Planner-only. `p` is the specificity tier the group flattens at — the tier of
  // the member that opened it. `o` is whether it can still absorb a later member.
  p: number;
  o: boolean;
};

// One runtime tag family's groups, in the order they were opened. Flattening is a
// stable sort by tier, so a group's position among its peers is where it opened.
type UnionBucket = {
  m: number;
  t: UnionGroup[];
};

// Sparse reverse-stream summary of every member after the current group,
// keyed by its non-overlapping runtime tag family. `false` is an overlap
// barrier (a broad member or conflicting discriminator keys); otherwise the
// tuple holds the common key and its SameValueZero values.
//
// This proves distinct exact/semantic discriminator values disjoint in O(1)
// expected time per member without comparing it with every later member.
type UnionOverlapSummary = [string, Set<unknown>] | false;

const unionGroup = (member: UnionMember): UnionGroup => ({
  m: member.m,
  a: [member],
  f: member.f & 16,
  p: member.p,
  o: false,
});

const unionDiscriminator = (schema: Internal): UnionDiscriminator | undefined => {
  if (isLiteral(schema)) {
    return ["", schema.const];
  }
  const fields = (schema.properties || schema.items) as
    | Record<string, Internal>
    | undefined;
  for (const key in fields) {
    const field = fields[key]!;
    if (isLiteral(field)) {
      return [key, field.const];
    }
  }
  return U;
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
  // What the unmatched side is called in the message — the union sits opposite
  // it, so this is also which of source/target `variants` belongs to.
  outputSide: boolean
): void => {
  const other = outputSide ? target : source;
  let matched: Internal | undefined = U;
  let unmatched = false;
  for (let idx = 0; idx < variants.length; idx++) {
    const variant = variants[idx]!;
    const match = outputSide ? unionOutput(variant) : variant;
    // The sentinel-aware unionOutput already reports a never-linked chain on
    // the output side, so the explicit link walk is only for the input side.
    if (variant.type === neverTag || (outputSide ? match.type === neverTag : unionNeverLink(variant))) {
      continue;
    }
    if (unionSameType(other, match)) matched ||= variant;
    else unmatched = true;
  }
  if (matched !== U && unmatched) {
    unionInvalid(
      input,
      source,
      target,
      `${inputExpression(matched!)} has the same type as the ${outputSide ? "target" : "source"} and the others don't`
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
    `${inputExpression(variant)} has no same-type variant on the other side`
  );

const unionInvalid = (input: Val, from: Internal, to: Internal, why: string): never =>
  B_invalidOperation(
    input,
    `Invalid operation: can't convert ${inputExpression(from)} to ${inputExpression(
      to
    )} — ${why}. Use S.to to say what you mean, or S.never to mark a variant unreachable`
  );

// ── Normalize → Analyze → Plan → Emit ────────────────────────────────────────

const unionAnalyze = (
  sourceMask: number,
  flags: number,
  sourceTag: number,
  variants: Internal[],
  source: Internal,
  nan: number
): UnionMember[] => {
  const out: UnionMember[] = [];
  const unknownSource = sourceTag & 1;
  const sourceBoundary = sourceTag & (256 | 512);
  const unionSource =
    sourceBoundary &&
    sourceMask !== ~0;
  const sourceDiscriminator = unionDiscriminator(source);
  const exact = flags & 1;
  const broadObject = flags & 64;
  const broadNumber = flags & tagFlags[numberTag]!;
  const numberish = tagFlags[numberTag]! | 2048;
  for (let i = 0; i < variants.length; i++) {
    const s = variants[i]!;
    const tag = tagFlags[s.type]!;
    const inputMask = unionMask(s, 1, nan);
    const d = unionDiscriminator(s);
    const same = unionRuntimeSame(source, s);
    const discriminatorDisjoint =
      sourceDiscriminator !== U &&
      d !== U &&
      same &&
      sourceDiscriminator[0] === d[0] &&
      !unionLiteralEqual(sourceDiscriminator[1], d[1]);
    const accepts =
      !(tag & 32768) &&
      !unionNeverLink(s) &&
      !discriminatorDisjoint &&
      (!exact ||
        (isLiteral(s)
          ? unionLiteralEqual(s.const, source.const)
          : sourceMask & inputMask));
    const native = sourceMask & tag;
    const coerces =
      accepts &&
      !unknownSource &&
      !(unionSource ? native : same);
    const output = unionOutput(s);
    const traits = unionTraits(s);
    const sourceDeopt = sourceBoundary && (!unionSource || coerces);
    const effect =
      output.type === neverTag
        ? 3
        : (traits & 4) || sourceDeopt
          ? 4
          : coerces || (traits & 8)
            ? 2
            : (traits & 1) || (tag & (64 | 128 | 8192 | 512 | 256))
              ? 1
              : 0;
    out[i] = {
      i,
      s,
      m: accepts
        ? unknownSource
          ? inputMask
          : unionSource
            ? native
              ? inputMask
              : s.type === undefinedTag && (sourceMask & 32)
                ? 32
                : s.type === nullTag && (sourceMask & 16)
                  ? 16
                  : // Reached only by coercion. Every built-in cross-tag
                    // coercion parses a string (`BigInt`, `Number`, `new Date`),
                    // so a source that can produce one is assumed to be coerced
                    // through it — narrow enough to keep the case out of an
                    // unnecessary fallback. With no string in the source that
                    // guess describes nothing, and claiming too little would let
                    // the dispatch raise where a later member should have run,
                    // so fall back to "any type the source produces".
                    sourceMask & 2
                    ? 2
                    : sourceMask
            : sourceMask
        : 0,
      o: !!accepts && output.type !== neverTag,
      e: effect,
      f:
        (traits & 7) |
        (effect ? 1 : 0) |
        (sourceDeopt ? 4 : 0) |
        ((!unknownSource && same) || (tag & (1 | 256 | 512 | 4096 | 32768)) ? 16 : 0),
      p:
        (s.type === objectTag && nestedLoc in s.properties!) ||
        (broadObject && (tag & (128 | 8192))) ||
        (broadNumber && (tag & 2048))
          ? 0
          : d !== U
            ? 1
            : 2,
      k: tag & 8192 ? s.class : s.type,
      r: tag & (64 | 8192)
        ? 64 | 8192
        : tag & numberish
          ? numberish
          : unionWiden(tag, nan),
      d,
    };
  }
  return out;
};

// An `undefined` or `null` member that hands its value back is tried before
// any earlier member whose source is `unknown`. Such a member accepts every
// value, so the sentinel would otherwise reach its custom coder — the one
// value that coder was never written for (#347). Only an object property can
// express "absent" at all, and `S.optional` appends the sentinel after the
// wrapped union, so declaration order can't say the sentinel comes first.
const unionHoistSentinels = (members: UnionMember[]): UnionMember[] => {
  let at = -1;
  for (let i = 0; i < members.length; i++) {
    const member = members[i]!;
    const tag = tagFlags[member.s.type]!;
    if (at < 0) {
      if (member.m && (tag & 1)) at = i;
    } else if (member.e < 2 && (tag & (16 | 32))) {
      members.splice(i, 1);
      members.splice(at++, 0, member);
    }
  }
  return members;
};

const unionPlan = (members: UnionMember[]): UnionGroup[] => {
  const sequence: (UnionBucket | UnionGroup)[] = [];
  const active: (UnionBucket | undefined)[] = [];
  const priority: (UnionBucket | undefined)[] = [];

  // `total` — types some member accepts *totally*: effect 0 takes every value its
  // type narrow admits and hands it back untouched. `effects` — types some member
  // that changes the value accepts.
  let total = 0;
  let effects = 0;
  for (let i = 0; i < members.length; i++) {
    const member = members[i]!;
    if (member.e > 1) {
      effects |= member.m;
    } else if (!member.e) {
      total |= member.m;
    }
  }

  for (let i = 0; i < members.length; i++) {
    const member = members[i]!;
    // A pass-through member accepting no type beyond what some total member
    // already covers is dead, whichever side of it that member sits on: both
    // produce the input unchanged, so which one a value reaches is unobservable,
    // and a value the total member rejects was outside both. Dropping it before
    // anything is compiled is what collapses `"a" | string | "b"` to one narrow.
    //
    // Two things make a member observable despite passing its value through:
    //
    //   - It can raise a *foreign* error (`f & 2`) — a user refiner, a getter.
    //     That escapes the union rather than reading as "this one didn't match",
    //     so running it is the observable part, not what it returns.
    //   - A member that *does* change the value accepts one of the same types. It
    //     may sit anywhere, because dropping a pass-through hands its values to
    //     whatever runs next: `S.literal(-0) | string-from-number | number`
    //     decodes 0 to `-0` only because the literal claims it first.
    //
    // Only validating members are dropped, never a total one — two total members
    // for a type would each read as covered by the other and both vanish. Nothing
    // is lost by keeping them: they share a group, and `unionEmit` collapses their
    // empty conditions into the one narrow anyway.
    if (
      member.m === 0 ||
      (member.e === 1 && !(member.f & 2) && !(member.m & (effects | ~total)))
    ) {
      continue;
    }

    const bucketed =
      member.r !== ~0 &&
      (member.m & ~member.r) === 0;
    // Field discriminants prove members don't compete, so payload opacity
    // must not exclude them — that stranded later TAGs behind a type-narrow
    // miss (#392). Coercing literals stay ungrouped: a later same-type
    // catch-all would wrap them in try/catch longer than sequential checks.
    const compatible =
      member.e < 2 ||
      (member.d !== U && (member.e === 4 || member.d[0] !== ""));
    let bucket = bucketed
      ? member.p === 0
        ? priority[member.r] || active[member.r]
        : active[member.r]
      : U;

    // One walk of the bucket settles everything positional about this member:
    // which group it joins, whether a broad group is already placed, and which
    // other groups it closes. Closing on a bucket the split below then abandons
    // is harmless — that bucket leaves `active`, so nothing can join it again.
    //
    // A member joins a group that is still open, holds the same runtime type, and
    // sits on the same side of the pass-through boundary — unless both carry
    // the same discriminant key, in which case they never compete and the
    // effect cut would only strand a later TAG behind this group's type
    // narrow (#392). Tier is deliberately *not* part of the test: tiers order
    // groups by specificity, and specificity between two members of one type
    // that both hand the value back is unobservable, so they belong under one
    // narrow. Tier 0 is the exception — it holds what must be tried first (an
    // exact NaN, a nested object), and folding one into a broader group would
    // bury it behind that group's own members.
    let open: UnionGroup | undefined = U;
    let broad = false;
    if (bucket !== U) {
      for (let j = 0; j < bucket.t.length; j++) {
        const group = bucket.t[j]!;
        const first = group.a[0]!;
        broad ||= group.p === 2;
        if (
          open === U &&
          compatible &&
          group.o &&
          first.k === member.k &&
          ((first.e < 2) === (member.e < 2) ||
            (first.d !== U && first.d[0] === member.d?.[0])) &&
          (group.p === 0) === (member.p === 0)
        ) {
          open = group;
        } else if (group.o && (group.m & member.m)) {
          group.o = false;
        }
      }
    }

    for (const key in active) {
      const other = active[+key]!;
      if (other !== bucket && (other.m & member.m)) {
        delete active[+key];
      }
    }

    if (!bucketed) {
      for (const key in priority) {
        if (priority[+key]!.m & member.m) {
          delete priority[+key];
        }
      }
      sequence.push(unionGroup(member));
      continue;
    }

    // Flattening would hoist a discriminated member ahead of a broad one already
    // in this bucket, changing which member a value reaches — so it starts a
    // bucket of its own instead. Unless it joins that member's own group, where
    // the two are ordered against each other by the chain rather than by tier.
    if (bucket !== U && open === U && member.p === 1 && broad) {
      delete active[member.r];
      bucket = U;
    }

    if (bucket === U) {
      bucket = {
        m: 0,
        t: [],
      };
      active[member.r] = bucket;
      priority[member.r] ||= bucket;
      sequence.push(bucket);
    }
    bucket.m |= member.m;

    if (open !== U) {
      open.a.push(member);
      open.m |= member.m;
      open.f &= ~16;
    } else {
      const group = unionGroup(member);
      group.o = compatible;
      bucket.t.push(group);
    }
    if (!compatible) {
      delete active[member.r];
    }
  }

  const plan: UnionGroup[] = [];
  for (let i = 0; i < sequence.length; i++) {
    const item = sequence[i]!;
    if ("a" in item) {
      plan.push(item);
    } else {
      // Stable, so groups of one tier keep the order they were opened in.
      plan.push(...item.t.sort((a, b) => a.p - b.p));
    }
  }

  const later: (UnionOverlapSummary | undefined)[] = [];
  let laterMask = 0;
  let laterBroad = 0;
  for (let i = plan.length - 1; i >= 0; i--) {
    const group = plan[i]!;
    let key: unknown = U;
    let values: Set<unknown> | undefined;
    for (let j = group.a.length - 1; j >= 0; j--) {
      const member = group.a[j]!;
      const d = member.d;
      const conflict =
        d === U || key === false || (key !== U && key !== d[0]);
      if (
        key !== U &&
        (conflict || values!.has(d[1]))
      ) {
        member.f |= 8;
        group.f |= 2;
      }
      if (conflict) {
        key = false;
      } else {
        key = d[0];
        (values ||= new Set()).add(d[1]);
      }
    }
    const head = group.a[0]!;
    const route = head.r;
    const semantic = later[route];
    let overlaps =
      !!(laterMask & group.m) &&
      (!!(laterBroad & group.m) ||
        key === false ||
        semantic === U ||
        semantic === false ||
        semantic[0] !== key);
    if (!overlaps && semantic !== U && semantic !== false) {
      for (const value of values!) {
        if (semantic[1].has(value)) {
          overlaps = true;
          break;
        }
      }
    }
    if (
      overlaps ||
      (laterMask &&
        (tagFlags[head.s.type]! & (1 | 256 | 512 | 4096 | 32768)) &&
        (head.s.to !== U || head.s.parser !== U))
    ) {
      group.f |= 8 | 2;
    }
    if (group.a.length !== 1 || !(group.f & 16)) {
      group.n = unionNarrowSchema(head.s);
      // A single-member string group carries its member's `format` (which
      // toJSONSchema reads), `escapeFree` and `noValidation` onto the narrow:
      // the group emit appends the member's format check, so the escape-free
      // splice holds inside the case. A multi-member group can't — one narrow
      // stands in for every member — and `format: "json"` must not, since
      // jsonString reads that as "already JSON text" (see fieldPiece).
      if (
        group.a.length === 1 &&
        head.s.format !== U &&
        head.s.format !== "json" &&
        group.n.type === stringTag
      ) {
        group.n.format = head.s.format;
        group.n.escapeFree = head.s.escapeFree;
        group.n.noValidation = head.s.noValidation;
      }
    }
    if (route !== ~0 && (group.m & ~route) === 0) {
      if (key === false) {
        later[route] = false;
      } else if (semantic === U) {
        later[route] = [key as string, values!];
      } else if (semantic !== false) {
        if (semantic[0] !== key) {
          later[route] = false;
        } else {
          for (const value of values!) {
            semantic[1].add(value);
          }
        }
      }
    } else {
      laterBroad |= group.m;
    }
    if (laterMask & group.m) {
      group.f |= 32;
    }
    laterMask |= group.m;
  }
  return plan;
};

const unionEmit = (
  input: Val,
  self: Internal,
  plan: UnionGroup[],
  toPerCase: Internal | undefined,
  trustedSelf?: boolean
): Val => {
  const initialInline = input.i;
  let output = B_refine(input);
  // An async case only has to be awaited so that its rejection can be caught and
  // the value handed to a later group — which is exactly where a group is marked
  // for fallback. With no fallback anywhere, the sole async case's promise is
  // returned unawaited, saving the async wrapper.
  const awaitAsync = plan.some((group) => group.f & 2);
  const outputBySource: (Internal | undefined)[] = [];
  let salvaged = "";
  let rethrow = "";
  let expected = "";
  const ctx: UnionCtx = {
    f: (caught) =>
      `${B_embed(input, unionFail.bind(U, self, input.path))}(${input.v()}${salvaged}${caught})`,
    r: () => rethrow || (rethrow = B_embed(input, getOrRethrow)),
    s: () => expected || (expected = B_embed(input, self)),
  };
  // Trusting a case's discriminant requires it to actually discriminate:
  // unique among every member's (two ReScript variants can share their first
  // literal field, e.g. `TAG: "Connective"`, and only differ on a later one),
  // and no *other* member may accept this member's runtime type. The second
  // half is what makes "the value passed the union" mean "the value matches
  // this variant": in `S.union([{kind: "a", v: S.string}, S.unknown])`,
  // `{kind: "a", v: 1}` is accepted by `unknown`, and the `kind` discriminant
  // then routes it to the first case — which must still check `v`. Comparing
  // acceptance masks rather than type keys is what catches the broad member;
  // `unknown` keys itself `"unknown"` and would otherwise look disjoint from
  // every object.
  const unionDTrusted = (member: UnionMember): boolean => {
    const d = member.d!;
    const tag = tagFlags[member.s.type]!;
    for (const group of plan) {
      for (const m of group.a) {
        if (m === member) continue;
        if (m.d !== U ? m.d[0] === d[0] && unionLiteralEqual(m.d[1], d[1]) : m.m & tag) {
          return false;
        }
      }
    }
    return true;
  };
  const compile = (
    member: UnionMember,
    source: Val,
    target: Val
  ): UnionCase | undefined => {
    const mark = input.g.t;
    const caseInput = B_scope(source);
    caseInput.u = true;
    caseInput.t = source.t;
    caseInput.io = false;
    caseInput.e = member.s;
    // Trusted source + a field discriminant to dispatch on: the case converts
    // from its own variant instead of re-validating from unknown — what makes
    // `decode` skip member validation the way a typed object does. Restricted
    // to field-discriminated members (a literal or scalar member's validation
    // check IS its dispatch condition) that can't fall through (a falling
    // member is re-dispatched by its body failing, which a trusted body never
    // does). The discriminant equality, which trusted compilation no longer
    // emits as a hoisted check, is added to the dispatch condition below.
    const trustedD =
      trustedSelf &&
      member.p === 1 &&
      !(member.f & 8) &&
      member.d![0] !== "" &&
      unionDTrusted(member)
        ? member.d
        : U;
    if (trustedD !== U) caseInput.s = member.s;
    let caseOut: Val;
    const options = input.g.o;
    input.g.o |= 4;
    try {
      caseOut = parse(caseInput);
    } catch (exn) {
      if (!self.perVariant) throw exn;
      salvaged += `,${B_embed(input, getOrRethrow(exn))}`;
      return U;
    } finally {
      input.g.o = options;
    }
    if (member.o) outputBySource[member.i] = caseOut.s;
    const cond: HoistCond = { c: "", h: [] };
    // Hoist the type narrow even when the member can fall through. A value the
    // narrow rejects could never have been accepted by this member, so skipping
    // it with `if(cond)` reaches the next member exactly like catching would —
    // but without re-emitting the narrow as a statement, without the try/catch
    // when nothing deeper can fail, and without recording a "reason" for a
    // member the value was never plausibly an instance of.
    let body = B_merge(caseOut, cond);
    const async = caseOut.f & 1;
    output.f |= async;
    if (caseOut.t) {
      output.t = true;
      const itemVar = target.v();
      if (async || caseOut.i !== itemVar) {
        body += `${itemVar}=${async && awaitAsync ? "await " : ""}${caseOut.i}`;
      }
    }
    if (trustedD !== U) {
      const dSchema = ((member.s.properties || member.s.items) as Record<
        string,
        Internal
      >)[trustedD[0]]!;
      // `===` can't match a NaN discriminant (`x===NaN` is always false), which
      // made the member unreachable and rejected a value it declares. The
      // hoisted-check path this replaces used the same NaN-aware form.
      const dRead = `${source.v()}[${inlinedValueFromString(trustedD[0])}]`;
      const dCond =
        dSchema.type === nanTag
          ? `Number.isNaN(${dRead})`
          : `${dRead}===${B_inlineConst(caseInput, dSchema)}`;
      cond.c = cond.c ? `${dCond}&&${cond.c}` : dCond;
    }
    return {
      c: cond.c,
      b: body,
      f:
        (body !== "" && input.g.t !== mark ? 1 : 0) |
        (async && awaitAsync ? 2 : 0) |
        (member.f & 8),
    };
  };

  const cases: UnionCase[] = [];
  for (let i = 0; i < plan.length; i++) {
    const group = plan[i]!;
    if (group.a.length === 1 && group.f & 16) {
      const c = compile(group.a[0]!, input, input);
      if (c !== U) {
        c.f |= group.f & 8;
        cases.push(c);
        if (c.c === "" && c.b === "") break;
      }
      continue;
    }

    const mark = input.g.t;
    const narrowInput = B_scope(input);
    narrowInput.io = false;
    narrowInput.e = group.n!;
    const narrow = parse(narrowInput);
    // The narrow proves the group's runtime tag, but its minimal schema
    // forgets what the source guarantees about that tag's CONTENT — a `S.json`
    // source's object is a dict of json whose fields still need converting (a
    // bigint field arrives as its string form). Restore the source's own
    // same-tag variant as the case input, so a structured case converts from
    // it instead of validating the already-converted shape
    // (specs/jsonstring-union-encode.yaml). Direct single-member cases keep
    // `input.s` and never lose it.
    if (tagFlags[group.n!.type]! & (64 | 128)) {
      const resolved = unionRefDef(input.s);
      const sourceVariant =
        resolved !== U && resolved.anyOf !== U
          ? resolved.anyOf.find((v) => v.type === group.n!.type)
          : U;
      if (sourceVariant !== U) narrow.s = sourceVariant;
    }
    const inner: UnionCase[] = [];
    for (let j = 0; j < group.a.length; j++) {
      const c = compile(group.a[j]!, narrow, narrowInput);
      if (c !== U) {
        inner.push(c);
        if (c.c === "" && c.b === "") break;
      }
    }
    if (!inner.length) continue;

    const cond: HoistCond = { c: "", h: [] };
    let body: string;
    let grouped = false;
    if (inner.every((c) => c.b === "")) {
      if (!inner.some((c) => c.c === "")) {
        const fused = unionOr(inner);
        B_pushCheck(narrow, { c: () => fused, f: failInvalidType });
      }
      body = B_merge(narrow, cond);
    } else {
      const narrowCode = B_merge(narrow, cond);
      const only = inner.length === 1 ? inner[0]! : U;
      if (only !== U && narrowCode === "") {
        if (only.c !== "") {
          cond.c = cond.c ? `${cond.c}&&${only.c}` : only.c;
        }
        body = only.b;
      } else {
        // Flag 32: a later group still accepts this runtime type. Entering
        // on the type narrow alone would throw+break on a TAG this group
        // does not own and never reach that later group (#392).
        if (inner.length > 1 && group.f & 32 && inner.every((c) => c.c)) {
          const fused = unionOr(inner);
          cond.c = cond.c ? `${cond.c}&&${fused}` : fused;
        }
        body = narrowCode + unionEmitChain(inner, ctx);
        grouped = inner.length > 1;
      }
    }
    cases.push({
      c: cond.c,
      b: body,
      f:
        (body !== "" && input.g.t !== mark ? 1 : 0) |
        (inner.some((c) => c.f & 2) ? 2 : 0) |
        (group.f & 8) |
        (grouped ? 4 : 0),
    });
    if (body === "" && cond.c === "") break;
  }

  // Once an unconditional identity case is reached, preceding identity-only
  // conditions cannot affect the result: every input is accepted unchanged.
  // Erase the whole dispatch instead of emitting a redundant `for(;;)` loop.
  const noop =
    cases.every((c) => c.b === "") && cases.some((c) => c.c === "");
  const pure =
    !noop &&
    cases.length > 0 &&
    cases.every((c) => c.c !== "" && c.b === "");
  const asyncDispatch = cases.some((c) => c.f & 2);
  if (pure) {
    const fused = unionOr(cases);
    output = B_refine(
      B_refine(output, output.s, [{ c: () => fused, f: failInvalidType }], self)
    );
  } else if (!noop) {
    const dispatch = unionEmitChain(cases, ctx);
    if (asyncDispatch) {
      const itemVar = input.v();
      output.i = `(async(${itemVar})=>{${dispatch};return ${itemVar}})(${itemVar})`;
    } else {
      output.cp += dispatch;
    }
  }
  if (!asyncDispatch) output.i = input.i;
  let out: Val;
  if (output.f & 1) {
    output.i = `Promise.resolve(${output.i})`;
    output.v = _notVar;
    out = output;
  } else if (
    output.v === _var &&
    input.cp === "" &&
    output.cp === "" &&
    !pure &&
    initialInline === "i"
  ) {
    input.hd = "";
    input.v = _notVar;
    input.i = initialInline;
    out = input;
  } else {
    out = output;
  }
  const outputAnyOf = outputBySource.filter(Boolean) as Internal[];
  out.s = outputAnyOf.length ? unionFactory(outputAnyOf) : never_;
  if (toPerCase !== U) {
    out.io = true;
    out.e = unionOutput(toPerCase);
    return out;
  }
  out.e = self;
  return B_markOutput(out, input);
};


export const unionDecoder: Builder = (input: Val) => {
  const self = input.e;
  // The union's own `.to` chain, applied per case during decoding. None when a
  // custom parser owns the conversion, or when the target is the `noValidation`
  // sentinel `S.assertInput` appends: fusing that into every case replaces each
  // member's own check with the sentinel's, which silently breaks dispatch.
  // Left alone, it converts the union's assembled output once instead.
  const toPerCase =
    self.parser === U && self.to !== U && self.to.noValidation !== true ? self.to : U;
  let variants = self.anyOf!;

  if (
    // Already validated against this exact schema.
    (input.io! && input.e === input.s) ||
    (input.s === self && toPerCase === U && variants.every(unionIsNoop)) ||
    (input.s.type === anyOfTag &&
      toPerCase === U &&
      input.s.anyOf!.every((inS, idx) => {
        const s = variants[idx];
        return (
          s !== U &&
          !(tagFlags[inS.type]! &
            (128 | 8192 | 512 | 256 | 64)) &&
          inS.type === s.type &&
          unionLiteralEqual(inS.const, s.const) &&
          inS.to === U &&
          s.to === U
        );
      }))
  ) {
    return input;
  }

  const initialTagFlag = tagFlags[input.s.type]!;
  // A source typed as this very union was already validated against it (the
  // parse direction reaches here as `unknown`), so a case the discriminant
  // selects can trust its variant's fields the way a typed object does —
  // re-validating them is what made `decode` compile the same code as
  // `parse`. The widening below still runs: dispatch needs the runtime
  // narrows, since the value's variant is only known at runtime.
  // `self.tr` is the same guarantee arriving second-hand: `unionRewrite`
  // already performed this widening on a union-typed source, so the val no
  // longer names it. Without that, a union serialized as an array item or
  // object field — which reaches the target through `unionEncoder` — would
  // re-validate every field inside the container's loop.
  const trustedSelf = input.s === self || self.tr;
  if (
    (initialTagFlag & 256) ||
    (input.s.encoder === U && (initialTagFlag & 512))
  ) {
    input.s = unknown;
  }

  // A never-slot variant yields to its siblings, but with nothing left to
  // yield to the operation itself is the mistake.
  if (variants.every(unionNeverLink)) {
    B_invalidOperation(
      input,
      `Every variant of ${inputExpression(self)} is marked as never`
    );
  }

  const source = input.s;
  const nan = input.g.o & 2 ? 2048 : 0;
  let flags = 0;
  const sourceLiteral = isLiteral(source);
  for (let i = 0; i < variants.length; i++) {
    const member = variants[i]!;
    if (sourceLiteral && isLiteral(member) && unionLiteralEqual(member.const, source.const)) {
      flags |= 1;
    }
    flags |= tagFlags[member.type]! & (64 | tagFlags[numberTag]!);
  }
  const sourceTag = tagFlags[source.type]!;
  // A source that can hold anything constrains nothing, so it can't prove two
  // cases disjoint.
  // Rule 2 — matching some but not all target variants is ambiguous: pass the
  // value through to the matching one, or attempt decoding in definition order?
  // Two sources are never ambiguous: `unknown`, which may already be any of the
  // variant types (so nothing is coerced either way), and a const the target
  // spells out exactly — that variant takes the value as it is, and no other
  // variant can produce it.
  if (!(sourceTag & 1) && !(flags & 1)) {
    unionCheckPartial(input, source, self, variants, false);
  }

  // A union carrying its own `.to` converts per variant, so rules 3 and 4 have
  // to resolve the target before it's fused into the cases — appending the whole
  // target union instead would re-enter as an ambiguous rule-2 conversion. The
  // union's own refiners ride along on each variant for the same reason: there
  // is no single pre-conversion output val left to attach them to.
  if (toPerCase !== U) {
    const perCase = unionTargetOwns(toPerCase)
      ? variants.map((v) => (unionOutput(v).type === neverTag ? U : toPerCase))
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

  return unionEmit(
    input,
    self,
    unionPlan(unionHoistSentinels(unionAnalyze(unionMask(source, 2, nan), flags, sourceTag, variants, source, nan))),
    toPerCase,
    trustedSelf
  );
};

// Calls each source refiner at most once so its predicate is embedded once and
// every case references the same `e[N]` — `B_embed` is append-only, so a
// per-case call would duplicate it.
const unionRefinerAttacher = (self: Internal): ((mut: Internal) => void) => {
  const cached: (Check[] | undefined)[] = [];
  return (mut: Internal) => {
    for (let i = 0; i < 2; i++) {
      const key = i ? "inputRefiner" : "refiner";
      const source = self[key];
      if (source !== U) {
        const current = mut[key];
        mut[key] = (input: Val) => {
          const shared = cached[i] || (cached[i] = source(input));
          return current === U ? shared : current(input).concat(shared);
        };
      }
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
  const mut = baseSchema(anyOfTag, false, unionDecoder);
  mut.anyOf = anyOf;
  mut.has = has;
  mut.encoder = unionEncoder;
  mut.perVariant = input.s.perVariant;
  // The variants above were mapped from `input.s`'s, so the value is already
  // known to satisfy one of them — a fact the `unknown` below throws away. See
  // `tr` in base.ts: this is the only place allowed to claim it.
  mut.tr = true;
  return B_refine(input, unknown, U, mut);
};

// Appends `.to(target)` to every source variant. A variant already ending in
// `never` stays as it is: it's an explicit rejection, not a path to convert.
export const unionRewriteTo = (input: Val, target: Internal): Val =>
  unionRewrite(input, (variant) =>
    unionOutput(variant).type === neverTag
      ? variant
      : updateOutput<Internal>(variant, (mut) => {
          mut.to = target;
        })
  );


// Whether the union should hand itself to the target untouched — recursive
// schemas and `S.json` decode a union source per variant on their own, and a
// `noValidation` target (`S.assertInput`'s result sentinel) discards the value, so
// converting each member into it would replace every member's check with the
// sentinel's.
const unionTargetOwns = (target: Internal) =>
  target.noValidation === true ||
  (tagFlags[unionOutput(target).type]! & 512) ||
  (target.type === anyOfTag && target.anyOf!.some((v) => tagFlags[v.type]! & 512));

// Applied by the parse loop when a union-typed val meets a different expected
// schema — rules 3 and 4.
export const unionEncoder: Encoder = (input: Val, target: Internal) => {
  if (unionTargetOwns(target)) return input;
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
  // nothing; and a `noValidation` target (S.assertInput's result sentinel), which
  // discards the value entirely.
  if (!(tagFlags[target.type]! & 1) && !target.noValidation) {
    unionCheckPartial(input, source, target, variants, true);
  }
  return variants.map((variant) =>
    unionOutput(variant).type === neverTag ? U : target
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
  let sourceNullish = 0;

  for (let s = 0; s < variants.length; s++) {
    const sourceVariant = variants[s]!;
    const sourceOut = unionOutput(sourceVariant);
    const produces =
      sourceVariant.type !== neverTag && sourceOut.type !== neverTag;
    if (!produces) {
      continue;
    }
    // An arm whose codec link target is the union itself, or a copy sharing
    // its `anyOf` (Option.getOr's default arm), already produces the whole
    // target union: nothing left to append, and it covers every target
    // variant on its own.
    if (sourceOut === target || (target.anyOf !== U && sourceOut.anyOf === target.anyOf)) {
      for (let t = 0; t < targets.length; t++) {
        covered[t] = true;
      }
      continue;
    }
    const sameTyped = targets.filter(
      (targetVariant, t) =>
        targetVariant.type !== neverTag &&
        !unionNeverLink(targetVariant) &&
        unionSameType(sourceOut, targetVariant) &&
        (covered[t] = true)
    );
    sourceNullish |= tagFlags[sourceOut.type]! & (32 | 16);
    if (sameTyped.length === 1) {
      matches[s] = sameTyped[0]!;
    } else if (sameTyped.length > 1) {
      // "Same type" is tag-level, so several target variants can share it. For a
      // structured variant that's too coarse to pick by definition order — every
      // object shape is `object` — so a candidate that *is* this variant's own
      // output takes it as the pass-through rule 4 describes. Otherwise hand the
      // value to all the candidates and let their own dispatch (and fallback)
      // sort it out.
      matches[s] =
        (tagFlags[sourceOut.type]! & (64 | 128 | 8192 | 512 | 256)) &&
        sameTyped.includes(sourceOut)
          ? sourceOut
          : unionFactory(sameTyped);
    }
    if (matches[s] !== U) {
      continue;
    }
    // Nullish bridge: an unmatched null/undefined may take the opposite
    // nullish variant. A same-type match above always wins.
    const opposite = unionOpposite(sourceOut);
    if (opposite !== U) {
      matches[s] = targets.find(
        (candidate) =>
          candidate.type === opposite &&
          !unionNeverLink(candidate) &&
          unionOutput(candidate).type !== neverTag
      );
    }
    if (matches[s] === U) {
      unionUncovered(input, source, target, sourceOut);
    }
  }
  for (let t = 0; t < targets.length; t++) {
    const targetVariant = targets[t]!;
    const opposite = unionOpposite(targetVariant);
    // A nullish target is covered by the opposite nullish source through the
    // bridge, even without a same-type match of its own.
    if (
      targetVariant.type !== neverTag &&
      !unionNeverLink(targetVariant) &&
      !covered[t] &&
      (opposite === U ||
        unionOutput(targetVariant).type === neverTag ||
        !(sourceNullish & tagFlags[opposite]!))
    ) {
      unionUncovered(input, source, target, targetVariant);
    }
  }

  return matches.map((matched, idx) => {
    if (matched === U) return matched;
    const sourceOut = unionOutput(variants[idx]!);
    // Whether the matched target adds nothing to the source variant's output:
    // same type, no transformation, refinement or nested structure left to
    // check, so the type check the dispatch already emits is the whole
    // conversion. Appending it anyway would re-decode the value into a schema
    // it already satisfies — and leave the case's output val describing a
    // `.to` that has already run. A target const narrows the source; only a
    // target that constrains nothing (or exactly the same value) is a
    // pass-through.
    return matched === sourceOut ||
      (unionIsNoop(matched) &&
        matched.refiner === U &&
        matched.inputRefiner === U &&
        matched.noValidation === U &&
        (matched.const === U || unionLiteralEqual(matched.const, sourceOut.const)) &&
        !(tagFlags[matched.type]! & (64 | 128 | 8192 | 512 | 256)) &&
        unionSameType(matched, sourceOut))
      ? U
      : matched;
  });
};

// ── Factory ──────────────────────────────────────────────────────────────────

export const unionFactory = (schemas: Internal[]): Internal => {
  if (!schemas.length) return panic("S.union requires at least one item");
  if (schemas.length === 1) return schemas[0]!;

  const has: Partial<Record<Tag, boolean>> = {};
  const anyOf: Internal[] = [];
  for (let idx = 0; idx < schemas.length; idx++) {
    const schema = schemas[idx]!;
    const nested = unionIsTransparent(schema) ? schema.anyOf! : [schema];
    for (let j = 0; j < nested.length; j++) {
      const member = nested[j]!;
      anyOf.push(member);
      setHas(has, member.type);
    }
  }

  const mut = baseSchema(anyOfTag, false, unionDecoder);
  mut.anyOf = anyOf;
  mut.encoder = unionEncoder;
  mut.has = has;
  return mut;
};
