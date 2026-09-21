import {
  arrayTag,
  type BGlobal,
  type Check,
  compilePath,
  type ErrorDetails,
  conversionSite,
  errorAt,
  errorSite,
  type Flag,
  hasPathDyn,
  immutableEmptyArray,
  inlinedProperty,
  inlinedValueFromString,
  inputExpression,
  type Internal,
  isLiteral,
  stringTag,
  type InvalidInputDetails,
  type Path,
  pathConcat,
  pathEmpty,
  pathExpr,
  pathToText,
  s,
  stringify,
  type SuryErrorRecord,
  toError,
  tagFlags,
  U,
  unknown,
  type Val
} from "./base";

export type Builder = (input: Val) => Val;
export type Encoder = (input: Val, target: Internal) => Val;

// `_var`/`_linkVar`/`_notVarBeforeValidation`/`_notVarAtParent`/`_notVar`
// and `failInvalidType` are top-level consts (not object methods) because
// they're compared/stored by reference (`val.v = _var`, `val.v !== _var`,
// `check.f === failInvalidType`) - a method wrapper would break that
// identity comparison.

export function _var(this: Val): string {
  return this.i;
}

// B_refine links through `prev`; B_scope through `b` so merge does not walk
// the source as `prev` (a scope is a new segment over the same value).
const _linkVar = function (this: Val): string {
  return (this.b || this.prev)!.v();
};

export function _notVarBeforeValidation(this: Val): string {
  const val = this;
  const v = B_varWithoutAllocation(val.g);
  val.cp = `let ${v}=${val.i};`;
  val.i = v;
  val.v = _var;
  return v;
}

export function _notVarAtParent(this: Val): string {
  const val = this;
  const parent = val.p!;
  // A re-readable field access (`parent[key]`). Its decl hoists onto the
  // parent, which outlives this field's own segment - field vals are often
  // materialized late (e.g. completeObjectVal's optional-field check), after
  // their merge code was emitted, so owning it here would drop the decl.
  // If the parent is itself finalized (cached bond after its block closed -
  // #240), re-read inline: the only still-open vals are ancestors whose
  // segments precede the parent's guard, so hoisting there could read
  // `parent[key]` before that guard; inlining defers it to a guarded use.
  if (parent.fz) {
    val.v = _var;
    return val.i;
  }
  const v = B_varWithoutAllocation(val.g);
  B_hoistDecl(parent, `${v}=${val.i}`);
  val.v = _var;
  val.i = v;
  return v;
}

export function _notVar(this: Val): string {
  const val: Val = this;
  // Already emitted (a late materialization after this val's segment was
  // merged - e.g. a fused `.to` stage reading a previous stage's transformed
  // output): owning a fresh decl here would drop it (the phantom-var fusion
  // bug). Re-read the inline expression instead. Like `_notVarAtParent`'s
  // finalized guard, but that sibling's inline is always an atomic
  // `parent[key]`, whereas a transform val's inline can be compound (e.g.
  // `""+x`), so parenthesize it to stay correct under any operator a consumer
  // wraps it in (`+(""+x)`, not `+""+x`). Mutating `inline` (not just
  // returning the wrap) keeps a second `.var()` - now routed through `_var` -
  // consistent. Re-reading is sound only because the inlines that reach here
  // are idempotent (`""+x`, `+x`): side-effecting/allocating coercions
  // (`BigInt(...)`, `new Date(...)`, `new Array(...)`) are var-materialized by
  // an eager check before they can finalize, and their referenced vars live
  // in an enclosing segment (not a closed loop/`.then` scope).
  if (val.fz) {
    val.v = _var;
    val.i = `(${val.i})`;
    return val.i;
  }
  const v = B_varWithoutAllocation(val.g);
  if (val.prev !== U) {
    // Own the decl in codeFromPrev: a non-empty codeFromPrev is
    // non-hoistable in `merge`, so a union discriminant reading this var
    // can't be lifted above its `let` (the str->to(option(int)) bug class).
    if (val.i === "") {
      // No inline value yet (assigned by code that already reads this val):
      // declare ahead of the existing producing code.
      val.cp = `let ${v};` + val.cp;
    } else {
      // Declare-and-assign after it; `v` is fresh, so nothing emitted reads it.
      val.cp += `let ${v}=${val.i};`;
    }
  } else {
    // No prev to anchor to; hoist onto the val itself (its own segment
    // outlives the materialization).
    B_hoistDecl(val, val.i === "" ? v : `${v}=${val.i}`);
  }
  val.v = _var;
  val.i = v;
  return v;
}

export const operationArgVar = "i";

// Pass this as `fail` on every check that wants "expected X, received Y"
// error semantics. Stable reference → adjacent checks fuse.
// A format's range check is a type check for that format, so it answers to
// `errorMessage.format` rather than `errorMessage.type` - keeping it the same
// Check the plain type-narrow uses is what lets the two fuse into one
// condition instead of two throws.
export const failInvalidType = (input: Val): (value: unknown) => ErrorDetails => {
  const em = input.e.errorMessage;
  return B_invalidInputBuilder(
    U,
    U,
    em && (input.e.format !== U && em.format !== U ? em.format : em.type !== U ? em.type : em._)
  )(input);
}

// Bumps the raise counter: an embedded value is reached through `e[N]`, and
// anything callable behind that accessor may raise - a fail helper, a user
// transform, `S.json`'s validator. Counting every embed over-reports for the
// inert ones (a symbol literal compared with `===`), which is the safe
// direction: union codegen wraps a case in a `try` it turns out not to need,
// rather than dropping the fallback a raise needed.
export const B_embed = (b: Val, value: unknown): string => (b.g.t++, B_embedPure(b, value));

// B_embed for a value generated code can't raise through - a helper that
// never throws. Skipping the raise counter keeps union codegen from wrapping
// the case in a `try` it doesn't need, and keeps loop bodies recognizable as
// throw-free so array/dict can drop an empty item loop.
export const B_embedPure = (b: Val, value: unknown): string => `e[${b.g.e.push(value) - 1}]`;

export const B_inlineConst = (b: Val, schema: Internal): string => {
  const tagFlag = tagFlags[schema.type]!, const_ = schema.const;
  return (tagFlag & 16)
    ? "void 0"
    : (tagFlag & 2)
      ? inlinedValueFromString(const_ as string)
      : (tagFlag & 1024)
        ? (const_ as unknown as string) + "n"
        : (tagFlag & (16384 | 4096 | 8192))
          // Symbol/function/instance consts are compared, never called.
          ? B_embedPure(b, schema.const)
          : const_ as unknown as string;
};

export const B_varWithoutAllocation = (g: BGlobal): string => `v${++g.v}`;

// Append a `let` declaration to a still-open owner val, emitted after the
// owner's checks in `merge`. The owner is the materialized val's immediate
// context (its `prev`, its `parent` for a field read, or itself); since the
// decl lands at the owner's segment end - after the owner's guard, before
// its dependent code - that immediate owner already dominates and outlives
// every use, so no separate scope-tree is needed. The owner must be
// unfinalized; `_notVarAtParent` guards this explicitly.
export const B_hoistDecl = (owner: Val, decl: string): void => {
  owner.hd += (owner.hd && ",") + decl;
}

export const B_operationArg = (
  schema: Internal,
  expected: Internal,
  flag: Flag,
  defs: Record<string, Internal> | undefined
): Val => {
  // Every Val literal in the codegen path lists the same fields in the same
  // order (undefined where unset) so V8 gives them all ONE hidden class -
  // monomorphic property reads in the hot merge/parse loops and faster
  // allocation. Keep this canonical order in sync across all Val creators.
  return {
    b: U,
    p: U,
    v: _var,
    i: operationArgVar,
    s: schema,
    io: U,
    e: expected,
    prev: U,
    f: 0,
    d: U,
    fv: U,
    cp: "",
    hd: "",
    fz: U,
    vc: U,
    u: U,
    t: U,
    path: pathEmpty,
    g: {
      d: defs,
      o: flag,
      e: [],
      v: -1,
      t: 0,
    },
    o: U,
  };
}

// Every raise the compiler makes, whether the operation is being built or run.
// The record carries no stack; parse.ts attaches one at whichever boundary the
// failure escapes through.
export const B_throw = (errorDetails: ErrorDetails): never => {
  throw toError(errorDetails);
}

export const B_unsupportedDecode = (b: Val, from: Internal, target: Internal): never =>
  B_throw({
    code: "unsupported_decode",
    from,
    to: target,
    reason: `Can't decode ${inputExpression(from)} -> ${inputExpression(target)}. Define custom codec with S.to`,
    path: compilePath(b.path),
  });

// Snapshot a static path into the embed; a PathDyn is the next generated
// argument, evaluated only on throw.
export const B_pathArg = (b: Val): string =>
  hasPathDyn(b.path) ? "," + pathExpr(b.path) : "";

export const B_pathSnap = (b: Val): Path | undefined =>
  hasPathDyn(b.path) ? U : (b.path as Path);

export const B_failWithArg = <TArg>(
  b: Val,
  fn: (arg: TArg, path?: Path) => ErrorDetails,
  arg: string,
): string =>
  `${B_embed(b, (a: TArg, p?: Path) => {
    B_throw(fn(a, p));
  })}(${arg}${B_pathArg(b)})`;

// Record a raise that reaches generated code without an embed behind it - the
// bare `throw` a loop wrapper re-raises a nested error with. Union codegen
// decides whether a case needs a `try` by bracketing an emission and reading
// `g.t`, so a raise counted by neither this nor `B_embed` is a case that
// silently loses its fallback.
export const B_markThrow = (b: Val): void => {
  b.g.t++;
}

// A coder's or refiner's own throw as `invalid_conversion`. Split from the
// Sury-cause branch below so an operation tail (every bundle) carries only this
// half. `(input, to) => (cause, path) => error`, like every other fail builder:
// the two schemas are the site's, what was thrown is the failure's.
const B_foreignFail = (
  input: Val,
  to: Internal
): ((cause: unknown, path: Path) => ErrorDetails) => {
  let site: object;
  return (cause, path) => {
    const error = Object.create((site ??= conversionSite(input.s, to))) as SuryErrorRecord;
    error.code = "invalid_conversion";
    error.path = path;
    error.cause = cause;
    error.reason =
      cause instanceof Error ? ("" + cause).replace(/^Error: /, "") : stringify(cause);
    return error as unknown as ErrorDetails;
  };
};

export const B_conversionFail = (
  input: Val,
  to: Internal
): ((cause: unknown, path?: Path) => ErrorDetails) => {
  const foreign = B_foreignFail(input, to);
  return (cause, path) => {
    const p = path ?? compilePath(input.path);
    if (cause && (cause as { s?: symbol }).s === s) {
      const error = cause as unknown as SuryErrorRecord;

      // A SuryError thrown by user code carries only the path it named, so the
      // path it was reached through is prepended here. Nothing arrives
      // pre-prepended any more - that was effectCtx, which is gone.
      //
      // Copied rather than mutated: user code may throw one retained instance
      // more than once, and prepending onto the instance makes the second parse
      // report `a.a`. Nothing to prepend means nothing to copy - `B_throw`
      // reparents whichever of the two it gets, and reparenting the instance to
      // the prototype it already has changes nothing, its stack included.
      //
      // Own descriptors onto the SAME prototype, not a spread: what the failing
      // check settled lives on its site prototype (base.ts, `errorSite`), and a
      // spread copies own properties only - the copy would come out reading
      // `Expected undefined`. Descriptors rather than `Object.assign` so an own
      // `reason` is carried as the data property it is instead of being pushed
      // through a setter.
      if (!p.length) return error as unknown as ErrorDetails;
      const copy = Object.create(
        Object.getPrototypeOf(error) as object,
        Object.getOwnPropertyDescriptors(error)
      ) as SuryErrorRecord;
      copy.path = pathConcat(p, error.path);
      return copy as unknown as ErrorDetails;
    }
    return foreign(cause, p);
  };
};

// The error an operation answers with when it answers rather than throws
// (Result, boolean, Standard Schema): a Sury failure as it is, anything else -
// a getter, a coder or refiner hit on a value it was never written for -
// wrapped, so every outcome shape holds a SuryError and a consumer never has to
// tell a validation failure from an exception by inspecting it. Not the
// throwing outcomes: there the exception is the answer, and it stays raw.
// `to` is the chain's last schema, the one the caller named: the operation
// arg's own `e` is the private chain-head copy `compileChain` builds.
export const B_errorOf = (input: Val): ((e: unknown) => SuryErrorRecord) => {
  let to = input.e;
  while (to.to) to = to.to;
  const foreign = B_foreignFail(input, to);
  const path = compilePath(input.path);
  return (e) =>
    e && (e as { s?: symbol }).s === s
      ? (e as SuryErrorRecord)
      : toError(foreign(e, path));
};

export const B_embedErrorOf = (input: Val): string => B_embedPure(input, B_errorOf(input));

// Drop-in `check.fail` builder for InvalidInput failures. The `(~input) =>
// (value, path) => error` shape is what makes the site prototype possible: the
// middle call happens once, while the check is being compiled, so everything
// the failures of this check share is settled there - the static path with it,
// concatenated once rather than per failure. It also snapshots rather than
// capturing the val, which would pin the whole val chain into the embed array.
//
// A path the generated code has to compute - a loop index, a for-in key -
// arrives as the second argument instead, and only on throw.
export const B_invalidInputBuilder = (
  expected?: Internal,
  extraPath: Path = pathEmpty,
  reasonOverride?: string
): (input: Val) => (value: unknown, path?: Path) => ErrorDetails => (input) => {
  const snap = B_pathSnap(input);
  const staticPath = snap !== U ? pathConcat(snap, extraPath) : U;
  // Built on the first failure, not here: a compile emits a check whether or
  // not any value ever fails it, and most never do.
  let site: object;
  return (value, path) =>
    errorAt(
      (site ??= errorSite(expected ?? input.e, (input.prev || input).s, reasonOverride)),
      staticPath ?? pathConcat(path ?? pathEmpty, extraPath),
      value
    );
};

export const B_failWithErrorMessage = (
  key: string,
  defaultMessage?: string
): (input: Val) => (value: unknown, path?: Path) => ErrorDetails => (input) => {
  const em = input.e.errorMessage as Record<string, string | undefined> | undefined;
  const m = em?.[key] ?? em?.["_"] ?? defaultMessage;
  return m !== U ? B_invalidInputBuilder(U, U, m)(input) : failInvalidType(input);
};

// Inline variant: emits the throw expression directly. Used by decoders
// that splice errors into custom JS (e.g. `catch(_){${embedInvalidInput}}`),
// not via the `check` pipeline.
export const B_embedInvalidInput = (input: Val, expected: Internal = input.e): string =>
  B_failWithArg(input, B_invalidInputBuilder(expected)(input), input.v());

// Caller must verify `val.vc` is truthy and `val.expected.noValidation !==
// true` first - the `!` unwrap below is unchecked. `inputVar` is usually
// `val.prev.var()`.
const B_emitChecks = (val: Val, inputVar: string): string => {
  const checks = val.vc!;
  let out = "", i = 0, len = checks.length;
  while (i < len) {
    const head = checks[i]!, fail = head.f;
    let cond = head.c(inputVar);
    i++;
    // Extend the fused cond while the next check shares this `fail`.
    while (i < len && checks[i]!.f === fail) {
      cond += "&&" + checks[i]!.c(inputVar);
      i++;
    }
    out += `${cond}||${B_failWithArg(val, fail(val), inputVar)};`;
  }
  return out;
}

// A hoisted type-narrow kept in both forms: `c` routes the value to the next
// union case (dispatch), and re-emitting it against `v` rejects the case from
// inside a `try` (fallback). Only `c` and the two strings it needs are captured -
// most cases never emit the rejecting form, so its closure and embed slot are
// built on demand (see `unionRejectCond`).
export type Hoist = {
  v: Val;
  i: string;
  c: string;
}
export type HoistCond = { c: string; h: Hoist[] }

// Walks the val.prev chain and assembles generated code: every
// non-`noValidation` check is emitted inline. With `~out` (union codegen),
// type-narrow checks (fail === failInvalidType) lift into it as a dispatch
// discriminant instead of being emitted; constraint refines still emit inline so
// their case-specific error message survives.
export const B_merge = (val: Val, out?: HoistCond): string => {
  let current: Val | undefined = val, code = "";

  while (current !== U) {
    const val: Val = current;
    current = val.prev;
    let currentCode = "";

    if (val.vc) {
      // Type-narrows hoist only when they can't strand a decl the lifted
      // check reads: a transforming val is safe iff prev is non-transforming
      // (stable input var) and this val has no codeFromPrev of its own -
      // else the lifted check runs before that producer (the
      // str->to(option(int)) "v0 is not defined" bug class).
      if (out && (!val.t || !val.prev!.t && val.cp === "")) {
        const inputVar = (current || val).v();
        const checks = val.vc;
        let hoisted = "";
        for (let i = 0; i < checks.length; i++) {
          const check = checks[i]!;
          const condCode = check.c(inputVar);
          if (check.f === failInvalidType) {
            hoisted = hoisted ? `${hoisted}&&${condCode}` : condCode;
          } else if (val.e.noValidation !== true) {
            // `noValidation` is intentionally bypassed for the hoisted part -
            // the cond routes between cases, it doesn't reject, so suppressing
            // it would break dispatch.
            currentCode += `${condCode}||${B_failWithArg(val, check.f(val), inputVar)};`;
          }
        }
        if (hoisted) {
          out.c = out.c ? `${hoisted}&&${out.c}` : hoisted;
          out.h.unshift({ v: val, i: inputVar, c: hoisted });
        }
      } else if (val.e.noValidation !== true) {
        // No prev means this is the operation argument itself, and its own var
        // already holds the value the checks are about.
        currentCode = B_emitChecks(val, (current || val).v());
      }
    }

    // Hoisted decls land after this val's checks (the old varsAllocation
    // slot).
    if (val.hd) currentCode += `let ${val.hd};`;

    // Now emitted: a later cached-bond materialization can't hoist onto it.
    val.fz = true;
    code = val.cp + currentCode + code;
  }

  return code;
}

// Rebinds `val.v` so the next call to it also stashes the resolved var name
// (and switches `nextVal.v` to the plain `_var` reader) onto `nextVal` -
// links a derived val's var resolution to its source without eagerly
// materializing a var. Shared by every "derive a val from a val" builder.
const B_linkVar = (val: Val, nextVal: Val): void => {
  const get = val.v.bind(val);
  val.v = () => (nextVal.i = get(), nextVal.v = _var, nextVal.i);
}

export const B_next = (prev: Val, initial: string, schema: Internal, expected: Internal = prev.e): Val => {
  // No `d`: this val is a *new* value, so `prev`'s field vals don't describe
  // it. Inheriting them let a reader of a transformed object read the fields
  // of the value that went in - a flattened member's codec ran and its result
  // was then discarded field by field (#368's FIXME). `valGet` re-reads them
  // off this value instead. B_scope, which names the *same* value, does share
  // `d` - that aliasing is the correct one.
  // Canonical Val field order (see B_operationArg).
  return {
    b: U,
    p: U,
    v: _notVar,
    i: initial,
    s: schema,
    io: U,
    e: expected,
    prev,
    f: 0,
    d: U,
    fv: U,
    cp: "",
    hd: "",
    fz: U,
    vc: U,
    u: U,
    t: true,
    path: prev.path,
    g: prev.g,
    o: U,
  };
}

// Pass a non-empty `~checks` or omit it. Never pass `~checks=[]` -
// that would break the val.checks "absent iff no checks" invariant.
export const B_refine = (val: Val, schema: Internal = val.s, checks?: Check[], expected: Internal = val.e): Val => {
  const shouldLink = val.v !== _var;
  // Canonical Val field order (see B_operationArg).
  const nextVal: Val = {
    b: U,
    p: U,
    v: shouldLink ? _linkVar : _var,
    i: val.i,
    s: schema,
    io: U,
    e: expected,
    prev: val,
    f: val.f,
    d: val.d,
    fv: U,
    cp: "",
    hd: "",
    fz: U,
    vc: checks,
    u: U,
    t: val.t,
    path: val.path,
    g: val.g,
    o: U,
  };
  if (shouldLink) B_linkVar(val, nextVal);
  return nextVal;
}

// Lazy-allocate helper for mutating an existing val (as opposed to
// building a local array and passing it through `refine`).
export const B_pushCheck = (val: Val, check: Check): void => {
  (val.vc ??= []).push(check);
}

// Applies both refiners. Output checks wrap `val` via refine; input checks push
// onto `valInput.vc`, which emits ahead of the decoder body - they have to read
// what the decoder was *handed*. A schema that narrows leaves nothing else to
// read it from: a union assigns its result over the operation argument, so an
// `allOf` refinement placed after it looks for keys the object arm just
// stripped. Sets isOutput on the result.
//
// The parse loop applies refiners itself only for primitive decoders, so every
// decoder that sets isOutput - object, array, tuple, union, recursive - has to
// call this. Not calling it silently drops the user's S.refine.
export const B_markOutput = (val: Val, valInput: Val): Val => {
  let outC: Check[] | undefined;
  const ir = valInput.e.inputRefiner;
  if (ir) {
    const c = ir(valInput);
    if (c.length) (valInput.vc ??= []).push(...c);
  }
  const rf = val.e.refiner;
  if (rf) {
    const c = rf(val);
    if (c.length) outC = c;
  }
  // An async result is a Promise, so the output checks run where the value
  // is: inside a `.then`, the way the parse loop continues an async val.
  if (outC && (val.f & 1)) {
    const v = val.v();
    val.i = `${v}.then(${v}=>{${B_merge(B_refine(B_scope(val), U, outC))}return ${v}})`;
    val.v = _notVar;
  } else if (outC) val = B_refine(val, U, outC);
  val.io = true;
  return val;
}

// Used in union codegen: splice a literal child's checks into the parent
// as dispatch discriminants. Each cond's `inputVar` is rewritten to
// `parent[key]`; `fail` stays shared so lifted checks fuse with the
// parent's own type guard. No-op if the child has no checks.
export const B_hoistChildChecks = (parent: Val, child: Val, key: string): void => {
  const checks = child.vc;
  if (checks) {
    const accessor = inlinedProperty("", key, parent.s.type === arrayTag);
    for (let i = 0; i < checks.length; i++) {
      const check = checks[i]!;
      B_pushCheck(parent, { c: (v) => check.c(v + accessor), f: check.f });
    }
    child.vc = U;
  }
}

export const B_dynamicScope = (from: Val, locationVar: string): Val => {
  // `additionalItems` doubles as the value schema for a dict-shaped val.
  // Extract it via a real pattern match: a non-`Schema` mode (`Strip`/`Strict`
  // on a fixed-property object) must never be cast to a schema - that string
  // reaching `isLiteral` is the `'const' in "strip"` crash. Callers only pass
  // dict sources; the `unknown` fallback keeps a misuse safe instead of crashing.
  const schemaAdditionalItems = from.s.additionalItems;
  const expectedAdditionalItems = from.e.additionalItems;
  // Canonical Val field order (see B_operationArg).
  return {
    b: U,
    p: from,
    v: _notVarBeforeValidation,
    i: `${from.v()}[${locationVar}]`,
    s:
      schemaAdditionalItems !== U && typeof schemaAdditionalItems !== "string"
        ? schemaAdditionalItems
        : unknown,
    io: U,
    e:
      expectedAdditionalItems !== U && typeof expectedAdditionalItems !== "string"
        ? expectedAdditionalItems
        : unknown,
    prev: U,
    f: from.f,
    d: U,
    fv: U,
    cp: "",
    hd: "",
    fz: U,
    vc: U,
    u: U,
    t: U,
    path: pathConcat(from.path, [{ e: locationVar }]),
    g: from.g,
    o: U,
  };
}

export const B_nextConst = (from: Val, schema: Internal, expected?: Internal): Val =>
  B_next(from, B_inlineConst(from, schema), schema, expected);

// The expression to read a val by when it will be read more than once - the
// conversion reads it, and whatever the conversion's own result is spliced into
// may read that. `v()` hands back the var that already stands for the value
// wherever one does, and hoists one only where the source is an expression
// nothing has named yet, which is the case a second read would repeat.
export const B_readOnce = (input: Val): string => input.v();
// Fresh var, already named: `v()` must not allocate a second copy of it.
export const B_nextVar = (input: Val, schema: Internal = input.e, expected: Internal = schema): Val => {
  const output = B_next(input, B_varWithoutAllocation(input.g), schema, expected);
  output.v = _var;
  return output;
};

// Same as B_nextVar but for the case that re-uses an input's var name as the
// output storage (field default `or`, missing-key encoder for optional fields).
// Also marks the val as the output side of the current io step.
export const B_nextVarOutput = (input: Val, initial: string, schema: Internal, expected: Internal = schema): Val => {
  const output = B_next(input, initial, schema, expected);
  output.v = _var;
  output.io = true;
  return output;
};

// A conversion's result, held in a var. The splice that reads it may read it
// twice (jsonString's escape-free form does), and unlike a property path this is
// a fresh pass over the whole value - so it is computed once, the way
// B_conversion computes a custom coder's result once.
export const B_computed = (
  input: Val,
  code: string,
  schema: Internal,
  failure?: string,
): Val => {
  const output = B_nextVar(input, schema, input.e);
  // With a `failure`, the whole `B_conversion` shape: a computation that can
  // throw on a value the operation trusted rather than checked reports it as a
  // failed conversion instead of escaping as whatever the platform raised.
  output.cp =
    failure === U
      ? `let ${output.i}=${code};`
      : `let ${output.i};try{${output.i}=${code}}catch(x){${failure}}`;
  return output;
};

export const B_asyncVal = (from: Val, initial: string): Val => {
  const v = B_next(from, initial, from.s);
  v.f = 1; // 1
  return v;
}

// A val the rest of the pipeline continues from inside a `.then`. Async is
// declared, not discovered: a sync operation that reaches one is rejected here,
// where it is written, rather than returning a promise its caller never asked
// for.
export const B_markAsync = (input: Val, output: Val): void => {
  if (!(input.g.o & 1)) { // 1
    B_throw({
      code: "invalid_operation",
      path: pathEmpty,
      reason: "Invalid async during sync operation",
    });
  }
  output.f |= 1; // 1
}

export const B_addObjectField = (objectVal: Val, location: string, val: Val): void => {
  if (objectVal.s.type === arrayTag) objectVal.s.items!.push(val.s);
  else {
    if (!val.o) objectVal.s.required!.push(location);
    objectVal.s.properties![location] = val.s;
  }

  // Async field values must be reachable as a plain identifier so
  // the accumulator in completeObjectVal can use val.inline as a
  // destructuring/reference target. For e.g. array-of-async, the
  // asyncVal's inline is a Promise.all(...) expression, not a var.
  // This has to happen before val->merge, which finalizes the prev
  // chain and locks the emitted code.
  if (val.f & 1) val.v(); // 1
  objectVal.cp += B_merge(val);
  objectVal.d![location] = val;
}

export const B_addKey = (objVal: Val, key: string, value: Val): string =>
  `${objVal.v()}[${key}]=${value.i}`;

export const B_scope = (val: Val): Val => {
  const shouldLink = val.v !== _var;

  // Canonical Val field order (see B_operationArg).
  const nextVal: Val = {
    b: val,
    p: U,
    v: shouldLink ? _linkVar : _var,
    i: val.i,
    s: val.s,
    io: val.io,
    e: val.e,
    prev: U,
    f: 0,
    // Shared, not dropped as in B_next: a scope names the same value, so the
    // same field vals describe it.
    d: val.d,
    fv: U,
    cp: "",
    hd: "",
    fz: U,
    vc: U,
    u: false,
    t: false,
    path: val.path,
    g: val.g,
    o: U,
  };
  if (shouldLink) B_linkVar(val, nextVal);
  return nextVal;
}

// Compiles one custom coder of `S.to` into the chain. The two seams differ in
// exactly one thing: what the coder's result claims to already be, which is
// what decides how much of the target the parse loop still runs over it.
//
//  - `junction` (the JS `{decode, encode}` surface): the result claims
//    `unknown`, so the loop owes the target a full decode. A coder returning
//    the wrong thing is caught there.
//  - otherwise (the ReScript adapter's decodeToOutput / encodeFromOutput):
//    the result claims the target itself, so the loop only runs what a typed
//    decode would, the same deal `S.decodeOrThrow` gives a caller who declares the
//    input's schema. The ReScript compiler has already checked the coder's
//    signature, so the skipped work is provably redundant.
//
// A literal target is the exception, and `compileDecoder` states the same
// rule for its typed input: a type says "string", never "the string \"a\"",
// so a const is checked whatever the value claims to be.
//
// Inside a union case the sync form rethrows foreign exceptions raw (the
// union owns exception classification) while still wrapping Sury failures
// with the reached path; the async form leaves the promise bare, since the
// case's own await/catch classifies rejections.
export const B_conversion = (
  fn: (value: unknown) => unknown,
  isAsync?: boolean,
  junction?: boolean,
): Builder => {
  return (input: Val): Val => {
    const target = input.e.to!;
    const output = B_nextVar(
      input,
      junction || isLiteral(target) ? unknown : target,
      target,
    );
    if (isAsync) B_markAsync(input, output);
    const embeddedFn = B_embed(input, fn);
    const inputValue = input.vc ? input.v() : input.i;
    const unionContext = input.g.o & 4; // 4
    if (unionContext && isAsync) {
      output.cp = `let ${output.i}=${embeddedFn}(${inputValue});`;
      return output;
    }
    // Whatever the coder throws - a `SuryError` it raised on purpose or a
    // TypeError it hit on a value it was never written for - is that
    // conversion failing, so in a union it is what hands the value to the
    // next case rather than aborting the operation (#347); a refiner's throw
    // is wrapped the same way (modifiers.ts `refine`). The foreign errors that
    // do escape a union are a getter's, which never enter this try.
    const failure = B_failWithArg(output, B_conversionFail(input, target), `x`);
    output.cp = `let ${output.i};try{${output.i}=${embeddedFn}(${inputValue})${
      isAsync ? `.catch(x=>${failure})` : ""
    }}catch(x){${failure}}`;
    // A val whose result the target's own refiners can attach to. `val.vc`
    // checks emit at the *pre-transform* slot (`prev.v()` in B_merge), so
    // leaving them on the coder's own val would validate what went into the
    // coder instead of what came out - `S.uuid->S.to(userSchema, ~custom=…)`
    // ran the uuid pattern over the user object. The junction seam never
    // hits this: its `unknown` source makes the loop compile the target's
    // decoder, which supplies a val of its own. The trusted seam claims the
    // target outright, so it has to supply one.
    return output.s === unknown ? output : B_refine(output);
  };
};

// The "never" codec slot. The union planner compares against this reference
// to find a direction a variant can't take: such a variant accepts nothing
// and yields to its siblings, while standalone it rejects the operation here,
// at creation.
export const B_neverSlot: Builder = (input: Val) =>
  B_invalidOperation(
    input,
    `Nothing decodes ${inputExpression(input.e)} -> ${inputExpression(input.e.to!)}. It is marked with S.never`,
  );

// The node a link's content reading comes from: the schema, or the arm that
// carries one where the schema is a union - which has neither a kind nor
// `.to` of its own, though linking a carrier to `S.optional(S.jsonString)` puts
// the same two readings on the table as linking it to `S.jsonString`.
export const B_contentNode = (schema: Internal): Internal =>
  (!(schema.flags! & 3) && schema.anyOf?.find((arm) => arm.flags! & 3)) || schema;

// Whether two payloads are of different kinds, which is what puts two readings
// of a link on the table - store the source's value in the target, or open the
// source and hand its payload over. Which applies is the reading bit on the target
// (CONTENT_CODEC_SPEC.md rules 1 to 3, all written down as the link is made);
// neither is rule 4, asked below. Both alphabets are one kind, and `S.json`
// and `S.jsonString` another.
export const B_contentDiffers = (from: Internal, to: Internal): boolean =>
  !!((from.flags! & 3) && (to.flags! & 3) && ((from.flags! ^ to.flags!) & 3));

// A plain string: the one schema that is both a JSON value and text
// (CONTENT_CODEC_SPEC.md), so a link from it into a JSON text format has the
// two readings a bytes carrier's has. A format (`S.email`) can't spell a
// document and a literal is a value, so each stores; a carrier's opened text
// and a union narrow carry the kind bits and are already known to be text.
export const B_isText = (schema: Internal): boolean =>
  schema.type === stringTag && !(schema.flags! & 3) && schema.format === U && !isLiteral(schema);

// CONTENT_CODEC_SPEC.md rule 4, asked while compiling by the schemas that
// declare a payload - `json`, `jsonString`, `base64`, `uint8Array`, `file` -
// and by a union carrying its `.to` into one. Two payload declarations of
// different kinds and nothing settling which reading applies: between two
// renderings the caller picks with a slot, and where a slot has nowhere to go
// - a union on either side, or `S.json`, the document itself with no opened
// form - the pair is undecodable as written.
//
// Asked here rather than by `S.to`, which is what makes the chained spelling
// legal: `S.file.with(S.to, S.jsonString).with(S.to, S.array(x))` grows the
// `.to` that settles it only on the second call, and a link-time check rejects
// a pipeline the compiler can see is fine. Inlined at each caller rather than
// wrapped around their decoders: a wrapper applied at module scope makes every
// operation reach the payload schemas, and `parseOrThrow` grew 10,988 gz.
//
// `from` is the node that authored the link into `to`, which is not `input.s`:
// a union case parses its arm from the type narrow, and a bytes read from text
// types its result as the format singleton, so `input.s` there is a schema with
// no `.to` at all - and the question would go unasked, leaving rule 2 to apply
// in silence. The `.to` step of the parse loop refines from the node it just
// finished, so `prev.e` is that node wherever the loop reached `to`; a union
// names itself, since its own decoder is what splits the link per arm.
//
// Not `@__NO_SIDE_EFFECTS__`: the call is the effect, and a bundler honouring
// the annotation would drop the statement as an unused pure call.
export const B_rejectUnsettled = (input: Val, to: Internal, from = input.prev && input.prev.e): void => {
  if (
    from &&
    from.to === to &&
    !(to.flags! & 12) &&
    B_contentDiffers(B_contentNode(from), B_contentNode(to))
  ) {
    !(from.flags! & 16) && !(to.flags! & 16) && B_contentNode(from) === from && B_contentNode(to) === to
      ? B_invalidOperation(
          input,
          `Ambiguous ${inputExpression(from)} -> ${inputExpression(to)}. Should the bytes be packed or unpacked? Choose with S.to and "pack" or "unpack"`,
        )
      : B_unsupportedDecode(input, from, to);
  }
};

export const B_invalidOperation = (val: Val, description: string): never =>
  B_throw({ code: "invalid_operation", reason: description, path: compilePath(val.path) });

const B_mergeWithCatch = (val: Val, catchFn: (errorVar: string) => string): string => {
  const valCode = B_merge(val);
  const errorVar = B_varWithoutAllocation(val.g);
  B_markThrow(val);
  const catchCode = `${catchFn(errorVar)};throw ${errorVar}`;
  if (val.f & 1) val.i = `${val.i}.catch(${errorVar}=>{${catchCode}})`;
  return `try{${valCode}}catch(${errorVar}){${catchCode}}`;
};

// Opaque embed (recursive self-call, S.json's any-value walk): a compiled
// function that does not take a path. Failures it throws are rooted at its
// own `[]`, so a nested use still prepends in catch. Inlined item parsers
// thread the path to the fail helper instead.
export const B_mergeWithPathPrepend = (val: Val, parent: Val): string =>
  !parent.path.length
    ? B_merge(val)
    : B_mergeWithCatch(
        val,
        (errorVar) => `${errorVar}.path=${pathExpr(parent.path, `...${errorVar}.path`)}`,
      );

export const noopOperation = (i: unknown): unknown => i;
(noopOperation as unknown as Record<string, unknown>)["embedded"] = immutableEmptyArray;
