import {
  arrayTag,
  type BGlobal,
  type Check,
  type ErrorDetails,
  type Flag,
  flagAsync,
  flagNone,
  flagUnionTransformContext,
  flagUnsafeHas,
  immutableEmptyArray,
  inlinedValueFromString,
  inputExpression,
  type Internal,
  type InvalidInputDetails,
  type Path,
  pathConcat,
  pathEmpty,
  pathFromInlinedLocation,
  s,
  stringify,
  SuryError,
  type SuryErrorRecord,
  tagFlagBigint,
  tagFlagFunction,
  tagFlagInstance,
  tagFlags,
  tagFlagString,
  tagFlagSymbol,
  tagFlagUndefined,
  U,
  unknown,
  type Val,
  valFlagAsync,
  valFlagNone,
} from "./base";

export type Builder = (input: Val) => Val;
export type Encoder = (input: Val, target: Internal) => Val;

// `_var`/`_bondVar`/`_prevVar`/`_notVarBeforeValidation`/`_notVarAtParent`/
// `_notVar` and `failInvalidType` are top-level consts (not object methods)
// because they're compared/stored by reference (`val.v = _var`,
// `val.v !== _var`, `check.f === failInvalidType`) — a method wrapper would
// break that identity comparison.

export function _var(this: Val): string {
  return this.i;
}

function _bondVar(this: Val): string {
  const val = this;
  const bond = val.b!;
  return bond.v();
}

function _prevVar(this: Val): string {
  const val = this;
  const prev = val.prev!;
  return prev.v();
}

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
  // parent, which outlives this field's own segment — field vals are often
  // materialized late (e.g. completeObjectVal's optional-field check), after
  // their merge code was emitted, so owning it here would drop the decl.
  // If the parent is itself finalized (cached bond after its block closed —
  // #240), re-read inline: the only still-open vals are ancestors whose
  // segments precede the parent's guard, so hoisting there could read
  // `parent[key]` before that guard; inlining defers it to a guarded use.
  if (parent.fz) {
    val.v = _var;
    return val.i;
  } else {
    const v = B_varWithoutAllocation(val.g);
    B_hoistDecl(parent, `${v}=${val.i}`);
    val.v = _var;
    val.i = v;
    return v;
  }
}

export function _notVar(this: Val): string {
  const val: Val = this;
  // Already emitted (a late materialization after this val's segment was
  // merged — e.g. a fused `.to` stage reading a previous stage's transformed
  // output): owning a fresh decl here would drop it (the phantom-var fusion
  // bug). Re-read the inline expression instead. Like `_notVarAtParent`'s
  // finalized guard, but that sibling's inline is always an atomic
  // `parent[key]`, whereas a transform val's inline can be compound (e.g.
  // `""+x`), so parenthesize it to stay correct under any operator a consumer
  // wraps it in (`+(""+x)`, not `+""+x`). Mutating `inline` (not just
  // returning the wrap) keeps a second `.var()` — now routed through `_var` —
  // consistent. Re-reading is sound only because the inlines that reach here
  // are idempotent (`""+x`, `+x`): side-effecting/allocating coercions
  // (`BigInt(...)`, `new Date(...)`, `new Array(...)`) are var-materialized by
  // an eager check before they can finalize, and their referenced vars live
  // in an enclosing segment (not a closed loop/`.then` scope).
  if (val.fz) {
    val.v = _var;
    val.i = `(${val.i})`;
    return val.i;
  } else {
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
        val.cp = val.cp + `let ${v}=${val.i};`;
      }
    } else {
      // No prev to anchor to; hoist onto the val itself (its own segment
      // outlives the materialization).
      if (val.i === "") {
        B_hoistDecl(val, v);
      } else {
        B_hoistDecl(val, `${v}=${val.i}`);
      }
    }
    val.v = _var;
    val.i = v;
    return v;
  }
}

export const operationArgVar = "i";

// Pass this as `fail` on every check that wants "expected X, received Y"
// error semantics. Stable reference → adjacent checks fuse.
// A format's range check is a type check for that format, so it answers to
// `errorMessage.format` rather than `errorMessage.type` — keeping it the same
// Check the plain type-narrow uses is what lets the two fuse into one
// condition instead of two throws.
export const failInvalidType = (input: Val): (value: unknown) => ErrorDetails => {
  const expected = input.e;
  const em = expected.errorMessage;
  const override =
    em !== U
      ? expected.format !== U && em.format !== U
        ? em.format
        : em.type !== U
          ? em.type
          : em._
      : U;
  return B_invalidInputBuilder(U, U, override)(input);
}

// Bumps the raise counter: an embedded value is reached through `e[N]`, and
// anything callable behind that accessor may raise — a fail helper, a user
// transform, `S.json`'s validator. Counting every embed over-reports for the
// inert ones (a symbol literal compared with `===`), which is the safe
// direction: union codegen wraps a case in a `try` it turns out not to need,
// rather than dropping the fallback a raise needed.
export const B_embed = (b: Val, value: unknown): string => {
  b.g.t++;
  return B_embedPure(b, value);
}

// B_embed for a value generated code can't raise through — a helper that
// never throws. Skipping the raise counter keeps union codegen from wrapping
// the case in a `try` it doesn't need, and keeps loop bodies recognizable as
// throw-free (see B_mergeWithCatch's `pureSince`).
export const B_embedPure = (b: Val, value: unknown): string => {
  const e = b.g.e;
  const l = e.length;
  e[l] = value;
  return `e[${l}]`;
}

export const B_inlineConst = (b: Val, schema: Internal): string => {
  const tagFlag = tagFlags[schema.type]!;
  const const_ = schema.const;
  if (flagUnsafeHas(tagFlag, tagFlagUndefined)) {
    return "void 0";
  } else if (flagUnsafeHas(tagFlag, tagFlagString)) {
    return inlinedValueFromString(const_ as string);
  } else if (flagUnsafeHas(tagFlag, tagFlagBigint)) {
    return (const_ as unknown as string) + "n";
  } else if (
    flagUnsafeHas(
      tagFlag,
      ((tagFlagSymbol | tagFlagFunction) | tagFlagInstance)
    )
  ) {
    return B_embed(b, schema.const);
  } else {
    return const_ as unknown as string;
  }
}

export const B_varWithoutAllocation = (global: BGlobal): string => {
  const newCounter = global.v + 1;
  global.v = newCounter;
  return `v${newCounter}`;
}

// Append a `let` declaration to a still-open owner val, emitted after the
// owner's checks in `merge`. The owner is the materialized val's immediate
// context (its `prev`, its `parent` for a field read, or itself); since the
// decl lands at the owner's segment end — after the owner's guard, before
// its dependent code — that immediate owner already dominates and outlives
// every use, so no separate scope-tree is needed. The owner must be
// unfinalized; `_notVarAtParent` guards this explicitly.
export const B_hoistDecl = (owner: Val, decl: string): void => {
  owner.hd = owner.hd === "" ? decl : owner.hd + "," + decl;
}


export const B_operationArg = (
  schema: Internal,
  expected: Internal,
  flag: Flag,
  defs: Record<string, Internal> | undefined
): Val => {
  // Every Val literal in the codegen path lists the same fields in the same
  // order (undefined where unset) so V8 gives them all ONE hidden class —
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
    f: valFlagNone,
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

export const B_throw = (errorDetails: ErrorDetails): never => {
  throw new SuryError(errorDetails);
}

export const B_unsupportedDecode = (b: Val, from: Internal, target: Internal): never => {
  return B_throw({
    code: "unsupported_decode",
    from: from,
    to: target,
    reason: `Can't decode ${inputExpression(from)} to ${inputExpression(
      target
    )}. Use S.to to define a custom decoder`,
    path: b.path,
  });
}

export const B_failWithArg = <TArg>(b: Val, fn: (arg: TArg) => ErrorDetails, arg: string): string => {
  return `${B_embed(b, (arg: TArg) => {
    B_throw(fn(arg));
  })}(${arg})`;
}

// Record a raise that reaches generated code without an embed behind it — the
// bare `throw` a loop wrapper re-raises a nested error with. Union codegen
// decides whether a case needs a `try` by bracketing an emission and reading
// `g.t`, so a raise counted by neither this nor `B_embed` is a case that
// silently loses its fallback.
export const B_markThrow = (b: Val): void => {
  b.g.t++;
}


export const B_makeInvalidConversionDetails = (input: Val, to: Internal, cause: unknown): ErrorDetails => {
  if (cause && (cause as { s?: symbol }).s === s) {
    const error = cause as unknown as SuryErrorRecord;

    // A SuryError thrown by user code carries only the path it named, so the
    // path it was reached through is prepended here. Nothing arrives
    // pre-prepended any more — that was effectCtx, which is gone.
    //
    // Copied rather than mutated: user code may throw one retained instance
    // more than once, and prepending onto the instance makes the second parse
    // report `["a"]["a"]`. Nothing to prepend means nothing to copy — `B_throw`
    // rebuilds a SuryError from whichever of the two it gets.
    return (
      input.path === pathEmpty
        ? error
        : { ...error, path: pathConcat(input.path, error.path) }
    ) as unknown as ErrorDetails;
  } else {
    let reason: string;
    if (cause instanceof Error) {
      const text = "" + cause;
      if (text.startsWith("Error: ")) {
        reason = text.slice(7);
      } else {
        reason = text;
      }
    } else {
      reason = stringify(cause);
    }
    return {
      code: "invalid_conversion",
      from: input.s,
      to: to,
      cause,
      path: input.path,
      reason,
    };
  }
}

// Checks run against `prev.var()`, so the runtime type at check time
// is `prev.schema`, not the post-narrowing schema on the current val.
const B_receivedSchema = (val: Val): Internal => {
  return val.prev !== U ? val.prev.s : val.s;
}

export const B_makeInvalidInputDetails = (
  expected: Internal,
  received: Internal,
  path: Path,
  input: unknown,
  unionErrors?: SuryErrorRecord[],
  reasonOverride?: string
): ErrorDetails => {
  let reasonRef: string;
  if (reasonOverride !== U) {
    reasonRef = reasonOverride;
  } else {
    const expectedExpression = inputExpression(expected);
    const receivedExpression = stringify(input);
    // `Expected Date, received Date` names the type twice and says nothing: the
    // type is right and the value is not (an Invalid Date, an Error carrying the
    // wrong payload). Saying `received invalid Date` is the only part of the
    // message that carries information in that case.
    reasonRef = `Expected ${expectedExpression}, received ${
      expectedExpression === receivedExpression ? "invalid " : ""
    }${receivedExpression}`;
  }
  if (unionErrors !== U) {
    const caseErrors = unionErrors;
    const seenReasons = new Set<string>();
    for (let idx = 0; idx < caseErrors.length; idx++) {
      const caseError = caseErrors[idx]!;
      const caseReason = caseError.reason.split("\n").join("\n  ");
      const location = caseError.path === "" ? "" : `At ${caseError.path}: `;
      const line = `\n- ${location}${caseReason}`;
      if (!seenReasons.has(line)) {
        seenReasons.add(line);
        reasonRef = reasonRef + line;
      }
    }
  }

  const details: InvalidInputDetails = {
    code: "invalid_input",
    expected: expected,
    received,
    path,
    reason: reasonRef,
    unionErrors,
    input,
  };
  return details;
}

// Drop-in `check.fail` builder for InvalidInput failures. The returned
// `(~input) => value => details` closure snapshots expected/received/path
// so it does not retain the val (otherwise the embed array would pin the
// whole val chain). Pass directly as `check.fail` to skip the wrapper.
export const B_invalidInputBuilder = (
  expected?: Internal,
  extraPath: Path = pathEmpty,
  reasonOverride?: string
): (input: Val) => (value: unknown) => ErrorDetails => {
  return (input: Val) => {
    const expected_ = expected !== U ? expected : input.e;
    const received = B_receivedSchema(input);
    const path = extraPath === pathEmpty ? input.path : pathConcat(input.path, extraPath);
    return (value: unknown) =>
      B_makeInvalidInputDetails(expected_, received, path, value, U, reasonOverride);
  };
}


export const B_failWithErrorMessage = (
  key: string,
  defaultMessage?: string
): (input: Val) => (value: unknown) => ErrorDetails => {
  return (input: Val) => {
    const em = input.e.errorMessage as Record<string, string | undefined> | undefined;
    const override = em !== U ? (em[key] !== U ? em[key] : em["_"]) : U;
    const m = override !== U ? override : defaultMessage;
    if (m !== U) {
      return B_invalidInputBuilder(U, U, m)(input);
    } else {
      return failInvalidType(input);
    }
  };
}

// Inline variant: emits the throw expression directly. Used by decoders
// that splice errors into custom JS (e.g. `catch(_){${embedInvalidInput}}`),
// not via the `check` pipeline.
export const B_embedInvalidInput = (input: Val, expected: Internal = input.e): string => {
  return B_failWithArg(input, B_invalidInputBuilder(expected)(input), input.v());
}

// Caller must verify `val.vc` is truthy and `val.expected.noValidation !==
// true` first — the `!` unwrap below is unchecked. `inputVar` is usually
// `val.prev.var()`.
const B_emitChecks = (val: Val, inputVar: string): string => {
  const checks = val.vc!;
  const len = checks.length;
  if (len === 1) {
    const check = checks[0]!;
    return `${check.c(inputVar)}||${B_failWithArg(val, check.f(val), inputVar)};`;
  } else {
    let out = "";
    let i = 0;
    while (i < len) {
      const head = checks[i]!;
      const fail = head.f;
      let cond = head.c(inputVar);
      i = i + 1;
      // Extend the fused cond while the next check shares this `fail`.
      while (i < len && checks[i]!.f === fail) {
        cond = cond + "&&" + checks[i]!.c(inputVar);
        i = i + 1;
      }
      out = out + `${cond}||${B_failWithArg(val, fail(val), inputVar)};`;
    }
    return out;
  }
}

// Whether a val's type-narrow checks can lift into a union dispatch
// condition without stranding a declaration the lifted check reads:
// non-transforming vals read the upstream input var (always safe); a
// transforming val is safe only when its prev is non-transforming (stable
// input var) and it has no codeFromPrev of its own to leave behind — else
// the lifted check runs before that producer (the str->to(option(int))
// "v0 is not defined" bug class).
export const B_isHoistable = (val: Val): boolean => {
  return val.t === true ? val.prev!.t !== true && val.cp === "" : true;
}

// A hoisted type-narrow kept in both forms: `c` routes the value to the next
// union case (dispatch), and re-emitting it against `v` rejects the case from
// inside a `try` (fallback). Only `c` and the two strings it needs are captured —
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
  let current: Val | undefined = val;
  let code = "";

  while (current !== U) {
    const val: Val = current;
    current = val.prev;

    let currentCode = "";

    if (val.vc) {
      if (out !== U && B_isHoistable(val)) {
        const inputVar = current!.v();
        const checks = val.vc;
        let hoisted = "";
        for (let i = 0; i < checks.length; i++) {
          const check = checks[i]!;
          const condCode = check.c(inputVar);
          if (check.f === failInvalidType) {
            hoisted = hoisted ? `${hoisted}&&${condCode}` : condCode;
          } else if (val.e.noValidation !== true) {
            // `noValidation` is intentionally bypassed for the hoisted part —
            // the cond routes between cases, it doesn't reject, so suppressing
            // it would break dispatch.
            currentCode =
              currentCode + `${condCode}||${B_failWithArg(val, check.f(val), inputVar)};`;
          }
        }
        if (hoisted) {
          out.c = out.c ? `${hoisted}&&${out.c}` : hoisted;
          out.h.unshift({ v: val, i: inputVar, c: hoisted });
        }
      } else if (val.e.noValidation !== true) {
        currentCode = B_emitChecks(val, current!.v());
      }
    }

    // Hoisted decls land after this val's checks (the old varsAllocation
    // slot).
    if (val.hd !== "") {
      currentCode = currentCode + `let ${val.hd};`;
    }

    // Now emitted: a later cached-bond materialization can't hoist onto it.
    val.fz = true;

    currentCode = val.cp + currentCode;

    code = currentCode + code;
  }

  return code;
}

// Rebinds `val.v` so the next call to it also stashes the resolved var name
// (and switches `nextVal.v` to the plain `_var` reader) onto `nextVal` —
// links a derived val's var resolution to its source without eagerly
// materializing a var. Shared by every "derive a val from a val" builder.
const B_linkVar = (val: Val, nextVal: Val): void => {
  const valVar: () => string = val.v.bind(val);
  val.v = () => {
    const v = valVar();
    nextVal.i = v;
    nextVal.v = _var;
    return v;
  };
}

export const B_next = (prev: Val, initial: string, schema: Internal, expected: Internal = prev.e): Val => {
  // No `d`: this val is a *new* value, so `prev`'s field vals don't describe
  // it. Inheriting them let a reader of a transformed object read the fields
  // of the value that went in — a flattened member's codec ran and its result
  // was then discarded field by field (#368's FIXME). `valGet` re-reads them
  // off this value instead. B_scope, which names the *same* value, does share
  // `d` — that aliasing is the correct one.
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
    f: valFlagNone,
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

// Pass a non-empty `~checks` or omit it. Never pass `~checks=[]` —
// that would break the val.checks "absent iff no checks" invariant.
export const B_refine = (val: Val, schema: Internal = val.s, checks?: Check[], expected: Internal = val.e): Val => {
  const shouldLink = val.v !== _var;
  // Canonical Val field order (see B_operationArg).
  const nextVal: Val = {
    b: U,
    p: U,
    v: shouldLink ? _prevVar : _var,
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
  if (shouldLink) {
    B_linkVar(val, nextVal);
  }
  return nextVal;
}

// Lazy-allocate helper for mutating an existing val (as opposed to
// building a local array and passing it through `refine`).
export const B_pushCheck = (val: Val, check: Check): void => {
  if (val.vc !== U) {
    val.vc.push(check);
  } else {
    val.vc = [check];
  }
}

// Applies both refiners. Input checks push onto valInput.checks
// (emit at pre-transform slot); output checks wrap val via refine.
// When valInput.prev is None, input checks fold into the output
// wrap so emit has a prev.var(). Sets isOutput on the result.
//
// The parse loop applies refiners itself only for primitive decoders, so every
// decoder that sets isOutput — object, array, tuple, union, recursive — has to
// call this. Not calling it silently drops the user's S.refine.
export const B_markOutput = (val: Val, valInput: Val): Val => {
  let deferredInputChecks: Check[] | undefined;
  const inputRefiner = valInput.e.inputRefiner;
  if (inputRefiner !== U) {
    const checks = inputRefiner(valInput);
    if (checks.length > 0) {
      if (valInput.prev !== U) {
        for (let i = 0; i < checks.length; i++) {
          B_pushCheck(valInput, checks[i]!);
        }
        deferredInputChecks = U;
      } else {
        deferredInputChecks = checks;
      }
    } else {
      deferredInputChecks = U;
    }
  } else {
    deferredInputChecks = U;
  }

  let outputChecks: Check[] | undefined;
  const refiner = val.e.refiner;
  if (refiner !== U) {
    const checks = refiner(val);
    outputChecks = checks.length > 0 ? checks : U;
  } else {
    outputChecks = U;
  }

  let result: Val;
  if (deferredInputChecks !== U && outputChecks !== U) {
    result = B_refine(val, U, deferredInputChecks.concat(outputChecks));
  } else if (deferredInputChecks !== U) {
    result = B_refine(val, U, deferredInputChecks);
  } else if (outputChecks !== U) {
    result = B_refine(val, U, outputChecks);
  } else {
    result = val;
  }
  result.io = true;
  return result;
}

// Used in union codegen: splice a literal child's checks into the parent
// as dispatch discriminants. Each cond's `inputVar` is rewritten to
// `parent[key]`; `fail` stays shared so lifted checks fuse with the
// parent's own type guard. No-op if the child has no checks.
export const B_hoistChildChecks = (parent: Val, child: Val, key: string): void => {
  if (child.vc) {
    const pathAppend = pathFromInlinedLocation(inlinedValueFromString(key));
    child.vc!.forEach((check) => {
      B_pushCheck(parent, {
        c: (inputVar) => check.c(inputVar + pathAppend),
        f: check.f,
      });
    });
    child.vc = U;
  }
}

export const B_dynamicScope = (from: Val, locationVar: string): Val => {
  // `additionalItems` doubles as the value schema for a dict-shaped val.
  // Extract it via a real pattern match: a non-`Schema` mode (`Strip`/`Strict`
  // on a fixed-property object) must never be cast to a schema — that string
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
    path: pathEmpty,
    g: from.g,
    o: U,
  };
}

// B_dynamicScope for a container iterated by value rather than by index: the
// loop variable IS the item (`for (const v1 of set)`), so there's no location
// to read the item back through and no `additionalItems` to take the schemas
// from — both sides are passed in. Same fresh-root shape otherwise: no `prev`,
// so merging the body stops at the loop.
export const B_iterScope = (
  from: Val,
  inline: string,
  schema: Internal,
  expected: Internal
): Val => {
  // Canonical Val field order (see B_operationArg).
  return {
    b: U,
    p: from,
    v: _notVarBeforeValidation,
    i: inline,
    s: schema,
    io: U,
    e: expected,
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
    path: pathEmpty,
    g: from.g,
    o: U,
  };
}

export const B_nextConst = (from: Val, schema: Internal, expected?: Internal): Val => {
  return B_next(from, B_inlineConst(from, schema), schema, expected);
}

export const B_asyncVal = (from: Val, initial: string): Val => {
  const v = B_next(from, initial, from.s);
  v.f = valFlagAsync;
  return v;
}

export const B_addObjectField = (objectVal: Val, location: string, val: Val): void => {
  if (objectVal.s.type === arrayTag) {
    objectVal.s.items!.push(val.s);
  } else {
    if (!val.o) {
      objectVal.s.required!.push(location);
    }
    objectVal.s.properties![location] = val.s;
  }

  // Async field values must be reachable as a plain identifier so
  // the accumulator in completeObjectVal can use val.inline as a
  // destructuring/reference target. For e.g. array-of-async, the
  // asyncVal's inline is a Promise.all(...) expression, not a var.
  // This has to happen before val->merge, which finalizes the prev
  // chain and locks the emitted code.
  if (flagUnsafeHas(val.f, valFlagAsync)) {
    val.v();
  }
  objectVal.cp = objectVal.cp + B_merge(val);
  objectVal.d![location] = val;
}

export const B_addKey = (objVal: Val, key: string, value: Val): string => {
  return `${objVal.v()}[${key}]=${value.i}`;
}

export const B_scope = (val: Val): Val => {
  const shouldLink = val.v !== _var;

  // TODO: Simplify bond
  // Canonical Val field order (see B_operationArg).
  const nextVal: Val = {
    b: val,
    p: U,
    v: shouldLink ? _bondVar : _var,
    i: val.i,
    s: val.s,
    io: val.io,
    e: val.e,
    prev: U,
    f: flagNone,
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
  if (shouldLink) {
    B_linkVar(val, nextVal);
  }
  return nextVal;
}

export const B_embedTransformation = (input: Val, fn: (input: unknown) => unknown, isAsync: boolean): Val => {
  const outputVar = B_varWithoutAllocation(input.g);
  const output = B_next(input, outputVar, unknown, input.e.to!);
  output.v = _var;
  if (isAsync) {
    if (!flagUnsafeHas(input.g.o, flagAsync)) {
      B_throw({
        code: "invalid_operation",
        path: pathEmpty,
        reason:
          "Encountered unexpected async transform or refine. Use parseAsyncOrThrow operation instead",
      });
    }
    output.f |= valFlagAsync;
  }
  const embeddedFn = B_embed(input, fn);
  const inputValue = input.vc ? input.v() : input.i;
  if (input.g.o & flagUnionTransformContext) {
    // The enclosing union owns exception classification. Wrapping a foreign
    // exception here would make it look like a Sury mismatch.
    output.cp = `let ${outputVar}=${embeddedFn}(${inputValue});`;
    return output;
  }
  const failure = `${B_failWithArg(
    output,
    (e: unknown) => B_makeInvalidConversionDetails(input, unknown, e),
    `x`
  )}`;
  // Feed the transform the input's var when it already carries checks — it's
  // materialized into a var anyway (the check references it), so reuse it
  // instead of re-inlining the source expression (e.g. `i["x"]`) twice.
  output.cp = `let ${outputVar};try{${outputVar}=${embeddedFn}(${inputValue})${
    isAsync ? `.catch(x=>${failure})` : ""
  }}catch(x){${failure}}`;
  return output;
}


export const B_invalidOperation = (val: Val, description: string): never => {
  return B_throw({ code: "invalid_operation", reason: description, path: val.path });
}

const B_mergeWithCatch = (
  val: Val,
  catchFn: (errorVar: string) => string,
  appendSafe?: () => string,
  pureSince?: number
): string => {
  const valCode = B_merge(val);
  // `pureSince` is the raise counter before the val was built: unchanged means
  // nothing merged can throw, so the catch wrapper is dead. Without an append
  // the code itself is dead too — an untransformed, unfailable body is only
  // orphaned `let`s — and dropping it lets the caller skip its loop entirely.
  const pure = pureSince !== U && val.g.t === pureSince;
  if (
    (valCode === "" || pure) &&
    // FIXME: Instead of this wrap all S.transform in a try/catch
    !flagUnsafeHas(val.f, valFlagAsync)
  ) {
    return appendSafe !== U ? valCode + appendSafe() : pure ? "" : valCode;
  } else {
    const errorVar = B_varWithoutAllocation(val.g);

    B_markThrow(val);
    const catchCode = `${catchFn(errorVar)};throw ${errorVar}`;

    if (flagUnsafeHas(val.f, valFlagAsync)) {
      val.i = `${val.i}.catch(${errorVar}=>{${catchCode}})`;
    }
    return `try{${valCode}${
      appendSafe !== U ? appendSafe() : ""
    }}catch(${errorVar}){${catchCode}}`;
  }
}

export const B_mergeWithPathPrepend = (
  val: Val,
  parent: Val,
  locationVar?: string,
  appendSafe?: () => string,
  pureSince?: number
): string => {
  if (val.path === pathEmpty && locationVar === U) {
    return B_merge(val);
  } else {
    return B_mergeWithCatch(
      val,
      (errorVar) =>
        `${errorVar}.path=${
          parent.path === "" ? "" : `${inlinedValueFromString(parent.path)}+`
        }${locationVar !== U ? `'["'+${locationVar}+'"]'+` : ""}${errorVar}.path`,
      appendSafe,
      pureSince
    );
  }
}

export function noopOperation(i: unknown): unknown {
  return i;
}
(noopOperation as unknown as Record<string, unknown>)["embedded"] = immutableEmptyArray;
// TODO: Split validation code and transformation code
