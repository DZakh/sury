import {
  arrayTag,
  type BGlobal,
  type Check,
  type ErrorDetails,
  type Flag,
  getOrRethrow,
  immutableEmptyArray,
  inlinedValueFromString,
  inputExpression,
  type Internal,
  isLiteral,
  type InvalidInputDetails,
  type Path,
  pathEmpty,
  pathFromInlinedLocation,
  s,
  stringify,
  SuryError,
  type SuryErrorRecord,
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
// `check.f === failInvalidType`) — a method wrapper would break that
// identity comparison.

export function _var(this: Val): string {
  return this.i;
}

// B_refine links through `prev`; B_scope through `b` so merge does not walk
// the source as `prev` (a scope is a new segment over the same value).
function _linkVar(this: Val): string {
  return (this.b || this.prev)!.v();
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
// `errorMessage.format` rather than `errorMessage.type` — keeping it the same
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
// anything callable behind that accessor may raise — a fail helper, a user
// transform, `S.json`'s validator. Counting every embed over-reports for the
// inert ones (a symbol literal compared with `===`), which is the safe
// direction: union codegen wraps a case in a `try` it turns out not to need,
// rather than dropping the fallback a raise needed.
export const B_embed = (b: Val, value: unknown): string => (b.g.t++, B_embedPure(b, value));

// B_embed for a value generated code can't raise through — a helper that
// never throws. Skipping the raise counter keeps union codegen from wrapping
// the case in a `try` it doesn't need, and keeps loop bodies recognizable as
// throw-free (see B_mergeWithCatch's `pureSince`).
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
          ? B_embed(b, schema.const)
          : const_ as unknown as string;
}

export const B_varWithoutAllocation = (g: BGlobal): string => `v${++g.v}`;

// Append a `let` declaration to a still-open owner val, emitted after the
// owner's checks in `merge`. The owner is the materialized val's immediate
// context (its `prev`, its `parent` for a field read, or itself); since the
// decl lands at the owner's segment end — after the owner's guard, before
// its dependent code — that immediate owner already dominates and outlives
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

export const B_throw = (errorDetails: ErrorDetails): never => {
  throw new SuryError(errorDetails);
}

export const B_unsupportedDecode = (b: Val, from: Internal, target: Internal): never =>
  B_throw({
    code: "unsupported_decode",
    from,
    to: target,
    reason: `Can't decode ${inputExpression(from)} to ${inputExpression(target)}. Use S.to to define a custom decoder`,
    path: b.path,
  });

export const B_failWithArg = <TArg>(b: Val, fn: (arg: TArg) => ErrorDetails, arg: string): string =>
  `${B_embed(b, (a: TArg) => {
    B_throw(fn(a));
  })}(${arg})`;

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
    return (input.path ? { ...error, path: input.path + error.path } : error) as unknown as ErrorDetails;
  }
  let reason: string;
  if (cause instanceof Error) {
    reason = "" + cause;
    if (reason.startsWith("Error: ")) reason = reason.slice(7);
  } else {
    reason = stringify(cause);
  }
  return {
    code: "invalid_conversion",
    from: input.s,
    to,
    cause,
    path: input.path,
    reason,
  };
}

export const B_makeInvalidInputDetails = (
  expected: Internal,
  received: Internal,
  path: Path,
  input: unknown,
  unionErrors?: SuryErrorRecord[],
  reasonOverride?: string
): ErrorDetails => {
  let reasonRef = reasonOverride;
  if (reasonRef === U) {
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
  if (unionErrors) {
    const seenReasons = new Set<string>();
    for (let idx = 0; idx < unionErrors.length; idx++) {
      const caseError = unionErrors[idx]!;
      const line = `\n- ${caseError.path === "" ? "" : `At ${caseError.path}: `}${caseError.reason.split("\n").join("\n  ")}`;
      if (!seenReasons.has(line)) {
        seenReasons.add(line);
        reasonRef += line;
      }
    }
  }

  return {
    code: "invalid_input",
    expected,
    received,
    path,
    reason: reasonRef,
    unionErrors,
    input,
  };
}

// Drop-in `check.fail` builder for InvalidInput failures. The returned
// `(~input) => value => details` closure snapshots expected/received/path
// so it does not retain the val (otherwise the embed array would pin the
// whole val chain). Pass directly as `check.fail` to skip the wrapper.
export const B_invalidInputBuilder = (
  expected?: Internal,
  extraPath: Path = pathEmpty,
  reasonOverride?: string
): (input: Val) => (value: unknown) => ErrorDetails => (input) => {
  const path = extraPath ? input.path + extraPath : input.path;
  return (value) =>
    B_makeInvalidInputDetails(
      expected ?? input.e,
      (input.prev || input).s,
      path,
      value,
      U,
      reasonOverride,
    );
};

export const B_failWithErrorMessage = (
  key: string,
  defaultMessage?: string
): (input: Val) => (value: unknown) => ErrorDetails => (input) => {
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
// true` first — the `!` unwrap below is unchecked. `inputVar` is usually
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
  let current: Val | undefined = val, code = "";

  while (current !== U) {
    const val: Val = current;
    current = val.prev;
    let currentCode = "";

    if (val.vc) {
      // Type-narrows hoist only when they can't strand a decl the lifted
      // check reads: a transforming val is safe iff prev is non-transforming
      // (stable input var) and this val has no codeFromPrev of its own —
      // else the lifted check runs before that producer (the
      // str->to(option(int)) "v0 is not defined" bug class).
      // A val with no `prev` is checked against itself: a scope over a typed
      // source (a union case's narrow) carries its checks with nothing before
      // it, and its own var is the value they read.
      if (out && (!val.t || (current !== U && !current.t && val.cp === ""))) {
        const inputVar = (current || val).v();
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
            currentCode += `${condCode}||${B_failWithArg(val, check.f(val), inputVar)};`;
          }
        }
        if (hoisted) {
          out.c = out.c ? `${hoisted}&&${out.c}` : hoisted;
          out.h.unshift({ v: val, i: inputVar, c: hoisted });
        }
      } else if (val.e.noValidation !== true) {
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
// (and switches `nextVal.v` to the plain `_var` reader) onto `nextVal` —
// links a derived val's var resolution to its source without eagerly
// materializing a var. Shared by every "derive a val from a val" builder.
const B_linkVar = (val: Val, nextVal: Val): void => {
  const get = val.v.bind(val);
  val.v = () => (nextVal.i = get(), nextVal.v = _var, nextVal.i);
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

// Pass a non-empty `~checks` or omit it. Never pass `~checks=[]` —
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

// Applies both refiners. Input checks push onto valInput.checks
// (emit at pre-transform slot); output checks wrap val via refine.
// When valInput.prev is None, input checks fold into the output
// wrap so emit has a prev.var(). Sets isOutput on the result.
//
// The parse loop applies refiners itself only for primitive decoders, so every
// decoder that sets isOutput — object, array, tuple, union, recursive — has to
// call this. Not calling it silently drops the user's S.refine.
export const B_markOutput = (val: Val, valInput: Val): Val => {
  let inC: Check[] | undefined, outC: Check[] | undefined;
  const ir = valInput.e.inputRefiner;
  if (ir) {
    const c = ir(valInput);
    if (c.length) {
      if (valInput.prev) (valInput.vc ??= []).push(...c);
      else inC = c;
    }
  }
  const rf = val.e.refiner;
  if (rf) {
    const c = rf(val);
    if (c.length) outC = c;
  }
  val = inC ? B_refine(val, U, outC ? inC.concat(outC) : inC) : outC ? B_refine(val, U, outC) : val;
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
    const pathAppend = pathFromInlinedLocation(inlinedValueFromString(key));
    for (let i = 0; i < checks.length; i++) {
      const check = checks[i]!;
      B_pushCheck(parent, { c: (v) => check.c(v + pathAppend), f: check.f });
    }
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

export const B_nextConst = (from: Val, schema: Internal, expected?: Internal): Val =>
  B_next(from, B_inlineConst(from, schema), schema, expected);

// The expression to read a val by when it will be read more than once — the
// conversion reads it, and whatever the conversion's own result is spliced into
// may read that. `v()` hands back the var that already stands for the value
// wherever one does, and hoists one only where the source is an expression
// nothing has named yet, which is the case a second read would repeat.
export const B_readOnce = (input: Val): string => input.v();

// A conversion's result, held in a var. The splice that reads it may read it
// twice (jsonString's escape-free form does), and unlike a property path this is
// a fresh pass over the whole value — so it is computed once, the way
// B_conversion computes a custom coder's result once.
export const B_computed = (
  input: Val,
  code: string,
  schema: Internal,
  failure?: string,
): Val => {
  const outputVar = B_varWithoutAllocation(input.g);
  const output = B_next(input, outputVar, schema);
  output.v = _var;
  // With a `failure`, the whole `B_conversion` shape: a computation that can
  // throw on a value the operation trusted rather than checked reports it as a
  // failed conversion instead of escaping as whatever the platform raised.
  output.cp =
    failure === U
      ? `let ${outputVar}=${code};`
      : `let ${outputVar};try{${outputVar}=${code}}catch(x){${failure}}`;
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
//    decode would, the same deal `S.decoder` gives a caller who declares the
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
    const outputVar = B_varWithoutAllocation(input.g);
    const output = B_next(
      input,
      outputVar,
      junction || isLiteral(target) ? unknown : target,
      target,
    );
    output.v = _var;
    if (isAsync) B_markAsync(input, output);
    const embeddedFn = B_embed(input, fn);
    // Reuse the input's var when checks already materialized it, instead of
    // re-inlining the source expression twice.
    const inputValue = input.vc ? input.v() : input.i;
    const unionContext = input.g.o & 4; // 4
    if (unionContext && isAsync) {
      output.cp = `let ${outputVar}=${embeddedFn}(${inputValue});`;
      return output;
    }
    const rethrow = unionContext ? `${B_embed(input, getOrRethrow)}(x);` : "";
    const failure = B_failWithArg(
      output,
      (e: unknown) => B_makeInvalidConversionDetails(input, target, e),
      `x`,
    );
    output.cp = `let ${outputVar};try{${outputVar}=${embeddedFn}(${inputValue})${
      isAsync ? `.catch(x=>${failure})` : ""
    }}catch(x){${rethrow}${failure}}`;
    // A val whose result the target's own refiners can attach to. `val.vc`
    // checks emit at the *pre-transform* slot (`prev.v()` in B_merge), so
    // leaving them on the coder's own val would validate what went into the
    // coder instead of what came out — `S.uuid->S.to(userSchema, ~custom=…)`
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
    `Can't decode ${inputExpression(input.e)} to ${inputExpression(
      input.e.to!,
    )}. The conversion is marked as never`,
  );

// CONTENT_CODEC_SPEC.md rules 3 and 4, for the direction a link is written in:
// two schemas whose payloads disagree (`content`) have two readings of it —
// store the source's value in the target, or open the source and hand its
// payload over — and the target naming its own payload with `.to` is what picks
// the second. Compiling can't tell the two apart, because reversing a chain
// turns that payload declaration into just another link: the legal
// `X -> jsonString -> File` and the rejected `jsonString -> File` reach the
// decoder as the same pair. So the reading is settled where the link is made,
// and an unreadable one takes a slot that rejects the operation instead.
// The node a link's content reading comes from: the schema, or the arm that
// carries one where the schema is a union — which has neither `content` nor
// `.to` of its own, though linking a carrier to `S.optional(S.jsonString)` puts
// the same two readings on the table as linking it to `S.jsonString`.
export const B_contentNode = (schema: Internal): Internal =>
  (schema.content === U && schema.anyOf?.find((arm) => arm.content !== U)) || schema;

// Half of CONTENT_CODEC_SPEC.md rule 4's question: whether two payloads are of
// different kinds, which is what puts two readings on the table — store the
// source's value in the target, or open the source and hand its payload over.
// The other half, a `.to` on the target picking the second (rule 3), stays with
// each caller, along with the `B_contentNode` walk that finds a marker on a
// union arm. Compiling can't tell the two readings apart,
// because reversing a chain turns a payload declaration into just another link,
// so the reading is settled where the link is made — and what to say about it
// differs by where that was, so the message stays with the caller too.
export const B_contentDiffers = (from?: Internal, to?: Internal): boolean =>
  from !== U && to !== U && from !== to && !(from.bc && to.bc);

// Which reading of a content link applies: a `"pack"`/`"unpack"` slot the caller
// wrote wins (rule 1), and otherwise a target that names its own payload is what
// asks for the source to be opened (rule 3). Read by the carriers, never by the
// formats — the format side only ever asks whether a `content` marker is there.
export const B_readsPayload = (target: Internal): boolean =>
  target.opens ?? target.to !== U;

export const B_invalidOperation = (val: Val, description: string): never =>
  B_throw({ code: "invalid_operation", reason: description, path: val.path });

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
    // FIXME: Instead of this wrap every custom coder in a try/catch
    !(val.f & 1) // 1
  ) {
    return appendSafe ? valCode + appendSafe() : pure ? "" : valCode;
  }
  const errorVar = B_varWithoutAllocation(val.g);
  B_markThrow(val);
  const catchCode = `${catchFn(errorVar)};throw ${errorVar}`;
  if (val.f & 1) val.i = `${val.i}.catch(${errorVar}=>{${catchCode}})`;
  return `try{${valCode}${appendSafe ? appendSafe() : ""}}catch(${errorVar}){${catchCode}}`;
}

export const B_mergeWithPathPrepend = (
  val: Val,
  parent: Val,
  locationVar?: string,
  appendSafe?: () => string,
  pureSince?: number
): string =>
  val.path === pathEmpty && locationVar === U
    ? B_merge(val)
    : B_mergeWithCatch(
        val,
        (errorVar) =>
          `${errorVar}.path=${parent.path ? `${inlinedValueFromString(parent.path)}+` : ""}${
            locationVar !== U ? `'["'+${locationVar}+'"]'+` : ""
          }${errorVar}.path`,
        appendSafe,
        pureSince,
      );

export function noopOperation(i: unknown): unknown {
  return i;
}
(noopOperation as unknown as Record<string, unknown>)["embedded"] = immutableEmptyArray;
// TODO: Split validation code and transformation code
