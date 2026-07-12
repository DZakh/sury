import { SuryError, unknown } from "./schema";
import { BGlobal, Check, Code, ErrorDetails, Internal, InvalidInputDetails, SuryErrorRecord, Val, immutableEmptyArray, s, shouldPrependPathKey, stringify, toExpression } from "./types";
import { Flag, flagAsync, flagNone, flagUnsafeHas, valFlagAsync, valFlagNone } from "./flags";
import { Path, inlinedValueFromString, pathConcat, pathEmpty, pathFromInlinedLocation } from "./path";
import { arrayTag, tagFlagBigint, tagFlagFunction, tagFlagInstance, tagFlagString, tagFlagSymbol, tagFlagUndefined, tagFlags } from "./tags";

export type Builder = (input: Val) => Val;
export type Encoder = (input: Val, target: Internal) => Val;

export type EffectCtx = {
  fail: (message: string, path?: Path) => never;
};

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
  // Own the decl ahead of any existing code so it precedes the checks that
  // read this val.
  val.cp = typeof val.cp === "string" ? `let ${v}=${val.i};` + val.cp : [`let ${v}=${val.i};`, val.cp];
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
  // their merge code was emitted. With deferred emission the parent's decl
  // hole stays fillable after merge; only a genuinely frozen parent (tree
  // stringified into a closure, or discarded) falls back to an inline
  // re-read, which defers the `parent[key]` access to a guarded use.
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
  // Genuinely frozen (tree stringified into a closure body, or discarded):
  // owning a fresh decl here would drop it (the phantom-var fusion bug).
  // Re-read the inline expression instead. Like `_notVarAtParent`'s frozen
  // guard, but a transform val's inline can be compound (e.g. `""+x`), so
  // parenthesize it to stay correct under any operator a consumer wraps it
  // in. Re-reading is sound only because the inlines that reach here are
  // idempotent (`""+x`, `+x`).
  if (val.fz) {
    val.v = _var;
    val.i = `(${val.i})`;
    return val.i;
  } else {
    const v = B_varWithoutAllocation(val.g);
    if (val.prev !== undefined) {
      // Own the decl in codeFromPrev: a non-empty codeFromPrev is
      // non-hoistable in `merge`, so a union discriminant reading this var
      // can't be lifted above its `let` (the str->to(option(int)) bug class).
      // Declare-and-assign after existing code; `v` is fresh, so nothing
      // emitted reads it.
      B_addCode(val, `let ${v}=${val.i};`);
    } else {
      // No prev to anchor to; hoist onto the val itself (its own segment
      // outlives the materialization).
      B_hoistDecl(val, `${v}=${val.i}`);
    }
    val.v = _var;
    val.i = v;
    return v;
  }
}

export const operationArgVar = "i";

// Pass this as `fail` on every check that wants "expected X, received Y"
// error semantics. Stable reference → adjacent checks fuse.
export const failInvalidType = (input: Val): (value: unknown) => ErrorDetails => {
  const em = input.e.errorMessage;
  const override = em !== undefined ? (em.type !== undefined ? em.type : em._) : undefined;
  return B_invalidInputBuilder(undefined, undefined, override)(input);
}

export const B_embed = (b: Val, value: unknown): string => {
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

// Escape it once per compiled operation.
// Use bGlobal as cache, so we don't allocate another object + it's garbage collected.
export const B_inlineLocation = (global: BGlobal, location: string): string => {
  const key = `"${location}"`;
  const cached = (global as unknown as Record<string, string | undefined>)[key];
  if (cached !== undefined) {
    return cached;
  } else {
    const inlinedLocation = inlinedValueFromString(location);
    (global as unknown as Record<string, string>)[key] = inlinedLocation;
    return inlinedLocation;
  }
}


export const B_varWithoutAllocation = (global: BGlobal): string => {
  const newCounter = global.v + 1;
  global.v = newCounter;
  return `v${newCounter}`;
}

// Append a `let` declaration to an owner val, emitted after the owner's
// checks when the join resolves the owner's marker in the rope. The owner is
// the materialized val's immediate context (its `prev`, its `parent` for a
// field read, or itself); since the decl lands at the owner's segment end —
// after the owner's guard, before its dependent code — that immediate owner
// already dominates and outlives every use. Legal any time before the final
// join, even after the owner's segment was merged; only a frozen owner
// (`fz`, already joined into a string) can no longer accept decls —
// `_notVarAtParent` guards this explicitly.
export const B_hoistDecl = (owner: Val, decl: string): void => {
  owner.hd = owner.hd === "" ? decl : owner.hd + "," + decl;
}

// Grow a val's code rope. String-on-string concatenates (V8 rope, cheap);
// anything else nests without copying.
export const B_addCode = (val: Val, code: Code): void => {
  val.cp =
    typeof val.cp === "string" && typeof code === "string"
      ? val.cp + code
      : [val.cp, code];
}

// The single stringification point: flattens the rope into a string,
// resolving each val marker to its codeFromPrev + checks + hoisted decls.
// Resolving a val seals it — after its slots have been read into a string,
// a late materialization can no longer emit there and must fall back to an
// inline re-read. Mid-compile joins (closure bodies, discarded empty
// trees) therefore need no extra bookkeeping: joining IS freezing.
// Returns its result rather than accumulating into a shared field, so a
// join nested inside another (a closure body resolved mid-walk) can't
// corrupt the outer one.
export const B_joinCode = (code: Code): string => {
  if (typeof code === "string") {
    return code;
  }
  if (Array.isArray(code)) {
    let acc = "";
    for (let i = 0; i < code.length; i++) {
      acc = acc + B_joinCode(code[i]!);
    }
    return acc;
  }
  code.fz = true;
  return B_joinCode(code.cp) + code.ck + (code.hd !== "" ? `let ${code.hd};` : "");
}


export const B_operationArg = (
  schema: Internal,
  expected: Internal,
  flag: Flag,
  defs: Record<string, Internal> | undefined
): Val => {
  return {
    cp: "",
    ck: "",
    hd: "",
    v: _var,
    i: operationArgVar,
    f: valFlagNone,
    s: schema,
    e: expected,
    path: pathEmpty,
    g: {
      d: defs,
      o: flag,
      e: [],
      v: -1,
    },
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
    reason: `Can't decode ${toExpression(from)} to ${toExpression(
      target
    )}. Use S.to to define a custom decoder`,
    path: b.path,
  });
}

export const B_failWithArg = <Arg>(b: Val, fn: (arg: Arg) => ErrorDetails, arg: string): string => {
  return `${B_embed(b, (arg: Arg) => {
    B_throw(fn(arg));
  })}(${arg})`;
}

export const B_makeInvalidConversionDetails = (input: Val, to: Internal, cause: unknown): ErrorDetails => {
  if (cause && (cause as { s?: symbol }).s === s) {
    const error = cause as unknown as SuryErrorRecord;

    // Read about this in shouldPrependPathKey comment.
    if (!error[shouldPrependPathKey]) {
      error["path"] = pathConcat(input.path, error.path);
    }
    return error as unknown as ErrorDetails;
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
  return val.prev !== undefined ? val.prev.s : val.s;
}

export const B_makeInvalidInputDetails = (
  expected: Internal,
  received: Internal,
  path: Path,
  input: unknown,
  includeInput: boolean,
  unionErrors?: SuryErrorRecord[],
  reasonOverride?: string
): ErrorDetails => {
  let reasonRef =
    reasonOverride !== undefined
      ? reasonOverride
      : `Expected ${toExpression(expected)}, received ${
          includeInput ? stringify(input) : toExpression(received)
        }`;
  if (unionErrors !== undefined) {
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
  };
  if (includeInput) {
    details.input = input;
  }
  return details;
}

// Drop-in `check.fail` builder for InvalidInput failures. The returned
// `(~input) => value => details` closure snapshots expected/received/path
// so it does not retain the val (otherwise the embed array would pin the
// whole val chain). Pass directly as `check.fail` to skip the wrapper.
export const B_invalidInputBuilder = (
  expected?: Internal,
  extraPath: Path = pathEmpty,
  reasonOverride?: string,
  includeInput: boolean = true
): (input: Val) => (value: unknown) => ErrorDetails => {
  return (input: Val) => {
    const expected_ = expected !== undefined ? expected : input.e;
    const received = B_receivedSchema(input);
    const path = extraPath === pathEmpty ? input.path : pathConcat(input.path, extraPath);
    return (value: unknown) =>
      B_makeInvalidInputDetails(
        expected_,
        received,
        path,
        value,
        includeInput,
        undefined,
        reasonOverride
      );
  };
}


export const B_failWithErrorMessage = (
  key: string,
  defaultMessage?: string
): (input: Val) => (value: unknown) => ErrorDetails => {
  return (input: Val) => {
    const em = input.e.errorMessage as Record<string, string | undefined> | undefined;
    const override = em !== undefined ? (em[key] !== undefined ? em[key] : em["_"]) : undefined;
    const m = override !== undefined ? override : defaultMessage;
    if (m !== undefined) {
      return B_invalidInputBuilder(undefined, undefined, m)(input);
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
// "v0 is not defined" bug class). Shared by `merge(~hoistCond)` and the
// union deopt scan so they can't drift. Phase 2's {pre, cond, body}
// dispatch will lift the producer into `pre`, collapsing this to "the
// check is a type-narrow."
export const B_isHoistable = (val: Val): boolean => {
  return val.t === true ? val.prev!.t !== true && val.cp === "" : true;
}

// Whether a val's checks can lift together with its no-throw producer
// (`pe`) folded into the dispatch condition. The producer must read a
// stable var: a non-transforming prev, or the chain root (the dispatch
// subject itself, materialized before the branch even when the wider chain
// carries a transform flag). Shared by `merge(~hoistCond)` and the union
// deopt scan so they can't drift.
export const B_isPeLiftable = (val: Val): boolean => {
  const prev = val.prev;
  return (
    val.pe !== undefined &&
    val.t === true &&
    prev !== undefined &&
    (prev.t !== true || prev.prev === undefined) &&
    val.cp === `let ${val.i}=${val.pe};`
  );
}

// Walks the val.prev chain and assembles generated code. When
// `~hoistCond` is provided (union codegen), type-narrow checks
// (fail === failInvalidType) lift into that ref as a dispatch
// discriminant instead of being emitted; constraint refines still
// emit inline so their case-specific error message survives. All
// other callers pass no `~hoistCond` and get the plain merge:
// every non-`noValidation` check is emitted inline.
export const B_merge = (val: Val, hoistCond?: { contents: string; pl?: boolean }): Code => {
  let current: Val | undefined = val;
  const code: Code[] = [];

  while (current !== undefined) {
    const val: Val = current;
    current = val.prev;

    let currentCode = "";

    if (val.vc) {
      // Whether this val's type-narrows may lift into the dispatch cond:
      // plain-hoistable vals read the stable prev var; a val fed by its own
      // no-throw producer (`pe`, e.g. `+i`) lifts by folding the producer
      // into the cond as a comma expression and demoting the decl to a
      // function-scoped `var`, so the assignment in the cond lands before
      // the branch body that reads it — str->to(option(int)) union cases
      // become `(v0=+i,!Number.isNaN(v0))` instead of try/catch dispatch.
      let liftInput: string | undefined;
      let pureLift = false;
      if (hoistCond !== undefined) {
        if (B_isHoistable(val)) {
          liftInput = current!.v();
        } else if (B_isPeLiftable(val)) {
          liftInput = val.i;
          pureLift = true;
        }
      }
      if (liftInput !== undefined) {
        // Partition: route type-narrows to hoistCond, emit refines inline.
        // `noValidation` is intentionally bypassed for the hoisted part —
        // the cond routes between union cases, it doesn't reject, so
        // suppressing would break dispatch.
        const inputVar = liftInput;
        // `liftInput` is only set when `hoistCond` is defined.
        const cond = hoistCond!;
        const allChecks = val.vc!;
        let localHoist = "";
        for (let i = 0; i < allChecks.length; i++) {
          const check = allChecks[i]!;
          const condCode = check.c(inputVar);
          if (check.f === failInvalidType) {
            if (localHoist) {
              localHoist = `${localHoist}&&${condCode}`;
            } else {
              localHoist = condCode;
            }
          } else if (val.e.noValidation !== true) {
            currentCode =
              currentCode + `${condCode}||${B_failWithArg(val, check.f(val), inputVar)};`;
          }
        }
        if (localHoist) {
          if (pureLift) {
            val.cp = `var ${inputVar};`;
            localHoist = `(${inputVar}=${val.pe},${localHoist})`;
            // The cond now carries a rejecting validation, not just a
            // routing discriminant — an only-case must emit the exhaustive
            // else instead of falling through (see unionDecoder).
            cond.pl = true;
          }
          if (cond.contents) {
            cond.contents = `${localHoist}&&${cond.contents}`;
          } else {
            cond.contents = localHoist;
          }
        }
      } else if (val.e.noValidation !== true) {
        const prev = current!;
        currentCode = B_emitChecks(val, prev.v());
      }
    }

    // The val is its own hole: the join reads codeFromPrev, this checks
    // code (`ck`), and the hoisted decls (the old varsAllocation slot) at
    // the end, so all three stay writable after this merge. One flat array
    // per merge, reversed to root-first order.
    val.ck = currentCode;
    code.push(val);
  }

  return code.reverse();
}

// Collapse a merge result that is empty right now to a sealed "" so
// emptiness checks downstream are plain string comparisons and a late fill
// can't be silently dropped into a discarded tree. Kept out of `merge`
// itself so bundles without unions/catch-wrapping don't ship it.
export const B_collapseMerge = (code: Code): Code => {
  const vals = code as Val[];
  for (let i = 0; i < vals.length; i++) {
    const v = vals[i]!;
    if (v.ck !== "" || v.hd !== "" || v.cp !== "") {
      return code;
    }
  }
  for (let i = 0; i < vals.length; i++) {
    vals[i]!.fz = true;
  }
  return "";
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
  return {
    // FIXME: `d` (the object-field-vals dict) and other val fields that hold
    // child vals are shared by reference with `prev`/`val`, not copied — see
    // the matching note on B_scope's `d: val.d` below. Whether that aliasing
    // is actually safe is an open question, not a settled design.
    prev,
    v: _notVar,
    i: initial,
    f: valFlagNone,
    s: schema,
    e: expected,
    cp: "",
    ck: "",
    hd: "",
    path: prev.path,
    g: prev.g,
    t: true,
    d: prev.d,
  };
}

// Pass a non-empty `~checks` or omit it. Never pass `~checks=[]` —
// that would break the val.checks "absent iff no checks" invariant.
export const B_refine = (val: Val, schema: Internal = val.s, checks?: Check[], expected: Internal = val.e): Val => {
  const shouldLink = val.v !== _var;
  const nextVal: Val = {
    prev: val,
    i: val.i,
    v: shouldLink ? _prevVar : _var,
    f: val.f,
    s: schema,
    e: expected,
    cp: "",
    ck: "",
    hd: "",
    vc: checks,
    path: val.path,
    g: val.g,
    t: val.t,
    d: val.d,
  };
  if (shouldLink) {
    B_linkVar(val, nextVal);
  }
  return nextVal;
}

// Lazy-allocate helper for mutating an existing val (as opposed to
// building a local array and passing it through `refine`).
export const B_pushCheck = (val: Val, check: Check): void => {
  if (val.vc !== undefined) {
    val.vc.push(check);
  } else {
    val.vc = [check];
  }
}

// Applies both refiners. Input checks push onto valInput.checks
// (emit at pre-transform slot); output checks wrap val via refine.
// When valInput.prev is None, input checks fold into the output
// wrap so emit has a prev.var(). Sets isOutput on the result.
export const B_markOutput = (val: Val, valInput: Val): Val => {
  let deferredInputChecks: Check[] | undefined;
  const inputRefiner = valInput.e.inputRefiner;
  if (inputRefiner !== undefined) {
    const checks = inputRefiner(valInput);
    if (checks.length > 0) {
      if (valInput.prev !== undefined) {
        for (let i = 0; i < checks.length; i++) {
          B_pushCheck(valInput, checks[i]!);
        }
        deferredInputChecks = undefined;
      } else {
        deferredInputChecks = checks;
      }
    } else {
      deferredInputChecks = undefined;
    }
  } else {
    deferredInputChecks = undefined;
  }

  let outputChecks: Check[] | undefined;
  const refiner = val.e.refiner;
  if (refiner !== undefined) {
    const checks = refiner(val);
    outputChecks = checks.length > 0 ? checks : undefined;
  } else {
    outputChecks = undefined;
  }

  let result: Val;
  if (deferredInputChecks !== undefined && outputChecks !== undefined) {
    result = B_refine(val, undefined, deferredInputChecks.concat(outputChecks));
  } else if (deferredInputChecks !== undefined) {
    result = B_refine(val, undefined, deferredInputChecks);
  } else if (outputChecks !== undefined) {
    result = B_refine(val, undefined, outputChecks);
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
    const pathAppend = pathFromInlinedLocation(B_inlineLocation(parent.g, key));
    child.vc!.forEach((check) => {
      B_pushCheck(parent, {
        c: (inputVar) => check.c(inputVar + pathAppend),
        f: check.f,
      });
    });
    child.vc = undefined;
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
  return {
    v: _notVarBeforeValidation,
    i: `${from.v()}[${locationVar}]`,
    f: from.f,
    s:
      schemaAdditionalItems !== undefined && typeof schemaAdditionalItems !== "string"
        ? schemaAdditionalItems
        : unknown,
    e:
      expectedAdditionalItems !== undefined && typeof expectedAdditionalItems !== "string"
        ? expectedAdditionalItems
        : unknown,
    cp: "",
    ck: "",
    hd: "",
    p: from,
    path: pathEmpty,
    g: from.g,
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
  B_addCode(objectVal, B_merge(val));
  objectVal.d![location] = val;
}

export const B_mergeObjectFields = (target: Val, vals: Record<string, Val>): void => {
  for (const location of Object.keys(vals)) {
    B_addObjectField(target, location, vals[location]!);
  }
}

export const B_addKey = (objVal: Val, key: string, value: Val): string => {
  return `${objVal.v()}[${key}]=${value.i}`;
}

export const B_scope = (val: Val): Val => {
  const shouldLink = val.v !== _var;

  // TODO: Simplify bond
  const nextVal: Val = {
    i: val.i,
    s: val.s,
    e: val.e,
    f: flagNone,
    path: val.path,
    g: val.g,
    v: shouldLink ? _bondVar : _var,
    b: val,
    cp: "",
    ck: "",
    hd: "",
    u: false,
    t: false,
    io: val.io,
    d: val.d, // See the aliasing note on B_next's `d: prev.d` above.
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
  const failure = `${B_failWithArg(
    output,
    (e: unknown) => B_makeInvalidConversionDetails(input, unknown, e),
    `x`
  )}`;
  // Feed the transform the input's var when it already carries checks — it's
  // materialized into a var anyway (the check references it), so reuse it
  // instead of re-inlining the source expression (e.g. `i["x"]`) twice.
  output.cp = `let ${outputVar};try{${outputVar}=${embeddedFn}(${
    input.vc ? input.v() : input.i
  })${isAsync ? `.catch(x=>${failure})` : ""}}catch(x){${failure}}`;
  return output;
}

export const B_effectCtx = (input: Val): EffectCtx => {
  return {
    fail: (message: string, path: Path = pathEmpty): never => {
      const error = new SuryError(
        B_invalidInputBuilder(undefined, path, message, false)(input)(void 0)
      );
      // Read about this in shouldPrependPathKey comment.
      (error as unknown as Record<string, unknown>)[shouldPrependPathKey] = 1;
      throw error;
    },
  };
}

export const B_invalidOperation = (val: Val, description: string): never => {
  return B_throw({ code: "invalid_operation", reason: description, path: val.path });
}

const B_mergeWithCatch = (
  val: Val,
  catchFn: (errorVar: string) => string,
  appendSafe?: () => string
): Code => {
  const valCode = B_collapseMerge(B_merge(val));
  if (
    valCode === "" &&
    // FIXME: Instead of this wrap all S.transform in a try/catch
    !flagUnsafeHas(val.f, valFlagAsync)
  ) {
    // valCode is a collapsed (sealed) "" here, so plain string concat keeps
    // downstream emptiness checks cheap.
    return (valCode as string) + (appendSafe !== undefined ? appendSafe() : "");
  } else {
    const errorVar = B_varWithoutAllocation(val.g);

    const catchCode = `${catchFn(errorVar)};throw ${errorVar}`;

    if (flagUnsafeHas(val.f, valFlagAsync)) {
      val.i = `${val.i}.catch(${errorVar}=>{${catchCode}})`;
    }
    return [
      "try{",
      valCode,
      appendSafe !== undefined ? appendSafe() : "",
      `}catch(${errorVar}){${catchCode}}`,
    ];
  }
}

export const B_mergeWithPathPrepend = (
  val: Val,
  parent: Val,
  locationVar?: string,
  appendSafe?: () => string
): Code => {
  if (val.path === pathEmpty && locationVar === undefined) {
    return B_merge(val);
  } else {
    return B_mergeWithCatch(
      val,
      (errorVar) =>
        `${errorVar}.path=${
          parent.path === "" ? "" : `${inlinedValueFromString(parent.path)}+`
        }${locationVar !== undefined ? `'["'+${locationVar}+'"]'+` : ""}${errorVar}.path`,
      appendSafe
    );
  }
}

export function noopOperation(i: unknown): unknown {
  return i;
}
(noopOperation as unknown as Record<string, unknown>)["embedded"] = immutableEmptyArray;
// TODO: Split validation code and transformation code
