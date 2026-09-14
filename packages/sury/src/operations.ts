// The operation surface: turning schemas into callables.
//
// Every operation names its outcome in its own name (`OrThrow`, `AsResult`,
// `AsPromiseOrReject`, `AsResultPromise`, `AsPromisableResult`) and takes any
// of four call forms. The Result outcomes are compiled, not wrapped: the tail
// that builds `{success, value, error}` is emitted into the operation's own
// body, which is what lets a schema that provably cannot throw skip the `try`
// entirely - a decision no `safe(() => ...)` wrapper can make.
//
// Deliberately free of top-level side effects, and deliberately NOT the module
// that installs the schema prototype's interop getters (standard.ts): a bundle
// reaching a module carries its top-level statements, and an operation must not
// drag the Standard Schema machinery in with it.

import {
  type Flag,
  initSchema,
  type Internal,
  isOwnSchema,
  panic,
  panicNotSchema,
  U,
  undefinedTag
} from "./base";
import {
 B_embedErrorOf,
 B_varWithoutAllocation,
 operationArgVar
} from "./builder";
import {
 literalDecoder
} from "./primitives";
import {
 __setTail,
 getOp,
 reverse,
 type Tail,
 throwTail
} from "./parse";
import {
 compileIsEqual,
 type IsEqual
} from "./eq";

// The `undefined` sentinel an assert/validate operation decodes to: the value
// runs the whole pipeline and the result is dropped.
export const assertResult: Internal = /* @__PURE__ */ initSchema(undefinedTag, literalDecoder, (s) => {
  s.const = U;
  s.noValidation = true;
});

// ── Operation tail ───────────────────────────────────────────────────────────
//
// The tail every operation compiles once a mode beyond "the value, or a throw"
// has been reached. Plain throw mode still delegates to `throwTail`, so the
// throw path's generated code is byte-for-byte what it always was.

// The two Result shapes: 128 the JS `Result`, 256 ReScript's
// `result<'value, S.error>`. The Standard Schema shape (1024) is emitted by
// `throwTail` instead - see the comment there. Neither bit means the value is
// its own answer, which is `is`'s `true`.
//
// The JS pair carries the same keys in the same order - `void 0` in the slot
// the branch doesn't use - so the two branches share one hidden class and a
// consumer's `.success`/`.value` reads stay monomorphic. It is also what makes
// `const { value, error } = result` narrow on the TS side (the `?: undefined`
// sibling fields in `Result`): one decision, both halves.
const okResult = (flag: Flag, value: string): string =>
  flag & 256
    ? `{TAG:"Ok",_0:${value}}`
    : flag & 128
      ? `{success:true,value:${value},error:void 0}`
      : value;
// The bare `false` is `is`'s answer; nothing else reaches this without a
// Result shape to fill.
const errResult = (flag: Flag, e: string): string =>
  flag & 256
    ? `{TAG:"Error",_0:${e}}`
    : flag & 128
      ? `{success:false,value:void 0,error:${e}}`
      : "false";

const operationTail: Tail = (input, code, out, isAsync, flag, hasDefs) => {
  // 2048 (`makeInput`/`makeOutput`) hands back the value it was given. The
  // operation's parameter still is that value unless the body assigned to it
  // (`g.r`: a union rebinds it while dispatching, and `return i` would answer
  // with the encoded form), in which case it is bound before the body.
  let value = out;
  if (flag & 2048) {
    if (input.g.r) {
      value = B_varWithoutAllocation(input.g);
      code = `let ${value}=${operationArgVar};${code}`;
    } else {
      value = operationArgVar;
    }
  }
  // A promise is only produced for the async flag; the promisable mode (512)
  // asks for the value's own shape instead. `hasDefs` says the compile is
  // NESTED (recursive.ts is the only caller that passes defs), and a nested
  // operation stays throwing: the generated code around it prepends the path
  // on the way out, and has to reach the value's failure before the promise
  // does.
  const toPromise = !!(flag & 1) && !(flag & 512) && !hasDefs;
  // No answer of its own for a failure - the exception still is the answer, so
  // `throwTail` still decides the identity case and the promise lift.
  if (!(flag & (128 | 256 | 4096))) {
    const body = throwTail(
      input,
      code,
      flag & 2048 && isAsync ? `${out}.then(()=>${value})` : value,
      isAsync,
      flag,
      hasDefs,
    );
    // A promise-returning operation must not throw synchronously: a value that
    // fails its type check before the first await rejects the same way one
    // that fails after it does, so `OrReject` is the whole story its name
    // tells. The raise counter says whether anything merged can throw at all.
    //
    // Safe to decide here rather than from a flag bit, even though the tail is
    // registered globally: EVERY operation carrying the async flag goes through
    // `tailDispatch`, so none of them can be compiled before this emitter is in
    // place. 512 is also the bit that keeps the Standard Schema tail out -
    // 1024 is only ever compiled with 512 (standard.ts) - where `throwTail`
    // already emitted its own `try`.
    if (!body || !toPromise || !input.g.t) return body;
    const e = B_varWithoutAllocation(input.g);
    return `try{${body}}catch(${e}){return Promise.reject(${e})}`;
  }
  const errVar = B_varWithoutAllocation(input.g);
  // 4096 (`isInput`/`isOutput`) answers `true`; 2048 already picked its value.
  const valueVar = isAsync ? B_varWithoutAllocation(input.g) : value;
  const success = okResult(flag, flag & 4096 ? "true" : flag & 2048 ? value : valueVar);
  // Every failure comes back as a SuryError (`B_errorOf`), so the Result's
  // `error` is one shape; `is` only needs the fact of it.
  const failure = errResult(flag, flag & 4096 ? "" : `${B_embedErrorOf(input)}(${errVar})`);
  const body = isAsync
    ? // Inlined into the promise chain the operation already builds, rather
      // than wrapped around it.
      `${code}return ${out}.then(${valueVar}=>(${success}),${errVar}=>(${failure}))`
    : `${code}return ${toPromise ? `Promise.resolve(${success})` : success}`;
  // The raise counter: when nothing merged can throw, the operation needs no
  // `try` at all - the decision a `safe(() => ...)` wrapper can never make.
  // A failure the sync phase raises has to come back in the shape the success
  // path uses, so an async operation's answer is a promise either way: the
  // consumer sees one shape whether the value died before the first await or
  // after it.
  return input.g.t
    ? `try{${body}}catch(${errVar}){return ${
        isAsync || toPromise ? `Promise.resolve(${failure})` : failure
      }}`
    : body;
};

// ── Call-form dispatch ───────────────────────────────────────────────────────

// The four call forms, told apart by `arguments.length` and by which arguments
// are Sury schemas:
//
//   op(s...)        → the compiled operation (curried / data-last)
//   op(s..., data)  → immediate, schema-first
//   op(data, s...)  → immediate, data-first
//
// Three schemas is the ceiling - it is ReScript's ~from/~via/~to; a longer
// chain is written with `.with(S.to, ...)`.
//
// Only the argument count partitions the forms: an argument is NEVER tested
// for `undefined`, or `op(S.void, undefined)` (arity 2, a trailing non-schema,
// so a parse of `undefined`) would be misread as the compiled form of
// `op(S.void)`.
//
// Accepted and documented: `op(s1, s2)` always reads as a chain, so parsing a
// Sury schema *as data* is only available compiled - `S.parseOrThrow(Meta)(s)`.
// No argument order makes both reachable.
const panicArity = (): never =>
  panic("Expected at most 3 schemas and a value. Use .with(S.to, ...) for a longer chain");

const dispatch = (
  n: number,
  a: unknown,
  b: unknown,
  c: unknown,
  d: unknown,
  tail: Internal | undefined,
  rev: boolean,
  flag: Flag,
): unknown => {
  if (n > 4) panicArity();
  let data: unknown = U;
  // The data is only ever the first or the last argument. Taking a leading one
  // out of the way first leaves the slots below holding the chain alone, so
  // every form past this point is the same walk over `a, b, c`.
  let immediate = !isOwnSchema(a);
  if (immediate) {
    (data = a), (a = b), (b = c), (c = d), n--;
    if (n < 1 || !isOwnSchema(a)) panicNotSchema();
  }
  // How many leading slots hold schemas, 1 to 3: the chain's own length.
  // Counted rather than inferred from `b !== U`, or a hole
  // (`op(s, undefined, s)`) would read as a two-schema chain.
  const k = n > 1 && isOwnSchema(b) ? (n > 2 && isOwnSchema(c) ? 3 : 2) : 1;
  if (n > k) {
    // One argument past the chain is the data. Anything else is a foreign
    // Standard Schema, a hole, or a value in the middle (`op(s, data, s)`) -
    // the one misplacement the argument count can't tell apart from a hole.
    // A schema in that slot is a fourth chain member instead.
    if (immediate || n > k + 1) panic("Expected a Sury schema. The data goes first or last");
    data = k > 2 ? (isOwnSchema(d) ? panicArity() : d) : k > 1 ? c : b;
    immediate = true;
  }
  // Flag bit 8 says the operation accepts anything, so the chain's source is
  // `S.unknown` rather than its own head (`parse`, and the check/make families;
  // `decode`/`encode` start from their first schema argument). It is a flag
  // rather than an `S.unknown` schema spliced in front of the chain: that node
  // cost a `copySchema` and a dead pass through `noopDecoder` per compile, and a
  // slot in every one of those operations' cache keys - which is what let `getOp`
  // drop from five schema slots to four.
  // `rev` reverses the first argument, which is what makes an operation run the
  // encode direction. `tail` closes the chain: `assertResult` for the families
  // that validate and discard (`is`, `assert`, `make`).
  const first = (rev ? reverse(a as Internal) : a) as Internal;
  // The chain, in order: the caller's schemas, then the tail. `getOp` reads
  // exactly as many positional arguments as it is told, so the tail has to sit
  // right after the last schema - which is why each arity is written out with
  // its own count rather than padded with `U`; it also allocates nothing on a
  // cache hit.
  const op =
    k > 2
      ? getOp(flag, tail ? 4 : 3, first, b as Internal, c as Internal, tail)
      : k > 1
        ? getOp(flag, tail ? 3 : 2, first, b as Internal, tail)
        : getOp(flag, tail ? 2 : 1, first, tail);
  return immediate ? op(data) : op;
};

// Every operation whose tail is more than "the value, or a throw" comes through
// here, so the emitter is registered on first use. It can't be registered at
// this module's top level: that is a side effect, and a bundle that reaches
// this module for `parseOrThrow` would then carry the emitter too (parse.ts,
// `__setTail`).
const tailDispatch = (
  n: number,
  a: unknown,
  b: unknown,
  c: unknown,
  d: unknown,
  tail: Internal | undefined,
  rev: boolean,
  flag: Flag,
): unknown => {
  __setTail(operationTail);
  return dispatch(n, a, b, c, d, tail, rev, flag);
};

const makeDispatch = (
  n: number,
  a: unknown,
  b: unknown,
  tail: Internal | undefined,
  rev: boolean,
  flag: Flag,
): unknown =>
  n > 2 || (n === 2 && isOwnSchema(a) && isOwnSchema(b))
    ? panic("Expected a single schema and a value. Chain with .with(S.to, ...) first")
    : tailDispatch(n, a, b, U, U, tail, rev, flag);

// ── Operations ───────────────────────────────────────────────────────────────
//
// NEVER annotate one of these `@__NO_SIDE_EFFECTS__`. The immediate call forms
// execute, and a validation-only call discards its result
// (`S.parseOrThrow(User, data)` used as a check) - esbuild drops an annotated
// pure call whose result is unused, which would silently delete the validation.
// tests/treeShaking_test.ts holds the matching `EFFECTFUL` entries.
//
// One declaration each, never `export const parseAsResult = makeOp(...)`: a
// factory call at the top level is a side effect that no bundler will shake.
// And `function`, not an arrow, against the house style: the call form is
// read off `arguments.length`, which an arrow doesn't have.
//
// The flag literals are the return modes documented in base.ts. Outcome:
// 0 OrThrow · 128 AsResult (256 for the ReScript shape) · 1 AsPromiseOrReject ·
// 1|128 AsResultPromise · 1|128|512 AsPromisableResult. Family: 2048 yield
// the operation's input (`make*`) · 4096 answer a boolean (`is*`).

export function parseOrThrow(a?: unknown, b?: unknown, c?: unknown, d?: unknown): unknown {
  return dispatch(arguments.length, a, b, c, d, U, false, 8);
}

export function parseAsResult(a?: unknown, b?: unknown, c?: unknown, d?: unknown): unknown {
  return tailDispatch(arguments.length, a, b, c, d, U, false, 8 | 128);
}

export function parseAsPromiseOrReject(a?: unknown, b?: unknown, c?: unknown, d?: unknown): unknown {
  return tailDispatch(arguments.length, a, b, c, d, U, false, 1 | 8);
}

export function parseAsResultPromise(a?: unknown, b?: unknown, c?: unknown, d?: unknown): unknown {
  return tailDispatch(arguments.length, a, b, c, d, U, false, 1 | 8 | 128);
}

// The Result outcome without committing to a shape: a synchronous schema
// answers with the Result itself, an async one with a promise of it. One
// compile covers both, so a caller who doesn't know a schema's async-ness
// doesn't have to lift every answer into a promise to find out.
export function parseAsPromisableResult(a?: unknown, b?: unknown, c?: unknown, d?: unknown): unknown {
  return tailDispatch(arguments.length, a, b, c, d, U, false, 1 | 8 | 128 | 512);
}

export function decodeOrThrow(a?: unknown, b?: unknown, c?: unknown, d?: unknown): unknown {
  return dispatch(arguments.length, a, b, c, d, U, false, 0);
}

export function decodeAsResult(a?: unknown, b?: unknown, c?: unknown, d?: unknown): unknown {
  return tailDispatch(arguments.length, a, b, c, d, U, false, 128);
}

export function decodeAsPromiseOrReject(a?: unknown, b?: unknown, c?: unknown, d?: unknown): unknown {
  return tailDispatch(arguments.length, a, b, c, d, U, false, 1);
}

export function decodeAsResultPromise(a?: unknown, b?: unknown, c?: unknown, d?: unknown): unknown {
  return tailDispatch(arguments.length, a, b, c, d, U, false, 1 | 128);
}

export function decodeAsPromisableResult(a?: unknown, b?: unknown, c?: unknown, d?: unknown): unknown {
  return tailDispatch(arguments.length, a, b, c, d, U, false, 1 | 128 | 512);
}

// Only the first schema is reversed: `S.encodeOrThrow(a, ...rest)` starts from
// a's Output and runs the rest of the chain forward, so a pipeline after the
// reversed schema is written exactly as in `decode`. Compare
// `S.decodeOrThrow(S.reverse(a), ...rest)`, which is its longhand.
export function encodeOrThrow(a?: unknown, b?: unknown, c?: unknown, d?: unknown): unknown {
  return dispatch(arguments.length, a, b, c, d, U, true, 0);
}

export function encodeAsResult(a?: unknown, b?: unknown, c?: unknown, d?: unknown): unknown {
  return tailDispatch(arguments.length, a, b, c, d, U, true, 128);
}

export function encodeAsPromiseOrReject(a?: unknown, b?: unknown, c?: unknown, d?: unknown): unknown {
  return tailDispatch(arguments.length, a, b, c, d, U, true, 1);
}

export function encodeAsResultPromise(a?: unknown, b?: unknown, c?: unknown, d?: unknown): unknown {
  return tailDispatch(arguments.length, a, b, c, d, U, true, 1 | 128);
}

export function encodeAsPromisableResult(a?: unknown, b?: unknown, c?: unknown, d?: unknown): unknown {
  return tailDispatch(arguments.length, a, b, c, d, U, true, 1 | 128 | 512);
}

// The make family validates and hands back the value it was given, rather than
// the decoded clone `parse` would build: the checks run, their result is
// discarded, and the value keeps its identity. One schema only: a chain is
// `.with(S.to, ...)` on the schema first.
export function makeInputOrThrow(a?: unknown, b?: unknown, c?: unknown, d?: unknown): unknown {
  return makeDispatch(arguments.length, a, b, assertResult, false, 8 | 2048);
}

export function makeInputAsResult(a?: unknown, b?: unknown, c?: unknown, d?: unknown): unknown {
  return makeDispatch(arguments.length, a, b, assertResult, false, 8 | 128 | 2048);
}

export function makeInputAsPromiseOrReject(a?: unknown, b?: unknown, c?: unknown, d?: unknown): unknown {
  return makeDispatch(arguments.length, a, b, assertResult, false, 1 | 8 | 2048);
}

export function makeInputAsResultPromise(a?: unknown, b?: unknown, c?: unknown, d?: unknown): unknown {
  return makeDispatch(arguments.length, a, b, assertResult, false, 1 | 8 | 128 | 2048);
}

export const makeInputAsPromisableResult = function (
  a?: unknown,
  b?: unknown,
  c?: unknown,
  d?: unknown,
): unknown {
  return makeDispatch(arguments.length, a, b, assertResult, false, 1 | 8 | 128 | 512 | 2048);
}

export function makeOutputOrThrow(a?: unknown, b?: unknown, c?: unknown, d?: unknown): unknown {
  return makeDispatch(arguments.length, a, b, assertResult, true, 8 | 2048);
}

export function makeOutputAsResult(a?: unknown, b?: unknown, c?: unknown, d?: unknown): unknown {
  return makeDispatch(arguments.length, a, b, assertResult, true, 8 | 128 | 2048);
}

export function makeOutputAsPromiseOrReject(a?: unknown, b?: unknown, c?: unknown, d?: unknown): unknown {
  return makeDispatch(arguments.length, a, b, assertResult, true, 1 | 8 | 2048);
}

export function makeOutputAsResultPromise(a?: unknown, b?: unknown, c?: unknown, d?: unknown): unknown {
  return makeDispatch(arguments.length, a, b, assertResult, true, 1 | 8 | 128 | 2048);
}

export const makeOutputAsPromisableResult = function (
  a?: unknown,
  b?: unknown,
  c?: unknown,
  d?: unknown,
): unknown {
  return makeDispatch(arguments.length, a, b, assertResult, true, 1 | 8 | 128 | 512 | 2048);
}

// ── Checks ───────────────────────────────────────────────────────────────────
//
// Direction is a free parameter here - there is no value to read it off - so
// it is spelled out and mandatory. `is*` answers a boolean and never throws;
// `is*AsPromise` resolves to one and never rejects.

export function isInput(a?: unknown, b?: unknown, c?: unknown, d?: unknown): unknown {
  return tailDispatch(arguments.length, a, b, c, d, assertResult, false, 8 | 4096);
}

export function isOutput(a?: unknown, b?: unknown, c?: unknown, d?: unknown): unknown {
  return tailDispatch(arguments.length, a, b, c, d, assertResult, true, 8 | 4096);
}

export function isInputAsPromise(a?: unknown, b?: unknown, c?: unknown, d?: unknown): unknown {
  return tailDispatch(arguments.length, a, b, c, d, assertResult, false, 1 | 8 | 4096);
}

export function isOutputAsPromise(a?: unknown, b?: unknown, c?: unknown, d?: unknown): unknown {
  return tailDispatch(arguments.length, a, b, c, d, assertResult, true, 1 | 8 | 4096);
}

// Equality is the one operation that takes a PAIR, so it has its own dispatch:
// the schema is either the first argument or the last, never in the middle,
// and there is no chain to walk. Arity partitions the forms exactly as above.
// `isEqualInput(S.void)` (arity 1) is the compiled form, and the comparison of
// two absent values is `isEqualInput(S.void, undefined, undefined)` (arity 3).
//
// Value-last exists for ReScript, whose labelled `~schema` compiles to a
// trailing positional argument.
const eqDispatch = (n: number, a: unknown, b: unknown, c: unknown, rev: boolean): unknown => {
  const schema = (n === 1 || isOwnSchema(a) ? a : c) as Internal;
  if (!isOwnSchema(schema) || (n !== 1 && n !== 3)) return panicNotSchema();
  const compiled: IsEqual = compileIsEqual(rev ? reverse(schema) : schema);
  return n === 1 ? compiled : schema === a ? compiled(b, c) : compiled(a, b);
};

export function isEqualInput(a?: unknown, b?: unknown, c?: unknown): unknown {
  return eqDispatch(arguments.length, a, b, c, false);
}

export function isEqualOutput(a?: unknown, b?: unknown, c?: unknown): unknown {
  return eqDispatch(arguments.length, a, b, c, true);
}

export function assertInputOrThrow(a?: unknown, b?: unknown, c?: unknown, d?: unknown): unknown {
  return dispatch(arguments.length, a, b, c, d, assertResult, false, 8);
}

export function assertOutputOrThrow(a?: unknown, b?: unknown, c?: unknown, d?: unknown): unknown {
  return dispatch(arguments.length, a, b, c, d, assertResult, true, 8);
}

export const assertInputAsPromiseOrReject = function (
  a?: unknown,
  b?: unknown,
  c?: unknown,
  d?: unknown,
): unknown {
  return tailDispatch(arguments.length, a, b, c, d, assertResult, false, 1 | 8);
}

export const assertOutputAsPromiseOrReject = function (
  a?: unknown,
  b?: unknown,
  c?: unknown,
  d?: unknown,
): unknown {
  return tailDispatch(arguments.length, a, b, c, d, assertResult, true, 1 | 8);
}

// ── ReScript result surface ──────────────────────────────────────────────────
//
// JS `Result` is `{success, value, error}`; ReScript's `result<'value, S.error>`
// is `{TAG, _0}`. Two shapes, one compiler (256 instead of 128), so
// these are legitimate `$` exports: a ReScript-only result shape has no public
// JS equivalent. The ReScript tail ships only to bundles importing S.res.mjs,
// so the two shapes tree-shake independently.

export function $parseAsResult(a?: unknown, b?: unknown, c?: unknown, d?: unknown): unknown {
  return tailDispatch(arguments.length, a, b, c, d, U, false, 8 | 256);
}

export function $parseAsResultPromise(a?: unknown, b?: unknown, c?: unknown, d?: unknown): unknown {
  return tailDispatch(arguments.length, a, b, c, d, U, false, 1 | 8 | 256);
}

export function $encodeAsResult(a?: unknown, b?: unknown, c?: unknown, d?: unknown): unknown {
  return tailDispatch(arguments.length, a, b, c, d, U, true, 256);
}

export function $encodeAsResultPromise(a?: unknown, b?: unknown, c?: unknown, d?: unknown): unknown {
  return tailDispatch(arguments.length, a, b, c, d, U, true, 1 | 256);
}

export function $makeAsResult(a?: unknown, b?: unknown, c?: unknown, d?: unknown): unknown {
  return makeDispatch(arguments.length, a, b, assertResult, true, 8 | 256 | 2048);
}

export function $makeAsResultPromise(a?: unknown, b?: unknown, c?: unknown, d?: unknown): unknown {
  return makeDispatch(arguments.length, a, b, assertResult, true, 1 | 8 | 256 | 2048);
}
