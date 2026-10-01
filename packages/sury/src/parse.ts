import {
  type BGlobal,
  anyOfTag,
  baseSchema,
  type Builder,
  configurableValueOptions,
  copySchema,
  getOrRethrow,
  type Encoder,
  type Flag,
  globalConfig,
  initSchema,
  inputExpression,
  instanceTag,
  type Internal,
  isLiteral,
  neverTag,
  numberTag,
  objectTag,
  panic,
  reversedKey,
  s,
  schemaPrototype,
  setHas,
  stringTag,
  type SuryErrorRecord,
  tagFlags,
  U,
  unknown,
  undefinedTag,
  unknownTag,
  updateOutput,
  type Val,
  valKey,
  valueOptions
} from "./base";
import {
  B_detached,
  B_failInvalidInput,
  B_embedPure,
  B_errorOf,
  B_inlineConst,
  B_let,
  B_markOutput,
  B_merge,
  B_next,
  B_operationArg,
  B_refine,
  B_scope,
  type Settled,
  settledTag,
  B_unsupportedDecode,
  B_varWithoutAllocation,
  failInvalidType,
  noopOperation,
  operationArgVar
} from "./builder";
import {
  instanceofCond,
  isArrayCond,
  nanCond,
  numberTagCond,
  objectTagCond,
  typeofCond
} from "./primitives";

export const parse = (input: Val): Val => {
  let result: Val = input;
  let appliedEncoderRef: Encoder | undefined = U;
  let loopCount = 0;
  while (!result.io || result.e.to) {
    const appliedEncoder: Encoder | undefined = appliedEncoderRef;
    appliedEncoderRef = U;
    const loopInput = result;

    if (++loopCount > 50) panic("Loop count exceeded 50");

    const defs = loopInput.e["$defs"];
    // Copied, never adopted: a second `$defs` in the same operation - two
    // independent `S.recursive` schemas in one object - would otherwise merge
    // into the first schema's own record and leave it holding definitions that
    // are not its own for the rest of the program. Null prototype because the
    // keys are the names the caller gave `S.recursive`.
    if (defs) loopInput.g.d = Object.assign(loopInput.g.d || Object.create(null), defs);

    // The val is a promise, so the rest of the chain has to run inside a
    // `.then`. The flag alone is the right guard: a second condition could only
    // have been "and there is something to wrap", which is not knowable before
    // parsing the remainder - so the decision lives below, where the recursive
    // parse has already answered it, and an empty remainder refines instead of
    // wrapping. Across the spec corpus the no-wrap arm is reached by exactly one
    // shape, `S.file.with(S.to, S.uint8Array)`, where reading the file IS the
    // whole operation.
    if (loopInput.f & 1) {
      const operationInputVar = loopInput.v();
      const operationInput = B_scope(loopInput);
      let operationOutput!: Val;
      const operationCode = B_detached(loopInput.g, () =>
        B_merge((operationOutput = parse(operationInput))),
      );
      result =
        operationInput.i !== operationOutput.i || operationCode !== ""
          ? B_next(
              loopInput,
              `${operationInputVar}.then(${operationInputVar}=>{${operationCode}return ${operationOutput.i}})`,
              operationOutput.s,
              operationOutput.e,
            )
          : B_refine(loopInput, operationOutput.s, U, operationOutput.e);
      result.f |= 1;
      result.io = true;
    } else if (loopInput.io) {
      const to = loopInput.e.to!;
      result = loopInput.e.parser ? loopInput.e.parser(loopInput) : B_refine(result, U, U, to);
    } else {
      const maybeEncoder = loopInput.s.encoder;
      if (
        maybeEncoder &&
        maybeEncoder !== appliedEncoder &&
        loopInput.s !== loopInput.e &&
        loopInput.e.type !== unknownTag &&
        // A `noValidation` target takes the value as it stands when it is a
        // whole document (`S.json`, whose parse is the only check it has) or
        // when the operation discards it anyway (S.assertInputOrThrow's `undefined` result
        // sentinel). Every other such target still gets its conversion:
        // `noValidation` drops the checks, not the re-representation.
        !(loopInput.e.noValidation && (loopInput.e.flags & 16 || loopInput.e.type === undefinedTag))
      ) {
        result = maybeEncoder(loopInput, loopInput.e);
      }

      // If encoder didn't change the value, we can decode it,
      // otherwise let's start the loop from the beginning
      if (loopInput !== result) appliedEncoderRef = maybeEncoder!;
      else {
        result = loopInput.e.decoder(loopInput);
        // Primitive decoder (no internal transforms): apply refiners here.
        // Advanced decoders set isOutput themselves and own refiner application.
        if (!result.io) result = B_markOutput(result, result);
      }
    }
  }

  return result;
}

// A Sury failure is a record until it crosses into user code, and there are
// exactly three places where it does: the compiled operation, the promise an
// async one hands back (`rejectionBoundary`), and the compile itself. All three
// attach the stack here.
//
// `cut` is the frame the trace starts *after* - the operation for a raise while
// it runs, `getOp` for one while it is built. Without it the top frames are the
// compiler walking down to the value or the schema that failed, which is ten of
// them at the build boundary: more than `Error.stackTraceLimit` allows, so the
// line the caller actually wrote falls off the end of its own stack trace.
//
// Ours and stack-free only. `"stack" in` rather than reading it: on an error
// that has one the read formats the trace, and anything arriving with a stack -
// a foreign exception, one user code built with `new S.Error` and threw - is
// already pointing at a better line than this.
//
// `captureStackTrace` is V8's, and `cut` is the whole reason to prefer it: only
// it can drop the frames between the raise and the caller. Every other engine
// gets the stack a throwaway `Error` was born with, which is the same trace
// with this function and the boundary's own frames still on top - worse than
// V8's and better than the nothing an engine without the API used to get.
// Counting those frames off would couple this to the call depth of whichever
// boundary reaches it, so they stay.
//
// `defineProperty` rather than an assignment, because `stack` is not part of
// what a failure reads back as: V8 writes a non-enumerable one, and a plain
// write here would put it in `Object.keys(error)` on those engines alone.
//
// Hands back what it was given, so each boundary reads as the one expression
// it rethrows or rejects with.
const captureStackAt = (thrown: SuryErrorRecord, cut: unknown): SuryErrorRecord => {
  if (thrown && thrown.s === s && !("stack" in thrown)) {
    const capture = (
      Error as unknown as { captureStackTrace?: (target: object, cut: unknown) => void }
    ).captureStackTrace;
    if (capture) capture(thrown, cut);
    else
      Object.defineProperty(thrown, "stack", {
        value: new Error().stack,
        configurable: true,
        writable: true,
      });
  }
  return thrown;
};

// A failure an async operation reaches after its first await rejects the
// promise the operation handed back, out of reach of the `try` it runs in. This
// is that promise's handler, and its own cut. A microtask calls it, so all that
// sits below is the runtime's microtask pump, then the caller as a frame V8
// threads back through its `await`. A caller that chained a bare `.catch` has
// no such frame, and its stack names nothing it wrote - but it still has one,
// as every failure that reaches user code by a throw or a rejection does.
const rejectionBoundary = (thrown: SuryErrorRecord): never => {
  throw captureStackAt(thrown, rejectionBoundary);
};

// The throwing outcomes: the value, a promise of it, or a raise. `undefined`
// means no body at all - the operation is the identity, and the caller hands
// back `noopOperation`.
const throwTail = (
  input: Val,
  code: string,
  out: string,
  isAsync: boolean,
  flag: Flag,
  hasDefs: boolean,
): string | undefined => {
  if (code === "" && out === operationArgVar && !(flag & 1)) return U;
  // An appended hop rather than a second argument to whatever `.then` built the
  // value: that argument would never see a failure raised inside the `.then`
  // itself, which is where an async value's own checks run.
  const body = `${code}return ${
    (flag & 1) && !isAsync && !hasDefs
      ? `Promise.resolve(${out})`
      : isAsync && !hasDefs && input.g.t
        ? `${out}.catch(${B_embedPure(input, rejectionBoundary)})`
        : out
  }`;
  // The run boundary, cutting at the operation itself so the caller's line ends
  // up on top instead of five frames of library. Only where something can raise
  // at all (`g.t`) - or, for a promise-returning operation, wherever the body
  // reads the value, since a getter's throw has to reject too - and never for a
  // nested compile (recursive.ts), whose throw is generated code's own business
  // and is caught and re-raised by the operation around it.
  //
  // The sync phase only. What an async operation raises after its first await
  // is `rejectionBoundary`'s.
  if (!(input.g.t || (flag & 1 && !(flag & 512) && code)) || hasDefs) return body;
  const g = input.g;
  const e = B_varWithoutAllocation(g);
  // A promise-returning operation must not throw synchronously: a value that
  // fails before the first await rejects the same way one that fails after it
  // does, so `OrReject` is the whole story its name tells. The promisable mode
  // (512) answers in the value's own shape instead, and throws. Lifted in this
  // one `catch`, with the stack taken on the way, rather than rethrown for a
  // second `try` around it to catch.
  return `try{${body}}catch(${e}){${
    flag & 1 && !(flag & 512)
      ? `return ${B_embedPure(input, (thrown: SuryErrorRecord) =>
          Promise.reject(captureStackAt(thrown, g.f)),
        )}`
      : B_embedPure(input, (thrown: SuryErrorRecord): never => {
          throw captureStackAt(thrown, g.f);
        })
  }(${e})}`;
};

// The answering outcomes: 128 the JS `Result`, 256 ReScript's
// `result<'value, S.error>`, 4096 `is`'s boolean. `~standard.validate` is the
// promisable JS Result (1|128|512) - a Result IS a Standard Schema result, so
// there is one shape to keep, not two.
//
// The JS pair carries the same keys in the same order - `void 0` in the slot
// the branch doesn't use - so the two branches share one hidden class and a
// consumer's `.success`/`.value` reads stay monomorphic. It is also what makes
// `const { value, error } = result` narrow on the TS side (the `?: undefined`
// sibling fields in `Result`): one decision, both halves.
//
// `issues` is the fourth of those keys, and the one that makes a Result a
// Standard Schema result. It is free where a consumer branches on the Result at
// the call site, which is most of them: nothing outlives the frame, so V8 drops
// the object and the key with it. It costs a store only where the Result is
// kept, and taking it off the success branch to save that splits the hidden
// class and hands the same nanosecond back on the reads. Both halves are in
// specs/scenarios.yaml - `parse-as-result-consumed` against
// `parse-as-result-compiled`, and `result-read`.
const okResult = (flag: Flag, value: string): string =>
  flag & 256
    ? `{TAG:"Ok",_0:${value}}`
    : flag & 128
      ? `{success:true,value:${value},error:void 0,issues:void 0}`
      : value;

// The JS Result reports every failure the body found. They sit on a linked
// list, newest first, each node led by the one before it - `[, record]`, or
// `[, builder, value, path?]` built only now that it is read (union.ts links
// its members' failures the same way). A raise that ended the body comes last.
//
// `message` is the error's `reason` and not its formatted `message`: the
// location is in `path`, which is omitted at the root, as Standard Schema
// consumers expect - a consumer that renders both would say it twice.
const failureOf =
  (errorOf: (e: unknown) => SuryErrorRecord, lift: { a?: boolean }) =>
  (list: unknown[] | undefined, raised?: 1, thrown?: unknown): unknown => {
    // What async children found comes after what the sync phase did, in the
    // order the joins list them (builder.ts `B_join`).
    const late = raised ? ((thrown as Settled | undefined)?.t === settledTag ? (thrown as Settled).l : [thrown]) : [];
    let size = late.length;
    for (let n = list; n; n = n[0] as unknown[] | undefined) size++;
    const issues = new Array(size);
    let error!: SuryErrorRecord;
    const add = (e: SuryErrorRecord) => {
      issues[--size] = { message: e.reason, path: e.path.length ? e.path : U };
      error = e;
    };
    for (let idx = late.length; idx--; ) add(errorOf(late[idx]));
    for (let n = list; n; n = n[0] as unknown[] | undefined)
      add((n.length < 3 ? n[1] : (n[1] as Function)(n[2], n[3])) as SuryErrorRecord);
    const result = { success: false, value: U, error, issues };
    return lift.a ? Promise.resolve(result) : result;
  };

// How one compile of an operation answers: `x` is where a failure the body
// finds goes (`BGlobal.x`), asked before the body is emitted; `t` ends the body
// once it is known.
type Outcome = {
  x: BGlobal["x"];
  t: (code: string, out: string, isAsync: boolean) => string | undefined;
};

const outcomeOf = (input: Val, flag: Flag, hasDefs: boolean): Outcome => {
  const g = input.g;
  // 2048 (`makeInput`/`makeOutput`) hands back the value it was given. The
  // operation's parameter still is that value unless the body assigned to it
  // (`g.r`: a union rebinds it while dispatching, and `return i` would answer
  // with the encoded form), in which case it is bound before the body.
  const given = (code: string): [string, string] => {
    if (!g.r) return [code, operationArgVar];
    const value = B_varWithoutAllocation(g);
    return [B_let(g, value, operationArgVar) + code, value];
  };
  // A nested compile (recursive.ts) answers with its value, so it raises.
  if (!(flag & (128 | 256 | 4096)) || hasDefs)
    return {
      x: U,
      t: (code, out, isAsync) => {
        if (flag & 2048) {
          const [body, value] = given(code);
          return throwTail(input, body, isAsync ? `${out}.then(()=>${value})` : value, isAsync, flag, hasDefs);
        }
        return throwTail(input, code, out, isAsync, flag, hasDefs);
      },
    };
  // A failure the sync phase finds comes back in the shape the success path
  // uses, so an async operation's answer is a promise either way. The
  // promisable mode (512) answers in the body's own shape, known only once the
  // body is, so the JS Result's builder reads it off `lift` when it runs.
  const lift: { a?: boolean } = {};
  const lifted = (value: string) => (flag & 1 && !(flag & 512) ? `Promise.resolve(${value})` : value);
  let x: NonNullable<BGlobal["x"]> & { k?: 1 }, failure: string, list: string | undefined;
  let named = false;
  if (flag & 128) {
    list = g.k = B_varWithoutAllocation(g);
    g.l = [];
    failure = B_embedPure(input, failureOf(B_errorOf(input), lift));
    // Always onto the list: an exit emitted before a collecting child can
    // still run after it, the next time round a loop.
    x = (record) => ((named = true), `return ${failure}(${record ? `[${list},${record(true)}]` : list})`);
    // Tagged so a container knows its children collect (builder.ts `B_field`).
    x.k = 1;
    g.y = () => `return ${failure}(${list})`;
  } else if (flag & 4096) {
    failure = "false";
    const no = `return ${lifted(failure)}`;
    x = () => no;
  } else {
    const errorOf = B_errorOf(input);
    failure = B_embedPure(input, (e: unknown) => ({ TAG: "Error", _0: errorOf(e) }));
    x = (record) => `return ${lifted(`${failure}(${record!()})`)}`;
  }
  const failOf = (e?: string): string =>
    list
      ? `${failure}(${g.kj ? list : 0}${e ? `,1,${e}` : ""})`
      : flag & 4096
        ? failure
        : `${failure}(${e})`;
  // What the sync phase collected fails the operation after all, async or
  // not - it is only read once the body is done.
  const done = (success: string): string => (g.kj ? `${list}?${failOf()}:${success}` : success);
  return {
    x,
    t: (code, out, isAsync) => {
      const errVar = B_varWithoutAllocation(g);
      let value = out;
      if (flag & 2048) [code, value] = given(code);
      lift.a = !!(flag & 1) && (isAsync || !(flag & 512));
      const valueVar = isAsync ? B_varWithoutAllocation(g) : value;
      const success = okResult(flag, flag & 4096 ? "true" : flag & 2048 ? value : valueVar);
      let body = isAsync
        ? `${code}return ${out}.then(${valueVar}=>${done(`(${success})`)},${errVar}=>(${failOf(errVar)}))`
        : `${code}return ${done(lifted(success))}`;
      // An outcome with an answer of its own never throws, and any body can:
      // what it reads may be a getter or a proxy, even where nothing it checks
      // can fail. Only an empty one needs no `try` - the decision a
      // `safe(() => ...)` wrapper can never make.
      if (code)
        body = `try{${body}}catch(${errVar}){return ${list ? failOf(errVar) : lifted(failOf(errVar))}}`;
      const names = g.kj || named ? [list, ...g.l!] : g.l || [];
      return names.length ? `let ${[...new Set(names)]};${body}` : body;
    },
  };
};

export const compileDecoder = (
  schema: Internal,
  expected: Internal,
  flag: Flag,
  defs: Record<string, Internal> | undefined,
  node?: OpNode
): (input: unknown) => unknown => {
  const input = B_operationArg(isLiteral(schema) ? unknown : schema, expected, flag, defs);
  const outcome = outcomeOf(input, flag, !!defs);
  input.g.x = outcome.x;

  const output = parse(input);
  const code = B_merge(output);
  const isAsync = !!(output.f & 1);
  if (node) {
    node.y = isAsync;
    node.t = output.t === true;
  }

  const body = outcome.t(code, output.i, isAsync);
  if (!body) return noopOperation;
  const fn = new Function("e", "s", `return ${operationArgVar}=>{${body}}`)(input.g.e, s);
  fn.embedded = input.g.e;
  // What the throw boundary's stack capture cuts at. Handed over after the
  // fact because the emitter runs before the function it names exists.
  input.g.f = fn;
  return fn;
}
export const getOutputSchema = (schema: Internal): Internal => {
  while (schema.to) schema = schema.to;
  return schema;
}
// The two sides of a schema trade places: what parsed now serializes, what
// refined the input now refines the output. `delete` rather than `= U` because
// `unionIsTransparent` (union.ts) counts a schema's keys, and a key left
// present with an undefined value would stop every union from flattening.
const reverseSwap = (mut: Record<string, unknown>, a: string, b: string): void => {
  const previous = mut[a];
  mut[b] === U ? delete mut[a] : (mut[a] = mut[b]);
  previous === U ? delete mut[b] : (mut[b] = previous);
}

// Null prototype: the keys are user-controlled property names, and assigning
// `__proto__` on a plain `{}` reparents the object instead of adding a key -
// which reparented the reversed property dict onto the property's own schema and
// dropped the key, so `outputExpression` rendered schema internals.
const reverseDict = (dict: Record<string, Internal>): Record<string, Internal> => {
  const reversed: Record<string, Internal> = Object.create(null);
  for (const key in dict) {
    reversed[key] = reverse(dict[key]!);
  }
  return reversed;
}

// The general `reversed` getter: every schema can answer its reverse - the
// self-reverse prototype shadows this with `this`, and a first read here
// computes, then caches both directions as own non-enumerable properties
// (own beats the getter on every later read). Free bundle-wise: `toString`
// above already makes `reverse` unshakeable. Reading `r` therefore has side
// effects - a debugger that expands prototype getters computes the reverse
// and writes the cache; harmless, but not inert.
Object.defineProperty(schemaPrototype, reversedKey, {
  get(this: Internal): Internal {
    const schema = this;
    let reversedHead: Internal | undefined = U;
    let current: Internal | undefined = schema;
    while (current) {
      const mut = copySchema(current!);
      const next = mut.to;
      reversedHead ? (mut.to = reversedHead) : delete mut.to;
      const record = mut as unknown as Record<string, unknown>;
      reverseSwap(record, "parser", "serializer");
      reverseSwap(record, "refiner", "inputRefiner");
      // The link into this node is now the one out of it, read the other way.
      // Only a chain a content schema is part of has a reading anything reads,
      // so the schema that answers it is found on this node, the next, or the
      // next's arms - and a bundle with no content schema ships none of it.
      const reverseReading =
        mut.reverseReading ||
        (next && (next.reverseReading || next.anyOf?.find((arm) => arm.reverseReading)?.reverseReading));
      const flags = (mut.flags & ~12) | (reverseReading ? reverseReading(mut, next) : 0);
      flags ? (mut.flags = flags) : delete record["flags"];
      // Deleted, not parked in a holding field: encode has no absent-input arm,
      // and double reversal reads the cache below rather than re-deriving, so
      // nothing needs the old value back.
      delete record["default"];
      // Examples are stored in their owner's input form, which is this copy's
      // output form; the JSON Schema renderer decodes them back for this side.
      delete record["examples"];
      if (mut.items) mut.items = mut.items.map(reverse);
      if (mut.properties) mut.properties = reverseDict(mut.properties);
      // Skip tuple
      if (typeof mut.additionalItems === objectTag) {
        mut.additionalItems = reverse(mut.additionalItems as Internal);
      }
      if (mut.anyOf) {
        const anyOf = mut.anyOf;
        const has: Record<string, boolean> = {};
        const newAnyOf: Internal[] = [];
        for (let idx = 0; idx < anyOf.length; idx++) {
          const s = anyOf[idx]!;
          const reversed = reverse(s);
          newAnyOf.push(reversed);
          setHas(has, reversed);
        }
        mut.has = has;
        mut.anyOf = newAnyOf;
      }
      if (mut["$defs"]) mut["$defs"] = reverseDict(mut["$defs"]);
      reversedHead = mut;
      current = next;
    }

    // defineProperty (slower, once per schema) keeps the cache non-enumerable:
    // enumerability is load-bearing, not cosmetic - copySchema's Object.assign,
    // optionFactory-style spreads, and unionIsTransparent's field count all walk
    // enumerable fields and must not see it.
    const r = reversedHead!;
    valueOptions[valKey] = r;
    Object.defineProperty(schema, reversedKey, valueOptions as PropertyDescriptor);
    valueOptions[valKey] = schema;
    Object.defineProperty(r, reversedKey, valueOptions as PropertyDescriptor);
    return r;
  },
});

// @__NO_SIDE_EFFECTS__
export const reverse = (schema: Internal): Internal => schema.r!;

// A value a schema stores on its Output side - a default, an example - held to
// it by one parse through `reverse`, which checks it as an Output and hands it
// back in the Input form it is stored in. A never or async encode makes that
// uncomputable, which leaves no Output validation to hold the value to, so
// there is nothing to check: `undefined`.
export const decodeOutput = (output: Internal): ((v: unknown) => unknown) | undefined => {
  try {
    return getOp(0, 2, unknown, output) as (v: unknown) => unknown;
  } catch (exn) {
    if ((getOrRethrow(exn) as unknown as { code: string }).code !== "invalid_operation") throw exn;
  }
}

// The default of `S.optional(x, v)` and `s.fieldOr(_, x, v)` is a value of the
// Output type, written the way the schema outputs it. Not the chain's tail: a
// container keeps its items' transforms inside itself, so its tail is still the
// Input form (#452).
export const setDefault = (owner: Internal, original: Internal, v: unknown): void => {
  let output = reverse(original);
  // `S.recursive`'s definitions, while its definer is still running. A nested
  // `S.recursive` hands back a bare `$ref`, so an item reached inside a definer
  // names definitions the check would not otherwise see. They ride on the copy
  // it compiles against, and `parse` merges them for the whole operation, so a
  // ref inside a union resolves too. Only once the record holds something: an
  // empty one names nothing, and a copy would miss the operation cached on the
  // item itself.
  const building = globalConfig.d;
  if (building !== U && output["$defs"] === U && Object.keys(building).length) {
    output = copySchema(output);
    output["$defs"] = building;
  }
  const decode = decodeOutput(output);
  if (decode) {
    try {
      owner.default = decode(v);
    } catch (exn) {
      panic(
        `Invalid default for ${inputExpression(owner)}: ${
          (getOrRethrow(exn) as unknown as { message: string })["message"]
        }`
      );
    }
  }
}

// Lives here rather than beside `inputExpression` in base.ts so that only the
// consumers who ask for the output side carry `reverse`.
// @__NO_SIDE_EFFECTS__
export const outputExpression = (schema: Internal): string =>
  inputExpression(reverse(schema));

// THE compiled-operation cache: a linked list of nodes on the cache target
// (the newest-seq schema argument) under `memoKey`, newest node first, matched
// by identity-comparing the schema arguments and the resolved flag - no string
// keys, since a key assembled per call is never interned and re-hashes on
// every lookup. Non-enumerable so copySchema's Object.assign can't carry it
// onto a derived schema. Nothing evicts: a `S.global` flag change strands the
// old flag's nodes, and each node pins its argument schemas for the target's
// lifetime - both bounded by the number of distinct (args, flag) operations
// ever asked of the schema.
//
// recursiveDecoder (advanced/recursive.ts) shares this storage; its lookup
// triple (inputSchema, def, flag) is a two-schema node stored on `def`. That
// is why `v` admits 0: a def mid-compilation holds the sentinel so inner
// circular references embed the NODE and call `.v` at runtime - the node
// exists before the function it will hold, and a recompile under corrected
// assumptions overwrites `v` in place. getOp never observes the sentinel:
// a def is only mid-compilation inside a synchronous recursiveDecoder pass,
// and a pass that throws unlinks its node (removeOpNode) on the way out.
export type OpNode = {
  a: Internal[]; // the schema arguments, in order
  f: Flag;
  v: ((from: unknown) => unknown) | 0;
  n: OpNode | undefined; // next (older) node
  // @as("t") - hasTransform, @as("y") - isAsync. Facts about the compiled
  // operation, not about any schema in it: one schema is transforming under
  // one flag and not under another, and two operations sharing a chain must
  // not overwrite each other's answer. `recursiveDecoder` writes and reads
  // them, and needs them mid-compile - which is why they live on the node its
  // circular reference already finds rather than being returned. Left off the
  // literal in `addOpNode`: the lookup walk never reads them, so the shape a
  // recursive compile adds them to is not one it has to stay off.
  t?: boolean;
  y?: boolean;
};

// Splits a `T | undefined` union into the members left and whether
// `undefined` was among them; any other schema is the one member. The
// `undefined` of `S.optional(T, default)` has the default behind it, and
// counts: the value may be absent, and absence produces the default.
export const optionalMembers = (output: Internal): [Internal[], boolean] => {
  const members: Internal[] = [];
  let hasUndefined = false;
  if (output.type === anyOfTag && output.anyOf !== U) {
    for (let idx = 0; idx < output.anyOf.length; idx++) {
      const member = output.anyOf[idx]!;
      if (member.type === undefinedTag || getOutputSchema(member).type === undefinedTag) hasUndefined = true;
      else members.push(member);
    }
  } else members.push(output);
  return [members, hasUndefined];
};
const memoKey = "c";

// Prepend-only write, shared with recursiveDecoder. A defineProperty per NEW
// operation (not per call), next to a compile that dwarfs it.
export const addOpNode = (
  schema: Internal,
  a: Internal[],
  f: Flag,
  v: ((from: unknown) => unknown) | 0
): OpNode => {
  const created: OpNode = {
    a,
    f,
    v,
    n: (schema as unknown as Record<string, OpNode | undefined>)[memoKey],
  };
  (configurableValueOptions as Record<string, unknown>)[valKey] = created;
  Object.defineProperty(schema, memoKey, configurableValueOptions as PropertyDescriptor);
  return created;
};

// recursiveDecoder's failed-compile cleanup: a node left with `v === 0` would
// read as a live circular reference on the next attempt, which would then
// call 0 at runtime. Only that error path needs this, so it shakes away with
// `recursive`.
export const removeOpNode = (schema: Internal, node: OpNode): void => {
  let cur = (schema as unknown as Record<string, OpNode | undefined>)[memoKey]!;
  if (cur === node) {
    (configurableValueOptions as Record<string, unknown>)[valKey] = node.n;
    Object.defineProperty(schema, memoKey, configurableValueOptions as PropertyDescriptor);
  } else {
    while (cur.n !== node) cur = cur.n!;
    cur.n = node.n;
  }
};

// recursiveDecoder's lookup - always exactly two schemas, and a plain read
// rather than `getOp`: a hit must not compile a missing node into existence.
export const findOpNode = (
  schema: Internal,
  s0: Internal,
  s1: Internal,
  f: Flag
): OpNode | undefined => {
  let node = (schema as unknown as Record<string, OpNode | undefined>)[memoKey];
  while (node) {
    const a = node.a;
    if (node.f === f && a.length === 2 && a[0] === s0 && a[1] === s1) return node;
    node = node.n;
  }
  return U;
};

// Builds and memoizes the operation for a chain of schema arguments. Called
// only on a cache miss, so everything it allocates is paid once per distinct
// (args, flag) operation.
const compileChain = (
  cacheTarget: Internal,
  args: Internal[],
  flag: Flag
): (from: unknown) => unknown => {
  let schema: Internal = args[args.length - 1]!;
  for (let i = args.length - 2; i >= 0; i--) {
    const to = schema;
    schema = updateOutput(args[i]!, (mut) => {
      mut.to = to;
      // Rule 3, materialized exactly as `codecTo` does it, and for the same
      // reason: `reverse` re-points `.to` and would lose it. A `.to` of
      // `undefined` (`S.assertInputOrThrow`'s result sentinel) declares nothing.
      if (mut.flags & 3 && !(mut.flags & 12) && to.type !== undefinedTag) mut.flags |= 4;
    });
  }
  // Flag 8: the caller knows nothing about the input, so the chain's own head
  // is not the source type - `unknown` is, and the head's decoder emits its
  // type checks against it. Read here rather than in `compileDecoder`, whose
  // other caller (recursive.ts) passes a source of its own and inherits this
  // bit through `g.o`.
  let f: (from: unknown) => unknown;
  // The build boundary. A chain with no codec, an async schema in a sync
  // operation, a reading that can't be chosen - all raised from here down, and
  // this is where they become an exception. Free: `getOp` reaches this on a
  // memo miss only, so a compile pays for the `try` and nothing else does.
  try {
    f = compileDecoder((flag & 8) ? unknown : schema, schema, flag, U) as (
      from: unknown,
    ) => unknown;
  } catch (thrown) {
    throw captureStackAt(thrown as SuryErrorRecord, getOp);
  }
  addOpNode(cacheTarget, args, flag, f);
  return f;
};

// THE operation lookup: `n` (1 to 5) says how many schema slots are filled, so
// the memo walk is straight-line and nothing is allocated on a hit. Arity-
// specialised on purpose - the variadic form this replaced read its
// `arguments`, which V8 must materialize the moment the object is aliased to a
// variable, and that was measurably the bulk of an operation lookup.
// @__NO_SIDE_EFFECTS__
export const getOp = (
  opFlag: Flag,
  n: number,
  a0: Internal,
  a1?: Internal,
  a2?: Internal,
  a3?: Internal
): (from: unknown) => unknown => {
  const flag = opFlag | globalConfig.f;
  // The cache lives on the newest-seq argument: the one schema every node for
  // this operation is reachable from.
  let cacheTarget = a0;
  let seq = a0.seq!;
  if (n > 1) {
    if (a1!.seq! > seq) (seq = a1!.seq!), (cacheTarget = a1!);
    if (n > 2) {
      if (a2!.seq! > seq) (seq = a2!.seq!), (cacheTarget = a2!);
      if (n > 3 && a3!.seq! > seq) cacheTarget = a3!;
    }
  }

  let node = (cacheTarget as unknown as Record<string, OpNode | undefined>)[memoKey];
  while (node) {
    const a = node.a;
    if (
      node.f === flag &&
      a.length === n &&
      a[0] === a0 &&
      (n < 2 || a[1] === a1) &&
      (n < 3 || a[2] === a2) &&
      (n < 4 || a[3] === a3)
    ) {
      return node.v as (from: unknown) => unknown;
    }
    node = node.n;
  }

  // The one allocation, on the miss path only: a compile dwarfs the spare
  // array a `slice` copies out of.
  return compileChain(cacheTarget, [a0, a1!, a2!, a3!].slice(0, n), flag);
};

export const nestedLoc = "BS_PRIVATE_NESTED_SOME_NONE";

export const never_: Internal = /* @__PURE__ */ initSchema(neverTag, (input: Val) => {
  // Carry `never` as the val's own schema, not the input's: nothing gets past
  // this branch, so a union built from its cases' output schemas must not list
  // the input type as something the union can produce.
  const output = B_refine(input, never_, U, never_);
  output.cp = B_failInvalidInput(input) + ";";
  return output;
});

export const nestedOptionParser: Builder = (input: Val) => {
  const nextSchema = input.e.to!;
  return B_next(
    input,
    `{${nestedLoc}:${getOutputSchema(input.e).properties![nestedLoc]!.const as string}}`,
    nextSchema,
    nextSchema,
  );
};

export const instanceDecoder: Builder = (input: Val) => {
  const inputTagFlag = tagFlags[input.s.type]!;
  return (inputTagFlag & 1)
    ? B_refine(input, input.e, [{ c: (v) => instanceofCond(input, input.e.class, v), f: failInvalidType }])
    : (inputTagFlag & 8192) && input.s.class === input.e.class
      ? input
      : B_unsupportedDecode(input, input.s, input.e);
};

// On a runtime that has no such global there is no schema to be had, so `class`
// reports that instead of sitting there as `undefined` for its readers to
// dereference. Every route into the schema goes through `class` - the decoder's
// `instanceof`, the rendering and the JSON Schema emit via `.name`, and
// `copySchema`'s `Object.assign` for `.with(…)` and `reverse` - so all of them
// answer with this one sentence rather than a TypeError, or worse, a schema
// that builds and fails later - converting a schema that only decodes to one
// included, since the encode-reverse copies the target to get there.
//
// Enumerable, so the `Object.assign` copy is one of the routes it covers.
// `console.log` still works: `util.inspect` shows an accessor rather than
// invoking it.
export const unsupportedInstance = (s: Internal, name: string): void => {
  Object.defineProperty(s, "class", {
    enumerable: true,
    get: () => panic(`S.${name} is not supported in this runtime`),
  });
};

// @__NO_SIDE_EFFECTS__
export const instance = (class_: unknown): Internal => {
  const mut = baseSchema(instanceTag, true, instanceDecoder);
  mut.class = class_;
  return mut;
}

// Type-narrow condition for a union variant, built from the shared atoms with no
// per-type factory reference - so unused type decoders tree-shake.
//
// Cross-module contract: a decoder's own type narrow must be exactly what this
// returns for its tag. A union group's shared narrow stands in for its members'
// type checks, so a decoder that narrowed more loosely - an object mode dropping
// `!Array.isArray` because it rebuilds the value anyway - would widen what the
// case accepts past what its acceptance mask claims, and arrays would dispatch
// to an object member.
export const typeCheckCond = (input: Val, schema: Internal, inputVar: string): string => {
  const tagFlag = tagFlags[schema.type]!;
  if ((tagFlag & 64)) return objectTagCond(inputVar);
  if ((tagFlag & 128)) return isArrayCond(inputVar);
  if ((tagFlag & 8192)) return instanceofCond(input, schema.class, inputVar);
  if ((tagFlag & 4)) return numberTagCond(inputVar, !!(input.g.o & 2));
  if ((tagFlag & 2048)) return nanCond(inputVar);
  if ((tagFlag & (16 | 32))) {
    // null/undefined reuse literalDecoder's inline-const form (=== null / void 0)
    return `${inputVar}===${B_inlineConst(input, schema)}`;
  }
  if ((tagFlag & (2 | 8 | 1024 | 16384))) {
    // literals reuse this typeof check; their per-const check stays in the case body
    return schema.format === "env"
      ? `(typeof ${inputVar}==="string"||${inputVar}===void 0)`
      : typeofCond(schema.type)(inputVar);
  }
  // Unreachable: catch-all tags use the `unknown` narrow, never this path.
  return "";
}
