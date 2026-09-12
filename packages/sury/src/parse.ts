import {
  baseSchema,
  type Builder,
  configurableValueOptions,
  copySchema,
  type Encoder,
  type Flag,
  getOrRethrow,
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
  pathConcat,
  pathDynamic,
  pathEmpty,
  reversedKey,
  s,
  schemaPrototype,
  setHas,
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
  B_embedInvalidInput,
  B_embedPure,
  B_errorOf,
  B_inlineConst,
  B_markOutput,
  B_merge,
  B_next,
  B_operationArg,
  B_refine,
  B_scope,
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
    if (defs) loopInput.g.d ? Object.assign(loopInput.g.d, defs) : (loopInput.g.d = defs);

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
      const operationOutput = parse(operationInput);
      const operationCode = B_merge(operationOutput);
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
        !(loopInput.e.noValidation && (loopInput.e.isJson || loopInput.e.type === undefinedTag))
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
export const parseDynamic = (input: Val): Val => {
  try {
    return parse(input);
  } catch (exn) {
    const error = getOrRethrow(exn);
    // For the case parent must always be present
    error.path = pathConcat(
      input.p ? input.p.path : pathEmpty,
      pathConcat(pathConcat(input.path, pathDynamic), error.path),
    );
    throw error;
  }
}

// How a compiled operation's body ends. `undefined` means "no body at all" -
// the operation is the identity, and the caller hands back `noopOperation`.
//
// A mutable binding rather than a branch on the flag: `compileDecoder` is in
// every bundle, and the Result modes' emitter (operations.ts) must not be. The
// throw tail below is the default, and reaching a Result operation is what
// swaps in the emitter that knows both. Registration happens inside that
// operation, not at its module's top level, which would be a side effect that
// survives the same shaking.
export type Tail = (
  input: Val,
  code: string,
  out: string,
  isAsync: boolean,
  flag: Flag,
  hasDefs: boolean,
) => string | undefined;

// Throw mode, plus the Standard Schema tail (mode bit 1024). The Standard
// Schema arm is here rather than behind the `__setTail` hook because the
// `~standard` prototype getter can never be tree-shaken (standard.ts), so a
// registration from it would drag the whole emitter into every consumer bundle
// - the very thing the hook exists to prevent. Keeping the one shape that
// getter needs here costs a branch; the JS and ReScript Result shapes stay
// behind the hook (operations.ts).
export const throwTail: Tail = (input, code, out, isAsync, flag, hasDefs) => {
  if (flag & 1024) {
    // `path` is omitted at the root, which is what Standard Schema consumers
    // expect. Built by an embedded function rather than inline: the failure
    // path reads the error three times, and the same closure serves the
    // sync catch and the promise's rejection handler.
    const errorOf = B_errorOf(input);
    const issues = B_embedPure(input, (e: unknown) => {
      const error = errorOf(e);
      return { issues: [{ message: error.reason, path: error.path.length ? error.path : U }] };
    });
    const v = isAsync ? B_varWithoutAllocation(input.g) : "";
    const body = isAsync
      ? `${code}return ${out}.then(${v}=>({value:${v}}),${issues})`
      : `${code}return {value:${out}}`;
    // The raise counter: when nothing merged can throw, no `try`. An async
    // operation answers with a promise either way, so a failure the sync
    // phase raises comes back in the same shape as one after the await.
    if (!input.g.t) return body;
    const e = B_varWithoutAllocation(input.g);
    return `try{${body}}catch(${e}){return ${
      isAsync ? `Promise.resolve(${issues}(${e}))` : `${issues}(${e})`
    }}`;
  }
  return code === "" && out === operationArgVar && !(flag & 1)
    ? U
    : `${code}return ${(flag & 1) && !isAsync && !hasDefs ? `Promise.resolve(${out})` : out}`;
};

let emitTail: Tail = throwTail;
export const __setTail = (fn: Tail): void => {
  emitTail = fn;
};

export const compileDecoder = (
  schema: Internal,
  expected: Internal,
  flag: Flag,
  defs: Record<string, Internal> | undefined,
  node?: OpNode
): (input: unknown) => unknown => {
  const input = B_operationArg(isLiteral(schema) ? unknown : schema, expected, flag, defs);

  const output = parse(input);
  const code = B_merge(output);
  const isAsync = !!(output.f & 1);
  if (node) {
    node.y = isAsync;
    node.t = output.t === true;
  }

  const body = emitTail(input, code, output.i, isAsync, flag, !!defs);
  if (!body) return noopOperation;
  const fn = new Function("e", "s", `return ${operationArgVar}=>{${body}}`)(input.g.e, s);
  fn.embedded = input.g.e;
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
      // The link into this node is now the one out of it, read the other way:
      // opening `current` into `next` was storing `next` into `current`.
      next && next.opens !== U ? (mut.opens = !next.opens) : delete mut.opens;
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
          setHas(has, reversed.type);
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
      // reason: `reverse` re-points `.to` and would lose it.
      if (mut.content !== U && mut.opens === U) mut.opens = true;
    });
  }
  // Flag 8: the caller knows nothing about the input, so the chain's own head
  // is not the source type - `unknown` is, and the head's decoder emits its
  // type checks against it. Read here rather than in `compileDecoder`, whose
  // other caller (recursive.ts) passes a source of its own and inherits this
  // bit through `g.o`.
  const f = compileDecoder(
    (flag & 8) ? unknown : schema,
    schema,
    flag,
    U,
  ) as (from: unknown) => unknown;
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
  output.cp = B_embedInvalidInput(input) + ";";
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
