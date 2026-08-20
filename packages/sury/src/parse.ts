import {
  baseSchema,
  type Builder,
  configurableValueOptions,
  copySchema,
  type Encoder,
  type Flag,
  flagAsync,
  flagDisableNanNumberValidation,
  flagUnsafeHas,
  getOrRethrow,
  globalConfig,
  immutableEmptyArray,
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
  tagFlagArray,
  tagFlagBigint,
  tagFlagBoolean,
  tagFlagInstance,
  tagFlagNaN,
  tagFlagNull,
  tagFlagNumber,
  tagFlagObject,
  tagFlags,
  tagFlagString,
  tagFlagSymbol,
  tagFlagUndefined,
  tagFlagUnknown,
  U,
  unknown,
  unknownTag,
  updateOutput,
  type Val,
  valFlagAsync,
  valKey,
  valueOptions,
} from "./base";
import {
  B_embedInvalidInput,
  B_inlineConst,
  B_markOutput,
  B_merge,
  B_next,
  B_operationArg,
  B_refine,
  B_scope,
  B_unsupportedDecode,
  failInvalidType,
  noopOperation,
  operationArgVar,
} from "./builder";
import {
  instanceofCond,
  isArrayCond,
  nanCond,
  objectTagCond,
  typeofCond,
} from "./primitives";

export const parse = (input: Val): Val => {
  let result: Val = input;
  let appliedEncoderRef: Encoder | undefined = U;
  let loopCount = 0;
  while (!result.io || result.e.to) {
    const appliedEncoder: Encoder | undefined = appliedEncoderRef;
    appliedEncoderRef = U;
    const loopInput = result;

    loopCount = loopCount + 1;

    if (loopCount > 50) {
      const error = new Error("Loop count exceeded 50");
      throw error;
    }

    if (loopInput.e["$defs"]) {
      if (loopInput.g.d) {
        Object.assign(loopInput.g.d!, loopInput.e["$defs"]!);
      } else {
        loopInput.g.d = loopInput.e["$defs"];
      }
    }

    if (
      flagUnsafeHas(
        loopInput.f,
        valFlagAsync,
      ) // FIXME: is the `valFlagAsync` check alone sufficient here, or was
        // there originally a second condition (dropped during the ReScript
        // port) that this branch also needs? Unconfirmed — see PR discussion.
    ) {
      const operationInputVar = loopInput.v();

      const operationInput = B_scope(loopInput);
      const operationOutput = parse(operationInput);
      const operationCode = B_merge(operationOutput);
      if (operationInput.i !== operationOutput.i || operationCode !== "") {
        result = B_next(
          loopInput,
          `${operationInputVar}.then(${operationInputVar}=>{${operationCode}return ${operationOutput.i}})`,
          operationOutput.s,
          operationOutput.e,
        );
      } else {
        result = B_refine(loopInput, operationOutput.s, U, operationOutput.e);
      }
      result.f |= valFlagAsync;
      result.io = true;
    } else if (loopInput.io) {
      // It's guaranteed that to is not undefined, because it's checked in the while condition
      const to = loopInput.e.to!;
      if (loopInput.e.parser !== U) {
        result = loopInput.e.parser(loopInput);
      } else {
        result = B_refine(result, U, U, to);
      }
    } else {
      const maybeEncoder = loopInput.s.encoder;
      if (
        maybeEncoder &&
        maybeEncoder !== appliedEncoder &&
        loopInput.s !== loopInput.e &&
        loopInput.e.type !== unknownTag &&
        // A `noValidation` target (S.assert's result sentinel) throws the value
        // away, so there is nothing for an encoder to re-represent.
        !loopInput.e.noValidation
      ) {
        result = maybeEncoder!(loopInput, loopInput.e);
      }

      // If encoder didn't change the value, we can decode it,
      // otherwise let's start the loop from the beginning
      if (loopInput !== result) {
        appliedEncoderRef = maybeEncoder!;
      } else {
        result = loopInput.e.decoder(loopInput);

        // Primitive decoder (no internal transforms): apply refiners here.
        // Advanced decoders set isOutput themselves and own refiner application.
        if (!result.io) {
          result = B_markOutput(result, result);
        }
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
      input.p !== U ? input.p.path : pathEmpty,
      pathConcat(pathConcat(input.path, pathDynamic), error.path),
    );

    throw error;
  }
}

export const compileDecoder = (
  schema: Internal,
  expected: Internal,
  flag: Flag,
  defs: Record<string, Internal> | undefined
): (input: unknown) => unknown => {
  const input = B_operationArg(isLiteral(schema) ? unknown : schema, expected, flag, defs);

  const output = parse(input);
  const code = B_merge(output);

  const isAsync = flagUnsafeHas(output.f, valFlagAsync);
  expected.isAsync = isAsync;
  const hasTransform = output.t === true;
  expected.hasTransform = hasTransform;

  if (
    code === "" &&
    (output === input || output.i === input.i) &&
    !flagUnsafeHas(flag, flagAsync)
  ) {
    return noopOperation;
  } else {
    let inlinedOutput = output.i;
    if (flagUnsafeHas(flag, flagAsync) && !isAsync && !defs) {
      inlinedOutput = `Promise.resolve(${inlinedOutput})`;
    }

    const inlinedFunction = `${operationArgVar}=>{${code}return ${inlinedOutput}}`;

    const fn = new Function("e", "s", `return ${inlinedFunction}`)(input.g.e, s);
    fn.embedded = input.g.e;
    return fn;
  }
}
export const getOutputSchema = (schema: Internal): Internal => {
  if (schema.to !== U) {
    return getOutputSchema(schema.to);
  } else {
    return schema;
  }
}
// The two sides of a schema trade places: what parsed now serializes, what
// refined the input now refines the output. `delete` rather than `= U` because
// `unionIsTransparent` (union.ts) counts a schema's keys, and a key left
// present with an undefined value would stop every union from flattening.
const reverseSwap = (mut: Record<string, unknown>, a: string, b: string): void => {
  const previous = mut[a];
  if (mut[b] !== U) {
    mut[a] = mut[b];
  } else {
    delete mut[a];
  }
  if (previous !== U) {
    mut[b] = previous;
  } else {
    delete mut[b];
  }
}

// Null prototype: the keys are user-controlled property names, and assigning
// `__proto__` on a plain `{}` reparents the object instead of adding a key —
// which reparented the reversed property dict onto the property's own schema and
// dropped the key, so `outputExpression` rendered schema internals.
const reverseDict = (dict: Record<string, Internal>): Record<string, Internal> => {
  const reversed: Record<string, Internal> = Object.create(null);
  for (const key in dict) {
    reversed[key] = reverse(dict[key]!);
  }
  return reversed;
}

// The general `reversed` getter: every schema can answer its reverse — the
// self-reverse prototype shadows this with `this`, and a first read here
// computes, then caches both directions as own non-enumerable properties
// (own beats the getter on every later read). Free bundle-wise: `toString`
// above already makes `reverse` unshakeable. Reading `r` therefore has side
// effects — a debugger that expands prototype getters computes the reverse
// and writes the cache; harmless, but not inert.
Object.defineProperty(schemaPrototype, reversedKey, {
  get: function (this: Internal): Internal {
    const schema = this;
    let reversedHead: Internal | undefined = U;
    let current: Internal | undefined = schema;
    while (current) {
      const mut = copySchema(current!);
      const next = mut.to;
      if (reversedHead === U) {
        delete mut.to;
      } else {
        mut.to = reversedHead;
      }
      const record = mut as unknown as Record<string, unknown>;
      reverseSwap(record, "parser", "serializer");
      reverseSwap(record, "refiner", "inputRefiner");
      // Deleted, not parked in a holding field: encode has no absent-input arm,
      // and double reversal reads the cache below rather than re-deriving, so
      // nothing needs the old value back.
      delete record["default"];
      if (mut.items !== U) {
        mut.items = mut.items.map(reverse);
      }
      if (mut.properties !== U) {
        mut.properties = reverseDict(mut.properties);
      }
      // Skip tuple
      if (typeof mut.additionalItems === objectTag) {
        mut.additionalItems = reverse(mut.additionalItems as Internal);
      }
      if (mut.anyOf !== U) {
        const anyOf = mut.anyOf;
        const has: Record<string, boolean> = {};
        const newAnyOf: Internal[] = [];
        for (let idx = 0; idx <= anyOf.length - 1; idx++) {
          const s = anyOf[idx]!;
          const reversed = reverse(s);
          newAnyOf.push(reversed);
          setHas(has, reversed.type);
        }
        mut.has = has;
        mut.anyOf = newAnyOf;
      }
      if (mut["$defs"] !== U) {
        mut["$defs"] = reverseDict(mut["$defs"]);
      }
      reversedHead = mut;
      current = next;
    }

    // defineProperty (slower, once per schema) keeps the cache non-enumerable:
    // enumerability is load-bearing, not cosmetic — copySchema's Object.assign,
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
// by identity-comparing the schema arguments and the resolved flag — no string
// keys, since a key assembled per call is never interned and re-hashes on
// every lookup. Non-enumerable so copySchema's Object.assign can't carry it
// onto a derived schema. Nothing evicts: a `S.global` flag change strands the
// old flag's nodes, and each node pins its argument schemas for the target's
// lifetime — both bounded by the number of distinct (args, flag) operations
// ever asked of the schema.
//
// recursiveDecoder (advanced/recursive.ts) shares this storage; its lookup
// triple (inputSchema, def, flag) is a two-schema node stored on `def`. That
// is why `v` admits 0: a def mid-compilation holds the sentinel so inner
// circular references embed the NODE and call `.v` at runtime — the node
// exists before the function it will hold, and a recompile under corrected
// assumptions overwrites `v` in place. getDecoder never observes the sentinel:
// a def is only mid-compilation inside a synchronous recursiveDecoder pass,
// and a pass that throws unlinks its node (removeOpNode) on the way out.
export type OpNode = {
  a: Internal[]; // the schema arguments, in order
  f: Flag;
  v: ((from: unknown) => unknown) | 0;
  n: OpNode | undefined; // next (older) node
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

// recursiveDecoder's lookup — always exactly two schemas. getDecoder keeps
// its own inline walk: passing its `arguments` alias out would force the
// allocation this cache exists to avoid.
export const findOpNode = (
  schema: Internal,
  s0: Internal,
  s1: Internal,
  f: Flag
): OpNode | undefined => {
  let node = (schema as unknown as Record<string, OpNode | undefined>)[memoKey];
  while (node !== U) {
    const a = node.a;
    if (node.f === f && a.length === 2 && a[0] === s0 && a[1] === s1) {
      return node;
    }
    node = node.n;
  }
  return U;
};

// A plain (non-arrow, to keep `arguments`) function so call sites can pass
// getDecoder(s1, s2[, s3][, flag]) with any number of schemas plus an
// optional trailing flag — the body reads `arguments` directly; the declared
// rest param (unused, hence `_`) exists only to make that call shape typecheck.
// @__NO_SIDE_EFFECTS__
export function getDecoder(..._args: unknown[]): (from: unknown) => unknown {
  const args = arguments as unknown as unknown[];
  let idx = 0;
  let flag: Flag | undefined = U;
  let maxSeq = 0;
  let cacheTarget: Internal | undefined = U;

  while (flag === U) {
    const arg = args[idx];
    if (!arg) {
      flag = globalConfig.f;
    } else if (typeof arg === numberTag) {
      flag = (arg as Flag) | globalConfig.f;
    } else {
      const schema: Internal = arg as Internal;
      const seq = schema.seq!;
      if (seq > maxSeq) {
        maxSeq = seq;
        cacheTarget = schema;
      }
      idx = idx + 1;
    }
  }

  if (cacheTarget === U) {
    return panic("No schema provided for decoder.");
  } else {
    let node = (cacheTarget as unknown as Record<string, OpNode | undefined>)[memoKey];
    while (node !== U) {
      const a = node.a;
      if (node.f === flag && a.length === idx) {
        let i = idx;
        while (i-- !== 0 && a[i] === args[i]) {}
        if (i < 0) {
          return node.v as (from: unknown) => unknown;
        }
      }
      node = node.n;
    }

    let schema: Internal = args[idx - 1] as Internal;
    for (let i = idx - 2; i >= 0; i--) {
      const to = schema;
      schema = updateOutput(args[i] as Internal, (mut) => {
        mut.to = to;
      });
    }
    const f = compileDecoder(schema, schema, flag!, U) as (from: unknown) => unknown;
    addOpNode(
      cacheTarget,
      immutableEmptyArray.slice.call(args, 0, idx) as Internal[],
      flag!,
      f
    );
    return f;
  }
}

export const nestedLoc = "BS_PRIVATE_NESTED_SOME_NONE";

const neverBuilderFn = (input: Val): Val => {
  // Carry `never` as the val's own schema, not the input's: nothing gets past
  // this branch, so a union built from its cases' output schemas must not list
  // the input type as something the union can produce.
  const output = B_refine(input, never_, U, never_);
  output.cp = B_embedInvalidInput(input) + ";";
  return output;
}
export const never_: Internal = /* @__PURE__ */ initSchema(neverTag, neverBuilderFn);

export const nestedOptionParser: Builder = (input: Val) => {
  const nextSchema = input.e.to!;
  return B_next(
    input,
    `{${nestedLoc}:${getOutputSchema(input.e).properties![nestedLoc]!.const as string}}`,
    nextSchema,
    nextSchema
  );
};

export const instanceDecoder: Builder = (input: Val) => {
  const inputTagFlag = tagFlags[input.s.type]!;
  if (flagUnsafeHas(inputTagFlag, tagFlagUnknown)) {
    return B_refine(input, input.e, [
      {
        c: instanceofCond(input, input.e.class),
        f: failInvalidType,
      },
    ]);
  } else if (flagUnsafeHas(inputTagFlag, tagFlagInstance) && input.s.class === input.e.class) {
    return input;
  } else {
    return B_unsupportedDecode(input, input.s, input.e);
  }
};

// @__NO_SIDE_EFFECTS__
export const instance = (class_: unknown): Internal => {
  const mut = baseSchema(instanceTag, true, instanceDecoder);
  mut.class = class_;
  return mut;
}

// Type-narrow condition for a union variant, built from the shared atoms with no
// per-type factory reference — so unused type decoders tree-shake.
//
// Cross-module contract: a decoder's own type narrow must be exactly what this
// returns for its tag. A union group's shared narrow stands in for its members'
// type checks, so a decoder that narrowed more loosely — an object mode dropping
// `!Array.isArray` because it rebuilds the value anyway — would widen what the
// case accepts past what its acceptance mask claims, and arrays would dispatch
// to an object member.
export const typeCheckCond = (input: Val, schema: Internal, inputVar: string): string => {
  const tagFlag = tagFlags[schema.type]!;
  if (flagUnsafeHas(tagFlag, tagFlagObject)) {
    return `${objectTagCond(inputVar)}&&!${isArrayCond(inputVar)}`;
  } else if (flagUnsafeHas(tagFlag, tagFlagArray)) {
    return isArrayCond(inputVar);
  } else if (flagUnsafeHas(tagFlag, tagFlagInstance)) {
    return instanceofCond(input, schema.class)(inputVar);
  } else if (flagUnsafeHas(tagFlag, tagFlagNumber)) {
    const typeofCheck = typeofCond(numberTag)(inputVar);
    if (flagUnsafeHas(input.g.o, flagDisableNanNumberValidation)) {
      return typeofCheck;
    } else {
      return `${typeofCheck}&&!${nanCond(inputVar)}`;
    }
  } else if (flagUnsafeHas(tagFlag, tagFlagNaN)) {
    return nanCond(inputVar);
  } else if (flagUnsafeHas(tagFlag, (tagFlagUndefined | tagFlagNull))) {
    // null/undefined reuse literalDecoder's inline-const form (=== null / void 0)
    return `${inputVar}===${B_inlineConst(input, schema)}`;
  } else if (
    flagUnsafeHas(
      tagFlag,
      tagFlagString | tagFlagBoolean | tagFlagBigint | tagFlagSymbol
    )
  ) {
    // literals reuse this typeof check; their per-const check stays in the case body
    return typeofCond(schema.type)(inputVar);
  } else {
    // Unreachable: catch-all tags use the `unknown` narrow, never this path.
    return "";
  }
}
