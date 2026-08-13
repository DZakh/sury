// `S.map` — a `Map` on our side, an array of entries on the wire.

import {
  arrayTag,
  baseSchema,
  type Encoder,
  flagUnsafeHas,
  inputExpression,
  instanceTag,
  type Internal,
  tagFlagArray,
  tagFlagInstance,
  tagFlags,
  tagFlagUnknown,
  unknown,
  type Val,
  valFlagAsync,
} from "../base";
import {
  B_asyncVal,
  B_iterScope,
  B_markOutput,
  B_mergeWithPathPrepend,
  B_next,
  B_refine,
  B_unsupportedDecode,
  B_varWithoutAllocation,
  failInvalidType,
} from "../builder";
import { arrayDecoder, arrayFactory, definitionToSchema } from "../composites";
import { parse, parseDynamic } from "../parse";
import { instanceofCond } from "../primitives";

// Key and value live on `items`, where a tuple's do, so that `reverse`
// reverses them without knowing this schema exists — the rendering, the
// encoder and the entry loop all read them back off the schema they are handed
// rather than closing over them, which is what makes them follow the schema
// when it flips.
const itemAt = (schema: Internal | undefined, idx: number): Internal =>
  schema?.items?.[idx] ?? unknown;

const mapExpression = (schema: Internal): string =>
  `Map<${inputExpression(itemAt(schema, 0))}, ${inputExpression(itemAt(schema, 1))}>`;

// The wire form of one entry: the `[key, value]` pair `Array.from` produces and
// `new Map` consumes.
const entryFactory = (schema: Internal): Internal => {
  const mut = baseSchema(arrayTag, false, arrayDecoder);
  mut.items = [itemAt(schema, 0), itemAt(schema, 1)];
  mut.additionalItems = "strict";
  return mut;
};

const mapDecoder = (input: Val): Val => {
  const expected = input.e;
  const inputTagFlag = tagFlags[input.s.type]!;
  const isArraySource = flagUnsafeHas(inputTagFlag, tagFlagArray);

  let source: Val;
  if (flagUnsafeHas(inputTagFlag, tagFlagUnknown)) {
    // Narrowed to `Map<unknown, unknown>`, not to `expected`: the entries of a
    // Map that only just passed `instanceof` are unvalidated, and claiming the
    // expected schemas here would compile the loop below down to identity.
    source = B_refine(input, unknownMap, [
      {
        c: instanceofCond(input, expected.class),
        f: failInvalidType,
      },
    ]);
  } else if (
    isArraySource ||
    (flagUnsafeHas(inputTagFlag, tagFlagInstance) && input.s.class === expected.class)
  ) {
    source = input;
  } else {
    return B_unsupportedDecode(input, input.s, expected);
  }

  // An array source describes its entries through the item schema of the
  // array; a Map source through the Map schema itself.
  const sourceEntry = isArraySource
    ? (source.s.additionalItems as Internal | undefined)
    : source.s;

  const entryVar = B_varWithoutAllocation(source.g);
  const sourceVar = source.v();
  const location = `${entryVar}[0]`;

  const keyOutput = parseDynamic(
    B_iterScope(source, location, itemAt(sourceEntry, 0), itemAt(expected, 0)),
  );
  const valueScope = B_iterScope(
    source,
    `${entryVar}[1]`,
    itemAt(sourceEntry, 1),
    itemAt(expected, 1),
  );
  // The value continues the key's chain rather than starting its own, so the
  // entry merges as one block: a key materialized into a variable inside a
  // `try` of its own would be out of scope where the entry is stored.
  valueScope.prev = keyOutput;
  const valueOutput = parseDynamic(valueScope);

  const isAsync = flagUnsafeHas(keyOutput.f | valueOutput.f, valFlagAsync);
  // An array source is a different value, so it is rebuilt even when the
  // entries pass through untouched. An async entry can't be `set` as it
  // arrives, so it accumulates into an array that `Promise.all` resolves.
  const rebuild =
    isArraySource || keyOutput.t === true || valueOutput.t === true || isAsync;

  const outSchema = mapFactory(keyOutput.s, valueOutput.s);
  const out = rebuild
    ? B_next(
        source,
        isAsync ? "[]" : "new Map",
        isAsync ? arrayFactory(entryFactory(outSchema)) : outSchema,
      )
    : B_refine(source, expected);
  const outVar = rebuild ? out.v() : "";
  // Lazy: merging an entry can rename the val it produces (materialization)
  // and can wrap an async one in a `.catch`, both after this is decided.
  const append = (): string =>
    rebuild
      ? isAsync
        ? `${outVar}.push(Promise.all([${keyOutput.i},${valueOutput.i}]));`
        : `${outVar}.set(${keyOutput.i},${valueOutput.i});`
      : "";

  // The raise count is read right before the merge, not before the parse: a
  // check embeds its failure (and bumps the count) as it is emitted, not as it
  // is built, so this is what tells an entry that can fail — and needs itself
  // located in the path — from one that can't.
  const raiseCountBefore = source.g.t;
  const body = B_mergeWithPathPrepend(
    valueOutput,
    source,
    location,
    append,
    raiseCountBefore,
  );

  if (body !== "") {
    out.cp = out.cp + `for(let ${entryVar} of ${sourceVar}){${body}}`;
  }

  let output: Val;
  if (isAsync) {
    const resolvedVar = B_varWithoutAllocation(source.g);
    output = B_asyncVal(
      out,
      `Promise.all(${out.i}).then(${resolvedVar}=>new Map(${resolvedVar}))`,
    );
    output.s = outSchema;
  } else {
    output = out;
  }

  return B_markOutput(output, input);
};

const mapEncoder: Encoder = (input: Val, target: Internal): Val => {
  if (flagUnsafeHas(tagFlags[target.type]!, tagFlagArray)) {
    // The B_refine wrap is what makes the produced array the subject of the
    // target's checks — see the note in advanced/url.ts. The entries are left
    // to the target's own decoder, which is what encodes them.
    return parse(
      B_refine(
        B_next(
          input,
          `Array.from(${input.i})`,
          arrayFactory(entryFactory(input.s)),
          target,
        ),
      ),
    );
  }
  return input;
};

const mapFactory = (key: Internal, value: Internal): Internal => {
  const mut = baseSchema(instanceTag, !!(key.sr && value.sr), mapDecoder);
  mut.class = Map;
  mut.items = [key, value];
  mut.expression = mapExpression;
  mut.encoder = mapEncoder;
  return mut;
};

// Shared rather than built per compilation: it is a val's schema, never a
// consumer's, so one instance keeps the operation cache from re-keying.
const unknownMap: Internal = /* @__PURE__ */ mapFactory(unknown, unknown);

// @__NO_SIDE_EFFECTS__
export const map = (key: unknown, value: unknown): Internal =>
  mapFactory(definitionToSchema(key), definitionToSchema(value));
