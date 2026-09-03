// `S.map` — a `Map` on our side, an array of entries on the wire.

import {
  arrayTag,
  baseSchema,
  type Encoder,
  inputExpression,
  instanceTag,
  type Internal,
  tagFlags,
  U,
  unknown,
  type Val,
} from "../base";
import {
  B_asyncVal,
  B_collectAsync,
  B_forOf,
  B_iterScope,
  B_markOutput,
  B_mergeWithPathPrepend,
  B_next,
  B_refine,
  B_varWithoutAllocation,
} from "../builder";
import { arrayDecoder, arrayFactory, definitionToSchema } from "../composites";
import { iterableSource, parse, parseDynamic } from "../parse";

// One entry, described as the `[key, value]` tuple that `Array.from` produces
// and `new Map` consumes — the wire form, and the only place the key and value
// schemas live.
const entryFactory = (key: Internal, value: Internal): Internal => {
  const mut = baseSchema(arrayTag, false, arrayDecoder);
  mut.items = [key, value];
  mut.additionalItems = "strict";
  return mut;
};

// The entry hangs off `additionalItems`, where an array's item does, so that
// `reverse` inverts it (and through it the key and value) without knowing this
// schema exists — see advanced/set.ts.
//
// Cross-module invariant: NOT on `items`. That field means "the tuple slots of
// an array" to everything that pattern-matches a schema — union dispatch reads
// it as `properties || items`, and a Map whose key/value sat there made two Map
// members of a union look like one 2-tuple case, silently dropping the second.
const entryOf = (schema: Internal | undefined): Internal | undefined => {
  const entry = schema?.additionalItems;
  return entry !== U && typeof entry !== "string" ? entry : U;
};

const itemAt = (entry: Internal | undefined, idx: number): Internal =>
  entry?.items?.[idx] ?? unknown;

const mapExpression = (schema: Internal): string => {
  const entry = entryOf(schema);
  return `Map<${inputExpression(itemAt(entry, 0))}, ${inputExpression(itemAt(entry, 1))}>`;
};

const mapDecoder = (input: Val): Val => {
  const expected = input.e;
  const isArraySource = !!(tagFlags[input.s.type]! & 128);
  const inputEntry = entryOf(input.s);
  // An array converts entry by entry, so its item has to BE an entry — the
  // exact `[key, value]` shape, since `new Map` reads two slots and ignores the
  // rest, and the reverse direction (a tuple with more slots decoded from the
  // pair) has nothing to fill them with. Tested here rather than left to the
  // loop: a source of plain numbers would otherwise compile, then read
  // `undefined` out of every item at runtime.
  const source = iterableSource(
    input,
    unknownMap,
    isArraySource &&
      inputEntry !== U &&
      inputEntry.items?.length === 2 &&
      inputEntry.additionalItems === "strict",
  );

  const entryVar = B_varWithoutAllocation(source.g);
  const sourceVar = source.v();
  const fromEntry = entryOf(source.s);
  const toEntry = entryOf(expected);
  const toKey = itemAt(toEntry, 0);
  // A failing entry is located by its key when the key is what a path is made
  // of — a string or a number, on a Map that has it as a key already. Anything
  // else (an object key, a Date, a source array whose own errors count) is
  // located by its position, as a Set item is. Decided by the schema, so a key
  // that fails to be the string it should be is still what the error reports.
  const byKey = !isArraySource && !!(tagFlags[toKey.type]! & (2 | 4));
  const indexVar = byKey ? "" : B_varWithoutAllocation(source.g);
  const location = byKey ? `${entryVar}[0]` : indexVar;

  const raiseCountBefore = source.g.t;
  const keyOutput = parseDynamic(
    B_iterScope(source, `${entryVar}[0]`, itemAt(fromEntry, 0), toKey),
  );
  const valueScope = B_iterScope(
    source,
    `${entryVar}[1]`,
    itemAt(fromEntry, 1),
    itemAt(toEntry, 1),
  );
  // The value continues the key's chain rather than starting its own, so the
  // entry merges as one block: a key materialized into a variable inside a
  // `try` of its own would be out of scope where the entry is stored.
  valueScope.prev = keyOutput;
  const valueOutput = parseDynamic(valueScope);

  const isAsync = !!((keyOutput.f | valueOutput.f) & 1);
  const hasTransform = keyOutput.t === true || valueOutput.t === true;
  const rebuild = isArraySource || hasTransform || isAsync;
  // Nothing to do per entry: the wire array already IS the entry list, so the
  // constructor does the whole rebuild and the loop is left to validation.
  const fromSource = rebuild && !hasTransform && !isAsync;

  const outSchema = mapFactory(keyOutput.s, valueOutput.s);
  const out = rebuild
    ? B_next(
        source,
        fromSource ? `new Map(${sourceVar})` : isAsync ? "[]" : "new Map",
        isAsync ? arrayFactory(entryOf(outSchema)!) : outSchema,
      )
    : B_refine(source, expected);
  const outVar = rebuild && !fromSource ? out.v() : "";

  // An async entry is one promise over both halves, so the merge's own catch —
  // which reaches only the val it is handed — names where either failed. Its
  // expression is fixed here, before the merge, and the merge renames a half
  // when a check materializes it (`v0[1]` becomes `let v5=v0[1]`), so both are
  // read as variables now, which is the name the merge will use.
  const entry = isAsync
    ? B_asyncVal(valueOutput, `Promise.all([${keyOutput.v()},${valueOutput.v()}])`)
    : valueOutput;
  const canThrow = (): boolean => source.g.t !== raiseCountBefore;
  // Lazy: merging an entry can rename the val it produces (materialization)
  // and wraps an async one in a `.catch`, both after this is decided.
  const append = (): string =>
    `${
      outVar
        ? `${outVar}.${isAsync ? `push(${entry.i})` : `set(${keyOutput.i},${valueOutput.i})`};`
        : ""
    }${!byKey && canThrow() && !isAsync ? `${indexVar}++;` : ""}`;

  const body = B_mergeWithPathPrepend(entry, source, location, append, raiseCountBefore);
  const counted = !byKey && canThrow();
  B_forOf(
    out,
    entryVar,
    sourceVar,
    body,
    counted,
    indexVar,
    counted && isAsync ? B_varWithoutAllocation(source.g) : indexVar,
  );

  // `source`, not `input` — see iterableSource.
  return B_markOutput(isAsync ? B_collectAsync(out, "Map", outSchema) : out, source);
};

const mapEncoder: Encoder = (input: Val, target: Internal): Val => {
  if ((tagFlags[target.type]! & 128)) {
    // See setEncoder.
    return parse(
      B_refine(
        B_next(
          input,
          `Array.from(${input.i})`,
          arrayFactory(entryOf(input.s)!),
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
  mut.additionalItems = entryFactory(key, value);
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
