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
  B_iterScope,
  B_markOutput,
  B_mergeWithPathPrepend,
  B_next,
  B_pathPrependCode,
  B_refine,
  B_unsupportedDecode,
  B_varWithoutAllocation,
  failInvalidType,
} from "../builder";
import { arrayDecoder, arrayFactory, definitionToSchema } from "../composites";
import { parse, parseDynamic } from "../parse";
import { instanceofCond } from "../primitives";

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
// schema exists — the rendering, the encoder and the entry loop all read it
// back off the schema they are handed rather than closing over it, which is
// what makes them follow the schema when it flips.
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
  const inputTagFlag = tagFlags[input.s.type]!;
  const isArraySource = !!(inputTagFlag & 128);
  // Every source describes its entries the same way — through
  // `additionalItems`, an array's item schema and a Map's entry alike.
  const sourceEntry = entryOf(input.s);

  let source: Val;
  if ((inputTagFlag & 1)) {
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
    // An array converts entry by entry, so its item has to BE an entry. Tested
    // here rather than left to the loop: a source of plain numbers would
    // otherwise compile, then read `undefined` out of every item at runtime.
    (isArraySource && sourceEntry !== U && sourceEntry.type === arrayTag) ||
    ((inputTagFlag & 8192) && input.s.class === expected.class)
  ) {
    // Refined even with no checks of its own, as arrayDecoder does: an
    // input-side refine (a size bound, reversed) can only emit before the loop
    // when the val it attaches to has a `prev` — the bare operation arg has
    // none, and B_markOutput would defer the check past the rebuild.
    source = B_refine(input);
  } else {
    return B_unsupportedDecode(input, input.s, expected);
  }

  const entryVar = B_varWithoutAllocation(source.g);
  const sourceVar = source.v();
  const location = `${entryVar}[0]`;
  const fromEntry = entryOf(source.s);
  const toEntry = entryOf(expected);

  const raiseCountBefore = source.g.t;
  const keyOutput = parseDynamic(
    B_iterScope(source, location, itemAt(fromEntry, 0), itemAt(toEntry, 0)),
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
  // An array source is a different value, so it is rebuilt even when the
  // entries pass through untouched. An async entry can't be `set` as it
  // arrives, so it accumulates into an array that `Promise.all` resolves.
  const rebuild = isArraySource || hasTransform || isAsync;
  // Nothing to do per entry: the wire array already IS the entry list, so the
  // constructor does the whole rebuild and the loop is left to validation.
  const fromSource = rebuild && !hasTransform && !isAsync;

  let out: Val;
  let outSchema: Internal | undefined;
  if (rebuild) {
    outSchema = mapFactory(keyOutput.s, valueOutput.s);
    out = B_next(
      source,
      fromSource ? `new Map(${sourceVar})` : isAsync ? "[]" : "new Map",
      isAsync ? arrayFactory(entryOf(outSchema)!) : outSchema,
    );
  } else {
    out = B_refine(source, expected);
  }
  const outVar = rebuild && !fromSource ? out.v() : "";
  // An async key hands its promise to `Promise.all` unwrapped, so the merge's
  // own wrap (which only ever reaches the val it merges — the value) never
  // names where it failed.
  const keyErrorVar = isAsync ? B_varWithoutAllocation(source.g) : "";
  // Lazy: merging an entry can rename the val it produces (materialization)
  // and can wrap an async one in a `.catch`, both after this is decided.
  const append = (): string => {
    if (!isAsync) {
      return `${outVar}.set(${keyOutput.i},${valueOutput.i});`;
    }
    const key = (keyOutput.f & 1)
      ? `${keyOutput.i}.catch(${keyErrorVar}=>{${B_pathPrependCode(source, location, keyErrorVar)};throw ${keyErrorVar}})`
      : keyOutput.i;
    return `${outVar}.push(Promise.all([${key},${valueOutput.i}]));`;
  };

  // The count sampled above tells an entry that can fail — and so needs itself
  // located in the path — from one that can't: a check embeds its failure as it
  // is emitted, a recursive reference embeds its operation as it is built, and
  // the sample precedes both.
  const body = B_mergeWithPathPrepend(
    valueOutput,
    source,
    location,
    outVar ? append : U,
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
    output.s = outSchema!;
  } else {
    output = out;
  }

  // `source`, not `input`: an input-side refine or size bound belongs to the
  // Map that came in, and only a val with a `prev` puts it before the loop —
  // handed the bare operation arg, B_markOutput defers it past the rebuild and
  // bounds the result instead (arrayDecoder passes its refined val for the
  // same reason).
  return B_markOutput(output, source);
};

const mapEncoder: Encoder = (input: Val, target: Internal): Val => {
  if ((tagFlags[target.type]! & 128)) {
    // The B_refine wrap is what makes the produced array the subject of the
    // target's checks — see the note in advanced/url.ts. The entries are left
    // to the target's own decoder, which is what encodes them.
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
