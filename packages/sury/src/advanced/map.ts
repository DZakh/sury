// `S.map`: a `Map` on our side, an array of entries on the wire.

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
  B_child,
  B_collectAsync,
  B_collects,
  B_countCopy,
  B_forOf,
  B_iterScope,
  B_markOutput,
  B_merge,
  B_next,
  B_refine,
  B_settle,
  B_sink,
  B_varWithoutAllocation,
} from "../builder";
import { arrayDecoder, arrayFactory, definitionToSchema } from "../composites";
import { iterableSource, parse } from "../parse";

// One entry, described as the `[key, value]` tuple that `Array.from` produces
// and `new Map` consumes. That is the wire form, and the only place the key
// and value schemas live.
const entryFactory = (key: Internal, value: Internal): Internal => {
  const mut = baseSchema(arrayTag, false, arrayDecoder);
  mut.items = [key, value];
  mut.additionalItems = "strict";
  return mut;
};

// The entry hangs off `additionalItems`, where an array's item does, so that
// `reverse` inverts it (and through it the key and value) without knowing this
// schema exists: see advanced/set.ts.
//
// Cross-module invariant: NOT on `items`. That field means "the tuple slots of
// an array" to everything that pattern-matches a schema. Union dispatch reads
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
  // An array converts entry by entry, so its item has to BE an entry, in the
  // exact `[key, value]` shape: `new Map` reads two slots and ignores the rest,
  // and the reverse direction (a tuple with more slots decoded from the pair)
  // has nothing to fill them with. Tested here rather than left to the
  // loop: a source of plain numbers would otherwise compile, then read
  // `undefined` out of every item at runtime.
  const source = iterableSource(
    input,
    (unknownMap ||= mapFactory(unknown, unknown)),
    isArraySource &&
      inputEntry !== U &&
      inputEntry.items?.length === 2 &&
      inputEntry.additionalItems === "strict",
  );

  const g = source.g;
  const entryVar = B_varWithoutAllocation(g);
  const sourceVar = source.v();
  const fromEntry = entryOf(source.s);
  const toEntry = entryOf(expected);
  const toKey = itemAt(toEntry, 0);
  // A failing entry is located by its key when the key is what a path is made
  // of, a string or a number, on a Map that has it as a key already. Anything
  // else (an object key, a Date, a source array whose own errors count) is
  // located by its position, as a Set item is. Decided by the schema, so a key
  // that fails to be the string it should be is still what the error reports.
  const byKey = !isArraySource && !!(tagFlags[toKey.type]! & (2 | 4));
  const indexVar = byKey ? "" : B_varWithoutAllocation(g);
  const location = byKey ? `${entryVar}[0]` : indexVar;

  const failCountBefore = g.t + g.j;
  const keyScope = B_iterScope(source, `${entryVar}[0]`, itemAt(fromEntry, 0), toKey, location);
  const valueScope = B_iterScope(
    source,
    `${entryVar}[1]`,
    itemAt(fromEntry, 1),
    itemAt(toEntry, 1),
    location,
  );
  let keyOutput!: Val, valueOutput!: Val, out!: Val, outSchema!: Internal, isAsync = false;
  let counted = false, counterVar = indexVar;
  const collected = g.kj;
  const body = B_sink(g, () => {
    const collects = B_collects(g);
    // A half that settles (`B_settle`) has its rejection read as a value, so
    // the join sees both halves of every entry.
    const settles = (val: Val): boolean => collects && !!g.k && !!(val.f & 1);
    // The key and the value are children of their own, as an object's fields
    // are, so an operation that collects reports a bad key and a bad value of
    // one entry both. What a half materializes is declared at the body's head
    // (B_let), which is what lets the entry be stored after both blocks; a
    // converted half is materialized inside its own, since the store runs
    // even after a half failed and must not evaluate the conversion again.
    const half = (scope: Val): [Val, string] => {
      let val!: Val, merged = "";
      const emit = () => {
        val = parse(scope);
        if (collects && (val.t === true || val.f & 1)) val.v();
        if (settles(val)) val.fs?.(val, true);
        merged = B_merge(val);
      };
      const wrap = collects ? B_child(g, emit) : (emit(), U);
      return [val, wrap ? wrap(merged, val) : merged];
    };
    const [k, keyCode] = half(keyScope);
    const [v, valueCode] = half(valueScope);
    keyOutput = k;
    valueOutput = v;
    isAsync = !!((k.f | v.f) & 1);
    // Nothing to do per entry: the wire array already IS the entry list, so
    // the constructor does the whole rebuild and the loop is left to
    // validation.
    const add = k.t === true || v.t === true || isAsync;
    outSchema = mapFactory(k.s, v.s);
    out =
      isArraySource || add
        ? B_next(
            source,
            add ? (isAsync ? "[]" : "new Map") : `new Map(${sourceVar})`,
            isAsync ? arrayFactory(unknown) : outSchema,
          )
        : B_refine(source, expected);
    // An async entry goes onto the list as its two halves, which
    // B_collectAsync pairs back up.
    const stored = (val: Val): string => (settles(val) ? B_settle(out, val) : val.i);
    const code = add
      ? `${keyCode}${valueCode}${out.v()}.${
          isAsync ? `push(${stored(k)},${stored(v)})` : `set(${k.i},${v.i})`
        };`
      : g.t + g.j === failCountBefore
        ? ""
        : keyCode + valueCode;
    counted = !byKey && g.t + g.j !== failCountBefore;
    const copy = counted && isAsync
      ? B_countCopy(g, indexVar, (counterVar = B_varWithoutAllocation(g)))
      : "";
    return copy + code;
  });
  if (g.kj !== collected) out.k = true;
  B_forOf(out, entryVar, sourceVar, body, counted ? counterVar : U, counted && !isAsync);

  // `source`, not `input`: see iterableSource.
  return B_markOutput(isAsync ? B_collectAsync(out, "Map", outSchema, true) : out, source);
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
// consumer's, so one instance keeps the operation cache from re-keying. Built
// on first decode, not at module load: a schema built at load time gives the
// shape every factory starts from a branch of its own, and that alone made
// creating a literal, an array or a record 15-55% slower.
let unknownMap: Internal;

// @__NO_SIDE_EFFECTS__
export const map = (key: unknown, value: unknown): Internal =>
  mapFactory(definitionToSchema(key), definitionToSchema(value));
