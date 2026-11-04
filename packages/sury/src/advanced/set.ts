// `S.set`: a `Set` on our side, an array on the wire.

import {
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
  _var,
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
import { arrayFactory, definitionToSchema } from "../composites";
import { iterableSource, parse } from "../parse";

// The item lives on `additionalItems`, where an array's does, so that
// `reverse` reverses it without knowing this schema exists. The rendering,
// the encoder and the item loop all read it back off the schema they are
// handed rather than closing over it, which is what makes them follow the
// schema when it flips.
const itemOf = (schema: Internal): Internal => {
  const item = schema.additionalItems;
  return item !== U && typeof item !== "string" ? item : unknown;
};

const setExpression = (schema: Internal): string =>
  `Set<${inputExpression(itemOf(schema))}>`;

const setDecoder = (input: Val): Val => {
  const expected = input.e;
  const isArraySource = !!(tagFlags[input.s.type]! & 128);
  const source = iterableSource(input, (unknownSet ||= setFactory(unknown)), true);
  const g = source.g;

  const sourceVar = source.v();
  const itemVar = B_varWithoutAllocation(g);
  // A Set item has no key to be located by, so a failing one is located by its
  // position, which is well defined since iteration follows insertion order.
  const indexVar = B_varWithoutAllocation(g);
  const failCountBefore = g.t + g.j;
  const itemInput = B_iterScope(source, itemVar, itemOf(source.s), itemOf(expected), indexVar);
  // The item expression is already a variable, the loop's, so reading it must
  // not copy it into a second one the way a dynamic member read would.
  itemInput.v = _var;
  let itemOutput!: Val, out!: Val, outSchema!: Internal, isAsync = false, counted = false;
  let counterVar = indexVar;
  const collected = g.kj;
  const body = B_sink(g, () => {
    let itemMerge = "", settles = false, add = false;
    const collects = B_collects(g);
    const emit = () => {
      itemOutput = parse(itemInput);
      isAsync = !!(itemOutput.f & 1);
      // An async item can't be `add`ed as it arrives, so it accumulates into an
      // array that `Promise.all` resolves. With nothing to do per item the
      // constructor does the whole rebuild, and the loop is left to whatever
      // validation the items still need.
      add = itemOutput.t === true || isAsync;
      outSchema = setFactory(itemOutput.s);
      out =
        isArraySource || add
          ? B_next(
              source,
              add ? (isAsync ? "[]" : "new Set") : `new Set(${sourceVar})`,
              isAsync ? arrayFactory(itemOutput.s) : outSchema,
            )
          : B_refine(source, expected);
      settles = isAsync && collects && !!g.k;
      if (settles) itemOutput.fs?.(itemOutput, true);
      itemMerge = B_merge(itemOutput);
    };
    const wrap = collects ? B_child(g, emit) : (emit(), U);
    const code = add
      ? `${itemMerge}${out.v()}.${isAsync ? "push" : "add"}(${
          settles ? B_settle(out, itemOutput) : itemOutput.i
        });`
      : g.t + g.j === failCountBefore
        ? ""
        : itemMerge;
    counted = g.t + g.j !== failCountBefore;
    const copy = counted && isAsync
      ? B_countCopy(g, indexVar, (counterVar = B_varWithoutAllocation(g)))
      : "";
    return copy + (wrap ? wrap(code, itemOutput) : code);
  });
  if (g.kj !== collected) out.k = true;
  B_forOf(out, itemVar, sourceVar, body, counted ? counterVar : U, counted && !isAsync);

  // `source`, not `input`: see iterableSource.
  return B_markOutput(isAsync ? B_collectAsync(out, "Set", outSchema) : out, source);
};

const setEncoder: Encoder = (input: Val, target: Internal): Val => {
  if ((tagFlags[target.type]! & 128)) {
    // The B_refine wrap is what makes the produced array the subject of the
    // target's checks, as the note in advanced/url.ts explains. The items are
    // left to the target's own decoder, which is what encodes them.
    return parse(
      B_refine(
        B_next(input, `Array.from(${input.i})`, arrayFactory(itemOf(input.s)), target),
      ),
    );
  }
  return input;
};

const setFactory = (item: Internal): Internal => {
  const mut = baseSchema(instanceTag, !!item.sr, setDecoder);
  mut.class = Set;
  mut.additionalItems = item;
  mut.expression = setExpression;
  mut.encoder = setEncoder;
  return mut;
};

// Shared rather than built per compilation: it is a val's schema, never a
// consumer's, so one instance keeps the operation cache from re-keying. Built
// on first decode, not at module load: a schema built at load time gives the
// shape every factory starts from a branch of its own, and that alone made
// creating a literal, an array or a record 15-55% slower.
let unknownSet: Internal;

// @__NO_SIDE_EFFECTS__
export const set = (item: unknown): Internal => setFactory(definitionToSchema(item));
