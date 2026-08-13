// `S.set` — a `Set` on our side, an array on the wire.

import {
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
  U,
  unknown,
  type Val,
  valFlagAsync,
} from "../base";
import {
  _var,
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
import { arrayFactory, definitionToSchema } from "../composites";
import { parse, parseDynamic } from "../parse";
import { instanceofCond } from "../primitives";

// The item lives on `additionalItems`, where an array's does, so that
// `reverse` reverses it without knowing this schema exists — the rendering,
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
  const item = itemOf(expected);
  const inputTagFlag = tagFlags[input.s.type]!;
  const isArraySource = flagUnsafeHas(inputTagFlag, tagFlagArray);

  let source: Val;
  if (flagUnsafeHas(inputTagFlag, tagFlagUnknown)) {
    // Narrowed to `Set<unknown>`, not to `expected`: the items of a Set that
    // only just passed `instanceof` are unvalidated, and claiming the expected
    // item schema here would compile the loop below down to identity.
    source = B_refine(input, unknownSet, [
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

  const sourceItem = itemOf(source.s);

  let output: Val;
  if (!isArraySource && item === unknown && sourceItem === unknown) {
    output = B_refine(source, expected);
  } else {
    const sourceVar = source.v();
    const itemVar = B_varWithoutAllocation(source.g);
    const itemInput = B_iterScope(source, itemVar, sourceItem, item);
    // The item expression is already a variable — the loop's — so reading it
    // must not copy it into a second one, as a dynamic member read would.
    itemInput.v = _var;
    const itemOutput = parseDynamic(itemInput);
    const isAsync = flagUnsafeHas(itemOutput.f, valFlagAsync);
    // An array source is a different value, so it is rebuilt even when the
    // items pass through untouched. An async item can't be `add`ed as it
    // arrives, so it accumulates into an array that `Promise.all` resolves.
    const rebuild = isArraySource || itemOutput.t === true || isAsync;
    const out = rebuild
      ? B_next(
          source,
          isAsync ? "[]" : "new Set",
          isAsync ? arrayFactory(itemOutput.s) : setFactory(itemOutput.s),
        )
      : B_refine(source, expected);
    const outVar = rebuild ? out.v() : "";
    // Lazy: merging the item can rename the val it produces (materialization)
    // and can wrap an async one in a `.catch`, both after this is decided.
    const add = (): string =>
      rebuild ? `${outVar}.${isAsync ? "push" : "add"}(${itemOutput.i});` : "";

    // A Set item has no key to be located by, so a failing one is located by
    // its position — well defined, since iteration follows insertion order.
    // The counter is only worth keeping when the body can actually fail, and
    // that's known only once it has been merged: a check embeds its failure
    // (and bumps the raise count) as it is emitted, not as it is built.
    const indexVar = B_varWithoutAllocation(source.g);
    // An async item reads its location from inside a `.catch` closure that
    // outlives the iteration, so the location it reads has to be a binding the
    // loop body owns — the counter itself is one binding shared by every
    // iteration, and by rejection time it holds the final count.
    const counterVar = isAsync ? B_varWithoutAllocation(source.g) : indexVar;
    const raiseCountBefore = source.g.t;
    const canThrow = (): boolean => source.g.t !== raiseCountBefore;
    const body = B_mergeWithPathPrepend(
      itemOutput,
      source,
      indexVar,
      () => `${add()}${canThrow() && !isAsync ? `${indexVar}++;` : ""}`,
      raiseCountBefore,
    );

    if (body !== "") {
      const counted = canThrow();
      out.cp =
        out.cp +
        `${counted ? `let ${counterVar}=0;` : ""}for(let ${itemVar} of ${sourceVar}){${
          counted && isAsync ? `let ${indexVar}=${counterVar}++;` : ""
        }${body}}`;
    }

    if (isAsync) {
      const resolvedVar = B_varWithoutAllocation(source.g);
      output = B_asyncVal(
        out,
        `Promise.all(${out.i}).then(${resolvedVar}=>new Set(${resolvedVar}))`,
      );
      output.s = setFactory(itemOutput.s);
    } else {
      output = out;
    }
  }

  return B_markOutput(output, input);
};

const setEncoder: Encoder = (input: Val, target: Internal): Val => {
  if (flagUnsafeHas(tagFlags[target.type]!, tagFlagArray)) {
    // The B_refine wrap is what makes the produced array the subject of the
    // target's checks — see the note in advanced/url.ts. The items are left to
    // the target's own decoder, which is what encodes them.
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
// consumer's, so one instance keeps the operation cache from re-keying.
const unknownSet: Internal = /* @__PURE__ */ setFactory(unknown);

// @__NO_SIDE_EFFECTS__
export const set = (item: unknown): Internal => setFactory(definitionToSchema(item));
