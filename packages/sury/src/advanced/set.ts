// `S.set` — a `Set` on our side, an array on the wire.

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
  B_collectAsync,
  B_iterScope,
  B_markOutput,
  B_mergeWithPathPrepend,
  B_next,
  B_refine,
  B_varWithoutAllocation,
} from "../builder";
import { arrayFactory, definitionToSchema } from "../composites";
import { iterableSource, parse, parseDynamic } from "../parse";

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
  const isArraySource = !!(tagFlags[input.s.type]! & 128);
  const source = iterableSource(input, unknownSet, true);

  const sourceVar = source.v();
  const itemVar = B_varWithoutAllocation(source.g);
  const raiseCountBefore = source.g.t;
  const itemInput = B_iterScope(source, itemVar, itemOf(source.s), itemOf(expected));
  // The item expression is already a variable — the loop's — so reading it must
  // not copy it into a second one, as a dynamic member read would.
  itemInput.v = _var;
  const itemOutput = parseDynamic(itemInput);
  const isAsync = !!(itemOutput.f & 1);
  const hasTransform = itemOutput.t === true;
  // An array source is a different value, so it is rebuilt even when the items
  // pass through untouched. An async item can't be `add`ed as it arrives, so it
  // accumulates into an array that `Promise.all` resolves.
  const rebuild = isArraySource || hasTransform || isAsync;
  // Nothing to do per item: the constructor does the whole rebuild, and the
  // loop is left to whatever validation the items still need.
  const fromSource = rebuild && !hasTransform && !isAsync;

  const outSchema = setFactory(itemOutput.s);
  const out = rebuild
    ? B_next(
        source,
        fromSource ? `new Set(${sourceVar})` : isAsync ? "[]" : "new Set",
        isAsync ? arrayFactory(itemOutput.s) : outSchema,
      )
    : B_refine(source, expected);
  const outVar = rebuild && !fromSource ? out.v() : "";
  // Lazy: merging the item can rename the val it produces (materialization) and
  // can wrap an async one in a `.catch`, both after this is decided.
  const add = (): string =>
    outVar ? `${outVar}.${isAsync ? "push" : "add"}(${itemOutput.i});` : "";

  // A Set item has no key to be located by, so a failing one is located by its
  // position — well defined, since iteration follows insertion order. The
  // counter is only worth keeping when the body can actually fail, which is
  // only known after the merge: a check embeds its failure as it is emitted,
  // where a recursive reference embeds its operation as it is built — the count
  // is sampled before both, so neither escapes the test.
  const indexVar = B_varWithoutAllocation(source.g);
  // An async item reads its location from inside a `.catch` closure that
  // outlives the iteration, so the location it reads has to be a binding the
  // loop body owns — the counter itself is one binding shared by every
  // iteration, and by rejection time it holds the final count.
  const counterVar = isAsync ? B_varWithoutAllocation(source.g) : indexVar;
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

  // `source`, not `input`: an input-side refine or size bound belongs to the
  // Set that came in, and only a val with a `prev` puts it before the loop —
  // handed the bare operation arg, B_markOutput defers it past the rebuild and
  // bounds the result instead (arrayDecoder passes its refined val for the same
  // reason).
  return B_markOutput(isAsync ? B_collectAsync(out, "Set", outSchema) : out, source);
};

const setEncoder: Encoder = (input: Val, target: Internal): Val => {
  if ((tagFlags[target.type]! & 128)) {
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
