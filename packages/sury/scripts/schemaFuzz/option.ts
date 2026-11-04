// The option family. The coder each schema also goes under is one whose output
// already holds `undefined`, which no other draw makes.
//
//   none   the factory's empty value parses to None, unless the item's own Input
//          side takes it: then the item answers.
//   some   any other value the item takes parses to Some of what the item
//          decodes it to. Some(None) and deeper are ReScript's
//          `{BS_PRIVATE_NESTED_SOME_NONE: n}`, one level deeper per option.
//   peel   `S.Option.getOr(None)` takes one option back off: over the factory
//          applied twice it answers every value as the factory applied once.

import { show, structural } from "../unionFuzz/sample";
import { type Ctx, type Family, reason } from "./context";

type Op = (value: unknown) => unknown;

const NESTED = "BS_PRIVATE_NESTED_SOME_NONE";
const FACTORIES = [
  ["$option", [undefined]],
  ["$nullAsOption", [null]],
  ["$nullableAsOption", [undefined, null]],
] as const;

const some = (value: unknown): unknown => {
  if (value === undefined) return { [NESTED]: 0 };
  const depth = (value as Record<string, unknown> | null)?.[NESTED];
  return typeof depth === "number" ? { [NESTED]: depth + 1 } : value;
};

const settle = (op: Op, value: unknown) => {
  try {
    const result = op(value);
    return { ok: true, value: result, show: show(result) };
  } catch (error) {
    return { ok: false, value: undefined, show: `a throw (${reason(error)})` };
  }
};

const check = (ctx: Ctx): void => {
  const { S, schema, inputs, isInput, report, compile, count } = ctx;
  const coder = compile("coder", () =>
    S.to(schema, S.optional(S.json), { decode: (v: unknown) => v, encode: (v: unknown) => v }),
  );
  for (const [label, item] of [
    ["", schema],
    ["coder ", coder],
  ] as const) {
    if (item === undefined) continue;
    const decode = compile<Op>("some", () => S.decodeOrThrow(item));
    for (const [factory, empties] of FACTORIES) {
      const parse = compile<Op>("none", () => S.parseOrThrow(S[factory](item)));
      if (!parse) continue;
      for (const empty of empties) {
        if (isInput(empty) === true) continue;
        count("results");
        try {
          const none = parse(empty);
          if (none !== undefined) report("none", `${factory}(${label}item) parsed ${show(empty)} to ${show(none)}, not None`);
        } catch (error) {
          report("none", `${factory}(${label}item) threw on ${show(empty)} - ${reason(error)}`);
        }
      }
      const peeled = compile<Op>("peel", () =>
        S.parseOrThrow(S.$Option_getOr(S[factory](S[factory](item)), undefined)),
      );
      if (peeled) {
        for (const value of [...empties, ...inputs.map(([i]) => i)]) {
          const once = settle(parse, value);
          const twice = settle(peeled, value);
          if (once.ok !== twice.ok || (once.ok && !structural(once.value, twice.value))) {
            report(
              "peel",
              `getOr(${factory}(${factory}(${label}item)), None) read ${show(value)} as ${twice.show}, not ${once.show}`,
            );
          }
        }
      }
      if (!decode) continue;
      for (const [i] of inputs) {
        if ((empties as readonly unknown[]).includes(i)) continue;
        let expected: unknown;
        try {
          expected = some(decode(i));
        } catch {
          continue;
        }
        count("results");
        try {
          const parsed = parse(i);
          if (!structural(parsed, expected)) {
            report("some", `${factory}(${label}item) parsed ${show(i)} to ${show(parsed)}, not Some: ${show(expected)}`);
          }
        } catch (error) {
          report("some", `${factory}(${label}item) threw on ${show(i)}, which the item takes - ${reason(error)}`);
        }
      }
    }
  }
};

export const option: Family = { check };
