// `S.list` — the ReScript linked-list representation.

import {
 objectTag,
 pathEmpty,
 SuryError,
 type Internal,
 unknown
} from "../base";
import {
 B_conversion
} from "../builder";
import {
 array
} from "../composites";
import {
 codecTo
} from "../modifiers";

// PORT-NOTE: ReScript list runtime (v12): empty list = `0`, cons cell =
// `{hd, tl}`. These two helpers replicate Stdlib List.fromArray / List.toArray
// exactly for that representation.
type RescriptList = 0 | { hd: unknown; tl: RescriptList };

const listFromArray = (array: unknown[]): RescriptList => {
  let list: RescriptList = 0;
  for (let i = array.length - 1; i >= 0; i--) {
    list = { hd: array[i], tl: list };
  }
  return list;
}

const listToArray = (list: unknown): unknown[] => {
  const array: unknown[] = [];
  let current = list;
  while (current !== 0) {
    if (
      current === null ||
      typeof current !== objectTag ||
      !("hd" in (current as object)) ||
      !("tl" in (current as object))
    ) {
      throw new SuryError({
        code: "invalid_operation",
        path: pathEmpty,
        reason: "Expected list",
      });
    }
    const cons = current as { hd: unknown; tl: unknown };
    array.push(cons.hd);
    current = cons.tl;
  }
  return array;
}

// @__NO_SIDE_EFFECTS__
export const list = (schema: unknown): Internal => {
  // `unknown` target: a ReScript list has no schema of its own to land on.
  return codecTo(
    array(schema),
    unknown,
    B_conversion((array: unknown) => listFromArray(array as unknown[])),
    B_conversion(listToArray)
  );
}
