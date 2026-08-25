// `S.list` — the ReScript linked-list representation.

import {
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

const listToArray = (list: RescriptList): unknown[] => {
  const array: unknown[] = [];
  let current = list;
  while (current !== 0) {
    array.push(current.hd);
    current = current.tl;
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
    B_conversion((list: unknown) => listToArray(list as RescriptList))
  );
}
