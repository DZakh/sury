// `S.list` — the ReScript linked-list representation.

import type { Internal } from "../base";
import { array } from "../composites";
import { transform } from "../modifiers";

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
export const list = (schema: Internal): Internal => {
  return transform(array(schema), (_: unknown) => ({
    p: (array: unknown) => listFromArray(array as unknown[]),
    s: (list: unknown) => listToArray(list as RescriptList),
  }));
}
