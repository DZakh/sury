// `S.list` - the ReScript linked-list representation.

import {
 copySchema,
 type Internal,
 unknown
} from "../base";
import {
 B_conversion,
 B_invalidInputBuilder
} from "../builder";
import {
 array
} from "../composites";
import {
 codecTo
} from "../modifiers";

// Empty list is `0`; a cons cell is `{hd, tl}`. fromArray / toArray match
// Stdlib List for that representation.
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

// What a ReScript list outputs, `0` or a `{hd, tl}` cell, so it fails with a
// reason rather than the walk's TypeError and an output seam tells it from an
// object. A narrow, so it skips a value already typed as the list.
const rescriptList: Internal = /* @__PURE__ */ (() => {
  const mut = copySchema(unknown);
  mut.name = "list";
  mut.refiner = (input) =>
    input.s === input.e
      ? []
      : [
          {
            c: (v) => `(${v}===0||typeof ${v}==="object"&&${v}&&"hd" in ${v}&&"tl" in ${v})`,
            f: B_invalidInputBuilder(),
          },
        ];
  return mut;
})();

// @__NO_SIDE_EFFECTS__
export const list = (schema: unknown): Internal => {
  return codecTo(
    array(schema),
    rescriptList,
    B_conversion((array: unknown) => listFromArray(array as unknown[])),
    B_conversion((list: unknown) => listToArray(list as RescriptList))
  );
}
