// The tree a generated member is built from, carried beside its printed id so a
// known bug can be described by the shape that triggers it - "a union with a
// member that carries a default" - at any depth, rather than by a substring of
// the id that only matches the depths someone happened to write down. The
// grammar builds it at the same place it builds the schema (`generate.ts`).

export type Shape = {
  name: string;
  args: Shape[];
  // A modifier's name (`with`), or a default's printed value (`#value`).
  raw?: string;
};

export const node = (name: string, ...args: Shape[]): Shape => ({ name, args });
export const valueNode = (text: string): Shape => ({ name: "#value", args: [], raw: text });

export const nodes = function* (shape: Shape): Generator<Shape> {
  yield shape;
  for (const arg of shape.args) yield* nodes(arg);
};

export const some = (shape: Shape, test: (node: Shape) => boolean): boolean => {
  for (const node of nodes(shape)) if (test(node)) return true;
  return false;
};

// ---- predicates the known-bug registry is written in ----------------------

// `optional(x, d)` / `nullable(x, d)`: the default is the second argument.
export const hasDefault = (shape: Shape): boolean =>
  (shape.name === "optional" || shape.name === "nullable") && shape.args.length === 2;

// The nodes that compile to a union over their arguments.
const UNION_LIKE = new Set(["union", "optional", "nullable", "nullish"]);
export const unionMembers = (node: Shape): Shape[] =>
  UNION_LIKE.has(node.name) ? (node.name === "union" ? node.args : node.args.slice(0, 1)) : [];

export const admitsUndefined = (node: Shape): boolean =>
  node.name === "optional" ||
  node.name === "nullish" ||
  node.name === "undefined" ||
  node.name === "void" ||
  node.name === "any" ||
  node.name === "unknown" ||
  (node.name === "nullable" && admitsUndefined(node.args[0]!)) ||
  (node.name === "union" && node.args.some(admitsUndefined));

const ANY = new Set(["any", "unknown"]);

// A member that takes every value of its kind, so a later member's value is
// its to claim: an object whose every field may be absent accepts any object,
// and a container of `any` accepts any container. Seen through the wrappers
// that only add an absent case.
export const absorbs = (node: Shape): boolean =>
  ANY.has(node.name) ||
  node.name === "fieldOr" ||
  ((node.name === "field" || node.name === "renamed") && admitsUndefined(node.args[0]!)) ||
  ((node.name === "list" || node.name === "array") && some(node.args[0]!, (n) => ANY.has(n.name))) ||
  (["optional", "nullable", "nullish"].includes(node.name) && absorbs(node.args[0]!));
