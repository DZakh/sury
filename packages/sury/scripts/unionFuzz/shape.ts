// The tree a generated member is built from, carried beside its printed id so a
// `KNOWN_BUGS` entry can be described by the shape that triggers it - "a member
// whose default takes a later member's `undefined`" - at any depth, rather than
// by a substring of the id that only matches the depths someone happened to
// write down. The grammar builds it at the same place it builds the schema
// (`generate.ts`).

export type Shape = {
  name: string;
  args: Shape[];
  // A modifier's name (`with`), or a default's printed value (`#value`).
  raw?: string;
  form?: "list" | "tree" | "union";
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

// `env` reads an unset variable, so its input admits `undefined` too.
export const admitsUndefined = (node: Shape): boolean =>
  node.name === "optional" ||
  node.name === "getOr" ||
  node.name === "env" ||
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
  node.name === "record" ||
  // `{head, next?}` over a head that may be absent, and it or `head | self[]`
  // over `any`/`unknown`: every field may be absent, or anything goes.
  (node.name === "recursive" &&
    node.form !== "tree" &&
    (ANY.has(node.args[0]!.name) || (node.form === "list" && admitsUndefined(node.args[0]!)))) ||
  ((node.name === "field" || node.name === "renamed") && admitsUndefined(node.args[0]!)) ||
  ((node.name === "list" || node.name === "array") && some(node.args[0]!, (n) => ANY.has(n.name))) ||
  (["optional", "nullable", "nullish"].includes(node.name) && absorbs(node.args[0]!));

const admitsNull = (node: Shape): boolean =>
  ANY.has(node.name) ||
  ["null", "json", "nullable", "nullish", "fromEmpty"].includes(node.name) ||
  (node.name === "optional" && admitsNull(node.args[0]!)) ||
  (node.name === "union" && node.args.some(admitsNull));

export const shadowsEmpty = (union: Shape): boolean =>
  union.args.some((node, idx) => {
    if (node.args[1]?.name !== "#value") return false;
    const later = union.args.slice(idx + 1);
    return node.name === "optional" ? later.some(admitsUndefined) : node.name === "nullable" && later.some(admitsNull);
  });
