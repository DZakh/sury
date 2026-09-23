// A generated member's id, read back into the tree it was printed from, so a
// known bug can be described by the shape that triggers it - "a union with a
// member that carries a default" - at any depth, rather than by a substring
// that only matches the depths someone happened to write down.
//
// The printers are in `generate.ts` and `catalog.ts`; this reads what they
// write. A default is kept as raw text, since it is a value, not a shape.

export type Shape = {
  name: string;
  args: Shape[];
  // The raw text of a default, or of anything that is a value rather than a
  // schema (`FormData(a=1)`, `"e1"`).
  raw?: string;
};

const leaf = (name: string): Shape => ({ name, args: [] });
const raw = (text: string): Shape => ({ name: "#value", args: [], raw: text });

// Splits at top-level commas, stepping over strings and every bracket kind.
const split = (text: string): string[] => {
  const parts: string[] = [];
  let depth = 0;
  let quoted = false;
  let start = 0;
  for (let i = 0; i < text.length; i++) {
    const ch = text[i]!;
    if (quoted) {
      if (ch === "\\") i++;
      else if (ch === '"') quoted = false;
    } else if (ch === '"') quoted = true;
    else if (ch === "(" || ch === "[" || ch === "{") depth++;
    else if (ch === ")" || ch === "]" || ch === "}") depth--;
    else if (ch === "," && depth === 0) {
      parts.push(text.slice(start, i));
      start = i + 1;
    }
  }
  parts.push(text.slice(start));
  return parts;
};

// The index of the bracket closing the one at `open`.
const closing = (text: string, open: number): number => {
  let depth = 0;
  let quoted = false;
  for (let i = open; i < text.length; i++) {
    const ch = text[i]!;
    if (quoted) {
      if (ch === "\\") i++;
      else if (ch === '"') quoted = false;
    } else if (ch === '"') quoted = true;
    else if (ch === "(" || ch === "[" || ch === "{") depth++;
    else if (ch === ")" || ch === "]" || ch === "}") {
      depth--;
      if (depth === 0) return i;
    }
  }
  return text.length - 1;
};

// `optional` and `nullable` print their default as a second argument.
const DEFAULTABLE = new Set(["optional", "nullable"]);

export const parseShape = (text: string): Shape => {
  text = text.trim();
  if (text.startsWith("{")) {
    const inner = text.slice(1, closing(text, 0));
    const tail = text.slice(closing(text, 0) + 1);
    let node: Shape;
    if (inner.startsWith("fieldOr(")) {
      const [, item, value] = split(inner.slice(8, closing(inner, 7)));
      node = { name: "fieldOr", args: [parseShape(item!), raw(value!)] };
    } else if (inner.startsWith("TAG:R,_0:<-a:")) {
      node = { name: "renamed", args: [parseShape(inner.slice(13))] };
    } else if (/^(TAG:T\d+|kind:k\d+),/.test(inner)) {
      node = { name: "tagged", args: [parseShape(inner.slice(inner.indexOf(",") + 1))] };
    } else if (inner.startsWith("f:")) {
      node = { name: "field", args: [parseShape(inner.slice(2))] };
    } else {
      node = leaf("payload");
    }
    return withSuffixes(node, tail);
  }
  const name = /^[\w$]+/.exec(text)?.[0];
  if (!name) return raw(text);
  let rest = text.slice(name.length);
  let node: Shape = leaf(name);
  if (rest.startsWith("(")) {
    const end = closing(rest, 0);
    const parts = split(rest.slice(1, end));
    node = {
      name,
      args: parts.map((part, i) =>
        DEFAULTABLE.has(name) && i === 1 ? raw(part) : name === "enum" ? raw(part) : parseShape(part),
      ),
    };
    rest = rest.slice(end + 1);
  }
  return withSuffixes(node, rest);
};

// `.with(to)` and friends wrap what they follow.
const withSuffixes = (node: Shape, rest: string): Shape => {
  while (rest.startsWith(".with(")) {
    const end = closing(rest, 5);
    node = { name: "with", args: [node], raw: rest.slice(6, end) };
    rest = rest.slice(end + 1);
  }
  return node;
};

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
export const hasDefault = (node: Shape): boolean =>
  DEFAULTABLE.has(node.name) && node.args.length === 2;

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
