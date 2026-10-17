import { type Enum, type File, type Message, nestedTypes } from "./model";

// protoplugin's safeIdentifier: names a TypeScript declaration can't take.
const reservedIdentifiers = new Set([
  "break", "case", "catch", "class", "const", "continue", "debugger", "default", "delete", "do", "else",
  "export", "extends", "false", "finally", "for", "function", "if", "import", "in", "instanceof", "new",
  "null", "return", "super", "switch", "this", "throw", "true", "try", "typeof", "var", "void", "while",
  "with", "yield", "enum", "implements", "interface", "let", "package", "private", "protected", "public",
  "static", "Object", "bigint", "number", "boolean", "string", "object", "globalThis", "Uint8Array", "Partial",
]);

const safeIdentifier = (name: string): string => (reservedIdentifiers.has(name) ? `${name}$` : name);

// The type name without the package, nesting joined by `_`: `pkg.User.Address`
// is `User_Address`.
const identifier = (desc: Message | Enum): string => {
  const pkg = desc.file.proto.package;
  return desc.typeName.substring(pkg.length > 0 ? pkg.length + 1 : 0).replace(/\./g, "_");
};

const salt = (i: number): string => (i === 0 ? "" : i === 1 ? "$" : `$${i - 1}`);

export type Names = { shape: Map<Message | Enum, string>; schema: Map<Message | Enum, string> };

const cache = new WeakMap<File, Names>();

// Every name a file exports, clashes resolved the way protoplugin resolves them:
// shapes first, then the file's descriptor name, then the `Schema` names.
export const namesOf = (file: File): Names => {
  let names = cache.get(file);
  if (names !== undefined) return names;
  const taken = new Set<string>();
  const claim = (ideal: (i: number) => string): string => {
    let name: string;
    for (let i = 0; ; i++) if (!taken.has((name = ideal(i)))) break;
    taken.add(name);
    return name;
  };
  names = { shape: new Map(), schema: new Map() };
  for (const desc of nestedTypes(file)) names.shape.set(desc, claim((i) => safeIdentifier(identifier(desc) + salt(i))));
  // protobuf-es exports `file_<path>` beside them, which no generated file here
  // does; it still takes the name first, so every later one lands where
  // protobuf-es puts it.
  claim((i) => safeIdentifier(`file_${file.proto.name.replace(/[^a-zA-Z0-9_]+/g, "_")}${salt(i)}`));
  for (const desc of nestedTypes(file)) names.schema.set(desc, claim((i) => safeIdentifier(`${identifier(desc)}Schema${salt(i)}`)));
  cache.set(file, names);
  return names;
};
