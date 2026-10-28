import type { Element, Enum, Field, File, Message, Oneof, Scalar } from "./model";
import { nestedTypes } from "./model";
import { namesOf } from "./names";
import { type Group, type Options, componentsOf, groupOf, groupRef, header, inCallback, is64, isRecursive, messageOf, isStruct, packable, wktImport, wrapperScalar } from "./shared";

const keywords = new Set([
  "and", "as", "assert", "async", "await", "catch", "constraint", "downto", "else", "exception", "external",
  "false", "for", "if", "in", "include", "lazy", "let", "list", "module", "mutable", "of", "open", "private",
  "rec", "switch", "to", "true", "try", "type", "when", "while", "with",
]);

const capitalize = (name: string): string => name.charAt(0).toUpperCase() + name.slice(1);
const uncapitalize = (name: string): string => name.charAt(0).toLowerCase() + name.slice(1);

const lowerIdent = (name: string): string => {
  let out = uncapitalize(name.replace(/\$/g, "_").replace(/[^A-Za-z0-9_]/g, "_"));
  if (/^[0-9]/.test(out)) out = `_${out}`;
  return keywords.has(out) ? `${out}_` : out;
};

// SCREAMING_SNAKE becomes PascalCase, as ReScript spells a variant.
const upperIdent = (name: string): string => {
  const clean = name.replace(/\$/g, "").replace(/[^A-Za-z0-9_]/g, "_");
  const pascal = /^[A-Z0-9_]+$/.test(clean)
    ? clean.toLowerCase().split("_").filter(Boolean).map(capitalize).join("")
    : capitalize(clean);
  return /^[A-Z]/.test(pascal) ? pascal : `V${pascal}`;
};

// A file's module: its path, since a ReScript project's namespace is flat and
// two `user.proto` files in different directories would otherwise collide.
export const fileModule = (file: File): string => `${capitalize(file.name.replace(/[^A-Za-z0-9]/g, "_"))}_pb`;

export const fieldNames = (message: Message): Map<Field | Oneof, string> => {
  const names = new Map<Field | Oneof, string>();
  const taken = new Set<string>();
  for (const member of message.members) {
    let name = lowerIdent(member.localName);
    while (taken.has(name)) name = `${name}_`;
    taken.add(name);
    names.set(member, name);
  }
  return names;
};

// The modules the generated code names, which a message or enum declared
// before that code would shadow.
const usedModules = new Set(["Float", "JSON", "S", "Stdlib", "Uint8Array"]);

// The types the generated code names, which a oneof's type or a recursive
// group's would shadow.
const usedTypes = new Set(["array", "bigint", "bool", "dict", "float", "int", "option", "string", "t", "unit"]);

const claim = (name: string, taken: Set<string>): string => {
  while (taken.has(name)) name = `${name}_`;
  taken.add(name);
  return name;
};

const modules = new WeakMap<File, Map<Message | Enum, string>>();

export const moduleOf = (desc: Message | Enum): string => {
  let names = modules.get(desc.file);
  if (names === undefined) {
    const taken = new Set(usedModules);
    names = new Map();
    for (const type of nestedTypes(desc.file)) names.set(type, claim(upperIdent(namesOf(desc.file).shape.get(type)!.replace(/\$/g, "_")), taken));
    modules.set(desc.file, names);
  }
  return names.get(desc)!;
};

const scalarTypes: Record<Scalar, [string, string]> = {
  double: ["float", "number"],
  float: ["float", "number"],
  int32: ["int", "int"],
  sint32: ["int", "int"],
  sfixed32: ["int", "int"],
  uint32: ["S.integer", "integer"],
  fixed32: ["S.integer", "integer"],
  int64: ["bigint", "bigint"],
  uint64: ["bigint", "bigint"],
  sint64: ["bigint", "bigint"],
  fixed64: ["bigint", "bigint"],
  sfixed64: ["bigint", "bigint"],
  bool: ["bool", "bool"],
  string: ["string", "string"],
  bytes: ["Uint8Array.t", "uint8Array"],
};

export type ResContext = {
  // How this file refers to Sury: `S.` in a consumer's file, nothing inside S.res.
  s: string;
  wellKnownModule: (desc: Message | Enum) => string;
  // The JS module each schema is an external from, instead of one built here.
  // An external is an unchecked cast, so every type is then spelled as the
  // value that module's schema holds: a oneof with no member set is
  // `{case: undefined}`, as protobuf-es has it, and a label is the JS property.
  bind?: string;
};

const consumerContext = (generating: Set<string>): ResContext => ({
  s: "S.",
  wellKnownModule: (desc) => (wktImport(desc.file, generating) ? `S.Protobuf.${moduleOf(desc)}` : ""),
});

// A recursive group's types are declared together first, since a ReScript type
// can't refer to one declared after it.
export const resModules = (file: File, ctx: ResContext): { body: string; usesNumber: boolean } => {
  const S = ctx.s;
  let usesNumber = false;
  const ref = (desc: Message | Enum): string => {
    if (desc.file === file) return moduleOf(desc);
    return ctx.wellKnownModule(desc) || `${fileModule(desc.file)}.${moduleOf(desc)}`;
  };
  // Inside a recursive group, a type is its group name rather than `M.t`,
  // which isn't declared yet where the group is. Outside it, `M.t`: a later
  // module's oneof type may take the group name's spelling.
  const groupNames = new Map<Message, string>();
  const groupTaken = new Set(usedTypes);
  let declaring = new Set<Message>();
  const typeRef = (desc: Message | Enum): string => (declaring.has(desc as Message) ? groupNames.get(desc as Message)! : `${ref(desc)}.t`);

  const scalarSchema = (scalar: Scalar, asString: boolean): string => {
    if (asString && is64(scalar)) return `${S}string`;
    const name = scalarTypes[scalar][1];
    if (name === "number") {
      usesNumber = true;
      return "number";
    }
    return `${S}${name}`;
  };

  const scalarType = (scalar: Scalar, asString: boolean): string => {
    if (asString && is64(scalar)) return "string";
    const type = scalarTypes[scalar][0];
    return type.startsWith("S.") ? `${S}${type.slice(2)}` : type;
  };

  const elementType = (field: Field, element: Element): string => {
    if (element.kind === "scalar") return scalarType(element.scalar, field.longAsString && field.mapKey === undefined);
    if (element.kind === "enum") return typeRef(element.enum);
    if (isStruct(field, element)) return "dict<JSON.t>";
    return typeRef(messageOf(element));
  };

  const oneofNames = new Map<Oneof, string>();
  const oneofTypeName = (message: Message, oneof: Oneof): string => {
    if (groupNames.has(message)) return `${groupNames.get(message)}_${lowerIdent(oneof.localName)}`;
    if (!oneofNames.has(oneof)) {
      const taken = new Set(usedTypes);
      for (const other of message.oneofs) oneofNames.set(other, claim(lowerIdent(other.localName), taken));
    }
    return oneofNames.get(oneof)!;
  };

  const fieldType = (field: Field): string => {
    const wrapped = wrapperScalar(field);
    if (wrapped !== undefined) return `option<${scalarType(wrapped, false)}>`;
    const element = elementType(field, field.element);
    if (field.mapKey !== undefined) return `dict<${element}>`;
    if (field.list) return `array<${element}>`;
    return field.optional || field.element.kind === "message" ? `option<${element}>` : element;
  };

  const wireType = (field: Field, element: Element): string =>
    element.kind === "scalar" ? `#${element.scalar}`
    : element.kind === "enum" ? "#enum"
    : isStruct(field, element) ? `#"google.protobuf.Struct"`
    : "#message";

  const numbered = (field: Field, schema: string, type: string): string => {
    let args = `${field.number}, ~type_=${type}`;
    if (field.mapKey !== undefined) args += `, ~key=#${field.mapKey}`;
    if (field.list && !field.packed && packable(field.element)) args += ", ~packed=false";
    return `${schema}->${S}protobufField(${args})`;
  };

  const local = (message: Message): string => `${uncapitalize(moduleOf(message))}Schema`;

  const messageRef = (message: Message, group: Group | undefined): string => {
    if (!group?.members.has(message)) return `${ref(message)}.schema`;
    const name = local(message);
    const at = groupRef(group, message, () => [`let ${name} = ${member(message, group)}`, `${name}Ref.contents = Some(${name})`]);
    return at === "outer" ? `${name}Ref.contents->Stdlib.Option.getUnsafe` : name;
  };

  const elementSchema = (field: Field, element: Element, scope: Group | undefined): string => {
    if (element.kind === "scalar") return scalarSchema(element.scalar, field.longAsString && field.mapKey === undefined);
    if (element.kind === "enum") return `${ref(element.enum)}.schema`;
    if (isStruct(field, element)) return `${S}dict(${S}json)`;
    return messageRef(messageOf(element), scope);
  };

  const fieldSchema = (field: Field, scope: Group | undefined): string => {
    const wrapped = wrapperScalar(field);
    if (wrapped !== undefined) {
      return numbered(field, `${S}option(${scalarSchema(wrapped, false)})`, `#"${(field.element as { message: Message }).message.typeName}"`);
    }
    const element = elementSchema(field, field.element, scope);
    const type = wireType(field, field.element);
    if (field.mapKey !== undefined) return numbered(field, `${S}dict(${element})`, type);
    if (field.list) return numbered(field, `${S}array(${element})`, type);
    return numbered(field, field.optional || field.element.kind === "message" ? `${S}option(${element})` : element, type);
  };

  const oneofSchema = (oneof: Oneof, scope: Group | undefined, indent: string): string => {
    const arms = oneof.fields.map((field) => {
      const element = elementSchema(field, field.element, scope);
      // Named in a group, whose oneofs share one scope and may share a constructor.
      const s = scope === undefined ? "s" : `(s): ${oneofTypeName(field.parent, oneof)}`;
      return `${indent}    ${S}schema(${s} => ${upperIdent(field.localName)}({value: s.matches(${numbered(field, element, wireType(field, field.element))})})),`;
    });
    return `${S}option(\n${indent}  ${S}union([\n${arms.join("\n")}\n${indent}  ]),\n${indent})`;
  };

  const recordBody = (message: Message, scope: Group | undefined, indent: string): string => {
    const names = fieldNames(message);
    const lines = message.members.map((member) =>
      member.kind === "oneof"
        ? `${indent}  ${names.get(member)}: s.matches(${oneofSchema(member, scope, `${indent}  `)}),`
        : `${indent}  ${names.get(member)}: s.matches(${fieldSchema(member, scope)}),`,
    );
    return `{\n${lines.join("\n")}\n${indent}}`;
  };

  const objectSchema = (message: Message, scope: Group | undefined, indent: string): string =>
    message.members.length === 0 ? `${S}schema(_ => ({}: t))` : `${S}schema(s => ${recordBody(message, scope, indent)})`;

  // On the definition itself, so every path to a recursive message reaches it.
  const named = (schema: string, desc: Message | Enum): string => `${schema}->${S}protobufTypeName(${JSON.stringify(desc.typeName)})`;

  // The record is named for the same reason a oneof arm is: a group's records
  // share one scope and may share labels.
  function member(message: Message, group: Group): string {
    const depth = group.open.length;
    const indent = "  ".repeat(depth + 2);
    const { body, statements, readsItself } = inCallback(group, message, () =>
      named(`${S}schema((s): ${groupNames.get(message)} => ${recordBody(message, group, indent)})`, message),
    );
    const head = `${S}recursive(${JSON.stringify(namesOf(file).shape.get(message))}, ${readsItself ? local(message) : "_"} =>`;
    const close = "  ".repeat(depth + 1);
    return statements.length === 0
      ? `${head}\n${indent}${body}\n${close})`
      : `${head} {\n${statements.map((statement) => `${indent}${statement}\n`).join("")}${indent}${body}\n${close}})`;
  }

  // A named function's: ReScript flattens a block, and a function called where
  // it is written, into the module's own statements, where filling a ref is a
  // side effect of the export that does it. A ref is spelled with its type, as
  // a oneof's `None` or `Some` would shadow option's.
  const groupDecl = (root: Message, rest: Message[]): string => {
    const name = `${uncapitalize(moduleOf(root))}Group`;
    const refs = rest.map((message) => `  let ${local(message)}Ref: ref<option<${S}t<${groupNames.get(message)}>>> = {contents: None}\n`);
    const built = member(root, groupOf([root, ...rest]));
    const read = rest.map((message) => `${local(message)}Ref.contents->Stdlib.Option.getUnsafe`);
    return (
      `%%private(let ${name} = () => {\n${refs.join("")}  let ${local(root)} = ${built}\n  (${[local(root), ...read].join(", ")})\n})\n` +
      `%%private(let (${[root, ...rest].map(local).join(", ")}) = ${name}())\n`
    );
  };

  const docstring = (path: number[], indent: string): string => {
    const text = file.comments.get(path.join("."));
    if (text === undefined || text.trim() === "") return "";
    const lines = text.replace(/\n$/, "").split("\n").map((line) => (line.startsWith(" ") ? line.slice(1) : line).trimEnd());
    return `${indent}/**\n${lines.map((line) => (line ? `${indent} ${line.replace(/\*\//g, "*\\/")}` : "")).join("\n")}\n${indent} */\n`;
  };

  const oneofDecl = (message: Message, oneof: Oneof, keyword: string, indent: string): string => {
    const constructors = oneof.fields.map((field) => upperIdent(field.localName));
    const arms = oneof.fields.map(
      (field, idx) => `${indent}  | @as(${JSON.stringify(field.localName)}) ${constructors[idx]}({value: ${elementType(field, field.element)}})`,
    );
    if (ctx.bind !== undefined) {
      let unset = "Unset";
      while (constructors.includes(unset)) unset = `${unset}_`;
      arms.push(`${indent}  | @as(undefined) ${unset}({value?: unit})`);
    }
    return `${indent}@tag("case")\n${indent}${keyword} ${oneofTypeName(message, oneof)} =\n${arms.join("\n")}\n`;
  };

  const recordDecl = (message: Message, indent: string): string => {
    const names = fieldNames(message);
    const label = (member: Field | Oneof): string =>
      ctx.bind !== undefined && names.get(member) !== member.localName ? `@as(${JSON.stringify(member.localName)}) ${names.get(member)}` : names.get(member)!;
    const lines = message.members.map((member) =>
      member.kind === "oneof"
        ? `${indent}  ${label(member)}: ${ctx.bind !== undefined ? oneofTypeName(message, member) : `option<${oneofTypeName(message, member)}>`},`
        : `${docstring(member.path, `${indent}  `)}${indent}  ${label(member)}: ${fieldType(member)},`,
    );
    return `{\n${lines.join("\n")}\n${indent}}`;
  };

  // `t` is the module's own type by the time this is read, so a bound schema
  // names S's schema type by its other name.
  const schemaDecl = (desc: Message | Enum, built: () => string): string =>
    ctx.bind !== undefined
      ? `  @module(${JSON.stringify(ctx.bind)}) external schema: ${S}schema<t> = ${JSON.stringify(namesOf(file).schema.get(desc))}\n`
      : `  let schema: ${S}t<t> = ${built()}\n`;

  const out: string[] = [];
  for (const desc of nestedTypes(file)) {
    if (desc.kind !== "enum") continue;
    const seen = new Set<number>();
    const values = desc.values.filter((value) => !seen.has(value.number) && seen.add(value.number));
    const constructors = values.map((value) => upperIdent(value.localName));
    out.push(
      `${docstring(desc.path, "")}module ${moduleOf(desc)} = {\n` +
        `  type t =${values.map((value, idx) => ` | @as(${value.number}) ${constructors[idx]}`).join("")}\n` +
        `${schemaDecl(desc, () => named(`${S}enum([${constructors.join(", ")}])`, desc))}}\n`,
    );
  }

  const messages = [...nestedTypes(file)].filter((desc): desc is Message => desc.kind === "message");
  for (const component of componentsOf(messages)) {
    if (isRecursive(component)) {
      for (const message of component) groupNames.set(message, claim(lowerIdent(moduleOf(message)), groupTaken));
      const decls: string[] = [];
      declaring = new Set(component);
      for (const message of component) {
        for (const oneof of message.oneofs) decls.push(oneofDecl(message, oneof, decls.length ? "and" : "type rec", ""));
        const record = message.members.length === 0 ? "" : ` = ${recordDecl(message, "")}`;
        decls.push(`${decls.length ? "and" : "type rec"} ${groupNames.get(message)}${record}\n`);
      }
      declaring = new Set();
      out.push(decls.join(""));
      const [root, ...rest] = component as [Message, ...Message[]];
      if (rest.length > 0 && ctx.bind === undefined) out.push(groupDecl(root, rest));
      for (const message of component) {
        out.push(
          `${docstring(message.path, "")}module ${moduleOf(message)} = {\n  type t = ${groupNames.get(message)}\n` +
            `${schemaDecl(message, () => (rest.length === 0 ? member(root, groupOf(component)) : local(message)))}}\n`,
        );
      }
      continue;
    }
    const message = component[0]!;
    const oneofs = message.oneofs.map((oneof) => oneofDecl(message, oneof, "type", "  ")).join("");
    const any = message.typeName === "google.protobuf.Any";
    const type =
      message.members.length === 0 ? "  type t = {}\n"
      : `  type t = ${any ? `${S}protobufAny = ` : ""}${recordDecl(message, "  ")}\n`;
    out.push(
      `${docstring(message.path, "")}module ${moduleOf(message)} = {\n${oneofs}${type}` +
        schemaDecl(message, () => (any ? `${S}protobufAny` : named(objectSchema(message, undefined, "  "), message))) +
        "}\n",
    );
  }
  return { body: out.join("\n"), usesNumber };
};

// Where a file's ReScript goes, without the extension: beside its .proto, named as its module.
export const resPath = (file: File): string => `${file.name.slice(0, file.name.lastIndexOf("/") + 1)}${fileModule(file)}`;

export const emitRes = (file: File, options: Options, generating: Set<string>, version: string): { name: string; content: string } => {
  const { body, usesNumber } = resModules(file, consumerContext(generating));
  // `S.float` refuses NaN, which a double or float holds as readily as any value.
  const number = usesNumber ? "let number = S.union([S.float, S.literal(Float.Constants.nan)])\n\n" : "";
  return { name: `${resPath(file)}.res`, content: `${header(file, options, version)}@@warning("-30")\n\n${number}${body}` };
};
