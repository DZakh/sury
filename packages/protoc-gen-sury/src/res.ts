import type { Element, Enum, Field, File, Message, Oneof, Scalar } from "./model";
import { nestedTypes } from "./model";
import { namesOf } from "./names";
import { type Options, componentsOf, isStruct, wktFiles, wrapperScalar } from "./shared";

const keywords = new Set([
  "and", "as", "assert", "async", "await", "catch", "constraint", "downto", "else", "exception", "external",
  "false", "for", "if", "in", "include", "lazy", "let", "list", "module", "mutable", "of", "open", "private",
  "rec", "switch", "to", "true", "try", "type", "when", "while", "with",
]);

const capitalize = (name: string): string => name.charAt(0).toUpperCase() + name.slice(1);
const uncapitalize = (name: string): string => name.charAt(0).toLowerCase() + name.slice(1);

// A lowercase identifier: a field, a type or a binding.
const lowerIdent = (name: string): string => {
  let out = uncapitalize(name.replace(/\$/g, "_").replace(/[^A-Za-z0-9_]/g, "_"));
  if (/^[0-9]/.test(out)) out = `_${out}`;
  return keywords.has(out) ? `${out}_` : out;
};

// An uppercase identifier: a module or a constructor. SCREAMING_SNAKE becomes
// PascalCase, as ReScript spells a variant.
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

// A record field per member, the protobuf-es property made a ReScript label.
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

export const moduleOf = (desc: Message | Enum): string => upperIdent(namesOf(desc.file).shape.get(desc)!.replace(/\$/g, "_"));

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

type Scope = { bindings: Map<Message, string>; component?: Set<Message>; depth: number };

export type ResContext = {
  // How this file refers to Sury: `S.` in a consumer's file, nothing inside S.res.
  s: string;
  // Where a well-known type another file declares lives.
  wkt: (desc: Message | Enum) => string;
};

// The well-known types ship in sury as `SuryProtobuf`: a module of its own, so
// a ReScript program that never names one never loads them.
export const wktModule = "SuryProtobuf";

export const consumerContext = (generating: Set<string>): ResContext => ({
  s: "S.",
  wkt: (desc) => (wktFiles.has(desc.file.proto.name) && !generating.has(desc.file.proto.name) ? `${wktModule}.${moduleOf(desc)}` : ""),
});

// The modules of a file: each enum and message a module holding its type `t`
// and its `schema`. A recursive group's types are declared together first,
// since a ReScript type can't refer to one declared after it.
export const resModules = (file: File, ctx: ResContext): { body: string; usesNumber: boolean } => {
  const S = ctx.s;
  let usesNumber = false;
  const ref = (desc: Message | Enum): string => {
    if (desc.file === file) return moduleOf(desc);
    return ctx.wkt(desc) || `${fileModule(desc.file)}.${moduleOf(desc)}`;
  };
  // Inside a recursive group, a type is its group name rather than `M.t`,
  // which isn't declared yet where the group is.
  const groupNames = new Map<Message, string>();
  const typeRef = (desc: Message | Enum): string => groupNames.get(desc as Message) ?? `${ref(desc)}.t`;

  const scalarSchema = (scalar: Scalar, asString: boolean): string => {
    if (asString && scalar.includes("64")) return `${S}string`;
    const name = scalarTypes[scalar][1];
    if (name === "number") {
      usesNumber = true;
      return "number";
    }
    return `${S}${name}`;
  };

  const scalarType = (scalar: Scalar, asString: boolean): string => {
    if (asString && scalar.includes("64")) return "string";
    const type = scalarTypes[scalar][0];
    return type.startsWith("S.") ? `${S}${type.slice(2)}` : type;
  };

  const elementType = (field: Field, element: Element): string => {
    if (element.kind === "scalar") return scalarType(element.scalar, field.longAsString && field.mapKey === undefined);
    if (element.kind === "enum") return typeRef(element.enum);
    if (isStruct(field, element)) return "dict<JSON.t>";
    return typeRef(element.message);
  };

  const oneofTypeName = (message: Message, oneof: Oneof): string =>
    groupNames.has(message) ? `${groupNames.get(message)}_${lowerIdent(oneof.localName)}` : lowerIdent(oneof.localName);

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
    if (field.list && !field.packed && (field.element.kind === "enum" || (field.element.kind === "scalar" && field.element.scalar !== "string" && field.element.scalar !== "bytes"))) {
      args += ", ~packed=false";
    }
    return `${schema}->${S}protobufField(${args})`;
  };

  const messageRef = (message: Message, scope: Scope): string => {
    const bound = scope.bindings.get(message);
    if (bound !== undefined) return bound;
    if (scope.component?.has(message)) return recursiveExpr(message, scope);
    return `${ref(message)}.schema`;
  };

  const elementSchema = (field: Field, element: Element, scope: Scope): string => {
    if (element.kind === "scalar") return scalarSchema(element.scalar, field.longAsString && field.mapKey === undefined);
    if (element.kind === "enum") return `${ref(element.enum)}.schema`;
    if (isStruct(field, element)) return `${S}dict(${S}json)`;
    return messageRef(element.message, scope);
  };

  const fieldSchema = (field: Field, scope: Scope): string => {
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

  const oneofSchema = (message: Message, oneof: Oneof, scope: Scope, indent: string): string => {
    const arms = oneof.fields.map((field) => {
      const element = elementSchema(field, field.element, scope);
      return `${indent}    ${S}schema(s => ${upperIdent(field.localName)}({value: s.matches(${numbered(field, element, wireType(field, field.element))})})),`;
    });
    void message;
    return `${S}option(\n${indent}  ${S}union([\n${arms.join("\n")}\n${indent}  ]),\n${indent})`;
  };

  const recordBody = (message: Message, scope: Scope, indent: string): string => {
    const names = fieldNames(message);
    const lines = message.members.map((member) =>
      member.kind === "oneof"
        ? `${indent}  ${names.get(member)}: s.matches(${oneofSchema(message, member, scope, `${indent}  `)}),`
        : `${indent}  ${names.get(member)}: s.matches(${fieldSchema(member, scope)}),`,
    );
    return `{\n${lines.join("\n")}\n${indent}}`;
  };

  const objectSchema = (message: Message, scope: Scope, indent: string): string =>
    message.members.length === 0 ? `${S}schema(_ => %raw(\`{}\`))` : `${S}schema(s => ${recordBody(message, scope, indent)})`;

  function recursiveExpr(message: Message, scope: Scope): string {
    const name = scope.depth === 0 ? "self" : `self${scope.depth + 1}`;
    const inner: Scope = { bindings: new Map(scope.bindings).set(message, name), component: scope.component, depth: scope.depth + 1 };
    const indent = "  ".repeat(scope.depth + 2);
    const body = objectSchema(message, inner, indent);
    const binder = new RegExp(`\\b${name}\\b`).test(body) ? name : `_${name}`;
    const annotated = scope.depth === 0 ? binder : `(${binder}: ${S}t<${typeRef(message)}>)`;
    return `${S}recursive(${JSON.stringify(namesOf(file).shape.get(message))}, ${annotated} =>\n${indent}${body.replace(new RegExp(`\\b${name}\\b`, "g"), binder)}\n${"  ".repeat(scope.depth + 1)})`;
  }

  const docstring = (path: number[], indent: string): string => {
    const text = file.comments.get(path.join("."));
    if (text === undefined || text.trim() === "") return "";
    const lines = text.replace(/\n$/, "").split("\n").map((line) => (line.startsWith(" ") ? line.slice(1) : line).trimEnd());
    return `${indent}/**\n${lines.map((line) => (line ? `${indent} ${line.replace(/\*\//g, "*\\/")}` : "")).join("\n")}\n${indent} */\n`;
  };

  const oneofDecl = (message: Message, oneof: Oneof, keyword: string, indent: string): string => {
    const arms = oneof.fields.map(
      (field) => `${indent}  | @as(${JSON.stringify(field.localName)}) ${upperIdent(field.localName)}({value: ${fieldType({ ...field, list: false, optional: false, element: field.element })
        .replace(/^option<(.*)>$/, "$1")}})`,
    );
    return `${indent}@tag("case")\n${indent}${keyword} ${oneofTypeName(message, oneof)} =\n${arms.join("\n")}\n`;
  };

  const recordDecl = (message: Message, indent: string): string => {
    const names = fieldNames(message);
    const lines = message.members.map((member) =>
      member.kind === "oneof"
        ? `${indent}  ${names.get(member)}: option<${oneofTypeName(message, member)}>,`
        : `${docstring(member.path, `${indent}  `)}${indent}  ${names.get(member)}: ${fieldType(member)},`,
    );
    return `{\n${lines.join("\n")}\n${indent}}`;
  };

  const out: string[] = [];
  for (const desc of nestedTypes(file)) {
    if (desc.kind !== "enum") continue;
    const seen = new Set<number>();
    const values = desc.values.filter((value) => !seen.has(value.number) && seen.add(value.number));
    const constructors = values.map((value) => upperIdent(value.localName));
    out.push(
      `${docstring(desc.path, "")}module ${moduleOf(desc)} = {\n` +
        `  type t =${values.map((value, idx) => ` | @as(${value.number}) ${constructors[idx]}`).join("")}\n` +
        `  let schema: ${S}t<t> = ${S}enum([${constructors.join(", ")}])\n}\n`,
    );
  }

  const messages = [...nestedTypes(file)].filter((desc): desc is Message => desc.kind === "message");
  for (const component of componentsOf(messages)) {
    const recursive = component.length > 1 || component[0]!.fields.some((field) => field.element.kind === "message" && field.element.message === component[0]);
    if (recursive) {
      for (const message of component) groupNames.set(message, lowerIdent(moduleOf(message)));
      const decls: string[] = [];
      for (const message of component) {
        for (const oneof of message.oneofs) decls.push(oneofDecl(message, oneof, decls.length ? "and" : "type rec", ""));
        const record = message.members.length === 0 ? "" : ` = ${recordDecl(message, "")}`;
        decls.push(`${decls.length ? "and" : "type rec"} ${groupNames.get(message)}${record}\n`);
      }
      out.push(decls.join(""));
      const members = new Set(component);
      for (const message of component) {
        const scope: Scope = { bindings: new Map(), component: members, depth: 0 };
        out.push(
          `${docstring(message.path, "")}module ${moduleOf(message)} = {\n  type t = ${groupNames.get(message)}\n` +
            `  let schema: ${S}t<t> = ${recursiveExpr(message, scope)}\n}\n`,
        );
      }
      continue;
    }
    const message = component[0]!;
    const oneofs = message.oneofs.map((oneof) => oneofDecl(message, oneof, "type", "  ")).join("");
    const type = message.members.length === 0 ? "  type t\n" : `  type t = ${recordDecl(message, "  ")}\n`;
    out.push(
      `${docstring(message.path, "")}module ${moduleOf(message)} = {\n${oneofs}${type}` +
        `  let schema: ${S}t<t> = ${objectSchema(message, { bindings: new Map(), depth: 0 }, "  ")}\n}\n`,
    );
  }
  return { body: out.join("\n"), usesNumber };
};

// `S.float` refuses NaN, which a double or float holds as readily as any value.
export const numberDecl = (s: string): string => `let number = ${s}union([${s}float, ${s}literal(Float.Constants.nan)])\n`;

export const emitRes = (file: File, options: Options, generating: Set<string>, version: string): { name: string; content: string } => {
  const { body, usesNumber } = resModules(file, consumerContext(generating));
  const params = options.raw ? ` with parameter "${options.raw}"` : "";
  const header =
    `// @generated by protoc-gen-sury${version ? ` v${version}` : ""}${params}\n` +
    `// @generated from file ${file.proto.name} (${file.proto.package ? `package ${file.proto.package}, ` : ""}syntax proto3)\n` +
    `@@warning("-30")\n\n`;
  const dir = file.name.includes("/") ? `${file.name.slice(0, file.name.lastIndexOf("/"))}/` : "";
  return { name: `${dir}${fileModule(file)}.res`, content: `${header}${usesNumber ? `${numberDecl("S.")}\n` : ""}${body}` };
};
