import { type Element, type Enum, type Field, type File, type Message, type Oneof, nestedTypes, type Scalar } from "./model";
import { namesOf } from "./names";
import { type Group, type Options, componentsOf, groupOf, groupRef, header, inCallback, is64, isRecursive, messageOf, importPath, isStruct, jsdoc, outputPath, packable, statedKey, statedType, wrapperScalar, wktImport } from "./shared";

const scalarType = (scalar: Scalar, asString: boolean): string =>
  is64(scalar) ? (asString ? "string" : "bigint")
  : scalar === "bool" ? "boolean"
  : scalar === "string" ? "string"
  : scalar === "bytes" ? "Uint8Array"
  : "number";

const scalarSchemas: Record<Scalar, string> = {
  double: "$number",
  float: "$number",
  int32: "int32",
  sint32: "int32",
  sfixed32: "int32",
  uint32: "integer",
  fixed32: "integer",
  int64: "bigint",
  uint64: "bigint",
  sint64: "bigint",
  fixed64: "bigint",
  sfixed64: "bigint",
  bool: "boolean",
  string: "string",
  bytes: "uint8Array",
};

const property = (name: string): string => (/^[A-Za-z_$][\w$]*$/.test(name) && name !== "__proto__" ? name : JSON.stringify(name));

class Symbols {
  private readonly taken: Set<string>;
  private readonly imports = new Map<string, Map<string, { alias: string; typeOnly: boolean }>>();
  readonly S: string;

  constructor(
    private readonly file: File,
    private readonly options: Options,
    private readonly generating: Set<string>,
  ) {
    const names = namesOf(file);
    this.taken = new Set([...names.shape.values(), ...names.schema.values()]);
    let s = "S";
    for (let i = 1; this.taken.has(s); i++) s = `S$${i === 1 ? "" : i - 1}`;
    this.taken.add(s);
    this.S = s;
  }

  private use(from: string, name: string, typeOnly: boolean): string {
    let module = this.imports.get(from);
    if (module === undefined) this.imports.set(from, (module = new Map()));
    const existing = module.get(name);
    if (existing !== undefined) {
      if (!typeOnly) existing.typeOnly = false;
      return existing.alias;
    }
    let alias = name;
    for (let i = 1; this.taken.has(alias); i++) alias = `${name}$${i}`;
    this.taken.add(alias);
    module.set(name, { alias, typeOnly });
    return alias;
  }

  ref(desc: Message | Enum, kind: "shape" | "schema"): string {
    const name = namesOf(desc.file)[kind].get(desc)!;
    if (desc.file === this.file) return name;
    const wkt = wktImport(desc.file, this.generating);
    const from = wkt ?? importPath(this.file, desc.file, this.options);
    // A type is only named in a type position, and an enum's object doubles
    // as its type, so only a message's type imports type-only.
    return this.use(from, name, kind === "shape" && desc.kind === "message");
  }

  jsonObject(): string {
    return this.use("sury/protobuf/wkt", "JsonObject", true);
  }

  importDecls(): string {
    let out = `import * as ${this.S} from "sury";\n`;
    for (const [from, names] of this.imports) {
      const parts = [...names].map(([name, { alias, typeOnly }]) =>
        `${typeOnly ? "type " : ""}${name}${alias === name ? "" : ` as ${alias}`}`,
      );
      out += `import { ${parts.join(", ")} } from "${from}";\n`;
    }
    return out;
  }
}

export const emitTs = (file: File, options: Options, generating: Set<string>, version: string): { name: string; content: string } => {
  const sym = new Symbols(file, options, generating);
  const S = sym.S;
  const names = namesOf(file);
  const comment = (path: number[], tag: string, deprecated = false, indent = ""): string =>
    jsdoc(file.comments.get(path.join(".")), tag, deprecated, indent);

  // `S.number` refuses NaN, which a double or float holds as readily as any
  // other value; the file declares the schema that admits it once.
  let usesNumber = false;
  const scalarSchema = (name: string): string => {
    if (name !== "$number") return `${S}.${name}`;
    usesNumber = true;
    return "$number";
  };

  const elementType = (field: Field, element: Element): string => {
    if (element.kind === "scalar") return scalarType(element.scalar, field.longAsString && field.mapKey === undefined);
    if (element.kind === "enum") return sym.ref(element.enum, "shape");
    if (isStruct(field, element)) return sym.jsonObject();
    return sym.ref(messageOf(element), "shape");
  };

  const fieldType = (field: Field): string => {
    const wrapped = wrapperScalar(field);
    if (wrapped !== undefined) return scalarType(wrapped, false);
    const element = elementType(field, field.element);
    if (field.mapKey !== undefined) return `{ [key: string]: ${element} }`;
    return field.list ? `${element}[]` : element;
  };

  // A group member is read by the name of its export, which the group's
  // variable and the callback parameter holding it shadow.
  const messageRef = (message: Message, group: Group | undefined): string => {
    if (!group?.members.has(message)) return sym.ref(message, "schema");
    const name = names.schema.get(message)!;
    groupRef(group, message, () => [`${name} = ${member(message, group)};`]);
    return name;
  };

  const elementSchema = (field: Field, element: Element, scope: Group | undefined): string => {
    if (element.kind === "scalar") {
      return scalarSchema(field.longAsString && field.mapKey === undefined && is64(element.scalar) ? "string" : scalarSchemas[element.scalar]);
    }
    if (element.kind === "enum") return sym.ref(element.enum, "schema");
    if (isStruct(field, element)) return `${S}.record(${S}.json)`;
    return messageRef(messageOf(element), scope);
  };

  const numbered = (field: Field, schema: string, type: string | undefined): string => {
    const opts = [`number: ${field.number}`];
    if (type !== undefined) opts.push(`type: ${JSON.stringify(type)}`);
    if (statedKey(field) !== undefined) opts.push(`key: ${JSON.stringify(field.mapKey)}`);
    if (field.list && !field.packed && packable(field.element)) opts.push("packed: false");
    return `${S}.protobufField(${schema}, ${opts.length === 1 ? field.number : `{ ${opts.join(", ")} }`})`;
  };

  const fieldSchema = (field: Field, scope: Group | undefined): string => {
    const wrapped = wrapperScalar(field);
    if (wrapped !== undefined) {
      return numbered(field, `${S}.optional(${scalarSchema(scalarSchemas[wrapped])})`, (field.element as { message: Message }).message.typeName);
    }
    const element = elementSchema(field, field.element, scope);
    const type = statedType(field, field.element);
    if (field.mapKey !== undefined) return numbered(field, `${S}.record(${element})`, type);
    if (field.list) return numbered(field, `${S}.array(${element})`, type);
    const present = field.optional || field.element.kind === "message";
    return numbered(field, present ? `${S}.optional(${element})` : element, type);
  };

  const oneofType = (oneof: Oneof, indent: string): string =>
    [
      ...oneof.fields.map((field) => `${indent}  | { case: ${JSON.stringify(field.localName)}; value: ${elementType(field, field.element)} }`),
      `${indent}  | { case: undefined; value?: undefined }`,
    ].join("\n");

  const oneofSchema = (oneof: Oneof, scope: Group | undefined, indent: string): string => {
    const arms = oneof.fields.map((field) => {
      const element = elementSchema(field, field.element, scope);
      return `${indent}  ${S}.schema({ case: ${JSON.stringify(field.localName)}, value: ${numbered(field, element, statedType(field, field.element))} }),`;
    });
    arms.push(`${indent}  /* @__PURE__ */ ${S}.schemaOf<{ case: undefined; value?: undefined }>()({ case: undefined, value: ${S}.optional(${S}.schema(undefined)) }),`);
    return `${S}.union([\n${arms.join("\n")}\n${indent}])`;
  };

  const objectBody = (message: Message, scope: Group | undefined, indent: string): string => {
    const lines = message.members.map((member) =>
      member.kind === "oneof"
        ? `${indent}  ${property(member.localName)}: ${oneofSchema(member, scope, `${indent}  `)},`
        : `${indent}  ${property(member.localName)}: ${fieldSchema(member, scope)},`,
    );
    return lines.length ? `{\n${lines.join("\n")}\n${indent}}` : "{}";
  };

  function member(message: Message, group: Group): string {
    const depth = group.open.length + (group.members.size > 1 ? 1 : 0);
    const indent = "  ".repeat(depth + 1);
    const shape = names.shape.get(message)!;
    const { body, statements, readsItself } = inCallback(group, message, () => typed(`${S}.schema(${objectBody(message, group, indent)})`, message));
    const head = `${S}.recursive<${shape}>(${JSON.stringify(shape)}, (${readsItself ? names.schema.get(message) : ""}) =>`;
    const close = "  ".repeat(depth);
    return statements.length === 0
      ? `${head}\n${indent}${body},\n${close})`
      : `${head} {\n${statements.map((statement) => `${indent}${statement}\n`).join("")}${indent}return ${body};\n${close}})`;
  }

  const typeDecl = (message: Message): string => {
    const lines = message.members.map((member) => {
      if (member.kind === "oneof") {
        return `${comment(member.path, `@generated from oneof ${message.typeName}.${member.name}`, false, "  ")}  ${property(member.localName)}:\n${oneofType(member, "  ")};`;
      }
      // `| undefined` is what S.optional decodes an absent field to, and what a
      // consumer under exactOptionalPropertyTypes is checked against.
      const optional = wrapperScalar(member) !== undefined || (!member.list && member.mapKey === undefined && (member.optional || member.element.kind === "message"));
      return `${comment(member.path, `@generated from field: ${fieldDeclaration(member)}`, member.deprecated, "  ")}  ${property(member.localName)}${optional ? "?" : ""}: ${fieldType(member)}${optional ? " | undefined" : ""};`;
    });
    return `${comment(message.path, `@generated from message ${message.typeName}`, message.deprecated)}export type ${names.shape.get(message)} = {${lines.length ? `\n${lines.join("\n\n")}\n` : ""}};\n`;
  };

  const out: string[] = [];
  const enums = [...nestedTypes(file)].filter((desc): desc is Enum => desc.kind === "enum");
  const messages = [...nestedTypes(file)].filter((desc): desc is Message => desc.kind === "message");
  // On the definition itself, so every path to a recursive message reaches it.
  const typed = (schema: string, desc: Message | Enum): string => `${S}.protobufTypeName(${schema}, ${JSON.stringify(desc.typeName)})`;

  for (const desc of enums) {
    const name = names.shape.get(desc)!;
    const members = desc.values.map((value) =>
      `${comment(value.path, `@generated from enum value: ${value.name} = ${value.number};`, value.deprecated, "  ")}  ${property(value.localName)}: ${value.number},`,
    );
    const numbers = [...new Set(desc.values.map((value) => value.number))];
    const doc = comment(desc.path, `@generated from enum ${desc.typeName}`, desc.deprecated);
    out.push(
      `${doc}export const ${name} = {\n${members.join("\n\n")}\n} as const;\n\n` +
        `${doc}export type ${name} = (typeof ${name})[keyof typeof ${name}];\n\n` +
        `export const ${names.schema.get(desc)} = ${typed(`${S}.union([${numbers.join(", ")}])`, desc)};\n`,
    );
  }

  for (const component of componentsOf(messages)) {
    for (const message of component) out.push(typeDecl(message));
    const [root, ...rest] = component.map((message) => names.schema.get(message)!) as [string, ...string[]];
    if (isRecursive(component)) {
      const built = member(component[0]!, groupOf(component));
      if (rest.length === 0) {
        out.push(`export const ${root} = ${built};\n`);
        continue;
      }
      const all = [root, ...rest].join(", ");
      const variables = component.slice(1).map((message) => `  let ${names.schema.get(message)}!: ${S}.Schema<${names.shape.get(message)}>;\n`);
      out.push(`export const [${all}] = (() => {\n${variables.join("")}  const ${root} = ${built};\n  return [${all}] as const;\n})();\n`);
      continue;
    }
    const message = component[0]!;
    const shape = names.shape.get(message)!;
    if (message.typeName === "google.protobuf.Any") {
      // Built from its fields, Any would lack the pack and unpack `S.protobufAny` carries.
      out.push(`export const ${root}: ${S}.Schema<${shape}, ${shape}> = ${S}.protobufAny;\n`);
    } else {
      const body = objectBody(message, undefined, "");
      // `S.schemaOf` is annotated, the function it returns can't be: without
      // this, a bundler keeps every message of a file once one is imported.
      out.push(`export const ${root} = ${typed(`${S}.meta(/* @__PURE__ */ ${S}.schemaOf<${shape}>()(${body}), { name: ${JSON.stringify(shape)} })`, message)};\n`);
    }
  }

  const body = (usesNumber ? `const $number = ${S}.union([${S}.number, ${S}.schema(NaN)]);\n\n` : "") + out.join("\n");
  return {
    name: `${outputPath(file)}.ts`,
    content: `${header(file, options, version)}/* eslint-disable */\n${options.tsNocheck ? "// @ts-nocheck\n" : ""}\n${sym.importDecls()}\n${body}`,
  };
};

// The field as protobuf-es writes it after `@generated from field:`.
const fieldDeclaration = (field: Field): string => {
  const typeName = (element: Element): string =>
    element.kind === "scalar" ? element.scalar : element.kind === "enum" ? element.enum.typeName : messageOf(element).typeName;
  const opts: string[] = [];
  if (field.list && field.proto.options?.packed === false) opts.push("packed = false");
  if (field.longAsString) opts.push("jstype = JS_STRING");
  if (field.deprecated) opts.push("deprecated = true");
  const rule = field.mapKey !== undefined ? "" : field.list ? "repeated " : field.optional ? "optional " : "";
  const type = field.mapKey !== undefined ? `map<${field.mapKey}, ${typeName(field.element)}>` : typeName(field.element);
  return `${rule}${type} ${field.name} = ${field.number}${opts.length ? ` [${opts.join(", ")}]` : ""};`;
};
