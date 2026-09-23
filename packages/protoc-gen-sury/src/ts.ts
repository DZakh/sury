import { type Element, type Enum, type Field, type File, type Message, type Oneof, nestedTypes, type Scalar } from "./model";
import { namesOf } from "./names";
import { type Options, componentsOf, importPath, isStruct, jsdoc, outputPath, wrapperScalar, wktImport } from "./shared";

const is64 = (scalar: Scalar): boolean => scalar.includes("64");

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

const packable = (element: Element): boolean =>
  element.kind === "enum" || (element.kind === "scalar" && element.scalar !== "string" && element.scalar !== "bytes");

const property = (name: string): string => (/^[A-Za-z_$][\w$]*$/.test(name) && name !== "__proto__" ? name : JSON.stringify(name));

// A symbol this file refers to: declared here, or imported from the file that
// declares it, under an alias when its name is taken.
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
    return this.use("sury/wkt", "JsonObject", true);
  }

  header(): string {
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

type Scope = { bindings: Map<Message, string>; component?: Set<Message>; depth: number };

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
    return sym.ref(element.message, "shape");
  };

  const fieldType = (field: Field): string => {
    const wrapped = wrapperScalar(field);
    if (wrapped !== undefined) return scalarType(wrapped, false);
    const element = elementType(field, field.element);
    if (field.mapKey !== undefined) return `{ [key: string]: ${element} }`;
    return field.list ? `${element}[]` : element;
  };

  const messageRef = (message: Message, scope: Scope): string => {
    const bound = scope.bindings.get(message);
    if (bound !== undefined) return bound;
    if (scope.component?.has(message)) return recursiveExpr(message, scope);
    return sym.ref(message, "schema");
  };

  const elementSchema = (field: Field, element: Element, scope: Scope): string => {
    if (element.kind === "scalar") {
      return scalarSchema(field.longAsString && field.mapKey === undefined && is64(element.scalar) ? "string" : scalarSchemas[element.scalar]);
    }
    if (element.kind === "enum") return sym.ref(element.enum, "schema");
    if (isStruct(field, element)) return `${S}.record(${S}.json)`;
    return messageRef(element.message, scope);
  };

  const wireType = (field: Field, element: Element): string =>
    element.kind === "scalar" ? element.scalar
    : element.kind === "enum" ? "enum"
    : isStruct(field, element) ? "google.protobuf.Struct"
    : "message";

  const numbered = (field: Field, schema: string, type: string): string => {
    let opts = `number: ${field.number}, type: ${JSON.stringify(type)}`;
    if (field.mapKey !== undefined) opts += `, key: ${JSON.stringify(field.mapKey)}`;
    if (field.list && !field.packed && packable(field.element)) opts += ", packed: false";
    return `${S}.protobufField(${schema}, { ${opts} })`;
  };

  const fieldSchema = (field: Field, scope: Scope): string => {
    const wrapped = wrapperScalar(field);
    if (wrapped !== undefined) {
      return numbered(field, `${S}.optional(${scalarSchema(scalarSchemas[wrapped])})`, (field.element as { message: Message }).message.typeName);
    }
    const element = elementSchema(field, field.element, scope);
    const type = wireType(field, field.element);
    if (field.mapKey !== undefined) return numbered(field, `${S}.record(${element})`, type);
    if (field.list) return numbered(field, `${S}.array(${element})`, type);
    const present = field.optional || field.element.kind === "message";
    return numbered(field, present ? `${S}.optional(${element})` : element, type);
  };

  const oneofType = (oneof: Oneof, indent: string): string =>
    [
      ...oneof.fields.map((field) => `${indent}  | { case: ${JSON.stringify(field.localName)}; value: ${fieldType({ ...field, list: false })} }`),
      `${indent}  | { case?: undefined; value?: undefined }`,
    ].join("\n");

  const oneofSchema = (oneof: Oneof, scope: Scope, indent: string): string => {
    const arms = oneof.fields.map((field) => {
      const element = elementSchema(field, field.element, scope);
      return `${indent}  ${S}.schema({ case: ${JSON.stringify(field.localName)}, value: ${numbered(field, element, wireType(field, field.element))} }),`;
    });
    arms.push(`${indent}  ${S}.schema({ case: undefined, value: ${S}.optional(${S}.schema(undefined)) }),`);
    return `${S}.union([\n${arms.join("\n")}\n${indent}])`;
  };

  const objectBody = (message: Message, scope: Scope, indent: string): string => {
    const lines = message.members.map((member) =>
      member.kind === "oneof"
        ? `${indent}  ${property(member.localName)}: ${oneofSchema(member, scope, `${indent}  `)},`
        : `${indent}  ${property(member.localName)}: ${fieldSchema(member, scope)},`,
    );
    return lines.length ? `{\n${lines.join("\n")}\n${indent}}` : "{}";
  };

  function recursiveExpr(message: Message, scope: Scope): string {
    const self = scope.depth === 0 ? "self" : `self${scope.depth + 1}`;
    const inner: Scope = { bindings: new Map(scope.bindings).set(message, self), component: scope.component, depth: scope.depth + 1 };
    const indent = "  ".repeat(scope.depth + 1);
    const body = objectBody(message, inner, indent);
    // A member only its siblings refer back to leaves its own binding unread.
    const binder = new RegExp(`\\b${self}\\b`).test(body) ? self : `_${self}`;
    return `${S}.recursive<${names.shape.get(message)}>(${JSON.stringify(names.shape.get(message))}, (${binder}) =>\n${indent}${S}.schema(${body}),\n${"  ".repeat(scope.depth)})`;
  }

  const typeDecl = (message: Message): string => {
    const lines = message.members.map((member) => {
      if (member.kind === "oneof") {
        return `${comment(member.path, `@generated from oneof ${message.typeName}.${member.name}`, false, "  ")}  ${property(member.localName)}:\n${oneofType(member, "  ")};`;
      }
      const optional = wrapperScalar(member) !== undefined || (!member.list && member.mapKey === undefined && (member.optional || member.element.kind === "message"));
      return `${comment(member.path, `@generated from field: ${fieldDeclaration(member)}`, member.deprecated, "  ")}  ${property(member.localName)}${optional ? "?" : ""}: ${fieldType(member)};`;
    });
    return `${comment(message.path, `@generated from message ${message.typeName}`, message.deprecated)}export type ${names.shape.get(message)} = {${lines.length ? `\n${lines.join("\n\n")}\n` : ""}};\n`;
  };

  const out: string[] = [];
  const enums = [...nestedTypes(file)].filter((desc): desc is Enum => desc.kind === "enum");
  const messages = [...nestedTypes(file)].filter((desc): desc is Message => desc.kind === "message");

  for (const desc of enums) {
    const name = names.shape.get(desc)!;
    const members = desc.values.map((value) =>
      `${comment(value.path, `@generated from enum value: ${value.name} = ${value.number};`, value.deprecated, "  ")}  ${property(value.localName)}: ${value.number},`,
    );
    const numbers = [...new Set(desc.values.map((value) => value.number))];
    const firstNamed = new Map<number, string>();
    for (const value of desc.values) if (!firstNamed.has(value.number)) firstNamed.set(value.number, value.localName);
    const literals = numbers.map((number) => `${name}.${firstNamed.get(number)}`);
    const doc = comment(desc.path, `@generated from enum ${desc.typeName}`, desc.deprecated);
    out.push(
      `${doc}export const ${name} = {\n${members.join("\n\n")}\n} as const;\n\n` +
        `${doc}export type ${name} = (typeof ${name})[keyof typeof ${name}];\n\n` +
        `export const ${names.schema.get(desc)} = ${S}.union([${literals.join(", ")}]);\n`,
    );
  }

  for (const component of componentsOf(messages)) {
    const recursive = component.length > 1 || component[0]!.fields.some((field) => field.element.kind === "message" && field.element.message === component[0]);
    for (const message of component) out.push(typeDecl(message));
    const members = new Set(component);
    for (const message of component) {
      const schema = names.schema.get(message)!;
      const shape = names.shape.get(message)!;
      if (recursive) {
        const scope: Scope = { bindings: new Map(), component: members, depth: 0 };
        out.push(`export const ${schema} = ${recursiveExpr(message, scope)};\n`);
      } else {
        const body = objectBody(message, { bindings: new Map(), depth: 0 }, "");
        out.push(`export const ${schema} = ${S}.meta(${S}.schemaOf<${shape}>()(${body}), { name: ${JSON.stringify(shape)} });\n`);
      }
    }
  }

  const params = options.raw ? ` with parameter "${options.raw}"` : "";
  const header =
    `// @generated by protoc-gen-sury${version ? ` v${version}` : ""}${params}\n` +
    `// @generated from file ${file.proto.name} (${file.proto.package ? `package ${file.proto.package}, ` : ""}syntax proto3)\n` +
    `/* eslint-disable */\n${options.tsNocheck ? "// @ts-nocheck\n" : ""}\n`;
  const body = (usesNumber ? `const $number = ${S}.union([${S}.number, ${S}.schema(NaN)]);\n\n` : "") + out.join("\n");
  return { name: `${outputPath(file)}.ts`, content: `${header}${sym.header()}\n${body}` };
};

// The field as protobuf-es writes it after `@generated from field:`.
export const fieldDeclaration = (field: Field): string => {
  const typeName = (element: Element): string =>
    element.kind === "scalar" ? element.scalar : element.kind === "enum" ? element.enum.typeName : element.message.typeName;
  const opts: string[] = [];
  if (field.list && field.proto.options?.packed === false) opts.push("packed = false");
  if (field.longAsString) opts.push("jstype = JS_STRING");
  if (field.deprecated) opts.push("deprecated = true");
  const rule = field.mapKey !== undefined ? "" : field.list ? "repeated " : field.optional ? "optional " : "";
  const type = field.mapKey !== undefined ? `map<${field.mapKey}, ${typeName(field.element)}>` : typeName(field.element);
  return `${rule}${type} ${field.name} = ${field.number}${opts.length ? ` [${opts.join(", ")}]` : ""};`;
};
