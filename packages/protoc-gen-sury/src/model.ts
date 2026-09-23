// Descriptors resolved into the shapes the emitters read. Names follow
// protobuf-es (`@bufbuild/protobuf` registry and `@bufbuild/protoplugin`
// names), so a message, enum, field or oneof is called what protoc-gen-es
// calls it; `test/parity.test.ts` holds the generated names and types to it.
import type {
  DescriptorProto,
  EnumDescriptorProto,
  FieldDescriptorProto,
  FileDescriptorProto,
} from "./descriptor";

export type Scalar =
  | "double"
  | "float"
  | "int64"
  | "uint64"
  | "int32"
  | "fixed64"
  | "fixed32"
  | "bool"
  | "string"
  | "bytes"
  | "uint32"
  | "sfixed32"
  | "sfixed64"
  | "sint32"
  | "sint64";

// FieldDescriptorProto.Type, by number. 10 is a group, 11 a message, 14 an enum.
const scalars: Record<number, Scalar> = {
  1: "double",
  2: "float",
  3: "int64",
  4: "uint64",
  5: "int32",
  6: "fixed64",
  7: "fixed32",
  8: "bool",
  9: "string",
  12: "bytes",
  13: "uint32",
  15: "sfixed32",
  16: "sfixed64",
  17: "sint32",
  18: "sint64",
};

export type File = {
  kind: "file";
  proto: FileDescriptorProto;
  name: string;
  messages: Message[];
  enums: Enum[];
  comments: Map<string, string>;
};

export type Message = {
  kind: "message";
  proto: DescriptorProto;
  file: File;
  parent?: Message;
  name: string;
  typeName: string;
  fields: Field[];
  // Real oneofs only: a proto3 `optional` field's synthetic oneof is presence.
  oneofs: Oneof[];
  // Fields and oneofs in declaration order, a oneof where its first member sits.
  members: (Field | Oneof)[];
  nestedMessages: Message[];
  nestedEnums: Enum[];
  path: number[];
  deprecated: boolean;
};

export type EnumValue = { name: string; localName: string; number: number; path: number[]; deprecated: boolean };

export type Enum = {
  kind: "enum";
  proto: EnumDescriptorProto;
  file: File;
  parent?: Message;
  name: string;
  typeName: string;
  values: EnumValue[];
  path: number[];
  deprecated: boolean;
};

export type Oneof = { kind: "oneof"; name: string; localName: string; fields: Field[]; path: number[] };

// What a field holds, or what a list or map holds each of.
export type Element =
  | { kind: "scalar"; scalar: Scalar }
  | { kind: "enum"; enum: Enum }
  | { kind: "message"; message: Message }
  // Resolved in every file of a request, refused only in one being generated.
  | { kind: "unsupported"; reason: string };

export type Field = {
  kind: "field";
  proto: FieldDescriptorProto;
  parent: Message;
  name: string;
  number: number;
  localName: string;
  element: Element;
  list: boolean;
  // The key scalar of a map field.
  mapKey?: Scalar;
  oneof?: Oneof;
  // proto3 `optional`: a scalar or enum with explicit presence.
  optional: boolean;
  packed: boolean;
  longAsString: boolean;
  path: number[];
  deprecated: boolean;
};

// protoc's default JSON name, which is also what protobuf-es names a property.
export const protoCamelCase = (snakeCase: string): string => {
  let capNext = false;
  let out = "";
  for (const c of snakeCase) {
    if (c === "_") capNext = true;
    else if (c >= "0" && c <= "9") (out += c), (capNext = false);
    else (out += capNext ? c.toUpperCase() : c), (capNext = false);
  }
  return out;
};

const reservedObjectProperties = new Set(["constructor", "toString", "toJSON", "valueOf"]);

export const safeObjectProperty = (name: string): string =>
  reservedObjectProperties.has(name) ? `${name}$` : name;

const camelToSnakeCase = (camel: string): string =>
  (camel.substring(0, 1) + camel.substring(1).replace(/[A-Z]/g, (c) => `_${c}`)).toLowerCase();

// The prefix every value repeats from the enum's name, which protobuf-es drops:
// `PHONE_TYPE_MOBILE` of `PhoneType` is `MOBILE`.
const enumSharedPrefix = (enumName: string, values: { name: string }[]): string | undefined => {
  const prefix = `${camelToSnakeCase(enumName)}_`;
  for (const value of values) {
    if (!value.name.toLowerCase().startsWith(prefix)) return undefined;
    const shortName = value.name.substring(prefix.length);
    if (shortName.length === 0 || /^\d/.test(shortName)) return undefined;
  }
  return prefix;
};

const typeNameOf = (file: FileDescriptorProto, parent: Message | undefined, name: string): string =>
  parent ? `${parent.typeName}.${name}` : file.package ? `${file.package}.${name}` : name;

export type Registry = {
  files: Map<string, File>;
  types: Map<string, Message | Enum>;
};

// Resolves every file of a request: a field's type can live in any of them.
export const buildRegistry = (protos: FileDescriptorProto[]): Registry => {
  const registry: Registry = { files: new Map(), types: new Map() };
  const pending: [Message, DescriptorProto][] = [];
  const mapEntries = new Map<string, DescriptorProto>();

  const addEnum = (proto: EnumDescriptorProto, file: File, parent: Message | undefined, path: number[]): Enum => {
    const prefix = enumSharedPrefix(proto.name, proto.value);
    const desc: Enum = {
      kind: "enum",
      proto,
      file,
      parent,
      name: proto.name,
      typeName: typeNameOf(file.proto, parent, proto.name),
      values: proto.value.map((value, idx) => ({
        name: value.name,
        localName: safeObjectProperty(prefix === undefined ? value.name : value.name.substring(prefix.length)),
        number: value.number,
        path: [...path, 2, idx],
        deprecated: value.options?.deprecated ?? false,
      })),
      path,
      deprecated: proto.options?.deprecated ?? false,
    };
    registry.types.set(desc.typeName, desc);
    return desc;
  };

  const addMessage = (proto: DescriptorProto, file: File, parent: Message | undefined, path: number[]): Message | undefined => {
    const typeName = typeNameOf(file.proto, parent, proto.name);
    if (proto.options?.mapEntry) {
      mapEntries.set(typeName, proto);
      return undefined;
    }
    const desc: Message = {
      kind: "message",
      proto,
      file,
      parent,
      name: proto.name,
      typeName,
      fields: [],
      oneofs: [],
      members: [],
      nestedMessages: [],
      nestedEnums: [],
      path,
      deprecated: proto.options?.deprecated ?? false,
    };
    registry.types.set(typeName, desc);
    proto.enumType.forEach((e, idx) => desc.nestedEnums.push(addEnum(e, file, desc, [...path, 4, idx])));
    proto.nestedType.forEach((m, idx) => {
      const nested = addMessage(m, file, desc, [...path, 3, idx]);
      if (nested) desc.nestedMessages.push(nested);
    });
    pending.push([desc, proto]);
    return desc;
  };

  for (const proto of protos) {
    const comments = new Map<string, string>();
    for (const location of proto.sourceCodeInfo?.location ?? []) {
      if (location.leadingComments !== undefined) comments.set(location.path.join("."), location.leadingComments);
    }
    const file: File = { kind: "file", proto, name: proto.name.replace(/\.proto$/, ""), messages: [], enums: [], comments };
    registry.files.set(proto.name, file);
    proto.enumType.forEach((e, idx) => file.enums.push(addEnum(e, file, undefined, [5, idx])));
    proto.messageType.forEach((m, idx) => {
      const message = addMessage(m, file, undefined, [4, idx]);
      if (message) file.messages.push(message);
    });
  }

  const elementOf = (proto: FieldDescriptorProto, where: string): Element => {
    const scalar = scalars[proto.type];
    if (scalar !== undefined) return { kind: "scalar", scalar };
    if (proto.type === 10) return { kind: "unsupported", reason: `${where} is a group, which protoc-gen-sury does not generate` };
    const type = registry.types.get(proto.typeName.replace(/^\./, ""));
    if (type === undefined) throw new Error(`${where} refers to ${proto.typeName}, which no file in the request declares`);
    return type.kind === "enum" ? { kind: "enum", enum: type } : { kind: "message", message: type };
  };

  for (const [desc, proto] of pending) {
    const oneofs = proto.oneofDecl.map((decl, idx): Oneof => ({
      kind: "oneof",
      name: decl.name,
      localName: safeObjectProperty(protoCamelCase(decl.name)),
      fields: [],
      path: [...desc.path, 8, idx],
    }));
    proto.field.forEach((fieldProto, idx) => {
      const where = `field ${desc.typeName}.${fieldProto.name}`;

      const oneof =
        fieldProto.oneofIndex !== undefined && !fieldProto.proto3Optional ? oneofs[fieldProto.oneofIndex] : undefined;
      let mapKey: Scalar | undefined;
      const list = fieldProto.label === 3;
      const entry = list && fieldProto.type === 11 ? mapEntries.get(fieldProto.typeName.replace(/^\./, "")) : undefined;
      let element: Element;
      if (entry !== undefined) {
        mapKey = scalars[entry.field.find((f) => f.number === 1)!.type];
        element = elementOf(entry.field.find((f) => f.number === 2)!, where);
      } else element = elementOf(fieldProto, where);
      const field: Field = {
        kind: "field",
        proto: fieldProto,
        parent: desc,
        name: fieldProto.name,
        number: fieldProto.number,
        localName: oneof ? protoCamelCase(fieldProto.name) : safeObjectProperty(protoCamelCase(fieldProto.name)),
        element,
        list: list && mapKey === undefined,
        mapKey,
        oneof,
        optional: fieldProto.proto3Optional,
        packed: fieldProto.options?.packed !== false,
        longAsString: fieldProto.options?.jstype === 1,
        path: [...desc.path, 2, idx],
        deprecated: fieldProto.options?.deprecated ?? false,
      };
      desc.fields.push(field);
      if (oneof) {
        if (oneof.fields.length === 0) desc.members.push(oneof);
        oneof.fields.push(field);
      } else desc.members.push(field);
    });
    desc.oneofs = oneofs.filter((oneof) => oneof.fields.length > 0);
  }
  return registry;
};

// Every message and enum of a file, in the order protobuf-es hands out names:
// each message followed by its nested types, messages before enums.
export function* nestedTypes(owner: File | Message): Generator<Message | Enum> {
  const messages = owner.kind === "message" ? owner.nestedMessages : owner.messages;
  const enums = owner.kind === "message" ? owner.nestedEnums : owner.enums;
  for (const message of messages) {
    yield message;
    yield* nestedTypes(message);
  }
  yield* enums;
}
