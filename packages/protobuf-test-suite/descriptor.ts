// A `.proto` file turned into a FileDescriptorProto, which is the only thing
// protobuf-es reads: it has no runtime `.proto` parser, and the parse plus
// descriptor export protobufjs offers is not usable here - it throws on every
// map field and writes `packed=false` onto proto3 repeated scalars that are
// packed by default. Building the descriptor from `protocol-buffers-schema`'s
// AST instead keeps protobuf-es off protobufjs's code entirely, which is the
// point of running the suite against it at all.
import { create } from "@bufbuild/protobuf";
import {
  type DescriptorProto,
  DescriptorProtoSchema,
  type EnumDescriptorProto,
  EnumDescriptorProtoSchema,
  type FieldDescriptorProto,
  FieldDescriptorProtoSchema,
  FieldDescriptorProto_Label,
  FieldDescriptorProto_Type,
  type FileDescriptorProto,
  FileDescriptorProtoSchema,
} from "@bufbuild/protobuf/wkt";
import schemaParse from "protocol-buffers-schema";

type AstField = {
  name: string;
  type: string;
  tag: number;
  map: { from: string; to: string } | null;
  oneof: string | null;
  repeated: boolean;
  options: Record<string, string>;
};

type AstEnum = { name: string; values: Record<string, { value: number }> };

type AstMessage = {
  name: string;
  fields: AstField[];
  messages: AstMessage[];
  enums: AstEnum[];
};

type Ast = { syntax: number; package: string | null; messages: AstMessage[]; enums: AstEnum[] };

// `protocol-buffers-schema` drops the proto3 `optional` label, and that label
// is the whole of explicit presence: without it a field set to its zero value
// is elided on re-encode and the round trip loses it. Recovered from the
// source with a scan that tracks the message a line is inside, so a nested
// message's field is not confused with its parent's.
const optionalFields = (source: string): Set<string> => {
  const found = new Set<string>();
  const stack: string[] = [];
  for (const line of source.split("\n")) {
    const opens = /^\s*message\s+([A-Za-z_]\w*)/.exec(line);
    if (opens) {
      stack.push(opens[1]!);
      continue;
    }
    if (/^\s*}/.test(line)) {
      stack.pop();
      continue;
    }
    const field = /^\s*optional\s+[\w.]+\s+([A-Za-z_]\w*)\s*=/.exec(line);
    if (field) found.add(`${stack.join(".")}.${field[1]!}`);
  }
  return found;
};

const SCALARS: Record<string, FieldDescriptorProto_Type> = {
  double: FieldDescriptorProto_Type.DOUBLE,
  float: FieldDescriptorProto_Type.FLOAT,
  int64: FieldDescriptorProto_Type.INT64,
  uint64: FieldDescriptorProto_Type.UINT64,
  int32: FieldDescriptorProto_Type.INT32,
  fixed64: FieldDescriptorProto_Type.FIXED64,
  fixed32: FieldDescriptorProto_Type.FIXED32,
  bool: FieldDescriptorProto_Type.BOOL,
  string: FieldDescriptorProto_Type.STRING,
  bytes: FieldDescriptorProto_Type.BYTES,
  uint32: FieldDescriptorProto_Type.UINT32,
  sfixed32: FieldDescriptorProto_Type.SFIXED32,
  sfixed64: FieldDescriptorProto_Type.SFIXED64,
  sint32: FieldDescriptorProto_Type.SINT32,
  sint64: FieldDescriptorProto_Type.SINT64,
};

// `mapEntry` is the message a map field is repeated over. protoc derives the
// name from the field's, and the name has to match or a decoder looking the
// type up by reference finds nothing.
const entryName = (field: string): string =>
  `${field.replace(/(^|_)([a-z])/g, (_, __, c: string) => c.toUpperCase())}Entry`;

// Every declared type, fully qualified, and which of them are enums - a
// reference names one or the other and the descriptor has to say which.
type Declared = { all: Set<string>; enums: Set<string> };

const collect = (messages: AstMessage[], enums: AstEnum[], scope: string, into: Declared): void => {
  for (const e of enums) {
    into.all.add(`${scope}.${e.name}`);
    into.enums.add(`${scope}.${e.name}`);
  }
  for (const m of messages) {
    const qualified = `${scope}.${m.name}`;
    into.all.add(qualified);
    collect(m.messages ?? [], m.enums ?? [], qualified, into);
  }
};

// protoc resolves a reference innermost scope outwards, so a nested name may
// shadow a top-level one and `toProtoOrThrow` is free to nest.
const resolve = (declared: Declared, scope: string, reference: string): string => {
  if (reference.startsWith(".")) return reference;
  let at = scope;
  for (;;) {
    const candidate = `${at}.${reference}`;
    if (declared.all.has(candidate)) return candidate;
    if (at === "") throw new Error(`unresolved type ${reference} in ${scope || "<root>"}`);
    at = at.slice(0, at.lastIndexOf("."));
  }
};

const fieldOf = (
  field: AstField,
  declared: Declared,
  scope: string,
  label: FieldDescriptorProto_Label,
): FieldDescriptorProto => {
  const scalar = SCALARS[field.type];
  const typeName = scalar === undefined ? resolve(declared, scope, field.type) : undefined;
  return create(FieldDescriptorProtoSchema, {
    name: field.name,
    number: field.tag,
    label,
    type: scalar ??
      (declared.enums.has(typeName!) ? FieldDescriptorProto_Type.ENUM : FieldDescriptorProto_Type.MESSAGE),
    typeName,
    // Left unset unless the file said so: a proto3 repeated scalar is packed
    // by default, and writing the default out is what made protobufjs's own
    // descriptor export unusable here.
    options: "packed" in field.options ? { packed: field.options["packed"] === "true" } : undefined,
  });
};

const messageOf = (
  message: AstMessage,
  declared: Declared,
  scope: string,
  optional: Set<string>,
  path: string,
): DescriptorProto => {
  const qualified = `${scope}.${message.name}`;
  const here = path === "" ? message.name : `${path}.${message.name}`;
  const out = create(DescriptorProtoSchema, { name: message.name });
  out.nestedType = (message.messages ?? []).map((m) => messageOf(m, declared, qualified, optional, here));
  out.enumType = (message.enums ?? []).map(enumOf);
  const oneofIndex = new Map<string, number>();
  for (const field of message.fields) {
    if (field.map !== null) {
      const entry = entryName(field.name);
      const entryType = create(DescriptorProtoSchema, {
        name: entry,
        options: { mapEntry: true },
        field: [
          fieldOf({ ...field, name: "key", tag: 1, type: field.map.from, map: null, options: {} }, declared, qualified, FieldDescriptorProto_Label.OPTIONAL),
          fieldOf({ ...field, name: "value", tag: 2, type: field.map.to, map: null, options: {} }, declared, qualified, FieldDescriptorProto_Label.OPTIONAL),
        ],
      });
      out.nestedType.push(entryType);
      out.field.push(create(FieldDescriptorProtoSchema, {
        name: field.name,
        number: field.tag,
        label: FieldDescriptorProto_Label.REPEATED,
        type: FieldDescriptorProto_Type.MESSAGE,
        typeName: `${qualified}.${entry}`,
      }));
      continue;
    }
    const descriptor = fieldOf(
      field,
      declared,
      qualified,
      field.repeated ? FieldDescriptorProto_Label.REPEATED : FieldDescriptorProto_Label.OPTIONAL,
    );
    if (field.oneof !== null) {
      let index = oneofIndex.get(field.oneof);
      if (index === undefined) {
        index = out.oneofDecl.length;
        oneofIndex.set(field.oneof, index);
        out.oneofDecl.push({ $typeName: "google.protobuf.OneofDecl", name: field.oneof } as never);
      }
      descriptor.oneofIndex = index;
    } else if (!field.repeated && optional.has(`${here}.${field.name}`)) {
      descriptor.proto3Optional = true;
      descriptor.oneofIndex = out.oneofDecl.length;
      out.oneofDecl.push({ $typeName: "google.protobuf.OneofDecl", name: `_${field.name}` } as never);
    }
    out.field.push(descriptor);
  }
  return out;
};

const enumOf = (e: AstEnum): EnumDescriptorProto =>
  create(EnumDescriptorProtoSchema, {
    name: e.name,
    value: Object.keys(e.values).map((name) => ({
      $typeName: "google.protobuf.EnumValueDescriptorProto" as const,
      name,
      number: e.values[name]!.value,
    })) as never,
  });

export const fileDescriptorOf = (source: string, name = "suite.proto"): FileDescriptorProto => {
  const ast = schemaParse(source) as unknown as Ast;
  const pkg = ast.package ?? "";
  const scope = pkg === "" ? "" : `.${pkg}`;
  const declared: Declared = { all: new Set(), enums: new Set() };
  collect(ast.messages ?? [], ast.enums ?? [], scope, declared);
  return create(FileDescriptorProtoSchema, {
    name,
    package: pkg === "" ? undefined : pkg,
    syntax: "proto3",
    messageType: (ast.messages ?? []).map((m) => messageOf(m, declared, scope, optionalFields(source), "")),
    enumType: (ast.enums ?? []).map(enumOf),
  });
};
