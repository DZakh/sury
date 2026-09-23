// The parts of google/protobuf/descriptor.proto and compiler/plugin.proto the
// generator reads, decoded by Sury itself. Both files are proto2, which the
// generator doesn't take, so these stay hand-written; `test/input.test.ts`
// holds them to protobuf-es's own decode of the same requests.
import * as S from "sury";

const field = <TInput, TOutput>(schema: S.Schema<TInput, TOutput>, number: number, type: S.ProtobufType) =>
  S.protobufField(schema, { number, type });

export type FieldOptions = { packed?: boolean; jstype: number; deprecated: boolean };

export type FieldDescriptorProto = {
  name: string;
  number: number;
  label: number;
  type: number;
  typeName: string;
  extendee: string;
  defaultValue?: string;
  options?: FieldOptions;
  oneofIndex?: number;
  jsonName?: string;
  proto3Optional: boolean;
};

export type EnumValueDescriptorProto = { name: string; number: number; options?: { deprecated: boolean } };

export type EnumDescriptorProto = { name: string; value: EnumValueDescriptorProto[]; options?: { deprecated: boolean } };

export type DescriptorProto = {
  name: string;
  field: FieldDescriptorProto[];
  nestedType: DescriptorProto[];
  enumType: EnumDescriptorProto[];
  extension: FieldDescriptorProto[];
  options?: { mapEntry: boolean; deprecated: boolean };
  oneofDecl: { name: string }[];
};

export type SourceCodeLocation = { path: number[]; leadingComments?: string; trailingComments?: string };

export type FileDescriptorProto = {
  name: string;
  package: string;
  dependency: string[];
  messageType: DescriptorProto[];
  enumType: EnumDescriptorProto[];
  service: { name: string }[];
  extension: FieldDescriptorProto[];
  sourceCodeInfo?: { location: SourceCodeLocation[] };
  syntax: string;
};

export type CodeGeneratorRequest = {
  fileToGenerate: string[];
  parameter?: string;
  protoFile: FileDescriptorProto[];
};

export type CodeGeneratorResponse = {
  error?: string;
  supportedFeatures: bigint;
  file: { name: string; content: string }[];
};

const deprecatedOptions = S.schema({ deprecated: field(S.boolean, 3, "bool") });

const FieldDescriptorProtoSchema = S.schema({
  name: field(S.string, 1, "string"),
  extendee: field(S.string, 2, "string"),
  number: field(S.int32, 3, "int32"),
  label: field(S.int32, 4, "enum"),
  type: field(S.int32, 5, "enum"),
  typeName: field(S.string, 6, "string"),
  defaultValue: field(S.optional(S.string), 7, "string"),
  options: field(
    S.optional(
      S.schema({
        packed: field(S.optional(S.boolean), 2, "bool"),
        deprecated: field(S.boolean, 3, "bool"),
        jstype: field(S.int32, 6, "enum"),
      }),
    ),
    8,
    "message",
  ),
  oneofIndex: field(S.optional(S.int32), 9, "int32"),
  jsonName: field(S.optional(S.string), 10, "string"),
  proto3Optional: field(S.boolean, 17, "bool"),
});

const EnumDescriptorProtoSchema = S.schema({
  name: field(S.string, 1, "string"),
  value: field(
    S.array(
      S.schema({
        name: field(S.string, 1, "string"),
        number: field(S.int32, 2, "int32"),
        options: field(S.optional(S.schema({ deprecated: field(S.boolean, 1, "bool") })), 3, "message"),
      }),
    ),
    2,
    "message",
  ),
  options: field(S.optional(deprecatedOptions), 3, "message"),
});

const DescriptorProtoSchema = S.recursive<DescriptorProto>("DescriptorProto", (self) =>
  S.schema({
    name: field(S.string, 1, "string"),
    field: field(S.array(FieldDescriptorProtoSchema), 2, "message"),
    nestedType: field(S.array(self), 3, "message"),
    enumType: field(S.array(EnumDescriptorProtoSchema), 4, "message"),
    extension: field(S.array(FieldDescriptorProtoSchema), 6, "message"),
    options: field(
      S.optional(S.schema({ deprecated: field(S.boolean, 3, "bool"), mapEntry: field(S.boolean, 7, "bool") })),
      7,
      "message",
    ),
    oneofDecl: field(S.array(S.schema({ name: field(S.string, 1, "string") })), 8, "message"),
  }),
);

const FileDescriptorProtoSchema = S.schema({
  name: field(S.string, 1, "string"),
  package: field(S.string, 2, "string"),
  dependency: field(S.array(S.string), 3, "string"),
  messageType: field(S.array(DescriptorProtoSchema), 4, "message"),
  enumType: field(S.array(EnumDescriptorProtoSchema), 5, "message"),
  service: field(S.array(S.schema({ name: field(S.string, 1, "string") })), 6, "message"),
  extension: field(S.array(FieldDescriptorProtoSchema), 7, "message"),
  sourceCodeInfo: field(
    S.optional(
      S.schema({
        location: field(
          S.array(
            S.schema({
              path: field(S.array(S.int32), 1, "int32"),
              leadingComments: field(S.optional(S.string), 3, "string"),
              trailingComments: field(S.optional(S.string), 4, "string"),
            }),
          ),
          1,
          "message",
        ),
      }),
    ),
    9,
    "message",
  ),
  syntax: field(S.string, 12, "string"),
});

const CodeGeneratorRequestSchema = S.schema({
  fileToGenerate: field(S.array(S.string), 1, "string"),
  parameter: field(S.optional(S.string), 2, "string"),
  protoFile: field(S.array(FileDescriptorProtoSchema), 15, "message"),
});

const CodeGeneratorResponseSchema = S.schema({
  error: field(S.optional(S.string), 1, "string"),
  supportedFeatures: field(S.bigint, 2, "uint64"),
  file: field(
    S.array(S.schema({ name: field(S.string, 1, "string"), content: field(S.string, 15, "string") })),
    15,
    "message",
  ),
});

export const decodeRequest = S.decodeOrThrow(S.protobuf, CodeGeneratorRequestSchema) as (
  bytes: Uint8Array,
) => CodeGeneratorRequest;

export const decodeFileDescriptorSet = S.decodeOrThrow(
  S.protobuf,
  S.schema({ file: field(S.array(FileDescriptorProtoSchema), 1, "message") }),
) as (bytes: Uint8Array) => { file: FileDescriptorProto[] };

export const encodeResponse = S.decodeOrThrow(CodeGeneratorResponseSchema, S.protobuf) as (
  response: CodeGeneratorResponse,
) => Uint8Array;
