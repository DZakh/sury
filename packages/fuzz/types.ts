export const CASE_VERSION = 1;

export type PrimitiveName =
  | "string"
  | "any"
  | "boolean"
  | "int32"
  | "number"
  | "nan"
  | "bigint"
  | "symbol"
  | "void"
  | "never"
  | "unknown"
  | "json"
  | "jsonString"
  | "jsonStringWithSpace"
  | "uint8Array"
  | "isoDateTime"
  | "port"
  | "email"
  | "uuid"
  | "cuid"
  | "url"
  | "date";

export type NumberValue =
  | { kind: "finite"; value: number }
  | { kind: "nan" }
  | { kind: "infinity"; negative: boolean }
  | { kind: "negative-zero" };

export type ValueAst =
  | { kind: "string"; value: string }
  | { kind: "number"; value: NumberValue }
  | { kind: "bigint"; value: string }
  | { kind: "boolean"; value: boolean }
  | { kind: "null" }
  | { kind: "undefined" }
  | { kind: "array"; items: ValueAst[] }
  | { kind: "object"; entries: [string, ValueAst][] };

export type CodecFunctionName =
  | "identity"
  | "to-string"
  | "to-number"
  | "to-boolean"
  | "length"
  | "first"
  | "wrap-array"
  | "constant-string"
  | "constant-number"
  | "constant-boolean"
  | "constant-null"
  | "constant-undefined"
  | "empty-array"
  | "empty-object";

export type ToCodec =
  | { kind: "builtin" }
  | { kind: "custom-decoder"; decoder: CodecFunctionName }
  | {
      kind: "custom-bidirectional";
      decoder: CodecFunctionName;
      encoder: CodecFunctionName;
    };

export type SchemaAst =
  | { kind: "primitive"; name: PrimitiveName }
  | { kind: "literal"; value: ValueAst }
  | { kind: "enum"; values: ValueAst[] }
  | { kind: "instance" }
  | { kind: "array"; item: SchemaAst }
  | { kind: "tuple"; items: SchemaAst[] }
  | { kind: "record"; value: SchemaAst }
  | {
      kind: "object";
      factory: "schema" | "object";
      fields: [string, SchemaAst][];
    }
  | { kind: "union"; members: SchemaAst[] }
  | { kind: "optional" | "nullable" | "nullish"; inner: SchemaAst }
  | {
      kind: "refine";
      refinement:
        | "always"
        | "never"
        | "non-empty"
        | "non-negative"
        | "min-length"
        | "max-length"
        | "gte"
        | "lte"
        | "gt"
        | "lt"
        | "length"
        | "empty"
        | "nonEmpty"
        | "pattern";
      argument?: number;
      inner: SchemaAst;
    }
  | {
      kind: "modifier";
      modifier: "strict" | "strip" | "deepStrict" | "deepStrip" | "noValidation" | "meta";
      inner: SchemaAst;
    }
  | {
      kind: "unary";
      api:
        | "compactColumns"
        | "list"
        | "trim"
        | "brand"
        | "shape"
        | "asyncDecoderAssert"
        | "reverse";
      inner: SchemaAst;
      codec?: CodecFunctionName;
    }
  | { kind: "merge"; left: SchemaAst; right: SchemaAst }
  | { kind: "to"; source: SchemaAst; target: SchemaAst; codec: ToCodec }
  | { kind: "recursive"; name: string; leaf: SchemaAst };

export type OperationKind =
  | "parser"
  | "decoder"
  | "encoder"
  | "asyncParser"
  | "asyncDecoder"
  | "asyncEncoder";

export type CompilerCase = {
  version: typeof CASE_VERSION;
  id: string;
  operation: OperationKind;
  schemas: SchemaAst[];
  runWitness: boolean;
};

export type FailurePhase = "schema" | "compile" | "source" | "runtime" | "timeout";

export type Failure = {
  phase: FailurePhase;
  name: string;
  message: string;
  signature: string;
};

export type CaseResult =
  | { status: "compiled"; cacheHit: boolean; witness: "passed" | "sury-error" | "skipped" }
  | { status: "expected-error"; name: string; message: string }
  | { status: "bug"; failure: Failure };

export type FailureArtifact = {
  artifactVersion: 1;
  createdAt: string;
  seed?: number;
  original: CompilerCase;
  minimized: CompilerCase;
  failure: Failure;
};
