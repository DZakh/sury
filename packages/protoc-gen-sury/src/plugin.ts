import type { CodeGeneratorRequest, CodeGeneratorResponse } from "./descriptor";
import { buildRegistry, nestedTypes } from "./model";
import { emitRes } from "./res";
import { parseOptions } from "./shared";
import { emitTs } from "./ts";

// CodeGeneratorResponse.Feature.FEATURE_PROTO3_OPTIONAL: proto3 `optional` fields
// are understood, so protoc hands them over rather than refusing the file.
const PROTO3_OPTIONAL = 1n;

export const generate = (request: CodeGeneratorRequest, version: string): CodeGeneratorResponse => {
  try {
    const options = parseOptions(request.parameter);
    const registry = buildRegistry(request.protoFile);
    const generating = new Set(request.fileToGenerate);
    const files: CodeGeneratorResponse["file"] = [];
    for (const name of request.fileToGenerate) {
      const file = registry.files.get(name);
      if (file === undefined) throw new Error(`${name} is not among the files in the request`);
      if (file.proto.syntax !== "proto3") {
        throw new Error(`${name} is ${file.proto.syntax || "proto2"}; protoc-gen-sury generates proto3 files`);
      }
      if (file.proto.extension.length > 0 || file.messages.some(function hasExtension(m): boolean {
        return m.proto.extension.length > 0 || m.nestedMessages.some(hasExtension);
      })) {
        throw new Error(`${name} declares an extension, which protoc-gen-sury does not generate`);
      }
      for (const desc of nestedTypes(file)) {
        if (desc.kind !== "message") continue;
        for (const field of desc.fields) {
          if (field.element.kind === "unsupported") throw new Error(field.element.reason);
          const type = field.element.kind === "message" ? field.element.message : field.element.kind === "enum" ? field.element.enum : undefined;
          if (type !== undefined && type.file.proto.syntax !== "proto3") {
            throw new Error(`field ${desc.typeName}.${field.name} refers to ${type.typeName}, from ${type.file.proto.name}, which is ${type.file.proto.syntax || "proto2"}`);
          }
        }
      }
      if (!options.keepEmptyFiles && [...nestedTypes(file)].length === 0) continue;
      if (options.targets.has("ts")) files.push(emitTs(file, options, generating, version));
      if (options.targets.has("res")) files.push(emitRes(file, options, generating, version));
    }
    return { supportedFeatures: PROTO3_OPTIONAL, file: files };
  } catch (error) {
    return { supportedFeatures: PROTO3_OPTIONAL, error: error instanceof Error ? error.message : String(error), file: [] };
  }
};
