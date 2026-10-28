import type { CodeGeneratorRequest, CodeGeneratorResponse } from "./descriptor";
import { buildRegistry, nestedTypes } from "./model";
import { emitRes } from "./res";
import type { Options } from "./shared";
import { emitTs } from "./ts";

const parseOptions = (parameter: string | undefined): Options => {
  const options: Options = { targets: new Set(["ts"]), importExtension: "", keepEmptyFiles: false, tsNocheck: false, parameter: parameter ?? "" };
  for (const pair of (parameter ?? "").split(",")) {
    if (pair.trim() === "") continue;
    const [key, value = ""] = pair.split("=").map((part) => part.trim()) as [string, string?];
    if (key === "target") {
      const targets = value.split("+");
      if (targets.some((target) => target !== "ts" && target !== "res")) {
        throw new Error(`invalid option "target=${value}": the targets are ts, res and ts+res`);
      }
      options.targets = new Set(targets as ("ts" | "res")[]);
    } else if (key === "import_extension") {
      if (value !== "none" && value !== "js" && value !== "ts") {
        throw new Error(`invalid option "import_extension=${value}": use none, js or ts`);
      }
      options.importExtension = value === "none" ? "" : `.${value}`;
    } else if (key === "keep_empty_files" || key === "ts_nocheck") {
      if (value !== "" && value !== "true" && value !== "false") throw new Error(`invalid option "${pair}": use true or false`);
      options[key === "ts_nocheck" ? "tsNocheck" : "keepEmptyFiles"] = value !== "false";
    } else {
      throw new Error(`unknown option "${key}": protoc-gen-sury takes target, import_extension, keep_empty_files and ts_nocheck`);
    }
  }
  return options;
};

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
      const types = [...nestedTypes(file)];
      if (file.proto.extension.length > 0 || types.some((desc) => desc.kind === "message" && desc.proto.extension.length > 0)) {
        throw new Error(`${name} declares an extension, which protoc-gen-sury does not generate`);
      }
      for (const desc of types) {
        if (desc.kind !== "message") continue;
        for (const field of desc.fields) {
          if (field.element.kind === "unsupported") throw new Error(field.element.reason);
          const type = field.element.kind === "message" ? field.element.message : field.element.kind === "enum" ? field.element.enum : undefined;
          if (type !== undefined && type.file.proto.syntax !== "proto3") {
            throw new Error(`field ${desc.typeName}.${field.name} refers to ${type.typeName}, from ${type.file.proto.name}, which is ${type.file.proto.syntax || "proto2"}`);
          }
        }
      }
      if (!options.keepEmptyFiles && types.length === 0) continue;
      if (options.targets.has("ts")) files.push(emitTs(file, options, generating, version));
      if (options.targets.has("res")) files.push(emitRes(file, options, generating, version));
    }
    return { supportedFeatures: PROTO3_OPTIONAL, file: files };
  } catch (error) {
    return { supportedFeatures: PROTO3_OPTIONAL, error: error instanceof Error ? error.message : String(error), file: [] };
  }
};
