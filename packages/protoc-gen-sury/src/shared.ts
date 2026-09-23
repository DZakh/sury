import { type Element, type Field, type File, type Message, protoCamelCase, type Scalar } from "./model";

export type Options = {
  targets: Set<"ts" | "res">;
  importExtension: "" | ".js" | ".ts";
  keepEmptyFiles: boolean;
  tsNocheck: boolean;
  // The parameter as given, for the generated header.
  raw: string;
};

export const parseOptions = (parameter: string | undefined): Options => {
  const options: Options = { targets: new Set(["ts"]), importExtension: "", keepEmptyFiles: false, tsNocheck: false, raw: parameter ?? "" };
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

// The well-known type files `sury/wkt` ships, generated from the same protos,
// which a generated file imports rather than expecting beside it.
export const wktFiles = new Set([
  "google/protobuf/any.proto",
  "google/protobuf/api.proto",
  "google/protobuf/duration.proto",
  "google/protobuf/empty.proto",
  "google/protobuf/field_mask.proto",
  "google/protobuf/source_context.proto",
  "google/protobuf/struct.proto",
  "google/protobuf/timestamp.proto",
  "google/protobuf/type.proto",
  "google/protobuf/wrappers.proto",
]);

export const wktImport = (file: File, generating: Set<string>): string | undefined =>
  wktFiles.has(file.proto.name) && !generating.has(file.proto.name) ? "sury/wkt" : undefined;

export const outputPath = (file: File): string => `${file.name}_pb`;

export const importPath = (from: File, to: File, options: Options): string => {
  const fromParts = from.name.split("/").slice(0, -1);
  const toParts = outputPath(to).split("/");
  let common = 0;
  while (common < fromParts.length && common < toParts.length - 1 && fromParts[common] === toParts[common]) common++;
  const up = fromParts.length - common;
  const rest = toParts.slice(common).join("/");
  return `${up === 0 ? "./" : "../".repeat(up)}${rest}${options.importExtension}`;
};

const wrappers: Record<string, Scalar> = {
  "google.protobuf.DoubleValue": "double",
  "google.protobuf.FloatValue": "float",
  "google.protobuf.Int64Value": "int64",
  "google.protobuf.UInt64Value": "uint64",
  "google.protobuf.Int32Value": "int32",
  "google.protobuf.UInt32Value": "uint32",
  "google.protobuf.BoolValue": "bool",
  "google.protobuf.StringValue": "string",
  "google.protobuf.BytesValue": "bytes",
};

// protobuf-es unboxes a wrapper only on a singular field outside a oneof.
export const wrapperScalar = (field: Field): Scalar | undefined =>
  field.element.kind === "message" && !field.list && field.mapKey === undefined && field.oneof === undefined
    ? wrappers[field.element.message.typeName]
    : undefined;

// ...and holds a Struct as a JSON object anywhere but inside Value itself.
export const isStruct = (field: Field, element: Element): boolean =>
  element.kind === "message" &&
  element.message.typeName === "google.protobuf.Struct" &&
  field.parent.typeName !== "google.protobuf.Value";

// Messages grouped so that each group only refers to earlier ones or itself: a
// group of more than one, or of one that refers to itself, is recursive.
// Tarjan's algorithm emits each strongly connected component after every
// component it reaches.
export const componentsOf = (messages: Message[]): Message[][] => {
  const inFile = new Set(messages);
  const index = new Map<Message, number>();
  const low = new Map<Message, number>();
  const stack: Message[] = [];
  const onStack = new Set<Message>();
  const out: Message[][] = [];
  const visit = (message: Message): void => {
    index.set(message, index.size);
    low.set(message, index.get(message)!);
    stack.push(message);
    onStack.add(message);
    for (const field of message.fields) {
      if (field.element.kind !== "message") continue;
      const next = field.element.message;
      if (!inFile.has(next)) continue;
      if (!index.has(next)) {
        visit(next);
        low.set(message, Math.min(low.get(message)!, low.get(next)!));
      } else if (onStack.has(next)) low.set(message, Math.min(low.get(message)!, index.get(next)!));
    }
    if (low.get(message) === index.get(message)) {
      const component: Message[] = [];
      let member: Message;
      do {
        member = stack.pop()!;
        onStack.delete(member);
        component.unshift(member);
      } while (member !== message);
      out.push(component.sort((a, b) => messages.indexOf(a) - messages.indexOf(b)));
    }
  };
  for (const message of messages) if (!index.has(message)) visit(message);
  return out;
};

export const jsdoc = (text: string | undefined, tag: string, deprecated: boolean, indent: string): string => {
  const lines: string[] = [];
  if (text !== undefined && text.trim() !== "") {
    for (const line of text.replace(/\n$/, "").split("\n")) lines.push((line.startsWith(" ") ? line.slice(1) : line).trimEnd());
    lines.push("");
  }
  lines.push(tag);
  if (deprecated) lines.push("@deprecated");
  return `${indent}/**\n${lines.map((line) => `${indent} *${line ? ` ${line.replace(/\*\//g, "*\\/")}` : ""}`).join("\n")}\n${indent} */\n`;
};

export { protoCamelCase };
