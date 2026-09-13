// Checks testMessages.ts against the `.proto` it claims to be written from.
//
// Without this the pinned commit is decoration: the harness would fetch the
// corpus, read nothing out of it, and a bump that added a field or changed a
// wire type would show up only as a conformance case failing for reasons no
// one could see. Here it says which field, and how.
//
// Comparison is field number -> wire type and label, which is all the binary
// format has. The JS type a field decodes to is testMessages.ts's own
// business, and the runner already checks it by comparing bytes.
import { readFileSync } from "node:fs";
import { join } from "node:path";
import schemaParse from "protocol-buffers-schema";
import { testAllTypesProto3 } from "./testMessages";

const MESSAGE = "TestAllTypesProto3";
const PROTO = join("proto", "google", "protobuf", "test_messages_proto3.proto");

type AstField = {
  name: string;
  type: string;
  tag: number;
  map: { from: string; to: string } | null;
  oneof: string | null;
  repeated: boolean;
  options: Record<string, string>;
};

export type Divergence = { number: number; name: string; detail: string };

// `google.protobuf.NullValue` is the one well-known type that is an enum
// rather than a message, and nothing in the file it is referenced from says
// so. Resolving the import would be a whole `.proto` resolver for one name.
const WKT_ENUMS = new Set(["google.protobuf.NullValue"]);

// What the wire calls a field, from the `.proto` side: the scalar name, or the
// shape a message/enum/map takes.
const upstreamShape = (field: AstField, enums: Set<string>): string => {
  if (field.map !== null) return `map<${field.map.from}, ?>`;
  const local = field.type.replace(/^.*\./, "");
  const kind = enums.has(field.type) || enums.has(local) || WKT_ENUMS.has(field.type)
    ? "enum"
    : /^[A-Z]/.test(local)
      ? "message"
      : field.type;
  const packed = field.repeated && field.options["packed"] === "false" ? " [packed=false]" : "";
  return `${field.repeated ? "repeated " : ""}${kind}${packed}`;
};

const suryShape = (stored: {
  type: string;
  packed: boolean;
  key: string;
  repeated: boolean;
  map: boolean;
}): string => {
  if (stored.map) return `map<${stored.key}, ?>`;
  const packed = stored.repeated && !stored.packed ? " [packed=false]" : "";
  return `${stored.repeated ? "repeated " : ""}${stored.type}${packed}`;
};

// The field metadata `S.protobufField` stored, keyed by number. Read off the
// schema rather than re-derived, so this compares what the codec will actually
// do.
const declared = (): Map<number, { name: string; shape: string }> => {
  const out = new Map<number, { name: string; shape: string }>();
  const properties = (testAllTypesProto3 as unknown as { properties: Record<string, unknown> })
    .properties;
  for (const key of Object.keys(properties)) {
    let schema = properties[key] as
      | { protobufField?: unknown; to?: unknown; anyOf?: unknown[] }
      | undefined;
    // `S.optional` wraps, and `.with` chains, so walk to the one that carries
    // the field metadata.
    const seen: unknown[] = [];
    while (schema !== undefined && schema.protobufField === undefined && seen.length < 20) {
      seen.push(schema);
      const next = (schema.anyOf as { protobufField?: unknown }[] | undefined)?.find(
        (m) => m.protobufField !== undefined,
      );
      schema = (next ?? schema.to) as typeof schema;
    }
    const stored = schema?.protobufField as
      | { number: number; type: string; packed: boolean; key: string }
      | undefined;
    if (stored === undefined) continue;
    const property = properties[key] as { type?: string; additionalItems?: unknown };
    const repeated = property.type === "array";
    const map = property.type === "object" && typeof property.additionalItems === "object";
    out.set(stored.number, { name: key, shape: suryShape({ ...stored, repeated, map }) });
  }
  return out;
};

export const divergences = (upstreamDir: string): Divergence[] => {
  const ast = schemaParse(readFileSync(join(upstreamDir, PROTO), "utf8")) as unknown as {
    messages: { name: string; fields: AstField[]; enums: { name: string }[] }[];
    enums: { name: string }[];
  };
  const message = ast.messages.find((m) => m.name === MESSAGE);
  if (message === undefined) throw new Error(`${PROTO} has no ${MESSAGE}`);
  const enums = new Set([
    ...ast.enums.map((e) => e.name),
    ...(message.enums ?? []).map((e) => e.name),
  ]);

  const ours = declared();
  const found: Divergence[] = [];
  for (const field of message.fields) {
    const mine = ours.get(field.tag);
    if (mine === undefined) {
      found.push({ number: field.tag, name: field.name, detail: "not declared" });
      continue;
    }
    const want = upstreamShape(field, enums);
    // A map's value type and a message's identity are not comparable here;
    // the shape captures the label, the key type and the wire kind.
    if (want.replace(/, \?>/, ", ?>") !== mine.shape.replace(/, \?>/, ", ?>")) {
      found.push({
        number: field.tag,
        name: field.name,
        detail: `.proto says ${want}, testMessages.ts says ${mine.shape}`,
      });
    }
    ours.delete(field.tag);
  }
  for (const [number, mine] of ours) {
    found.push({ number, name: mine.name, detail: "declared, but not in the .proto" });
  }
  return found.sort((a, b) => a.number - b.number);
};
