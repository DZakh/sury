// conformance.proto, as Sury schemas. The harness speaks the runner's own
// protocol with the codec under test, which is the cheapest way to keep the
// two honest: a `oneof`, a `bytes` field and an open enum all have to work
// before a single conformance case is read.
import * as S from "sury";

// Mirrors conformance.WireFormat. Open, as proto3 enums are: a format this
// build does not know decodes as its number and is answered with `skipped`.
export const PROTOBUF = 1;
export const JSON_FORMAT = 2;
export const TEXT_FORMAT = 4;

export const conformanceRequest = S.schema({
  protobufPayload: S.optional(S.uint8Array).with(S.protobufField, { number: 1, oneof: "payload" }),
  jsonPayload: S.optional(S.string).with(S.protobufField, { number: 2, oneof: "payload" }),
  jspbPayload: S.optional(S.string).with(S.protobufField, { number: 7, oneof: "payload" }),
  textPayload: S.optional(S.string).with(S.protobufField, { number: 8, oneof: "payload" }),
  requestedOutputFormat: S.int32.with(S.protobufField, { number: 3, type: "enum" }),
  messageType: S.string.with(S.protobufField, 4),
  testCategory: S.int32.with(S.protobufField, { number: 5, type: "enum" }),
  printUnknownFields: S.boolean.with(S.protobufField, 9),
});

export const conformanceResponse = S.schema({
  parseError: S.optional(S.string).with(S.protobufField, { number: 1, oneof: "result" }),
  runtimeError: S.optional(S.string).with(S.protobufField, { number: 2, oneof: "result" }),
  protobufPayload: S.optional(S.uint8Array).with(S.protobufField, { number: 3, oneof: "result" }),
  jsonPayload: S.optional(S.string).with(S.protobufField, { number: 4, oneof: "result" }),
  skipped: S.optional(S.string).with(S.protobufField, { number: 5, oneof: "result" }),
  serializeError: S.optional(S.string).with(S.protobufField, { number: 6, oneof: "result" }),
  textPayload: S.optional(S.string).with(S.protobufField, { number: 8, oneof: "result" }),
  timeoutError: S.optional(S.string).with(S.protobufField, { number: 9, oneof: "result" }),
});

export type ConformanceResponse = S.Output<typeof conformanceResponse>;
