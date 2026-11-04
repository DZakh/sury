#!/usr/bin/env -S npx tsx
// The testee. Google's conformance_test_runner spawns this, then writes
// length-prefixed ConformanceRequests on stdin and reads ConformanceResponses
// from stdout until it closes the pipe.
//
// Modelled on the harness bufbuild/protobuf-conformance gives each
// implementation (impl/*/runner.ts, Apache-2.0); the protocol is the same for
// every testee, the codec under test is not.
import { readSync, writeSync } from "node:fs";
import { pathToFileURL } from "node:url";
import * as S from "sury";
import {
  type ConformanceResponse,
  JSON_FORMAT,
  PROTOBUF,
  TEXT_FORMAT,
  conformanceRequest,
  conformanceResponse,
} from "./protocol";
import { testAllTypesProto3, testAllTypesProto3Strict } from "./testMessages";

const decodeRequest = S.decodeOrThrow(S.protobuf, conformanceRequest);
const encodeResponse = S.decodeOrThrow(conformanceResponse, S.protobuf);
// `pnpm conformance generated` points this at what protoc-gen-sury writes for
// the same .proto, so the suite holds the generator to the same score.
const generated = process.env.SURY_CONFORMANCE_GENERATED;
const message: S.Schema<unknown, unknown> =
  generated === undefined ? testAllTypesProto3 : (await import(generated)).TestAllTypesProto3Schema;
// The generated module built again with every object strict: S.deepStrict
// stops at the S.recursive a message that holds itself is.
const strictGenerated = async (path: string): Promise<S.Schema<unknown, unknown>> => {
  S.global({ defaultAdditionalItems: "strict" });
  const module = await import(`${pathToFileURL(path).href}?strict`);
  S.global({});
  return module.TestAllTypesProto3Schema;
};
const strictMessage = generated === undefined ? testAllTypesProto3Strict : await strictGenerated(generated);
const decodeMessage = S.decodeOrThrow(S.protobuf, message);
const encodeMessage = S.decodeOrThrow(message, S.protobuf);
// ProtoJSON rejects a key the message doesn't declare unless the request asks
// to ignore it, which is S.strict's answer and the default's.
const fromJson = S.decodeOrThrow(S.jsonString.with(S.to, S.protobufJson), message);
const fromJsonStrict = S.decodeOrThrow(S.jsonString.with(S.to, S.protobufJson), strictMessage);
const toJson = S.decodeOrThrow(message, S.protobufJson.with(S.to, S.jsonString));
const JSON_IGNORE_UNKNOWN_PARSING_TEST = 3;

// The one message type S.protobuf speaks. proto2 and the editions variants are
// answered `skipped`, which the runner counts as neither pass nor fail.
const PROTO3 = "protobuf_test_messages.proto3.TestAllTypesProto3";

const empty: ConformanceResponse = {
  parseError: undefined,
  runtimeError: undefined,
  protobufPayload: undefined,
  jsonPayload: undefined,
  skipped: undefined,
  serializeError: undefined,
  textPayload: undefined,
  timeoutError: undefined,
};

// The runner reports one `skipped` total. Counting the reasons here is what
// lets the CI line say what was excluded rather than just how much.
const tally: Record<string, number> = Object.create(null);
const skip = (why: string): ConformanceResponse => {
  tally[why] = (tally[why] ?? 0) + 1;
  return { ...empty, skipped: why };
};

const test = (request: S.Output<typeof conformanceRequest>): ConformanceResponse => {
  // Text format is a wire format Sury does not speak.
  if (request.textPayload !== undefined) return skip("text format input is not supported");
  if (request.jspbPayload !== undefined) return skip("JSPB is Google-internal");
  if (request.requestedOutputFormat === TEXT_FORMAT) return skip("text format output is not supported");
  if (request.requestedOutputFormat !== PROTOBUF && request.requestedOutputFormat !== JSON_FORMAT) return skip("unknown output format");
  if (request.messageType !== PROTO3) {
    // proto2 and the editions variants, named by their short form so the skip
    // breakdown reads as a list of message types rather than of full paths.
    return skip(`${request.messageType.replace(/^.*\./, "")} messages are not supported`);
  }
  if (request.protobufPayload === undefined && request.jsonPayload === undefined) return skip("no payload");

  let value;
  try {
    value = request.jsonPayload !== undefined
      ? (request.testCategory === JSON_IGNORE_UNKNOWN_PARSING_TEST ? fromJson : fromJsonStrict)(request.jsonPayload)
      : decodeMessage(request.protobufPayload!);
  } catch (error) {
    // A parse error is the right answer for the suite's intentionally invalid
    // inputs, so this is not a failure by itself.
    return { ...empty, parseError: String((error as Error).message) };
  }
  try {
    return request.requestedOutputFormat === JSON_FORMAT
      ? { ...empty, jsonPayload: toJson(value) }
      : { ...empty, protobufPayload: encodeMessage(value) };
  } catch (error) {
    return { ...empty, serializeError: String((error as Error).message) };
  }
};

// The runner frames every message with a 4-byte little-endian length. A short
// read means the pipe closed between frames, which is how the run ends.
const readAll = (length: number): Buffer | undefined => {
  const buffer = Buffer.alloc(length);
  let read = 0;
  while (read < length) {
    let got: number;
    try {
      got = readSync(0, buffer, read, length - read, null);
    } catch (error) {
      if ((error as NodeJS.ErrnoException).code === "EAGAIN") continue;
      if ((error as NodeJS.ErrnoException).code === "EOF") return undefined;
      throw error;
    }
    if (got === 0) return undefined;
    read += got;
  }
  return buffer;
};

const writeAll = (buffer: Buffer): void => {
  let written = 0;
  while (written < buffer.length) {
    written += writeSync(1, buffer, written, buffer.length - written);
  }
};

const serve = (): boolean => {
  const header = readAll(4);
  if (header === undefined) return false;
  const body = readAll(header.readUInt32LE(0));
  if (body === undefined) throw new Error("unexpected EOF inside a request");

  let response: ConformanceResponse;
  try {
    response = test(decodeRequest(new Uint8Array(body)));
  } catch (error) {
    // Anything that escapes `test` is ours, not the payload's: the runner
    // counts a runtime error as a failure, which is what we want to see.
    response = { ...empty, runtimeError: String((error as Error).stack ?? error) };
  }

  const payload = Buffer.from(encodeResponse(response));
  const out = Buffer.alloc(4 + payload.length);
  out.writeUInt32LE(payload.length, 0);
  payload.copy(out, 4);
  writeAll(out);
  return true;
};

let served = 0;
try {
  while (serve()) served++;
} catch (error) {
  process.stderr.write(`sury conformance: exiting after ${served} tests: ${String(error)}\n`);
  process.exit(1);
}
process.stderr.write(`sury-skip-tally ${JSON.stringify(tally)}\n`);
