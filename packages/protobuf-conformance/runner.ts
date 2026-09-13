#!/usr/bin/env -S npx tsx
// The testee. Google's conformance_test_runner spawns this, then writes
// length-prefixed ConformanceRequests on stdin and reads ConformanceResponses
// from stdout until it closes the pipe.
//
// Modelled on the harness bufbuild/protobuf-conformance gives each
// implementation (impl/*/runner.ts, Apache-2.0); the protocol is the same for
// every testee, the codec under test is not.
import { readSync, writeSync } from "node:fs";
import * as S from "sury";
import {
  type ConformanceResponse,
  JSON_FORMAT,
  PROTOBUF,
  TEXT_FORMAT,
  conformanceRequest,
  conformanceResponse,
} from "./protocol";
import { testAllTypesProto3 } from "./testMessages";

const decodeRequest = S.decodeOrThrow(S.protobuf, conformanceRequest);
const encodeResponse = S.decodeOrThrow(conformanceResponse, S.protobuf);
const decodeMessage = S.decodeOrThrow(S.protobuf, testAllTypesProto3);
const encodeMessage = S.decodeOrThrow(testAllTypesProto3, S.protobuf);

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
  // ProtoJSON and text format are separate wire formats Sury does not speak.
  if (request.jsonPayload !== undefined) return skip("ProtoJSON input is not supported");
  if (request.textPayload !== undefined) return skip("text format input is not supported");
  if (request.jspbPayload !== undefined) return skip("JSPB is Google-internal");
  if (request.requestedOutputFormat === JSON_FORMAT) return skip("ProtoJSON output is not supported");
  if (request.requestedOutputFormat === TEXT_FORMAT) return skip("text format output is not supported");
  if (request.requestedOutputFormat !== PROTOBUF) return skip("unknown output format");
  if (request.messageType !== PROTO3) {
    // proto2 and the editions variants, named by their short form so the skip
    // breakdown reads as a list of message types rather than of full paths.
    return skip(`${request.messageType.replace(/^.*\./, "")} messages are not supported`);
  }
  if (request.protobufPayload === undefined) return skip("no payload");

  let value;
  try {
    value = decodeMessage(request.protobufPayload);
  } catch (error) {
    // A parse error is the right answer for the suite's intentionally invalid
    // inputs, so this is not a failure by itself.
    return { ...empty, parseError: String((error as Error).message) };
  }
  try {
    return { ...empty, protobufPayload: encodeMessage(value) };
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
