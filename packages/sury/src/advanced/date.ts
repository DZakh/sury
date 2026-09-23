// `S.date` - an ISO string on the JSON side, a `Date` on ours.

import {
  copySchema,
  initSchema,
  instanceTag,
  type Internal,
  stringTag,
  tagFlags,
  U,
  type Val
} from "../base";
import {
 B_embedInvalidInput,
 B_next,
 B_nextVar,
 B_refine,
 B_unsupportedDecode,
 failInvalidType
} from "../builder";
import {
 instanceDecoder,
 parse
} from "../parse";
import {
 stringDecoderFn
} from "../primitives";
import type { ProtobufCodec } from "./protobufField";

export const invalidDateRefine = (input: Val): Val => {
  return B_refine(input, input.e, [
    {
      c: (inputVar) => `!Number.isNaN(${inputVar}.getTime())`,
      f: failInvalidType,
    },
  ]);
}

// The `toISOString()` result, described once. It outlives the encoder call: it
// becomes the enclosing object's property schema and is reached later as another
// operation's target, so it needs a real decoder (#369) and a stable identity
// for the seq-keyed operation cache - a fresh copy per compilation was both the
// bug and a cache miss.
const dateTimeString: Internal = /* @__PURE__ */ initSchema(
  stringTag,
  stringDecoderFn,
  (s) => {
    s.format = "date-time";
    // `toISOString()` emits only digits, `-:.TZ` and a sign.
    s.formatFlag = 1;
  },
);

// The decoder names `date` rather than the `init` callback's `s`: it is built
// before the schema exists, and only ever runs after.
export const date: Internal = /* @__PURE__ */ initSchema(
  instanceTag,
  (input: Val): Val => {
    const inputTagFlag = tagFlags[input.s.type]!;
    if ((inputTagFlag & 2)) {
      return invalidDateRefine(B_next(input, `new Date(${input.i})`, date));
    } else if ((inputTagFlag & 1)) {
      return invalidDateRefine(instanceDecoder(input));
    } else if ((inputTagFlag & 8192) && input.s.class === date.class) {
      return input;
    } else {
      return B_unsupportedDecode(input, input.s, input.e);
    }
  },
  (s) => {
    s.class = Date;

    // Encoder: Date → string (via toISOString) when target is string
    s.encoder = (input, target) => {
      const toTagFlag = tagFlags[target.type]!;
      if ((toTagFlag & 2)) {
        // `toISOString()` throws a bare RangeError on an invalid Date, which
        // carries no path and never matches `S.Raised` - so the throw is
        // caught and reported against the Date node (`input.s`), which names
        // `Date` in the error. A try/catch costs a valid Date nothing, where a
        // `getTime()` check would run on every encode.
        // The B_refine wrap is what makes the produced string the subject of
        // the target's checks (see the note in advanced/url.ts). Without it
        // `S.isoDateTime.with(S.to, S.date)` tests the datetime regex against
        // the `Date`, which stringifies to "Wed Jan 01 2020 …" and never
        // matches.
        // `noValidation` on the Date is the promise it is valid, so the raw
        if (input.s.noValidation) {
          return parse(B_refine(B_next(input, `${input.i}.toISOString()`, dateTimeString, target)));
        }
        const output = B_nextVar(input, dateTimeString, target);
        output.cp = `let ${output.i};try{${output.i}=${input.v()}.toISOString()}catch(_){${B_embedInvalidInput(
          input,
          input.s,
        )}}`;
        return parse(B_refine(output));
      } else {
        return input;
      }
    };
  },
);

// `S.protobufTimestamp`: `S.date` as a `google.protobuf.Timestamp` field, the
// seconds (1) and nanos (2) since the epoch, read to the millisecond a Date
// holds. Seen again, a Timestamp merges into the one before field by field.
// Neither end checks Google's year 1 to 9999: the binary format never has,
// and a Date past it is still a Date.
export const protobufTimestamp: Internal = /* @__PURE__ */ (() => {
  const s = copySchema(date);
  s.protobufCodec = {
    type: "google.protobuf.Timestamp",
    file: "google/protobuf/timestamp.proto",
    read: (r, _depth, prev) => {
      let ms = prev === U ? 0 : (prev as Date).getTime();
      let seconds = Math.floor(ms / 1000);
      let nanos = (ms - seconds * 1000) * 1e6;
      while (r.pos < r.limit) {
        const tag = r.tag();
        if (tag === 8) seconds = Number(r.int64());
        else if (tag === 16) nanos = r.varint32() | 0;
        else r.skipTag(tag);
      }
      ms = seconds * 1000 + Math.trunc(nanos / 1e6);
      if (!(Math.abs(ms) <= 864e13)) throw Error("protobuf Timestamp is outside the range of a Date");
      return new Date(ms);
    },
    write: (w, value) => {
      const ms = value instanceof Date ? value.getTime() : NaN;
      if (ms !== ms) throw Error("invalid Timestamp");
      const seconds = Math.floor(ms / 1000);
      const nanos = (ms - seconds * 1000) * 1e6;
      if (seconds) {
        w.varint32(8);
        w.varint64(BigInt(seconds));
      }
      if (nanos) {
        w.varint32(16);
        w.varint32(nanos);
      }
    },
  } satisfies ProtobufCodec;
  return s;
})();
