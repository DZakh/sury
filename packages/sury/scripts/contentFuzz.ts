// Content-axis fuzzer (CONTENT_CODEC_SPEC.md).
//
//   pnpm --filter=sury fuzz:content
//   pnpm --filter=sury fuzz:content --show-known
//
// A reading - pack or unpack - is decided by a slot, a field position or a
// declared payload, written down when a link is made and mirrored by
// `reverse`. A spec pins one link. This crosses every source with every target
// and every slot, and checks what holds whichever reading applies:
//
//   crash       nothing but a Sury rejection or a result comes out of building
//               a link, compiling it or running it.
//   ONE_WAY     a link that compiles one direction compiles the other.
//   REVERSE     `reverse(link)` decodes what the link encodes, and encodes
//               what it decodes - the forward reading and the mirrored one
//               agree, value for value, and compile or refuse together.
//   ROUND_TRIP  a value the link decodes encodes back to an input that
//               decodes to the same value.
//   SLOT        a link with two readings and no slot refuses and names both,
//               and each slot then builds; a link with one reading reads the
//               same with `"unpack"` as without it, since the slot declares
//               the source rather than picking a second reading.
//
// Each catalog lists the cases known not to hold, with the reason written by
// hand (see fuzzKit.ts). Exhaustive, so there is no seed.

import * as S from "../index.mjs";
import { catalog, compile, fault, same, show } from "./fuzzKit";

type Source = { schema: unknown; inputs: unknown[] };

const bytes = (text: string): Uint8Array => new TextEncoder().encode(text);
const doc = S.schema({ a: S.number });

// One source per way a value relates to a document: plain text (the one that is
// both), a format or a literal (a value), a number (no text), bytes and their
// text forms (a representation of another kind), an entry (text that is a
// representation), and the document format itself.
const SOURCES: Record<string, Source> = {
  string: { schema: S.string, inputs: ["hi", '{"a":1}', '"x"', "42", ""] },
  trimmed: { schema: S.string.with(S.trim), inputs: [' {"a":1} ', "hi"] },
  "optional-string": { schema: S.optional(S.string), inputs: ['{"a":1}', "hi", undefined] },
  email: { schema: S.email, inputs: ["a@b.co"] },
  literal: { schema: S.schema("x"), inputs: ["x"] },
  number: { schema: S.number, inputs: [42, 0] },
  bytes: { schema: S.uint8Array, inputs: [bytes('{"a":1}'), bytes("hi"), new Uint8Array([137, 80, 78, 71])] },
  base64: { schema: S.base64, inputs: ["aGk=", "eyJhIjoxfQ=="] },
  base64url: { schema: S.base64url, inputs: ["aGk", "eyJhIjoxfQ"] },
  env: { schema: S.env, inputs: ['{"a":1}', "42", undefined, ""] },
  "json-string": { schema: S.jsonString, inputs: ['{"a":1}', '"x"', "42"] },
};

// Every shape a target can take on the axis: the document with and without a
// declared payload, behind a nullish arm, the document itself, plain types,
// bytes and their text form, and a union holding the document.
const TARGETS: Record<string, unknown> = {
  "json-string": S.jsonString,
  "json-string-doc": S.jsonString.with(S.to, doc),
  "json-string-string": S.jsonString.with(S.to, S.string),
  "optional-json-string": S.optional(S.jsonString),
  "optional-json-string-doc": S.optional(S.jsonString.with(S.to, doc)),
  json: S.json,
  string: S.string,
  email: S.email,
  number: S.number,
  bytes: S.uint8Array,
  base64: S.base64,
  doc,
  "json-string-or-number": S.union([S.jsonString, S.number]),
};

const SLOTS = ["", "unpack", "pack"] as const;

const ONE_WAY: Record<string, string> = {
  "optional-string/json-string-or-number:pack":
    "the source's absent arm has nowhere to go in a target with none, so decode refuses while encode, which never writes one, compiles; without the slot both directions stop earlier, at the union question",
  "env/json":
    "`S.json` is the document, not a rendering of one, so an entry's text has no decoder into it; encode passes a JSON value that already is a string straight through as the entry",
  "env/string":
    "a bare string target must say what a blank entry means, with `S.nonEmpty`, `S.minLength(0)` or `S.optional`; an encode writes no blank entry, so it has no question to ask",
  "json-string/optional-json-string":
    "encode meets an absent arm the JSON string has no form for, and the union compiler asks instead of guessing (CODEC_SPEC.md rule 2); decode never produces that arm",
  "json-string/optional-json-string-doc": "the same, with a declared payload",
  "json-string/json-string-or-number": "the same, for a number arm",
};
const REVERSE: Record<string, string> = {};
const ROUND_TRIP: Record<string, string> = {};
const SLOT: Record<string, string> = {
  "optional-string/json-string:unpack":
    "`\"unpack\"` declares every value of the source to be text, and `undefined` is not - it would be stored as the JSON `null`; `\"pack\"` answers the question the unslotted link asks, and the reading placed on the text alone unpacks",
};

const known = catalog({
  ONE_WAY: { label: "links that compile one direction only", cases: ONE_WAY },
  REVERSE: { label: "links whose reverse reads differently", cases: REVERSE },
  ROUND_TRIP: { label: "values that do not survive decode, encode, decode", cases: ROUND_TRIP },
  SLOT: { label: "slots that do not do what they declare", cases: SLOT },
});
const { findings } = known;

type Fn = (value: unknown) => unknown;

// A run's outcome: a value, a Sury rejection, or a crash - which is recorded
// here and treated as a rejection by the caller, so one crash is one finding.
const run = (id: string, what: string, fn: Fn, value: unknown): { ok: true; value: unknown } | { ok: false; why: string } => {
  try {
    return { ok: true, value: fn(value) };
  } catch (error) {
    const { rejected, crash } = fault(error);
    if (crash) findings.push(`${id}: ${what} ${show(value)} crashed - ${crash}`);
    return { ok: false, why: rejected ?? crash! };
  }
};

let links = 0;
let unbuilt = 0;
let oneDirection = 0;
let values = 0;

// What each (source, target) link reads without a slot, for the SLOT property.
const unslotted = new Map<string, { decode?: Fn; ambiguous: boolean }>();

for (const [sourceName, source] of Object.entries(SOURCES)) {
  for (const [targetName, target] of Object.entries(TARGETS)) {
    for (const slot of SLOTS) {
      const half = slot ? `${targetName}:${slot}` : targetName;
      const id = `${sourceName}/${half}`;
      const pair = `${sourceName}/${targetName}`;

      const built = compile<unknown>(() =>
        slot
          ? (source.schema as S.Schema<unknown>).with(S.to, target as never, slot)
          : (source.schema as S.Schema<unknown>).with(S.to, target as never),
      );
      if (built.crash) findings.push(`${id}: building the link crashed - ${built.crash}`);
      const link = built.fn;
      const decode: { fn?: Fn; rejected?: string; crash?: string } = link ? compile(() => S.decodeOrThrow(link as never) as unknown as Fn) : (built as { rejected?: string });
      const encode: { fn?: Fn; rejected?: string; crash?: string } = link ? compile(() => S.encodeOrThrow(link as never) as unknown as Fn) : (built as { rejected?: string });
      for (const [direction, result] of [
        ["decode", decode],
        ["encode", encode],
      ] as const) {
        if (link && result.crash) findings.push(`${id}: ${direction} compile crashed - ${result.crash}`);
      }
      const why = decode.rejected ?? encode.rejected ?? "";
      // The question a slot answers. Sury asks others - which union member a
      // value is, what a blank env entry means - and a slot answers none of them.
      const ambiguous = why.includes("packed or unpacked");
      if (!slot) unslotted.set(pair, { decode: decode.fn, ambiguous });

      // SLOT, first half: two readings and no slot is a refusal naming both,
      // after which each slot is a link that builds and compiles.
      if (slot) {
        const bare = unslotted.get(pair)!;
        if (bare.ambiguous) {
          if (!decode.fn || !encode.fn) {
            known.miss("SLOT", sourceName, half, `${id}: the unslotted link asks for a reading, but this one refuses - ${why}`);
          } else {
            known.hold("SLOT", sourceName, half, id);
          }
        }
      } else if (ambiguous && !why.includes('Choose with S.to and "pack" or "unpack"')) {
        findings.push(`${id}: refuses as ambiguous without naming the two readings - ${why}`);
      }

      if (!link) {
        unbuilt += 1;
        continue;
      }
      links += 1;

      if (!decode.fn !== !encode.fn) {
        known.miss(
          "ONE_WAY",
          sourceName,
          half,
          `${id}: ${decode.fn ? "decodes" : "encodes"} but refuses to ${decode.fn ? "encode" : "decode"} - ${why}`,
        );
      } else if (decode.fn) {
        known.hold("ONE_WAY", sourceName, half, id);
      }

      // REVERSE, compile half: the mirror agrees on which directions exist.
      const reversed = compile(() => S.reverse(link as never));
      const mirrorDecode = reversed.fn ? compile(() => S.decodeOrThrow(reversed.fn as never) as unknown as Fn) : (reversed as { fn?: Fn; crash?: string });
      const mirrorEncode = reversed.fn ? compile(() => S.encodeOrThrow(reversed.fn as never) as unknown as Fn) : (reversed as { fn?: Fn; crash?: string });
      if (reversed.crash || mirrorDecode.crash || mirrorEncode.crash) {
        findings.push(`${id}: reversing crashed - ${reversed.crash ?? mirrorDecode.crash ?? mirrorEncode.crash}`);
      }
      if (!encode.fn !== !mirrorDecode.fn || !decode.fn !== !mirrorEncode.fn) {
        known.miss(
          "REVERSE",
          sourceName,
          half,
          `${id}: encode ${encode.fn ? "compiles" : "refuses"} but its reverse's decode ${mirrorDecode.fn ? "compiles" : "refuses"}; decode ${decode.fn ? "compiles" : "refuses"} but its reverse's encode ${mirrorEncode.fn ? "compiles" : "refuses"}`,
        );
      }

      if (!decode.fn) {
        oneDirection += 1;
        continue;
      }

      let mirrored = true;
      for (const input of source.inputs) {
        const printed = show(input);
        const read = run(id, "decoding", decode.fn, input);
        if (!read.ok) continue;
        values += 1;

        // SLOT, second half: the slot declares the source, so a link with one
        // reading reads the same with it.
        if (slot === "unpack") {
          const bare = unslotted.get(pair)!;
          if (bare.decode) {
            const plain = run(id, "decoding without the slot", bare.decode, input);
            if (plain.ok && !same(plain.value, read.value)) {
              known.miss(
                "SLOT",
                sourceName,
                half,
                `${id} <- ${printed}: reads ${show(read.value)}, but ${show(plain.value)} without the slot`,
                printed,
              );
            }
          }
        }

        if (mirrorEncode.fn) {
          const back = run(id, "encoding through the reverse", mirrorEncode.fn, input);
          if (back.ok && !same(back.value, read.value)) {
            mirrored = false;
            known.miss(
              "REVERSE",
              sourceName,
              half,
              `${id} <- ${printed}: decodes to ${show(read.value)}, its reverse encodes to ${show(back.value)}`,
              printed,
            );
          }
        }

        if (!encode.fn) continue;
        const written = run(id, "encoding", encode.fn, read.value);
        if (!written.ok) {
          known.miss("ROUND_TRIP", sourceName, half, `${id} <- ${printed}: decodes to ${show(read.value)}, which encode refuses - ${written.why}`, printed);
          continue;
        }
        if (mirrorDecode.fn) {
          const mirror = run(id, "decoding through the reverse", mirrorDecode.fn, read.value);
          if (mirror.ok && !same(mirror.value, written.value)) {
            mirrored = false;
            known.miss(
              "REVERSE",
              sourceName,
              half,
              `${id} <- ${printed}: encodes ${show(read.value)} to ${show(written.value)}, its reverse decodes it to ${show(mirror.value)}`,
              printed,
            );
          }
        }
        const again = run(id, "decoding what it encoded", decode.fn, written.value);
        if (!again.ok || !same(again.value, read.value)) {
          known.miss(
            "ROUND_TRIP",
            sourceName,
            half,
            `${id} <- ${printed}: decodes to ${show(read.value)}, encodes to ${show(written.value)}, which ${again.ok ? `decodes to ${show(again.value)}` : `refuses - ${again.why}`}`,
            printed,
          );
        } else {
          known.hold("ROUND_TRIP", sourceName, half, `${id} <- ${printed}`, printed);
        }
      }
      if (mirrored) known.hold("REVERSE", sourceName, half, id);
    }
  }
}

known.finish(
  `${links} links built of ${Object.keys(SOURCES).length}x${Object.keys(TARGETS).length}x${SLOTS.length} (${unbuilt} refused when built, ${oneDirection} refusing to decode), ${values} values read`,
);
