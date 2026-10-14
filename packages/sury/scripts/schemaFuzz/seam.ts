// The seam family: an output-seam coder (`$to`, what ReScript's `~custom`
// compiles to) in front of the drawn schema.
//
// That coder hands over and takes the schema's Output (the output seam,
// CODEC_SPEC.md "Custom conversions"), so whatever the schema converts on its
// own - a field's coder, a nested transform - has already happened to what it
// returns. Each sampled Output goes through a coder that hands it back as is.
//
//   untouched   decode answers the very Output the coder returned, and the
//               schema's own isOutput accepts it: nothing in the schema ran its
//               conversion a second time.
//   handed-back encode gives the coder that Output unchanged, for the same
//               reason in the other direction.
//
// Equality is the structural walk, as in the codec family. Held only on an
// Output the schema itself reads back from its own encode: a union whose arms
// overlap answers another arm for it either way, and the codec family owns
// that.

import { show, structural } from "../unionFuzz/sample";
import { type Ctx, type Family, reason } from "./context";

type Op = (value: unknown) => unknown;

const settled = (value: unknown): boolean => !(value instanceof Promise);

const check = (ctx: Ctx): void => {
  const { S, schema, outputs, isOutput, report, compile, count } = ctx;
  // A target with a chain of its own is refused with "The target already
  // converts": the coder would skip that chain, and no Output names its input.
  if (!outputs.length || (schema as { to?: unknown }).to !== undefined) return;
  const box = S.schema({ v: S.unknown });
  const seam = compile<Op>("seam", () =>
    S.$to(box, schema, { decode: (input: { v: unknown }) => input.v, encode: (output: unknown) => ({ v: output }) }),
  );
  if (!seam) return;
  const decode = compile<Op>("untouched", () => S.decodeOrThrow(seam));
  const encode = compile<Op>("handed-back", () => S.encodeOrThrow(seam));
  const ownEncode = S.encodeOrThrow(schema) as Op;
  const ownDecode = S.decodeOrThrow(schema) as Op;
  const readsBack = (o: unknown): boolean => {
    try {
      const decoded = ownDecode(ownEncode(o));
      return settled(decoded) && structural(decoded, o);
    } catch {
      return false;
    }
  };
  for (const [o] of outputs) {
    if (!readsBack(o)) continue;
    if (decode) {
      try {
        const decoded = decode({ v: o });
        if (settled(decoded)) {
          count("results");
          if (!structural(decoded, o) || isOutput(decoded) !== true) {
            report("untouched", `the coder returned ${show(o)} and decode answered ${show(decoded)}`);
          }
        }
      } catch (error) {
        report("untouched", `decode threw on ${show(o)}, which isOutput accepts - ${reason(error)}`);
      }
    }
    if (encode) {
      try {
        const encoded = encode(o) as { v: unknown };
        if (settled(encoded)) {
          count("results");
          if (!structural(encoded.v, o)) report("handed-back", `encode handed the coder ${show(encoded.v)} for ${show(o)}`);
        }
      } catch (error) {
        report("handed-back", `encode threw on ${show(o)}, which isOutput accepts - ${reason(error)}`);
      }
    }
  }
};

export const seam: Family = { check };
