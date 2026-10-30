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
//   described   the seam's Output JSON Schema publishes the target's
//               definitions, each once and under the target's name.
//   through     with one coder beside "auto", the direction without one
//               answers what the link without coders does, and an operation
//               going on past it (an assert, a second schema) still runs the
//               rest, while the coder's direction holds untouched or
//               handed-back as above.
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
  const { S, schema, outputs, report, compile, count } = ctx;
  if (!outputs.length) return;
  const box = S.schema({ v: S.unknown });
  const seamTo = (target: unknown) =>
    S.$to(box, target, { decode: (input: { v: unknown }) => input.v, encode: (output: unknown) => ({ v: output }) });
  const hold = (target: unknown, place: (o: unknown) => unknown, seamOf?: () => unknown): void => {
    const seam = compile<Op>("seam", seamOf ?? (() => seamTo(target)));
    if (!seam) return;
    const decode = compile<Op>("untouched", () => S.decodeOrThrow(seam));
    const encode = compile<Op>("handed-back", () => S.encodeOrThrow(seam));
    const isOutput = compile<Op>("untouched", () => S.isOutput(target));
    const ownEncode = compile<Op>("seam", () => S.encodeOrThrow(target));
    const ownDecode = compile<Op>("seam", () => S.decodeOrThrow(target));
    if (!isOutput || !ownEncode || !ownDecode) return;
    // Only the names when the seam is the definition: a default the target
    // describes is one the coder's Output never reaches.
    const described = (of: unknown): string => {
      try {
        const defs = (S.toOutputJSONSchemaOrThrow(of) as { $defs?: object }).$defs ?? {};
        return seamOf ? Object.keys(defs).join() : JSON.stringify(defs);
      } catch (error) {
        return `refused - ${reason(error)}`;
      }
    };
    const own = described(target);
    const through = described(seam);
    count("descriptions");
    if (own !== through) report("described", `the seam publishes ${through}, the target ${own}`);
    const readsBack = (o: unknown): boolean => {
      try {
        const decoded = ownDecode(ownEncode(o));
        return settled(decoded) && structural(decoded, o);
      } catch {
        return false;
      }
    };
    for (const [drawn] of outputs) {
      const o = place(drawn);
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
  // A copy of `S.unknown`, which every schema converts from and into on its
  // own; `S.unknown` itself is the seam carve-out `S.any` takes.
  const loose = S.meta(S.unknown, { description: "seam" });
  const mixed = (target: unknown, place: (o: unknown) => unknown): void => {
    const decodes = compile<Op>("seam", () => S.$to(loose, target, { decode: (o: unknown) => o, encode: "auto" }));
    const encodes = compile<Op>("seam", () => S.$to(loose, target, { decode: "auto", encode: (o: unknown) => o }));
    if (!decodes || !encodes) return;
    const isOutput = compile<Op>("untouched", () => S.isOutput(target));
    const plainEncode = compile<Op>("seam", () => S.encodeOrThrow(S.to(loose, target)));
    const plainDecode = compile<Op>("seam", () => S.decodeOrThrow(S.to(loose, target)));
    const coderDecode = compile<Op>("untouched", () => S.decodeOrThrow(decodes));
    const autoEncode = compile<Op>("through", () => S.encodeOrThrow(decodes));
    const autoDecode = compile<Op>("through", () => S.decodeOrThrow(encodes));
    const coderEncode = compile<Op>("handed-back", () => S.encodeOrThrow(encodes));
    const assertsOutput = compile<Op>("through", () => S.assertOutputOrThrow(decodes));
    const assertsInput = compile<Op>("through", () => S.assertInputOrThrow(encodes));
    // A second schema that refuses everything: an encode that drops what comes
    // after the seam returns instead of throwing.
    const tail = S.unknown.with(S.refine, () => false, { error: "the second schema" });
    const encodesOn = compile<Op>("through", () => S.encodeOrThrow(decodes, tail));
    if (!isOutput || !plainEncode || !plainDecode) return;
    const run = (property: string, op: Op | undefined, value: unknown, expected: unknown, what: string): void => {
      if (!op) return;
      try {
        const result = op(value);
        if (!settled(result)) return;
        count("results");
        if (!structural(result, expected) || (property === "untouched" && isOutput(result) !== true)) {
          report(property, `${what} ${show(value)} answered ${show(result)}, not ${show(expected)}`);
        }
      } catch (error) {
        report(property, `${what} threw on ${show(value)} - ${reason(error)}`);
      }
    };
    for (const [drawn] of outputs) {
      const o = place(drawn);
      let input: unknown;
      try {
        input = plainEncode(o);
        const decoded = plainDecode(input);
        if (!settled(input) || !settled(decoded) || !structural(decoded, o)) continue;
      } catch {
        continue;
      }
      run("untouched", coderDecode, o, o, "the coder returned, and decode of");
      run("through", autoEncode, o, input, "encode of");
      run("through", autoDecode, input, o, "decode of");
      run("handed-back", coderEncode, o, o, "the coder took, and encode of");
      run("through", assertsOutput, o, undefined, "assertOutput of");
      run("through", assertsInput, input, undefined, "assertInput of");
      if (encodesOn) {
        try {
          const result = encodesOn(o);
          if (settled(result)) report("through", `encode of ${show(o)} into a second schema skipped it and answered ${show(result)}`);
        } catch (error) {
          if (!(error as Error).message.includes("the second schema")) report("through", `encode of ${show(o)} into a second schema threw ${reason(error)}`);
        }
      }
    }
  };
  // A target with a chain of its own is refused with "The target already
  // converts": the coder would skip that chain, and no Output names its input.
  if ((schema as { to?: unknown }).to === undefined) {
    hold(schema, (o) => o);
    mixed(schema, (o) => o);
  }
  // The schema again two levels into a recursive target, where the coder's
  // Output reaches it through a ref's definition rather than as a child, and
  // where a chain of its own is a field's like any other.
  const node = (self: unknown) => S.schema({ v: schema, next: S.optional(self) });
  const twice = (o: unknown) => ({ v: o, next: { v: o, next: undefined } });
  const recursive = S.recursive("Seam", node);
  hold(recursive, twice);
  mixed(recursive, twice);
  // And the seam as the definition itself, so its target reaches the ref
  // before the definer has returned it.
  hold(recursive, twice, () => S.recursive("Seam", (self: unknown) => seamTo(node(self))));
};

export const seam: Family = { check };
