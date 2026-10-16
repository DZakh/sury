// The codec family: `decode` and `encode` held against the schema's own
// validators and against each other.
//
// What no spec can pin is the agreement between the two sides of a schema,
// because the interesting disagreements need a shape nobody thought to write:
// a container whose transform lives in its items rather than on its `.to`, a
// default written in the Output form, a union under either (#452).
//
//   conformance  an operation's result lands on the side it claims: a decode
//                produces a value the schema's own `isOutput` accepts, an
//                encode one its `isInput` accepts. It needs no oracle - the
//                schema is asked about its own answer - and a half-applied
//                transform always breaks it.
//   agreement    `parse` and `decode` answer the same thing for an input the
//                Input side accepts. They differ only in whether they validate,
//                so a transform one runs and the other skips shows here and
//                nowhere else.
//   round-trip   `decode(encode(o))` is `o`. An encode that undoes less, or
//                more, than the decode it mirrors fails this even when both
//                ends conform. Skipped for a member the grammar marks lossy.
//   reverse      `encode(schema)` is `decode(reverse(schema))`. One operation
//                reached two ways, so a reverse that rebuilds rather than swaps
//                is caught against itself.
//   absent       a key a JSON document leaves out reads through `S.json` the
//                way `parse` reads it, also with the schema held as a field,
//                bare and under `S.shape`.
//   present null a `null` under a present key reads through `S.json` the way
//                `parse` reads it, rejected where parse rejects it.
//
// Equality here is the structural walk, never `isEqual*`: that comparator is
// what the eq family tests, and a property holding only because both sides are
// wrong is not a property.

import { show, structural } from "../unionFuzz/sample";
import { type Ctx, type Family, reason } from "./context";

type Op = (value: unknown) => unknown;

const settled = (value: unknown): boolean => !(value instanceof Promise);

const isRecord = (value: unknown): value is Record<string, unknown> =>
  typeof value === "object" && value !== null;

const has = (value: unknown, key: string): boolean => isRecord(value) && key in value;

const at = (value: unknown, path: string[]): unknown =>
  path.reduce<unknown>((v, key) => (isRecord(v) ? v[key] : undefined), value);

const firstDivergentAbsent = (document: unknown, parsed: unknown, read: unknown, path: string[]): string[] | undefined => {
  if (!isRecord(document) || !isRecord(parsed) || !isRecord(read)) return undefined;
  // A renamed or reshaped field is not an absent one.
  if (Array.isArray(document) !== Array.isArray(parsed) || Object.keys(document).some((key) => !(key in parsed))) {
    return undefined;
  }
  for (const key of Object.keys(parsed)) {
    const next = [...path, key];
    if (!(key in document)) {
      if (!structural(read[key], parsed[key])) return next;
    } else {
      const found = firstDivergentAbsent(document[key], parsed[key], read[key], next);
      if (found) return found;
    }
  }
  return undefined;
};

const check = (ctx: Ctx): void => {
  const { S, schema, reversed, lossy, inputs, outputs, isInput, isOutput, report, compile, count } = ctx;

  const decode = compile<Op>("decode", () => S.decodeOrThrow(schema));
  const encode = compile<Op>("encode", () => S.encodeOrThrow(schema));
  const parse = compile<Op>("agreement", () => S.parseOrThrow(schema));
  const viaReverse = compile<Op>("reverse", () => S.decodeOrThrow(reversed));
  const viaJson = compile<Op>("absent", () => S.parseOrThrow(S.json.with(S.to, schema)));

  if (decode) {
    for (const [i] of inputs) {
      let decoded: unknown;
      try {
        decoded = decode(i);
      } catch (error) {
        // `decode` trusts its input, so a value `isInput` accepted must get
        // through it. A throw is the two disagreeing about the same side.
        report("conformance", `decode threw on ${show(i)}, which isInput accepts - ${reason(error)}`);
        continue;
      }
      if (!settled(decoded)) continue;
      count("results");
      if (isOutput(decoded) !== true) {
        report("conformance", `decode turned ${show(i)} into ${show(decoded)}, which the schema's own isOutput rejects`);
      }
      if (!parse) continue;
      let parsed: unknown;
      try {
        parsed = parse(i);
      } catch (error) {
        report("agreement", `parse threw on ${show(i)} but decode did not - ${reason(error)}`);
        continue;
      }
      if (settled(parsed) && !structural(parsed, decoded)) {
        report("agreement", `parse answered ${show(parsed)} and decode ${show(decoded)} for ${show(i)}`);
      }
    }
  }

  if (parse && viaJson) {
    for (const [i] of inputs) {
      let document: unknown;
      let parsed: unknown;
      try {
        document = JSON.parse(JSON.stringify(i));
        parsed = parse(document);
      } catch {
        continue;
      }
      if (!settled(parsed)) continue;
      count("documents");
      let read: unknown;
      try {
        read = viaJson(document);
      } catch (error) {
        const path = (error as { path?: string[] }).path ?? [];
        const parent = path.slice(0, -1);
        const key = path[parent.length];
        if (key !== undefined && isRecord(at(document, parent)) && !has(at(document, parent), key) && has(at(parsed, parent), key)) {
          report("absent", `parse accepted ${show(document)} but S.json threw at the absent ${path.join(".")} - ${reason(error)}`);
        }
        continue;
      }
      if (!settled(read)) continue;
      const path = firstDivergentAbsent(document, parsed, read, []);
      if (path) {
        report("absent", `the absent ${path.join(".")} of ${show(document)} read as ${show(at(read, path))} through S.json and as ${show(at(parsed, path))} by parse`);
      }
    }
  }

  for (const [field, build] of [
    ["field", () => S.schema({ a: schema })],
    ["shaped field", () => S.schema({ a: S.shape(schema, (v: unknown) => ({ v })) })],
  ] as const) {
    let holder: unknown;
    let parseHolder: Op;
    try {
      holder = build();
      parseHolder = S.parseOrThrow(holder);
    } catch {
      continue;
    }
    const read = compile<Op>("absent", () => S.parseOrThrow(S.json.with(S.to, holder)));
    if (!read) continue;
    // NaN and a required `S.void` are written as `null`.
    let writeBack: Op | undefined;
    const spellsNull = (answer: unknown): boolean => {
      try {
        return ((writeBack ??= S.encodeOrThrow(S.json.with(S.to, holder)))(answer) as { a?: unknown }).a === null;
      } catch {
        return false;
      }
    };
    // A fresh document per read: a parse may write into the object it reads.
    for (const [property, fresh] of [
      ["absent", () => ({})],
      ["present null", () => ({ a: null })],
    ] as const) {
      const document = fresh();
      let parsed: unknown;
      let parseError: unknown;
      try {
        parsed = parseHolder(fresh());
      } catch (error) {
        parseError = error;
      }
      if (parseError === undefined && !settled(parsed)) continue;
      let answer: unknown;
      try {
        answer = read(fresh());
      } catch (error) {
        if (parseError === undefined) {
          report(property, `parse read ${show(document)} as ${show(parsed)} but S.json threw with the schema as a ${field} - ${reason(error)}`);
        }
        continue;
      }
      if (!settled(answer)) continue;
      if (parseError !== undefined) {
        // A carrier may meet a required key another way; a present null it may
        // take only as the value's own JSON spelling.
        if (property === "present null" && !spellsNull(answer)) {
          report(property, `parse rejected ${show(document)} but S.json read it as ${show(answer)} with the schema as a ${field}`);
        }
      } else if (!structural(answer, parsed)) {
        report(property, `parse read ${show(document)} as ${show(parsed)} and S.json as ${show(answer)} with the schema as a ${field}`);
      }
    }
  }

  if (!encode) return;
  for (const [o] of outputs) {
    let encoded: unknown;
    try {
      encoded = encode(o);
    } catch (error) {
      report("conformance", `encode threw on ${show(o)}, which isOutput accepts - ${reason(error)}`);
      continue;
    }
    if (!settled(encoded)) continue;
    count("results");
    if (isInput(encoded) !== true) {
      report("conformance", `encode turned ${show(o)} into ${show(encoded)}, which the schema's own isInput rejects`);
    }
    if (viaReverse) {
      try {
        const mirror = viaReverse(o);
        if (settled(mirror) && !structural(mirror, encoded)) {
          report("reverse", `encode answered ${show(encoded)} for ${show(o)} but decoding the reverse answered ${show(mirror)}`);
        }
      } catch (error) {
        report("reverse", `decoding the reverse threw on ${show(o)} but encode did not - ${reason(error)}`);
      }
    }
    if (!decode || lossy) continue;
    let back: unknown;
    try {
      back = decode(encoded);
    } catch (error) {
      report("round-trip", `decoding the encode of ${show(o)} threw - ${reason(error)}`);
      continue;
    }
    if (settled(back) && !structural(back, o)) {
      report("round-trip", `${show(o)} encoded to ${show(encoded)} and decoded back to ${show(back)}`);
    }
  }
};

export const codec: Family = { check };
