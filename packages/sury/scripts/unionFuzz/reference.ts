import { outcomeOf } from "./outcome";
import type { Outcome, Sury } from "./types";

type Schema = {
  type?: string;
  anyOf?: Schema[];
  [key: string]: unknown;
};

// Same cut as `unionIsTransparent` in union.ts: a nested union with only the
// factory's own fields flattens into the parent. Grouping/fallback bits from
// `unionPlan` / `unionEmit` are not consulted.
const isTransparentUnion = (schema: Schema): boolean => {
  if (schema.type !== "anyOf") return false;
  let fields = 0;
  for (const key in schema) {
    if (key !== "isAsync" && key !== "hasTransform") fields++;
  }
  return fields === 6;
};

export const flattenVariants = (schemas: readonly unknown[]): unknown[] => {
  const out: unknown[] = [];
  for (const schema of schemas) {
    const s = schema as Schema;
    if (isTransparentUnion(s) && s.anyOf) {
      out.push(...flattenVariants(s.anyOf));
    } else {
      out.push(schema);
    }
  }
  return out;
};

export const variantsOf = (unionSchema: unknown): unknown[] => {
  const s = unionSchema as Schema;
  if (s.type === "anyOf" && Array.isArray(s.anyOf)) return s.anyOf.slice();
  return [unionSchema];
};

const tryEachParser = (
  S: Sury,
  variants: readonly unknown[],
  input: unknown,
): Outcome => {
  const errors: unknown[] = [];
  for (const member of variants) {
    let fn: (value: unknown) => unknown;
    try {
      fn = S.parser(member);
    } catch (error) {
      return outcomeOf(S, () => {
        throw error;
      });
    }
    const next = outcomeOf(S, () => fn(input));
    if (next.ok) return next;
    if (next.kind === "foreign") return next;
    errors.push(next);
  }
  if (errors.length === 0) {
    return outcomeOf(S, () => {
      throw new S.Error();
    });
  }
  const last = errors[errors.length - 1] as Extract<
    Outcome,
    { ok: false; kind: "sury" }
  >;
  return {
    ok: false,
    kind: "sury",
    message: last.message,
    reasons: errors.length,
  };
};

export const referenceParse = (
  S: Sury,
  unionSchema: unknown,
  input: unknown,
): Outcome => tryEachParser(S, variantsOf(unionSchema), input);

export const referenceEncode = (
  S: Sury,
  unionSchema: unknown,
  input: unknown,
): Outcome => {
  // Encode is parse of the reverse. Sequential `S.encoder(member)` is not an
  // oracle: identity member encoders skip the type check and accept anything.
  try {
    return referenceParse(S, S.reverse(unionSchema), input);
  } catch (error) {
    return outcomeOf(S, () => {
      throw error;
    });
  }
};

export const compiledParse = (
  S: Sury,
  unionSchema: unknown,
  input: unknown,
): Outcome =>
  outcomeOf(S, () => {
    const fn = S.parser(unionSchema);
    return fn(input);
  });

export const compiledEncode = (
  S: Sury,
  unionSchema: unknown,
  input: unknown,
): Outcome =>
  outcomeOf(S, () => {
    const fn = S.encoder(unionSchema);
    return fn(input);
  });

export const memberParse = (
  S: Sury,
  member: unknown,
  input: unknown,
): Outcome =>
  outcomeOf(S, () => {
    const fn = S.parser(member);
    return fn(input);
  });

export const memberEncode = (
  S: Sury,
  member: unknown,
  input: unknown,
): Outcome =>
  outcomeOf(S, () => {
    const fn = S.encoder(member);
    return fn(input);
  });
