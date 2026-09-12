// Field conversion shared by `S.formData`, `S.env`, `S.urlSearchParams` and
// `S.queryString`. An entry is a string (a form may also carry a `File`).
// Carriers keep their own read/write loops.

import {
  anyOfTag,
  copySchema,
  inputExpression,
  type Internal,
  isOptional,
  nullTag,
  setHas,
  tagFlags,
  type Tag,
  U,
  undefinedTag,
  unknown,
  unknownTag,
  type Val
} from "../base";
import {
  _var,
  B_embedInvalidInput,
  B_invalidOperation,
  B_markOutput,
  B_merge,
  B_next,
  B_refine,
  B_scope,
  B_unsupportedDecode,
  operationArgVar
} from "../builder";
import {
  parse
} from "../parse";
import {
  string
} from "../primitives";

export const presentArm = (schema: Internal): Internal => {
  if (schema.type !== anyOfTag) {
    return schema;
  }
  const present: Internal[] = [];
  const has: Partial<Record<Tag, boolean>> = {};
  for (const variant of schema.anyOf!) {
    if (variant.type !== undefinedTag && variant.type !== nullTag) {
      present.push(variant);
      setHas(has, variant.type);
    }
  }
  if (present.length < 2) {
    return present[0] || schema;
  }
  const mut = copySchema(schema);
  mut.anyOf = present;
  mut.has = has;
  return mut;
};

export const decidesBlank = (schema: Internal): boolean => {
  const flag = tagFlags[schema.type]!;
  return flag & 256
    ? schema.anyOf!.every(decidesBlank)
    : !(flag & 2) ||
        schema.minLength !== U ||
        schema.const !== U ||
        schema.format !== U ||
        (schema.pattern !== U && !schema.pattern.test("")) ||
        (schema.to !== U && decidesBlank(schema.to));
};

export const admitsBlank = (schema: Internal): boolean =>
  schema.minLength === 0 ||
  schema.const === "" ||
  (schema.type === anyOfTag && schema.anyOf!.some(admitsBlank));

export const isAbsent = (schema: Internal): boolean =>
  isOptional(schema) ||
  schema.type === nullTag ||
  (schema.type === anyOfTag && !!schema.has![nullTag]);

export const beforeTo = (schema: Internal): Internal => {
  if (schema.to === U) {
    return schema;
  }
  const mut = copySchema(schema);
  // `delete`, not `= U`: `unionIsTransparent` counts a schema's keys, and a
  // key left present with an undefined value stops every union flattening.
  delete mut.to;
  return mut;
};

export const asList = (value: unknown): unknown[] =>
  value === U ? [] : Array.isArray(value) ? value : [value];

// The check names the target: `Expected number, received undefined` is the
// field's own vocabulary, and `string` is not.
export const asText = (input: Val, target: Internal): Val => {
  const output = B_next(input, input.i, string, target);
  output.v = _var;
  output.cp = `typeof ${input.i}==="string"||${B_embedInvalidInput(input, target)};`;
  return output;
};

// An assignment into the operation's own parameter: `make*` reads `g.r` to
// know the parameter is no longer the value it was given.
const rebinds = (item: Val): void => {
  if (item.i === operationArgVar) {
    item.g.r = true;
  }
};

export const armCode = (item: Val, source: Internal, target: Internal): string => {
  const armIn = B_scope(item);
  armIn.io = false;
  armIn.s = source;
  armIn.e = target;
  const armOut = parse(armIn);
  item.f |= armOut.f & 1;
  if (armOut.i === item.i) {
    return B_merge(armOut);
  }
  rebinds(item);
  return B_merge(armOut) + `${item.i}=${armOut.i};`;
};

export const absentArm = (schema: Internal): Internal =>
  schema.anyOf?.find(
    (variant) => variant.type === (isOptional(schema) ? undefinedTag : nullTag),
  ) || schema;

export const absentCode = (item: Val, schema: Internal): string => {
  const absent = absentArm(schema);
  if (absent.to !== U) {
    return armCode(item, absent, absent);
  }
  if (isOptional(schema)) {
    return "";
  }
  rebinds(item);
  return `${item.i}=null`;
};

export const readWrapped = (
  item: Val,
  schema: Internal,
  present: Internal,
  folds: boolean | undefined,
): Val => {
  const v = item.i;
  const presentCode = armCode(item, item.s, present);
  let code = presentCode;
  if (folds !== U) {
    const absent = absentCode(item, schema);
    code = presentCode
      ? `if(${folds ? v : `${v}!==void 0`}){${presentCode}}${absent && `else{${absent}}`}`
      : absent
        ? `if(${folds ? `!${v}` : `${v}===void 0`}){${absent}}`
        : "";
  }
  const output = B_next(item, v, beforeTo(schema), schema);
  output.v = _var;
  output.io = true;
  output.cp = code;
  output.f |= item.f & 1;
  return parse(B_markOutput(output, item));
};

// Text-only entry: no files, no checkboxes. Optional targets convert the
// present arm so `string -> string | undefined` is not rejected as ambiguous.
export const convertTextEntry = (
  input: Val,
  target: Internal,
  self: Internal,
  blank?: boolean,
): Val => {
  const present = isAbsent(target) ? presentArm(target) : target;
  // Same split as a form field: a required `S.string` must choose, an
  // optional/nullable one reads `""` as absent. `self` is the no-blank
  // converter so the present arm does not re-enter this check.
  if (blank && isAbsent(target) && !admitsBlank(present)) {
    // Chained, not scoped: a parse checked the text on `input`, and only a
    // `prev` walk emits it.
    const item = B_next(input, input.i, self, target);
    item.v = _var;
    // The form loop does `||void 0` before this wrap. Env fields are already
    // in the object, so `""` would otherwise survive an optional with no else.
    if (isOptional(target) && absentArm(target).to === U) {
      item.cp = `${item.i}=${item.i}||void 0;`;
      rebinds(item);
    }
    return readWrapped(item, target, present, true);
  }
  if (blank && !decidesBlank(target)) {
    B_invalidOperation(
      input,
      `Ambiguous "" for ${inputExpression(target)}. Should a blank input be rejected, kept, or read as absent? Choose with S.nonEmpty, S.minLength(0), or S.optional`,
    );
  }
  const flag = tagFlags[present.type]!;
  const textUnion = (flag & 256) && present.anyOf!.every((variant) => tagFlags[variant.type]! & 2);
  if ((flag & 256) && !textUnion) {
    if (self !== input.s) {
      const output = B_next(input, input.i, self, present);
      output.v = _var;
      return output;
    }
    return B_unsupportedDecode(input, input.s, present);
  }
  if ((flag & (64 | 128 | 512 | 8192)) && present.class !== Date) {
    return B_unsupportedDecode(input, input.s, present);
  }
  // A source that is already text (`S.env`) keeps its type instead of being
  // checked again. Not for a jsonString target, which reads the text as its
  // document only from an unknown source: a string is a value it would escape
  // (CONTENT_CODEC_SPEC.md).
  if ((tagFlags[input.s.type]! & 2) && present.format !== "json") {
    return B_refine(input, string, U, present);
  }
  return !textUnion && (present.type === unknownTag || (flag & (2 | 16 | 32)))
    ? B_refine(input, unknown, U, present)
    : asText(input, present);
};
