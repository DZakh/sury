import type { Builder, Encoder } from "./builder";
import type { Path } from "./path";
import {
  arrayTag,
  instanceTag,
  nanTag,
  nullTag,
  objectTag,
  Tag,
  tagFlagBigint,
  tagFlagFunction,
  tagFlagObject,
  tagFlagString,
  tagFlagUndefined,
  tagFlags,
  undefinedTag,
  unionTag,
} from "./tags";
import { Flag, flagUnsafeHas } from "./flags";

export const vendor = "sury";
// Internal symbol to easily identify a SuryError instance.
export const s = /* @__PURE__ */ Symbol(vendor);
// Internal symbol to identify the item proxy (see the makeObjectVal Proxy use).
export const itemSymbol = /* @__PURE__ */ Symbol(vendor + ":item");

// A hacky way to prevent prepending path when error is caught.
// Can be removed after we remove effectCtx
// and there's not way to throw outside of the operation context.
export const shouldPrependPathKey = "p";

export type NumberFormat = "int32" | "port";
export type StringFormat = "json" | "date-time" | "email" | "uuid" | "cuid" | "url";
export type ArrayFormat = "compactColumns";
export type Format = NumberFormat | StringFormat | ArrayFormat;

export type AdditionalItemsMode = "strip" | "strict";

export type InvalidInputDetails = {
  code: "invalid_input";
  path: Path;
  reason: string;
  expected: Internal;
  received: Internal;
  input?: unknown;
  unionErrors?: SuryErrorRecord[];
}
export type InvalidOperationDetails = {
  code: "invalid_operation";
  path: Path;
  reason: string;
}
export type UnsupportedDecodeDetails = {
  code: "unsupported_decode";
  path: Path;
  reason: string;
  from: Internal;
  to: Internal;
}
export type InvalidConversionDetails = {
  code: "invalid_conversion";
  path: Path;
  reason: string;
  from: Internal;
  to: Internal;
  cause?: unknown;
}
export type UnrecognizedKeysDetails = {
  code: "unrecognized_keys";
  path: Path;
  reason: string;
  keys: string[];
}
export type ErrorDetails =
  | InvalidInputDetails
  | InvalidOperationDetails
  | UnsupportedDecodeDetails
  | InvalidConversionDetails
  | UnrecognizedKeysDetails;

export type SuryErrorRecord = Record<string, unknown> & {
  message: string;
  reason: string;
  path: Path;
}

export type AdditionalItems = AdditionalItemsMode | Internal;

export type SchemaErrorMessage = {
  // Catch-all override, used when no more specific key matches.
  _?: string;
  format?: string;
  type?: string;
  minimum?: string;
  maximum?: string;
  minLength?: string;
  maxLength?: string;
  minItems?: string;
  maxItems?: string;
  pattern?: string;
}

export type Internal = {
  type: Tag;
  // A serial number for the schema, used for caching operations.
  seq?: number;
  // Builder for transforming to the "to" schema. If missing, should apply
  // coercion logic.
  parser?: Builder;
  // A field on the "to" schema, to turn it into "parser", when reversing.
  serializer?: Builder;
  // Logic for built-in decoding to the schema type.
  decoder: Builder;
  // Logic for built-in encoding from the schema type.
  encoder?: Encoder;
  // Custom validations on input (before decoder).
  inputRefiner?: (input: Val) => Check[];
  // Custom validations on output (after decoder).
  refiner?: (input: Val) => Check[];
  // A schema we transform to.
  to?: Internal;
  // When transforming with changing shape, store from which path it came
  // from. For S.object, S.tuple, and S.shape.
  from?: string[];
  // The index of the flattened schema reshaping is happening from.
  fromFlattened?: number;
  flattened?: Internal[];
  const?: unknown;
  class?: unknown;
  name?: string;
  title?: string;
  description?: string;
  deprecated?: boolean;
  examples?: unknown[];
  default?: unknown;
  fromDefault?: unknown;
  format?: Format;
  has?: Partial<Record<Tag, boolean>>;
  anyOf?: Internal[];
  additionalItems?: AdditionalItems;
  items?: Internal[];
  required?: string[];
  properties?: Record<string, Internal>;
  noValidation?: boolean;
  minimum?: number;
  maximum?: number;
  minLength?: number;
  maxLength?: number;
  minItems?: number;
  maxItems?: number;
  pattern?: RegExp;
  errorMessage?: SchemaErrorMessage;
  space?: number;
  "$ref"?: string;
  "$defs"?: Record<string, Internal>;
  isAsync?: boolean; // Optional value means that it's not lazily computed yet.
  hasTransform?: boolean; // Optional value means that it's not lazily computed yet.
  "~standard"?: unknown;
  // The reversed (Input ↔ Output swapped) schema, cached lazily as a hidden
  // non-enumerable property via Object.defineProperty (see schema.ts/parse.ts).
  r?: Internal;
}

export type BGlobal = {
  // @as("v") — varCounter
  v: number;
  // @as("o") — flag
  o: number;
  // @as("e") — embeded
  e: unknown[];
  // @as("d") — defs
  d?: Record<string, Internal>;
}

// Adjacent checks sharing `fail` by reference equality are fused with `&&`
// in `emitChecks`, so pass the same helper (e.g. failInvalidType) to every
// check on a val if you want them to emit as one `||`-throw line.
export type Check = {
  // @as("c") — cond
  c: (inputVar: string) => string;
  // @as("f") — fail
  f: (input: Val) => (value: unknown) => ErrorDetails;
}

export type Val = {
  // We might have the same value, but different instances of the val
  // object. Use the bond field, to connect the var call. @as("b") — bond
  b?: Val;
  // @as("p") — parent
  p?: Val;
  // @as("v") — var
  v: () => string;
  // @as("i") — inline
  i: string;
  // The schema of the value that is being parsed. @as("s") — schema
  s: Internal;
  // Whether the val is at output part of expected schema. Needed for
  // schemas like S.array(S.nullAsOption) where child schemas might be
  // transformed. @as("io") — isOutput
  io?: boolean;
  // The schema of the value that we expect to parse into. @as("e") — expected
  e: Internal;
  prev?: Val;
  // @as("f") — flag
  f: Flag;
  // @as("d") — vals
  d?: Record<string, Val>;
  // @as("fv") — flattenedVals
  fv?: Val[];
  // @as("cp") — codeFromPrev
  cp: string;
  // Comma-joined `let` declarations hoisted onto this val by descendants
  // that couldn't own them. Emitted after this val's checks in `merge` (the
  // old varsAllocation slot). @as("hd") — hoistedDecls
  hd: string;
  // Set by `merge` once this val's code is emitted, so a later cached-bond
  // materialization re-reads inline instead of hoisting onto it (#240).
  // @as("fz") — finalized
  fz?: boolean;
  // Invariant: absent iff no checks. Never stored as `[]` so callers can
  // test presence with a plain truthy check instead of length.
  // @as("vc") — checks
  vc?: Check[];
  // @as("u") — isUnion
  u?: boolean;
  // Whether the chain starting from the root prev has a transformation.
  // @as("t") — hasTransform
  t?: boolean;
  path: Path;
  // @as("g") — global
  g: BGlobal;
  // This is to mark an object field as optional. Fields like this should be
  // skipped when the value is undefined. @as("o") — optional
  o?: boolean;
}

export const immutableEmptyArray: unknown[] = [];
// Null-prototype: used as a schema's `properties` placeholder, so an
// indexed/`in` lookup for a field named after an Object.prototype member
// (constructor, toString, hasOwnProperty, ...) must not resolve to
// something inherited instead of correctly reporting "no such property".
export const immutableEmptyObject: Record<string, unknown> = Object.create(null);

// This is dirty
export const isSchemaObject = (obj: unknown): boolean => {
  return !!(obj as { "~standard"?: unknown })["~standard"];
}

export const constField = "const";
export const isLiteral = (schema: Internal): boolean => {
  return constField in schema;
}

export const isOptional = (schema: Internal): boolean => {
  return (
    schema.type === undefinedTag ||
    (schema.type === unionTag && undefinedTag in schema.has!)
  );
}

export const stringify = (unknown: unknown): string => {
  const tagFlag = tagFlags[typeof unknown as Tag]!;

  if (flagUnsafeHas(tagFlag, tagFlagUndefined)) {
    return undefinedTag;
  } else if (flagUnsafeHas(tagFlag, tagFlagObject)) {
    if (unknown === null) {
      return nullTag;
    } else if (Array.isArray(unknown)) {
      return `[${unknown.map(stringify).join(", ")}]`;
    } else if ((unknown as { constructor: unknown }).constructor === Object) {
      const dict = unknown as Record<string, unknown>;
      return `{ ${Object.keys(dict)
        .map((key) => `${key}: ${stringify(dict[key])}; `)
        .join("")}}`;
    } else {
      return Object.prototype.toString.call(unknown);
    }
  } else if (flagUnsafeHas(tagFlag, tagFlagString)) {
    return `"${unknown as string}"`;
  } else if (flagUnsafeHas(tagFlag, tagFlagBigint)) {
    return `${unknown as bigint}n`;
  } else if (flagUnsafeHas(tagFlag, tagFlagFunction)) {
    return `Function`;
  } else {
    return (unknown as { toString: () => string }).toString();
  }
}

export const toExpression = (schema: Internal): string => {
  if (schema.name !== undefined) {
    return schema.name;
  } else if (schema.const !== undefined) {
    return stringify(schema.const);
  } else if (schema.anyOf !== undefined) {
    return schema.anyOf.map(toExpression).join(" | ");
  } else if (schema.format === "compactColumns") {
    // For compactColumns, show the column types if we have properties from .to
    const to = schema.to;
    if (to !== undefined) {
      const props = to.properties;
      if (props !== undefined) {
        const keys = Object.keys(props);
        return `[${keys
          .map((key) => {
            const propSchema = props[key]!;
            return `${toExpression(propSchema)}[]`;
          })
          .join(", ")}]`;
      } else {
        return "unknown[][]";
      }
    } else {
      // No S.to applied, reuse the array expression logic
      const additionalItems = schema.additionalItems;
      if (additionalItems !== undefined && typeof additionalItems === "object") {
        const innerArraySchema = additionalItems;
        return `${toExpression(innerArraySchema)}[]`;
      } else {
        return "unknown[][]";
      }
    }
  } else if (schema.format !== undefined) {
    return schema.format;
  } else if (schema.type === objectTag) {
    const properties = schema.properties!;
    const locations = Object.keys(properties);
    if (locations.length === 0) {
      if (typeof schema.additionalItems === objectTag) {
        const additionalItems = schema.additionalItems as Internal;
        return `{ [key: string]: ${toExpression(additionalItems)}; }`;
      } else {
        return `{}`;
      }
    } else {
      return `{ ${locations
        .map((location) => {
          return `${location}: ${toExpression(properties[location]!)};`;
        })
        .join(" ")} }`;
    }
  } else if (schema.type === nanTag) {
    return "NaN";
  } else if (schema.type === arrayTag) {
    const items = schema.items!;
    if (typeof schema.additionalItems === objectTag) {
      const additionalItems = schema.additionalItems as Internal;
      const itemName = toExpression(additionalItems);
      return (additionalItems.type === unionTag ? `(${itemName})` : itemName) + "[]";
    } else {
      return `[${items.map((schema) => toExpression(schema)).join(", ")}]`;
    }
  } else if (schema.type === instanceTag) {
    return (schema.class as { name: string }).name;
  } else {
    return schema.type;
  }
}
