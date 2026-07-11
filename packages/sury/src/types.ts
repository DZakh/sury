// core.ts — Sury's core schema/decode/encode/codegen engine.
//
// This is a line-for-line port of Sury.res's implementation. Sury.res is now
// a thin ReScript bindings layer over this file (`@module("sury/core")
// external ...`, see src/Sury.res) — the actual logic lives here, as plain
// TypeScript, so it can be read, profiled, and changed without going through
// the ReScript compiler. See CLAUDE.md for the architecture this implements
// (Input vs Output, the decode pipeline, refiner ownership, Val).
//
// Porting notes (apply throughout this file, not just here):
//  - `val`/`check`/`bGlobal` are internal, ephemeral compile-time bookkeeping
//    structures used only while *building* a decoder (never exposed to
//    consumers, never serialized). Sury.res deliberately stores their fields
//    under short `@as(...)` runtime names (e.g. `codeFromPrev` -> `cp`) to
//    keep the library's own shipped bundle small — plain esbuild/terser
//    minification never renames object properties, only local variables, so
//    every repeated `.codeFromPrev` access directly inflates the bundle.
//    TypeScript has no equivalent of `@as` (no compile-time property
//    renaming), so this port keeps the *actual* field names short too, each
//    annotated with the full CLAUDE.md name in a comment. This is the one
//    place this port deviates from "use full names" — see the field tables
//    below.
//  - `internal` (the mutable, all-fields-optional schema representation) and
//    the public tagged-union schema type are the SAME runtime object: ReScript
//    compiles `@tag("type") type t<'value> = private | Never({...}) | ...`
//    constructors to plain object literals `{type: "never", ...}`, and
//    `internal.tag` has `@as("type")`, i.e. `internal.type` *is* that same
//    discriminant field. `castToPublic`/`castToInternal` are `%identity`
//    casts because they're two TypeScript-level views of one object. This
//    port keeps that: one `Schema` class/constructor, and a `SuryType<V>`
//    discriminated-union *type* used only for the public-facing surface,
//    related to `Internal` via plain `as` casts (verified against a
//    from-scratch ReScript compilation probe, not against compiled output).
//  - Anywhere the ReScript source uses `Obj.magic`/`%identity`, this port
//    uses an inline `as` cast at the use site rather than a wrapper function
//    — TypeScript doesn't need a named helper for a no-op cast, and avoiding
//    the extra call keeps the hot path as direct as the ReScript version.
//  - No runtime imports: everything this module needs from JSONSchema/
//    StandardSchema is either pure logic ported alongside it, or (for the
//    JSON Schema conversion entry point) a mutable ref-cell set from outside
//    at module init — the same circular-dependency-breaking trick the
//    ReScript source already used for `standardJSONSchemaRef`.

// =============================================================================
// Path
// =============================================================================
//
// A Path is the already-escaped JS index-expression text for a location in
// the input, e.g. `["a"]["b"]` — ready to be spliced into generated code or
// an error message. Ported from Sury.res's `module Path`.

export type Path = string;

export const pathEmpty: Path = "";
export const pathDynamic: Path = "[]";

// Scans for the first `"` or `\n`; if none is found, the string needs no
// escaping beyond wrapping in quotes (the common case for field names), so
// we skip the JSON.stringify call. Ported from X.Inlined.Value.fromString.
export const inlinedValueFromString = (str: string): string => {
  for (let idx = 0; idx < str.length; idx++) {
    const ch = str[idx];
    if (ch === '"' || ch === "\n") return JSON.stringify(str);
  }
  return `"${str}"`;
}

export const pathFromInlinedLocation = (inlinedLocation: string): Path => {
  return `[${inlinedLocation}]`;
}

export const pathFromLocation = (location: string): Path => {
  return `[${inlinedValueFromString(location)}]`;
}

export const pathToArray = (path: Path): string[] => {
  switch (path) {
    case "":
      return [];
    default:
      return JSON.parse(path.split(`"]["`).join(`","`)) as string[];
  }
}

export const pathFromArray = (array: string[]): Path => {
  switch (array.length) {
    case 0:
      return "";
    case 1:
      return pathFromLocation(array[0]!);
    default:
      return array.map(pathFromLocation).join("");
  }
}

export const pathConcat = (path: Path, concatedPath: Path): Path => {
  return path + concatedPath;
}

// =============================================================================
// Vendor symbols / misc top-level constants
// =============================================================================

export const vendor = "sury";
// Internal symbol to easily identify a SuryError instance.
export const s = /* @__PURE__ */ Symbol(vendor);
// Internal symbol to identify the item proxy (see the makeObjectVal Proxy use).
export const itemSymbol = /* @__PURE__ */ Symbol(vendor + ":item");

// A hacky way to prevent prepending path when error is caught.
// Can be removed after we remove effectCtx
// and there's not way to throw outside of the operation context.
export const shouldPrependPathKey = "p";

// =============================================================================
// tag / format types
// =============================================================================
//
// Ported as TS string-literal unions (matching the `@as(...)` wire values
// exactly) rather than enums — these are the actual runtime discriminant
// values, not just a type-level convenience.

export type Tag =
  | "string"
  | "number"
  | "bigint"
  | "boolean"
  | "symbol"
  | "null"
  | "undefined"
  | "nan"
  | "function"
  | "instance"
  | "array"
  | "object"
  | "union"
  | "never"
  | "unknown"
  | "ref";

// Use variables to reduce bundle size with min+gzip
// Also as a good practice (ignore that we have tag variant 😅)
export const stringTag: Tag = "string";
export const numberTag: Tag = "number";
export const bigintTag: Tag = "bigint";
export const booleanTag: Tag = "boolean";
export const symbolTag: Tag = "symbol";
export const nullTag: Tag = "null";
export const undefinedTag: Tag = "undefined";
export const nanTag: Tag = "nan";
export const functionTag: Tag = "function";
export const instanceTag: Tag = "instance";
export const arrayTag: Tag = "array";
export const objectTag: Tag = "object";
export const unionTag: Tag = "union";
export const neverTag: Tag = "never";
export const unknownTag: Tag = "unknown";
export const refTag: Tag = "ref";

export type NumberFormat = "int32" | "port";
export type StringFormat = "json" | "date-time" | "email" | "uuid" | "cuid" | "url";
export type ArrayFormat = "compactColumns";
export type Format = NumberFormat | StringFormat | ArrayFormat;

export type AdditionalItemsMode = "strip" | "strict";

// =============================================================================
// Flag / ValFlag / TagFlag
// =============================================================================
//
// `flag` is a plain int bitmask (ReScript: `and flag = int`).

export type Flag = number;

// Flat consts (former ReScript `module Flag` with @inline members): the
// public bits threaded through operations. `Flag.with` was the `%orint`
// intrinsic — call sites use `|` directly.
export const flagNone: Flag = 0;
export const flagAsync: Flag = 1;
export const flagDisableNanNumberValidation: Flag = 2;
// flatten: 64
// let without = (flags, flag) => flags->with(flag)->Int.bitwiseXor(flag)

// Truthiness of the bitwise-and (any-overlap), matching the source's
// `Int.bitwiseAnd->Obj.magic` — NOT an all-bits-set test. inlineConst
// relies on this to test one tag against a union of tag bits.
export const flagUnsafeHas = (acc: Flag, flag: Flag): boolean => {
  return (acc & flag) !== 0;
}

// Internal-only flag bits threaded through `val.f` during codegen (distinct
// bit space from the public flag consts above).
export const valFlagNone = 0;
export const valFlagAsync = 1;

// One bit per tag, so a set of tags can be tested with a single bitwise-and
// (see typeCheckCond / inlineConst). `tagFlags` maps a runtime tag string to
// its bit. These were a ReScript module with @inline members — kept as flat
// consts (no namespace object) so the minifier can inline the numbers and no
// property lookup happens on the hot path.
export const tagFlagUnknown = 1;
export const tagFlagString = 2;
export const tagFlagNumber = 4;
export const tagFlagBoolean = 8;
export const tagFlagUndefined = 16;
export const tagFlagNull = 32;
export const tagFlagObject = 64;
export const tagFlagArray = 128;
export const tagFlagUnion = 256;
export const tagFlagRef = 512;
export const tagFlagBigint = 1024;
export const tagFlagNaN = 2048;
export const tagFlagFunction = 4096;
export const tagFlagInstance = 8192;
export const tagFlagSymbol = 16384;
export const tagFlagNever = 32768;
export const tagFlags: Record<string, number> = {
  [unknownTag]: 1,
  [stringTag]: 2,
  [numberTag]: 4,
  [booleanTag]: 8,
  [undefinedTag]: 16,
  [nullTag]: 32,
  [objectTag]: 64,
  [arrayTag]: 128,
  [unionTag]: 256,
  [refTag]: 512,
  [bigintTag]: 1024,
  [nanTag]: 2048,
  ["function"]: 4096,
  [instanceTag]: 8192,
  [neverTag]: 32768,
  [symbolTag]: 16384,
};

// =============================================================================
// error / errorDetails
// =============================================================================
//
// `errorDetails` is ported as a TS discriminated union on `code` (matching
// `@tag("code")`) — a real union at the type level; at runtime it's still a
// plain object with a string `code` field, exactly what ReScript's
// tagged-variant compilation already produces (verified via probe).

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

// The public-facing error shape (`error` in Sury.res): `{message, reason,
// path}`, always also carrying whatever fields the originating errorDetails
// variant had (SuryError's constructor copies every param key onto `this`).
export type SuryErrorRecord = Record<string, unknown> & {
  message: string;
  reason: string;
  path: Path;
}

// =============================================================================
// internal / additionalItems / has / untagged
// =============================================================================
//
// `internal` is the mutable, all-fields-optional working representation used
// throughout this file. It and the public tagged-union schema type are the
// same runtime object (see the file header) — `Internal` below is that one
// shape; the public `SuryType<Value>` union (defined near the public API
// surface, further down) is a TypeScript-only view of the same object.
//
// `additionalItems` is `@unboxed`: `Schema(t<unknown>)`'s payload is
// unwrapped at runtime (the value itself, not `{TAG:"Schema",_0:...}`), so
// at runtime `additionalItems` is exactly `"strip" | "strict" | Internal` —
// distinguish the schema case with `typeof v !== "string"`.
export type AdditionalItems = AdditionalItemsMode | Internal;

export type Has = {
  string?: boolean;
  number?: boolean;
  never?: boolean;
  unknown?: boolean;
  bigint?: boolean;
  boolean?: boolean;
  symbol?: boolean;
  null?: boolean;
  undefined?: boolean;
  nan?: boolean;
  function?: boolean;
  instance?: boolean;
  array?: boolean;
  object?: boolean;
}

export type SchemaErrorMessage = {
  // @as("_")
  catchAll?: string;
  format?: string;
  // @as("type")
  type_?: string;
  minimum?: string;
  maximum?: string;
  minLength?: string;
  maxLength?: string;
  minItems?: string;
  maxItems?: string;
  pattern?: string;
}

export type Builder = (input: Val) => Val;
export type Encoder = (input: Val, target: Internal) => Val;

// The mutable mutable schema representation. `.type` is the public tagged
// union's discriminant field (`@as("type")` on `internal.tag`) — this same
// object, viewed through `SuryType<Value>`, is what the public API returns.
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
  has?: Record<string, boolean>;
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
}

// =============================================================================
// val / check / bGlobal
// =============================================================================
//
// The compile-time view of a runtime value at one point in generated code
// (see CLAUDE.md "Val"). Field names are kept short (matching the ReScript
// `@as(...)` runtime names) for bundle size — see the file header. Full name
// is given in each comment.

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

// =============================================================================
// isSchemaObject / isLiteral / isOptional
// =============================================================================

// Shared immutable empties (X.Array.immutableEmpty / X.Object.immutableEmpty).
export const immutableEmptyArray: unknown[] = [];
export const immutableEmptyObject: Record<string, unknown> = {};

// This is dirty
export const isSchemaObject = (obj: unknown): boolean => {
  return (obj as { "~standard"?: unknown })["~standard"] as unknown as boolean;
}

export const constField = "const";
// The `in` operator (not a `!== undefined` check) is load-bearing: the
// Undefined literal schema stores `const` present with value `undefined`.
export const isLiteral = (schema: Internal): boolean => {
  return constField in schema;
}

export const isOptional = (schema: Internal): boolean => {
  return (
    schema.type === undefinedTag ||
    (schema.type === unionTag && undefinedTag in schema.has!)
  );
}

// =============================================================================
// stringify / toExpression
// =============================================================================

export const stringify = (unknown: unknown): string => {
  const tagFlag = tagFlags[(typeof unknown as Tag)]!;

  if (flagUnsafeHas(tagFlag, tagFlagUndefined)) {
    return undefinedTag;
  } else if (flagUnsafeHas(tagFlag, tagFlagObject)) {
    if (unknown === null) {
      return nullTag;
    } else if (Array.isArray(unknown)) {
      const array = unknown as unknown[];
      let string = "[";
      for (let i = 0; i < array.length; i++) {
        if (i !== 0) {
          string = string + ", ";
        }
        string = string + stringify(array[i]);
      }
      return string + "]";
    } else if ((unknown as { constructor: unknown }).constructor === Object) {
      const dict = unknown as Record<string, unknown>;
      const keys = Object.keys(dict);
      let string = "{ ";
      for (let i = 0; i < keys.length; i++) {
        const key = keys[i]!;
        const value = dict[key];
        string = `${string}${key}: ${stringify(value)}; `;
      }
      return string + "}";
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
      if ((typeof schema.additionalItems as Tag) === objectTag) {
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
  } else if ((schema as unknown as Val).b) {
    // Case for val
    return schema.type;
  } else if (schema.type === arrayTag) {
    const items = schema.items!;
    if ((typeof schema.additionalItems as Tag) === objectTag) {
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
