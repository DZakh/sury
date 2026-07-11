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
export function inlinedValueFromString(str: string): string {
  for (let idx = 0; idx < str.length; idx++) {
    const ch = str[idx];
    if (ch === '"' || ch === "\n") return JSON.stringify(str);
  }
  return `"${str}"`;
}

export function pathFromInlinedLocation(inlinedLocation: string): Path {
  return `[${inlinedLocation}]`;
}

export function pathFromLocation(location: string): Path {
  return `[${inlinedValueFromString(location)}]`;
}

export function pathToArray(path: Path): string[] {
  switch (path) {
    case "":
      return [];
    default:
      return JSON.parse(path.split(`"]["`).join(`","`)) as string[];
  }
}

export function pathFromArray(array: string[]): Path {
  switch (array.length) {
    case 0:
      return "";
    case 1:
      return pathFromLocation(array[0]!);
    default:
      return array.map(pathFromLocation).join("");
  }
}

export function pathConcat(path: Path, concatedPath: Path): Path {
  return path + concatedPath;
}

// =============================================================================
// Vendor symbols / misc top-level constants
// =============================================================================

const vendor = "sury";
// Internal symbol to easily identify a SuryError instance.
const s = Symbol(vendor);
// Internal symbol to identify the item proxy (see the makeObjectVal Proxy use).
const itemSymbol = Symbol(vendor + ":item");

// A hacky way to prevent prepending path when error is caught.
// Can be removed after we remove effectCtx
// and there's not way to throw outside of the operation context.
const shouldPrependPathKey = "p";

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

// `typeof` as ReScript's `%typeof` sees it: same as JS `typeof`, except it
// additionally distinguishes `"nan"` from `"number"` is NOT done here (that's
// a separate, explicit NaN check elsewhere) — this is a straight `typeof`.
export function typeOf(value: unknown): Tag {
  return typeof value as Tag;
}

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

export const Flag = {
  none: 0 as Flag,
  async: 1 as Flag,
  disableNanNumberValidation: 2 as Flag,
  // flatten: 64
  with: (a: Flag, b: Flag): Flag => a | b,
  // let without = (flags, flag) => flags->with(flag)->Int.bitwiseXor(flag)

  // Truthiness of the bitwise-and (any-overlap), matching the source's
  // `Int.bitwiseAnd->Obj.magic` — NOT an all-bits-set test. inlineConst
  // relies on this to test one tag against a union of tag bits.
  unsafeHas: (acc: Flag, flag: Flag): boolean => (acc & flag) !== 0,
  has: (acc: Flag, flag: Flag): boolean => (acc & flag) !== 0,
};

// Internal-only flag bits threaded through `val.f` during codegen (distinct
// bit space from the public `Flag` module above).
export const ValFlag = {
  none: 0,
  async: 1,
};

// One bit per tag, so a set of tags can be tested with a single bitwise-and
// (see typeCheckCond / inlineConst). `get` maps a runtime tag string to its
// bit via the `flags` lookup table.
export const TagFlag = {
  unknown: 1,
  string: 2,
  number: 4,
  boolean: 8,
  undefined: 16,
  null: 32,
  object: 64,
  array: 128,
  union: 256,
  ref: 512,
  bigint: 1024,
  nan: 2048,
  function: 4096,
  instance: 8192,
  symbol: 16384,
  _never: 32768,
  flags: {
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
  } as Record<string, number>,
  get: (tag: Tag): number => TagFlag.flags[tag]!,
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
export function isSchemaObject(obj: unknown): boolean {
  return (obj as { "~standard"?: unknown })["~standard"] as unknown as boolean;
}

export const constField = "const";
// The `in` operator (not a `!== undefined` check) is load-bearing: the
// Undefined literal schema stores `const` present with value `undefined`.
export function isLiteral(schema: Internal): boolean {
  return constField in schema;
}

export function isOptional(schema: Internal): boolean {
  return (
    schema.type === undefinedTag ||
    (schema.type === unionTag && undefinedTag in schema.has!)
  );
}

// =============================================================================
// stringify / toExpression
// =============================================================================

export function stringify(unknown: unknown): string {
  const tagFlag = TagFlag.get(typeOf(unknown));

  if (Flag.unsafeHas(tagFlag, TagFlag.undefined)) {
    return undefinedTag;
  } else if (Flag.unsafeHas(tagFlag, TagFlag.object)) {
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
  } else if (Flag.unsafeHas(tagFlag, TagFlag.string)) {
    return `"${unknown as string}"`;
  } else if (Flag.unsafeHas(tagFlag, TagFlag.bigint)) {
    return `${unknown as bigint}n`;
  } else if (Flag.unsafeHas(tagFlag, TagFlag.function)) {
    return `Function`;
  } else {
    return (unknown as { toString: () => string }).toString();
  }
}

export function toExpression(schema: Internal): string {
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
      if (typeOf(schema.additionalItems) === objectTag) {
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
    if (typeOf(schema.additionalItems) === objectTag) {
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

// =============================================================================
// Schema / SuryError raw-JS runtime (ported verbatim from Sury.res's
// `%%raw` block inside `module InternalError`)
// =============================================================================

// A schema instance has a null prototype (no inherited Object.prototype
// members — schema objects can have arbitrary property names, e.g. via
// object field keys, and a null prototype avoids any collision/shadowing
// weirdness) with exactly one member: `.with`, a trampoline so every schema
// gets `.with(fn, ...args)` (== `fn(schema, ...args)`) without storing a
// per-instance method.
//
// `Internal` (the mutable schema representation used throughout this file)
// and the public tagged-union schema type are the SAME runtime object — see
// the file header. `Schema` instances serve both roles; the public surface
// is just a TypeScript-level cast (see castToPublic/castToInternal below).
export function Schema(this: Internal): void {}
const schemaPrototype: Record<string, unknown> = Object.create(null);
Object.defineProperty(schemaPrototype, "with", {
  get(this: Internal) {
    return (fn: (self: Internal, ...args: unknown[]) => unknown, ...args: unknown[]) =>
      fn(this, ...args);
  },
});
// Also has ~standard below
Schema.prototype = schemaPrototype;

// A serial number for schema instances, used for caching operations
// (see copySchema below). Matches the source's module-scope `seq` counter.
let seq = 1;

// Exn identity bridge — see the file header on `type exn += private Exn` and
// the design note in Sury.res: ReScript's `catch { | S.Exn(e) => }` pattern
// match compiles to `x.RE_EXN_ID === Exn`, where `Exn` is a unique object
// created wherever `type exn += private Exn(error)` is declared. Since that
// declaration now lives in the (still-ReScript) Sury.res bindings module,
// not here, Sury.res calls `__setExnId` once at module load to hand us that
// identity. Must stay a getter (not a plain value) on SuryError's prototype:
// the class body below runs before Sury.res's module-init call does.
let exnId: unknown = {};
export function __setExnId(id: unknown): void {
  exnId = id;
}

// `message` is defined further down (formatErrorMessage); SuryError's
// `message` getter below forward-references it — this works because the
// getter isn't invoked until some caller actually reads `.message`, by
// which point the whole module has finished initializing.
class SuryError extends Error {
  constructor(params: Record<string, unknown>) {
    super();
    for (const key in params) {
      (this as Record<string, unknown>)[key] = params[key];
    }
  }
  get message(): string {
    return formatErrorMessage(this as unknown as SuryErrorRecord);
  }
  // The exn's "payload" _is_ the exception object itself.
  get _1(): this {
    return this;
  }
  get RE_EXN_ID(): unknown {
    return exnId;
  }
}
Object.defineProperty(SuryError.prototype, "name", { value: "SuryError" });
Object.defineProperty(SuryError.prototype, "s", { value: s });

function getOrRethrow(exn: unknown): SuryErrorRecord {
  if (exn && (exn as { s?: symbol }).s === s) {
    return exn as unknown as SuryErrorRecord;
  } else {
    throw exn;
  }
}

// TODO: Throw S.Error
function panic(message: string): never {
  throw new Error(`[Sury] ${message}`);
}

function formatErrorMessage(error: SuryErrorRecord): string {
  return `${error.path === "" ? "" : `Failed at ${error.path}: `}${error.reason}`;
}

// The public `S.Error` class (Error.class in Sury.res's `module Error`).
export const errorClass: unknown = SuryError;

export const InternalError = {
  make: (errorDetails: ErrorDetails): SuryErrorRecord =>
    new SuryError(errorDetails as unknown as Record<string, unknown>) as unknown as SuryErrorRecord,
  getOrRethrow,
  panic,
  message: formatErrorMessage,
};

// =============================================================================
// globalConfig
// =============================================================================

export type GlobalConfig = {
  // @as("m")
  m: (error: SuryErrorRecord) => string;
  // @as("d") — defsAccumulator
  d?: Record<string, Internal>;
  // @as("a") — defaultAdditionalItems
  a: AdditionalItems;
  // @as("f") — defaultFlag
  f: Flag;
}

export type GlobalConfigOverride = {
  defaultAdditionalItems?: AdditionalItemsMode;
  disableNanNumberValidation?: boolean;
}

const initialOnAdditionalItems: AdditionalItemsMode = "strip";
const initialDefaultFlag: Flag = ValFlag.none as unknown as Flag;
export const globalConfig: GlobalConfig = {
  m: InternalError.message,
  d: undefined,
  a: initialOnAdditionalItems as unknown as AdditionalItems,
  f: initialDefaultFlag,
};

// =============================================================================
// base / cached / copySchema / updateOutput
// =============================================================================

const valueOptions: Record<string, unknown> = {};
const configurableValueOptions = { configurable: true };
const valKey = "value";
const reversedKey = "r";

export function baseSchema(tag: Tag, selfReverse: boolean): Internal {
  const schema = new (Schema as unknown as { new (): Internal })();
  schema.type = tag;
  schema.seq = seq++;
  if (selfReverse) {
    valueOptions[valKey] = schema;
    Object.defineProperty(schema, reversedKey, { ...configurableValueOptions, value: schema });
  }
  return schema;
}

export function noopDecoder(input: Val): Val {
  return input;
}

const factoryCache: Record<string, Internal> = {};

export function cached(key: string, tag: Tag, init: (schema: Internal) => void): Internal {
  const existing = factoryCache[key];
  if (existing !== undefined) {
    return existing;
  } else {
    const schema = baseSchema(tag, true);
    init(schema);
    factoryCache[key] = schema;
    return schema;
  }
}

export const unknown: Internal = baseSchema(unknownTag, true);
unknown.decoder = noopDecoder;

export function copySchema(schema: Internal): Internal {
  const c = new (Schema as unknown as { new (): Internal })();
  for (const k in schema) {
    (c as unknown as Record<string, unknown>)[k] = (schema as unknown as Record<string, unknown>)[k];
  }
  c.seq = seq++;
  return c;
}

export function updateOutput<Value>(schema: Internal, fn: (schema: Internal) => void): Value {
  const root = copySchema(schema);
  let mut = root;
  while (mut.to !== undefined) {
    const next = copySchema(mut.to);
    mut.to = next;
    mut = next;
  }
  // This should be the Output schema
  fn(mut);
  return root as unknown as Value;
}
// =============================================================================
// Fragment 02 — module Builder (Sury.res lines 1083-1903)
// =============================================================================
//
// TODO(integration):
//  - `Builder.make(fn)` / `Builder.encoder(fn)` in ReScript are typed identity
//    casts (`Obj.magic`). They export NOTHING here — call sites in later
//    sections translate `Builder.make((~input) => …)` / `Builder.encoder(…)`
//    to just the plain function expression.
//  - expects from prelude: `Val`, `Check`, `Builder`, `Encoder`, `BGlobal`,
//    `Internal`, `ErrorDetails`, `SuryErrorRecord`, `Flag`, `ValFlag`,
//    `TagFlag`, `InternalError`, `stringify`, `toExpression`, `pathEmpty`,
//    `pathConcat`, `pathFromInlinedLocation`, `inlinedValueFromString`,
//    `unknown`, `arrayTag`, `s` (symbol), `shouldPrependPathKey`,
//    `immutableEmptyArray`.
//
// PORT-NOTE: `type s<'value>` (the effect ctx record, Sury.res line 1050) is
// prelude territory but core.ts has no runtime/type for it yet — `EffectCtx`
// is declared here for `effectCtx`'s return type.
export type EffectCtx = {
  fail: (message: string, path?: Path) => never;
};

// PORT-NOTE: `%raw("this")`-based functions (`_var`, `_bondVar`, `_prevVar`,
// `_notVarBeforeValidation`, `_notVarAtParent`, `_notVar`) and
// `failInvalidType` are standalone consts (not only `B.` members) because
// they're compared/stored by reference (`val.v = _var`, `val.v !== _var`,
// `check.f === failInvalidType`). `B` re-exports them so external call sites
// can keep saying `_var` / `failInvalidType`.

function _var(this: Val): string {
  return this.i;
}

function _bondVar(this: Val): string {
  const val = this;
  const bond = val.b!;
  return bond.v();
}

function _prevVar(this: Val): string {
  const val = this;
  const prev = val.prev!;
  return prev.v();
}

function _notVarBeforeValidation(this: Val): string {
  const val = this;
  const v = B_varWithoutAllocation(val.g);
  val.cp = `let ${v}=${val.i};`;
  val.i = v;
  val.v = _var;
  return v;
}

function _notVarAtParent(this: Val): string {
  const val = this;
  const parent = val.p!;
  // A re-readable field access (`parent[key]`). Its decl hoists onto the
  // parent, which outlives this field's own segment — field vals are often
  // materialized late (e.g. completeObjectVal's optional-field check), after
  // their merge code was emitted, so owning it here would drop the decl.
  // If the parent is itself finalized (cached bond after its block closed —
  // #240), re-read inline: the only still-open vals are ancestors whose
  // segments precede the parent's guard, so hoisting there could read
  // `parent[key]` before that guard; inlining defers it to a guarded use.
  if (parent.fz) {
    val.v = _var;
    return val.i;
  } else {
    const v = B_varWithoutAllocation(val.g);
    B_hoistDecl(parent, `${v}=${val.i}`);
    val.v = _var;
    val.i = v;
    return v;
  }
}

function _notVar(this: Val): string {
  const val: Val = this;
  // Already emitted (a late materialization after this val's segment was
  // merged — e.g. a fused `.to` stage reading a previous stage's transformed
  // output): owning a fresh decl here would drop it (the phantom-var fusion
  // bug). Re-read the inline expression instead. Like `_notVarAtParent`'s
  // finalized guard, but that sibling's inline is always an atomic
  // `parent[key]`, whereas a transform val's inline can be compound (e.g.
  // `""+x`), so parenthesize it to stay correct under any operator a consumer
  // wraps it in (`+(""+x)`, not `+""+x`). Mutating `inline` (not just
  // returning the wrap) keeps a second `.var()` — now routed through `_var` —
  // consistent. Re-reading is sound only because the inlines that reach here
  // are idempotent (`""+x`, `+x`): side-effecting/allocating coercions
  // (`BigInt(...)`, `new Date(...)`, `new Array(...)`) are var-materialized by
  // an eager check before they can finalize, and their referenced vars live
  // in an enclosing segment (not a closed loop/`.then` scope).
  if (val.fz) {
    val.v = _var;
    val.i = `(${val.i})`;
    return val.i;
  } else {
    const v = B_varWithoutAllocation(val.g);
    if (val.prev !== undefined) {
      // Own the decl in codeFromPrev: a non-empty codeFromPrev is
      // non-hoistable in `merge`, so a union discriminant reading this var
      // can't be lifted above its `let` (the str->to(option(int)) bug class).
      if (val.i === "") {
        // No inline value yet (assigned by code that already reads this val):
        // declare ahead of the existing producing code.
        val.cp = `let ${v};` + val.cp;
      } else {
        // Declare-and-assign after it; `v` is fresh, so nothing emitted reads it.
        val.cp = val.cp + `let ${v}=${val.i};`;
      }
    } else {
      // No prev to anchor to; hoist onto the val itself (its own segment
      // outlives the materialization).
      if (val.i === "") {
        B_hoistDecl(val, v);
      } else {
        B_hoistDecl(val, `${v}=${val.i}`);
      }
    }
    val.v = _var;
    val.i = v;
    return v;
  }
}

const operationArgVar = "i";

// Pass this as `fail` on every check that wants "expected X, received Y"
// error semantics. Stable reference → adjacent checks fuse.
function failInvalidType(input: Val): (value: unknown) => ErrorDetails {
  let override: string | undefined;
  const em = input.e.errorMessage;
  if (em !== undefined) {
    const d = em as unknown as Record<string, string | undefined>;
    override = d["type"] !== undefined ? d["type"] : d["_"];
  } else {
    override = undefined;
  }
  return B_invalidInputBuilder(undefined, undefined, override)(input);
}

// The B "module" is flattened to individual `B_`-prefixed functions (instead
// of one object literal) so bundlers can tree-shake each helper separately —
// exactly the shape the ReScript compiler used to emit for `module B`.
export function B_embed(b: Val, value: unknown): string {
  const e = b.g.e;
  const l = e.length;
  e[l] = value;
  return `e[${l}]`;
}

export function B_inlineConst(b: Val, schema: Internal): string {
  const tagFlag = TagFlag.get(schema.type);
  const const_ = schema.const;
  if (Flag.unsafeHas(tagFlag, TagFlag.undefined)) {
    return "void 0";
  } else if (Flag.unsafeHas(tagFlag, TagFlag.string)) {
    return inlinedValueFromString(const_ as string);
  } else if (Flag.unsafeHas(tagFlag, TagFlag.bigint)) {
    return (const_ as unknown as string) + "n";
  } else if (
    Flag.unsafeHas(
      tagFlag,
      Flag.with(Flag.with(TagFlag.symbol, TagFlag.function), TagFlag.instance)
    )
  ) {
    return B_embed(b, schema.const);
  } else {
    return const_ as unknown as string;
  }
}

// Escape it once per compiled operation.
// Use bGlobal as cache, so we don't allocate another object + it's garbage collected.
export function B_inlineLocation(global: BGlobal, location: string): string {
  const key = `"${location}"`;
  const cached = (global as unknown as Record<string, string | undefined>)[key];
  if (cached !== undefined) {
    return cached;
  } else {
    const inlinedLocation = inlinedValueFromString(location);
    (global as unknown as Record<string, string>)[key] = inlinedLocation;
    return inlinedLocation;
  }
}


export function B_varWithoutAllocation(global: BGlobal): string {
  const newCounter = global.v + 1;
  global.v = newCounter;
  return `v${newCounter}`;
}

// Append a `let` declaration to a still-open owner val, emitted after the
// owner's checks in `merge`. The owner is the materialized val's immediate
// context (its `prev`, its `parent` for a field read, or itself); since the
// decl lands at the owner's segment end — after the owner's guard, before
// its dependent code — that immediate owner already dominates and outlives
// every use, so no separate scope-tree is needed. The owner must be
// unfinalized; `_notVarAtParent` guards this explicitly.
export function B_hoistDecl(owner: Val, decl: string): void {
  owner.hd = owner.hd === "" ? decl : owner.hd + "," + decl;
}


export function B_operationArg(
  schema: Internal,
  expected: Internal,
  flag: Flag,
  defs: Record<string, Internal> | undefined
): Val {
  return {
    cp: "",
    hd: "",
    v: _var,
    i: operationArgVar,
    f: ValFlag.none,
    s: schema,
    e: expected,
    path: pathEmpty,
    g: {
      d: defs,
      o: flag,
      e: [],
      v: -1,
    },
  };
}

export function B_throw(errorDetails: ErrorDetails): never {
  throw InternalError.make(errorDetails);
}

export function B_unsupportedDecode(b: Val, from: Internal, target: Internal): never {
  return B_throw({
    code: "unsupported_decode",
    from: from,
    to: target,
    reason: `Can't decode ${toExpression(from)} to ${toExpression(
      target
    )}. Use S.to to define a custom decoder`,
    path: b.path,
  });
}

export function B_failWithArg<Arg>(b: Val, fn: (arg: Arg) => ErrorDetails, arg: string): string {
  return `${B_embed(b, (arg: Arg) => {
    B_throw(fn(arg));
  })}(${arg})`;
}

export function B_makeInvalidConversionDetails(input: Val, to: Internal, cause: unknown): ErrorDetails {
  if (cause && (cause as { s?: symbol }).s === s) {
    const error = cause as unknown as SuryErrorRecord;

    // Read about this in shouldPrependPathKey comment.
    if (!(cause as Record<string, unknown>)[shouldPrependPathKey]) {
      (cause as Record<string, unknown>)["path"] = pathConcat(input.path, error.path);
    }
    return error as unknown as ErrorDetails;
  } else {
    let reason: string;
    if (cause instanceof Error) {
      const text = "" + cause;
      if (text.startsWith("Error: ")) {
        reason = text.slice(7);
      } else {
        reason = text;
      }
    } else {
      reason = stringify(cause);
    }
    return {
      code: "invalid_conversion",
      from: input.s,
      to: to,
      cause,
      path: input.path,
      reason,
    };
  }
}

// Checks run against `prev.var()`, so the runtime type at check time
// is `prev.schema`, not the post-narrowing schema on the current val.
export function B_receivedSchema(val: Val): Internal {
  return val.prev !== undefined ? val.prev.s : val.s;
}

export function B_makeInvalidInputDetails(
  expected: Internal,
  received: Internal,
  path: Path,
  input: unknown,
  includeInput: boolean,
  unionErrors?: SuryErrorRecord[],
  reasonOverride?: string
): ErrorDetails {
  let reasonRef =
    reasonOverride !== undefined
      ? reasonOverride
      : `Expected ${toExpression(expected)}, received ${
          includeInput ? stringify(input) : toExpression(received)
        }`;
  if (unionErrors !== undefined) {
    const caseErrors = unionErrors;
    const reasonsDict: Record<string, number> = {};
    for (let idx = 0; idx < caseErrors.length; idx++) {
      const caseError = caseErrors[idx]!;
      const caseReason = caseError.reason.split("\n").join("\n  ");
      const location = caseError.path === "" ? "" : `At ${caseError.path}: `;
      const line = `\n- ${location}${caseReason}`;
      if (!reasonsDict[line]) {
        reasonsDict[line] = 1;
        reasonRef = reasonRef + line;
      }
    }
  }

  const details: ErrorDetails = {
    code: "invalid_input",
    expected: expected,
    received,
    path,
    reason: reasonRef,
    unionErrors,
  };
  if (includeInput) {
    (details as unknown as Record<string, unknown>)["input"] = input;
  }
  return details;
}

// Drop-in `check.fail` builder for InvalidInput failures. The returned
// `(~input) => value => details` closure snapshots expected/received/path
// so it does not retain the val (otherwise the embed array would pin the
// whole val chain). Pass directly as `check.fail` to skip the wrapper.
export function B_invalidInputBuilder(
  expected?: Internal,
  extraPath: Path = pathEmpty,
  reasonOverride?: string,
  includeInput: boolean = true
): (input: Val) => (value: unknown) => ErrorDetails {
  return (input: Val) => {
    const expected_ = expected !== undefined ? expected : input.e;
    const received = B_receivedSchema(input);
    const path = extraPath === pathEmpty ? input.path : pathConcat(input.path, extraPath);
    return (value: unknown) =>
      B_makeInvalidInputDetails(
        expected_,
        received,
        path,
        value,
        includeInput,
        undefined,
        reasonOverride
      );
  };
}


export function B_failWithErrorMessage(
  key: string,
  defaultMessage?: string
): (input: Val) => (value: unknown) => ErrorDetails {
  return (input: Val) => {
    let override: string | undefined;
    const em = input.e.errorMessage;
    if (em !== undefined) {
      const d = em as unknown as Record<string, string | undefined>;
      override = d[key] !== undefined ? d[key] : d["_"];
    } else {
      override = undefined;
    }
    const m = override !== undefined ? override : defaultMessage;
    if (m !== undefined) {
      return B_invalidInputBuilder(undefined, undefined, m)(input);
    } else {
      return failInvalidType(input);
    }
  };
}

// Inline variant: emits the throw expression directly. Used by decoders
// that splice errors into custom JS (e.g. `catch(_){${embedInvalidInput}}`),
// not via the `check` pipeline.
export function B_embedInvalidInput(input: Val, expected: Internal = input.e): string {
  return B_failWithArg(input, B_invalidInputBuilder(expected)(input), input.v());
}

// Caller must verify `val.checks->unsafeToBool` and
// `val.expected.noValidation !== Some(true)` first — the unwrap below
// is unchecked. `inputVar` is usually `val.prev.var()`.
export function B_emitChecks(val: Val, inputVar: string): string {
  const checks = val.vc!;
  const len = checks.length;
  if (len === 1) {
    const check = checks[0]!;
    return `${check.c(inputVar)}||${B_failWithArg(val, check.f(val), inputVar)};`;
  } else {
    let out = "";
    let i = 0;
    while (i < len) {
      const head = checks[i]!;
      const fail = head.f;
      let cond = head.c(inputVar);
      i = i + 1;
      // Extend the fused cond while the next check shares this `fail`.
      while (i < len && checks[i]!.f === fail) {
        cond = cond + "&&" + checks[i]!.c(inputVar);
        i = i + 1;
      }
      out = out + `${cond}||${B_failWithArg(val, fail(val), inputVar)};`;
    }
    return out;
  }
}

// Whether a val's type-narrow checks can lift into a union dispatch
// condition without stranding a declaration the lifted check reads:
// non-transforming vals read the upstream input var (always safe); a
// transforming val is safe only when its prev is non-transforming (stable
// input var) and it has no codeFromPrev of its own to leave behind — else
// the lifted check runs before that producer (the str->to(option(int))
// "v0 is not defined" bug class). Shared by `merge(~hoistCond)` and the
// union deopt scan so they can't drift. Phase 2's {pre, cond, body}
// dispatch will lift the producer into `pre`, collapsing this to "the
// check is a type-narrow."
export function B_isHoistable(val: Val): boolean {
  return val.t === true ? val.prev!.t !== true && val.cp === "" : true;
}

// Walks the val.prev chain and assembles generated code. When
// `~hoistCond` is provided (union codegen), type-narrow checks
// (fail === failInvalidType) lift into that ref as a dispatch
// discriminant instead of being emitted; constraint refines still
// emit inline so their case-specific error message survives. All
// other callers pass no `~hoistCond` and get the plain merge:
// every non-`noValidation` check is emitted inline.
export function B_merge(val: Val, hoistCond?: { contents: string }): string {
  let current: Val | undefined = val;
  let code = "";

  while (current !== undefined) {
    const val: Val = current;
    current = val.prev;

    let currentCode = "";

    if (val.vc) {
      if (hoistCond !== undefined && B_isHoistable(val)) {
        // Partition: route type-narrows to hoistCond, emit refines inline.
        // `noValidation` is intentionally bypassed for the hoisted part —
        // the cond routes between union cases, it doesn't reject, so
        // suppressing would break dispatch.
        const prev = current!;
        const inputVar = prev.v();
        const allChecks = val.vc!;
        let localHoist = "";
        for (let i = 0; i < allChecks.length; i++) {
          const check = allChecks[i]!;
          const condCode = check.c(inputVar);
          if (check.f === failInvalidType) {
            if (localHoist) {
              localHoist = `${localHoist}&&${condCode}`;
            } else {
              localHoist = condCode;
            }
          } else if (val.e.noValidation !== true) {
            currentCode =
              currentCode + `${condCode}||${B_failWithArg(val, check.f(val), inputVar)};`;
          }
        }
        if (localHoist) {
          const cond = hoistCond;
          if (cond.contents) {
            cond.contents = `${localHoist}&&${cond.contents}`;
          } else {
            cond.contents = localHoist;
          }
        }
      } else if (val.e.noValidation !== true) {
        const prev = current!;
        currentCode = B_emitChecks(val, prev.v());
      }
    }

    // Hoisted decls land after this val's checks (the old varsAllocation
    // slot).
    if (val.hd !== "") {
      currentCode = currentCode + `let ${val.hd};`;
    }

    // Now emitted: a later cached-bond materialization can't hoist onto it.
    val.fz = true;

    currentCode = val.cp + currentCode;

    code = currentCode + code;
  }

  return code;
}

export function B_next(prev: Val, initial: string, schema: Internal, expected: Internal = prev.e): Val {
  return {
    // FIXME: vals and other object.val fields should be copied
    prev,
    v: _notVar,
    i: initial,
    f: ValFlag.none,
    s: schema,
    e: expected,
    cp: "",
    hd: "",
    path: prev.path,
    g: prev.g,
    t: true,
    d: prev.d,
  };
}

// Pass a non-empty `~checks` or omit it. Never pass `~checks=[]` —
// that would break the val.checks "absent iff no checks" invariant.
export function B_refine(val: Val, schema: Internal = val.s, checks?: Check[], expected: Internal = val.e): Val {
  const shouldLink = val.v !== _var;
  const nextVal: Val = {
    prev: val,
    i: val.i,
    v: shouldLink ? _prevVar : _var,
    f: val.f,
    s: schema,
    e: expected,
    cp: "",
    hd: "",
    vc: checks,
    path: val.path,
    g: val.g,
    t: val.t,
    d: val.d,
  };
  if (shouldLink) {
    const valVar: () => string = val.v.bind(val);
    val.v = () => {
      const v = valVar();
      nextVal.i = v;
      nextVal.v = _var;
      return v;
    };
  }
  return nextVal;
}

// Lazy-allocate helper for mutating an existing val (as opposed to
// building a local array and passing it through `refine`).
export function B_pushCheck(val: Val, check: Check): void {
  if (val.vc !== undefined) {
    val.vc.push(check);
  } else {
    val.vc = [check];
  }
}

// Applies both refiners. Input checks push onto valInput.checks
// (emit at pre-transform slot); output checks wrap val via refine.
// When valInput.prev is None, input checks fold into the output
// wrap so emit has a prev.var(). Sets isOutput on the result.
// TODO: async output refiner must run inside .then(), not on the Promise.
export function B_markOutput(val: Val, valInput: Val): Val {
  let deferredInputChecks: Check[] | undefined;
  const inputRefiner = valInput.e.inputRefiner;
  if (inputRefiner !== undefined) {
    const checks = inputRefiner(valInput);
    if (checks.length > 0) {
      if (valInput.prev !== undefined) {
        for (let i = 0; i < checks.length; i++) {
          B_pushCheck(valInput, checks[i]!);
        }
        deferredInputChecks = undefined;
      } else {
        deferredInputChecks = checks;
      }
    } else {
      deferredInputChecks = undefined;
    }
  } else {
    deferredInputChecks = undefined;
  }

  let outputChecks: Check[] | undefined;
  const refiner = val.e.refiner;
  if (refiner !== undefined) {
    const checks = refiner(val);
    outputChecks = checks.length > 0 ? checks : undefined;
  } else {
    outputChecks = undefined;
  }

  let result: Val;
  if (deferredInputChecks !== undefined && outputChecks !== undefined) {
    result = B_refine(val, undefined, deferredInputChecks.concat(outputChecks));
  } else if (deferredInputChecks !== undefined) {
    result = B_refine(val, undefined, deferredInputChecks);
  } else if (outputChecks !== undefined) {
    result = B_refine(val, undefined, outputChecks);
  } else {
    result = val;
  }
  result.io = true;
  return result;
}

// Used in union codegen: splice a literal child's checks into the parent
// as dispatch discriminants. Each cond's `inputVar` is rewritten to
// `parent[key]`; `fail` stays shared so lifted checks fuse with the
// parent's own type guard. No-op if the child has no checks.
export function B_hoistChildChecks(parent: Val, child: Val, key: string): void {
  if (child.vc) {
    const pathAppend = pathFromInlinedLocation(B_inlineLocation(parent.g, key));
    child.vc!.forEach((check) => {
      B_pushCheck(parent, {
        c: (inputVar) => check.c(inputVar + pathAppend),
        f: check.f,
      });
    });
    child.vc = undefined;
  }
}

export function B_dynamicScope(from: Val, locationVar: string): Val {
  // `additionalItems` doubles as the value schema for a dict-shaped val.
  // Extract it via a real pattern match: a non-`Schema` mode (`Strip`/`Strict`
  // on a fixed-property object) must never be cast to a schema — that string
  // reaching `isLiteral` is the `'const' in "strip"` crash. Callers only pass
  // dict sources; the `unknown` fallback keeps a misuse safe instead of crashing.
  const schemaAdditionalItems = from.s.additionalItems;
  const expectedAdditionalItems = from.e.additionalItems;
  return {
    v: _notVarBeforeValidation,
    i: `${from.v()}[${locationVar}]`,
    f: from.f,
    s:
      schemaAdditionalItems !== undefined && typeof schemaAdditionalItems !== "string"
        ? schemaAdditionalItems
        : unknown,
    e:
      expectedAdditionalItems !== undefined && typeof expectedAdditionalItems !== "string"
        ? expectedAdditionalItems
        : unknown,
    cp: "",
    hd: "",
    p: from,
    path: pathEmpty,
    g: from.g,
  };
}

export function B_nextConst(from: Val, schema: Internal, expected?: Internal): Val {
  return B_next(from, B_inlineConst(from, schema), schema, expected);
}

export function B_asyncVal(from: Val, initial: string): Val {
  const v = B_next(from, initial, from.s);
  v.f = ValFlag.async;
  return v;
}

export function B_Val_Object_add(objectVal: Val, location: string, val: Val): void {
  if (objectVal.s.type === arrayTag) {
    objectVal.s.items!.push(val.s);
  } else {
    if (!val.o) {
      objectVal.s.required!.push(location);
    }
    objectVal.s.properties![location] = val.s;
  }

  // Async field values must be reachable as a plain identifier so
  // the accumulator in completeObjectVal can use val.inline as a
  // destructuring/reference target. For e.g. array-of-async, the
  // asyncVal's inline is a Promise.all(...) expression, not a var.
  // This has to happen before val->merge, which finalizes the prev
  // chain and locks the emitted code.
  if (Flag.unsafeHas(val.f, ValFlag.async)) {
    val.v();
  }
  objectVal.cp = objectVal.cp + B_merge(val);
  objectVal.d![location] = val;
}

export function B_Val_Object_merge(target: Val, vals: Record<string, Val>): void {
  const locations = Object.keys(vals);
  for (let idx = 0; idx < locations.length; idx++) {
    const location = locations[idx]!;
    B_Val_Object_add(target, location, vals[location]!);
  }
}

export function B_Val_var(val: Val): string {
  return val.v();
}

export function B_Val_addKey(objVal: Val, key: string, value: Val): string {
  return `${objVal.v()}[${key}]=${value.i}`;
}

export function B_Val_scope(val: Val): Val {
  const shouldLink = val.v !== _var;

  // TODO: Simplify bond
  const nextVal: Val = {
    i: val.i,
    s: val.s,
    e: val.e,
    f: Flag.none,
    path: val.path,
    g: val.g,
    v: shouldLink ? _bondVar : _var,
    b: val,
    cp: "",
    hd: "",
    u: false,
    t: false,
    io: val.io,
    d: val.d, // TODO: Is this correct?
  };
  if (shouldLink) {
    const valVar: () => string = val.v.bind(val);
    val.v = () => {
      const v = valVar();
      nextVal.i = v;
      nextVal.v = _var;
      return v;
    };
  }
  return nextVal;
}

export function B_embedTransformation(input: Val, fn: (input: unknown) => unknown, isAsync: boolean): Val {
  const outputVar = B_varWithoutAllocation(input.g);
  const output = B_next(input, outputVar, unknown, input.e.to!);
  output.v = _var;
  if (isAsync) {
    if (!Flag.unsafeHas(input.g.o, Flag.async)) {
      B_throw({
        code: "invalid_operation",
        path: pathEmpty,
        reason:
          "Encountered unexpected async transform or refine. Use parseAsyncOrThrow operation instead",
      });
    }
    output.f = Flag.with(output.f, ValFlag.async);
  }
  const embededFn = B_embed(input, fn);
  const failure = `${B_failWithArg(
    output,
    (e: unknown) => B_makeInvalidConversionDetails(input, unknown, e),
    `x`
  )}`;
  // Feed the transform the input's var when it already carries checks — it's
  // materialized into a var anyway (the check references it), so reuse it
  // instead of re-inlining the source expression (e.g. `i["x"]`) twice.
  output.cp = `let ${outputVar};try{${outputVar}=${embededFn}(${
    input.vc ? input.v() : input.i
  })${isAsync ? `.catch(x=>${failure})` : ""}}catch(x){${failure}}`;
  return output;
}

export function B_effectCtx(input: Val): EffectCtx {
  return {
    fail: (message: string, path: Path = pathEmpty): never => {
      const error = InternalError.make(
        B_invalidInputBuilder(undefined, path, message, false)(input)(void 0)
      );
      // Read about this in shouldPrependPathKey comment.
      (error as Record<string, unknown>)[shouldPrependPathKey] = 1;
      throw error;
    },
  };
}

export function B_invalidOperation(val: Val, description: string): never {
  return B_throw({ code: "invalid_operation", reason: description, path: val.path });
}

export function B_mergeWithCatch(
  val: Val,
  catchFn: (errorVar: string) => string,
  appendSafe?: () => string
): string {
  const valCode = B_merge(val);
  if (
    valCode === "" &&
    // FIXME: Instead of this wrap all S.transform in a try/catch
    !Flag.unsafeHas(val.f, ValFlag.async)
  ) {
    return valCode + (appendSafe !== undefined ? appendSafe() : "");
  } else {
    const errorVar = B_varWithoutAllocation(val.g);

    const catchCode = `${catchFn(errorVar)};throw ${errorVar}`;

    if (Flag.unsafeHas(val.f, ValFlag.async)) {
      val.i = `${val.i}.catch(${errorVar}=>{${catchCode}})`;
    }
    return `try{${valCode}${
      appendSafe !== undefined ? appendSafe() : ""
    }}catch(${errorVar}){${catchCode}}`;
  }
}

export function B_mergeWithPathPrepend(
  val: Val,
  parent: Val,
  locationVar?: string,
  appendSafe?: () => string
): string {
  if (val.path === pathEmpty && locationVar === undefined) {
    return B_merge(val);
  } else {
    return B_mergeWithCatch(
      val,
      (errorVar) =>
        `${errorVar}.path=${
          parent.path === "" ? "" : `${inlinedValueFromString(parent.path)}+`
        }${locationVar !== undefined ? `'["'+${locationVar}+'"]'+` : ""}${errorVar}.path`,
      appendSafe
    );
  }
}

// Kept a named `function` so `fn.toString()` reads `function noopOperation(i)`
// — tests (and the U.res wallaby workaround) match on that exact text.
export function noopOperation(i: unknown): unknown {
  return i;
}
(noopOperation as unknown as Record<string, unknown>)["embedded"] = immutableEmptyArray;
// TODO: Split validation code and transformation code
// =============================================================================
// Fragment 03 — primitives (Sury.res lines 1905-2255)
// int32FormatValidation, typeofCond, nanCond, isArrayCond, objectTagCond,
// instanceofCond, numberDecoder, float, int, inputToString, stringDecoderFn,
// string, booleanDecoder, bool, bigintDecoder, bigint, symbolDecoder, symbol,
// setHas, jsonName, literalDecoder, unit, nullLiteral, nan, Literal.
//
// TODO(integration): expects from the Builder/B section (earlier section):
//   B_embed, B_refine, B_next, B_nextConst, B_varWithoutAllocation, _var,
//   B_unsupportedDecode, failInvalidType, B_embedInvalidInput, B_inlineConst
// TODO(integration): expects from the prelude: TagFlag, Flag, cached,
//   baseSchema, isLiteral, typeOf, tag consts, Internal, Val, Check, Builder.
// =============================================================================

export const int32FormatValidation = (inputVar: string) => {
  return `${inputVar}<=2147483647&&${inputVar}>=-2147483648&&${inputVar}%1===0`;
};

// Atomic type-narrow conditions, shared by the type decoders and the union
// dispatch (`typeCheckCond`) so the two can't drift.
export const typeofCond = (tag: Tag) => (inputVar: string): string =>
  `typeof ${inputVar}==="${tag}"`;
export const nanCond = (inputVar: string): string => `Number.isNaN(${inputVar})`;
export const isArrayCond = (inputVar: string): string => `Array.isArray(${inputVar})`;
export const objectTagCond = (inputVar: string): string =>
  `${typeofCond(objectTag)(inputVar)}&&${inputVar}`;
// PORT-NOTE: `class` is a reserved word in TS — the labeled arg `~class` is
// ported as the parameter name `class_`.
export const instanceofCond = (b: Val, class_: unknown) => (inputVar: string): string =>
  `${inputVar} instanceof ${B_embed(b, class_)}`;

export const numberDecoder: Builder = (input: Val) => {
  const inputTagFlag = TagFlag.get(input.s.type);
  if (Flag.unsafeHas(inputTagFlag, TagFlag.unknown)) {
    const checks: Check[] = [
      {
        c: typeofCond(numberTag),
        f: failInvalidType,
      },
    ];
    if (input.e.format === "int32") {
      checks.push({
        c: (inputVar) => int32FormatValidation(inputVar),
        f: failInvalidType,
      });
    } else {
      if (!Flag.unsafeHas(input.g.o, Flag.disableNanNumberValidation)) {
        checks.push({
          c: (inputVar) => `!${nanCond(inputVar)}`,
          f: failInvalidType,
        });
      }
    }
    return B_refine(input, input.e, checks);
  } else if (Flag.unsafeHas(inputTagFlag, TagFlag.string)) {
    const outputVar = B_varWithoutAllocation(input.g);

    const output = B_next(input, outputVar, input.e);
    output.v = _var;
    // Own the `+input` coercion (decl included) in codeFromPrev so it's
    // non-hoistable: feeding a union dispatch (e.g. str->to(option(int))) can't
    // lift the type-narrow check below above its `let v0=+i`.
    output.cp = `let ${outputVar}=+${input.v()};`;

    output.vc = [
      {
        c: (_inputVar) =>
          input.e.format === "int32"
            ? int32FormatValidation(outputVar)
            : `!${nanCond(outputVar)}`,
        f: failInvalidType,
      },
    ];
    return output;
  } else if (!Flag.unsafeHas(inputTagFlag, TagFlag.number)) {
    return B_unsupportedDecode(input, input.s, input.e);
  } else if (input.s.format !== input.e.format && input.e.format === "int32") {
    return B_refine(input, input.e, [
      {
        c: (inputVar) => int32FormatValidation(inputVar),
        f: failInvalidType,
      },
    ]);
  } else {
    return input;
  }
};

export const float = () =>
  cached(numberTag, numberTag, (s) => {
    s.decoder = numberDecoder;
  });

export const int = () =>
  cached("i", numberTag, (s) => {
    s.format = "int32";
    s.decoder = numberDecoder;
  });

// PORT-NOTE: the source's `let rec inputToString = ... and stringDecoderFn =
// ... and string = ...` mutual-recursion group falls inside this section's
// line range, so all three are ported here (the name list in the task omitted
// stringDecoderFn/string, but they are inseparable from inputToString).
export function inputToString(input: Val): Val {
  return B_next(input, `""+${input.i}`, string());
}
export function stringDecoderFn(input: Val): Val {
  const inputTagFlag = TagFlag.get(input.s.type);
  if (Flag.unsafeHas(inputTagFlag, TagFlag.unknown)) {
    return B_refine(input, input.e, [
      {
        c: typeofCond(stringTag),
        f: failInvalidType,
      },
    ]);
  } else if (
    Flag.unsafeHas(
      inputTagFlag,
      Flag.with(
        TagFlag.boolean,
        Flag.with(
          TagFlag.number,
          Flag.with(
            TagFlag.bigint,
            Flag.with(TagFlag.undefined, Flag.with(TagFlag.null, TagFlag.nan)),
          ),
        ),
      ),
    ) && isLiteral(input.s)
  ) {
    const const_ = "" + (input.s.const as unknown as string);
    const schema = baseSchema(stringTag, false);
    schema.const = const_ as unknown;
    return B_next(input, `"${const_}"`, schema);
  } else if (
    Flag.unsafeHas(
      inputTagFlag,
      Flag.with(TagFlag.boolean, Flag.with(TagFlag.number, TagFlag.bigint)),
    )
  ) {
    return inputToString(input);
  } else if (!Flag.unsafeHas(inputTagFlag, TagFlag.string)) {
    return B_unsupportedDecode(input, input.s, input.e);
  } else {
    return input;
  }
}
export function string(): Internal {
  return cached(stringTag, stringTag, (s) => {
    s.decoder = stringDecoderFn;
  });
}

export const booleanDecoder: Builder = (input: Val) => {
  const inputTagFlag = TagFlag.get(input.s.type);
  if (Flag.unsafeHas(inputTagFlag, TagFlag.unknown)) {
    return B_refine(input, input.e, [
      {
        c: typeofCond(booleanTag),
        f: failInvalidType,
      },
    ]);
  } else if (Flag.unsafeHas(inputTagFlag, TagFlag.string)) {
    const outputVar = B_varWithoutAllocation(input.g);

    const output = B_next(input, outputVar, input.e);
    output.v = _var;

    const inputVar = input.v();
    output.cp = `let ${outputVar};(${output.i}=${inputVar}==="true")||${inputVar}==="false"||${B_embedInvalidInput(
      input,
    )};`;
    return output;
  } else if (!Flag.unsafeHas(inputTagFlag, TagFlag.boolean)) {
    return B_unsupportedDecode(input, input.s, input.e);
  } else {
    return input;
  }
};

export const bool = () =>
  cached(booleanTag, booleanTag, (s) => {
    s.decoder = booleanDecoder;
  });

export const bigintDecoder: Builder = (input: Val) => {
  const inputTagFlag = TagFlag.get(input.s.type);

  if (Flag.unsafeHas(inputTagFlag, TagFlag.unknown)) {
    return B_refine(input, input.e, [
      {
        c: typeofCond(bigintTag),
        f: failInvalidType,
      },
    ]);
  } // TODO: Skip formats which 100% don't match
  else if (Flag.unsafeHas(inputTagFlag, TagFlag.string)) {
    const outputVar = B_varWithoutAllocation(input.g);
    const output = B_next(input, outputVar, input.e);
    output.v = _var;
    output.cp = `let ${outputVar};try{${outputVar}=BigInt(${input.v()})}catch(_){${B_embedInvalidInput(
      input,
    )}}`;
    return output;
  } else if (Flag.unsafeHas(inputTagFlag, TagFlag.number)) {
    return B_next(input, `BigInt(${input.i})`, input.e);
  } else if (!Flag.unsafeHas(inputTagFlag, TagFlag.bigint)) {
    return B_unsupportedDecode(input, input.s, input.e);
  } else {
    return input;
  }
};

export const bigint = () =>
  cached(bigintTag, bigintTag, (s) => {
    s.decoder = bigintDecoder;
  });

export const symbolDecoder: Builder = (input: Val) => {
  const inputTagFlag = TagFlag.get(input.s.type);
  if (Flag.unsafeHas(inputTagFlag, TagFlag.unknown)) {
    return B_refine(input, input.e, [
      {
        c: typeofCond(symbolTag),
        f: failInvalidType,
      },
    ]);
  } else if (!Flag.unsafeHas(inputTagFlag, TagFlag.symbol)) {
    return B_unsupportedDecode(input, input.s, input.e);
  } else {
    return input;
  }
};

export const symbol = () =>
  cached(symbolTag, symbolTag, (s) => {
    s.decoder = symbolDecoder;
  });

export function setHas(has: Record<string, boolean>, tag: Tag): void {
  has[
    Flag.unsafeHas(TagFlag.get(tag), Flag.with(TagFlag.union, TagFlag.ref))
      ? unknownTag
      : tag
  ] = true;
}

export const jsonName = `JSON`;

export const literalDecoder: Builder = (input: Val) => {
  const expectedSchema = input.e;
  if (expectedSchema.noValidation! && !input.u!) {
    return B_nextConst(input, expectedSchema);
  } else if (isLiteral(input.s)) {
    if (input.s.const === expectedSchema.const) {
      return input;
    } else {
      return B_nextConst(input, expectedSchema);
    }
  } else {
    const schemaTagFlag = TagFlag.get(expectedSchema.type);

    if (
      Flag.unsafeHas(TagFlag.get(input.s.type), TagFlag.string) &&
      Flag.unsafeHas(
        schemaTagFlag,
        Flag.with(
          TagFlag.boolean,
          Flag.with(
            TagFlag.number,
            Flag.with(
              TagFlag.bigint,
              Flag.with(TagFlag.undefined, Flag.with(TagFlag.null, TagFlag.nan)),
            ),
          ),
        ),
      )
    ) {
      const stringConstSchema = baseSchema(stringTag, false);
      stringConstSchema.const = "" + (expectedSchema.const as unknown as string);

      const stringConstVal = B_nextConst(input, stringConstSchema, stringConstSchema);

      stringConstVal.vc = [
        {
          c: (inputVar) => `${inputVar}==="${stringConstSchema.const as unknown as string}"`,
          f: failInvalidType,
        },
      ];

      return B_nextConst(stringConstVal, expectedSchema, expectedSchema);
    } else if (Flag.unsafeHas(schemaTagFlag, TagFlag.nan)) {
      return B_refine(input, expectedSchema, [
        {
          c: nanCond,
          f: failInvalidType,
        },
      ]);
    } else {
      return B_refine(input, expectedSchema, [
        {
          c: (inputVar) => `${inputVar}===${B_inlineConst(input, expectedSchema)}`,
          f: failInvalidType,
        },
      ]);
    }
  }
};

export const unit = () =>
  cached(undefinedTag, undefinedTag, (s) => {
    s.const = void 0;
    s.decoder = literalDecoder;
  });

export const nullLiteral = () =>
  cached(nullTag, nullTag, (s) => {
    s.const = null;
    s.decoder = literalDecoder;
  });

export const nan = () =>
  cached(nanTag, nanTag, (s) => {
    s.const = NaN;
    s.decoder = literalDecoder;
  });

export function Literal_parse(value: unknown): Internal {
  if (value === null) {
    return nullLiteral();
  } else {
    const tag = typeOf(value);
    if (tag === undefinedTag) {
      return unit();
    } else if (tag === numberTag && Number.isNaN(value as number)) {
      return nan();
    } else if (tag === objectTag) {
      const s = baseSchema(instanceTag, true);
      s.class = (value as Record<string, unknown>)["constructor"];
      s.const = value;
      s.decoder = literalDecoder;
      return s;
    } else {
      const s = baseSchema(tag, true);
      s.const = value;
      s.decoder = literalDecoder;
      return s;
    }
  }
}
// =============================================================================
// Section: Sury.res lines 2256-2708
// parse / parseDynamic / isAsyncInternal / compileDecoder / getOutputSchema /
// reverse / getDecoder / nestedLoc / itemCode / neverBuilderFn / never_ /
// nestedOptionParser / instanceDecoder / instance / typeCheckCond
//
// TODO(integration): expects from earlier sections:
//   - `B` (Builder.B): B_Val_scope, B_next, B_refine, B_merge, B_markOutput,
//     B_operationArg, B_operationArgVar, B_unsupportedDecode,
//     B_embedInvalidInput, B_inlineConst, failInvalidType
//   - `Builder` const: Builder.make (identity cast), Builder.noopOperation
//   - `setHas` (Sury.res ~2137)
//   - cond atoms: `typeofCond`, `nanCond`, `isArrayCond`, `objectTagCond`,
//     `instanceofCond` (Sury.res ~1911-1915)
// Note on getDecoder2/getDecoder3: they are `@val external` self-references to
// `getDecoder` (it reads `arguments`). Call sites elsewhere become plain
// `getDecoder(s1, s2, flag?)` / `getDecoder(s1, s2, s3, flag?)` calls — no
// separate bindings are emitted here.
// =============================================================================

export function parse(input: Val): Val {
  let valRef: Val = input;
  let appliedEncoderRef: Encoder | undefined = undefined;
  let loopCount = 0;
  while (!valRef.io || (valRef.e.to as unknown as boolean)) {
    const appliedEncoder: Encoder | undefined = appliedEncoderRef;
    appliedEncoderRef = undefined;
    const loopInput = valRef;

    loopCount = loopCount + 1;

    // Console.log(loopInput)
    if (loopCount > 50) {
      const error = new Error("Loop count exceeded 100");
      throw error;
    }

    if (loopInput.e["$defs"] as unknown as boolean) {
      if (loopInput.g.d as unknown as boolean) {
        Object.assign(loopInput.g.d!, loopInput.e["$defs"]!);
      } else {
        loopInput.g.d = loopInput.e["$defs"];
      }
    }

    if (
      Flag.unsafeHas(
        loopInput.f,
        ValFlag.async,
      ) /* FIXME: why was it needed? && step.contents !== #convert */
    ) {
      const operationInputVar = loopInput.v();

      const operationInput = B_Val_scope(loopInput);
      const operationOutput = parse(operationInput);
      const operationCode = B_merge(operationOutput);
      if (operationInput.i !== operationOutput.i || operationCode !== "") {
        valRef = B_next(
          loopInput,
          `${operationInputVar}.then(${operationInputVar}=>{${operationCode}return ${operationOutput.i}})`,
          operationOutput.s,
          operationOutput.e,
        );
      } else {
        valRef = B_refine(loopInput, operationOutput.s, undefined, operationOutput.e);
      }
      valRef.f = Flag.with(valRef.f, ValFlag.async);
      valRef.io = true;
    } else if (loopInput.io) {
      // It's guaranteed that to is not None, because it's checked in the while condition
      const to = loopInput.e.to!;
      if (loopInput.e.parser !== undefined) {
        valRef = loopInput.e.parser(loopInput);
      } else {
        valRef = B_refine(valRef, undefined, undefined, to);
      }
    } else {
      const maybeEncoder = loopInput.s.encoder;
      if (
        (maybeEncoder as unknown as boolean) &&
        maybeEncoder !== appliedEncoder &&
        loopInput.s !== loopInput.e &&
        loopInput.e.type !== unknownTag
      ) {
        valRef = maybeEncoder!(loopInput, loopInput.e);
      }

      // If encoder didn't change the value, we can decode it,
      // otherwise let's start the loop from the beginning
      if (loopInput !== valRef) {
        appliedEncoderRef = maybeEncoder!;
      } else {
        valRef = loopInput.e.decoder(loopInput);

        // Primitive decoder (no internal transforms): apply refiners here.
        // Advanced decoders set isOutput themselves and own refiner application.
        if (!valRef.io) {
          valRef = B_markOutput(valRef, valRef);
        }
      }
    }
  }

  return valRef;
}
export function parseDynamic(input: Val): Val {
  try {
    return parse(input);
  } catch (exn) {
    const error = InternalError.getOrRethrow(exn);
    (error as unknown as Record<string, unknown>)["path"] =
      // For the case parent must always be present
      pathConcat(
        input.p !== undefined ? input.p.path : pathEmpty,
        pathConcat(pathConcat(input.path, pathDynamic), error.path),
      );

    throw error;
  }
}

export function isAsyncInternal(
  schema: Internal,
  defs: Record<string, Internal> | undefined
): boolean {
  try {
    const input = B_operationArg(unknown, schema, Flag.async, defs);
    const output = parse(input);
    const isAsync = Flag.has(output.f, ValFlag.async);
    schema.isAsync = isAsync;
    return isAsync;
  } catch (exn) {
    InternalError.getOrRethrow(exn);
    return false;
  }
}
export function compileDecoder(
  schema: Internal,
  expected: Internal,
  flag: Flag,
  defs: Record<string, Internal> | undefined
): (input: unknown) => unknown {
  const input = B_operationArg(isLiteral(schema) ? unknown : schema, expected, flag, defs);

  const output = parse(input);
  const code = B_merge(output);

  const isAsync = Flag.has(output.f, ValFlag.async);
  expected.isAsync = isAsync;
  const hasTransform = output.t === true;
  expected.hasTransform = hasTransform;

  if (
    code === "" &&
    (output === input || output.i === input.i) &&
    !Flag.unsafeHas(flag, Flag.async)
  ) {
    return noopOperation;
  } else {
    let inlinedOutput = output.i;
    if (Flag.unsafeHas(flag, Flag.async) && !isAsync && !(defs as unknown as boolean)) {
      inlinedOutput = `Promise.resolve(${inlinedOutput})`;
    }

    const inlinedFunction = `${operationArgVar}=>{${code}return ${inlinedOutput}}`;

    // Console.log(inlinedFunction)

    const fn = new Function("e", "s", `return ${inlinedFunction}`)(input.g.e, s);
    (fn as unknown as Record<string, unknown>)["embedded"] = input.g.e;
    return fn;
  }
}
export function getOutputSchema(schema: Internal): Internal {
  if (schema.to !== undefined) {
    return getOutputSchema(schema.to);
  } else {
    return schema;
  }
}
// FIXME: Define it as a schema property
export function reverse(schema: Internal): Internal {
  if (reversedKey in (schema as unknown as Record<string, unknown>)) {
    return (schema as unknown as Record<string, unknown>)[reversedKey] as Internal;
  } else {
    let reversedHead: Internal | undefined = undefined;
    let current: Internal | undefined = schema;

    while (current as unknown as boolean) {
      const mut = copySchema(current!);
      const next = mut.to;
      if (reversedHead === undefined) {
        delete mut.to;
      } else {
        mut.to = reversedHead;
      }
      const parser = mut.parser;
      if (mut.serializer !== undefined) {
        mut.parser = mut.serializer;
      } else {
        delete mut.parser;
      }
      if (parser !== undefined) {
        mut.serializer = parser;
      } else {
        delete mut.serializer;
      }
      // Swap inputRefiner and refiner
      const refiner = mut.refiner;
      if (mut.inputRefiner !== undefined) {
        mut.refiner = mut.inputRefiner;
      } else {
        delete mut.refiner;
      }
      if (refiner !== undefined) {
        mut.inputRefiner = refiner;
      } else {
        delete mut.inputRefiner;
      }
      const fromDefault = mut.fromDefault;
      if (mut.default !== undefined) {
        mut.fromDefault = mut.default;
      } else {
        delete mut.fromDefault;
      }
      if (fromDefault !== undefined) {
        mut.default = fromDefault;
      } else {
        delete mut.default;
      }
      if (mut.items !== undefined) {
        mut.items = mut.items.map(reverse);
      }
      if (mut.properties !== undefined) {
        const properties = mut.properties;
        const newProperties: Record<string, Internal> = {};
        const keys = Object.keys(properties);
        for (let idx = 0; idx <= keys.length - 1; idx++) {
          const key = keys[idx]!;
          newProperties[key] = reverse(properties[key]!);
        }
        mut.properties = newProperties;
      }
      // Skip tuple
      if (typeOf(mut.additionalItems) === objectTag) {
        mut.additionalItems = reverse(mut.additionalItems as unknown as Internal);
      }
      if (mut.anyOf !== undefined) {
        const anyOf = mut.anyOf;
        const has: Record<string, boolean> = {};
        const newAnyOf: Internal[] = [];
        for (let idx = 0; idx <= anyOf.length - 1; idx++) {
          const s = anyOf[idx]!;
          const reversed = reverse(s);
          newAnyOf.push(reversed);
          setHas(has, reversed.type);
        }
        mut.has = has;
        mut.anyOf = newAnyOf;
      }
      if (mut["$defs"] !== undefined) {
        const defs = mut["$defs"];
        const reversedDefs: Record<string, Internal> = {};
        for (let idx = 0; idx <= Object.keys(defs).length - 1; idx++) {
          const key = Object.keys(defs)[idx]!;
          reversedDefs[key] = reverse(defs[key]!);
        }
        mut["$defs"] = reversedDefs;
      }
      reversedHead = mut;
      current = next;
    }

    // Use defineProperty even though it's slower
    // but it improves logging experience a lot
    // for some reason Wallaby still shows the property
    const r = reversedHead!;
    valueOptions[valKey] = r;
    Object.defineProperty(schema, reversedKey, valueOptions as PropertyDescriptor);
    valueOptions[valKey] = schema;
    Object.defineProperty(r, reversedKey, valueOptions as PropertyDescriptor);
    return r;
  }
}

// PORT-NOTE: The ReScript signature `(~s1 as _, ~flag as _=?)` discards its
// labeled args and the body reads `arguments` directly — so this is a plain
// (non-arrow, to keep `arguments`) function with dummy params for arity.
// getDecoder2/getDecoder3 call sites become getDecoder(s1, s2[, s3][, flag]).
export function getDecoder(
  _s1?: unknown,
  _s2?: unknown,
  _s3?: unknown,
  _flag?: unknown
): (from: unknown) => unknown {
  const args = arguments as unknown as unknown[];
  let idx = 0;
  let flag: Flag | undefined = undefined;
  let keyRef = "";
  let maxSeq = 0;
  let cacheTarget: Internal | undefined = undefined;

  while (flag === undefined) {
    const arg = args[idx];
    if (!(arg as unknown as boolean)) {
      const f = globalConfig.f;
      flag = f;
      keyRef = keyRef + "-" + f;
    } else if (typeOf(arg) === numberTag) {
      const f = Flag.with(arg as unknown as Flag, globalConfig.f);
      flag = f;
      keyRef = keyRef + "-" + f;
    } else {
      const schema: Internal = arg as unknown as Internal;
      const seq: number = schema.seq as unknown as number;
      if (seq > maxSeq) {
        maxSeq = seq;
        cacheTarget = schema;
      }
      keyRef = keyRef + (seq as unknown as string) + "-";
      idx = idx + 1;
    }
  }

  if (cacheTarget === undefined) {
    return InternalError.panic("No schema provided for decoder.");
  } else {
    const key = keyRef;
    if (key in (cacheTarget as unknown as Record<string, unknown>)) {
      return (cacheTarget as unknown as Record<string, unknown>)[key] as (
        from: unknown
      ) => unknown;
    } else {
      let schema: Internal = args[idx - 1] as unknown as Internal;
      for (let i = idx - 2; i >= 0; i--) {
        const to = schema;
        schema = updateOutput(args[i] as unknown as Internal, (mut) => {
          mut.to = to;
        });
      }
      const f = compileDecoder(
        schema,
        schema,
        flag!,
        0 as unknown as Record<string, Internal> | undefined
      );
      // Reusing the same object makes it a little bit faster
      valueOptions[valKey] = f;
      // Use defineProperty, so the cache keys are not enumerable
      Object.defineProperty(cacheTarget, key, valueOptions as PropertyDescriptor);
      return f as (from: unknown) => unknown;
    }
  }
}

export const nestedLoc = "BS_PRIVATE_NESTED_SOME_NONE";

// @unboxed — runtime value is the string or the array itself.
export type ItemCode = string | string[];

export function neverBuilderFn(input: Val): Val {
  const output = B_refine(input, undefined, undefined, never_());
  output.cp = B_embedInvalidInput(input) + ";";
  return output;
}
export function never_(): Internal {
  return cached(neverTag as string, neverTag, (s) => {
    s.decoder = neverBuilderFn;
  });
}

export const nestedOptionParser: Builder = ((input: Val) => {
  const nextSchema = input.e.to!;
  return B_next(
    input,
    `{${nestedLoc}:${getOutputSchema(input.e).properties![nestedLoc]!.const as unknown as string}}`,
    nextSchema,
    nextSchema
  );
});

export const instanceDecoder: Builder = ((input: Val) => {
  const inputTagFlag = TagFlag.get(input.s.type);
  if (Flag.unsafeHas(inputTagFlag, TagFlag.unknown)) {
    return B_refine(input, input.e, [
      {
        c: instanceofCond(input, input.e.class),
        f: failInvalidType,
      },
    ]);
  } else if (Flag.unsafeHas(inputTagFlag, TagFlag.instance) && input.s.class === input.e.class) {
    return input;
  } else {
    return B_unsupportedDecode(input, input.s, input.e);
  }
});

export function instance(class_: unknown): Internal {
  const mut = baseSchema(instanceTag, true);
  mut.class = class_;
  mut.decoder = instanceDecoder;
  return mut;
}

// Type-narrow condition for a union variant, built from the shared atoms with no
// per-type factory reference — so unused type decoders tree-shake.
export function typeCheckCond(input: Val, schema: Internal, inputVar: string): string {
  const tagFlag = TagFlag.get(schema.type);
  if (Flag.unsafeHas(tagFlag, TagFlag.object)) {
    return `${objectTagCond(inputVar)}&&!${isArrayCond(inputVar)}`;
  } else if (Flag.unsafeHas(tagFlag, TagFlag.array)) {
    return isArrayCond(inputVar);
  } else if (Flag.unsafeHas(tagFlag, TagFlag.instance)) {
    return instanceofCond(input, schema.class)(inputVar);
  } else if (Flag.unsafeHas(tagFlag, TagFlag.number)) {
    const typeofCheck = typeofCond(numberTag)(inputVar);
    if (Flag.unsafeHas(input.g.o, Flag.disableNanNumberValidation)) {
      return typeofCheck;
    } else {
      return `${typeofCheck}&&!${nanCond(inputVar)}`;
    }
  } else if (Flag.unsafeHas(tagFlag, TagFlag.nan)) {
    return nanCond(inputVar);
  } else if (Flag.unsafeHas(tagFlag, Flag.with(TagFlag.undefined, TagFlag.null))) {
    // null/undefined reuse literalDecoder's inline-const form (=== null / void 0)
    return `${inputVar}===${B_inlineConst(input, schema)}`;
  } else if (
    Flag.unsafeHas(
      tagFlag,
      Flag.with(Flag.with(Flag.with(TagFlag.string, TagFlag.boolean), TagFlag.bigint), TagFlag.symbol)
    )
  ) {
    // literals reuse this typeof check; their per-const check stays in the case body
    return typeofCond(schema.type)(inputVar);
  } else {
    // Unreachable: catch-all tags use the `unknown` narrow, never this path.
    return "";
  }
}
// =============================================================================
// Section 05: object/tuple/array/dict/union decoders & encoders
// Ported from Sury.res lines 2709-4186 (`let rec makeObjectVal` … `valGet`,
// everything before `recursiveDecoder`).
//
// TODO(integration): expects the following externals from other sections:
//  - `B` (the Builder const object: _notVar, _var, _notVarAtParent,
//    B_refine, B_next, B_nextConst, B_merge, B_mergeWithPathPrepend,
//    B_dynamicScope, B_varWithoutAllocation, B_inlineLocation, B_inlineConst,
//    B_hoistChildChecks, B_hoistDecl, B_failWithArg, B_pushCheck,
//    B_isHoistable, B_embed, B_asyncVal, B_unsupportedDecode,
//    B_makeInvalidInputDetails, B_throw, failInvalidType, B_markOutput,
//    B_Val_scope, B_Val_var, B_Val_addKey, B_Val_Object_add) — Builder section
//  - `parse`, `parseDynamic`, `getOutputSchema` — parse-loop section (~2256)
//  - `typeCheckCond`, `isArrayCond`, `objectTagCond` — primitives section
//  - `never_`, `nestedOptionParser`, `nestedLoc` — section just before this one (~2615)
//  - `jsonName`, `setHas`, `unit` — primitives/config section (~2137-2211)
//  - `Literal` (Literal_parse) — literal section (~2229)
//  - from the prelude (core.ts): Val, Check, Internal, Builder, Encoder,
//    Flag, ValFlag, TagFlag, baseSchema, cached, unknown, updateOutput,
//    copySchema, isLiteral, isOptional, InternalError, reversedKey,
//    immutableEmptyArray, immutableEmptyObject, pathFromInlinedLocation,
//    pathConcat, arrayTag/objectTag/… tag consts
// =============================================================================

// PORT-NOTE: `B_Val_Object_t` is `{...val}` — the same runtime shape as `val`,
// so this port uses the prelude's `Val` type for object vals.

export function makeObjectVal(prev: Val, schema: Internal): Val {
  return {
    prev,
    v: _notVar,
    i: "",
    f: ValFlag.none,
    s: (schema.type === arrayTag
      ? {
          type: arrayTag,
          items: [],
          additionalItems: "strict",
          decoder: arrayDecoder,
        }
      : {
          type: objectTag,
          required: [],
          properties: {},
          additionalItems: "strict",
          decoder: objectDecoder,
        }) as Internal,
    e: prev.e,
    d: {},
    t: true,
    cp: "",
    hd: "",
    path: prev.path,
    g: prev.g,
  };
}
export function completeObjectVal(objectVal: Val): Val {
  const isArray = objectVal.s.type === arrayTag;
  let inline = "";
  let promiseAllContent = "";
  let optionalSettingCode: ((objectVar: string) => string) | undefined = undefined;

  const keys = Object.keys(objectVal.d!);

  for (let idx = 0; idx < keys.length; idx++) {
    const key = keys[idx]!;
    const val = objectVal.d![key]!;
    if (Flag.unsafeHas(val.f, ValFlag.async)) {
      promiseAllContent = promiseAllContent + val.i + ",";
    }
    if (val.o) {
      const existingFn = optionalSettingCode as ((objectVar: string) => string) | undefined;
      optionalSettingCode = (objectVar: string) => {
        return (
          (existingFn === undefined ? "" : existingFn(objectVar)) +
          `if(${val.v()}!==void 0){${objectVar}[${B_inlineLocation(objectVal.g, key)}]=${val.i}}`
        );
      };
    } else {
      inline =
        inline +
        (isArray ? `${val.i}` : `${B_inlineLocation(objectVal.g, key)}:${val.i}`) +
        ",";
    }
  }

  objectVal.i = isArray ? "[" + inline + "]" : "{" + inline + "}";

  // FIXME: Test whether it's needed
  // objectVal.additionalItems = Some(Strict)
  const valWithRequired = objectVal;

  if (promiseAllContent) {
    // FIXME: Test how it works with optional and fix it
    const operationInput = B_Val_scope(valWithRequired);
    operationInput.io = true;
    const operationOutput = parse(operationInput);
    const operationCode = B_merge(operationOutput);

    if (operationCode === "" && promiseAllContent === `${operationOutput.i},`) {
      valWithRequired.i = operationOutput.i;
    } else {
      valWithRequired.i = `Promise.all([${promiseAllContent}]).then(([${promiseAllContent}])=>{${operationCode}return ${operationOutput.i}})`;
    }
    valWithRequired.f = Flag.with(valWithRequired.f, ValFlag.async);
    valWithRequired.s = operationOutput.s;
    valWithRequired.e = operationOutput.e;
    valWithRequired.io = true;
    return valWithRequired;
  } else {
    if (optionalSettingCode === undefined) {
      return valWithRequired;
    } else {
      const code = optionalSettingCode(valWithRequired.v());
      const output = B_refine(valWithRequired);
      output.cp = output.cp + code;
      return output;
    }
  }
}
export function array(item: Internal): Internal {
  const itemInternal = item;
  const mut = baseSchema(
    arrayTag,
    (itemInternal as unknown as Record<string, unknown>)[reversedKey] ===
      (itemInternal as unknown),
  );
  mut.additionalItems = itemInternal;
  mut.items = immutableEmptyArray as Internal[];
  mut.decoder = arrayDecoder;
  return mut;
}
export function arrayDecoder(unknownInput: Val): Val {
  const isUnion = unknownInput.u!;
  const expectedSchema = unknownInput.e;
  const unknownInputTagFlag = TagFlag.get(unknownInput.s.type);
  const expectedItems = expectedSchema.items!;
  const expectedLength = expectedItems.length;

  let input: Val;
  if (Flag.unsafeHas(unknownInputTagFlag, Flag.with(TagFlag.unknown, TagFlag.array))) {
    const isArrayInput = Flag.unsafeHas(unknownInputTagFlag, TagFlag.array);
    let schema: Internal;
    if (!isArrayInput) {
      schema = array(unknown);
    } else {
      schema = unknownInput.s;
    }
    const checks: Check[] = [];
    if (!isArrayInput) {
      checks.push({
        c: isArrayCond,
        f: failInvalidType,
      });
    }

    const schemaAdditionalItems = schema.additionalItems;
    const isExactSize =
      schemaAdditionalItems !== undefined && typeof schemaAdditionalItems !== "string"
        ? false
        : schema.items!.length === expectedLength;

    if (!isExactSize) {
      const expectedAdditionalItems = expectedSchema.additionalItems;
      if (expectedAdditionalItems === "strict") {
        checks.push({
          c: (inputVar) => `${inputVar}.length===${expectedLength}`,
          f: failInvalidType,
        });
      } else if (expectedAdditionalItems === "strip") {
        checks.push({
          c: (inputVar) => `${inputVar}.length>=${expectedLength}`,
          f: failInvalidType,
        });
      }
    }

    // Apply refine also when there are no checks,
    // so literals for union cases don't mutate input
    // FIXME: This should be removed and validation be attached to output
    if (checks.length > 0) {
      input = B_refine(unknownInput, schema, checks);
    } else {
      input = B_refine(unknownInput, schema);
    }
  } else {
    input = B_unsupportedDecode(unknownInput, unknownInput.s, expectedSchema);
  }

  let output: Val;
  const expectedAdditionalItems = expectedSchema.additionalItems;
  if (expectedAdditionalItems !== undefined && typeof expectedAdditionalItems !== "string") {
    const itemSchema = expectedAdditionalItems;
    if (itemSchema === unknown) {
      output = input;
    } else {
      const inputVar = B_Val_var(input);
      const iteratorVar = B_varWithoutAllocation(input.g);

      const itemInput = B_dynamicScope(input, iteratorVar);
      const itemOutput = parseDynamic(itemInput);
      const hasTransform = itemOutput.t!;
      const output2 = hasTransform
        ? B_next(input, `new Array(${inputVar}.length)`, expectedSchema) // FIXME: schema here should be input.expected output
        : B_refine(input, expectedSchema);

      const itemCode = B_mergeWithPathPrepend(
        itemOutput,
        input,
        iteratorVar,
        hasTransform ? () => B_Val_addKey(output2, iteratorVar, itemOutput) : undefined,
      );

      if (hasTransform || itemCode !== "") {
        output2.cp =
          output2.cp +
          `for(let ${iteratorVar}=${expectedLength};${iteratorVar}<${inputVar}.length;++${iteratorVar}){${itemCode}}`;
      }

      if (Flag.unsafeHas(itemOutput.f, ValFlag.async)) {
        output = B_asyncVal(output2, `Promise.all(${output2.i})`);
      } else {
        output = output2;
      }
    }
  } else {
    const objectVal = makeObjectVal(input, expectedSchema);
    let shouldRecreateInput: boolean;
    {
      const ai = expectedSchema.additionalItems;
      // Since we have a check validating the exact properties existence
      if (ai === "strict") {
        shouldRecreateInput = false;
      } else if (ai === "strip") {
        const inputAi = input.s.additionalItems;
        shouldRecreateInput =
          inputAi !== undefined && typeof inputAi !== "string"
            ? true
            : input.s.items!.length !== expectedLength;
      } else {
        shouldRecreateInput = true;
      }
    }

    for (let idx = 0; idx < expectedLength; idx++) {
      const schema = expectedItems[idx]!;
      const key = String(idx);
      const itemInput = valGet(input, key);
      itemInput.e = schema;
      itemInput.io = false;
      itemInput.u = isUnion; // We want to controll validation on the decoder side
      const itemOutput = parse(itemInput);

      if (isUnion && isLiteral(schema)) {
        B_hoistChildChecks(input, itemOutput, key);
      }

      B_Val_Object_add(objectVal, key, itemOutput);
      if (!shouldRecreateInput) {
        shouldRecreateInput = itemOutput.t!;
      }
    }

    // After input.schema was used, set it to selfSchema
    // so it has a more accurate name in error messages
    if (shouldRecreateInput) {
      output = completeObjectVal(objectVal);
    } else {
      const o = B_refine(input);
      o.cp = objectVal.cp;
      o.d = objectVal.d;
      output = o;
    }
  }
  return B_markOutput(output, input);
}
export function objectDecoder(unknownInput: Val): Val {
  const isUnion = unknownInput.u!;
  const expectedSchema = unknownInput.e;

  const unknownInputTagFlag = TagFlag.get(unknownInput.s.type);

  let input: Val;
  if (Flag.unsafeHas(unknownInputTagFlag, Flag.with(TagFlag.unknown, TagFlag.object))) {
    const isObjectInput = Flag.unsafeHas(unknownInputTagFlag, TagFlag.object);
    let schema: Internal;
    if (!isObjectInput) {
      // TODO: Use dictFactory here
      const mut = baseSchema(objectTag, false);
      mut.properties = immutableEmptyObject as Record<string, Internal>;
      mut.additionalItems = unknown;
      schema = mut;
    } else {
      schema = unknownInput.s;
    }
    const checks: Check[] = [];
    if (!isObjectInput) {
      checks.push({
        c: objectTagCond,
        f: failInvalidType,
      });
      if (expectedSchema.additionalItems !== "strip") {
        // For strip case we recreate the value
        // For other cases we might optimize it,
        // this is why the check is a must have
        checks.push({
          c: (inputVar) => `!${isArrayCond(inputVar)}`,
          f: failInvalidType,
        });
      }
    }

    // Apply refine also when there are no checks,
    // so literals for union cases don't mutate input
    if (checks.length > 0) {
      input = B_refine(unknownInput, schema, checks);
    } else {
      input = B_refine(unknownInput, schema);
    }
  } else {
    input = B_unsupportedDecode(unknownInput, unknownInput.s, expectedSchema);
  }

  // The target's value schema when it's a dict (additionalProperties), else None
  // for a fixed-property object target.
  const expectedAdditionalItems = expectedSchema.additionalItems;
  const dictItem: Internal | undefined =
    expectedAdditionalItems !== undefined && typeof expectedAdditionalItems !== "string"
      ? expectedAdditionalItems
      : undefined;
  // Only a dict source can be iterated dynamically (`for..in`). A fixed-property
  // object source coerced into a dict target reuses the static object-literal
  // construction below, driven by the source's known keys.
  const inputAdditionalItems = input.s.additionalItems;
  const sourceIsDict =
    inputAdditionalItems !== undefined && typeof inputAdditionalItems !== "string";

  let output: Val;
  // dict<unknown> target: any object/dict is already a valid value, pass through.
  if (dictItem !== undefined && dictItem === unknown) {
    output = input;
  } else if (dictItem !== undefined && sourceIsDict) {
    const inputVar = input.v();
    const keyVar = B_varWithoutAllocation(input.g);
    const itemInput = B_dynamicScope(input, keyVar);
    const itemOutput = parseDynamic(itemInput);

    const hasTransform = itemOutput.t!;
    const output2 = hasTransform
      ? // FIXME: schema should be expectedSchema output
        B_next(input, "{}", expectedSchema)
      : B_refine(input, expectedSchema);

    const itemCode = B_mergeWithPathPrepend(
      itemOutput,
      input,
      keyVar,
      hasTransform ? () => B_Val_addKey(output2, keyVar, itemOutput) : undefined,
    );

    if (hasTransform || itemCode !== "") {
      output2.cp = output2.cp + `for(let ${keyVar} in ${inputVar}){${itemCode}}`;
    }

    if (Flag.unsafeHas(itemOutput.f, ValFlag.async)) {
      const resolveVar = B_varWithoutAllocation(output2.g);
      const rejectVar = B_varWithoutAllocation(output2.g);
      const asyncParseResultVar = B_varWithoutAllocation(output2.g);
      const counterVar = B_varWithoutAllocation(output2.g);
      const outputVar = B_Val_var(output2);
      output = B_asyncVal(
        output2,
        `new Promise((${resolveVar},${rejectVar})=>{let ${counterVar}=Object.keys(${outputVar}).length;for(let ${keyVar} in ${outputVar}){${outputVar}[${keyVar}].then(${asyncParseResultVar}=>{${outputVar}[${keyVar}]=${asyncParseResultVar};if(${counterVar}--===1){${resolveVar}(${outputVar})}},${rejectVar})}})`,
      );
    } else {
      output = output2;
    }
  } else if (dictItem !== undefined) {
    const itemSchema = dictItem;
    // Encode a fixed-property object into a dict: build an object literal from
    // the SOURCE's keys, coercing every value to the dict's value schema.
    // `completeObjectVal` drops a field that is still optional after coercion.
    // (A dict source took the dynamic branch above, so the source is an object.)
    const objectVal = makeObjectVal(input, expectedSchema);
    const keys = Object.keys(input.s.properties!);
    for (let idx = 0; idx < keys.length; idx++) {
      const key = keys[idx]!;
      const itemInput = valGet(input, key);
      itemInput.e = itemSchema;
      itemInput.io = false;
      itemInput.u = isUnion;
      B_Val_Object_add(objectVal, key, parse(itemInput));
    }
    output = completeObjectVal(objectVal);
  } else {
    // Build a fixed-property object target (from a dict or object source).
    const properties = expectedSchema.properties!;
    const keys = Object.keys(properties);
    const keysCount = keys.length;

    const objectVal = makeObjectVal(input, expectedSchema);
    let shouldRecreateInput: boolean;
    {
      const ai = expectedSchema.additionalItems;
      // Since we have a check validating the exact properties existence
      if (ai === "strict") {
        shouldRecreateInput = false;
      } else if (ai === "strip") {
        shouldRecreateInput =
          sourceIsDict || Object.keys(input.s.properties!).length !== keysCount;
      } else {
        shouldRecreateInput = true;
      }
    }

    // FIXME: hack — detect "JSON-sourced object" via additionalItems=json
    // (set by jsonEncoderFn) and patch the field read inline to coalesce
    // `??null`. The proper fix is for the JSON pipeline to treat missing
    // object keys as the option's empty sentinel, instead of leaving
    // objectDecoder to sniff the source and rewrite codegen by hand:
    //   - jsonEncoderFn rewrites the option arm from `v===void 0` to
    //     `v===null` because JSON has no undefined,
    //   - but `i[key]` for a missing key returns undefined, so the
    //     rewritten arm rejects `{}` for `{foo: option<...>}`.
    // Detection is fragile (string-compares the schema name) and only
    // covers the union-with-undefined shape; fold this into a shared
    // JSON option representation post-release.
    const isJsonParent =
      inputAdditionalItems !== undefined && typeof inputAdditionalItems !== "string"
        ? inputAdditionalItems.name === jsonName
        : false;

    for (let idx = 0; idx < keysCount; idx++) {
      const key = keys[idx]!;
      const schema = properties[key]!;

      const itemInput = valGet(input, key);
      itemInput.e = schema;
      itemInput.io = false;
      itemInput.u = isUnion; // We want to controll validation on the decoder side
      if (isJsonParent && schema.type === unionTag && schema.has![undefinedTag]) {
        itemInput.i = `(${itemInput.i}??null)`;
      }

      const itemOutput = parse(itemInput);

      if (isUnion && isLiteral(schema)) {
        B_hoistChildChecks(input, itemOutput, key);
      }

      B_Val_Object_add(objectVal, key, itemOutput);
      if (!shouldRecreateInput) {
        shouldRecreateInput = itemOutput.t!;
      }
    }

    if (
      expectedSchema.additionalItems === "strict" &&
      inputAdditionalItems !== undefined &&
      typeof inputAdditionalItems !== "string"
    ) {
      const keyVar = B_varWithoutAllocation(objectVal.g);
      B_hoistDecl(input, keyVar);
      objectVal.cp = objectVal.cp + `for(${keyVar} in ${input.v()}){if(`;
      if (keys.length === 0) {
        objectVal.cp = objectVal.cp + "true";
      } else {
        for (let idx = 0; idx < keys.length; idx++) {
          const key = keys[idx]!;
          if (idx !== 0) {
            objectVal.cp = objectVal.cp + "&&";
          }
          objectVal.cp = objectVal.cp + `${keyVar}!==${B_inlineLocation(input.g, key)}`;
        }
      }
      objectVal.cp =
        objectVal.cp +
        `){${B_failWithArg(
          input,
          (exccessFieldName: string) =>
            ({
              code: "unrecognized_keys",
              path: objectVal.path,
              reason: `Unrecognized key "${exccessFieldName}"`,
              keys: [exccessFieldName],
            }) as ErrorDetails,
          keyVar,
        )}}}`;
    }

    // After input.schema was used, set it to selfSchema
    // so it has a more accurate name in error messages
    if (shouldRecreateInput) {
      output = completeObjectVal(objectVal);
    } else {
      const o = B_refine(input);
      o.cp = objectVal.cp;
      o.d = objectVal.d;
      output = o;
    }
  }
  return B_markOutput(output, input);
}

export function dictFactory(item: Internal): Internal {
  const mut = baseSchema(
    objectTag,
    (item as unknown as Record<string, unknown>)[reversedKey] === (item as unknown),
  );
  mut.properties = immutableEmptyObject as Record<string, Internal>;
  mut.additionalItems = item;
  mut.decoder = objectDecoder;
  return mut;
}

export function unionToKey(schema: Internal): string {
  return Flag.unsafeHas(TagFlag.get(schema.type), TagFlag.instance)
    ? (schema.class as { name: string })["name"]
    : schema.type;
}

export function unionIsPriority(tagFlag: number, byKey: Record<string, unknown[]>): boolean {
  return (
    (Flag.unsafeHas(tagFlag, Flag.with(TagFlag.array, TagFlag.instance)) &&
      objectTag in byKey) ||
    (Flag.unsafeHas(tagFlag, TagFlag.nan) && numberTag in byKey)
  );
}

// Whether decoding a value already known to be of the schema type
// is a noop — no transformation anywhere in the schema tree.
// Recursive refs are conservatively treated as transforming
export function unionIsSelfDecodeNoop(schema: Internal): boolean {
  const additionalItems = schema.additionalItems;
  return (
    schema.to === undefined &&
    schema.parser === undefined &&
    !Flag.unsafeHas(TagFlag.get(schema.type), TagFlag.ref) &&
    (schema.anyOf !== undefined ? schema.anyOf.every(unionIsSelfDecodeNoop) : true) &&
    (schema.items !== undefined ? schema.items.every(unionIsSelfDecodeNoop) : true) &&
    (schema.properties !== undefined
      ? Object.values(schema.properties).every(unionIsSelfDecodeNoop)
      : true) &&
    (additionalItems !== undefined && typeof additionalItems !== "string"
      ? unionIsSelfDecodeNoop(additionalItems)
      : true)
  );
}

export function unionIsWiderSchema(schemaAnyOf: Internal[], inputAnyOf: Internal[]): boolean {
  return inputAnyOf.every((inputSchema, idx) => {
    const schema = schemaAnyOf[idx];
    if (schema !== undefined) {
      return (
        !Flag.unsafeHas(
          TagFlag.get(inputSchema.type),
          Flag.with(
            Flag.with(
              Flag.with(Flag.with(TagFlag.array, TagFlag.instance), TagFlag.ref),
              TagFlag.union,
            ),
            TagFlag.object,
          ),
        ) &&
        inputSchema.type === schema.type &&
        inputSchema.const === schema.const &&
        inputSchema.to === undefined
      );
    } else {
      return false;
    }
  });
}

// The union's own `.to` chain which is applied per case during decoding.
// None when the union has a custom parser owning the `.to` conversion
export function unionGetToPerCase(schema: Internal): Internal | undefined {
  return schema.parser === undefined && schema.to !== undefined ? schema.to : undefined;
}

// Whether a union-typed input can be decoded by dispatching
// over its variants with `.to(target)` appended to each
export function unionCanDispatchPerVariant(inputAnyOf: Internal[], target: Internal): boolean {
  return (
    // S.json and recursive targets keep their dedicated union-input handling
    !Flag.unsafeHas(TagFlag.get(getOutputSchema(target).type), TagFlag.ref) &&
    !(
      target.type === unionTag &&
      target.anyOf!.some((v) => Flag.unsafeHas(TagFlag.get(v.type), TagFlag.ref))
    ) &&
    // Variants with transformations or recursive refs (option machinery,
    // transformed unions) aren't supported per-variant yet
    !inputAnyOf.some(
      (v) =>
        v.to !== undefined ||
        v.parser !== undefined ||
        Flag.unsafeHas(TagFlag.get(v.type), TagFlag.ref),
    )
  );
}

// Re-drives the source union with `.to(target)` appended, so its decoder
// dispatches per variant and each variant converts to the target
// independently (the documented per-source-variant algorithm)
export function unionPerVariantVal(input: Val, target: Internal): Val {
  return B_refine(
    input,
    unknown,
    undefined,
    updateOutput<Internal>(input.s, (mut) => {
      mut.to = target;
    }),
  );
}

// Applied by the parse loop when a union-typed val
// meets a different expected schema
export function unionEncoder(input: Val, target: Internal): Val {
  const inputAnyOf = input.s.anyOf!;
  if (
    target.type === unionTag &&
    unionGetToPerCase(target) === undefined &&
    unionIsWiderSchema(target.anyOf!, inputAnyOf)
  ) {
    // The target union decoder passes a narrower union input through as-is
    return input;
  } else if (unionCanDispatchPerVariant(inputAnyOf, target)) {
    return unionPerVariantVal(input, target);
  } else {
    return input;
  }
}

export function unionDecoder(input: Val): Val {
  const selfSchema = input.e;
  let schemas = selfSchema.anyOf!;
  const initialInputTagFlag = TagFlag.get(input.s.type);

  const toPerCase = unionGetToPerCase(selfSchema);

  if (
    // The input val is already of the union type (trusted self-decode).
    // Only allowed when no variant transforms the value
    (input.s === selfSchema &&
      toPerCase === undefined &&
      schemas.every(unionIsSelfDecodeNoop)) ||
    (Flag.unsafeHas(initialInputTagFlag, TagFlag.union) &&
      unionIsWiderSchema(schemas, input.s.anyOf!) &&
      toPerCase === undefined) ||
    (input.io! && input.e === input.s)
  ) {
    return input;
  } else {
    if (
      Flag.unsafeHas(initialInputTagFlag, TagFlag.union) ||
      (input.s.encoder === undefined && Flag.unsafeHas(initialInputTagFlag, TagFlag.ref))
    ) {
      input.s = unknown;
    }

    let activeKeyRef = "";
    if (
      !Flag.unsafeHas(
        initialInputTagFlag,
        Flag.with(Flag.with(TagFlag.union, TagFlag.ref), TagFlag.unknown),
      )
    ) {
      const sourceKey = unionToKey(input.s);
      let hasNull = false;
      let hasUndefined = false;
      const len = schemas.length;
      let i = 0;
      while (activeKeyRef === "" && i < len) {
        const s = schemas[i]!;
        if (unionToKey(s) === sourceKey) {
          activeKeyRef = sourceKey;
        } else if (s.type === nullTag) {
          hasNull = true;
        } else if (s.type === undefinedTag) {
          hasUndefined = true;
        }
        i = i + 1;
      }
      if (activeKeyRef === "") {
        if (Flag.unsafeHas(initialInputTagFlag, TagFlag.undefined) && hasNull) {
          activeKeyRef = nullTag;
        } else if (Flag.unsafeHas(initialInputTagFlag, TagFlag.null) && hasUndefined) {
          activeKeyRef = undefinedTag;
        }
      }
    }
    const activeKey = activeKeyRef;

    const initialInline = input.i;

    const fail = (caught: string) => {
      return `${B_embed(
        input,
        // PORT-NOTE: the source lambda reads `arguments`, so this must stay a
        // `function` expression (X.Function.toExpression made it a plain
        // uncurried function in ReScript; a TS function expression already is).
        function () {
          const args = arguments;
          B_throw(
            B_makeInvalidInputDetails(
              selfSchema,
              unknown,
              input.path,
              args[0],
              true,
              args.length > 1
                ? (Array.from(args).slice(1) as unknown as SuryErrorRecord[])
                : undefined,
            ),
          );
        },
      )}(${input.v()}${caught})`;
    };

    // Create a copy of the input val, so we can mutate it
    // It's still the same value though, until mutated
    const output = B_refine(input);
    const outputAnyOf: Internal[] = [];

    // Set when a single-case block fails at codegen time, so the caller
    // can drop the block and pass the embedded error along instead of
    // emitting a guaranteed runtime throw
    let staticBlockFailure = "";

    const getArrItemsCode = (arr: unknown[], isDeopt: boolean): string => {
      const typeValidationInput = arr[0] as Val;
      const typeValidationOutput = arr[1] as Val;

      let itemStart = "";
      let itemEnd = "";
      let itemNextElse = false;
      let itemNoop = "";
      let caught = "";

      // Accumulate schemas code by refinement (discriminant)
      // so if we have two schemas with the same discriminant
      // We can generate a single switch statement
      // with try/catch blocks for each item
      // If we come across an item without a discriminant
      // we need to dump all accumulated schemas in try block
      // and have the item without discriminant as catch all
      // If we come across an item without a discriminant
      // and without any code, it means that this item is always valid
      // and we should exit early
      //
      // PORT-NOTE: `itemCode = Single(string) | Multiple(array<string>)` is
      // @unboxed — runtime value is the string itself or the array itself, so
      // the cases are discriminated with Array.isArray.
      let byDiscriminant: Record<string, string | string[]> = {};

      const preItems = 2;
      let itemIdx = preItems;
      const lastIdx = arr.length - 1;
      while (itemIdx <= lastIdx) {
        // Copy it one more time, since every case decoder
        // might mutate the input
        const input = B_Val_scope(typeValidationOutput);
        input.u = true;
        input.t = typeValidationOutput.t;
        input.io = false;
        input.e = arr[itemIdx] as Internal;

        const isLast = itemIdx === lastIdx;
        const isFirst = itemIdx === preItems;
        const isOnlyCase = isFirst && isLast;
        let withExhaustiveCheck = !isOnlyCase;

        let itemSkipped = false;
        let itemCodeRef = "";
        const itemCondRef = { contents: "" };
        try {
          const itemOutput = parse(input);
          outputAnyOf.push(itemOutput.s);

          itemCodeRef = B_merge(itemOutput, itemCondRef);

          if (itemOutput.t!) {
            output.t = true;
            if (Flag.unsafeHas(itemOutput.f, ValFlag.async)) {
              output.f = Flag.with(output.f, ValFlag.async);
            }
            const itemVar = typeValidationInput.v();
            if (itemOutput.i !== itemVar) {
              itemCodeRef =
                itemCodeRef +
                // Need to allocate a var here, so we don't mutate the input object field
                `${itemVar}=${itemOutput.i}`;
            }
          }
        } catch (exn) {
          const errorVar = B_embed(input, InternalError.getOrRethrow(exn));
          caught = `${caught},${errorVar}`;
          if (isDeopt && isOnlyCase) {
            staticBlockFailure = errorVar;
            itemSkipped = true;
          } else if (isLast) {
            withExhaustiveCheck = false;
            itemCodeRef = isDeopt ? "throw " + errorVar : fail(caught);
          } else {
            // The case is guaranteed to fail at runtime, so skip its code
            // and keep the embedded error for the exhaustive failure args
            itemSkipped = true;
          }
        }
        const itemCond = itemCondRef.contents;
        const itemCode = itemCodeRef;

        // Accumulate item parser when it has a discriminant
        if (!itemSkipped && itemCond) {
          if (itemCode) {
            const existing = byDiscriminant[itemCond];
            if (existing !== undefined) {
              if (Array.isArray(existing)) {
                existing.push(itemCode);
              } else {
                byDiscriminant[itemCond] = [existing, itemCode];
              }
            } else {
              byDiscriminant[itemCond] = itemCode;
            }
          } else {
            // We have a condition but without additional parsing logic
            // So we accumulate it in case it's needed for a refinement later
            itemNoop = itemNoop ? `${itemNoop}||${itemCond}` : itemCond;
          }
        }

        // Allocate all accumulated discriminants
        // If we have an item without a discriminant
        // and need to deopt. Or we are at the last item
        if (!itemSkipped && (!itemCond || isLast)) {
          const accedDiscriminants = Object.keys(byDiscriminant);
          for (let idx = 0; idx < accedDiscriminants.length; idx++) {
            const discrim = accedDiscriminants[idx]!;
            const if_ = itemNextElse ? "else if" : "if";
            itemStart = itemStart + if_ + `(${discrim}){`;
            const entry = byDiscriminant[discrim]!;
            if (!Array.isArray(entry)) {
              itemStart = itemStart + entry + "}";
            } else {
              let caught = "";
              for (let idx = 0; idx < entry.length; idx++) {
                const code = entry[idx]!;
                const errorVar = `e` + idx;
                itemStart = itemStart + `try{${code}}catch(${errorVar}){`;
                caught = `${caught},${errorVar}`;
              }
              itemStart = itemStart + fail(caught) + "}".repeat(entry.length) + "}";
            }
            itemNextElse = true;
          }
          byDiscriminant = {};
        }

        if (!itemSkipped && !itemCond) {
          if (!itemCode) {
            // If we don't have a condition (discriminant)
            // and additional parsing logic,
            // it means that this item is always passes
            // so we can remove preceding accumulated refinements
            // and exit early even if there are other items
            itemNoop = "";
            itemIdx = lastIdx;
            withExhaustiveCheck = false;
          } else {
            // The item without refinement should switch to deopt mode
            // Since there might be validation in the body
            if (itemNoop) {
              const if_ = itemNextElse ? "else if" : "if";
              itemStart = itemStart + if_ + `(!(${itemNoop})){`;
              itemEnd = "}" + itemEnd;
              itemNoop = "";
              itemNextElse = false;
            }
            if (isLast && (isDeopt || !withExhaustiveCheck || isFirst)) {
              // For the last item don't add try/catch
              itemStart = itemStart + `${itemNextElse ? "else{" : ""}${itemCode}`;
              itemEnd = (itemNextElse ? "}" : "") + itemEnd;
            } else {
              const errorVar = `e` + (itemIdx - preItems);
              itemStart =
                itemStart + `${itemNextElse ? "else{" : ""}try{${itemCode}}catch(${errorVar}){`;
              itemEnd = (itemNextElse ? "}" : "") + "}" + itemEnd;
              caught = `${caught},${errorVar}`;
              itemNextElse = false;
            }
          }
        }
        if (isLast) {
          if (itemNoop) {
            if (
              itemStart ||
              // Skipped cases have their errors embedded,
              // which the hoisted check below can't reference
              caught
            ) {
              const if_ = itemNextElse ? "else if" : "if";
              itemStart = itemStart + if_ + `(!(${itemNoop})){${fail(caught)}}`;
            } else {
              B_pushCheck(typeValidationOutput, {
                c: (_inputVar) => `(${itemNoop})`,
                f: failInvalidType,
              });
            }
          } else if (withExhaustiveCheck) {
            const errorCode = fail(caught);
            itemStart = itemStart + (itemNextElse ? `else{${errorCode}}` : errorCode);
          }
        }

        itemIdx = itemIdx + 1;
      }

      return itemStart + itemEnd;
    };

    let start = "";
    let end = "";
    let caught = "";
    // If we got a case which always passes,
    // we can exit early
    let exit = false;

    const lastIdx = schemas.length - 1;
    let byKey: Record<string, unknown[]> = {};
    let keys: string[] = [];

    // FIXME: minimal fix — applies the union's refiner/inputRefiner per
    // surviving case (previously dropped when the union has `.to`). The
    // emit shape isn't ideal; fold this into the shared refiner pipeline
    // post-release.
    const appendUnionRefiners = (() => {
      const unionRefiner = selfSchema.refiner;
      const unionInputRefiner = selfSchema.inputRefiner;
      // Call each source refiner at most once so its predicate is embedded
      // in `input.global.embeded` once and every case references the same
      // `e[N]`. `B_embed` is append-only, so a per-case call would duplicate.
      const cachedRefinerChecks: { contents: Check[] | undefined } = { contents: undefined };
      const cachedInputRefinerChecks: { contents: Check[] | undefined } = {
        contents: undefined,
      };
      const attach = (
        current: ((input: Val) => Check[]) | undefined,
        source: ((input: Val) => Check[]) | undefined,
        cache: { contents: Check[] | undefined },
      ): ((input: Val) => Check[]) | undefined => {
        if (source === undefined) {
          return current;
        } else {
          const fn = source;
          const getCached = (input: Val): Check[] => {
            if (cache.contents !== undefined) {
              return cache.contents;
            } else {
              const checks = fn(input);
              cache.contents = checks;
              return checks;
            }
          };
          if (current === undefined) {
            return getCached;
          } else {
            const existing = current;
            return (input: Val) => {
              const arr = existing(input);
              const next = getCached(input);
              for (let i = 0; i < next.length; i++) {
                arr.push(next[i]!);
              }
              return arr;
            };
          }
        }
      };
      return (mut: Internal) => {
        const r = attach(mut.refiner, unionRefiner, cachedRefinerChecks);
        if (r !== undefined) {
          mut.refiner = r;
        }
        const ir = attach(mut.inputRefiner, unionInputRefiner, cachedInputRefinerChecks);
        if (ir !== undefined) {
          mut.inputRefiner = ir;
        }
      };
    })();

    // Tier 1: for a typed const input, variants with a matching const are
    // tried before catch-all and differently-const'ed variants
    if (isLiteral(input.s)) {
      const matching: Internal[] = [];
      const rest: Internal[] = [];
      for (let idx = 0; idx <= lastIdx; idx++) {
        const schema = schemas[idx]!;
        if (isLiteral(schema) && schema.const === input.s.const) {
          matching.push(schema);
        } else {
          rest.push(schema);
        }
      }
      schemas = matching.concat(rest);
    }

    for (let idx = 0; idx <= lastIdx; idx++) {
      const schema =
        toPerCase !== undefined
          ? updateOutput<Internal>(schemas[idx]!, (mut) => {
              appendUnionRefiners(mut);
              mut.to = toPerCase;
            })
          : schemas[idx]!;
      const tag = schema.type;
      const tagFlag = TagFlag.get(tag);
      const key = unionToKey(schema);

      if (activeKey !== "" && activeKey !== key) {
        // not in active tier — skip
      } else if (
        Flag.unsafeHas(tagFlag, TagFlag.undefined) &&
        "fromDefault" in (selfSchema as unknown as Record<string, unknown>)
      ) {
        // skip it
      } else {
        const initialArr = byKey[key];
        if (initialArr !== undefined) {
          const arr = initialArr;
          if (
            Flag.unsafeHas(tagFlag, TagFlag.object) &&
            nestedLoc in schema.properties!
          ) {
            // This is a special case for https://github.com/DZakh/sury/issues/150
            // When nested option goes together with an empty object schema
            // Since we put None case check second, we need to change priority here.
            arr.splice(arr.length - 1, 0, schema as unknown);
          } else if (
            // TODO: Is this check needed?
            // There can only be one valid. Dedupe
            !Flag.unsafeHas(
              tagFlag,
              Flag.with(Flag.with(TagFlag.undefined, TagFlag.null), TagFlag.nan),
            )
          ) {
            arr.push(schema as unknown);
          }
        } else {
          // Recreate input val for every schema
          // since we will mutate it
          const typeValidationInput = B_Val_scope(input);
          // Tree-shaking: build the narrow without a per-type factory. A
          // `string()`/`instance()`/… reference would pin every type decoder into
          // any union-using bundle — and `S.optional`/`S.nullable` are unions.
          if (
            Flag.unsafeHas(
              tagFlag,
              Flag.with(
                Flag.with(
                  Flag.with(Flag.with(TagFlag.unknown, TagFlag.union), TagFlag.ref),
                  TagFlag.function,
                ),
                TagFlag._never,
              ),
            )
          ) {
            // unknown / union / ref / json / function / never have no `typeof`
            // discriminant — the deopt (try-each) path handles them, so no
            // narrow is needed.
            typeValidationInput.e = unknown;
          } else {
            // A minimal narrow standing in as the variant's runtime schema,
            // carrying the member's encoder so a pending `.to` reverse reaches it.
            const narrow = baseSchema(schema.type, false);
            narrow.encoder = schema.encoder;
            if (Flag.unsafeHas(tagFlag, TagFlag.instance)) {
              narrow.class = schema.class;
            } else if (Flag.unsafeHas(tagFlag, TagFlag.object)) {
              narrow.properties = immutableEmptyObject as Record<string, Internal>;
              narrow.additionalItems = unknown;
            } else if (Flag.unsafeHas(tagFlag, TagFlag.array)) {
              narrow.additionalItems = unknown;
              narrow.items = immutableEmptyArray as Internal[];
            } else if (
              Flag.unsafeHas(
                tagFlag,
                Flag.with(Flag.with(TagFlag.null, TagFlag.undefined), TagFlag.nan),
              )
            ) {
              // null/undefined/nan stay literals so the case body passes through.
              narrow.const = schema.const;
            }
            // Per-invocation, not hoisted: this narrow is re-decoded during `.to`
            // per-variant conversion — with the union's `unknown` input (emit the
            // discriminant) or a concrete coerced value (delegate to schema.decoder).
            narrow.decoder = (input: Val) => {
              if (Flag.unsafeHas(TagFlag.get(input.s.type), TagFlag.unknown)) {
                return B_refine(input, input.e, [
                  {
                    c: (inputVar) => typeCheckCond(input, schema, inputVar),
                    f: failInvalidType,
                  },
                ]);
              } else {
                return schema.decoder(input);
              }
            };
            typeValidationInput.e = narrow;
          }

          let typeValidationOutput: Val;
          try {
            typeValidationOutput = parse(typeValidationInput);
          } catch (_) {
            // Discard any checks parse managed to push before throwing,
            // so the deopt path doesn't see leftover partial state.
            typeValidationInput.vc = undefined;
            typeValidationOutput = typeValidationInput;
          }

          if (unionIsPriority(tagFlag, byKey)) {
            // Not the fastest way, but it's the simplest way
            // to make sure NaN is checked before number
            // And instance and array checked before object
            keys.unshift(key);
          } else {
            keys.push(key);
          }
          byKey[key] = [
            typeValidationInput as unknown,
            typeValidationOutput as unknown,
            schema as unknown,
          ];

          let shouldDeopt = true;
          let valRef: Val | undefined = typeValidationOutput;
          while (valRef !== undefined && shouldDeopt) {
            const v: Val = valRef;
            valRef = v.prev;
            // Deopt to a try/catch block unless every level's checks are
            // hoistable into the dispatch condition (same rule as merge).
            shouldDeopt = !(v.vc && B_isHoistable(v));
          }

          if (shouldDeopt) {
            for (let keyIdx = 0; keyIdx < keys.length; keyIdx++) {
              const key = keys[keyIdx]!;
              if (!exit) {
                const arr = byKey[key]!;
                const typeValidationOutput = arr[1] as Val;
                const itemsCode = getArrItemsCode(arr, true);
                const blockCode = B_merge(typeValidationOutput) + itemsCode;

                const embeddedError = staticBlockFailure;
                if (embeddedError) {
                  staticBlockFailure = "";
                  if (blockCode) {
                    // Type validation code is still relevant — restore the throw
                    const errorVar = `e` + (idx + keyIdx);
                    start =
                      start + `try{${blockCode}throw ${embeddedError}}catch(${errorVar}){`;
                    end = "}" + end;
                    caught = `${caught},${errorVar}`;
                  } else {
                    // The block always fails — drop it
                    // and pass the embedded error along
                    caught = `${caught},${embeddedError}`;
                  }
                } else if (blockCode) {
                  const errorVar = `e` + (idx + keyIdx);
                  start = start + `try{${blockCode}}catch(${errorVar}){`;
                  end = "}" + end;
                  caught = `${caught},${errorVar}`;
                } else {
                  exit = true;
                }
              }
            }

            byKey = {};
            keys = [];
          }
        }
      }
    }

    if (!exit) {
      let nextElse = false;
      let noop = "";

      for (let idx = 0; idx < keys.length; idx++) {
        const arr = byKey[keys[idx]!]!;
        const typeValidationOutput = arr[1] as Val;
        const firstSchema = arr[2] as Internal;

        const itemsCode = getArrItemsCode(arr, false);

        const blockCondRef = { contents: "" };
        const blockCode = B_merge(typeValidationOutput, blockCondRef) + itemsCode;
        const blockCond = blockCondRef.contents;

        if (blockCode || unionIsPriority(TagFlag.get(firstSchema.type), byKey)) {
          const if_ = nextElse ? "else if" : "if";
          start = start + if_ + `(${blockCond}){${blockCode}}`;
          nextElse = true;
        } else {
          noop = noop ? `${noop}||${blockCond}` : blockCond;
        }
      }

      const errorCode = fail(caught);
      start =
        start +
        (noop
          ? (nextElse ? "else if" : "if") + `(!(${noop})){${errorCode}}`
          : nextElse
            ? `else{${errorCode}}`
            : end === ""
              ? // The bare fail call might be followed by more code, eg `return`
                errorCode + ";"
              : errorCode);
    }

    output.cp = output.cp + start + end;

    // In case if input.var was called, but output.var wasn't
    if (input.i !== output.i) {
      output.i = input.i;
    }

    let o: Val;
    if (Flag.unsafeHas(output.f, ValFlag.async)) {
      output.i = `Promise.resolve(${output.i})`;
      output.v = _notVar;
      o = output;
    } else if (output.v === _var) {
      // TODO: Think how to make it more robust
      // Recreate to not break the logic to determine
      // whether the output is changed

      // Use output.b instead of b because of mergeWithCatch
      // Should refactor mergeWithCatch to make it simpler
      // All of this is a hack to make mergeWithCatch think that there are no changes. eg S.array(S.option(item))
      if (input.cp === "" && output.cp === "" && initialInline === "i") {
        // FIXME: Might not be not needed
        input.hd = "";
        input.v = _notVar;
        input.i = initialInline;
        o = input;
      } else {
        o = output;
      }
    } else {
      o = output;
    }

    // Build the output schema from collected case output schemas. Variants
    // coercing to the same `.to` target now produce structurally-identical (but
    // not identity-equal) outputs; `toJSONSchema` collapses the duplicate.
    o.s = outputAnyOf.length ? unionFactory(outputAnyOf) : never_();
    if (toPerCase !== undefined) {
      o.io = true;
      o.e = getOutputSchema(toPerCase);
    } else {
      o.e = selfSchema;
    }

    return o;
  }
}
export function unionFactory(schemas: Internal[]): Internal {
  // TODO:
  // 1. Fitler out items without parser
  // 2. Remove duplicate schemas
  // 3. Spread Union and JSON if they are not transformed
  // 4. Provide correct `has` value for Union and JSON
  if (schemas.length === 0) {
    return InternalError.panic("S.union requires at least one item");
  } else if (schemas.length === 1) {
    return schemas[0]!;
  } else {
    const has: Record<string, boolean> = {};
    const anyOf = new Set<Internal>();

    for (let idx = 0; idx < schemas.length; idx++) {
      const schema = schemas[idx]!;

      // Check if the union is not transformed
      if (schema.type === unionTag && schema.to === undefined) {
        schema.anyOf!.forEach((item) => {
          anyOf.add(item);
        });
        Object.assign(has, schema.has!);
      } else {
        anyOf.add(schema);
        setHas(has, schema.type);
      }
    }
    const mut = baseSchema(unionTag, false);
    mut.anyOf = Array.from(anyOf);
    mut.decoder = unionDecoder;
    mut.encoder = unionEncoder;
    mut.has = has;
    return mut;
  }
}

export function nestedNone(): Internal {
  const itemSchema = Literal_parse(0);
  // FIXME: dict{}
  const properties: Record<string, Internal> = {};
  properties[nestedLoc] = itemSchema;
  return {
    type: objectTag,
    required: [nestedLoc],
    properties,
    additionalItems: "strip",
    decoder: objectDecoder,
    // TODO: Support this as a default coercion
    serializer: (input: Val) => {
      const nextSchema = input.e.to!;
      return B_nextConst(input, nextSchema, nextSchema);
      // FIXME: Need to set isOutput?
    },
  } as Internal;
}

export function nestedOption(item: Internal): Internal {
  return updateOutput<Internal>(item, (mut) => {
    mut.to = nestedNone();
    mut.parser = nestedOptionParser;
  });
}

// PORT-NOTE: the `~unit` labeled arg is renamed to `unitSchema` so the
// default expression can still reference the module-level `unit` factory.
export function optionFactory(item: Internal, unitSchema: Internal = unit()): Internal {
  const out = getOutputSchema(item);
  if (out.type === undefinedTag) {
    return unionFactory([unitSchema, nestedOption(item)]);
  } else if (out.type === unionTag) {
    const anyOf = out.anyOf;
    const has = out.has;
    return updateOutput<Internal>(item, (mut) => {
      const schemas = anyOf!;
      const mutHas = { ...has! };

      const newAnyOf: Internal[] = [];
      for (let idx = 0; idx < schemas.length; idx++) {
        const schema = schemas[idx]!;
        let toPush: Internal;
        const schemaOut = getOutputSchema(schema);
        if (schemaOut.type === undefinedTag) {
          mutHas[unitSchema.type] = true;
          newAnyOf.push(unitSchema);
          toPush = nestedOption(schema);
        } else if (schemaOut.properties !== undefined) {
          const properties = schemaOut.properties;
          const nestedSchema = properties[nestedLoc];
          if (nestedSchema !== undefined) {
            toPush = updateOutput<Internal>(schema, (mut) => {
              // FIXME: dict{}
              const properties: Record<string, Internal> = {};
              properties[nestedLoc] = {
                ...nestedSchema,
                const: ((nestedSchema.const as unknown as number) + 1) as unknown,
              } as Internal;
              mut.properties = properties;
            });
          } else {
            toPush = schema;
          }
        } else {
          toPush = schema;
        }
        newAnyOf.push(toPush);
      }

      if (newAnyOf.length === schemas.length) {
        mutHas[unitSchema.type] = true;
        newAnyOf.push(unitSchema);
      }

      mut.anyOf = newAnyOf;
      mut.has = mutHas;
    });
  } else {
    return unionFactory([item, unitSchema]);
  }
}

export function option(item: Internal): Internal {
  return optionFactory(item, unit());
}

export function valGet(parent: Val, location: string): Val {
  let vals: Record<string, Val>;
  if (parent.d !== undefined) {
    vals = parent.d;
  } else {
    const d: Record<string, Val> = {};
    parent.d = d;
    vals = d;
  }

  const existing = vals[location];
  if (existing !== undefined) {
    return B_Val_scope(existing);
  } else {
    let locationSchema: Internal | undefined;
    if (parent.s.type === objectTag) {
      locationSchema = parent.s.properties![location];
    } else {
      locationSchema = (parent.s.items! as unknown as Record<string, Internal>)[location];
    }
    let schema: Internal;
    if (locationSchema !== undefined) {
      schema = locationSchema;
    } else {
      const additionalItems = parent.s.additionalItems;
      if (additionalItems !== undefined && typeof additionalItems !== "string") {
        const s = additionalItems;
        // A `dict<V>` read by a fixed key may be absent (dicts have no required
        // keys), so model it as `option<V>` and let the union coercion handle a
        // missing key uniformly. Scoped to dict parents (objectTag) with a
        // concrete value type — array->tuple rest reads (arrayTag) and
        // json/unknown values read as-is. `option` is reachable directly because
        // B_Val_get now lives in the decoder `let rec` group (no forward ref).
        if (
          parent.s.type === objectTag &&
          s.type !== unknownTag &&
          !Flag.unsafeHas(TagFlag.get(s.type), TagFlag.ref) &&
          !isOptional(s)
        ) {
          schema = option(s);
        } else {
          schema = s;
        }
      } else {
        schema = B_unsupportedDecode(parent, parent.s, parent.e) as unknown as Internal;
      }
    }

    const pathAppend = pathFromInlinedLocation(B_inlineLocation(parent.g, location));

    const item: Val = {
      v: _notVarAtParent,
      i: isLiteral(schema) ? B_inlineConst(parent, schema) : `${B_Val_var(parent)}${pathAppend}`,
      f: ValFlag.none,
      s: schema,
      e: schema,
      cp: "",
      hd: "",
      path: pathConcat(parent.path, pathAppend),
      g: parent.g,
      p: parent,
    };
    vals[location] = item;
    return item;
  }
}
// =============================================================================
// Fragment 06 — operations (Sury.res lines 4187–4863)
// =============================================================================
//
// TODO(integration): expects from other sections:
//   - B (Builder.B const object: embed, next, refine, varWithoutAllocation,
//     _var, mergeWithPathPrepend, inlineConst, embedTransformation, effectCtx,
//     invalidOperation, invalidInputBuilder)
//   - compileDecoder(schema, expected, flag, defs)
//   - getDecoder(...args) — variadic (schemas..., flag?) decoder cache/compiler
//   - reverse(schema)
//   - getOutputSchema(schema)
//   - isAsyncInternal(schema, defs)
//   - unionFactory(items)
//   - Literal (Literal_parse)
//   - literalDecoder
//   - nullLiteral(), unit() — literal factories
// From the prelude (already in core.ts): Internal, Val, Check, Builder, Flag,
// ValFlag, Path helpers, InternalError, globalConfig, baseSchema, cached,
// copySchema, updateOutput, unknown, noopDecoder, schemaPrototype, vendor, s,
// valueOptions, configurableValueOptions, valKey, typeOf, objectTag,
// undefinedTag, refTag, toExpression.
//
// PORT-NOTE: `JsResult` is defined here (first fragment to need it); if
// another fragment also defines it, dedupe at integration time.

export const recursiveDecoder: Builder = (input) => {
  const expectedSchema = input.e;

  const schemaRef = expectedSchema["$ref"]!;
  const defs = input.g.d!;
  // Ignore #/$defs/
  const identifier = schemaRef.slice(8);
  const def = defs[identifier]!;
  const flag = input.g.o;

  const inputSchema = input.s.seq === expectedSchema.seq ? def : input.s;

  const key = `${inputSchema.seq}-${def.seq}--${flag}`;
  let recOperation = "";

  const fn = (def as unknown as Record<string, unknown>)[key];
  if (fn !== undefined) {
    // Circular reference (fn === 0) or already compiled
    recOperation =
      fn === (0 as unknown)
        ? B_embed(input, def) + `["${key}"]`
        : B_embed(input, fn);
  } else {
    // Optimistic compilation with recompile if assumptions were wrong
    let assumedHasTransform = def.hasTransform !== undefined ? def.hasTransform : false;
    let assumedIsAsync = def.isAsync !== undefined ? def.isAsync : false;
    let compileNeeded = true;
    let finalFn: unknown = 0;

    while (compileNeeded) {
      compileNeeded = false;

      // Set optimistic values on def before compiling (if not already set)
      // Inner circular references will read these values
      if (def.hasTransform === undefined) {
        def.hasTransform = assumedHasTransform;
      }
      if (def.isAsync === undefined) {
        def.isAsync = assumedIsAsync;
      }

      // Mark as in-progress
      (configurableValueOptions as unknown as Record<string, unknown>)[valKey] = 0;
      Object.defineProperty(def, key, configurableValueOptions as PropertyDescriptor);

      // Compile
      const fn = compileDecoder(inputSchema, def, flag, defs);

      // Cache result
      valueOptions[valKey] = fn;
      Object.defineProperty(def, key, valueOptions as PropertyDescriptor);

      finalFn = fn;

      // Check if actual values differ from assumed
      const actualHasTransform = def.hasTransform!;
      const actualIsAsync = def.isAsync!;

      if (
        actualHasTransform !== assumedHasTransform ||
        actualIsAsync !== assumedIsAsync
      ) {
        // Wrong assumption - update and recompile
        assumedHasTransform = actualHasTransform;
        assumedIsAsync = actualIsAsync;
        // Delete cached function to force recompilation
        delete (def as unknown as Record<string, unknown>)[key];
        compileNeeded = true;
      }
    }

    // Embed only the final compiled function to avoid wasting embed slots on recompiles
    recOperation = B_embed(input, finalFn);
  }

  const hasTransform = def.hasTransform === true;
  const isAsync = def.isAsync!;

  // Result var decl, prepended after the re-merge below so it sits outside the
  // try/catch mergeWithPathPrepend may wrap the assignment in (stays in scope).
  let outputDecl = "";
  let output: Val;
  if (hasTransform || isAsync) {
    const outputVar = B_varWithoutAllocation(input.g);
    outputDecl = `let ${outputVar};`;

    output = B_next(input, outputVar, expectedSchema, expectedSchema);
    output.v = _var;

    output.cp = `${outputVar}=${recOperation}(${input.i});`;

    if (isAsync) {
      output.f = Flag.with(output.f, ValFlag.async);
    }
  } else {
    // No transform: call for validation but don't capture result
    output = B_refine(input, expectedSchema, undefined, expectedSchema);
    output.cp = `${recOperation}(${input.i});`;
  }

  output.prev = undefined;
  output.cp = outputDecl + B_mergeWithPathPrepend(output, input);

  // Un-finalize: this val may be reused as input to a subsequent parser (e.g.
  // S.transform on a recursive schema) and must accept hoisted decls again.
  output.fz = undefined;
  output.prev = input;

  return output;
};

// PORT-NOTE: StandardSchema/JSONSchema types are ported as loose, type-only
// aliases (no runtime import allowed here). `JSONSchemaT` stands in for
// JSONSchema.t.
export type StandardIssue = {
  message: string;
  path?: unknown[];
};
export type StandardResult = {
  value?: unknown;
  issues?: StandardIssue[];
};
export type StandardProps = {
  version: number;
  vendor: string;
  validate: (input: unknown) => StandardResult;
  jsonSchema?: {
    input: (options: StandardJsonSchemaOptions) => JSONSchemaT;
    output: (options: StandardJsonSchemaOptions) => JSONSchemaT;
  };
};

// Forward reference for the Standard JSON Schema converter, whose body depends
// on `toJSONSchema` and `reverse` (defined later in the file). It is assigned
// right after those functions are defined. The getter below runs lazily (only
// on property access), so the ref deref is never on the hot path.
export const standardJSONSchemaRef: {
  contents: (
    schema: Internal,
    options: StandardJsonSchemaOptions,
    isOutput: boolean
  ) => JSONSchemaT;
} = {
  contents: 0 as unknown as (
    schema: Internal,
    options: StandardJsonSchemaOptions,
    isOutput: boolean
  ) => JSONSchemaT,
};

// Indirection keeps toJSONSchema/reverse tree-shakeable; see enableStandardJSONSchema below.
export function getStandardJSONSchema(
  schema: Internal,
  options: StandardJsonSchemaOptions,
  isOutput: boolean
): JSONSchemaT {
  if (standardJSONSchemaRef.contents as unknown as boolean) {
    return standardJSONSchemaRef.contents(schema, options, isOutput);
  } else {
    throw InternalError.make({
      code: "invalid_operation",
      path: pathEmpty,
      reason:
        "~standard.jsonSchema requires S.enableStandardJSONSchema() to be called first",
    });
  }
}

Object.defineProperty(schemaPrototype, "~standard", {
  get: function (this: Internal) {
    const schema = this;
    const standard: StandardProps = {
      version: 1,
      vendor,
      validate: (input: unknown): StandardResult => {
        try {
          return {
            value: (getDecoder(unknown, schema) as (input: unknown) => unknown)(input),
          };
        } catch (exn) {
          const error = InternalError.getOrRethrow(exn);
          return {
            issues: [
              {
                message: error.reason,
                // PORT-NOTE: the source maps each key through the unboxed
                // `StandardSchema.Issue.String` constructor, which is an
                // identity at runtime — the map is dropped here.
                path:
                  error.path === pathEmpty ? undefined : pathToArray(error.path),
              },
            ],
          };
        }
      },
      // Standard JSON Schema spec: https://standardschema.dev/json-schema
      // `input` returns the JSON Schema of the schema's input type,
      // `output` the JSON Schema of its output type. The `$schema` URI is
      // stamped according to `options.target`; an unsupported target throws.
      // Throws before enableStandardJSONSchema is called.
      jsonSchema: {
        input: (options) => getStandardJSONSchema(schema, options, false),
        output: (options) => getStandardJSONSchema(schema, options, true),
      },
    };
    return standard;
  },
});

// =============
// Builder functions
// =============

export function parser(schema: Internal): (input: unknown) => unknown {
  return getDecoder(unknown, schema) as (input: unknown) => unknown;
}

export function asyncParser(schema: Internal): (input: unknown) => Promise<unknown> {
  return getDecoder(unknown, schema, Flag.async) as (input: unknown) => Promise<unknown>;
}

export function decoder(from: Internal, to: Internal): (input: unknown) => unknown {
  return getDecoder(reverse(from), to) as (input: unknown) => unknown;
}

export function asyncDecoder(from: Internal, to: Internal): (input: unknown) => Promise<unknown> {
  return getDecoder(reverse(from), to, Flag.async) as (input: unknown) => Promise<unknown>;
}

export function decoder1(schema: Internal): (input: unknown) => unknown {
  return getDecoder(schema) as (input: unknown) => unknown;
}

export function asyncDecoder1(schema: Internal): (input: unknown) => Promise<unknown> {
  return getDecoder(schema, Flag.async) as (input: unknown) => Promise<unknown>;
}

// =============
// Operations
// =============

export function getAssertResult(): Internal {
  return cached("a", undefinedTag, (s) => {
    s.const = void 0;
    s.decoder = literalDecoder;
    s.noValidation = true;
  });
}

export function parseOrThrow(any: unknown, schema: Internal): unknown {
  return (getDecoder(unknown, schema) as (input: unknown) => unknown)(any);
}

export function parseAsyncOrThrow(any: unknown, schema: Internal): Promise<unknown> {
  return (getDecoder(unknown, schema, Flag.async) as (input: unknown) => Promise<unknown>)(any);
}

export function assertOrThrow(any: unknown, schema: Internal): void {
  (getDecoder(unknown, schema, getAssertResult()) as (input: unknown) => unknown)(any);
}

export function assertAsyncOrThrow(any: unknown, schema: Internal): Promise<void> {
  return (
    getDecoder(unknown, schema, getAssertResult(), Flag.async) as (
      input: unknown
    ) => Promise<void>
  )(any);
}

export function decodeOrThrow(any: unknown, from: Internal, to: Internal): unknown {
  return (getDecoder(reverse(from), to) as (input: unknown) => unknown)(any);
}

export function decodeAsyncOrThrow(any: unknown, from: Internal, to: Internal): Promise<unknown> {
  return (getDecoder(reverse(from), to, Flag.async) as (input: unknown) => Promise<unknown>)(any);
}

export function isAsync(schema: Internal): boolean {
  if (schema.isAsync === undefined) {
    return isAsyncInternal(schema, 0 as unknown as Record<string, Internal>);
  } else {
    return schema.isAsync;
  }
}

// PORT-NOTE: jsResult<'v> ported as a `success`-discriminated union per
// conventions.
export type JsResult<V> =
  | { success: true; value: V }
  | { success: false; error: SuryErrorRecord };

export function wrapExnToFailure(exn: unknown): JsResult<never> {
  if (exn && (exn as { s?: symbol }).s === s) {
    return { success: false, error: exn as unknown as SuryErrorRecord };
  } else {
    throw exn;
  }
}

export function js_safe<V>(fn: () => V): JsResult<V> {
  try {
    return {
      success: true,
      value: fn(),
    };
  } catch (exn) {
    return wrapExnToFailure(exn);
  }
}

export function js_safeAsync<V>(fn: () => Promise<V>): Promise<JsResult<V>> {
  try {
    return fn().then(
      (value): JsResult<V> => ({ success: true, value }),
      wrapExnToFailure
    );
  } catch (exn) {
    return Promise.resolve(wrapExnToFailure(exn));
  }
}

// PORT-NOTE: `module Metadata` → `MetadataModule`, with `Id` nested so call
// sites read `MetadataModule.Id.make(...)`. `Id.t<'metadata>` is a string at
// runtime; `unionToKey` was `%identity` and is dropped.
export type MetadataId = string;

export const MetadataModule = {
  Id: {
    make: (namespace: string, name: string): MetadataId => {
      return `m:${namespace}:${name}`;
    },
    internal: (name: string): MetadataId => {
      return `m:${name}`;
    },
  },
  get: (schema: Internal, id: MetadataId): unknown => {
    return (schema as unknown as Record<string, unknown>)[id];
  },
  setInPlace: (schema: Internal, id: MetadataId, metadata: unknown): void => {
    (schema as unknown as Record<string, unknown>)[id] = metadata;
  },
  set: (schema: Internal, id: MetadataId, metadata: unknown): Internal => {
    const mut = copySchema(schema);
    MetadataModule.setInPlace(mut, id, metadata);
    return mut;
  },
};

export const defsPath = `#/$defs/`;
export function recursive(name: string, fn: (schema: Internal) => Internal): Internal {
  const ref = `${defsPath}${name}`;
  const refSchema = baseSchema(refTag, false);
  refSchema["$ref"] = ref;
  refSchema.name = name;
  refSchema.decoder = recursiveDecoder;

  // This is for mutual recursion
  const isNestedRec = globalConfig.d as unknown as boolean;
  if (!isNestedRec) {
    globalConfig.d = {};
  }
  const def = fn(refSchema);
  if (def.name as unknown as boolean) {
    refSchema.name = def.name;
  }
  globalConfig.d![name] = def;

  if (isNestedRec) {
    return refSchema;
  } else {
    const schema = baseSchema(refTag, false);
    schema.name = refSchema.name;
    schema["$ref"] = ref;
    schema["$defs"] = globalConfig.d;
    schema.decoder = recursiveDecoder;

    globalConfig.d = undefined;

    return schema;
  }
}

export function noValidation(schema: Internal, value: boolean): Internal {
  const mut = copySchema(schema);

  // TODO: Test for discriminant literal
  // TODO: Better test reverse
  mut.noValidation = value;
  return mut;
}

export function internalRefine(
  schema: Internal,
  makeRefiner: (mut: Internal) => (input: Val) => Check[]
): Internal {
  return updateOutput(schema, (mut) => {
    const refiner = makeRefiner(mut);
    const existingRefiner = mut.refiner;
    if (existingRefiner !== undefined) {
      mut.refiner = (input) => {
        const arr = existingRefiner(input);
        const next = refiner(input);
        for (let i = 0; i < next.length; i++) {
          arr.push(next[i]!);
        }
        return arr;
      };
    } else {
      mut.refiner = refiner;
    }
  });
}

export function refine(
  schema: Internal,
  refineCheck: (value: unknown) => boolean,
  error?: string,
  path?: string[]
): Internal {
  const message = error !== undefined ? error : "Refinement failed";
  const extraPath = path !== undefined ? pathFromArray(path) : pathEmpty;
  return internalRefine(schema, (_) => (input) => {
    const embeddedCheck = B_embed(input, refineCheck);
    return [
      {
        c: (inputVar) => `${embeddedCheck}(${inputVar})`,
        f: B_invalidInputBuilder(undefined, extraPath, message),
      },
    ];
  });
}

export function getMutErrorMessage(mut: Internal): Record<string, string> {
  const em: Record<string, string> = mut.errorMessage
    ? { ...(mut.errorMessage as unknown as Record<string, string>) }
    : {};
  mut.errorMessage = em as unknown as SchemaErrorMessage;
  return em;
}

export type TransformDefinition<Input = unknown, Output = unknown> = {
  // @as("p") — parser
  p?: (input: Input) => Output;
  // @as("a") — asyncParser
  a?: (input: Input) => Promise<Output>;
  // @as("s") — serializer
  s?: (output: Output) => Input;
};

// PORT-NOTE: `s<'output>` (the effect ctx passed to the transformer) is what
// `B_effectCtx` returns: `{ fail(message, path?): never }`.

export function transform(
  schema: Internal,
  transformer: (ctx: EffectCtx) => TransformDefinition
): Internal {
  return updateOutput(schema, (mut) => {
    mut.parser = (input) => {
      const definition = transformer(B_effectCtx(input));
      if (definition.p !== undefined && definition.a === undefined) {
        return B_embedTransformation(input, definition.p, false);
      } else if (definition.p === undefined && definition.a !== undefined) {
        return B_embedTransformation(input, definition.a, true);
      } else if (
        definition.p === undefined &&
        definition.a === undefined &&
        definition.s === undefined
      ) {
        return B_refine(input, undefined, undefined, input.e.to!);
      } else if (definition.p === undefined && definition.a === undefined) {
        return B_invalidOperation(input, `The S.transform parser is missing`);
      } else {
        return B_invalidOperation(
          input,
          `The S.transform doesn't allow parser and asyncParser at the same time. Remove parser in favor of asyncParser`
        );
      }
    };
    const to = copySchema(unknown);
    to.serializer = (input) => {
      const definition = transformer(B_effectCtx(input));
      if (definition.s !== undefined) {
        return B_embedTransformation(input, definition.s, false);
      } else if (
        definition.p === undefined &&
        definition.a === undefined &&
        definition.s === undefined
      ) {
        return B_refine(input, undefined, undefined, input.e.to!);
      } else {
        return B_invalidOperation(input, `The S.transform serializer is missing`);
      }
    };
    mut.to = to;
    delete mut.isAsync;
  });
}

export function nullAsUnit(): Internal {
  // PORT-NOTE: local `s` renamed to `schema` — `s` is the module-level error
  // identity symbol in this file.
  const schema = copySchema(nullLiteral());
  schema.to = unit();
  return schema;
}

// PORT-NOTE: `Option.default = Value(unknown) | Callback(unit => unknown)` is
// a regular (boxed) variant used only within this module; ported with a
// string `TAG` discriminant — the representation never escapes.
export type OptionDefault =
  | { TAG: "Value"; _0: unknown }
  | { TAG: "Callback"; _0: () => unknown };

export const Option_getWithDefault = (schema: Internal, default_: OptionDefault): Internal => {
  return updateOutput(schema, (mut) => {
    const anyOf = mut.anyOf;
    if (anyOf !== undefined) {
      const outputItems: Internal[] = [];
      // FIXME: drop `originalItems` once unionDecoder can reverse member
      // `.to` chains — then mut.default + the serializer can both run
      // through `schema->reverse` directly.
      const originalItems: Internal[] = [];

      for (let idx = 0; idx < anyOf.length; idx++) {
        const schema = anyOf[idx]!;
        const outputSchema = getOutputSchema(schema);
        switch (outputSchema.type) {
          case undefinedTag:
            break;
          default:
            outputItems.push(outputSchema);
            originalItems.push(schema);
        }
      }

      const item: Internal =
        outputItems.length === 0
          ? InternalError.panic(`Can't set default for ${toExpression(mut)}`)
          : outputItems.length === 1
            ? outputItems[0]!
            : unionFactory(outputItems);
      const originalItem: Internal =
        originalItems.length === 1 ? originalItems[0]! : unionFactory(originalItems);

      if (default_.TAG === "Value") {
        const v = default_._0;
        // Full unknown -> item decode so primitive item types still get type-checked.
        try {
          (getDecoder(unknown, item) as (input: unknown) => unknown)(v);
        } catch (exn) {
          const error = InternalError.getOrRethrow(exn);
          InternalError.panic(
            `Invalid default for ${toExpression(mut)}: ${
              (error as unknown as { message: string })["message"]
            }`
          );
        }
        // Best-effort input form for JSON Schema metadata.
        // FIXME: running a decoder at schema-creation time isn't a goal —
        // it compiles + executes a fresh decode pipeline per default. Replace
        // with something cheaper (or move to lazy/JSON-Schema-export time)
        // before the official v11 release.
        try {
          mut.default = (getDecoder(reverse(originalItem)) as (input: unknown) => unknown)(v);
        } catch (_exn) {}
      }

      mut.parser = (input) => {
        const nextSchema = input.e.to!;
        const inputVar = input.v();
        return B_next(
          input,
          `${inputVar}===void 0?${
            default_.TAG === "Value"
              ? B_inlineConst(input, Literal_parse(default_._0))
              : `${B_embed(input, default_._0)}()`
          }:${inputVar}`,
          nextSchema,
          nextSchema
        );
      };
      const to = copySchema(item);

      const originalDecoder = to.decoder;
      to.serializer = (input) => {
        const nextSchema = reverse(originalItem);
        return B_refine(originalDecoder(input), nextSchema, undefined, nextSchema);
      };

      // FIXME: This looks wrong, but this is how it was with prev architecture
      to.decoder = noopDecoder;

      mut.to = to;
    } else {
      InternalError.panic(`Can't set default for ${toExpression(mut)}`);
    }
  });
};

export const Option_getOr = (schema: Internal, defalutValue: unknown): Internal =>
  Option_getWithDefault(schema, { TAG: "Value", _0: defalutValue });
export const Option_getOrWith = (schema: Internal, defalutCb: () => unknown): Internal =>
  Option_getWithDefault(schema, { TAG: "Callback", _0: defalutCb });

// PORT-NOTE: `Object.s` (the object ctx record) → `ObjectCtx`; field names are
// the runtime names from `@as` (`f` for `field`, others unchanged).
export type ObjectCtx = {
  // @as("f") — field
  f: (location: string, schema: Internal) => unknown;
  fieldOr: (location: string, schema: Internal, or: unknown) => unknown;
  tag: (location: string, value: unknown) => void;
  nested: (location: string) => ObjectCtx;
  flatten: (schema: Internal) => unknown;
};

export const ObjectModule = {
  setAdditionalItems: (
    schema: Internal,
    additionalItems: AdditionalItems,
    deep: boolean
  ): Internal => {
    const currentAdditionalItems = schema.additionalItems;
    if (
      currentAdditionalItems !== undefined &&
      currentAdditionalItems !== additionalItems &&
      typeOf(currentAdditionalItems) !== objectTag
    ) {
      const mut = copySchema(schema);
      mut.additionalItems = additionalItems;
      if (deep) {
        const items = schema.items;
        if (items !== undefined) {
          const newItems: Internal[] = [];
          for (let idx = 0; idx < items.length; idx++) {
            const s = items[idx]!;
            newItems.push(ObjectModule.setAdditionalItems(s, additionalItems, deep));
          }
          mut.items = newItems;
        }

        const properties = schema.properties;
        if (properties !== undefined) {
          const newProperties: Record<string, Internal> = {};
          const keys = Object.keys(properties);
          for (let idx = 0; idx < keys.length; idx++) {
            const key = keys[idx]!;
            newProperties[key] = ObjectModule.setAdditionalItems(
              properties[key]!,
              additionalItems,
              deep
            );
          }
          mut.properties = newProperties;
        }
      }
      return mut;
    } else {
      return schema;
    }
  },
};

export function strip(schema: Internal): Internal {
  return ObjectModule.setAdditionalItems(schema, "strip", false);
}

export function deepStrip(schema: Internal): Internal {
  return ObjectModule.setAdditionalItems(schema, "strip", true);
}

export function strict(schema: Internal): Internal {
  return ObjectModule.setAdditionalItems(schema, "strict", false);
}

export function deepStrict(schema: Internal): Internal {
  return ObjectModule.setAdditionalItems(schema, "strict", true);
}

// PORT-NOTE: `module Tuple` contains only the ctx record type — no runtime
// const is emitted; `Tuple.s` → `TupleCtx`.
export type TupleCtx = {
  item: (idx: number, schema: Internal) => unknown;
  tag: (idx: number, value: unknown) => void;
};
// =============================================================================
// Section 07: json / formats — Sury.res lines 4864-5484
// (jsonEncoderFn / isJsonable / jsonDecoderFn / json, jsonString,
//  jsonStringWithSpace, uint8Array, isoDateTime, port, email, uuid, cuid,
//  url, invalidDateRefine, date, to, list, meta, brand)
//
// TODO(integration): expects from other sections:
//   - B (B_refine, B_next, B_nextConst, B_embed, B_embedInvalidInput,
//     B_unsupportedDecode, B_failWithErrorMessage, failInvalidType,
//     _var, B_varWithoutAllocation, B_Val_Object_add)
//   - parse (the parse loop, Sury.res ~2256)
//   - stringDecoderFn, numberDecoder, arrayDecoder, literalDecoder,
//     unionDecoder, instanceDecoder, recursiveDecoder
//   - string, bool, float, unit, nullLiteral (primitive factories)
//   - array, dictFactory, unionFactory
//   - makeObjectVal, completeObjectVal, valGet, unionPerVariantVal,
//     inputToString
//   - transform, updateOutput, reverse, getDecoder
//   - jsonName (= `JSON`, Sury.res 2146), defsPath (= `#/$defs/`, 4514)
//   - baseSchema, cached, copySchema, unknown, isLiteral, TagFlag, Flag,
//     inlinedValueFromString (prelude — baseSchema needs to be exported)
//   - ValObject type (B_Val_Object_t) — makeObjectVal's return type
// =============================================================================

export function jsonEncoderFn(input: Val, target: Internal): Val {
  const toTagFlag = TagFlag.get(target.type);

  if (
    Flag.unsafeHas(
      toTagFlag,
      Flag.with(
        Flag.with(Flag.with(TagFlag.string, TagFlag.boolean), TagFlag.number),
        TagFlag.null,
      ),
    )
  ) {
    return parse(B_refine(input, unknown, undefined, target));
  } else if (Flag.unsafeHas(toTagFlag, Flag.with(TagFlag.undefined, TagFlag.nan))) {
    const jsonExpected = copySchema(nullLiteral());
    jsonExpected.to = target;
    return parse(B_refine(input, unknown, undefined, jsonExpected));
  } else if (Flag.unsafeHas(toTagFlag, TagFlag.array)) {
    // Validate that the input is an array
    // and then update the schema to be an array of json instead of array of unknown
    const jsonExpected = array(unknown);
    const output = parse(B_refine(input, unknown, undefined, jsonExpected));
    output.s.additionalItems = json();
    output.e = target;
    output.io = false;
    return output;
  } else if (Flag.unsafeHas(toTagFlag, TagFlag.object)) {
    // Validate that the input is an object
    // and then update the schema to be an object of json instead of object of unknown
    const jsonExpected = dictFactory(unknown);
    const output = parse(B_refine(input, unknown, undefined, jsonExpected));
    output.s.additionalItems = json();
    output.e = target;
    output.io = false;
    return output;
  } else if (Flag.unsafeHas(toTagFlag, Flag.with(TagFlag.union, TagFlag.ref))) {
    return input;
  } else {
    // For non-JSON types (bigint, instance, etc.), decode through string
    const jsonExpected = copySchema(string());
    jsonExpected.to = target;
    return parse(B_refine(input, unknown, undefined, jsonExpected));
  }
}

export function isJsonable(schema: Internal): boolean {
  const tagFlag = TagFlag.get(schema.type);
  return (
    Flag.unsafeHas(
      tagFlag,
      Flag.with(
        Flag.with(Flag.with(TagFlag.string, TagFlag.number), TagFlag.boolean),
        TagFlag.null,
      ),
    ) ||
    schema["$ref"] === json()["$ref"] ||
    (Flag.unsafeHas(tagFlag, TagFlag.union) && schema.anyOf!.every(isJsonable)) ||
    (Flag.unsafeHas(tagFlag, TagFlag.array) &&
      (typeof schema.additionalItems === "object"
        ? isJsonable(schema.additionalItems as Internal)
        : true) &&
      schema.items!.every(isJsonable)) ||
    (Flag.unsafeHas(tagFlag, TagFlag.object) &&
      (typeof schema.additionalItems === "object"
        ? isJsonable(schema.additionalItems as Internal)
        : true) &&
      Object.values(schema.properties!).every(isJsonable))
  );
}

export function jsonDecoderFn(input: Val): Val {
  const inputTagFlag = TagFlag.get(input.s.type);

  if (isJsonable(input.s)) {
    return input;
  } else if (Flag.unsafeHas(inputTagFlag, Flag.with(TagFlag.undefined, TagFlag.nan))) {
    return B_nextConst(input, nullLiteral());
  } else if (Flag.unsafeHas(inputTagFlag, TagFlag.array)) {
    const expected = baseSchema(arrayTag, false);
    expected.items = input.s.items!.map((_) => json());
    expected.decoder = arrayDecoder;
    expected.additionalItems =
      typeof input.s.additionalItems === "object"
        ? json()
        : input.s.additionalItems;
    expected.to = input.e.to;
    return parse(B_refine(input, undefined, undefined, expected));
  } else if (Flag.unsafeHas(inputTagFlag, TagFlag.object)) {
    if (typeof input.s.additionalItems === "object") {
      const expected = dictFactory(json());
      expected.to = input.e.to;
      return parse(B_refine(input, undefined, undefined, expected));
    } else {
      const jsonVal = makeObjectVal(input, input.s);
      jsonVal.e = json();
      if (input.e.to as unknown as boolean) {
        jsonVal.e = copySchema(jsonVal.e);
        jsonVal.e.to = input.e.to;
      }

      const keys = Object.keys(input.s.properties!);
      for (let idx = 0; idx <= keys.length - 1; idx++) {
        const key = keys[idx]!;
        const itemVal = valGet(input, key);
        itemVal.io = false;

        if (
          itemVal.s.type === unionTag &&
          (itemVal.s.has![undefinedTag as string] as unknown as boolean)
        ) {
          itemVal.e = unionFactory([unit(), json()]);
          const itemOutput = parse(itemVal);
          itemOutput.o = true;
          B_Val_Object_add(jsonVal, key, itemOutput);
        } else {
          itemVal.e = json();
          B_Val_Object_add(jsonVal, key, parse(itemVal));
        }
      }

      return completeObjectVal(jsonVal);
    }
  } else if (Flag.unsafeHas(inputTagFlag, TagFlag.ref)) {
    // FIXME: Should be a unified solution for ref inputs
    return recursiveDecoder(input);
  } else if (
    Flag.unsafeHas(inputTagFlag, TagFlag.union) &&
    // Union-tagged schemas always carry `anyOf` and `has`
    // (set by unionFactory, reverse and the S.json def).
    // Unions with an undefined variant are not supported,
    // since undefined is not representable in JSON
    !((undefinedTag as string) in input.s.has!)
  ) {
    // Decode each union variant to JSON separately
    return parse(unionPerVariantVal(input, input.e));
  } else if (Flag.unsafeHas(inputTagFlag, TagFlag.unknown)) {
    const to = input.e.to!;
    // Whether we can optimize encoding during decoding
    const preEncode: boolean =
      (to as unknown as boolean) && !(input.e.parser as unknown as boolean); // && !(selfSchema.refiner->Obj.magic) FIXME:
    if (preEncode) {
      input.s = json();
      return jsonEncoderFn(input, input.e);
    } else if (input.e.noValidation!) {
      input.s = json();
      return input;
    } else {
      return recursiveDecoder(input);
    }
  } else {
    try {
      const expected = copySchema(string());
      expected.to = input.e;
      input.e = expected;
      return parse(input);
    } catch (_) {
      return B_unsupportedDecode(input, input.s, json());
    }
  }
}

export function json(): Internal {
  return cached(jsonName, refTag, (s) => {
    const jsonRef = baseSchema(refTag, true);
    jsonRef["$ref"] = `${defsPath}${jsonName}`;
    jsonRef.name = jsonName;

    jsonRef.decoder = jsonDecoderFn;
    const jsonEncoder = jsonEncoderFn;
    jsonRef.encoder = jsonEncoder;

    s["$ref"] = jsonRef["$ref"];
    s.name = jsonName;
    s.decoder = jsonDecoderFn;
    s.encoder = jsonEncoder;

    const anyOf = [
      string(),
      bool(),
      float(),
      nullLiteral(),
      dictFactory(jsonRef),
      array(jsonRef),
    ];
    const has: Record<string, boolean> = {};
    anyOf.forEach((schema) => {
      has[schema.type as string] = true;
    });

    const jsonDef = baseSchema(unionTag, true);
    jsonDef.anyOf = anyOf;
    jsonDef.has = has;
    jsonDef.decoder = unionDecoder;
    jsonDef.name = jsonName;
    jsonDef.type = unionTag;

    const defs: Record<string, Internal> = {};
    defs[jsonName] = jsonDef;
    s["$defs"] = defs;
  });
}

export const jsonString = /* @__PURE__ */ (() => {
  const inlineJsonString = (input: Val, schema: Internal): string => {
    const tagFlag = TagFlag.get(schema.type);
    const const_ = schema.const;
    if (Flag.unsafeHas(tagFlag, Flag.with(TagFlag.undefined, TagFlag.null))) {
      return `"null"`;
    } else if (Flag.unsafeHas(tagFlag, TagFlag.string)) {
      return JSON.stringify(
        inlinedValueFromString(const_ as unknown as string),
      ) as unknown as string;
    } else if (Flag.unsafeHas(tagFlag, TagFlag.bigint)) {
      return `"\\"${const_}\\""`;
    } else if (Flag.unsafeHas(tagFlag, Flag.with(TagFlag.number, TagFlag.boolean))) {
      return `"${const_}"`;
    } else {
      return B_unsupportedDecode(input, schema, input.e);
    }
  };

  const constSchemaToJsonStringConst = (input: Val, target: Internal): string => {
    const tagFlag = TagFlag.get(target.type);
    const const_ = target.const;
    if (Flag.unsafeHas(tagFlag, Flag.with(TagFlag.undefined, TagFlag.null))) {
      return `null`;
    } else if (Flag.unsafeHas(tagFlag, TagFlag.string)) {
      return inlinedValueFromString(
        const_ as unknown as string,
      ) as unknown as string;
    } else if (Flag.unsafeHas(tagFlag, TagFlag.bigint)) {
      return `"${const_}"`;
    } else if (Flag.unsafeHas(tagFlag, Flag.with(TagFlag.number, TagFlag.boolean))) {
      return ("" + (const_ as unknown as string)) as string;
    } else {
      return B_unsupportedDecode(input, input.s, target);
    }
  };

  const jsonStringEncoder: Encoder = (input, target) => {
    if (target.format !== "json") {
      if (isLiteral(target)) {
        const jsonStringConstSchema = baseSchema(stringTag, true);
        jsonStringConstSchema.const = constSchemaToJsonStringConst(
          input,
          target,
        ) as unknown;
        jsonStringConstSchema.to = target;
        jsonStringConstSchema.decoder = literalDecoder;
        return B_refine(input, undefined, undefined, jsonStringConstSchema);
      } else {
        const outputVar = B_varWithoutAllocation(input.g);

        const nextSchema = copySchema(json());
        nextSchema.to = target;

        const output = B_next(input, outputVar, nextSchema, nextSchema);
        output.io = true;
        output.v = _var;

        const inputVar = input.v();
        output.cp = `let ${outputVar};try{${outputVar}=JSON.parse(${inputVar})}catch(t){${B_embedInvalidInput(
          input,
          input.s,
        )}}`;

        return output;
      }
    } else {
      return input;
    }
  };

  const jsonStringDecoder: Builder = (input) => {
    const inputTagFlag = TagFlag.get(input.s.type);
    const expectedSchema = input.e;

    if (Flag.unsafeHas(inputTagFlag, TagFlag.unknown)) {
      const to = expectedSchema.to!;
      // Whether we can optimize encoding during decoding
      const preEncode: boolean =
        (to as unknown as boolean) &&
        to.type !== unknownTag &&
        !(expectedSchema.parser as unknown as boolean) &&
        !(expectedSchema.refiner as unknown as boolean);

      const stringVal = stringDecoderFn(input);
      stringVal.s = expectedSchema;
      stringVal.e = expectedSchema;

      if (preEncode) {
        return jsonStringEncoder(stringVal, to);
      } else {
        const stringVar = stringVal.v();
        const output = B_refine(stringVal, expectedSchema);
        output.cp = `try{JSON.parse(${stringVar})}catch(t){${B_embedInvalidInput(
          stringVal,
        )}}`;
        return output;
      }
    } else if (input.s.format === "json") {
      return input;
    } else if (isLiteral(input.s)) {
      return B_next(input, inlineJsonString(input, input.s), expectedSchema);
    } else if (Flag.unsafeHas(inputTagFlag, TagFlag.string)) {
      return B_next(input, `JSON.stringify(${input.i})`, expectedSchema);
    } else if (Flag.unsafeHas(inputTagFlag, Flag.with(TagFlag.number, TagFlag.boolean))) {
      const output = inputToString(input);
      output.s = expectedSchema;
      return output;
    } else if (Flag.unsafeHas(inputTagFlag, TagFlag.bigint)) {
      return B_next(input, `"\\""+${input.i}+"\\""`, expectedSchema);
    } else if (Flag.unsafeHas(inputTagFlag, Flag.with(TagFlag.object, TagFlag.array))) {
      const jsonVal = parse(B_refine(input, undefined, undefined, json()));
      return B_next(
        jsonVal,
        `JSON.stringify(${jsonVal.i}${
          expectedSchema.space === 0 || expectedSchema.space === undefined
            ? ""
            : `,null,${expectedSchema.space}`
        })`,
        expectedSchema,
        expectedSchema,
      );
    } else {
      return B_unsupportedDecode(input, input.s, expectedSchema);
    }
  };

  return (): Internal =>
    cached("json", stringTag, (s) => {
      s.format = "json";
      s.name = `${jsonName} string`;
      s.encoder = jsonStringEncoder;
      s.decoder = jsonStringDecoder;
    });
})();

export function jsonStringWithSpace(space: number): Internal {
  const mut = copySchema(jsonString());
  mut.space = space;
  return mut;
}

export function uint8Array(): Internal {
  return cached("u", instanceTag, (s) => {
    s.class = Uint8Array;
    s.decoder = (inputArg: Val): Val => {
      const inputTagFlag = TagFlag.get(inputArg.s.type);
      let input = inputArg;

      if (Flag.unsafeHas(inputTagFlag, TagFlag.string)) {
        input = B_next(
          input,
          `${B_embed(input, new TextEncoder() as unknown)}.encode(${input.i})`,
          s,
        );
      } else if (Flag.unsafeHas(inputTagFlag, Flag.with(TagFlag.unknown, TagFlag.instance))) {
        input = instanceDecoder(input);
      }

      if (inputArg.e.to !== undefined && inputArg.e.parser === undefined) {
        const to = inputArg.e.to;
        const toTagFlag = TagFlag.get(to.type);
        if (Flag.unsafeHas(toTagFlag, TagFlag.string)) {
          input = B_next(
            input,
            `${B_embed(input, new TextDecoder() as unknown)}.decode(${input.i})`,
            string(),
          );
        }
        return input;
      } else {
        return input;
      }
    };
  });
}

export function isoDateTime(): Internal {
  return cached("date-time", stringTag, (s) => {
    const datetimeRe = /^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(\.\d+)?Z$/;
    s.decoder = stringDecoderFn;
    s.format = "date-time";
    s.refiner = (input) => {
      return [
        {
          c: (inputVar) => `${B_embed(input, datetimeRe)}.test(${inputVar})`,
          f: B_failWithErrorMessage(
            "format",
            "Invalid datetime string! Expected UTC",
          ),
        },
      ];
    };
  });
}

export function port(): Internal {
  return cached("port", numberTag, (s) => {
    s.decoder = numberDecoder;
    s.format = "port";
    s.refiner = (_input) => {
      return [
        {
          c: (inputVar) => `${inputVar}>0&&${inputVar}<65536&&${inputVar}%1===0`,
          f: B_failWithErrorMessage("format"),
        },
      ];
    };
  });
}

export function email(): Internal {
  return cached("email", stringTag, (s) => {
    const emailRegex = /^(?!\.)(?!.*\.\.)([A-Z0-9_'+\-\.]*)[A-Z0-9_+-]@([A-Z0-9][A-Z0-9\-]*\.)+[A-Z]{2,}$/i;
    s.decoder = stringDecoderFn;
    s.format = "email";
    s.refiner = (input) => {
      return [
        {
          c: (inputVar) => `${B_embed(input, emailRegex)}.test(${inputVar})`,
          f: B_failWithErrorMessage("format"),
        },
      ];
    };
  });
}

export function uuid(): Internal {
  return cached("uuid", stringTag, (s) => {
    const uuidRegex = /^[0-9a-fA-F]{8}\b-[0-9a-fA-F]{4}\b-[0-9a-fA-F]{4}\b-[0-9a-fA-F]{4}\b-[0-9a-fA-F]{12}$/i;
    s.decoder = stringDecoderFn;
    s.format = "uuid";
    s.refiner = (input) => {
      return [
        {
          c: (inputVar) => `${B_embed(input, uuidRegex)}.test(${inputVar})`,
          f: B_failWithErrorMessage("format"),
        },
      ];
    };
  });
}

export function cuid(): Internal {
  return cached("cuid", stringTag, (s) => {
    const cuidRegex = /^c[^\s-]{8,}$/i;
    s.decoder = stringDecoderFn;
    s.format = "cuid";
    s.refiner = (input) => {
      return [
        {
          c: (inputVar) => `${B_embed(input, cuidRegex)}.test(${inputVar})`,
          f: B_failWithErrorMessage("format"),
        },
      ];
    };
  });
}

export function url(): Internal {
  return cached("url", stringTag, (s) => {
    const urlValidator: unknown = (s: string) => {
      try {
        new URL(s);
        return true;
      } catch (_) {
        return false;
      }
    };
    s.decoder = stringDecoderFn;
    s.format = "url";
    s.refiner = (input) => {
      return [
        {
          c: (inputVar) => `${B_embed(input, urlValidator)}(${inputVar})`,
          f: B_failWithErrorMessage("format"),
        },
      ];
    };
  });
}

export function invalidDateRefine(input: Val): Val {
  return B_refine(input, input.e, [
    {
      c: (inputVar) => `!Number.isNaN(${inputVar}.getTime())`,
      f: failInvalidType,
    },
  ]);
}

export function date(): Internal {
  return cached(instanceTag as string, instanceTag, (s) => {
    s.class = Date;
    s.decoder = (input: Val): Val => {
      const inputTagFlag = TagFlag.get(input.s.type);
      if (Flag.unsafeHas(inputTagFlag, TagFlag.string)) {
        return invalidDateRefine(B_next(input, `new Date(${input.i})`, s));
      } else if (Flag.unsafeHas(inputTagFlag, TagFlag.unknown)) {
        return invalidDateRefine(instanceDecoder(input));
      } else if (Flag.unsafeHas(inputTagFlag, TagFlag.instance) && input.s.class === s.class) {
        return input;
      } else {
        return B_unsupportedDecode(input, input.s, input.e);
      }
    };

    // Encoder: Date → string (via toISOString) when target is string
    s.encoder = (input, target) => {
      const toTagFlag = TagFlag.get(target.type);
      if (Flag.unsafeHas(toTagFlag, TagFlag.string)) {
        const dateTimeString = baseSchema(stringTag, false);
        dateTimeString.format = "date-time";
        return parse(
          B_next(input, `${input.i}.toISOString()`, dateTimeString, target),
        );
      } else {
        return input;
      }
    };
  });
}

export function to(from: Internal, target: Internal): Internal {
  // It makes sense, since S.to quite often will be used
  // inside of a framework where we don't control what's the to argument
  if (from === target) {
    return from;
  } else {
    return updateOutput(from, (mut) => {
      mut.to = target;
      // A tricky part about parser is that we don't know the input type in ReScript
      // so we need to directly parse to output instead of input
      // switch parser {
      // | Some(p) =>
      //   mut.parser = Some(
      //     Builder.make((b, ~input, , ~path as _) => {
      //       // TODO: Support async, reverse, nested parsing
      //       b->B_embedSyncOperation(~input, ~fn=p)
      //     }),
      //   )
      // | None => ()
      // }
    });
  }
}

// PORT-NOTE: ReScript list runtime (v12): empty list = `0`, cons cell =
// `{hd, tl}`. These two helpers replicate Stdlib List.fromArray / List.toArray
// exactly for that representation.
type RescriptList = 0 | { hd: unknown; tl: RescriptList };

function listFromArray(array: unknown[]): RescriptList {
  let list: RescriptList = 0;
  for (let i = array.length - 1; i >= 0; i--) {
    list = { hd: array[i], tl: list };
  }
  return list;
}

function listToArray(list: RescriptList): unknown[] {
  const array: unknown[] = [];
  let current = list;
  while (current !== 0) {
    array.push(current.hd);
    current = current.tl;
  }
  return array;
}

export function list(schema: Internal): Internal {
  return transform(array(schema), (_: unknown) => ({
    p: (array: unknown) => listFromArray(array as unknown[]),
    s: (list: unknown) => listToArray(list as RescriptList),
  })) as unknown as Internal;
}

export type Meta<Value> = {
  name?: string;
  title?: string;
  description?: string;
  deprecated?: boolean;
  examples?: Value[];
  errorMessage?: SchemaErrorMessage;
};

// TODO: Better test reverse
export function meta<Value>(schema: Internal, data: Meta<Value>): Internal {
  const mut = copySchema(schema);
  if (data.name !== undefined) {
    if (data.name === "") {
      mut.name = undefined;
    } else {
      mut.name = data.name;
    }
  }
  if (data.title !== undefined) {
    if (data.title === "") {
      mut.title = undefined;
    } else {
      mut.title = data.title;
    }
  }
  if (data.description !== undefined) {
    if (data.description === "") {
      mut.description = undefined;
    } else {
      mut.description = data.description;
    }
  }
  if (data.deprecated !== undefined) {
    mut.deprecated = data.deprecated;
  }
  if (data.examples !== undefined) {
    if (data.examples.length === 0) {
      mut.examples = undefined; // FIXME: Delete instead of None
    } else {
      mut.examples = data.examples.map(
        getDecoder(reverse(schema)) as unknown as (example: Value) => unknown,
      );
    }
  }
  if (data.errorMessage !== undefined) {
    const em = data.errorMessage;
    const emDict = em as unknown as Record<string, string>;
    if (Object.keys(emDict).length === 0) {
      mut.errorMessage = undefined;
    } else {
      mut.errorMessage = em;
    }
  }
  return mut;
}

export function brand(schema: Internal, id: string): Internal {
  const mut = copySchema(schema);
  mut.name = id;
  return mut;
}
// =============================================================================
// Fragment 08 — module Schema (Sury.res lines 5485-6177)
// `module Schema` (definition-to-schema factory) + `schema`, `js_schema`,
// `literal`, `enum`.
//
// TODO(integration): expects from other sections:
//   - getOutputSchema
//   - objectDecoder, arrayDecoder, literalDecoder
//   - optionFactory, OptionModule (Option_getOr)
//   - unit (the unit schema factory, `unit()`)
//   - makeObjectVal, completeObjectVal
//   - valGet
//   - parse, reverse
//   - Literal (Literal_parse)
//   - unionFactory
//   - B (B_Val_scope, B_Val_Object_add, B_Val_Object_merge, B_nextConst,
//     _notVarAtParent, B_markOutput, B_merge, B_inlineLocation,
//     B_invalidOperation)
//   - ObjectCtx type (module Object's `Object.s` ctx record — runtime keys:
//     f/fieldOr/tag/nested/flatten) from the Object section
//   - TupleCtx type (module Tuple's `Tuple.s` — runtime keys: item/tag)
//
// From the prelude: Internal, Val, baseSchema, copySchema, updateOutput,
// globalConfig, InternalError, itemSymbol, isLiteral, isSchemaObject,
// toExpression, typeOf, objectTag, arrayTag, instanceTag,
// inlinedValueFromString, immutableEmptyArray, pathEmpty, pathConcat,
// pathFromInlinedLocation, Path.
//
// PORT-NOTE: `module Schema` is exported as `SchemaModule` (the name `Schema`
// is taken by the schema constructor in the prelude). Its members are defined
// as standalone functions (mutual recursion between shape/nested/object/
// definitionToSchema/... is awkward inside an object literal) with
// `schema`-prefixed names where the bare name would collide with other
// sections (`schemaShape`, `schemaNested`, `schemaObject`, `schemaTuple`,
// `schemaFactory`), then attached to `SchemaModule` so call sites read
// `schemaFactory`, `schemaObject`, etc.
// =============================================================================

// module Schema = {

// type rec shapedSerializerAcc — internal accumulator for shapedSerializer.
// Field names are the runtime names (no @as on this record).
type ShapedSerializerAcc = {
  val?: Val;
  properties?: Record<string, ShapedSerializerAcc>;
  flattened?: ShapedSerializerAcc[];
};

// Schema.s — the ctx passed to S.schema's definer. @as("m") matches.
export type SchemaCtx = {
  m: (schema: Internal) => unknown;
};

const inputFrom = immutableEmptyArray as string[];

// advancedObjectCtx:
//   Public API for JS/TS users.
//   It shouldn't be used from ReScript and
//   needed only because we use @as for field to reduce bundle-size
//   of ReScript compiled code
// PORT-NOTE: lifted to top level as `AdvancedObjectCtx`. Runtime keys:
// `field` (@as("field") _jsField) plus the spread `...Object.s` keys
// (`f` for field via @as("f"), fieldOr, tag, nested, flatten).
export type AdvancedObjectCtx = {
  field: (fieldName: string, schema: Internal) => unknown;
  f: (fieldName: string, schema: Internal) => unknown;
  fieldOr: (fieldName: string, schema: Internal, or: unknown) => unknown;
  tag: (tag: string, asValue: unknown) => void;
  nested: (fieldName: string) => AdvancedObjectCtx;
  flatten: (schema: Internal) => unknown;
};

// module Definition = { ... } — both members are @inline one-liners, inlined
// at use sites below per conventions:
//   isNode(definition) = definition->typeof === objectTag && definition !== null
//   toEmbededItem(definition) = definition[itemSymbol]

function proxifyShapedSchema(schema: Internal, from: string[], fromFlattened?: number): unknown {
  const mut = copySchema(getOutputSchema(schema));
  mut.from = from;
  if (fromFlattened !== undefined) {
    mut.fromFlattened = fromFlattened;
  }
  return new Proxy(mut as unknown as object, {
    get(target: Internal, prop) {
      if (prop === (itemSymbol as unknown)) {
        return target;
      } else {
        const location = prop as unknown as string;

        let maybeField: Internal | undefined;
        if (target.properties !== undefined) {
          maybeField = target.properties[location];
        } else if (target.items !== undefined) {
          // If there are no properties, then it must be Tuple
          maybeField = target.items[location as unknown as number];
        } else {
          maybeField = undefined;
        }
        if (maybeField === undefined) {
          InternalError.panic(`Cannot read property "${location}" of ${toExpression(target)}`);
        }

        return proxifyShapedSchema(
          maybeField!,
          target.from!.concat(location),
          target.fromFlattened
        );
      }
    },
  } as ProxyHandler<object>);
}

export function schemaShape<Value>(schema: Internal, definer: (value: unknown) => unknown): Value {
  return updateOutput<Value>(schema, (mut) => {
    const fromProxy = proxifyShapedSchema(mut, inputFrom);
    const definition: unknown = definer(fromProxy);
    if (definition === fromProxy) {
      // ()
    } else {
      mut.parser = shapedParser;
      mut.to = definitionToShapedSchema(definition);
    }
  });
}

export function schemaNested(this: Record<string, unknown>, fieldName: string): AdvancedObjectCtx {
  const parentCtx = this as unknown as AdvancedObjectCtx & Record<string, unknown>; // TODO: Add a check that it's binded?
  const cacheId = `~${fieldName}`;

  const cachedCtx = parentCtx[cacheId] as AdvancedObjectCtx | undefined;
  if (cachedCtx !== undefined) {
    return cachedCtx;
  } else {
    const properties: Record<string, Internal> = {};
    const required: string[] = [];
    let schema: Internal;
    {
      const s = baseSchema(objectTag, false);
      s.required = required;
      s.properties = properties;
      s.additionalItems = globalConfig.a;
      s.decoder = objectDecoder;
      schema = s;
    }

    const parentSchema: Internal = (parentCtx.f(fieldName, schema) as Record<symbol, Internal>)[
      itemSymbol
    ]!;

    const field = (fieldName: string, schema: Internal): unknown => {
      const inlinedLocation = inlinedValueFromString(fieldName);
      if (fieldName in properties) {
        InternalError.panic(`The field ${inlinedLocation} defined twice`);
      }
      required.push(fieldName);
      properties[fieldName] = schema;
      return proxifyShapedSchema(
        schema,
        parentSchema.from!.concat(fieldName),
        parentSchema.fromFlattened
      );
    };

    const tag = (tag: string, asValue: unknown): void => {
      field(tag, definitionToSchema(asValue));
    };

    const fieldOr = (fieldName: string, schema: Internal, or: unknown): unknown => {
      return field(fieldName, Option_getOr(optionFactory(schema), or));
    };

    const flatten = (schema: Internal): unknown => {
      if (schema.type === objectTag) {
        const flattenedProperties = schema.properties;
        const to = schema.to;
        if (to as unknown as boolean) {
          InternalError.panic(
            `Unsupported nested flatten for transformed object schema ${toExpression(schema)}`
          );
        }
        const flattenedKeys = Object.keys(flattenedProperties!);
        const result: Record<string, unknown> = {};
        for (let idx = 0; idx < flattenedKeys.length; idx++) {
          const key = flattenedKeys[idx]!;
          result[key] = field(key, flattenedProperties![key]!);
        }
        return result;
      } else {
        return InternalError.panic(`Can't flatten ${toExpression(schema)} schema`);
      }
    };

    const ctx: AdvancedObjectCtx = {
      // js/ts methods
      field,
      // methods
      f: field,
      fieldOr,
      tag,
      nested: schemaNested,
      flatten,
    };

    (parentCtx as Record<string, unknown>)[cacheId] = ctx;

    return ctx;
  }
}

export function schemaObject(definer: (ctx: AdvancedObjectCtx) => unknown): Internal {
  let flattened: Internal[] | undefined = void 0;
  const properties: Record<string, Internal> = {};

  const flatten = (schema: Internal): unknown => {
    if (schema.type === objectTag) {
      const flattenedProperties = schema.properties!;
      const flattenedKeys = Object.keys(flattenedProperties);
      for (let idx = 0; idx < flattenedKeys.length; idx++) {
        const key = flattenedKeys[idx]!;
        const flattenedSchema = flattenedProperties[key]!;
        const existing = properties[key];
        if (existing !== undefined && existing === flattenedSchema) {
          // ()
        } else if (existing !== undefined) {
          InternalError.panic(`The field "${key}" defined twice with incompatible schemas`);
        } else {
          properties[key] = flattenedSchema;
        }
      }
      const f = flattened || (flattened = []);
      return proxifyShapedSchema(schema, inputFrom, f.push(schema) - 1);
    } else {
      return InternalError.panic(`The '${toExpression(schema)}' schema can't be flattened`);
    }
  };

  const field = (fieldName: string, schema: Internal): unknown => {
    if (fieldName in properties) {
      InternalError.panic(`The field "${fieldName}" defined twice with incompatible schemas`);
    }
    properties[fieldName] = schema;
    return proxifyShapedSchema(schema, [fieldName]);
  };

  const tag = (tag: string, asValue: unknown): void => {
    field(tag, definitionToSchema(asValue));
  };

  const fieldOr = (fieldName: string, schema: Internal, or: unknown): unknown => {
    return field(fieldName, Option_getOr(optionFactory(schema), or));
  };

  const ctx: AdvancedObjectCtx = {
    // js/ts methods
    field,
    // methods
    f: field,
    fieldOr,
    tag,
    nested: schemaNested,
    flatten,
  };

  const definition = definer(ctx);

  const mut = baseSchema(objectTag, false);
  mut.required = Object.keys(properties);
  mut.properties = properties;
  mut.additionalItems = globalConfig.a;
  mut.decoder = objectDecoder;
  mut.parser = shapedParser;
  mut.to = definitionToShapedSchema(definition);
  if (flattened !== undefined) {
    mut.flattened = flattened;
  }
  return mut;
}

export function schemaTuple(definer: (ctx: TupleCtx) => unknown): Internal {
  const items: Internal[] = [];

  const item = (idx: number, schema: Internal): unknown => {
    const location = String(idx);
    if (items[idx] as unknown as boolean) {
      return InternalError.panic(`The item [${location}] is defined multiple times`);
    } else {
      items[idx] = schema;
      return proxifyShapedSchema(schema, [String(idx)]);
    }
  };

  const tag = (idx: number, asValue: unknown): void => {
    item(idx, definitionToSchema(asValue));
  };

  const ctx: TupleCtx = {
    item,
    tag,
  };

  const definition = definer(ctx);

  for (let idx = 0; idx < items.length; idx++) {
    if (!items[idx]) {
      items[idx] = unit();
    }
  }

  const mut = baseSchema(arrayTag, false);
  mut.items = items;
  mut.additionalItems = "strict";
  mut.decoder = arrayDecoder;
  mut.parser = shapedParser;
  mut.to = definitionToShapedSchema(definition);
  return mut;
}

function getValByFrom(input: Val, from: string[], idx: number): Val {
  // FIXME: TODO: something with flattened
  const key = from[idx];
  if (key !== undefined) {
    return getValByFrom(input.d![key]!, from, idx + 1);
  } else {
    return input;
  }
}

// Assemble an object/tuple val from a per-location field producer. Shared by
// the shaped-parser reshape (reads each child via `from` paths) and the
// flatten reuse path (reads each key from the parent's decoded `vals`).
function assembleShapedObject(
  input: Val,
  schema: Internal,
  field: (location: string, childSchema: Internal) => Val
): Val {
  const output = makeObjectVal(input, schema);
  output.io = true;
  if (schema.items !== undefined) {
    const items = schema.items;
    for (let idx = 0; idx < items.length; idx++) {
      const location = String(idx);
      B_Val_Object_add(output, location, field(location, items[idx]!));
    }
  } else if (schema.properties !== undefined) {
    const properties = schema.properties;
    const keys = Object.keys(properties);
    for (let idx = 0; idx < keys.length; idx++) {
      const location = keys[idx]!;
      B_Val_Object_add(output, location, field(location, properties[location]!));
    }
  } else {
    // FIXME: Use a path
    InternalError.panic(`Don't know where the value is coming from: ${toExpression(schema)}`);
  }
  return completeObjectVal(output);
}

function getShapedParserOutput(input: Val, targetSchema: Internal): Val {
  let v: Val;
  if (targetSchema.fromFlattened !== undefined) {
    v = B_Val_scope(
      getValByFrom(input.fv![targetSchema.fromFlattened]!, targetSchema.from!, 0)
    );
  } else if (targetSchema.from !== undefined) {
    v = B_Val_scope(getValByFrom(input, targetSchema.from, 0));
  } else if (isLiteral(targetSchema)) {
    v = B_nextConst(input, targetSchema);
  } else {
    v = assembleShapedObject(input, targetSchema, (_location, childSchema) =>
      getShapedParserOutput(input, childSchema)
    );
  }
  v.prev = undefined;
  v.e = targetSchema;
  return v;
}

export function shapedParser(input: Val): Val {
  const flattened = input.e.flattened;
  if (flattened !== undefined) {
    const flattenedVals: Val[] = [];
    for (let idx = 0; idx < flattened.length; idx++) {
      const flattenedSchema = flattened[idx]!;
      // The flattened object's keys are merged into the parent's properties and
      // already decoded by the parent objectDecoder, so `input` holds their
      // decoded vals. Reuse them here instead of decoding again — re-decoding
      // would re-apply field-level transforms on the already-transformed value
      // (issue #271).
      let flattenedVal: Val;
      if (flattenedSchema.to !== undefined) {
        // The flattened schema has its own reshape/transform. Mark the input as
        // output so the parse loop skips the decoder and runs only that `.to`,
        // reading the decoded fields back through the shared `vals`.
        const flattenedInput = B_Val_scope(input);
        flattenedInput.e = flattenedSchema;
        flattenedInput.io = true;
        flattenedVal = parse(flattenedInput);
      } else {
        // No reshape: project the flattened schema's own keys out of the
        // parent's decoded fields (selection without decoding), then apply the
        // flattened schema's own refiners. Materializing the projection gives it
        // an inline restricted to its keys, so a whole-object read of the
        // flattened result can't leak sibling fields of the parent.
        const assembled = assembleShapedObject(input, flattenedSchema, (location, _childSchema) =>
          valGet(input, location)
        );
        assembled.e = flattenedSchema;
        // The reused field vals are declared by the parent's own code; detach
        // from `prev` (as getShapedParserOutput does) so `merge` doesn't
        // re-emit the parent's declarations. Done before markOutput so any
        // refiner wrap it adds still points at the assembled object.
        assembled.prev = undefined;
        flattenedVal = B_markOutput(assembled, assembled);
      }
      flattenedVals.push(flattenedVal);
      input.cp = input.cp + B_merge(flattenedVal);
    }
    input.fv = flattenedVals;
  }

  const targetSchema = input.e.to!;
  const output = getShapedParserOutput(input, targetSchema);
  output.t = true;
  output.prev = input;
  return B_markOutput(output, input);
}

function prepareShapedSerializerAcc(acc: ShapedSerializerAcc, input: Val): void {
  if (input.e.from !== undefined) {
    const from = input.e.from;
    const fromFlattened = input.e.fromFlattened;
    let accAtFrom: ShapedSerializerAcc;
    if (fromFlattened !== undefined) {
      if (acc.flattened === undefined) {
        acc.flattened = [];
      }
      const existing = acc.flattened[fromFlattened];
      if (existing === undefined) {
        const newAcc: ShapedSerializerAcc = {};
        acc.flattened[fromFlattened] = newAcc;
        accAtFrom = newAcc;
      } else {
        accAtFrom = existing;
      }
    } else {
      accAtFrom = acc;
    }
    for (let idx = 0; idx < from.length; idx++) {
      const key = from[idx]!;
      let p: Record<string, ShapedSerializerAcc>;
      if (accAtFrom.properties !== undefined) {
        p = accAtFrom.properties;
      } else {
        p = {};

        accAtFrom.properties = p;
      }
      const existingAcc = p[key];
      if (existingAcc !== undefined) {
        accAtFrom = existingAcc;
      } else {
        const newAcc: ShapedSerializerAcc = {};
        p[key] = newAcc;
        accAtFrom = newAcc;
      }
    }
    accAtFrom.val = input;
  } else if (input.d !== undefined) {
    const vals = input.d;
    const keys = Object.keys(vals);
    for (let idx = 0; idx < keys.length; idx++) {
      prepareShapedSerializerAcc(acc, vals[keys[idx]!]!);
    }
  }
}

function getShapedSerializerOutput(
  input: Val,
  acc: ShapedSerializerAcc | undefined,
  targetSchema: Internal,
  path: Path
): Val {
  if (acc !== undefined && acc.val !== undefined) {
    const v = B_Val_scope(acc.val);
    v.t = true;
    v.s = targetSchema;
    v.e = targetSchema;
    return parse(v);
  } else {
    if (isLiteral(targetSchema)) {
      const v = B_nextConst(input, targetSchema, targetSchema);
      v.prev = undefined;
      v.p = input;
      v.v = _notVarAtParent;
      v.io = true;
      return parse(v);
    } else {
      // When acc is None (discriminant field with no input), follow the to chain
      // to get the actual output schema properties (e.g., for reversed transformed objects)
      const resolvedTargetSchema = acc === undefined ? getOutputSchema(targetSchema) : targetSchema;
      const v = makeObjectVal(input, resolvedTargetSchema);
      v.e = resolvedTargetSchema;
      v.io = true;
      v.prev = undefined;
      v.p = input;
      v.v = _notVarAtParent;

      if (
        resolvedTargetSchema.items !== undefined &&
        !(acc === undefined && typeOf(resolvedTargetSchema.additionalItems) === objectTag)
      ) {
        const items = resolvedTargetSchema.items;
        for (let idx = 0; idx < items.length; idx++) {
          const location = String(idx);
          B_Val_Object_add(
            v,
            location,
            getShapedSerializerOutput(
              input,
              acc !== undefined && acc.properties !== undefined
                ? acc.properties[location]
                : undefined,
              items[idx]!,
              pathConcat(path, pathFromInlinedLocation(B_inlineLocation(input.g, location)))
            )
          );
        }
      } else if (
        resolvedTargetSchema.properties !== undefined &&
        !(acc === undefined && typeOf(resolvedTargetSchema.additionalItems) === objectTag)
      ) {
        const properties = resolvedTargetSchema.properties;
        const flattened = resolvedTargetSchema.flattened;
        if (flattened !== undefined && acc !== undefined && acc.flattened !== undefined) {
          const flattenedSchemas = flattened;
          const flattenedAcc = acc.flattened;
          flattenedAcc.forEach((acc, idx) => {
            const flattenedOutput = getShapedSerializerOutput(
              input,
              acc,
              reverse(flattenedSchemas[idx]!),
              path
            );
            B_Val_Object_merge(v, flattenedOutput.d!);
          });
        }

        const keys = Object.keys(properties);
        for (let idx = 0; idx < keys.length; idx++) {
          const location = keys[idx]!;

          // Skip fields added by flattened
          if (!(location in v.d!)) {
            B_Val_Object_add(
              v,
              location,
              getShapedSerializerOutput(
                input,
                acc !== undefined && acc.properties !== undefined
                  ? acc.properties[location]
                  : undefined,
                properties[location]!,
                pathConcat(path, pathFromInlinedLocation(B_inlineLocation(input.g, location)))
              )
            );
          }
        }
      } else {
        // PORT-NOTE: the source shadows `path` here; renamed to `path2` (TS
        // can't redeclare a parameter in the same scope).
        const path2 =
          targetSchema.from !== undefined
            ? path + targetSchema.from.map((item) => `["${item}"]`).join("")
            : path;
        B_invalidOperation(
          input,
          `Missing input for ${toExpression(targetSchema)}` +
            (path2 === "" ? "" : ` at ${path2}`)
        );
      }

      return completeObjectVal(v);
    }
  }
}

export function shapedSerializer(input: Val): Val {
  const acc: ShapedSerializerAcc = {};
  prepareShapedSerializerAcc(acc, input);

  const targetSchema = input.e.to!;
  const output = getShapedSerializerOutput(input, acc, targetSchema, pathEmpty);
  output.t = true;
  output.prev = input;
  return output;
}

function definitionToShapedSchema(definition: unknown): Internal {
  const s = copySchema(
    traverseDefinition(
      definition,
      // Definition.toEmbededItem
      (definition: unknown) =>
        (definition as Record<symbol, Internal | undefined>)[itemSymbol]
    )
  );
  s.serializer = shapedSerializer;
  return s;
}

export function definitionToSchema(definition: unknown): Internal {
  return traverseDefinition(definition, (node) => {
    if (isSchemaObject(node)) {
      return node as unknown as Internal;
    } else {
      return undefined;
    }
  });
}

function traverseDefinition(
  definition: unknown,
  onNode: (node: unknown) => Internal | undefined
): Internal {
  // Definition.isNode
  if (typeOf(definition) === objectTag && definition !== null) {
    const s = onNode(definition);
    if (s !== undefined) {
      return s;
    } else {
      if (Array.isArray(definition)) {
        const node = definition as unknown[];
        for (let idx = 0; idx < node.length; idx++) {
          const schema = traverseDefinition(node[idx], onNode);
          node[idx] = schema as unknown;
        }
        const items = node as unknown as Internal[];

        const mut = baseSchema(arrayTag, false);
        mut.items = items;
        mut.additionalItems = "strict";
        mut.decoder = arrayDecoder;
        return mut;
      } else {
        const cnstr = (definition as Record<string, unknown>)["constructor"];
        if ((cnstr as unknown as boolean) && cnstr !== Object) {
          const mut = baseSchema(instanceTag, true);
          mut.class = cnstr;
          mut.const = definition;
          mut.decoder = literalDecoder;
          return mut;
        } else {
          const node = definition as Record<string, unknown>;
          const fieldNames = Object.keys(node);
          const length = fieldNames.length;
          for (let idx = 0; idx < length; idx++) {
            const location = fieldNames[idx]!;
            const schema = traverseDefinition(node[location], onNode);
            node[location] = schema as unknown;
          }
          const mut = baseSchema(objectTag, false);
          mut.required = fieldNames;
          mut.properties = node as unknown as Record<string, Internal>;
          mut.additionalItems = globalConfig.a;
          mut.decoder = objectDecoder;
          return mut;
        }
      }
    }
  } else {
    return Literal_parse(definition);
  }
}

function schemaMatches(schema: Internal): unknown {
  return schema as unknown;
}
const schemaCtx: SchemaCtx = {
  m: schemaMatches,
};
export function schemaFactory(definer: (ctx: unknown) => unknown): Internal {
  return definitionToSchema(definer(schemaCtx as unknown));
}

// PORT-NOTE: `module Schema` exported as SchemaModule (name `Schema` is taken
// by the schema constructor in the prelude). Members defined as standalone
// functions above and attached here so call sites can use SchemaModule.*.

// } — end module Schema

// Identifier aliases (not `schemaFactory` property reads) so esbuild
// can tree-shake: a property-read initializer is treated as possibly
// side-effectful and would retain the whole schema machinery in every bundle.
export const schema = schemaFactory;

export function js_schema(definition: unknown): Internal {
  return definitionToSchema(definition);
}
export const literal = js_schema;

// PORT-NOTE: `enum` is a reserved word in TS — defined as `enum_` and
// re-exported under the name `enum` (legal as an export alias).
function enum_(values: unknown[]): Internal {
  return unionFactory(values.map(literal));
}
export { enum_ as enum };
// =============================================================================
// Section 09 — Sury.res lines 6179-6943
// compactColumnsDecoder, compactColumns, public aliases (object, nullAsOption,
// null_, array alias, dict, shape, tuple, tuple1/2/3, union), assertNumber and
// built-in refinements (intMin/intMax/floatMin/floatMax, array*/string* length
// refinements, pattern, trim, nullable, nullableAsOption).
//
// TODO(integration): expects `B` (Builder helpers: refine, next, markOutput,
//   varWithoutAllocation, Val.scope, Val.var, _notVarBeforeValidation, _var,
//   asyncVal, merge, inlineLocation, embed, failInvalidType,
//   failWithErrorMessage) from the Builder section.
// TODO(integration): expects `parse` from the parse-loop section.
// TODO(integration): expects `array` (the array factory) from the array section.
// TODO(integration): expects `SchemaModule` (object, shape, tuple,
//   definitionToSchema) from the Schema factory section (~5485).
// TODO(integration): expects `optionFactory`, `unionFactory`, `dictFactory`,
//   `unit`, `nullAsUnit`, `nullLiteral` from earlier sections.
// TODO(integration): expects `internalRefine` and `getMutErrorMessage` from
//   section 06.
// TODO(integration): expects `transform` from the transform section.
// =============================================================================

export function compactColumnsDecoder(input: Val): Val {
  const selfSchema = input.e;
  const isUnknownInput = Flag.unsafeHas(
    TagFlag.get(input.s.type) as unknown as Flag,
    TagFlag.unknown as unknown as Flag,
  );

  // Find the object schema whose properties define the columns.
  // Forward (columnar → rows): props come from selfSchema.to.additionalItems.
  // Reverse (rows → columnar): props come from input.schema.additionalItems (the
  // object schema left over after the preceding parse pipeline step).
  let forwardProps: Record<string, Internal> | undefined;
  if (
    selfSchema.to !== undefined &&
    typeof selfSchema.to.additionalItems === "object"
  ) {
    forwardProps = (selfSchema.to.additionalItems as Internal).properties;
  } else {
    forwardProps = undefined;
  }
  const isForwardDirection = forwardProps as unknown as boolean;
  let maybeProperties: Record<string, Internal> | undefined;
  if (isForwardDirection) {
    maybeProperties = forwardProps;
  } else {
    if (
      input.s.additionalItems !== undefined &&
      typeof input.s.additionalItems === "object"
    ) {
      maybeProperties = (input.s.additionalItems as Internal).properties;
    } else {
      maybeProperties = undefined;
    }
  }

  if (maybeProperties === undefined) {
    return InternalError.panic(
      "S.compactColumns supports only object schemas. Use S.compactColumns(S.unknown)->S.to(S.array(objectSchema)).",
    );
  } else {
    const properties = maybeProperties;
    const keys = Object.keys(properties);
    const keysLen = keys.length;

    // Forward: output already matches selfSchema.to, reuse it so
    // markOutput picks up its refiner. selfSchema.to is Some here —
    // isForwardDirection reads through it above.
    // Reverse: runtime shape differs (array of arrays of unknown),
    // so build fresh and propagate .to for downstream steps.
    let outputSchema: Internal;
    if (isForwardDirection) {
      outputSchema = selfSchema.to!;
    } else {
      const s = array(array(unknown)) as unknown as Internal;
      s.to = selfSchema.to;
      outputSchema = s;
    }

    if (keysLen === 0) {
      let input2 = input;
      if (isUnknownInput) {
        input2 = B_refine(input, undefined, [
          {
            c: (inputVar: string) =>
              `Array.isArray(${inputVar})&&${inputVar}.length===0`,
            f: failInvalidType,
          },
        ]);
      }
      const output = B_next(input2, "[]", outputSchema, outputSchema);
      return B_markOutput(output, input2);
    } else if (isForwardDirection) {
      // Forward direction: columnar → rows
      let input2 = input;
      if (isUnknownInput) {
        input2 = B_refine(input, undefined, [
          {
            c: (inputVar: string) => {
              let check = `Array.isArray(${inputVar})&&${inputVar}.length===${keysLen}`;
              for (let idx = 0; idx <= keysLen - 1; ++idx) {
                check = check + `&&Array.isArray(${inputVar}[${idx}])`;
              }
              return check;
            },
            f: failInvalidType,
          },
        ]);
      }

      const inputVar = input2.v();
      const iteratorVar = B_varWithoutAllocation(input2.g);
      const outputVar = B_varWithoutAllocation(input2.g);

      // Declared source item type from selfSchema (the compactColumns schema).
      const declaredItemSchema: Internal = (() => {
        const innerArray: Internal = selfSchema.additionalItems as unknown as Internal;
        return innerArray.additionalItems as unknown as Internal;
      })();

      // Actual runtime item type: unknown for top-level parser, or
      // the typed source when the caller passed already-typed data.
      let runtimeItemSchema: Internal;
      if (isUnknownInput) {
        runtimeItemSchema = unknown;
      } else {
        const innerArray: Internal = input2.s.additionalItems as unknown as Internal;
        runtimeItemSchema = innerArray.additionalItems as unknown as Internal;
      }

      let lengthCode = "";
      let itemBuildCode = "";
      let itemParseCode = "";
      let asyncInlines = "";
      let hasAsync = false;
      for (let idx = 0; idx <= keysLen - 1; ++idx) {
        const key = keys[idx]!;
        const idxStr = `${idx}`;
        const rawValueCode = `${inputVar}[${idxStr}][${iteratorVar}]`;

        const fieldSchema = properties[key]!;

        // When the declared source differs from the runtime type
        // (e.g. runtime=unknown, declared=json), chain through the
        // declared type first so parse validates the value matches
        // the source schema before converting to the field type.
        let itemExpected: Internal;
        if (declaredItemSchema !== runtimeItemSchema) {
          const chained = copySchema(declaredItemSchema);
          chained.to = fieldSchema;
          itemExpected = chained;
        } else {
          itemExpected = fieldSchema;
        }

        const itemInput = B_Val_scope(input2);
        itemInput.i = rawValueCode;
        itemInput.s = runtimeItemSchema;
        itemInput.e = itemExpected;
        itemInput.v = _notVarBeforeValidation;
        itemInput.io = false;

        // Path like ["bar"] so validation errors carry the field location.
        itemInput.path = pathFromInlinedLocation(B_inlineLocation(input2.g, key));

        const itemOutput = parse(itemInput);
        if (
          Flag.unsafeHas(
            itemOutput.f as unknown as Flag,
            ValFlag.async as unknown as Flag,
          )
        ) {
          hasAsync = true;
        }

        itemParseCode = itemParseCode + B_merge(itemOutput);
        lengthCode = lengthCode + `${inputVar}[${idxStr}].length,`;
        asyncInlines = asyncInlines + `${itemOutput.i},`;
        itemBuildCode =
          itemBuildCode + `${inlinedValueFromString(key)}:${itemOutput.i},`;
      }

      const output = B_next(input2, outputVar, outputSchema, outputSchema);
      output.v = _var;
      // Row accumulator: declared at the head of its own segment, before the
      // `for` below that fills it.
      output.cp = `let ${outputVar}=new Array(Math.max(${lengthCode}));`;

      // Wrap the row body in a single try/catch that prepends the row index to
      // any thrown error — giving paths like ["0"]["bar"]. A single wrapper is
      // used (rather than per-field) so that `let` variables declared while
      // parsing one field remain in scope for the object construction.
      let rowAssign: string;
      if (hasAsync) {
        // For async fields, each row becomes a promise that awaits all field values
        // via Promise.all, and the final output is Promise.all of all row promises.
        const rowResultVar = B_varWithoutAllocation(input2.g);
        let asyncBuildCode = "";
        for (let idx = 0; idx <= keysLen - 1; ++idx) {
          const key = keys[idx]!;
          asyncBuildCode =
            asyncBuildCode +
            `${inlinedValueFromString(key)}:${rowResultVar}[${idx}],`;
        }
        rowAssign = `${outputVar}[${iteratorVar}]=Promise.all([${asyncInlines}]).then(${rowResultVar}=>({${asyncBuildCode}}));`;
      } else {
        rowAssign = `${outputVar}[${iteratorVar}]={${itemBuildCode}};`;
      }

      const rowBody = itemParseCode + rowAssign;
      let wrappedBody: string;
      if (itemParseCode === "") {
        wrappedBody = rowBody;
      } else {
        const errorVar = B_varWithoutAllocation(input2.g);
        wrappedBody = `try{${rowBody}}catch(${errorVar}){${errorVar}.path='["'+${iteratorVar}+'"]'+${errorVar}.path;throw ${errorVar}}`;
      }
      output.cp =
        output.cp +
        `for(let ${iteratorVar}=0;${iteratorVar}<${outputVar}.length;++${iteratorVar}){${wrappedBody}}`;

      let output2 = output;
      if (hasAsync) {
        output2 = B_asyncVal(output, `Promise.all(${outputVar})`);
      }
      return B_markOutput(output2, input2);
    } else {
      // Reverse direction: rows → columnar
      // When the declared source type is unknown, field values have
      // already been transformed by the object schema's reverse parse
      // and can be copied directly. When it differs (e.g. json), we
      // need per-field parse to convert values back to the source type
      // (e.g. bigint→string for json compatibility).
      const inputVar = B_Val_var(input);
      const iteratorVar = B_varWithoutAllocation(input.g);
      const outputVar = B_varWithoutAllocation(input.g);

      const declaredItemSchema: Internal = (() => {
        const innerArray: Internal = selfSchema.additionalItems as unknown as Internal;
        return innerArray.additionalItems as unknown as Internal;
      })();
      const needsPerFieldTransform = declaredItemSchema !== unknown;

      let initialArraysCode = "";
      let settingCode = "";
      let perFieldCode = "";
      for (let idx = 0; idx <= keysLen - 1; ++idx) {
        const key = keys[idx]!;
        initialArraysCode = initialArraysCode + `new Array(${inputVar}.length),`;

        if (needsPerFieldTransform) {
          const fieldSchema = properties[key]!;
          const rawValueCode = `${inputVar}[${iteratorVar}][${inlinedValueFromString(key)}]`;

          const itemInput = B_Val_scope(input);
          itemInput.i = rawValueCode;
          itemInput.s = fieldSchema;
          itemInput.e = declaredItemSchema;
          itemInput.v = _notVarBeforeValidation;
          itemInput.io = false;
          itemInput.path = pathFromInlinedLocation(B_inlineLocation(input.g, key));

          const itemOutput = parse(itemInput);
          perFieldCode = perFieldCode + B_merge(itemOutput);
          settingCode =
            settingCode +
            `${outputVar}[${idx}][${iteratorVar}]=${itemOutput.i};`;
        } else {
          settingCode =
            settingCode +
            `${outputVar}[${idx}][${iteratorVar}]=${inputVar}[${iteratorVar}][${inlinedValueFromString(key)}];`;
        }
      }

      const output = B_next(input, outputVar, outputSchema, outputSchema);
      output.v = _var;
      // Columnar accumulator: declared before the `for` that fills it.
      output.cp = `let ${outputVar}=[${initialArraysCode}];`;
      const loopBody = perFieldCode + settingCode;
      let wrappedBody: string;
      if (needsPerFieldTransform && perFieldCode !== "") {
        const errorVar = B_varWithoutAllocation(input.g);
        wrappedBody = `try{${loopBody}}catch(${errorVar}){${errorVar}.path='["'+${iteratorVar}+'"]'+${errorVar}.path;throw ${errorVar}}`;
      } else {
        wrappedBody = loopBody;
      }
      output.cp =
        output.cp +
        `for(let ${iteratorVar}=0;${iteratorVar}<${inputVar}.length;++${iteratorVar}){${wrappedBody}}`;
      return B_markOutput(output, input);
    }
  }
}

export function compactColumns(inputSchema: Internal): Internal {
  const innerArray = array(inputSchema);
  const mut = array(innerArray) as unknown as Internal;
  mut.format = "compactColumns";
  mut.decoder = compactColumnsDecoder;
  return mut;
}

// let inline = {
//   let rec internalInline = (schema, ~variant as maybeVariant=?, ()) => {
//     let mut = schema->castToInternal->copy

//     let inlinedSchema = switch mut {
//     | {?const} if isLiteral(mut) => `S.literal(%raw(\`${literal->Literal.toString}\`))`
//     | {anyOf} => {
//         let variantNamesCounter = dict{}
//         `S.union([${anyOf
//           ->Array.map(s => {
//             let variantName = s.name()
//             let numberOfVariantNames = switch variantNamesCounter->Dict.get(variantName) {
//             | Some(n) => n
//             | None => 0
//             }
//             variantNamesCounter->Dict.set(variantName, numberOfVariantNames->X.Int.plus(1))
//             let variantName = switch numberOfVariantNames {
//             | 0 => variantName
//             | _ =>
//               variantName ++ numberOfVariantNames->X.Int.plus(1)->X.Int.unsafeToString
//             }
//             let inlinedVariant = `#${variantName->X.Inlined.Value.fromString}`
//             s->internalInline(~variant=inlinedVariant, ())
//           })
//           ->Array.join(", ")}])`
//       }
//     | {tag: JSON} => `S.json(~validate=${validated->(Obj.magic: bool => string)})`
//     | {tag: TupleTuple({items: [s0]}) => `S.tuple1(${s0.schema->internalInline()})`
//     | Tuple({items: [s0, s1]}) =>
//       `S.tuple2(${s0.schema->internalInline()}, ${s1.schema->internalInline()})`
//     | Tuple({items: [s0, s1, s2]}) =>
//       `S.tuple3(${s0.schema->internalInline()}, ${s1.schema->internalInline()}, ${s2.schema->internalInline()})`
//     | Tuple({items}) =>
//       `S.tuple(s => (${items
//         ->Array.mapWithIndex((schema, idx) =>
//           `s.item(${idx->X.Int.unsafeToString}, ${schema.schema->internalInline()})`
//         )
//         ->Array.join(", ")}))`
//     | Object({items: []}) => `S.object(_ => ())`
//     | Object({items}) =>
//       `S.object(s =>
//   {
//     ${items
//         ->Array.map(item => {
//           `${item.inlinedLocation}: s.field(${item.inlinedLocation}, ${item.schema->internalInline()})`
//         })
//         ->Array.join(",\n    ")},
//   }
// )`
//     | String => `S.string`
//     | Int => `S.int`
//     | Float => `S.float`
//     | BigInt => `S.bigint`
//     | Bool => `S.bool`
//     | Option(schema) => `S.option(${schema->internalInline()})`
//     | Null(schema) => `S.nullAsOption(${schema->internalInline()})`
//     | Never => `S.never`
//     | Unknown => `S.unknown`
//     | Array(schema) => `S.array(${schema->internalInline()})`
//     | Dict(schema) => `S.dict(${schema->internalInline()})`
//     }

//     let inlinedSchema = switch schema->Option.default {
//     | Some(default) => {
//         metadataMap->X.Dict.deleteInPlace(Option.defaultMetadataId->Metadata.Id.unionToKey)
//         switch default {
//         | Value(defaultValue) =>
//           inlinedSchema ++
//           `->S.Option.getOr(%raw(\`${defaultValue->X.Inlined.Value.stringify}\`))`
//         | Callback(defaultCb) =>
//           inlinedSchema ++
//           `->S.Option.getOrWith(() => %raw(\`${defaultCb()->X.Inlined.Value.stringify}\`))`
//         }
//       }

//     | None => inlinedSchema
//     }

//     let inlinedSchema = switch schema->deprecation {
//     | Some(message) => {
//         metadataMap->X.Dict.deleteInPlace(deprecationMetadataId->Metadata.Id.unionToKey)
//         inlinedSchema ++ `->S.deprecate(${message->X.Inlined.Value.fromString})`
//       }

//     | None => inlinedSchema
//     }

//     let inlinedSchema = switch schema->description {
//     | Some(message) => {
//         metadataMap->X.Dict.deleteInPlace(descriptionMetadataId->Metadata.Id.unionToKey)
//         inlinedSchema ++ `->S.describe(${message->X.Inlined.Value.stringify})`
//       }

//     | None => inlinedSchema
//     }

//     let inlinedSchema = switch schema->classify {
//     | Object({additionalItems: Strict}) => inlinedSchema ++ `->S.strict`
//     | _ => inlinedSchema
//     }

//     let inlinedSchema = switch schema->classify {
//     | String
//     | Literal(String(_)) =>
//       switch schema->String.refinements {
//       | [] => inlinedSchema
//       | refinements =>
//         metadataMap->X.Dict.deleteInPlace(String.Refinement.metadataId->Metadata.Id.unionToKey)
//         inlinedSchema ++
//         refinements
//         ->Array.map(refinement => {
//           switch refinement {
//           | {kind: Email, message} =>
//             `->S.email(~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Url, message} => `->S.url(~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Uuid, message} =>
//             `->S.uuid(~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Cuid, message} =>
//             `->S.cuid(~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Min({length}), message} =>
//             `->S.stringMinLength(${length->X.Int.unsafeToString}, ~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Max({length}), message} =>
//             `->S.stringMaxLength(${length->X.Int.unsafeToString}, ~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Length({length}), message} =>
//             `->S.stringLength(${length->X.Int.unsafeToString}, ~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Pattern({re}), message} =>
//             `->S.pattern(%re(${re
//               ->X.Re.toString
//               ->X.Inlined.Value.fromString}), ~message=${message->X.Inlined.Value.fromString})`
//           }
//         })
//         ->Array.join("")
//       }
//     | Int =>
//       // | Literal(Int(_)) ???
//       switch schema->Int.refinements {
//       | [] => inlinedSchema
//       | refinements =>
//         metadataMap->X.Dict.deleteInPlace(Int.Refinement.metadataId->Metadata.Id.unionToKey)
//         inlinedSchema ++
//         refinements
//         ->Array.map(refinement => {
//           switch refinement {
//           | {kind: Max({value}), message} =>
//             `->S.intMax(${value->X.Int.unsafeToString}, ~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Min({value}), message} =>
//             `->S.intMin(${value->X.Int.unsafeToString}, ~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Port, message} =>
//             `->S.port(~message=${message->X.Inlined.Value.fromString})`
//           }
//         })
//         ->Array.join("")
//       }
//     | Float =>
//       // | Literal(Float(_)) ???
//       switch schema->Float.refinements {
//       | [] => inlinedSchema
//       | refinements =>
//         metadataMap->X.Dict.deleteInPlace(Float.Refinement.metadataId->Metadata.Id.unionToKey)
//         inlinedSchema ++
//         refinements
//         ->Array.map(refinement => {
//           switch refinement {
//           | {kind: Max({value}), message} =>
//             `->S.floatMax(${value->X.Inlined.Float.toRescript}, ~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Min({value}), message} =>
//             `->S.floatMin(${value->X.Inlined.Float.toRescript}, ~message=${message->X.Inlined.Value.fromString})`
//           }
//         })
//         ->Array.join("")
//       }

//     | Array(_) =>
//       switch schema->Array.refinements {
//       | [] => inlinedSchema
//       | refinements =>
//         metadataMap->X.Dict.deleteInPlace(Array.Refinement.metadataId->Metadata.Id.unionToKey)
//         inlinedSchema ++
//         refinements
//         ->Array.map(refinement => {
//           switch refinement {
//           | {kind: Max({length}), message} =>
//             `->S.arrayMaxLength(${length->X.Int.unsafeToString}, ~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Min({length}), message} =>
//             `->S.arrayMinLength(${length->X.Int.unsafeToString}, ~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Length({length}), message} =>
//             `->S.arrayLength(${length->X.Int.unsafeToString}, ~message=${message->X.Inlined.Value.fromString})`
//           }
//         })
//         ->Array.join("")
//       }

//     | _ => inlinedSchema
//     }

//     let inlinedSchema = if metadataMap->Dict.keysToArray->Array.length !== 0 {
//       `{
//   let s = ${inlinedSchema}
//   let _ = %raw(\`s.m = ${metadataMap->JSON.stringifyAny->Option.getUnsafe}\`)
//   s
// }`
//     } else {
//       inlinedSchema
//     }

//     let inlinedSchema = switch maybeVariant {
//     | Some(variant) => inlinedSchema ++ `->S.shape(v => ${variant}(v))`
//     | None => inlinedSchema
//     }

//     inlinedSchema
//   }

//   schema => {
//     schema->castToUnknown->internalInline()
//   }
// }

// PORT-NOTE: `object`, `shape`, `tuple` alias `Schema.object/shape/tuple`
// (renamed `SchemaModule` per conventions) — kept as aliases.
export const object = schemaObject;
export const nullAsOption = (item: Internal): Internal =>
  optionFactory(item, nullAsUnit());
// PORT-NOTE: `null` is a reserved word in JS/TS binding position — exported
// as `null_`; the ReScript bindings layer maps it back to `S.null`.
export const null_ = (item: Internal): Internal =>
  unionFactory([item, nullLiteral()]);
// PORT-NOTE: `let array = array` in the source is a self-alias no-op
// (re-exposing the earlier `array` factory at this point in the module) —
// skipped; the `array` binding from its own section is already exported.
export const dict = dictFactory;
export const shape = schemaShape;
export const tuple = schemaTuple;
export const tuple1 = (v0: Internal): Internal =>
  tuple((s: any) => s.item(0, v0));
export const tuple2 = (v0: Internal, v1: Internal): Internal =>
  definitionToSchema([v0, v1] as unknown as Internal);
export const tuple3 = (v0: Internal, v1: Internal, v2: Internal): Internal =>
  definitionToSchema([v0, v1, v2] as unknown as Internal);
export const union = unionFactory;

// =============
// Built-in refinements
// =============

export const assertNumber: (fnName: string, n: unknown) => void = (fnName, n) => {
  if ((typeof n as Tag) !== numberTag || Number.isNaN(n)) {
    throw InternalError.make({
      code: "invalid_operation",
      path: pathEmpty,
      reason: `[S.${fnName}] Expected number, received ${stringify(n)}`,
    } as unknown as ErrorDetails);
  }
};

export function intMin(schema: Internal, minValue: number, maybeMessage?: string): Internal {
  assertNumber("min", minValue);
  const message =
    maybeMessage !== undefined
      ? maybeMessage
      : `Number must be greater than or equal to ${minValue}`;
  return internalRefine(schema, (mut: Internal) => {
    mut.minimum = minValue;
    getMutErrorMessage(mut)["minimum"] = message;
    return (_input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}>${minValue - 1}`,
          f: B_failWithErrorMessage("minimum", message),
        },
      ];
    };
  });
}

export function intMax(schema: Internal, maxValue: number, maybeMessage?: string): Internal {
  assertNumber("max", maxValue);
  const message =
    maybeMessage !== undefined
      ? maybeMessage
      : `Number must be lower than or equal to ${maxValue}`;
  return internalRefine(schema, (mut: Internal) => {
    mut.maximum = maxValue;
    getMutErrorMessage(mut)["maximum"] = message;
    return (_input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}<${maxValue + 1}`,
          f: B_failWithErrorMessage("maximum", message),
        },
      ];
    };
  });
}

export function floatMin(schema: Internal, minValue: number, maybeMessage?: string): Internal {
  assertNumber("min", minValue);
  const message =
    maybeMessage !== undefined
      ? maybeMessage
      : `Number must be greater than or equal to ${minValue}`;
  return internalRefine(schema, (mut: Internal) => {
    mut.minimum = minValue;
    getMutErrorMessage(mut)["minimum"] = message;
    return (input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}>=${B_embed(input, minValue)}`,
          f: B_failWithErrorMessage("minimum", message),
        },
      ];
    };
  });
}

export function floatMax(schema: Internal, maxValue: number, maybeMessage?: string): Internal {
  assertNumber("max", maxValue);
  const message =
    maybeMessage !== undefined
      ? maybeMessage
      : `Number must be lower than or equal to ${maxValue}`;
  return internalRefine(schema, (mut: Internal) => {
    mut.maximum = maxValue;
    getMutErrorMessage(mut)["maximum"] = message;
    return (input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}<=${B_embed(input, maxValue)}`,
          f: B_failWithErrorMessage("maximum", message),
        },
      ];
    };
  });
}

export function arrayMinLength(schema: Internal, length: number, maybeMessage?: string): Internal {
  assertNumber("min", length);
  const message =
    maybeMessage !== undefined
      ? maybeMessage
      : `Array must be ${length} or more items long`;
  return internalRefine(schema, (mut: Internal) => {
    mut.minItems = length;
    getMutErrorMessage(mut)["minItems"] = message;
    return (_input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}.length>${length - 1}`,
          f: B_failWithErrorMessage("minItems", message),
        },
      ];
    };
  });
}

export function arrayMaxLength(schema: Internal, length: number, maybeMessage?: string): Internal {
  assertNumber("max", length);
  const message =
    maybeMessage !== undefined
      ? maybeMessage
      : `Array must be ${length} or fewer items long`;
  return internalRefine(schema, (mut: Internal) => {
    mut.maxItems = length;
    getMutErrorMessage(mut)["maxItems"] = message;
    return (_input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}.length<${length + 1}`,
          f: B_failWithErrorMessage("maxItems", message),
        },
      ];
    };
  });
}

export function arrayLength(schema: Internal, length: number, maybeMessage?: string): Internal {
  assertNumber("length", length);
  const message =
    maybeMessage !== undefined
      ? maybeMessage
      : `Array must be exactly ${length} items long`;
  return internalRefine(schema, (mut: Internal) => {
    mut.minItems = length;
    mut.maxItems = length;
    const em = getMutErrorMessage(mut);
    em["minItems"] = message;
    em["maxItems"] = message;
    return (_input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}.length===${length}`,
          f: B_failWithErrorMessage("minItems", message),
        },
      ];
    };
  });
}

export function stringMinLength(schema: Internal, length: number, maybeMessage?: string): Internal {
  assertNumber("min", length);
  const message =
    maybeMessage !== undefined
      ? maybeMessage
      : `String must be ${length} or more characters long`;
  return internalRefine(schema, (mut: Internal) => {
    mut.minLength = length;
    getMutErrorMessage(mut)["minLength"] = message;
    return (_input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}.length>${length - 1}`,
          f: B_failWithErrorMessage("minLength", message),
        },
      ];
    };
  });
}

export function stringMaxLength(schema: Internal, length: number, maybeMessage?: string): Internal {
  assertNumber("max", length);
  const message =
    maybeMessage !== undefined
      ? maybeMessage
      : `String must be ${length} or fewer characters long`;
  return internalRefine(schema, (mut: Internal) => {
    mut.maxLength = length;
    getMutErrorMessage(mut)["maxLength"] = message;
    return (_input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}.length<${length + 1}`,
          f: B_failWithErrorMessage("maxLength", message),
        },
      ];
    };
  });
}

export function stringLength(schema: Internal, length: number, maybeMessage?: string): Internal {
  assertNumber("length", length);
  const message =
    maybeMessage !== undefined
      ? maybeMessage
      : `String must be exactly ${length} characters long`;
  return internalRefine(schema, (mut: Internal) => {
    mut.minLength = length;
    mut.maxLength = length;
    const em = getMutErrorMessage(mut);
    em["minLength"] = message;
    em["maxLength"] = message;
    return (_input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}.length===${length}`,
          f: B_failWithErrorMessage("minLength", message),
        },
      ];
    };
  });
}

export function pattern(schema: Internal, re: RegExp, message: string = `Invalid pattern`): Internal {
  return internalRefine(schema, (mut: Internal) => {
    mut.pattern = re;
    getMutErrorMessage(mut)["pattern"] = message;
    return (input: Val) => {
      const embededRe = B_embed(input, re);
      return [
        {
          c: (inputVar: string) =>
            re.global
              ? `(${embededRe}.lastIndex=0,${embededRe}.test(${inputVar}))`
              : `${embededRe}.test(${inputVar})`,
          f: B_failWithErrorMessage("pattern", message),
        },
      ];
    };
  });
}

export function trim(schema: Internal): Internal {
  const transformer = (string: unknown) => (string as string).trim();
  return transform(schema, (_: unknown) => ({
    p: transformer,
    s: transformer,
  }));
}

export function nullable(schema: Internal): Internal {
  return unionFactory([schema, unit(), nullLiteral()]);
}

export function nullableAsOption(schema: Internal): Internal {
  return unionFactory([schema, unit(), nullAsUnit()]);
}

// =============
// JS/TS API
// =============
// ============================================================================
// Section: Sury.res lines 6944-7136 — JS public API wrappers
// (js_parser, js_asyncParser, js_asyncDecoder, js_encoder, js_asyncEncoder,
//  js_assert, js_is, js_union, js_to, js_refine, noop, js_asyncDecoderAssert,
//  js_optional, js_nullable, js_merge, global, reverse re-export)
//
// TODO(integration): expects from other sections:
//   - getDecoder (raw variadic; reads extra args / flag) — earlier section
//   - reverse (internal schema reverse) — earlier section; also see PORT-NOTE
//     at the bottom: Sury.res line 7135 re-types it for the public API
//   - getAssertResult — earlier section (~4411)
//   - unionFactory — earlier section (~3989)
//   - definitionToSchema — Schema factory section (~5485+)
//   - B (Builder.B helpers: varWithoutAllocation, next, _var, embed,
//     failWithArg, makeInvalidConversionDetails, invalidInputBuilder) —
//     builder section (~1083+)
//   - internalRefine — earlier section (~4560)
//   - transform — earlier section (~4628)
//   - Option_getOr / Option_getOrWith — Option module (~4773)
//   - unit, nullAsUnit, nullLiteral — literal factories section (~2211+)
//   - objectDecoder — object section
// Prelude (core.ts) provides: unknown, isSchemaObject, InternalError,
//   updateOutput, copySchema, baseSchema, typeOf, functionTag, stringTag,
//   objectTag, pathFromArray, pathEmpty, globalConfig, Flag,
//   initialOnAdditionalItems, initialDefaultFlag, GlobalConfigOverride,
//   Internal, Val, Check.
// ============================================================================

export const js_parser = (...args: any[]) => (getDecoder as any)(unknown, ...args);

export const js_asyncParser = (...args: any[]) => (getDecoder as any)(unknown, ...args, 1);

export const js_asyncDecoder = (...args: any[]) => (getDecoder as any)(...args, 1);

export const js_encoder = (...args: any[]) => (getDecoder as any)(...args.map(reverse));

export const js_asyncEncoder = (...args: any[]) => (getDecoder as any)(...args.map(reverse), 1);

// Accepts both `(schema, data)` and `(data, schema)` arg orders. We tell them
// apart by the Standard Schema marker on a schema object. The truthiness guard
// keeps `null`/`undefined` data from throwing on the marker access, routing it
// to the data slot so validation fails with a proper Sury error.
export const js_assert = (a: unknown, b: unknown): unknown => {
  const aIsSchema = (a as unknown as boolean) && isSchemaObject(a);
  const schema = (aIsSchema ? a : b) as Internal;
  const data = aIsSchema ? b : a;
  // PORT-NOTE: getDecoder3 is a @val external self-call of the variadic
  // getDecoder — ported as a plain 3-arg call per conventions.
  return (getDecoder as any)(unknown, schema, getAssertResult())(data);
};

export const js_is = (a: unknown, b: unknown): boolean => {
  try {
    js_assert(a, b);
    return true;
  } catch (exn) {
    // Rethrow anything that isn't a Sury validation failure.
    InternalError.getOrRethrow(exn);
    return false;
  }
};

export const js_union = (values: unknown[]) =>
  unionFactory(values.map(definitionToSchema) as unknown as Internal[]);

export const js_to = /* @__PURE__ */ (() => {
  // FIXME: Test how it'll work if we have async var as input
  // FIXME: Might not work well with object targets
  const customBuilder = (fn: (value: unknown) => unknown): Builder => {
    // PORT-NOTE: Builder.make is an Obj.magic identity in the source — the
    // builder function is used directly.
    return (input: Val): Val => {
      const target = input.e.to!;
      const outputVar = B_varWithoutAllocation(input.g);
      const output = B_next(input, outputVar, target, target);
      output.v = _var;
      output.cp = `let ${outputVar};try{${output.i}=${B_embed(
        input,
        fn,
      )}(${input.i})}catch(x){${B_failWithArg(
        output,
        (e: unknown) => B_makeInvalidConversionDetails(input, target, e),
        `x`,
      )}}`;
      return output;
    };
  };

  return (
    schema: Internal,
    target: Internal,
    maybeDecoder?: (value: unknown) => unknown,
    maybeEncoder?: (target: unknown) => unknown,
  ) => {
    return updateOutput(schema, (mut) => {
      if (maybeEncoder !== undefined) {
        const targetMut = copySchema(target);
        targetMut.serializer = customBuilder(maybeEncoder);
        mut.to = targetMut;
      } else {
        mut.to = target;
      }
      if (maybeDecoder !== undefined) {
        mut.parser = customBuilder(maybeDecoder);
      }
    });
  };
})();

export const js_refine = (
  schema: Internal,
  refineCheck: (value: unknown) => boolean,
  refineOptions?: { error?: string; path?: string[] },
) => {
  const message =
    refineOptions !== undefined
      ? refineOptions["error"] !== undefined
        ? refineOptions["error"]
        : "Refinement failed"
      : "Refinement failed";
  const extraPath =
    refineOptions !== undefined
      ? refineOptions["path"] !== undefined
        ? pathFromArray(refineOptions["path"])
        : pathEmpty
      : pathEmpty;
  return internalRefine(schema, (_: Internal) => (input: Val): Check[] => {
    const embeddedCheck = B_embed(input, refineCheck);
    return [
      {
        c: (inputVar: string) => `${embeddedCheck}(${inputVar})`,
        f: B_invalidInputBuilder(undefined, extraPath, message),
      },
    ];
  });
};

export const noop = <A>(a: A): A => a;
export const js_asyncDecoderAssert = (
  schema: Internal,
  assertFn: (value: unknown) => Promise<unknown>,
) => {
  return transform(schema, (_: unknown) => {
    return {
      a: (v: unknown) => assertFn(v).then(() => v),
      s: noop,
    };
  });
};

export const js_optional = (schema: Internal, maybeOr: unknown): Internal => {
  // TODO: maybeOr should be part of the unit schema
  schema = unionFactory([schema, unit()]) as unknown as Internal;
  if (maybeOr !== undefined && typeOf(maybeOr) === functionTag) {
    return Option_getOrWith(schema, maybeOr as () => unknown) as unknown as Internal;
  } else if (maybeOr !== undefined) {
    return Option_getOr(schema, maybeOr) as unknown as Internal;
  } else {
    return schema;
  }
};

export const js_nullable = (schema: Internal, maybeOr: unknown): Internal => {
  // TODO: maybeOr should be part of the unit schema
  if (maybeOr !== undefined) {
    const schema2 = unionFactory([schema, nullAsUnit()]) as unknown as Internal;
    if (typeOf(maybeOr) === functionTag) {
      return Option_getOrWith(schema2, maybeOr as () => unknown) as unknown as Internal;
    } else {
      return Option_getOr(schema2, maybeOr) as unknown as Internal;
    }
  } else {
    return unionFactory([schema, nullLiteral()]) as unknown as Internal;
  }
};

export const js_merge = (s1: Internal, s2: Internal): Internal => {
  // PORT-NOTE: the source matches on the public `Object({...})` variants —
  // at runtime that's a `type === "object"` check plus field reads, ported
  // as explicit conditions below.
  let result: Internal | undefined;
  if (
    s1.type === objectTag &&
    s2.type === objectTag &&
    // Filter out S.record schemas
    typeOf(s1.additionalItems) === stringTag &&
    typeOf(s2.additionalItems) === stringTag &&
    !(s1.to as unknown as boolean) &&
    !(s2.to as unknown as boolean)
  ) {
    const properties1 = s1.properties!;
    const properties2 = s2.properties!;
    const properties = { ...properties1 };
    const keys2 = Object.keys(properties2);

    for (let idx = 0; idx <= keys2.length - 1; idx++) {
      const key = keys2[idx]!;
      properties[key] = properties2[key]!;
    }

    const mut = baseSchema(objectTag, false);

    // TODO: Merge to required fields
    mut.required = Object.keys(properties);
    mut.properties = properties;
    mut.additionalItems = s1.additionalItems;
    mut.decoder = objectDecoder;
    result = mut;
  }
  if (result !== undefined) {
    return result;
  } else {
    return InternalError.panic(
      "The merge supports only structured object schemas without transformations",
    );
  }
};

// PORT-NOTE: kept the source's `global` name — legal as a module-scoped
// export even though Node types declare a `global` var.
export const global = (override: GlobalConfigOverride): void => {
  globalConfig.a = (
    override.defaultAdditionalItems !== undefined
      ? override.defaultAdditionalItems
      : initialOnAdditionalItems
  ) as unknown as AdditionalItems;
  globalConfig.f =
    override.disableNanNumberValidation === true
      ? Flag.disableNanNumberValidation
      : initialDefaultFlag;
};

// PORT-NOTE: Sury.res line 7135 `let reverse = reverse->Obj.magic` merely
// re-types the internal `reverse` for the public API — no runtime change.
// TODO(integration): `reverse` must already be defined/exported by its
// earlier section; do NOT redefine it here.
// =============================================================================
// Fragment 11 — JSON Schema (Sury.res lines 7137-7971, end of file)
// module RescriptJSONSchema, toJSONSchemaOptions, targetSchemaUri,
// toJSONSchema, enableStandardJSONSchema, extendJSONSchema,
// castAnySchemaToJsonableS, fromJSONSchema, min, max, length, trailing
// public re-typings.
//
// TODO(integration): expects from earlier sections:
//   - prelude: Internal, Val, Flag, TagFlag, Path helpers (pathEmpty,
//     pathDynamic, pathConcat, pathFromLocation), InternalError, tag consts
//     (objectTag, unknownTag, refTag, stringTag, arrayTag, numberTag,
//     undefinedTag, unionTag), baseSchema, isLiteral, isOptional,
//     toExpression, stringify, unknown, globalConfig
//   - Builder section: B_operationArg, B_makeInvalidInputDetails, parse
//   - reverse (schema reversing), Literal (Literal_parse)
//   - MetadataModule (module Metadata): MetadataModule.Id.internal,
//     MetadataModule.get, MetadataModule.set
//   - defsPath, jsonName
//   - section 06: standardJSONSchemaRef ({ contents } ref cell) and the
//     Standard JSON Schema options type (see PORT-NOTE on JsonSchemaTarget /
//     StandardJsonSchemaOptions below — drop the local aliases here if
//     section 06 already declares them)
//   - factories/public fns: int, json, never_, null_ (rename of `null` — a
//     reserved word in TS; whatever name section 10 picked for
//     `let null = item => ...` must be used here), option, Option_getOr,
//     definitionToSchema, schemaFactory, object, dict,
//     tuple, array, union, literal, strict, refine, meta, assertOrThrow,
//     string, bool, float, email, url, uuid, isoDateTime, pattern
//   - section 09 refinements: intMin, intMax, floatMin, floatMax,
//     stringMinLength, stringMaxLength, stringLength, arrayMinLength,
//     arrayMaxLength, arrayLength
//
// PORT-NOTE: no runtime values had to be imported from JSONSchema.res or
// StandardSchema.res — everything runtime-relevant there is `%identity`
// externals (Arrayable.single/array, Mutable.fromReadOnly/toReadOnly,
// Result casts) or `Object.assign` (Mutable.mixin), all inlined below.
// Their types are ported as loose TS aliases with the RUNTIME field names
// (`$ref`, `$schema`, `$defs`, `type`, `if`, `else` — the `@as(...)` names,
// not the ReScript field names `ref`/`schema`/`defs`/`type_`/`if_`/`else_`).
// =============================================================================

// -----------------------------------------------------------------------------
// JSONSchema.res types (loose port; runtime field names)
// -----------------------------------------------------------------------------

/**
 * Primitive type
 * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-6.1.1
 */
export type JSONSchemaTypeName =
  | "string"
  | "number"
  | "integer"
  | "boolean"
  | "object"
  | "array"
  | "null";

// PORT-NOTE: JSONSchema.Arrayable.t<'item> is an untagged `item | item[]`;
// `Arrayable.single`/`Arrayable.array` are %identity and are dropped at call
// sites, `Arrayable.isArray` is Array.isArray, and `Arrayable.classify` is an
// inline Array.isArray test.
export type JSONSchemaArrayable<Item> = Item | Item[];

// PORT-NOTE: JSONSchema's `definition` is `@unboxed
// Schema(t) | @as(false) Never | @as(true) Any` — at runtime a definition is
// the schema object itself, `false`, or `true`. The `Schema(...)` wrapping
// at construction sites is a no-op and is dropped; `Never` -> `false`,
// `Any` -> `true`; the `Schema(t)` pattern -> `typeof d !== "boolean"`.
export type JSONSchemaDefinition = JSONSchemaT | boolean;

/**
 * JSON Schema v7
 * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01
 */
// PORT-NOTE: JSONSchema.t and JSONSchema.Mutable.t are the same runtime
// object (Mutable.fromReadOnly/toReadOnly are %identity); TS has no
// readonly/mutable split worth keeping here, so a single mutable type serves
// both, and Mutable.fromReadOnly/toReadOnly calls are dropped.
export type JSONSchemaT = {
  $id?: string;
  $ref?: string;
  $schema?: string;
  /**
   * @see https://datatracker.ietf.org/doc/html/draft-bhutton-json-schema-00#section-8.2.4
   * @see https://datatracker.ietf.org/doc/html/draft-bhutton-json-schema-validation-00#appendix-A
   */
  $defs?: Record<string, JSONSchemaDefinition>;
  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-6.1
   */
  type?: JSONSchemaArrayable<JSONSchemaTypeName>;
  enum?: unknown[];
  const?: unknown;
  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-6.2
   */
  multipleOf?: number;
  maximum?: number;
  exclusiveMaximum?: number;
  minimum?: number;
  exclusiveMinimum?: number;
  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-6.3
   */
  maxLength?: number;
  minLength?: number;
  pattern?: string;
  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-6.4
   */
  items?: JSONSchemaArrayable<JSONSchemaDefinition>;
  prefixItems?: JSONSchemaDefinition[];
  additionalItems?: JSONSchemaDefinition;
  maxItems?: number;
  minItems?: number;
  uniqueItems?: boolean;
  contains?: JSONSchemaT;
  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-6.5
   */
  maxProperties?: number;
  minProperties?: number;
  required?: string[];
  properties?: Record<string, JSONSchemaDefinition>;
  patternProperties?: Record<string, JSONSchemaDefinition>;
  additionalProperties?: JSONSchemaDefinition;
  dependencies?: Record<string, unknown>;
  propertyNames?: JSONSchemaDefinition;
  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-6.6
   */
  if?: JSONSchemaDefinition;
  then?: JSONSchemaDefinition;
  else?: JSONSchemaDefinition;
  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-6.7
   */
  allOf?: JSONSchemaDefinition[];
  anyOf?: JSONSchemaDefinition[];
  oneOf?: JSONSchemaDefinition[];
  not?: JSONSchemaDefinition;
  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-7
   */
  format?: string;
  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-8
   */
  contentMediaType?: string;
  contentEncoding?: string;
  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-9
   */
  definitions?: Record<string, JSONSchemaDefinition>;
  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-10
   */
  title?: string;
  description?: string;
  deprecated?: boolean;
  nullable?: boolean;
  default?: unknown;
  readOnly?: boolean;
  writeOnly?: boolean;
  examples?: unknown[];
};

// -----------------------------------------------------------------------------
// StandardSchema.res JsonSchema types (loose port)
// -----------------------------------------------------------------------------

// PORT-NOTE: StandardSchema.JsonSchema.target is `@unboxed | @as("draft-07")
// Draft07 | @as("draft-2020-12") Draft202012 | @as("openapi-3.0") OpenApi30 |
// Unknown(string)` — at runtime it's just a string; the known dialects are
// compared as string literals, everything else is the `Unknown` case.
// TODO(integration): if section 06 already declares these two aliases for
// standardJSONSchemaRef's signature, keep a single declaration.
export type JsonSchemaTarget = "draft-07" | "draft-2020-12" | "openapi-3.0" | (string & {});

// `StandardJSONSchemaV1.Options`.
export type StandardJsonSchemaOptions = {
  target: JsonSchemaTarget;
  libraryOptions?: Record<string, unknown>;
};

// -----------------------------------------------------------------------------
// module RescriptJSONSchema
// -----------------------------------------------------------------------------
//
// PORT-NOTE: ported as standalone functions (the mutually recursive
// encodeToJsonSchema / internalToJSONSchema / internalToJSONSchemaBase group
// needs plain function declarations) plus a `RescriptJSONSchema` const
// object so qualified call sites (`jsonSchemaMerge`, `.internalToJSONSchema`,
// `.jsonSchemaMetadataId`) keep reading like the source. The `include
// JSONSchema` is covered by the type aliases above.

export const jsonSchemaMetadataId: string = MetadataModule.Id.internal("JSONSchema");

// @val external merge: (@as(json`{}`) _, t, t) => t = "Object.assign"
export function jsonSchemaMerge(a: JSONSchemaT, b: JSONSchemaT): JSONSchemaT {
  return Object.assign({}, a, b);
}

export function applyMetadataOverlay(
  jsonSchema: JSONSchemaT,
  schema: Internal,
  defs: Record<string, Internal>
): void {
  if (schema.description !== undefined) {
    jsonSchema.description = schema.description;
  }
  if (schema.title !== undefined) {
    jsonSchema.title = schema.title;
  }
  if (schema.deprecated !== undefined) {
    jsonSchema.deprecated = schema.deprecated;
  }
  if (schema.examples !== undefined) {
    jsonSchema.examples = schema.examples as // If a schema is Jsonable,
    // then examples are Jsonable too.
    unknown[];
  }
  if (schema["$defs"] !== undefined) {
    Object.assign(defs, schema["$defs"]);
  }
  const metadataRawSchema = MetadataModule.get(schema, jsonSchemaMetadataId) as
    | JSONSchemaT
    | undefined;
  if (metadataRawSchema !== undefined) {
    Object.assign(jsonSchema, metadataRawSchema);
  }
}

export function encodeToJsonSchema(
  schema: Internal,
  path: Path,
  defs: Record<string, Internal>,
  parent: Internal,
  target: JsonSchemaTarget
): JSONSchemaT | undefined {
  const schemaInternal = schema;
  const reversed = reverse(schemaInternal);
  const input = B_operationArg(
    unknown,
    reversed,
    Flag.none,
    0 as unknown as Record<string, Internal>
  );
  try {
    const output = parse(input);
    // The parse produces a val whose .schema reflects the
    // JSON-compatible transformed structure.
    return internalToJSONSchema(output.s, path, defs, parent, target);
  } catch (exn) {
    InternalError.getOrRethrow(exn);

    // Parse failed — caller falls through to normal tag-based logic.
    return undefined;
  }
}

export function internalToJSONSchema(
  schema: Internal,
  path: Path,
  defs: Record<string, Internal>,
  parent: Internal,
  target: JsonSchemaTarget
): JSONSchemaT {
  const schemaInternal = schema;
  // When a schema has `.to`, we can try to encode-reverse it to get a more
  // precise JSON schema (e.g. `format: "date-time"` for `S.string->S.to(S.date)`).
  // For a user-applied `.to` on a union (no `parser`) the encode-reverse output
  // is the schema produced by the union decoder, already shrunk to the
  // surviving variants — exactly what a downstream JSON Schema should describe.
  // Unions with a `parser` come from the option machinery (S.option,
  // Option.getOrWith, ...) where the union's anyOf is the input format we want
  // to keep describing. Object/array still need their nested item metadata, so
  // they keep using the base path.
  const tagFlag = TagFlag.get(schemaInternal.type);
  const hasUserTo =
    (schemaInternal.to as unknown as boolean) &&
    !Flag.unsafeHas(tagFlag, Flag.with(TagFlag.object, TagFlag.array)) &&
    !(Flag.unsafeHas(tagFlag, TagFlag.union) && (schemaInternal.parser as unknown as boolean));
  const encoded = hasUserTo
    ? encodeToJsonSchema(schema, path, defs, parent, target)
    : undefined;
  if (encoded !== undefined) {
    const mutableJs = encoded;
    applyMetadataOverlay(mutableJs, schema, defs);
    return mutableJs;
  } else {
    return internalToJSONSchemaBase(schema, path, defs, parent, target);
  }
}

export function internalToJSONSchemaBase(
  schema: Internal,
  path: Path,
  defs: Record<string, Internal>,
  parent: Internal,
  target: JsonSchemaTarget
): JSONSchemaT {
  const jsonSchema: JSONSchemaT = {};
  // OpenAPI 3.0 has no `const`; describe a single allowed value with `enum`.
  const setConstOrEnum = (value: unknown) => {
    if (target === "openapi-3.0") {
      jsonSchema.enum = [value];
    } else {
      jsonSchema.const = value;
    }
  };
  const tag = schema.type;
  if (tag === stringTag) {
    const const_ = schema.const as string | undefined;
    const format = schema.format;
    jsonSchema.type = "string";
    switch (format) {
      case "date-time":
        jsonSchema.format = "date-time";
        break;
      case "email":
        jsonSchema.format = "email";
        break;
      case "uuid":
        jsonSchema.format = "uuid";
        break;
      case "url":
        jsonSchema.format = "uri";
        break;
      // Some(Cuid) | Some(JSON) | None => ()
      default:
        break;
    }
    const internal = schema;
    if (internal.minLength !== undefined) {
      jsonSchema.minLength = internal.minLength;
    }
    if (internal.maxLength !== undefined) {
      jsonSchema.maxLength = internal.maxLength;
    }
    if (internal.pattern !== undefined) {
      jsonSchema.pattern = (internal.pattern as unknown as { source: string }).source;
    }
    if (const_ !== undefined) {
      setConstOrEnum(const_);
    }
  } else if (tag === numberTag) {
    const internal = schema;
    const format = schema.format;
    const const_ = schema.const as number | undefined;
    if (format === "int32") {
      jsonSchema.type = "integer";
      jsonSchema.minimum = -2147483648;
      jsonSchema.maximum = 2147483647;
    } else if (format === "port") {
      jsonSchema.type = "integer";
      jsonSchema.minimum = 0;
      jsonSchema.maximum = 65535;
    } else {
      jsonSchema.type = "number";
    }
    if (internal.minimum !== undefined) {
      jsonSchema.minimum = internal.minimum;
    }
    if (internal.maximum !== undefined) {
      jsonSchema.maximum = internal.maximum;
    }
    if (const_ !== undefined) {
      setConstOrEnum(const_);
    }
  } else if (tag === booleanTag) {
    const const_ = schema.const as boolean | undefined;
    jsonSchema.type = "boolean";
    if (const_ !== undefined) {
      setConstOrEnum(const_);
    }
  } else if (tag === arrayTag) {
    const additionalItems = schema.additionalItems!;
    const items = schema.items!;
    if (typeof additionalItems === "object") {
      const childSchema = additionalItems;
      jsonSchema.items = internalToJSONSchema(
        childSchema,
        pathConcat(path, pathDynamic),
        defs,
        schema,
        target
      );
      jsonSchema.type = "array";
      const internal = schema;
      if (internal.minItems !== undefined) {
        jsonSchema.minItems = internal.minItems;
      }
      if (internal.maxItems !== undefined) {
        jsonSchema.maxItems = internal.maxItems;
      }
    } else {
      const itemDefinitions: JSONSchemaDefinition[] = items.map((itemSchema, idx) => {
        return internalToJSONSchema(
          itemSchema,
          pathConcat(path, pathFromLocation(idx.toString())),
          defs,
          schema,
          target
        );
      });
      const itemsNumber = itemDefinitions.length;

      jsonSchema.type = "array";
      jsonSchema.minItems = itemsNumber;
      jsonSchema.maxItems = itemsNumber;
      if (target === "openapi-3.0") {
        // OpenAPI 3.0 has no tuple support. Describe a fixed-length array
        // whose every item matches any of the positional item schemas.
        jsonSchema.items = { anyOf: itemDefinitions };
      } else if (target === "draft-2020-12") {
        // draft-2020-12 uses `prefixItems` for positional schemas.
        jsonSchema.prefixItems = itemDefinitions;
      } else {
        // draft-07 (default) uses an `items` array for positional schemas.
        jsonSchema.items = itemDefinitions;
      }
    }
  } else if (tag === unionTag) {
    const anyOf = schema.anyOf!;
    const literals: unknown[] = [];
    const items: JSONSchemaDefinition[] = [];
    const seen: Record<string, boolean> = {};

    anyOf.forEach((childSchema) => {
      // Filter out undefined to support optional fields
      if (childSchema.type === undefinedTag && parent.type === objectTag) {
        // ()
      } else {
        const childJsonSchema = internalToJSONSchema(childSchema, path, defs, schema, target);
        // Collapse structurally-identical members (e.g. variants coercing to
        // the same `.to` target) so the union renders as `T`, not `anyOf:[T,T]`.
        const key = JSON.stringify(childJsonSchema) as unknown as string;
        if (!(key in seen)) {
          seen[key] = true;
          items.push(childJsonSchema);
          if (isLiteral(childSchema)) {
            literals.push(
              childSchema.const // If a schema is Jsonable, the const is Jsonable too.
            );
          }
        }
      }
    });

    const itemsNumber = items.length;

    if (schema.default !== undefined) {
      jsonSchema.default = schema.default;
    }

    // Detect whether a definition is the "null" representation for the
    // current target. Sury models nullable as a union `[X, null]`; for
    // openapi-3.0 the null variant is `{enum:[null]}` (see the Null case),
    // for other targets it is `{type:"null"}`.
    const isNullDefinition = (definition: JSONSchemaDefinition): boolean => {
      if (typeof definition !== "boolean") {
        const t = definition;
        if (t.type !== undefined && (t.type as unknown) === "null") {
          return true;
        } else if (t.enum !== undefined && t.enum.length === 1 && t.enum[0] === null) {
          return true;
        } else {
          return false;
        }
      } else {
        return false;
      }
    };

    // TODO: Write a breaking test with itemsNumber === 0
    if (itemsNumber === 1) {
      Object.assign(jsonSchema, items[0] as unknown as JSONSchemaT);
    } else if (literals.length === itemsNumber) {
      jsonSchema.enum = literals;
    } else if (
      // OpenAPI 3.0 collapse of `X | null` into `{...X, nullable: true}`.
      target === "openapi-3.0" &&
      itemsNumber === 2 &&
      (isNullDefinition(items[0]!) || isNullDefinition(items[1]!))
    ) {
      const nullIsFirst = isNullDefinition(items[0]!);
      const nonNull = items[nullIsFirst ? 1 : 0]!;
      if (typeof nonNull !== "boolean") {
        const nonNullSchema = nonNull;
        Object.assign(jsonSchema, nonNullSchema);
        jsonSchema.nullable = true;
      } else {
        // `Any`/`Never` non-null variants can't be merged into a single
        // nullable schema; fall back to anyOf.
        jsonSchema.anyOf = items;
      }
    } else {
      jsonSchema.anyOf = items;
    }
  } else if (tag === objectTag) {
    const properties = schema.properties!;
    const additionalItems = schema.additionalItems!;
    if (typeof additionalItems === "object") {
      const childSchema = additionalItems;
      jsonSchema.type = "object";
      const childJsonSchema = internalToJSONSchema(
        childSchema,
        pathConcat(path, pathDynamic),
        defs,
        schema,
        target
      );
      jsonSchema.additionalProperties =
        Object.keys(childJsonSchema as unknown as Record<string, unknown>).length === 0
          ? true // JSONSchema.Any
          : childJsonSchema;
    } else {
      const required: string[] = [];
      const keys = Object.keys(properties);
      const jsonProperties: Record<string, JSONSchemaDefinition> = {};

      for (let idx = 0; idx <= keys.length - 1; idx++) {
        const key = keys[idx]!;
        const itemSchema = properties[key]!;
        const fieldSchema = internalToJSONSchema(
          itemSchema,
          pathConcat(path, pathFromLocation(key)),
          defs,
          schema,
          target
        );
        if (!isOptional(itemSchema)) {
          required.push(key);
        }
        jsonProperties[key] = fieldSchema;
      }

      jsonSchema.type = "object";
      jsonSchema.properties = jsonProperties;
      if (additionalItems === "strict") {
        jsonSchema.additionalProperties = false; // JSONSchema.Never
      }
      // Strip | Schema(_) => ()
      if (required.length !== 0) {
        jsonSchema.required = required;
      }
    }
  } else if (tag === refTag && schema["$ref"] === `${defsPath}${jsonName}`) {
    // S.json → empty {}
  } else if (tag === refTag) {
    jsonSchema.$ref = schema["$ref"];
  } else if (tag === nullTag) {
    if (target === "openapi-3.0") {
      // OpenAPI 3.0 has no `null` type. Use an enum as a workaround.
      jsonSchema.enum = [null];
    } else {
      jsonSchema.type = "null";
    }
  } else if (tag === neverTag) {
    jsonSchema.not = {}; // Schema({})
  } else {
    throw InternalError.make(
      B_makeInvalidInputDetails(
        // Just needs `.name` for the message - avoid json()'s recursive union.
        (() => {
          const s = baseSchema(unknownTag, false);
          s.name = jsonName;
          return s;
        })(),
        Flag.unsafeHas(TagFlag.get(parent.type), TagFlag.union) ? parent : schema,
        path,
        0 as unknown as undefined,
        false
      )
    );
  }

  applyMetadataOverlay(jsonSchema, schema, defs);

  return jsonSchema;
}


// -----------------------------------------------------------------------------
// toJSONSchema / enableStandardJSONSchema / extendJSONSchema
// -----------------------------------------------------------------------------

export type toJSONSchemaOptions = { target?: JsonSchemaTarget };

// Single source of truth for the `target` -> `$schema` URI mapping (mirrors
// @valibot/to-json-schema). Returns the URI to stamp, or `None` when the target
// has no `$schema` (openapi-3.0). Raises an `invalid_operation` error for
// `Unknown` (an unsupported target, e.g. one that arrived as an arbitrary
// string from JS via the Standard JSON Schema `Options`).
export function targetSchemaUri(target: JsonSchemaTarget): string | undefined {
  switch (target) {
    case "draft-07":
      return "http://json-schema.org/draft-07/schema#";
    case "draft-2020-12":
      return "https://json-schema.org/draft/2020-12/schema";
    // OpenAPI 3.0 has no `$schema` property.
    case "openapi-3.0":
      return undefined;
    default: {
      const unsupported = target;
      throw InternalError.make({
        code: "invalid_operation",
        path: pathEmpty,
        reason: `Unsupported JSON Schema target: ${unsupported}`,
      });
    }
  }
}

export function toJSONSchema(schema: Internal, options?: toJSONSchemaOptions): JSONSchemaT {
  // Resolve the target and the `$schema` URI to stamp. When no options object is
  // provided we keep the historical behavior: default to "draft-07" and do NOT
  // stamp `$schema`. With options, an unsupported target throws up front (even
  // for openapi-3.0, which stamps no `$schema`).
  let target: JsonSchemaTarget;
  let schemaUri: string | undefined;
  if (options !== undefined) {
    target = options.target !== undefined ? options.target : "draft-07";
    schemaUri = targetSchemaUri(target);
  } else {
    target = "draft-07";
    schemaUri = undefined;
  }
  const rootSchema = schema;
  const defs: Record<string, Internal> = {};
  const jsonSchema = internalToJSONSchema(
    rootSchema,
    pathEmpty,
    defs,
    rootSchema,
    target
  );
  delete (defs as Record<string, unknown>).JSON; // %raw(`delete defs.JSON`)
  const defsKeys = Object.keys(defs);
  if (defsKeys.length) {
    // Reuse the same object to prevent allocations
    // Nothing critical, just because we can
    const jsonSchemDefs = defs as unknown as Record<string, JSONSchemaDefinition>;
    defsKeys.forEach((key) => {
      const schema = defs[key]!;
      jsonSchemDefs[key] = internalToJSONSchema(
        schema,
        pathEmpty,
        // It's not possible to have nested recursive schema.
        // It should be grouped to a single $defs of the most top-level schema.
        0 as unknown as Record<string, Internal>,
        schema,
        target
      );
    });
    jsonSchema.$defs = jsonSchemDefs;
  }
  if (schemaUri !== undefined) {
    jsonSchema.$schema = schemaUri;
  }
  return jsonSchema;
}

// Wiring this inside a function (vs top level) is what makes toJSONSchema/reverse tree-shakeable.
//
// Mirrors @valibot/to-json-schema's `toStandardJsonSchema`: the `target` option
// selects the JSON Schema dialect (and the stamped `$schema` URI), and an
// unsupported target throws. `output` converts the reversed schema, since
// `S.reverse` swaps Input <-> Output and `toJSONSchema` returns the input-type
// schema of whatever it receives.
export function enableStandardJSONSchema(): void {
  standardJSONSchemaRef.contents = (
    schema: Internal,
    options: StandardJsonSchemaOptions,
    isOutput: boolean
  ) => {
    // The converter just forwards the target; `toJSONSchema` is the single
    // source of truth for the `$schema` URI mapping and the unsupported-target
    // throw. Passing an options object (vs none) is what makes `toJSONSchema`
    // stamp `$schema`, which the Standard JSON Schema spec requires.
    return toJSONSchema(isOutput ? reverse(schema) : schema, { target: options.target });
  };
}

export function extendJSONSchema(schema: Internal, jsonSchema: JSONSchemaT): Internal {
  const existingSchemaExtend = MetadataModule.get(schema, jsonSchemaMetadataId) as
    | JSONSchemaT
    | undefined;
  return MetadataModule.set(
    schema,
    jsonSchemaMetadataId,
    existingSchemaExtend !== undefined
      ? jsonSchemaMerge(existingSchemaExtend, jsonSchema)
      : jsonSchema
  );
}

// -----------------------------------------------------------------------------
// fromJSONSchema
// -----------------------------------------------------------------------------

// PORT-NOTE: `castAnySchemaToJsonableS` is a bare `Obj.magic` (a pure no-op
// type re-cast, `schema<'any> => schema<JSON.t>`). It has no runtime body, so
// no value is emitted here and every `->castAnySchemaToJsonableS` call below
// is simply dropped. If the public bindings layer needs the name, it's a TS
// `as` cast there.

// PORT-NOTE: the `let rec fromJSONSchema = { let helper = ...; jsonSchema => ... }`
// block-scoped helpers (primitiveToSchema, toIntSchema,
// definitionToDefaultValue) are hoisted to module-scope functions —
// same behavior, they close over nothing but module-level bindings.

function primitiveToSchema(primitive: unknown): Internal {
  return Literal_parse(primitive);
}

function toIntSchema(jsonSchema: JSONSchemaT): Internal {
  let schema = int();
  // TODO: Support jsonSchema.multipleOf when it's in rescript-schema
  // if (typeof jsonSchema.multipleOf === "number" && jsonSchema.multipleOf !== 1) {
  //  r += `.multipleOf(${jsonSchema.multipleOf})`;
  // }
  if (jsonSchema.minimum !== undefined) {
    schema = intMin(schema, jsonSchema.minimum | 0);
  } else if (jsonSchema.exclusiveMinimum !== undefined) {
    schema = intMin(schema, (jsonSchema.exclusiveMinimum + 1) | 0);
  }
  if (jsonSchema.maximum !== undefined) {
    schema = intMax(schema, jsonSchema.maximum | 0);
  } else if (jsonSchema.exclusiveMinimum !== undefined) {
    schema = intMax(schema, (jsonSchema.exclusiveMinimum - 1) | 0);
  }
  return schema;
}

function definitionToDefaultValue(definition: JSONSchemaDefinition): unknown {
  if (typeof definition !== "boolean") {
    return definition.default;
  } else {
    return undefined;
  }
}

export function fromJSONSchema(jsonSchema: JSONSchemaT): Internal {
  const anySchema = json();

  const jsonDefinitionToSchema = (definition: JSONSchemaDefinition): Internal => {
    if (typeof definition !== "boolean") {
      return fromJSONSchema(definition);
    } else if (definition === true) {
      // Any
      return anySchema;
    } else {
      // Never
      return never_();
    }
  };

  let schema: Internal;
  if ((jsonSchema as { nullable?: boolean }).nullable) {
    schema = null_(
      fromJSONSchema(
        jsonSchemaMerge(jsonSchema, { nullable: false } as unknown as JSONSchemaT)
      )
    );
  } else if (jsonSchema.type === "object") {
    if (jsonSchema.properties !== undefined) {
      const properties = jsonSchema.properties;
      const obj: Record<string, Internal> = {};
      Object.keys(properties).forEach((key) => {
        const property = properties[key]!;
        let propertySchema = jsonDefinitionToSchema(property);
        if (!(jsonSchema.required !== undefined && jsonSchema.required.includes(key))) {
          const defaultValue = definitionToDefaultValue(property);
          if (defaultValue !== undefined) {
            propertySchema = Option_getOr(option(propertySchema), defaultValue);
          } else {
            propertySchema = option(propertySchema);
          }
        }
        obj[key] = propertySchema;
      });
      schema = definitionToSchema(obj);
      if (jsonSchema.additionalProperties !== undefined && jsonSchema.additionalProperties === false) {
        schema = strict(schema);
      }
    } else {
      const additionalProperties = jsonSchema.additionalProperties;
      if (additionalProperties !== undefined) {
        if (additionalProperties === true) {
          // Any
          schema = dict(anySchema);
        } else if (additionalProperties === false) {
          // Never
          schema = strict(object(() => {}));
        } else {
          schema = dict(fromJSONSchema(additionalProperties));
        }
      } else {
        schema = schemaFactory(() => {});
      }
    }

    // TODO: jsonSchema.anyOf and jsonSchema.oneOf support
  } else if (jsonSchema.type === "array") {
    if (jsonSchema.prefixItems !== undefined) {
      // draft-2020-12 describes tuples with `prefixItems` instead of an
      // `items` array.
      const prefixItems = jsonSchema.prefixItems;
      schema = tuple((s: { item: (idx: number, schema: Internal) => unknown }) =>
        prefixItems.map((d, idx) => s.item(idx, jsonDefinitionToSchema(d)))
      );
    } else if (jsonSchema.items !== undefined) {
      const items = jsonSchema.items;
      if (Array.isArray(items)) {
        schema = tuple((s: { item: (idx: number, schema: Internal) => unknown }) =>
          items.map((d, idx) => s.item(idx, jsonDefinitionToSchema(d)))
        );
      } else {
        schema = array(jsonDefinitionToSchema(items));
      }
    } else {
      schema = array(anySchema);
    }
    if (jsonSchema.minItems !== undefined) {
      schema = arrayMinLength(schema, jsonSchema.minItems);
    }
    if (jsonSchema.maxItems !== undefined) {
      schema = arrayMaxLength(schema, jsonSchema.maxItems);
    }
  } else if (jsonSchema.anyOf !== undefined) {
    const definitions = jsonSchema.anyOf;
    if (definitions.length === 0) {
      schema = anySchema;
    } else if (definitions.length === 1) {
      schema = jsonDefinitionToSchema(definitions[0]!);
    } else {
      schema = union(definitions.map(jsonDefinitionToSchema));
    }
  } else if (jsonSchema.allOf !== undefined) {
    const definitions = jsonSchema.allOf;
    if (definitions.length === 0) {
      schema = anySchema;
    } else if (definitions.length === 1) {
      schema = jsonDefinitionToSchema(definitions[0]!);
    } else {
      schema = refine(
        anySchema,
        (data: unknown) => {
          return definitions.every((d) => {
            try {
              assertOrThrow(data, jsonDefinitionToSchema(d));
              return true;
            } catch (_) {
              return false;
            }
          });
        },
        "Should pass for all schemas of the allOf property."
      );
    }
  } else if (jsonSchema.oneOf !== undefined) {
    const definitions = jsonSchema.oneOf;
    if (definitions.length === 0) {
      schema = anySchema;
    } else if (definitions.length === 1) {
      schema = jsonDefinitionToSchema(definitions[0]!);
    } else {
      schema = refine(
        anySchema,
        (data: unknown) => {
          let validCount = 0;
          definitions.forEach((d) => {
            try {
              assertOrThrow(data, jsonDefinitionToSchema(d));
              validCount = validCount + 1;
            } catch (_) {
              // ()
            }
          });
          return validCount === 1;
        },
        "Should pass exactly one schema according to the oneOf property."
      );
    }
  } else if (jsonSchema.not !== undefined) {
    const not = jsonSchema.not;
    schema = refine(
      anySchema,
      (data: unknown) => {
        try {
          assertOrThrow(data, jsonDefinitionToSchema(not));
          return false;
        } catch (_) {
          return true;
        }
      },
      "Should NOT be valid against schema in the not property."
    );
    // needs to come before primitives
  } else if (jsonSchema.enum !== undefined) {
    const primitives = jsonSchema.enum;
    if (primitives.length === 0) {
      schema = anySchema;
    } else if (primitives.length === 1) {
      schema = primitiveToSchema(primitives[0]);
    } else {
      schema = union(primitives.map(primitiveToSchema));
    }
  } else if (jsonSchema.const !== undefined) {
    schema = primitiveToSchema(jsonSchema.const);
  } else if (Array.isArray(jsonSchema.type)) {
    const types = jsonSchema.type;
    schema = union(
      types.map((type_) => {
        return fromJSONSchema(
          jsonSchemaMerge(jsonSchema, { type: type_ })
        );
      })
    );
  } else if (jsonSchema.type === "string") {
    if (jsonSchema.format === "email") {
      schema = email();
    } else if (jsonSchema.format === "uri") {
      schema = url();
    } else if (jsonSchema.format === "uuid") {
      schema = uuid();
    } else if (jsonSchema.format === "date-time") {
      schema = isoDateTime();
    } else {
      schema = string();
    }
    if (jsonSchema.pattern !== undefined) {
      schema = pattern(schema, new RegExp(jsonSchema.pattern));
    }
    if (jsonSchema.minLength !== undefined) {
      schema = stringMinLength(schema, jsonSchema.minLength);
    }
    if (jsonSchema.maxLength !== undefined) {
      schema = stringMaxLength(schema, jsonSchema.maxLength);
    }
  } else if (jsonSchema.type === "integer") {
    schema = toIntSchema(jsonSchema);
  } else if (jsonSchema.type === "number" && jsonSchema.format === "int64") {
    schema = toIntSchema(jsonSchema);
  } else if (jsonSchema.type === "number" && jsonSchema.multipleOf === 1) {
    schema = toIntSchema(jsonSchema);
  } else if (jsonSchema.type === "number") {
    schema = float();
    if (jsonSchema.minimum !== undefined) {
      schema = floatMin(schema, jsonSchema.minimum);
    } else if (jsonSchema.exclusiveMinimum !== undefined) {
      schema = floatMin(schema, jsonSchema.exclusiveMinimum + 1);
    }
    if (jsonSchema.maximum !== undefined) {
      schema = floatMax(schema, jsonSchema.maximum);
    } else if (jsonSchema.exclusiveMinimum !== undefined) {
      schema = floatMax(schema, jsonSchema.exclusiveMinimum - 1);
    }
  } else if (jsonSchema.type === "boolean") {
    schema = bool();
  } else if (jsonSchema.type === "null") {
    schema = literal(null);
  } else if (
    jsonSchema.if !== undefined &&
    jsonSchema.then !== undefined &&
    jsonSchema.else !== undefined
  ) {
    const ifSchema = jsonDefinitionToSchema(jsonSchema.if);
    const thenSchema = jsonDefinitionToSchema(jsonSchema.then);
    const elseSchema = jsonDefinitionToSchema(jsonSchema.else);
    schema = refine(
      anySchema,
      (data: unknown) => {
        let passed;
        try {
          assertOrThrow(data, ifSchema);
          passed = true;
        } catch (_) {
          passed = false;
        }
        try {
          if (passed) {
            assertOrThrow(data, thenSchema);
          } else {
            assertOrThrow(data, elseSchema);
          }
          return true;
        } catch (_) {
          return false;
        }
      },
      "Should pass the if/then/else schema validation."
    );
  } else if (jsonSchema.type !== undefined) {
    throw InternalError.make({
      code: "invalid_operation",
      path: pathEmpty,
      reason: `Unsupported JSON Schema type: ${jsonSchema.type as unknown as string}`,
    });
  } else {
    schema = anySchema;
  }

  if (
    jsonSchema.description !== undefined ||
    jsonSchema.deprecated !== undefined ||
    jsonSchema.examples !== undefined ||
    jsonSchema.title !== undefined
  ) {
    // PORT-NOTE: ReScript's `title: ?jsonSchema.title` optional-field punning
    // assigns the option value directly (present-with-undefined when None) —
    // a plain object literal with possibly-undefined fields matches that.
    schema = meta(schema, {
      title: jsonSchema.title,
      description: jsonSchema.description,
      deprecated: jsonSchema.deprecated,
      examples: jsonSchema.examples,
    } as unknown as Parameters<typeof meta>[1]);
  }

  return schema;
}

// -----------------------------------------------------------------------------
// min / max / length
// -----------------------------------------------------------------------------

export function min(schema: Internal, minValue: number, maybeMessage?: string): Internal {
  switch (schema.type) {
    case stringTag:
      return stringMinLength(schema, minValue, maybeMessage);
    case arrayTag:
      return arrayMinLength(schema, minValue, maybeMessage);
    case numberTag:
      // Number({format: Int32 | Port}) vs Number(_)
      return schema.format === "int32" || schema.format === "port"
        ? intMin(schema, minValue, maybeMessage)
        : floatMin(schema, minValue as unknown as number, maybeMessage);
    default:
      return InternalError.panic(
        `S.min is not supported for ${toExpression(schema)} schema. Coerce the schema to string, number or array using S.to first.`
      );
  }
}

export function max(schema: Internal, maxValue: number, maybeMessage?: string): Internal {
  switch (schema.type) {
    case stringTag:
      return stringMaxLength(schema, maxValue, maybeMessage);
    case arrayTag:
      return arrayMaxLength(schema, maxValue, maybeMessage);
    case numberTag:
      // Number({format: Int32 | Port}) vs Number(_)
      return schema.format === "int32" || schema.format === "port"
        ? intMax(schema, maxValue, maybeMessage)
        : floatMax(schema, maxValue as unknown as number, maybeMessage);
    default:
      return InternalError.panic(
        `S.max is not supported for ${toExpression(schema)} schema. Coerce the schema to string, number or array using S.to first.`
      );
  }
}

export function length(schema: Internal, length: number, maybeMessage?: string): Internal {
  switch (schema.type) {
    case stringTag:
      return stringLength(schema, length, maybeMessage);
    case arrayTag:
      return arrayLength(schema, length, maybeMessage);
    default:
      return InternalError.panic(
        `S.length is not supported for ${toExpression(schema)} schema. Coerce the schema to string or array using S.to first.`
      );
  }
}

// -----------------------------------------------------------------------------
// Trailing public re-typings (Sury.res lines 7949-7971)
// -----------------------------------------------------------------------------
//
// PORT-NOTE: every one of these is a PURE NO-OP — a bare `Obj.magic` (or
// `castToPublic` for `unknown`) that re-types an existing function/value from
// its `internal`-returning form to the public `t<'x>`-returning form without
// touching the runtime value. In this TS port the runtime object is `Internal`
// everywhere and the public typing lives in the bindings layer, so NO runtime
// code is emitted for any of them. Listed for completeness (all no-ops):
//
//   nullAsUnit, never_, unknown (castToPublic of the `unknown` schema const),
//   unit, nullLiteral, nan, string, bool, int, float, bigint, symbol, date,
//   json, jsonString, jsonStringWithSpace, uint8Array, isoDateTime, port,
//   email, uuid, cuid, url
//
// The bindings layer (Sury.res / S.d.ts) should re-export the already-defined
// functions of the same names under their public types.
