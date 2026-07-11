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
function inlinedValueFromString(str: string): string {
  for (let idx = 0; idx < str.length; idx++) {
    const ch = str[idx];
    if (ch === '"' || ch === "\n") return JSON.stringify(str);
  }
  return `"${str}"`;
}

function pathFromInlinedLocation(inlinedLocation: string): Path {
  return `[${inlinedLocation}]`;
}

function pathFromLocation(location: string): Path {
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
  // @as("$ref")
  ref?: string;
  // @as("$defs")
  defs?: Record<string, Internal>;
  isAsync?: boolean; // Optional value means that it's not lazily computed yet.
  hasTransform?: boolean; // Optional value means that it's not lazily computed yet.
  // @as("~standard")
  standard?: unknown;
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
  return (obj as { standard?: unknown }).standard as unknown as boolean;
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
// can keep saying `B._var` / `B.failInvalidType`.

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
  const v = B.varWithoutAllocation(val.g);
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
    const v = B.varWithoutAllocation(val.g);
    B.hoistDecl(parent, `${v}=${val.i}`);
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
    const v = B.varWithoutAllocation(val.g);
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
        B.hoistDecl(val, v);
      } else {
        B.hoistDecl(val, `${v}=${val.i}`);
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
  return B.invalidInputBuilder(undefined, undefined, override)(input);
}

export const B = {
  embed(b: Val, value: unknown): string {
    const e = b.g.e;
    const l = e.length;
    e[l] = value;
    return `e[${l}]`;
  },

  inlineConst(b: Val, schema: Internal): string {
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
      return B.embed(b, schema.const);
    } else {
      return const_ as unknown as string;
    }
  },

  // Escape it once per compiled operation.
  // Use bGlobal as cache, so we don't allocate another object + it's garbage collected.
  inlineLocation(global: BGlobal, location: string): string {
    const key = `"${location}"`;
    const cached = (global as unknown as Record<string, string | undefined>)[key];
    if (cached !== undefined) {
      return cached;
    } else {
      const inlinedLocation = inlinedValueFromString(location);
      (global as unknown as Record<string, string>)[key] = inlinedLocation;
      return inlinedLocation;
    }
  },

  _var,
  _bondVar,
  _prevVar,

  varWithoutAllocation(global: BGlobal): string {
    const newCounter = global.v + 1;
    global.v = newCounter;
    return `v${newCounter}`;
  },

  // Append a `let` declaration to a still-open owner val, emitted after the
  // owner's checks in `merge`. The owner is the materialized val's immediate
  // context (its `prev`, its `parent` for a field read, or itself); since the
  // decl lands at the owner's segment end — after the owner's guard, before
  // its dependent code — that immediate owner already dominates and outlives
  // every use, so no separate scope-tree is needed. The owner must be
  // unfinalized; `_notVarAtParent` guards this explicitly.
  hoistDecl(owner: Val, decl: string): void {
    owner.hd = owner.hd === "" ? decl : owner.hd + "," + decl;
  },

  _notVarBeforeValidation,
  _notVarAtParent,
  _notVar,

  operationArg(
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
  },

  throw(errorDetails: ErrorDetails): never {
    throw InternalError.make(errorDetails);
  },

  unsupportedDecode(b: Val, from: Internal, target: Internal): never {
    return B.throw({
      code: "unsupported_decode",
      from: from,
      to: target,
      reason: `Can't decode ${toExpression(from)} to ${toExpression(
        target
      )}. Use S.to to define a custom decoder`,
      path: b.path,
    });
  },

  failWithArg<Arg>(b: Val, fn: (arg: Arg) => ErrorDetails, arg: string): string {
    return `${B.embed(b, (arg: Arg) => {
      B.throw(fn(arg));
    })}(${arg})`;
  },

  makeInvalidConversionDetails(input: Val, to: Internal, cause: unknown): ErrorDetails {
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
  },

  // Checks run against `prev.var()`, so the runtime type at check time
  // is `prev.schema`, not the post-narrowing schema on the current val.
  receivedSchema(val: Val): Internal {
    return val.prev !== undefined ? val.prev.s : val.s;
  },

  makeInvalidInputDetails(
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
  },

  // Drop-in `check.fail` builder for InvalidInput failures. The returned
  // `(~input) => value => details` closure snapshots expected/received/path
  // so it does not retain the val (otherwise the embed array would pin the
  // whole val chain). Pass directly as `check.fail` to skip the wrapper.
  invalidInputBuilder(
    expected?: Internal,
    extraPath: Path = pathEmpty,
    reasonOverride?: string,
    includeInput: boolean = true
  ): (input: Val) => (value: unknown) => ErrorDetails {
    return (input: Val) => {
      const expected_ = expected !== undefined ? expected : input.e;
      const received = B.receivedSchema(input);
      const path = extraPath === pathEmpty ? input.path : pathConcat(input.path, extraPath);
      return (value: unknown) =>
        B.makeInvalidInputDetails(
          expected_,
          received,
          path,
          value,
          includeInput,
          undefined,
          reasonOverride
        );
    };
  },

  failInvalidType,

  failWithErrorMessage(
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
        return B.invalidInputBuilder(undefined, undefined, m)(input);
      } else {
        return failInvalidType(input);
      }
    };
  },

  // Inline variant: emits the throw expression directly. Used by decoders
  // that splice errors into custom JS (e.g. `catch(_){${embedInvalidInput}}`),
  // not via the `check` pipeline.
  embedInvalidInput(input: Val, expected: Internal = input.e): string {
    return B.failWithArg(input, B.invalidInputBuilder(expected)(input), input.v());
  },

  // Caller must verify `val.checks->unsafeToBool` and
  // `val.expected.noValidation !== Some(true)` first — the unwrap below
  // is unchecked. `inputVar` is usually `val.prev.var()`.
  emitChecks(val: Val, inputVar: string): string {
    const checks = val.vc!;
    const len = checks.length;
    if (len === 1) {
      const check = checks[0]!;
      return `${check.c(inputVar)}||${B.failWithArg(val, check.f(val), inputVar)};`;
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
        out = out + `${cond}||${B.failWithArg(val, fail(val), inputVar)};`;
      }
      return out;
    }
  },

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
  isHoistable(val: Val): boolean {
    return val.t === true ? val.prev!.t !== true && val.cp === "" : true;
  },

  // Walks the val.prev chain and assembles generated code. When
  // `~hoistCond` is provided (union codegen), type-narrow checks
  // (fail === failInvalidType) lift into that ref as a dispatch
  // discriminant instead of being emitted; constraint refines still
  // emit inline so their case-specific error message survives. All
  // other callers pass no `~hoistCond` and get the plain merge:
  // every non-`noValidation` check is emitted inline.
  merge(val: Val, hoistCond?: { contents: string }): string {
    let current: Val | undefined = val;
    let code = "";

    while (current !== undefined) {
      const val: Val = current;
      current = val.prev;

      let currentCode = "";

      if (val.vc) {
        if (hoistCond !== undefined && B.isHoistable(val)) {
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
                currentCode + `${condCode}||${B.failWithArg(val, check.f(val), inputVar)};`;
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
          currentCode = B.emitChecks(val, prev.v());
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
  },

  next(prev: Val, initial: string, schema: Internal, expected: Internal = prev.e): Val {
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
  },

  // Pass a non-empty `~checks` or omit it. Never pass `~checks=[]` —
  // that would break the val.checks "absent iff no checks" invariant.
  refine(val: Val, schema: Internal = val.s, checks?: Check[], expected: Internal = val.e): Val {
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
  },

  // Lazy-allocate helper for mutating an existing val (as opposed to
  // building a local array and passing it through `refine`).
  pushCheck(val: Val, check: Check): void {
    if (val.vc !== undefined) {
      val.vc.push(check);
    } else {
      val.vc = [check];
    }
  },

  // Applies both refiners. Input checks push onto valInput.checks
  // (emit at pre-transform slot); output checks wrap val via refine.
  // When valInput.prev is None, input checks fold into the output
  // wrap so emit has a prev.var(). Sets isOutput on the result.
  // TODO: async output refiner must run inside .then(), not on the Promise.
  markOutput(val: Val, valInput: Val): Val {
    let deferredInputChecks: Check[] | undefined;
    const inputRefiner = valInput.e.inputRefiner;
    if (inputRefiner !== undefined) {
      const checks = inputRefiner(valInput);
      if (checks.length > 0) {
        if (valInput.prev !== undefined) {
          for (let i = 0; i < checks.length; i++) {
            B.pushCheck(valInput, checks[i]!);
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
      result = B.refine(val, undefined, deferredInputChecks.concat(outputChecks));
    } else if (deferredInputChecks !== undefined) {
      result = B.refine(val, undefined, deferredInputChecks);
    } else if (outputChecks !== undefined) {
      result = B.refine(val, undefined, outputChecks);
    } else {
      result = val;
    }
    result.io = true;
    return result;
  },

  // Used in union codegen: splice a literal child's checks into the parent
  // as dispatch discriminants. Each cond's `inputVar` is rewritten to
  // `parent[key]`; `fail` stays shared so lifted checks fuse with the
  // parent's own type guard. No-op if the child has no checks.
  hoistChildChecks(parent: Val, child: Val, key: string): void {
    if (child.vc) {
      const pathAppend = pathFromInlinedLocation(B.inlineLocation(parent.g, key));
      child.vc!.forEach((check) => {
        B.pushCheck(parent, {
          c: (inputVar) => check.c(inputVar + pathAppend),
          f: check.f,
        });
      });
      child.vc = undefined;
    }
  },

  dynamicScope(from: Val, locationVar: string): Val {
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
  },

  nextConst(from: Val, schema: Internal, expected?: Internal): Val {
    return B.next(from, B.inlineConst(from, schema), schema, expected);
  },

  asyncVal(from: Val, initial: string): Val {
    const v = B.next(from, initial, from.s);
    v.f = ValFlag.async;
    return v;
  },

  Val: {
    Object: {
      add(objectVal: Val, location: string, val: Val): void {
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
        objectVal.cp = objectVal.cp + B.merge(val);
        objectVal.d![location] = val;
      },

      merge(target: Val, vals: Record<string, Val>): void {
        const locations = Object.keys(vals);
        for (let idx = 0; idx < locations.length; idx++) {
          const location = locations[idx]!;
          B.Val.Object.add(target, location, vals[location]!);
        }
      },
    },

    var(val: Val): string {
      return val.v();
    },

    addKey(objVal: Val, key: string, value: Val): string {
      return `${objVal.v()}[${key}]=${value.i}`;
    },

    scope(val: Val): Val {
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
    },
  },

  embedTransformation(input: Val, fn: (input: unknown) => unknown, isAsync: boolean): Val {
    const outputVar = B.varWithoutAllocation(input.g);
    const output = B.next(input, outputVar, unknown, input.e.to!);
    output.v = _var;
    if (isAsync) {
      if (!Flag.unsafeHas(input.g.o, Flag.async)) {
        B.throw({
          code: "invalid_operation",
          path: pathEmpty,
          reason:
            "Encountered unexpected async transform or refine. Use parseAsyncOrThrow operation instead",
        });
      }
      output.f = Flag.with(output.f, ValFlag.async);
    }
    const embededFn = B.embed(input, fn);
    const failure = `${B.failWithArg(
      output,
      (e: unknown) => B.makeInvalidConversionDetails(input, unknown, e),
      `x`
    )}`;
    // Feed the transform the input's var when it already carries checks — it's
    // materialized into a var anyway (the check references it), so reuse it
    // instead of re-inlining the source expression (e.g. `i["x"]`) twice.
    output.cp = `let ${outputVar};try{${outputVar}=${embededFn}(${
      input.vc ? input.v() : input.i
    })${isAsync ? `.catch(x=>${failure})` : ""}}catch(x){${failure}}`;
    return output;
  },

  effectCtx(input: Val): EffectCtx {
    return {
      fail: (message: string, path: Path = pathEmpty): never => {
        const error = InternalError.make(
          B.invalidInputBuilder(undefined, path, message, false)(input)(void 0)
        );
        // Read about this in shouldPrependPathKey comment.
        (error as Record<string, unknown>)[shouldPrependPathKey] = 1;
        throw error;
      },
    };
  },

  invalidOperation(val: Val, description: string): never {
    return B.throw({ code: "invalid_operation", reason: description, path: val.path });
  },

  mergeWithCatch(
    val: Val,
    catchFn: (errorVar: string) => string,
    appendSafe?: () => string
  ): string {
    const valCode = B.merge(val);
    if (
      valCode === "" &&
      // FIXME: Instead of this wrap all S.transform in a try/catch
      !Flag.unsafeHas(val.f, ValFlag.async)
    ) {
      return valCode + (appendSafe !== undefined ? appendSafe() : "");
    } else {
      const errorVar = B.varWithoutAllocation(val.g);

      const catchCode = `${catchFn(errorVar)};throw ${errorVar}`;

      if (Flag.unsafeHas(val.f, ValFlag.async)) {
        val.i = `${val.i}.catch(${errorVar}=>{${catchCode}})`;
      }
      return `try{${valCode}${
        appendSafe !== undefined ? appendSafe() : ""
      }}catch(${errorVar}){${catchCode}}`;
    }
  },

  mergeWithPathPrepend(
    val: Val,
    parent: Val,
    locationVar?: string,
    appendSafe?: () => string
  ): string {
    if (val.path === pathEmpty && locationVar === undefined) {
      return B.merge(val);
    } else {
      return B.mergeWithCatch(
        val,
        (errorVar) =>
          `${errorVar}.path=${
            parent.path === "" ? "" : `${inlinedValueFromString(parent.path)}+`
          }${locationVar !== undefined ? `'["'+${locationVar}+'"]'+` : ""}${errorVar}.path`,
        appendSafe
      );
    }
  },
};

export const noopOperation = (i: unknown): unknown => i;
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
//   B.embed, B.refine, B.next, B.nextConst, B.varWithoutAllocation, B._var,
//   B.unsupportedDecode, B.failInvalidType, B.embedInvalidInput, B.inlineConst
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
  `${inputVar} instanceof ${B.embed(b, class_)}`;

export const numberDecoder: Builder = (input: Val) => {
  const inputTagFlag = TagFlag.get(input.s.type);
  if (Flag.unsafeHas(inputTagFlag, TagFlag.unknown)) {
    const checks: Check[] = [
      {
        c: typeofCond(numberTag),
        f: B.failInvalidType,
      },
    ];
    if (input.e.format === "int32") {
      checks.push({
        c: (inputVar) => int32FormatValidation(inputVar),
        f: B.failInvalidType,
      });
    } else {
      if (!Flag.unsafeHas(input.g.o, Flag.disableNanNumberValidation)) {
        checks.push({
          c: (inputVar) => `!${nanCond(inputVar)}`,
          f: B.failInvalidType,
        });
      }
    }
    return B.refine(input, input.e, checks);
  } else if (Flag.unsafeHas(inputTagFlag, TagFlag.string)) {
    const outputVar = B.varWithoutAllocation(input.g);

    const output = B.next(input, outputVar, input.e);
    output.v = B._var;
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
        f: B.failInvalidType,
      },
    ];
    return output;
  } else if (!Flag.unsafeHas(inputTagFlag, TagFlag.number)) {
    return B.unsupportedDecode(input, input.s, input.e);
  } else if (input.s.format !== input.e.format && input.e.format === "int32") {
    return B.refine(input, input.e, [
      {
        c: (inputVar) => int32FormatValidation(inputVar),
        f: B.failInvalidType,
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
  return B.next(input, `""+${input.i}`, string());
}
export function stringDecoderFn(input: Val): Val {
  const inputTagFlag = TagFlag.get(input.s.type);
  if (Flag.unsafeHas(inputTagFlag, TagFlag.unknown)) {
    return B.refine(input, input.e, [
      {
        c: typeofCond(stringTag),
        f: B.failInvalidType,
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
    return B.next(input, `"${const_}"`, schema);
  } else if (
    Flag.unsafeHas(
      inputTagFlag,
      Flag.with(TagFlag.boolean, Flag.with(TagFlag.number, TagFlag.bigint)),
    )
  ) {
    return inputToString(input);
  } else if (!Flag.unsafeHas(inputTagFlag, TagFlag.string)) {
    return B.unsupportedDecode(input, input.s, input.e);
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
    return B.refine(input, input.e, [
      {
        c: typeofCond(booleanTag),
        f: B.failInvalidType,
      },
    ]);
  } else if (Flag.unsafeHas(inputTagFlag, TagFlag.string)) {
    const outputVar = B.varWithoutAllocation(input.g);

    const output = B.next(input, outputVar, input.e);
    output.v = B._var;

    const inputVar = input.v();
    output.cp = `let ${outputVar};(${output.i}=${inputVar}==="true")||${inputVar}==="false"||${B.embedInvalidInput(
      input,
    )};`;
    return output;
  } else if (!Flag.unsafeHas(inputTagFlag, TagFlag.boolean)) {
    return B.unsupportedDecode(input, input.s, input.e);
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
    return B.refine(input, input.e, [
      {
        c: typeofCond(bigintTag),
        f: B.failInvalidType,
      },
    ]);
  } // TODO: Skip formats which 100% don't match
  else if (Flag.unsafeHas(inputTagFlag, TagFlag.string)) {
    const outputVar = B.varWithoutAllocation(input.g);
    const output = B.next(input, outputVar, input.e);
    output.v = B._var;
    output.cp = `let ${outputVar};try{${outputVar}=BigInt(${input.v()})}catch(_){${B.embedInvalidInput(
      input,
    )}}`;
    return output;
  } else if (Flag.unsafeHas(inputTagFlag, TagFlag.number)) {
    return B.next(input, `BigInt(${input.i})`, input.e);
  } else if (!Flag.unsafeHas(inputTagFlag, TagFlag.bigint)) {
    return B.unsupportedDecode(input, input.s, input.e);
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
    return B.refine(input, input.e, [
      {
        c: typeofCond(symbolTag),
        f: B.failInvalidType,
      },
    ]);
  } else if (!Flag.unsafeHas(inputTagFlag, TagFlag.symbol)) {
    return B.unsupportedDecode(input, input.s, input.e);
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
    return B.nextConst(input, expectedSchema);
  } else if (isLiteral(input.s)) {
    if (input.s.const === expectedSchema.const) {
      return input;
    } else {
      return B.nextConst(input, expectedSchema);
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

      const stringConstVal = B.nextConst(input, stringConstSchema, stringConstSchema);

      stringConstVal.vc = [
        {
          c: (inputVar) => `${inputVar}==="${stringConstSchema.const as unknown as string}"`,
          f: B.failInvalidType,
        },
      ];

      return B.nextConst(stringConstVal, expectedSchema, expectedSchema);
    } else if (Flag.unsafeHas(schemaTagFlag, TagFlag.nan)) {
      return B.refine(input, expectedSchema, [
        {
          c: nanCond,
          f: B.failInvalidType,
        },
      ]);
    } else {
      return B.refine(input, expectedSchema, [
        {
          c: (inputVar) => `${inputVar}===${B.inlineConst(input, expectedSchema)}`,
          f: B.failInvalidType,
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

export const Literal = {
  parse: (value: unknown): Internal => {
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
  },
};
// =============================================================================
// Section: Sury.res lines 2256-2708
// parse / parseDynamic / isAsyncInternal / compileDecoder / getOutputSchema /
// reverse / getDecoder / nestedLoc / itemCode / neverBuilderFn / never_ /
// nestedOptionParser / instanceDecoder / instance / typeCheckCond
//
// TODO(integration): expects from earlier sections:
//   - `B` (Builder.B): B.Val.scope, B.next, B.refine, B.merge, B.markOutput,
//     B.operationArg, B.operationArgVar, B.unsupportedDecode,
//     B.embedInvalidInput, B.inlineConst, B.failInvalidType
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

    if (loopInput.e.defs as unknown as boolean) {
      if (loopInput.g.d as unknown as boolean) {
        Object.assign(loopInput.g.d!, loopInput.e.defs!);
      } else {
        loopInput.g.d = loopInput.e.defs;
      }
    }

    if (
      Flag.unsafeHas(
        loopInput.f,
        ValFlag.async,
      ) /* FIXME: why was it needed? && step.contents !== #convert */
    ) {
      const operationInputVar = loopInput.v();

      const operationInput = B.Val.scope(loopInput);
      const operationOutput = parse(operationInput);
      const operationCode = B.merge(operationOutput);
      if (operationInput.i !== operationOutput.i || operationCode !== "") {
        valRef = B.next(
          loopInput,
          `${operationInputVar}.then(${operationInputVar}=>{${operationCode}return ${operationOutput.i}})`,
          operationOutput.s,
          operationOutput.e,
        );
      } else {
        valRef = B.refine(loopInput, operationOutput.s, undefined, operationOutput.e);
      }
      valRef.f = Flag.with(valRef.f, ValFlag.async);
      valRef.io = true;
    } else if (loopInput.io) {
      // It's guaranteed that to is not None, because it's checked in the while condition
      const to = loopInput.e.to!;
      if (loopInput.e.parser !== undefined) {
        valRef = loopInput.e.parser(loopInput);
      } else {
        valRef = B.refine(valRef, undefined, undefined, to);
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
          valRef = B.markOutput(valRef, valRef);
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
    const input = B.operationArg(unknown, schema, Flag.async, defs);
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
  const input = B.operationArg(isLiteral(schema) ? unknown : schema, expected, flag, defs);

  const output = parse(input);
  const code = B.merge(output);

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
      if (mut.defs !== undefined) {
        const defs = mut.defs;
        const reversedDefs: Record<string, Internal> = {};
        for (let idx = 0; idx <= Object.keys(defs).length - 1; idx++) {
          const key = Object.keys(defs)[idx]!;
          reversedDefs[key] = reverse(defs[key]!);
        }
        mut.defs = reversedDefs;
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
  const output = B.refine(input, undefined, undefined, never_());
  output.cp = B.embedInvalidInput(input) + ";";
  return output;
}
export function never_(): Internal {
  return cached(neverTag as string, neverTag, (s) => {
    s.decoder = neverBuilderFn;
  });
}

export const nestedOptionParser: Builder = ((input: Val) => {
  const nextSchema = input.e.to!;
  return B.next(
    input,
    `{${nestedLoc}:${getOutputSchema(input.e).properties![nestedLoc]!.const as unknown as string}}`,
    nextSchema,
    nextSchema
  );
});

export const instanceDecoder: Builder = ((input: Val) => {
  const inputTagFlag = TagFlag.get(input.s.type);
  if (Flag.unsafeHas(inputTagFlag, TagFlag.unknown)) {
    return B.refine(input, input.e, [
      {
        c: instanceofCond(input, input.e.class),
        f: B.failInvalidType,
      },
    ]);
  } else if (Flag.unsafeHas(inputTagFlag, TagFlag.instance) && input.s.class === input.e.class) {
    return input;
  } else {
    return B.unsupportedDecode(input, input.s, input.e);
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
    return `${inputVar}===${B.inlineConst(input, schema)}`;
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
