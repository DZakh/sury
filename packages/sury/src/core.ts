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
  with: (a: Flag, b: Flag): Flag => a | b,
  unsafeHas: (flag: Flag, test: Flag): boolean => (flag & test) === test,
};

// Internal-only flag bits threaded through `val.f` during codegen (distinct
// bit space from the public `Flag` module above).
export const ValFlag = {
  none: 0,
  async: 1,
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

const InternalError = {
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

function baseSchema(tag: Tag, selfReverse: boolean): Internal {
  const schema = new (Schema as unknown as { new (): Internal })();
  schema.type = tag;
  schema.seq = seq++;
  if (selfReverse) {
    valueOptions[valKey] = schema;
    Object.defineProperty(schema, reversedKey, { ...configurableValueOptions, value: schema });
  }
  return schema;
}

function noopDecoder(input: Val): Val {
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
