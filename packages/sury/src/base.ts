// The base layer: the data model (`Internal`, `Val`, `Check`), the schema
// object and its prototype, tags, flags and paths — merged into one module
// because they are mutually dependent by nature and splitting them bought
// nothing but import churn. Nothing here imports from another module in the
// package: base is the bottom of the layering, so every other module can reach
// it without a cycle.

// Lives here rather than in builder.ts so base has no outgoing edge: both are
// one-liners over `Val`/`Internal`, and builder.ts importing them back is free.
export type Builder = (input: Val) => Val;
export type Encoder = (input: Val, target: Internal) => Val;

// ── flags ─────────────────────────────────────────────────────────────────────

export type Flag = number;

export const flagNone: Flag = 0;
export const flagAsync: Flag = 1;
export const flagDisableNanNumberValidation: Flag = 2;
// Compile-time context: a custom transform emitted inside a union case must
// preserve the original exception so union dispatch can distinguish Sury
// failures (fall through) from foreign exceptions (escape).
export const flagUnionTransformContext: Flag = 4;
// flatten: 64

export const flagUnsafeHas = (acc: Flag, flag: Flag): boolean => {
  return (acc & flag) !== 0;
}

export const valFlagNone: Flag = 0;
export const valFlagAsync: Flag = 1;

// ── path ──────────────────────────────────────────────────────────────────────

export type Path = string;

export const pathEmpty: Path = "";
export const pathDynamic: Path = "[]";

export const inlinedValueFromString = (str: string): string => {
  return str.includes('"') || str.includes("\n") ? JSON.stringify(str) : `"${str}"`;
}

export const pathFromInlinedLocation = (inlinedLocation: string): Path => {
  return `[${inlinedLocation}]`;
}

// @__NO_SIDE_EFFECTS__
export const pathFromLocation = (location: string): Path => {
  return `[${inlinedValueFromString(location)}]`;
}

// @__NO_SIDE_EFFECTS__
export const pathToArray = (path: Path): string[] => {
  return path === "" ? [] : (JSON.parse(path.split(`"]["`).join(`","`)) as string[]);
}

// @__NO_SIDE_EFFECTS__
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

// @__NO_SIDE_EFFECTS__
export const pathConcat = (path: Path, concatedPath: Path): Path => {
  return path + concatedPath;
}

// ── tags ──────────────────────────────────────────────────────────────────────

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
  | "anyOf"
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
export const anyOfTag: Tag = "anyOf";
export const neverTag: Tag = "never";
export const unknownTag: Tag = "unknown";
export const refTag: Tag = "ref";

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
export const tagFlags: Record<Tag, number> = {
  [unknownTag]: 1,
  [stringTag]: 2,
  [numberTag]: 4,
  [booleanTag]: 8,
  [undefinedTag]: 16,
  [nullTag]: 32,
  [objectTag]: 64,
  [arrayTag]: 128,
  [anyOfTag]: 256,
  [refTag]: 512,
  [bigintTag]: 1024,
  [nanTag]: 2048,
  [functionTag]: 4096,
  [instanceTag]: 8192,
  [neverTag]: 32768,
  [symbolTag]: 16384,
};

// ── types ─────────────────────────────────────────────────────────────────────

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
  // Sury's own "this read may be absent" union — a dict value read by a fixed
  // key, modelled as `V | undefined`. The conversion rules (2-4) don't apply to
  // it: it isn't a user-written widening whose intent could be ambiguous, so
  // each variant converts to whatever the target is, and a variant with no
  // decoder to that target drops out with its error reported per value.
  perVariant?: boolean;
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
  // @as("t") — throwCounter. Bumped by every helper that emits a raise into
  // generated code, so a builder can bracket a stretch of emission and learn
  // whether what it produced can throw. Read the difference, never the value.
  t: number;
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

// Shared `undefined` for every value-position use across the implementation:
// a bare `undefined` minifies to `void 0` (6 chars), this const to 1. Never
// interpolate it into generated-code strings — emitted JS text keeps literal
// `void 0`.
export const U = undefined;

export const immutableEmptyArray: unknown[] = [];
// Null-prototype: used as a schema's `properties` placeholder, so an
// indexed/`in` lookup for a field named after an Object.prototype member
// (constructor, toString, hasOwnProperty, ...) must not resolve to
// something inherited instead of correctly reporting "no such property".
export const immutableEmptyObject: Record<string, unknown> = Object.create(null);

// Probe the Standard Schema marker's *presence* with `in` instead of reading
// it: the `~standard` prototype getter allocates a fresh StandardProps object
// (+4 closures) on every access, and this runs per-node while building every
// `S.schema({...})`. `in` walks the prototype chain without invoking the
// getter. The `typeof === object` guard keeps primitives (passed by
// `js_assert`) from throwing on `in` and reproduces the old falsy-on-primitive
// result.
export const isSchemaObject = (obj: unknown): boolean => {
  return typeof obj === objectTag && obj !== null && "~standard" in (obj as object);
}

export const constField = "const";
export const isLiteral = (schema: Internal): boolean => {
  return constField in schema;
}

export const isOptional = (schema: Internal): boolean => {
  return (
    schema.type === undefinedTag ||
    (schema.type === anyOfTag && undefinedTag in schema.has!)
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

// @__NO_SIDE_EFFECTS__
export const toExpression = (schema: Internal): string => {
  if (schema.name !== U) {
    return schema.name;
  } else if (schema.const !== U) {
    return stringify(schema.const);
  } else if (schema.anyOf !== U) {
    // Repeated members remain significant to decoding (the same effectful
    // schema may intentionally run more than once), but not to the human
    // expression describing the union. Identity-only deduplication avoids
    // conflating distinct symbols/classes that merely render alike.
    return [...new Set(schema.anyOf)].map(toExpression).join(" | ");
  } else if (schema.format === "compactColumns") {
    // For compactColumns, show the column types if we have properties from .to
    const to = schema.to;
    if (to !== U) {
      const props = to.properties;
      if (props !== U) {
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
      if (additionalItems !== U && typeof additionalItems === "object") {
        const innerArraySchema = additionalItems;
        return `${toExpression(innerArraySchema)}[]`;
      } else {
        return "unknown[][]";
      }
    }
  } else if (schema.format !== U) {
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
      return (additionalItems.type === anyOfTag ? `(${itemName})` : itemName) + "[]";
    } else {
      return `[${items.map((schema) => toExpression(schema)).join(", ")}]`;
    }
  } else if (schema.type === instanceTag) {
    return (schema.class as { name: string }).name;
  } else {
    return schema.type;
  }
}

// ── schema ────────────────────────────────────────────────────────────────────

export function Schema(this: Internal): void {}
export const schemaPrototype: Record<string, unknown> = Object.create(null);
// A plain (non-enumerable) method, not a getter returning a closure: the
// getter form allocated a fresh arrow on every `.with` access, and `.with` is
// the primary modifier API called all over user construction code. The method
// binds `this` through the call, so no per-access closure is needed.
Object.defineProperty(schemaPrototype, "with", {
  value(this: Internal, fn: (self: Internal, ...args: unknown[]) => unknown, ...args: unknown[]): unknown {
    return fn(this, ...args);
  },
});
// Also has ~standard below
Schema.prototype = schemaPrototype;

let seq = 1;

let exnId: unknown = {};
export const __setExnId = (id: unknown): void => {
  exnId = id;
}

export class SuryError extends Error {
  constructor(params: ErrorDetails | Record<string, unknown>) {
    super();
    Object.assign(this, params);
  }
  get message(): string {
    return formatErrorMessage(this as unknown as SuryErrorRecord);
  }
  get _1(): this {
    return this;
  }
  get RE_EXN_ID(): unknown {
    return exnId;
  }
}
Object.defineProperty(SuryError.prototype, "name", { value: "SuryError" });
Object.defineProperty(SuryError.prototype, "s", { value: s });

export const getOrRethrow = (exn: unknown): SuryErrorRecord => {
  if (exn && (exn as { s?: symbol }).s === s) {
    return exn as SuryErrorRecord;
  } else {
    throw exn;
  }
}

// Internal invariant/misuse errors (bad schema construction, not input
// validation) — intentionally a plain Error, not SuryError: there's no
// ErrorDetails shape (code/path/reason) to attach at these call sites.
export const panic = (message: string): never => {
  throw new Error(`[Sury] ${message}`);
}

const formatErrorMessage = (error: SuryErrorRecord): string => {
  return `${error.path === "" ? "" : `Failed at ${error.path}: `}${error.reason}`;
}

export const errorClass: unknown = SuryError;

export type GlobalConfig = {
  m: (error: SuryErrorRecord) => string; // messageFormatter
  d?: Record<string, Internal>; // defsAccumulator
  a: AdditionalItems; // defaultAdditionalItems
  f: Flag; // defaultFlag
}

export type GlobalConfigOverride = {
  defaultAdditionalItems?: AdditionalItemsMode;
  disableNanNumberValidation?: boolean;
}

export const initialOnAdditionalItems: AdditionalItemsMode = "strip";
export const initialDefaultFlag: Flag = valFlagNone;
export const globalConfig: GlobalConfig = {
  m: formatErrorMessage,
  d: U,
  a: initialOnAdditionalItems,
  f: initialDefaultFlag,
};

export const valueOptions: Record<string, unknown> = {};
export const configurableValueOptions = { configurable: true };
export const valKey = "value";
export const reversedKey = "r";

const SchemaCtor = Schema as unknown as { new (): Internal };

export const baseSchema = (tag: Tag, selfReverse: boolean): Internal => {
  const schema = new SchemaCtor();
  schema.type = tag;
  schema.seq = seq++;
  if (selfReverse) {
    valueOptions[valKey] = schema;
    Object.defineProperty(schema, reversedKey, valueOptions as PropertyDescriptor);
  }
  return schema;
}

export const noopDecoder: Builder = (input: Val) => {
  return input;
}

// Every built-in singleton schema must be a module-level const initialized by
// a single `/* @__PURE__ */ initSchema(...)` expression: the module system is
// what guarantees one instance per schema (the compiled-decoder cache in
// getDecoder is keyed by `seq` and stored on the instance, so a fresh copy
// per use would recompile every time), and the single pure expression is what
// lets a consumer's bundler drop the unused ones.
// @__NO_SIDE_EFFECTS__
export const initSchema = (tag: Tag, init: (schema: Internal) => void): Internal => {
  const schema = baseSchema(tag, true);
  init(schema);
  return schema;
}

// Deliberately NOT the single-pure-expression form the other singletons use:
// `unknown` is reachable from nearly every export, so it never tree-shakes
// anyway, and the bare statement pair minifies smaller than any wrapper that
// would make it droppable.
export const unknown: Internal = baseSchema(unknownTag, true);
unknown.decoder = noopDecoder;

export const copySchema = (schema: Internal): Internal => {
  const c: Internal = Object.assign(new SchemaCtor(), schema);
  c.seq = seq++;
  return c;
}

export const updateOutput = <TValue>(schema: Internal, fn: (schema: Internal) => void): TValue => {
  const root = copySchema(schema);
  let mut = root;
  while (mut.to !== U) {
    const next = copySchema(mut.to);
    mut.to = next;
    mut = next;
  }
  // This should be the Output schema
  fn(mut);
  return root as unknown as TValue;
}

export const setHas = (has: Partial<Record<Tag, boolean>>, tag: Tag): void => {
  has[flagUnsafeHas(tagFlags[tag]!, tagFlagUnion | tagFlagRef) ? unknownTag : tag] = true;
}

// The JSON Schema pointer prefix. Shared rather than owned by jsonschema.ts:
// `S.recursive` mints `$ref`s against it and `S.json` names itself through it,
// and both sit below the converter in the layering.
export const defsPath = `#/$defs/`;

// `S.json`'s schema identity, recognised by name where importing the schema
// itself would close a cycle (composites' JSON-sourced object reads).
export const jsonName = `JSON`;
