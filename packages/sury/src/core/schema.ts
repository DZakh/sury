import { AdditionalItems, AdditionalItemsMode, ErrorDetails, Flag, Internal, SuryErrorRecord, Tag, Val, s, unknownTag, valFlagNone } from "./types";
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
export const schemaPrototype: Record<string, unknown> = Object.create(null);
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
export class SuryError extends Error {
  constructor(params: ErrorDetails | Record<string, unknown>) {
    super();
    for (const key in params) {
      (this as unknown as Record<string, unknown>)[key] = (params as Record<string, unknown>)[key];
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

export const getOrRethrow = (exn: unknown): SuryErrorRecord => {
  if (exn && (exn as { s?: symbol }).s === s) {
    return exn as unknown as SuryErrorRecord;
  } else {
    throw exn;
  }
}

// TODO: Throw S.Error
export const panic = (message: string): never => {
  throw new Error(`[Sury] ${message}`);
}

const formatErrorMessage = (error: SuryErrorRecord): string => {
  return `${error.path === "" ? "" : `Failed at ${error.path}: `}${error.reason}`;
}

// The public `S.Error` class (Error.class in Sury.res's `module Error`).
export const errorClass: unknown = SuryError;


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

export const initialOnAdditionalItems: AdditionalItemsMode = "strip";
export const initialDefaultFlag: Flag = valFlagNone as unknown as Flag;
export const globalConfig: GlobalConfig = {
  m: formatErrorMessage,
  d: undefined,
  a: initialOnAdditionalItems as unknown as AdditionalItems,
  f: initialDefaultFlag,
};

// =============================================================================
// base / cached / copySchema / updateOutput
// =============================================================================

export const valueOptions: Record<string, unknown> = {};
export const configurableValueOptions = { configurable: true };
export const valKey = "value";
export const reversedKey = "r";

export const baseSchema = (tag: Tag, selfReverse: boolean): Internal => {
  const schema = new (Schema as unknown as { new (): Internal })();
  schema.type = tag;
  schema.seq = seq++;
  if (selfReverse) {
    // Reuse the module-level `valueOptions` descriptor object (no per-schema
    // allocation), exactly like the source — and unlike the reverse cache,
    // this descriptor is deliberately non-configurable.
    valueOptions[valKey] = schema;
    Object.defineProperty(schema, reversedKey, valueOptions as PropertyDescriptor);
  }
  return schema;
}

export const noopDecoder = (input: Val): Val => {
  return input;
}

const factoryCache: Record<string, Internal> = {};

export const cached = (key: string, tag: Tag, init: (schema: Internal) => void): Internal => {
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

export const copySchema = (schema: Internal): Internal => {
  const c = new (Schema as unknown as { new (): Internal })();
  for (const k in schema) {
    (c as unknown as Record<string, unknown>)[k] = (schema as unknown as Record<string, unknown>)[k];
  }
  c.seq = seq++;
  return c;
}

export const updateOutput = <Value>(schema: Internal, fn: (schema: Internal) => void): Value => {
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
