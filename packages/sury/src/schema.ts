import { AdditionalItems, AdditionalItemsMode, ErrorDetails, Internal, SuryErrorRecord, Val, s } from "./types";
import type { Builder } from "./builder";
import { Flag, valFlagNone } from "./flags";
import { Tag, unknownTag } from "./tags";

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

let seq = 1;

let exnId: unknown = {};
export function __setExnId(id: unknown): void {
  exnId = id;
}

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

export const errorClass: unknown = SuryError;

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

export const valueOptions: Record<string, unknown> = {};
export const configurableValueOptions = { configurable: true };
export const valKey = "value";
export const reversedKey = "r";

export const baseSchema = (tag: Tag, selfReverse: boolean): Internal => {
  const schema = new (Schema as unknown as { new (): Internal })();
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
