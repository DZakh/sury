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
  [unionTag]: 256,
  [refTag]: 512,
  [bigintTag]: 1024,
  [nanTag]: 2048,
  [functionTag]: 4096,
  [instanceTag]: 8192,
  [neverTag]: 32768,
  [symbolTag]: 16384,
};
