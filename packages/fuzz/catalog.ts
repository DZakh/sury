import type { CodecFunctionName, ValueAst } from "./types";

export const CODEC_FUNCTION_NAMES: readonly CodecFunctionName[] = [
  "identity",
  "to-string",
  "to-number",
  "to-boolean",
  "length",
  "first",
  "wrap-array",
  "constant-string",
  "constant-number",
  "constant-boolean",
  "constant-null",
  "constant-undefined",
  "empty-array",
  "empty-object",
];

export const codecFunction = (name: CodecFunctionName): ((value: unknown) => unknown) => {
  switch (name) {
    case "identity":
      return (value) => value;
    case "to-string":
      return (value) => String(value);
    case "to-number":
      return (value) => Number(value);
    case "to-boolean":
      return (value) => Boolean(value);
    case "length":
      return (value) =>
        typeof value === "string" || Array.isArray(value) ? value.length : 0;
    case "first":
      return (value) => (Array.isArray(value) ? value[0] : value);
    case "wrap-array":
      return (value) => [value];
    case "constant-string":
      return () => "fuzz";
    case "constant-number":
      return () => 1;
    case "constant-boolean":
      return () => true;
    case "constant-null":
      return () => null;
    case "constant-undefined":
      return () => undefined;
    case "empty-array":
      return () => [];
    case "empty-object":
      return () => ({});
  }
};

export const valueFromAst = (value: ValueAst): unknown => {
  switch (value.kind) {
    case "string":
    case "boolean":
      return value.value;
    case "number":
      switch (value.value.kind) {
        case "finite":
          return value.value.value;
        case "nan":
          return NaN;
        case "infinity":
          return value.value.negative ? -Infinity : Infinity;
        case "negative-zero":
          return -0;
      }
    case "bigint":
      return BigInt(value.value);
    case "null":
      return null;
    case "undefined":
      return undefined;
    case "array":
      return value.items.map(valueFromAst);
    case "object":
      return Object.fromEntries(value.entries.map(([key, item]) => [key, valueFromAst(item)]));
  }
};

export const renderValue = (value: ValueAst): string => {
  switch (value.kind) {
    case "string":
      return JSON.stringify(value.value);
    case "boolean":
      return String(value.value);
    case "number":
      switch (value.value.kind) {
        case "finite":
          return String(value.value.value);
        case "nan":
          return "NaN";
        case "infinity":
          return value.value.negative ? "-Infinity" : "Infinity";
        case "negative-zero":
          return "-0";
      }
    case "bigint":
      return `${value.value}n`;
    case "null":
      return "null";
    case "undefined":
      return "undefined";
    case "array":
      return `[${value.items.map(renderValue).join(", ")}]`;
    case "object":
      return `{ ${value.entries
        .map(([key, item]) => `${JSON.stringify(key)}: ${renderValue(item)}`)
        .join(", ")} }`;
  }
};
