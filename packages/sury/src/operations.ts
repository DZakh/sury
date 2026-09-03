// Operations: turning a schema into a callable, and the StandardSchema
// interop surface built on top of them.

import {
  type Flag,
  getOrRethrow,
  globalConfig,
  initSchema,
  inputExpression,
  type Internal,
  pathEmpty,
  s,
  schemaPrototype,
  SuryError,
  type SuryErrorRecord,
  U,
  undefinedTag,
  unknown,
  valKey,
  valueOptions,
  vendor
} from "./base";
import type { JSONSchemaT, StandardJsonSchemaOptions } from "./jsonschema";
import {
 getDecoder,
 reverse
} from "./parse";
import {
 literalDecoder
} from "./primitives";

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

// The Standard JSON Schema converter, installed by enableStandardJSONSchema
// (jsonschema.ts). A plain mutable module binding — the indirection is NOT a
// forward-reference workaround but the tree-shaking gate: the `~standard`
// prototype getter below is always retained, so it must not statically
// reference the converter or every parser-only bundle would ship the whole
// JSON Schema machinery. Only calling the public opt-in pulls it in.
let standardJSONSchemaConverter:
  | ((schema: Internal, options: StandardJsonSchemaOptions, isOutput: boolean) => JSONSchemaT)
  | undefined;
export const __setStandardJSONSchemaConverter = (
  fn: (schema: Internal, options: StandardJsonSchemaOptions, isOutput: boolean) => JSONSchemaT
): void => {
  standardJSONSchemaConverter = fn;
};

export const getStandardJSONSchema = (
  schema: Internal,
  options: StandardJsonSchemaOptions,
  isOutput: boolean
): JSONSchemaT => {
  if (standardJSONSchemaConverter !== U) {
    return standardJSONSchemaConverter(schema, options, isOutput);
  } else {
    throw new SuryError({
      code: "invalid_operation",
      path: pathEmpty,
      reason:
        "~standard.jsonSchema requires S.enableStandardJSONSchema() to be called first",
    });
  }
}

// Mirrors the declared `Schema<TInput, TOutput>`, so a logged schema reads the
// way its type does — input first, as the type parameters are ordered.
// Collapsed to one parameter when the sides match, because the point is a
// readable log line, not a literal type.
//
// A prototype method can never be tree-shaken, so this puts `reverse` in every
// consumer's bundle whether or not they ever print a schema — an accepted cost,
// recorded across bundleSize.yaml. Walking the `.to` chain instead would be
// cheaper and wrong: the output of `{ a: string -> int32 }` is `{ a: int32; }`,
// which only a recursive reversal produces.
// Deliberately not also registered as Node's `nodejs.util.inspect.custom`:
// `console.log(schema)` keeps showing the internal shape, which is what someone
// logging a schema is usually trying to see. Ask for the expression explicitly
// with `${schema}` or `String(schema)`.
Object.defineProperty(schemaPrototype, "toString", {
  value: function (this: Internal): string {
    const input = inputExpression(this);
    const output = inputExpression(reverse(this));
    return `Schema<${input === output ? input : `${input}, ${output}`}>`;
  },
});

// A lazy prototype getter (not an eager per-schema property — that would put
// 2 allocations + 4 closures on the baseSchema hot path for a feature most
// schemas never use), cached on first access: Standard Schema consumers read
// `schema["~standard"].validate` per validation call, so an uncached getter
// re-allocates the whole props object per request. The cache is written as a
// NON-enumerable own property (valueOptions descriptor) on purpose —
// copySchema's Object.assign copies enumerable own props, and the cached
// object closes over THIS schema, so an enumerable cache would leak onto
// derived schemas and validate against the wrong one; non-enumerable means
// copies lazily re-derive their own.
Object.defineProperty(schemaPrototype, "~standard", {
  get: function (this: Internal) {
    const schema = this;
    // The decoder lives in the closure: the Standard Schema contract is a
    // per-call `schema["~standard"].validate(input)`, so the getDecoder
    // lookup can't be hoisted by the consumer and would outweigh the decode.
    // `globalConfig.f` is getDecoder's flag source, so re-reading it is the
    // whole invalidation condition.
    let decoderFlag: Flag | undefined = U;
    let decoder: (input: unknown) => unknown;
    const standard: StandardProps = {
      version: 1,
      vendor,
      validate: (input: unknown): StandardResult => {
        // Outside the try: a conversion rejected at operation creation fails
        // for every input — a schema bug for the developer, not an `issues`
        // entry for whoever is filling in the form. It throws on every call,
        // since `decoderFlag` commits only once there is a decoder.
        if (decoderFlag !== globalConfig.f) {
          decoder = getDecoder(unknown, schema) as (input: unknown) => unknown;
          decoderFlag = globalConfig.f;
        }
        try {
          return {
            value: decoder(input),
          };
        } catch (exn) {
          const error = getOrRethrow(exn);
          return {
            issues: [
              {
                message: error.reason,
                path: error.path.length ? (error.path as unknown[]) : U,
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
    valueOptions[valKey] = standard;
    Object.defineProperty(schema, "~standard", valueOptions as PropertyDescriptor);
    return standard;
  },
});

// =============
// Operations
// =============

export const assertResult: Internal = /* @__PURE__ */ initSchema(undefinedTag, literalDecoder, (s) => {
  s.const = U;
  s.noValidation = true;
});

export const assertOrThrow = (any: unknown, schema: Internal): void => {
  (getDecoder(unknown, schema, assertResult) as (input: unknown) => unknown)(any);
}

export type JsResult<TValue> =
  | { success: true; value: TValue }
  | { success: false; error: SuryErrorRecord };

export const wrapExnToFailure = (exn: unknown): JsResult<never> => {
  if (exn && (exn as { s?: symbol }).s === s) {
    return { success: false, error: exn as unknown as SuryErrorRecord };
  } else {
    throw exn;
  }
}

export const safe = <TValue>(fn: () => TValue): JsResult<TValue> => {
  try {
    return {
      success: true,
      value: fn(),
    };
  } catch (exn) {
    return wrapExnToFailure(exn);
  }
}

export const safeAsync = <TValue>(fn: () => Promise<TValue>): Promise<JsResult<TValue>> => {
  try {
    return fn().then(
      (value): JsResult<TValue> => ({ success: true, value }),
      wrapExnToFailure
    );
  } catch (exn) {
    return Promise.resolve(wrapExnToFailure(exn));
  }
}
