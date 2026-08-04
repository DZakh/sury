/** The Standard Schema interface. */
export interface StandardSchemaV1<TInput = unknown, TOutput = TInput> {
  /** The Standard Schema properties. */
  readonly "~standard": StandardSchemaV1.Props<TInput, TOutput>;
}

export declare namespace StandardSchemaV1 {
  /** The Standard Schema properties interface. */
  export interface Props<TInput = unknown, TOutput = TInput> {
    /** The version number of the standard. */
    readonly version: 1;
    /** The vendor name of the schema library. */
    readonly vendor: string;
    /** Validates unknown input values. */
    readonly validate: (
      value: unknown
    ) => Result<TOutput> | Promise<Result<TOutput>>;
    /** Inferred types associated with the schema. */
    readonly types?: Types<TInput, TOutput> | undefined;
  }

  /** The result interface of the validate function. */
  export type Result<TOutput> = SuccessResult<TOutput> | FailureResult;

  /** The result interface if validation succeeds. */
  export interface SuccessResult<TOutput> {
    /** The typed output value. */
    readonly value: TOutput;
    /** The non-existent issues. */
    readonly issues?: undefined;
  }

  /** The result interface if validation fails. */
  export interface FailureResult {
    /** The issues of failed validation. */
    readonly issues: ReadonlyArray<Issue>;
  }

  /** The issue interface of the failure output. */
  export interface Issue {
    /** The error message of the issue. */
    readonly message: string;
    /** The path of the issue, if any. */
    readonly path?: ReadonlyArray<PropertyKey | PathSegment> | undefined;
  }

  /** The path segment interface of the issue. */
  export interface PathSegment {
    /** The key representing a path segment. */
    readonly key: PropertyKey;
  }

  /** The Standard Schema types interface. */
  export interface Types<TInput = unknown, TOutput = TInput> {
    /** The input type of the schema. */
    readonly input: TInput;
    /** The output type of the schema. */
    readonly output: TOutput;
  }

  /** Infers the input type of a Standard Schema. */
  export type InferInput<TSchema extends StandardSchemaV1> = NonNullable<
    TSchema["~standard"]["types"]
  >["input"];

  /** Infers the output type of a Standard Schema. */
  export type InferOutput<TSchema extends StandardSchemaV1> = NonNullable<
    TSchema["~standard"]["types"]
  >["output"];
}

/**
 * The Standard Typed interface.
 * This is a base type extended by other specs.
 */
export interface StandardTypedV1<TInput = unknown, TOutput = TInput> {
  readonly "~standard": StandardTypedV1.Props<TInput, TOutput>;
}

export declare namespace StandardTypedV1 {
  export interface Props<TInput = unknown, TOutput = TInput> {
    readonly version: 1;
    readonly vendor: string;
    readonly types?: Types<TInput, TOutput> | undefined;
  }
  export interface Types<TInput = unknown, TOutput = TInput> {
    readonly input: TInput;
    readonly output: TOutput;
  }
  export type InferInput<TSchema extends StandardTypedV1> = NonNullable<
    TSchema["~standard"]["types"]
  >["input"];
  export type InferOutput<TSchema extends StandardTypedV1> = NonNullable<
    TSchema["~standard"]["types"]
  >["output"];
}

/** The Standard JSON Schema interface. https://standardschema.dev/json-schema */
export interface StandardJSONSchemaV1<TInput = unknown, TOutput = TInput> {
  readonly "~standard": StandardJSONSchemaV1.Props<TInput, TOutput>;
}

export declare namespace StandardJSONSchemaV1 {
  export interface Props<TInput = unknown, TOutput = TInput>
    extends StandardTypedV1.Props<TInput, TOutput> {
    readonly jsonSchema: StandardJSONSchemaV1.Converter;
  }
  export interface Converter {
    readonly input: (options: StandardJSONSchemaV1.Options) => Record<string, unknown>;
    readonly output: (options: StandardJSONSchemaV1.Options) => Record<string, unknown>;
  }
  export type Target =
    | "draft-2020-12"
    | "draft-07"
    | "openapi-3.0"
    | ({} & string);
  export interface Options {
    readonly target: Target;
    readonly libraryOptions?: Record<string, unknown> | undefined;
  }
  export interface Types<TInput = unknown, TOutput = TInput>
    extends StandardTypedV1.Types<TInput, TOutput> {}
  export type InferInput<TSchema extends StandardTypedV1> =
    StandardTypedV1.InferInput<TSchema>;
  export type InferOutput<TSchema extends StandardTypedV1> =
    StandardTypedV1.InferOutput<TSchema>;
}


export type SuccessResult<TValue> = {
  readonly success: true;
  readonly value: TValue;
  readonly error?: undefined;
};

export type FailureResult = {
  readonly success: false;
  readonly error: Error;
};

export type Result<TValue> = SuccessResult<TValue> | FailureResult;

export type JSON =
  | string
  | boolean
  | number
  | null
  | { [key: string]: JSON }
  | JSON[];

export type NumberFormat = "int32" | "port";
export type StringFormat = "json" | "date-time" | "email" | "uuid" | "cuid" | "url";
export type ArrayFormat = "compactColumns";
export type Format = NumberFormat | StringFormat | ArrayFormat;

// `TOutput = TInput` so an identity schema is spelled `Schema<string>`. The
// default is dependent, so TS instantiates it at every one-arg reference —
// internal references write `Schema<unknown, unknown>` in full to keep that
// off the per-schema type-cost the specs measure.
export type Schema<TInput = unknown, TOutput = TInput> = {
  with<TTargetInput = unknown, TTargetOutput = unknown>(
    to: (
      schema: Schema<unknown, unknown>,
      target: Schema<unknown, unknown>,
      decode?: ((value: unknown) => unknown) | undefined,
      encode?: (value: unknown) => TOutput
    ) => Schema<unknown, unknown>,
    target: SchemaLike<TTargetInput, TTargetOutput>,
    decode?: ((value: TOutput) => TTargetInput) | undefined,
    encode?: (value: TTargetOutput) => TOutput
  ): Schema<TInput, TTargetOutput>;
  with(
    refine: (
      schema: Schema<unknown, unknown>,
      refineCheck: (value: unknown) => boolean,
      refineOptions?: { error?: string; path?: string[] }
    ) => Schema<unknown, unknown>,
    refineCheck: (value: TOutput) => boolean,
    refineOptions?: { error?: string; path?: string[] }
  ): Schema<TInput, TOutput>;
  // This overload is what both S.refine and S.shape resolve to under
  // overload matching — the exact mechanism that routes S.refine calls here
  // instead of the more specific `refine` overload above hasn't been pinned
  // down. Treat it as load-bearing for both call sites and verify against
  // S_refine_test.res / S_shape_test.res before changing its shape.
  with<TShape>(
    fn: (
      schema: Schema<unknown, unknown>,
      callback: ((value: unknown) => unknown) | undefined
    ) => Schema<unknown, unknown>,
    callback: ((value: TOutput) => TShape) | undefined
  ): Schema<TInput, TShape>;
  with<TNextInput, TNextOutput>(
    fn: (schema: Schema<TInput, TOutput>) => SchemaLike<TNextInput, TNextOutput>
  ): Schema<TNextInput, TNextOutput>;
  // Constraining TArg1 to string | number makes a literal arg1 infer its
  // literal type instead of widening — `.with(S.brand, "myId")` needs the
  // string literal for nominal typing, `.with(S.length, 2)` the number
  // literal for its tuple-typed result. One overload for both: a second
  // overload would be attempted (and instantiated) by every `.with` call
  // that falls through to the general case, taxing schemas that never pass
  // a literal. The next overload covers the general arg1 case.
  with<TNextInput, TNextOutput, TArg1 extends string | number>(
    fn: (
      schema: Schema<TInput, TOutput>,
      arg1: TArg1
    ) => SchemaLike<TNextInput, TNextOutput>,
    arg1: TArg1
  ): Schema<TNextInput, TNextOutput>;
  with<TNextInput, TNextOutput, TArg1>(
    fn: (
      schema: Schema<TInput, TOutput>,
      arg1: TArg1
    ) => SchemaLike<TNextInput, TNextOutput>,
    arg1: TArg1
  ): Schema<TNextInput, TNextOutput>;
  with<TNextInput, TNextOutput, TArg1, TArg2>(
    fn: (
      schema: Schema<TInput, TOutput>,
      arg1: TArg1,
      arg2: TArg2
    ) => SchemaLike<TNextInput, TNextOutput>,
    arg1: TArg1,
    arg2: TArg2
  ): Schema<TNextInput, TNextOutput>;

  /**
   * The schema as `Schema<input, output>`, collapsed to `Schema<input>` when
   * the two sides match. Used by string coercion — interpolation, `String()`,
   * `"%s"`. `console.log(schema)` still shows the internal schema shape.
   *
   * ```ts
   * `${S.string}`                    // "Schema<string>"
   * `${S.to(S.string, S.number)}`    // "Schema<string, number>"
   * ```
   */
  toString(): string;

  readonly $defs?: Record<string, Schema<unknown, unknown>>;

  readonly name?: string;
  readonly title?: string;
  readonly description?: string;
  readonly deprecated?: boolean;
  readonly examples?: TInput[];
  readonly noValidation?: boolean;
  readonly default?: TInput;
  readonly to?: Schema<unknown, unknown>;
  readonly errorMessage?: SchemaErrorMessage;

  // jsonSchema.input/.output throw until enableStandardJSONSchema() is called.
  readonly ["~standard"]: StandardSchemaV1.Props<TInput, TOutput> &
    StandardJSONSchemaV1.Props<TInput, TOutput>;
} & (
  | {
      readonly type: "never";
    }
  | {
      readonly type: "unknown";
    }
  | {
      readonly type: "string";
      readonly format?: StringFormat;
      readonly const?: string;
      readonly minLength?: number;
      readonly maxLength?: number;
      readonly pattern?: RegExp;
    }
  | {
      readonly type: "number";
      readonly format?: NumberFormat;
      readonly const?: number;
      readonly minimum?: number;
      readonly maximum?: number;
    }
  | {
      readonly type: "bigint";
      readonly const?: bigint;
    }
  | {
      readonly type: "boolean";
      readonly const?: boolean;
    }
  | {
      readonly type: "symbol";
      readonly const?: symbol;
    }
  | {
      readonly type: "null";
      readonly const: null;
    }
  | {
      readonly type: "undefined";
      readonly const: undefined;
    }
  | {
      readonly type: "nan";
      readonly const: number;
    }
  | {
      readonly type: "function";
      readonly const?: TInput;
    }
  | {
      readonly type: "instance";
      readonly class: Class<TInput>;
      readonly const?: TInput;
    }
  | {
      readonly type: "array";
      readonly items: Schema<unknown, unknown>;
      readonly additionalItems: AdditionalItemsMode | Schema<unknown, unknown>;
      readonly format?: ArrayFormat;
      readonly minItems?: number;
      readonly maxItems?: number;
    }
  | {
      readonly type: "object";
      readonly properties: {
        [key: string]: Schema<unknown, unknown>;
      };
      readonly additionalItems: AdditionalItemsMode | Schema<unknown, unknown>;
      readonly required?: string[];
    }
  | {
      readonly type: "anyOf";
      readonly anyOf: Schema<unknown, unknown>[];
      readonly has: Record<
        | "string"
        | "number"
        | "never"
        | "unknown"
        | "bigint"
        | "boolean"
        | "symbol"
        | "null"
        | "undefined"
        | "nan"
        | "function"
        | "instance"
        | "array"
        | "object",
        boolean
      >;
    }
  | {
      readonly type: "ref";
      readonly $ref: string;
    }
);

export abstract class Path {
  protected opaque: unknown;
} /* simulate opaque types */

type BaseError = {
  readonly path: Path;
  readonly message: string;
  readonly reason: string;
};

export type Error =
  | (BaseError & {
      readonly code: "invalid_input";
      readonly expected: Schema<unknown, unknown>;
      readonly received: Schema<unknown, unknown>;
      readonly input?: unknown;
      readonly unionErrors?: readonly Error[];
    })
  | (BaseError & {
      readonly code: "invalid_operation";
    })
  | (BaseError & {
      readonly code: "unsupported_decode";
      readonly from: Schema<unknown, unknown>;
      readonly to: Schema<unknown, unknown>;
    })
  | (BaseError & {
      readonly code: "invalid_conversion";
      readonly from: Schema<unknown, unknown>;
      readonly to: Schema<unknown, unknown>;
      readonly cause?: unknown;
    })
  | (BaseError & {
      readonly code: "unrecognized_keys";
      readonly keys: readonly string[];
    });

export const Error: {
  new (): Error;
  prototype: Error;
};

// Extract Output/Input by matching only the `~standard` marker instead of the
// full `Schema<…>` shape (whose 14-member union + `with` overloads are costly to
// instantiate per match). `types` is optional, so the pattern keeps it optional.
export type Output<T> = T extends {
  readonly ["~standard"]: { readonly types?: { readonly output: infer TOutput } };
}
  ? TOutput
  : never;
export type Infer<T> = Output<T>;
export type Input<T> = T extends {
  readonly ["~standard"]: { readonly types?: { readonly input: infer TInput } };
}
  ? TInput
  : never;

// Utility types for decoder function with multiple schemas
type ExtractFirstInput<TSchemas extends readonly SchemaLike<any, any>[]> =
  TSchemas extends readonly [SchemaLike<infer TFirstInput, any>, ...any[]]
    ? TFirstInput
    : never;

// Utility types for encoder function with multiple schemas
type ExtractFirstOutput<TSchemas extends readonly SchemaLike<any, any>[]> =
  TSchemas extends readonly [SchemaLike<any, infer TFirstOutput>, ...any[]]
    ? TFirstOutput
    : never;

type ExtractLastOutput<TSchemas extends readonly SchemaLike<any, any>[]> =
  TSchemas extends readonly [...any[], SchemaLike<any, infer TLastOutput>]
    ? TLastOutput
    : TSchemas extends readonly [SchemaLike<any, infer TSingleOutput>]
    ? TSingleOutput
    : never;

type ExtractLastInput<TSchemas extends readonly SchemaLike<any, any>[]> =
  TSchemas extends readonly [...any[], SchemaLike<infer TLastInput, any>]
    ? TLastInput
    : TSchemas extends readonly [SchemaLike<infer TSingleInput, any>]
    ? TSingleInput
    : never;

// Match the `~standard` marker instead of the full `Schema<…>` shape for the
// same instantiation-cost reason as `Output<T>` above.
// `-readonly` undoes the `readonly` that a `const T` call site (schema/union)
// stamps onto every nested property — that marker only exists to keep literal
// types from widening and shouldn't leak into the inferred Output/Input.
export type UnknownToOutput<T> = T extends {
  readonly ["~standard"]: { readonly types?: { readonly output: infer TOutput } };
}
  ? TOutput
  : T extends (...args: any[]) => any
  ? T
  : T extends unknown[]
  ? { -readonly [K in keyof T]: UnknownToOutput<T[K]> }
  : T extends { [k in keyof T]: unknown }
  ? ResolveObject<{ -readonly [K in keyof T]: UnknownToOutput<T[K]> }>
  : T;

export type UnknownToInput<T> = T extends {
  readonly ["~standard"]: { readonly types?: { readonly input: infer TInput } };
}
  ? TInput
  : T extends (...args: any[]) => any
  ? T
  : T extends unknown[]
  ? { -readonly [K in keyof T]: UnknownToInput<T[K]> }
  : T extends { [k in keyof T]: unknown }
  ? ResolveObject<{ -readonly [K in keyof T]: UnknownToInput<T[K]> }>
  : T;

// Lightweight parameter type for inferring a schema's Output/Input: matching
// the `~standard` marker instead of the full `Schema<…>` shape (14-member
// union + `with` overloads) keeps per-call instantiation cost low.
type SchemaLike<TInput, TOutput> = {
  readonly ["~standard"]: {
    readonly types?:
      | { readonly output: TOutput; readonly input: TInput }
      | undefined;
  };
};

export type Brand<T, TId extends string> = T & {
  /**
   *  TypeScript won't suggest strings beginning with a space as properties.
   *  Useful for symbol-like string properties.
   */
  readonly [" brand"]: [T, TId];
};

export function brand<TId extends string, TInput = unknown, TOutput = unknown>(
  schema: SchemaLike<TInput, TOutput>,
  brandId: TId
): Schema<TInput, Brand<TOutput, TId>>;

// `TFields` already holds each field's resolved type. A field is optional iff
// its type admits `undefined`, so an `S.never` field stays required. The split
// is skipped when no field is optional. Required keys come first, optional last
// — matching the ordering Zod (and the wider Standard Schema ecosystem) infers,
// so a Sury type reads the same as its cross-library equivalent.
type ResolveObject<TFields> = undefined extends TFields[keyof TFields]
  ? Flatten<
      {
        [K in keyof TFields as undefined extends TFields[K] ? never : K]: TFields[K];
      } & {
        [K in keyof TFields as undefined extends TFields[K] ? K : never]?: TFields[K];
      }
    >
  : Flatten<TFields>;

// Flatten an intersection into one object, keeping values verbatim (incl. `never`).
type Flatten<T> = T extends object ? { [K in keyof T]: T[K] } : T;

// Homomorphic mapped type over a tuple `T` preserves its arity — a plain
// (non-tuple) array `T` has `T["length"]` widened to `number`, in which case
// there's nothing positional to map and `T` is returned as-is.
type UnknownArrayToOutput<T extends unknown[]> = number extends T["length"]
  ? T
  : { -readonly [K in keyof T]: UnknownToOutput<T[K]> };
type UnknownArrayToInput<T extends unknown[]> = number extends T["length"]
  ? T
  : { -readonly [K in keyof T]: UnknownToInput<T[K]> };

export function schema<const T extends unknown[]>(
  schemas: [...T]
): Schema<[...UnknownArrayToInput<T>], [...UnknownArrayToOutput<T>]>;
export function schema<const T>(
  value: T
): Schema<UnknownToInput<T>, UnknownToOutput<T>>;

export function literal<const T>(
  value: T
): Schema<UnknownToInput<T>, UnknownToOutput<T>>;

export function union<const TFirst, const TRest extends unknown[]>(
  schemas: [TFirst, ...TRest]
): Schema<
  UnknownToInput<TFirst> | UnknownArrayToInput<TRest>[number],
  UnknownToOutput<TFirst> | UnknownArrayToOutput<TRest>[number]
>;
export function union<const T>(
  schemas: readonly T[]
): Schema<UnknownToInput<T>, UnknownToOutput<T>>;

export const string: Schema<string, string>;
export const boolean: Schema<boolean, boolean>;
export const int32: Schema<number, number>;
export const number: Schema<number, number>;
export const bigint: Schema<bigint, bigint>;
export const symbol: Schema<symbol, symbol>;
export const never: Schema<never, never>;
export const unknown: Schema<unknown, unknown>;
export const any: Schema<any, any>;
declare const void_: Schema<void, void>;
export { void_ as void };

export const json: Schema<JSON, JSON>;

export const jsonString: Schema<string, string>;
export const jsonStringWithSpace: (space: number) => Schema<string, string>;

export const uint8Array: Schema<Uint8Array, Uint8Array>;

export const isoDateTime: Schema<string, string>;

export const port: Schema<number, number>;

export const email: Schema<string, string>;

export const uuid: Schema<string, string>;

export const cuid: Schema<string, string>;

export const url: Schema<string, string>;

export const date: Schema<Date, Date>;

export function safe<TValue>(scope: () => TValue): Result<TValue>;
export function safeAsync<TValue>(
  scope: () => Promise<TValue>
): Promise<Result<TValue>>;

export function reverse<TInput, TOutput>(
  schema: SchemaLike<TInput, TOutput>
): Schema<TOutput, TInput>;

export function parser<TOutput>(
  schema: SchemaLike<unknown, TOutput>
): (data: unknown) => TOutput;
export function parser<TOutput>(
  from: SchemaLike<unknown, unknown>,
  target: SchemaLike<unknown, TOutput>
): (data: unknown) => TOutput;
export function parser<
  TSchemas extends readonly [SchemaLike<any, any>, ...SchemaLike<any, any>[]]
>(...schemas: TSchemas): (data: unknown) => ExtractLastOutput<TSchemas>;

export function asyncParser<TOutput>(
  schema: SchemaLike<unknown, TOutput>
): (data: unknown) => Promise<TOutput>;
export function asyncParser<TOutput>(
  from: SchemaLike<unknown, unknown>,
  target: SchemaLike<unknown, TOutput>
): (data: unknown) => Promise<TOutput>;
export function asyncParser<
  TSchemas extends readonly [SchemaLike<any, any>, ...SchemaLike<any, any>[]]
>(...schemas: TSchemas): (data: unknown) => Promise<ExtractLastOutput<TSchemas>>;

export function decoder<TInput, TOutput>(
  schema: SchemaLike<TInput, TOutput>
): (data: TInput) => TOutput;
export function decoder<TInput, TOutput>(
  from: SchemaLike<TInput, unknown>,
  target: SchemaLike<unknown, TOutput>
): (data: TInput) => TOutput;
export function decoder<
  TSchemas extends readonly [SchemaLike<any, any>, ...SchemaLike<any, any>[]]
>(
  ...schemas: TSchemas
): (data: ExtractFirstInput<TSchemas>) => ExtractLastOutput<TSchemas>;

export function asyncDecoder<TInput, TOutput>(
  schema: SchemaLike<TInput, TOutput>
): (data: TInput) => Promise<TOutput>;
export function asyncDecoder<TInput, TOutput>(
  from: SchemaLike<TInput, unknown>,
  target: SchemaLike<unknown, TOutput>
): (data: TInput) => Promise<TOutput>;
export function asyncDecoder<
  TSchemas extends readonly [SchemaLike<any, any>, ...SchemaLike<any, any>[]]
>(
  ...schemas: TSchemas
): (data: ExtractFirstInput<TSchemas>) => Promise<ExtractLastOutput<TSchemas>>;

export function encoder<TInput, TOutput>(
  schema: SchemaLike<TInput, TOutput>
): (data: TOutput) => TInput;
export function encoder<TInput, TOutput>(
  from: SchemaLike<unknown, TOutput>,
  target: SchemaLike<TInput, unknown>
): (data: TOutput) => TInput;
export function encoder<
  TSchemas extends readonly [SchemaLike<any, any>, ...SchemaLike<any, any>[]]
>(
  ...schemas: TSchemas
): (data: ExtractFirstOutput<TSchemas>) => ExtractLastInput<TSchemas>;

export function asyncEncoder<TInput, TOutput>(
  schema: SchemaLike<TInput, TOutput>
): (data: TOutput) => Promise<TInput>;
export function asyncEncoder<TInput, TOutput>(
  from: SchemaLike<unknown, TOutput>,
  target: SchemaLike<TInput, unknown>
): (data: TOutput) => Promise<TInput>;
export function asyncEncoder<
  TSchemas extends readonly [SchemaLike<any, any>, ...SchemaLike<any, any>[]]
>(
  ...schemas: TSchemas
): (data: ExtractFirstOutput<TSchemas>) => Promise<ExtractLastInput<TSchemas>>;

export function assert<TInput, TOutput>(
  schema: SchemaLike<TInput, TOutput>,
  data: unknown
): asserts data is TInput;
export function assert<TInput, TOutput>(
  data: unknown,
  schema: SchemaLike<TInput, TOutput>
): asserts data is TInput;

export function is<TInput, TOutput>(
  schema: SchemaLike<TInput, TOutput>,
  data: unknown
): data is TInput;
export function is<TInput, TOutput>(
  data: unknown,
  schema: SchemaLike<TInput, TOutput>
): data is TInput;

export function tuple<TInput extends unknown[], TOutput>(
  definer: (s: {
    item: <TItemOutput>(
      inputIndex: number,
      schema: SchemaLike<unknown, TItemOutput>
    ) => TItemOutput;
    tag: (inputIndex: number, value: unknown) => void;
  }) => TOutput
): Schema<TInput, TOutput>;
export function tuple<const T extends unknown[]>(
  schemas: [...T]
): Schema<[...UnknownArrayToInput<T>], [...UnknownArrayToOutput<T>]>;

export function optional<
  TInput,
  TOutput,
  TOr extends TOutput | undefined = undefined
>(
  schema: SchemaLike<TInput, TOutput>,
  or?: (() => TOr) | TOr,
  // To make .with work
  _?: never
): Schema<
  TInput | undefined,
  TOr extends undefined ? TOutput | undefined : TOutput
>;

export function nullable<TInput, TOutput, TOr extends TOutput | null = null>(
  schema: SchemaLike<TInput, TOutput>,
  or?: (() => TOr) | TOr,
  // To make .with work
  _?: never
): Schema<TInput | null, TOr extends null ? TOutput | null : TOutput>;

export const nullish: <TInput, TOutput>(
  schema: SchemaLike<TInput, TOutput>
) => Schema<TInput | undefined | null, TOutput | undefined | null>;

export type Class<T> = new (...args: readonly any[]) => T;
export const instance: <T>(class_: Class<T>) => Schema<T, T>;

export const array: <TInput, TOutput>(
  schema: SchemaLike<TInput, TOutput>
) => Schema<TInput[], TOutput[]>;

export const compactColumns: <TInput, TOutput>(
  schema: SchemaLike<TInput, TOutput>
) => Schema<TInput[][], TOutput[][]>;

export const record: <TInput, TOutput>(
  schema: SchemaLike<TInput, TOutput>
) => Schema<Record<string, TInput>, Record<string, TOutput>>;

type ObjectCtx<TInput extends Record<string, unknown>> = {
  field: <TFieldOutput>(
    name: string,
    schema: SchemaLike<unknown, TFieldOutput>
  ) => TFieldOutput;
  fieldOr: <TFieldOutput>(
    name: string,
    schema: SchemaLike<unknown, TFieldOutput>,
    or: TFieldOutput
  ) => TFieldOutput;
  tag: <TTagName extends keyof TInput>(
    name: TTagName,
    value: TInput[TTagName]
  ) => void;
  flatten: <TFieldOutput>(
    schema: SchemaLike<unknown, TFieldOutput>
  ) => TFieldOutput;
  nested: (name: string) => ObjectCtx<Record<string, unknown>>;
};

export function object<TInput extends Record<string, unknown>, TOutput>(
  definer: (ctx: ObjectCtx<TInput>) => TOutput
): Schema<TInput, TOutput>;
export function object<T extends Record<string, unknown>>(
  definition: T
): Schema<UnknownToInput<T>, UnknownToOutput<T>>;

export function strip<TInput extends Record<string, unknown>, TOutput>(
  schema: SchemaLike<TInput, TOutput>
): Schema<TInput, TOutput>;
export function deepStrip<TInput extends Record<string, unknown>, TOutput>(
  schema: SchemaLike<TInput, TOutput>
): Schema<TInput, TOutput>;
export function strict<TInput extends Record<string, unknown>, TOutput>(
  schema: SchemaLike<TInput, TOutput>
): Schema<TInput, TOutput>;
export function deepStrict<TInput extends Record<string, unknown>, TOutput>(
  schema: SchemaLike<TInput, TOutput>
): Schema<TInput, TOutput>;

// Bare Flatten, not ResolveObject: re-splitting the merged intersection to
// hoist optionals last nearly doubled this type's instantiation cost, so Merge
// keeps insertion order.
type Merge<TLeft, TRight> = Flatten<
  { [K in keyof TLeft as K extends keyof TRight ? never : K]: TLeft[K] } & TRight
>;

export function merge<
  TInput1,
  TOutput1 extends Record<string, unknown>,
  TInput2,
  TOutput2 extends Record<string, unknown>
>(
  schema1: SchemaLike<TInput1, TOutput1>,
  schema2: SchemaLike<TInput2, TOutput2>
): Schema<Merge<TInput1, TInput2>, Merge<TOutput1, TOutput2>>;

export function recursive<TInput = unknown, TOutput = TInput>(
  identifier: string,
  definer: (schema: Schema<TInput, TOutput>) => Schema<TInput, TOutput>
): Schema<TInput, TOutput>;

export type SchemaErrorMessage = {
  /** Catch-all override, used when no more specific key below matches the failing check. */
  _?: string;
  format?: string;
  type?: string;
  minimum?: string;
  maximum?: string;
  exclusiveMinimum?: string;
  exclusiveMaximum?: string;
  minLength?: string;
  maxLength?: string;
  minItems?: string;
  maxItems?: string;
  pattern?: string;
};

export type Meta<TOutput> = {
  name?: string;
  title?: string;
  description?: string;
  deprecated?: boolean;
  examples?: TOutput[];
  errorMessage?: SchemaErrorMessage;
};

export function meta<TInput, TOutput>(
  schema: SchemaLike<TInput, TOutput>,
  meta: Meta<TOutput>
): Schema<TInput, TOutput>;

export function inputExpression(schema: SchemaLike<unknown, unknown>): string;
export function outputExpression(schema: SchemaLike<unknown, unknown>): string;
export function noValidation<TInput, TOutput>(
  schema: SchemaLike<TInput, TOutput>,
  value: boolean
): Schema<TInput, TOutput>;

export function asyncDecoderAssert<TInput, TOutput>(
  schema: SchemaLike<TInput, TOutput>,
  assertFn: (value: TOutput) => Promise<void>
): Schema<TInput, TOutput>;

export function refine<TInput, TOutput>(
  schema: SchemaLike<TInput, TOutput>,
  refineCheck: (value: TOutput) => boolean,
  refineOptions?: {
    error?: string;
    path?: string[];
  }
): Schema<TInput, TOutput>;

export const gt: <TInput, TOutput extends number | bigint>(
  schema: SchemaLike<TInput, TOutput>,
  value: TOutput,
  message?: string
) => Schema<TInput, TOutput>;
export const gte: <TInput, TOutput extends number | bigint>(
  schema: SchemaLike<TInput, TOutput>,
  value: TOutput,
  message?: string
) => Schema<TInput, TOutput>;
export const lt: <TInput, TOutput extends number | bigint>(
  schema: SchemaLike<TInput, TOutput>,
  value: TOutput,
  message?: string
) => Schema<TInput, TOutput>;
export const lte: <TInput, TOutput extends number | bigint>(
  schema: SchemaLike<TInput, TOutput>,
  value: TOutput,
  message?: string
) => Schema<TInput, TOutput>;

// A pinned length is arity: `S.array(S.string).with(S.length, 2)` admits
// exactly `[string, string]`, and `S.empty` exactly `[]` / `""`, so the
// refined type says so instead of keeping the unbounded one. Only a literal
// `N` pins — a `number`-typed bound narrows nothing (`number extends N`
// guard). The 64-step cap bails to the unbounded type: past it a spelled-out
// tuple hurts hover DX more than it helps, and an unguarded recursion turns
// `S.length(schema, 1e6)` (or a fractional bound, which never hits `N`) into
// a compile error instead of the runtime one it already raises.
type Repeat<E, N extends number, Acc extends E[]> = Acc["length"] extends N
  ? Acc
  : Acc extends { length: 64 }
  ? E[]
  : Repeat<E, N, [...Acc, E]>;
type Sized<T, N extends number> = number extends N
  ? T
  : 0 extends N
  ? T extends string
    ? ""
    : T extends unknown[]
    ? []
    : T
  : T extends (infer E)[]
  ? Repeat<E, N, []>
  : T;

export const minLength: <TInput, TOutput extends string | unknown[]>(
  schema: SchemaLike<TInput, TOutput>,
  length: number,
  message?: string
) => Schema<TInput, TOutput>;
export const maxLength: <TInput, TOutput extends string | unknown[]>(
  schema: SchemaLike<TInput, TOutput>,
  length: number,
  message?: string
) => Schema<TInput, TOutput>;
export const length: <TInput, TOutput extends string | unknown[], N extends number>(
  schema: SchemaLike<TInput, TOutput>,
  length: N,
  message?: string
) => Schema<Sized<TInput, N>, Sized<TOutput, N>>;
export const empty: <TInput, TOutput extends string | unknown[]>(
  schema: SchemaLike<TInput, TOutput>,
  message?: string
) => Schema<Sized<TInput, 0>, Sized<TOutput, 0>>;
export const nonEmpty: <TInput, TOutput extends string | unknown[]>(
  schema: SchemaLike<TInput, TOutput>,
  message?: string
) => Schema<TInput, TOutput>;

export const pattern: <TInput>(
  schema: SchemaLike<TInput, string>,
  re: RegExp,
  message?: string
) => Schema<TInput, string>;
export const trim: <TInput>(
  schema: SchemaLike<TInput, string>
) => Schema<TInput, string>;

export type AdditionalItemsMode = "strip" | "strict";

export type GlobalConfigOverride = {
  defaultAdditionalItems?: AdditionalItemsMode;
  disableNanNumberValidation?: boolean;
};

export function global(globalConfigOverride: GlobalConfigOverride): void;

export function shape<TShape = unknown, TInput = unknown, TOutput = unknown>(
  schema: SchemaLike<TInput, TOutput>,
  shaper: (value: TOutput) => TShape
): Schema<TInput, TShape>;

export function to<
  TInput = unknown,
  TOutput = unknown,
  TTargetInput = unknown,
  TTargetOutput = unknown
>(
  schema: SchemaLike<TInput, TOutput>,
  target: SchemaLike<TTargetInput, TTargetOutput>,
  decode?: ((value: TOutput) => TTargetInput) | undefined,
  encode?: (value: TTargetOutput) => TOutput
): Schema<TInput, TTargetOutput>;

export function toJSONSchema<TInput, TOutput>(
  schema: SchemaLike<TInput, TOutput>,
  options?: {
    target?: "draft-07" | "draft-2020-12" | "openapi-3.0";
  }
): JSONSchema7;
export function fromJSONSchema<TOutput extends JSON>(
  jsonSchema: JSONSchema7
): Schema<JSON, TOutput>;
export function extendJSONSchema<TInput, TOutput>(
  schema: SchemaLike<TInput, TOutput>,
  jsonSchema: JSONSchema7
): Schema<TInput, TOutput>;
/** Enables `~standard.jsonSchema`; its input/output throw before this is called. */
export function enableStandardJSONSchema(): void;

// ==================================================================================================
// JSON Schema Draft 07
// ==================================================================================================
// https://tools.ietf.org/html/draft-handrews-json-schema-validation-01
// --------------------------------------------------------------------------------------------------

/**
 * Primitive type
 * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-6.1.1
 */
export type JSONSchema7TypeName =
  | "string" //
  | "number"
  | "integer"
  | "boolean"
  | "object"
  | "array"
  | "null";

/**
 * Primitive type
 * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-6.1.1
 */
export type JSONSchema7Type =
  | string //
  | number
  | boolean
  | JSONSchema7Object
  | JSONSchema7Array
  | null;

// Workaround for infinite type recursion
export interface JSONSchema7Object {
  [key: string]: JSONSchema7Type;
}

// Workaround for infinite type recursion
// https://github.com/Microsoft/TypeScript/issues/3496#issuecomment-128553540
export interface JSONSchema7Array extends Array<JSONSchema7Type> {}

/**
 * Meta schema
 *
 * Recommended values:
 * - 'http://json-schema.org/schema#'
 * - 'http://json-schema.org/hyper-schema#'
 * - 'http://json-schema.org/draft-07/schema#'
 * - 'http://json-schema.org/draft-07/hyper-schema#'
 *
 * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-5
 */
export type JSONSchema7Version = string;

/**
 * JSON Schema v7
 * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01
 */
export type JSONSchema7Definition = JSONSchema7 | boolean;
export interface JSONSchema7 {
  $id?: string | undefined;
  $ref?: string | undefined;
  $schema?: JSONSchema7Version | undefined;
  $comment?: string | undefined;

  /**
   * @see https://datatracker.ietf.org/doc/html/draft-bhutton-json-schema-00#section-8.2.4
   * @see https://datatracker.ietf.org/doc/html/draft-bhutton-json-schema-validation-00#appendix-A
   */
  $defs?:
    | {
        [key: string]: JSONSchema7Definition;
      }
    | undefined;

  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-6.1
   */
  type?: JSONSchema7TypeName | JSONSchema7TypeName[] | undefined;
  enum?: JSONSchema7Type[] | undefined;
  const?: JSONSchema7Type | undefined;

  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-6.2
   */
  multipleOf?: number | undefined;
  maximum?: number | undefined;
  exclusiveMaximum?: number | undefined;
  minimum?: number | undefined;
  exclusiveMinimum?: number | undefined;

  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-6.3
   */
  maxLength?: number | undefined;
  minLength?: number | undefined;
  pattern?: string | undefined;

  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-6.4
   */
  items?: JSONSchema7Definition | JSONSchema7Definition[] | undefined;
  prefixItems?: JSONSchema7Definition[] | undefined;
  additionalItems?: JSONSchema7Definition | undefined;
  maxItems?: number | undefined;
  minItems?: number | undefined;
  uniqueItems?: boolean | undefined;
  contains?: JSONSchema7Definition | undefined;

  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-6.5
   */
  maxProperties?: number | undefined;
  minProperties?: number | undefined;
  required?: string[] | undefined;
  properties?:
    | {
        [key: string]: JSONSchema7Definition;
      }
    | undefined;
  patternProperties?:
    | {
        [key: string]: JSONSchema7Definition;
      }
    | undefined;
  additionalProperties?: JSONSchema7Definition | undefined;
  dependencies?:
    | {
        [key: string]: JSONSchema7Definition | string[];
      }
    | undefined;
  propertyNames?: JSONSchema7Definition | undefined;

  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-6.6
   */
  if?: JSONSchema7Definition | undefined;
  then?: JSONSchema7Definition | undefined;
  else?: JSONSchema7Definition | undefined;

  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-6.7
   */
  allOf?: JSONSchema7Definition[] | undefined;
  anyOf?: JSONSchema7Definition[] | undefined;
  oneOf?: JSONSchema7Definition[] | undefined;
  not?: JSONSchema7Definition | undefined;

  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-7
   */
  format?: string | undefined;

  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-8
   */
  contentMediaType?: string | undefined;
  contentEncoding?: string | undefined;

  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-9
   */
  definitions?:
    | {
        [key: string]: JSONSchema7Definition;
      }
    | undefined;

  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-10
   */
  title?: string | undefined;
  description?: string | undefined;
  default?: JSONSchema7Type | undefined;
  readOnly?: boolean | undefined;
  writeOnly?: boolean | undefined;
  examples?: JSONSchema7Type | undefined;
}
