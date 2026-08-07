// The Standard Schema and JSON Schema specs are mirrored under ./src/types.
// Imported as well as re-exported, since the declarations below refer to them.
import type { StandardJSONSchemaV1, StandardSchemaV1 } from "./src/types/standard.js";
import type {
  JSONSchema,
  JSONSchema2020,
  JSONSchema7,
  OpenAPISchema30,
} from "./src/types/jsonschema.js";

export * from "./src/types/standard.js";
export * from "./src/types/jsonschema.js";



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
  // validate reports a failed input as `issues`, but throws when the schema
  // has no compilable parse operation at all (a rejected `.to` conversion) —
  // that's a bug in the schema, not a verdict on the value.
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

export { union as anyOf };

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
//
// A bound binds one value, so it may only rewrite the input side when that is
// the same value as the output — `TInput extends TOutput` is what guards it.
// A codec's input is a different value that happens to be reachable from the
// bounded one, and its length says nothing: `S.string.with(S.to, S.array(...))`
// under `S.empty` bounds the array, never the string it decodes from.
// `Tail` is what follows the `N` fixed elements: nothing for an exact bound,
// `E[]` for a lower one, which is the only difference between the two.
type Repeat<E, N extends number, Acc extends unknown[], Tail extends unknown[]> =
  Acc["length"] extends N
    ? [...Acc, ...Tail]
    : Acc["length"] extends 64
    ? E[]
    : Repeat<E, N, [...Acc, E], Tail>;
// `N extends N` distributes, so a bound that isn't one literal resolves per
// member. Without it `0 | 2` matches the empty branch alone and silently pins
// the type to `[]`.
//
// Every bound speaks only while the arity is still open (`number extends
// T["length"]`) — on a tuple it is either the no-op the runtime makes of a
// redundant bound or a contradiction, and `Repeat` rebuilding a tuple from the
// union of its elements would turn `["bar", number]` under a redundant
// `length(2)` into `[number | "bar", number | "bar"]`.
type Sized<T, N extends number> = number extends N
  ? T
  : N extends N
  ? T extends (infer E)[]
    ? number extends T["length"]
      ? Repeat<E, N, [], []>
      : T
    : T extends string
    ? N extends 0
      ? ""
      : T
    : T
  : never;
// A lower bound fixes a head and leaves the tail open, so unlike the exact one
// it still has something new to say when stacked on its own kind — the head
// just grows.
//
// No string case. A tuple carries its arity, but TypeScript has no type for a
// string of at least N characters — `${string}${string}` is `string`, since each
// segment matches the empty string — so every lower bound leaves a string as it
// found it. The exact bound reaches `""` only because that one length has a
// literal to name it.
type AtLeast<T, N extends number> = number extends N
  ? T
  : N extends N
  ? T extends (infer E)[]
    ? number extends T["length"]
      ? Repeat<E, N, [], E[]>
      : T
    : T
  : never;
// `Sized<T, 0>` reaches the same answers, through a guard on a bound that
// can't vary and a `Repeat` that stops on its first step. Spelling the two
// cases `empty` has costs less than either.
type Emptied<T> = T extends unknown[]
  ? number extends T["length"]
    ? []
    : T
  : T extends string
  ? ""
  : T;
// `AtLeast<T, 1>`, minus the guard on a bound that can't vary.
type NonEmptied<T> = T extends (infer E)[]
  ? number extends T["length"]
    ? [E, ...E[]]
    : T
  : T;

export const minLength: <TInput, TOutput extends string | unknown[], N extends number>(
  schema: SchemaLike<TInput, TOutput>,
  length: N,
  message?: string
) => Schema<TInput extends TOutput ? AtLeast<TInput, N> : TInput, AtLeast<TOutput, N>>;
export const maxLength: <TInput, TOutput extends string | unknown[]>(
  schema: SchemaLike<TInput, TOutput>,
  length: number,
  message?: string
) => Schema<TInput, TOutput>;
export const length: <TInput, TOutput extends string | unknown[], N extends number>(
  schema: SchemaLike<TInput, TOutput>,
  length: N,
  message?: string
) => Schema<TInput extends TOutput ? Sized<TInput, N> : TInput, Sized<TOutput, N>>;
export const empty: <TInput, TOutput extends string | unknown[]>(
  schema: SchemaLike<TInput, TOutput>,
  message?: string
) => Schema<TInput extends TOutput ? Emptied<TInput> : TInput, Emptied<TOutput>>;
export const nonEmpty: <TInput, TOutput extends string | unknown[]>(
  schema: SchemaLike<TInput, TOutput>,
  message?: string
) => Schema<TInput extends TOutput ? NonEmptied<TInput> : TInput, NonEmptied<TOutput>>;

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

// The dialect the `target` option selects decides the shape of the result, so
// each one gets its own overload. Falling back to the widest type for a
// non-literal target is what keeps a caller holding `target` in a variable
// compiling.
export function toJSONSchema<TInput, TOutput>(
  schema: SchemaLike<TInput, TOutput>
): JSONSchema7;
export function toJSONSchema<TInput, TOutput>(
  schema: SchemaLike<TInput, TOutput>,
  options: { target?: "draft-07" }
): JSONSchema7;
export function toJSONSchema<TInput, TOutput>(
  schema: SchemaLike<TInput, TOutput>,
  options: { target: "draft-2020-12" }
): JSONSchema2020;
export function toJSONSchema<TInput, TOutput>(
  schema: SchemaLike<TInput, TOutput>,
  options: { target: "openapi-3.0" }
): OpenAPISchema30;
export function toJSONSchema<TInput, TOutput>(
  schema: SchemaLike<TInput, TOutput>,
  options: { target: StandardJSONSchemaV1.Target }
): JSONSchema;
/**
 * Builds a schema from a JSON Schema at runtime.
 *
 * Takes `unknown` so a schema read from a file or an API needs no cast. To have
 * TypeScript check one written inline, annotate it: `{ ... } satisfies S.JSONSchema`.
 *
 * The result parses JSON into JSON — the described type is not known statically.
 * Use `S.to` to refine it further.
 */
export function fromJSONSchema(jsonSchema: unknown): Schema<JSON, JSON>;
export function extendJSONSchema<TInput, TOutput>(
  schema: SchemaLike<TInput, TOutput>,
  jsonSchema: JSONSchema
): Schema<TInput, TOutput>;
/** Enables `~standard.jsonSchema`; its input/output throw before this is called. */
export function enableStandardJSONSchema(): void;
