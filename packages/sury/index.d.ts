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
/**
 * A type definition that exists at runtime. The parameters read in the
 * direction data flows — `Schema<TInput, TOutput>` is the encoded type the
 * schema accepts, then the decoded type it produces — and `TOutput` defaults
 * to `TInput`, so an identity schema is just `S.Schema<string>`.
 *
 * To accept "any schema producing `T`", leave the input side `unknown`:
 *
 * ```ts
 * const parse = <T>(schema: S.Schema<unknown, T>, data: unknown): T =>
 *   S.parser(schema)(data);
 * ```
 *
 * The runtime representation is JSON Schema-shaped and readable as-is:
 * `S.schema("Hi")` logs `{ type: "string", const: "Hi" }`.
 */
export type Schema<TInput = unknown, TOutput = TInput> = {
  /**
   * Fluent application: `schema.with(fn, ...args)` is `fn(schema, ...args)`
   * with the types threaded through — one method covers `S.to`, `S.refine`,
   * `S.meta`, every refinement, and your own functions of the same shape.
   *
   * ```ts
   * const schema = S.string
   *   .with(S.minLength, 1)
   *   .with(S.to, S.number)
   *   .with(S.meta, { description: "Numeric id on the wire" });
   * ```
   *
   * In a shared module, prefer the functional form (`S.meta(schema, …)`)
   * for metadata-only tweaks — a `.with` call on an opaque receiver can't
   * be dropped by tree-shaking, the functional call can.
   */
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
  // Constraining TArg1 to string makes a string-literal arg1 (e.g.
  // `.with(S.brand, "myId")`) infer its literal type instead of widening to
  // `string` — needed for brand-based nominal typing. The next overload
  // covers the general (non-string) arg1 case.
  with<TNextInput, TNextOutput, TArg1 extends string>(
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
   * `${S.string}`;                   // "Schema<string>"
   * `${S.to(S.string, S.number)}`;   // "Schema<string, number>"
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

/**
 * Everything Sury throws — an `Error` subclass, so `err instanceof S.Error`
 * works. `message` includes the failure path; the `code`-specific fields
 * carry structured detail. Wrap operations with {@link safe} /
 * {@link safeAsync} when you'd rather have a result than an exception.
 */
export const Error: {
  new (): Error;
  prototype: Error;
};

// Extract Output/Input by matching only the `~standard` marker instead of the
// full `Schema<…>` shape (whose 14-member union + `with` overloads are costly to
// instantiate per match). `types` is optional, so the pattern keeps it optional.
/**
 * The decoded type a schema produces. {@link Infer} is an alias; {@link Input}
 * extracts the encoded side.
 *
 * ```ts
 * const playerSchema = S.schema({ username: S.string, xp: S.number });
 * type Player = S.Infer<typeof playerSchema>;
 * // { username: string; xp: number }
 * ```
 */
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

/**
 * Attaches a type-only nominal brand to the schema's output — runtime
 * behavior doesn't change, so only values that went through the schema
 * satisfy the branded type.
 *
 * ```ts
 * const userIdSchema = S.string.with(S.brand, "UserId");
 * type UserId = S.Infer<typeof userIdSchema>; // S.Brand<string, "UserId">
 *
 * const id: UserId = S.parser(userIdSchema)("u_123");
 * // @ts-expect-error - a plain string is not a UserId
 * const notId: UserId = "u_123";
 * ```
 */
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

/**
 * Turns any definition into a schema: schemas stay as-is, plain values become
 * deep-checked literals keeping their narrow type, objects and arrays
 * recurse. {@link literal}, {@link object} and {@link tuple} are aliases for
 * when the name reads better.
 *
 * ```ts
 * const playerSchema = S.schema({
 *   kind: "player", // literal field, inferred as "player" — not string
 *   username: S.string,
 *   xp: S.number,
 * });
 *
 * S.schema([S.string, S.number]); // tuple
 * S.schema("tuna"); // literal
 * ```
 */
export function schema<const T extends unknown[]>(
  schemas: [...T]
): Schema<[...UnknownArrayToInput<T>], [...UnknownArrayToOutput<T>]>;
export function schema<const T>(
  value: T
): Schema<UnknownToInput<T>, UnknownToOutput<T>>;

/** Alias of {@link schema} — reads better when the definition is a single literal value, e.g. `S.literal("tuna")`. */
export function literal<const T>(
  value: T
): Schema<UnknownToInput<T>, UnknownToOutput<T>>;

/**
 * Logical OR: members are matched in the order they're passed and the first
 * fit wins. Members go through {@link schema}, so enums and discriminated
 * unions need no extra ceremony:
 *
 * ```ts
 * S.union(["Win", "Draw", "Loss"]);
 *
 * const shapeSchema = S.union([
 *   { kind: "circle", radius: S.number },
 *   { kind: "square", x: S.number },
 * ]);
 * ```
 *
 * Also exported as `S.anyOf`, matching the JSON Schema keyword it maps to.
 */
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
/** A number restricted to 32-bit integers — `"type": "integer"` in JSON Schema terms. */
export const int32: Schema<number, number>;
export const number: Schema<number, number>;
export const bigint: Schema<bigint, bigint>;
export const symbol: Schema<symbol, symbol>;
/** Fails on every value. Useful to forbid a field, or with `S.to` to mark a union member unreachable. */
export const never: Schema<never, never>;
/** Accepts any value as-is. */
export const unknown: Schema<unknown, unknown>;
/** Same as {@link unknown} at runtime, but typed `any` so the result needs no casting. */
export const any: Schema<any, any>;
declare const void_: Schema<void, void>;
export { void_ as void };

/**
 * Any JSON value: `string | boolean | number | null | { … } | […]`. Also a
 * pipeline stage — `schema.with(S.to, S.json)` describes "whatever this is
 * on the wire, as JSON".
 */
export const json: Schema<JSON, JSON>;

/**
 * A string containing valid JSON. Chain it to parse and validate in one
 * generated function — no `JSON.parse` in your own code:
 *
 * ```ts
 * const schema = S.jsonString.with(S.to, S.number);
 * S.parser(schema)("123"); // 123
 * S.encoder(schema)(123); // "123"
 * ```
 */
export const jsonString: Schema<string, string>;
/** {@link jsonString} that pretty-prints with the given indentation when encoding. */
export const jsonStringWithSpace: (space: number) => Schema<string, string>;

/**
 * A `Uint8Array` instance. Chain to decode a UTF-8 byte payload:
 *
 * ```ts
 * S.uint8Array.with(S.to, S.string); // bytes -> text, reversible
 * ```
 */
export const uint8Array: Schema<Uint8Array, Uint8Array>;

/**
 * An ISO 8601 UTC datetime string — no timezone offsets, arbitrary
 * sub-second precision. To decode into a `Date`, use
 * `S.string.with(S.to, S.date)` instead.
 */
export const isoDateTime: Schema<string, string>;

/** A valid TCP port number. */
export const port: Schema<number, number>;

/**
 * An email address, by a deliberately simple regex — the only real way to
 * validate an email is to send something to it.
 */
export const email: Schema<string, string>;

export const uuid: Schema<string, string>;

export const cuid: Schema<string, string>;

export const url: Schema<string, string>;

/**
 * A `Date` instance that isn't Invalid Date. Validates existing objects —
 * for "ISO string -> Date" use `S.string.with(S.to, S.date)`.
 */
export const date: Schema<Date, Date>;

/**
 * Runs the callback and turns whatever it throws into a typed result —
 * the functional alternative to try/catch, with room for more logic than a
 * single operation:
 *
 * ```ts
 * const result = S.safe(() => S.parser(S.string)(123));
 * if (result.success) result.value; // string
 * else result.error; // S.Error
 * ```
 */
export function safe<TValue>(scope: () => TValue): Result<TValue>;
/** {@link safe} for async callbacks — resolves to the result instead of rejecting. */
export function safeAsync<TValue>(
  scope: () => Promise<TValue>
): Promise<Result<TValue>>;

/**
 * The same schema with Input and Output swapped — validation and
 * transformations run backwards.
 *
 * ```ts
 * const schema = S.string.with(S.to, S.number);
 * S.parser(S.reverse(schema))(123); // "123"
 * ```
 *
 * `S.encoder(schema)` is the shorthand when you just want the reverse
 * conversion function.
 */
export function reverse<TInput, TOutput>(
  schema: SchemaLike<TInput, TOutput>
): Schema<TOutput, TInput>;

/**
 * Compiles the schema into a function that validates unknown input and
 * returns a typed deep copy, with unknown object keys stripped by default.
 * Invalid input throws {@link Error}.
 *
 * ```ts
 * const parse = S.parser(S.schema({ id: S.string }));
 * parse({ id: "1" }); // { id: "1" }
 * parse({ id: 1 }); // throws S.Error
 * ```
 *
 * It's {@link decoder} with `S.unknown` on the input side — extra schemas
 * chain into a pipeline the same way. When you only need a yes/no, use
 * {@link assert} or {@link is}; they skip building the output and run 2–3×
 * faster.
 */
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

/** {@link parser} for schemas with async transformations — the returned function resolves to the output. */
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

/**
 * Compiles a conversion from the first schema's Input to the last schema's
 * Output, fused into one generated function. The input is trusted to match
 * the first schema — its type checks are skipped, everything downstream
 * (transforms, refinements, target validation) still runs.
 *
 * ```ts
 * const userSchema = S.schema({ id: S.string });
 *
 * // JSON text -> validated user, in one pass
 * const parseUser = S.decoder(S.jsonString, userSchema);
 * parseUser('{"id":"1"}'); // { id: "1" }
 * ```
 *
 * For untrusted input, use {@link parser}.
 */
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

/** {@link decoder} for schemas with async transformations — the returned function resolves to the output. */
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

/**
 * The reverse of {@link decoder}: converts the last schema's Output back to
 * the first schema's Input, running every transformation backwards.
 *
 * ```ts
 * const userSchema = S.schema({ id: S.string.with(S.to, S.bigint) });
 * S.encoder(userSchema, S.jsonString)({ id: 1n }); // '{"id":"1"}'
 * ```
 */
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

/** {@link encoder} for schemas with async transformations — the returned function resolves to the input. */
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

/**
 * Validates without building an output — 2–3× faster than {@link parser}.
 * Arguments work in either order, and the checked value's type narrows:
 *
 * ```ts
 * declare const data: unknown;
 * S.assert(data, S.string);
 * data; // string from here on
 * ```
 */
export function assert<TInput, TOutput>(
  schema: SchemaLike<TInput, TOutput>,
  data: unknown
): asserts data is TInput;
export function assert<TInput, TOutput>(
  data: unknown,
  schema: SchemaLike<TInput, TOutput>
): asserts data is TInput;

/**
 * {@link assert} as a boolean type guard — the same fast validate-only path,
 * arguments in either order:
 *
 * ```ts
 * declare const data: unknown;
 * if (S.is(data, S.string)) {
 *   data; // string in this branch
 * }
 * ```
 */
export function is<TInput, TOutput>(
  schema: SchemaLike<TInput, TOutput>,
  data: unknown
): data is TInput;
export function is<TInput, TOutput>(
  data: unknown,
  schema: SchemaLike<TInput, TOutput>
): data is TInput;

/**
 * For a plain tuple, `S.schema([S.string, S.number])` is enough. The definer
 * form restructures positional data into a friendlier shape with zero
 * runtime overhead — and the same schema encodes it back:
 *
 * ```ts
 * const athleteSchema = S.tuple((s) => ({
 *   name: s.item(0, S.string),
 *   jerseyNumber: s.item(1, S.number),
 * }));
 * // parse: ["Wilt", 13] -> { name: "Wilt", jerseyNumber: 13 }
 * ```
 */
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

/**
 * Accepts `undefined`, optionally replacing it with a default. Pass a
 * function to compute the default per parse:
 *
 * ```ts
 * S.optional(S.string); // string | undefined
 * S.optional(S.string, "anonymous"); // string — undefined becomes "anonymous"
 * S.optional(S.number, Math.random); // fresh default on every parse
 * ```
 *
 * An object field whose type admits `undefined` is automatically optional.
 */
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

/**
 * Accepts `null`, optionally replacing it with a default:
 *
 * ```ts
 * S.nullable(S.string); // string | null
 * S.nullable(S.string, "fallback"); // string — null becomes "fallback"
 * ```
 */
export function nullable<TInput, TOutput, TOr extends TOutput | null = null>(
  schema: SchemaLike<TInput, TOutput>,
  or?: (() => TOr) | TOr,
  // To make .with work
  _?: never
): Schema<TInput | null, TOr extends null ? TOutput | null : TOutput>;

/** Accepts both `undefined` and `null` — {@link optional} and {@link nullable} in one. */
export const nullish: <TInput, TOutput>(
  schema: SchemaLike<TInput, TOutput>
) => Schema<TInput | undefined | null, TOutput | undefined | null>;

export type Class<T> = new (...args: readonly any[]) => T;
/**
 * Validates `data instanceof class_` — and the go-to base for a custom
 * schema around a third-party class: add decode/encode logic with `S.to`
 * and a readable name with `S.meta`.
 *
 * ```ts
 * const blobSchema = S.instance(Blob);
 * ```
 */
export const instance: <T>(class_: Class<T>) => Schema<T, T>;

export const array: <TInput, TOutput>(
  schema: SchemaLike<TInput, TOutput>
) => Schema<TInput[], TOutput[]>;

/**
 * Rows laid out as one column-array per field — pair with `S.to` to move
 * between the two layouts in both directions:
 *
 * ```ts
 * const rowSchema = S.schema({ id: S.string, deleted: S.boolean });
 * const schema = S.compactColumns(S.json).with(S.to, S.array(rowSchema));
 *
 * S.encoder(schema)([{ id: "0", deleted: false }]); // [["0"], [false]]
 * ```
 */
export const compactColumns: <TInput, TOutput>(
  schema: SchemaLike<TInput, TOutput>
) => Schema<TInput[][], TOutput[][]>;

/** `{ [key: string]: TOutput }` — validates the values, keeps the keys. */
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

/**
 * For plain shapes, `S.schema({ … })` is enough. The definer form renames
 * and moves fields with zero runtime overhead, and the same schema encodes
 * the transformed value back to the original format:
 *
 * ```ts
 * const userSchema = S.object((s) => ({
 *   id: s.field("USER_ID", S.number),
 *   name: s.field("USER_NAME", S.string),
 * }));
 * // parse: { USER_ID: 1, USER_NAME: "John" } -> { id: 1, name: "John" }
 * ```
 *
 * The context also offers `fieldOr` for defaults, `tag` for discriminants,
 * `flatten` to reuse another object schema's fields, and `nested` for
 * reaching into child objects.
 */
export function object<TInput extends Record<string, unknown>, TOutput>(
  definer: (ctx: ObjectCtx<TInput>) => TOutput
): Schema<TInput, TOutput>;
export function object<T extends Record<string, unknown>>(
  definition: T
): Schema<UnknownToInput<T>, UnknownToOutput<T>>;

/** Restores the default policy of silently stripping unknown object keys (top level only). */
export function strip<TInput extends Record<string, unknown>, TOutput>(
  schema: SchemaLike<TInput, TOutput>
): Schema<TInput, TOutput>;
/** {@link strip} applied to every nested object schema as well. */
export function deepStrip<TInput extends Record<string, unknown>, TOutput>(
  schema: SchemaLike<TInput, TOutput>
): Schema<TInput, TOutput>;
/**
 * Fails on unknown object keys instead of stripping them (top level only —
 * see {@link deepStrict}). To make this the default for every schema, use
 * `S.global({ defaultAdditionalItems: "strict" })`.
 */
export function strict<TInput extends Record<string, unknown>, TOutput>(
  schema: SchemaLike<TInput, TOutput>
): Schema<TInput, TOutput>;
/** {@link strict} applied to every nested object schema as well. */
export function deepStrict<TInput extends Record<string, unknown>, TOutput>(
  schema: SchemaLike<TInput, TOutput>
): Schema<TInput, TOutput>;

// Bare Flatten, not ResolveObject: re-splitting the merged intersection to
// hoist optionals last nearly doubled this type's instantiation cost, so Merge
// keeps insertion order.
type Merge<TLeft, TRight> = Flatten<
  { [K in keyof TLeft as K extends keyof TRight ? never : K]: TLeft[K] } & TRight
>;

/**
 * Combines the fields of two object schemas. Throws where it's called if
 * the schemas share keys; the result inherits the strip/strict policy of
 * the second schema.
 *
 * ```ts
 * const teacherSchema = S.merge(
 *   S.schema({ students: S.array(S.string) }),
 *   S.schema({ id: S.string })
 * );
 * ```
 */
export function merge<
  TInput1,
  TOutput1 extends Record<string, unknown>,
  TInput2,
  TOutput2 extends Record<string, unknown>
>(
  schema1: SchemaLike<TInput1, TOutput1>,
  schema2: SchemaLike<TInput2, TOutput2>
): Schema<Merge<TInput1, TInput2>, Merge<TOutput1, TOutput2>>;

/**
 * A schema that references itself. TypeScript can't infer the type, so pass
 * it explicitly — one parameter when the schema doesn't transform, both in
 * `Schema<TInput, TOutput>` order when it does. The identifier names the
 * schema in errors and `$defs`.
 *
 * ```ts
 * type Node = { id: string; children: Node[] };
 *
 * const nodeSchema = S.recursive<Node>("Node", (nodeSchema) =>
 *   S.schema({ id: S.string, children: S.array(nodeSchema) })
 * );
 * ```
 */
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

/**
 * A copy of the schema with metadata attached — it surfaces in
 * `S.toJSONSchema` output and in error messages (`name`, `errorMessage`).
 *
 * ```ts
 * S.string.with(S.meta, { description: "User-visible label" });
 *
 * // Override validation messages per constraint, or "_" as catch-all
 * S.email.with(S.meta, { errorMessage: { format: "Must be a valid email" } });
 * ```
 */
export function meta<TInput, TOutput>(
  schema: SchemaLike<TInput, TOutput>,
  meta: Meta<TOutput>
): Schema<TInput, TOutput>;

/**
 * Human-readable expression of the schema's Input type, e.g.
 * `"{ abc: number; }"` — what error messages print. The format is subject
 * to change.
 */
export function inputExpression(schema: SchemaLike<unknown, unknown>): string;
/** {@link inputExpression} for the schema's Output type. */
export function outputExpression(schema: SchemaLike<unknown, unknown>): string;
/**
 * Turns off the schema's own type checks in parse operations — transforms
 * and refinements still run. For trusted data where you only want the
 * conversion.
 */
export function noValidation<TInput, TOutput>(
  schema: SchemaLike<TInput, TOutput>,
  value: boolean
): Schema<TInput, TOutput>;

/**
 * An async check that runs on decode only — throw inside to reject the
 * value. Schemas with async logic compile with {@link asyncParser} /
 * {@link asyncDecoder}.
 *
 * ```ts
 * declare const isActiveUser: (id: string) => Promise<boolean>;
 *
 * const idSchema = S.uuid.with(S.asyncDecoderAssert, async (id) => {
 *   if (!(await isActiveUser(id))) {
 *     throw new Error(`The user ${id} is inactive.`);
 *   }
 * });
 * ```
 */
export function asyncDecoderAssert<TInput, TOutput>(
  schema: SchemaLike<TInput, TOutput>,
  assertFn: (value: TOutput) => Promise<void>
): Schema<TInput, TOutput>;

/**
 * Custom validation for checks the type system can't express. Return `false`
 * to fail; the check runs on both parse and encode. Chain several — they
 * apply in order.
 *
 * ```ts
 * const passwordFormSchema = S.schema({
 *   password: S.string,
 *   confirm: S.string,
 * }).with(S.refine, (data) => data.password === data.confirm, {
 *   error: "Passwords don't match",
 *   path: ["confirm"], // attach the error to a specific field
 * });
 * ```
 */
export function refine<TInput, TOutput>(
  schema: SchemaLike<TInput, TOutput>,
  refineCheck: (value: TOutput) => boolean,
  refineOptions?: {
    error?: string;
    path?: string[];
  }
): Schema<TInput, TOutput>;

/**
 * Requires `output > value`. Works on `S.number`, `S.bigint` and the numeric
 * formats, whose own range takes part — a bound outside it fails where it's
 * written. All built-in refinements take an optional custom message last:
 *
 * ```ts
 * S.number.with(S.gt, 0, "Must be positive");
 * ```
 */
export const gt: <TInput, TOutput extends number | bigint>(
  schema: SchemaLike<TInput, TOutput>,
  value: TOutput,
  message?: string
) => Schema<TInput, TOutput>;
/** Requires `output >= value` — see {@link gt}. */
export const gte: <TInput, TOutput extends number | bigint>(
  schema: SchemaLike<TInput, TOutput>,
  value: TOutput,
  message?: string
) => Schema<TInput, TOutput>;
/** Requires `output < value` — see {@link gt}. */
export const lt: <TInput, TOutput extends number | bigint>(
  schema: SchemaLike<TInput, TOutput>,
  value: TOutput,
  message?: string
) => Schema<TInput, TOutput>;
/** Requires `output <= value` — see {@link gt}. */
export const lte: <TInput, TOutput extends number | bigint>(
  schema: SchemaLike<TInput, TOutput>,
  value: TOutput,
  message?: string
) => Schema<TInput, TOutput>;

/**
 * Requires `output.length >= length` — works on strings and arrays, like the
 * rest of the length refinements ({@link maxLength}, {@link length},
 * {@link empty}, {@link nonEmpty}). Optional custom message last:
 *
 * ```ts
 * S.string.with(S.minLength, 5, "Too short");
 * S.array(S.string).with(S.minLength, 1);
 * ```
 */
export const minLength: <TInput, TOutput extends string | unknown[]>(
  schema: SchemaLike<TInput, TOutput>,
  length: number,
  message?: string
) => Schema<TInput, TOutput>;
/** Requires `output.length <= length` — see {@link minLength}. */
export const maxLength: <TInput, TOutput extends string | unknown[]>(
  schema: SchemaLike<TInput, TOutput>,
  length: number,
  message?: string
) => Schema<TInput, TOutput>;
/** Requires `output.length === length` — see {@link minLength}. */
export const length: <TInput, TOutput extends string | unknown[]>(
  schema: SchemaLike<TInput, TOutput>,
  length: number,
  message?: string
) => Schema<TInput, TOutput>;
/** Requires an empty string or array — see {@link minLength}. */
export const empty: <TInput, TOutput extends string | unknown[]>(
  schema: SchemaLike<TInput, TOutput>,
  message?: string
) => Schema<TInput, TOutput>;
/** Requires a non-empty string or array — see {@link minLength}. */
export const nonEmpty: <TInput, TOutput extends string | unknown[]>(
  schema: SchemaLike<TInput, TOutput>,
  message?: string
) => Schema<TInput, TOutput>;

/** Requires the string to match the regex, e.g. `S.string.with(S.pattern, /^\d+$/, "Must be numeric")`. */
export const pattern: <TInput>(
  schema: SchemaLike<TInput, string>,
  re: RegExp,
  message?: string
) => Schema<TInput, string>;
/** Trims surrounding whitespace on parse. */
export const trim: <TInput>(
  schema: SchemaLike<TInput, string>
) => Schema<TInput, string>;

export type AdditionalItemsMode = "strip" | "strict";

export type GlobalConfigOverride = {
  defaultAdditionalItems?: AdditionalItemsMode;
  disableNanNumberValidation?: boolean;
};

/**
 * Overrides library-wide defaults — call once at app startup, before
 * operations are compiled:
 *
 * ```ts
 * S.global({ defaultAdditionalItems: "strict" });
 * ```
 */
export function global(globalConfigOverride: GlobalConfigOverride): void;

/**
 * Declarative restructuring: the callback receives a proxy, not the value —
 * property accesses are recorded and compiled to direct assignments, and the
 * change reverses for encoding. No conditions or other runtime logic inside;
 * reach for `S.to` with a custom decode when you need that.
 *
 * ```ts
 * const circleSchema = S.number.with(S.shape, (radius) => ({
 *   kind: "circle",
 *   radius,
 * }));
 *
 * S.parser(circleSchema)(1); // { kind: "circle", radius: 1 }
 * S.encoder(circleSchema)({ kind: "circle", radius: 1 }); // 1
 * ```
 */
export function shape<TShape = unknown, TInput = unknown, TOutput = unknown>(
  schema: SchemaLike<TInput, TOutput>,
  shaper: (value: TOutput) => TShape
): Schema<TInput, TShape>;

/**
 * Converts to another schema, inferring the coercion — and its reverse —
 * from the two types. Works at the top level or inside any field, and the
 * whole chain compiles into one generated function.
 *
 * ```ts
 * const schema = S.string.with(S.to, S.number);
 *
 * S.parser(schema)("123"); // 123
 * S.parser(schema)("abc"); // throws: Expected number, received "abc"
 * S.encoder(schema)(123); // "123"
 * ```
 *
 * Prefer the built-in coercions; when none fits, pass custom decode/encode
 * functions:
 *
 * ```ts
 * const centsSchema = S.string.with(
 *   S.to,
 *   S.number,
 *   (dollars) => Math.round(Number(dollars) * 100),
 *   (cents) => (cents / 100).toFixed(2)
 * );
 * ```
 */
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

/**
 * Emits `"draft-07"` (the default), `"draft-2020-12"`, or `"openapi-3.0"`.
 * Properties and examples come out in the schema's Input format; convert
 * `S.reverse(schema)` for the Output side.
 *
 * ```ts
 * S.toJSONSchema(S.schema({ id: S.string }), { target: "draft-2020-12" });
 * ```
 */
export function toJSONSchema<TInput, TOutput>(
  schema: SchemaLike<TInput, TOutput>,
  options?: {
    target?: "draft-07" | "draft-2020-12" | "openapi-3.0";
  }
): JSONSchema7;
/**
 * Builds a schema from a JSON Schema document — the reverse of
 * {@link toJSONSchema}.
 *
 * ```ts
 * const schema = S.fromJSONSchema<string>({ type: "string", format: "email" });
 * ```
 */
export function fromJSONSchema<TOutput extends JSON>(
  jsonSchema: JSONSchema7
): Schema<JSON, TOutput>;
/** Attaches raw JSON Schema keywords that merge into {@link toJSONSchema} output. */
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
