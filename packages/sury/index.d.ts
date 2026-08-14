// The Standard Schema and JSON Schema specs are mirrored under ./src/types.
// Imported as well as re-exported, since the declarations below refer to them.
import type { StandardJSONSchemaV1, StandardSchemaV1 } from "./src/types/standard.js";
import type {
  JSONSchema,
  JSONSchema2020,
  JSONSchema7,
  OpenAPISchema30,
} from "./src/types/jsonschema.js";
import type { FromJSONSchema, FromJSONSchemaOutput, JSON } from "./src/types/json.js";

export * from "./src/types/standard.js";
export * from "./src/types/jsonschema.js";
export * from "./src/types/json.js";



export type SuccessResult<TValue> = {
  readonly success: true;
  readonly value: TValue;
  readonly error?: undefined;
};

export type FailureResult = {
  readonly success: false;
  readonly error: Error;
};

/**
 * What {@link safe} returns. Narrow on `success` before reading `value`.
 *
 * ```ts
 * const result = S.safe(() => S.parser(schema)(data));
 * if (result.success) use(result.value);
 * else console.log(result.error.message);
 * ```
 */
export type Result<TValue> = SuccessResult<TValue> | FailureResult;

export type NumberFormat = "int32" | "port" | "integer";
export type StringFormat =
  | "json"
  | "date-time"
  | "email"
  | "uuid"
  | "cuid"
  | "uri"
  | "date"
  | "time"
  | "duration"
  | "hostname"
  | "idn-hostname"
  | "ipv4"
  | "ipv6"
  | "uri-reference"
  | "uri-template"
  | "iri"
  | "iri-reference"
  | "idn-email"
  | "json-pointer"
  | "relative-json-pointer";
export type ArrayFormat = "compactColumns";
export type Format = NumberFormat | StringFormat | ArrayFormat;

// `TOutput = TInput` so an identity schema is spelled `Schema<string>`. The
// default is dependent, so TS instantiates it at every one-arg reference —
// internal references write `Schema<unknown, unknown>` in full to keep that
// off the per-schema type-cost the specs measure.
/**
 * A schema, typed in the direction data flows: the encoded type it accepts,
 * then the decoded type it produces. `TOutput` defaults to `TInput`, so an
 * identity schema is `S.Schema<string>`.
 *
 * To accept "any schema producing `T`, whatever it takes", leave the input
 * side `unknown`:
 *
 * ```ts
 * const parse = <T>(schema: S.Schema<unknown, T>, data: unknown): T =>
 *   S.parser(schema)(data);
 * ```
 */
export type Schema<TInput = unknown, TOutput = TInput> = {
  /**
   * Applies a schema function as a method — `schema.with(S.fn, ...args)` is
   * `S.fn(schema, ...args)`, chainable left to right.
   *
   * ```ts
   * S.string.with(S.minLength, 3).with(S.to, S.bigint);
   * ```
   *
   * A `.with` call can never be tree-shaken (the receiver is opaque, so the
   * bundler can't see which function ran). In a library that ships to
   * browsers, prefer the functional spelling.
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
      readonly multipleOf?: number;
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
      readonly minSize?: number;
      readonly maxSize?: number;
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

/**
 * Where inside the value a failure happened — `["items"]["0"]["id"]`, and `""`
 * at the root. Opaque, so read it as a string: `String(error.path)`.
 */
export abstract class Path {
  protected opaque: unknown;
} /* simulate opaque types */

type BaseError = {
  readonly path: Path;
  readonly message: string;
  readonly reason: string;
};

/**
 * What every operation throws. `code` discriminates it, `message` is
 * pre-formatted for display and `reason` is the same without the path.
 *
 * ```ts
 * try {
 *   S.parser(schema)(data);
 * } catch (e) {
 *   if (e instanceof S.Error && e.code === "unrecognized_keys") console.log(e.keys);
 *   else throw e;
 * }
 * ```
 *
 * `invalid_input` is a value that didn't match. `invalid_operation` and
 * `unsupported_decode` mean the schema itself can't do what was asked, and
 * throw at the `S.parser` / `S.encoder` call before any value is seen.
 * `invalid_conversion` wraps whatever a custom decode/encode threw, as `cause`.
 */
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
/**
 * The decoded type a schema produces.
 *
 * ```ts
 * type Player = S.Output<typeof playerSchema>;
 * ```
 */
export type Output<T> = T extends {
  readonly ["~standard"]: { readonly types?: { readonly output: infer TOutput } };
}
  ? TOutput
  : never;
/** {@link Output}, under the name the rest of the ecosystem uses. */
export type Infer<T> = Output<T>;
/** The encoded type a schema accepts — what {@link encoder} gives back. */
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
 * Marks the output type as nominal, so only a parsed value satisfies it. A
 * type-only marker — pair it with the validation that earns the brand.
 *
 * ```ts
 * const userIdSchema = S.string.with(S.pattern, /^u_/).with(S.brand, "UserId");
 * type UserId = S.Infer<typeof userIdSchema>; // S.Brand<string, "UserId">
 *
 * const id: UserId = S.parser(userIdSchema)("u_123"); // ok
 * const nope: UserId = "u_123"; // type error
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
 * Turns a definition into a schema: schemas pass through, an object or array
 * is built member by member, and any other value becomes a literal.
 *
 * ```ts
 * S.schema({ kind: "circle", radius: S.number });
 * //? S.Schema<{ kind: "circle"; radius: number }>
 * ```
 *
 * Anywhere a schema is accepted a definition works too — `S.array({ id: S.string })`
 * — so this is mostly for the top level. {@link literal} is the same factory
 * under a name that reads better for one value; {@link object} and
 * {@link tuple} take the same definitions and add a definer form.
 */
export function schema<const T extends unknown[]>(
  schemas: [...T]
): Schema<[...UnknownArrayToInput<T>], [...UnknownArrayToOutput<T>]>;
export function schema<const T>(
  value: T
): Schema<UnknownToInput<T>, UnknownToOutput<T>>;

/**
 * {@link schema}, named for the single-value case: `S.literal("tuna")`,
 * `S.literal(2n)`, `S.literal(Symbol.iterator)`. Matching is `===`, except
 * that plain objects and arrays are compared deeply and `NaN` via
 * `Number.isNaN`.
 */
export function literal<const T>(
  value: T
): Schema<UnknownToInput<T>, UnknownToOutput<T>>;

/**
 * Accepts a value matching any member; the first one that fits wins, so order
 * is the tie-breaker.
 *
 * ```ts
 * S.union(["Win", "Draw", "Loss"]);
 * S.union([
 *   { kind: "circle", radius: S.number },
 *   { kind: "square", x: S.number },
 * ]);
 * ```
 *
 * A nested union counts as one flat union. Also exported as `S.anyOf`,
 * matching the JSON Schema keyword it emits.
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
/**
 * Whole number inside the signed 32-bit range, emitted as `format: "int32"`.
 * The range is a real bound, so a wider one contradicts it and throws where
 * it's written: `S.int32.with(S.gte, 3_000_000_000)`. Use {@link integer} when
 * the value can be larger.
 */
export const int32: Schema<number, number>;
/** Whole number of any magnitude, emitted as `format: "integer"`. */
export const integer: Schema<number, number>;
/** Any JS number. `NaN` is rejected, unless {@link GlobalConfigOverride} turns that check off. */
export const number: Schema<number, number>;
export const bigint: Schema<bigint, bigint>;
export const symbol: Schema<symbol, symbol>;
/**
 * Accepts nothing. Useful to mark a union member as unreachable rather than
 * unsupported: `S.boolean.with(S.to, S.union([S.string, S.never.with(S.to, S.symbol)]))`.
 */
export const never: Schema<never, never>;
/** Accepts anything, validating nothing — the input side of {@link parser}. */
export const unknown: Schema<unknown, unknown>;
/** {@link unknown}, typed `any` so its output flows on without a cast. */
export const any: Schema<any, any>;
/** `undefined`, reported as `void` in errors and expressions. */
declare const void_: Schema<void, void>;
export { void_ as void };

/** Any JSON value, checked all the way down. A good source for {@link to} when the shape is decided elsewhere: `S.json.with(S.to, userSchema)`. */
export const json: Schema<JSON, JSON>;

/**
 * A string holding valid JSON. Chain it to say what the JSON must contain:
 *
 * ```ts
 * S.jsonString.with(S.to, userSchema); // parse and validate in one pass
 * S.number.with(S.to, S.jsonString);   // stringify
 * ```
 *
 * Encoding to it generates a dedicated stringifier instead of calling
 * `JSON.stringify`, usually 1.3-2x faster.
 */
export const jsonString: Schema<string, string>;
/** {@link jsonString} that indents by `space` when encoding. */
export const jsonStringWithSpace: (space: number) => Schema<string, string>;

/** A `Uint8Array`. `S.uint8Array.with(S.to, S.string)` decodes it as UTF-8, and the reverse encodes. */
export const uint8Array: Schema<Uint8Array, Uint8Array>;

// `Blob` and `File` are ambient globals, from lib.dom or @types/node. Naming
// them bare fails to typecheck for a consumer who has neither — including one
// who never touches these schemas — so they resolve through `globalThis`: the
// real type wherever it exists, a structural stand-in where it doesn't. The
// stand-in stays usable rather than erroring, because a runtime can carry the
// value while the project carries no types for it.
/**
 * The runtime's `Blob`, or a structural stand-in when the project has no type
 * for it. Exported because that stand-in is otherwise unnameable: a consumer
 * with neither lib.dom nor @types/node has no `Blob` of their own to annotate
 * with.
 */
export type Blob = typeof globalThis extends {
  Blob: abstract new (...args: never) => infer T;
}
  ? T
  : { readonly size: number; readonly type: string };

/** The runtime's `File`, or a structural stand-in. See {@link Blob}. */
export type File = typeof globalThis extends {
  File: abstract new (...args: never) => infer T;
}
  ? T
  : Blob & { readonly name: string };

/** A `Blob`, sized in bytes with {@link minSize} / {@link maxSize} / {@link size}. */
export const blob: Schema<Blob, Blob>;

/** A `File`. Every `File` satisfies {@link blob}, not the other way round. Takes the same size bounds. */
export const file: Schema<File, File>;

/**
 * RFC 3339 timestamp, **UTC only** — an offset like `+02:00` is rejected, which
 * is narrower than the JSON Schema `date-time` format it emits.
 * Calendar-aware: month, day, hour, minute and leap second are all range-checked.
 * @example "1963-06-19T08:30:06.283185Z"
 */
export const isoDateTime: Schema<string, string>;

/** TCP/UDP port: an integer in 0-65535. */
export const port: Schema<number, number>;

/**
 * Email address, ASCII only. Practical rather than exhaustive: it wants a dot-TLD
 * domain, so `a@localhost` and `a@127.0.0.1` are rejected.
 * @example "joe.bloggs@example.com"
 */
export const email: Schema<string, string>;

/**
 * UUID in canonical 8-4-4-4-12 hex form, any version.
 * @example "f81d4fae-7dec-11d0-a765-00a0c91e6bf6"
 */
export const uuid: Schema<string, string>;

/**
 * CUID. Not a JSON Schema format, so `toJSONSchema` emits a plain `string` for it.
 * @example "cjld2cjxh0000qzrmn831i7rn"
 */
export const cuid: Schema<string, string>;

/**
 * An instance of the JS `URL` class, parsed by the WHATWG URL Standard — the same
 * shape as {@link date}. Bare it accepts a `URL`; `S.string.with(S.to, S.url)`
 * parses a string into one and encodes back via `.href`.
 *
 * Not the same language as {@link uri}: WHATWG silently percent-encodes spaces,
 * quotes and backslashes that RFC 3986 forbids, and rejects reg-names like
 * `999.999.999.999` that RFC 3986 allows. Use this when you want the parsed
 * object; use {@link uri} when you want to validate a string stays a string.
 * @example new URL("https://example.com/a?b=c")
 */
export const url: Schema<URL, URL>;

/**
 * The runtime's `URL`, or a structural stand-in when the project has no type
 * for it. See {@link Blob} — `URL` is a lib.dom/@types/node global too, so
 * naming it bare would fail to typecheck for a consumer who has neither, one
 * who never touches {@link url} included.
 */
export type URL = typeof globalThis extends {
  URL: abstract new (...args: never) => infer T;
}
  ? T
  : { readonly href: string; toString(): string };

/**
 * URI string, RFC 3986 — a scheme is required. See {@link uriReference} for the
 * relative form, and {@link url} for a parsed `URL` instance instead of a string.
 *
 * Syntax only: **any** scheme parses, including `javascript:` and `file:`. To
 * restrict them, compose a pattern — the emitted JSON Schema keeps both
 * constraints, so it still describes the behavior:
 * `S.uri.with(S.pattern, /^https?:\/\//)`
 * @example "http://foo.bar/?baz=qux#quux"
 */
export const uri: Schema<string, string>;

/**
 * RFC 3339 full-date, no time component. Calendar-aware: rejects `2021-02-29`,
 * `2021-13-45` and `2020-04-31`, and honors the ÷100/÷400 century leap rule.
 * @example "1963-06-19"
 */
export const isoDate: Schema<string, string>;

/**
 * RFC 3339 full-time. An offset is **required** — `"12:00:00"` is invalid.
 * Leap seconds are correlated against UTC, so `01:29:60+01:30` is valid and
 * `23:59:60+01:00` is not.
 * @example "08:30:06Z"
 */
export const isoTime: Schema<string, string>;

/**
 * RFC 3339 duration. The ABNF nests its components, so a unit may only be
 * followed by the next smaller one: `P1Y2M3D` is valid, `P1Y2D` and `PT1H2S` are
 * not. Fractional seconds are not in the grammar. Note `PT1M` is one minute and
 * `P1M` is one month.
 * @example "P4DT12H30M5S"
 */
export const duration: Schema<string, string>;

/**
 * RFC 1123 hostname: 1-63 character labels, 253 overall.
 *
 * Syntax only, and **not a security boundary**. A bare label like `localhost` is
 * a valid hostname, as are `169.254.169.254` and `metadata.google.internal`. An
 * `xn--` label is accepted on shape alone — its Punycode is not decoded, so a
 * label that IDNA2008 disallows still passes. For an SSRF guard or a homograph
 * filter, add your own check on top.
 * @example "www.example.com"
 */
export const hostname: Schema<string, string>;

/**
 * Internationalized hostname — {@link hostname}'s label shape over the four
 * Unicode label separators, with the character repertoire left open.
 *
 * The IDNA2008 property, bidi and contextual rules are **not** applied; see the
 * caveats on {@link hostname}, which all apply here too.
 * @example "실례.테스트"
 */
export const idnHostname: Schema<string, string>;

/**
 * Dotted-quad IPv4. Rejects the `inet_aton` shorthands (`127.1`, `0x7f000001`)
 * that often slip past naive filters.
 *
 * Syntax only: loopback, private and link-local ranges all parse, so
 * `127.0.0.1` and `169.254.169.254` are valid. Not an SSRF defense on its own.
 * @example "192.168.0.1"
 */
export const ipv4: Schema<string, string>;

/**
 * IPv6 in any RFC 4291 form, including IPv4-mapped (`::ffff:192.168.0.1`). A
 * zone id (`fe80::a%eth1`) is not part of the format.
 *
 * Syntax only — see the caveats on {@link ipv4}.
 * @example "::1"
 */
export const ipv6: Schema<string, string>;

/**
 * URI reference, RFC 3986 — the scheme and path are both optional, so relative
 * forms parse. This is usually what you want for a link or `href` field, since
 * {@link uri} would reject `/dashboard`.
 *
 * Very permissive by design: `""`, `"abc"`, `"//evil.com"` and
 * `"javascript:alert(1)"` are all valid references. Compose a pattern if you
 * need to narrow it.
 * @example "/abc"
 */
export const uriReference: Schema<string, string>;

/**
 * RFC 6570 URI template — a URL *pattern* with `{placeholders}`, not a URL.
 * Used by HAL/JSON:API hypermedia links and OpenAPI path patterns.
 * @example "http://example.com/dictionary/{term:1}/{term}"
 */
export const uriTemplate: Schema<string, string>;

/**
 * IRI, RFC 3987 — {@link uri} with non-ASCII characters allowed unescaped.
 * Validated by percent-encoding every non-ASCII character and testing the
 * result as a URI, per RFC 3987 §3.1.
 * @example "http://ƒøø.ßår/?∂éœ=πîx#πîüx"
 */
export const iri: Schema<string, string>;

/**
 * IRI reference — {@link uriReference} with non-ASCII characters allowed
 * unescaped. The same permissiveness caveats apply.
 * @example "/âππ"
 */
export const iriReference: Schema<string, string>;

/**
 * Internationalized email address, RFC 6531 — a Unicode local part and domain
 * are both allowed, including a quoted local part, though only one without
 * whitespace: `"john doe"@example.com` is rejected.
 *
 * Shape only, and much looser than {@link email}: RFC 6531 constrains little
 * beyond the length limits, so `a@b` and `a@localhost` are valid.
 * @example "실례@실례.테스트"
 */
export const idnEmail: Schema<string, string>;

/**
 * RFC 6901 JSON Pointer, as used by JSON Patch `path` and JSON Schema `$ref`
 * fragments. `""` is valid and addresses the whole document. `~` must be
 * escaped: `~0` is a literal `~`, `~1` is a literal `/`.
 *
 * It addresses a location, it does not make one safe to follow — `/__proto__`
 * is a well-formed pointer.
 * @example "/foo/bar~0/baz~1/%a"
 */
export const jsonPointer: Schema<string, string>;

/**
 * RFC 6901 relative JSON Pointer — a leading integer means "go up N levels".
 * A trailing `#` asks for the member name or array index rather than the value.
 * @example "2/0/baz/1/zip"
 */
export const relativeJsonPointer: Schema<string, string>;

/**
 * A `Date` instance; Invalid Date is rejected. To go from a string, convert:
 * `S.string.with(S.to, S.date)`. To validate a timestamp that stays a string,
 * use {@link isoDateTime}.
 */
export const date: Schema<Date, Date>;

/**
 * Runs `scope` and returns a {@link Result} instead of throwing. Only `S.Error`
 * is caught — anything else propagates.
 *
 * ```ts
 * const result = S.safe(() => S.parser(schema)(data));
 * ```
 *
 * The whole callback is covered, so a chain of operations can share one.
 */
export function safe<TValue>(scope: () => TValue): Result<TValue>;
/** {@link safe} for an async scope. */
export function safeAsync<TValue>(
  scope: () => Promise<TValue>
): Promise<Result<TValue>>;

/**
 * Swaps a schema's Input and Output — transformations, fields and all.
 *
 * ```ts
 * S.parser(S.reverse(userSchema))(user); // validates, then encodes
 * ```
 *
 * The result is an ordinary schema, so unlike {@link encoder} the reverse pass
 * validates too.
 */
export function reverse<TInput, TOutput>(
  schema: SchemaLike<TInput, TOutput>
): Schema<TOutput, TInput>;

/**
 * Compiles a validating parse function — `S.decoder(S.unknown, schema)` under
 * a shorter name. Compile once, call many times:
 *
 * ```ts
 * const parseUser = S.parser(userSchema);
 * parseUser(data);
 *
 * S.parser(S.jsonString, userSchema)(raw); // extra schemas make it a pipeline
 * ```
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

/** {@link parser} for a schema carrying an async step ({@link asyncDecoderAssert}). */
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
 * Compiles a conversion from Input to Output, or across a chain of schemas.
 * The type check {@link parser} performs is skipped — refinements and
 * transformations still run — so pass it data you already trust.
 *
 * ```ts
 * S.decoder(S.jsonString, userSchema)(raw);
 * S.decoder(S.string, S.date)("2024-01-01T00:00:00Z");
 * ```
 *
 * The whole chain fuses into one generated function, however many stages.
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

/** {@link decoder} for a schema carrying an async step. */
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
 * The mirror of {@link decoder}: Output back to Input.
 *
 * ```ts
 * S.encoder(userSchema)(user);
 * S.encoder(userSchema, S.jsonString)(user); // straight out to a JSON string
 * ```
 *
 * Validation is skipped here as well; `S.parser(S.reverse(schema))` is the
 * validating way back.
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

/** {@link encoder} for a schema carrying an async step. */
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
 * Throws unless the data matches, narrowing it in place. No output is built,
 * which makes it 2-3x faster than {@link parser} — reach for it when you only
 * need the verdict.
 *
 * ```ts
 * S.assert(data, S.string); // data is string from here on
 * ```
 *
 * The schema and the data go in either order.
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
 * Type guard over {@link assert}'s validate-only path, in either argument
 * order.
 *
 * ```ts
 * if (S.is(data, S.string)) data.trim();
 * ```
 *
 * A schema that can't validate at all (a rejected conversion) still throws —
 * that's a bug in the schema, not a `false`.
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
 * Fixed-length array. The definer form maps positions onto a shape, and back
 * again for free:
 *
 * ```ts
 * const athlete = S.tuple((s) => ({
 *   name: s.item(0, S.string),
 *   jerseyNumber: s.item(1, S.number),
 * }));
 * ```
 *
 * `s.tag(index, value)` pins a constant slot. Passing an array instead —
 * `S.tuple([S.string, S.number])` — is {@link schema}.
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

// `SchemaLike<TInput, TOutput> | TDef` in ONE signature: a schema matches the
// structural constituent and skips the recursive UnknownTo* machinery, only a
// raw definition falls through to TDef. Must stay one signature — `.with` infers
// through a single call signature only, so any overload pair collapses
// `schema.with(S.optional, …)` to Schema<unknown, unknown>.
/**
 * Adds `undefined` to what the schema accepts. A second argument replaces the
 * missing case after parsing — a function is re-run for every value:
 *
 * ```ts
 * S.optional(S.string, "tuna");
 * S.optional(S.number, Math.random);
 * ```
 *
 * An object field whose type admits `undefined` is what makes its key optional
 * in the inferred type.
 */
export function optional<
  const TDef = never,
  TInput = UnknownToInput<TDef>,
  TOutput = UnknownToOutput<TDef>,
  TOr extends TOutput | undefined = undefined
>(
  schema: SchemaLike<TInput, TOutput> | TDef,
  or?: (() => TOr) | TOr,
  // To make .with work
  _?: never
): Schema<
  TInput | undefined,
  TOr extends undefined ? TOutput | undefined : TOutput
>;

/**
 * {@link optional} for `null`: `S.nullable(S.string)`, or
 * `S.nullable(S.string, "fallback")` to replace the null case. Use
 * {@link nullish} to accept both `null` and `undefined`.
 */
export function nullable<
  const TDef = never,
  TInput = UnknownToInput<TDef>,
  TOutput = UnknownToOutput<TDef>,
  TOr extends TOutput | null = null
>(
  schema: SchemaLike<TInput, TOutput> | TDef,
  or?: (() => TOr) | TOr,
  // To make .with work
  _?: never
): Schema<TInput | null, TOr extends null ? TOutput | null : TOutput>;

/** Accepts `null` and `undefined`, and gives back whichever arrived. */
export const nullish: <
  const TDef = never,
  TInput = UnknownToInput<TDef>,
  TOutput = UnknownToOutput<TDef>
>(
  schema: SchemaLike<TInput, TOutput> | TDef
) => Schema<TInput | undefined | null, TOutput | undefined | null>;

export type Class<T> = new (...args: readonly any[]) => T;
/**
 * `data instanceof Class`, for a type Sury has no schema for.
 *
 * ```ts
 * S.instance(Set).with(S.minSize, 1);
 * ```
 *
 * It's also the usual base for a custom schema: add {@link to} with your own
 * decode/encode, and {@link meta} to give it a name in error messages.
 */
export const instance: <T>(class_: Class<T>) => Schema<T, T>;

/**
 * An array of a single element type. A literal length bound refines the type,
 * so indexing and destructuring stay checked:
 *
 * ```ts
 * S.array(S.number).with(S.length, 2); //? S.Schema<[number, number]>
 * ```
 */
export const array: <
  const TDef = never,
  TInput = UnknownToInput<TDef>,
  TOutput = UnknownToOutput<TDef>
>(
  schema: SchemaLike<TInput, TOutput> | TDef
) => Schema<TInput[], TOutput[]>;

/**
 * Columnar form of an array of rows — one array per field, in field order.
 *
 * ```ts
 * const schema = S.compactColumns(S.json).with(S.to, S.array(rowSchema));
 *
 * S.encoder(schema)([{ id: "0", ok: false }, { id: "1", ok: true }]);
 * // [["0", "1"], [false, true]]
 * ```
 *
 * Worth it for bulk payloads — a `UNNEST`-style INSERT, a wire format where
 * repeating every key per row costs more than the rows themselves.
 */
export const compactColumns: <
  const TDef = never,
  TInput = UnknownToInput<TDef>,
  TOutput = UnknownToOutput<TDef>
>(
  schema: SchemaLike<TInput, TOutput> | TDef
) => Schema<TInput[][], TOutput[][]>;

/** `{ [k: string]: T }` — every value checked, no key is required or forbidden. */
export const record: <
  const TDef = never,
  TInput = UnknownToInput<TDef>,
  TOutput = UnknownToOutput<TDef>
>(
  schema: SchemaLike<TInput, TOutput> | TDef
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
 * An object schema. Pass a plain definition — that's {@link schema} — or a
 * definer, which renames and restructures fields at no runtime cost and gives
 * you the encode direction with it:
 *
 * ```ts
 * const userSchema = S.object((s) => ({
 *   id: s.field("USER_ID", S.number),
 *   name: s.fieldOr("USER_NAME", S.string, "anonymous"),
 * }));
 *
 * S.parser(userSchema)({ USER_ID: 1, USER_NAME: "John" }); // { id: 1, name: "John" }
 * S.encoder(userSchema)({ id: 1, name: "John" });          // { USER_ID: 1, USER_NAME: "John" }
 * ```
 *
 * `s.tag(name, value)` pins a constant field, `s.nested(name)` gives the same
 * context one level down, and `s.flatten(schema)` inlines another object
 * schema's fields into this one.
 *
 * Undeclared keys are dropped; {@link strict} rejects them instead.
 */
export function object<TInput extends Record<string, unknown>, TOutput>(
  definer: (ctx: ObjectCtx<TInput>) => TOutput
): Schema<TInput, TOutput>;
export function object<T extends Record<string, unknown>>(
  definition: T
): Schema<UnknownToInput<T>, UnknownToOutput<T>>;

/** Back to the default: undeclared keys are dropped. Undoes {@link strict}. */
export function strip<TInput extends Record<string, unknown>, TOutput>(
  schema: SchemaLike<TInput, TOutput>
): Schema<TInput, TOutput>;
/** {@link strip} applied to nested object schemas too. */
export function deepStrip<TInput extends Record<string, unknown>, TOutput>(
  schema: SchemaLike<TInput, TOutput>
): Schema<TInput, TOutput>;
/**
 * Fails on a key the schema doesn't declare, instead of dropping it. Applies
 * to this object only — {@link deepStrict} covers the nested ones, and
 * `S.global({ defaultAdditionalItems: "strict" })` the whole app.
 */
export function strict<TInput extends Record<string, unknown>, TOutput>(
  schema: SchemaLike<TInput, TOutput>
): Schema<TInput, TOutput>;
/** {@link strict} applied to nested object schemas too. */
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
 * One object schema with both sets of fields. Throws on a shared key, and
 * keeps the *first* schema's strip/strict policy.
 *
 * ```ts
 * S.merge(S.schema({ students: S.array(S.string) }), S.schema({ id: S.string }));
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
 * A schema that refers to itself. TypeScript can't infer through the cycle,
 * so state the type:
 *
 * ```ts
 * type Node = { id: string; children: Node[] };
 *
 * const nodeSchema = S.recursive<Node>("Node", (node) =>
 *   S.schema({ id: S.string, children: S.array(node) })
 * );
 * ```
 *
 * Pass both parameters when the schema transforms — `S.recursive<unknown, Row>`.
 * `identifier` names it in errors and in the emitted `$defs`. Cyclical *data*
 * still loops forever.
 */
export function recursive<TInput = unknown, TOutput = TInput>(
  identifier: string,
  definer: (schema: Schema<TInput, TOutput>) => Schema<TInput, TOutput>
): Schema<TInput, TOutput>;

/**
 * Replaces the message of a failed check, keyed by the constraint that failed.
 * `{}` clears every override.
 *
 * ```ts
 * S.email.with(S.meta, { errorMessage: { format: "Must be a valid email" } });
 * ```
 */
export type SchemaErrorMessage = {
  /** Catch-all override, used when no more specific key below matches the failing check. */
  _?: string;
  format?: string;
  type?: string;
  minimum?: string;
  maximum?: string;
  exclusiveMinimum?: string;
  exclusiveMaximum?: string;
  multipleOf?: string;
  minLength?: string;
  maxLength?: string;
  minItems?: string;
  maxItems?: string;
  minSize?: string;
  maxSize?: string;
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
 * Documents a schema. It all lands in {@link toJSONSchema} output, and `name`
 * is what error messages call the schema.
 *
 * ```ts
 * S.string.with(S.meta, {
 *   description: "A useful bit of text",
 *   examples: ["hello"],
 * });
 * ```
 *
 * Metadata is per-schema, not inherited, so attach it where the constraint is.
 */
export function meta<TInput, TOutput>(
  schema: SchemaLike<TInput, TOutput>,
  meta: Meta<TOutput>
): Schema<TInput, TOutput>;

/**
 * The schema's input type, as the expression error messages use — `"{ abc: 123; }"`,
 * or its {@link meta} `name` when it has one. Handy for naming a custom schema
 * after its argument. The exact format may change between releases.
 */
export function inputExpression(schema: SchemaLike<unknown, unknown>): string;
/** {@link inputExpression} for the output side. */
export function outputExpression(schema: SchemaLike<unknown, unknown>): string;
/**
 * Drops this schema's own type check — its fields and items are still checked.
 * Worth it for data you constructed yourself and are only reshaping.
 */
export function noValidation<TInput, TOutput>(
  schema: SchemaLike<TInput, TOutput>,
  value: boolean
): Schema<TInput, TOutput>;

/**
 * An async check, run on decode only. Throw inside it to fail.
 *
 * ```ts
 * S.uuid.with(S.asyncDecoderAssert, async (id) => {
 *   if (!(await isActive(id))) throw new Error(`The user ${id} is inactive.`);
 * });
 * ```
 *
 * The schema becomes async, so parse it with {@link asyncParser}.
 */
export function asyncDecoderAssert<TInput, TOutput>(
  schema: SchemaLike<TInput, TOutput>,
  assertFn: (value: TOutput) => Promise<void>
): Schema<TInput, TOutput>;

/**
 * A check of your own, run on decode and encode alike. Return `false` to fail.
 *
 * ```ts
 * S.schema({ password: S.string, confirm: S.string }).with(
 *   S.refine,
 *   (v) => v.password === v.confirm,
 *   { error: "Passwords don't match", path: ["confirm"] }
 * );
 * ```
 *
 * `path` attaches the failure to a field rather than the whole object. A
 * refinement is opaque to {@link toJSONSchema}, so prefer a built-in
 * constraint ({@link pattern}, {@link minLength}, …) when one says the same
 * thing.
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
 * Exclusive lower bound, on any numeric schema — {@link number},
 * {@link integer}, {@link int32}, {@link port}, `S.bigint`.
 *
 * ```ts
 * S.number.with(S.gt, 0);
 * S.number.with(S.lte, 5, "this👏is👏too👏big");
 * ```
 *
 * Bounds are compared against each other and against the format's own range,
 * so a contradiction (`S.int32.with(S.gte, 3e9)`) throws where it's written
 * instead of building a schema nothing satisfies.
 */
export const gt: <TInput, TOutput extends number | bigint>(
  schema: SchemaLike<TInput, TOutput>,
  value: TOutput,
  message?: string
) => Schema<TInput, TOutput>;
/** Inclusive lower bound. See {@link gt}. */
export const gte: <TInput, TOutput extends number | bigint>(
  schema: SchemaLike<TInput, TOutput>,
  value: TOutput,
  message?: string
) => Schema<TInput, TOutput>;
/** Exclusive upper bound. See {@link gt}. */
export const lt: <TInput, TOutput extends number | bigint>(
  schema: SchemaLike<TInput, TOutput>,
  value: TOutput,
  message?: string
) => Schema<TInput, TOutput>;
/** Inclusive upper bound. See {@link gt}. */
export const lte: <TInput, TOutput extends number | bigint>(
  schema: SchemaLike<TInput, TOutput>,
  value: TOutput,
  message?: string
) => Schema<TInput, TOutput>;
/** Divisible by `value`. See {@link gt} for the schemas it applies to. */
export const multipleOf: <TInput, TOutput extends number | bigint>(
  schema: SchemaLike<TInput, TOutput>,
  value: TOutput,
  message?: string
) => Schema<TInput, TOutput>;

// A literal bound is arity, so the refined type says so; a `number`-typed
// bound narrows nothing. A bound may retype the input side only when the input
// is the same value as the bounded output — a codec's input is a different
// value and its length says nothing about it.
//
// `Tail` follows the N fixed elements: empty for an exact bound, `E[]` for a
// lower one. The 64 cap bails to `E[]` — past it TypeScript's recursion limit
// is nearer than the worth of a spelled-out tuple, and a fractional or huge
// bound would compile-error instead of failing at runtime as it already does.
type Repeat<E, N extends number, Acc extends unknown[], Tail extends unknown[]> =
  Acc["length"] extends N
    ? [...Acc, ...Tail]
    : Acc["length"] extends 64
    ? E[]
    : Repeat<E, N, [...Acc, E], Tail>;
// `N extends N` distributes; without it a union bound like `0 | 2` matches one
// branch and pins the type to it. The `number extends T["length"]` guard keeps
// a bound off an existing tuple, where `Repeat` would rebuild `["bar", number]`
// as `[number | "bar", number | "bar"]`.
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
// Kept separate from `Sized` deliberately: collapsing both into one
// `Bounded<T, N, Exact>` instantiates the discrimination at every use and
// regressed every spec that touches a bound.
//
// No string case: TypeScript can't say "at least N characters" — each segment
// of `${string}${string}` matches `""`, so it collapses to `string`. Only the
// exact bound reaches a string type, at `""`.
type AtLeast<T, N extends number> = number extends N
  ? T
  : N extends N
  ? T extends (infer E)[]
    ? number extends T["length"]
      ? Repeat<E, N, [], E[]>
      : T
    : T
  : never;
// `AtLeast<T, 1>` minus the guard on a bound that can't vary.
type NonEmptied<T> = T extends (infer E)[]
  ? number extends T["length"]
    ? [E, ...E[]]
    : T
  : T;
// Mutual assignability, not one-way: an input that is a strict subtype of the
// output keeps its own type, or `S.to(S.literal("x"), S.string)` under a bound
// would retype its input to a value that schema rejects. The brackets stop a
// union input from distributing and passing on one member.
type Same<T, U> = [T] extends [U] ? ([U] extends [T] ? true : false) : false;

/**
 * Lower bound on a string's or array's length. A literal bound shows up in the
 * inferred type:
 *
 * ```ts
 * S.array(S.string).with(S.minLength, 2); //? S.Schema<[string, string, ...string[]]>
 * ```
 *
 * For a `Blob`, a `File` or a `Set`, bound `.size` instead — {@link minSize}.
 */
export const minLength: <TInput, TOutput extends string | unknown[], N extends number>(
  schema: SchemaLike<TInput, TOutput>,
  length: N,
  message?: string
) => Schema<Same<TInput, TOutput> extends true ? AtLeast<TInput, N> : TInput, AtLeast<TOutput, N>>;
/** Upper bound on length. Nothing an upper bound implies is expressible in the type, so the type is unchanged. */
export const maxLength: <TInput, TOutput extends string | unknown[]>(
  schema: SchemaLike<TInput, TOutput>,
  length: number,
  message?: string
) => Schema<TInput, TOutput>;
/**
 * Exact length. On an array with a literal bound the result is a tuple, so
 * destructuring is checked:
 *
 * ```ts
 * const [lat, lng] = S.parser(S.array(S.number).with(S.length, 2))(input);
 * ```
 */
export const length: <TInput, TOutput extends string | unknown[], N extends number>(
  schema: SchemaLike<TInput, TOutput>,
  length: N,
  message?: string
) => Schema<Same<TInput, TOutput> extends true ? Sized<TInput, N> : TInput, Sized<TOutput, N>>;
/** `length >= 1`, typed `[T, ...T[]]` for an array. */
export const nonEmpty: <TInput, TOutput extends string | unknown[]>(
  schema: SchemaLike<TInput, TOutput>,
  message?: string
) => Schema<Same<TInput, TOutput> extends true ? NonEmptied<TInput> : TInput, NonEmptied<TOutput>>;

/**
 * Lower bound on `.size` — bytes for {@link blob} and {@link file}, entries
 * for something like `S.instance(Set)`. A bound of `0` is dropped, a negative
 * one is an error.
 */
export const minSize: <TInput, TOutput extends { size: number }>(
  schema: SchemaLike<TInput, TOutput>,
  size: number,
  message?: string
) => Schema<TInput, TOutput>;
/** Upper bound on `.size`. See {@link minSize}. */
export const maxSize: <TInput, TOutput extends { size: number }>(
  schema: SchemaLike<TInput, TOutput>,
  size: number,
  message?: string
) => Schema<TInput, TOutput>;
/** Exact `.size`. See {@link minSize}. */
export const size: <TInput, TOutput extends { size: number }>(
  schema: SchemaLike<TInput, TOutput>,
  size: number,
  message?: string
) => Schema<TInput, TOutput>;

/**
 * The string must match `re`. It rides along into the emitted JSON Schema, so
 * narrowing a format stays honest about what it accepts:
 *
 * ```ts
 * S.uri.with(S.pattern, /^https:\/\//);
 * // { type: "string", format: "uri", pattern: "^https:\\/\\/" }
 * ```
 */
export const pattern: <TInput>(
  schema: SchemaLike<TInput, string>,
  re: RegExp,
  message?: string
) => Schema<TInput, string>;
/** Trims surrounding whitespace — a transformation, not a check, and applied in both directions. */
export const trim: <TInput>(
  schema: SchemaLike<TInput, string>
) => Schema<TInput, string>;

/** What an object or tuple does with members it doesn't declare. */
export type AdditionalItemsMode = "strip" | "strict";

export type GlobalConfigOverride = {
  /** `"strict"` makes every object schema reject undeclared keys. Default `"strip"`. */
  defaultAdditionalItems?: AdditionalItemsMode;
  /** Skips the `NaN` check in every number schema — ~10% faster when your numbers can't be `NaN`. */
  disableNanNumberValidation?: boolean;
};

/**
 * Sets library-wide defaults. Call it once at startup, before the schemas
 * that should follow them are built.
 *
 * ```ts
 * S.global({ defaultAdditionalItems: "strict" });
 * ```
 */
export function global(globalConfigOverride: GlobalConfigOverride): void;

/**
 * Restructures a value, deriving the way back from the shape you return:
 *
 * ```ts
 * const circle = S.number.with(S.shape, (radius) => ({ kind: "circle", radius }));
 *
 * S.parser(circle)(1);                              // { kind: "circle", radius: 1 }
 * S.encoder(circle)({ kind: "circle", radius: 1 }); // 1
 * ```
 *
 * The argument is a proxy standing in for the value, not the value — branching
 * on it or calling its methods won't do what it looks like. Use {@link to}
 * with a decode function when you need real logic.
 */
export function shape<TShape = unknown, TInput = unknown, TOutput = unknown>(
  schema: SchemaLike<TInput, TOutput>,
  shaper: (value: TOutput) => TShape
): Schema<TInput, TShape>;

/**
 * Converts to another type, in both directions:
 *
 * ```ts
 * const schema = S.string.with(S.to, S.number);
 *
 * S.parser(schema)("123"); // 123
 * S.encoder(schema)(123);  // "123"
 * ```
 *
 * Any schema is a valid target — `S.json`, `S.jsonString`, `S.date`,
 * `S.uint8Array`, a union, an object schema — and nested conversions fold into
 * the same generated function, so a deep pipeline costs no more than a shallow
 * one. A conversion Sury can't derive, or one with more than one reasonable
 * meaning, is rejected at the `S.parser` / `S.encoder` call with the rewrite in
 * the message.
 *
 * Pass `decode` / `encode` only for logic of your own; prefer the built-in
 * conversion where there is one.
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

// The dialect the `target` option selects decides the shape of the result, so
// each one gets its own overload. Falling back to the widest type for a
// non-literal target is what keeps a caller holding `target` in a variable
// compiling.
/**
 * ```ts
 * S.toJSONSchema(schema);                              // draft-07
 * S.toJSONSchema(schema, { target: "draft-2020-12" });
 * ```
 *
 * Keywords and examples describe the **Input** side — pass
 * `S.reverse(schema)` for the output side. The target picks the result type,
 * so `prefixItems` is there to reach for on a 2020-12 result and `nullable` on
 * an OpenAPI one.
 */
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
 * A document written inline is validated and typed, following a `$ref` into the
 * same document — recursive ones included. A `$ref` leading outside it (a URL,
 * a `urn:`, an `$anchor`, a `$id` base) throws, so bundle first. To also have
 * TypeScript check the document itself, annotate it:
 * `{ ... } satisfies S.JSONSchema` — the annotation widens literals (e.g.
 * `required`, `enum`), so the inferred type gets wider too.
 *
 * A schema read from a file or an API needs no cast — a non-literal argument
 * (`unknown`, `S.JSON`, a dialect type) falls back to `Schema<JSON, JSON>`.
 * Use `S.to` to refine it further.
 */
export function fromJSONSchema<
  const T extends { type: "string" | "number" | "integer" | "boolean" | "null" },
>(
  jsonSchema: T
): Schema<FromJSONSchema<T>>;
export function fromJSONSchema<const T = unknown>(
  jsonSchema: T
): Schema<FromJSONSchema<T>, FromJSONSchemaOutput<T>>;
/**
 * Merges extra keywords into what {@link toJSONSchema} emits for this schema —
 * vendor extensions, a `$id`, a hand-written `description`. Validation is
 * untouched, so keep the two in agreement yourself.
 */
export function extendJSONSchema<TInput, TOutput>(
  schema: SchemaLike<TInput, TOutput>,
  jsonSchema: JSONSchema
): Schema<TInput, TOutput>;
/** Enables `~standard.jsonSchema`; its input/output throw before this is called. */
export function enableStandardJSONSchema(): void;
