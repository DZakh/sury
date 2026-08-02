// Mirrors of the Standard Schema specs (https://standardschema.dev). These are
// external contracts, not Sury's own API — they change only when the spec does,
// which is why they live apart from S.d.ts. Re-exported from S.d.ts, so every
// name here is public.

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
