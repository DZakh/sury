// The file was initially hand written to support namespaces
// and to reuse code between TS API.
// Probably can be changed to generated, but kept handwritten for now

/* eslint-disable */
/* tslint:disable */

/** The Standard Schema interface. */
export interface StandardSchemaV1<Input = unknown, Output = Input> {
  /** The Standard Schema properties. */
  readonly "~standard": StandardSchemaV1.Props<Input, Output>;
}

export declare namespace StandardSchemaV1 {
  /** The Standard Schema properties interface. */
  export interface Props<Input = unknown, Output = Input> {
    /** The version number of the standard. */
    readonly version: 1;
    /** The vendor name of the schema library. */
    readonly vendor: string;
    /** Validates unknown input values. */
    readonly validate: (
      value: unknown
    ) => Result<Output> | Promise<Result<Output>>;
    /** Inferred types associated with the schema. */
    readonly types?: Types<Input, Output> | undefined;
  }

  /** The result interface of the validate function. */
  export type Result<Output> = SuccessResult<Output> | FailureResult;

  /** The result interface if validation succeeds. */
  export interface SuccessResult<Output> {
    /** The typed output value. */
    readonly value: Output;
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
  export interface Types<Input = unknown, Output = Input> {
    /** The input type of the schema. */
    readonly input: Input;
    /** The output type of the schema. */
    readonly output: Output;
  }

  /** Infers the input type of a Standard Schema. */
  export type InferInput<Schema extends StandardSchemaV1> = NonNullable<
    Schema["~standard"]["types"]
  >["input"];

  /** Infers the output type of a Standard Schema. */
  export type InferOutput<Schema extends StandardSchemaV1> = NonNullable<
    Schema["~standard"]["types"]
  >["output"];
}

export type EffectCtx<Output, Input> = {
  schema: t<Output, Input>;
  fail: (message: string) => never;
};

export type Result<Value> =
  | {
      success: true;
      value: Value;
    }
  | { success: false; error: error };

export type Json =
  | string
  | boolean
  | number
  | null
  | { [key: string]: Json }
  | Json[];

declare const øbrand: unique symbol;

export type t<Output, Input = unknown> = {
  with<Transformed>(
    transform: (
      schema: t<unknown, unknown>,
      parser:
        | ((value: unknown, s: EffectCtx<unknown, unknown>) => unknown)
        | undefined,
      serializer?: (value: unknown, s: EffectCtx<unknown, unknown>) => Input
    ) => t<unknown, unknown>,
    parser:
      | ((value: Output, s: EffectCtx<unknown, unknown>) => Transformed)
      | undefined,
    serializer?: (value: Transformed, s: EffectCtx<unknown, unknown>) => Input
  ): t<Transformed, Input>;
  with(
    refine: (
      schema: t<unknown, unknown>,
      refiner: (value: unknown, s: EffectCtx<unknown, unknown>) => Promise<void>
    ) => t<unknown, unknown>,
    refiner: (value: Output, s: EffectCtx<Output, Input>) => Promise<void>
  ): t<Output, Input>;
  // with(message: string): t<Output, Input>; TODO: implement
  with<O, I>(fn: (schema: t<Output, Input>) => t<O, I>): t<O, I>;
  with<O, I, A1>(
    fn: (schema: t<Output, Input>, arg1: A1) => t<O, I>,
    arg1: A1
  ): t<O, I>;
  with<O, I, A1, A2>(
    fn: (schema: t<Output, Input>, arg1: A1, arg2: A2) => t<O, I>,
    arg1: A1,
    arg2: A2
  ): t<O, I>;

  readonly name?: string;
  readonly description?: string;
  readonly deprecated?: boolean;

  readonly ["~standard"]: StandardSchemaV1.Props<Input, Output>;
  readonly [øbrand]: unknown;
} & (
  | {
      type: "never";
    }
  | {
      type: "unknown";
    }
  | {
      type: "string";
      const?: string;
    }
  | {
      type: "number";
      format?: "int32";
      const?: number;
    }
  | {
      type: "bigint";
      const?: bigint;
    }
  | {
      type: "boolean";
      const?: boolean;
    }
  | {
      type: "symbol";
      const?: symbol;
    }
  | {
      type: "null";
      const: null;
    }
  | {
      type: "undefined";
      const: undefined;
    }
  | {
      type: "nan";
      const: number;
    }
  | {
      type: "function";
      const?: unknown;
    }
  | {
      type: "instance";
      const?: unknown;
    }
  | {
      type: "array";
      items: item[];
      fields: Record<string, item>;
      additionalItems: "strip" | "strict" | t<unknown>;
      unnest?: true;
    }
  | {
      type: "object";
      items: item[];
      fields: Record<string, item>;
      additionalItems: "strip" | "strict" | t<unknown>;
    }
  | {
      type: "union";
      anyOf: t<unknown>[];
      has: Record<
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
);
/* simulate opaque types */

export type item = {
  schema: t<unknown>;
  location: string;
  inlinedLocation: string;
};

export abstract class Path_t {
  protected opaque: any;
} /* simulate opaque types */

export class error {
  readonly flag: number;
  readonly code: errorCode;
  readonly path: Path_t;
  readonly message: string;
  readonly reason: string;
}

export abstract class errorCode {
  protected opaque: any;
} /* simulate opaque types */
