import { Json, Result, t, EffectCtx } from "./S.gen";
export { Json, Result, EffectCtx, error as Error } from "./S.gen";

export type Schema<Output, Input = unknown> = t<Output, Input>;

export type Output<T> = T extends Schema<infer Output, unknown>
  ? Output
  : never;
export type Input<T> = T extends Schema<unknown, infer Input> ? Input : never;

type UnknownToOutput<T> = T extends Schema<unknown>
  ? Output<T>
  : T extends unknown[]
  ? { [K in keyof T]: UnknownToOutput<T[K]> }
  : T extends { [k in keyof T]: unknown }
  ? Flatten<
      {
        [k in keyof T as HasUndefined<UnknownToOutput<T[k]>> extends true
          ? k
          : never]?: UnknownToOutput<T[k]>;
      } & {
        [k in keyof T as HasUndefined<UnknownToOutput<T[k]>> extends true
          ? never
          : k]: UnknownToOutput<T[k]>;
      }
    >
  : T;

type UnknownToInput<T> = T extends Schema<unknown>
  ? Input<T>
  : T extends unknown[]
  ? { [K in keyof T]: UnknownToInput<T[K]> }
  : T extends { [k in keyof T]: unknown }
  ? Flatten<
      {
        [k in keyof T as HasUndefined<UnknownToInput<T[k]>> extends true
          ? k
          : never]?: UnknownToInput<T[k]>;
      } & {
        [k in keyof T as HasUndefined<UnknownToInput<T[k]>> extends true
          ? never
          : k]: UnknownToInput<T[k]>;
      }
    >
  : T;

// Grok told that it makes things faster
// TODO: Verify it with ArkType test framework
type HasUndefined<T> = [T] extends [undefined]
  ? true
  : undefined extends T
  ? true
  : false;

// Utility to flatten the type into a single object
type Flatten<T> = T extends object
  ? { [K in keyof T as T[K] extends never ? never : K]: T[K] }
  : T;

type UnknownArrayToOutput<
  T extends unknown[],
  Length extends number = T["length"]
> = Length extends Length
  ? number extends Length
    ? T
    : _RestToOutput<T, Length, []>
  : never;
type _RestToOutput<
  T extends unknown[],
  Length extends number,
  Accumulated extends unknown[],
  Index extends number = Accumulated["length"]
> = Index extends Length
  ? Accumulated
  : _RestToOutput<T, Length, [...Accumulated, UnknownToOutput<T[Index]>]>;
type UnknownArrayToInput<
  T extends unknown[],
  Length extends number = T["length"]
> = Length extends Length
  ? number extends Length
    ? T
    : _RestToInput<T, Length, []>
  : never;
type _RestToInput<
  T extends unknown[],
  Length extends number,
  Accumulated extends unknown[],
  Index extends number = Accumulated["length"]
> = Index extends Length
  ? Accumulated
  : _RestToInput<T, Length, [...Accumulated, UnknownToInput<T[Index]>]>;

type Literal =
  | string
  | number
  | boolean
  | symbol
  | bigint
  | undefined
  | null
  | []
  | Schema<unknown>;

export function schema<T extends Literal>(
  value: T
): Schema<UnknownToOutput<T>, UnknownToInput<T>>;
export function schema<T extends Literal[]>(
  schemas: [...T]
): Schema<[...UnknownArrayToOutput<T>], [...UnknownArrayToInput<T>]>;
export function schema<T extends unknown[]>(
  schemas: [...T]
): Schema<[...UnknownArrayToOutput<T>], [...UnknownArrayToInput<T>]>;
export function schema<T>(
  value: T
): Schema<UnknownToOutput<T>, UnknownToInput<T>>;

export function union<A extends Literal, B extends Literal[]>(
  schemas: [A, ...B]
): Schema<
  UnknownToOutput<A> | UnknownArrayToOutput<B>[number],
  UnknownToInput<A> | UnknownArrayToInput<B>[number]
>;
export function union<A, B extends unknown[]>(
  schemas: [A, ...B]
): Schema<
  UnknownToOutput<A> | UnknownArrayToOutput<B>[number],
  UnknownToInput<A> | UnknownArrayToInput<B>[number]
>;

export const string: Schema<string, string>;
export const boolean: Schema<boolean, boolean>;
export const int32: Schema<number, number>;
export const number: Schema<number, number>;
export const bigint: Schema<bigint, bigint>;
export const never: Schema<never, never>;
export const unknown: Schema<unknown, unknown>;
export const json: (validate: boolean) => Schema<Json, Json>;

export function safe<Value>(scope: () => Value): Result<Value>;
export function safeAsync<Value>(
  scope: () => Promise<Value>
): Promise<Result<Value>>;

export function reverse<Output, Input>(
  schema: Schema<Output, Input>
): Schema<Input, Output>;

export function parseOrThrow<Output, Input>(
  data: unknown,
  schema: Schema<Output, Input>
): Output;
export function parseJsonOrThrow<Output, Input>(
  json: Json,
  schema: Schema<Output, Input>
): Output;
export function parseJsonStringOrThrow<Output, Input>(
  jsonString: string,
  schema: Schema<Output, Input>
): Output;
export function parseAsyncOrThrow<Output, Input>(
  data: unknown,
  schema: Schema<Output, Input>
): Promise<Output>;

export function convertOrThrow<Output, Input>(
  data: Input,
  schema: Schema<Output, Input>
): Output;
export function convertToJsonOrThrow<Output, Input>(
  data: Input,
  schema: Schema<Output, Input>
): Json;
export function convertToJsonStringOrThrow<Output, Input>(
  data: Input,
  schema: Schema<Output, Input>
): string;

export function reverseConvertOrThrow<Output, Input>(
  value: Output,
  schema: Schema<Output, Input>
): Input;
export function reverseConvertToJsonOrThrow<Output, Input>(
  value: Output,
  schema: Schema<Output, Input>
): Json;
export function reverseConvertToJsonStringOrThrow<Output, Input>(
  value: Output,
  schema: Schema<Output, Input>
): string;

export function assertOrThrow<Output, Input>(
  data: unknown,
  schema: Schema<Output, Input>
): asserts data is Input;

export function tuple<Output, Input extends unknown[]>(
  definer: (s: {
    item: <ItemOutput>(
      inputIndex: number,
      schema: Schema<ItemOutput, unknown>
    ) => ItemOutput;
    tag: (inputIndex: number, value: unknown) => void;
  }) => Output
): Schema<Output, Input>;

export function optional<Output, Input, Or = undefined>(
  schema: Schema<Output, Input>,
  or?: (() => Or) | Or,
  // To make .with work
  _?: never
): Schema<
  Or extends undefined ? Output | undefined : Output | Or,
  Input | undefined
>;

export function nullable<Output, Input, Or = undefined>(
  schema: Schema<Output, Input>,
  or?: (() => Or) | Or,
  // To make .with work
  _?: never
): Schema<
  Or extends undefined ? Output | undefined : Output | Or,
  Input | null
>;

export const nullish: <Output, Input>(
  schema: Schema<Output, Input>
) => Schema<Output | undefined | null, Input | undefined | null>;

export const array: <Output, Input>(
  schema: Schema<Output, Input>
) => Schema<Output[], Input[]>;

export const unnest: <Output, Input extends Record<string, unknown>>(
  schema: Schema<Output, Input>
) => Schema<
  Output[],
  {
    [K in keyof Input]: Input[K][];
  }[keyof Input][]
>;

export const record: <Output, Input>(
  schema: Schema<Output, Input>
) => Schema<Record<string, Output>, Record<string, Input>>;

export const jsonString: <Output>(
  schema: Schema<Output, unknown>,
  space?: number
) => Schema<Output, string>;

type ObjectCtx<Input extends Record<string, unknown>> = {
  field: <FieldOutput>(
    name: string,
    schema: Schema<FieldOutput, unknown>
  ) => FieldOutput;
  fieldOr: <FieldOutput>(
    name: string,
    schema: Schema<FieldOutput, unknown>,
    or: FieldOutput
  ) => FieldOutput;
  tag: <TagName extends keyof Input>(
    name: TagName,
    value: Input[TagName]
  ) => void;
  flatten: <FieldOutput>(schema: Schema<FieldOutput, unknown>) => FieldOutput;
  nested: (name: string) => ObjectCtx<Record<string, unknown>>;
};

export function object<Output, Input extends Record<string, unknown>>(
  definer: (ctx: ObjectCtx<Input>) => Output
): Schema<Output, Input>;

export function strip<Output, Input extends Record<string, unknown>>(
  schema: Schema<Output, Input>
): Schema<Output, Input>;
export function deepStrip<Output, Input extends Record<string, unknown>>(
  schema: Schema<Output, Input>
): Schema<Output, Input>;
export function strict<Output, Input extends Record<string, unknown>>(
  schema: Schema<Output, Input>
): Schema<Output, Input>;
export function deepStrict<Output, Input extends Record<string, unknown>>(
  schema: Schema<Output, Input>
): Schema<Output, Input>;

export function merge<O1, O2>(
  schema1: Schema<O1, Record<string, unknown>>,
  schema2: Schema<O2, Record<string, unknown>>
): Schema<
  {
    [K in keyof O1 | keyof O2]: K extends keyof O2
      ? O2[K]
      : K extends keyof O1
      ? O1[K]
      : never;
  },
  Record<string, unknown>
>;

export function custom<Output, Input = unknown>(
  name: string,
  parser: (data: unknown, s: EffectCtx<unknown, unknown>) => Output
): Schema<Output, Input>;
export function custom<Output, Input = unknown>(
  name: string,
  parser: (data: unknown, s: EffectCtx<unknown, unknown>) => Output | undefined,
  serializer: (value: Output, s: EffectCtx<unknown, unknown>) => Input
): Schema<Output, Input>;

export function recursive<Output, Input = Output>(
  definer: (schema: Schema<Output, Input>) => Schema<Output, Input>
): Schema<Output, Input>;

export type Meta = {
  name?: string;
  description?: string;
  deprecated?: boolean;
};

export function meta<Output, Input>(
  schema: Schema<Output, Input>,
  meta: Meta
): Schema<Output, Input>;

export function toExpression(schema: Schema<unknown>): string;
export function noValidation<Output, Input>(
  schema: Schema<Output, Input>,
  value: boolean
): Schema<Output, Input>;

export function asyncParserRefine<Output, Input>(
  schema: Schema<Output, Input>,
  refiner: (value: Output, s: EffectCtx<Output, Input>) => Promise<void>
): Schema<Output, Input>;

export function refine<Output, Input>(
  schema: Schema<Output, Input>,
  refiner: (value: Output, s: EffectCtx<Output, Input>) => void
): Schema<Output, Input>;

export function transform<Transformed, Output = unknown, Input = unknown>(
  schema: Schema<Output, Input>,
  parser:
    | ((value: Output, s: EffectCtx<unknown, unknown>) => Transformed)
    | undefined,
  serializer?: (value: Transformed, s: EffectCtx<unknown, unknown>) => Output
): Schema<Transformed, Input>;

export const min: <Output extends string | number | unknown[], Input>(
  schema: Schema<Output, Input>,
  length: number,
  message?: string
) => Schema<Output, Input>;
export const max: <Output extends string | number | unknown[], Input>(
  schema: Schema<Output, Input>,
  length: number,
  message?: string
) => Schema<Output, Input>;
export const length: <Output extends string | unknown[], Input>(
  schema: Schema<Output, Input>,
  length: number,
  message?: string
) => Schema<Output, Input>;

export const port: <Input>(
  schema: Schema<number, Input>,
  message?: string
) => Schema<number, Input>;

export const email: <Input>(
  schema: Schema<string, Input>,
  message?: string
) => Schema<string, Input>;
export const uuid: <Input>(
  schema: Schema<string, Input>,
  message?: string
) => Schema<string, Input>;
export const cuid: <Input>(
  schema: Schema<string, Input>,
  message?: string
) => Schema<string, Input>;
export const url: <Input>(
  schema: Schema<string, Input>,
  message?: string
) => Schema<string, Input>;
export const pattern: <Input>(
  schema: Schema<string, Input>,
  re: RegExp,
  message?: string
) => Schema<string, Input>;
export const datetime: <Input>(
  schema: Schema<string, Input>,
  message?: string
) => Schema<Date, Input>;
export const trim: <Input>(
  schema: Schema<string, Input>
) => Schema<string, Input>;

export type UnknownKeys = "strip" | "strict";

export type GlobalConfigOverride = {
  defaultAdditionalItems?: UnknownKeys;
  disableNanNumberValidation?: boolean;
};

export function setGlobalConfig(
  globalConfigOverride: GlobalConfigOverride
): void;

type CompileInputMappings<Input, Output> = {
  Input: Input;
  Output: Output;
  Any: unknown;
  Json: Json;
  JsonString: string;
};

type CompileOutputMappings<Input, Output> = {
  Output: Output;
  Input: Input;
  Assert: void;
  Json: Json;
  JsonString: string;
};

export type CompileInputOption = keyof CompileInputMappings<unknown, unknown>;
export type CompileOutputOption = keyof CompileOutputMappings<unknown, unknown>;
export type CompileModeOption = "Sync" | "Async";

export function compile<
  Output,
  Input,
  InputOption extends CompileInputOption,
  OutputOption extends CompileOutputOption,
  ModeOption extends CompileModeOption
>(
  schema: Schema<Output, Input>,
  input: InputOption,
  output: OutputOption,
  mode: ModeOption,
  typeValidation?: boolean
): (
  input: CompileInputMappings<Input, Output>[InputOption]
) => ModeOption extends "Sync"
  ? CompileOutputMappings<Input, Output>[OutputOption]
  : ModeOption extends "Async"
  ? Promise<CompileOutputMappings<Input, Output>[OutputOption]>
  : never;

export function shape<Output, Input, Shape>(
  schema: Schema<Output, Input>,
  shaper: (value: Output) => Shape
): Schema<Shape, Input>;

export function to<
  FromInput,
  ToOutput,
  FromOutput = FromInput,
  ToInput = ToOutput
>(
  from: Schema<FromOutput, FromInput>,
  to: Schema<ToOutput, ToInput>
): Schema<ToOutput, FromInput>;

export function toJSONSchema<Output, Input>(
  schema: Schema<Output, Input>
): JSONSchema7;
export function extendJSONSchema<Output, Input>(
  schema: Schema<Output, Input>,
  jsonSchema: JSONSchema7
): Schema<Output, Input>;

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
