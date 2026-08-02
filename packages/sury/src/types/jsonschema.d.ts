// JSON Schema dialects, as Sury reads and writes them.
//
// Four interfaces: a wide `JSONSchema` for input, and one per dialect
// `toJSONSchema` can emit. Two rules hold them together:
//
//  1. Every dialect interface must stay assignable to `JSONSchema`. That is what
//     lets `fromJSONSchema(toJSONSchema(schema, options))` and
//     `extendJSONSchema(schema, toJSONSchema(other))` compose without a cast.
//     A keyword added to a dialect must also exist on `JSONSchema`.
//  2. Each interface is written out in full, on purpose. They mirror frozen
//     external specs, so there is no shared base to keep in sync — and a flat
//     interface is what makes hovers, autocomplete and error messages name the
//     dialect instead of expanding an intersection. Do not refactor these into
//     `extends`, `Omit` or mapped types.
//
// These describe what `toJSONSchema` actually emits, which is not always what
// the dialect's spec would say (`$defs` on every target, `examples` on OpenAPI).
// Where the two differ, the emitter wins and the deviation is commented — a type
// that describes an idealized dialect would be lying about the return value.
// The emitting branches live in src/jsonschema.ts (`target === ...`).

/**
 * Primitive type
 * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-6.1.1
 */
export type JSONSchemaTypeName =
  | "string" //
  | "number"
  | "integer"
  | "boolean"
  | "object"
  | "array"
  | "null";

/** `type` names available in OpenAPI 3.0, which models null with `nullable` instead. */
export type OpenAPISchema30TypeName =
  | "string" //
  | "number"
  | "integer"
  | "boolean"
  | "object"
  | "array";

/**
 * Any JSON value, as it appears in `const`, `enum`, `default` and `examples`.
 * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-6.1.1
 */
export type JSONSchemaValue =
  | string //
  | number
  | boolean
  | JSONSchemaValueObject
  | JSONSchemaValueArray
  | null;

// Interfaces (not aliases) to express the recursion.
// https://github.com/Microsoft/TypeScript/issues/3496#issuecomment-128553540
export interface JSONSchemaValueObject {
  [key: string]: JSONSchemaValue;
}
export interface JSONSchemaValueArray extends Array<JSONSchemaValue> {}

/** The OpenAPI 3.0 discriminator object. */
export interface OpenAPIDiscriminator {
  propertyName: string;
  mapping?: { [key: string]: string } | undefined;
}

/** The OpenAPI 3.0 XML object. */
export interface OpenAPIXML {
  name?: string | undefined;
  namespace?: string | undefined;
  prefix?: string | undefined;
  attribute?: boolean | undefined;
  wrapped?: boolean | undefined;
}

/** The OpenAPI 3.0 external documentation object. */
export interface OpenAPIExternalDocs {
  url: string;
  description?: string | undefined;
}

export type JSONSchemaDefinition = JSONSchema | boolean;

/**
 * A JSON Schema of any dialect Sury understands — draft-06 through draft-2020-12,
 * plus the OpenAPI 3.0 `nullable` extension.
 *
 * This is the type to author against (`{ ... } satisfies S.JSONSchema`) and what
 * `extendJSONSchema` accepts. Every keyword of every supported dialect is here,
 * including ones `fromJSONSchema` rejects at runtime: being able to write a
 * keyword down and get a targeted error beats an "unknown property" error that
 * doesn't say why.
 *
 * Draft-04 is not supported: it spells `$id` as `id`, and its boolean
 * `exclusiveMinimum`/`exclusiveMaximum` pair with `minimum`/`maximum` rather than
 * replacing them, which Sury does not interpret.
 */
export interface JSONSchema {
  $id?: string | undefined;
  $ref?: string | undefined;
  $schema?: string | undefined;
  $anchor?: string | undefined;
  $comment?: string | undefined;
  $defs?: { [key: string]: JSONSchemaDefinition } | undefined;
  definitions?: { [key: string]: JSONSchemaDefinition } | undefined;

  type?: JSONSchemaTypeName | JSONSchemaTypeName[] | undefined;
  enum?: JSONSchemaValue[] | undefined;
  const?: JSONSchemaValue | undefined;

  multipleOf?: number | undefined;
  maximum?: number | undefined;
  minimum?: number | undefined;
  // A number since draft-06. OpenAPI 3.0 keeps draft-04's boolean flag, which
  // modifies `maximum`/`minimum` instead of standing on its own.
  exclusiveMaximum?: number | boolean | undefined;
  exclusiveMinimum?: number | boolean | undefined;

  maxLength?: number | undefined;
  minLength?: number | undefined;
  pattern?: string | undefined;
  format?: string | undefined;
  contentMediaType?: string | undefined;
  contentEncoding?: string | undefined;
  contentSchema?: JSONSchemaDefinition | undefined;

  // An array of schemas up to draft-07 (positional), a single schema from
  // draft-2020-12 (where `prefixItems` took over the positional form).
  items?: JSONSchemaDefinition | JSONSchemaDefinition[] | undefined;
  prefixItems?: JSONSchemaDefinition[] | undefined;
  additionalItems?: JSONSchemaDefinition | undefined;
  unevaluatedItems?: JSONSchemaDefinition | undefined;
  maxItems?: number | undefined;
  minItems?: number | undefined;
  uniqueItems?: boolean | undefined;
  contains?: JSONSchemaDefinition | undefined;
  minContains?: number | undefined;
  maxContains?: number | undefined;

  properties?: { [key: string]: JSONSchemaDefinition } | undefined;
  patternProperties?: { [key: string]: JSONSchemaDefinition } | undefined;
  additionalProperties?: JSONSchemaDefinition | undefined;
  unevaluatedProperties?: JSONSchemaDefinition | undefined;
  propertyNames?: JSONSchemaDefinition | undefined;
  required?: string[] | undefined;
  maxProperties?: number | undefined;
  minProperties?: number | undefined;
  dependencies?: { [key: string]: JSONSchemaDefinition | string[] } | undefined;
  dependentSchemas?: { [key: string]: JSONSchemaDefinition } | undefined;
  dependentRequired?: { [key: string]: string[] } | undefined;

  allOf?: JSONSchemaDefinition[] | undefined;
  anyOf?: JSONSchemaDefinition[] | undefined;
  oneOf?: JSONSchemaDefinition[] | undefined;
  not?: JSONSchemaDefinition | undefined;
  if?: JSONSchemaDefinition | undefined;
  then?: JSONSchemaDefinition | undefined;
  else?: JSONSchemaDefinition | undefined;

  title?: string | undefined;
  description?: string | undefined;
  default?: JSONSchemaValue | undefined;
  deprecated?: boolean | undefined;
  readOnly?: boolean | undefined;
  writeOnly?: boolean | undefined;
  examples?: JSONSchemaValue[] | undefined;

  /** OpenAPI 3.0 only. Elsewhere nullability is `type: ["...", "null"]`. */
  nullable?: boolean | undefined;
  /** OpenAPI 3.0 only. */
  example?: JSONSchemaValue | undefined;
  /** OpenAPI 3.0 only. */
  discriminator?: OpenAPIDiscriminator | undefined;
  /** OpenAPI 3.0 only. */
  xml?: OpenAPIXML | undefined;
  /** OpenAPI 3.0 only. */
  externalDocs?: OpenAPIExternalDocs | undefined;

  // Vendor extensions are open, unknown keywords are not: `x-internal` passes,
  // a misspelled `requird` is still caught.
  [vendorExtension: `x-${string}`]: unknown;
}

export type JSONSchema7Definition = JSONSchema7 | boolean;

/**
 * JSON Schema draft-07 — what `toJSONSchema` emits by default.
 *
 * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01
 */
export interface JSONSchema7 {
  $id?: string | undefined;
  $ref?: string | undefined;
  $schema?: string | undefined;
  $comment?: string | undefined;
  // draft-07 spells this `definitions`; `$defs` arrived in draft-2019-09. Sury
  // emits `$defs` on every target, so both are here.
  $defs?: { [key: string]: JSONSchema7Definition } | undefined;
  definitions?: { [key: string]: JSONSchema7Definition } | undefined;

  type?: JSONSchemaTypeName | JSONSchemaTypeName[] | undefined;
  enum?: JSONSchemaValue[] | undefined;
  const?: JSONSchemaValue | undefined;

  multipleOf?: number | undefined;
  maximum?: number | undefined;
  exclusiveMaximum?: number | undefined;
  minimum?: number | undefined;
  exclusiveMinimum?: number | undefined;

  maxLength?: number | undefined;
  minLength?: number | undefined;
  pattern?: string | undefined;
  format?: string | undefined;
  contentMediaType?: string | undefined;
  contentEncoding?: string | undefined;

  /** An array describes a tuple positionally. draft-2020-12 uses `prefixItems`. */
  items?: JSONSchema7Definition | JSONSchema7Definition[] | undefined;
  additionalItems?: JSONSchema7Definition | undefined;
  maxItems?: number | undefined;
  minItems?: number | undefined;
  uniqueItems?: boolean | undefined;
  contains?: JSONSchema7Definition | undefined;

  properties?: { [key: string]: JSONSchema7Definition } | undefined;
  patternProperties?: { [key: string]: JSONSchema7Definition } | undefined;
  additionalProperties?: JSONSchema7Definition | undefined;
  propertyNames?: JSONSchema7Definition | undefined;
  required?: string[] | undefined;
  maxProperties?: number | undefined;
  minProperties?: number | undefined;
  dependencies?: { [key: string]: JSONSchema7Definition | string[] } | undefined;

  allOf?: JSONSchema7Definition[] | undefined;
  anyOf?: JSONSchema7Definition[] | undefined;
  oneOf?: JSONSchema7Definition[] | undefined;
  not?: JSONSchema7Definition | undefined;
  if?: JSONSchema7Definition | undefined;
  then?: JSONSchema7Definition | undefined;
  else?: JSONSchema7Definition | undefined;

  title?: string | undefined;
  description?: string | undefined;
  default?: JSONSchemaValue | undefined;
  // draft-2019-09 keyword. Sury emits it on every target from schema metadata.
  deprecated?: boolean | undefined;
  readOnly?: boolean | undefined;
  writeOnly?: boolean | undefined;
  examples?: JSONSchemaValue[] | undefined;

  [vendorExtension: `x-${string}`]: unknown;
}

export type JSONSchema2020Definition = JSONSchema2020 | boolean;

/**
 * JSON Schema draft-2020-12 — `toJSONSchema(schema, { target: "draft-2020-12" })`.
 *
 * @see https://json-schema.org/draft/2020-12/schema
 */
export interface JSONSchema2020 {
  $id?: string | undefined;
  $ref?: string | undefined;
  $schema?: string | undefined;
  $anchor?: string | undefined;
  $dynamicRef?: string | undefined;
  $dynamicAnchor?: string | undefined;
  $comment?: string | undefined;
  $defs?: { [key: string]: JSONSchema2020Definition } | undefined;

  type?: JSONSchemaTypeName | JSONSchemaTypeName[] | undefined;
  enum?: JSONSchemaValue[] | undefined;
  const?: JSONSchemaValue | undefined;

  multipleOf?: number | undefined;
  maximum?: number | undefined;
  exclusiveMaximum?: number | undefined;
  minimum?: number | undefined;
  exclusiveMinimum?: number | undefined;

  maxLength?: number | undefined;
  minLength?: number | undefined;
  pattern?: string | undefined;
  format?: string | undefined;
  contentMediaType?: string | undefined;
  contentEncoding?: string | undefined;
  contentSchema?: JSONSchema2020Definition | undefined;

  /** Positional schemas; `items` then constrains the elements after them. */
  prefixItems?: JSONSchema2020Definition[] | undefined;
  /** A single schema — the draft-07 array form became `prefixItems`. */
  items?: JSONSchema2020Definition | undefined;
  unevaluatedItems?: JSONSchema2020Definition | undefined;
  maxItems?: number | undefined;
  minItems?: number | undefined;
  uniqueItems?: boolean | undefined;
  contains?: JSONSchema2020Definition | undefined;
  minContains?: number | undefined;
  maxContains?: number | undefined;

  properties?: { [key: string]: JSONSchema2020Definition } | undefined;
  patternProperties?: { [key: string]: JSONSchema2020Definition } | undefined;
  additionalProperties?: JSONSchema2020Definition | undefined;
  unevaluatedProperties?: JSONSchema2020Definition | undefined;
  propertyNames?: JSONSchema2020Definition | undefined;
  required?: string[] | undefined;
  maxProperties?: number | undefined;
  minProperties?: number | undefined;
  dependentSchemas?: { [key: string]: JSONSchema2020Definition } | undefined;
  dependentRequired?: { [key: string]: string[] } | undefined;

  allOf?: JSONSchema2020Definition[] | undefined;
  anyOf?: JSONSchema2020Definition[] | undefined;
  oneOf?: JSONSchema2020Definition[] | undefined;
  not?: JSONSchema2020Definition | undefined;
  if?: JSONSchema2020Definition | undefined;
  then?: JSONSchema2020Definition | undefined;
  else?: JSONSchema2020Definition | undefined;

  title?: string | undefined;
  description?: string | undefined;
  default?: JSONSchemaValue | undefined;
  deprecated?: boolean | undefined;
  readOnly?: boolean | undefined;
  writeOnly?: boolean | undefined;
  examples?: JSONSchemaValue[] | undefined;

  [vendorExtension: `x-${string}`]: unknown;
}

/** OpenAPI 3.0 has no boolean form for subschemas — only `additionalProperties` takes one. */
export type OpenAPISchema30Definition = OpenAPISchema30;

/**
 * The OpenAPI 3.0 Schema Object — `toJSONSchema(schema, { target: "openapi-3.0" })`.
 *
 * A restricted, partly divergent draft-04: no `const`, no `null` type name, no
 * tuples, no `if`/`then`/`else`, and `nullable` in place of a union with null.
 *
 * @see https://spec.openapis.org/oas/v3.0.3#schema-object
 */
export interface OpenAPISchema30 {
  $ref?: string | undefined;
  // Not part of OpenAPI 3.0, which collects reusable schemas under
  // `components/schemas`. Sury emits `$defs` on every target, so a schema with
  // recursive references carries it here too.
  $defs?: { [key: string]: OpenAPISchema30Definition } | undefined;

  /** A single name — OpenAPI 3.0 has no type arrays, and no `"null"`. */
  type?: OpenAPISchema30TypeName | undefined;
  enum?: JSONSchemaValue[] | undefined;

  multipleOf?: number | undefined;
  maximum?: number | undefined;
  minimum?: number | undefined;
  /** draft-04 style: a flag modifying `maximum`, not a value of its own. */
  exclusiveMaximum?: boolean | undefined;
  /** draft-04 style: a flag modifying `minimum`, not a value of its own. */
  exclusiveMinimum?: boolean | undefined;

  maxLength?: number | undefined;
  minLength?: number | undefined;
  pattern?: string | undefined;
  format?: string | undefined;

  /** A single schema. Sury renders a tuple as `{ anyOf: [...] }` with fixed length. */
  items?: OpenAPISchema30Definition | undefined;
  maxItems?: number | undefined;
  minItems?: number | undefined;
  uniqueItems?: boolean | undefined;

  properties?: { [key: string]: OpenAPISchema30Definition } | undefined;
  additionalProperties?: OpenAPISchema30Definition | boolean | undefined;
  required?: string[] | undefined;
  maxProperties?: number | undefined;
  minProperties?: number | undefined;

  allOf?: OpenAPISchema30Definition[] | undefined;
  anyOf?: OpenAPISchema30Definition[] | undefined;
  oneOf?: OpenAPISchema30Definition[] | undefined;
  not?: OpenAPISchema30Definition | undefined;

  /** `X | null`, which has no `"null"` type name to fall back on here. */
  nullable?: boolean | undefined;
  discriminator?: OpenAPIDiscriminator | undefined;
  xml?: OpenAPIXML | undefined;
  externalDocs?: OpenAPIExternalDocs | undefined;

  title?: string | undefined;
  description?: string | undefined;
  default?: JSONSchemaValue | undefined;
  deprecated?: boolean | undefined;
  readOnly?: boolean | undefined;
  writeOnly?: boolean | undefined;
  example?: JSONSchemaValue | undefined;
  // OpenAPI 3.0 defines the singular `example`; Sury emits `examples` from
  // schema metadata regardless of target.
  examples?: JSONSchemaValue[] | undefined;

  [vendorExtension: `x-${string}`]: unknown;
}
