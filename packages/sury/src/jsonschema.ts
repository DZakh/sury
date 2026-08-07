// PORT-NOTE: no runtime values had to be imported from JSONSchema.res or
// StandardSchema.res — everything runtime-relevant there is `%identity`
// externals (Arrayable.single/array, Mutable.fromReadOnly/toReadOnly,
// Result casts) or `Object.assign` (Mutable.mixin), all inlined below.
// Their types are ported as loose TS aliases with the RUNTIME field names
// (`$ref`, `$schema`, `$defs`, `type`, `if`, `else` — the `@as(...)` names,
// not the ReScript field names `ref`/`schema`/`defs`/`type_`/`if_`/`else_`).
// =============================================================================

import {
  anyOfTag,
  arrayTag,
  baseSchema,
  booleanTag,
  copySchema,
  defsPath,
  flagNone,
  flagUnsafeHas,
  getOrRethrow,
  inputExpression,
  type Internal,
  isLiteral,
  isOptional,
  jsonName,
  s as errorSymbol,
  neverTag,
  nullTag,
  numberTag,
  objectTag,
  type Path,
  pathConcat,
  pathDynamic,
  pathEmpty,
  pathFromLocation,
  refTag,
  stringTag,
  SuryError,
  tagFlagArray,
  tagFlagObject,
  tagFlags,
  tagFlagUnion,
  U,
  undefinedTag,
  unknown,
} from "./base";
import { json } from "./advanced/json";
import { recursiveDecoder } from "./advanced/recursive";
import { B_operationArg } from "./builder";
import { array, option } from "./composites";
import { definitionToSchema, schemaFactory } from "./factory";
import {
  meta,
  Metadata_get,
  Metadata_Id_internal,
  Metadata_set,
  Option_getOr,
  refine,
  refineInput,
  strict,
} from "./modifiers";
import { __setStandardJSONSchemaConverter, assertOrThrow } from "./operations";
import { never_, parse, reverse } from "./parse";
import { bool, float, int, Literal_parse, string } from "./primitives";
import {
  dict,
  email,
  gt,
  gte,
  isoDateTime,
  lt,
  lte,
  maxLength,
  minLength,
  null_,
  object,
  pattern,
  tuple,
  union,
  url,
  uuid,
} from "./refinements";

/**
 * Primitive type
 * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-6.1.1
 */
export type JSONSchemaTypeName =
  | "string"
  | "number"
  | "integer"
  | "boolean"
  | "object"
  | "array"
  | "null";

// PORT-NOTE: JSONSchema.Arrayable.t<'item> is an untagged `item | item[]`;
// `Arrayable.single`/`Arrayable.array` are %identity and are dropped at call
// sites, `Arrayable.isArray` is Array.isArray, and `Arrayable.classify` is an
// inline Array.isArray test.
export type JSONSchemaArrayable<TItem> = TItem | TItem[];

// PORT-NOTE: JSONSchema's `definition` is `@unboxed
// Schema(t) | @as(false) Never | @as(true) Any` — at runtime a definition is
// the schema object itself, `false`, or `true`. The `Schema(...)` wrapping
// at construction sites is a no-op and is dropped; `Never` -> `false`,
// `Any` -> `true`; the `Schema(t)` pattern -> `typeof d !== "boolean"`.
export type JSONSchemaDefinition = JSONSchemaT | boolean;

/**
 * Every JSON Schema keyword the converters read or write, across all supported
 * dialects. The same set is spelled out in JSONSchema.res (the ReScript-facing
 * type) and in src/types/jsonschema.d.ts (the JS-facing `JSONSchema`, which also
 * splits per dialect for `toJSONSchema`'s result). A keyword added to one
 * belongs in all three.
 *
 * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01
 */
// PORT-NOTE: JSONSchema.t and JSONSchema.Mutable.t are the same runtime
// object (Mutable.fromReadOnly/toReadOnly are %identity); TS has no
// readonly/mutable split worth keeping here, so a single mutable type serves
// both, and Mutable.fromReadOnly/toReadOnly calls are dropped.
export type JSONSchemaT = {
  $id?: string;
  $ref?: string;
  $schema?: string;
  /**
   * @see https://datatracker.ietf.org/doc/html/draft-bhutton-json-schema-00#section-8.2.4
   * @see https://datatracker.ietf.org/doc/html/draft-bhutton-json-schema-validation-00#appendix-A
   */
  $defs?: Record<string, JSONSchemaDefinition>;
  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-6.1
   */
  type?: JSONSchemaArrayable<JSONSchemaTypeName>;
  enum?: unknown[];
  const?: unknown;
  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-6.2
   */
  multipleOf?: number;
  maximum?: number;
  // draft-04/OpenAPI 3.0 spell exclusivity as a boolean modifier on
  // minimum/maximum; draft-06+ as an independent numeric bound.
  exclusiveMaximum?: number | boolean;
  minimum?: number;
  exclusiveMinimum?: number | boolean;
  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-6.3
   */
  maxLength?: number;
  minLength?: number;
  pattern?: string;
  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-6.4
   */
  items?: JSONSchemaArrayable<JSONSchemaDefinition>;
  prefixItems?: JSONSchemaDefinition[];
  additionalItems?: JSONSchemaDefinition;
  unevaluatedItems?: JSONSchemaDefinition;
  maxItems?: number;
  minItems?: number;
  uniqueItems?: boolean;
  contains?: JSONSchemaDefinition;
  minContains?: number;
  maxContains?: number;
  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-6.5
   */
  maxProperties?: number;
  minProperties?: number;
  required?: string[];
  properties?: Record<string, JSONSchemaDefinition>;
  patternProperties?: Record<string, JSONSchemaDefinition>;
  additionalProperties?: JSONSchemaDefinition;
  unevaluatedProperties?: JSONSchemaDefinition;
  dependencies?: Record<string, unknown>;
  dependentSchemas?: Record<string, JSONSchemaDefinition>;
  dependentRequired?: Record<string, string[]>;
  propertyNames?: JSONSchemaDefinition;
  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-6.6
   */
  if?: JSONSchemaDefinition;
  then?: JSONSchemaDefinition;
  else?: JSONSchemaDefinition;
  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-6.7
   */
  allOf?: JSONSchemaDefinition[];
  anyOf?: JSONSchemaDefinition[];
  oneOf?: JSONSchemaDefinition[];
  not?: JSONSchemaDefinition;
  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-7
   */
  format?: string;
  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-8
   */
  contentMediaType?: string;
  contentEncoding?: string;
  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-9
   */
  definitions?: Record<string, JSONSchemaDefinition>;
  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-10
   */
  title?: string;
  description?: string;
  deprecated?: boolean;
  nullable?: boolean;
  default?: unknown;
  readOnly?: boolean;
  writeOnly?: boolean;
  examples?: unknown[];
};

// PORT-NOTE: StandardSchema.JsonSchema.target is `@unboxed | @as("draft-07")
// Draft07 | @as("draft-2020-12") Draft202012 | @as("openapi-3.0") OpenApi30 |
// Unknown(string)` — at runtime it's just a string; the known dialects are
// compared as string literals, everything else is the `Unknown` case.
// TODO(integration): if section 06 already declares these two aliases for
// standardJSONSchemaRef's signature, keep a single declaration.
export type JsonSchemaTarget = "draft-07" | "draft-2020-12" | "openapi-3.0" | (string & {});

// Compared on every emit branch that differs by dialect; naming it once keeps
// the literal out of the bundle at each of those sites.
const openApi30 = "openapi-3.0";

export type StandardJsonSchemaOptions = {
  target: JsonSchemaTarget;
  libraryOptions?: Record<string, unknown>;
};

// encodeToJsonSchema / internalToJSONSchema / internalToJSONSchemaBase below
// are mutually recursive, so they're declared as standalone top-level
// functions rather than nested closures.

const jsonSchemaMetadataId: string = /* @__PURE__ */ Metadata_Id_internal("JSONSchema");

const jsonSchemaMerge = (a: JSONSchemaT, b: JSONSchemaT): JSONSchemaT => {
  return Object.assign({}, a, b);
}

const applyMetadataOverlay = (
  jsonSchema: JSONSchemaT,
  schema: Internal,
  defs: Record<string, Internal>
): void => {
  if (schema.description !== U) {
    jsonSchema.description = schema.description;
  }
  if (schema.title !== U) {
    jsonSchema.title = schema.title;
  }
  if (schema.deprecated !== U) {
    jsonSchema.deprecated = schema.deprecated;
  }
  if (schema.examples !== U) {
    // If a schema is Jsonable, then examples are Jsonable too.
    jsonSchema.examples = schema.examples;
  }
  if (schema["$defs"] !== U) {
    Object.assign(defs, schema["$defs"]);
  }
  const metadataRawSchema = Metadata_get(schema, jsonSchemaMetadataId) as
    | JSONSchemaT
    | undefined;
  if (metadataRawSchema !== U) {
    Object.assign(jsonSchema, metadataRawSchema);
  }
}

const encodeToJsonSchema = (
  schema: Internal,
  path: Path,
  defs: Record<string, Internal>,
  parent: Internal,
  target: JsonSchemaTarget
): JSONSchemaT | undefined => {
  const schemaInternal = schema;
  const reversed = reverse(schemaInternal);
  const input = B_operationArg(unknown, reversed, flagNone, U);
  try {
    const output = parse(input);
    // The parse produces a val whose .schema reflects the
    // JSON-compatible transformed structure.
    return internalToJSONSchema(output.s, path, defs, parent, target);
  } catch (exn) {
    getOrRethrow(exn);

    // Parse failed — caller falls through to normal tag-based logic.
    return U;
  }
}

const internalToJSONSchema = (
  schema: Internal,
  path: Path,
  defs: Record<string, Internal>,
  parent: Internal,
  target: JsonSchemaTarget
): JSONSchemaT => {
  const schemaInternal = schema;
  // When a schema has `.to`, we can try to encode-reverse it to get a more
  // precise JSON schema (e.g. `format: "date-time"` for `S.string->S.to(S.date)`).
  // For a user-applied `.to` on a union (no `parser`) the encode-reverse output
  // is the schema produced by the union decoder, already shrunk to the
  // surviving variants — exactly what a downstream JSON Schema should describe.
  // Unions with a `parser` come from the option machinery (S.option,
  // Option.getOrWith, ...) where the union's anyOf is the input format we want
  // to keep describing. Object/array still need their nested item metadata, so
  // they keep using the base path.
  const tagFlag = tagFlags[schemaInternal.type]!;
  const hasUserTo =
    !!schemaInternal.to &&
    !flagUnsafeHas(tagFlag, (tagFlagObject | tagFlagArray)) &&
    !(flagUnsafeHas(tagFlag, tagFlagUnion) && !!schemaInternal.parser);
  const encoded = hasUserTo
    ? encodeToJsonSchema(schema, path, defs, parent, target)
    : U;
  if (encoded !== U) {
    applyMetadataOverlay(encoded, schema, defs);
    return encoded;
  } else {
    return internalToJSONSchemaBase(schema, path, defs, parent, target);
  }
}

const internalToJSONSchemaBase = (
  schema: Internal,
  path: Path,
  defs: Record<string, Internal>,
  parent: Internal,
  target: JsonSchemaTarget
): JSONSchemaT => {
  const jsonSchema: JSONSchemaT = {};
  // OpenAPI 3.0 has no `const`; describe a single allowed value with `enum`.
  const setConstOrEnum = (value: unknown) => {
    if (target === openApi30) {
      jsonSchema.enum = [value];
    } else {
      jsonSchema.const = value;
    }
  };
  const tag = schema.type;
  if (tag === stringTag) {
    const const_ = schema.const as string | undefined;
    const format = schema.format;
    jsonSchema.type = "string";
    switch (format) {
      case "date-time":
        jsonSchema.format = "date-time";
        break;
      case "email":
        jsonSchema.format = "email";
        break;
      case "uuid":
        jsonSchema.format = "uuid";
        break;
      case "url":
        jsonSchema.format = "uri";
        break;
      default:
        break;
    }
    if (schema.minLength !== U) {
      jsonSchema.minLength = schema.minLength;
    }
    if (schema.maxLength !== U) {
      jsonSchema.maxLength = schema.maxLength;
    }
    if (schema.pattern !== U) {
      jsonSchema.pattern = schema.pattern.source;
    }
    if (const_ !== U) {
      setConstOrEnum(const_);
    }
  } else if (tag === numberTag) {
    const format = schema.format;
    const const_ = schema.const as number | undefined;
    // A bigint schema never reaches here (it fails as non-JSON first), so the
    // `number | bigint` bound fields are always numbers by this point. The
    // refinements keep at most one per side, so these are mutually exclusive.
    const minimum = schema.minimum as number | undefined;
    const maximum = schema.maximum as number | undefined;
    const exclusiveMinimum = schema.exclusiveMinimum as number | undefined;
    const exclusiveMaximum = schema.exclusiveMaximum as number | undefined;
    // int32 and port carry their range as bound fields, so nothing
    // format-specific is left to emit here — and a user bound that superseded
    // one of them has already cleared it.
    jsonSchema.type = format === "int32" || format === "port" ? "integer" : "number";
    if (minimum !== U) {
      jsonSchema.minimum = minimum;
    }
    if (maximum !== U) {
      jsonSchema.maximum = maximum;
    }
    // draft-06 made exclusive bounds independent numeric keywords; draft-04 —
    // which OpenAPI 3.0 follows — spells them as booleans modifying
    // minimum/maximum.
    if (exclusiveMinimum !== U) {
      if (target === openApi30) {
        jsonSchema.minimum = exclusiveMinimum;
        jsonSchema.exclusiveMinimum = true;
      } else {
        jsonSchema.exclusiveMinimum = exclusiveMinimum;
      }
    }
    if (exclusiveMaximum !== U) {
      if (target === openApi30) {
        jsonSchema.maximum = exclusiveMaximum;
        jsonSchema.exclusiveMaximum = true;
      } else {
        jsonSchema.exclusiveMaximum = exclusiveMaximum;
      }
    }
    if (const_ !== U) {
      setConstOrEnum(const_);
    }
  } else if (tag === booleanTag) {
    const const_ = schema.const as boolean | undefined;
    jsonSchema.type = "boolean";
    if (const_ !== U) {
      setConstOrEnum(const_);
    }
  } else if (tag === arrayTag) {
    const additionalItems = schema.additionalItems!;
    const items = schema.items!;
    if (typeof additionalItems === "object") {
      jsonSchema.items = internalToJSONSchema(
        additionalItems,
        pathConcat(path, pathDynamic),
        defs,
        schema,
        target
      );
      jsonSchema.type = "array";
      if (schema.minItems !== U) {
        jsonSchema.minItems = schema.minItems;
      }
      if (schema.maxItems !== U) {
        jsonSchema.maxItems = schema.maxItems;
      }
    } else {
      const itemDefinitions: JSONSchemaT[] = items.map((itemSchema, idx) => {
        return internalToJSONSchema(
          itemSchema,
          pathConcat(path, pathFromLocation(idx.toString())),
          defs,
          schema,
          target
        );
      });
      const itemsNumber = itemDefinitions.length;

      jsonSchema.type = "array";
      jsonSchema.minItems = itemsNumber;
      jsonSchema.maxItems = itemsNumber;
      if (target === openApi30) {
        // OpenAPI 3.0 has no tuple support. Describe a fixed-length array
        // whose every item matches any of the positional item schemas.
        jsonSchema.items = { anyOf: itemDefinitions };
      } else if (target === "draft-2020-12") {
        // draft-2020-12 uses `prefixItems` for positional schemas.
        jsonSchema.prefixItems = itemDefinitions;
      } else {
        // draft-07 (default) uses an `items` array for positional schemas.
        jsonSchema.items = itemDefinitions;
      }
    }
  } else if (tag === anyOfTag) {
    const anyOf = schema.anyOf!;
    const literals: unknown[] = [];
    const items: JSONSchemaT[] = [];
    const seen: Record<string, boolean> = {};

    anyOf.forEach((childSchema) => {
      // Filter out undefined to support optional fields — no `else` branch
      // needed, this variant is simply skipped.
      if (!(childSchema.type === undefinedTag && parent.type === objectTag)) {
        const childJsonSchema = internalToJSONSchema(childSchema, path, defs, schema, target);
        // Collapse structurally-identical members (e.g. variants coercing to
        // the same `.to` target) so the union renders as `T`, not `anyOf:[T,T]`.
        const key = JSON.stringify(childJsonSchema);
        if (!(key in seen)) {
          seen[key] = true;
          items.push(childJsonSchema);
          if (isLiteral(childSchema)) {
            literals.push(
              childSchema.const // If a schema is Jsonable, the const is Jsonable too.
            );
          }
        }
      }
    });

    const itemsNumber = items.length;

    if (schema.default !== U) {
      jsonSchema.default = schema.default;
    }

    // Detect whether a definition is the "null" representation for the
    // current target. Sury models nullable as a union `[X, null]`; for
    // openapi-3.0 the null variant is `{enum:[null]}` (see the Null case),
    // for other targets it is `{type:"null"}`.
    const isNullDefinition = (definition: JSONSchemaDefinition): boolean => {
      if (typeof definition !== "boolean") {
        const t = definition;
        if (t.type === "null") {
          return true;
        } else if (t.enum !== U && t.enum.length === 1 && t.enum[0] === null) {
          return true;
        } else {
          return false;
        }
      } else {
        return false;
      }
    };

    // TODO: Write a breaking test with itemsNumber === 0
    if (itemsNumber === 1) {
      Object.assign(jsonSchema, items[0]);
    } else if (literals.length === itemsNumber) {
      jsonSchema.enum = literals;
    } else if (
      // OpenAPI 3.0 collapse of `X | null` into `{...X, nullable: true}`.
      target === openApi30 &&
      itemsNumber === 2 &&
      (isNullDefinition(items[0]!) || isNullDefinition(items[1]!))
    ) {
      const nullIsFirst = isNullDefinition(items[0]!);
      const nonNull = items[nullIsFirst ? 1 : 0]!;
      if (typeof nonNull !== "boolean") {
        const nonNullSchema = nonNull;
        Object.assign(jsonSchema, nonNullSchema);
        jsonSchema.nullable = true;
      } else {
        // `Any`/`Never` non-null variants can't be merged into a single
        // nullable schema; fall back to anyOf.
        jsonSchema.anyOf = items;
      }
    } else {
      jsonSchema.anyOf = items;
    }
  } else if (tag === objectTag) {
    const properties = schema.properties!;
    const additionalItems = schema.additionalItems!;
    if (typeof additionalItems === "object") {
      jsonSchema.type = "object";
      const childJsonSchema = internalToJSONSchema(
        additionalItems,
        pathConcat(path, pathDynamic),
        defs,
        schema,
        target
      );
      jsonSchema.additionalProperties =
        Object.keys(childJsonSchema).length === 0 ? true : childJsonSchema;
    } else {
      const required: string[] = [];
      const jsonProperties: Record<string, JSONSchemaDefinition> = {};

      Object.keys(properties).forEach((key) => {
        const itemSchema = properties[key]!;
        const fieldSchema = internalToJSONSchema(
          itemSchema,
          pathConcat(path, pathFromLocation(key)),
          defs,
          schema,
          target
        );
        if (!isOptional(itemSchema)) {
          required.push(key);
        }
        jsonProperties[key] = fieldSchema;
      });

      jsonSchema.type = "object";
      jsonSchema.properties = jsonProperties;
      if (additionalItems === "strict") {
        jsonSchema.additionalProperties = false;
      }
      if (required.length !== 0) {
        jsonSchema.required = required;
      }
    }
  } else if (tag === refTag && schema["$ref"] === `${defsPath}${jsonName}`) {
    // S.json → empty {}
  } else if (tag === refTag) {
    jsonSchema.$ref = schema["$ref"];
  } else if (tag === nullTag) {
    if (target === openApi30) {
      // OpenAPI 3.0 has no `null` type. Use an enum as a workaround.
      jsonSchema.enum = [null];
    } else {
      jsonSchema.type = "null";
    }
  } else if (tag === neverTag) {
    jsonSchema.not = {};
  } else {
    // Not `invalid_input`: nothing was parsed, so there is no input to report
    // and no schema a value failed against. What failed is the conversion
    // itself, on a schema that has no JSON Schema equivalent — which is what
    // `invalid_operation` describes. The offending schema is named in the
    // reason and located by `path`.
    const offender = flagUnsafeHas(tagFlags[parent.type]!, tagFlagUnion) ? parent : schema;
    throw new SuryError({
      code: "invalid_operation",
      path,
      reason: `Expected ${jsonName}, received ${inputExpression(offender)}`,
    });
  }

  applyMetadataOverlay(jsonSchema, schema, defs);

  return jsonSchema;
}

export type toJSONSchemaOptions = { target?: JsonSchemaTarget };

// Single source of truth for the `target` -> `$schema` URI mapping (mirrors
// @valibot/to-json-schema). Returns the URI to stamp, or `None` when the target
// has no `$schema` (openapi-3.0). Raises an `invalid_operation` error for
// `Unknown` (an unsupported target, e.g. one that arrived as an arbitrary
// string from JS via the Standard JSON Schema `Options`).
const targetSchemaUri = (target: JsonSchemaTarget): string | undefined => {
  switch (target) {
    case "draft-07":
      return "http://json-schema.org/draft-07/schema#";
    case "draft-2020-12":
      return "https://json-schema.org/draft/2020-12/schema";
    // OpenAPI 3.0 has no `$schema` property.
    case openApi30:
      return U;
    default: {
      const unsupported = target;
      throw new SuryError({
        code: "invalid_operation",
        path: pathEmpty,
        reason: `Unsupported JSON Schema target: ${unsupported}`,
      });
    }
  }
}

// @__NO_SIDE_EFFECTS__
export const toJSONSchema = (schema: Internal, options?: toJSONSchemaOptions): JSONSchemaT => {
  // Resolve the target and the `$schema` URI to stamp. When no options object is
  // provided we keep the historical behavior: default to "draft-07" and do NOT
  // stamp `$schema`. With options, an unsupported target throws up front (even
  // for openapi-3.0, which stamps no `$schema`).
  let target: JsonSchemaTarget;
  let schemaUri: string | undefined;
  if (options !== U) {
    target = options.target !== U ? options.target : "draft-07";
    schemaUri = targetSchemaUri(target);
  } else {
    target = "draft-07";
    schemaUri = U;
  }
  const defs: Record<string, Internal> = {};
  const jsonSchema = internalToJSONSchema(schema, pathEmpty, defs, schema, target);
  delete (defs as Record<string, unknown>).JSON;
  const defsKeys = Object.keys(defs);
  if (defsKeys.length) {
    // Reuse the same object to prevent allocations
    // Nothing critical, just because we can
    const jsonSchemDefs = defs as unknown as Record<string, JSONSchemaDefinition>;
    defsKeys.forEach((key) => {
      const schema = defs[key]!;
      jsonSchemDefs[key] = internalToJSONSchema(
        schema,
        pathEmpty,
        // A fresh, thrown-away sink — it's not possible to have nested
        // recursive schemas here; everything should be grouped into the
        // single top-level $defs collected above, not accumulate into a
        // second one.
        {},
        schema,
        target
      );
    });
    jsonSchema.$defs = jsonSchemDefs;
  }
  if (schemaUri !== U) {
    jsonSchema.$schema = schemaUri;
  }
  return jsonSchema;
}

// Wiring this inside a function (vs top level) is what makes toJSONSchema/reverse tree-shakeable.
//
// Mirrors @valibot/to-json-schema's `toStandardJsonSchema`: the `target` option
// selects the JSON Schema dialect (and the stamped `$schema` URI), and an
// unsupported target throws. `output` converts the reversed schema, since
// `S.reverse` swaps Input <-> Output and `toJSONSchema` returns the input-type
// schema of whatever it receives.
export const enableStandardJSONSchema = (): void => {
  __setStandardJSONSchemaConverter((schema, options, isOutput) => {
    // The converter just forwards the target; `toJSONSchema` is the single
    // source of truth for the `$schema` URI mapping and the unsupported-target
    // throw. Passing an options object (vs none) is what makes `toJSONSchema`
    // stamp `$schema`, which the Standard JSON Schema spec requires.
    return toJSONSchema(isOutput ? reverse(schema) : schema, { target: options.target });
  });
}

// @__NO_SIDE_EFFECTS__
export const extendJSONSchema = (schema: Internal, jsonSchema: JSONSchemaT): Internal => {
  const existingSchemaExtend = Metadata_get(schema, jsonSchemaMetadataId) as
    | JSONSchemaT
    | undefined;
  return Metadata_set(
    schema,
    jsonSchemaMetadataId,
    existingSchemaExtend !== U
      ? jsonSchemaMerge(existingSchemaExtend, jsonSchema)
      : jsonSchema
  );
}

// PORT-NOTE: `castAnySchemaToJsonableS` is a bare `Obj.magic` (a pure no-op
// type re-cast, `schema<'any> => schema<JSON.t>`). It has no runtime body, so
// no value is emitted here and every `->castAnySchemaToJsonableS` call below
// is simply dropped. If the public bindings layer needs the name, it's a TS
// `as` cast there.

// PORT-NOTE: the `let rec fromJSONSchema = { let helper = ...; jsonSchema => ... }`
// block-scoped helpers (primitiveToSchema, toIntSchema,
// definitionToDefaultValue) are hoisted to module-scope functions —
// same behavior, they close over nothing but module-level bindings.

const primitiveToSchema = (primitive: unknown): Internal => {
  return Literal_parse(primitive);
}

// draft-04 (and OpenAPI 3.0) make `exclusiveMinimum` a boolean that flips the
// meaning of `minimum`; draft-06+ make it an independent numeric bound. `true`
// therefore consumes `minimum` rather than adding a second bound, and the two
// spellings never both apply.
const exclusiveBound = (
  inclusive: number | undefined,
  exclusive: number | boolean | undefined
): number | undefined =>
  exclusive === true ? inclusive : typeof exclusive === "number" ? exclusive : U;

const inclusiveBound = (
  inclusive: number | undefined,
  exclusive: number | boolean | undefined
): number | undefined => (exclusive === true ? U : inclusive);

// The integer and number branches read the same four keywords the same way,
// so they share one pass rather than each spelling it out.
const withNumericBounds = (schema: Internal, jsonSchema: JSONSchemaT): Internal => {
  // TODO: Support jsonSchema.multipleOf
  const min = inclusiveBound(jsonSchema.minimum, jsonSchema.exclusiveMinimum);
  const exMin = exclusiveBound(jsonSchema.minimum, jsonSchema.exclusiveMinimum);
  const max = inclusiveBound(jsonSchema.maximum, jsonSchema.exclusiveMaximum);
  const exMax = exclusiveBound(jsonSchema.maximum, jsonSchema.exclusiveMaximum);
  if (min !== U) {
    schema = applyBound(schema, gte, min);
  }
  if (exMin !== U) {
    schema = applyBound(schema, gt, exMin);
  }
  if (max !== U) {
    schema = applyBound(schema, lte, max);
  }
  if (exMax !== U) {
    schema = applyBound(schema, lt, exMax);
  }
  return schema;
}

const toIntSchema = (jsonSchema: JSONSchemaT): Internal => withNumericBounds(int, jsonSchema);

// Assertion keywords Sury doesn't model. Silently ignoring one widens the
// schema — the validator then accepts data the author wrote the keyword to
// reject — so creation fails instead. Annotations (`title`, `default`,
// `$comment`, …) are ignored on purpose and stay out of this list.
const unsupportedKeywords = [
  "multipleOf",
  "uniqueItems",
  "contains",
  "minContains",
  "maxContains",
  "patternProperties",
  "propertyNames",
  "minProperties",
  "maxProperties",
  "dependencies",
  "dependentSchemas",
  "dependentRequired",
  "unevaluatedProperties",
  "unevaluatedItems",
  "additionalItems",
];

// Which JSON type each assertion keyword constrains. A keyword says nothing
// about an instance of any other type — `{minLength: 3}` accepts `42` — so a
// schema without `type` has to apply each group only to its own type.
const keywordTypes: [JSONSchemaTypeName, string[]][] = [
  ["string", ["pattern", "minLength", "maxLength"]],
  ["number", ["minimum", "maximum", "exclusiveMinimum", "exclusiveMaximum"]],
  ["object", ["properties", "required", "additionalProperties"]],
  ["array", ["items", "prefixItems", "minItems", "maxItems"]],
];

const jsonTypeOf = (data: unknown): string =>
  data === null
    ? "null"
    : Array.isArray(data)
      ? "array"
      : typeof data === "boolean"
        ? "boolean"
        : typeof data === "number"
          ? "number"
          : typeof data === "string"
            ? "string"
            : "object";

const passesSchema = (data: unknown, schema: Internal): boolean => {
  try {
    assertOrThrow(data, schema);
    return true;
  } catch (_) {
    return false;
  }
};

const definitionToDefaultValue = (definition: JSONSchemaDefinition): unknown => {
  if (typeof definition !== "boolean") {
    return definition.default;
  } else {
    return U;
  }
}

// A document may describe an empty range — `{minimum: 5, maximum: 1}`, or a
// bound past int32's edge. That is legal JSON Schema with no inhabitants, so
// it has to load, where the same bounds written by hand are a caller bug that
// the public bound panics on. Reading that panic as `never` is what lets this
// file use the real bounds instead of restating when they conflict. Scoped to
// one application so an unrelated panic still escapes, as does a SuryError
// (a malformed bound value, say).
const applyBound = (
  schema: Internal,
  bound: (schema: Internal, value: number) => Internal,
  value: number
): Internal => {
  // Already empty — a further bound can only panic on it and land back here.
  if (schema.type === neverTag) {
    return schema;
  }
  try {
    return bound(schema, value);
  } catch (exn) {
    if (exn && (exn as { s?: symbol }).s === errorSymbol) {
      throw exn;
    }
    return never_;
  }
}

// What a whole `fromJSONSchema` call shares: `$ref` is a JSON Pointer resolved
// from the document's root wherever it appears, so the nested calls need the
// root and each other's work, not just their own subschema.
type RefContext = {
  root: JSONSchemaT;
  // pointer -> the ref schema minted for it, before its target is built. A
  // pointer reached again while its own target is still building is a cycle,
  // and finds this instead of recursing forever.
  ph: Record<string, Internal>;
  // pointer -> the built target, once building finished
  built: Record<string, Internal>;
  // The pointers a cycle actually came back to. Only those become `$defs`
  // entries; a `$ref` to a finite shape inlines it, so the generated code
  // pays for a call per value only where recursion makes one unavoidable.
  cyc: Record<string, true>;
  defs: Record<string, Internal>;
  // Def names already taken. `S.json` mints its def under `JSON`, into the
  // same dict at compile time, so a document's own `JSON` has to be renamed
  // rather than shadow it.
  names: Record<string, true>;
};

const refError = (reason: string): SuryError =>
  new SuryError({
    code: "invalid_operation",
    path: pathEmpty,
    reason,
  });

// RFC 6901: `~1` is `/` and `~0` is `~`, in that order, and the fragment may
// arrive percent-encoded.
const unescapePointer = (segment: string): string => {
  // A raw `%` that isn't valid percent-encoding ("50%") is common in real
  // documents; take the segment literally rather than letting a URIError
  // escape past the SuryError contract. Literally, not replacement-char
  // substituted the way a non-throwing decoder would: raw text can still match
  // the key the document wrote, `U+FFFD` never can (see IDEAS.md on `deuri`).
  try {
    segment = decodeURIComponent(segment);
  } catch {}
  return segment.replace(/~1/g, "/").replace(/~0/g, "~");
};

const resolveRef = (ref: string, ctx: RefContext): Internal => {
  if (ctx.cyc[ref]) {
    return ctx.ph[ref]!;
  }
  const built = ctx.built[ref];
  if (built !== U) {
    return built;
  }
  const placeholder = ctx.ph[ref];
  if (placeholder !== U) {
    ctx.cyc[ref] = true;
    return placeholder;
  }

  const segments = ref.split("/");
  if (segments[0] !== "#") {
    throw refError(
      `Unsupported JSON Schema $ref: ${ref}. Only JSON Pointers into the same document (#/…) resolve — $id, $anchor and remote refs don't`
    );
  }
  let target: unknown = ctx.root;
  for (let i = 1; i < segments.length; i++) {
    target =
      target !== null && typeof target === "object"
        ? (target as Record<string, unknown>)[unescapePointer(segments[i]!)]
        : U;
  }
  // A pointer that lands on a non-schema value (a string, `null`, an `enum`
  // array) must fail like a dangling one — falling through would hand the
  // untyped branch an accept-everything `S.json`.
  if (
    target === null ||
    (typeof target !== "object" && typeof target !== "boolean") ||
    Array.isArray(target)
  ) {
    throw refError(`Failed to resolve JSON Schema $ref: ${ref}`);
  }

  // The pointer's last segment is the name the document already uses for the
  // definition, so a round-trip through toJSONSchema keeps it. `#` points at
  // the document itself and has no segment to take. `/`, `~` and `%` can't
  // keep: recursiveDecoder slices the raw suffix off `$ref`, so they'd come
  // back out of toJSONSchema as a pointer that resolves to a different key.
  const base =
    ref === "#"
      ? "Root"
      : unescapePointer(segments[segments.length - 1]!).replace(/[~/%]/g, "_");
  let name = base;
  let suffix = 2;
  while (ctx.names[name]) {
    name = base + suffix++;
  }
  ctx.names[name] = true;

  const refSchema = baseSchema(refTag, false);
  refSchema["$ref"] = `${defsPath}${name}`;
  refSchema.name = name;
  refSchema.decoder = recursiveDecoder;
  ctx.ph[ref] = refSchema;

  const def = jsonDefinitionToSchema(target as JSONSchemaDefinition, ctx);
  // A cycle with no schema between the refs (`{"$ref": "#"}`, or A→B→A) builds
  // a def that is its own placeholder: nothing to compile, only recursion, and
  // compiling it would recurse forever. Comparing `$ref` rather than identity
  // also catches the placeholder coming back wrapped in a meta copy.
  if ((def as Record<string, unknown>)["$ref"] === refSchema["$ref"]) {
    throw refError(`Infinite JSON Schema $ref loop: ${ref}`);
  }
  ctx.built[ref] = def;
  if (ctx.cyc[ref]) {
    ctx.defs[name] = def;
    return refSchema;
  }
  // The target turned out finite, so the ref inlines and the minted schema is
  // discarded — release the name for a def that will actually occupy it.
  delete ctx.names[name];
  return def;
};

const jsonDefinitionToSchema = (
  definition: JSONSchemaDefinition,
  ctx: RefContext
): Internal =>
  typeof definition !== "boolean"
    ? fromJSONSchema(definition, ctx)
    : definition
      ? json
      : never_;

// The compiler reads `$defs` off the schema it is handed, so every schema that
// gets compiled as a root of its own needs the document's — the outermost one,
// and each schema `passesSchema` runs, since a `$ref` inside an `allOf` member
// resolves against the same document as everywhere else. Copied because the
// schema may be a shared instance (an interned primitive, or a def inlined at
// more than one use). The live `ctx.defs` is handed over rather than a snapshot:
// a cycle still being built registers its def after this returns.
const withDefs = (schema: Internal, ctx: RefContext): Internal => {
  const copy = copySchema(schema);
  // `S.json` names itself through a `$defs` of its own, so fold rather than
  // replace — dropping it leaves the compiler with a `$ref` it can't resolve.
  if (copy["$defs"] !== U) {
    Object.assign(ctx.defs, copy["$defs"]);
  }
  copy["$defs"] = ctx.defs;
  return copy;
};

const asAssertion = (definition: JSONSchemaDefinition, ctx: RefContext): Internal =>
  withDefs(jsonDefinitionToSchema(definition, ctx), ctx);

// @__NO_SIDE_EFFECTS__
export const fromJSONSchema = (
  jsonSchema: JSONSchemaT,
  parentCtx?: RefContext
): Internal => {
  const anySchema = json;
  // Every nested call threads the caller's context; only the outermost one
  // owns the document, and only it publishes the `$defs` collected below.
  const ctx: RefContext =
    parentCtx !== U
      ? parentCtx
      : {
          root: jsonSchema,
          ph: {},
          built: {},
          cyc: {},
          defs: {},
          names: { [jsonName]: true },
        };

  for (let i = 0; i < unsupportedKeywords.length; i++) {
    const keyword = unsupportedKeywords[i]!;
    if ((jsonSchema as Record<string, unknown>)[keyword] !== U) {
      throw new SuryError({
        code: "invalid_operation",
        path: pathEmpty,
        reason: `Unsupported JSON Schema keyword: ${keyword}. Ignoring it would accept data the schema rejects — remove it, or express the constraint with S.refine on the result`,
      });
    }
  }

  // The dispatch order of this chain is mirrored by `JSONSchemaResolve` in
  // src/types/json.d.ts — reordering branches here changes which keyword wins a
  // conflict, so the type-level chain must move with it.
  let schema: Internal;
  if (jsonSchema.nullable) {
    schema = null_(fromJSONSchema(jsonSchemaMerge(jsonSchema, { nullable: false }), ctx));
  } else if (jsonSchema["$ref"] !== U) {
    // A `$ref` replaces the assertion keywords beside it, which is draft-07's
    // and OpenAPI 3.0's reading; draft-2019-09 made it assert alongside them.
    // The two disagree and this converter takes no dialect, so it follows the
    // spelling that documents are actually written in — a sibling next to a
    // `$ref` is written to be ignored, because that is what draft-07 validators
    // did. `nullable` above and the composition keywords below still layer on.
    schema = resolveRef(jsonSchema["$ref"], ctx);
  } else if (jsonSchema.type === "object") {
    if (jsonSchema.properties !== U) {
      const properties = jsonSchema.properties;
      // Null prototype: a JSON Schema may declare a property named `__proto__`,
      // and on a plain `{}` that assignment replaces the object's prototype
      // instead of adding a key.
      const obj: Record<string, Internal> = Object.create(null);
      Object.keys(properties).forEach((key) => {
        const property = properties[key]!;
        let propertySchema = jsonDefinitionToSchema(property, ctx);
        if (!jsonSchema.required?.includes(key)) {
          const defaultValue = definitionToDefaultValue(property);
          if (defaultValue !== U) {
            propertySchema = Option_getOr(option(propertySchema), defaultValue);
          } else {
            propertySchema = option(propertySchema);
          }
        }
        obj[key] = propertySchema;
      });
      schema = definitionToSchema(obj);
      if (jsonSchema.additionalProperties === false) {
        schema = strict(schema);
      }
    } else {
      const additionalProperties = jsonSchema.additionalProperties;
      if (additionalProperties !== U) {
        if (additionalProperties === true) {
          schema = dict(anySchema);
        } else if (additionalProperties === false) {
          schema = strict(object(() => {}));
        } else {
          schema = dict(fromJSONSchema(additionalProperties, ctx));
        }
      } else {
        schema = schemaFactory({});
      }
    }

    // TODO: jsonSchema.anyOf and jsonSchema.oneOf support
  } else if (jsonSchema.type === "array") {
    if (jsonSchema.prefixItems !== U) {
      // draft-2020-12 describes tuples with `prefixItems` instead of an
      // `items` array.
      const prefixItems = jsonSchema.prefixItems;
      schema = tuple((s: { item: (idx: number, schema: Internal) => unknown }) =>
        prefixItems.map((d, idx) => s.item(idx, jsonDefinitionToSchema(d, ctx)))
      );
    } else if (jsonSchema.items !== U) {
      const items = jsonSchema.items;
      if (Array.isArray(items)) {
        schema = tuple((s: { item: (idx: number, schema: Internal) => unknown }) =>
          items.map((d, idx) => s.item(idx, jsonDefinitionToSchema(d, ctx)))
        );
      } else {
        schema = array(jsonDefinitionToSchema(items, ctx));
      }
    } else {
      schema = array(anySchema);
    }
    if (jsonSchema.minItems !== U) {
      schema = applyBound(schema, minLength, jsonSchema.minItems);
    }
    if (jsonSchema.maxItems !== U) {
      schema = applyBound(schema, maxLength, jsonSchema.maxItems);
    }
  } else if (jsonSchema.anyOf !== U) {
    const definitions = jsonSchema.anyOf;
    if (definitions.length === 0) {
      schema = anySchema;
    } else if (definitions.length === 1) {
      schema = jsonDefinitionToSchema(definitions[0]!, ctx);
    } else {
      schema = union(definitions.map((d) => jsonDefinitionToSchema(d, ctx)));
    }
  // needs to come before primitives
  } else if (jsonSchema.enum !== U) {
    const primitives = jsonSchema.enum;
    if (primitives.length === 0) {
      schema = anySchema;
    } else if (primitives.length === 1) {
      schema = primitiveToSchema(primitives[0]);
    } else {
      schema = union(primitives.map(primitiveToSchema));
    }
  } else if (jsonSchema.const !== U) {
    schema = primitiveToSchema(jsonSchema.const);
  } else if (Array.isArray(jsonSchema.type)) {
    const types = jsonSchema.type;
    schema = union(
      types.map((type) => fromJSONSchema(jsonSchemaMerge(jsonSchema, { type }), ctx))
    );
  } else if (jsonSchema.type === "string") {
    if (jsonSchema.format === "email") {
      schema = email;
    } else if (jsonSchema.format === "uri") {
      schema = url;
    } else if (jsonSchema.format === "uuid") {
      schema = uuid;
    } else if (jsonSchema.format === "date-time") {
      schema = isoDateTime;
    } else {
      schema = string;
    }
    if (jsonSchema.pattern !== U) {
      schema = pattern(schema, new RegExp(jsonSchema.pattern));
    }
    if (jsonSchema.minLength !== U) {
      schema = applyBound(schema, minLength, jsonSchema.minLength);
    }
    if (jsonSchema.maxLength !== U) {
      schema = applyBound(schema, maxLength, jsonSchema.maxLength);
    }
  } else if (jsonSchema.type === "integer") {
    schema = toIntSchema(jsonSchema);
  } else if (jsonSchema.type === "number" && jsonSchema.format === "int64") {
    schema = toIntSchema(jsonSchema);
  } else if (jsonSchema.type === "number" && jsonSchema.multipleOf === 1) {
    schema = toIntSchema(jsonSchema);
  } else if (jsonSchema.type === "number") {
    schema = withNumericBounds(float, jsonSchema);
  } else if (jsonSchema.type === "boolean") {
    schema = bool;
  } else if (jsonSchema.type === "null") {
    schema = schemaFactory(null);
  } else if (jsonSchema.type !== U) {
    throw new SuryError({
      code: "invalid_operation",
      path: pathEmpty,
      reason: `Unsupported JSON Schema type: ${jsonSchema.type}`,
    });
  } else {
    // No `type`, but the assertion keywords still bind — each to its own JSON
    // type, and vacuously to every other. Recursing with the type pinned reuses
    // the branches above; the runtime guard is what keeps it vacuous.
    const guarded: [string, Internal][] = [];
    for (let i = 0; i < keywordTypes.length; i++) {
      const [type, keywords] = keywordTypes[i]!;
      if (keywords.some((k) => (jsonSchema as Record<string, unknown>)[k] !== U)) {
        guarded.push([type, asAssertion(jsonSchemaMerge(jsonSchema, { type }), ctx)]);
      }
    }
    schema =
      guarded.length === 0
        ? anySchema
        : refine(
            anySchema,
            (data: unknown) => {
              const type = jsonTypeOf(data);
              return guarded.every(
                ([guardType, guardSchema]) =>
                  type !== guardType || passesSchema(data, guardSchema)
              );
            },
            "Should pass the schema's assertion keywords for its type."
          );
  }

  // Composition keywords constrain *in addition to* everything above, so they
  // layer on as refinements rather than replacing the shape — a schema is not
  // either "an object with these properties" or "an allOf", it is both.
  if (jsonSchema.allOf !== U) {
    const definitions = jsonSchema.allOf;
    const schemas = definitions.map((d) => asAssertion(d, ctx));
    if (schemas.length > 0) {
      schema = refineInput(
        schema,
        (data: unknown) => schemas.every((s) => passesSchema(data, s)),
        "Should pass for all schemas of the allOf property."
      );
    }
  }
  if (jsonSchema.oneOf !== U) {
    const definitions = jsonSchema.oneOf;
    const schemas = definitions.map((d) => asAssertion(d, ctx));
    if (schemas.length > 0) {
      schema = refineInput(
        schema,
        (data: unknown) =>
          schemas.filter((s) => passesSchema(data, s)).length === 1,
        "Should pass exactly one schema according to the oneOf property."
      );
    }
  }
  if (jsonSchema.not !== U) {
    const notSchema = asAssertion(jsonSchema.not, ctx);
    schema = refineInput(
      schema,
      (data: unknown) => !passesSchema(data, notSchema),
      "Should NOT be valid against schema in the not property."
    );
  }
  if (jsonSchema.if !== U) {
    // `then`/`else` default to "always passes" when absent.
    const ifSchema = asAssertion(jsonSchema.if, ctx);
    const thenSchema =
      jsonSchema.then !== U ? asAssertion(jsonSchema.then, ctx) : U;
    const elseSchema =
      jsonSchema.else !== U ? asAssertion(jsonSchema.else, ctx) : U;
    schema = refineInput(
      schema,
      (data: unknown) => {
        const branch = passesSchema(data, ifSchema) ? thenSchema : elseSchema;
        return branch === U || passesSchema(data, branch);
      },
      "Should pass the if/then/else schema validation."
    );
  }

  if (
    jsonSchema.description !== U ||
    jsonSchema.deprecated !== U ||
    jsonSchema.examples !== U ||
    jsonSchema.title !== U
  ) {
    schema = meta(schema, {
      title: jsonSchema.title,
      description: jsonSchema.description,
      deprecated: jsonSchema.deprecated,
      examples: jsonSchema.examples,
    });
  }

  // `cyc` rather than `defs`, which `withDefs` also folds unrelated defs into:
  // what decides whether the outermost schema needs one is whether the document
  // had a cycle to name.
  if (parentCtx === U && Object.keys(ctx.cyc).length !== 0) {
    schema = withDefs(schema, ctx);
  }

  return schema;
}

// PORT-NOTE: every one of these is a PURE NO-OP — a bare `Obj.magic` (or
// `castToPublic` for `unknown`) that re-types an existing function/value from
// its `internal`-returning form to the public `t<'x>`-returning form without
// touching the runtime value. In this TS port the runtime object is `Internal`
// everywhere and the public typing lives in the bindings layer, so NO runtime
// code is emitted for any of them. Listed for completeness (all no-ops):
//
//   nullAsUnit, never_, unknown (castToPublic of the `unknown` schema const),
//   unit, nullLiteral, nan, string, bool, int, float, bigint, symbol, date,
//   json, jsonString, jsonStringWithSpace, uint8Array, isoDateTime, port,
//   email, uuid, cuid, url
//
// The bindings layer (Sury.res / index.d.ts) should re-export the already-defined
// functions of the same names under their public types.
