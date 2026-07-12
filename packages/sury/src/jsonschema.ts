import { definitionToSchema, schemaFactory } from "./factory";
import { array, option } from "./composites";
import { email, isoDateTime, json, meta, url, uuid } from "./formats";
import { arrayLength, arrayMaxLength, arrayMinLength, dict, floatMax, floatMin, intMax, intMin, null_, object, pattern, stringLength, stringMaxLength, stringMinLength, tuple, union } from "./refinements";
import { SuryError, baseSchema, getOrRethrow, panic, unknown } from "./schema";
import { Literal_parse, bool, float, int, jsonName, string } from "./primitives";
import { B_makeInvalidInputDetails, B_operationArg } from "./builder";
import { never_, parse, reverse } from "./parse";
import { Internal, isLiteral, isOptional, toExpression } from "./types";
import { Path, pathConcat, pathDynamic, pathEmpty, pathFromLocation } from "./path";
import { flagNone, flagUnsafeHas } from "./flags";
import { arrayTag, booleanTag, neverTag, nullTag, numberTag, objectTag, refTag, stringTag, tagFlagArray, tagFlagObject, tagFlagUnion, tagFlags, undefinedTag, unionTag, unknownTag } from "./tags";
import { Metadata_Id_internal, Metadata_get, Metadata_set, Option_getOr, assertOrThrow, defsPath, refine, __setStandardJSONSchemaConverter, strict } from "./operations";

// PORT-NOTE: no runtime values had to be imported from JSONSchema.res or
// StandardSchema.res — everything runtime-relevant there is `%identity`
// externals (Arrayable.single/array, Mutable.fromReadOnly/toReadOnly,
// Result casts) or `Object.assign` (Mutable.mixin), all inlined below.
// Their types are ported as loose TS aliases with the RUNTIME field names
// (`$ref`, `$schema`, `$defs`, `type`, `if`, `else` — the `@as(...)` names,
// not the ReScript field names `ref`/`schema`/`defs`/`type_`/`if_`/`else_`).
// =============================================================================

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
export type JSONSchemaArrayable<Item> = Item | Item[];

// PORT-NOTE: JSONSchema's `definition` is `@unboxed
// Schema(t) | @as(false) Never | @as(true) Any` — at runtime a definition is
// the schema object itself, `false`, or `true`. The `Schema(...)` wrapping
// at construction sites is a no-op and is dropped; `Never` -> `false`,
// `Any` -> `true`; the `Schema(t)` pattern -> `typeof d !== "boolean"`.
export type JSONSchemaDefinition = JSONSchemaT | boolean;

/**
 * JSON Schema v7
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
  exclusiveMaximum?: number;
  minimum?: number;
  exclusiveMinimum?: number;
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
  maxItems?: number;
  minItems?: number;
  uniqueItems?: boolean;
  contains?: JSONSchemaT;
  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-6.5
   */
  maxProperties?: number;
  minProperties?: number;
  required?: string[];
  properties?: Record<string, JSONSchemaDefinition>;
  patternProperties?: Record<string, JSONSchemaDefinition>;
  additionalProperties?: JSONSchemaDefinition;
  dependencies?: Record<string, unknown>;
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
  if (schema.description !== undefined) {
    jsonSchema.description = schema.description;
  }
  if (schema.title !== undefined) {
    jsonSchema.title = schema.title;
  }
  if (schema.deprecated !== undefined) {
    jsonSchema.deprecated = schema.deprecated;
  }
  if (schema.examples !== undefined) {
    // If a schema is Jsonable, then examples are Jsonable too.
    jsonSchema.examples = schema.examples;
  }
  if (schema["$defs"] !== undefined) {
    Object.assign(defs, schema["$defs"]);
  }
  const metadataRawSchema = Metadata_get(schema, jsonSchemaMetadataId) as
    | JSONSchemaT
    | undefined;
  if (metadataRawSchema !== undefined) {
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
  const input = B_operationArg(unknown, reversed, flagNone, undefined);
  try {
    const output = parse(input);
    // The parse produces a val whose .schema reflects the
    // JSON-compatible transformed structure.
    return internalToJSONSchema(output.s, path, defs, parent, target);
  } catch (exn) {
    getOrRethrow(exn);

    // Parse failed — caller falls through to normal tag-based logic.
    return undefined;
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
    : undefined;
  if (encoded !== undefined) {
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
    if (target === "openapi-3.0") {
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
    if (schema.minLength !== undefined) {
      jsonSchema.minLength = schema.minLength;
    }
    if (schema.maxLength !== undefined) {
      jsonSchema.maxLength = schema.maxLength;
    }
    if (schema.pattern !== undefined) {
      jsonSchema.pattern = schema.pattern.source;
    }
    if (const_ !== undefined) {
      setConstOrEnum(const_);
    }
  } else if (tag === numberTag) {
    const format = schema.format;
    const const_ = schema.const as number | undefined;
    if (format === "int32") {
      jsonSchema.type = "integer";
      jsonSchema.minimum = -2147483648;
      jsonSchema.maximum = 2147483647;
    } else if (format === "port") {
      jsonSchema.type = "integer";
      jsonSchema.minimum = 0;
      jsonSchema.maximum = 65535;
    } else {
      jsonSchema.type = "number";
    }
    if (schema.minimum !== undefined) {
      jsonSchema.minimum = schema.minimum;
    }
    if (schema.maximum !== undefined) {
      jsonSchema.maximum = schema.maximum;
    }
    if (const_ !== undefined) {
      setConstOrEnum(const_);
    }
  } else if (tag === booleanTag) {
    const const_ = schema.const as boolean | undefined;
    jsonSchema.type = "boolean";
    if (const_ !== undefined) {
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
      if (schema.minItems !== undefined) {
        jsonSchema.minItems = schema.minItems;
      }
      if (schema.maxItems !== undefined) {
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
      if (target === "openapi-3.0") {
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
  } else if (tag === unionTag) {
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

    if (schema.default !== undefined) {
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
        } else if (t.enum !== undefined && t.enum.length === 1 && t.enum[0] === null) {
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
      target === "openapi-3.0" &&
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
    if (target === "openapi-3.0") {
      // OpenAPI 3.0 has no `null` type. Use an enum as a workaround.
      jsonSchema.enum = [null];
    } else {
      jsonSchema.type = "null";
    }
  } else if (tag === neverTag) {
    jsonSchema.not = {};
  } else {
    throw new SuryError(
      B_makeInvalidInputDetails(
        // Just needs `.name` for the message - avoid json()'s recursive union.
        (() => {
          const s = baseSchema(unknownTag, false);
          s.name = jsonName;
          return s;
        })(),
        flagUnsafeHas(tagFlags[parent.type]!, tagFlagUnion) ? parent : schema,
        path,
        undefined,
        false
      )
    );
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
    case "openapi-3.0":
      return undefined;
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

export const toJSONSchema = (schema: Internal, options?: toJSONSchemaOptions): JSONSchemaT => {
  // Resolve the target and the `$schema` URI to stamp. When no options object is
  // provided we keep the historical behavior: default to "draft-07" and do NOT
  // stamp `$schema`. With options, an unsupported target throws up front (even
  // for openapi-3.0, which stamps no `$schema`).
  let target: JsonSchemaTarget;
  let schemaUri: string | undefined;
  if (options !== undefined) {
    target = options.target !== undefined ? options.target : "draft-07";
    schemaUri = targetSchemaUri(target);
  } else {
    target = "draft-07";
    schemaUri = undefined;
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
  if (schemaUri !== undefined) {
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

export const extendJSONSchema = (schema: Internal, jsonSchema: JSONSchemaT): Internal => {
  const existingSchemaExtend = Metadata_get(schema, jsonSchemaMetadataId) as
    | JSONSchemaT
    | undefined;
  return Metadata_set(
    schema,
    jsonSchemaMetadataId,
    existingSchemaExtend !== undefined
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

const toIntSchema = (jsonSchema: JSONSchemaT): Internal => {
  let schema = int();
  // TODO: Support jsonSchema.multipleOf
  if (jsonSchema.minimum !== undefined) {
    schema = intMin(schema, jsonSchema.minimum | 0);
  } else if (jsonSchema.exclusiveMinimum !== undefined) {
    schema = intMin(schema, (jsonSchema.exclusiveMinimum + 1) | 0);
  }
  if (jsonSchema.maximum !== undefined) {
    schema = intMax(schema, jsonSchema.maximum | 0);
  } else if (jsonSchema.exclusiveMinimum !== undefined) {
    schema = intMax(schema, (jsonSchema.exclusiveMinimum - 1) | 0);
  }
  return schema;
}

const definitionToDefaultValue = (definition: JSONSchemaDefinition): unknown => {
  if (typeof definition !== "boolean") {
    return definition.default;
  } else {
    return undefined;
  }
}

export const fromJSONSchema = (jsonSchema: JSONSchemaT): Internal => {
  const anySchema = json();

  const jsonDefinitionToSchema = (definition: JSONSchemaDefinition): Internal => {
    if (typeof definition !== "boolean") {
      return fromJSONSchema(definition);
    } else if (definition === true) {
      return anySchema;
    } else {
      return never_();
    }
  };

  let schema: Internal;
  if (jsonSchema.nullable) {
    schema = null_(fromJSONSchema(jsonSchemaMerge(jsonSchema, { nullable: false })));
  } else if (jsonSchema.type === "object") {
    if (jsonSchema.properties !== undefined) {
      const properties = jsonSchema.properties;
      const obj: Record<string, Internal> = {};
      Object.keys(properties).forEach((key) => {
        const property = properties[key]!;
        let propertySchema = jsonDefinitionToSchema(property);
        if (!jsonSchema.required?.includes(key)) {
          const defaultValue = definitionToDefaultValue(property);
          if (defaultValue !== undefined) {
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
      if (additionalProperties !== undefined) {
        if (additionalProperties === true) {
          schema = dict(anySchema);
        } else if (additionalProperties === false) {
          schema = strict(object(() => {}));
        } else {
          schema = dict(fromJSONSchema(additionalProperties));
        }
      } else {
        schema = schemaFactory({});
      }
    }

    // TODO: jsonSchema.anyOf and jsonSchema.oneOf support
  } else if (jsonSchema.type === "array") {
    if (jsonSchema.prefixItems !== undefined) {
      // draft-2020-12 describes tuples with `prefixItems` instead of an
      // `items` array.
      const prefixItems = jsonSchema.prefixItems;
      schema = tuple((s: { item: (idx: number, schema: Internal) => unknown }) =>
        prefixItems.map((d, idx) => s.item(idx, jsonDefinitionToSchema(d)))
      );
    } else if (jsonSchema.items !== undefined) {
      const items = jsonSchema.items;
      if (Array.isArray(items)) {
        schema = tuple((s: { item: (idx: number, schema: Internal) => unknown }) =>
          items.map((d, idx) => s.item(idx, jsonDefinitionToSchema(d)))
        );
      } else {
        schema = array(jsonDefinitionToSchema(items));
      }
    } else {
      schema = array(anySchema);
    }
    if (jsonSchema.minItems !== undefined) {
      schema = arrayMinLength(schema, jsonSchema.minItems);
    }
    if (jsonSchema.maxItems !== undefined) {
      schema = arrayMaxLength(schema, jsonSchema.maxItems);
    }
  } else if (jsonSchema.anyOf !== undefined) {
    const definitions = jsonSchema.anyOf;
    if (definitions.length === 0) {
      schema = anySchema;
    } else if (definitions.length === 1) {
      schema = jsonDefinitionToSchema(definitions[0]!);
    } else {
      schema = union(definitions.map(jsonDefinitionToSchema));
    }
  } else if (jsonSchema.allOf !== undefined) {
    const definitions = jsonSchema.allOf;
    if (definitions.length === 0) {
      schema = anySchema;
    } else if (definitions.length === 1) {
      schema = jsonDefinitionToSchema(definitions[0]!);
    } else {
      schema = refine(
        anySchema,
        (data: unknown) => {
          return definitions.every((d) => {
            try {
              assertOrThrow(data, jsonDefinitionToSchema(d));
              return true;
            } catch (_) {
              return false;
            }
          });
        },
        "Should pass for all schemas of the allOf property."
      );
    }
  } else if (jsonSchema.oneOf !== undefined) {
    const definitions = jsonSchema.oneOf;
    if (definitions.length === 0) {
      schema = anySchema;
    } else if (definitions.length === 1) {
      schema = jsonDefinitionToSchema(definitions[0]!);
    } else {
      schema = refine(
        anySchema,
        (data: unknown) => {
          let validCount = 0;
          definitions.forEach((d) => {
            try {
              assertOrThrow(data, jsonDefinitionToSchema(d));
              validCount = validCount + 1;
            } catch {
              // Not valid against this definition — doesn't count towards validCount.
            }
          });
          return validCount === 1;
        },
        "Should pass exactly one schema according to the oneOf property."
      );
    }
  } else if (jsonSchema.not !== undefined) {
    const not = jsonSchema.not;
    schema = refine(
      anySchema,
      (data: unknown) => {
        try {
          assertOrThrow(data, jsonDefinitionToSchema(not));
          return false;
        } catch (_) {
          return true;
        }
      },
      "Should NOT be valid against schema in the not property."
    );
    // needs to come before primitives
  } else if (jsonSchema.enum !== undefined) {
    const primitives = jsonSchema.enum;
    if (primitives.length === 0) {
      schema = anySchema;
    } else if (primitives.length === 1) {
      schema = primitiveToSchema(primitives[0]);
    } else {
      schema = union(primitives.map(primitiveToSchema));
    }
  } else if (jsonSchema.const !== undefined) {
    schema = primitiveToSchema(jsonSchema.const);
  } else if (Array.isArray(jsonSchema.type)) {
    const types = jsonSchema.type;
    schema = union(types.map((type) => fromJSONSchema(jsonSchemaMerge(jsonSchema, { type }))));
  } else if (jsonSchema.type === "string") {
    if (jsonSchema.format === "email") {
      schema = email();
    } else if (jsonSchema.format === "uri") {
      schema = url();
    } else if (jsonSchema.format === "uuid") {
      schema = uuid();
    } else if (jsonSchema.format === "date-time") {
      schema = isoDateTime();
    } else {
      schema = string();
    }
    if (jsonSchema.pattern !== undefined) {
      schema = pattern(schema, new RegExp(jsonSchema.pattern));
    }
    if (jsonSchema.minLength !== undefined) {
      schema = stringMinLength(schema, jsonSchema.minLength);
    }
    if (jsonSchema.maxLength !== undefined) {
      schema = stringMaxLength(schema, jsonSchema.maxLength);
    }
  } else if (jsonSchema.type === "integer") {
    schema = toIntSchema(jsonSchema);
  } else if (jsonSchema.type === "number" && jsonSchema.format === "int64") {
    schema = toIntSchema(jsonSchema);
  } else if (jsonSchema.type === "number" && jsonSchema.multipleOf === 1) {
    schema = toIntSchema(jsonSchema);
  } else if (jsonSchema.type === "number") {
    schema = float();
    if (jsonSchema.minimum !== undefined) {
      schema = floatMin(schema, jsonSchema.minimum);
    } else if (jsonSchema.exclusiveMinimum !== undefined) {
      schema = floatMin(schema, jsonSchema.exclusiveMinimum + 1);
    }
    if (jsonSchema.maximum !== undefined) {
      schema = floatMax(schema, jsonSchema.maximum);
    } else if (jsonSchema.exclusiveMinimum !== undefined) {
      schema = floatMax(schema, jsonSchema.exclusiveMinimum - 1);
    }
  } else if (jsonSchema.type === "boolean") {
    schema = bool();
  } else if (jsonSchema.type === "null") {
    schema = schemaFactory(null);
  } else if (
    jsonSchema.if !== undefined &&
    jsonSchema.then !== undefined &&
    jsonSchema.else !== undefined
  ) {
    const ifSchema = jsonDefinitionToSchema(jsonSchema.if);
    const thenSchema = jsonDefinitionToSchema(jsonSchema.then);
    const elseSchema = jsonDefinitionToSchema(jsonSchema.else);
    schema = refine(
      anySchema,
      (data: unknown) => {
        let passed;
        try {
          assertOrThrow(data, ifSchema);
          passed = true;
        } catch (_) {
          passed = false;
        }
        try {
          if (passed) {
            assertOrThrow(data, thenSchema);
          } else {
            assertOrThrow(data, elseSchema);
          }
          return true;
        } catch (_) {
          return false;
        }
      },
      "Should pass the if/then/else schema validation."
    );
  } else if (jsonSchema.type !== undefined) {
    throw new SuryError({
      code: "invalid_operation",
      path: pathEmpty,
      reason: `Unsupported JSON Schema type: ${jsonSchema.type}`,
    });
  } else {
    schema = anySchema;
  }

  if (
    jsonSchema.description !== undefined ||
    jsonSchema.deprecated !== undefined ||
    jsonSchema.examples !== undefined ||
    jsonSchema.title !== undefined
  ) {
    schema = meta(schema, {
      title: jsonSchema.title,
      description: jsonSchema.description,
      deprecated: jsonSchema.deprecated,
      examples: jsonSchema.examples,
    });
  }

  return schema;
}

export const min = (schema: Internal, minValue: number, maybeMessage?: string): Internal => {
  switch (schema.type) {
    case stringTag:
      return stringMinLength(schema, minValue, maybeMessage);
    case arrayTag:
      return arrayMinLength(schema, minValue, maybeMessage);
    case numberTag:
      return schema.format === "int32" || schema.format === "port"
        ? intMin(schema, minValue, maybeMessage)
        : floatMin(schema, minValue, maybeMessage);
    default:
      return panic(
        `S.min is not supported for ${toExpression(schema)} schema. Coerce the schema to string, number or array using S.to first.`
      );
  }
}

export const max = (schema: Internal, maxValue: number, maybeMessage?: string): Internal => {
  switch (schema.type) {
    case stringTag:
      return stringMaxLength(schema, maxValue, maybeMessage);
    case arrayTag:
      return arrayMaxLength(schema, maxValue, maybeMessage);
    case numberTag:
      return schema.format === "int32" || schema.format === "port"
        ? intMax(schema, maxValue, maybeMessage)
        : floatMax(schema, maxValue, maybeMessage);
    default:
      return panic(
        `S.max is not supported for ${toExpression(schema)} schema. Coerce the schema to string, number or array using S.to first.`
      );
  }
}

export const length = (schema: Internal, length: number, maybeMessage?: string): Internal => {
  switch (schema.type) {
    case stringTag:
      return stringLength(schema, length, maybeMessage);
    case arrayTag:
      return arrayLength(schema, length, maybeMessage);
    default:
      return panic(
        `S.length is not supported for ${toExpression(schema)} schema. Coerce the schema to string or array using S.to first.`
      );
  }
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
// The bindings layer (Sury.res / S.d.ts) should re-export the already-defined
// functions of the same names under their public types.
