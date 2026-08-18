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
import {
  array,
  arrayDecoder,
  dict,
  objectDecoder,
  option,
} from "./composites";
import { schemaFactory } from "./factory";
import {
  meta,
  Metadata_get,
  Metadata_Id_internal,
  Metadata_set,
  Option_getOr,
  deepStrict,
  refineInput,
} from "./modifiers";
import { __setStandardJSONSchemaConverter, assertOrThrow } from "./operations";
import { never_, parse, reverse } from "./parse";
import { bool, float, integer, Literal_parse, string } from "./primitives";
import {
  duration,
  email,
  gt,
  gte,
  hostname,
  idnEmail,
  idnHostname,
  ipv4,
  ipv6,
  iri,
  iriReference,
  isoDate,
  isoDateTime,
  isoTime,
  jsonPointer,
  lt,
  lte,
  maxLength,
  minLength,
  multipleOf,
  null_,
  pattern,
  relativeJsonPointer,
  union,
  uri,
  uriReference,
  uriTemplate,
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
 * splits per dialect for `inputJSONSchema`'s result). A keyword added to one
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
const draft07Uri = "http://json-schema.org/draft-07/schema#";
const draft2020Uri = "https://json-schema.org/draft/2020-12/schema";

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

const isAnyJSONSchema = (definition: JSONSchemaDefinition | undefined): boolean =>
  definition === true || (!!definition && Object.keys(definition).length === 0);

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
    // String formats store the JSON Schema name verbatim, so they pass
    // through. Only `cuid` and `json` have no JSON Schema equivalent — a
    // denylist of the two costs less than an allowlist of the eighteen, and
    // stays flat as formats are added.
    if (format !== U && format !== "cuid" && format !== "json") {
      jsonSchema.format = format;
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
    jsonSchema.type = format !== U ? "integer" : "number";
    if (schema.multipleOf !== U) {
      jsonSchema.multipleOf = schema.multipleOf as number;
    }
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
    if (items.length === 0 && typeof additionalItems === "object") {
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
      let minItems = itemsNumber;
      if (typeof additionalItems === "object") {
        while (minItems > 0 && isOptional(items[minItems - 1]!)) minItems--;
      }

      jsonSchema.type = "array";
      if (schema.minItems !== U || minItems !== 0) {
        jsonSchema.minItems = schema.minItems ?? minItems;
      }
      if (schema.maxItems !== U || typeof additionalItems !== "object") {
        jsonSchema.maxItems = schema.maxItems ?? itemsNumber;
      }
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
      if (typeof additionalItems === "object") {
        if (additionalItems.type === neverTag) {
          if (target === "draft-2020-12") jsonSchema.items = false;
          else if (target !== openApi30) jsonSchema.additionalItems = false;
        } else {
          const rest = internalToJSONSchema(
            additionalItems,
            pathConcat(path, pathDynamic),
            defs,
            schema,
            target
          );
          if (Object.keys(rest).length !== 0) {
            if (target === "draft-2020-12") jsonSchema.items = rest;
            else if (target !== openApi30) jsonSchema.additionalItems = rest;
          }
        }
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
      if (
        !(
          childSchema.type === undefinedTag &&
          (parent.type === objectTag ||
            (parent.type === arrayTag &&
              typeof parent.additionalItems === "object" &&
              parent.items!.includes(schema)))
        )
      ) {
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
    const required: string[] = [];
    const jsonProperties: Record<string, JSONSchemaDefinition> = Object.create(null);
    const propertyKeys = Object.keys(properties);
    propertyKeys.forEach((key) => {
      const itemSchema = properties[key]!;
      if (!isOptional(itemSchema)) required.push(key);
      jsonProperties[key] = internalToJSONSchema(
        itemSchema,
        pathConcat(path, pathFromLocation(key)),
        defs,
        schema,
        target
      );
    });

    jsonSchema.type = "object";
    if (Object.keys(jsonProperties).length !== 0 || typeof additionalItems !== "object") {
      jsonSchema.properties = jsonProperties;
    }
    if (typeof additionalItems === "object") {
      const rest = internalToJSONSchema(
        additionalItems,
        pathConcat(path, pathDynamic),
        defs,
        schema,
        target
      );
      if (Object.keys(rest).length !== 0) jsonSchema.additionalProperties = rest;
    } else if (additionalItems === "strict") {
      jsonSchema.additionalProperties = false;
    }
    if (required.length !== 0) jsonSchema.required = required;
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

export type JSONSchemaOptions = { target?: JsonSchemaTarget };

// Single source of truth for the `target` -> `$schema` URI mapping (mirrors
// @valibot/to-json-schema). Returns the URI to stamp, or `None` when the target
// has no `$schema` (openapi-3.0). Raises an `invalid_operation` error for
// `Unknown` (an unsupported target, e.g. one that arrived as an arbitrary
// string from JS via the Standard JSON Schema `Options`).
const targetSchemaUri = (target: JsonSchemaTarget): string | undefined => {
  switch (target) {
    case "draft-07":
      return draft07Uri;
    case "draft-2020-12":
      return draft2020Uri;
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
export const inputJSONSchema = (schema: Internal, options?: JSONSchemaOptions): JSONSchemaT => {
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
  if (options !== U) delete jsonSchema.$schema;
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

// `S.reverse` swaps Input <-> Output, and the conversion always describes the
// input type of what it receives.
// @__NO_SIDE_EFFECTS__
export const outputJSONSchema = (schema: Internal, options?: JSONSchemaOptions): JSONSchemaT =>
  inputJSONSchema(reverse(schema), options);

// Wiring this inside a function (vs top level) is what makes the converter and
// `reverse` tree-shakeable.
//
// Mirrors @valibot/to-json-schema's `toStandardJsonSchema`: the `target` option
// selects the JSON Schema dialect (and the stamped `$schema` URI), and an
// unsupported target throws.
export const enableStandardJSONSchema = (): void => {
  __setStandardJSONSchemaConverter((schema, options, isOutput) => {
    // The converter just forwards the target; the conversion is the single
    // source of truth for the `$schema` URI mapping and the unsupported-target
    // throw. Passing an options object (vs none) is what makes it stamp
    // `$schema`, which the Standard JSON Schema spec requires.
    return (isOutput ? outputJSONSchema : inputJSONSchema)(schema, { target: options.target });
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

// `const`/`enum` values. An object or array goes through schemaFactory (what
// `S.literal` is) so it becomes a structural schema whose fields are literals,
// matching the document's meaning — a deep comparison. Literal_parse would
// make it an instance literal compared by reference, which rejects every value
// but the one the document object itself was built from.
const primitiveToSchema = (primitive: unknown): Internal =>
  primitive !== null && typeof primitive === "object"
    ? // deepStrict because `const` is equality, not a shape: an object with an
      // extra property is a different value, where a plain object schema would
      // ignore the extra and accept it. Cloned first: schemaFactory converts
      // its definition in place, and this one belongs to the caller's
      // document (which is JSON, so the round-trip is lossless).
      deepStrict(schemaFactory(JSON.parse(JSON.stringify(primitive))))
    : Literal_parse(primitive);

// The inverse of the format pass-through in inputJSONSchema. Every format Sury can
// emit has to round-trip back to the schema that emitted it, so a format added
// on one side without the other is a reversibility bug. A record rather than a
// branch chain: reaching fromJSONSchema at all means wanting the whole
// vocabulary, so there is nothing here for a bundler to drop anyway.
//
// Null-prototype because the key is attacker-controlled: `format: "constructor"`
// against a plain literal resolves up the chain to a truthy function, which then
// flows on as if it were a schema instead of falling back to `string`.
const stringFormatSchemas = {
  __proto__: null,
  "date-time": isoDateTime,
  date: isoDate,
  time: isoTime,
  duration: duration,
  email: email,
  "idn-email": idnEmail,
  hostname: hostname,
  "idn-hostname": idnHostname,
  ipv4: ipv4,
  ipv6: ipv6,
  uri: uri,
  "uri-reference": uriReference,
  "uri-template": uriTemplate,
  iri: iri,
  "iri-reference": iriReference,
  uuid: uuid,
  "json-pointer": jsonPointer,
  "relative-json-pointer": relativeJsonPointer,
} as unknown as Record<string, Internal | undefined>;

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
  // `multipleOf: 1` on an integer schema restates what the format already
  // checks — storing it would emit a keyword the author's document may not
  // have had (the int-schema branches synthesize integer from other spellings).
  if (jsonSchema.multipleOf !== U && !(schema.format !== U && jsonSchema.multipleOf === 1)) {
    schema = applyBound(schema, multipleOf, jsonSchema.multipleOf);
  }
  return schema;
}

const toIntSchema = (jsonSchema: JSONSchemaT): Internal => withNumericBounds(integer, jsonSchema);

// Assertion keywords Sury doesn't model. Silently ignoring one widens the
// schema — the validator then accepts data the author wrote the keyword to
// reject — so creation fails instead. Annotations (`title`, `default`,
// `$comment`, …) are ignored on purpose and stay out of this list.
const unsupportedKeywords = [
  "$dynamicRef",
  "$recursiveRef",
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
];

// Which JSON type each assertion keyword constrains. A keyword says nothing
// about an instance of any other type — `{minLength: 3}` accepts `42` — so a
// schema without `type` has to apply each group only to its own type.
const keywordTypes: [JSONSchemaTypeName, string[]][] = [
  ["string", ["pattern", "minLength", "maxLength"]],
  ["number", ["minimum", "maximum", "exclusiveMinimum", "exclusiveMaximum", "multipleOf"]],
  ["object", ["properties", "required", "additionalProperties"]],
  ["array", ["items", "prefixItems", "minItems", "maxItems"]],
];

// The keywords that layer on top of a base type rather than describing one.
// Pinning a type — for a `type` array member, or for an untyped document's
// per-type pass — has to drop them, or every member re-applies the whole
// document.
const layeredKeywords = [
  "nullable",
  "enum",
  "const",
  "allOf",
  "anyOf",
  "oneOf",
  "not",
  "if",
  "then",
  "else",
  "default",
  "title",
  "description",
  "deprecated",
  "examples",
];

// `required` names keys the shape around it can't make mandatory — a `dict`,
// or an object whose `additionalProperties` schema keeps it one.
const withRequired = (schema: Internal, required: string[]): Internal =>
  refineInput(
    schema,
    (data: unknown) => required.every((key) => Object.hasOwn(data as object, key)),
    "Should contain every required property."
  );

const passesSchema = (data: unknown, schema: Internal): boolean => {
  try {
    assertOrThrow(data, schema);
    return true;
  } catch {
    return false;
  }
};

const codePointLength = (value: string): number => {
  const end = value.length;
  let length = end;
  for (let idx = 0; idx < end - 1; idx++) {
    const first = value.charCodeAt(idx);
    if (first >= 0xd800 && first <= 0xdbff) {
      const second = value.charCodeAt(idx + 1);
      if (second >= 0xdc00 && second <= 0xdfff) {
        length--;
        idx++;
      }
    }
  }
  return length;
};

const B_invalidLengthRange = (
  minimum = 0,
  maximum = minimum
): boolean =>
  !Number.isSafeInteger(minimum) ||
  minimum < 0 ||
  !Number.isSafeInteger(maximum) ||
  maximum < minimum;

const definitionDefault = (definition: JSONSchemaDefinition): unknown =>
  typeof definition === "object" ? definition.default : U;

// `default` is an annotation, not an assertion: a document may carry one its own
// property schema rejects (`{type: "integer", default: ""}` is everywhere in
// hand-written OpenAPI) and it still has to load. `Option_getOr` panics on that,
// because writing one by hand *is* a caller bug — so an unusable default falls
// back to a plain optional that keeps the annotation for the round-trip. Same
// shape as `applyBound`: a SuryError still escapes.
const withDefault = (property: Internal, defaultValue: unknown): Internal => {
  const optional = option(property);
  try {
    return Option_getOr(optional, defaultValue);
  } catch (exn) {
    if (exn && (exn as { s?: symbol }).s === errorSymbol) {
      throw exn;
    }
    return extendJSONSchema(optional, { default: defaultValue });
  }
};

const withoutLayeredKeywords = (
  jsonSchema: JSONSchemaT,
  type: JSONSchemaTypeName
): JSONSchemaT => {
  const base = { ...jsonSchema, type };
  for (let idx = 0; idx < layeredKeywords.length; idx++) {
    delete (base as Record<string, unknown>)[layeredKeywords[idx]!];
  }
  return base;
};

const objectSchema = (
  properties: Record<string, Internal>,
  additionalItems: Internal | "strip" | "strict"
): Internal => {
  const children = Object.values(properties);
  const schema = baseSchema(
    objectTag,
    children.every((child) => !!child.sr) &&
      (typeof additionalItems === "string" || !!additionalItems.sr),
    objectDecoder
  );
  // Every other producer of `required` (builder, factory, composites) means the
  // non-optional properties in declaration order, not the document's `required`
  // set — which is the same list only once each absent key has been wrapped in
  // `option`, and in a different order.
  schema.required = Object.keys(properties).filter((key) => !isOptional(properties[key]!));
  schema.properties = properties;
  schema.additionalItems = additionalItems;
  return schema;
};

const tupleSchema = (items: Internal[]): Internal => {
  const schema = baseSchema(arrayTag, items.every((item) => !!item.sr), arrayDecoder);
  schema.items = items;
  schema.additionalItems = "strict";
  return schema;
};

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
  root: JSONSchemaDefinition;
  refSiblings: boolean;
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

const B_refSiblingsForDialect = (uri: string | undefined): boolean => {
  if (uri === U) return false;
  // JSON Schema only changed `$ref` from a replacement into an applicator in
  // the two date-named dialect families matched by this alternation.
  return /^https?:\/\/json-schema\.org\/draft\/20(?:19-09|20-12)\/schema#?$/.test(
    uri
  );
};

const B_compilePattern = (source: string): RegExp => {
  try {
    return new RegExp(source, "u");
  } catch {
    try {
      return new RegExp(source);
    } catch {
      throw refError(`Invalid JSON Schema pattern: ${JSON.stringify(source)}`);
    }
  }
};

// RFC 6901: `~1` is `/` and `~0` is `~`, in that order, and the fragment may
// arrive percent-encoded.
const unescapePointer = (segment: string): string => {
  // A raw `%` that isn't valid percent-encoding ("50%") is common in real
  // documents; take the segment literally rather than letting a URIError
  // escape past the SuryError contract. Literally, not replacement-char
  // substituted the way a non-throwing decoder would: raw text can still match
  // the key the document wrote, `U+FFFD` never can. That rules out swapping in
  // a table-driven decoder here, whatever it saves on the throwing path.
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
  // definition, so a JSON Schema round-trip keeps it. `#` points at
  // the document itself and has no segment to take. `/`, `~` and `%` can't
  // keep: recursiveDecoder slices the raw suffix off `$ref`, so they'd come
  // back out as a pointer that resolves to a different key.
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

  const refSchema = baseSchema(refTag, false, recursiveDecoder);
  refSchema["$ref"] = `${defsPath}${name}`;
  refSchema.name = name;
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

// The JSON rendering of a definition that only ever runs through
// `passesSchema` — a keyword whose constraint no Sury schema carries, so
// The JSON Schema of the built schema would return the shape the refinement sits
// on, not the keyword. The document's own text is the rendering, with one
// rewrite it can't skip: a `$ref` whose target turned out finite was inlined
// and has no `$defs` entry left to point at, so it expands here. Only a `$ref`
// the cycle detector kept survives as a pointer.
const assertionToJSONDefinition = (
  definition: JSONSchemaDefinition,
  schema: Internal,
  ctx: RefContext
): JSONSchemaDefinition => {
  const rewrite = (
    current: JSONSchemaDefinition,
    resolved?: Internal
  ): JSONSchemaDefinition => {
    if (typeof current === "boolean") return current;
    const ref = current["$ref"];
    if (ref !== U) {
      if (!ctx.cyc[ref]) {
        const target = resolved ?? ctx.built[ref];
        if (target === U) throw refError(`Failed to resolve JSON Schema $ref: ${ref}`);
        const expanded = inputJSONSchema(target);
        if (!ctx.refSiblings) return expanded;
        const siblings = { ...current };
        delete siblings["$ref"];
        const rewritten = rewrite(siblings);
        return isAnyJSONSchema(rewritten)
          ? expanded
          : jsonSchemaMerge(expanded, { allOf: [rewritten] });
      }
      const siblingsSchema = { ...current };
      delete siblingsSchema["$ref"];
      const siblings = ctx.refSiblings ? rewrite(siblingsSchema) : {};
      return jsonSchemaMerge(siblings as JSONSchemaT, { $ref: ctx.ph[ref]!["$ref"] });
    }

    const output: JSONSchemaT = { ...current };
    delete output["$defs"];
    delete output.definitions;
    const mapRecord = (record: Record<string, JSONSchemaDefinition>) =>
      Object.fromEntries(Object.entries(record).map(([key, child]) => [key, rewrite(child)]));
    if (current.properties !== U) output.properties = mapRecord(current.properties);
    if (current.items !== U)
      output.items = Array.isArray(current.items)
        ? current.items.map((child) => rewrite(child))
        : rewrite(current.items);
    if (current.prefixItems !== U)
      output.prefixItems = current.prefixItems.map((child) => rewrite(child));
    const singleKeywords = [
      "additionalItems",
      "if",
      "then",
      "else",
      "not",
    ] as const;
    for (let idx = 0; idx < singleKeywords.length; idx++) {
      const keyword = singleKeywords[idx]!;
      const child = current[keyword];
      if (child !== U) output[keyword] = rewrite(child) as never;
    }
    if (current.additionalProperties !== U) {
      const additionalProperties = rewrite(current.additionalProperties);
      if (isAnyJSONSchema(additionalProperties)) delete output.additionalProperties;
      else output.additionalProperties = additionalProperties;
    }
    const arrayKeywords = ["allOf", "anyOf", "oneOf"] as const;
    for (let idx = 0; idx < arrayKeywords.length; idx++) {
      const keyword = arrayKeywords[idx]!;
      const children = current[keyword];
      if (children !== U) output[keyword] = children.map((child) => rewrite(child));
    }
    return output;
  };
  return rewrite(definition, schema);
};

// @__NO_SIDE_EFFECTS__
export const fromJSONSchema = (
  jsonSchema: JSONSchemaDefinition,
  parentCtx?: RefContext
): Internal => {
  const anySchema = json;
  if (typeof jsonSchema === "boolean") {
    return jsonSchema ? anySchema : never_;
  }
  const refSiblings =
    parentCtx === U && B_refSiblingsForDialect(jsonSchema["$schema"]);
  // Every nested call threads the caller's context; only the outermost one
  // owns the document, and only it publishes the `$defs` collected below.
  const ctx: RefContext =
    parentCtx !== U
      ? parentCtx
      : {
          root: jsonSchema,
          refSiblings,
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

  // The base type dispatch is mirrored by `JSONSchemaResolve` in
  // src/types/json.d.ts.
  let schema: Internal;
  if (jsonSchema["$ref"] !== U) {
    // Draft-07 and OpenAPI 3.0 ignore assertion siblings beside `$ref`;
    // draft-2019-09 and newer apply them. The root `$schema` selects the rule.
    schema = resolveRef(jsonSchema["$ref"], ctx);
    if (ctx.refSiblings) {
      const siblingKeywords: JSONSchemaT = {};
      // Every assertion keyword that may sit beside a `$ref`: the per-type ones
      // `keywordTypes` maps, plus the five that pick a type rather than
      // constrain one. Derived so the two can't drift, and built here rather
      // than at module scope — a top-level call is a side effect to esbuild,
      // which then pins both arrays into every export's bundle.
      const candidates = (["type", "enum", "const", "format", "additionalItems"] as string[]).concat(
        ...keywordTypes.map(([, keywords]) => keywords)
      );
      for (let idx = 0; idx < candidates.length; idx++) {
        const keyword = candidates[idx]!;
        const value = (jsonSchema as Record<string, unknown>)[keyword];
        if (value !== U) (siblingKeywords as Record<string, unknown>)[keyword] = value;
      }
      if (Object.keys(siblingKeywords).length) {
        const siblingSchema = asAssertion(siblingKeywords, ctx);
        schema = refineInput(
          schema,
          (data: unknown) => passesSchema(data, siblingSchema),
          "Should pass the keywords adjacent to the $ref."
        );
        schema = extendJSONSchema(
          schema,
          {
            allOf: [
              assertionToJSONDefinition(siblingKeywords, siblingSchema, ctx),
            ],
          }
        );
      }
    }
  } else if (jsonSchema.type === "object") {
    const definitions = jsonSchema.properties;
    if (definitions === U) {
      const additional = jsonSchema.additionalProperties;
      const required = jsonSchema.required;
      if (additional === false) {
        // Nothing may appear, so a required key has nowhere to live.
        schema = required?.length ? never_ : objectSchema(Object.create(null), "strict");
      } else {
        schema = dict(
          additional === U || isAnyJSONSchema(additional)
            ? anySchema
            : jsonDefinitionToSchema(additional, ctx)
        );
        if (required?.length) {
          schema = extendJSONSchema(withRequired(schema, required), { required });
        }
      }
    } else {
      const additional = jsonSchema.additionalProperties;
      if (additional === U || additional === false) {
        const properties: Record<string, Internal> = Object.create(null);
        const required = new Set(jsonSchema.required);
        for (const key of Object.keys(definitions)) {
          const definition = definitions[key]!;
          let property = jsonDefinitionToSchema(definition, ctx);
          if (!required.has(key)) {
            const defaultValue = definitionDefault(definition);
            property =
              defaultValue === U ? option(property) : withDefault(property, defaultValue);
          }
          properties[key] = property;
        }
        for (const key of required) {
          if (!(key in properties)) properties[key] = anySchema;
        }
        schema = objectSchema(properties, additional === false ? "strict" : "strip");
      } else {
        schema = dict(anySchema);
        const propertyKeys = Object.keys(definitions);
        const propertySchemas = propertyKeys.map((key) => {
          const definition = definitions[key]!;
          const propertySchema = asAssertion(definition, ctx);
          return [
            key,
            propertySchema,
            assertionToJSONDefinition(definition, propertySchema, ctx),
          ] as const;
        });
        schema = refineInput(
          schema,
          (data: unknown) =>
            propertySchemas.every(
              ([key, propertySchema]) =>
                !Object.hasOwn(data as object, key) ||
                passesSchema((data as Record<string, unknown>)[key], propertySchema)
            ),
          "Should pass every declared property schema."
        );
        let additionalSchema = isAnyJSONSchema(additional)
          ? U
          : asAssertion(additional, ctx);
        let roundTripAdditional = additionalSchema === U
          ? U
          : assertionToJSONDefinition(additional, additionalSchema, ctx);
        if (roundTripAdditional !== U && isAnyJSONSchema(roundTripAdditional)) {
          additionalSchema = roundTripAdditional = U;
        }
        if (additionalSchema !== U) {
          const propertyNames = new Set(propertyKeys);
          schema = refineInput(
            schema,
            (data: unknown) =>
              Object.keys(data as Record<string, unknown>).every(
                (key) =>
                  propertyNames.has(key) ||
                  passesSchema((data as Record<string, unknown>)[key], additionalSchema)
              ),
            "Should pass the additionalProperties schema."
          );
        }
        if (jsonSchema.required?.length) {
          schema = withRequired(schema, jsonSchema.required);
        }
        const objectKeywords: JSONSchemaT = {
          properties: Object.fromEntries(
            propertySchemas.map(([key, , definition]) => [key, definition])
          ),
        };
        if (roundTripAdditional !== U)
          objectKeywords.additionalProperties = roundTripAdditional;
        if (jsonSchema.required !== U) objectKeywords.required = jsonSchema.required;
        schema = extendJSONSchema(schema, objectKeywords);
      }
    }
  } else if (jsonSchema.type === "array") {
    const prefixItems =
      jsonSchema.prefixItems !== U
        ? jsonSchema.prefixItems
        : Array.isArray(jsonSchema.items)
          ? jsonSchema.items
          : U;
    // A tuple carries its own arity, so it needs no `minItems`/`maxItems` pass;
    // every other array shape does.
    let pinned = false;
    if (prefixItems !== U) {
      const length = prefixItems.length;
      const minimum = jsonSchema.minItems ?? 0;
      const restDefinition =
        jsonSchema.prefixItems !== U
          ? Array.isArray(jsonSchema.items)
            ? true
            : (jsonSchema.items ?? true)
          : (jsonSchema.additionalItems ?? true);
      // `items: false` caps the length at the prefix, and so does a `maxItems`
      // landing inside it. A Sury tuple is the shape only when the bounds pin
      // the length to exactly the prefix — and when they cross, the document
      // describes an array no value can have, which is `never` rather than the
      // two contradictory length checks the bounds pass would emit.
      const maximum = Math.min(
        jsonSchema.maxItems ?? Infinity,
        restDefinition === false ? length : Infinity
      );
      if (minimum > maximum) {
        schema = never_;
      } else if (minimum === length && maximum === length) {
        pinned = true;
        schema = tupleSchema(
          prefixItems.map((definition) => jsonDefinitionToSchema(definition, ctx))
        );
      } else {
        const prefixSchemas = prefixItems.map((definition) => asAssertion(definition, ctx));
        const restSchema = restDefinition === true ? U : asAssertion(restDefinition, ctx);
        schema = refineInput(
          array(anySchema),
          (data: unknown) => {
            const items = data as unknown[];
            const prefixLength = Math.min(items.length, prefixSchemas.length);
            for (let idx = 0; idx < prefixLength; idx++) {
              if (!passesSchema(items[idx], prefixSchemas[idx]!)) return false;
            }
            if (restSchema !== U) {
              for (let idx = prefixSchemas.length; idx < items.length; idx++) {
                if (!passesSchema(items[idx], restSchema)) return false;
              }
            }
            return true;
          },
          "Should pass the positional and additional item schemas."
        );
        const tupleKeywords: JSONSchemaT = {};
        if (jsonSchema.prefixItems !== U) {
          tupleKeywords.prefixItems = prefixItems.map((definition, idx) =>
            assertionToJSONDefinition(definition, prefixSchemas[idx]!, ctx)
          );
          if (jsonSchema.items !== U && !Array.isArray(jsonSchema.items)) {
            tupleKeywords.items = assertionToJSONDefinition(
              jsonSchema.items,
              restSchema === U ? anySchema : restSchema,
              ctx
            );
          }
        } else {
          tupleKeywords.items = prefixItems.map((definition, idx) =>
            assertionToJSONDefinition(definition, prefixSchemas[idx]!, ctx)
          );
          if (jsonSchema.additionalItems !== U) {
            tupleKeywords.additionalItems = assertionToJSONDefinition(
              jsonSchema.additionalItems,
              restSchema === U ? anySchema : restSchema,
              ctx
            );
          }
        }
        schema = extendJSONSchema(schema, tupleKeywords);
      }
    } else if (jsonSchema.items !== U) {
      const items = jsonSchema.items;
      schema = array(jsonDefinitionToSchema(items as JSONSchemaDefinition, ctx));
    } else {
      schema = array(anySchema);
    }
    if (!pinned) {
      const minimum = jsonSchema.minItems;
      const maximum = jsonSchema.maxItems;
      if (B_invalidLengthRange(minimum, maximum)) {
        schema = never_;
      } else {
        if (minimum) {
          schema = applyBound(schema, minLength, minimum);
        }
        if (maximum !== U) {
          schema = applyBound(schema, maxLength, maximum);
        }
      }
    }
  } else if (Array.isArray(jsonSchema.type)) {
    const types = jsonSchema.type;
    schema = types.length
      ? union(
          types.map((type) =>
            fromJSONSchema(withoutLayeredKeywords(jsonSchema, type), ctx)
          )
        )
      : never_;
  } else if (jsonSchema.type === "string") {
    schema = stringFormatSchemas[jsonSchema.format!] || string;
    if (jsonSchema.pattern !== U) {
      schema = pattern(schema, B_compilePattern(jsonSchema.pattern));
    }
    if (jsonSchema.minLength !== U || jsonSchema.maxLength !== U) {
      const minimum = jsonSchema.minLength;
      const maximum = jsonSchema.maxLength;
      if (B_invalidLengthRange(minimum, maximum)) {
        schema = never_;
      } else if (minimum !== 0 || maximum !== U) {
        schema = refineInput(
          schema,
          (data: unknown) => {
            const stringData = data as string;
            if (minimum !== U && stringData.length < minimum) return false;
            if (minimum === U && maximum !== U && stringData.length <= maximum)
              return true;
            const length = codePointLength(stringData);
            return (minimum === U || length >= minimum) && (maximum === U || length <= maximum);
          },
          "Should have a code-point length within the JSON Schema bounds."
        );
      }
      // `minLength: 0` asserts nothing, so a document carrying only that has
      // no keyword to store and no copy to pay for.
      if (schema.type !== neverTag && (minimum || maximum !== U)) {
        const lengthKeywords: JSONSchemaT = {};
        if (minimum) lengthKeywords.minLength = minimum;
        if (maximum !== U) lengthKeywords.maxLength = maximum;
        schema = extendJSONSchema(schema, lengthKeywords);
      }
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
    const schemas: Internal[] = [];
    let constrained = false;
    for (let i = 0; i < keywordTypes.length; i++) {
      const [type, keywords] = keywordTypes[i]!;
      const applies = keywords.some(
        (key) => (jsonSchema as Record<string, unknown>)[key] !== U
      );
      if (applies) constrained = true;
      schemas.push(
        fromJSONSchema(
          applies ? withoutLayeredKeywords(jsonSchema, type) : { type },
          ctx
        )
      );
    }
    schema = constrained
      ? union([...schemas, bool, schemaFactory(null)])
      : anySchema;
  }

  // `const`/`enum` replace the base with native literals after filtering out
  // values that fail sibling assertions. The resulting runtime only needs the
  // literal checks; re-running the base for every parse would be redundant.
  if (jsonSchema["$ref"] === U && jsonSchema.enum !== U) {
    const assertion = withDefs(schema, ctx);
    const candidates = jsonSchema.enum
      .filter(
        (candidate) => schema === anySchema || passesSchema(candidate, assertion)
      )
      .map(primitiveToSchema);
    schema =
      candidates.length === 0
        ? never_
        : candidates.length === 1
          ? candidates[0]!
          : union(candidates);
  }
  if (jsonSchema["$ref"] === U && jsonSchema.const !== U) {
    schema = schema === anySchema || passesSchema(jsonSchema.const, withDefs(schema, ctx))
      ? primitiveToSchema(jsonSchema.const)
      : never_;
  }

  // Composition keywords constrain *in addition to* everything above — so they
  // layer on as refinements rather than replacing the shape. The exception is a
  // base nothing has constrained yet: intersecting with "any JSON" is the
  // member itself, so the member compiles natively instead, keeping the union
  // codegen and the per-member error a document with no sibling keywords
  // deserves. `schema === anySchema` is exactly that test — every other branch
  // above, and `enum`/`const`, replace it.
  if (jsonSchema.allOf !== U) {
    const definitions = jsonSchema.allOf;
    if (definitions.length !== 0) {
      // Only a lone member: Sury has no intersection to compile two into.
      if (schema === anySchema && definitions.length === 1) {
        schema = jsonDefinitionToSchema(definitions[0]!, ctx);
      } else {
        const schemas = definitions.map((d) => asAssertion(d, ctx));
        schema = refineInput(
          schema,
          (data: unknown) => schemas.every((s) => passesSchema(data, s)),
          "Should pass for all schemas of the allOf property."
        );
        schema = extendJSONSchema(schema, {
          allOf: definitions.map((definition, idx) =>
            assertionToJSONDefinition(definition, schemas[idx]!, ctx)
          ),
        });
      }
    }
  }
  if (jsonSchema.anyOf !== U) {
    const definitions = jsonSchema.anyOf;
    if (definitions.length === 0) {
      schema = never_;
    } else if (schema === anySchema) {
      const members = definitions.map((d) => jsonDefinitionToSchema(d, ctx));
      schema = members.length === 1 ? members[0]! : union(members);
    } else {
      const schemas = definitions.map((d) => asAssertion(d, ctx));
      schema = refineInput(
        schema,
        (data: unknown) => schemas.some((candidate) => passesSchema(data, candidate)),
        "Should pass at least one schema according to the anyOf property."
      );
      schema = extendJSONSchema(schema, {
        anyOf: definitions.map((definition, idx) =>
          assertionToJSONDefinition(definition, schemas[idx]!, ctx)
        ),
      });
    }
  }
  if (jsonSchema.oneOf !== U) {
    const definitions = jsonSchema.oneOf;
    if (definitions.length === 0) {
      schema = never_;
    } else {
      const schemas = definitions.map((d) => asAssertion(d, ctx));
      schema = refineInput(
        schema,
        (data: unknown) =>
          schemas.filter((candidate) => passesSchema(data, candidate)).length === 1,
        "Should pass exactly one schema according to the oneOf property."
      );
      schema = extendJSONSchema(schema, {
        oneOf: definitions.map((definition, idx) =>
          assertionToJSONDefinition(definition, schemas[idx]!, ctx)
        ),
      });
    }
  }
  if (jsonSchema.not !== U) {
    const notSchema = asAssertion(jsonSchema.not, ctx);
    schema = refineInput(
      schema,
      (data: unknown) => !passesSchema(data, notSchema),
      "Should NOT be valid against schema in the not property."
    );
    schema = extendJSONSchema(schema, {
      not: assertionToJSONDefinition(jsonSchema.not, notSchema, ctx),
    });
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
    const conditionalKeywords: JSONSchemaT = {
      if: assertionToJSONDefinition(jsonSchema.if, ifSchema, ctx),
    };
    if (jsonSchema.then !== U)
      conditionalKeywords.then = assertionToJSONDefinition(jsonSchema.then, thenSchema!, ctx);
    if (jsonSchema.else !== U)
      conditionalKeywords.else = assertionToJSONDefinition(jsonSchema.else, elseSchema!, ctx);
    schema = extendJSONSchema(schema, conditionalKeywords);
  }

  // OpenAPI 3.0's `nullable` widens whatever the rest of the document
  // describes, so it wraps the finished schema instead of re-entering the
  // dispatch with `nullable: false`: re-entering applies every keyword below
  // the base twice, and lets `enum`/`const` replace the very schema `null` was
  // added to. Mirrored by `JSONSchemaResolve` in src/types/json.d.ts, which
  // likewise unions `null` onto the fully resolved type. Skipped for an
  // unconstrained base, which already admits `null`.
  if (jsonSchema.nullable && schema !== anySchema) {
    schema = null_(schema);
  }

  if (jsonSchema.default !== U) {
    schema = extendJSONSchema(schema, { default: jsonSchema.default });
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
