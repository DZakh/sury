import { unionFactory } from "./composites";
import { Literal_parse, literalDecoder, nullLiteral, unit } from "./primitives";
import { SuryError, baseSchema, cached, configurableValueOptions, copySchema, getOrRethrow, globalConfig, noopDecoder, panic, schemaPrototype, unknown, updateOutput, valKey, valueOptions } from "./schema";
import type { JSONSchemaT, StandardJsonSchemaOptions } from "./jsonschema";
import { compileDecoder, getDecoder, getOutputSchema, isAsyncInternal, reverse } from "./parse";
import { B_effectCtx, B_embed, B_embedTransformation, B_inlineConst, B_invalidInputBuilder, B_invalidOperation, B_mergeWithPathPrepend, B_next, B_refine, B_varWithoutAllocation, EffectCtx, _var } from "./builder";
import { AdditionalItems, Check, Internal, SchemaErrorMessage, SuryErrorRecord, Val, s, toExpression, vendor } from "./types";
import { Builder } from "./builder";
import { flagAsync, valFlagAsync } from "./flags";
import { pathEmpty, pathFromArray, pathToArray } from "./path";
import { objectTag, refTag, undefinedTag } from "./tags";

export const recursiveDecoder: Builder = (input) => {
  const expectedSchema = input.e;

  const schemaRef = expectedSchema["$ref"]!;
  const defs = input.g.d!;
  // Ignore #/$defs/
  const identifier = schemaRef.slice(8);
  const def = defs[identifier]!;
  const flag = input.g.o;

  const inputSchema = input.s.seq === expectedSchema.seq ? def : input.s;

  const key = `${inputSchema.seq}-${def.seq}--${flag}`;
  let recOperation = "";

  const fn = (def as unknown as Record<string, unknown>)[key];
  if (fn !== undefined) {
    // Circular reference (fn === 0) or already compiled
    recOperation = fn === 0 ? B_embed(input, def) + `["${key}"]` : B_embed(input, fn);
  } else {
    // Optimistic compilation with recompile if assumptions were wrong
    let assumedHasTransform = def.hasTransform !== undefined ? def.hasTransform : false;
    let assumedIsAsync = def.isAsync !== undefined ? def.isAsync : false;
    let compileNeeded = true;
    let finalFn: unknown = 0;

    while (compileNeeded) {
      compileNeeded = false;

      // Set optimistic values on def before compiling (if not already set)
      // Inner circular references will read these values
      if (def.hasTransform === undefined) {
        def.hasTransform = assumedHasTransform;
      }
      if (def.isAsync === undefined) {
        def.isAsync = assumedIsAsync;
      }

      // Mark as in-progress
      (configurableValueOptions as unknown as Record<string, unknown>)[valKey] = 0;
      Object.defineProperty(def, key, configurableValueOptions as PropertyDescriptor);

      // Compile
      const fn = compileDecoder(inputSchema, def, flag, defs);

      // Cache result
      valueOptions[valKey] = fn;
      Object.defineProperty(def, key, valueOptions as PropertyDescriptor);

      finalFn = fn;

      // Check if actual values differ from assumed
      const actualHasTransform = def.hasTransform!;
      const actualIsAsync = def.isAsync!;

      if (
        actualHasTransform !== assumedHasTransform ||
        actualIsAsync !== assumedIsAsync
      ) {
        // Wrong assumption - update and recompile
        assumedHasTransform = actualHasTransform;
        assumedIsAsync = actualIsAsync;
        // Delete cached function to force recompilation
        delete (def as unknown as Record<string, unknown>)[key];
        compileNeeded = true;
      }
    }

    // Embed only the final compiled function to avoid wasting embed slots on recompiles
    recOperation = B_embed(input, finalFn);
  }

  const hasTransform = def.hasTransform === true;
  const isAsync = def.isAsync!;

  // Result var decl, prepended after the re-merge below so it sits outside the
  // try/catch mergeWithPathPrepend may wrap the assignment in (stays in scope).
  let outputDecl = "";
  let output: Val;
  if (hasTransform || isAsync) {
    const outputVar = B_varWithoutAllocation(input.g);
    outputDecl = `let ${outputVar};`;

    output = B_next(input, outputVar, expectedSchema, expectedSchema);
    output.v = _var;

    output.cp = `${outputVar}=${recOperation}(${input.i});`;

    if (isAsync) {
      output.f |= valFlagAsync;
    }
  } else {
    // No transform: call for validation but don't capture result
    output = B_refine(input, expectedSchema, undefined, expectedSchema);
    output.cp = `${recOperation}(${input.i});`;
  }

  output.prev = undefined;
  output.cp = outputDecl + B_mergeWithPathPrepend(output, input);

  // Un-finalize: this val may be reused as input to a subsequent parser (e.g.
  // S.transform on a recursive schema) and must accept hoisted decls again.
  output.fz = undefined;
  output.prev = input;

  return output;
};

// PORT-NOTE: StandardSchema/JSONSchema types are ported as loose, type-only
// aliases (no runtime import allowed here). `JSONSchemaT` stands in for
// JSONSchema.t.
export type StandardIssue = {
  message: string;
  path?: unknown[];
};
export type StandardResult = {
  value?: unknown;
  issues?: StandardIssue[];
};
export type StandardProps = {
  version: number;
  vendor: string;
  validate: (input: unknown) => StandardResult;
  jsonSchema?: {
    input: (options: StandardJsonSchemaOptions) => JSONSchemaT;
    output: (options: StandardJsonSchemaOptions) => JSONSchemaT;
  };
};

// The Standard JSON Schema converter, installed by enableStandardJSONSchema
// (jsonschema.ts). A plain mutable module binding — the indirection is NOT a
// forward-reference workaround but the tree-shaking gate: the `~standard`
// prototype getter below is always retained, so it must not statically
// reference the converter or every parser-only bundle would ship the whole
// toJSONSchema machinery. Only calling the public opt-in pulls it in.
let standardJSONSchemaConverter:
  | ((schema: Internal, options: StandardJsonSchemaOptions, isOutput: boolean) => JSONSchemaT)
  | undefined;
export const __setStandardJSONSchemaConverter = (
  fn: (schema: Internal, options: StandardJsonSchemaOptions, isOutput: boolean) => JSONSchemaT
): void => {
  standardJSONSchemaConverter = fn;
};

export const getStandardJSONSchema = (
  schema: Internal,
  options: StandardJsonSchemaOptions,
  isOutput: boolean
): JSONSchemaT => {
  if (standardJSONSchemaConverter !== undefined) {
    return standardJSONSchemaConverter(schema, options, isOutput);
  } else {
    throw new SuryError({
      code: "invalid_operation",
      path: pathEmpty,
      reason:
        "~standard.jsonSchema requires S.enableStandardJSONSchema() to be called first",
    });
  }
}

Object.defineProperty(schemaPrototype, "~standard", {
  get: function (this: Internal) {
    const schema = this;
    const standard: StandardProps = {
      version: 1,
      vendor,
      validate: (input: unknown): StandardResult => {
        try {
          return {
            value: (getDecoder(unknown, schema) as (input: unknown) => unknown)(input),
          };
        } catch (exn) {
          const error = getOrRethrow(exn);
          return {
            issues: [
              {
                message: error.reason,
                path:
                  error.path === pathEmpty ? undefined : pathToArray(error.path),
              },
            ],
          };
        }
      },
      // Standard JSON Schema spec: https://standardschema.dev/json-schema
      // `input` returns the JSON Schema of the schema's input type,
      // `output` the JSON Schema of its output type. The `$schema` URI is
      // stamped according to `options.target`; an unsupported target throws.
      // Throws before enableStandardJSONSchema is called.
      jsonSchema: {
        input: (options) => getStandardJSONSchema(schema, options, false),
        output: (options) => getStandardJSONSchema(schema, options, true),
      },
    };
    return standard;
  },
});

// =============
// Operations
// =============

export const getAssertResult = (): Internal => {
  return cached("a", undefinedTag, (s) => {
    s.const = void 0;
    s.decoder = literalDecoder;
    s.noValidation = true;
  });
}

export const assertOrThrow = (any: unknown, schema: Internal): void => {
  (getDecoder(unknown, schema, getAssertResult()) as (input: unknown) => unknown)(any);
}

export const assertAsyncOrThrow = (any: unknown, schema: Internal): Promise<void> => {
  return (
    getDecoder(unknown, schema, getAssertResult(), flagAsync) as (
      input: unknown
    ) => Promise<void>
  )(any);
}

export const isAsync = (schema: Internal): boolean => {
  if (schema.isAsync === undefined) {
    return isAsyncInternal(schema, undefined);
  } else {
    return schema.isAsync;
  }
}

export type JsResult<V> =
  | { success: true; value: V }
  | { success: false; error: SuryErrorRecord };

export const wrapExnToFailure = (exn: unknown): JsResult<never> => {
  if (exn && (exn as { s?: symbol }).s === s) {
    return { success: false, error: exn as unknown as SuryErrorRecord };
  } else {
    throw exn;
  }
}

export const js_safe = <V>(fn: () => V): JsResult<V> => {
  try {
    return {
      success: true,
      value: fn(),
    };
  } catch (exn) {
    return wrapExnToFailure(exn);
  }
}

export const js_safeAsync = <V>(fn: () => Promise<V>): Promise<JsResult<V>> => {
  try {
    return fn().then(
      (value): JsResult<V> => ({ success: true, value }),
      wrapExnToFailure
    );
  } catch (exn) {
    return Promise.resolve(wrapExnToFailure(exn));
  }
}

// PORT-NOTE: `module Metadata` → flat `Metadata_*` functions. `Id.t<'metadata>` is a string at
// runtime; `unionToKey` was `%identity` and is dropped.
export type MetadataId = string;

export const Metadata_Id_make = (namespace: string, name: string): MetadataId => {
  return `m:${namespace}:${name}`;
};
export const Metadata_Id_internal = (name: string): MetadataId => {
  return `m:${name}`;
};
export const Metadata_get = (schema: Internal, id: MetadataId): unknown => {
  return (schema as unknown as Record<string, unknown>)[id];
};
export const Metadata_setInPlace = (schema: Internal, id: MetadataId, metadata: unknown): void => {
  (schema as unknown as Record<string, unknown>)[id] = metadata;
};
export const Metadata_set = (schema: Internal, id: MetadataId, metadata: unknown): Internal => {
  const mut = copySchema(schema);
  Metadata_setInPlace(mut, id, metadata);
  return mut;
};

export const defsPath = `#/$defs/`;
export const recursive = (name: string, fn: (schema: Internal) => Internal): Internal => {
  const ref = `${defsPath}${name}`;
  const refSchema = baseSchema(refTag, false);
  refSchema["$ref"] = ref;
  refSchema.name = name;
  refSchema.decoder = recursiveDecoder;

  // This is for mutual recursion
  const isNestedRec = globalConfig.d !== undefined;
  if (!isNestedRec) {
    globalConfig.d = {};
  }
  const def = fn(refSchema);
  if (def.name) {
    refSchema.name = def.name;
  }
  globalConfig.d![name] = def;

  if (isNestedRec) {
    return refSchema;
  } else {
    const schema = baseSchema(refTag, false);
    schema.name = refSchema.name;
    schema["$ref"] = ref;
    schema["$defs"] = globalConfig.d;
    schema.decoder = recursiveDecoder;

    globalConfig.d = undefined;

    return schema;
  }
}

export const noValidation = (schema: Internal, value: boolean): Internal => {
  const mut = copySchema(schema);

  // TODO: Test for discriminant literal
  // TODO: Better test reverse
  mut.noValidation = value;
  return mut;
}

export const internalRefine = (
  schema: Internal,
  makeRefiner: (mut: Internal) => (input: Val) => Check[]
): Internal => {
  return updateOutput(schema, (mut) => {
    const refiner = makeRefiner(mut);
    const existingRefiner = mut.refiner;
    if (existingRefiner !== undefined) {
      mut.refiner = (input) => {
        const arr = existingRefiner(input);
        arr.push(...refiner(input));
        return arr;
      };
    } else {
      mut.refiner = refiner;
    }
  });
}

export const refine = (
  schema: Internal,
  refineCheck: (value: unknown) => boolean,
  error?: string,
  path?: string[]
): Internal => {
  const message = error !== undefined ? error : "Refinement failed";
  const extraPath = path !== undefined ? pathFromArray(path) : pathEmpty;
  return internalRefine(schema, (_) => (input) => {
    const embeddedCheck = B_embed(input, refineCheck);
    return [
      {
        c: (inputVar) => `${embeddedCheck}(${inputVar})`,
        f: B_invalidInputBuilder(undefined, extraPath, message),
      },
    ];
  });
}

export const getMutErrorMessage = (mut: Internal): SchemaErrorMessage => {
  const em: SchemaErrorMessage = mut.errorMessage ? { ...mut.errorMessage } : {};
  mut.errorMessage = em;
  return em;
}

export type TransformDefinition<Input = unknown, Output = unknown> = {
  // @as("p") — parser
  p?: (input: Input) => Output;
  // @as("a") — asyncParser
  a?: (input: Input) => Promise<Output>;
  // @as("s") — serializer
  s?: (output: Output) => Input;
};

// PORT-NOTE: `s<'output>` (the effect ctx passed to the transformer) is what
// `B_effectCtx` returns: `{ fail(message, path?): never }`.

export const transform = (
  schema: Internal,
  transformer: (ctx: EffectCtx) => TransformDefinition
): Internal => {
  return updateOutput(schema, (mut) => {
    mut.parser = (input) => {
      const definition = transformer(B_effectCtx(input));
      if (definition.p !== undefined && definition.a === undefined) {
        return B_embedTransformation(input, definition.p, false);
      } else if (definition.p === undefined && definition.a !== undefined) {
        return B_embedTransformation(input, definition.a, true);
      } else if (
        definition.p === undefined &&
        definition.a === undefined &&
        definition.s === undefined
      ) {
        return B_refine(input, undefined, undefined, input.e.to!);
      } else if (definition.p === undefined && definition.a === undefined) {
        return B_invalidOperation(input, `The S.transform parser is missing`);
      } else {
        return B_invalidOperation(
          input,
          `The S.transform doesn't allow parser and asyncParser at the same time. Remove parser in favor of asyncParser`
        );
      }
    };
    const to = copySchema(unknown);
    to.serializer = (input) => {
      const definition = transformer(B_effectCtx(input));
      if (definition.s !== undefined) {
        return B_embedTransformation(input, definition.s, false);
      } else if (
        definition.p === undefined &&
        definition.a === undefined &&
        definition.s === undefined
      ) {
        return B_refine(input, undefined, undefined, input.e.to!);
      } else {
        return B_invalidOperation(input, `The S.transform serializer is missing`);
      }
    };
    mut.to = to;
    delete mut.isAsync;
  });
}

export const nullAsUnit = (): Internal => {
  // PORT-NOTE: local `s` renamed to `schema` — `s` is the module-level error
  // identity symbol in this file.
  const schema = copySchema(nullLiteral());
  schema.to = unit();
  return schema;
}

// A default is either an eager value or a lazily-called callback — used only
// within this module, never exposed to callers.
export type OptionDefault =
  | { type: "value"; value: unknown }
  | { type: "callback"; callback: () => unknown };

export const Option_getWithDefault = (schema: Internal, default_: OptionDefault): Internal => {
  return updateOutput(schema, (mut) => {
    const anyOf = mut.anyOf;
    if (anyOf !== undefined) {
      const outputItems: Internal[] = [];
      // FIXME: drop `originalItems` once unionDecoder can reverse member
      // `.to` chains — then mut.default + the serializer can both run
      // through `schema->reverse` directly.
      const originalItems: Internal[] = [];

      for (let idx = 0; idx < anyOf.length; idx++) {
        const schema = anyOf[idx]!;
        const outputSchema = getOutputSchema(schema);
        if (outputSchema.type !== undefinedTag) {
          outputItems.push(outputSchema);
          originalItems.push(schema);
        }
      }

      const item: Internal =
        outputItems.length === 0
          ? panic(`Can't set default for ${toExpression(mut)}`)
          : outputItems.length === 1
            ? outputItems[0]!
            : unionFactory(outputItems);
      const originalItem: Internal =
        originalItems.length === 1 ? originalItems[0]! : unionFactory(originalItems);

      if (default_.type === "value") {
        const v = default_.value;
        // Full unknown -> item decode so primitive item types still get type-checked.
        try {
          (getDecoder(unknown, item) as (input: unknown) => unknown)(v);
        } catch (exn) {
          const error = getOrRethrow(exn);
          panic(
            `Invalid default for ${toExpression(mut)}: ${
              (error as unknown as { message: string })["message"]
            }`
          );
        }
        // Best-effort input form for JSON Schema metadata.
        // FIXME: running a decoder at schema-creation time isn't a goal —
        // it compiles + executes a fresh decode pipeline per default. Replace
        // with something cheaper (or move to lazy/JSON-Schema-export time)
        // before the official v11 release.
        try {
          mut.default = (getDecoder(reverse(originalItem)) as (input: unknown) => unknown)(v);
        } catch (_exn) {}
      }

      mut.parser = (input) => {
        const nextSchema = input.e.to!;
        const inputVar = input.v();
        return B_next(
          input,
          `${inputVar}===void 0?${
            default_.type === "value"
              ? B_inlineConst(input, Literal_parse(default_.value))
              : `${B_embed(input, default_.callback)}()`
          }:${inputVar}`,
          nextSchema,
          nextSchema
        );
      };
      const to = copySchema(item);

      const originalDecoder = to.decoder;
      to.serializer = (input) => {
        const nextSchema = reverse(originalItem);
        return B_refine(originalDecoder(input), nextSchema, undefined, nextSchema);
      };

      // FIXME: This looks wrong, but this is how it was with prev architecture
      to.decoder = noopDecoder;

      mut.to = to;
    } else {
      panic(`Can't set default for ${toExpression(mut)}`);
    }
  });
};

export const Option_getOr = (schema: Internal, defaultValue: unknown): Internal =>
  Option_getWithDefault(schema, { type: "value", value: defaultValue });
export const Option_getOrWith = (schema: Internal, defaultCb: () => unknown): Internal =>
  Option_getWithDefault(schema, { type: "callback", callback: defaultCb });

// PORT-NOTE: `Object.s` (the object ctx record) → `ObjectCtx`; field names are
// the runtime names from `@as` (`f` for `field`, others unchanged).
export type ObjectCtx = {
  // @as("f") — field
  f: (location: string, schema: Internal) => unknown;
  fieldOr: (location: string, schema: Internal, or: unknown) => unknown;
  tag: (location: string, value: unknown) => void;
  nested: (location: string) => ObjectCtx;
  flatten: (schema: Internal) => unknown;
};

export const Object_setAdditionalItems = (
  schema: Internal,
  additionalItems: AdditionalItems,
  deep: boolean
): Internal => {
  const currentAdditionalItems = schema.additionalItems;
  if (
    currentAdditionalItems !== undefined &&
    currentAdditionalItems !== additionalItems &&
    typeof currentAdditionalItems !== objectTag
  ) {
    const mut = copySchema(schema);
    mut.additionalItems = additionalItems;
    if (deep) {
      const items = schema.items;
      if (items !== undefined) {
        mut.items = items.map((s) => Object_setAdditionalItems(s, additionalItems, deep));
      }

      const properties = schema.properties;
      if (properties !== undefined) {
        mut.properties = Object.fromEntries(
          Object.keys(properties).map((key) => [
            key,
            Object_setAdditionalItems(properties[key]!, additionalItems, deep),
          ])
        );
      }
    }
    return mut;
  } else {
    return schema;
  }
};

export const strip = (schema: Internal): Internal => {
  return Object_setAdditionalItems(schema, "strip", false);
}

export const deepStrip = (schema: Internal): Internal => {
  return Object_setAdditionalItems(schema, "strip", true);
}

export const strict = (schema: Internal): Internal => {
  return Object_setAdditionalItems(schema, "strict", false);
}

export const deepStrict = (schema: Internal): Internal => {
  return Object_setAdditionalItems(schema, "strict", true);
}

export type TupleCtx = {
  item: (idx: number, schema: Internal) => unknown;
  tag: (idx: number, value: unknown) => void;
};
