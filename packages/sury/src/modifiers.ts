// Modifiers: everything that takes a schema and returns a changed schema —
// refinements' machinery, transforms, metadata, object modes and defaults.
// Distinct from `operations.ts`, which compiles a schema into a callable.

import {
  type AdditionalItems,
  type Check,
  copySchema,
  getOrRethrow,
  inputExpression,
  type Internal,
  noopDecoder,
  objectTag,
  panic,
  pathEmpty,
  type SchemaErrorMessage,
  U,
  undefinedTag,
  unknown,
  updateOutput,
  type Val,
} from "./base";
import {
  B_embed,
  B_embedTransformation,
  B_inlineConst,
  B_invalidInputBuilder,
  B_invalidOperation,
  B_next,
  B_refine,
} from "./builder";
import { getDecoder, getOutputSchema, reverse } from "./parse";
import { Literal_parse, nullLiteral, unit } from "./primitives";
import { unionFactory } from "./union";

// PORT-NOTE: `module Metadata` → flat `Metadata_*` functions. `Id.t<'metadata>` is a string at
// runtime; `unionToKey` was `%identity` and is dropped.
export type MetadataId = string;

// @__NO_SIDE_EFFECTS__
export const Metadata_Id_make = (namespace: string, name: string): MetadataId => {
  return `m:${namespace}:${name}`;
};
export const Metadata_Id_internal = (name: string): MetadataId => {
  return `m:${name}`;
};
// @__NO_SIDE_EFFECTS__
export const Metadata_get = (schema: Internal, id: MetadataId): unknown => {
  return (schema as unknown as Record<string, unknown>)[id];
};
export const Metadata_setInPlace = (schema: Internal, id: MetadataId, metadata: unknown): void => {
  (schema as unknown as Record<string, unknown>)[id] = metadata;
};
// @__NO_SIDE_EFFECTS__
export const Metadata_set = (schema: Internal, id: MetadataId, metadata: unknown): Internal => {
  const mut = copySchema(schema);
  Metadata_setInPlace(mut, id, metadata);
  return mut;
};

// @__NO_SIDE_EFFECTS__
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
    if (existingRefiner !== U) {
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

// @__NO_SIDE_EFFECTS__
export const refine = (
  schema: Internal,
  refineCheck: (value: unknown) => boolean,
  error?: string,
  path?: string[]
): Internal => {
  const message = error !== U ? error : "Refinement failed";
  const extraPath = path !== U ? path : pathEmpty;
  return internalRefine(schema, (_) => (input) => {
    const embeddedCheck = B_embed(input, refineCheck);
    return [
      {
        c: (inputVar) => `${embeddedCheck}(${inputVar})`,
        f: B_invalidInputBuilder(U, extraPath, message),
      },
    ];
  });
}

// `refine`, but on the schema's Input rather than its assembled Output. A JSON
// Schema composition keyword (`allOf`, `not`, …) asserts about the data as
// given, and an object schema strips unknown keys on the way out — an output
// refiner would judge `{a}` where the document said `{a, b}`.
export const refineInput = (
  schema: Internal,
  refineCheck: (value: unknown) => boolean,
  error?: string
): Internal => {
  const message = error !== U ? error : "Refinement failed";
  return updateOutput(schema, (mut) => {
    const refiner = (input: Val): Check[] => {
      const embeddedCheck = B_embed(input, refineCheck);
      return [
        {
          c: (inputVar) => `${embeddedCheck}(${inputVar})`,
          f: B_invalidInputBuilder(U, pathEmpty, message),
        },
      ];
    };
    const existing = mut.inputRefiner;
    mut.inputRefiner =
      existing !== U
        ? (input) => {
            const arr = existing(input);
            arr.push(...refiner(input));
            return arr;
          }
        : refiner;
  });
}

export const getMutErrorMessage = (mut: Internal): SchemaErrorMessage => {
  const em: SchemaErrorMessage = mut.errorMessage ? { ...mut.errorMessage } : {};
  mut.errorMessage = em;
  return em;
}

export type TransformDefinition<TInput = unknown, TOutput = unknown> = {
  // @as("p") — parser
  p?: (input: TInput) => TOutput;
  // @as("a") — asyncParser
  a?: (input: TInput) => Promise<TOutput>;
  // @as("s") — serializer
  s?: (output: TOutput) => TInput;
};

// The transformer takes no argument. It used to receive an effect ctx whose
// only member was `fail`; a transform now fails by throwing, which every
// caller of a transform already handles — B_makeInvalidConversionDetails
// adopts a thrown SuryError as-is and wraps anything else as
// `invalid_conversion`.

// @__NO_SIDE_EFFECTS__
export const transform = (
  schema: Internal,
  transformer: () => TransformDefinition
): Internal => {
  return updateOutput(schema, (mut) => {
    mut.parser = (input) => {
      const definition = transformer();
      if (definition.p !== U && definition.a === U) {
        return B_embedTransformation(input, definition.p, false);
      } else if (definition.p === U && definition.a !== U) {
        return B_embedTransformation(input, definition.a, true);
      } else if (
        definition.p === U &&
        definition.a === U &&
        definition.s === U
      ) {
        return B_refine(input, U, U, input.e.to!);
      } else if (definition.p === U && definition.a === U) {
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
      const definition = transformer();
      if (definition.s !== U) {
        return B_embedTransformation(input, definition.s, false);
      } else if (
        definition.p === U &&
        definition.a === U &&
        definition.s === U
      ) {
        return B_refine(input, U, U, input.e.to!);
      } else {
        return B_invalidOperation(input, `The S.transform serializer is missing`);
      }
    };
    mut.to = to;
    delete mut.isAsync;
  });
}

// Not initSchema: that would stamp the self-reverse marker, and this codec's
// reverse (unit -> null) must stay lazily derived — copySchema drops
// nullLiteral's non-enumerable `r` on purpose.
export const nullAsUnit: Internal = /* @__PURE__ */ (() => {
  // PORT-NOTE: local `s` renamed to `schema` — `s` is the module-level error
  // identity symbol in this file.
  const schema = copySchema(nullLiteral);
  schema.to = unit;
  return schema;
})();

// A default is either an eager value or a lazily-called callback — used only
// within this module, never exposed to callers.
export type OptionDefault =
  | { type: "value"; value: unknown }
  | { type: "callback"; callback: () => unknown };

export const Option_getWithDefault = (schema: Internal, default_: OptionDefault): Internal => {
  return updateOutput(schema, (mut) => {
    const anyOf = mut.anyOf;
    if (anyOf !== U) {
      const outputItems: Internal[] = [];
      // FIXME: drop `originalItems` once the union decoder can reverse member
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
          ? panic(`Can't set default for ${inputExpression(mut)}`)
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
            `Invalid default for ${inputExpression(mut)}: ${
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
        return B_refine(originalDecoder(input), nextSchema, U, nextSchema);
      };

      // FIXME: This looks wrong, but this is how it was with prev architecture
      to.decoder = noopDecoder;

      mut.to = to;
    } else {
      panic(`Can't set default for ${inputExpression(mut)}`);
    }
  });
};

// @__NO_SIDE_EFFECTS__
export const Option_getOr = (schema: Internal, defaultValue: unknown): Internal =>
  Option_getWithDefault(schema, { type: "value", value: defaultValue });
// @__NO_SIDE_EFFECTS__
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
    currentAdditionalItems !== U &&
    currentAdditionalItems !== additionalItems &&
    typeof currentAdditionalItems !== objectTag
  ) {
    const mut = copySchema(schema);
    mut.additionalItems = additionalItems;
    if (deep) {
      const items = schema.items;
      if (items !== U) {
        mut.items = items.map((s) => Object_setAdditionalItems(s, additionalItems, deep));
      }

      const properties = schema.properties;
      if (properties !== U) {
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

// @__NO_SIDE_EFFECTS__
export const strip = (schema: Internal): Internal => {
  return Object_setAdditionalItems(schema, "strip", false);
}

// @__NO_SIDE_EFFECTS__
export const deepStrip = (schema: Internal): Internal => {
  return Object_setAdditionalItems(schema, "strip", true);
}

// @__NO_SIDE_EFFECTS__
export const strict = (schema: Internal): Internal => {
  return Object_setAdditionalItems(schema, "strict", false);
}

// @__NO_SIDE_EFFECTS__
export const deepStrict = (schema: Internal): Internal => {
  return Object_setAdditionalItems(schema, "strict", true);
}

export type TupleCtx = {
  item: (idx: number, schema: Internal) => unknown;
  tag: (idx: number, value: unknown) => void;
};

export type Meta<TValue> = {
  name?: string;
  title?: string;
  description?: string;
  deprecated?: boolean;
  examples?: TValue[];
  errorMessage?: SchemaErrorMessage;
};

// TODO: Better test reverse
// @__NO_SIDE_EFFECTS__
export const meta = <TValue>(schema: Internal, data: Meta<TValue>): Internal => {
  const mut = copySchema(schema);
  if (data.name !== U) {
    if (data.name === "") {
      mut.name = U;
    } else {
      mut.name = data.name;
    }
  }
  if (data.title !== U) {
    if (data.title === "") {
      mut.title = U;
    } else {
      mut.title = data.title;
    }
  }
  if (data.description !== U) {
    if (data.description === "") {
      mut.description = U;
    } else {
      mut.description = data.description;
    }
  }
  if (data.deprecated !== U) {
    mut.deprecated = data.deprecated;
  }
  if (data.examples !== U) {
    if (data.examples.length === 0) {
      delete mut.examples;
    } else {
      mut.examples = data.examples.map(getDecoder(reverse(schema)));
    }
  }
  if (data.errorMessage !== U) {
    const em = data.errorMessage;
    if (Object.keys(em).length === 0) {
      mut.errorMessage = U;
    } else {
      mut.errorMessage = em;
    }
  }
  return mut;
}

// @__NO_SIDE_EFFECTS__
export const brand = (schema: Internal, id: string): Internal => {
  const mut = copySchema(schema);
  mut.name = id;
  return mut;
}
