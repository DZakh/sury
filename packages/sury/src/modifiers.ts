// Modifiers: everything that takes a schema and returns a changed schema —
// refinements' machinery, transforms, metadata, object modes and defaults.
// Distinct from `operations.ts`, which compiles a schema into a callable.

import {
  type AdditionalItems,
  type Builder,
  type Check,
  copySchema,
  functionTag,
  getOrRethrow,
  inputExpression,
  type Internal,
  objectTag,
  panic,
  pathEmpty,
  pathFromArray,
  type SchemaErrorMessage,
  U,
  undefinedTag,
  unknown,
  updateOutput,
  type Val,
} from "./base";
import {
  _var,
  B_embed,
  B_contentSlot,
  B_inlineConst,
  B_invalidInputBuilder,
  B_neverSlot,
  B_next,
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
  const extraPath = path !== U ? pathFromArray(path) : pathEmpty;
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

// The `S.to` codec wiring: the decode slot rides the source's output node as
// its `parser`, the encode slot a copy of the target as its `serializer`.
// That placement is what makes reversal free: `reverseSwap` trades the two
// fields, so the encode coder becomes the reversed chain's parser and double
// reversal restores every slot. Slot semantics (auto/never/async/the JS
// shorthand) are resolved by the caller into Builders; a boolean is a content
// reading (`true` opens the direction's own source) and rides the schema that
// direction converts into, which is what makes reversal swap those too. `U`
// means no slot, i.e. the built-in conversion — or, where the pair has two of
// them, the rejection `B_contentSlot` supplies.
export const codecTo = (
  schema: Internal,
  target: Internal,
  decode?: Builder | boolean,
  encode?: Builder | boolean
): Internal => {
  const root: Internal = updateOutput(schema, (mut) => {
    const ambiguous = B_contentSlot(mut, target);
    const opened = typeof decode === "boolean";
    const parser = typeof decode === functionTag ? (decode as Builder) : opened ? U : ambiguous;
    const serializer =
      typeof encode === functionTag
        ? (encode as Builder)
        : typeof encode === "boolean"
          ? U
          : ambiguous;
    if (serializer !== U || opened) {
      // copySchema keeps `anyOf` shared by reference with the target, and
      // unionResolveToUnion recognizes an arm producing the whole target union
      // by exactly that shared array. A deep copy here would silently break
      // Option.getOr's default arms.
      const targetMut = copySchema(target);
      if (serializer !== U) {
        targetMut.serializer = serializer;
      }
      if (opened) {
        targetMut.opens = decode as boolean;
      }
      mut.to = targetMut;
    } else {
      mut.to = target;
    }
    if (parser !== U) {
      mut.parser = parser;
    }
    if (typeof encode === "boolean") {
      mut.opens = encode;
    }
  });
  // copySchema carries a cached isAsync/hasTransform from the source and a
  // custom slot can change both, so let the next compile re-derive them.
  // Slotless links keep the fast path: a built-in conversion never turns
  // async.
  if (decode !== U || encode !== U) {
    delete root.isAsync;
    delete root.hasTransform;
  }
  return root;
};

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

// Every undefined-producing variant converts to the item union, supplying the
// default on decode and taking the never slot on encode so it yields to its
// siblings there. Spelling the default as ordinary union arms is what lets the
// planner treat it like any other variant.
export const Option_getWithDefault = (schema: Internal, default_: OptionDefault): Internal => {
  return updateOutput(schema, (mut) => {
    const anyOf = mut.anyOf;
    if (anyOf === U) {
      return panic(`Can't set default for ${inputExpression(mut)}`);
    }
    const outputItems: Internal[] = [];
    const originalItems: Internal[] = [];

    for (let idx = 0; idx < anyOf.length; idx++) {
      const variant = anyOf[idx]!;
      const outputSchema = getOutputSchema(variant);
      if (outputSchema.type !== undefinedTag) {
        // Dedupe by identity: two arms sharing one output instance (the bool
        // singleton) would otherwise make every rule-4 match ambiguous.
        if (!outputItems.includes(outputSchema)) {
          outputItems.push(outputSchema);
        }
        originalItems.push(variant);
      }
    }

    const item: Internal =
      outputItems.length === 0
        ? panic(`Can't set default for ${inputExpression(mut)}`)
        : outputItems.length === 1
          ? outputItems[0]!
          : unionFactory(outputItems);

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
      const originalItem: Internal =
        originalItems.length === 1 ? originalItems[0]! : unionFactory(originalItems);
      // Best-effort input form for JSON Schema metadata. A never or async
      // encode makes it uncomputable, so skip it rather than throw: metadata
      // is not a value operation.
      try {
        mut.default = (getDecoder(reverse(originalItem)) as (input: unknown) => unknown)(v);
      } catch (_exn) {}
    }

    // Not B_conversion: an eager default inlines as a constant instead of
    // costing an embed slot and a call, and a callback's throw keeps escaping
    // raw the way it always did.
    const decodeB: Builder = (input) => {
      const target = input.e.to!;
      const output = B_next(
        input,
        default_.type === "value"
          ? B_inlineConst(input, Literal_parse(default_.value))
          : `${B_embed(input, default_.callback)}()`,
        target,
        target
      );
      if (default_.type === "value") {
        // A constant inline is idempotent, so re-reads need no var. The
        // callback form stays materializable, since re-reading it would call
        // the callback twice.
        output.v = _var;
      }
      return output;
    };
    mut.anyOf = anyOf.map((variant) =>
      getOutputSchema(variant).type === undefinedTag
        ? codecTo(variant, item, decodeB, B_neverSlot)
        : variant
    );
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
  const set =
    currentAdditionalItems !== U &&
    currentAdditionalItems !== additionalItems &&
    typeof currentAdditionalItems !== objectTag;
  // A deep pass still has to descend through a level that already carries the
  // mode — a tuple is strict from the start, and its object items are not.
  // When nothing changes anywhere in the subtree, return the same object:
  // a repeated call stays identity-stable, so the operation cache (keyed on
  // the schema object) keeps hitting.
  let changed = set;
  const mapItem = (s: Internal): Internal => {
    const mapped = Object_setAdditionalItems(s, additionalItems, deep);
    if (mapped !== s) {
      changed = true;
    }
    return mapped;
  };
  const items = deep ? schema.items : U;
  const newItems = items !== U ? items.map(mapItem) : U;
  const properties = deep ? schema.properties : U;
  const newProperties =
    properties !== U
      ? Object.fromEntries(
          Object.keys(properties).map((key) => [key, mapItem(properties[key]!)])
        )
      : U;
  if (!changed) {
    return schema;
  }
  const mut = copySchema(schema);
  if (set) {
    mut.additionalItems = additionalItems;
  }
  if (newItems !== U) {
    mut.items = newItems;
  }
  if (newProperties !== U) {
    mut.properties = newProperties;
  }
  return mut;
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
      // A never or async encode makes the input-form examples uncomputable,
      // so skip them rather than throw. Only the operation-level rejection is
      // absorbed; a per-value failure still names the author's bad example.
      try {
        mut.examples = data.examples.map(getDecoder(reverse(schema)));
      } catch (exn) {
        if ((getOrRethrow(exn) as unknown as { code: string }).code !== "invalid_operation") {
          throw exn;
        }
        delete mut.examples;
      }
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
