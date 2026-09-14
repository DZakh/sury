// Modifiers: everything that takes a schema and returns a changed schema -
// refinements' machinery, transforms, metadata, object modes and defaults.
// Distinct from `operations.ts`, which compiles a schema into a callable.

import {
  anyOfTag,
  type AdditionalItems,
  baseSchema,
  type Builder,
  type Check,
  configurableValueOptions,
  copySchema,
  copyTo,
  functionTag,
  getOrRethrow,
  inputExpression,
  type Internal,
  objectTag,
  panic,
  pathEmpty,
  type SchemaErrorMessage,
  setHas,
  U,
  undefinedTag,
  unknown,
  updateOutput,
  type Val,
  valKey,
  type Path,
} from "./base";
import {
  _var,
  B_embed,
  B_contentDiffers,
  B_contentNode,
  B_inlineConst,
  B_invalidInputBuilder,
  B_invalidOperation,
  B_neverSlot,
  B_next,
  B_nextConst,
  B_refine,
  B_unsupportedDecode,
  B_throw,
  B_makeInvalidConversionDetails,
} from "./builder";
import {
  objectDecoder
} from "./composites";
import {
 getOp,
 getOutputSchema,
 nestedLoc,
 nestedOptionParser,
 reverse
} from "./parse";
import {
 Literal_parse,
 nullLiteral,
 unit
} from "./primitives";
import {
 unionFactory
} from "./union";

// Lives here rather than in composites.ts so objectDecoder's module has no
// static edge to unionFactory. baseSchema, not an object literal: a union
// member has to carry the schema prototype, where the lazily derived reverse
// (`schema.r`) lives.
const nestedNone = (): Internal => {
  const itemSchema = Literal_parse(0);
  const properties: Record<string, Internal> = {};
  properties[nestedLoc] = itemSchema;
  const mut = baseSchema(objectTag, false, objectDecoder);
  mut.required = [nestedLoc];
  mut.properties = properties;
  mut.additionalItems = "strip";
  mut.serializer = (input: Val) => {
    const nextSchema = input.e.to!;
    return B_nextConst(input, nextSchema, nextSchema);
  };
  return mut;
}

const nestedOption = (item: Internal): Internal => {
  return updateOutput<Internal>(item, (mut) => {
    mut.to = nestedNone();
    mut.parser = nestedOptionParser;
  });
}

export const optionFactory = (item: Internal, unitSchema: Internal = unit): Internal => {
  const out = getOutputSchema(item);
  if (out.type === undefinedTag) {
    return unionFactory([unitSchema, nestedOption(item)]);
  } else if (out.type === anyOfTag) {
    const anyOf = out.anyOf;
    const has = out.has;
    return updateOutput<Internal>(item, (mut) => {
      const schemas = anyOf!;
      const mutHas = { ...has! };

      const newAnyOf: Internal[] = [];
      for (let idx = 0; idx < schemas.length; idx++) {
        const schema = schemas[idx]!;
        let toPush: Internal;
        const schemaOut = getOutputSchema(schema);
        if (schemaOut.type === undefinedTag) {
          mutHas[unitSchema.type] = true;
          newAnyOf.push(unitSchema);
          toPush = nestedOption(schema);
        } else if (schemaOut.properties !== U) {
          const properties = schemaOut.properties;
          const nestedSchema = properties[nestedLoc];
          if (nestedSchema !== U) {
            toPush = updateOutput<Internal>(schema, (mut) => {
              // copySchema, not a spread: a spread keeps the original's seq,
              // and two schemas sharing a seq can collide in the seq-keyed
              // operation caches.
              const bumped = copySchema(nestedSchema);
              bumped.const = (nestedSchema.const as number) + 1;
              const properties: Record<string, Internal> = {};
              properties[nestedLoc] = bumped;
              mut.properties = properties;
            });
          } else {
            toPush = schema;
          }
        } else {
          toPush = schema;
        }
        newAnyOf.push(toPush);
      }

      if (newAnyOf.length === schemas.length) {
        mutHas[unitSchema.type] = true;
        newAnyOf.push(unitSchema);
      }

      mut.anyOf = newAnyOf;
      mut.has = mutHas;
    });
  } else {
    return unionFactory([item, unitSchema]);
  }
}

// @__NO_SIDE_EFFECTS__
export const option = (item: Internal): Internal => {
  return optionFactory(item, unit);
}

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
  mut.noValidation = value;
  return mut;
};

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
  path?: Path
): Internal => {
  const message = error !== U ? error : "Refinement failed";
  const extraPath = path !== U ? path : pathEmpty;
  return internalRefine(schema, (_) => (input) => {
    // Whatever the check throws is that refinement failing, the same way a
    // coder's throw is its conversion failing (`B_conversion`): a `TypeError` it
    // hit on a value it was never written for is not a bug in the caller's
    // `.catch`. Wrapped in the embedded function rather than in a generated
    // `try`, which would catch the refinement's own failure raise too.
    const embeddedCheck = B_embed(input, (value: unknown) => {
      try {
        return refineCheck(value);
      } catch (cause) {
        B_throw(B_makeInvalidConversionDetails(input, input.s, cause));
      }
    });
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
// given, and an object schema strips unknown keys on the way out - an output
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
// reading (`true` opens the source) and rides the target as `opens`, the one
// reading a link has. `U` means no slot, i.e. the built-in conversion - and
// whether that exists is the payload schemas' question, asked while compiling.
export const codecTo = (
  schema: Internal,
  target: Internal,
  decode?: Builder | boolean,
  encode?: Builder | boolean
): Internal => {
  const root: Internal = updateOutput(schema, (mut) => {
    const opened = typeof decode === "boolean";
    const parser = typeof decode === functionTag ? (decode as Builder) : U;
    const serializer = typeof encode === functionTag ? (encode as Builder) : U;
    if (serializer !== U || opened) {
      // copySchema keeps `anyOf` shared by reference with the target, and
      // unionResolveToUnion recognizes an arm producing the whole target union
      // by exactly that shared array. A deep copy here would silently break
      // Option.getOr's default arms.
      //
      // A link built with either slot owns its tail: the slot lands on this
      // copy, never on a target the caller may link to again.
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
    // CONTENT_CODEC_SPEC.md rule 3, written down the moment it becomes true: a
    // payload that gains a `.to` names what it holds, so the link into it opens
    // its source. Materialized here rather than read off `.to !== U` by the
    // payload schemas, because `reverse` re-points `.to` and would lose it,
    // while it carries `opens` across.
    if (mut.content !== U && mut.opens === U) mut.opens = true;
    if (parser !== U) {
      mut.parser = parser;
    }
  });
  return root;
};

type LinkNode = {
  s: Internal;
  t: Internal;
  k: unknown; // the `S.to` reading this link was written with
  r: Internal;
  n: LinkNode | undefined;
};
const linkKey = "l";

// A slotless link is a pure function of its two arguments, so the chain it
// builds is shared rather than rebuilt: written inline in a hot path -
// `S.parseOrThrow(S.jsonString.with(S.to, userSchema))(body)`, once per request
// - a fresh chain is also a fresh operation-cache target, so the schema
// recompiled every call. 7.2us against 293ns. Sound only because a compiled
// operation no longer writes anything back onto the schema it compiled
// (`OpNode`).
//
// Kept apart from `codecTo` so the three callers that always pass slots -
// `trim`, `list`, `Option_getOr` - carry none of it: each reshapes its own
// result afterwards, so none could be interned anyway, and sharing one function
// made them pay up to 60 gzipped bytes for a cache they never reach.
//
// `reading` joins the key, because it is bounded by construction: absent or one
// of two strings, so three entries per pair. A coder object or an inline
// function is fresh every call and would add a node it can never hit again - on
// a pair of singletons, which never dies.
//
// The node goes on the newer of the pair, non-enumerable, exactly as
// `addOpNode` stores an operation: `seq` is monotonic, so it lands on the
// argument that dies first and a long-lived schema paired with throwaways keeps
// nothing alive.
// @__NO_SIDE_EFFECTS__
export const linkTo = (
  schema: Internal,
  target: Internal,
  reading?: unknown,
  decode?: boolean,
  encode?: boolean
): Internal => {
  const store = schema.seq! > target.seq! ? schema : target;
  let node = (store as unknown as Record<string, LinkNode | undefined>)[linkKey];
  while (node) {
    if (node.s === schema && node.t === target && node.k === reading) return node.r;
    node = node.n;
  }
  const root = codecTo(schema, target, decode, encode);
  const created: LinkNode = {
    s: schema,
    t: target,
    k: reading,
    r: root,
    n: (store as unknown as Record<string, LinkNode | undefined>)[linkKey],
  };
  (configurableValueOptions as Record<string, unknown>)[valKey] = created;
  Object.defineProperty(store, linkKey, configurableValueOptions as PropertyDescriptor);
  return root;
};

// Not initSchema: that would stamp the self-reverse marker, and this codec's
// reverse (unit -> null) must stay lazily derived — copySchema drops it.
export const nullAsUnit: Internal = /* @__PURE__ */ copyTo(nullLiteral, unit);

// A default is either an eager value or a lazily-called callback - used only
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
        (getOp(0, 2, unknown, item) as (input: unknown) => unknown)(v);
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
        mut.default = (getOp(0, 1, reverse(originalItem)) as (input: unknown) => unknown)(v);
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
      // The same seam B_conversion's `trusted` provides: `vc` checks emit at
      // the pre-transform slot, so the item's input refiners would run over
      // the absent value that came in rather than the default that replaced it.
      return B_refine(output);
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

export type ObjectCtx = {
  // @as("f") - field
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
  // mode - a tuple is strict from the start, and its object items are not.
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

export type Meta<TValue> = {
  name?: string;
  title?: string;
  description?: string;
  deprecated?: boolean;
  examples?: TValue[];
  errorMessage?: SchemaErrorMessage;
};

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
      // A full parse through the reversed schema: the example is checked as an
      // output value and stored in its input form. A never or async encode
      // makes that uncomputable, so it is skipped rather than thrown; a
      // per-value failure still names the author's bad example.
      try {
        mut.examples = data.examples.map(
          getOp(0, 2, unknown, reverse(schema)) as (input: unknown) => unknown,
        );
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
