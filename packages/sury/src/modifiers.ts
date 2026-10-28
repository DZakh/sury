// Modifiers: everything that takes a schema and returns a changed schema -
// refinements' machinery, transforms, metadata, object modes and defaults.
// Distinct from `operations.ts`, which compiles a schema into a callable.

import {
  type AdditionalItems,
  baseSchema,
  type Builder,
  type Check,
  configurableValueOptions,
  copySchema,
  copyTo,
  functionTag,
  inputExpression,
  type Internal,
  objectTag,
  panic,
  pathEmpty,
  refTag,
  reversedKey,
  type SchemaErrorMessage,
  setHas,
  U,
  undefinedTag,
  unknown,
  unknownTag,
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
  B_pathArg,
  B_refine,
  B_unsupportedDecode,
  B_throw,
  B_conversionFail,
} from "./builder";
import {
  objectDecoder
} from "./composites";
import {
 decodeOutput,
 getOutputSchema,
 nestedLoc,
 nestedOptionParser,
 parse,
 reverse,
 setDefault
} from "./parse";
import {
 Literal_parse,
 nullLiteral,
 unit
} from "./primitives";
import {
 unionFactory,
 unionWrap
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

// The schema read one option level deeper: an undefined it outputs is Some(None)
// and a Some(None) marker counts one more Some. Recurses into output unions,
// since a coder's input never reaches its output.
export const someOf = (schema: Internal): Internal => {
  const out = getOutputSchema(schema);
  const nestedSchema = out.properties?.[nestedLoc];
  if (out.type === undefinedTag) {
    return nestedOption(schema);
  } else if (nestedSchema !== U) {
    return updateOutput<Internal>(schema, (mut) => {
      // copySchema, not a spread: a spread keeps the original's seq,
      // and two schemas sharing a seq can collide in the seq-keyed
      // operation caches.
      const bumped = copySchema(nestedSchema);
      bumped.const = (nestedSchema.const as number) + 1;
      const properties: Record<string, Internal> = {};
      properties[nestedLoc] = bumped;
      mut.properties = properties;
    });
  } else if (out.anyOf !== U) {
    return updateOutput<Internal>(schema, (mut) => {
      mut.anyOf = out.anyOf!.map(someOf);
    });
  } else {
    return schema;
  }
}

// @__NO_SIDE_EFFECTS__
export const option = (item: Internal): Internal =>
  unionWrap(someOf(item), [unit]);

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
    const fail = B_conversionFail(input, input.s);
    const embeddedCheck = B_embed(input, (value: unknown, path?: Path) => {
      try {
        return refineCheck(value);
      } catch (cause) {
        B_throw(fail(cause, path));
      }
    });
    return [
      {
        c: (inputVar) => `${embeddedCheck}(${inputVar}${B_pathArg(input)})`,
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
// reading (`true` opens the source) and rides the target as a reading bit, the one
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
        targetMut.flags = (targetMut.flags & ~12) | (decode ? 4 : 8);
        // The arm's decoder is what meets the source, so a union target hands
        // the reading to the arm carrying a payload. The array is replaced
        // only then: unionResolveToUnion knows an arm producing the whole
        // target union by the shared `anyOf` reference.
        if (targetMut.anyOf?.some((arm) => arm.flags & 3)) {
          targetMut.anyOf = targetMut.anyOf.map((arm) => {
            if (!(arm.flags & 3)) return arm;
            const armMut = copySchema(arm);
            armMut.flags = (arm.flags & ~12) | (decode ? 4 : 8);
            return armMut;
          });
        }
      }
      mut.to = targetMut;
    } else {
      mut.to = target;
    }
    // CONTENT_CODEC_SPEC.md rule 3, written down the moment it becomes true: a
    // payload that gains a `.to` names what it holds, so the link into it opens
    // its source. Materialized here rather than read off `.to !== U` by the
    // payload schemas, because `reverse` re-points `.to` and would lose it,
    // while it carries the reading across.
    if (mut.flags & 3 && !(mut.flags & 12)) mut.flags |= 4;
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

export type Slot = Builder | boolean | undefined;

// A ref met while its own definer still runs has no definition to ask yet, and
// is viewed on demand like one that converts. A parser always sits beside a
// `.to`, so `to` stands for both.
const converting = (schema: Internal, seen: Set<Internal>): boolean =>
  !seen.has(schema) &&
  (seen.add(schema),
  !!(schema.to || schema.serializer || (schema.definition && !schema.definition())) ||
    [
      schema.definition?.(),
      schema.additionalItems,
      ...Object.values(schema.properties || {}),
      ...(schema.items || []),
      ...(schema.anyOf || []),
    ].some((child) => typeof child === "object" && converting(child, seen)));

// One view per definition: compiled operations are cached on the object a ref's
// `definition` answers, and the view of a recursive definition holds refs back
// to it.
const outputViews = /* @__PURE__ */ new WeakMap<Internal, Internal>();

// The definition each output view (`outputView`) was built from.
const viewSources = /* @__PURE__ */ new WeakMap<Internal, Internal>();

// A view claims exactly its source's output, so whoever names definitions
// (jsonschema.ts `refKey`) names both as one. Reversed too, which is how an
// output is described, but only through a reverse already built. A view
// converts nothing, so it is its own view and never another's source.
export const viewSource = (def: Internal): Internal => {
  const reversed = Object.hasOwn(def, reversedKey) && viewSources.has(def.r!);
  const source = viewSources.get(reversed ? def.r! : def);
  return source === U ? def : reversed ? source.r! : source;
};

// What a schema's output claims, as a schema of its own: every node at the end
// of its chain, children likewise, its refinements kept and its conversions
// dropped. A value already converted is checked against it without converting
// it again.
const outputView = (schema: Internal): Internal => {
  let last = schema;
  while (last.to) last = last.to;
  let converts = last !== schema || !!last.serializer;
  const viewOf = (child: Internal): Internal => {
    const view = outputView(child);
    if (view !== child) converts = true;
    return view;
  };
  const view = copySchema(last);
  delete view.serializer;
  // No schema describes what a coder into `unknown` (`S.list`) outputs, so a
  // value is told apart the way S.isOutput tells it: by the node's own reverse
  // accepting it, whose failure is the reason. A built-in conversion's Output
  // is the `unknown` it claims, and one whose reverse doesn't compile (a
  // "never" or decode-only encode) has nothing to hold it to.
  if (last !== schema && last.type === unknownTag && last.serializer) {
    const own = view.refiner;
    view.refiner = (val) => {
      const reversed = reverse(schema);
      const is = decodeOutput(reversed, 4096);
      const checks = own ? own(val) : [];
      if (!is) return checks;
      const embedded = B_embed(val, is);
      return checks.concat({
        c: (v) => `${embedded}(${v})`,
        // Asked again only for the reason: inside a union a failing arm is how
        // a later one is reached, and the boolean costs no error.
        f: (input) => {
          const fail = B_conversionFail(input, input.s);
          return (value, path) => {
            let cause: unknown;
            try {
              decodeOutput(reversed)!(value);
            } catch (thrown) {
              cause = thrown;
            }
            return fail(cause, path);
          };
        },
      });
    };
  }
  if (view.properties) {
    const views: Record<string, Internal> = Object.create(null);
    for (const key in view.properties) views[key] = viewOf(view.properties[key]!);
    view.properties = views;
  }
  if (view.items) view.items = view.items.map(viewOf);
  if (typeof view.additionalItems === "object") view.additionalItems = viewOf(view.additionalItems);
  if (view.anyOf) {
    view.anyOf = view.anyOf.map(viewOf);
    view.has = {};
    for (const member of view.anyOf) setHas(view.has, member);
  }
  const definition = last.definition;
  if (definition) {
    const def = definition();
    if (def === U || converting(def, new Set())) converts = true;
    // Answered on demand: viewing the definition now would view the refs
    // inside it, and through them the definition again.
    view.definition = () => {
      const def = definition();
      if (def === U) return def;
      let defView = outputViews.get(def);
      // While it is built, a ref inside it asking is told the definition itself:
      // only whether it converts is read then, never compiled.
      if (defView === U) {
        outputViews.set(def, def);
        outputViews.set(def, (defView = outputView(def)));
        if (defView !== def) viewSources.set(defView, def);
      }
      return defView;
    };
  }
  return converts ? view : schema;
};

// An output-seam coder claims the target as its result, so a target that
// still converts on its own would have that conversion skipped. The junction
// seam of `to` feeds the target's chain instead, so it stays legal there, as do
// the slots that place no coder. A reading is exempt with `B_neverSlot`:
// neither places a coder, so neither claims the target's result - the very case
// a reading exists for is a target that converts on its own.
// A coder's result is what the target's output claims (its view): the
// target's own conversions, a field's coder among them, would run again over a
// value already past them. An encode coder is handed the target's output, so
// the link is to the view, and a decode with no coder runs the plain link's own
// step into the target from where the seam's stands. A decode coder beside an
// encode with none keeps the link to the target, which that encode converts
// through, and checks its result against the view.
export const seamTarget = (schema: Internal, target: Internal, decode: Slot, encode: Slot): Internal => {
  const view =
    (typeof decode === functionTag && decode !== B_neverSlot) || (typeof encode === functionTag && encode !== B_neverSlot)
      ? target.to
        ? panic(`The target already converts. Chain S.to instead of passing a custom codec`)
        : outputView(target)
      : target;
  if (view === target) return codecTo(schema, target, decode, encode);
  if (typeof encode === functionTag) {
    return codecTo(schema, view, typeof decode === functionTag ? decode : seamStep(seamPlain(schema, target, decode as boolean | undefined)), encode);
  }
  return codecTo(schema, target, seamStep((input) => B_refine((decode as Builder)(input), view, U, view)), encode);
};

// The plain link's step into the target, from where the seam's link stands.
const seamPlain = (schema: Internal, target: Internal, reading: boolean | undefined) => (input: Val): Val => {
  let into = codecTo(schema, target, reading);
  while (into.to!.to) into = into.to!;
  const from = B_refine(input, into, U, into);
  from.io = true;
  return from;
};

// Runs `step` to its end, and hands the value on as the seam link's next node's
// output, which it now is.
const seamStep = (step: (input: Val) => Val): Builder => (input: Val) => {
  const through = B_refine(parse(step(input)), U, U, input.e.to);
  through.io = true;
  return through;
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
        // The default is read as the item on every decode, so an item that is
        // the definition being built would read the default as that definition,
        // find the same absent field in it, and read its default, without end.
        if (outputSchema.type === refTag && !outputSchema.definition!()) {
          panic(
            `Can't set default for ${inputExpression(mut)}: the default is read as ${outputSchema.name}, which would need a default of its own`
          );
        }
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
      setDefault(
        mut,
        originalItems.length === 1 ? originalItems[0]! : unionFactory(originalItems),
        default_.value
      );
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
      const result = B_refine(output);
      // Already a value of the Output type, so it lands past the item's
      // decoder: a container's decoder would read it as Input (#452).
      result.io = true;
      return result;
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
      const decode = decodeOutput(reverse(schema));
      if (decode) mut.examples = data.examples.map(decode);
      else delete mut.examples;
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
