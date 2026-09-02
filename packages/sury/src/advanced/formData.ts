// `S.formData` — a form submission as a browser or `Request.formData()` hands
// it over, and the body a `fetch` call sends. An entry is a string or a
// `File`, so an object schema reads its fields through the string coercions
// the env pattern already compiles (`"42"` -> 42), a file field takes the entry
// as it is, and a repeated key is an array. Nothing reads file bytes, so both
// directions are sync.
//
// Not on the content axis (CONTENT_CODEC_SPEC.md): a form has no JSON document
// form and no format opens into one, so a link to it never has two readings,
// and a `FormData` in a JSON position has no document, the way `S.blob` has
// none. Bracket notation (`user[name]`) is deliberately out — a nested value
// travels as a `S.jsonString.with(S.to, …)` field.

import {
  anyOfTag,
  arrayTag,
  copySchema,
  inlinedValueFromString,
  instanceTag,
  initSchema,
  type Internal,
  isOptional,
  pathConcat,
  pathFromInlinedLocation,
  setHas,
  stringTag,
  type Tag,
  tagFlags,
  U,
  undefinedTag,
  unknown,
  unknownTag,
  type Val
} from "../base";
import {
  _notVarAtParent,
  _var,
  B_addObjectField,
  B_dynamicScope,
  B_embed,
  B_embedInvalidInput,
  B_markOutput,
  B_merge,
  B_mergeWithPathPrepend,
  B_next,
  B_scope,
  B_unsupportedDecode,
  B_varWithoutAllocation
} from "../builder";
import {
  arrayFactory,
  completeObjectVal,
  makeObjectVal,
  valGet
} from "../composites";
import {
  getOutputSchema,
  instanceDecoder,
  parse,
  unsupportedInstance
} from "../parse";
import {
 bool,
 string
} from "../primitives";

// An empty text input submits `""`, and the codec reads it as absent: the
// field takes the `undefined` an unset optional takes, so a required string
// rejects it — unless the schema admits the empty string by saying so, with
// `minLength 0` or the literal `""`. Decided on the field's input schema,
// since the rule is about what the wire hands over.
const admitsEmpty = (schema: Internal): boolean =>
  schema.type === anyOfTag
    ? schema.anyOf!.some(admitsEmpty)
    : schema.type === stringTag && (schema.minLength === 0 || schema.const === "");

// A required boolean field can only be a checkbox — nothing else a browser
// sends is one — so it reads the way a checkbox submits: absent (or the empty
// value) is `false`, and a present entry is `"on"`, or the `"true"`/`"false"`
// a hidden input carries. An optional boolean keeps the tri-state, for a form
// that tells "unchecked" from "not on the page".
const isCheckbox = (schema: Internal): boolean =>
  (tagFlags[schema.type]! & 8) !== 0 && schema.const === U;

const readCheckbox = (item: Val, schema: Internal): Val => {
  const v = item.i;
  const outputVar = B_varWithoutAllocation(item.g);
  const output = B_next(item, outputVar, bool, schema);
  output.v = _var;
  output.cp = `let ${outputVar};(${outputVar}=${v}==="on"||${v}==="true")||${v}==="false"||${v}===void 0||${B_embedInvalidInput(
    item,
    schema,
  )};`;
  return B_markOutput(output, item);
};

// A repeated key is how a form carries an array, and `getAll` is its read —
// `[]` when the key is absent, never `undefined`.
const readsAll = (schema: Internal): boolean =>
  schema.type === anyOfTag ? schema.anyOf!.some(readsAll) : schema.type === arrayTag;

const isBlobClass = (class_: unknown): boolean => {
  const blobClass = (globalThis as { Blob?: unknown }).Blob as
    | (abstract new () => unknown)
    | undefined;
  return (
    blobClass !== U &&
    (class_ === blobClass || (class_ as { prototype?: unknown }).prototype instanceof blobClass)
  );
};

// A blob field takes the entry as it is, and so does `unknown`. Everything
// else on the wire is text, so the field reads through a `string` stage: the
// entry is checked to be one, and the field's own decoder coerces from there,
// exactly as it does from `S.record(S.string)`.
const takesEntry = (schema: Internal): boolean =>
  schema.type === anyOfTag
    ? schema.anyOf!.some(takesEntry)
    : schema.type === unknownTag ||
      (schema.type === instanceTag && isBlobClass(schema.class));

// A string-tagged target checks the entry is a string itself — and reads it
// as its own document where it is a format, which a `string` stage in front
// would instead escape into a JSON string value.
const fromText = (schema: Internal): Internal => {
  if ((tagFlags[schema.type]! & 2)) {
    return schema;
  }
  const text = copySchema(string);
  text.to = schema;
  return text;
};

// The optional's other arms, as one schema. Rebuilt from the union's own
// pieces rather than through unionFactory, so `S.formData` doesn't carry the
// union compiler for a form that never has an optional field.
const presentArm = (schema: Internal): Internal => {
  if (schema.type !== anyOfTag) {
    return schema;
  }
  const present = schema.anyOf!.filter((variant) => variant.type !== undefinedTag);
  if (present.length === 1) {
    return present[0]!;
  }
  const mut = copySchema(schema);
  mut.anyOf = present;
  const has: Partial<Record<Tag, boolean>> = {};
  present.forEach((variant) => setHas(has, variant.type));
  mut.has = has;
  return mut;
};

// `append` takes a string or a blob as it is; every other entry is the string
// the value converts to, through the same encoders a JSON document uses.
// `field` marks the whole field, as opposed to an item or an optional's arm:
// only there is a boolean the checkbox `readCheckbox` reads, so only there
// does it write like one — `"on"` when set, nothing otherwise.
const appendValue = (val: Val, fdVar: string, keyText: string, fd: Internal, field: boolean): string => {
  const schema = val.s;
  const tagFlag = tagFlags[schema.type]!;
  if (field && isCheckbox(schema)) {
    return `if(${val.i}){${fdVar}.append(${keyText},"on")}`;
  }
  if ((tagFlag & 128) && typeof schema.additionalItems === "object" && !schema.items!.length) {
    const arrayVar = val.v();
    const iterVar = B_varWithoutAllocation(val.g);
    const raiseCountBefore = val.g.t;
    // B_dynamicScope reads the item off `e`; the recursive call picks the
    // item's own target.
    val.e = schema;
    const item = B_dynamicScope(val, iterVar);
    const itemCode = B_mergeWithPathPrepend(
      item,
      val,
      iterVar,
      () => appendValue(item, fdVar, keyText, fd, false),
      raiseCountBefore,
    );
    return `for(let ${iterVar}=0;${iterVar}<${arrayVar}.length;++${iterVar}){${itemCode}}`;
  }
  if (schema.type === anyOfTag && schema.has![undefinedTag]) {
    // Absent is not an entry, so the whole append sits behind the guard.
    // Compiled on a chain detached from the field val, the way json.ts's
    // guardedJsonPiece does, so the conversion's own code lands inside it.
    const inputVar = val.v();
    const presentSchema = presentArm(schema);
    const detached = B_next(val, inputVar, presentSchema, presentSchema);
    detached.v = _var;
    detached.prev = U;
    return `if(${inputVar}!==void 0){${appendValue(detached, fdVar, keyText, fd, false)}}`;
  }
  if ((tagFlag & 2) || ((tagFlag & 8192) && isBlobClass(schema.class))) {
    return `${fdVar}.append(${keyText},${val.i});`;
  }
  if (!(tagFlag & ((4 | 8) | (32 | 1024) | (2048 | 8192) | 256))) {
    return B_unsupportedDecode(val, schema, fd);
  }
  val.io = false;
  val.e = string;
  const converted = parse(val);
  return B_merge(converted) + `${fdVar}.append(${keyText},${converted.i});`;
};

const objectToFormData = (input: Val): Val => {
  const fdVar = B_varWithoutAllocation(input.g);
  const properties = input.s.properties!;
  let code = `let ${fdVar}=new ${B_embed(input, input.e.class)}();`;
  for (const key in properties) {
    code += appendValue(valGet(input, key), fdVar, inlinedValueFromString(key), input.e, true);
  }
  const output = B_next(input, fdVar, input.e);
  output.v = _var;
  output.cp = code;
  return output;
};

// An optional field is the dict-to-object missing-key shape — a possibly
// absent entry, each arm converted on its own. The present entry converts to
// the field's present arm rather than to the whole optional: a string reaching
// `X | undefined` would be routed through the union rules, which reject
// `string | undefined` outright and otherwise dispatch on the text
// `"undefined"`. The absent one runs the undefined arm's own chain, which is
// where `S.optional(x, default)` keeps its default.
const readOptionalText = (item: Val, schema: Internal): Val => {
  const v = item.i;
  const arm = (source: Internal, target: Internal): string => {
    const armIn = B_scope(item);
    armIn.io = false;
    armIn.s = source;
    armIn.e = target;
    const armOut = parse(armIn);
    return B_merge(armOut) + (armOut.i === v ? "" : `${v}=${armOut.i};`);
  };
  const presentBody = arm(unknown, fromText(presentArm(schema)));
  const absent = schema.anyOf!.find((variant) => variant.type === undefinedTag)!;
  const absentBody = absent.to === U ? "" : arm(absent, absent);
  const output = B_next(item, v, getOutputSchema(schema), schema);
  output.v = _var;
  output.io = true;
  output.cp = `if(${v}!==void 0){${presentBody}}${absentBody === "" ? "" : `else{${absentBody}}`}`;
  return output;
};

const formDataToObject = (input: Val, target: Internal): Val => {
  const objectVal = makeObjectVal(input, target);
  const inputVar = input.v();
  const properties = target.properties!;
  for (const key in properties) {
    const schema = properties[key]!;
    const keyText = inlinedValueFromString(key);
    const all = readsAll(schema);
    const raw = takesEntry(schema);
    const optionalText = !all && !raw && isOptional(schema);
    const checkbox = !all && !raw && !optionalText && isCheckbox(schema);
    let expected = schema;
    if (all) {
      expected = presentArm(schema);
      const arrayItem = expected.additionalItems;
      if (!raw && typeof arrayItem === "object") {
        expected = copySchema(expected);
        expected.additionalItems = fromText(arrayItem);
      }
    } else if (!raw && !optionalText && !checkbox) {
      expected = fromText(schema);
    }
    // A field val the way valGet builds one: hung off the parent rather than
    // chained through `prev`, so each field's merge emits its own read and not
    // the parent's code again. Absent reads as `undefined`, the way a missing
    // object key does, so the error and the optional handling match an
    // object's; `||` folds the empty-string rule into the same read.
    // Canonical Val field order (see B_operationArg in builder.ts).
    const item: Val = {
      b: U,
      p: input,
      v: _notVarAtParent,
      i: all
        ? `${inputVar}.getAll(${keyText})`
        : `${inputVar}.get(${keyText})${admitsEmpty(schema) ? "??" : "||"}void 0`,
      s: all ? arrayFactory(unknown) : unknown,
      io: U,
      e: expected,
      prev: U,
      f: 0,
      d: U,
      fv: U,
      cp: "",
      hd: "",
      fz: U,
      vc: U,
      u: U,
      t: true,
      path: pathConcat(input.path, pathFromInlinedLocation(keyText)),
      g: input.g,
      o: U,
    };
    // Materialized up front: a read is a call, and a property read's cheap
    // re-read (see _notVarAtParent) would otherwise call it again wherever a
    // derived val copied the expression before the check named the var.
    item.v();
    B_addObjectField(
      objectVal,
      key,
      optionalText ? readOptionalText(item, schema) : checkbox ? readCheckbox(item, schema) : parse(item),
    );
  }
  return B_markOutput(completeObjectVal(objectVal), input);
};

export const formData: Internal = /* @__PURE__ */ initSchema(
  instanceTag,
  (input: Val): Val =>
    (tagFlags[input.s.type]! & 64) && typeof input.s.additionalItems === "string"
      ? objectToFormData(input)
      : instanceDecoder(input),
  (s) => {
    // Read inside the initializer, for the reason file.ts gives: a module-scope
    // member read is not something esbuild drops, and `FormData` landed in
    // Node 18.
    s.class = (globalThis as unknown as Record<string, unknown>)["FormData"];
    if (s.class === U) {
      unsupportedInstance(s, "formData");
    }
    s.encoder = (input, target) => {
      const targetTagFlag = tagFlags[target.type]!;
      return (targetTagFlag & 64) && typeof target.additionalItems === "string"
        ? formDataToObject(input, target)
        : // A union picks its variant by narrowing the form to an object it
          // isn't, so the dispatch never reaches the codec — say so here, where
          // the pair is still named.
          (targetTagFlag & 256)
          ? B_unsupportedDecode(input, input.s, target)
          : input;
    };
  },
);
