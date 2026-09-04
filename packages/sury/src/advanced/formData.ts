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
  type ErrorDetails,
  inlinedValueFromString,
  instanceTag,
  initSchema,
  type Internal,
  isOptional,
  pathConcat,
  setHas,
  tagFlags,
  type Tag,
  U,
  undefinedTag,
  unknown,
  unknownTag,
  type Val
} from "../base";
import {
  _var,
  B_addObjectField,
  B_dynamicScope,
  B_embed,
  B_embedInvalidInput,
  B_failWithArg,
  B_hoistDecl,
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
  instanceDecoder,
  parse,
  unsupportedInstance
} from "../parse";
import {
 string
} from "../primitives";

const isBlobClass = (class_: unknown): boolean => {
  const blobClass = (globalThis as { Blob?: unknown }).Blob as
    | (abstract new () => unknown)
    | undefined;
  return (
    blobClass !== U &&
    (class_ === blobClass || (class_ as { prototype?: unknown }).prototype instanceof blobClass)
  );
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

// A boolean field can only be a checkbox — nothing else a browser sends is one
// — so it reads the way a checkbox submits: absent is unchecked, and a present
// entry is `"on"`, or the `"true"`/`"false"` a hidden input carries. True of a
// boolean however it is wrapped: `S.optional(S.boolean, false)` is the natural
// spelling of "checkbox, default unchecked", and its entry is still `"on"`.
const isCheckbox = (schema: Internal): boolean =>
  schema.type === anyOfTag
    ? schema.anyOf!.every((variant) => variant.type === undefinedTag || isCheckbox(variant))
    : (tagFlags[schema.type]! & 8) !== 0 && schema.const === U;

// A blob takes the entry as it is, and so does `unknown`. Everything else on
// the wire is text, so it reads through a `string` stage: the entry is checked
// to be one, and the target's own decoder coerces from there, exactly as it
// does from `S.record(S.string)`.
const takesEntry = (schema: Internal): boolean =>
  schema.type === unknownTag ||
  (schema.type === instanceTag && isBlobClass(schema.class));

// A string-tagged target checks the entry is a string itself — and reads it as
// its own document where it is a format, which a `string` stage in front would
// instead escape into a JSON string value.
const fromText = (schema: Internal): Internal => {
  if (takesEntry(schema) || (tagFlags[schema.type]! & 2)) {
    return schema;
  }
  const text = copySchema(string);
  text.to = schema;
  return text;
};

// What a field's own `.to` converts from, so a reader that assembles the value
// itself can hand the parse loop something still owing that conversion.
const beforeTo = (schema: Internal): Internal => {
  if (schema.to === U) {
    return schema;
  }
  const mut = copySchema(schema);
  // `delete`, not `= U`: `unionIsTransparent` counts a schema's keys, and a
  // key left present with an undefined value stops every union flattening.
  delete mut.to;
  return mut;
};

// A repeated key is how a form carries an array, and `getAll` is its read.
const listItem = (schema: Internal): Internal | undefined => {
  const item = schema.additionalItems;
  return schema.type === arrayTag && typeof item === "object" && !schema.items!.length
    ? item
    : U;
};

// Every field decision, taken once off the target: `present` is what a supplied
// entry converts to, and the rest say how the entry is read. They are read
// together because they interact — a `S.array(S.file)` is a list whose *item*
// takes the entry, which is not the same question as the field taking one.
type Field = {
  optional: boolean;
  present: Internal;
  item: Internal | undefined;
  checkbox: boolean;
};

const classify = (schema: Internal): Field => {
  const present = presentArm(schema);
  return {
    optional: isOptional(schema),
    present,
    item: listItem(present),
    checkbox: isCheckbox(present),
  };
};

// The value the parse loop continues from, for a reader that assembled it
// rather than compiling one: `s` still owes the field's own `.to`, so the loop
// runs it instead of dropping it.
const assembled = (item: Val, schema: Internal, code: string, resultVar: string): Val => {
  const output = B_next(item, resultVar, beforeTo(schema), schema);
  output.v = _var;
  output.io = true;
  output.cp = code;
  return parse(B_markOutput(output, item));
};

// One arm of a possibly-absent entry, compiled on a scope of the field's own
// val and written back into `into` — the reader's result var, which is not
// always the val's own: a checkbox assembles its boolean elsewhere.
const armCode = (item: Val, source: Internal, target: Internal, into: string): string => {
  const armIn = B_scope(item);
  armIn.io = false;
  armIn.s = source;
  armIn.e = target;
  const armOut = parse(armIn);
  return B_merge(armOut) + (armOut.i === into ? "" : `${into}=${armOut.i};`);
};

// The absent arm's own chain, which is where `S.optional(x, default)` keeps its
// default. Empty when there is nothing to run.
const absentCode = (item: Val, field: Field, schema: Internal, into: string): string => {
  if (!field.optional) {
    return "";
  }
  const absent = schema.anyOf!.find((variant) => variant.type === undefinedTag)!;
  return absent.to === U ? "" : `else{${armCode(item, absent, absent, into)}}`;
};

// A possibly-absent entry, each arm converted on its own. The present one
// converts to the field's present arm rather than to the whole optional: a
// string reaching `X | undefined` would be routed through the union rules,
// which reject `string | undefined` outright and otherwise dispatch on the text
// `"undefined"`.
const readOptional = (
  item: Val,
  field: Field,
  schema: Internal,
  source: Internal,
  target: Internal,
): Val =>
  assembled(
    item,
    schema,
    `if(${item.i}!==void 0){${armCode(item, source, target, item.i)}}${absentCode(
      item,
      field,
      schema,
      item.i,
    )}`,
    item.i,
  );

const readCheckbox = (item: Val, field: Field, schema: Internal): Val => {
  const v = item.i;
  const outputVar = B_varWithoutAllocation(item.g);
  const read = `(${outputVar}=${v}==="on"||${v}==="true")||${v}==="false"||`;
  const fail = B_embedInvalidInput(item, schema);
  return assembled(
    item,
    schema,
    field.optional
      ? // Absent leaves the var undefined, which is the tri-state's third value
        // and what a default converts from.
        `let ${outputVar};if(${v}!==void 0){${read}${fail}}${absentCode(
          item,
          field,
          schema,
          outputVar,
        )}`
      : // An unchecked box sends nothing, so absent is `false` — which is what
        // the comparisons already assigned by the time the guard admits it.
        `let ${outputVar};${read}${v}===void 0||${fail};`,
    outputVar,
  );
};

// `append` takes a string or a blob as it is; every other entry is the string
// the value converts to, through the same encoders a JSON document uses.
const appendValue = (val: Val, fdVar: string, keyText: string): string => {
  const schema = val.s;
  const tagFlag = tagFlags[schema.type]!;
  if (isCheckbox(schema)) {
    // `false` is written out rather than omitted, even though a browser omits
    // an unchecked box: a default resolves before the encode sees the field
    // (`S.optional(S.boolean, true)` arrives as a plain boolean), so omitting
    // would hand that default back on the way in and lose the value. Only the
    // body Sury writes is more explicit than a browser's — the decode still
    // reads a real submission, where absent is `false`.
    const append = `${fdVar}.append(${keyText},${val.i}?"on":"false")`;
    return (tagFlag & 256) && schema.has![undefinedTag]
      ? `if(${val.i}!==void 0){${append}}`
      : `${append};`;
  }
  const item = listItem(schema);
  if (item !== U) {
    const arrayVar = val.v();
    const iterVar = B_varWithoutAllocation(val.g);
    const raiseCountBefore = val.g.t;
    // B_dynamicScope reads the item off `e`; the recursive call picks the
    // item's own target.
    val.e = schema;
    const itemVal = B_dynamicScope(val, iterVar);
    // Built before the merge, not inside its callback: `B_mergeWithCatch` runs
    // the merge first, so a var this materializes on the item afterwards would
    // have its `let` dropped and the loop body would read an undeclared name.
    const appendCode = appendValue(itemVal, fdVar, keyText);
    const itemCode = B_mergeWithPathPrepend(
      itemVal,
      val,
      iterVar,
      () => appendCode,
      raiseCountBefore,
    );
    return `for(let ${iterVar}=0;${iterVar}<${arrayVar}.length;++${iterVar}){${itemCode}}`;
  }
  if ((tagFlag & 256) && schema.has![undefinedTag]) {
    // Absent is not an entry, so the whole append sits behind the guard.
    // Compiled on a chain detached from the field val, the way json.ts's
    // guardedJsonPiece does, so the conversion's own code lands inside it.
    const inputVar = val.v();
    const presentSchema = presentArm(schema);
    const detached = B_next(val, inputVar, presentSchema, presentSchema);
    detached.v = _var;
    detached.prev = U;
    return `if(${inputVar}!==void 0){${appendValue(detached, fdVar, keyText)}}`;
  }
  if ((tagFlag & 2) || ((tagFlag & 8192) && isBlobClass(schema.class))) {
    return `${fdVar}.append(${keyText},${val.i});`;
  }
  if (!(tagFlag & ((4 | 8) | (32 | 1024) | (2048 | 8192) | 256))) {
    return B_unsupportedDecode(val, schema, formData);
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
    code += appendValue(valGet(input, key), fdVar, inlinedValueFromString(key));
  }
  const output = B_next(input, fdVar, input.e);
  output.v = _var;
  output.cp = code;
  return output;
};

const formDataToObject = (input: Val, target: Internal): Val => {
  const objectVal = makeObjectVal(input, target);
  const inputVar = input.v();
  const properties = target.properties!;
  for (const key in properties) {
    const schema = properties[key]!;
    const keyText = inlinedValueFromString(key);
    const field = classify(schema);
    const list = field.item !== U;

    // An empty text input submits `""`, and only an optional field reads it as
    // absent — that is the one case where the entry carries no value, and it is
    // what makes a default apply. A required field is handed `""` unchanged, so
    // the target answers for itself: `S.string` accepts it, `S.nonEmpty` and
    // `S.number` reject it in their own words. A checkbox is the exception
    // either way: a box carries no text, so an empty value is an unchecked box
    // rather than a value to report on.
    //
    // A list is `getAll`, which answers `[]` rather than `undefined`; an
    // optional one folds that empty read into absent, since a form has no other
    // way to submit an empty list.
    const readVar = B_varWithoutAllocation(input.g);
    if (list && field.optional) {
      // Two declarations rather than one self-referencing initializer, which
      // would read `readVar` inside its own `let` and hit the temporal dead
      // zone. Both land in the same `let`, in order.
      const allVar = B_varWithoutAllocation(input.g);
      B_hoistDecl(input, `${allVar}=${inputVar}.getAll(${keyText})`);
      B_hoistDecl(input, `${readVar}=${allVar}.length?${allVar}:void 0`);
    } else if (list) {
      B_hoistDecl(input, `${readVar}=${inputVar}.getAll(${keyText})`);
    } else if (takesEntry(field.present)) {
      // A file input with nothing chosen still submits: the HTML Standard's
      // entry list gets "a new File object with an empty name,
      // application/octet-stream as type, and an empty body". That sentinel is
      // not an upload, so it reads as absent — a required field then reports a
      // missing file rather than accepting an empty one. A string entry falls
      // through the guard untouched (`"".name` is undefined).
      const entryVar = B_varWithoutAllocation(input.g);
      B_hoistDecl(
        input,
        `${readVar}=(${entryVar}=${inputVar}.get(${keyText}))&&${entryVar}.name===""&&!${entryVar}.size?void 0:${entryVar}??void 0`,
      );
    } else {
      B_hoistDecl(
        input,
        `${readVar}=${inputVar}.get(${keyText})${
          field.optional || field.checkbox ? "||" : "??"
        }void 0`,
      );
    }

    // A field val the way valGet builds one: hung off the parent rather than
    // chained through `prev`, so each field's merge emits its own read and not
    // the parent's code again. Absent reads as `undefined`, the way a missing
    // object key does, so the error and the optional handling match an object's.
    // Canonical Val field order (see B_operationArg in builder.ts).
    const item: Val = {
      b: U,
      p: input,
      v: _var,
      i: readVar,
      s: list && !field.optional ? arrayFactory(unknown) : unknown,
      io: U,
      e: schema,
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
      path: pathConcat(input.path, [key]),
      g: input.g,
      o: U,
    };

    let output: Val;
    if (field.checkbox) {
      output = readCheckbox(item, field, schema);
    } else if (list) {
      // The item decides for itself whether it takes the entry — a
      // `S.array(S.file)` is a list of entries, not of text.
      let listTarget = field.present;
      if (!takesEntry(field.item!)) {
        listTarget = copySchema(listTarget);
        listTarget.additionalItems = fromText(field.item!);
      }
      output = field.optional
        ? readOptional(item, field, schema, arrayFactory(unknown), listTarget)
        : ((item.e = listTarget), parse(item));
    } else if (takesEntry(field.present)) {
      output = parse(item);
    } else if (field.optional) {
      output = readOptional(item, field, schema, unknown, fromText(field.present));
    } else {
      item.e = fromText(schema);
      output = parse(item);
    }
    B_addObjectField(objectVal, key, output);
  }

  if (target.additionalItems === "strict") {
    const keyVar = B_varWithoutAllocation(input.g);
    const fail = B_failWithArg(
      input,
      (excessFieldName: string) =>
        ({
          code: "unrecognized_keys",
          path: objectVal.path,
          reason: `Unrecognized key "${excessFieldName}"`,
          keys: [excessFieldName],
        }) as ErrorDetails,
      keyVar,
    );
    let cond = "";
    for (const key in properties) {
      cond += `${cond ? "&&" : ""}${keyVar}!==${inlinedValueFromString(key)}`;
    }
    objectVal.cp +=
      `for(const ${keyVar} of ${inputVar}.keys())` + (cond ? `if(${cond})` : "") + fail + ";";
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
