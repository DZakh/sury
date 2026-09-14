// `S.formData` - a form submission as a browser or `Request.formData()` hands
// it over, and the body a `fetch` call sends. An entry is a string or a
// `File`, so an object schema reads its fields through the string coercions
// the env pattern already compiles (`"42"` -> 42), a file field takes the entry
// as it is, and a repeated key is an array. Nothing reads file bytes, so both
// directions are sync.
//
// Not on the content axis (CONTENT_CODEC_SPEC.md): a form has no JSON document
// form and no format opens into one, so a link to it never has two readings,
// and a `FormData` in a JSON position has no document, the way `S.blob` has
// none. Bracket notation (`user[name]`) is deliberately out - a nested value
// travels as a `S.jsonString.with(S.to, …)` field.

import {
  anyOfTag,
  arrayTag,
  inlinedValueFromString,
  instanceTag,
  initSchema,
  inputExpression,
  type Internal,
  isOptional,
  nullTag,
  pathConcat,
  tagFlags,
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
  B_hoistDecl,
  B_markOutput,
  B_refine,
  B_merge,
  B_mergeWithPathPrepend,
  B_invalidOperation,
  B_next,
  B_nextConst,
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
  bool,
  string
} from "../primitives";
import {
  absentArm,
  admitsBlank,
  asList,
  asText,
  decidesBlank,
  isAbsent,
  presentArm,
  readWrapped
} from "./entries";

const isBlobClass = (class_: unknown): boolean => {
  const blobClass = (globalThis as { Blob?: unknown }).Blob as
    | (abstract new () => unknown)
    | undefined;
  return (
    blobClass !== U &&
    class_ !== U &&
    (class_ === blobClass || (class_ as { prototype?: unknown }).prototype instanceof blobClass)
  );
};

const isCheckbox = (schema: Internal): boolean =>
  schema.type === anyOfTag
    ? schema.anyOf!.some((variant) => tagFlags[variant.type]! & 8) &&
      schema.anyOf!.every(
        (variant) =>
          variant.type === undefinedTag || variant.type === nullTag || isCheckbox(variant),
      )
    : (tagFlags[schema.type]! & 8) !== 0;

// A `Map`, not an object: the keys are whatever the client sent, and
// `__proto__` is one of them. An unchosen file input still submits an empty
// unnamed File (HTML Standard); that sentinel is not an upload.
const readEntries = (formData: FormData): Map<string, unknown> => {
  const entries = new Map<string, unknown>();
  for (const [key, value] of formData as unknown as Iterable<[string, unknown]>) {
    if (typeof value !== "string" && (value as File).name === "" && !(value as File).size) {
      continue;
    }
    const prev = entries.get(key);
    prev === U
      ? entries.set(key, value)
      : Array.isArray(prev)
        ? prev.push(value)
        : entries.set(key, [prev, value]);
  }
  return entries;
};

// Named and instance-tagged so no text target shares its type: a same-typed
// arm would be taken as a pass-through and the hook never consulted. Two of
// them, differing in whether a blank entry is a question for the target.
const entrySchema = (blank: boolean): Internal =>
  initSchema(instanceTag, instanceDecoder, (s) => {
    s.name = "form field";
    // Set inside the initializer: a property write at module scope is a
    // statement esbuild keeps, and every bundle would carry the codec.
    s.encoder = (input: Val, target: Internal): Val => {
      if (blank && !decidesBlank(target)) {
        B_invalidOperation(
          input,
          `Ambiguous "" for ${inputExpression(target)}. Should a blank input be rejected, kept, or read as absent? Choose with S.nonEmpty, S.minLength(0), or S.optional`,
        );
      }
      const flag = tagFlags[target.type]!;
      if (flag & 8) {
        return readCheckbox(input, target);
      }
      if (flag & 256) {
        if (target.anyOf!.every((variant) => tagFlags[variant.type]! & 2)) {
          return asText(input, target);
        }
        const output = B_next(input, input.i, formDataValue, target);
        output.v = _var;
        return output;
      }
      if (flag & (64 | 128 | 512)) {
        return B_unsupportedDecode(input, s, target);
      }
      return (
        target.type === unknownTag ||
        (target.type === instanceTag && isBlobClass(target.class)) ||
        (flag & (2 | 16 | 32))
      )
        ? B_refine(input, unknown, U, target)
        : asText(input, target);
    };
  });
const formDataEntry: Internal = /* @__PURE__ */ entrySchema(true);
const formDataValue: Internal = /* @__PURE__ */ entrySchema(false);
const formDataList: Internal = /* @__PURE__ */ arrayFactory(formDataValue);

const readCheckbox = (input: Val, target: Internal): Val => {
  const v = input.i;
  if (target.const !== U) {
    const output = B_nextConst(input, target);
    output.cp = `${target.const ? `${v}==="on"||${v}==="true"` : `${v}==="false"||!${v}`}||${B_embedInvalidInput(input, target)};`;
    return output;
  }
  // Into a var of its own, not the entry's: a later check fails on the
  // boolean, and the next arm of a union then reads the entry as sent.
  const out = B_varWithoutAllocation(input.g);
  const output = B_next(input, out, bool, target);
  output.v = _var;
  output.cp = `let ${out}=${v}==="on"||${v}==="true"||(${v}==="false"||!${v}?false:${B_embedInvalidInput(input, target)});`;
  return output;
};

const assertListItems = (val: Val, schema: Internal): void => {
  const unsupported = (why: string): never =>
    B_invalidOperation(val, `Can't decode form field -> ${inputExpression(schema)}. ${why}`);
  const rest = schema.additionalItems;
  for (const item of schema.items!.concat(typeof rest === "object" ? [rest] : [])) {
    if (isCheckbox(item)) {
      unsupported(`A checkbox group sends the value of each checked box, so read it as string[]`);
    }
    if (isAbsent(item)) {
      unsupported(`A repeated key is positional, so every item needs an entry`);
    }
    if (item.type === arrayTag) {
      unsupported(`A repeated key is flat`);
    }
  }
};

const appendValue = (val: Val, fdVar: string, keyText: string, inList?: boolean): string => {
  const schema = val.s;
  const tagFlag = tagFlags[schema.type]!;
  if (tagFlag & (16 | 32)) {
    return "";
  }
  if (!inList && isCheckbox(schema)) {
    if (schema.const !== U) {
      return schema.const ? `${fdVar}.append(${keyText},"on");` : "";
    }
    return isAbsent(schema)
      ? `if(${val.i}!=null){${fdVar}.append(${keyText},${val.i}?"on":"false")}`
      : `if(${val.i}){${fdVar}.append(${keyText},"on")}`;
  }
  if (schema.type === arrayTag) {
    assertListItems(val, schema);
    const slots = schema.items!;
    if (slots.length) {
      let code = "";
      for (let idx = 0; idx < slots.length; idx++) {
        const slot = valGet(val, `${idx}`);
        code += B_mergeWithPathPrepend(slot, val, U, () =>
          appendValue(B_scope(slot), fdVar, keyText, true),
        );
      }
      return code;
    }
    const arrayVar = val.v();
    const iterVar = B_varWithoutAllocation(val.g);
    const raiseCountBefore = val.g.t;
    val.e = schema;
    const itemVal = B_dynamicScope(val, iterVar);
    // Built before the merge, not inside its callback: `B_mergeWithCatch` runs
    // the merge first, so a var this materializes on the item afterwards would
    // have its `let` dropped. On a scope of the item, not the item: the same
    // val in both would emit a union's dispatch `let` twice.
    const appendCode = appendValue(B_scope(itemVal), fdVar, keyText, true);
    const itemCode = B_mergeWithPathPrepend(
      itemVal,
      val,
      iterVar,
      () => appendCode,
      raiseCountBefore,
    );
    return `for(let ${iterVar}=0;${iterVar}<${arrayVar}.length;++${iterVar}){${itemCode}}`;
  }
  const present = presentArm(schema);
  if (isAbsent(schema) && present.type !== unknownTag) {
    if (present === schema) {
      return "";
    }
    const inputVar = val.v();
    const detached = B_next(val, inputVar, present, present);
    detached.v = _var;
    detached.prev = U;
    return `if(${inputVar}!=null){${appendValue(detached, fdVar, keyText, inList)}}`;
  }
  if (present.type === unknownTag) {
    const v = val.v();
    return `if(${v}!=null){typeof ${v}==="string"||${v} instanceof ${B_embed(val, globalThis.Blob)}||${B_embedInvalidInput(val, formDataEntry)};${fdVar}.append(${keyText},${v});}`;
  }
  if ((tagFlag & 2) || ((tagFlag & 8192) && isBlobClass(schema.class))) {
    return `${fdVar}.append(${keyText},${val.i});`;
  }
  if (!(tagFlag & ((4 | 8) | 1024 | (2048 | 8192) | 256))) {
    return B_unsupportedDecode(val, schema, formData);
  }
  // A union dispatch assigns its result into `target.v()`. After the slot
  // is merged that is the input lvalue (`i.a[0]`), so convert a copy.
  if (tagFlag & 256) {
    const tmp = B_varWithoutAllocation(val.g);
    const detached = B_next(val, tmp, val.s, string);
    detached.v = _var;
    detached.prev = U;
    detached.io = false;
    const converted = parse(detached);
    return `let ${tmp}=${val.i};` + B_merge(converted) + `${fdVar}.append(${keyText},${converted.i});`;
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
    const field = valGet(input, key);
    code += appendValue(field, fdVar, inlinedValueFromString(key));
  }
  const output = B_next(input, fdVar, input.e);
  output.v = _var;
  output.cp = code;
  return output;
};

const formDataToObject = (input: Val, target: Internal): Val => {
  // A browser appends entries no schema declared (`_charset_`, `dirname`,
  // image-button `.x`/`.y`), so "no entries but these" is not something a
  // form can promise.
  if (target.additionalItems === "strict") {
    B_invalidOperation(
      input,
      `Can't decode FormData -> ${inputExpression(target)} with S.strict. A browser adds entries no schema declares, so use S.strip`,
    );
  }
  const objectVal = makeObjectVal(input);

  const entriesVar = B_varWithoutAllocation(input.g);
  B_hoistDecl(input, `${entriesVar}=${B_embed(input, readEntries)}(${input.v()})`);
  const properties = target.properties!;
  let listRead = "";
  for (const key in properties) {
    const schema = properties[key]!;
    const keyText = inlinedValueFromString(key);
    const present = presentArm(schema);
    const absent = isAbsent(schema);
    const list = present.type === arrayTag;
    const folds = absent && !list && !admitsBlank(present);
    const readVar = B_varWithoutAllocation(input.g);
    const slot = `${entriesVar}.get(${keyText})`;
    B_hoistDecl(
      input,
      list
        ? `${readVar}=${(listRead ||= B_embed(input, asList))}(${slot})`
        : `${readVar}=${slot}${folds && isOptional(schema) && absentArm(schema).to === U ? "||void 0" : ""}`,
    );

    // Canonical Val field order (see B_operationArg in builder.ts). Hung off
    // the parent rather than chained through `prev`, so each field's merge
    // emits its own read.
    const item: Val = {
      b: U,
      p: input,
      v: _var,
      i: readVar,
      s: list ? formDataList : folds ? formDataValue : formDataEntry,
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

    if (list) {
      assertListItems(item, present);
      if (absent && absentArm(schema).to !== U) {
        B_invalidOperation(
          item,
          `Can't decode form field -> ${inputExpression(present)} with a default. No entries is the empty list, so the default is never read`,
        );
      }
    } else if (present.type === unknownTag) {
      item.cp = `Array.isArray(${readVar})&&${B_embedInvalidInput(item, formDataEntry)};`;
    }
    B_addObjectField(
      objectVal,
      key,
      absent ? readWrapped(item, schema, present, list ? U : folds) : parse(item),
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
        : (targetTagFlag & (1 | 8192))
          ? input
          : B_unsupportedDecode(input, input.s, target);
    };
  },
);
