// `S.urlSearchParams` - a query as `URLSearchParams`. `S.queryString` is the
// same entries as text. No files. Repeated keys are arrays. Blank strings
// follow the formData rule.

import {
  arrayTag,
  inlinedValueFromString,
  instanceTag,
  initSchema,
  inputExpression,
  type Internal,
  isOptional,
  pathConcat,
  stringTag,
  tagFlags,
  U,
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
  B_invalidOperation,
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
  string,
  stringDecoderFn
} from "../primitives";
import {
  absentArm,
  admitsBlank,
  asList,
  convertTextEntry,
  isAbsent,
  presentArm,
  readWrapped
} from "./entries";

const searchValue: Internal = /* @__PURE__ */ initSchema(instanceTag, instanceDecoder, (s) => {
  s.name = "search param";
  s.encoder = (input, target) => convertTextEntry(input, target, s);
});
const searchEntry: Internal = /* @__PURE__ */ initSchema(instanceTag, instanceDecoder, (s) => {
  s.name = "search param";
  s.encoder = (input, target) => convertTextEntry(input, target, searchValue, true);
});
const searchList: Internal = /* @__PURE__ */ arrayFactory(searchValue);

const readEntries = (params: URLSearchParams): Map<string, unknown> => {
  const entries = new Map<string, unknown>();
  for (const [key, value] of params as unknown as Iterable<[string, unknown]>) {
    const prev = entries.get(key);
    prev === U
      ? entries.set(key, value)
      : Array.isArray(prev)
        ? prev.push(value)
        : entries.set(key, [prev, value]);
  }
  return entries;
};

const assertListItems = (val: Val, schema: Internal): void => {
  const unsupported = (why: string): never =>
    B_invalidOperation(val, `Can't decode search param -> ${inputExpression(schema)}. ${why}`);
  const rest = schema.additionalItems;
  for (const item of schema.items!.concat(typeof rest === "object" ? [rest] : [])) {
    if (isAbsent(item)) {
      unsupported(`A repeated key is positional, so every item needs an entry`);
    }
    if (item.type === arrayTag) {
      unsupported(`A repeated key is flat`);
    }
  }
};

const appendValue = (val: Val, destVar: string, keyText: string): string => {
  const schema = val.s;
  const tagFlag = tagFlags[schema.type]!;
  if (tagFlag & (16 | 32)) {
    return "";
  }
  if (schema.type === arrayTag) {
    assertListItems(val, schema);
    const slots = schema.items!;
    if (slots.length) {
      let code = "";
      for (let idx = 0; idx < slots.length; idx++) {
        const slot = valGet(val, `${idx}`);
        code += B_mergeWithPathPrepend(slot, val, U, () =>
          appendValue(B_scope(slot), destVar, keyText),
        );
      }
      return code;
    }
    const arrayVar = val.v();
    const iterVar = B_varWithoutAllocation(val.g);
    const raiseCountBefore = val.g.t;
    val.e = schema;
    const itemVal = B_dynamicScope(val, iterVar);
    const appendCode = appendValue(B_scope(itemVal), destVar, keyText);
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
    return `if(${inputVar}!=null){${appendValue(detached, destVar, keyText)}}`;
  }
  if (present.type === unknownTag) {
    const v = val.v();
    return `if(${v}!=null){typeof ${v}==="string"||${B_embedInvalidInput(val, searchEntry)};${destVar}.append(${keyText},${v});}`;
  }
  if (tagFlag & 2) {
    return `${destVar}.append(${keyText},${val.i});`;
  }
  if ((tagFlag & 8192) && schema.class !== Date) {
    return B_unsupportedDecode(val, schema, urlSearchParams);
  }
  if (!(tagFlag & ((4 | 8) | 1024 | (2048 | 8192) | 256))) {
    return B_unsupportedDecode(val, schema, urlSearchParams);
  }
  if (tagFlag & 256) {
    const tmp = B_varWithoutAllocation(val.g);
    const detached = B_next(val, tmp, val.s, string);
    detached.v = _var;
    detached.prev = U;
    detached.io = false;
    const converted = parse(detached);
    return `let ${tmp}=${val.i};` + B_merge(converted) + `${destVar}.append(${keyText},${converted.i});`;
  }
  val.io = false;
  val.e = string;
  const converted = parse(val);
  return B_merge(converted) + `${destVar}.append(${keyText},${converted.i});`;
};

const objectToSearchParams = (input: Val): Val => {
  const destVar = B_varWithoutAllocation(input.g);
  const properties = input.s.properties!;
  let code = `let ${destVar}=new ${B_embed(input, urlSearchParams.class)}();`;
  for (const key in properties) {
    const field = valGet(input, key);
    code += appendValue(field, destVar, inlinedValueFromString(key));
  }
  const output = B_next(input, destVar, input.e);
  output.v = _var;
  output.cp = code;
  return output;
};

const searchParamsToObject = (input: Val, target: Internal): Val => {
  if (target.additionalItems === "strict") {
    B_invalidOperation(
      input,
      `Can't decode URLSearchParams -> ${inputExpression(target)} with S.strict. A query carries keys no schema declares, so use S.strip`,
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
    const item: Val = {
      b: U,
      p: input,
      v: _var,
      i: readVar,
      s: list ? searchList : folds ? searchValue : searchEntry,
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
          `Can't decode search param -> ${inputExpression(present)} with a default. No entries is the empty list, so the default is never read`,
        );
      }
    } else if (present.type === unknownTag) {
      item.cp = `Array.isArray(${readVar})&&${B_embedInvalidInput(item, searchEntry)};`;
    }
    B_addObjectField(
      objectVal,
      key,
      absent ? readWrapped(item, schema, present, list ? U : folds) : parse(item),
    );
  }
  return B_markOutput(completeObjectVal(objectVal), input);
};

const isObjectTarget = (schema: Internal): boolean =>
  (tagFlags[schema.type]! & 64) !== 0 && typeof schema.additionalItems === "string";

export const urlSearchParams: Internal = /* @__PURE__ */ initSchema(
  instanceTag,
  (input: Val): Val =>
    isObjectTarget(input.s) ? objectToSearchParams(input) : instanceDecoder(input),
  (s) => {
    s.class = (globalThis as unknown as Record<string, unknown>)["URLSearchParams"];
    if (s.class === U) {
      unsupportedInstance(s, "urlSearchParams");
    }
    s.name = "URLSearchParams";
    s.encoder = (input, target) =>
      isObjectTarget(target)
        ? searchParamsToObject(input, target)
        : (tagFlags[target.type]! & (1 | 8192)) || target.name === "query string"
          ? input
          : B_unsupportedDecode(input, input.s, target);
  },
);

const queryStringDecoder = (input: Val): Val => {
  if (isObjectTarget(input.s)) {
    const params = objectToSearchParams(input);
    return B_next(params, `${params.i}.toString()`, queryString);
  }
  if ((tagFlags[input.s.type]! & 8192) && input.s.class === urlSearchParams.class) {
    return B_next(input, `${input.i}.toString()`, queryString);
  }
  return stringDecoderFn(input);
};

export const queryString: Internal = /* @__PURE__ */ initSchema(
  stringTag,
  queryStringDecoder,
  (s) => {
    s.name = "query string";
    s.format = "queryString";
    s.encoder = (input, target) => {
      if (
        isObjectTarget(target) ||
        ((tagFlags[target.type]! & 8192) && target.class === urlSearchParams.class)
      ) {
        const parsed = B_next(
          input,
          `new ${B_embed(input, urlSearchParams.class)}(${input.v()})`,
          urlSearchParams,
          target,
        );
        parsed.v = _var;
        return parsed;
      }
      if (tagFlags[target.type]! & 2) {
        return input;
      }
      return B_unsupportedDecode(input, input.s, target);
    };
  },
);
