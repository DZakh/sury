// Generated from core.ts by scripts/build-core.mjs, PLEASE EDIT WITH CARE
const pathEmpty = "";
const pathDynamic = "[]";
function inlinedValueFromString(str) {
  for (let idx = 0; idx < str.length; idx++) {
    const ch = str[idx];
    if (ch === '"' || ch === "\n") return JSON.stringify(str);
  }
  return `"${str}"`;
}
function pathFromInlinedLocation(inlinedLocation) {
  return `[${inlinedLocation}]`;
}
function pathFromLocation(location) {
  return `[${inlinedValueFromString(location)}]`;
}
function pathToArray(path) {
  switch (path) {
    case "":
      return [];
    default:
      return JSON.parse(path.split(`"]["`).join(`","`));
  }
}
function pathFromArray(array) {
  switch (array.length) {
    case 0:
      return "";
    case 1:
      return pathFromLocation(array[0]);
    default:
      return array.map(pathFromLocation).join("");
  }
}
function pathConcat(path, concatedPath) {
  return path + concatedPath;
}
const vendor = "sury";
const s = Symbol(vendor);
const itemSymbol = /* @__PURE__ */ Symbol(vendor + ":item");
const shouldPrependPathKey = "p";
const stringTag = "string";
const numberTag = "number";
const bigintTag = "bigint";
const booleanTag = "boolean";
const symbolTag = "symbol";
const nullTag = "null";
const undefinedTag = "undefined";
const nanTag = "nan";
const functionTag = "function";
const instanceTag = "instance";
const arrayTag = "array";
const objectTag = "object";
const unionTag = "union";
const neverTag = "never";
const unknownTag = "unknown";
const refTag = "ref";
function typeOf(value) {
  return typeof value;
}
const Flag = {
  none: 0,
  async: 1,
  with: (a, b) => a | b,
  unsafeHas: (flag, test) => (flag & test) === test
};
const ValFlag = {
  none: 0,
  async: 1
};
function Schema() {
}
const schemaPrototype = /* @__PURE__ */ Object.create(null);
Object.defineProperty(schemaPrototype, "with", {
  get() {
    return (fn, ...args) => fn(this, ...args);
  }
});
Schema.prototype = schemaPrototype;
let seq = 1;
let exnId = {};
function __setExnId(id) {
  exnId = id;
}
class SuryError extends Error {
  constructor(params) {
    super();
    for (const key in params) {
      this[key] = params[key];
    }
  }
  get message() {
    return formatErrorMessage(this);
  }
  // The exn's "payload" _is_ the exception object itself.
  get _1() {
    return this;
  }
  get RE_EXN_ID() {
    return exnId;
  }
}
Object.defineProperty(SuryError.prototype, "name", { value: "SuryError" });
Object.defineProperty(SuryError.prototype, "s", { value: s });
function getOrRethrow(exn) {
  if (exn && exn.s === s) {
    return exn;
  } else {
    throw exn;
  }
}
function panic(message) {
  throw new Error(`[Sury] ${message}`);
}
function formatErrorMessage(error) {
  return `${error.path === "" ? "" : `Failed at ${error.path}: `}${error.reason}`;
}
const InternalError = {
  make: (errorDetails) => new SuryError(errorDetails),
  getOrRethrow,
  panic,
  message: formatErrorMessage
};
const initialOnAdditionalItems = "strip";
const initialDefaultFlag = ValFlag.none;
const globalConfig = {
  m: InternalError.message,
  d: void 0,
  a: initialOnAdditionalItems,
  f: initialDefaultFlag
};
const valueOptions = {};
const configurableValueOptions = { configurable: true };
const valKey = "value";
const reversedKey = "r";
function baseSchema(tag, selfReverse) {
  const schema = new Schema();
  schema.type = tag;
  schema.seq = seq++;
  if (selfReverse) {
    valueOptions[valKey] = schema;
    Object.defineProperty(schema, reversedKey, { ...configurableValueOptions, value: schema });
  }
  return schema;
}
function noopDecoder(input) {
  return input;
}
const factoryCache = {};
function cached(key, tag, init) {
  const existing = factoryCache[key];
  if (existing !== void 0) {
    return existing;
  } else {
    const schema = baseSchema(tag, true);
    init(schema);
    factoryCache[key] = schema;
    return schema;
  }
}
const unknown = baseSchema(unknownTag, true);
unknown.decoder = noopDecoder;
function copySchema(schema) {
  const c = new Schema();
  for (const k in schema) {
    c[k] = schema[k];
  }
  c.seq = seq++;
  return c;
}
function updateOutput(schema, fn) {
  const root = copySchema(schema);
  let mut = root;
  while (mut.to !== void 0) {
    const next = copySchema(mut.to);
    mut.to = next;
    mut = next;
  }
  fn(mut);
  return root;
}
export {
  Flag,
  Schema,
  ValFlag,
  __setExnId,
  arrayTag,
  bigintTag,
  booleanTag,
  cached,
  copySchema,
  functionTag,
  globalConfig,
  instanceTag,
  nanTag,
  neverTag,
  nullTag,
  numberTag,
  objectTag,
  pathConcat,
  pathDynamic,
  pathEmpty,
  pathFromArray,
  pathToArray,
  refTag,
  stringTag,
  symbolTag,
  typeOf,
  undefinedTag,
  unionTag,
  unknown,
  unknownTag,
  updateOutput
};
