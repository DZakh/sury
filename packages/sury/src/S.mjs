/* @ts-self-types="./S.d.ts" */
// Generated from entry.ts by scripts/pack.ts, PLEASE EDIT WITH CARE

// src/base.ts
var flagNone = 0;
var flagAsync = 1;
var flagDisableNanNumberValidation = 2;
var flagUnionTransformContext = 4;
var flagUnsafeHas = (acc, flag) => {
  return (acc & flag) !== 0;
};
var valFlagNone = 0;
var valFlagAsync = 1;
var pathEmpty = "";
var pathDynamic = "[]";
var inlinedValueFromString = (str) => {
  return str.includes('"') || str.includes("\n") ? JSON.stringify(str) : `"${str}"`;
};
var pathFromInlinedLocation = (inlinedLocation) => {
  return `[${inlinedLocation}]`;
};
var pathFromLocation = /* @__NO_SIDE_EFFECTS__ */ (location) => {
  return `[${inlinedValueFromString(location)}]`;
};
var pathToArray = /* @__NO_SIDE_EFFECTS__ */ (path) => {
  return path === "" ? [] : JSON.parse(path.split(`"]["`).join(`","`));
};
var pathFromArray = /* @__NO_SIDE_EFFECTS__ */ (array2) => {
  switch (array2.length) {
    case 0:
      return "";
    case 1:
      return /* @__PURE__ */ pathFromLocation(array2[0]);
    default:
      return array2.map(pathFromLocation).join("");
  }
};
var pathConcat = /* @__NO_SIDE_EFFECTS__ */ (path, concatedPath) => {
  return path + concatedPath;
};
var stringTag = "string";
var numberTag = "number";
var bigintTag = "bigint";
var booleanTag = "boolean";
var symbolTag = "symbol";
var nullTag = "null";
var undefinedTag = "undefined";
var nanTag = "nan";
var functionTag = "function";
var instanceTag = "instance";
var arrayTag = "array";
var objectTag = "object";
var anyOfTag = "anyOf";
var neverTag = "never";
var unknownTag = "unknown";
var refTag = "ref";
var tagFlagUnknown = 1;
var tagFlagString = 2;
var tagFlagNumber = 4;
var tagFlagBoolean = 8;
var tagFlagUndefined = 16;
var tagFlagNull = 32;
var tagFlagObject = 64;
var tagFlagArray = 128;
var tagFlagUnion = 256;
var tagFlagRef = 512;
var tagFlagBigint = 1024;
var tagFlagNaN = 2048;
var tagFlagFunction = 4096;
var tagFlagInstance = 8192;
var tagFlagSymbol = 16384;
var tagFlagNever = 32768;
var tagFlags = {
  [unknownTag]: 1,
  [stringTag]: 2,
  [numberTag]: 4,
  [booleanTag]: 8,
  [undefinedTag]: 16,
  [nullTag]: 32,
  [objectTag]: 64,
  [arrayTag]: 128,
  [anyOfTag]: 256,
  [refTag]: 512,
  [bigintTag]: 1024,
  [nanTag]: 2048,
  [functionTag]: 4096,
  [instanceTag]: 8192,
  [neverTag]: 32768,
  [symbolTag]: 16384
};
var vendor = "sury";
var s = /* @__PURE__ */ Symbol(vendor);
var itemSymbol = /* @__PURE__ */ Symbol(vendor + ":item");
var U = void 0;
var immutableEmptyArray = [];
var immutableEmptyObject = /* @__PURE__ */ Object.create(null);
var isSchemaObject = (obj) => {
  return typeof obj === objectTag && obj !== null && "~standard" in obj;
};
var constField = "const";
var isLiteral = (schema) => {
  return constField in schema;
};
var isOptional = (schema) => {
  return schema.type === undefinedTag || schema.type === anyOfTag && undefinedTag in schema.has;
};
var namedConstructor = (unknown2) => {
  const ctor = Object.getPrototypeOf(unknown2)?.constructor;
  return ctor !== Object && ctor?.name;
};
var stringifyLeaf = (unknown2) => {
  const tagFlag = tagFlags[typeof unknown2];
  if (flagUnsafeHas(tagFlag, tagFlagUndefined)) {
    return undefinedTag;
  } else if (flagUnsafeHas(tagFlag, tagFlagObject | tagFlagFunction)) {
    return unknown2 === null ? nullTag : Array.isArray(unknown2) ? `Array(${unknown2.length})` : namedConstructor(unknown2) || objectTag;
  } else if (flagUnsafeHas(tagFlag, tagFlagString)) {
    return `"${unknown2}"`;
  } else if (flagUnsafeHas(tagFlag, tagFlagBigint)) {
    return `${unknown2}n`;
  } else {
    return unknown2.toString();
  }
};
var stringify = (unknown2) => {
  if (unknown2 !== null && typeof unknown2 === objectTag) {
    if (Array.isArray(unknown2)) {
      const items = unknown2;
      let body = "";
      for (let idx = 0; idx < items.length; idx++) {
        if (idx === 5) {
          body = body + ", ...";
          break;
        }
        body = body + (idx ? ", " : "") + stringifyLeaf(items[idx]);
      }
      return `[${body}]`;
    }
    if (!namedConstructor(unknown2)) {
      const dict = unknown2;
      let body = "";
      let count = 0;
      for (const key in dict) {
        if (count++ === 5) {
          body = body + "... ";
          break;
        }
        body = body + key + ": " + stringifyLeaf(dict[key]) + "; ";
      }
      return body ? `{ ${body}}` : "{}";
    }
  }
  return stringifyLeaf(unknown2);
};
var inputExpression = /* @__NO_SIDE_EFFECTS__ */ (schema, skipOverride) => {
  if (schema.name) {
    return schema.name;
  } else if (schema.const !== U) {
    return stringify(schema.const);
  } else if (schema.expression && !skipOverride) {
    return schema.expression(schema);
  } else if (schema.anyOf !== U) {
    const anyOf = schema.anyOf;
    const seen = /* @__PURE__ */ new Set();
    let body = "";
    for (let idx = 0; idx < anyOf.length; idx++) {
      const expression = /* @__PURE__ */ inputExpression(anyOf[idx]);
      if (!seen.has(expression)) {
        seen.add(expression);
        body = body + (body ? " | " : "") + expression;
      }
    }
    return body;
  } else if (schema.type === objectTag) {
    const properties = schema.properties;
    const additionalItems = schema.additionalItems;
    let body = "";
    for (const location in properties) {
      body = body + location + ": " + /* @__PURE__ */ inputExpression(properties[location]) + "; ";
    }
    if (typeof additionalItems === objectTag) {
      body = body + "[key: string]: " + /* @__PURE__ */ inputExpression(additionalItems) + "; ";
    }
    return body ? `{ ${body}}` : "{}";
  } else if (schema.type === arrayTag) {
    const additionalItems = schema.additionalItems;
    if (typeof additionalItems === objectTag) {
      const item = additionalItems;
      const itemName = /* @__PURE__ */ inputExpression(item);
      return (item.type === anyOfTag || item.bounds !== U ? `(${itemName})` : itemName) + "[]";
    }
    const items = schema.items;
    let body = "";
    for (let idx = 0; idx < items.length; idx++) {
      body = body + (idx ? ", " : "") + /* @__PURE__ */ inputExpression(items[idx]);
    }
    return `[${body}]`;
  } else if (schema.format) {
    return schema.format;
  } else if (schema.type === instanceTag) {
    return schema.class.name;
  } else {
    return schema.type;
  }
};
function Schema() {
}
var schemaPrototype = /* @__PURE__ */ Object.create(null);
Object.defineProperty(schemaPrototype, "with", {
  value(fn, ...args) {
    return fn(this, ...args);
  }
});
Schema.prototype = schemaPrototype;
var seq = 1;
var exnId = {};
var __setExnId = (id) => {
  exnId = id;
};
var SuryError = class extends Error {
  constructor(params) {
    super();
    Object.assign(this, params);
  }
  get message() {
    return formatErrorMessage(this);
  }
  get _1() {
    return this;
  }
  get RE_EXN_ID() {
    return exnId;
  }
};
Object.defineProperty(SuryError.prototype, "name", { value: "SuryError" });
Object.defineProperty(SuryError.prototype, "s", { value: s });
var getOrRethrow = (exn) => {
  if (exn && exn.s === s) {
    return exn;
  } else {
    throw exn;
  }
};
var panic = (message) => {
  throw new Error(`[Sury] ${message}`);
};
var formatErrorMessage = (error) => {
  return `${error.path === "" ? "" : `Failed at ${error.path}: `}${error.reason}`;
};
var errorClass = SuryError;
var initialOnAdditionalItems = "strip";
var initialDefaultFlag = valFlagNone;
var globalConfig = {
  m: formatErrorMessage,
  d: U,
  a: initialOnAdditionalItems,
  f: initialDefaultFlag
};
var valueOptions = {};
var configurableValueOptions = { configurable: true };
var valKey = "value";
var reversedKey = "r";
var SchemaCtor = Schema;
var baseSchema = (tag, selfReverse) => {
  const schema = new SchemaCtor();
  schema.type = tag;
  schema.seq = seq++;
  if (selfReverse) {
    valueOptions[valKey] = schema;
    Object.defineProperty(schema, reversedKey, valueOptions);
  }
  return schema;
};
var noopDecoder = (input) => {
  return input;
};
var initSchema = /* @__NO_SIDE_EFFECTS__ */ (tag, init) => {
  const schema = baseSchema(tag, true);
  init(schema);
  return schema;
};
var unknown = baseSchema(unknownTag, true);
unknown.decoder = noopDecoder;
var copySchema = (schema) => {
  const c = Object.assign(new SchemaCtor(), schema);
  c.seq = seq++;
  return c;
};
var updateOutput = (schema, fn) => {
  const root = copySchema(schema);
  let mut = root;
  while (mut.to !== U) {
    const next = copySchema(mut.to);
    mut.to = next;
    mut = next;
  }
  fn(mut);
  return root;
};
var setHas = (has, tag) => {
  has[flagUnsafeHas(tagFlags[tag], tagFlagUnion | tagFlagRef) ? unknownTag : tag] = true;
};
var defsPath = `#/$defs/`;
var jsonName = `JSON`;

// src/builder.ts
function _var() {
  return this.i;
}
function _bondVar() {
  const val = this;
  const bond = val.b;
  return bond.v();
}
function _prevVar() {
  const val = this;
  const prev = val.prev;
  return prev.v();
}
function _notVarBeforeValidation() {
  const val = this;
  const v = B_varWithoutAllocation(val.g);
  val.cp = `let ${v}=${val.i};`;
  val.i = v;
  val.v = _var;
  return v;
}
function _notVarAtParent() {
  const val = this;
  const parent = val.p;
  if (parent.fz) {
    val.v = _var;
    return val.i;
  } else {
    const v = B_varWithoutAllocation(val.g);
    B_hoistDecl(parent, `${v}=${val.i}`);
    val.v = _var;
    val.i = v;
    return v;
  }
}
function _notVar() {
  const val = this;
  if (val.fz) {
    val.v = _var;
    val.i = `(${val.i})`;
    return val.i;
  } else {
    const v = B_varWithoutAllocation(val.g);
    if (val.prev !== U) {
      if (val.i === "") {
        val.cp = `let ${v};` + val.cp;
      } else {
        val.cp = val.cp + `let ${v}=${val.i};`;
      }
    } else {
      if (val.i === "") {
        B_hoistDecl(val, v);
      } else {
        B_hoistDecl(val, `${v}=${val.i}`);
      }
    }
    val.v = _var;
    val.i = v;
    return v;
  }
}
var operationArgVar = "i";
var failInvalidType = (input) => {
  const expected = input.e;
  const em = expected.errorMessage;
  const override = em !== U ? expected.format !== U && em.format !== U ? em.format : em.type !== U ? em.type : em._ : U;
  return B_invalidInputBuilder(U, U, override)(input);
};
var B_embed = (b, value) => {
  const e = b.g.e;
  const l = e.length;
  e[l] = value;
  b.g.t++;
  return `e[${l}]`;
};
var B_inlineConst = (b, schema) => {
  const tagFlag = tagFlags[schema.type];
  const const_ = schema.const;
  if (flagUnsafeHas(tagFlag, tagFlagUndefined)) {
    return "void 0";
  } else if (flagUnsafeHas(tagFlag, tagFlagString)) {
    return inlinedValueFromString(const_);
  } else if (flagUnsafeHas(tagFlag, tagFlagBigint)) {
    return const_ + "n";
  } else if (flagUnsafeHas(
    tagFlag,
    tagFlagSymbol | tagFlagFunction | tagFlagInstance
  )) {
    return B_embed(b, schema.const);
  } else {
    return const_;
  }
};
var B_varWithoutAllocation = (global2) => {
  const newCounter = global2.v + 1;
  global2.v = newCounter;
  return `v${newCounter}`;
};
var B_hoistDecl = (owner, decl) => {
  owner.hd = owner.hd === "" ? decl : owner.hd + "," + decl;
};
var B_operationArg = (schema, expected, flag, defs) => {
  return {
    b: U,
    p: U,
    v: _var,
    i: operationArgVar,
    s: schema,
    io: U,
    e: expected,
    prev: U,
    f: valFlagNone,
    d: U,
    fv: U,
    cp: "",
    hd: "",
    fz: U,
    vc: U,
    u: U,
    t: U,
    path: pathEmpty,
    g: {
      d: defs,
      o: flag,
      e: [],
      v: -1,
      t: 0
    },
    o: U
  };
};
var B_throw = (errorDetails) => {
  throw new SuryError(errorDetails);
};
var B_unsupportedDecode = (b, from, target) => {
  return B_throw({
    code: "unsupported_decode",
    from,
    to: target,
    reason: `Can't decode ${inputExpression(from)} to ${inputExpression(
      target
    )}. Use S.to to define a custom decoder`,
    path: b.path
  });
};
var B_failWithArg = (b, fn, arg) => {
  return `${B_embed(b, (arg2) => {
    B_throw(fn(arg2));
  })}(${arg})`;
};
var B_markThrow = (b) => {
  b.g.t++;
};
var B_makeInvalidConversionDetails = (input, to, cause) => {
  if (cause && cause.s === s) {
    const error = cause;
    return input.path === pathEmpty ? error : { ...error, path: pathConcat(input.path, error.path) };
  } else {
    let reason;
    if (cause instanceof Error) {
      const text = "" + cause;
      if (text.startsWith("Error: ")) {
        reason = text.slice(7);
      } else {
        reason = text;
      }
    } else {
      reason = stringify(cause);
    }
    return {
      code: "invalid_conversion",
      from: input.s,
      to,
      cause,
      path: input.path,
      reason
    };
  }
};
var B_receivedSchema = (val) => {
  return val.prev !== U ? val.prev.s : val.s;
};
var B_makeInvalidInputDetails = (expected, received, path, input, unionErrors, reasonOverride) => {
  let reasonRef;
  if (reasonOverride !== U) {
    reasonRef = reasonOverride;
  } else {
    const expectedExpression = inputExpression(expected);
    const receivedExpression = stringify(input);
    reasonRef = `Expected ${expectedExpression}, received ${expectedExpression === receivedExpression ? "invalid " : ""}${receivedExpression}`;
  }
  if (unionErrors !== U) {
    const caseErrors = unionErrors;
    const seenReasons = /* @__PURE__ */ new Set();
    for (let idx = 0; idx < caseErrors.length; idx++) {
      const caseError = caseErrors[idx];
      const caseReason = caseError.reason.split("\n").join("\n  ");
      const location = caseError.path === "" ? "" : `At ${caseError.path}: `;
      const line = `
- ${location}${caseReason}`;
      if (!seenReasons.has(line)) {
        seenReasons.add(line);
        reasonRef = reasonRef + line;
      }
    }
  }
  const details = {
    code: "invalid_input",
    expected,
    received,
    path,
    reason: reasonRef,
    unionErrors,
    input
  };
  return details;
};
var B_invalidInputBuilder = (expected, extraPath = pathEmpty, reasonOverride) => {
  return (input) => {
    const expected_ = expected !== U ? expected : input.e;
    const received = B_receivedSchema(input);
    const path = extraPath === pathEmpty ? input.path : pathConcat(input.path, extraPath);
    return (value) => B_makeInvalidInputDetails(expected_, received, path, value, U, reasonOverride);
  };
};
var B_failWithErrorMessage = (key, defaultMessage) => {
  return (input) => {
    const em = input.e.errorMessage;
    const override = em !== U ? em[key] !== U ? em[key] : em["_"] : U;
    const m = override !== U ? override : defaultMessage;
    if (m !== U) {
      return B_invalidInputBuilder(U, U, m)(input);
    } else {
      return failInvalidType(input);
    }
  };
};
var B_embedInvalidInput = (input, expected = input.e) => {
  return B_failWithArg(input, B_invalidInputBuilder(expected)(input), input.v());
};
var B_emitChecks = (val, inputVar) => {
  const checks = val.vc;
  const len = checks.length;
  if (len === 1) {
    const check = checks[0];
    return `${check.c(inputVar)}||${B_failWithArg(val, check.f(val), inputVar)};`;
  } else {
    let out = "";
    let i = 0;
    while (i < len) {
      const head = checks[i];
      const fail = head.f;
      let cond = head.c(inputVar);
      i = i + 1;
      while (i < len && checks[i].f === fail) {
        cond = cond + "&&" + checks[i].c(inputVar);
        i = i + 1;
      }
      out = out + `${cond}||${B_failWithArg(val, fail(val), inputVar)};`;
    }
    return out;
  }
};
var B_isHoistable = (val) => {
  return val.t === true ? val.prev.t !== true && val.cp === "" : true;
};
var B_merge = (val, out) => {
  let current = val;
  let code = "";
  while (current !== U) {
    const val2 = current;
    current = val2.prev;
    let currentCode = "";
    if (val2.vc) {
      if (out !== U && B_isHoistable(val2)) {
        const inputVar = current.v();
        const checks = val2.vc;
        let hoisted = "";
        for (let i = 0; i < checks.length; i++) {
          const check = checks[i];
          const condCode = check.c(inputVar);
          if (check.f === failInvalidType) {
            hoisted = hoisted ? `${hoisted}&&${condCode}` : condCode;
          } else if (val2.e.noValidation !== true) {
            currentCode = currentCode + `${condCode}||${B_failWithArg(val2, check.f(val2), inputVar)};`;
          }
        }
        if (hoisted) {
          out.c = out.c ? `${hoisted}&&${out.c}` : hoisted;
          out.h.unshift({ v: val2, i: inputVar, c: hoisted });
        }
      } else if (val2.e.noValidation !== true) {
        currentCode = B_emitChecks(val2, current.v());
      }
    }
    if (val2.hd !== "") {
      currentCode = currentCode + `let ${val2.hd};`;
    }
    val2.fz = true;
    currentCode = val2.cp + currentCode;
    code = currentCode + code;
  }
  return code;
};
var B_linkVar = (val, nextVal) => {
  const valVar = val.v.bind(val);
  val.v = () => {
    const v = valVar();
    nextVal.i = v;
    nextVal.v = _var;
    return v;
  };
};
var B_next = (prev, initial, schema, expected = prev.e) => {
  return {
    b: U,
    p: U,
    v: _notVar,
    i: initial,
    s: schema,
    io: U,
    e: expected,
    prev,
    f: valFlagNone,
    d: prev.d,
    fv: U,
    cp: "",
    hd: "",
    fz: U,
    vc: U,
    u: U,
    t: true,
    path: prev.path,
    g: prev.g,
    o: U
  };
};
var B_refine = (val, schema = val.s, checks, expected = val.e) => {
  const shouldLink = val.v !== _var;
  const nextVal = {
    b: U,
    p: U,
    v: shouldLink ? _prevVar : _var,
    i: val.i,
    s: schema,
    io: U,
    e: expected,
    prev: val,
    f: val.f,
    d: val.d,
    fv: U,
    cp: "",
    hd: "",
    fz: U,
    vc: checks,
    u: U,
    t: val.t,
    path: val.path,
    g: val.g,
    o: U
  };
  if (shouldLink) {
    B_linkVar(val, nextVal);
  }
  return nextVal;
};
var B_pushCheck = (val, check) => {
  if (val.vc !== U) {
    val.vc.push(check);
  } else {
    val.vc = [check];
  }
};
var B_markOutput = (val, valInput) => {
  let deferredInputChecks;
  const inputRefiner = valInput.e.inputRefiner;
  if (inputRefiner !== U) {
    const checks = inputRefiner(valInput);
    if (checks.length > 0) {
      if (valInput.prev !== U) {
        for (let i = 0; i < checks.length; i++) {
          B_pushCheck(valInput, checks[i]);
        }
        deferredInputChecks = U;
      } else {
        deferredInputChecks = checks;
      }
    } else {
      deferredInputChecks = U;
    }
  } else {
    deferredInputChecks = U;
  }
  let outputChecks;
  const refiner = val.e.refiner;
  if (refiner !== U) {
    const checks = refiner(val);
    outputChecks = checks.length > 0 ? checks : U;
  } else {
    outputChecks = U;
  }
  let result;
  if (deferredInputChecks !== U && outputChecks !== U) {
    result = B_refine(val, U, deferredInputChecks.concat(outputChecks));
  } else if (deferredInputChecks !== U) {
    result = B_refine(val, U, deferredInputChecks);
  } else if (outputChecks !== U) {
    result = B_refine(val, U, outputChecks);
  } else {
    result = val;
  }
  result.io = true;
  return result;
};
var B_hoistChildChecks = (parent, child, key) => {
  if (child.vc) {
    const pathAppend = pathFromInlinedLocation(inlinedValueFromString(key));
    child.vc.forEach((check) => {
      B_pushCheck(parent, {
        c: (inputVar) => check.c(inputVar + pathAppend),
        f: check.f
      });
    });
    child.vc = U;
  }
};
var B_dynamicScope = (from, locationVar) => {
  const schemaAdditionalItems = from.s.additionalItems;
  const expectedAdditionalItems = from.e.additionalItems;
  return {
    b: U,
    p: from,
    v: _notVarBeforeValidation,
    i: `${from.v()}[${locationVar}]`,
    s: schemaAdditionalItems !== U && typeof schemaAdditionalItems !== "string" ? schemaAdditionalItems : unknown,
    io: U,
    e: expectedAdditionalItems !== U && typeof expectedAdditionalItems !== "string" ? expectedAdditionalItems : unknown,
    prev: U,
    f: from.f,
    d: U,
    fv: U,
    cp: "",
    hd: "",
    fz: U,
    vc: U,
    u: U,
    t: U,
    path: pathEmpty,
    g: from.g,
    o: U
  };
};
var B_nextConst = (from, schema, expected) => {
  return B_next(from, B_inlineConst(from, schema), schema, expected);
};
var B_asyncVal = (from, initial) => {
  const v = B_next(from, initial, from.s);
  v.f = valFlagAsync;
  return v;
};
var B_addObjectField = (objectVal, location, val) => {
  if (objectVal.s.type === arrayTag) {
    objectVal.s.items.push(val.s);
  } else {
    if (!val.o) {
      objectVal.s.required.push(location);
    }
    objectVal.s.properties[location] = val.s;
  }
  if (flagUnsafeHas(val.f, valFlagAsync)) {
    val.v();
  }
  objectVal.cp = objectVal.cp + B_merge(val);
  objectVal.d[location] = val;
};
var B_mergeObjectFields = (target, vals) => {
  for (const location of Object.keys(vals)) {
    B_addObjectField(target, location, vals[location]);
  }
};
var B_addKey = (objVal, key, value) => {
  return `${objVal.v()}[${key}]=${value.i}`;
};
var B_scope = (val) => {
  const shouldLink = val.v !== _var;
  const nextVal = {
    b: val,
    p: U,
    v: shouldLink ? _bondVar : _var,
    i: val.i,
    s: val.s,
    io: val.io,
    e: val.e,
    prev: U,
    f: flagNone,
    d: val.d,
    // See the aliasing note on B_next's `d: prev.d` above.
    fv: U,
    cp: "",
    hd: "",
    fz: U,
    vc: U,
    u: false,
    t: false,
    path: val.path,
    g: val.g,
    o: U
  };
  if (shouldLink) {
    B_linkVar(val, nextVal);
  }
  return nextVal;
};
var B_conversion = (fn, isAsync, junction) => {
  return (input) => {
    const target = input.e.to;
    const outputVar = B_varWithoutAllocation(input.g);
    const output = B_next(
      input,
      outputVar,
      junction || isLiteral(target) ? unknown : target,
      target
    );
    output.v = _var;
    if (isAsync) {
      if (!flagUnsafeHas(input.g.o, flagAsync)) {
        B_throw({
          code: "invalid_operation",
          path: pathEmpty,
          reason: "The conversion is async. Use the Async version of the operation"
        });
      }
      output.f |= valFlagAsync;
    }
    const embeddedFn = B_embed(input, fn);
    const inputValue = input.vc ? input.v() : input.i;
    const unionContext = input.g.o & flagUnionTransformContext;
    if (unionContext && isAsync) {
      output.cp = `let ${outputVar}=${embeddedFn}(${inputValue});`;
      return output;
    }
    const rethrow = unionContext ? `${B_embed(input, getOrRethrow)}(x);` : "";
    const failure = B_failWithArg(
      output,
      (e) => B_makeInvalidConversionDetails(input, target, e),
      `x`
    );
    output.cp = `let ${outputVar};try{${outputVar}=${embeddedFn}(${inputValue})${isAsync ? `.catch(x=>${failure})` : ""}}catch(x){${rethrow}${failure}}`;
    return output;
  };
};
var B_neverSlot = (input) => B_invalidOperation(
  input,
  `Can't decode ${inputExpression(input.e)} to ${inputExpression(
    input.e.to
  )}. The conversion is marked as never`
);
var B_invalidOperation = (val, description) => {
  return B_throw({ code: "invalid_operation", reason: description, path: val.path });
};
var B_mergeWithCatch = (val, catchFn, appendSafe) => {
  const valCode = B_merge(val);
  if (valCode === "" && // FIXME: Instead of this wrap all S.transform in a try/catch
  !flagUnsafeHas(val.f, valFlagAsync)) {
    return valCode + (appendSafe !== U ? appendSafe() : "");
  } else {
    const errorVar = B_varWithoutAllocation(val.g);
    B_markThrow(val);
    const catchCode = `${catchFn(errorVar)};throw ${errorVar}`;
    if (flagUnsafeHas(val.f, valFlagAsync)) {
      val.i = `${val.i}.catch(${errorVar}=>{${catchCode}})`;
    }
    return `try{${valCode}${appendSafe !== U ? appendSafe() : ""}}catch(${errorVar}){${catchCode}}`;
  }
};
var B_mergeWithPathPrepend = (val, parent, locationVar, appendSafe) => {
  if (val.path === pathEmpty && locationVar === U) {
    return B_merge(val);
  } else {
    return B_mergeWithCatch(
      val,
      (errorVar) => `${errorVar}.path=${parent.path === "" ? "" : `${inlinedValueFromString(parent.path)}+`}${locationVar !== U ? `'["'+${locationVar}+'"]'+` : ""}${errorVar}.path`,
      appendSafe
    );
  }
};
function noopOperation(i) {
  return i;
}
noopOperation["embedded"] = immutableEmptyArray;

// src/primitives.ts
var int32FormatValidation = (inputVar) => {
  return `${inputVar}<=2147483647&&${inputVar}>=-2147483648&&${inputVar}%1===0`;
};
var typeofCondCache = {};
var typeofCond = (tag) => typeofCondCache[tag] || (typeofCondCache[tag] = (inputVar) => `typeof ${inputVar}==="${tag}"`);
var nanCond = (inputVar) => `Number.isNaN(${inputVar})`;
var isArrayCond = (inputVar) => `Array.isArray(${inputVar})`;
var objectTagCond = (inputVar) => `${typeofCond(objectTag)(inputVar)}&&${inputVar}`;
var instanceofCond = (b, class_) => (inputVar) => `${inputVar} instanceof ${B_embed(b, class_)}`;
var typeofCheckCache = {};
var typeofCheck = (tag) => typeofCheckCache[tag] || (typeofCheckCache[tag] = { c: typeofCond(tag), f: failInvalidType });
var notNanCheck = { c: (inputVar) => `!${nanCond(inputVar)}`, f: failInvalidType };
var int32Check = { c: int32FormatValidation, f: failInvalidType };
var nanCheck = { c: nanCond, f: failInvalidType };
var B_refineTypeofUnknown = (input, tag) => {
  return B_refine(input, input.e, [typeofCheck(tag)]);
};
var B_nextVar = (input, expected) => {
  const output = B_next(input, B_varWithoutAllocation(input.g), expected);
  output.v = _var;
  return output;
};
var numberDecoder = (input) => {
  const inputTagFlag = tagFlags[input.s.type];
  if (flagUnsafeHas(inputTagFlag, tagFlagUnknown)) {
    const checks = [typeofCheck(numberTag)];
    if (input.e.format === "int32") {
      checks.push(int32Check);
    } else {
      if (!flagUnsafeHas(input.g.o, flagDisableNanNumberValidation)) {
        checks.push(notNanCheck);
      }
    }
    return B_refine(input, input.e, checks);
  } else if (flagUnsafeHas(inputTagFlag, tagFlagString)) {
    const output = B_nextVar(input, input.e);
    output.cp = `let ${output.i}=+${input.v()};`;
    output.vc = [
      {
        c: (_inputVar) => input.e.format === "int32" ? int32FormatValidation(output.i) : `!${nanCond(output.i)}`,
        f: failInvalidType
      }
    ];
    return output;
  } else if (flagUnsafeHas(inputTagFlag, tagFlagNaN) && input.e.format !== "int32" && flagUnsafeHas(input.g.o, flagDisableNanNumberValidation)) {
    return B_refine(input, input.e);
  } else if (!flagUnsafeHas(inputTagFlag, tagFlagNumber)) {
    return B_unsupportedDecode(input, input.s, input.e);
  } else if (input.s.format !== input.e.format && input.e.format === "int32") {
    return B_refine(input, input.e, [int32Check]);
  } else {
    return input;
  }
};
var float = /* @__PURE__ */ initSchema(numberTag, (s2) => {
  s2.decoder = numberDecoder;
});
var int = /* @__PURE__ */ initSchema(numberTag, (s2) => {
  s2.format = "int32";
  s2.minimum = -2147483648;
  s2.maximum = 2147483647;
  s2.decoder = numberDecoder;
});
var inputToString = (input) => {
  return B_next(input, `""+${input.i}`, string);
};
var stringDecoderFn = (input) => {
  const inputTagFlag = tagFlags[input.s.type];
  if (flagUnsafeHas(inputTagFlag, tagFlagUnknown)) {
    return B_refineTypeofUnknown(input, stringTag);
  } else if (flagUnsafeHas(
    inputTagFlag,
    tagFlagBoolean | tagFlagNumber | tagFlagBigint | tagFlagUndefined | tagFlagNull | tagFlagNaN
  ) && isLiteral(input.s)) {
    const const_ = "" + input.s.const;
    const schema = baseSchema(stringTag, false);
    schema.const = const_;
    return B_next(input, `"${const_}"`, schema);
  } else if (flagUnsafeHas(inputTagFlag, tagFlagBoolean | tagFlagNumber | tagFlagBigint)) {
    return inputToString(input);
  } else if (!flagUnsafeHas(inputTagFlag, tagFlagString)) {
    return B_unsupportedDecode(input, input.s, input.e);
  } else {
    return input;
  }
};
var string = /* @__PURE__ */ initSchema(stringTag, (s2) => {
  s2.decoder = stringDecoderFn;
});
var booleanDecoder = (input) => {
  const inputTagFlag = tagFlags[input.s.type];
  if (flagUnsafeHas(inputTagFlag, tagFlagUnknown)) {
    return B_refineTypeofUnknown(input, booleanTag);
  } else if (flagUnsafeHas(inputTagFlag, tagFlagString)) {
    const output = B_nextVar(input, input.e);
    const inputVar = input.v();
    output.cp = `let ${output.i};(${output.i}=${inputVar}==="true")||${inputVar}==="false"||${B_embedInvalidInput(
      input
    )};`;
    return output;
  } else if (!flagUnsafeHas(inputTagFlag, tagFlagBoolean)) {
    return B_unsupportedDecode(input, input.s, input.e);
  } else {
    return input;
  }
};
var bool = /* @__PURE__ */ initSchema(booleanTag, (s2) => {
  s2.decoder = booleanDecoder;
});
var bigintDecoder = (input) => {
  const inputTagFlag = tagFlags[input.s.type];
  if (flagUnsafeHas(inputTagFlag, tagFlagUnknown)) {
    return B_refineTypeofUnknown(input, bigintTag);
  } else if (flagUnsafeHas(inputTagFlag, tagFlagString)) {
    const output = B_nextVar(input, input.e);
    output.cp = `let ${output.i};try{${output.i}=BigInt(${input.v()})}catch(_){${B_embedInvalidInput(
      input
    )}}`;
    return output;
  } else if (flagUnsafeHas(inputTagFlag, tagFlagNumber)) {
    return B_next(input, `BigInt(${input.i})`, input.e);
  } else if (!flagUnsafeHas(inputTagFlag, tagFlagBigint)) {
    return B_unsupportedDecode(input, input.s, input.e);
  } else {
    return input;
  }
};
var bigint = /* @__PURE__ */ initSchema(bigintTag, (s2) => {
  s2.decoder = bigintDecoder;
});
var symbolDecoder = (input) => {
  const inputTagFlag = tagFlags[input.s.type];
  if (flagUnsafeHas(inputTagFlag, tagFlagUnknown)) {
    return B_refineTypeofUnknown(input, symbolTag);
  } else if (!flagUnsafeHas(inputTagFlag, tagFlagSymbol)) {
    return B_unsupportedDecode(input, input.s, input.e);
  } else {
    return input;
  }
};
var symbol = /* @__PURE__ */ initSchema(symbolTag, (s2) => {
  s2.decoder = symbolDecoder;
});
var literalDecoder = (input) => {
  const expectedSchema = input.e;
  if (expectedSchema.noValidation && !input.u) {
    return B_nextConst(input, expectedSchema);
  } else if (isLiteral(input.s)) {
    if (input.s.const === expectedSchema.const) {
      return input;
    } else {
      return B_nextConst(input, expectedSchema);
    }
  } else {
    const schemaTagFlag = tagFlags[expectedSchema.type];
    if (flagUnsafeHas(tagFlags[input.s.type], tagFlagString) && flagUnsafeHas(
      schemaTagFlag,
      tagFlagBoolean | tagFlagNumber | tagFlagBigint | tagFlagUndefined | tagFlagNull | tagFlagNaN
    )) {
      const stringConstSchema = baseSchema(stringTag, false);
      stringConstSchema.const = "" + expectedSchema.const;
      const stringConstVal = B_nextConst(input, stringConstSchema, stringConstSchema);
      stringConstVal.vc = [
        {
          c: (inputVar) => `${inputVar}==="${stringConstSchema.const}"`,
          f: failInvalidType
        }
      ];
      return B_nextConst(stringConstVal, expectedSchema, expectedSchema);
    } else if (flagUnsafeHas(schemaTagFlag, tagFlagNaN)) {
      return B_refine(input, expectedSchema, [nanCheck]);
    } else {
      return B_refine(input, expectedSchema, [
        {
          c: (inputVar) => `${inputVar}===${B_inlineConst(input, expectedSchema)}`,
          f: failInvalidType
        }
      ]);
    }
  }
};
var unit = /* @__PURE__ */ initSchema(undefinedTag, (s2) => {
  s2.const = U;
  s2.decoder = literalDecoder;
});
var void_ = /* @__PURE__ */ initSchema(undefinedTag, (s2) => {
  s2.const = U;
  s2.name = "void";
  s2.decoder = literalDecoder;
});
var nullLiteral = /* @__PURE__ */ initSchema(nullTag, (s2) => {
  s2.const = null;
  s2.decoder = literalDecoder;
});
var nan = /* @__PURE__ */ initSchema(nanTag, (s2) => {
  s2.const = NaN;
  s2.decoder = literalDecoder;
});
var Literal_parse = (value) => {
  if (value === null) {
    return nullLiteral;
  } else {
    const tag = typeof value;
    if (tag === undefinedTag) {
      return unit;
    } else if (tag === numberTag && Number.isNaN(value)) {
      return nan;
    } else if (tag === objectTag) {
      const s2 = baseSchema(instanceTag, true);
      s2.class = value["constructor"];
      s2.const = value;
      s2.decoder = literalDecoder;
      return s2;
    } else {
      const s2 = baseSchema(tag, true);
      s2.const = value;
      s2.decoder = literalDecoder;
      return s2;
    }
  }
};

// src/parse.ts
var parse = (input) => {
  let result = input;
  let appliedEncoderRef = U;
  let loopCount = 0;
  while (!result.io || result.e.to) {
    const appliedEncoder = appliedEncoderRef;
    appliedEncoderRef = U;
    const loopInput = result;
    loopCount = loopCount + 1;
    if (loopCount > 50) {
      const error = new Error("Loop count exceeded 50");
      throw error;
    }
    if (loopInput.e["$defs"]) {
      if (loopInput.g.d) {
        Object.assign(loopInput.g.d, loopInput.e["$defs"]);
      } else {
        loopInput.g.d = loopInput.e["$defs"];
      }
    }
    if (flagUnsafeHas(
      loopInput.f,
      valFlagAsync
    )) {
      const operationInputVar = loopInput.v();
      const operationInput = B_scope(loopInput);
      const operationOutput = parse(operationInput);
      const operationCode = B_merge(operationOutput);
      if (operationInput.i !== operationOutput.i || operationCode !== "") {
        result = B_next(
          loopInput,
          `${operationInputVar}.then(${operationInputVar}=>{${operationCode}return ${operationOutput.i}})`,
          operationOutput.s,
          operationOutput.e
        );
      } else {
        result = B_refine(loopInput, operationOutput.s, U, operationOutput.e);
      }
      result.f |= valFlagAsync;
      result.io = true;
    } else if (loopInput.io) {
      const to = loopInput.e.to;
      if (loopInput.e.parser !== U) {
        result = loopInput.e.parser(loopInput);
      } else {
        result = B_refine(result, U, U, to);
      }
    } else {
      const maybeEncoder = loopInput.s.encoder;
      if (maybeEncoder && maybeEncoder !== appliedEncoder && loopInput.s !== loopInput.e && loopInput.e.type !== unknownTag && // A `noValidation` target (S.assert's result sentinel) throws the value
      // away, so there is nothing for an encoder to re-represent.
      !loopInput.e.noValidation) {
        result = maybeEncoder(loopInput, loopInput.e);
      }
      if (loopInput !== result) {
        appliedEncoderRef = maybeEncoder;
      } else {
        result = loopInput.e.decoder(loopInput);
        if (!result.io) {
          result = B_markOutput(result, result);
        }
      }
    }
  }
  return result;
};
var parseDynamic = (input) => {
  try {
    return parse(input);
  } catch (exn) {
    const error = getOrRethrow(exn);
    error.path = pathConcat(
      input.p !== U ? input.p.path : pathEmpty,
      pathConcat(pathConcat(input.path, pathDynamic), error.path)
    );
    throw error;
  }
};
var compileDecoder = (schema, expected, flag, defs) => {
  const input = B_operationArg(isLiteral(schema) ? unknown : schema, expected, flag, defs);
  const output = parse(input);
  const code = B_merge(output);
  const isAsync = flagUnsafeHas(output.f, valFlagAsync);
  expected.isAsync = isAsync;
  const hasTransform = output.t === true;
  expected.hasTransform = hasTransform;
  if (code === "" && (output === input || output.i === input.i) && !flagUnsafeHas(flag, flagAsync)) {
    return noopOperation;
  } else {
    let inlinedOutput = output.i;
    if (flagUnsafeHas(flag, flagAsync) && !isAsync && !defs) {
      inlinedOutput = `Promise.resolve(${inlinedOutput})`;
    }
    const inlinedFunction = `${operationArgVar}=>{${code}return ${inlinedOutput}}`;
    const fn = new Function("e", "s", `return ${inlinedFunction}`)(input.g.e, s);
    fn.embedded = input.g.e;
    return fn;
  }
};
var getOutputSchema = (schema) => {
  if (schema.to !== U) {
    return getOutputSchema(schema.to);
  } else {
    return schema;
  }
};
var reverseSwap = (mut, a, b) => {
  const previous = mut[a];
  if (mut[b] !== U) {
    mut[a] = mut[b];
  } else {
    delete mut[a];
  }
  if (previous !== U) {
    mut[b] = previous;
  } else {
    delete mut[b];
  }
};
var reverseDict = (dict) => {
  const reversed = /* @__PURE__ */ Object.create(null);
  for (const key in dict) {
    reversed[key] = /* @__PURE__ */ reverse(dict[key]);
  }
  return reversed;
};
var reverse = /* @__NO_SIDE_EFFECTS__ */ (schema) => {
  const schemaRecord = schema;
  if (reversedKey in schemaRecord) {
    return schemaRecord[reversedKey];
  } else {
    let reversedHead = U;
    let current = schema;
    while (current) {
      const mut = copySchema(current);
      const next = mut.to;
      if (reversedHead === U) {
        delete mut.to;
      } else {
        mut.to = reversedHead;
      }
      const record = mut;
      reverseSwap(record, "parser", "serializer");
      reverseSwap(record, "refiner", "inputRefiner");
      reverseSwap(record, "fromDefault", "default");
      if (mut.items !== U) {
        mut.items = mut.items.map(reverse);
      }
      if (mut.properties !== U) {
        mut.properties = reverseDict(mut.properties);
      }
      if (typeof mut.additionalItems === objectTag) {
        mut.additionalItems = /* @__PURE__ */ reverse(mut.additionalItems);
      }
      if (mut.anyOf !== U) {
        const anyOf = mut.anyOf;
        const has = {};
        const newAnyOf = [];
        for (let idx = 0; idx <= anyOf.length - 1; idx++) {
          const s2 = anyOf[idx];
          const reversed = /* @__PURE__ */ reverse(s2);
          newAnyOf.push(reversed);
          setHas(has, reversed.type);
        }
        mut.has = has;
        mut.anyOf = newAnyOf;
      }
      if (mut["$defs"] !== U) {
        mut["$defs"] = reverseDict(mut["$defs"]);
      }
      reversedHead = mut;
      current = next;
    }
    const r = reversedHead;
    valueOptions[valKey] = r;
    Object.defineProperty(schema, reversedKey, valueOptions);
    valueOptions[valKey] = schema;
    Object.defineProperty(r, reversedKey, valueOptions);
    return r;
  }
};
var outputExpression = /* @__NO_SIDE_EFFECTS__ */ (schema) => inputExpression(/* @__PURE__ */ reverse(schema));
// @__NO_SIDE_EFFECTS__
function getDecoder(..._args) {
  const args = arguments;
  let idx = 0;
  let flag = U;
  let keyRef = "";
  let maxSeq = 0;
  let cacheTarget = U;
  while (flag === U) {
    const arg = args[idx];
    if (!arg) {
      const f = globalConfig.f;
      flag = f;
      keyRef = keyRef + "-" + f;
    } else if (typeof arg === numberTag) {
      const f = arg | globalConfig.f;
      flag = f;
      keyRef = keyRef + "-" + f;
    } else {
      const schema = arg;
      const seq2 = schema.seq;
      if (seq2 > maxSeq) {
        maxSeq = seq2;
        cacheTarget = schema;
      }
      keyRef = keyRef + seq2 + "-";
      idx = idx + 1;
    }
  }
  if (cacheTarget === U) {
    return panic("No schema provided for decoder.");
  } else {
    const key = keyRef;
    const cacheTargetRecord = cacheTarget;
    if (key in cacheTargetRecord) {
      return cacheTargetRecord[key];
    } else {
      let schema = args[idx - 1];
      for (let i = idx - 2; i >= 0; i--) {
        const to = schema;
        schema = updateOutput(args[i], (mut) => {
          mut.to = to;
        });
      }
      const f = compileDecoder(schema, schema, flag, U);
      valueOptions[valKey] = f;
      Object.defineProperty(cacheTarget, key, valueOptions);
      return f;
    }
  }
}
var nestedLoc = "BS_PRIVATE_NESTED_SOME_NONE";
var neverBuilderFn = (input) => {
  const output = B_refine(input, never_, U, never_);
  output.cp = B_embedInvalidInput(input) + ";";
  return output;
};
var never_ = /* @__PURE__ */ initSchema(neverTag, (s2) => {
  s2.decoder = neverBuilderFn;
});
var nestedOptionParser = (input) => {
  const nextSchema = input.e.to;
  return B_next(
    input,
    `{${nestedLoc}:${getOutputSchema(input.e).properties[nestedLoc].const}}`,
    nextSchema,
    nextSchema
  );
};
var instanceDecoder = (input) => {
  const inputTagFlag = tagFlags[input.s.type];
  if (flagUnsafeHas(inputTagFlag, tagFlagUnknown)) {
    return B_refine(input, input.e, [
      {
        c: instanceofCond(input, input.e.class),
        f: failInvalidType
      }
    ]);
  } else if (flagUnsafeHas(inputTagFlag, tagFlagInstance) && input.s.class === input.e.class) {
    return input;
  } else {
    return B_unsupportedDecode(input, input.s, input.e);
  }
};
var instance = /* @__NO_SIDE_EFFECTS__ */ (class_) => {
  const mut = baseSchema(instanceTag, true);
  mut.class = class_;
  mut.decoder = instanceDecoder;
  return mut;
};
var typeCheckCond = (input, schema, inputVar) => {
  const tagFlag = tagFlags[schema.type];
  if (flagUnsafeHas(tagFlag, tagFlagObject)) {
    return `${objectTagCond(inputVar)}&&!${isArrayCond(inputVar)}`;
  } else if (flagUnsafeHas(tagFlag, tagFlagArray)) {
    return isArrayCond(inputVar);
  } else if (flagUnsafeHas(tagFlag, tagFlagInstance)) {
    return instanceofCond(input, schema.class)(inputVar);
  } else if (flagUnsafeHas(tagFlag, tagFlagNumber)) {
    const typeofCheck2 = typeofCond(numberTag)(inputVar);
    if (flagUnsafeHas(input.g.o, flagDisableNanNumberValidation)) {
      return typeofCheck2;
    } else {
      return `${typeofCheck2}&&!${nanCond(inputVar)}`;
    }
  } else if (flagUnsafeHas(tagFlag, tagFlagNaN)) {
    return nanCond(inputVar);
  } else if (flagUnsafeHas(tagFlag, tagFlagUndefined | tagFlagNull)) {
    return `${inputVar}===${B_inlineConst(input, schema)}`;
  } else if (flagUnsafeHas(
    tagFlag,
    tagFlagString | tagFlagBoolean | tagFlagBigint | tagFlagSymbol
  )) {
    return typeofCond(schema.type)(inputVar);
  } else {
    return "";
  }
};

// src/union.ts
var unionAnyTag = ~0;
var unionBoundaryTags = tagFlagUnion | tagFlagRef | tagFlagFunction;
var unionOpaqueTags = tagFlagUnknown | unionBoundaryTags | tagFlagNever;
var unionRuntimeSame = (a, b) => a.type === b.type && a.class === b.class;
var unionSameType = (a, b) => a === b || unionRuntimeSame(a, b) && !(tagFlags[a.type] & (tagFlagRef | tagFlagUnion)) && a.format === b.format;
var unionLiteralEqual = (a, b) => a === b || a !== a && b !== b;
var unionOutput = (schema) => {
  let output = schema;
  while (output.type !== neverTag && output.to !== U) {
    if (output.parser === B_neverSlot) return never_;
    output = output.to;
  }
  return output;
};
var unionNeverLink = (schema) => {
  for (let node = schema; node !== U && node.type !== neverTag; node = node.to) {
    if (node.parser === B_neverSlot) return true;
  }
  return false;
};
var unionIsTransparent = (schema) => {
  if (schema.type !== anyOfTag) return false;
  let fields = 0;
  for (const key in schema) {
    if (key !== "isAsync" && key !== "hasTransform") fields++;
  }
  return fields === 6;
};
var unionTraits = (schema) => {
  const tag = tagFlags[schema.type];
  let traits = 0;
  if (tag & unionBoundaryTags || schema.parser !== U) return 15;
  if (schema.refiner !== U || schema.inputRefiner !== U) {
    traits |= 3;
  } else if (tag & (tagFlagObject | tagFlagArray | tagFlagInstance)) {
    traits |= 2;
  }
  if (schema.format !== U || isLiteral(schema)) {
    traits |= 1;
  }
  const to = schema.to;
  if (to !== U) {
    if (to === schema || to.parser !== U || tagFlags[to.type] & unionBoundaryTags) {
      traits |= 15;
    } else if (!(to.noValidation === true || tagFlags[to.type] & tagFlagUnknown || unionRuntimeSame(schema, to) || to.type === anyOfTag && unionMask(to, 1) & tag)) {
      traits |= 9;
    } else {
      traits |= unionTraits(to);
    }
  }
  const fields = schema.items || schema.properties;
  for (const key in fields) {
    const field = fields[key];
    traits |= unionTraits(field);
  }
  if (typeof schema.additionalItems === "object") {
    traits |= unionTraits(schema.additionalItems);
  }
  return traits;
};
var unionIsNoop = (schema) => {
  if (schema.to !== U || schema.parser !== U || tagFlags[schema.type] & tagFlagRef) {
    return false;
  }
  const fields = schema.anyOf || schema.items || schema.properties;
  for (const key in fields) {
    if (!unionIsNoop(fields[key])) return false;
  }
  return typeof schema.additionalItems !== "object" || unionIsNoop(schema.additionalItems);
};
var unionIsWider = (variants, inputVariants) => inputVariants.every((inputSchema, idx) => {
  const schema = variants[idx];
  return schema !== U && !(tagFlags[inputSchema.type] & (tagFlagArray | tagFlagInstance | tagFlagRef | tagFlagUnion | tagFlagObject)) && inputSchema.type === schema.type && unionLiteralEqual(inputSchema.const, schema.const) && inputSchema.to === U && schema.to === U;
});
var unionFail = (schema, path, input, ...unionErrors) => B_throw(
  B_makeInvalidInputDetails(
    schema,
    unknown,
    path,
    input,
    unionErrors.length ? unionErrors : U
  )
);
var unionEmitChain = (cases, ctx) => {
  if (cases.length === 1) {
    const c = cases[0];
    if (c.b === "" && c.c === "") return "";
    if (c.b === "") {
      return `if(!(${c.c})){${ctx.f("")}}`;
    }
    if (c.c === "") return c.b + ";";
    return `if(${c.c}){${c.b}}else{${ctx.f("")}}`;
  }
  let code = "";
  let caught = false;
  let exhaustive = false;
  const attempt = (c, idx) => {
    if (c.b === "") return "break";
    const body = c.b.endsWith(";") ? c.b : `${c.b};`;
    if (c.f & 1 && (c.f & unionMemberFalls || caught)) {
      caught = true;
      const record = c.f & 4 ? `x=${ctx.r()}(x);if(x.expected===${ctx.s()}){x=x.unionErrors;x&&(r||(r=[])).push(...x)}else{(r||(r=[])).push(x)}` : `(r||(r=[])).push(${ctx.r()}(x))`;
      return `try{${body}break}catch(x){${record}${!(c.f & unionMemberFalls) && unconditional > idx ? `;${ctx.f(",...(r||[])")}` : ""}}`;
    }
    return `${body}break`;
  };
  let unconditional = -1;
  for (let idx = 0; idx < cases.length; idx++) {
    if (cases[idx].c === "") unconditional = idx;
  }
  let last = "";
  let open = false;
  for (let idx = 0; idx < cases.length; idx++) {
    const c = cases[idx];
    const shared = c.c !== "" && c.c === last;
    if (shared && !open) continue;
    const arm = attempt(c, idx);
    open = arm[0] === "t";
    last = c.c;
    if (shared) {
      code = `${code.slice(0, -1)}${arm}}`;
    } else if (c.c === "") {
      code += open ? arm : `${arm};`;
      if (!open) {
        exhaustive = true;
        break;
      }
    } else {
      code += arm === "break" ? `if(${c.c})break;` : `if(${c.c}){${arm}}`;
    }
  }
  if (!exhaustive) {
    code += ctx.f(caught ? ",...(r||[])" : "");
  }
  return `for(;;){${caught ? "let r;" : ""}${code}}`;
};
var unionNarrowSchema = (schema) => {
  const tagFlag = tagFlags[schema.type];
  const container = tagFlagObject | tagFlagArray;
  const narrow = baseSchema(schema.type, false);
  narrow.encoder = schema.encoder;
  if (tagFlag & tagFlagInstance) {
    narrow.class = schema.class;
  } else if (tagFlag & container) {
    narrow.additionalItems = unknown;
    if (tagFlag & tagFlagObject) {
      narrow.properties = immutableEmptyObject;
    } else {
      narrow.items = immutableEmptyArray;
    }
  } else if (tagFlag & (tagFlagNull | tagFlagUndefined | tagFlagNaN)) {
    narrow.const = schema.const;
  }
  narrow.decoder = (input) => {
    if (tagFlags[input.s.type] & tagFlagUnknown) {
      return B_refine(input, input.e, [
        {
          c: (inputVar) => typeCheckCond(input, schema, inputVar),
          f: failInvalidType
        }
      ]);
    }
    if (unionRuntimeSame(input.s, narrow)) {
      return tagFlag & container ? B_refine(input, input.e) : input;
    }
    return schema.decoder(input);
  };
  return narrow;
};
var unionObjectish = tagFlagObject | tagFlagInstance;
var unionStructured = tagFlagObject | tagFlagArray | tagFlagInstance | tagFlagRef | tagFlagUnion;
var unionWiden = (tagFlag, nan2) => tagFlag | (tagFlag & unionObjectish ? unionObjectish : tagFlag & tagFlags[numberTag] ? nan2 : 0);
var unionMask = (schema, mode, nan2 = 0) => {
  if (mode === 2) {
    const defs = schema["$defs"];
    const ref = schema["$ref"];
    if (defs !== U && ref !== U) {
      const resolved = defs[ref.slice(ref.lastIndexOf("/") + 1)];
      if (resolved !== U && resolved !== schema) {
        return unionMask(resolved, 1, nan2);
      }
    }
  }
  const tagFlag = tagFlags[schema.type];
  if (!mode && tagFlag & tagFlagNever) {
    return 0;
  }
  if (mode && tagFlag & tagFlagUnion) {
    let mask = 0;
    const variants = schema.anyOf;
    for (let i = 0; i < variants.length; i++) {
      mask |= unionMask(variants[i], 1, nan2);
    }
    return mask;
  }
  return tagFlag & (tagFlagUnknown | tagFlagUnion | tagFlagRef) ? unionAnyTag : unionWiden(tagFlag, nan2);
};
var unionMemberFalls = 8;
var unionMemberDirect = 16;
var unionGroup = (member) => ({
  m: member.m,
  a: [member],
  f: member.f & unionMemberDirect,
  p: member.p,
  o: false
});
var unionDiscriminator = (schema) => {
  if (isLiteral(schema)) {
    return ["", schema.const];
  }
  const fields = schema.properties || schema.items;
  for (const key in fields) {
    const field = fields[key];
    if (isLiteral(field)) {
      return [key, field.const];
    }
  }
  return U;
};
var unionCheckPartial = (input, source, target, variants, outputSide) => {
  const other = outputSide ? target : source;
  let matched = U;
  let unmatched = false;
  for (let idx = 0; idx < variants.length; idx++) {
    const variant = variants[idx];
    const match = outputSide ? unionOutput(variant) : variant;
    if (variant.type === neverTag || (outputSide ? match.type === neverTag : unionNeverLink(variant))) {
      continue;
    }
    if (unionSameType(other, match)) {
      matched || (matched = variant);
    } else {
      unmatched = true;
    }
  }
  if (matched !== U && unmatched) {
    unionInvalid(
      input,
      source,
      target,
      `${inputExpression(matched)} has the same type as the ${outputSide ? "target" : "source"} and the others don't`
    );
  }
};
var unionUncovered = (input, source, target, variant) => unionInvalid(
  input,
  source,
  target,
  `${inputExpression(variant)} has no same-type variant on the other side`
);
var unionInvalid = (input, from, to, why) => B_invalidOperation(
  input,
  `Invalid operation: can't convert ${inputExpression(from)} to ${inputExpression(
    to
  )} \u2014 ${why}. Use S.to to say what you mean, or S.never to mark a variant unreachable`
);
var unionNormalize = (variants, source, nan2) => {
  let flags = 0;
  const sourceLiteral = isLiteral(source);
  for (let i = 0; i < variants.length; i++) {
    const member = variants[i];
    if (sourceLiteral && isLiteral(member) && unionLiteralEqual(member.const, source.const)) {
      flags |= tagFlagUnknown;
    }
    flags |= tagFlags[member.type] & (tagFlagObject | tagFlags[numberTag]);
  }
  return {
    m: unionMask(source, 2, nan2),
    f: flags,
    t: tagFlags[source.type]
  };
};
var unionAnalyze = (normalized, variants, source, nan2) => {
  const sourceMask = normalized.m;
  const normalizedFlags = normalized.f;
  const out = [];
  const sourceTag = normalized.t;
  const unknownSource = sourceTag & tagFlagUnknown;
  const sourceBoundary = sourceTag & (tagFlagUnion | tagFlagRef);
  const unionSource = sourceBoundary && sourceMask !== unionAnyTag;
  const sourceDiscriminator = unionDiscriminator(source);
  const exact = normalizedFlags & tagFlagUnknown;
  const broadObject = normalizedFlags & tagFlagObject;
  const broadNumber = normalizedFlags & tagFlags[numberTag];
  const numberish = tagFlags[numberTag] | tagFlagNaN;
  for (let i = 0; i < variants.length; i++) {
    const s2 = variants[i];
    const tag = tagFlags[s2.type];
    const inputMask = unionMask(s2, 1, nan2);
    const d = unionDiscriminator(s2);
    const same = unionRuntimeSame(source, s2);
    const discriminatorDisjoint = sourceDiscriminator !== U && d !== U && same && sourceDiscriminator[0] === d[0] && !unionLiteralEqual(sourceDiscriminator[1], d[1]);
    const accepts = !(tag & tagFlagNever) && !unionNeverLink(s2) && !discriminatorDisjoint && (!exact || (isLiteral(s2) ? unionLiteralEqual(s2.const, source.const) : sourceMask & inputMask));
    const native = sourceMask & tag;
    const coerces = accepts && !unknownSource && !(unionSource ? native : same);
    const output = unionOutput(s2);
    const traits = unionTraits(s2);
    const sourceDeopt = sourceBoundary && (!unionSource || coerces);
    const effect = output.type === neverTag ? 3 : traits & 4 || sourceDeopt ? 4 : coerces || traits & 8 ? 2 : traits & 1 || tag & unionStructured ? 1 : 0;
    const nested = s2.type === objectTag && nestedLoc in s2.properties;
    const f = traits & 7 | (effect !== 0 ? 1 : 0) | (sourceDeopt ? 4 : 0) | (!unknownSource && same || tag & unionOpaqueTags ? unionMemberDirect : 0);
    const p = nested || broadObject && tag & (tagFlagArray | tagFlagInstance) || broadNumber && tag & tagFlagNaN ? 0 : d !== U ? 1 : 2;
    out.push({
      i,
      s: s2,
      m: accepts ? unknownSource ? inputMask : unionSource ? native ? inputMask : s2.type === undefinedTag && sourceMask & tagFlagNull ? tagFlagNull : s2.type === nullTag && sourceMask & tagFlagUndefined ? tagFlagUndefined : (
        // Reached only by coercion. Every built-in cross-tag
        // coercion parses a string (`BigInt`, `Number`, `new Date`),
        // so a source that can produce one is assumed to be coerced
        // through it — narrow enough to keep the case out of an
        // unnecessary fallback. With no string in the source that
        // guess describes nothing, and claiming too little would let
        // the dispatch raise where a later member should have run,
        // so fall back to "any type the source produces".
        sourceMask & tagFlagString ? tagFlagString : sourceMask
      ) : sourceMask : 0,
      o: !!accepts && output.type !== neverTag,
      e: effect,
      f,
      p,
      k: tag & tagFlagInstance ? s2.class : s2.type,
      r: tag & unionObjectish ? unionObjectish : tag & numberish ? numberish : unionWiden(tag, nan2),
      d
    });
  }
  return out;
};
var unionPlan = (members) => {
  var _a;
  const sequence = [];
  const active = [];
  const priority = [];
  let total = 0;
  let effects = 0;
  for (let i = 0; i < members.length; i++) {
    const member = members[i];
    if (member.e > 1) {
      effects |= member.m;
    } else if (!member.e) {
      total |= member.m;
    }
  }
  for (let i = 0; i < members.length; i++) {
    const member = members[i];
    if (member.m === 0 || member.e === 1 && !(member.f & 2) && !(member.m & (effects | ~total))) {
      continue;
    }
    const bucketed = member.r !== unionAnyTag && (member.m & ~member.r) === 0;
    const compatible = member.e < 2 || member.e === 4 && member.d?.[0] === "";
    let bucket = bucketed ? member.p === 0 ? priority[member.r] || active[member.r] : active[member.r] : U;
    let open = U;
    let broad = false;
    if (bucket !== U) {
      for (let j = 0; j < bucket.t.length; j++) {
        const group = bucket.t[j];
        const first = group.a[0];
        broad || (broad = group.p === 2);
        if (open === U && compatible && group.o && first.k === member.k && first.e < 2 === member.e < 2 && group.p === 0 === (member.p === 0)) {
          open = group;
        } else if (group.o && group.m & member.m) {
          group.o = false;
        }
      }
    }
    for (const key in active) {
      const other = active[+key];
      if (other !== bucket && other.m & member.m) {
        delete active[+key];
      }
    }
    if (!bucketed) {
      for (const key in priority) {
        if (priority[+key].m & member.m) {
          delete priority[+key];
        }
      }
      sequence.push(unionGroup(member));
      continue;
    }
    if (bucket !== U && open === U && member.p === 1 && broad) {
      delete active[member.r];
      bucket = U;
    }
    if (bucket === U) {
      bucket = {
        m: 0,
        t: []
      };
      active[member.r] = bucket;
      priority[_a = member.r] || (priority[_a] = bucket);
      sequence.push(bucket);
    }
    bucket.m |= member.m;
    if (open !== U) {
      open.a.push(member);
      open.m |= member.m;
      open.f &= ~unionMemberDirect;
    } else {
      const group = unionGroup(member);
      group.o = compatible;
      bucket.t.push(group);
    }
    if (!compatible) {
      delete active[member.r];
    }
  }
  const plan = [];
  for (let i = 0; i < sequence.length; i++) {
    const item = sequence[i];
    if ("a" in item) {
      plan.push(item);
    } else {
      plan.push(...item.t.sort((a, b) => a.p - b.p));
    }
  }
  const later = [];
  let laterMask = 0;
  let laterBroad = 0;
  for (let i = plan.length - 1; i >= 0; i--) {
    const group = plan[i];
    let key = U;
    let values;
    for (let j = group.a.length - 1; j >= 0; j--) {
      const member = group.a[j];
      const d = member.d;
      const conflict2 = d === U || key === false || key !== U && key !== d[0];
      if (key !== U && (conflict2 || values.has(d[1]))) {
        member.f |= unionMemberFalls;
        group.f |= 2;
      }
      if (conflict2) {
        key = false;
      } else {
        key = d[0];
        (values || (values = /* @__PURE__ */ new Set())).add(d[1]);
      }
    }
    const route = group.a[0].r;
    const semantic = later[route];
    let overlaps = !!(laterMask & group.m) && (!!(laterBroad & group.m) || key === false || semantic === U || semantic === false || semantic[0] !== key);
    if (!overlaps && semantic !== U && semantic !== false) {
      for (const value of values) {
        if (semantic[1].has(value)) {
          overlaps = true;
          break;
        }
      }
    }
    if (overlaps || laterMask && tagFlags[group.a[0].s.type] & unionOpaqueTags && (group.a[0].s.to !== U || group.a[0].s.parser !== U)) {
      group.f |= unionMemberFalls | 2;
    }
    if (group.a.length !== 1 || !(group.f & unionMemberDirect)) {
      group.n = unionNarrowSchema(group.a[0].s);
    }
    if (route !== unionAnyTag && (group.m & ~route) === 0) {
      if (key === false) {
        later[route] = false;
      } else if (semantic === U) {
        later[route] = [key, values];
      } else if (semantic !== false) {
        if (semantic[0] !== key) {
          later[route] = false;
        } else {
          for (const value of values) {
            semantic[1].add(value);
          }
        }
      }
    } else {
      laterBroad |= group.m;
    }
    laterMask |= group.m;
  }
  return plan;
};
var unionEmit = (input, self, plan, toPerCase) => {
  const initialInline = input.i;
  let output = B_refine(input);
  const awaitAsync = plan.some((group) => group.f & 2);
  const outputBySource = [];
  let salvaged = "";
  let rethrow = "";
  let expected = "";
  const ctx = {
    f: (caught) => `${B_embed(input, unionFail.bind(U, self, input.path))}(${input.v()}${salvaged}${caught})`,
    r: () => rethrow || (rethrow = B_embed(input, getOrRethrow)),
    s: () => expected || (expected = B_embed(input, self))
  };
  const compile = (member, source, target = source) => {
    const mark = input.g.t;
    const caseInput = B_scope(source);
    caseInput.u = true;
    caseInput.t = source.t;
    caseInput.io = false;
    caseInput.e = member.s;
    let caseOut;
    const options = input.g.o;
    input.g.o |= flagUnionTransformContext;
    try {
      if (self.perVariant) {
        try {
          caseOut = parse(caseInput);
        } catch (exn) {
          salvaged += `,${B_embed(input, getOrRethrow(exn))}`;
          return U;
        }
      } else {
        caseOut = parse(caseInput);
      }
    } finally {
      input.g.o = options;
    }
    if (member.o) outputBySource[member.i] = caseOut.s;
    const cond = { c: "", h: [] };
    const falls = member.f & unionMemberFalls;
    let body = B_merge(caseOut, cond);
    const async = caseOut.f & valFlagAsync;
    output.f |= async;
    if (caseOut.t) {
      output.t = true;
      const itemVar = target.v();
      if (async || caseOut.i !== itemVar) {
        body += `${itemVar}=${async && awaitAsync ? "await " : ""}${caseOut.i}`;
      }
    }
    const flags = (body !== "" && input.g.t !== mark ? 1 : 0) | (async && awaitAsync ? 2 : 0) | (falls ? unionMemberFalls : 0);
    return { c: cond.c, b: body, f: flags };
  };
  const cases = [];
  for (let i = 0; i < plan.length; i++) {
    const group = plan[i];
    if (group.a.length === 1 && group.f & unionMemberDirect) {
      const c = compile(group.a[0], input);
      if (c !== U) {
        if (group.f & unionMemberFalls) {
          c.f |= unionMemberFalls;
        }
        cases.push(c);
        if (c.c === "" && c.b === "") break;
      }
      continue;
    }
    const mark = input.g.t;
    const narrowInput = B_scope(input);
    narrowInput.io = false;
    narrowInput.e = group.n;
    const narrow = parse(narrowInput);
    const inner = [];
    for (let j = 0; j < group.a.length; j++) {
      const c = compile(group.a[j], narrow, narrowInput);
      if (c !== U) {
        inner.push(c);
        if (c.c === "" && c.b === "") break;
      }
    }
    if (!inner.length) continue;
    const cond = { c: "", h: [] };
    let body;
    let grouped = false;
    if (inner.every((c) => c.b === "")) {
      if (!inner.some((c) => c.c === "")) {
        let fused = inner.map((c) => c.c).join("||");
        if (inner.length > 1) fused = `(${fused})`;
        B_pushCheck(narrow, { c: () => fused, f: failInvalidType });
      }
      body = B_merge(narrow, cond);
    } else {
      const narrowCode = B_merge(narrow, cond);
      const only = inner.length === 1 ? inner[0] : U;
      if (only !== U && narrowCode === "") {
        if (only.c !== "") {
          cond.c = cond.c ? `${cond.c}&&${only.c}` : only.c;
        }
        body = only.b;
      } else {
        body = narrowCode + unionEmitChain(inner, ctx);
        grouped = inner.length > 1;
      }
    }
    const flags = (body !== "" && input.g.t !== mark ? 1 : 0) | (inner.some((c) => c.f & 2) ? 2 : 0) | group.f & unionMemberFalls | (grouped ? 4 : 0);
    cases.push({ c: cond.c, b: body, f: flags });
    if (body === "" && cond.c === "") break;
  }
  const noop = cases.length > 0 && cases.every((c) => c.b === "") && cases.some((c) => c.c === "");
  const pure = !noop && cases.length > 0 && cases.every((c) => c.c !== "" && c.b === "");
  const asyncDispatch = cases.some((c) => c.f & 2);
  if (pure) {
    let fused = cases.map((c) => c.c).join("||");
    if (cases.length > 1) fused = `(${fused})`;
    output = B_refine(
      B_refine(output, output.s, [{ c: () => fused, f: failInvalidType }], self)
    );
  } else if (!noop) {
    const dispatch = unionEmitChain(cases, ctx);
    if (asyncDispatch) {
      const itemVar = input.v();
      output.i = `(async(${itemVar})=>{${dispatch};return ${itemVar}})(${itemVar})`;
    } else {
      output.cp += dispatch;
    }
  }
  if (!asyncDispatch) output.i = input.i;
  let out;
  if (output.f & valFlagAsync) {
    output.i = `Promise.resolve(${output.i})`;
    output.v = _notVar;
    out = output;
  } else if (output.v === _var && input.cp === "" && output.cp === "" && !pure && initialInline === "i") {
    input.hd = "";
    input.v = _notVar;
    input.i = initialInline;
    out = input;
  } else {
    out = output;
  }
  const outputAnyOf = outputBySource.filter(Boolean);
  out.s = outputAnyOf.length ? unionFactory(outputAnyOf) : never_;
  if (toPerCase !== U) {
    out.io = true;
    out.e = unionOutput(toPerCase);
    return out;
  }
  out.e = self;
  return B_markOutput(out, input);
};
var unionDecoder = (input) => {
  const self = input.e;
  const toPerCase = self.parser === U && self.to !== U && self.to.noValidation !== true ? self.to : U;
  let variants = self.anyOf;
  if (
    // Already validated against this exact schema.
    input.io && input.e === input.s || input.s === self && toPerCase === U && variants.every(unionIsNoop) || input.s.type === anyOfTag && toPerCase === U && unionIsWider(variants, input.s.anyOf)
  ) {
    return input;
  }
  const initialTagFlag = tagFlags[input.s.type];
  if (initialTagFlag & tagFlagUnion || input.s.encoder === U && initialTagFlag & tagFlagRef) {
    input.s = unknown;
  }
  if (variants.every(unionNeverLink)) {
    B_invalidOperation(
      input,
      `Every variant of ${inputExpression(self)} is marked as never`
    );
  }
  const source = input.s;
  const nan2 = flagUnsafeHas(input.g.o, flagDisableNanNumberValidation) ? tagFlagNaN : 0;
  const normalized = unionNormalize(variants, source, nan2);
  if (!(normalized.t & tagFlagUnknown) && !(normalized.f & tagFlagUnknown)) {
    unionCheckPartial(input, source, self, variants, false);
  }
  if (toPerCase !== U) {
    const perCase = unionTargetOwns(toPerCase) ? variants.map((v) => unionOutput(v).type === neverTag ? U : toPerCase) : unionResolve(input, self, variants, toPerCase);
    const attach = self.refiner !== U || self.inputRefiner !== U ? unionRefinerAttacher(self) : U;
    variants = variants.map((variant, idx) => {
      const to = perCase[idx];
      return to === U && attach === U ? variant : updateOutput(variant, (mut) => {
        if (attach !== U) {
          attach(mut);
        }
        if (to !== U) {
          mut.to = to;
        }
      });
    });
  }
  const analyzed = unionAnalyze(normalized, variants, source, nan2);
  const plan = unionPlan(analyzed);
  return unionEmit(input, self, plan, toPerCase);
};
var unionRefinerAttacher = (self) => {
  const cached = [];
  return (mut) => {
    for (let i = 0; i < 2; i++) {
      const key = i ? "inputRefiner" : "refiner";
      const source = self[key];
      if (source !== U) {
        const current = mut[key];
        mut[key] = (input) => {
          const shared = cached[i] || (cached[i] = source(input));
          return current === U ? shared : current(input).concat(shared);
        };
      }
    }
  };
};
var unionRewrite = (input, map) => {
  const variants = input.s.anyOf;
  const anyOf = [];
  const has = {};
  for (let idx = 0; idx < variants.length; idx++) {
    const rewritten = map(variants[idx], idx);
    anyOf.push(rewritten);
    setHas(has, rewritten.type);
  }
  const mut = baseSchema(anyOfTag, false);
  mut.anyOf = anyOf;
  mut.has = has;
  mut.decoder = unionDecoder;
  mut.encoder = unionEncoder;
  mut.perVariant = input.s.perVariant;
  return B_refine(input, unknown, U, mut);
};
var unionRewriteTo = (input, target) => unionRewrite(
  input,
  (variant) => unionOutput(variant).type === neverTag ? variant : updateOutput(variant, (mut) => {
    mut.to = target;
  })
);
var unionTargetOwns = (target) => target.noValidation === true || tagFlags[unionOutput(target).type] & tagFlagRef || target.type === anyOfTag && target.anyOf.some((v) => tagFlags[v.type] & tagFlagRef);
var unionEncoder = (input, target) => {
  if (unionTargetOwns(target)) {
    return input;
  }
  const variants = input.s.anyOf;
  if (target.perVariant && target.anyOf.length === variants.length) {
    const targets = target.anyOf;
    return targets.every((tv, idx) => tv === variants[idx]) ? input : unionRewrite(input, (_variant, idx) => targets[idx]);
  }
  const resolved = unionResolve(input, input.s, variants, target);
  if (resolved.every((to) => to === U)) {
    return input;
  }
  return unionRewrite(input, (variant, idx) => {
    const to = resolved[idx];
    return to === U ? variant : updateOutput(variant, (mut) => {
      mut.to = to;
    });
  });
};
var unionNullish = tagFlagNull | tagFlagUndefined;
var unionOpposite = (schema) => schema.type === undefinedTag ? nullTag : schema.type === nullTag ? undefinedTag : U;
var unionResolve = (input, source, variants, target) => {
  if (source.perVariant) {
    return variants.map(() => target);
  }
  if (unionIsTransparent(target)) {
    return unionResolveToUnion(input, source, variants, target);
  }
  if (!(tagFlags[target.type] & tagFlagUnknown) && !target.noValidation) {
    unionCheckPartial(input, source, target, variants, true);
  }
  return variants.map(
    (variant) => unionOutput(variant).type === neverTag ? U : target
  );
};
var unionResolveToUnion = (input, source, variants, target) => {
  const targets = target.anyOf;
  const matches = [];
  const covered = [];
  let sourceNullish = 0;
  for (let s2 = 0; s2 < variants.length; s2++) {
    const sourceVariant = variants[s2];
    const sourceOut = unionOutput(sourceVariant);
    const produces = sourceVariant.type !== neverTag && sourceOut.type !== neverTag;
    if (!produces) {
      continue;
    }
    if (sourceOut === target || target.anyOf !== U && sourceOut.anyOf === target.anyOf) {
      for (let t = 0; t < targets.length; t++) {
        covered[t] = true;
      }
      continue;
    }
    const sameTyped = targets.filter(
      (targetVariant, t) => targetVariant.type !== neverTag && !unionNeverLink(targetVariant) && unionSameType(sourceOut, targetVariant) && (covered[t] = true)
    );
    sourceNullish |= tagFlags[sourceOut.type] & unionNullish;
    if (sameTyped.length === 1) {
      matches[s2] = sameTyped[0];
    } else if (sameTyped.length > 1) {
      matches[s2] = tagFlags[sourceOut.type] & unionStructured && sameTyped.includes(sourceOut) ? sourceOut : unionFactory(sameTyped);
    }
    if (matches[s2] !== U) {
      continue;
    }
    const opposite = unionOpposite(sourceOut);
    if (opposite !== U) {
      matches[s2] = targets.find(
        (candidate) => candidate.type === opposite && !unionNeverLink(candidate) && unionOutput(candidate).type !== neverTag
      );
    }
    if (matches[s2] === U) {
      unionUncovered(input, source, target, sourceOut);
    }
  }
  for (let t = 0; t < targets.length; t++) {
    const targetVariant = targets[t];
    const opposite = unionOpposite(targetVariant);
    if (targetVariant.type !== neverTag && !unionNeverLink(targetVariant) && !covered[t] && (opposite === U || unionOutput(targetVariant).type === neverTag || !(sourceNullish & tagFlags[opposite]))) {
      unionUncovered(input, source, target, targetVariant);
    }
  }
  return matches.map(
    (matched, idx) => matched !== U && unionAddsNothing(matched, unionOutput(variants[idx])) ? U : matched
  );
};
var unionAddsNothing = (matched, sourceOut) => matched === sourceOut || unionIsNoop(matched) && matched.refiner === U && matched.inputRefiner === U && matched.noValidation === U && // A target const narrows the source; only a target that constrains nothing
// (or exactly the same value) is a pass-through.
(matched.const === U || unionLiteralEqual(matched.const, sourceOut.const)) && !(tagFlags[matched.type] & unionStructured) && unionSameType(matched, sourceOut);
var unionFactory = (schemas) => {
  if (schemas.length === 0) {
    return panic("S.union requires at least one item");
  } else if (schemas.length === 1) {
    return schemas[0];
  }
  const has = {};
  const anyOf = [];
  for (let idx = 0; idx < schemas.length; idx++) {
    const schema = schemas[idx];
    const nested = unionIsTransparent(schema) ? schema.anyOf : U;
    for (let j = 0; j < (nested === U ? 1 : nested.length); j++) {
      const member = nested === U ? schema : nested[j];
      anyOf.push(member);
      setHas(has, member.type);
    }
  }
  const mut = baseSchema(anyOfTag, false);
  mut.anyOf = anyOf;
  mut.decoder = unionDecoder;
  mut.encoder = unionEncoder;
  mut.has = has;
  return mut;
};

// src/composites.ts
var isItemSchema = (x) => x !== U && typeof x !== "string";
var makeObjectVal = (prev, schema) => {
  return {
    b: U,
    p: U,
    v: _notVar,
    i: "",
    s: schema.type === arrayTag ? {
      type: arrayTag,
      items: [],
      additionalItems: "strict",
      decoder: arrayDecoder
    } : {
      type: objectTag,
      required: [],
      properties: /* @__PURE__ */ Object.create(null),
      additionalItems: "strict",
      decoder: objectDecoder
    },
    io: U,
    e: prev.e,
    prev,
    f: valFlagNone,
    d: /* @__PURE__ */ Object.create(null),
    fv: U,
    cp: "",
    hd: "",
    fz: U,
    vc: U,
    u: U,
    t: true,
    path: prev.path,
    g: prev.g,
    o: U
  };
};
var completeObjectVal = (objectVal) => {
  const isArray = objectVal.s.type === arrayTag;
  let inline = "";
  let promiseAllContent = "";
  let optionalSettingCode = U;
  const keys = Object.keys(objectVal.d);
  for (let idx = 0; idx < keys.length; idx++) {
    const key = keys[idx];
    const val = objectVal.d[key];
    if (flagUnsafeHas(val.f, valFlagAsync)) {
      promiseAllContent = promiseAllContent + val.i + ",";
    }
    if (val.o) {
      const existingFn = optionalSettingCode;
      optionalSettingCode = (objectVar) => {
        return (existingFn === U ? "" : existingFn(objectVar)) + `if(${val.v()}!==void 0){${objectVar}[${inlinedValueFromString(key)}]=${val.i}}`;
      };
    } else {
      inline = inline + (isArray ? `${val.i}` : `${inlinedValueFromString(key)}:${val.i}`) + ",";
    }
  }
  objectVal.i = isArray ? "[" + inline + "]" : "{" + inline + "}";
  const valWithRequired = objectVal;
  if (promiseAllContent) {
    const operationInput = B_scope(valWithRequired);
    operationInput.io = true;
    const operationOutput = parse(operationInput);
    const operationCode = B_merge(operationOutput);
    if (operationCode === "" && promiseAllContent === `${operationOutput.i},`) {
      valWithRequired.i = operationOutput.i;
    } else {
      valWithRequired.i = `Promise.all([${promiseAllContent}]).then(([${promiseAllContent}])=>{${operationCode}return ${operationOutput.i}})`;
    }
    valWithRequired.f |= valFlagAsync;
    valWithRequired.s = operationOutput.s;
    valWithRequired.e = operationOutput.e;
    valWithRequired.io = true;
    return valWithRequired;
  } else {
    if (optionalSettingCode === U) {
      return valWithRequired;
    } else {
      const code = optionalSettingCode(valWithRequired.v());
      const output = B_refine(valWithRequired);
      output.cp = output.cp + code;
      return output;
    }
  }
};
var array = /* @__NO_SIDE_EFFECTS__ */ (item) => {
  const itemInternal = item;
  const mut = baseSchema(arrayTag, itemInternal.r === itemInternal);
  mut.additionalItems = itemInternal;
  mut.items = immutableEmptyArray;
  mut.decoder = arrayDecoder;
  return mut;
};
var arrayDecoder = (unknownInput) => {
  const isUnion = unknownInput.u;
  const expectedSchema = unknownInput.e;
  const unknownInputTagFlag = tagFlags[unknownInput.s.type];
  const expectedItems = expectedSchema.items;
  const expectedLength = expectedItems.length;
  let input;
  if (flagUnsafeHas(unknownInputTagFlag, tagFlagUnknown | tagFlagArray)) {
    const isArrayInput = flagUnsafeHas(unknownInputTagFlag, tagFlagArray);
    let schema;
    if (!isArrayInput) {
      schema = /* @__PURE__ */ array(unknown);
    } else {
      schema = unknownInput.s;
    }
    const checks = [];
    if (!isArrayInput) {
      checks.push({
        c: isArrayCond,
        f: failInvalidType
      });
    }
    const schemaAdditionalItems = schema.additionalItems;
    const isExactSize = isItemSchema(schemaAdditionalItems) ? false : schema.items.length === expectedLength;
    if (!isExactSize) {
      const expectedAdditionalItems2 = expectedSchema.additionalItems;
      if (expectedAdditionalItems2 === "strict") {
        checks.push({
          c: (inputVar) => `${inputVar}.length===${expectedLength}`,
          f: failInvalidType
        });
      } else if (expectedAdditionalItems2 === "strip") {
        checks.push({
          c: (inputVar) => `${inputVar}.length>=${expectedLength}`,
          f: failInvalidType
        });
      }
    }
    if (checks.length > 0) {
      input = B_refine(unknownInput, schema, checks);
    } else {
      input = B_refine(unknownInput, schema);
    }
  } else {
    input = B_unsupportedDecode(unknownInput, unknownInput.s, expectedSchema);
  }
  let output;
  const expectedAdditionalItems = expectedSchema.additionalItems;
  if (isItemSchema(expectedAdditionalItems)) {
    const itemSchema = expectedAdditionalItems;
    if (itemSchema === unknown) {
      output = input;
    } else {
      const inputVar = input.v();
      const iteratorVar = B_varWithoutAllocation(input.g);
      const itemInput = B_dynamicScope(input, iteratorVar);
      const itemOutput = parseDynamic(itemInput);
      const hasTransform = itemOutput.t;
      const output2 = hasTransform ? (
        // The next `.to` segment decodes from this schema — item-output, not expectedSchema (#284)
        B_next(input, `new Array(${inputVar}.length)`, /* @__PURE__ */ array(itemOutput.s))
      ) : B_refine(input, expectedSchema);
      const itemCode = B_mergeWithPathPrepend(
        itemOutput,
        input,
        iteratorVar,
        hasTransform ? () => B_addKey(output2, iteratorVar, itemOutput) : U
      );
      if (hasTransform || itemCode !== "") {
        output2.cp = output2.cp + `for(let ${iteratorVar}=${expectedLength};${iteratorVar}<${inputVar}.length;++${iteratorVar}){${itemCode}}`;
      }
      if (flagUnsafeHas(itemOutput.f, valFlagAsync)) {
        output = B_asyncVal(output2, `Promise.all(${output2.i})`);
      } else {
        output = output2;
      }
    }
  } else {
    const objectVal = makeObjectVal(input, expectedSchema);
    let shouldRecreateInput;
    {
      const ai = expectedSchema.additionalItems;
      if (ai === "strict") {
        shouldRecreateInput = false;
      } else if (ai === "strip") {
        const inputAi = input.s.additionalItems;
        shouldRecreateInput = isItemSchema(inputAi) ? true : input.s.items.length !== expectedLength;
      } else {
        shouldRecreateInput = true;
      }
    }
    for (let idx = 0; idx < expectedLength; idx++) {
      const schema = expectedItems[idx];
      const key = String(idx);
      const itemInput = valGet(input, key);
      itemInput.e = schema;
      itemInput.io = false;
      itemInput.u = isUnion;
      const itemOutput = parse(itemInput);
      if (isUnion && isLiteral(schema)) {
        B_hoistChildChecks(input, itemOutput, key);
      }
      B_addObjectField(objectVal, key, itemOutput);
      if (!shouldRecreateInput) {
        shouldRecreateInput = itemOutput.t;
      }
    }
    if (shouldRecreateInput) {
      output = completeObjectVal(objectVal);
    } else {
      const o = B_refine(input, expectedSchema);
      o.cp = objectVal.cp;
      o.d = objectVal.d;
      output = o;
    }
  }
  return B_markOutput(output, input);
};
var objectDecoder = (unknownInput) => {
  const isUnion = unknownInput.u;
  const expectedSchema = unknownInput.e;
  const unknownInputTagFlag = tagFlags[unknownInput.s.type];
  let input;
  if (flagUnsafeHas(unknownInputTagFlag, tagFlagUnknown | tagFlagObject)) {
    const isObjectInput = flagUnsafeHas(unknownInputTagFlag, tagFlagObject);
    let schema;
    if (!isObjectInput) {
      const mut = baseSchema(objectTag, false);
      mut.properties = immutableEmptyObject;
      mut.additionalItems = unknown;
      schema = mut;
    } else {
      schema = unknownInput.s;
    }
    const checks = [];
    if (!isObjectInput) {
      checks.push({
        c: objectTagCond,
        f: failInvalidType
      });
      checks.push({
        c: (inputVar) => `!${isArrayCond(inputVar)}`,
        f: failInvalidType
      });
    }
    if (checks.length > 0) {
      input = B_refine(unknownInput, schema, checks);
    } else {
      input = B_refine(unknownInput, schema);
    }
  } else {
    input = B_unsupportedDecode(unknownInput, unknownInput.s, expectedSchema);
  }
  const expectedAdditionalItems = expectedSchema.additionalItems;
  const dictItem = isItemSchema(expectedAdditionalItems) ? expectedAdditionalItems : U;
  const inputAdditionalItems = input.s.additionalItems;
  const sourceIsDict = isItemSchema(inputAdditionalItems);
  let output;
  if (dictItem !== U && dictItem === unknown) {
    output = input;
  } else if (dictItem !== U && sourceIsDict) {
    const inputVar = input.v();
    const keyVar = B_varWithoutAllocation(input.g);
    const itemInput = B_dynamicScope(input, keyVar);
    const itemOutput = parseDynamic(itemInput);
    const hasTransform = itemOutput.t;
    const output2 = hasTransform ? (
      // The next `.to` segment decodes from this schema — item-output, not expectedSchema (#284)
      B_next(input, "{}", /* @__PURE__ */ dictFactory(itemOutput.s))
    ) : B_refine(input, expectedSchema);
    const itemCode = B_mergeWithPathPrepend(
      itemOutput,
      input,
      keyVar,
      hasTransform ? () => B_addKey(output2, keyVar, itemOutput) : U
    );
    if (hasTransform || itemCode !== "") {
      output2.cp = output2.cp + `for(let ${keyVar} in ${inputVar}){${itemCode}}`;
    }
    if (flagUnsafeHas(itemOutput.f, valFlagAsync)) {
      const resolveVar = B_varWithoutAllocation(output2.g);
      const rejectVar = B_varWithoutAllocation(output2.g);
      const asyncParseResultVar = B_varWithoutAllocation(output2.g);
      const counterVar = B_varWithoutAllocation(output2.g);
      const outputVar = output2.v();
      output = B_asyncVal(
        output2,
        `new Promise((${resolveVar},${rejectVar})=>{let ${counterVar}=Object.keys(${outputVar}).length;for(let ${keyVar} in ${outputVar}){${outputVar}[${keyVar}].then(${asyncParseResultVar}=>{${outputVar}[${keyVar}]=${asyncParseResultVar};if(${counterVar}--===1){${resolveVar}(${outputVar})}},${rejectVar})}})`
      );
    } else {
      output = output2;
    }
  } else if (dictItem !== U) {
    const itemSchema = dictItem;
    const objectVal = makeObjectVal(input, expectedSchema);
    const keys = Object.keys(input.s.properties);
    for (let idx = 0; idx < keys.length; idx++) {
      const key = keys[idx];
      const itemInput = valGet(input, key);
      itemInput.e = itemSchema;
      itemInput.io = false;
      itemInput.u = isUnion;
      B_addObjectField(objectVal, key, parse(itemInput));
    }
    output = completeObjectVal(objectVal);
  } else {
    const properties = expectedSchema.properties;
    const keys = Object.keys(properties);
    const keysCount = keys.length;
    const objectVal = makeObjectVal(input, expectedSchema);
    let shouldRecreateInput;
    {
      const ai = expectedSchema.additionalItems;
      if (ai === "strict") {
        shouldRecreateInput = false;
      } else if (ai === "strip") {
        shouldRecreateInput = sourceIsDict || Object.keys(input.s.properties).length !== keysCount;
      } else {
        shouldRecreateInput = true;
      }
    }
    const isJsonParent = isItemSchema(inputAdditionalItems) ? inputAdditionalItems.name === jsonName : false;
    for (let idx = 0; idx < keysCount; idx++) {
      const key = keys[idx];
      const schema = properties[key];
      const itemInput = valGet(input, key);
      itemInput.e = schema;
      itemInput.io = false;
      itemInput.u = isUnion;
      if (isJsonParent && schema.type === anyOfTag && schema.has[undefinedTag]) {
        itemInput.i = `(${itemInput.i}??null)`;
      }
      const itemOutput = parse(itemInput);
      if (isUnion && isLiteral(schema)) {
        B_hoistChildChecks(input, itemOutput, key);
      }
      B_addObjectField(objectVal, key, itemOutput);
      if (!shouldRecreateInput) {
        shouldRecreateInput = itemOutput.t;
      }
    }
    if (expectedSchema.additionalItems === "strict" && isItemSchema(inputAdditionalItems)) {
      const keyVar = B_varWithoutAllocation(objectVal.g);
      B_hoistDecl(input, keyVar);
      objectVal.cp = objectVal.cp + `for(${keyVar} in ${input.v()}){if(`;
      if (keys.length === 0) {
        objectVal.cp = objectVal.cp + "true";
      } else {
        for (let idx = 0; idx < keys.length; idx++) {
          const key = keys[idx];
          if (idx !== 0) {
            objectVal.cp = objectVal.cp + "&&";
          }
          objectVal.cp = objectVal.cp + `${keyVar}!==${inlinedValueFromString(key)}`;
        }
      }
      objectVal.cp = objectVal.cp + `){${B_failWithArg(
        input,
        (excessFieldName) => ({
          code: "unrecognized_keys",
          path: objectVal.path,
          reason: `Unrecognized key "${excessFieldName}"`,
          keys: [excessFieldName]
        }),
        keyVar
      )}}}`;
    }
    if (shouldRecreateInput) {
      output = completeObjectVal(objectVal);
    } else {
      const o = B_refine(input, expectedSchema);
      o.cp = objectVal.cp;
      o.d = objectVal.d;
      output = o;
    }
  }
  return B_markOutput(output, input);
};
var dictFactory = /* @__NO_SIDE_EFFECTS__ */ (item) => {
  const mut = baseSchema(objectTag, item.r === item);
  mut.properties = immutableEmptyObject;
  mut.additionalItems = item;
  mut.decoder = objectDecoder;
  return mut;
};
var nestedNone = () => {
  const itemSchema = Literal_parse(0);
  const properties = {};
  properties[nestedLoc] = itemSchema;
  return {
    type: objectTag,
    required: [nestedLoc],
    properties,
    additionalItems: "strip",
    decoder: objectDecoder,
    // TODO: Support this as a default coercion
    serializer: (input) => {
      const nextSchema = input.e.to;
      return B_nextConst(input, nextSchema, nextSchema);
    }
  };
};
var nestedOption = (item) => {
  return updateOutput(item, (mut) => {
    mut.to = nestedNone();
    mut.parser = nestedOptionParser;
  });
};
var optionFactory = (item, unitSchema = unit) => {
  const out = getOutputSchema(item);
  if (out.type === undefinedTag) {
    return unionFactory([unitSchema, nestedOption(item)]);
  } else if (out.type === anyOfTag) {
    const anyOf = out.anyOf;
    const has = out.has;
    return updateOutput(item, (mut) => {
      const schemas = anyOf;
      const mutHas = { ...has };
      const newAnyOf = [];
      for (let idx = 0; idx < schemas.length; idx++) {
        const schema = schemas[idx];
        let toPush;
        const schemaOut = getOutputSchema(schema);
        if (schemaOut.type === undefinedTag) {
          mutHas[unitSchema.type] = true;
          newAnyOf.push(unitSchema);
          toPush = nestedOption(schema);
        } else if (schemaOut.properties !== U) {
          const properties = schemaOut.properties;
          const nestedSchema = properties[nestedLoc];
          if (nestedSchema !== U) {
            toPush = updateOutput(schema, (mut2) => {
              const properties2 = {};
              properties2[nestedLoc] = {
                ...nestedSchema,
                const: nestedSchema.const + 1
              };
              mut2.properties = properties2;
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
};
var option = /* @__NO_SIDE_EFFECTS__ */ (item) => {
  return optionFactory(item, unit);
};
var valGet = (parent, location) => {
  let vals;
  if (parent.d !== U) {
    vals = parent.d;
  } else {
    const d = /* @__PURE__ */ Object.create(null);
    parent.d = d;
    vals = d;
  }
  const existing = vals[location];
  if (existing !== U) {
    return B_scope(existing);
  } else {
    let locationSchema;
    if (parent.s.type === objectTag) {
      locationSchema = parent.s.properties[location];
    } else {
      locationSchema = parent.s.items[Number(location)];
    }
    let schema;
    if (locationSchema !== U) {
      schema = locationSchema;
    } else {
      const additionalItems = parent.s.additionalItems;
      if (isItemSchema(additionalItems)) {
        const s2 = additionalItems;
        if (parent.s.type === objectTag && s2.type !== unknownTag && !flagUnsafeHas(tagFlags[s2.type], tagFlagRef) && !isOptional(s2)) {
          schema = /* @__PURE__ */ option(s2);
          schema.perVariant = true;
        } else {
          schema = s2;
        }
      } else {
        schema = B_unsupportedDecode(parent, parent.s, parent.e);
      }
    }
    const pathAppend = pathFromInlinedLocation(inlinedValueFromString(location));
    const item = {
      b: U,
      p: parent,
      v: _notVarAtParent,
      i: isLiteral(schema) ? B_inlineConst(parent, schema) : `${parent.v()}${pathAppend}`,
      s: schema,
      io: U,
      e: schema,
      prev: U,
      f: valFlagNone,
      d: U,
      fv: U,
      cp: "",
      hd: "",
      fz: U,
      vc: U,
      u: U,
      t: U,
      path: pathConcat(parent.path, pathAppend),
      g: parent.g,
      o: U
    };
    vals[location] = item;
    return item;
  }
};

// src/advanced/recursive.ts
var recursiveDecoder = (input) => {
  const expectedSchema = input.e;
  const schemaRef = expectedSchema["$ref"];
  const defs = input.g.d;
  const identifier = schemaRef.slice(8);
  const def = defs[identifier];
  const flag = input.g.o;
  const inputSchema = input.s.seq === expectedSchema.seq ? def : input.s;
  const key = `${inputSchema.seq}-${def.seq}--${flag}`;
  let recOperation = "";
  const fn = def[key];
  if (fn !== U) {
    recOperation = fn === 0 ? B_embed(input, def) + `["${key}"]` : B_embed(input, fn);
  } else {
    let assumedHasTransform = def.hasTransform !== U ? def.hasTransform : false;
    let assumedIsAsync = def.isAsync !== U ? def.isAsync : false;
    let compileNeeded = true;
    let finalFn = 0;
    while (compileNeeded) {
      compileNeeded = false;
      if (def.hasTransform === U) {
        def.hasTransform = assumedHasTransform;
      }
      if (def.isAsync === U) {
        def.isAsync = assumedIsAsync;
      }
      configurableValueOptions[valKey] = 0;
      Object.defineProperty(def, key, configurableValueOptions);
      const fn2 = compileDecoder(inputSchema, def, flag, defs);
      valueOptions[valKey] = fn2;
      Object.defineProperty(def, key, valueOptions);
      finalFn = fn2;
      const actualHasTransform = def.hasTransform;
      const actualIsAsync = def.isAsync;
      if (actualHasTransform !== assumedHasTransform || actualIsAsync !== assumedIsAsync) {
        assumedHasTransform = actualHasTransform;
        assumedIsAsync = actualIsAsync;
        delete def[key];
        compileNeeded = true;
      }
    }
    recOperation = B_embed(input, finalFn);
  }
  const hasTransform = def.hasTransform === true;
  const isAsync = def.isAsync;
  let outputDecl = "";
  let output;
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
    output = B_refine(input, expectedSchema, U, expectedSchema);
    output.cp = `${recOperation}(${input.i});`;
  }
  output.prev = U;
  output.cp = outputDecl + B_mergeWithPathPrepend(output, input);
  output.fz = U;
  output.prev = input;
  return output;
};
var recursive = /* @__NO_SIDE_EFFECTS__ */ (name, fn) => {
  const ref = `${defsPath}${name}`;
  const refSchema = baseSchema(refTag, false);
  refSchema["$ref"] = ref;
  refSchema.name = name;
  refSchema.decoder = recursiveDecoder;
  const isNestedRec = globalConfig.d !== U;
  if (!isNestedRec) {
    globalConfig.d = {};
  }
  const def = fn(refSchema);
  if (def.name) {
    refSchema.name = def.name;
  }
  globalConfig.d[name] = def;
  if (isNestedRec) {
    return refSchema;
  } else {
    const schema = baseSchema(refTag, false);
    schema.name = refSchema.name;
    schema["$ref"] = ref;
    schema["$defs"] = globalConfig.d;
    schema.decoder = recursiveDecoder;
    globalConfig.d = U;
    return schema;
  }
};

// src/advanced/json.ts
var jsonEncoderFn = (input, target) => {
  const toTagFlag = tagFlags[target.type];
  if (flagUnsafeHas(
    toTagFlag,
    tagFlagString | tagFlagBoolean | tagFlagNumber | tagFlagNull
  )) {
    return parse(B_refine(input, unknown, U, target));
  } else if (flagUnsafeHas(toTagFlag, tagFlagUndefined | tagFlagNaN)) {
    const jsonExpected = copySchema(nullLiteral);
    jsonExpected.to = target;
    return parse(B_refine(input, unknown, U, jsonExpected));
  } else if (flagUnsafeHas(toTagFlag, tagFlagArray)) {
    const jsonExpected = array(unknown);
    const output = parse(B_refine(input, unknown, U, jsonExpected));
    output.s.additionalItems = json;
    output.e = target;
    output.io = false;
    return output;
  } else if (flagUnsafeHas(toTagFlag, tagFlagObject)) {
    const jsonExpected = dictFactory(unknown);
    const output = parse(B_refine(input, unknown, U, jsonExpected));
    output.s.additionalItems = json;
    output.e = target;
    output.io = false;
    return output;
  } else if (flagUnsafeHas(toTagFlag, tagFlagUnion | tagFlagRef)) {
    return input;
  } else {
    const jsonExpected = copySchema(string);
    jsonExpected.to = target;
    return parse(B_refine(input, unknown, U, jsonExpected));
  }
};
var isJsonable = (schema) => {
  const tagFlag = tagFlags[schema.type];
  return flagUnsafeHas(
    tagFlag,
    tagFlagString | tagFlagNumber | tagFlagBoolean | tagFlagNull
  ) || schema["$ref"] === json["$ref"] || flagUnsafeHas(tagFlag, tagFlagUnion) && schema.anyOf.every(isJsonable) || flagUnsafeHas(tagFlag, tagFlagArray) && (typeof schema.additionalItems === "object" ? isJsonable(schema.additionalItems) : true) && schema.items.every(isJsonable) || flagUnsafeHas(tagFlag, tagFlagObject) && (typeof schema.additionalItems === "object" ? isJsonable(schema.additionalItems) : true) && Object.values(schema.properties).every(isJsonable);
};
var jsonDecoderFn = (input) => {
  const inputTagFlag = tagFlags[input.s.type];
  if (isJsonable(input.s)) {
    return input;
  } else if (flagUnsafeHas(inputTagFlag, tagFlagUndefined | tagFlagNaN)) {
    return B_nextConst(input, nullLiteral);
  } else if (flagUnsafeHas(inputTagFlag, tagFlagArray)) {
    const expected = baseSchema(arrayTag, false);
    expected.items = input.s.items.map((_) => json);
    expected.decoder = arrayDecoder;
    expected.additionalItems = typeof input.s.additionalItems === "object" ? json : input.s.additionalItems;
    expected.to = input.e.to;
    return parse(B_refine(input, U, U, expected));
  } else if (flagUnsafeHas(inputTagFlag, tagFlagObject)) {
    if (typeof input.s.additionalItems === "object") {
      const expected = dictFactory(json);
      expected.to = input.e.to;
      return parse(B_refine(input, U, U, expected));
    } else {
      const jsonVal = makeObjectVal(input, input.s);
      jsonVal.e = json;
      if (input.e.to) {
        jsonVal.e = copySchema(jsonVal.e);
        jsonVal.e.to = input.e.to;
      }
      const keys = Object.keys(input.s.properties);
      for (let idx = 0; idx <= keys.length - 1; idx++) {
        const key = keys[idx];
        const itemVal = valGet(input, key);
        itemVal.io = false;
        if (itemVal.s.type === anyOfTag && itemVal.s.has[undefinedTag]) {
          const mapped = unionFactory(
            itemVal.s.anyOf.map((variant) => {
              const variantOutput = getOutputSchema(variant);
              return variantOutput.type === undefinedTag || isJsonable(variantOutput) ? variant : updateOutput(variant, (mut) => {
                mut.to = json;
              });
            })
          );
          mapped.perVariant = true;
          itemVal.e = mapped;
          const itemOutput = parse(itemVal);
          itemOutput.o = true;
          B_addObjectField(jsonVal, key, itemOutput);
        } else {
          itemVal.e = json;
          B_addObjectField(jsonVal, key, parse(itemVal));
        }
      }
      return completeObjectVal(jsonVal);
    }
  } else if (flagUnsafeHas(inputTagFlag, tagFlagRef)) {
    return recursiveDecoder(input);
  } else if (flagUnsafeHas(inputTagFlag, tagFlagUnion) && // Union-tagged schemas always carry `anyOf` and `has`
  // (set by unionFactory, reverse and the S.json def).
  // Unions with an undefined variant are not supported,
  // since undefined is not representable in JSON
  !(undefinedTag in input.s.has)) {
    return parse(unionRewriteTo(input, input.e));
  } else if (flagUnsafeHas(inputTagFlag, tagFlagUnknown)) {
    const to = input.e.to;
    const preEncode = !!to && !input.e.parser;
    if (preEncode) {
      input.s = json;
      return jsonEncoderFn(input, input.e);
    } else if (input.e.noValidation) {
      input.s = json;
      return input;
    } else {
      return recursiveDecoder(input);
    }
  } else {
    try {
      const expected = copySchema(string);
      expected.to = input.e;
      input.e = expected;
      return parse(input);
    } catch {
      return B_unsupportedDecode(input, input.s, json);
    }
  }
};
var json = /* @__PURE__ */ initSchema(refTag, (s2) => {
  const jsonRef = baseSchema(refTag, true);
  jsonRef["$ref"] = `${defsPath}${jsonName}`;
  jsonRef.name = jsonName;
  jsonRef.decoder = jsonDecoderFn;
  jsonRef.encoder = jsonEncoderFn;
  s2["$ref"] = jsonRef["$ref"];
  s2.name = jsonName;
  s2.decoder = jsonDecoderFn;
  s2.encoder = jsonEncoderFn;
  const anyOf = [
    string,
    bool,
    float,
    nullLiteral,
    dictFactory(jsonRef),
    array(jsonRef)
  ];
  const has = {};
  anyOf.forEach((schema) => {
    has[schema.type] = true;
  });
  const jsonDef = baseSchema(anyOfTag, true);
  jsonDef.anyOf = anyOf;
  jsonDef.has = has;
  jsonDef.decoder = unionDecoder;
  jsonDef.name = jsonName;
  jsonDef.type = anyOfTag;
  const defs = {};
  defs[jsonName] = jsonDef;
  s2["$defs"] = defs;
});
var jsonString = /* @__PURE__ */ (() => {
  const inlineJsonString = (input, schema) => {
    const tagFlag = tagFlags[schema.type];
    const const_ = schema.const;
    if (flagUnsafeHas(tagFlag, tagFlagUndefined | tagFlagNull)) {
      return `"null"`;
    } else if (flagUnsafeHas(tagFlag, tagFlagString)) {
      return JSON.stringify(inlinedValueFromString(const_));
    } else if (flagUnsafeHas(tagFlag, tagFlagBigint)) {
      return `"\\"${const_}\\""`;
    } else if (flagUnsafeHas(tagFlag, tagFlagNumber | tagFlagBoolean)) {
      return `"${const_}"`;
    } else {
      return B_unsupportedDecode(input, schema, input.e);
    }
  };
  const constSchemaToJsonStringConst = (input, target) => {
    const tagFlag = tagFlags[target.type];
    const const_ = target.const;
    if (flagUnsafeHas(tagFlag, tagFlagUndefined | tagFlagNull)) {
      return `null`;
    } else if (flagUnsafeHas(tagFlag, tagFlagString)) {
      return inlinedValueFromString(const_);
    } else if (flagUnsafeHas(tagFlag, tagFlagBigint)) {
      return `"${const_}"`;
    } else if (flagUnsafeHas(tagFlag, tagFlagNumber | tagFlagBoolean)) {
      return "" + const_;
    } else {
      return B_unsupportedDecode(input, input.s, target);
    }
  };
  const jsonStringEncoder = (input, target) => {
    if (target.format !== "json") {
      if (isLiteral(target)) {
        const jsonStringConstSchema = baseSchema(stringTag, true);
        jsonStringConstSchema.const = constSchemaToJsonStringConst(input, target);
        jsonStringConstSchema.to = target;
        jsonStringConstSchema.decoder = literalDecoder;
        return B_refine(input, U, U, jsonStringConstSchema);
      } else {
        const outputVar = B_varWithoutAllocation(input.g);
        const nextSchema = copySchema(json);
        nextSchema.to = target;
        const output = B_next(input, outputVar, nextSchema, nextSchema);
        output.io = true;
        output.v = _var;
        const inputVar = input.v();
        output.cp = `let ${outputVar};try{${outputVar}=JSON.parse(${inputVar})}catch(t){${B_embedInvalidInput(
          input,
          input.s
        )}}`;
        return output;
      }
    } else {
      return input;
    }
  };
  const jsonStringDecoder = (input) => {
    const inputTagFlag = tagFlags[input.s.type];
    const expectedSchema = input.e;
    if (flagUnsafeHas(inputTagFlag, tagFlagUnknown)) {
      const to = expectedSchema.to;
      const preEncode = !!to && to.type !== unknownTag && !expectedSchema.parser && !expectedSchema.refiner;
      const stringVal = stringDecoderFn(input);
      stringVal.s = expectedSchema;
      stringVal.e = expectedSchema;
      if (preEncode) {
        return jsonStringEncoder(stringVal, to);
      } else {
        const stringVar = stringVal.v();
        const output = B_refine(stringVal, expectedSchema);
        output.cp = `try{JSON.parse(${stringVar})}catch(t){${B_embedInvalidInput(
          stringVal
        )}}`;
        return output;
      }
    } else if (input.s.format === "json") {
      return input;
    } else if (isLiteral(input.s)) {
      return B_next(input, inlineJsonString(input, input.s), expectedSchema);
    } else if (flagUnsafeHas(inputTagFlag, tagFlagString)) {
      return B_next(input, `JSON.stringify(${input.i})`, expectedSchema);
    } else if (flagUnsafeHas(inputTagFlag, tagFlagNumber | tagFlagBoolean)) {
      const output = inputToString(input);
      output.s = expectedSchema;
      return output;
    } else if (flagUnsafeHas(inputTagFlag, tagFlagBigint)) {
      return B_next(input, `"\\""+${input.i}+"\\""`, expectedSchema);
    } else if (flagUnsafeHas(inputTagFlag, tagFlagObject | tagFlagArray)) {
      const jsonVal = parse(B_refine(input, U, U, json));
      return B_next(
        jsonVal,
        `JSON.stringify(${jsonVal.i}${expectedSchema.space === 0 || expectedSchema.space === U ? "" : `,null,${expectedSchema.space}`})`,
        expectedSchema,
        expectedSchema
      );
    } else {
      return B_unsupportedDecode(input, input.s, expectedSchema);
    }
  };
  return initSchema(stringTag, (s2) => {
    s2.format = "json";
    s2.name = `${jsonName} string`;
    s2.encoder = jsonStringEncoder;
    s2.decoder = jsonStringDecoder;
  });
})();
var jsonStringWithSpace = /* @__NO_SIDE_EFFECTS__ */ (space) => {
  const mut = copySchema(jsonString);
  mut.space = space;
  return mut;
};

// src/advanced/uint8Array.ts
var uint8Array = /* @__PURE__ */ initSchema(instanceTag, (s2) => {
  s2.class = Uint8Array;
  s2.decoder = (inputArg) => {
    const inputTagFlag = tagFlags[inputArg.s.type];
    let input = inputArg;
    if (flagUnsafeHas(inputTagFlag, tagFlagString)) {
      input = B_next(
        input,
        `${B_embed(input, new TextEncoder())}.encode(${input.i})`,
        s2
      );
    } else if (flagUnsafeHas(inputTagFlag, tagFlagUnknown | tagFlagInstance)) {
      input = instanceDecoder(input);
    }
    if (inputArg.e.to !== U && inputArg.e.parser === U && flagUnsafeHas(tagFlags[inputArg.e.to.type], tagFlagString)) {
      input = B_next(
        input,
        `${B_embed(input, new TextDecoder())}.decode(${input.i})`,
        string
      );
    }
    return input;
  };
});

// src/advanced/date.ts
var invalidDateRefine = (input) => {
  return B_refine(input, input.e, [
    {
      c: (inputVar) => `!Number.isNaN(${inputVar}.getTime())`,
      f: failInvalidType
    }
  ]);
};
var date = /* @__PURE__ */ initSchema(instanceTag, (s2) => {
  s2.class = Date;
  s2.decoder = (input) => {
    const inputTagFlag = tagFlags[input.s.type];
    if (flagUnsafeHas(inputTagFlag, tagFlagString)) {
      return invalidDateRefine(B_next(input, `new Date(${input.i})`, s2));
    } else if (flagUnsafeHas(inputTagFlag, tagFlagUnknown)) {
      return invalidDateRefine(instanceDecoder(input));
    } else if (flagUnsafeHas(inputTagFlag, tagFlagInstance) && input.s.class === s2.class) {
      return input;
    } else {
      return B_unsupportedDecode(input, input.s, input.e);
    }
  };
  s2.encoder = (input, target) => {
    const toTagFlag = tagFlags[target.type];
    if (flagUnsafeHas(toTagFlag, tagFlagString)) {
      const dateTimeString = baseSchema(stringTag, false);
      dateTimeString.format = "date-time";
      return parse(
        B_next(input, `${input.i}.toISOString()`, dateTimeString, target)
      );
    } else {
      return input;
    }
  };
});

// src/modifiers.ts
var Metadata_Id_make = /* @__NO_SIDE_EFFECTS__ */ (namespace, name) => {
  return `m:${namespace}:${name}`;
};
var Metadata_Id_internal = (name) => {
  return `m:${name}`;
};
var Metadata_get = /* @__NO_SIDE_EFFECTS__ */ (schema, id) => {
  return schema[id];
};
var Metadata_setInPlace = (schema, id, metadata) => {
  schema[id] = metadata;
};
var Metadata_set = /* @__NO_SIDE_EFFECTS__ */ (schema, id, metadata) => {
  const mut = copySchema(schema);
  Metadata_setInPlace(mut, id, metadata);
  return mut;
};
var noValidation = /* @__NO_SIDE_EFFECTS__ */ (schema, value) => {
  const mut = copySchema(schema);
  mut.noValidation = value;
  return mut;
};
var internalRefine = (schema, makeRefiner) => {
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
};
var refine = /* @__NO_SIDE_EFFECTS__ */ (schema, refineCheck, error, path) => {
  const message = error !== U ? error : "Refinement failed";
  const extraPath = path !== U ? pathFromArray(path) : pathEmpty;
  return internalRefine(schema, (_) => (input) => {
    const embeddedCheck = B_embed(input, refineCheck);
    return [
      {
        c: (inputVar) => `${embeddedCheck}(${inputVar})`,
        f: B_invalidInputBuilder(U, extraPath, message)
      }
    ];
  });
};
var refineInput = (schema, refineCheck, error) => {
  const message = error !== U ? error : "Refinement failed";
  return updateOutput(schema, (mut) => {
    const refiner = (input) => {
      const embeddedCheck = B_embed(input, refineCheck);
      return [
        {
          c: (inputVar) => `${embeddedCheck}(${inputVar})`,
          f: B_invalidInputBuilder(U, pathEmpty, message)
        }
      ];
    };
    const existing = mut.inputRefiner;
    mut.inputRefiner = existing !== U ? (input) => {
      const arr = existing(input);
      arr.push(...refiner(input));
      return arr;
    } : refiner;
  });
};
var getMutErrorMessage = (mut) => {
  const em = mut.errorMessage ? { ...mut.errorMessage } : {};
  mut.errorMessage = em;
  return em;
};
var codecTo = (schema, target, parserB, serializerB) => {
  const root = updateOutput(schema, (mut) => {
    if (serializerB !== U) {
      const targetMut = copySchema(target);
      targetMut.serializer = serializerB;
      mut.to = targetMut;
    } else {
      mut.to = target;
    }
    if (parserB !== U) {
      mut.parser = parserB;
    }
  });
  if (parserB !== U || serializerB !== U) {
    delete root.isAsync;
    delete root.hasTransform;
  }
  return root;
};
var nullAsUnit = /* @__PURE__ */ (() => {
  const schema = copySchema(nullLiteral);
  schema.to = unit;
  return schema;
})();
var Option_getWithDefault = (schema, default_) => {
  return updateOutput(schema, (mut) => {
    const anyOf = mut.anyOf;
    if (anyOf === U) {
      return panic(`Can't set default for ${inputExpression(mut)}`);
    }
    const outputItems = [];
    const originalItems = [];
    for (let idx = 0; idx < anyOf.length; idx++) {
      const variant = anyOf[idx];
      const outputSchema = getOutputSchema(variant);
      if (outputSchema.type !== undefinedTag) {
        if (!outputItems.includes(outputSchema)) {
          outputItems.push(outputSchema);
        }
        originalItems.push(variant);
      }
    }
    const item = outputItems.length === 0 ? panic(`Can't set default for ${inputExpression(mut)}`) : outputItems.length === 1 ? outputItems[0] : unionFactory(outputItems);
    if (default_.type === "value") {
      const v = default_.value;
      try {
        getDecoder(unknown, item)(v);
      } catch (exn) {
        const error = getOrRethrow(exn);
        panic(
          `Invalid default for ${inputExpression(mut)}: ${error["message"]}`
        );
      }
      const originalItem = originalItems.length === 1 ? originalItems[0] : unionFactory(originalItems);
      try {
        mut.default = getDecoder(reverse(originalItem))(v);
      } catch (_exn) {
      }
    }
    const decodeB = (input) => {
      const target = input.e.to;
      const output = B_next(
        input,
        default_.type === "value" ? B_inlineConst(input, Literal_parse(default_.value)) : `${B_embed(input, default_.callback)}()`,
        target,
        target
      );
      if (default_.type === "value") {
        output.v = _var;
      }
      return output;
    };
    mut.anyOf = anyOf.map(
      (variant) => getOutputSchema(variant).type === undefinedTag ? codecTo(variant, item, decodeB, B_neverSlot) : variant
    );
  });
};
var Option_getOr = /* @__NO_SIDE_EFFECTS__ */ (schema, defaultValue) => Option_getWithDefault(schema, { type: "value", value: defaultValue });
var Option_getOrWith = /* @__NO_SIDE_EFFECTS__ */ (schema, defaultCb) => Option_getWithDefault(schema, { type: "callback", callback: defaultCb });
var Object_setAdditionalItems = (schema, additionalItems, deep) => {
  const currentAdditionalItems = schema.additionalItems;
  if (currentAdditionalItems !== U && currentAdditionalItems !== additionalItems && typeof currentAdditionalItems !== objectTag) {
    const mut = copySchema(schema);
    mut.additionalItems = additionalItems;
    if (deep) {
      const items = schema.items;
      if (items !== U) {
        mut.items = items.map((s2) => Object_setAdditionalItems(s2, additionalItems, deep));
      }
      const properties = schema.properties;
      if (properties !== U) {
        mut.properties = Object.fromEntries(
          Object.keys(properties).map((key) => [
            key,
            Object_setAdditionalItems(properties[key], additionalItems, deep)
          ])
        );
      }
    }
    return mut;
  } else {
    return schema;
  }
};
var strip = /* @__NO_SIDE_EFFECTS__ */ (schema) => {
  return Object_setAdditionalItems(schema, "strip", false);
};
var deepStrip = /* @__NO_SIDE_EFFECTS__ */ (schema) => {
  return Object_setAdditionalItems(schema, "strip", true);
};
var strict = /* @__NO_SIDE_EFFECTS__ */ (schema) => {
  return Object_setAdditionalItems(schema, "strict", false);
};
var deepStrict = /* @__NO_SIDE_EFFECTS__ */ (schema) => {
  return Object_setAdditionalItems(schema, "strict", true);
};
var meta = /* @__NO_SIDE_EFFECTS__ */ (schema, data) => {
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
      try {
        mut.examples = data.examples.map(getDecoder(reverse(schema)));
      } catch (exn) {
        if (getOrRethrow(exn).code !== "invalid_operation") {
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
};
var brand = /* @__NO_SIDE_EFFECTS__ */ (schema, id) => {
  const mut = copySchema(schema);
  mut.name = id;
  return mut;
};

// src/factory.ts
var inputFrom = immutableEmptyArray;
var makeTag = (field) => (tag, asValue) => {
  field(tag, definitionToSchema(asValue));
};
var makeFieldOr = (field) => (fieldName, schema, or) => {
  return field(fieldName, Option_getOr(optionFactory(schema), or));
};
var proxifyShapedSchema = (schema, from, fromFlattened) => {
  const mut = copySchema(getOutputSchema(schema));
  mut.from = from;
  if (fromFlattened !== U) {
    mut.fromFlattened = fromFlattened;
  }
  return new Proxy(mut, {
    get(target, prop) {
      if (prop === itemSymbol) {
        return target;
      } else {
        const location = prop;
        let maybeField;
        if (target.properties !== U) {
          maybeField = target.properties[location];
        } else if (target.items !== U) {
          maybeField = target.items[location];
        } else {
          maybeField = U;
        }
        if (!maybeField) {
          panic(`Cannot read property "${location}" of ${inputExpression(target)}`);
        }
        return proxifyShapedSchema(
          maybeField,
          target.from.concat(location),
          target.fromFlattened
        );
      }
    }
  });
};
var schemaShape = /* @__NO_SIDE_EFFECTS__ */ (schema, definer) => {
  return updateOutput(schema, (mut) => {
    const fromProxy = proxifyShapedSchema(mut, inputFrom);
    const definition = definer(fromProxy);
    if (definition === fromProxy) {
    } else {
      mut.parser = shapedParser;
      mut.to = definitionToShapedSchema(definition);
    }
  });
};
function schemaNested(fieldName) {
  const parentCtx = this;
  const cacheId = `~${fieldName}`;
  const cachedCtx = parentCtx[cacheId];
  if (cachedCtx !== U) {
    return cachedCtx;
  } else {
    const properties = /* @__PURE__ */ Object.create(null);
    const required = [];
    let schema;
    {
      const s2 = baseSchema(objectTag, false);
      s2.required = required;
      s2.properties = properties;
      s2.additionalItems = globalConfig.a;
      s2.decoder = objectDecoder;
      schema = s2;
    }
    const parentSchema = parentCtx.f(fieldName, schema)[itemSymbol];
    const field = (fieldName2, schema2) => {
      const inlinedLocation = inlinedValueFromString(fieldName2);
      if (fieldName2 in properties) {
        panic(`The field ${inlinedLocation} defined twice`);
      }
      required.push(fieldName2);
      properties[fieldName2] = schema2;
      return proxifyShapedSchema(
        schema2,
        parentSchema.from.concat(fieldName2),
        parentSchema.fromFlattened
      );
    };
    const tag = makeTag(field);
    const fieldOr = makeFieldOr(field);
    const flatten = (schema2) => {
      if (schema2.type === objectTag) {
        const flattenedProperties = schema2.properties;
        const to = schema2.to;
        if (to) {
          panic(
            `Unsupported nested flatten for transformed object schema ${inputExpression(schema2)}`
          );
        }
        const flattenedKeys = Object.keys(flattenedProperties);
        const result = {};
        for (let idx = 0; idx < flattenedKeys.length; idx++) {
          const key = flattenedKeys[idx];
          result[key] = field(key, flattenedProperties[key]);
        }
        return result;
      } else {
        return panic(`Can't flatten ${inputExpression(schema2)} schema`);
      }
    };
    const ctx = {
      // js/ts methods
      field,
      // methods
      f: field,
      fieldOr,
      tag,
      nested: schemaNested,
      flatten
    };
    parentCtx[cacheId] = ctx;
    return ctx;
  }
}
var schemaObject = /* @__NO_SIDE_EFFECTS__ */ (definer) => {
  if (typeof definer !== "function") {
    return definitionToSchema(definer);
  }
  let flattened = U;
  const properties = /* @__PURE__ */ Object.create(null);
  const flatten = (schema) => {
    if (schema.type === objectTag) {
      const flattenedProperties = schema.properties;
      const flattenedKeys = Object.keys(flattenedProperties);
      for (let idx = 0; idx < flattenedKeys.length; idx++) {
        const key = flattenedKeys[idx];
        const flattenedSchema = flattenedProperties[key];
        const existing = properties[key];
        if (existing !== U && existing === flattenedSchema) {
        } else if (existing !== U) {
          panic(`The field "${key}" defined twice with incompatible schemas`);
        } else {
          properties[key] = flattenedSchema;
        }
      }
      const f = flattened || (flattened = []);
      return proxifyShapedSchema(schema, inputFrom, f.push(schema) - 1);
    } else {
      return panic(`The '${inputExpression(schema)}' schema can't be flattened`);
    }
  };
  const field = (fieldName, schema) => {
    if (fieldName in properties) {
      panic(`The field "${fieldName}" defined twice with incompatible schemas`);
    }
    properties[fieldName] = schema;
    return proxifyShapedSchema(schema, [fieldName]);
  };
  const tag = makeTag(field);
  const fieldOr = makeFieldOr(field);
  const ctx = {
    // js/ts methods
    field,
    // methods
    f: field,
    fieldOr,
    tag,
    nested: schemaNested,
    flatten
  };
  const definition = definer(ctx);
  const mut = baseSchema(objectTag, false);
  mut.required = Object.keys(properties);
  mut.properties = properties;
  mut.additionalItems = globalConfig.a;
  mut.decoder = objectDecoder;
  mut.parser = shapedParser;
  mut.to = definitionToShapedSchema(definition);
  if (flattened !== U) {
    mut.flattened = flattened;
  }
  return mut;
};
var schemaTuple = /* @__NO_SIDE_EFFECTS__ */ (definer) => {
  if (typeof definer !== "function") {
    return definitionToSchema(definer);
  }
  const items = [];
  const item = (idx, schema) => {
    const location = String(idx);
    if (items[idx]) {
      return panic(`The item [${location}] is defined multiple times`);
    } else {
      items[idx] = schema;
      return proxifyShapedSchema(schema, [location]);
    }
  };
  const tag = (idx, asValue) => {
    item(idx, definitionToSchema(asValue));
  };
  const ctx = {
    item,
    tag
  };
  const definition = definer(ctx);
  for (let idx = 0; idx < items.length; idx++) {
    if (!items[idx]) {
      items[idx] = unit;
    }
  }
  const mut = baseSchema(arrayTag, false);
  mut.items = items;
  mut.additionalItems = "strict";
  mut.decoder = arrayDecoder;
  mut.parser = shapedParser;
  mut.to = definitionToShapedSchema(definition);
  return mut;
};
var getValByFrom = (input, from, idx) => {
  const key = from[idx];
  if (key !== U) {
    return getValByFrom(input.d[key], from, idx + 1);
  } else {
    return input;
  }
};
var assembleShapedObject = (input, schema, field, init, onMissing) => {
  const output = makeObjectVal(input, schema);
  output.io = true;
  if (init !== U) {
    init(output);
  }
  if (schema.items !== U) {
    const items = schema.items;
    for (let idx = 0; idx < items.length; idx++) {
      const location = String(idx);
      B_addObjectField(output, location, field(location, items[idx]));
    }
  } else if (schema.properties !== U) {
    const properties = schema.properties;
    const keys = Object.keys(properties);
    for (let idx = 0; idx < keys.length; idx++) {
      const location = keys[idx];
      if (!(location in output.d)) {
        B_addObjectField(output, location, field(location, properties[location]));
      }
    }
  } else if (onMissing !== U) {
    onMissing();
  } else {
    panic(
      `Don't know where the value is coming from: ${inputExpression(schema)}` + (input.path === "" ? "" : ` at ${input.path}`)
    );
  }
  return completeObjectVal(output);
};
var getShapedParserOutput = (input, targetSchema) => {
  let v;
  if (targetSchema.fromFlattened !== U) {
    v = B_scope(
      getValByFrom(input.fv[targetSchema.fromFlattened], targetSchema.from, 0)
    );
  } else if (targetSchema.from !== U) {
    v = B_scope(getValByFrom(input, targetSchema.from, 0));
  } else if (isLiteral(targetSchema)) {
    v = B_nextConst(input, targetSchema);
  } else {
    v = assembleShapedObject(
      input,
      targetSchema,
      (_location, childSchema) => getShapedParserOutput(input, childSchema)
    );
  }
  v.prev = U;
  v.e = targetSchema;
  return v;
};
var shapedParser = (input) => {
  const flattened = input.e.flattened;
  if (flattened !== U) {
    const flattenedVals = [];
    for (let idx = 0; idx < flattened.length; idx++) {
      const flattenedSchema = flattened[idx];
      let flattenedVal;
      if (flattenedSchema.to !== U) {
        const flattenedInput = B_scope(input);
        flattenedInput.e = flattenedSchema;
        flattenedInput.io = true;
        flattenedVal = parse(flattenedInput);
      } else {
        const assembled = assembleShapedObject(
          input,
          flattenedSchema,
          (location, _childSchema) => valGet(input, location)
        );
        assembled.e = flattenedSchema;
        assembled.prev = U;
        flattenedVal = B_markOutput(assembled, assembled);
      }
      flattenedVals.push(flattenedVal);
      input.cp = input.cp + B_merge(flattenedVal);
    }
    input.fv = flattenedVals;
  }
  const targetSchema = input.e.to;
  const output = getShapedParserOutput(input, targetSchema);
  output.t = true;
  output.prev = input;
  return B_markOutput(output, input);
};
var prepareShapedSerializerAcc = (acc, input) => {
  if (input.e.from !== U) {
    const from = input.e.from;
    const fromFlattened = input.e.fromFlattened;
    let accAtFrom;
    if (fromFlattened !== U) {
      if (acc.flattened === U) {
        acc.flattened = [];
      }
      const existing = acc.flattened[fromFlattened];
      if (existing === U) {
        const newAcc = {};
        acc.flattened[fromFlattened] = newAcc;
        accAtFrom = newAcc;
      } else {
        accAtFrom = existing;
      }
    } else {
      accAtFrom = acc;
    }
    for (let idx = 0; idx < from.length; idx++) {
      const key = from[idx];
      let p;
      if (accAtFrom.properties !== U) {
        p = accAtFrom.properties;
      } else {
        p = {};
        accAtFrom.properties = p;
      }
      const existingAcc = p[key];
      if (existingAcc !== U) {
        accAtFrom = existingAcc;
      } else {
        const newAcc = {};
        p[key] = newAcc;
        accAtFrom = newAcc;
      }
    }
    accAtFrom.val = input;
  } else if (input.d !== U) {
    const vals = input.d;
    const keys = Object.keys(vals);
    for (let idx = 0; idx < keys.length; idx++) {
      prepareShapedSerializerAcc(acc, vals[keys[idx]]);
    }
  }
};
var getShapedSerializerOutput = (input, acc, targetSchema, path) => {
  if (acc !== U && acc.val !== U) {
    const v = B_scope(acc.val);
    v.t = true;
    v.e = targetSchema;
    return parse(v);
  } else if (isLiteral(targetSchema)) {
    const v = B_nextConst(input, targetSchema, targetSchema);
    v.prev = U;
    v.p = input;
    v.v = _notVarAtParent;
    v.io = true;
    return parse(v);
  } else {
    const resolvedTargetSchema = acc === U ? getOutputSchema(targetSchema) : targetSchema;
    const missingInput = () => {
      const path2 = targetSchema.from !== U ? path + targetSchema.from.map((item) => `["${item}"]`).join("") : path;
      return B_invalidOperation(
        input,
        `Missing input for ${inputExpression(targetSchema)}` + (path2 === "" ? "" : ` at ${path2}`)
      );
    };
    if (acc === U && typeof resolvedTargetSchema.additionalItems === objectTag) {
      return missingInput();
    }
    return assembleShapedObject(
      input,
      resolvedTargetSchema,
      (location, childSchema) => getShapedSerializerOutput(
        input,
        acc !== U && acc.properties !== U ? acc.properties[location] : U,
        childSchema,
        pathConcat(path, pathFromInlinedLocation(inlinedValueFromString(location)))
      ),
      (v) => {
        v.e = resolvedTargetSchema;
        v.prev = U;
        v.p = input;
        v.v = _notVarAtParent;
        const flattened = resolvedTargetSchema.flattened;
        if (flattened !== U && acc !== U && acc.flattened !== U) {
          const flattenedSchemas = flattened;
          const flattenedAcc = acc.flattened;
          flattenedAcc.forEach((acc2, idx) => {
            const flattenedOutput = getShapedSerializerOutput(
              input,
              acc2,
              reverse(flattenedSchemas[idx]),
              path
            );
            B_mergeObjectFields(v, flattenedOutput.d);
          });
        }
      },
      missingInput
    );
  }
};
var shapedSerializer = (input) => {
  const acc = {};
  prepareShapedSerializerAcc(acc, input);
  const targetSchema = input.e.to;
  const output = getShapedSerializerOutput(input, acc, targetSchema, pathEmpty);
  output.t = true;
  output.prev = input;
  return output;
};
var definitionToShapedSchema = (definition) => {
  const s2 = copySchema(
    traverseDefinition(
      definition,
      (definition2) => definition2[itemSymbol]
    )
  );
  s2.serializer = shapedSerializer;
  return s2;
};
var definitionToSchema = (definition) => {
  return traverseDefinition(definition, (node) => {
    if (isSchemaObject(node)) {
      return node;
    } else {
      return U;
    }
  });
};
var traverseDefinition = (definition, onNode) => {
  if (typeof definition === objectTag && definition !== null) {
    const s2 = onNode(definition);
    if (s2 !== U) {
      return s2;
    } else {
      if (Array.isArray(definition)) {
        const node = definition;
        for (let idx = 0; idx < node.length; idx++) {
          node[idx] = traverseDefinition(node[idx], onNode);
        }
        const items = node;
        const mut = baseSchema(arrayTag, false);
        mut.items = items;
        mut.additionalItems = "strict";
        mut.decoder = arrayDecoder;
        return mut;
      } else {
        const proto = Object.getPrototypeOf(definition);
        if (proto !== null && proto !== Object.prototype) {
          const mut = baseSchema(instanceTag, true);
          mut.class = definition["constructor"];
          mut.const = definition;
          mut.decoder = literalDecoder;
          return mut;
        } else {
          const node = definition;
          const fieldNames = Object.keys(node);
          const length2 = fieldNames.length;
          for (let idx = 0; idx < length2; idx++) {
            const location = fieldNames[idx];
            node[location] = traverseDefinition(node[location], onNode);
          }
          const mut = baseSchema(objectTag, false);
          mut.required = fieldNames;
          mut.properties = node;
          mut.additionalItems = globalConfig.a;
          mut.decoder = objectDecoder;
          return mut;
        }
      }
    }
  } else {
    return Literal_parse(definition);
  }
};
var schemaCtx = {
  m: (schema) => schema
};
var schemaDefiner = /* @__NO_SIDE_EFFECTS__ */ (definer) => {
  return definitionToSchema(definer(schemaCtx));
};
var schemaFactory = /* @__NO_SIDE_EFFECTS__ */ (definition) => {
  return definitionToSchema(definition);
};
var enum_ = /* @__NO_SIDE_EFFECTS__ */ (values) => {
  return unionFactory(values.map(schemaFactory));
};

// src/refinements.ts
var nullAsOption = /* @__NO_SIDE_EFFECTS__ */ (item) => optionFactory(item, nullAsUnit);
var null_ = (item) => unionFactory([item, nullLiteral]);
var expects = (fnName, expected, got) => `S.${fnName} expects ${expected}, got ${got}`;
var assertNumericBound = (fnName, schema, value) => {
  const tag = schema.type;
  if (tag !== numberTag && tag !== bigintTag) {
    panic(expects(fnName, "number | bigint schema", inputExpression(schema)));
  }
  if (tag === bigintTag ? typeof value !== bigintTag : typeof value !== numberTag || Number.isNaN(value)) {
    throw new SuryError({
      code: "invalid_operation",
      path: pathEmpty,
      reason: expects(fnName, inputExpression(schema), stringify(value))
    });
  }
};
var assertLengthBound = (fnName, schema, value) => {
  if (schema.type !== stringTag && schema.type !== arrayTag) {
    panic(expects(fnName, "string | array schema", inputExpression(schema)));
  }
  if (typeof value !== numberTag || !Number.isSafeInteger(value) || value < 0) {
    throw new SuryError({
      code: "invalid_operation",
      path: pathEmpty,
      reason: expects(fnName, "integer >= 0", stringify(value))
    });
  }
};
var lit = (value) => typeof value === bigintTag ? `${value}n` : `${value}`;
var sizeKey = (schema, upper) => schema.type === arrayTag ? upper ? "maxItems" : "minItems" : upper ? "maxLength" : "minLength";
var withBounds = (schema, base) => {
  const written = schema.bounds;
  const isArray = schema.type === arrayTag;
  const sized = isArray || schema.type === stringTag;
  const minKey = isArray ? "minItems" : sized ? "minLength" : "minimum";
  const maxKey = isArray ? "maxItems" : sized ? "maxLength" : "maximum";
  const exMin = written & 4 ? schema.exclusiveMinimum : U;
  const exMax = written & 8 ? schema.exclusiveMaximum : U;
  const low = exMin !== U ? exMin : written & 1 ? schema[minKey] : U;
  const high = exMax !== U ? exMax : written & 2 ? schema[maxKey] : U;
  const subject = sized ? `${base}.length` : base;
  if (low === U) {
    return `${subject} ${exMax !== U ? "<" : "<="} ${lit(high)}`;
  }
  if (high === U) {
    return `${subject} ${exMin !== U ? ">" : ">="} ${lit(low)}`;
  }
  return exMin === U && exMax === U && low === high ? `${subject} == ${lit(low)}` : `${lit(low)} ${exMin !== U ? "<" : "<="} ${subject} ${exMax !== U ? "<" : "<="} ${lit(high)}`;
};
var setBoundExpression = (mut, schema) => {
  if (schema.bounds === U) {
    const base = schema.expression;
    mut.expression = (s2) => withBounds(s2, base !== U ? base(s2) : inputExpression(s2, true));
  }
};
var narrowsLower = (schema, value, exclusive) => {
  const bound = value;
  const inclusive = schema.minimum;
  const strict2 = schema.exclusiveMinimum;
  return (inclusive === U || (exclusive ? bound >= inclusive : bound > inclusive)) && (strict2 === U || bound > strict2);
};
var narrowsUpper = (schema, value, exclusive) => {
  const bound = value;
  const inclusive = schema.maximum;
  const strict2 = schema.exclusiveMaximum;
  return (inclusive === U || (exclusive ? bound <= inclusive : bound < inclusive)) && (strict2 === U || bound < strict2);
};
var narrowsSize = (current, value, upper) => current === U || (upper ? value < current : value > current);
var conflict = (incoming, existing) => {
  panic(`${inputExpression(incoming)} contradicts ${inputExpression(existing)}`);
};
var asBound = (schema, key, bit, value) => {
  const mut = { ...schema, bounds: bit };
  mut[key] = value;
  setBoundExpression(mut, schema);
  return mut;
};
var assertLower = (schema, value, exclusive) => {
  const key = exclusive ? "exclusiveMinimum" : "minimum";
  const bit = exclusive ? 4 : 1;
  const bound = value;
  const inclusive = schema.maximum;
  const strict2 = schema.exclusiveMaximum;
  if (inclusive !== U && (exclusive ? bound >= inclusive : bound > inclusive)) {
    conflict(asBound(schema, key, bit, value), asBound(schema, "maximum", 2, inclusive));
  }
  if (strict2 !== U && bound >= strict2) {
    conflict(asBound(schema, key, bit, value), asBound(schema, "exclusiveMaximum", 8, strict2));
  }
};
var assertUpper = (schema, value, exclusive) => {
  const key = exclusive ? "exclusiveMaximum" : "maximum";
  const bit = exclusive ? 8 : 2;
  const bound = value;
  const inclusive = schema.minimum;
  const strict2 = schema.exclusiveMinimum;
  if (inclusive !== U && (exclusive ? bound <= inclusive : bound < inclusive)) {
    conflict(asBound(schema, key, bit, value), asBound(schema, "minimum", 1, inclusive));
  }
  if (strict2 !== U && bound <= strict2) {
    conflict(asBound(schema, key, bit, value), asBound(schema, "exclusiveMinimum", 4, strict2));
  }
};
var assertSize = (schema, value, upper) => {
  const otherKey = sizeKey(schema, !upper);
  const other = schema[otherKey];
  if (other !== U && (upper ? value < other : value > other)) {
    conflict(
      asBound(schema, sizeKey(schema, upper), upper ? 2 : 1, value),
      asBound(schema, otherKey, upper ? 1 : 2, other)
    );
  }
};
var gte = /* @__NO_SIDE_EFFECTS__ */ (schema, minValue, maybeMessage) => {
  assertNumericBound("gte", schema, minValue);
  assertLower(schema, minValue, false);
  if (!narrowsLower(schema, minValue, false)) return schema;
  return internalRefine(schema, (mut) => {
    setBoundExpression(mut, schema);
    mut.bounds = (schema.bounds ?? 0) & ~4 | 1;
    mut.minimum = minValue;
    mut.exclusiveMinimum = U;
    if (maybeMessage !== U) getMutErrorMessage(mut)["minimum"] = maybeMessage;
    return (_input) => {
      return [
        {
          c: (inputVar) => `${inputVar}>=${lit(minValue)}`,
          f: B_failWithErrorMessage("minimum")
        }
      ];
    };
  });
};
var lte = /* @__NO_SIDE_EFFECTS__ */ (schema, maxValue, maybeMessage) => {
  assertNumericBound("lte", schema, maxValue);
  assertUpper(schema, maxValue, false);
  if (!narrowsUpper(schema, maxValue, false)) return schema;
  return internalRefine(schema, (mut) => {
    setBoundExpression(mut, schema);
    mut.bounds = (schema.bounds ?? 0) & ~8 | 2;
    mut.maximum = maxValue;
    mut.exclusiveMaximum = U;
    if (maybeMessage !== U) getMutErrorMessage(mut)["maximum"] = maybeMessage;
    return (_input) => {
      return [
        {
          c: (inputVar) => `${inputVar}<=${lit(maxValue)}`,
          f: B_failWithErrorMessage("maximum")
        }
      ];
    };
  });
};
var gt = /* @__NO_SIDE_EFFECTS__ */ (schema, minValue, maybeMessage) => {
  assertNumericBound("gt", schema, minValue);
  assertLower(schema, minValue, true);
  if (!narrowsLower(schema, minValue, true)) return schema;
  return internalRefine(schema, (mut) => {
    setBoundExpression(mut, schema);
    mut.bounds = (schema.bounds ?? 0) & ~1 | 4;
    mut.exclusiveMinimum = minValue;
    mut.minimum = U;
    if (maybeMessage !== U) getMutErrorMessage(mut)["exclusiveMinimum"] = maybeMessage;
    return (_input) => {
      return [
        {
          c: (inputVar) => `${inputVar}>${lit(minValue)}`,
          f: B_failWithErrorMessage("exclusiveMinimum")
        }
      ];
    };
  });
};
var lt = /* @__NO_SIDE_EFFECTS__ */ (schema, maxValue, maybeMessage) => {
  assertNumericBound("lt", schema, maxValue);
  assertUpper(schema, maxValue, true);
  if (!narrowsUpper(schema, maxValue, true)) return schema;
  return internalRefine(schema, (mut) => {
    setBoundExpression(mut, schema);
    mut.bounds = (schema.bounds ?? 0) & ~2 | 8;
    mut.exclusiveMaximum = maxValue;
    mut.maximum = U;
    if (maybeMessage !== U) getMutErrorMessage(mut)["exclusiveMaximum"] = maybeMessage;
    return (_input) => {
      return [
        {
          c: (inputVar) => `${inputVar}<${lit(maxValue)}`,
          f: B_failWithErrorMessage("exclusiveMaximum")
        }
      ];
    };
  });
};
var minLength = /* @__NO_SIDE_EFFECTS__ */ (schema, length2, maybeMessage) => {
  assertLengthBound("minLength", schema, length2);
  assertSize(schema, length2, false);
  const key = sizeKey(schema, false);
  if (!narrowsSize(schema[key], length2, false)) return schema;
  return internalRefine(schema, (mut) => {
    setBoundExpression(mut, schema);
    mut.bounds = (schema.bounds ?? 0) | 1;
    mut[key] = length2;
    if (maybeMessage !== U) getMutErrorMessage(mut)[key] = maybeMessage;
    return (_input) => {
      return [
        {
          c: (inputVar) => `${inputVar}.length>${length2 - 1}`,
          f: B_failWithErrorMessage(key)
        }
      ];
    };
  });
};
var maxLength = /* @__NO_SIDE_EFFECTS__ */ (schema, length2, maybeMessage) => {
  assertLengthBound("maxLength", schema, length2);
  assertSize(schema, length2, true);
  const key = sizeKey(schema, true);
  if (!narrowsSize(schema[key], length2, true)) return schema;
  return internalRefine(schema, (mut) => {
    setBoundExpression(mut, schema);
    mut.bounds = (schema.bounds ?? 0) | 2;
    mut[key] = length2;
    if (maybeMessage !== U) getMutErrorMessage(mut)[key] = maybeMessage;
    return (_input) => {
      return [
        {
          c: (inputVar) => `${inputVar}.length<${length2 + 1}`,
          f: B_failWithErrorMessage(key)
        }
      ];
    };
  });
};
var length = /* @__NO_SIDE_EFFECTS__ */ (schema, length2, maybeMessage) => {
  assertLengthBound("length", schema, length2);
  assertSize(schema, length2, false);
  assertSize(schema, length2, true);
  const minKey = sizeKey(schema, false);
  const maxKey = sizeKey(schema, true);
  if (schema[minKey] === length2 && schema[maxKey] === length2) return schema;
  return internalRefine(schema, (mut) => {
    setBoundExpression(mut, schema);
    mut.bounds = (schema.bounds ?? 0) | 3;
    mut[minKey] = length2;
    mut[maxKey] = length2;
    if (maybeMessage !== U) {
      const em = getMutErrorMessage(mut);
      em[minKey] = maybeMessage;
      em[maxKey] = maybeMessage;
    }
    return (_input) => {
      return [
        {
          c: (inputVar) => `${inputVar}.length===${length2}`,
          f: B_failWithErrorMessage(minKey)
        }
      ];
    };
  });
};
var empty = /* @__NO_SIDE_EFFECTS__ */ (schema, maybeMessage) => /* @__PURE__ */ length(schema, 0, maybeMessage);
var nonEmpty = /* @__NO_SIDE_EFFECTS__ */ (schema, maybeMessage) => /* @__PURE__ */ minLength(schema, 1, maybeMessage);
var pattern = /* @__NO_SIDE_EFFECTS__ */ (schema, re, message = `Invalid pattern`) => {
  return internalRefine(schema, (mut) => {
    mut.pattern = re;
    getMutErrorMessage(mut)["pattern"] = message;
    return (input) => {
      const embededRe = B_embed(input, re);
      return [
        {
          c: (inputVar) => re.global ? `(${embededRe}.lastIndex=0,${embededRe}.test(${inputVar}))` : `${embededRe}.test(${inputVar})`,
          f: B_failWithErrorMessage("pattern", message)
        }
      ];
    };
  });
};
var trim = /* @__NO_SIDE_EFFECTS__ */ (schema) => {
  const transformer = B_conversion((value) => value.trim());
  return codecTo(schema, string, transformer, transformer);
};
var nullable = /* @__NO_SIDE_EFFECTS__ */ (schema) => {
  return unionFactory([schema, unit, nullLiteral]);
};
var nullableAsOption = /* @__NO_SIDE_EFFECTS__ */ (schema) => {
  return unionFactory([schema, unit, nullAsUnit]);
};
var isoDateTime = /* @__PURE__ */ initSchema(stringTag, (s2) => {
  const datetimeRe = /^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(\.\d+)?Z$/;
  s2.decoder = stringDecoderFn;
  s2.format = "date-time";
  s2.refiner = (input) => {
    return [
      {
        c: (inputVar) => `${B_embed(input, datetimeRe)}.test(${inputVar})`,
        f: B_failWithErrorMessage(
          "format",
          "Invalid datetime string! Expected UTC"
        )
      }
    ];
  };
});
var port = /* @__PURE__ */ initSchema(numberTag, (s2) => {
  s2.decoder = numberDecoder;
  s2.format = "port";
  s2.minimum = 0;
  s2.maximum = 65535;
  s2.refiner = (_input) => {
    return [
      {
        c: (inputVar) => `${inputVar}>=0&&${inputVar}<65536&&${inputVar}%1===0`,
        f: B_failWithErrorMessage("format")
      }
    ];
  };
});
var email = /* @__PURE__ */ initSchema(stringTag, (s2) => {
  const emailRegex = /^(?!\.)(?!.*\.\.)([A-Z0-9_'+\-\.]*)[A-Z0-9_+-]@([A-Z0-9][A-Z0-9\-]*\.)+[A-Z]{2,}$/i;
  s2.decoder = stringDecoderFn;
  s2.format = "email";
  s2.refiner = (input) => {
    return [
      {
        c: (inputVar) => `${B_embed(input, emailRegex)}.test(${inputVar})`,
        f: B_failWithErrorMessage("format")
      }
    ];
  };
});
var uuid = /* @__PURE__ */ initSchema(stringTag, (s2) => {
  const uuidRegex = /^[0-9a-fA-F]{8}\b-[0-9a-fA-F]{4}\b-[0-9a-fA-F]{4}\b-[0-9a-fA-F]{4}\b-[0-9a-fA-F]{12}$/i;
  s2.decoder = stringDecoderFn;
  s2.format = "uuid";
  s2.refiner = (input) => {
    return [
      {
        c: (inputVar) => `${B_embed(input, uuidRegex)}.test(${inputVar})`,
        f: B_failWithErrorMessage("format")
      }
    ];
  };
});
var cuid = /* @__PURE__ */ initSchema(stringTag, (s2) => {
  const cuidRegex = /^c[^\s-]{8,}$/i;
  s2.decoder = stringDecoderFn;
  s2.format = "cuid";
  s2.refiner = (input) => {
    return [
      {
        c: (inputVar) => `${B_embed(input, cuidRegex)}.test(${inputVar})`,
        f: B_failWithErrorMessage("format")
      }
    ];
  };
});
var url = /* @__PURE__ */ initSchema(stringTag, (s2) => {
  const urlValidator = (s3) => {
    try {
      new URL(s3);
      return true;
    } catch {
      return false;
    }
  };
  s2.decoder = stringDecoderFn;
  s2.format = "url";
  s2.refiner = (input) => {
    return [
      {
        c: (inputVar) => `${B_embed(input, urlValidator)}(${inputVar})`,
        f: B_failWithErrorMessage("format")
      }
    ];
  };
});

// src/operations.ts
var standardJSONSchemaConverter;
var __setStandardJSONSchemaConverter = (fn) => {
  standardJSONSchemaConverter = fn;
};
var getStandardJSONSchema = (schema, options, isOutput) => {
  if (standardJSONSchemaConverter !== U) {
    return standardJSONSchemaConverter(schema, options, isOutput);
  } else {
    throw new SuryError({
      code: "invalid_operation",
      path: pathEmpty,
      reason: "~standard.jsonSchema requires S.enableStandardJSONSchema() to be called first"
    });
  }
};
Object.defineProperty(schemaPrototype, "toString", {
  value: function() {
    const input = inputExpression(this);
    const output = inputExpression(reverse(this));
    return `Schema<${input === output ? input : `${input}, ${output}`}>`;
  }
});
Object.defineProperty(schemaPrototype, "~standard", {
  get: function() {
    const schema = this;
    const standard = {
      version: 1,
      vendor,
      validate: (input) => {
        try {
          return {
            value: getDecoder(unknown, schema)(input)
          };
        } catch (exn) {
          const error = getOrRethrow(exn);
          return {
            issues: [
              {
                message: error.reason,
                path: error.path === pathEmpty ? U : pathToArray(error.path)
              }
            ]
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
        output: (options) => getStandardJSONSchema(schema, options, true)
      }
    };
    valueOptions[valKey] = standard;
    Object.defineProperty(schema, "~standard", valueOptions);
    return standard;
  }
});
var assertResult = /* @__PURE__ */ initSchema(undefinedTag, (s2) => {
  s2.const = U;
  s2.decoder = literalDecoder;
  s2.noValidation = true;
});
var assertOrThrow = (any, schema) => {
  getDecoder(unknown, schema, assertResult)(any);
};
var assertAsyncOrThrow = (any, schema) => {
  return getDecoder(unknown, schema, assertResult, flagAsync)(any);
};
var wrapExnToFailure = (exn) => {
  if (exn && exn.s === s) {
    return { success: false, error: exn };
  } else {
    throw exn;
  }
};
var js_safe = (fn) => {
  try {
    return {
      success: true,
      value: fn()
    };
  } catch (exn) {
    return wrapExnToFailure(exn);
  }
};
var js_safeAsync = (fn) => {
  try {
    return fn().then(
      (value) => ({ success: true, value }),
      wrapExnToFailure
    );
  } catch (exn) {
    return Promise.resolve(wrapExnToFailure(exn));
  }
};

// src/jsapi.ts
var js_parser = /* @__NO_SIDE_EFFECTS__ */ (...args) => getDecoder(unknown, ...args);
var js_asyncParser = /* @__NO_SIDE_EFFECTS__ */ (...args) => getDecoder(unknown, ...args, 1);
var js_asyncDecoder = /* @__NO_SIDE_EFFECTS__ */ (...args) => getDecoder(...args, 1);
var js_encoder = /* @__NO_SIDE_EFFECTS__ */ (...args) => getDecoder(...args.map(reverse));
var js_asyncEncoder = /* @__NO_SIDE_EFFECTS__ */ (...args) => getDecoder(...args.map(reverse), 1);
var js_assert = (a, b) => {
  const aIsSchema = !!a && isSchemaObject(a);
  const schema = aIsSchema ? a : b;
  const data = aIsSchema ? b : a;
  return getDecoder(unknown, schema, assertResult)(data);
};
var js_is = (a, b) => {
  try {
    js_assert(a, b);
    return true;
  } catch (exn) {
    getOrRethrow(exn);
    return false;
  }
};
var js_union = /* @__NO_SIDE_EFFECTS__ */ (values) => unionFactory(values.map(definitionToSchema));
var ambiguousEncode = (input) => B_invalidOperation(
  input,
  "Encoding is ambiguous when only a decode function is provided. Use S.to(target, {decode, encode})"
);
var conversionBuilder = (slot, junction) => {
  const async = slot?.async;
  if (slot === "auto") {
    return U;
  } else if (slot === "never") {
    return B_neverSlot;
  } else if (typeof slot === functionTag) {
    return B_conversion(slot, false, junction);
  } else if (typeof async === functionTag && Object.keys(slot).length === 1) {
    return B_conversion(async, true, junction);
  } else {
    return panic(
      `Invalid conversion ${stringify(slot)}. Expected a function, "auto", "never" or {async: fn}`
    );
  }
};
var js_to = /* @__NO_SIDE_EFFECTS__ */ (schema, target, custom) => {
  let decode;
  let encode;
  let outputSeam = false;
  if (typeof custom === functionTag) {
    decode = B_conversion(custom, false, true);
    encode = ambiguousEncode;
  } else if (custom) {
    const codecs = custom;
    const toOutput = codecs["decodeToOutput"];
    outputSeam = !!toOutput;
    const decodeSlot = outputSeam ? toOutput : codecs["decode"];
    const encodeSlot = outputSeam ? codecs["encodeFromOutput"] : codecs["encode"];
    if (!decodeSlot || !encodeSlot || Object.keys(codecs).length !== 2) {
      return panic(
        `Custom codecs must define both decode and encode. Use "auto" for the built-in conversion`
      );
    }
    decode = conversionBuilder(decodeSlot, !outputSeam);
    encode = conversionBuilder(encodeSlot, !outputSeam);
  }
  if (schema === target && !decode && !encode) {
    return schema;
  }
  if (outputSeam && target.to && (decode && decode !== B_neverSlot || encode && encode !== B_neverSlot)) {
    return panic(
      `The target already converts. Chain S.to instead of passing a custom codec`
    );
  }
  return codecTo(schema, target, decode, encode);
};
var js_refine = /* @__NO_SIDE_EFFECTS__ */ (schema, refineCheck, refineOptions) => {
  const message = refineOptions?.error ?? "Refinement failed";
  const extraPath = refineOptions?.path !== U ? pathFromArray(refineOptions.path) : pathEmpty;
  return internalRefine(schema, (_) => (input) => {
    const embeddedCheck = B_embed(input, refineCheck);
    return [
      {
        c: (inputVar) => `${embeddedCheck}(${inputVar})`,
        f: B_invalidInputBuilder(U, extraPath, message)
      }
    ];
  });
};
var passthroughSlot = (input) => B_refine(input, input.e.to, U, input.e.to);
var js_asyncDecoderAssert = /* @__NO_SIDE_EFFECTS__ */ (schema, assertFn) => {
  return codecTo(
    schema,
    unknown,
    B_conversion((v) => assertFn(v).then(() => v), true),
    passthroughSlot
  );
};
var js_optional = /* @__NO_SIDE_EFFECTS__ */ (schema, maybeOr) => {
  schema = unionFactory([schema, unit]);
  if (maybeOr !== U && typeof maybeOr === functionTag) {
    return Option_getOrWith(schema, maybeOr);
  } else if (maybeOr !== U) {
    return Option_getOr(schema, maybeOr);
  } else {
    return schema;
  }
};
var js_nullable = /* @__NO_SIDE_EFFECTS__ */ (schema, maybeOr) => {
  if (maybeOr !== U) {
    const schema2 = unionFactory([schema, nullAsUnit]);
    if (typeof maybeOr === functionTag) {
      return Option_getOrWith(schema2, maybeOr);
    } else {
      return Option_getOr(schema2, maybeOr);
    }
  } else {
    return unionFactory([schema, nullLiteral]);
  }
};
var js_merge = /* @__NO_SIDE_EFFECTS__ */ (s1, s2) => {
  let result;
  if (s1.type === objectTag && s2.type === objectTag && // Filter out S.record schemas
  typeof s1.additionalItems === stringTag && typeof s2.additionalItems === stringTag && !s1.to && !s2.to) {
    const properties = { ...s1.properties, ...s2.properties };
    const mut = baseSchema(objectTag, false);
    mut.required = Object.keys(properties);
    mut.properties = properties;
    mut.additionalItems = s1.additionalItems;
    mut.decoder = objectDecoder;
    result = mut;
  }
  if (result !== U) {
    return result;
  } else {
    return panic(
      "The merge supports only structured object schemas without transformations"
    );
  }
};
var global = (override) => {
  globalConfig.a = override.defaultAdditionalItems !== U ? override.defaultAdditionalItems : initialOnAdditionalItems;
  globalConfig.f = override.disableNanNumberValidation === true ? flagDisableNanNumberValidation : initialDefaultFlag;
};

// src/advanced/compactColumns.ts
var compactColumnsExpression = (schema) => {
  const to = schema.to;
  const item = to !== U ? to.additionalItems : U;
  const props = typeof item === "object" ? item.properties : U;
  if (props === U) {
    return `${inputExpression(schema.additionalItems)}[]`;
  }
  let body = "";
  for (const key in props) {
    body = body + (body ? ", " : "") + inputExpression(props[key]) + "[]";
  }
  return `[${body}]`;
};
var compactColumnsDecoder = (input) => {
  const selfSchema = input.e;
  const isUnknownInput = flagUnsafeHas(tagFlags[input.s.type], tagFlagUnknown);
  const declaredItemSchema = selfSchema.additionalItems.additionalItems;
  let forwardProps;
  if (selfSchema.to !== U && typeof selfSchema.to.additionalItems === "object") {
    forwardProps = selfSchema.to.additionalItems.properties;
  } else {
    forwardProps = U;
  }
  const isForwardDirection = forwardProps !== U;
  let maybeProperties;
  if (isForwardDirection) {
    maybeProperties = forwardProps;
  } else {
    if (input.s.additionalItems !== U && typeof input.s.additionalItems === "object") {
      maybeProperties = input.s.additionalItems.properties;
    } else {
      maybeProperties = U;
    }
  }
  if (!maybeProperties) {
    return panic(
      "S.compactColumns supports only object schemas. Use S.compactColumns(S.unknown)->S.to(S.array(objectSchema))."
    );
  } else {
    const properties = maybeProperties;
    const keys = Object.keys(properties);
    const keysLen = keys.length;
    let outputSchema;
    if (isForwardDirection) {
      outputSchema = selfSchema.to;
    } else {
      const s2 = array(array(unknown));
      s2.to = selfSchema.to;
      outputSchema = s2;
    }
    if (keysLen === 0) {
      if (isUnknownInput) {
        input = B_refine(input, U, [
          {
            c: (inputVar) => `Array.isArray(${inputVar})&&${inputVar}.length===0`,
            f: failInvalidType
          }
        ]);
      }
      const output = B_next(input, "[]", outputSchema, outputSchema);
      return B_markOutput(output, input);
    } else if (isForwardDirection) {
      if (isUnknownInput) {
        input = B_refine(input, U, [
          {
            c: (inputVar2) => {
              let check = `Array.isArray(${inputVar2})&&${inputVar2}.length===${keysLen}`;
              for (let idx = 0; idx <= keysLen - 1; ++idx) {
                check = check + `&&Array.isArray(${inputVar2}[${idx}])`;
              }
              return check;
            },
            f: failInvalidType
          }
        ]);
      }
      const inputVar = input.v();
      const iteratorVar = B_varWithoutAllocation(input.g);
      const outputVar = B_varWithoutAllocation(input.g);
      let runtimeItemSchema;
      if (isUnknownInput) {
        runtimeItemSchema = unknown;
      } else {
        const innerArray = input.s.additionalItems;
        runtimeItemSchema = innerArray.additionalItems;
      }
      let lengthCode = "";
      let itemBuildCode = "";
      let itemParseCode = "";
      let asyncInlines = "";
      let hasAsync = false;
      for (let idx = 0; idx <= keysLen - 1; ++idx) {
        const key = keys[idx];
        const idxStr = `${idx}`;
        const rawValueCode = `${inputVar}[${idxStr}][${iteratorVar}]`;
        const fieldSchema = properties[key];
        let itemExpected;
        if (declaredItemSchema !== runtimeItemSchema) {
          const chained = copySchema(declaredItemSchema);
          chained.to = fieldSchema;
          itemExpected = chained;
        } else {
          itemExpected = fieldSchema;
        }
        const itemInput = B_scope(input);
        itemInput.i = rawValueCode;
        itemInput.s = runtimeItemSchema;
        itemInput.e = itemExpected;
        itemInput.v = _notVarBeforeValidation;
        itemInput.io = false;
        itemInput.path = pathFromInlinedLocation(inlinedValueFromString(key));
        const itemOutput = parse(itemInput);
        if (flagUnsafeHas(itemOutput.f, valFlagAsync)) {
          hasAsync = true;
        }
        itemParseCode = itemParseCode + B_merge(itemOutput);
        lengthCode = lengthCode + `${inputVar}[${idxStr}].length,`;
        asyncInlines = asyncInlines + `${itemOutput.i},`;
        itemBuildCode = itemBuildCode + `${inlinedValueFromString(key)}:${itemOutput.i},`;
      }
      let output = B_next(input, outputVar, outputSchema, outputSchema);
      output.v = _var;
      output.cp = `let ${outputVar}=new Array(Math.max(${lengthCode}));`;
      let rowAssign;
      if (hasAsync) {
        const rowResultVar = B_varWithoutAllocation(input.g);
        let asyncBuildCode = "";
        for (let idx = 0; idx <= keysLen - 1; ++idx) {
          const key = keys[idx];
          asyncBuildCode = asyncBuildCode + `${inlinedValueFromString(key)}:${rowResultVar}[${idx}],`;
        }
        rowAssign = `${outputVar}[${iteratorVar}]=Promise.all([${asyncInlines}]).then(${rowResultVar}=>({${asyncBuildCode}}));`;
      } else {
        rowAssign = `${outputVar}[${iteratorVar}]={${itemBuildCode}};`;
      }
      const rowBody = itemParseCode + rowAssign;
      let wrappedBody;
      if (itemParseCode === "") {
        wrappedBody = rowBody;
      } else {
        const errorVar = B_varWithoutAllocation(input.g);
        B_markThrow(input);
        wrappedBody = `try{${rowBody}}catch(${errorVar}){${errorVar}.path='["'+${iteratorVar}+'"]'+${errorVar}.path;throw ${errorVar}}`;
      }
      output.cp = output.cp + `for(let ${iteratorVar}=0;${iteratorVar}<${outputVar}.length;++${iteratorVar}){${wrappedBody}}`;
      if (hasAsync) {
        output = B_asyncVal(output, `Promise.all(${outputVar})`);
      }
      return B_markOutput(output, input);
    } else {
      const inputVar = input.v();
      const iteratorVar = B_varWithoutAllocation(input.g);
      const outputVar = B_varWithoutAllocation(input.g);
      const needsPerFieldTransform = declaredItemSchema !== unknown;
      let initialArraysCode = "";
      let settingCode = "";
      let perFieldCode = "";
      for (let idx = 0; idx <= keysLen - 1; ++idx) {
        const key = keys[idx];
        initialArraysCode = initialArraysCode + `new Array(${inputVar}.length),`;
        if (needsPerFieldTransform) {
          const fieldSchema = properties[key];
          const rawValueCode = `${inputVar}[${iteratorVar}][${inlinedValueFromString(key)}]`;
          const itemInput = B_scope(input);
          itemInput.i = rawValueCode;
          itemInput.s = fieldSchema;
          itemInput.e = declaredItemSchema;
          itemInput.v = _notVarBeforeValidation;
          itemInput.io = false;
          itemInput.path = pathFromInlinedLocation(inlinedValueFromString(key));
          const itemOutput = parse(itemInput);
          perFieldCode = perFieldCode + B_merge(itemOutput);
          settingCode = settingCode + `${outputVar}[${idx}][${iteratorVar}]=${itemOutput.i};`;
        } else {
          settingCode = settingCode + `${outputVar}[${idx}][${iteratorVar}]=${inputVar}[${iteratorVar}][${inlinedValueFromString(key)}];`;
        }
      }
      const output = B_next(input, outputVar, outputSchema, outputSchema);
      output.v = _var;
      output.cp = `let ${outputVar}=[${initialArraysCode}];`;
      const loopBody = perFieldCode + settingCode;
      let wrappedBody;
      if (needsPerFieldTransform && perFieldCode !== "") {
        const errorVar = B_varWithoutAllocation(input.g);
        B_markThrow(input);
        wrappedBody = `try{${loopBody}}catch(${errorVar}){${errorVar}.path='["'+${iteratorVar}+'"]'+${errorVar}.path;throw ${errorVar}}`;
      } else {
        wrappedBody = loopBody;
      }
      output.cp = output.cp + `for(let ${iteratorVar}=0;${iteratorVar}<${inputVar}.length;++${iteratorVar}){${wrappedBody}}`;
      return B_markOutput(output, input);
    }
  }
};
var compactColumns = /* @__NO_SIDE_EFFECTS__ */ (inputSchema) => {
  const innerArray = array(inputSchema);
  const mut = array(innerArray);
  mut.format = "compactColumns";
  mut.decoder = compactColumnsDecoder;
  mut.expression = compactColumnsExpression;
  return mut;
};

// src/advanced/list.ts
var listFromArray = (array2) => {
  let list2 = 0;
  for (let i = array2.length - 1; i >= 0; i--) {
    list2 = { hd: array2[i], tl: list2 };
  }
  return list2;
};
var listToArray = (list2) => {
  const array2 = [];
  let current = list2;
  while (current !== 0) {
    array2.push(current.hd);
    current = current.tl;
  }
  return array2;
};
var list = /* @__NO_SIDE_EFFECTS__ */ (schema) => {
  return codecTo(
    array(schema),
    unknown,
    B_conversion((array2) => listFromArray(array2)),
    B_conversion((list2) => listToArray(list2))
  );
};

// src/jsonschema.ts
var openApi30 = "openapi-3.0";
var jsonSchemaMetadataId = /* @__PURE__ */ Metadata_Id_internal("JSONSchema");
var jsonSchemaMerge = (a, b) => {
  return Object.assign({}, a, b);
};
var applyMetadataOverlay = (jsonSchema, schema, defs) => {
  if (schema.description !== U) {
    jsonSchema.description = schema.description;
  }
  if (schema.title !== U) {
    jsonSchema.title = schema.title;
  }
  if (schema.deprecated !== U) {
    jsonSchema.deprecated = schema.deprecated;
  }
  if (schema.examples !== U) {
    jsonSchema.examples = schema.examples;
  }
  if (schema["$defs"] !== U) {
    Object.assign(defs, schema["$defs"]);
  }
  const metadataRawSchema = Metadata_get(schema, jsonSchemaMetadataId);
  if (metadataRawSchema !== U) {
    Object.assign(jsonSchema, metadataRawSchema);
  }
};
var encodeToJsonSchema = (schema, path, defs, parent, target) => {
  const schemaInternal = schema;
  const reversed = reverse(schemaInternal);
  const input = B_operationArg(unknown, reversed, flagNone, U);
  try {
    const output = parse(input);
    return internalToJSONSchema(output.s, path, defs, parent, target);
  } catch (exn) {
    getOrRethrow(exn);
    return U;
  }
};
var internalToJSONSchema = (schema, path, defs, parent, target) => {
  const schemaInternal = schema;
  const tagFlag = tagFlags[schemaInternal.type];
  const hasUserTo = !!schemaInternal.to && !flagUnsafeHas(tagFlag, tagFlagObject | tagFlagArray) && !(flagUnsafeHas(tagFlag, tagFlagUnion) && !!schemaInternal.parser);
  const encoded = hasUserTo ? encodeToJsonSchema(schema, path, defs, parent, target) : U;
  if (encoded !== U) {
    applyMetadataOverlay(encoded, schema, defs);
    return encoded;
  } else {
    return internalToJSONSchemaBase(schema, path, defs, parent, target);
  }
};
var internalToJSONSchemaBase = (schema, path, defs, parent, target) => {
  const jsonSchema = {};
  const setConstOrEnum = (value) => {
    if (target === openApi30) {
      jsonSchema.enum = [value];
    } else {
      jsonSchema.const = value;
    }
  };
  const tag = schema.type;
  if (tag === stringTag) {
    const const_ = schema.const;
    const format = schema.format;
    jsonSchema.type = "string";
    switch (format) {
      case "date-time":
        jsonSchema.format = "date-time";
        break;
      case "email":
        jsonSchema.format = "email";
        break;
      case "uuid":
        jsonSchema.format = "uuid";
        break;
      case "url":
        jsonSchema.format = "uri";
        break;
      default:
        break;
    }
    if (schema.minLength !== U) {
      jsonSchema.minLength = schema.minLength;
    }
    if (schema.maxLength !== U) {
      jsonSchema.maxLength = schema.maxLength;
    }
    if (schema.pattern !== U) {
      jsonSchema.pattern = schema.pattern.source;
    }
    if (const_ !== U) {
      setConstOrEnum(const_);
    }
  } else if (tag === numberTag) {
    const format = schema.format;
    const const_ = schema.const;
    const minimum = schema.minimum;
    const maximum = schema.maximum;
    const exclusiveMinimum = schema.exclusiveMinimum;
    const exclusiveMaximum = schema.exclusiveMaximum;
    jsonSchema.type = format === "int32" || format === "port" ? "integer" : "number";
    if (minimum !== U) {
      jsonSchema.minimum = minimum;
    }
    if (maximum !== U) {
      jsonSchema.maximum = maximum;
    }
    if (exclusiveMinimum !== U) {
      if (target === openApi30) {
        jsonSchema.minimum = exclusiveMinimum;
        jsonSchema.exclusiveMinimum = true;
      } else {
        jsonSchema.exclusiveMinimum = exclusiveMinimum;
      }
    }
    if (exclusiveMaximum !== U) {
      if (target === openApi30) {
        jsonSchema.maximum = exclusiveMaximum;
        jsonSchema.exclusiveMaximum = true;
      } else {
        jsonSchema.exclusiveMaximum = exclusiveMaximum;
      }
    }
    if (const_ !== U) {
      setConstOrEnum(const_);
    }
  } else if (tag === booleanTag) {
    const const_ = schema.const;
    jsonSchema.type = "boolean";
    if (const_ !== U) {
      setConstOrEnum(const_);
    }
  } else if (tag === arrayTag) {
    const additionalItems = schema.additionalItems;
    const items = schema.items;
    if (typeof additionalItems === "object") {
      jsonSchema.items = internalToJSONSchema(
        additionalItems,
        pathConcat(path, pathDynamic),
        defs,
        schema,
        target
      );
      jsonSchema.type = "array";
      if (schema.minItems !== U) {
        jsonSchema.minItems = schema.minItems;
      }
      if (schema.maxItems !== U) {
        jsonSchema.maxItems = schema.maxItems;
      }
    } else {
      const itemDefinitions = items.map((itemSchema, idx) => {
        return internalToJSONSchema(
          itemSchema,
          pathConcat(path, pathFromLocation(idx.toString())),
          defs,
          schema,
          target
        );
      });
      const itemsNumber = itemDefinitions.length;
      jsonSchema.type = "array";
      jsonSchema.minItems = itemsNumber;
      jsonSchema.maxItems = itemsNumber;
      if (target === openApi30) {
        jsonSchema.items = { anyOf: itemDefinitions };
      } else if (target === "draft-2020-12") {
        jsonSchema.prefixItems = itemDefinitions;
      } else {
        jsonSchema.items = itemDefinitions;
      }
    }
  } else if (tag === anyOfTag) {
    const anyOf = schema.anyOf;
    const literals = [];
    const items = [];
    const seen = {};
    anyOf.forEach((childSchema) => {
      if (!(childSchema.type === undefinedTag && parent.type === objectTag)) {
        const childJsonSchema = internalToJSONSchema(childSchema, path, defs, schema, target);
        const key = JSON.stringify(childJsonSchema);
        if (!(key in seen)) {
          seen[key] = true;
          items.push(childJsonSchema);
          if (isLiteral(childSchema)) {
            literals.push(
              childSchema.const
              // If a schema is Jsonable, the const is Jsonable too.
            );
          }
        }
      }
    });
    const itemsNumber = items.length;
    if (schema.default !== U) {
      jsonSchema.default = schema.default;
    }
    const isNullDefinition = (definition) => {
      if (typeof definition !== "boolean") {
        const t = definition;
        if (t.type === "null") {
          return true;
        } else if (t.enum !== U && t.enum.length === 1 && t.enum[0] === null) {
          return true;
        } else {
          return false;
        }
      } else {
        return false;
      }
    };
    if (itemsNumber === 1) {
      Object.assign(jsonSchema, items[0]);
    } else if (literals.length === itemsNumber) {
      jsonSchema.enum = literals;
    } else if (
      // OpenAPI 3.0 collapse of `X | null` into `{...X, nullable: true}`.
      target === openApi30 && itemsNumber === 2 && (isNullDefinition(items[0]) || isNullDefinition(items[1]))
    ) {
      const nullIsFirst = isNullDefinition(items[0]);
      const nonNull = items[nullIsFirst ? 1 : 0];
      if (typeof nonNull !== "boolean") {
        const nonNullSchema = nonNull;
        Object.assign(jsonSchema, nonNullSchema);
        jsonSchema.nullable = true;
      } else {
        jsonSchema.anyOf = items;
      }
    } else {
      jsonSchema.anyOf = items;
    }
  } else if (tag === objectTag) {
    const properties = schema.properties;
    const additionalItems = schema.additionalItems;
    if (typeof additionalItems === "object") {
      jsonSchema.type = "object";
      const childJsonSchema = internalToJSONSchema(
        additionalItems,
        pathConcat(path, pathDynamic),
        defs,
        schema,
        target
      );
      jsonSchema.additionalProperties = Object.keys(childJsonSchema).length === 0 ? true : childJsonSchema;
    } else {
      const required = [];
      const jsonProperties = {};
      Object.keys(properties).forEach((key) => {
        const itemSchema = properties[key];
        const fieldSchema = internalToJSONSchema(
          itemSchema,
          pathConcat(path, pathFromLocation(key)),
          defs,
          schema,
          target
        );
        if (!isOptional(itemSchema)) {
          required.push(key);
        }
        jsonProperties[key] = fieldSchema;
      });
      jsonSchema.type = "object";
      jsonSchema.properties = jsonProperties;
      if (additionalItems === "strict") {
        jsonSchema.additionalProperties = false;
      }
      if (required.length !== 0) {
        jsonSchema.required = required;
      }
    }
  } else if (tag === refTag && schema["$ref"] === `${defsPath}${jsonName}`) {
  } else if (tag === refTag) {
    jsonSchema.$ref = schema["$ref"];
  } else if (tag === nullTag) {
    if (target === openApi30) {
      jsonSchema.enum = [null];
    } else {
      jsonSchema.type = "null";
    }
  } else if (tag === neverTag) {
    jsonSchema.not = {};
  } else {
    const offender = flagUnsafeHas(tagFlags[parent.type], tagFlagUnion) ? parent : schema;
    throw new SuryError({
      code: "invalid_operation",
      path,
      reason: `Expected ${jsonName}, received ${inputExpression(offender)}`
    });
  }
  applyMetadataOverlay(jsonSchema, schema, defs);
  return jsonSchema;
};
var targetSchemaUri = (target) => {
  switch (target) {
    case "draft-07":
      return "http://json-schema.org/draft-07/schema#";
    case "draft-2020-12":
      return "https://json-schema.org/draft/2020-12/schema";
    // OpenAPI 3.0 has no `$schema` property.
    case openApi30:
      return U;
    default: {
      const unsupported = target;
      throw new SuryError({
        code: "invalid_operation",
        path: pathEmpty,
        reason: `Unsupported JSON Schema target: ${unsupported}`
      });
    }
  }
};
var toJSONSchema = /* @__NO_SIDE_EFFECTS__ */ (schema, options) => {
  let target;
  let schemaUri;
  if (options !== U) {
    target = options.target !== U ? options.target : "draft-07";
    schemaUri = targetSchemaUri(target);
  } else {
    target = "draft-07";
    schemaUri = U;
  }
  const defs = {};
  const jsonSchema = internalToJSONSchema(schema, pathEmpty, defs, schema, target);
  delete defs.JSON;
  const defsKeys = Object.keys(defs);
  if (defsKeys.length) {
    const jsonSchemDefs = defs;
    defsKeys.forEach((key) => {
      const schema2 = defs[key];
      jsonSchemDefs[key] = internalToJSONSchema(
        schema2,
        pathEmpty,
        // A fresh, thrown-away sink — it's not possible to have nested
        // recursive schemas here; everything should be grouped into the
        // single top-level $defs collected above, not accumulate into a
        // second one.
        {},
        schema2,
        target
      );
    });
    jsonSchema.$defs = jsonSchemDefs;
  }
  if (schemaUri !== U) {
    jsonSchema.$schema = schemaUri;
  }
  return jsonSchema;
};
var enableStandardJSONSchema = () => {
  __setStandardJSONSchemaConverter((schema, options, isOutput) => {
    return /* @__PURE__ */ toJSONSchema(isOutput ? reverse(schema) : schema, { target: options.target });
  });
};
var extendJSONSchema = /* @__NO_SIDE_EFFECTS__ */ (schema, jsonSchema) => {
  const existingSchemaExtend = Metadata_get(schema, jsonSchemaMetadataId);
  return Metadata_set(
    schema,
    jsonSchemaMetadataId,
    existingSchemaExtend !== U ? jsonSchemaMerge(existingSchemaExtend, jsonSchema) : jsonSchema
  );
};
var primitiveToSchema = (primitive) => {
  return Literal_parse(primitive);
};
var exclusiveBound = (inclusive, exclusive) => exclusive === true ? inclusive : typeof exclusive === "number" ? exclusive : U;
var inclusiveBound = (inclusive, exclusive) => exclusive === true ? U : inclusive;
var withNumericBounds = (schema, jsonSchema) => {
  const min = inclusiveBound(jsonSchema.minimum, jsonSchema.exclusiveMinimum);
  const exMin = exclusiveBound(jsonSchema.minimum, jsonSchema.exclusiveMinimum);
  const max = inclusiveBound(jsonSchema.maximum, jsonSchema.exclusiveMaximum);
  const exMax = exclusiveBound(jsonSchema.maximum, jsonSchema.exclusiveMaximum);
  if (min !== U) {
    schema = applyBound(schema, gte, min);
  }
  if (exMin !== U) {
    schema = applyBound(schema, gt, exMin);
  }
  if (max !== U) {
    schema = applyBound(schema, lte, max);
  }
  if (exMax !== U) {
    schema = applyBound(schema, lt, exMax);
  }
  return schema;
};
var toIntSchema = (jsonSchema) => withNumericBounds(int, jsonSchema);
var unsupportedKeywords = [
  "multipleOf",
  "uniqueItems",
  "contains",
  "minContains",
  "maxContains",
  "patternProperties",
  "propertyNames",
  "minProperties",
  "maxProperties",
  "dependencies",
  "dependentSchemas",
  "dependentRequired",
  "unevaluatedProperties",
  "unevaluatedItems",
  "additionalItems"
];
var keywordTypes = [
  ["string", ["pattern", "minLength", "maxLength"]],
  ["number", ["minimum", "maximum", "exclusiveMinimum", "exclusiveMaximum"]],
  ["object", ["properties", "required", "additionalProperties"]],
  ["array", ["items", "prefixItems", "minItems", "maxItems"]]
];
var jsonTypeOf = (data) => data === null ? "null" : Array.isArray(data) ? "array" : typeof data === "boolean" ? "boolean" : typeof data === "number" ? "number" : typeof data === "string" ? "string" : "object";
var passesSchema = (data, schema) => {
  try {
    assertOrThrow(data, schema);
    return true;
  } catch (_) {
    return false;
  }
};
var definitionToDefaultValue = (definition) => {
  if (typeof definition !== "boolean") {
    return definition.default;
  } else {
    return U;
  }
};
var applyBound = (schema, bound, value) => {
  if (schema.type === neverTag) {
    return schema;
  }
  try {
    return bound(schema, value);
  } catch (exn) {
    if (exn && exn.s === s) {
      throw exn;
    }
    return never_;
  }
};
var fromJSONSchema = /* @__NO_SIDE_EFFECTS__ */ (jsonSchema) => {
  const anySchema = json;
  for (let i = 0; i < unsupportedKeywords.length; i++) {
    const keyword = unsupportedKeywords[i];
    if (jsonSchema[keyword] !== U) {
      throw new SuryError({
        code: "invalid_operation",
        path: pathEmpty,
        reason: `Unsupported JSON Schema keyword: ${keyword}. Ignoring it would accept data the schema rejects \u2014 remove it, or express the constraint with S.refine on the result`
      });
    }
  }
  const jsonDefinitionToSchema = (definition) => {
    if (typeof definition !== "boolean") {
      return /* @__PURE__ */ fromJSONSchema(definition);
    } else if (definition === true) {
      return anySchema;
    } else {
      return never_;
    }
  };
  let schema;
  if (jsonSchema.nullable) {
    schema = null_(/* @__PURE__ */ fromJSONSchema(jsonSchemaMerge(jsonSchema, { nullable: false })));
  } else if (jsonSchema.type === "object") {
    if (jsonSchema.properties !== U) {
      const properties = jsonSchema.properties;
      const obj = /* @__PURE__ */ Object.create(null);
      Object.keys(properties).forEach((key) => {
        const property = properties[key];
        let propertySchema = jsonDefinitionToSchema(property);
        if (!jsonSchema.required?.includes(key)) {
          const defaultValue = definitionToDefaultValue(property);
          if (defaultValue !== U) {
            propertySchema = Option_getOr(option(propertySchema), defaultValue);
          } else {
            propertySchema = option(propertySchema);
          }
        }
        obj[key] = propertySchema;
      });
      schema = definitionToSchema(obj);
      if (jsonSchema.additionalProperties === false) {
        schema = strict(schema);
      }
    } else {
      const additionalProperties = jsonSchema.additionalProperties;
      if (additionalProperties !== U) {
        if (additionalProperties === true) {
          schema = dictFactory(anySchema);
        } else if (additionalProperties === false) {
          schema = strict(schemaObject(() => {
          }));
        } else {
          schema = dictFactory(/* @__PURE__ */ fromJSONSchema(additionalProperties));
        }
      } else {
        schema = schemaFactory({});
      }
    }
  } else if (jsonSchema.type === "array") {
    if (jsonSchema.prefixItems !== U) {
      const prefixItems = jsonSchema.prefixItems;
      schema = schemaTuple(
        (s2) => prefixItems.map((d, idx) => s2.item(idx, jsonDefinitionToSchema(d)))
      );
    } else if (jsonSchema.items !== U) {
      const items = jsonSchema.items;
      if (Array.isArray(items)) {
        schema = schemaTuple(
          (s2) => items.map((d, idx) => s2.item(idx, jsonDefinitionToSchema(d)))
        );
      } else {
        schema = array(jsonDefinitionToSchema(items));
      }
    } else {
      schema = array(anySchema);
    }
    if (jsonSchema.minItems !== U) {
      schema = applyBound(schema, minLength, jsonSchema.minItems);
    }
    if (jsonSchema.maxItems !== U) {
      schema = applyBound(schema, maxLength, jsonSchema.maxItems);
    }
  } else if (jsonSchema.anyOf !== U) {
    const definitions = jsonSchema.anyOf;
    if (definitions.length === 0) {
      schema = anySchema;
    } else if (definitions.length === 1) {
      schema = jsonDefinitionToSchema(definitions[0]);
    } else {
      schema = unionFactory(definitions.map(jsonDefinitionToSchema));
    }
  } else if (jsonSchema.enum !== U) {
    const primitives = jsonSchema.enum;
    if (primitives.length === 0) {
      schema = anySchema;
    } else if (primitives.length === 1) {
      schema = primitiveToSchema(primitives[0]);
    } else {
      schema = unionFactory(primitives.map(primitiveToSchema));
    }
  } else if (jsonSchema.const !== U) {
    schema = primitiveToSchema(jsonSchema.const);
  } else if (Array.isArray(jsonSchema.type)) {
    const types = jsonSchema.type;
    schema = unionFactory(types.map((type) => /* @__PURE__ */ fromJSONSchema(jsonSchemaMerge(jsonSchema, { type }))));
  } else if (jsonSchema.type === "string") {
    if (jsonSchema.format === "email") {
      schema = email;
    } else if (jsonSchema.format === "uri") {
      schema = url;
    } else if (jsonSchema.format === "uuid") {
      schema = uuid;
    } else if (jsonSchema.format === "date-time") {
      schema = isoDateTime;
    } else {
      schema = string;
    }
    if (jsonSchema.pattern !== U) {
      schema = pattern(schema, new RegExp(jsonSchema.pattern));
    }
    if (jsonSchema.minLength !== U) {
      schema = applyBound(schema, minLength, jsonSchema.minLength);
    }
    if (jsonSchema.maxLength !== U) {
      schema = applyBound(schema, maxLength, jsonSchema.maxLength);
    }
  } else if (jsonSchema.type === "integer") {
    schema = toIntSchema(jsonSchema);
  } else if (jsonSchema.type === "number" && jsonSchema.format === "int64") {
    schema = toIntSchema(jsonSchema);
  } else if (jsonSchema.type === "number" && jsonSchema.multipleOf === 1) {
    schema = toIntSchema(jsonSchema);
  } else if (jsonSchema.type === "number") {
    schema = withNumericBounds(float, jsonSchema);
  } else if (jsonSchema.type === "boolean") {
    schema = bool;
  } else if (jsonSchema.type === "null") {
    schema = schemaFactory(null);
  } else if (jsonSchema.type !== U) {
    throw new SuryError({
      code: "invalid_operation",
      path: pathEmpty,
      reason: `Unsupported JSON Schema type: ${jsonSchema.type}`
    });
  } else {
    const guarded = [];
    for (let i = 0; i < keywordTypes.length; i++) {
      const [type, keywords] = keywordTypes[i];
      if (keywords.some((k) => jsonSchema[k] !== U)) {
        guarded.push([type, /* @__PURE__ */ fromJSONSchema(jsonSchemaMerge(jsonSchema, { type }))]);
      }
    }
    schema = guarded.length === 0 ? anySchema : refine(
      anySchema,
      (data) => {
        const type = jsonTypeOf(data);
        return guarded.every(
          ([guardType, guardSchema]) => type !== guardType || passesSchema(data, guardSchema)
        );
      },
      "Should pass the schema's assertion keywords for its type."
    );
  }
  if (jsonSchema.allOf !== U) {
    const definitions = jsonSchema.allOf;
    const schemas = definitions.map(jsonDefinitionToSchema);
    if (schemas.length > 0) {
      schema = refineInput(
        schema,
        (data) => schemas.every((s2) => passesSchema(data, s2)),
        "Should pass for all schemas of the allOf property."
      );
    }
  }
  if (jsonSchema.oneOf !== U) {
    const definitions = jsonSchema.oneOf;
    const schemas = definitions.map(jsonDefinitionToSchema);
    if (schemas.length > 0) {
      schema = refineInput(
        schema,
        (data) => schemas.filter((s2) => passesSchema(data, s2)).length === 1,
        "Should pass exactly one schema according to the oneOf property."
      );
    }
  }
  if (jsonSchema.not !== U) {
    const notSchema = jsonDefinitionToSchema(jsonSchema.not);
    schema = refineInput(
      schema,
      (data) => !passesSchema(data, notSchema),
      "Should NOT be valid against schema in the not property."
    );
  }
  if (jsonSchema.if !== U) {
    const ifSchema = jsonDefinitionToSchema(jsonSchema.if);
    const thenSchema = jsonSchema.then !== U ? jsonDefinitionToSchema(jsonSchema.then) : U;
    const elseSchema = jsonSchema.else !== U ? jsonDefinitionToSchema(jsonSchema.else) : U;
    schema = refineInput(
      schema,
      (data) => {
        const branch = passesSchema(data, ifSchema) ? thenSchema : elseSchema;
        return branch === U || passesSchema(data, branch);
      },
      "Should pass the if/then/else schema validation."
    );
  }
  if (jsonSchema.description !== U || jsonSchema.deprecated !== U || jsonSchema.examples !== U || jsonSchema.title !== U) {
    schema = meta(schema, {
      title: jsonSchema.title,
      description: jsonSchema.description,
      deprecated: jsonSchema.deprecated,
      examples: jsonSchema.examples
    });
  }
  return schema;
};
export {
  Metadata_Id_make as $res_Metadata_Id_make,
  Metadata_get as $res_Metadata_get,
  Metadata_set as $res_Metadata_set,
  Option_getOr as $res_Option_getOr,
  Option_getOrWith as $res_Option_getOrWith,
  assertAsyncOrThrow as $res_assertAsyncOrThrow,
  nullAsOption as $res_nullAsOption,
  nullAsUnit as $res_nullAsUnit,
  nullableAsOption as $res_nullableAsOption,
  option as $res_option,
  pathConcat as $res_pathConcat,
  pathFromArray as $res_pathFromArray,
  pathFromLocation as $res_pathFromLocation,
  pathToArray as $res_pathToArray,
  schemaDefiner as $res_schema,
  __setExnId as $res_setExnId,
  unit as $res_unit,
  errorClass as Error,
  unknown as any,
  array,
  js_assert as assert,
  js_asyncDecoder as asyncDecoder,
  js_asyncDecoderAssert as asyncDecoderAssert,
  js_asyncEncoder as asyncEncoder,
  js_asyncParser as asyncParser,
  bigint,
  bool,
  bool as boolean,
  brand,
  compactColumns,
  cuid,
  date,
  getDecoder as decoder,
  deepStrict,
  deepStrip,
  dictFactory as dict,
  email,
  empty,
  enableStandardJSONSchema,
  js_encoder as encoder,
  enum_ as enum,
  extendJSONSchema,
  float,
  fromJSONSchema,
  global,
  gt,
  gte,
  inputExpression,
  instance,
  int,
  int as int32,
  js_is as is,
  isoDateTime,
  json,
  jsonString,
  jsonStringWithSpace,
  length,
  list,
  schemaFactory as literal,
  lt,
  lte,
  maxLength,
  js_merge as merge,
  meta,
  minLength,
  nan,
  never_ as never,
  noValidation,
  nonEmpty,
  js_nullable as nullable,
  nullable as nullish,
  float as number,
  schemaObject as object,
  js_optional as optional,
  outputExpression,
  js_parser as parser,
  pattern,
  port,
  dictFactory as record,
  recursive,
  js_refine as refine,
  reverse,
  js_safe as safe,
  js_safeAsync as safeAsync,
  schemaFactory as schema,
  schemaShape as shape,
  strict,
  string,
  strip,
  symbol,
  js_to as to,
  toJSONSchema,
  trim,
  schemaTuple as tuple,
  uint8Array,
  js_union as union,
  unknown,
  url,
  uuid,
  void_ as void
};
