// `S.json` and its string form. A recursive union of the JSON types, plus the
// encoder/decoder that represent an arbitrary schema as JSON — the only schema
// that rewrites another schema's shape rather than just validating it.

import {
  anyOfTag,
  arrayTag,
  baseSchema,
  type Builder,
  cached,
  copySchema,
  defsPath,
  type Encoder,
  flagUnsafeHas,
  inlinedValueFromString,
  type Internal,
  isLiteral,
  jsonName,
  refTag,
  stringTag,
  type Tag,
  tagFlagArray,
  tagFlagBigint,
  tagFlagBoolean,
  tagFlagNaN,
  tagFlagNull,
  tagFlagNumber,
  tagFlagObject,
  tagFlagRef,
  tagFlags,
  tagFlagString,
  tagFlagUndefined,
  tagFlagUnion,
  tagFlagUnknown,
  U,
  undefinedTag,
  unknown,
  unknownTag,
  updateOutput,
  type Val,
} from "../base";
import {
  _var,
  B_addObjectField,
  B_embedInvalidInput,
  B_next,
  B_nextConst,
  B_refine,
  B_unsupportedDecode,
  B_varWithoutAllocation,
} from "../builder";
import {
  array,
  arrayDecoder,
  arrayExpression,
  completeObjectVal,
  dictFactory,
  makeObjectVal,
  valGet,
} from "../composites";
import { getOutputSchema, parse } from "../parse";
import {
  bool,
  float,
  inputToString,
  literalDecoder,
  nullLiteral,
  string,
  stringDecoderFn,
} from "../primitives";
import { unionDecoder, unionExpression, unionFactory, unionRewriteTo } from "../union";
import { recursiveDecoder } from "./recursive";

export const jsonEncoderFn = (input: Val, target: Internal): Val => {
  const toTagFlag = tagFlags[target.type]!;

  if (
    flagUnsafeHas(
      toTagFlag,
      tagFlagString | tagFlagBoolean | tagFlagNumber | tagFlagNull,
    )
  ) {
    return parse(B_refine(input, unknown, U, target));
  } else if (flagUnsafeHas(toTagFlag, (tagFlagUndefined | tagFlagNaN))) {
    const jsonExpected = copySchema(nullLiteral());
    jsonExpected.to = target;
    return parse(B_refine(input, unknown, U, jsonExpected));
  } else if (flagUnsafeHas(toTagFlag, tagFlagArray)) {
    // Validate that the input is an array
    // and then update the schema to be an array of json instead of array of unknown
    const jsonExpected = array(unknown);
    const output = parse(B_refine(input, unknown, U, jsonExpected));
    output.s.additionalItems = json();
    output.e = target;
    output.io = false;
    return output;
  } else if (flagUnsafeHas(toTagFlag, tagFlagObject)) {
    // Validate that the input is an object
    // and then update the schema to be an object of json instead of object of unknown
    const jsonExpected = dictFactory(unknown);
    const output = parse(B_refine(input, unknown, U, jsonExpected));
    output.s.additionalItems = json();
    output.e = target;
    output.io = false;
    return output;
  } else if (flagUnsafeHas(toTagFlag, (tagFlagUnion | tagFlagRef))) {
    return input;
  } else {
    // For non-JSON types (bigint, instance, etc.), decode through string
    const jsonExpected = copySchema(string());
    jsonExpected.to = target;
    return parse(B_refine(input, unknown, U, jsonExpected));
  }
}

export const isJsonable = (schema: Internal): boolean => {
  const tagFlag = tagFlags[schema.type]!;
  return (
    flagUnsafeHas(
      tagFlag,
      tagFlagString | tagFlagNumber | tagFlagBoolean | tagFlagNull,
    ) ||
    schema["$ref"] === json()["$ref"] ||
    (flagUnsafeHas(tagFlag, tagFlagUnion) && schema.anyOf!.every(isJsonable)) ||
    (flagUnsafeHas(tagFlag, tagFlagArray) &&
      (typeof schema.additionalItems === "object" ? isJsonable(schema.additionalItems) : true) &&
      schema.items!.every(isJsonable)) ||
    (flagUnsafeHas(tagFlag, tagFlagObject) &&
      (typeof schema.additionalItems === "object" ? isJsonable(schema.additionalItems) : true) &&
      Object.values(schema.properties!).every(isJsonable))
  );
}

export const jsonDecoderFn = (input: Val): Val => {
  const inputTagFlag = tagFlags[input.s.type]!;

  if (isJsonable(input.s)) {
    return input;
  } else if (flagUnsafeHas(inputTagFlag, (tagFlagUndefined | tagFlagNaN))) {
    return B_nextConst(input, nullLiteral());
  } else if (flagUnsafeHas(inputTagFlag, tagFlagArray)) {
    const expected = baseSchema(arrayTag, false);
    expected.items = input.s.items!.map((_) => json());
    expected.decoder = arrayDecoder;
    expected.x = arrayExpression;
    expected.additionalItems =
      typeof input.s.additionalItems === "object"
        ? json()
        : input.s.additionalItems;
    expected.to = input.e.to;
    return parse(B_refine(input, U, U, expected));
  } else if (flagUnsafeHas(inputTagFlag, tagFlagObject)) {
    if (typeof input.s.additionalItems === "object") {
      const expected = dictFactory(json());
      expected.to = input.e.to;
      return parse(B_refine(input, U, U, expected));
    } else {
      const jsonVal = makeObjectVal(input, input.s);
      jsonVal.e = json();
      if (input.e.to) {
        jsonVal.e = copySchema(jsonVal.e);
        jsonVal.e.to = input.e.to;
      }

      const keys = Object.keys(input.s.properties!);
      for (let idx = 0; idx <= keys.length - 1; idx++) {
        const key = keys[idx]!;
        const itemVal = valGet(input, key);
        itemVal.io = false;

        if (itemVal.s.type === anyOfTag && itemVal.s.has![undefinedTag]) {
          // Per-variant conversion instead of a generic `undefined | JSON`
          // check: an undefined variant stays undefined so the object
          // rebuild omits the field, while non-jsonable variants get
          // `.to(json)` appended and keep converting recursively (#311)
          const mapped = unionFactory(
            itemVal.s.anyOf!.map((variant) => {
              const variantOutput = getOutputSchema(variant);
              return variantOutput.type === undefinedTag || isJsonable(variantOutput)
                ? variant
                : updateOutput<Internal>(variant, (mut) => {
                    mut.to = json();
                  });
            })
          );
          // Already resolved variant by variant, so the union encoder pairs them
          // by position instead of re-matching them by type.
          mapped.perVariant = true;
          itemVal.e = mapped;
          const itemOutput = parse(itemVal);
          itemOutput.o = true;
          B_addObjectField(jsonVal, key, itemOutput);
        } else {
          itemVal.e = json();
          B_addObjectField(jsonVal, key, parse(itemVal));
        }
      }

      return completeObjectVal(jsonVal);
    }
  } else if (flagUnsafeHas(inputTagFlag, tagFlagRef)) {
    // FIXME: Should be a unified solution for ref inputs
    return recursiveDecoder(input);
  } else if (
    flagUnsafeHas(inputTagFlag, tagFlagUnion) &&
    // Union-tagged schemas always carry `anyOf` and `has`
    // (set by unionFactory, reverse and the S.json def).
    // Unions with an undefined variant are not supported,
    // since undefined is not representable in JSON
    !(undefinedTag in input.s.has!)
  ) {
    // Decode each union variant to JSON separately
    return parse(unionRewriteTo(input, input.e));
  } else if (flagUnsafeHas(inputTagFlag, tagFlagUnknown)) {
    const to = input.e.to!;
    // Whether we can optimize encoding during decoding
    // FIXME: should this also check !input.e.refiner, like jsonStringDecoder's preEncode does?
    const preEncode: boolean = !!to && !input.e.parser;
    if (preEncode) {
      input.s = json();
      return jsonEncoderFn(input, input.e);
    } else if (input.e.noValidation!) {
      input.s = json();
      return input;
    } else {
      return recursiveDecoder(input);
    }
  } else {
    try {
      const expected = copySchema(string());
      expected.to = input.e;
      input.e = expected;
      return parse(input);
    } catch {
      return B_unsupportedDecode(input, input.s, json());
    }
  }
}

export const json = (): Internal => {
  return cached(jsonName, refTag, (s) => {
    const jsonRef = baseSchema(refTag, true);
    jsonRef["$ref"] = `${defsPath}${jsonName}`;
    jsonRef.name = jsonName;

    jsonRef.decoder = jsonDecoderFn;
    const jsonEncoder = jsonEncoderFn;
    jsonRef.encoder = jsonEncoder;

    s["$ref"] = jsonRef["$ref"];
    s.name = jsonName;
    s.decoder = jsonDecoderFn;
    s.encoder = jsonEncoder;

    const anyOf = [
      string(),
      bool(),
      float(),
      nullLiteral(),
      dictFactory(jsonRef),
      array(jsonRef),
    ];
    const has: Partial<Record<Tag, boolean>> = {};
    anyOf.forEach((schema) => {
      has[schema.type] = true;
    });

    const jsonDef = baseSchema(anyOfTag, true);
    jsonDef.anyOf = anyOf;
    jsonDef.has = has;
    jsonDef.decoder = unionDecoder;
    jsonDef.name = jsonName;
    jsonDef.type = anyOfTag;
    // Renders as `name`, never through this — set only to keep the enumerable
    // field count `unionIsTransparent` matches on.
    jsonDef.x = unionExpression;

    const defs: Record<string, Internal> = {};
    defs[jsonName] = jsonDef;
    s["$defs"] = defs;
  });
}

export const jsonString = /* @__PURE__ */ (() => {
  const inlineJsonString = (input: Val, schema: Internal): string => {
    const tagFlag = tagFlags[schema.type]!;
    const const_ = schema.const;
    if (flagUnsafeHas(tagFlag, (tagFlagUndefined | tagFlagNull))) {
      return `"null"`;
    } else if (flagUnsafeHas(tagFlag, tagFlagString)) {
      return JSON.stringify(inlinedValueFromString(const_ as string));
    } else if (flagUnsafeHas(tagFlag, tagFlagBigint)) {
      return `"\\"${const_}\\""`;
    } else if (flagUnsafeHas(tagFlag, (tagFlagNumber | tagFlagBoolean))) {
      return `"${const_}"`;
    } else {
      return B_unsupportedDecode(input, schema, input.e);
    }
  };

  const constSchemaToJsonStringConst = (input: Val, target: Internal): string => {
    const tagFlag = tagFlags[target.type]!;
    const const_ = target.const;
    if (flagUnsafeHas(tagFlag, (tagFlagUndefined | tagFlagNull))) {
      return `null`;
    } else if (flagUnsafeHas(tagFlag, tagFlagString)) {
      return inlinedValueFromString(const_ as string);
    } else if (flagUnsafeHas(tagFlag, tagFlagBigint)) {
      return `"${const_}"`;
    } else if (flagUnsafeHas(tagFlag, (tagFlagNumber | tagFlagBoolean))) {
      return "" + const_;
    } else {
      return B_unsupportedDecode(input, input.s, target);
    }
  };

  const jsonStringEncoder: Encoder = (input, target) => {
    if (target.format !== "json") {
      if (isLiteral(target)) {
        const jsonStringConstSchema = baseSchema(stringTag, true);
        jsonStringConstSchema.const = constSchemaToJsonStringConst(input, target);
        jsonStringConstSchema.to = target;
        jsonStringConstSchema.decoder = literalDecoder;
        return B_refine(input, U, U, jsonStringConstSchema);
      } else {
        const outputVar = B_varWithoutAllocation(input.g);

        const nextSchema = copySchema(json());
        nextSchema.to = target;

        const output = B_next(input, outputVar, nextSchema, nextSchema);
        output.io = true;
        output.v = _var;

        const inputVar = input.v();
        output.cp = `let ${outputVar};try{${outputVar}=JSON.parse(${inputVar})}catch(t){${B_embedInvalidInput(
          input,
          input.s,
        )}}`;

        return output;
      }
    } else {
      return input;
    }
  };

  const jsonStringDecoder: Builder = (input) => {
    const inputTagFlag = tagFlags[input.s.type]!;
    const expectedSchema = input.e;

    if (flagUnsafeHas(inputTagFlag, tagFlagUnknown)) {
      const to = expectedSchema.to!;
      // Whether we can optimize encoding during decoding
      const preEncode: boolean =
        !!to && to.type !== unknownTag && !expectedSchema.parser && !expectedSchema.refiner;

      const stringVal = stringDecoderFn(input);
      stringVal.s = expectedSchema;
      stringVal.e = expectedSchema;

      if (preEncode) {
        return jsonStringEncoder(stringVal, to);
      } else {
        const stringVar = stringVal.v();
        const output = B_refine(stringVal, expectedSchema);
        output.cp = `try{JSON.parse(${stringVar})}catch(t){${B_embedInvalidInput(
          stringVal,
        )}}`;
        return output;
      }
    } else if (input.s.format === "json") {
      return input;
    } else if (isLiteral(input.s)) {
      return B_next(input, inlineJsonString(input, input.s), expectedSchema);
    } else if (flagUnsafeHas(inputTagFlag, tagFlagString)) {
      return B_next(input, `JSON.stringify(${input.i})`, expectedSchema);
    } else if (flagUnsafeHas(inputTagFlag, (tagFlagNumber | tagFlagBoolean))) {
      const output = inputToString(input);
      output.s = expectedSchema;
      return output;
    } else if (flagUnsafeHas(inputTagFlag, tagFlagBigint)) {
      return B_next(input, `"\\""+${input.i}+"\\""`, expectedSchema);
    } else if (flagUnsafeHas(inputTagFlag, (tagFlagObject | tagFlagArray))) {
      const jsonVal = parse(B_refine(input, U, U, json()));
      return B_next(
        jsonVal,
        `JSON.stringify(${jsonVal.i}${
          expectedSchema.space === 0 || expectedSchema.space === U
            ? ""
            : `,null,${expectedSchema.space}`
        })`,
        expectedSchema,
        expectedSchema,
      );
    } else {
      return B_unsupportedDecode(input, input.s, expectedSchema);
    }
  };

  return (): Internal =>
    cached("json", stringTag, (s) => {
      s.format = "json";
      s.name = `${jsonName} string`;
      s.encoder = jsonStringEncoder;
      s.decoder = jsonStringDecoder;
    });
})();

// @__NO_SIDE_EFFECTS__
export const jsonStringWithSpace = (space: number): Internal => {
  const mut = copySchema(jsonString());
  mut.space = space;
  return mut;
}
