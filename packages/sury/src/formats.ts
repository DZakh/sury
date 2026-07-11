import { defsPath, recursiveDecoder, transform } from "./operations";
import { array, arrayDecoder, completeObjectVal, dictFactory, makeObjectVal, unionDecoder, unionFactory, unionPerVariantVal, valGet } from "./composites";
import { bool, float, inputToString, jsonName, literalDecoder, nullLiteral, numberDecoder, string, stringDecoderFn, unit } from "./primitives";
import { baseSchema, cached, copySchema, unknown } from "./schema";
import { B_addObjectField, B_embed, B_embedInvalidInput, B_failWithErrorMessage, B_next, B_nextConst, B_refine, B_unsupportedDecode, B_varWithoutAllocation, _var, failInvalidType } from "./builder";
import { getDecoder, instanceDecoder, parse, reverse } from "./parse";
import { Internal, SchemaErrorMessage, Val, isLiteral } from "./types";
import { Builder, Encoder } from "./builder";
import { flagUnsafeHas } from "./flags";
import { inlinedValueFromString } from "./path";
import { Tag, arrayTag, instanceTag, numberTag, refTag, stringTag, tagFlagArray, tagFlagBigint, tagFlagBoolean, tagFlagInstance, tagFlagNaN, tagFlagNull, tagFlagNumber, tagFlagObject, tagFlagRef, tagFlagString, tagFlagUndefined, tagFlagUnion, tagFlagUnknown, tagFlags, undefinedTag, unionTag, unknownTag } from "./tags";

export const jsonEncoderFn = (input: Val, target: Internal): Val => {
  const toTagFlag = tagFlags[target.type]!;

  if (
    flagUnsafeHas(
      toTagFlag,
      tagFlagString | tagFlagBoolean | tagFlagNumber | tagFlagNull,
    )
  ) {
    return parse(B_refine(input, unknown, undefined, target));
  } else if (flagUnsafeHas(toTagFlag, (tagFlagUndefined | tagFlagNaN))) {
    const jsonExpected = copySchema(nullLiteral());
    jsonExpected.to = target;
    return parse(B_refine(input, unknown, undefined, jsonExpected));
  } else if (flagUnsafeHas(toTagFlag, tagFlagArray)) {
    // Validate that the input is an array
    // and then update the schema to be an array of json instead of array of unknown
    const jsonExpected = array(unknown);
    const output = parse(B_refine(input, unknown, undefined, jsonExpected));
    output.s.additionalItems = json();
    output.e = target;
    output.io = false;
    return output;
  } else if (flagUnsafeHas(toTagFlag, tagFlagObject)) {
    // Validate that the input is an object
    // and then update the schema to be an object of json instead of object of unknown
    const jsonExpected = dictFactory(unknown);
    const output = parse(B_refine(input, unknown, undefined, jsonExpected));
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
    return parse(B_refine(input, unknown, undefined, jsonExpected));
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
    expected.additionalItems =
      typeof input.s.additionalItems === "object"
        ? json()
        : input.s.additionalItems;
    expected.to = input.e.to;
    return parse(B_refine(input, undefined, undefined, expected));
  } else if (flagUnsafeHas(inputTagFlag, tagFlagObject)) {
    if (typeof input.s.additionalItems === "object") {
      const expected = dictFactory(json());
      expected.to = input.e.to;
      return parse(B_refine(input, undefined, undefined, expected));
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

        if (itemVal.s.type === unionTag && itemVal.s.has![undefinedTag]) {
          itemVal.e = unionFactory([unit(), json()]);
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
    return parse(unionPerVariantVal(input, input.e));
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

    const jsonDef = baseSchema(unionTag, true);
    jsonDef.anyOf = anyOf;
    jsonDef.has = has;
    jsonDef.decoder = unionDecoder;
    jsonDef.name = jsonName;
    jsonDef.type = unionTag;

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
        return B_refine(input, undefined, undefined, jsonStringConstSchema);
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
      const jsonVal = parse(B_refine(input, undefined, undefined, json()));
      return B_next(
        jsonVal,
        `JSON.stringify(${jsonVal.i}${
          expectedSchema.space === 0 || expectedSchema.space === undefined
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

export const jsonStringWithSpace = (space: number): Internal => {
  const mut = copySchema(jsonString());
  mut.space = space;
  return mut;
}

export const uint8Array = (): Internal => {
  return cached("u", instanceTag, (s) => {
    s.class = Uint8Array;
    s.decoder = (inputArg: Val): Val => {
      const inputTagFlag = tagFlags[inputArg.s.type]!;
      let input = inputArg;

      if (flagUnsafeHas(inputTagFlag, tagFlagString)) {
        input = B_next(
          input,
          `${B_embed(input, new TextEncoder())}.encode(${input.i})`,
          s,
        );
      } else if (flagUnsafeHas(inputTagFlag, (tagFlagUnknown | tagFlagInstance))) {
        input = instanceDecoder(input);
      }

      if (inputArg.e.to !== undefined && inputArg.e.parser === undefined) {
        const to = inputArg.e.to;
        const toTagFlag = tagFlags[to.type]!;
        if (flagUnsafeHas(toTagFlag, tagFlagString)) {
          input = B_next(
            input,
            `${B_embed(input, new TextDecoder())}.decode(${input.i})`,
            string(),
          );
        }
        return input;
      } else {
        return input;
      }
    };
  });
}

export const isoDateTime = (): Internal => {
  return cached("date-time", stringTag, (s) => {
    const datetimeRe = /^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(\.\d+)?Z$/;
    s.decoder = stringDecoderFn;
    s.format = "date-time";
    s.refiner = (input) => {
      return [
        {
          c: (inputVar) => `${B_embed(input, datetimeRe)}.test(${inputVar})`,
          f: B_failWithErrorMessage(
            "format",
            "Invalid datetime string! Expected UTC",
          ),
        },
      ];
    };
  });
}

export const port = (): Internal => {
  return cached("port", numberTag, (s) => {
    s.decoder = numberDecoder;
    s.format = "port";
    s.refiner = (_input) => {
      return [
        {
          c: (inputVar) => `${inputVar}>0&&${inputVar}<65536&&${inputVar}%1===0`,
          f: B_failWithErrorMessage("format"),
        },
      ];
    };
  });
}

export const email = (): Internal => {
  return cached("email", stringTag, (s) => {
    const emailRegex = /^(?!\.)(?!.*\.\.)([A-Z0-9_'+\-\.]*)[A-Z0-9_+-]@([A-Z0-9][A-Z0-9\-]*\.)+[A-Z]{2,}$/i;
    s.decoder = stringDecoderFn;
    s.format = "email";
    s.refiner = (input) => {
      return [
        {
          c: (inputVar) => `${B_embed(input, emailRegex)}.test(${inputVar})`,
          f: B_failWithErrorMessage("format"),
        },
      ];
    };
  });
}

export const uuid = (): Internal => {
  return cached("uuid", stringTag, (s) => {
    const uuidRegex = /^[0-9a-fA-F]{8}\b-[0-9a-fA-F]{4}\b-[0-9a-fA-F]{4}\b-[0-9a-fA-F]{4}\b-[0-9a-fA-F]{12}$/i;
    s.decoder = stringDecoderFn;
    s.format = "uuid";
    s.refiner = (input) => {
      return [
        {
          c: (inputVar) => `${B_embed(input, uuidRegex)}.test(${inputVar})`,
          f: B_failWithErrorMessage("format"),
        },
      ];
    };
  });
}

export const cuid = (): Internal => {
  return cached("cuid", stringTag, (s) => {
    const cuidRegex = /^c[^\s-]{8,}$/i;
    s.decoder = stringDecoderFn;
    s.format = "cuid";
    s.refiner = (input) => {
      return [
        {
          c: (inputVar) => `${B_embed(input, cuidRegex)}.test(${inputVar})`,
          f: B_failWithErrorMessage("format"),
        },
      ];
    };
  });
}

export const url = (): Internal => {
  return cached("url", stringTag, (s) => {
    const urlValidator = (s: string) => {
      try {
        new URL(s);
        return true;
      } catch {
        return false;
      }
    };
    s.decoder = stringDecoderFn;
    s.format = "url";
    s.refiner = (input) => {
      return [
        {
          c: (inputVar) => `${B_embed(input, urlValidator)}(${inputVar})`,
          f: B_failWithErrorMessage("format"),
        },
      ];
    };
  });
}

export const invalidDateRefine = (input: Val): Val => {
  return B_refine(input, input.e, [
    {
      c: (inputVar) => `!Number.isNaN(${inputVar}.getTime())`,
      f: failInvalidType,
    },
  ]);
}

export const date = (): Internal => {
  return cached(instanceTag, instanceTag, (s) => {
    s.class = Date;
    s.decoder = (input: Val): Val => {
      const inputTagFlag = tagFlags[input.s.type]!;
      if (flagUnsafeHas(inputTagFlag, tagFlagString)) {
        return invalidDateRefine(B_next(input, `new Date(${input.i})`, s));
      } else if (flagUnsafeHas(inputTagFlag, tagFlagUnknown)) {
        return invalidDateRefine(instanceDecoder(input));
      } else if (flagUnsafeHas(inputTagFlag, tagFlagInstance) && input.s.class === s.class) {
        return input;
      } else {
        return B_unsupportedDecode(input, input.s, input.e);
      }
    };

    // Encoder: Date → string (via toISOString) when target is string
    s.encoder = (input, target) => {
      const toTagFlag = tagFlags[target.type]!;
      if (flagUnsafeHas(toTagFlag, tagFlagString)) {
        const dateTimeString = baseSchema(stringTag, false);
        dateTimeString.format = "date-time";
        return parse(
          B_next(input, `${input.i}.toISOString()`, dateTimeString, target),
        );
      } else {
        return input;
      }
    };
  });
}

// PORT-NOTE: ReScript list runtime (v12): empty list = `0`, cons cell =
// `{hd, tl}`. These two helpers replicate Stdlib List.fromArray / List.toArray
// exactly for that representation.
type RescriptList = 0 | { hd: unknown; tl: RescriptList };

const listFromArray = (array: unknown[]): RescriptList => {
  let list: RescriptList = 0;
  for (let i = array.length - 1; i >= 0; i--) {
    list = { hd: array[i], tl: list };
  }
  return list;
}

const listToArray = (list: RescriptList): unknown[] => {
  const array: unknown[] = [];
  let current = list;
  while (current !== 0) {
    array.push(current.hd);
    current = current.tl;
  }
  return array;
}

export const list = (schema: Internal): Internal => {
  return transform(array(schema), (_: unknown) => ({
    p: (array: unknown) => listFromArray(array as unknown[]),
    s: (list: unknown) => listToArray(list as RescriptList),
  }));
}

export type Meta<Value> = {
  name?: string;
  title?: string;
  description?: string;
  deprecated?: boolean;
  examples?: Value[];
  errorMessage?: SchemaErrorMessage;
};

// TODO: Better test reverse
export const meta = <Value>(schema: Internal, data: Meta<Value>): Internal => {
  const mut = copySchema(schema);
  if (data.name !== undefined) {
    if (data.name === "") {
      mut.name = undefined;
    } else {
      mut.name = data.name;
    }
  }
  if (data.title !== undefined) {
    if (data.title === "") {
      mut.title = undefined;
    } else {
      mut.title = data.title;
    }
  }
  if (data.description !== undefined) {
    if (data.description === "") {
      mut.description = undefined;
    } else {
      mut.description = data.description;
    }
  }
  if (data.deprecated !== undefined) {
    mut.deprecated = data.deprecated;
  }
  if (data.examples !== undefined) {
    if (data.examples.length === 0) {
      delete mut.examples;
    } else {
      mut.examples = data.examples.map(getDecoder(reverse(schema)));
    }
  }
  if (data.errorMessage !== undefined) {
    const em = data.errorMessage;
    if (Object.keys(em).length === 0) {
      mut.errorMessage = undefined;
    } else {
      mut.errorMessage = em;
    }
  }
  return mut;
}

export const brand = (schema: Internal, id: string): Internal => {
  const mut = copySchema(schema);
  mut.name = id;
  return mut;
}
