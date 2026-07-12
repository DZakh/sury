import { nullLiteral, unit } from "./primitives";
import { getMutErrorMessage, internalRefine, nullAsUnit, transform } from "./operations";
import { schemaObject, schemaShape, schemaTuple } from "./factory";
import { parse } from "./parse";
import { SuryError, copySchema, panic, unknown } from "./schema";
import { B_scope, B_asyncVal, B_embed, B_failWithErrorMessage, B_inlineLocation, B_markOutput, B_merge, B_next, B_refine, B_varWithoutAllocation, Builder, _notVarBeforeValidation, _var, failInvalidType } from "./builder";
import { array, dictFactory, optionFactory, unionFactory } from "./composites";
import { Internal, Val, stringify } from "./types";
import { flagUnsafeHas, valFlagAsync } from "./flags";
import { inlinedValueFromString, pathEmpty, pathFromInlinedLocation } from "./path";
import { numberTag, tagFlagUnknown, tagFlags } from "./tags";

export const compactColumnsDecoder: Builder = (input: Val) => {
  const selfSchema = input.e;
  const isUnknownInput = flagUnsafeHas(tagFlags[input.s.type]!, tagFlagUnknown);

  // Declared source item type from selfSchema (the compactColumns schema);
  // used by both the forward and reverse directions below.
  const declaredItemSchema: Internal = (selfSchema.additionalItems as Internal)
    .additionalItems as Internal;

  // Find the object schema whose properties define the columns.
  // Forward (columnar → rows): props come from selfSchema.to.additionalItems.
  // Reverse (rows → columnar): props come from input.schema.additionalItems (the
  // object schema left over after the preceding parse pipeline step).
  let forwardProps: Record<string, Internal> | undefined;
  if (
    selfSchema.to !== undefined &&
    typeof selfSchema.to.additionalItems === "object"
  ) {
    forwardProps = (selfSchema.to.additionalItems as Internal).properties;
  } else {
    forwardProps = undefined;
  }
  const isForwardDirection = forwardProps !== undefined;
  let maybeProperties: Record<string, Internal> | undefined;
  if (isForwardDirection) {
    maybeProperties = forwardProps;
  } else {
    if (
      input.s.additionalItems !== undefined &&
      typeof input.s.additionalItems === "object"
    ) {
      maybeProperties = (input.s.additionalItems as Internal).properties;
    } else {
      maybeProperties = undefined;
    }
  }

  if (maybeProperties === undefined) {
    return panic(
      "S.compactColumns supports only object schemas. Use S.compactColumns(S.unknown)->S.to(S.array(objectSchema)).",
    );
  } else {
    const properties = maybeProperties;
    const keys = Object.keys(properties);
    const keysLen = keys.length;

    // Forward: output already matches selfSchema.to, reuse it so
    // markOutput picks up its refiner. selfSchema.to is Some here —
    // isForwardDirection reads through it above.
    // Reverse: runtime shape differs (array of arrays of unknown),
    // so build fresh and propagate .to for downstream steps.
    let outputSchema: Internal;
    if (isForwardDirection) {
      outputSchema = selfSchema.to!;
    } else {
      const s = array(array(unknown));
      s.to = selfSchema.to;
      outputSchema = s;
    }

    if (keysLen === 0) {
      if (isUnknownInput) {
        input = B_refine(input, undefined, [
          {
            c: (inputVar: string) =>
              `Array.isArray(${inputVar})&&${inputVar}.length===0`,
            f: failInvalidType,
          },
        ]);
      }
      const output = B_next(input, "[]", outputSchema, outputSchema);
      return B_markOutput(output, input);
    } else if (isForwardDirection) {
      // Forward direction: columnar → rows
      if (isUnknownInput) {
        input = B_refine(input, undefined, [
          {
            c: (inputVar: string) => {
              let check = `Array.isArray(${inputVar})&&${inputVar}.length===${keysLen}`;
              for (let idx = 0; idx <= keysLen - 1; ++idx) {
                check = check + `&&Array.isArray(${inputVar}[${idx}])`;
              }
              return check;
            },
            f: failInvalidType,
          },
        ]);
      }

      const inputVar = input.v();
      const iteratorVar = B_varWithoutAllocation(input.g);
      const outputVar = B_varWithoutAllocation(input.g);

      // Actual runtime item type: unknown for top-level parser, or
      // the typed source when the caller passed already-typed data.
      let runtimeItemSchema: Internal;
      if (isUnknownInput) {
        runtimeItemSchema = unknown;
      } else {
        const innerArray = input.s.additionalItems as Internal;
        runtimeItemSchema = innerArray.additionalItems as Internal;
      }

      let lengthCode = "";
      let itemBuildCode = "";
      let itemParseCode = "";
      let asyncInlines = "";
      let hasAsync = false;
      for (let idx = 0; idx <= keysLen - 1; ++idx) {
        const key = keys[idx]!;
        const idxStr = `${idx}`;
        const rawValueCode = `${inputVar}[${idxStr}][${iteratorVar}]`;

        const fieldSchema = properties[key]!;

        // When the declared source differs from the runtime type
        // (e.g. runtime=unknown, declared=json), chain through the
        // declared type first so parse validates the value matches
        // the source schema before converting to the field type.
        let itemExpected: Internal;
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

        // Path like ["bar"] so validation errors carry the field location.
        itemInput.path = pathFromInlinedLocation(B_inlineLocation(input.g, key));

        const itemOutput = parse(itemInput);
        if (flagUnsafeHas(itemOutput.f, valFlagAsync)) {
          hasAsync = true;
        }

        itemParseCode = itemParseCode + B_merge(itemOutput);
        lengthCode = lengthCode + `${inputVar}[${idxStr}].length,`;
        asyncInlines = asyncInlines + `${itemOutput.i},`;
        itemBuildCode =
          itemBuildCode + `${inlinedValueFromString(key)}:${itemOutput.i},`;
      }

      let output = B_next(input, outputVar, outputSchema, outputSchema);
      output.v = _var;
      // Row accumulator: declared at the head of its own segment, before the
      // `for` below that fills it.
      output.cp = `let ${outputVar}=new Array(Math.max(${lengthCode}));`;

      // Wrap the row body in a single try/catch that prepends the row index to
      // any thrown error — giving paths like ["0"]["bar"]. A single wrapper is
      // used (rather than per-field) so that `let` variables declared while
      // parsing one field remain in scope for the object construction.
      let rowAssign: string;
      if (hasAsync) {
        // For async fields, each row becomes a promise that awaits all field values
        // via Promise.all, and the final output is Promise.all of all row promises.
        const rowResultVar = B_varWithoutAllocation(input.g);
        let asyncBuildCode = "";
        for (let idx = 0; idx <= keysLen - 1; ++idx) {
          const key = keys[idx]!;
          asyncBuildCode =
            asyncBuildCode +
            `${inlinedValueFromString(key)}:${rowResultVar}[${idx}],`;
        }
        rowAssign = `${outputVar}[${iteratorVar}]=Promise.all([${asyncInlines}]).then(${rowResultVar}=>({${asyncBuildCode}}));`;
      } else {
        rowAssign = `${outputVar}[${iteratorVar}]={${itemBuildCode}};`;
      }

      const rowBody = itemParseCode + rowAssign;
      let wrappedBody: string;
      if (itemParseCode === "") {
        wrappedBody = rowBody;
      } else {
        const errorVar = B_varWithoutAllocation(input.g);
        wrappedBody = `try{${rowBody}}catch(${errorVar}){${errorVar}.path='["'+${iteratorVar}+'"]'+${errorVar}.path;throw ${errorVar}}`;
      }
      output.cp =
        output.cp +
        `for(let ${iteratorVar}=0;${iteratorVar}<${outputVar}.length;++${iteratorVar}){${wrappedBody}}`;

      if (hasAsync) {
        output = B_asyncVal(output, `Promise.all(${outputVar})`);
      }
      return B_markOutput(output, input);
    } else {
      // Reverse direction: rows → columnar
      // When the declared source type is unknown, field values have
      // already been transformed by the object schema's reverse parse
      // and can be copied directly. When it differs (e.g. json), we
      // need per-field parse to convert values back to the source type
      // (e.g. bigint→string for json compatibility).
      const inputVar = input.v();
      const iteratorVar = B_varWithoutAllocation(input.g);
      const outputVar = B_varWithoutAllocation(input.g);

      const needsPerFieldTransform = declaredItemSchema !== unknown;

      let initialArraysCode = "";
      let settingCode = "";
      let perFieldCode = "";
      for (let idx = 0; idx <= keysLen - 1; ++idx) {
        const key = keys[idx]!;
        initialArraysCode = initialArraysCode + `new Array(${inputVar}.length),`;

        if (needsPerFieldTransform) {
          const fieldSchema = properties[key]!;
          const rawValueCode = `${inputVar}[${iteratorVar}][${inlinedValueFromString(key)}]`;

          const itemInput = B_scope(input);
          itemInput.i = rawValueCode;
          itemInput.s = fieldSchema;
          itemInput.e = declaredItemSchema;
          itemInput.v = _notVarBeforeValidation;
          itemInput.io = false;
          itemInput.path = pathFromInlinedLocation(B_inlineLocation(input.g, key));

          const itemOutput = parse(itemInput);
          perFieldCode = perFieldCode + B_merge(itemOutput);
          settingCode =
            settingCode +
            `${outputVar}[${idx}][${iteratorVar}]=${itemOutput.i};`;
        } else {
          settingCode =
            settingCode +
            `${outputVar}[${idx}][${iteratorVar}]=${inputVar}[${iteratorVar}][${inlinedValueFromString(key)}];`;
        }
      }

      const output = B_next(input, outputVar, outputSchema, outputSchema);
      output.v = _var;
      // Columnar accumulator: declared before the `for` that fills it.
      output.cp = `let ${outputVar}=[${initialArraysCode}];`;
      const loopBody = perFieldCode + settingCode;
      let wrappedBody: string;
      if (needsPerFieldTransform && perFieldCode !== "") {
        const errorVar = B_varWithoutAllocation(input.g);
        wrappedBody = `try{${loopBody}}catch(${errorVar}){${errorVar}.path='["'+${iteratorVar}+'"]'+${errorVar}.path;throw ${errorVar}}`;
      } else {
        wrappedBody = loopBody;
      }
      output.cp =
        output.cp +
        `for(let ${iteratorVar}=0;${iteratorVar}<${inputVar}.length;++${iteratorVar}){${wrappedBody}}`;
      return B_markOutput(output, input);
    }
  }
}

export const compactColumns = (inputSchema: Internal): Internal => {
  const innerArray = array(inputSchema);
  const mut = array(innerArray);
  mut.format = "compactColumns";
  mut.decoder = compactColumnsDecoder;
  return mut;
}

export const object = schemaObject;
export const nullAsOption = (item: Internal): Internal =>
  optionFactory(item, nullAsUnit());
// `null` is a reserved word in JS/TS binding position, so this is exported
// as `null_`.
export const null_ = (item: Internal): Internal =>
  unionFactory([item, nullLiteral()]);
export const dict = dictFactory;
export const shape = schemaShape;
export const tuple = schemaTuple;
export const union = unionFactory;

// =============
// Built-in refinements
// =============

export const assertNumber = (fnName: string, n: unknown): void => {
  if (typeof n !== numberTag || Number.isNaN(n)) {
    throw new SuryError({
      code: "invalid_operation",
      path: pathEmpty,
      reason: `[S.${fnName}] Expected number, received ${stringify(n)}`,
    });
  }
};

export const intMin = (schema: Internal, minValue: number, maybeMessage?: string): Internal => {
  assertNumber("min", minValue);
  const message = maybeMessage ?? `Number must be greater than or equal to ${minValue}`;
  return internalRefine(schema, (mut: Internal) => {
    mut.minimum = minValue;
    getMutErrorMessage(mut)["minimum"] = message;
    return (_input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}>${minValue - 1}`,
          f: B_failWithErrorMessage("minimum", message),
        },
      ];
    };
  });
}

export const intMax = (schema: Internal, maxValue: number, maybeMessage?: string): Internal => {
  assertNumber("max", maxValue);
  const message = maybeMessage ?? `Number must be lower than or equal to ${maxValue}`;
  return internalRefine(schema, (mut: Internal) => {
    mut.maximum = maxValue;
    getMutErrorMessage(mut)["maximum"] = message;
    return (_input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}<${maxValue + 1}`,
          f: B_failWithErrorMessage("maximum", message),
        },
      ];
    };
  });
}

export const floatMin = (schema: Internal, minValue: number, maybeMessage?: string): Internal => {
  assertNumber("min", minValue);
  const message = maybeMessage ?? `Number must be greater than or equal to ${minValue}`;
  return internalRefine(schema, (mut: Internal) => {
    mut.minimum = minValue;
    getMutErrorMessage(mut)["minimum"] = message;
    return (input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}>=${B_embed(input, minValue)}`,
          f: B_failWithErrorMessage("minimum", message),
        },
      ];
    };
  });
}

export const floatMax = (schema: Internal, maxValue: number, maybeMessage?: string): Internal => {
  assertNumber("max", maxValue);
  const message = maybeMessage ?? `Number must be lower than or equal to ${maxValue}`;
  return internalRefine(schema, (mut: Internal) => {
    mut.maximum = maxValue;
    getMutErrorMessage(mut)["maximum"] = message;
    return (input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}<=${B_embed(input, maxValue)}`,
          f: B_failWithErrorMessage("maximum", message),
        },
      ];
    };
  });
}

export const arrayMinLength = (schema: Internal, length: number, maybeMessage?: string): Internal => {
  assertNumber("min", length);
  const message = maybeMessage ?? `Array must be ${length} or more items long`;
  return internalRefine(schema, (mut: Internal) => {
    mut.minItems = length;
    getMutErrorMessage(mut)["minItems"] = message;
    return (_input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}.length>${length - 1}`,
          f: B_failWithErrorMessage("minItems", message),
        },
      ];
    };
  });
}

export const arrayMaxLength = (schema: Internal, length: number, maybeMessage?: string): Internal => {
  assertNumber("max", length);
  const message = maybeMessage ?? `Array must be ${length} or fewer items long`;
  return internalRefine(schema, (mut: Internal) => {
    mut.maxItems = length;
    getMutErrorMessage(mut)["maxItems"] = message;
    return (_input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}.length<${length + 1}`,
          f: B_failWithErrorMessage("maxItems", message),
        },
      ];
    };
  });
}

export const arrayLength = (schema: Internal, length: number, maybeMessage?: string): Internal => {
  assertNumber("length", length);
  const message = maybeMessage ?? `Array must be exactly ${length} items long`;
  return internalRefine(schema, (mut: Internal) => {
    mut.minItems = length;
    mut.maxItems = length;
    const em = getMutErrorMessage(mut);
    em["minItems"] = message;
    em["maxItems"] = message;
    return (_input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}.length===${length}`,
          f: B_failWithErrorMessage("minItems", message),
        },
      ];
    };
  });
}

export const stringMinLength = (schema: Internal, length: number, maybeMessage?: string): Internal => {
  assertNumber("min", length);
  const message = maybeMessage ?? `String must be ${length} or more characters long`;
  return internalRefine(schema, (mut: Internal) => {
    mut.minLength = length;
    getMutErrorMessage(mut)["minLength"] = message;
    return (_input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}.length>${length - 1}`,
          f: B_failWithErrorMessage("minLength", message),
        },
      ];
    };
  });
}

export const stringMaxLength = (schema: Internal, length: number, maybeMessage?: string): Internal => {
  assertNumber("max", length);
  const message = maybeMessage ?? `String must be ${length} or fewer characters long`;
  return internalRefine(schema, (mut: Internal) => {
    mut.maxLength = length;
    getMutErrorMessage(mut)["maxLength"] = message;
    return (_input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}.length<${length + 1}`,
          f: B_failWithErrorMessage("maxLength", message),
        },
      ];
    };
  });
}

export const stringLength = (schema: Internal, length: number, maybeMessage?: string): Internal => {
  assertNumber("length", length);
  const message = maybeMessage ?? `String must be exactly ${length} characters long`;
  return internalRefine(schema, (mut: Internal) => {
    mut.minLength = length;
    mut.maxLength = length;
    const em = getMutErrorMessage(mut);
    em["minLength"] = message;
    em["maxLength"] = message;
    return (_input: Val) => {
      return [
        {
          c: (inputVar: string) => `${inputVar}.length===${length}`,
          f: B_failWithErrorMessage("minLength", message),
        },
      ];
    };
  });
}

export const pattern = (schema: Internal, re: RegExp, message: string = `Invalid pattern`): Internal => {
  return internalRefine(schema, (mut: Internal) => {
    mut.pattern = re;
    getMutErrorMessage(mut)["pattern"] = message;
    return (input: Val) => {
      const embededRe = B_embed(input, re);
      return [
        {
          c: (inputVar: string) =>
            re.global
              ? `(${embededRe}.lastIndex=0,${embededRe}.test(${inputVar}))`
              : `${embededRe}.test(${inputVar})`,
          f: B_failWithErrorMessage("pattern", message),
        },
      ];
    };
  });
}

export const trim = (schema: Internal): Internal => {
  const transformer = (string: unknown) => (string as string).trim();
  return transform(schema, (_: unknown) => ({
    p: transformer,
    s: transformer,
  }));
}

export const nullable = (schema: Internal): Internal => {
  return unionFactory([schema, unit(), nullLiteral()]);
}

export const nullableAsOption = (schema: Internal): Internal => {
  return unionFactory([schema, unit(), nullAsUnit()]);
}
