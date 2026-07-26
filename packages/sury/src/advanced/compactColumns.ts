// `S.compactColumns` — a row-of-objects schema read from column arrays, which
// is why it owns a decoder of its own rather than composing existing ones.

import {
  type Builder,
  copySchema,
  flagUnsafeHas,
  inlinedValueFromString,
  type Internal,
  panic,
  pathFromInlinedLocation,
  tagFlags,
  tagFlagUnknown,
  U,
  unknown,
  type Val,
  valFlagAsync,
} from "../base";
import {
  _notVarBeforeValidation,
  _var,
  B_asyncVal,
  B_markOutput,
  B_merge,
  B_next,
  B_refine,
  B_scope,
  B_varWithoutAllocation,
  failInvalidType,
} from "../builder";
import { array } from "../composites";
import { parse } from "../parse";

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
    selfSchema.to !== U &&
    typeof selfSchema.to.additionalItems === "object"
  ) {
    forwardProps = (selfSchema.to.additionalItems as Internal).properties;
  } else {
    forwardProps = U;
  }
  const isForwardDirection = forwardProps !== U;
  let maybeProperties: Record<string, Internal> | undefined;
  if (isForwardDirection) {
    maybeProperties = forwardProps;
  } else {
    if (
      input.s.additionalItems !== U &&
      typeof input.s.additionalItems === "object"
    ) {
      maybeProperties = (input.s.additionalItems as Internal).properties;
    } else {
      maybeProperties = U;
    }
  }

  if (!maybeProperties) {
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
        input = B_refine(input, U, [
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
        input = B_refine(input, U, [
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
        itemInput.path = pathFromInlinedLocation(inlinedValueFromString(key));

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
          itemInput.path = pathFromInlinedLocation(inlinedValueFromString(key));

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
