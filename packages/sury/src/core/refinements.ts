import { nullLiteral, unit } from "./primitives.ts";
import { getMutErrorMessage, internalRefine, nullAsUnit, transform } from "./operations.ts";
import { definitionToSchema, schemaObject, schemaShape, schemaTuple } from "./factory.ts";
import { parse } from "./parse.ts";
import { SuryError, copySchema, panic, unknown } from "./schema.ts";
import { B_Val_scope, B_asyncVal, B_embed, B_failWithErrorMessage, B_inlineLocation, B_markOutput, B_merge, B_next, B_refine, B_varWithoutAllocation, _notVarBeforeValidation, _var, failInvalidType } from "./builder.ts";
import { array, dictFactory, optionFactory, unionFactory } from "./composites.ts";
import { ErrorDetails, Flag, Internal, Tag, Val, flagUnsafeHas, inlinedValueFromString, numberTag, pathEmpty, pathFromInlinedLocation, stringify, tagFlagUnknown, tagFlags, valFlagAsync } from "./types.ts";
// Section 09 — Sury.res lines 6179-6943
// compactColumnsDecoder, compactColumns, public aliases (object, nullAsOption,
// null_, array alias, dict, shape, tuple, tuple1/2/3, union), assertNumber and
// built-in refinements (intMin/intMax/floatMin/floatMax, array*/string* length
// refinements, pattern, trim, nullable, nullableAsOption).
//
// TODO(integration): expects `B` (Builder helpers: refine, next, markOutput,
//   varWithoutAllocation, Val.scope, Val.var, _notVarBeforeValidation, _var,
//   asyncVal, merge, inlineLocation, embed, failInvalidType,
//   failWithErrorMessage) from the Builder section.
// TODO(integration): expects `parse` from the parse-loop section.
// TODO(integration): expects `array` (the array factory) from the array section.
// TODO(integration): expects `SchemaModule` (object, shape, tuple,
//   definitionToSchema) from the Schema factory section (~5485).
// TODO(integration): expects `optionFactory`, `unionFactory`, `dictFactory`,
//   `unit`, `nullAsUnit`, `nullLiteral` from earlier sections.
// TODO(integration): expects `internalRefine` and `getMutErrorMessage` from
//   section 06.
// TODO(integration): expects `transform` from the transform section.
// =============================================================================

export const compactColumnsDecoder = (input: Val): Val => {
  const selfSchema = input.e;
  const isUnknownInput = flagUnsafeHas(
    tagFlags[input.s.type]! as unknown as Flag,
    tagFlagUnknown as unknown as Flag,
  );

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
  const isForwardDirection = forwardProps as unknown as boolean;
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
      const s = array(array(unknown)) as unknown as Internal;
      s.to = selfSchema.to;
      outputSchema = s;
    }

    if (keysLen === 0) {
      let input2 = input;
      if (isUnknownInput) {
        input2 = B_refine(input, undefined, [
          {
            c: (inputVar: string) =>
              `Array.isArray(${inputVar})&&${inputVar}.length===0`,
            f: failInvalidType,
          },
        ]);
      }
      const output = B_next(input2, "[]", outputSchema, outputSchema);
      return B_markOutput(output, input2);
    } else if (isForwardDirection) {
      // Forward direction: columnar → rows
      let input2 = input;
      if (isUnknownInput) {
        input2 = B_refine(input, undefined, [
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

      const inputVar = input2.v();
      const iteratorVar = B_varWithoutAllocation(input2.g);
      const outputVar = B_varWithoutAllocation(input2.g);

      // Declared source item type from selfSchema (the compactColumns schema).
      const declaredItemSchema: Internal = (() => {
        const innerArray: Internal = selfSchema.additionalItems as unknown as Internal;
        return innerArray.additionalItems as unknown as Internal;
      })();

      // Actual runtime item type: unknown for top-level parser, or
      // the typed source when the caller passed already-typed data.
      let runtimeItemSchema: Internal;
      if (isUnknownInput) {
        runtimeItemSchema = unknown;
      } else {
        const innerArray: Internal = input2.s.additionalItems as unknown as Internal;
        runtimeItemSchema = innerArray.additionalItems as unknown as Internal;
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

        const itemInput = B_Val_scope(input2);
        itemInput.i = rawValueCode;
        itemInput.s = runtimeItemSchema;
        itemInput.e = itemExpected;
        itemInput.v = _notVarBeforeValidation;
        itemInput.io = false;

        // Path like ["bar"] so validation errors carry the field location.
        itemInput.path = pathFromInlinedLocation(B_inlineLocation(input2.g, key));

        const itemOutput = parse(itemInput);
        if (
          flagUnsafeHas(
            itemOutput.f as unknown as Flag,
            valFlagAsync as unknown as Flag,
          )
        ) {
          hasAsync = true;
        }

        itemParseCode = itemParseCode + B_merge(itemOutput);
        lengthCode = lengthCode + `${inputVar}[${idxStr}].length,`;
        asyncInlines = asyncInlines + `${itemOutput.i},`;
        itemBuildCode =
          itemBuildCode + `${inlinedValueFromString(key)}:${itemOutput.i},`;
      }

      const output = B_next(input2, outputVar, outputSchema, outputSchema);
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
        const rowResultVar = B_varWithoutAllocation(input2.g);
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
        const errorVar = B_varWithoutAllocation(input2.g);
        wrappedBody = `try{${rowBody}}catch(${errorVar}){${errorVar}.path='["'+${iteratorVar}+'"]'+${errorVar}.path;throw ${errorVar}}`;
      }
      output.cp =
        output.cp +
        `for(let ${iteratorVar}=0;${iteratorVar}<${outputVar}.length;++${iteratorVar}){${wrappedBody}}`;

      let output2 = output;
      if (hasAsync) {
        output2 = B_asyncVal(output, `Promise.all(${outputVar})`);
      }
      return B_markOutput(output2, input2);
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

      const declaredItemSchema: Internal = (() => {
        const innerArray: Internal = selfSchema.additionalItems as unknown as Internal;
        return innerArray.additionalItems as unknown as Internal;
      })();
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

          const itemInput = B_Val_scope(input);
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
  const mut = array(innerArray) as unknown as Internal;
  mut.format = "compactColumns";
  mut.decoder = compactColumnsDecoder;
  return mut;
}

// let inline = {
//   let rec internalInline = (schema, ~variant as maybeVariant=?, ()) => {
//     let mut = schema->castToInternal->copy

//     let inlinedSchema = switch mut {
//     | {?const} if isLiteral(mut) => `S.literal(%raw(\`${literal->Literal.toString}\`))`
//     | {anyOf} => {
//         let variantNamesCounter = dict{}
//         `S.union([${anyOf
//           ->Array.map(s => {
//             let variantName = s.name()
//             let numberOfVariantNames = switch variantNamesCounter->Dict.get(variantName) {
//             | Some(n) => n
//             | None => 0
//             }
//             variantNamesCounter->Dict.set(variantName, numberOfVariantNames->X.Int.plus(1))
//             let variantName = switch numberOfVariantNames {
//             | 0 => variantName
//             | _ =>
//               variantName ++ numberOfVariantNames->X.Int.plus(1)->X.Int.unsafeToString
//             }
//             let inlinedVariant = `#${variantName->X.Inlined.Value.fromString}`
//             s->internalInline(~variant=inlinedVariant, ())
//           })
//           ->Array.join(", ")}])`
//       }
//     | {tag: JSON} => `S.json(~validate=${validated->(Obj.magic: bool => string)})`
//     | {tag: TupleTuple({items: [s0]}) => `S.tuple1(${s0.schema->internalInline()})`
//     | Tuple({items: [s0, s1]}) =>
//       `S.tuple2(${s0.schema->internalInline()}, ${s1.schema->internalInline()})`
//     | Tuple({items: [s0, s1, s2]}) =>
//       `S.tuple3(${s0.schema->internalInline()}, ${s1.schema->internalInline()}, ${s2.schema->internalInline()})`
//     | Tuple({items}) =>
//       `S.tuple(s => (${items
//         ->Array.mapWithIndex((schema, idx) =>
//           `s.item(${idx->X.Int.unsafeToString}, ${schema.schema->internalInline()})`
//         )
//         ->Array.join(", ")}))`
//     | Object({items: []}) => `S.object(_ => ())`
//     | Object({items}) =>
//       `S.object(s =>
//   {
//     ${items
//         ->Array.map(item => {
//           `${item.inlinedLocation}: s.field(${item.inlinedLocation}, ${item.schema->internalInline()})`
//         })
//         ->Array.join(",\n    ")},
//   }
// )`
//     | String => `S.string`
//     | Int => `S.int`
//     | Float => `S.float`
//     | BigInt => `S.bigint`
//     | Bool => `S.bool`
//     | Option(schema) => `S.option(${schema->internalInline()})`
//     | Null(schema) => `S.nullAsOption(${schema->internalInline()})`
//     | Never => `S.never`
//     | Unknown => `S.unknown`
//     | Array(schema) => `S.array(${schema->internalInline()})`
//     | Dict(schema) => `S.dict(${schema->internalInline()})`
//     }

//     let inlinedSchema = switch schema->Option.default {
//     | Some(default) => {
//         metadataMap->X.Dict.deleteInPlace(Option.defaultMetadataId->Metadata.Id.unionToKey)
//         switch default {
//         | Value(defaultValue) =>
//           inlinedSchema ++
//           `->S.Option.getOr(%raw(\`${defaultValue->X.Inlined.Value.stringify}\`))`
//         | Callback(defaultCb) =>
//           inlinedSchema ++
//           `->S.Option.getOrWith(() => %raw(\`${defaultCb()->X.Inlined.Value.stringify}\`))`
//         }
//       }

//     | None => inlinedSchema
//     }

//     let inlinedSchema = switch schema->deprecation {
//     | Some(message) => {
//         metadataMap->X.Dict.deleteInPlace(deprecationMetadataId->Metadata.Id.unionToKey)
//         inlinedSchema ++ `->S.deprecate(${message->X.Inlined.Value.fromString})`
//       }

//     | None => inlinedSchema
//     }

//     let inlinedSchema = switch schema->description {
//     | Some(message) => {
//         metadataMap->X.Dict.deleteInPlace(descriptionMetadataId->Metadata.Id.unionToKey)
//         inlinedSchema ++ `->S.describe(${message->X.Inlined.Value.stringify})`
//       }

//     | None => inlinedSchema
//     }

//     let inlinedSchema = switch schema->classify {
//     | Object({additionalItems: Strict}) => inlinedSchema ++ `->S.strict`
//     | _ => inlinedSchema
//     }

//     let inlinedSchema = switch schema->classify {
//     | String
//     | Literal(String(_)) =>
//       switch schema->String.refinements {
//       | [] => inlinedSchema
//       | refinements =>
//         metadataMap->X.Dict.deleteInPlace(String.Refinement.metadataId->Metadata.Id.unionToKey)
//         inlinedSchema ++
//         refinements
//         ->Array.map(refinement => {
//           switch refinement {
//           | {kind: Email, message} =>
//             `->S.email(~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Url, message} => `->S.url(~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Uuid, message} =>
//             `->S.uuid(~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Cuid, message} =>
//             `->S.cuid(~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Min({length}), message} =>
//             `->S.stringMinLength(${length->X.Int.unsafeToString}, ~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Max({length}), message} =>
//             `->S.stringMaxLength(${length->X.Int.unsafeToString}, ~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Length({length}), message} =>
//             `->S.stringLength(${length->X.Int.unsafeToString}, ~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Pattern({re}), message} =>
//             `->S.pattern(%re(${re
//               ->X.Re.toString
//               ->X.Inlined.Value.fromString}), ~message=${message->X.Inlined.Value.fromString})`
//           }
//         })
//         ->Array.join("")
//       }
//     | Int =>
//       // | Literal(Int(_)) ???
//       switch schema->Int.refinements {
//       | [] => inlinedSchema
//       | refinements =>
//         metadataMap->X.Dict.deleteInPlace(Int.Refinement.metadataId->Metadata.Id.unionToKey)
//         inlinedSchema ++
//         refinements
//         ->Array.map(refinement => {
//           switch refinement {
//           | {kind: Max({value}), message} =>
//             `->S.intMax(${value->X.Int.unsafeToString}, ~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Min({value}), message} =>
//             `->S.intMin(${value->X.Int.unsafeToString}, ~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Port, message} =>
//             `->S.port(~message=${message->X.Inlined.Value.fromString})`
//           }
//         })
//         ->Array.join("")
//       }
//     | Float =>
//       // | Literal(Float(_)) ???
//       switch schema->Float.refinements {
//       | [] => inlinedSchema
//       | refinements =>
//         metadataMap->X.Dict.deleteInPlace(Float.Refinement.metadataId->Metadata.Id.unionToKey)
//         inlinedSchema ++
//         refinements
//         ->Array.map(refinement => {
//           switch refinement {
//           | {kind: Max({value}), message} =>
//             `->S.floatMax(${value->X.Inlined.Float.toRescript}, ~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Min({value}), message} =>
//             `->S.floatMin(${value->X.Inlined.Float.toRescript}, ~message=${message->X.Inlined.Value.fromString})`
//           }
//         })
//         ->Array.join("")
//       }

//     | Array(_) =>
//       switch schema->Array.refinements {
//       | [] => inlinedSchema
//       | refinements =>
//         metadataMap->X.Dict.deleteInPlace(Array.Refinement.metadataId->Metadata.Id.unionToKey)
//         inlinedSchema ++
//         refinements
//         ->Array.map(refinement => {
//           switch refinement {
//           | {kind: Max({length}), message} =>
//             `->S.arrayMaxLength(${length->X.Int.unsafeToString}, ~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Min({length}), message} =>
//             `->S.arrayMinLength(${length->X.Int.unsafeToString}, ~message=${message->X.Inlined.Value.fromString})`
//           | {kind: Length({length}), message} =>
//             `->S.arrayLength(${length->X.Int.unsafeToString}, ~message=${message->X.Inlined.Value.fromString})`
//           }
//         })
//         ->Array.join("")
//       }

//     | _ => inlinedSchema
//     }

//     let inlinedSchema = if metadataMap->Dict.keysToArray->Array.length !== 0 {
//       `{
//   let s = ${inlinedSchema}
//   let _ = %raw(\`s.m = ${metadataMap->JSON.stringifyAny->Option.getUnsafe}\`)
//   s
// }`
//     } else {
//       inlinedSchema
//     }

//     let inlinedSchema = switch maybeVariant {
//     | Some(variant) => inlinedSchema ++ `->S.shape(v => ${variant}(v))`
//     | None => inlinedSchema
//     }

//     inlinedSchema
//   }

//   schema => {
//     schema->castToUnknown->internalInline()
//   }
// }

// PORT-NOTE: `object`, `shape`, `tuple` alias `Schema.object/shape/tuple`
// (renamed `SchemaModule` per conventions) — kept as aliases.
export const object = schemaObject;
export const nullAsOption = (item: Internal): Internal =>
  optionFactory(item, nullAsUnit());
// PORT-NOTE: `null` is a reserved word in JS/TS binding position — exported
// as `null_`; the ReScript bindings layer maps it back to `S.null`.
export const null_ = (item: Internal): Internal =>
  unionFactory([item, nullLiteral()]);
// PORT-NOTE: `let array = array` in the source is a self-alias no-op
// (re-exposing the earlier `array` factory at this point in the module) —
// skipped; the `array` binding from its own section is already exported.
export const dict = dictFactory;
export const shape = schemaShape;
export const tuple = schemaTuple;
export const tuple1 = (v0: Internal): Internal =>
  tuple((s: any) => s.item(0, v0));
export const tuple2 = (v0: Internal, v1: Internal): Internal =>
  definitionToSchema([v0, v1] as unknown as Internal);
export const tuple3 = (v0: Internal, v1: Internal, v2: Internal): Internal =>
  definitionToSchema([v0, v1, v2] as unknown as Internal);
export const union = unionFactory;

// =============
// Built-in refinements
// =============

export const assertNumber: (fnName: string, n: unknown) => void = (fnName, n) => {
  if ((typeof n as Tag) !== numberTag || Number.isNaN(n)) {
    throw new SuryError({
      code: "invalid_operation",
      path: pathEmpty,
      reason: `[S.${fnName}] Expected number, received ${stringify(n)}`,
    } as unknown as ErrorDetails);
  }
};

export const intMin = (schema: Internal, minValue: number, maybeMessage?: string): Internal => {
  assertNumber("min", minValue);
  const message =
    maybeMessage !== undefined
      ? maybeMessage
      : `Number must be greater than or equal to ${minValue}`;
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
  const message =
    maybeMessage !== undefined
      ? maybeMessage
      : `Number must be lower than or equal to ${maxValue}`;
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
  const message =
    maybeMessage !== undefined
      ? maybeMessage
      : `Number must be greater than or equal to ${minValue}`;
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
  const message =
    maybeMessage !== undefined
      ? maybeMessage
      : `Number must be lower than or equal to ${maxValue}`;
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
  const message =
    maybeMessage !== undefined
      ? maybeMessage
      : `Array must be ${length} or more items long`;
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
  const message =
    maybeMessage !== undefined
      ? maybeMessage
      : `Array must be ${length} or fewer items long`;
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
  const message =
    maybeMessage !== undefined
      ? maybeMessage
      : `Array must be exactly ${length} items long`;
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
  const message =
    maybeMessage !== undefined
      ? maybeMessage
      : `String must be ${length} or more characters long`;
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
  const message =
    maybeMessage !== undefined
      ? maybeMessage
      : `String must be ${length} or fewer characters long`;
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
  const message =
    maybeMessage !== undefined
      ? maybeMessage
      : `String must be exactly ${length} characters long`;
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

// =============
// JS/TS API
// =============
// ============================================================================
