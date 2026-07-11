import { B_Val_Object_add, B_Val_Object_merge, B_Val_scope, B_inlineLocation, B_invalidOperation, B_markOutput, B_merge, B_nextConst, Builder, _notVarAtParent } from "./builder";
import { Literal_parse, literalDecoder, unit } from "./primitives";
import { Option_getOr, TupleCtx } from "./operations";
import { arrayDecoder, completeObjectVal, makeObjectVal, objectDecoder, optionFactory, unionFactory, valGet } from "./composites";
import { getOutputSchema, parse, reverse } from "./parse";
import { baseSchema, copySchema, globalConfig, panic, updateOutput } from "./schema";
import { Internal, Val, immutableEmptyArray, isLiteral, isSchemaObject, itemSymbol, toExpression } from "./types";
import { Path, inlinedValueFromString, pathConcat, pathEmpty, pathFromInlinedLocation } from "./path";
import { Tag, arrayTag, instanceTag, objectTag } from "./tags";

// PORT-NOTE: `module Schema` is exported as `SchemaModule` (the name `Schema`
// is taken by the schema constructor in the prelude). Its members are defined
// as standalone functions (mutual recursion between shape/nested/object/
// definitionToSchema/... is awkward inside an object literal) with
// `schema`-prefixed names where the bare name would collide with other
// sections (`schemaShape`, `schemaNested`, `schemaObject`, `schemaTuple`,
// `schemaFactory`), then attached to `SchemaModule` so call sites read
// `schemaFactory`, `schemaObject`, etc.
// =============================================================================

type ShapedSerializerAcc = {
  val?: Val;
  properties?: Record<string, ShapedSerializerAcc>;
  flattened?: ShapedSerializerAcc[];
};

export type SchemaCtx = {
  m: (schema: Internal) => unknown;
};

const inputFrom = immutableEmptyArray as string[];

// advancedObjectCtx:
//   Public API for JS/TS users.
//   It shouldn't be used from ReScript and
//   needed only because we use @as for field to reduce bundle-size
//   of ReScript compiled code
// PORT-NOTE: lifted to top level as `AdvancedObjectCtx`. Runtime keys:
// `field` (@as("field") _jsField) plus the spread `...Object.s` keys
// (`f` for field via @as("f"), fieldOr, tag, nested, flatten).
export type AdvancedObjectCtx = {
  field: (fieldName: string, schema: Internal) => unknown;
  f: (fieldName: string, schema: Internal) => unknown;
  fieldOr: (fieldName: string, schema: Internal, or: unknown) => unknown;
  tag: (tag: string, asValue: unknown) => void;
  nested: (fieldName: string) => AdvancedObjectCtx;
  flatten: (schema: Internal) => unknown;
};

const proxifyShapedSchema = (schema: Internal, from: string[], fromFlattened?: number): unknown => {
  const mut = copySchema(getOutputSchema(schema));
  mut.from = from;
  if (fromFlattened !== undefined) {
    mut.fromFlattened = fromFlattened;
  }
  return new Proxy(mut as unknown as object, {
    get(target: Internal, prop) {
      if (prop === (itemSymbol as unknown)) {
        return target;
      } else {
        const location = prop as unknown as string;

        let maybeField: Internal | undefined;
        if (target.properties !== undefined) {
          maybeField = target.properties[location];
        } else if (target.items !== undefined) {
          // If there are no properties, then it must be Tuple
          maybeField = target.items[location as unknown as number];
        } else {
          maybeField = undefined;
        }
        if (maybeField === undefined) {
          panic(`Cannot read property "${location}" of ${toExpression(target)}`);
        }

        return proxifyShapedSchema(
          maybeField!,
          target.from!.concat(location),
          target.fromFlattened
        );
      }
    },
  } as ProxyHandler<object>);
}

export const schemaShape = <Value>(schema: Internal, definer: (value: unknown) => unknown): Value => {
  return updateOutput<Value>(schema, (mut) => {
    const fromProxy = proxifyShapedSchema(mut, inputFrom);
    const definition: unknown = definer(fromProxy);
    if (definition === fromProxy) {
      // ()
    } else {
      mut.parser = shapedParser;
      mut.to = definitionToShapedSchema(definition);
    }
  });
}

export function schemaNested(this: Record<string, unknown>, fieldName: string): AdvancedObjectCtx {
  const parentCtx = this as unknown as AdvancedObjectCtx & Record<string, unknown>; // TODO: Add a check that it's binded?
  const cacheId = `~${fieldName}`;

  const cachedCtx = parentCtx[cacheId] as AdvancedObjectCtx | undefined;
  if (cachedCtx !== undefined) {
    return cachedCtx;
  } else {
    const properties: Record<string, Internal> = {};
    const required: string[] = [];
    let schema: Internal;
    {
      const s = baseSchema(objectTag, false);
      s.required = required;
      s.properties = properties;
      s.additionalItems = globalConfig.a;
      s.decoder = objectDecoder;
      schema = s;
    }

    const parentSchema: Internal = (parentCtx.f(fieldName, schema) as Record<symbol, Internal>)[
      itemSymbol
    ]!;

    const field = (fieldName: string, schema: Internal): unknown => {
      const inlinedLocation = inlinedValueFromString(fieldName);
      if (fieldName in properties) {
        panic(`The field ${inlinedLocation} defined twice`);
      }
      required.push(fieldName);
      properties[fieldName] = schema;
      return proxifyShapedSchema(
        schema,
        parentSchema.from!.concat(fieldName),
        parentSchema.fromFlattened
      );
    };

    const tag = (tag: string, asValue: unknown): void => {
      field(tag, definitionToSchema(asValue));
    };

    const fieldOr = (fieldName: string, schema: Internal, or: unknown): unknown => {
      return field(fieldName, Option_getOr(optionFactory(schema), or));
    };

    const flatten = (schema: Internal): unknown => {
      if (schema.type === objectTag) {
        const flattenedProperties = schema.properties;
        const to = schema.to;
        if (to as unknown as boolean) {
          panic(
            `Unsupported nested flatten for transformed object schema ${toExpression(schema)}`
          );
        }
        const flattenedKeys = Object.keys(flattenedProperties!);
        const result: Record<string, unknown> = {};
        for (let idx = 0; idx < flattenedKeys.length; idx++) {
          const key = flattenedKeys[idx]!;
          result[key] = field(key, flattenedProperties![key]!);
        }
        return result;
      } else {
        return panic(`Can't flatten ${toExpression(schema)} schema`);
      }
    };

    const ctx: AdvancedObjectCtx = {
      // js/ts methods
      field,
      // methods
      f: field,
      fieldOr,
      tag,
      nested: schemaNested,
      flatten,
    };

    (parentCtx as Record<string, unknown>)[cacheId] = ctx;

    return ctx;
  }
}

export const schemaObject = (definer: (ctx: AdvancedObjectCtx) => unknown): Internal => {
  let flattened: Internal[] | undefined = void 0;
  const properties: Record<string, Internal> = {};

  const flatten = (schema: Internal): unknown => {
    if (schema.type === objectTag) {
      const flattenedProperties = schema.properties!;
      const flattenedKeys = Object.keys(flattenedProperties);
      for (let idx = 0; idx < flattenedKeys.length; idx++) {
        const key = flattenedKeys[idx]!;
        const flattenedSchema = flattenedProperties[key]!;
        const existing = properties[key];
        if (existing !== undefined && existing === flattenedSchema) {
          // ()
        } else if (existing !== undefined) {
          panic(`The field "${key}" defined twice with incompatible schemas`);
        } else {
          properties[key] = flattenedSchema;
        }
      }
      const f = flattened || (flattened = []);
      return proxifyShapedSchema(schema, inputFrom, f.push(schema) - 1);
    } else {
      return panic(`The '${toExpression(schema)}' schema can't be flattened`);
    }
  };

  const field = (fieldName: string, schema: Internal): unknown => {
    if (fieldName in properties) {
      panic(`The field "${fieldName}" defined twice with incompatible schemas`);
    }
    properties[fieldName] = schema;
    return proxifyShapedSchema(schema, [fieldName]);
  };

  const tag = (tag: string, asValue: unknown): void => {
    field(tag, definitionToSchema(asValue));
  };

  const fieldOr = (fieldName: string, schema: Internal, or: unknown): unknown => {
    return field(fieldName, Option_getOr(optionFactory(schema), or));
  };

  const ctx: AdvancedObjectCtx = {
    // js/ts methods
    field,
    // methods
    f: field,
    fieldOr,
    tag,
    nested: schemaNested,
    flatten,
  };

  const definition = definer(ctx);

  const mut = baseSchema(objectTag, false);
  mut.required = Object.keys(properties);
  mut.properties = properties;
  mut.additionalItems = globalConfig.a;
  mut.decoder = objectDecoder;
  mut.parser = shapedParser;
  mut.to = definitionToShapedSchema(definition);
  if (flattened !== undefined) {
    mut.flattened = flattened;
  }
  return mut;
}

export const schemaTuple = (definer: (ctx: TupleCtx) => unknown): Internal => {
  const items: Internal[] = [];

  const item = (idx: number, schema: Internal): unknown => {
    const location = String(idx);
    if (items[idx] as unknown as boolean) {
      return panic(`The item [${location}] is defined multiple times`);
    } else {
      items[idx] = schema;
      return proxifyShapedSchema(schema, [String(idx)]);
    }
  };

  const tag = (idx: number, asValue: unknown): void => {
    item(idx, definitionToSchema(asValue));
  };

  const ctx: TupleCtx = {
    item,
    tag,
  };

  const definition = definer(ctx);

  for (let idx = 0; idx < items.length; idx++) {
    if (!items[idx]) {
      items[idx] = unit();
    }
  }

  const mut = baseSchema(arrayTag, false);
  mut.items = items;
  mut.additionalItems = "strict";
  mut.decoder = arrayDecoder;
  mut.parser = shapedParser;
  mut.to = definitionToShapedSchema(definition);
  return mut;
}

const getValByFrom = (input: Val, from: string[], idx: number): Val => {
  // FIXME: TODO: something with flattened
  const key = from[idx];
  if (key !== undefined) {
    return getValByFrom(input.d![key]!, from, idx + 1);
  } else {
    return input;
  }
}

// Owns the shaped structure walk: assembles an object/tuple val from a
// per-location field producer. `init` wires the fresh objectVal before the
// walk and may pre-populate `d` (flattened merge) — pre-populated locations
// are skipped. `onMissing` handles a non-object/tuple target.
const assembleShapedObject = (
  input: Val,
  schema: Internal,
  field: (location: string, childSchema: Internal) => Val,
  init?: (output: Val) => void,
  onMissing?: () => void
): Val => {
  const output = makeObjectVal(input, schema);
  output.io = true;
  if (init !== undefined) {
    init(output);
  }
  if (schema.items !== undefined) {
    const items = schema.items;
    for (let idx = 0; idx < items.length; idx++) {
      const location = String(idx);
      B_Val_Object_add(output, location, field(location, items[idx]!));
    }
  } else if (schema.properties !== undefined) {
    const properties = schema.properties;
    const keys = Object.keys(properties);
    for (let idx = 0; idx < keys.length; idx++) {
      const location = keys[idx]!;
      // Skip locations pre-populated by init (flattened fields)
      if (!(location in output.d!)) {
        B_Val_Object_add(output, location, field(location, properties[location]!));
      }
    }
  } else if (onMissing !== undefined) {
    onMissing();
  } else {
    // FIXME: Use a path
    panic(`Don't know where the value is coming from: ${toExpression(schema)}`);
  }
  return completeObjectVal(output);
}

const getShapedParserOutput = (input: Val, targetSchema: Internal): Val => {
  let v: Val;
  if (targetSchema.fromFlattened !== undefined) {
    v = B_Val_scope(
      getValByFrom(input.fv![targetSchema.fromFlattened]!, targetSchema.from!, 0)
    );
  } else if (targetSchema.from !== undefined) {
    v = B_Val_scope(getValByFrom(input, targetSchema.from, 0));
  } else if (isLiteral(targetSchema)) {
    v = B_nextConst(input, targetSchema);
  } else {
    v = assembleShapedObject(input, targetSchema, (_location, childSchema) =>
      getShapedParserOutput(input, childSchema)
    );
  }
  v.prev = undefined;
  v.e = targetSchema;
  return v;
}

export const shapedParser: Builder = (input: Val) => {
  const flattened = input.e.flattened;
  if (flattened !== undefined) {
    const flattenedVals: Val[] = [];
    for (let idx = 0; idx < flattened.length; idx++) {
      const flattenedSchema = flattened[idx]!;
      // The flattened object's keys are merged into the parent's properties and
      // already decoded by the parent objectDecoder, so `input` holds their
      // decoded vals. Reuse them here instead of decoding again — re-decoding
      // would re-apply field-level transforms on the already-transformed value
      // (issue #271).
      let flattenedVal: Val;
      if (flattenedSchema.to !== undefined) {
        // The flattened schema has its own reshape/transform. Mark the input as
        // output so the parse loop skips the decoder and runs only that `.to`,
        // reading the decoded fields back through the shared `vals`.
        const flattenedInput = B_Val_scope(input);
        flattenedInput.e = flattenedSchema;
        flattenedInput.io = true;
        flattenedVal = parse(flattenedInput);
      } else {
        // No reshape: project the flattened schema's own keys out of the
        // parent's decoded fields (selection without decoding), then apply the
        // flattened schema's own refiners. Materializing the projection gives it
        // an inline restricted to its keys, so a whole-object read of the
        // flattened result can't leak sibling fields of the parent.
        const assembled = assembleShapedObject(input, flattenedSchema, (location, _childSchema) =>
          valGet(input, location)
        );
        assembled.e = flattenedSchema;
        // The reused field vals are declared by the parent's own code; detach
        // from `prev` (as getShapedParserOutput does) so `merge` doesn't
        // re-emit the parent's declarations. Done before markOutput so any
        // refiner wrap it adds still points at the assembled object.
        assembled.prev = undefined;
        flattenedVal = B_markOutput(assembled, assembled);
      }
      flattenedVals.push(flattenedVal);
      input.cp = input.cp + B_merge(flattenedVal);
    }
    input.fv = flattenedVals;
  }

  const targetSchema = input.e.to!;
  const output = getShapedParserOutput(input, targetSchema);
  output.t = true;
  output.prev = input;
  return B_markOutput(output, input);
}

const prepareShapedSerializerAcc = (acc: ShapedSerializerAcc, input: Val): void => {
  if (input.e.from !== undefined) {
    const from = input.e.from;
    const fromFlattened = input.e.fromFlattened;
    let accAtFrom: ShapedSerializerAcc;
    if (fromFlattened !== undefined) {
      if (acc.flattened === undefined) {
        acc.flattened = [];
      }
      const existing = acc.flattened[fromFlattened];
      if (existing === undefined) {
        const newAcc: ShapedSerializerAcc = {};
        acc.flattened[fromFlattened] = newAcc;
        accAtFrom = newAcc;
      } else {
        accAtFrom = existing;
      }
    } else {
      accAtFrom = acc;
    }
    for (let idx = 0; idx < from.length; idx++) {
      const key = from[idx]!;
      let p: Record<string, ShapedSerializerAcc>;
      if (accAtFrom.properties !== undefined) {
        p = accAtFrom.properties;
      } else {
        p = {};

        accAtFrom.properties = p;
      }
      const existingAcc = p[key];
      if (existingAcc !== undefined) {
        accAtFrom = existingAcc;
      } else {
        const newAcc: ShapedSerializerAcc = {};
        p[key] = newAcc;
        accAtFrom = newAcc;
      }
    }
    accAtFrom.val = input;
  } else if (input.d !== undefined) {
    const vals = input.d;
    const keys = Object.keys(vals);
    for (let idx = 0; idx < keys.length; idx++) {
      prepareShapedSerializerAcc(acc, vals[keys[idx]!]!);
    }
  }
}

const getShapedSerializerOutput = (
  input: Val,
  acc: ShapedSerializerAcc | undefined,
  targetSchema: Internal,
  path: Path
): Val => {
  if (acc !== undefined && acc.val !== undefined) {
    // Placement of an already-decoded val — don't overwrite its schema (#284);
    // parse only re-advances `e` and emits nothing for an output val
    const v = B_Val_scope(acc.val);
    v.t = true;
    v.e = targetSchema;
    return parse(v);
  } else if (isLiteral(targetSchema)) {
    const v = B_nextConst(input, targetSchema, targetSchema);
    v.prev = undefined;
    v.p = input;
    v.v = _notVarAtParent;
    v.io = true;
    return parse(v);
  } else {
    // When acc is undefined (discriminant field with no input), follow the to chain
    // to get the actual output schema properties (e.g., for reversed transformed objects)
    const resolvedTargetSchema = acc === undefined ? getOutputSchema(targetSchema) : targetSchema;

    const missingInput = (): never => {
      // PORT-NOTE: the source shadows `path` here; renamed to `path2` (TS
      // can't redeclare a parameter in the same scope).
      const path2 =
        targetSchema.from !== undefined
          ? path + targetSchema.from.map((item) => `["${item}"]`).join("")
          : path;
      return B_invalidOperation(
        input,
        `Missing input for ${toExpression(targetSchema)}` + (path2 === "" ? "" : ` at ${path2}`)
      );
    };

    // A dict-like target has no fixed locations to walk without an input acc
    if (acc === undefined && (typeof resolvedTargetSchema.additionalItems as Tag) === objectTag) {
      return missingInput();
    }

    return assembleShapedObject(
      input,
      resolvedTargetSchema,
      (location, childSchema) =>
        getShapedSerializerOutput(
          input,
          acc !== undefined && acc.properties !== undefined ? acc.properties[location] : undefined,
          childSchema,
          pathConcat(path, pathFromInlinedLocation(B_inlineLocation(input.g, location)))
        ),
      (v) => {
        v.e = resolvedTargetSchema;
        v.prev = undefined;
        v.p = input;
        v.v = _notVarAtParent;
        const flattened = resolvedTargetSchema.flattened;
        if (flattened !== undefined && acc !== undefined && acc.flattened !== undefined) {
          const flattenedSchemas = flattened;
          const flattenedAcc = acc.flattened;
          flattenedAcc.forEach((acc, idx) => {
            const flattenedOutput = getShapedSerializerOutput(
              input,
              acc,
              reverse(flattenedSchemas[idx]!),
              path
            );
            B_Val_Object_merge(v, flattenedOutput.d!);
          });
        }
      },
      missingInput
    );
  }
}

export const shapedSerializer: Builder = (input: Val) => {
  const acc: ShapedSerializerAcc = {};
  prepareShapedSerializerAcc(acc, input);

  const targetSchema = input.e.to!;
  const output = getShapedSerializerOutput(input, acc, targetSchema, pathEmpty);
  output.t = true;
  output.prev = input;
  return output;
}

const definitionToShapedSchema = (definition: unknown): Internal => {
  const s = copySchema(
    traverseDefinition(
      definition,
      (definition: unknown) =>
        (definition as Record<symbol, Internal | undefined>)[itemSymbol]
    )
  );
  s.serializer = shapedSerializer;
  return s;
}

export const definitionToSchema = (definition: unknown): Internal => {
  return traverseDefinition(definition, (node) => {
    if (isSchemaObject(node)) {
      return node as unknown as Internal;
    } else {
      return undefined;
    }
  });
}

const traverseDefinition = (
  definition: unknown,
  onNode: (node: unknown) => Internal | undefined
): Internal => {
  if ((typeof definition as Tag) === objectTag && definition !== null) {
    const s = onNode(definition);
    if (s !== undefined) {
      return s;
    } else {
      if (Array.isArray(definition)) {
        const node = definition as unknown[];
        for (let idx = 0; idx < node.length; idx++) {
          const schema = traverseDefinition(node[idx], onNode);
          node[idx] = schema as unknown;
        }
        const items = node as unknown as Internal[];

        const mut = baseSchema(arrayTag, false);
        mut.items = items;
        mut.additionalItems = "strict";
        mut.decoder = arrayDecoder;
        return mut;
      } else {
        const cnstr = (definition as Record<string, unknown>)["constructor"];
        if ((cnstr as unknown as boolean) && cnstr !== Object) {
          const mut = baseSchema(instanceTag, true);
          mut.class = cnstr;
          mut.const = definition;
          mut.decoder = literalDecoder;
          return mut;
        } else {
          const node = definition as Record<string, unknown>;
          const fieldNames = Object.keys(node);
          const length = fieldNames.length;
          for (let idx = 0; idx < length; idx++) {
            const location = fieldNames[idx]!;
            const schema = traverseDefinition(node[location], onNode);
            node[location] = schema as unknown;
          }
          const mut = baseSchema(objectTag, false);
          mut.required = fieldNames;
          mut.properties = node as unknown as Record<string, Internal>;
          mut.additionalItems = globalConfig.a;
          mut.decoder = objectDecoder;
          return mut;
        }
      }
    }
  } else {
    return Literal_parse(definition);
  }
}

const schemaMatches = (schema: Internal): unknown => {
  return schema as unknown;
}
const schemaCtx: SchemaCtx = {
  m: schemaMatches,
};
export const schemaFactory = (definer: (ctx: unknown) => unknown): Internal => {
  return definitionToSchema(definer(schemaCtx as unknown));
}

// PORT-NOTE: `module Schema` exported as SchemaModule (name `Schema` is taken
// by the schema constructor in the prelude). Members defined as standalone
// functions above and attached here so call sites can use SchemaModule.*.

// Identifier aliases (not `schemaFactory` property reads) so esbuild
// can tree-shake: a property-read initializer is treated as possibly
// side-effectful and would retain the whole schema machinery in every bundle.
export const schema = schemaFactory;

export const js_schema = (definition: unknown): Internal => {
  return definitionToSchema(definition);
}
export const literal = js_schema;

// PORT-NOTE: `enum` is a reserved word in TS — defined as `enum_` and
// re-exported under the name `enum` (legal as an export alias).
const enum_ = (values: unknown[]): Internal => {
  return unionFactory(values.map(literal));
}
export { enum_ as enum };
