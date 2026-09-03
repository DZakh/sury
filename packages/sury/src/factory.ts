// The factory functions below (`schemaShape`, `schemaNested`, `schemaObject`,
// `schemaTuple`, `schemaDefiner`, `schemaFactory`) are standalone top-level
// functions rather than object methods — several are mutually recursive,
// which is awkward to express inside an object literal — with
// `schema`-prefixed names to avoid colliding with other sections.

import {
  anyOfTag,
  arrayTag,
  baseSchema,
  type Builder,
  copySchema,
  getOrRethrow,
  globalConfig,
  immutableEmptyArray,
  inlinedValueFromString,
  inputExpression,
  type Internal,
  isLiteral,
  itemSymbol,
  noopDecoder,
  objectTag,
  panic,
  type Path,
  pathConcat,
  pathEmpty,
  pathToText,
  isOptional,
  setHas,
  U,
  undefinedTag,
  unknown,
  updateOutput,
  type Val,
} from "./base";
import {
  _notVarAtParent,
  _var,
  B_addObjectField,
  B_inlineConst,
  B_invalidOperation,
  B_markOutput,
  B_merge,
  B_next,
  B_nextConst,
  B_scope,
} from "./builder";
import {
  arrayDecoder,
  completeObjectVal,
  definitionToSchema,
  makeArrayVal,
  makeObjectVal,
  objectDecoder,
  traverseDefinition,
  valGet,
} from "./composites";
import { type TupleCtx } from "./modifiers";
import { getDecoder, getOutputSchema, parse, reverse } from "./parse";
import { Literal_parse, unit } from "./primitives";
import { unionFactory } from "./union";

type ShapedSerializerAcc = {
  val?: Val;
  properties?: Record<string, ShapedSerializerAcc>;
  flattened?: ShapedSerializerAcc[];
};

export type SchemaCtx = {
  m: (schema: Internal) => unknown;
};

const inputFrom = immutableEmptyArray as string[];

// The public JS/TS-facing object-builder ctx: `field` is the long-form JS/TS
// name, `f` the short runtime alias (`ObjectCtx.f` in operations.ts) that
// both ship — `field` for DX, `f` because it's what codegen already looks up.
export type AdvancedObjectCtx = {
  field: (fieldName: string, schema: Internal) => unknown;
  f: (fieldName: string, schema: Internal) => unknown;
  fieldOr: (fieldName: string, schema: Internal, or: unknown) => unknown;
  tag: (tag: string, asValue: unknown) => void;
  nested: (fieldName: string) => AdvancedObjectCtx;
  flatten: (schema: Internal) => unknown;
};

const makeTag = (field: (location: string, schema: Internal) => unknown) =>
  (tag: string, asValue: unknown): void => {
    field(tag, definitionToSchema(asValue));
  };

// Field-with-default as `if(v===void 0)v=def` plus the item's own decoder —
// not `union([unit, item])` + Option_getOr. The anyOf/has/undefined shape is
// what isOptional, JSON Schema (skip the unit arm, emit `default`) and json
// omit still read; unionDecoder is what would pull the planner into every
// object export.
const fieldOrSchema = (schema: Internal, or: unknown): Internal => {
  const item = getOutputSchema(schema);
  const mut = baseSchema(anyOfTag, false, noopDecoder);
  mut.anyOf = [schema, unit];
  mut.has = { [undefinedTag]: true };
  setHas(mut.has, schema.type);
  // A `.to` is what makes reverse start at the item (output is required, not
  // optional). A serializer on a self-reverse item is what keeps encode
  // re-checking it — without one, a typed boolean property is trusted and the
  // check the union compiler used to emit disappears.
  if (schema.to === U) {
    const toMut = copySchema(schema);
    toMut.serializer = (input: Val) => {
      const itemInput = B_scope(input);
      itemInput.io = false;
      itemInput.s = unknown;
      itemInput.e = schema;
      return parse(itemInput);
    };
    mut.to = toMut;
  } else {
    mut.to = schema;
  }
  try {
    (getDecoder(unknown, item) as (input: unknown) => unknown)(or);
  } catch (exn) {
    const error = getOrRethrow(exn);
    panic(
      `Invalid default for ${inputExpression(mut)}: ${
        (error as unknown as { message: string })["message"]
      }`
    );
  }
  try {
    mut.default = (getDecoder(reverse(schema)) as (input: unknown) => unknown)(or);
  } catch (_exn) {}

  const parseAs = copySchema(schema);
  parseAs.expression = () => inputExpression(mut);

  mut.parser = (input: Val) => {
    const v = input.v();
    const defCode = B_inlineConst(input, Literal_parse(or));
    const itemInput = B_scope(input);
    itemInput.io = false;
    itemInput.s = unknown;
    itemInput.e = parseAs;
    itemInput.u = input.u;
    const itemOutput = parse(itemInput);
    const itemCode = B_merge(itemOutput);
    const assign = itemOutput.i === v ? "" : `${v}=${itemOutput.i};`;
    const output = B_next(input, v, item, item);
    output.v = _var;
    output.io = true;
    output.cp =
      itemCode === "" && assign === ""
        ? `if(${v}===void 0)${v}=${defCode};`
        : `if(${v}===void 0){${v}=${defCode}}else{${itemCode}${assign}}`;
    return output;
  };
  return mut;
};

const makeFieldOr = (field: (location: string, schema: Internal) => unknown) =>
  (fieldName: string, schema: Internal, or: unknown): unknown => {
    return field(fieldName, fieldOrSchema(schema, or));
  };

const proxifyShapedSchema = (schema: Internal, from: string[], fromFlattened?: number): unknown => {
  const mut = copySchema(getOutputSchema(schema));
  mut.from = from;
  if (fromFlattened !== U) {
    mut.fromFlattened = fromFlattened;
  }
  return new Proxy(mut, {
    get(target: Internal, prop) {
      if (prop === itemSymbol) {
        return target;
      } else {
        const location = prop as string;

        let maybeField: Internal | undefined;
        if (target.properties !== U) {
          maybeField = target.properties[location];
        } else if (target.items !== U) {
          // If there are no properties, then it must be Tuple
          maybeField = target.items[location as unknown as number];
        } else {
          maybeField = U;
        }
        if (!maybeField) {
          panic(`Cannot read property "${location}" of ${inputExpression(target)}`);
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

// @__NO_SIDE_EFFECTS__
export const schemaShape = <TValue>(schema: Internal, definer: (value: unknown) => unknown): TValue => {
  return updateOutput<TValue>(schema, (mut) => {
    const fromProxy = proxifyShapedSchema(mut, inputFrom);
    const definition: unknown = definer(fromProxy);
    if (definition === fromProxy) {
      // Definer returned the proxy unchanged: no reshape, keep the identity parser.
    } else {
      mut.parser = shapedParser;
      mut.to = definitionToShapedSchema(definition);
    }
  });
}

function schemaNested(this: AdvancedObjectCtx & Record<string, unknown>, fieldName: string): AdvancedObjectCtx {
  // TODO: Add a check that `this` is actually bound to a parent ctx?
  const parentCtx = this;
  const cacheId = `~${fieldName}`;

  const cachedCtx = parentCtx[cacheId] as AdvancedObjectCtx | undefined;
  if (cachedCtx !== U) {
    return cachedCtx;
  } else {
    const properties = Object.create(null) as Record<string, Internal>;
    const required: string[] = [];
    let schema: Internal;
    {
      const s = baseSchema(objectTag, false, objectDecoder);
      s.required = required;
      s.properties = properties;
      s.additionalItems = globalConfig.a;
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
      if (!isOptional(schema)) required.push(fieldName);
      properties[fieldName] = schema;
      return proxifyShapedSchema(
        schema,
        parentSchema.from!.concat(fieldName),
        parentSchema.fromFlattened
      );
    };

    const tag = makeTag(field);
    const fieldOr = makeFieldOr(field);

    const flatten = (schema: Internal): unknown => {
      if (schema.type === objectTag) {
        const flattenedProperties = schema.properties;
        const to = schema.to;
        if (to) {
          panic(`Can't flatten transformed ${inputExpression(schema)}`);
        }
        const flattenedKeys = Object.keys(flattenedProperties!);
        const result: Record<string, unknown> = {};
        for (let idx = 0; idx < flattenedKeys.length; idx++) {
          const key = flattenedKeys[idx]!;
          result[key] = field(key, flattenedProperties![key]!);
        }
        return result;
      } else {
        return panic(`Can't flatten ${inputExpression(schema)} schema`);
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

// @__NO_SIDE_EFFECTS__
export const schemaObject = (
  definer: ((ctx: AdvancedObjectCtx) => unknown) | Record<string, unknown>
): Internal => {
  if (typeof definer !== "function") {
    return definitionToSchema(definer);
  }
  let flattened: Internal[] | undefined = U;
  const properties = Object.create(null) as Record<string, Internal>;
  const required: string[] = [];

  const flatten = (schema: Internal): unknown => {
    if (schema.type === objectTag) {
      const flattenedProperties = schema.properties!;
      const flattenedKeys = Object.keys(flattenedProperties);
      for (let idx = 0; idx < flattenedKeys.length; idx++) {
        const key = flattenedKeys[idx]!;
        const flattenedSchema = flattenedProperties[key]!;
        const existing = properties[key];
        if (existing !== U && existing === flattenedSchema) {
          // Same field flattened in from two places — already registered, skip.
        } else if (existing !== U) {
          panic(`The field "${key}" defined twice with incompatible schemas`);
        } else {
          if (!isOptional(flattenedSchema)) required.push(key);
          properties[key] = flattenedSchema;
        }
      }
      const f = flattened || (flattened = []);
      return proxifyShapedSchema(schema, inputFrom, f.push(schema) - 1);
    } else {
      return panic(`The '${inputExpression(schema)}' schema can't be flattened`);
    }
  };

  const field = (fieldName: string, schema: Internal): unknown => {
    if (fieldName in properties) {
      panic(`The field "${fieldName}" defined twice with incompatible schemas`);
    }
    if (!isOptional(schema)) required.push(fieldName);
    properties[fieldName] = schema;
    return proxifyShapedSchema(schema, [fieldName]);
  };

  const tag = makeTag(field);
  const fieldOr = makeFieldOr(field);

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

  const mut = baseSchema(objectTag, false, objectDecoder);
  mut.required = required;
  mut.properties = properties;
  mut.additionalItems = globalConfig.a;
  mut.parser = shapedParser;
  mut.to = definitionToShapedSchema(definition);
  if (flattened !== U) {
    mut.flattened = flattened;
  }
  return mut;
}

// @__NO_SIDE_EFFECTS__
export const schemaTuple = (
  definer: ((ctx: TupleCtx) => unknown) | unknown[]
): Internal => {
  if (typeof definer !== "function") {
    return definitionToSchema(definer);
  }
  const items: Internal[] = [];

  const item = (idx: number, schema: Internal): unknown => {
    const location = String(idx);
    if (items[idx]) {
      return panic(`The item [${location}] is defined multiple times`);
    } else {
      items[idx] = schema;
      return proxifyShapedSchema(schema, [location]);
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
      items[idx] = unit;
    }
  }

  const mut = baseSchema(arrayTag, false, arrayDecoder);
  mut.items = items;
  mut.additionalItems = "strict";
  mut.parser = shapedParser;
  mut.to = definitionToShapedSchema(definition);
  return mut;
}

const getValByFrom = (input: Val, from: string[], idx: number): Val => {
  // Flattened schemas are resolved by the caller (getShapedParserOutput picks
  // the right `input.fv[fromFlattened]` before calling this) — this walk only
  // needs to handle a plain nested `from` path.
  const key = from[idx];
  if (key !== U) {
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
  const output = schema.type === arrayTag ? makeArrayVal(input, schema) : makeObjectVal(input, schema);
  output.io = true;
  if (init !== U) {
    init(output);
  }
  if (schema.items !== U) {
    const items = schema.items;
    for (let idx = 0; idx < items.length; idx++) {
      const location = String(idx);
      B_addObjectField(output, location, field(location, items[idx]!));
    }
  } else if (schema.properties !== U) {
    const properties = schema.properties;
    const keys = Object.keys(properties);
    for (let idx = 0; idx < keys.length; idx++) {
      const location = keys[idx]!;
      // Skip locations pre-populated by init (flattened fields)
      if (!(location in output.d!)) {
        B_addObjectField(output, location, field(location, properties[location]!));
      }
    }
  } else if (onMissing !== U) {
    onMissing();
  } else {
    panic(
      `Don't know where the value is coming from: ${inputExpression(schema)}` +
        (input.path.length ? ` at ${pathToText(input.path)}` : "")
    );
  }
  return completeObjectVal(output);
}

const getShapedParserOutput = (input: Val, targetSchema: Internal): Val => {
  let v: Val;
  if (targetSchema.fromFlattened !== U) {
    v = B_scope(
      getValByFrom(input.fv![targetSchema.fromFlattened]!, targetSchema.from!, 0)
    );
  } else if (targetSchema.from !== U) {
    v = B_scope(getValByFrom(input, targetSchema.from, 0));
  } else if (isLiteral(targetSchema)) {
    v = B_nextConst(input, targetSchema);
  } else {
    v = assembleShapedObject(input, targetSchema, (_location, childSchema) =>
      getShapedParserOutput(input, childSchema)
    );
  }
  v.prev = U;
  v.e = targetSchema;
  return v;
}

const shapedParser: Builder = (input: Val) => {
  const flattened = input.e.flattened;
  if (flattened !== U) {
    const flattenedVals: Val[] = [];
    for (let idx = 0; idx < flattened.length; idx++) {
      const flattenedSchema = flattened[idx]!;
      // The flattened object's keys are merged into the parent's properties and
      // already decoded by the parent objectDecoder, so `input` holds their
      // decoded vals. Reuse them here instead of decoding again — re-decoding
      // would re-apply field-level transforms on the already-transformed value
      // (issue #271).
      let flattenedVal: Val;
      if (flattenedSchema.to !== U) {
        // The flattened schema has its own reshape/transform. Mark the input as
        // output so the parse loop skips the decoder and runs only that `.to`,
        // reading the decoded fields back through the shared `vals`.
        const flattenedInput = B_scope(input);
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
        assembled.prev = U;
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
  if (input.e.from !== U) {
    const from = input.e.from;
    const fromFlattened = input.e.fromFlattened;
    let accAtFrom: ShapedSerializerAcc;
    if (fromFlattened !== U) {
      if (acc.flattened === U) {
        acc.flattened = [];
      }
      const existing = acc.flattened[fromFlattened];
      if (existing === U) {
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
        const newAcc: ShapedSerializerAcc = {};
        p[key] = newAcc;
        accAtFrom = newAcc;
      }
    }
    accAtFrom.val = input;
  } else if (input.d !== U) {
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
  if (acc !== U && acc.val !== U) {
    // Placement of an already-decoded val — don't overwrite its schema (#284);
    // parse only re-advances `e` and emits nothing for an output val
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
    // When acc is undefined (discriminant field with no input), follow the to chain
    // to get the actual output schema properties (e.g., for reversed transformed objects)
    const resolvedTargetSchema = acc === U ? getOutputSchema(targetSchema) : targetSchema;

    const missingInput = (): never => {
      // PORT-NOTE: the source shadows `path` here; renamed to `path2` (TS
      // can't redeclare a parameter in the same scope).
      const path2 =
        targetSchema.from !== U ? pathConcat(path, targetSchema.from) : path;
      return B_invalidOperation(
        input,
        `Missing input for ${inputExpression(targetSchema)}` +
          (path2.length ? ` at ${pathToText(path2)}` : "")
      );
    };

    // A dict-like target has no fixed locations to walk without an input acc
    if (acc === U && typeof resolvedTargetSchema.additionalItems === objectTag) {
      return missingInput();
    }

    const assembled = assembleShapedObject(
      input,
      resolvedTargetSchema,
      (location, childSchema) =>
        getShapedSerializerOutput(
          input,
          acc !== U && acc.properties !== U ? acc.properties[location] : U,
          childSchema,
          pathConcat(path, [location])
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
          flattenedAcc.forEach((acc, idx) => {
            const flattenedOutput = getShapedSerializerOutput(
              input,
              acc,
              reverse(flattenedSchemas[idx]!),
              path
            );
            // Only the member's fields are placed here, so take its code once
            // and read each field back out of it. `valGet` scopes a field the
            // member already emitted — a whole-object placement hands back the
            // very vals the parent's own decode declared, and adding those
            // unscoped is what emitted their `let`s a second time (#368) — and
            // synthesizes a read when the member ends in its own transform,
            // whose result carries no field vals of its own (B_next).
            v.cp = v.cp + B_merge(flattenedOutput);
            for (const key of Object.keys(flattenedOutput.d!)) {
              B_addObjectField(v, key, valGet(flattenedOutput, key));
            }
          });
        }
      },
      missingInput
    );
    // The walk built the head of `targetSchema`'s chain. If the schema also
    // carries a transform of its own, run it here: the assembled head is its
    // input, and nobody else will apply it (a pending operation-level `to`
    // — `parser` absent — is the compile pipeline's job, not ours).
    return targetSchema.parser === U ? assembled : parse(assembled);
  }
}

const shapedSerializer: Builder = (input: Val) => {
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
      (node) => (node as Record<symbol, Internal | undefined>)[itemSymbol]
    )
  );
  s.serializer = shapedSerializer;
  return s;
}

const schemaCtx: SchemaCtx = {
  m: (schema) => schema,
};
// @__NO_SIDE_EFFECTS__
export const schemaDefiner = (definer: (ctx: unknown) => unknown): Internal => {
  return definitionToSchema(definer(schemaCtx));
}

// Identifier alias (not a `schemaDefiner` property read) so esbuild can
// tree-shake: a property-read initializer is treated as possibly
// side-effectful and would retain the whole schema machinery in every bundle.
// @__NO_SIDE_EFFECTS__
export const schemaFactory = (definition: unknown): Internal => {
  return definitionToSchema(definition);
}

// PORT-NOTE: `enum` is a reserved word in TS — defined as `enum_` and
// re-exported under the name `enum` (legal as an export alias).
// @__NO_SIDE_EFFECTS__
const enum_ = (values: unknown[]): Internal => {
  return unionFactory(values.map(schemaFactory));
}
export { enum_ as enum };
