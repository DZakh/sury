// `S.recursive` — a schema that refers to itself. The decoder compiles the
// body once and routes every self-reference back through it by `$ref`.

import {
  baseSchema,
  type Builder,
  configurableValueOptions,
  defsPath,
  globalConfig,
  type Internal,
  refTag,
  U,
  type Val,
  valFlagAsync,
  valKey,
  valueOptions,
} from "../base";
import {
  _var,
  B_embed,
  B_mergeWithPathPrepend,
  B_next,
  B_refine,
  B_varWithoutAllocation,
} from "../builder";
import { compileDecoder } from "../parse";

export const recursiveDecoder: Builder = (input) => {
  const expectedSchema = input.e;

  const schemaRef = expectedSchema["$ref"]!;
  const defs = input.g.d!;
  // Ignore #/$defs/
  const identifier = schemaRef.slice(8);
  const def = defs[identifier]!;
  const flag = input.g.o;

  const inputSchema = input.s.seq === expectedSchema.seq ? def : input.s;

  const key = `${inputSchema.seq}-${def.seq}--${flag}`;
  let recOperation = "";

  const fn = (def as unknown as Record<string, unknown>)[key];
  if (fn !== U) {
    // Circular reference (fn === 0) or already compiled
    recOperation = fn === 0 ? B_embed(input, def) + `["${key}"]` : B_embed(input, fn);
  } else {
    // Optimistic compilation with recompile if assumptions were wrong
    let assumedHasTransform = def.hasTransform !== U ? def.hasTransform : false;
    let assumedIsAsync = def.isAsync !== U ? def.isAsync : false;
    let compileNeeded = true;
    let finalFn: unknown = 0;

    while (compileNeeded) {
      compileNeeded = false;

      // Set optimistic values on def before compiling (if not already set)
      // Inner circular references will read these values
      if (def.hasTransform === U) {
        def.hasTransform = assumedHasTransform;
      }
      if (def.isAsync === U) {
        def.isAsync = assumedIsAsync;
      }

      // Mark as in-progress
      (configurableValueOptions as unknown as Record<string, unknown>)[valKey] = 0;
      Object.defineProperty(def, key, configurableValueOptions as PropertyDescriptor);

      // Compile
      const fn = compileDecoder(inputSchema, def, flag, defs);

      // Cache result
      valueOptions[valKey] = fn;
      Object.defineProperty(def, key, valueOptions as PropertyDescriptor);

      finalFn = fn;

      // Check if actual values differ from assumed
      const actualHasTransform = def.hasTransform!;
      const actualIsAsync = def.isAsync!;

      if (
        actualHasTransform !== assumedHasTransform ||
        actualIsAsync !== assumedIsAsync
      ) {
        // Wrong assumption - update and recompile
        assumedHasTransform = actualHasTransform;
        assumedIsAsync = actualIsAsync;
        // Delete cached function to force recompilation
        delete (def as unknown as Record<string, unknown>)[key];
        compileNeeded = true;
      }
    }

    // Embed only the final compiled function to avoid wasting embed slots on recompiles
    recOperation = B_embed(input, finalFn);
  }

  const hasTransform = def.hasTransform === true;
  const isAsync = def.isAsync!;

  // Result var decl, prepended after the re-merge below so it sits outside the
  // try/catch mergeWithPathPrepend may wrap the assignment in (stays in scope).
  let outputDecl = "";
  let output: Val;
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
    // No transform: call for validation but don't capture result
    output = B_refine(input, expectedSchema, U, expectedSchema);
    output.cp = `${recOperation}(${input.i});`;
  }

  output.prev = U;
  output.cp = outputDecl + B_mergeWithPathPrepend(output, input);

  // Un-finalize: this val may be reused as input to a subsequent parser (e.g.
  // S.transform on a recursive schema) and must accept hoisted decls again.
  output.fz = U;
  output.prev = input;

  return output;
};

// @__NO_SIDE_EFFECTS__
export const recursive = (name: string, fn: (schema: Internal) => Internal): Internal => {
  const ref = `${defsPath}${name}`;
  const refSchema = baseSchema(refTag, false);
  refSchema["$ref"] = ref;
  refSchema.name = name;
  refSchema.decoder = recursiveDecoder;

  // This is for mutual recursion
  const isNestedRec = globalConfig.d !== U;
  if (!isNestedRec) {
    globalConfig.d = {};
  }
  const def = fn(refSchema);
  if (def.name) {
    refSchema.name = def.name;
  }
  globalConfig.d![name] = def;

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
}
