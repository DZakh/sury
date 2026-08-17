// `S.recursive` — a schema that refers to itself. The decoder compiles the
// body once and routes every self-reference back through it by `$ref`.

import {
  baseSchema,
  type Builder,
  defsPath,
  globalConfig,
  type Internal,
  refTag,
  U,
  type Val,
  valFlagAsync,
} from "../base";
import {
  _var,
  B_embed,
  B_mergeWithPathPrepend,
  B_next,
  B_refine,
  B_varWithoutAllocation,
} from "../builder";
import { addOpNode, compileDecoder, findOpNode, removeOpNode } from "../parse";

export const recursiveDecoder: Builder = (input) => {
  const expectedSchema = input.e;

  const schemaRef = expectedSchema["$ref"]!;
  const defs = input.g.d!;
  // Ignore #/$defs/
  const identifier = schemaRef.slice(8);
  const def = defs[identifier]!;
  const flag = input.g.o;

  const inputSchema = input.s.seq === expectedSchema.seq ? def : input.s;

  let recOperation = "";

  // The def's operations live in the same node cache getDecoder uses (see
  // OpNode in parse.ts), stored on `def`; getDecoder stores on its newest-seq
  // argument, so the two sides find each other's work whenever `def` is the
  // newer of the pair — otherwise the pair just compiles twice. `v === 0`
  // means this def is mid-compilation — a circular reference — and the NODE
  // is what gets embedded: it exists before the function it will hold, so
  // generated code calls `.v` at runtime and every recompile lands there for
  // free.
  const existing = findOpNode(def, inputSchema, def, flag);
  if (existing !== U) {
    recOperation =
      existing.v === 0 ? B_embed(input, existing) + ".v" : B_embed(input, existing.v);
  } else {
    // Optimistic compilation with recompile if assumptions were wrong
    let assumedHasTransform = def.hasTransform !== U ? def.hasTransform : false;
    let assumedIsAsync = def.isAsync !== U ? def.isAsync : false;
    let compileNeeded = true;
    const node = addOpNode(def, [inputSchema, def], flag, 0);

    try {
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

        // Back to in-progress: a recompile's inner circular references must
        // route through the node, not a stale function from the failed attempt.
        node.v = 0;

        node.v = compileDecoder(inputSchema, def, flag, defs);

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
          compileNeeded = true;
        }
      }
    } catch (exn) {
      // A throw leaves `v === 0` behind; unlinked, so a retry recompiles and
      // reports the schema bug instead of embedding a dead sentinel.
      removeOpNode(def, node);
      throw exn;
    }

    // Embed only the final compiled function to avoid wasting embed slots on recompiles
    recOperation = B_embed(input, node.v);
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
  const refSchema = baseSchema(refTag, false, recursiveDecoder);
  refSchema["$ref"] = ref;
  refSchema.name = name;

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
    const schema = baseSchema(refTag, false, recursiveDecoder);
    schema.name = refSchema.name;
    schema["$ref"] = ref;
    schema["$defs"] = globalConfig.d;

    globalConfig.d = U;

    return schema;
  }
}
