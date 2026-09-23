// `S.recursive` - a schema that refers to itself. The decoder compiles the
// body once and routes every self-reference back through it by `$ref`.

import {
  baseSchema,
  type Builder,
  defsPath,
  globalConfig,
  type Internal,
  refTag,
  U,
  type Val
} from "../base";
import {
  B_embed,
  B_mergeWithPathPrepend,
  B_nextVar,
  B_refine
} from "../builder";
import {
 addOpNode,
 compileDecoder,
 findOpNode,
 removeOpNode
} from "../parse";

export const recursiveDecoder: Builder = (input) => {
  const expectedSchema = input.e;

  const defs = input.g.d!;
  // A ref the compiler built carries its definition; one the author wrote names
  // it in the operation's record. Ignore #/$defs/
  const def = expectedSchema.definition || defs[expectedSchema["$ref"]!.slice(8)]!;
  // Masked to the compile-semantics bits (127 and below). A def compiles a
  // nested operation whose result generated code consumes, so it must throw:
  // inheriting the outer operation's return mode would have the inner one
  // answering `false` or a `{success}` object into the middle of a value.
  // Masking also lets the modes share one node per def.
  const flag = input.g.o & 127;
  // The memo key. A sync nested operation is byte-for-byte the top-level
  // `parseOrThrow(def)`, so the two share a node. An async one is not: nested,
  // it stays throwing where the top-level one lifts to a promise and rejects,
  // so it is keyed apart (8192, a bit no operation flag carries) - or a
  // `parseAsPromiseOrReject(def)` compiled through the wrapper would answer
  // a bare value, and its failure a synchronous throw.
  const key = flag & 1 ? flag | 8192 : flag;

  // A ref source stands for its definition when it carries one: converting from
  // it means converting from what it names, and a ref is opaque to every
  // decoder below. A ref the author wrote is left alone, since resolving one
  // would have a schema converting to `S.json` read its definition too and plan
  // from a union where it now plans from an opaque value.
  const inputSchema = input.s.definition || (input.s.seq === expectedSchema.seq ? def : input.s);

  let recOperation = "";

  // The def's operations live in the same node cache `getOp` uses (see OpNode
  // in parse.ts), stored on `def`; `getOp` stores on its newest-seq argument,
  // so the two sides find each other's work whenever `def` is the newer of the
  // pair - otherwise the pair just compiles twice. `v === 0`
  // means this def is mid-compilation - a circular reference - and the NODE
  // is what gets embedded: it exists before the function it will hold, so
  // generated code calls `.v` at runtime and every recompile lands there for
  // free.
  let opNode = findOpNode(def, inputSchema, def, key);
  if (opNode) {
    recOperation =
      opNode.v === 0 ? B_embed(input, opNode) + ".v" : B_embed(input, opNode.v);
  } else {
    // Optimistic compilation with recompile if assumptions were wrong.
    // Annotated: without it the assignment to `node.t` below narrows the field,
    // and inferring these from it back through `node.t` is circular.
    let assumedHasTransform: boolean = false;
    let assumedIsAsync: boolean = false;
    let compileNeeded = true;
    const node = addOpNode(def, [inputSchema, def], key, 0);

    try {
      while (compileNeeded) {
        compileNeeded = false;

        // The assumption goes on the node, which is what an inner circular
        // reference finds (`findOpNode` above) - so the two ends of the cycle
        // agree on the shape of the call before either is compiled.
        node.t = assumedHasTransform;
        node.y = assumedIsAsync;

        // Back to in-progress: a recompile's inner circular references must
        // route through the node, not a stale function from the failed attempt.
        node.v = 0;

        // `compileDecoder` overwrites both with what it actually built.
        node.v = compileDecoder(inputSchema, def, flag, defs, node);

        if (node.t !== assumedHasTransform || node.y !== assumedIsAsync) {
          // Wrong assumption - update and recompile
          assumedHasTransform = node.t!;
          assumedIsAsync = node.y!;
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
    opNode = node;
  }

  const hasTransform = opNode.t === true;
  const isAsync = opNode.y!;

  // Result var decl, prepended after the re-merge below so it sits outside the
  // try/catch mergeWithPathPrepend may wrap the assignment in (stays in scope).
  let outputDecl = "";
  let output: Val;
  if (hasTransform || isAsync) {
    output = B_nextVar(input, expectedSchema);
    outputDecl = `let ${output.i};`;

    output.cp = `${output.i}=${recOperation}(${input.i});`;

    if (isAsync) {
      output.f |= 1;
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
  const isNestedRec = !!globalConfig.d;
  if (!isNestedRec) {
    // Null prototype: the caller names the definition, so one named `__proto__`
    // would set this object's prototype instead of taking a key.
    globalConfig.d = Object.create(null);
  }
  let def: Internal;
  // A definer that throws must not leave the accumulator behind: every later
  // top-level `recursive` would then see itself as nested and return a ref
  // with no `$defs`. A nested one leaves it to the outer call, whose definer
  // may catch and carry on.
  try {
    def = fn(refSchema);
  } catch (e) {
    if (!isNestedRec) globalConfig.d = U;
    throw e;
  }
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
