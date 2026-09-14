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
  B_invalidOperation,
  B_mergeWithPathPrepend,
  B_nextVar,
  B_refine
} from "../builder";
import {
 addOpNode,
 compileDecoder,
 findOpNode,
 getOutputSchema,
 removeOpNode
} from "../parse";

export const recursiveDecoder: Builder = (input) => {
  const expectedSchema = input.e;

  const schemaRef = expectedSchema["$ref"]!;
  const defs = input.g.d;
  // Ignore #/$defs/
  const identifier = schemaRef.slice(8);
  // A ref whose definition this compilation was never handed. The JSON Schema
  // emit reaches one: it compiles a reversed SUB-schema to see what the output
  // side serializes to, and the `$defs` hangs off the root that sub-schema was
  // cut out of. Raised rather than left to read `undefined`, because the caller
  // there already swallows a Sury error and falls back to describing the value
  // it could not represent - which a TypeError escaped, taking the diagnostic
  // with it.
  const def =
    defs?.[identifier] ?? B_invalidOperation(input, `Missing definition for ${schemaRef}`);
  // What the definition hands back, for a definition that CONVERTS. A ref names
  // the definition, not a side of it, so a value the definition just produced
  // is spelled exactly like one still waiting to be decoded - and a `.to` stage
  // reading its items back off the container the stage before it built then ran
  // the definition a second time, over its own output: `S.recursive("N", n =>
  // S.array(n).with(S.to, S.set(n)))` decoded every item into the array, then
  // decoded the array again into the Set, which crashed on reading a Set's
  // `length`. Naming the output side tells the two apart, and only a converting
  // definition has two sides to confuse: for every other one the ref is the
  // honest answer, and `S.json` is recognised BY that ref (`S.jsonString` reads
  // its name), so handing back the union it expands to would unmake it.
  const converts = def.to !== U;
  const defOutput = converts ? getOutputSchema(def) : expectedSchema;
  if (converts && input.s === defOutput) return input;
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

  const inputSchema = input.s.seq === expectedSchema.seq ? def : input.s;

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
    output = B_nextVar(input, defOutput, expectedSchema);
    outputDecl = `let ${output.i};`;

    output.cp = `${output.i}=${recOperation}(${input.i});`;

    if (isAsync) {
      output.f |= 1;
    }
  } else {
    // No transform: call for validation but don't capture result
    output = B_refine(input, defOutput, U, expectedSchema);
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
