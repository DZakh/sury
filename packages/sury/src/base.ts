// The base layer: the data model (`Internal`, `Val`, `Check`), the schema
// object and its prototype, tags, flags and paths — merged into one module
// because they are mutually dependent by nature and splitting them bought
// nothing but import churn. Nothing here imports from another module in the
// package: base is the bottom of the layering, so every other module can reach
// it without a cycle.

// Lives here rather than in builder.ts so base has no outgoing edge: both are
// one-liners over `Val`/`Internal`, and builder.ts importing them back is free.
export type Builder = (input: Val) => Val;
export type Encoder = (input: Val, target: Internal) => Val;

// ── flags ─────────────────────────────────────────────────────────────────────

export type Flag = number;

// Bit-flag literals (esbuild does not inline named consts). Compile (`g.o` /
// op flag): 0 none, 1 async, 2 disableNaN, 4 union-transform-context (custom
// transform inside a union case preserves the original exception so dispatch
// can distinguish Sury failures from foreign ones), 64 flatten.
// Val (`Val.f`): 0 none, 1 async.

// ── path ──────────────────────────────────────────────────────────────────────

// Root-first. Static segments are strings; an index captured from a runtime
// variable in generated code is a number. "[]" marks "some element" for
// locations without a concrete value (JSON Schema conversion, dynamic parse).
//
// Never mutated: details objects, codegen closures and retained user errors
// share instances, so every prepend/concat allocates. Never a symbol: every
// key that becomes a segment comes from `Object.keys` or a generated `for-in`,
// which skip them — `S.res`'s `propertyKey` and the `~standard` bridge rely on
// that.
export type Path = readonly (string | number)[];

export const pathEmpty: Path = [];
export const pathDynamic: Path = ["[]"];

// Everything a raw splice into a double-quoted JS literal can't carry: the
// quote itself, `\` (an accidental escape reads as a different string), and
// both line terminators (a SyntaxError inside new Function).
const inlineUnsafeRe = /["\\\n\r]/;
export const inlinedValueFromString = (str: string): string =>
  inlineUnsafeRe.test(str) ? JSON.stringify(str) : `"${str}"`;

// `{__proto__:x}` is [[SetPrototypeOf]], not a data property. Object keys
// accept IdentifierName, so reserved words (`default`, `class`) unquote too.
const jsIdentRe = /^[A-Za-z_$][\w$]*$/;
export const inlinedObjectKey = (key: string): string =>
  key === "__proto__"
    ? '["__proto__"]'
    : jsIdentRe.test(key)
      ? key
      : inlinedValueFromString(key);

// @__NO_SIDE_EFFECTS__
export const pathConcat = (path: Path, concatedPath: Path): Path =>
  path.length ? (concatedPath.length ? path.concat(concatedPath) : path) : concatedPath;

// `user.tags[2]`, `["my key"]`. A non-string segment goes through `String`
// rather than the regexes: this runs inside the `message` getter, where a
// throw would mask the error being reported.
// @__NO_SIDE_EFFECTS__
export const pathToText = (path: Path): string => {
  let text = "";
  for (let idx = 0; idx < path.length; idx++) {
    const segment = path[idx]!;
    text +=
      typeof segment !== "string"
        ? `[${String(segment)}]`
        : /^\d+$/.test(segment)
          ? `[${segment}]`
          : segment === "[]"
            ? segment
            : jsIdentRe.test(segment)
              ? text
                ? `.${segment}`
                : segment
              : `[${inlinedValueFromString(segment)}]`;
  }
  return text;
}

// ── tags ──────────────────────────────────────────────────────────────────────

export type Tag =
  | "string"
  | "number"
  | "bigint"
  | "boolean"
  | "symbol"
  | "null"
  | "undefined"
  | "nan"
  | "function"
  | "instance"
  | "array"
  | "object"
  | "anyOf"
  | "never"
  | "unknown"
  | "ref";

// Use variables to reduce bundle size with min+gzip
// Also as a good practice (ignore that we have tag variant 😅)
export const stringTag: Tag = "string";
export const numberTag: Tag = "number";
export const bigintTag: Tag = "bigint";
export const booleanTag: Tag = "boolean";
export const symbolTag: Tag = "symbol";
export const nullTag: Tag = "null";
export const undefinedTag: Tag = "undefined";
export const nanTag: Tag = "nan";
export const functionTag: Tag = "function";
export const instanceTag: Tag = "instance";
export const arrayTag: Tag = "array";
export const objectTag: Tag = "object";
export const anyOfTag: Tag = "anyOf";
export const neverTag: Tag = "never";
export const unknownTag: Tag = "unknown";
export const refTag: Tag = "ref";

// Named once for the emit branches that differ by dialect. Here rather than in
// jsonschema.ts because a schema with a `jsonSchema` of its own names it too,
// and may not import upwards.
export const openApi30 = "openapi-3.0";

// Tag (`tagFlags`): unknown 1, string 2, number 4, boolean 8, undefined 16,
// null 32, object 64, array 128, union 256, ref 512, bigint 1024, nan 2048,
// function 4096, instance 8192, symbol 16384, never 32768.
export const tagFlags: Record<Tag, number> = {
  [unknownTag]: 1,
  [stringTag]: 2,
  [numberTag]: 4,
  [booleanTag]: 8,
  [undefinedTag]: 16,
  [nullTag]: 32,
  [objectTag]: 64,
  [arrayTag]: 128,
  [anyOfTag]: 256,
  [refTag]: 512,
  [bigintTag]: 1024,
  [nanTag]: 2048,
  [functionTag]: 4096,
  [instanceTag]: 8192,
  [neverTag]: 32768,
  [symbolTag]: 16384,
};

// ── types ─────────────────────────────────────────────────────────────────────

export const vendor = "sury";
// Internal symbol to easily identify a SuryError instance.
export const s = /* @__PURE__ */ Symbol(vendor);
// Internal symbol to identify the item proxy (see the makeObjectVal Proxy use).
export const itemSymbol = /* @__PURE__ */ Symbol(vendor + ":item");

// Every number format describes integer-valued numbers — numberDecoder skips
// the "integer" check for any formatted source on that invariant.
export type NumberFormat = "int32" | "port" | "integer";
// Mirrored by `StringFormat` in index.d.ts, which is the surface TS users see —
// a name added here without being added there is invisible to them, and a third
// copy lives in `S.res`. Every member but `json`, `base64`, `base64url` and
// `cuid` is a JSON Schema format name verbatim, which is what lets jsonschema.ts
// pass it through in both directions; the content-family members name a keyword
// of their own (`contentMediaType`, `contentEncoding`) instead, which
// jsonschema.ts spells out per dialect.
export type StringFormat =
  | "json"
  | "base64"
  | "base64url"
  | "date-time"
  | "email"
  | "uuid"
  | "cuid"
  | "uri"
  | "date"
  | "time"
  | "duration"
  | "hostname"
  | "idn-hostname"
  | "ipv4"
  | "ipv6"
  | "uri-reference"
  | "uri-template"
  | "iri"
  | "iri-reference"
  | "idn-email"
  | "json-pointer"
  | "relative-json-pointer";
export type ArrayFormat = "compactColumns";
export type Format = NumberFormat | StringFormat | ArrayFormat;

export type BytesCodec = {
  toBytes: (text: string) => Uint8Array;
  fromBytes: (bytes: Uint8Array) => string;
};

export type AdditionalItemsMode = "strip" | "strict";

export type InvalidInputDetails = {
  code: "invalid_input";
  path: Path;
  reason: string;
  expected: Internal;
  received: Internal;
  input?: unknown;
  unionErrors?: SuryErrorRecord[];
}
export type InvalidOperationDetails = {
  code: "invalid_operation";
  path: Path;
  reason: string;
}
export type UnsupportedDecodeDetails = {
  code: "unsupported_decode";
  path: Path;
  reason: string;
  from: Internal;
  to: Internal;
}
export type InvalidConversionDetails = {
  code: "invalid_conversion";
  path: Path;
  reason: string;
  from: Internal;
  to: Internal;
  cause?: unknown;
}
export type UnrecognizedKeysDetails = {
  code: "unrecognized_keys";
  path: Path;
  reason: string;
  keys: string[];
}
export type ErrorDetails =
  | InvalidInputDetails
  | InvalidOperationDetails
  | UnsupportedDecodeDetails
  | InvalidConversionDetails
  | UnrecognizedKeysDetails;

export type SuryErrorRecord = Record<string, unknown> & {
  message: string;
  reason: string;
  path: Path;
}

export type AdditionalItems = AdditionalItemsMode | Internal;

export type SchemaErrorMessage = {
  // Catch-all override, used when no more specific key matches.
  _?: string;
  format?: string;
  type?: string;
  minimum?: string;
  maximum?: string;
  exclusiveMinimum?: string;
  exclusiveMaximum?: string;
  multipleOf?: string;
  minLength?: string;
  maxLength?: string;
  minItems?: string;
  maxItems?: string;
  minSize?: string;
  maxSize?: string;
  pattern?: string;
}

export type Internal = {
  type: Tag;
  // A serial number for the schema, used for caching operations.
  seq?: number;
  // Builder for transforming to the "to" schema. If missing, should apply
  // coercion logic.
  parser?: Builder;
  // A field on the "to" schema, to turn it into "parser", when reversing.
  serializer?: Builder;
  // Logic for built-in decoding to the schema type.
  decoder: Builder;
  // Logic for built-in encoding from the schema type.
  encoder?: Encoder;
  // Custom validations on input (before decoder).
  inputRefiner?: (input: Val) => Check[];
  // Custom validations on output (after decoder).
  refiner?: (input: Val) => Check[];
  // A schema we transform to.
  to?: Internal;
  // When transforming with changing shape, store from which path it came
  // from. For S.object, S.tuple, and S.shape.
  from?: string[];
  // The index of the flattened schema reshaping is happening from.
  fromFlattened?: number;
  flattened?: Internal[];
  const?: unknown;
  class?: unknown;
  name?: string;
  title?: string;
  description?: string;
  deprecated?: boolean;
  examples?: unknown[];
  default?: unknown;
  format?: Format;
  // The content axis (CONTENT_CODEC_SPEC.md): the schema this value's payload
  // is stored as inside a JSON document — base64 text for bytes, the JSON value
  // itself for a JSON document. Two schemas that agree on it carry the same
  // kind of payload, so a link between them is a plain transfer; two that
  // disagree have two readings of it (store the value, or open it) and the
  // conversion asks instead of guessing. Absent means the value carries no
  // payload of its own.
  // Written only through `setContent` (below), which keeps it non-enumerable.
  content?: Internal;
  // Bytes-as-text codec on a format singleton (`S.base64`, `S.base64url`).
  // Presence is the payload *kind* `B_contentDiffers` uses, so the two alphabets
  // are one family without importing either format into builder.ts. Always read
  // Carriers look it up off `content.bc`. Copies of a format keep `bc` so
  // alphabet recoding still sees it. `S.trim` targets `string`, which has none.
  // Short: this name is in `B_contentDiffers`, which ships in every export.
  bc?: BytesCodec;
  // Which reading of a content link the caller wrote, when they wrote one.
  // `opens` is the reading of the link that converts INTO this schema — `true`
  // opens the source and hands its payload over, `false` stores its value —
  // and `opensBack` the same for the reversed chain, where this schema is the
  // target instead. `reverse` trades the two, the way it trades
  // parser/serializer, so each direction's slot lands on the node the other
  // direction reads it from. Absent means the link's shape decides — see
  // `B_readsPayload` in builder.ts.
  opens?: boolean;
  opensBack?: boolean;
  // jsonString splices this value between bare quotes with no escaping, so
  // every value the schema admits must be free of `"`, `\`, controls and lone
  // surrogates. Set it only where that is proven — a pattern whose range
  // excludes them, or a conversion that manufactures the string — and re-run
  // `pnpm --filter=sury fuzz:escfree`, because getting it wrong emits broken
  // JSON rather than merely over-escaped JSON. `noValidation` voids the proof;
  // the read site handles that.
  escapeFree?: boolean;
  has?: Partial<Record<Tag, boolean>>;
  anyOf?: Internal[];
  additionalItems?: AdditionalItems;
  items?: Internal[];
  required?: string[];
  properties?: Record<string, Internal>;
  noValidation?: boolean;
  // Sury's own "this read may be absent" union — a dict value read by a fixed
  // key, modelled as `V | undefined`. The conversion rules (2-4) don't apply to
  // it: it isn't a user-written widening whose intent could be ambiguous, so
  // each variant converts to whatever the target is, and a variant with no
  // decoder to that target drops out with its error reported per value.
  perVariant?: boolean;
  // Which bounds the caller actually wrote. int32 and port put their own
  // range in the fields below, so the values can't tell a caller's bound from
  // a format's — this can, and only the bound constructors ever set it.
  // 1 lower inclusive · 2 upper inclusive · 4 lower exclusive · 8 upper
  // exclusive. A schema bounds exactly one of its value, its length or its
  // size, so one pair of bits covers minimum/minLength/minItems/minSize alike.
  bounds?: number;
  minimum?: number | bigint;
  maximum?: number | bigint;
  // S.gt/S.lt always land here and S.gte/S.lte always land on
  // minimum/maximum, whatever the numeric type — the bound a schema reports
  // is the one its author wrote, not an equivalent rewritten form.
  exclusiveMinimum?: number | bigint;
  exclusiveMaximum?: number | bigint;
  multipleOf?: number | bigint;
  minLength?: number;
  maxLength?: number;
  minItems?: number;
  maxItems?: number;
  // Bytes, for the binary instances. No JSON Schema keyword bounds a blob's
  // size, so unlike the four above these don't reach the emit.
  minSize?: number;
  maxSize?: number;
  pattern?: RegExp;
  errorMessage?: SchemaErrorMessage;
  space?: number;
  // Marks `S.protobuf`: `toProto` finds it on a chain by this rather than by
  // the codec's encoder, which would drag the codec into a `toProto`-only
  // bundle.
  w?: true;
  pb?: unknown;
  // Compile-time only, set on a per-operation schema copy by the container
  // decoders' jsonString fusion (B_fuseIntoJsonString in composites.ts): the
  // container's dynamic items are typed but UNVALIDATED — the validation loop
  // was skipped because jsonStringAggregate re-parses each item from unknown
  // inside its own serialize loop. Carried on the schema (not the val) so it
  // survives the parse loop's per-segment B_refine.
  uv?: boolean;
  // Compile-time only, and `unionRewrite` (union.ts) is the ONLY producer: this
  // union's variants were rewritten from the variants of the union the value
  // was already typed as, so a dispatched case may convert from its own variant
  // instead of re-validating it. Spelling it `true` anywhere else licenses
  // skipping checks the value never passed — the rewrite is what makes it true,
  // because it drops the val's source to `unknown` and would otherwise lose the
  // guarantee the source union carried.
  tr?: boolean;
  "$ref"?: string;
  "$defs"?: Record<string, Internal>;
  // Written by compileDecoder onto the schema it compiled against, read back by
  // `S.recursive`: a recursive definition compiles optimistically, and these
  // two are what its inner circular references assume and what the fixpoint
  // compares to decide whether to recompile. Absent means "this schema has not
  // been compiled against yet", which is why `codecTo` deletes rather than
  // clears them. Nothing derives them without compiling, so there is no probe
  // to ask a schema whether it is async — an operation reports that by
  // rejecting.
  isAsync?: boolean;
  hasTransform?: boolean;
  "~standard"?: unknown;
  // Overrides how inputExpression renders this schema. Only for a schema whose
  // expression its tag can't produce — compactColumns, whose columns live on
  // the `.to` target. Everything structural is rendered by inputExpression
  // itself, so setting this is the exception, not the pattern.
  expression?: (schema: Internal) => string;
  // What this schema adds to the JSON Schema of a value that decodes to it.
  // jsonschema.ts reads it off `.to` and never off the schema being converted:
  // a schema whose own input isn't JSON has no document and must keep failing
  // the conversion. Unlike S.extendJSONSchema, which holds one document for
  // every dialect, it can answer per target. `unknown` because base.ts imports
  // nothing and `JSONSchemaT` lives upwards; the single read casts.
  jsonSchema?: (schema: Internal, target: string) => unknown;
  // The reversed (Input ↔ Output swapped) schema. Always readable: `this` via
  // the self-reverse prototype getter, otherwise computed and cached by the
  // general prototype getter (parse.ts). Reading it on a plain schema COMPUTES
  // the reverse — probe `sr` instead when only self-reverseness is asked.
  r?: Internal;
  // Set on the self-reverse prototype only — the cheap "reverses to itself"
  // probe (see selfReversePrototype below).
  sr?: boolean;
}

export type BGlobal = {
  // @as("v") — varCounter
  v: number;
  // @as("o") — flag
  o: number;
  // @as("e") — embeded
  e: unknown[];
  // @as("d") — defs
  d?: Record<string, Internal>;
  // @as("t") — throwCounter. Bumped by every helper that emits a raise into
  // generated code, so a builder can bracket a stretch of emission and learn
  // whether what it produced can throw. Read the difference, never the value.
  t: number;
  // @as("js") — the operation's asJsonString embed accessor, cached by
  // B_embedJsonStr (advanced/json.ts) on first use.
  js?: string;
}

// Adjacent checks sharing `fail` by reference equality are fused with `&&`
// in `emitChecks`, so pass the same helper (e.g. failInvalidType) to every
// check on a val if you want them to emit as one `||`-throw line.
export type Check = {
  // @as("c") — cond
  c: (inputVar: string) => string;
  // @as("f") — fail
  f: (input: Val) => (value: unknown) => ErrorDetails;
}

export type Val = {
  // We might have the same value, but different instances of the val
  // object. Use the bond field, to connect the var call. @as("b") — bond
  b?: Val;
  // @as("p") — parent
  p?: Val;
  // @as("v") — var
  v: () => string;
  // @as("i") — inline
  i: string;
  // The schema of the value that is being parsed. @as("s") — schema
  s: Internal;
  // Whether the val is at output part of expected schema. Needed for
  // schemas like S.array(S.nullAsOption) where child schemas might be
  // transformed. @as("io") — isOutput
  io?: boolean;
  // The schema of the value that we expect to parse into. @as("e") — expected
  e: Internal;
  prev?: Val;
  // @as("f") — flag
  f: Flag;
  // @as("d") — vals
  d?: Record<string, Val>;
  // @as("fv") — flattenedVals
  fv?: Val[];
  // @as("cp") — codeFromPrev
  cp: string;
  // Comma-joined `let` declarations hoisted onto this val by descendants
  // that couldn't own them. Emitted after this val's checks in `merge` (the
  // old varsAllocation slot). @as("hd") — hoistedDecls
  hd: string;
  // Set by `merge` once this val's code is emitted, so a later cached-bond
  // materialization re-reads inline instead of hoisting onto it (#240).
  // @as("fz") — finalized
  fz?: boolean;
  // Invariant: absent iff no checks. Never stored as `[]` so callers can
  // test presence with a plain truthy check instead of length.
  // @as("vc") — checks
  vc?: Check[];
  // @as("u") — isUnion
  u?: boolean;
  // Whether the chain starting from the root prev has a transformation.
  // @as("t") — hasTransform
  t?: boolean;
  path: Path;
  // @as("g") — global
  g: BGlobal;
  // This is to mark an object field as optional. Fields like this should be
  // skipped when the value is undefined. @as("o") — optional
  o?: boolean;
}

// Shared `undefined` for every value-position use across the implementation:
// a bare `undefined` minifies to `void 0` (6 chars), this const to 1. Never
// interpolate it into generated-code strings — emitted JS text keeps literal
// `void 0`.
export const U = undefined;

export const immutableEmptyArray: unknown[] = [];
// Null-prototype: used as a schema's `properties` placeholder, so an
// indexed/`in` lookup for a field named after an Object.prototype member
// (constructor, toString, hasOwnProperty, ...) must not resolve to
// something inherited instead of correctly reporting "no such property".
export const immutableEmptyObject: Record<string, unknown> = Object.create(null);

// Probe the Standard Schema marker's *presence* with `in` instead of reading
// it: the `~standard` prototype getter allocates a fresh StandardProps object
// (+4 closures) on every access, and this runs per-node while building every
// `S.schema({...})`. `in` walks the prototype chain without invoking the
// getter. The `typeof === object` guard keeps primitives (passed by
// `assert`) from throwing on `in` and reproduces the old falsy-on-primitive
// result.
export const isSchemaObject = (obj: unknown): boolean =>
  typeof obj === objectTag && obj !== null && "~standard" in (obj as object);

export const constField = "const";
export const isLiteral = (schema: Internal): boolean => constField in schema;

export const isOptional = (schema: Internal): boolean =>
  schema.type === undefinedTag || (schema.type === anyOfTag && undefinedTag in schema.has!);

// The constructor name worth printing, or a falsy value for anything a reader
// would learn nothing from: a plain object, a null prototype, an anonymous
// class (whose `name` is the empty string). Both callers below key off exactly
// this distinction — one to name the value, the other to decide whether to look
// inside it — so the `Object` comparison is written once.
// Throws on null; both callers exclude it first.
const namedConstructor = (unknown: unknown): string | undefined | false => {
  const ctor = (Object.getPrototypeOf(unknown) as { constructor?: { name?: string } } | null)
    ?.constructor;
  return ctor !== Object && ctor?.name;
}

// Names a value without looking inside it: the rendering every value gets when
// it is not the top level of a message. Zod, Valibot and ArkType print this at
// every level; `stringify` below adds one level of detail on top.
const stringifyLeaf = (unknown: unknown): string => {
  const tagFlag = tagFlags[typeof unknown as Tag]!;

  if ((tagFlag & 16)) {
    return undefinedTag;
  } else if ((tagFlag & (64 | 4096))) {
    // A named constructor is the whole diagnostic (Date, Map, Foo); anything
    // else is lowercase `object`, naming the value by type the way `string` and
    // `number` do rather than by its `Object` constructor.
    // Arrays carry their length: against a tuple, the length is the diagnostic.
    return unknown === null
      ? nullTag
      : Array.isArray(unknown)
        ? `Array(${unknown.length})`
        : namedConstructor(unknown) || objectTag;
  } else if ((tagFlag & 2)) {
    return `"${unknown as string}"`;
  } else if ((tagFlag & 1024)) {
    return `${unknown as bigint}n`;
  } else {
    return (unknown as { toString: () => string }).toString();
  }
}

// Renders a runtime value for the `received` half of an error message: a plain
// object or array expanded exactly one level, anything else named.
//
// Recursing without a limit is what let a cyclic value overflow the stack
// *inside the error formatter*; stopping at depth 1 keeps that fixed while
// still showing the shape that actually failed. One level is enough because a
// nested failure already reports its path (`Failed at user.id`) — the
// expansion is for "wrong shape entirely", which is visible at the top.
//
// Entries are capped for the same reason depth is: a 40-key input would
// otherwise produce a several-hundred-character message. The literal 5 is
// written out at both uses because esbuild does not inline a module-level
// const number.
export const stringify = (unknown: unknown): string => {
  if (unknown !== null && typeof unknown === objectTag) {
    if (Array.isArray(unknown)) {
      const items = unknown as unknown[];
      let body = "";
      for (let idx = 0; idx < items.length; idx++) {
        if (idx === 5) {
          body += ", ...";
          break;
        }
        body += (idx ? ", " : "") + stringifyLeaf(items[idx]);
      }
      return `[${body}]`;
    }
    if (!namedConstructor(unknown)) {
      const dict = unknown as Record<string, unknown>;
      let body = "";
      let count = 0;
      for (const key in dict) {
        if (count++ === 5) {
          body += "... ";
          break;
        }
        body += key + ": " + stringifyLeaf(dict[key]) + "; ";
      }
      return body ? `{ ${body}}` : "{}";
    }
  }
  return stringifyLeaf(unknown);
}

// `expression` sits after `const` and before the structural tags, so an override
// beats the shape it overrides while a literal still outranks both. It also has
// to beat the `format` fallback below: compactColumns is the sole array format.
//
// `skipOverride` renders the shape an override would have replaced. It exists
// for an override that wraps its own schema's rendering rather than replacing
// it — a bound, the only one today (`setBoundExpression` in refinements.ts) —
// which has to ask for the base rendering of the very schema whose `expression`
// is mid-call, and would recurse forever without this.
// @__NO_SIDE_EFFECTS__
export const inputExpression = (schema: Internal, skipOverride?: boolean): string => {
  if (schema.name) {
    return schema.name;
  } else if (schema.const !== U) {
    return stringify(schema.const);
  } else if (schema.expression && !skipOverride) {
    return schema.expression(schema);
  } else if (schema.anyOf !== U) {
    // Repeated members remain significant to decoding (the same effectful schema
    // may intentionally run more than once), but not to the expression. Deduping
    // on rendered text rather than identity means members which genuinely differ
    // but render alike — two distinct classes both named Foo — collapse, so this
    // is not a member count.
    const anyOf = schema.anyOf;
    const seen = new Set<string>();
    let body = "";
    for (let idx = 0; idx < anyOf.length; idx++) {
      const expression = inputExpression(anyOf[idx]!);
      if (!seen.has(expression)) {
        seen.add(expression);
        body += (body ? " | " : "") + expression;
      }
    }
    return body;
  } else if (schema.type === objectTag) {
    // Properties and an index signature share one accumulator: no factory
    // produces both at once today, but the shape is representable, and the
    // branchy version silently dropped the index signature.
    const properties = schema.properties!;
    const additionalItems = schema.additionalItems;
    let body = "";
    for (const location in properties) {
      body += location + ": " + inputExpression(properties[location]!) + "; ";
    }
    if (typeof additionalItems === objectTag) {
      body += "[key: string]: " + inputExpression(additionalItems as Internal) + "; ";
    }
    return body ? `{ ${body}}` : "{}";
  } else if (schema.type === arrayTag) {
    const additionalItems = schema.additionalItems;
    if (typeof additionalItems === objectTag) {
      const item = additionalItems as Internal;
      const itemName = inputExpression(item);
      // A bound or divisor reads as part of the item, not the array:
      // `int32 > 5[]` parses as an array-typed bound and `number % 2[]` as an
      // array-typed divisor, the same ambiguity a union has.
      return (item.type === anyOfTag || item.bounds !== U || item.multipleOf !== U
        ? `(${itemName})`
        : itemName) + "[]";
    }
    const items = schema.items!;
    let body = "";
    for (let idx = 0; idx < items.length; idx++) {
      body += (idx ? ", " : "") + inputExpression(items[idx]!);
    }
    return `[${body}]`;
  } else if (schema.format) {
    return schema.format;
  } else if (schema.type === instanceTag) {
    return (schema.class as { name: string }).name;
  } else {
    return schema.type;
  }
}

// ── schema ────────────────────────────────────────────────────────────────────

export function Schema(this: Internal): void {}
export const schemaPrototype: Record<string, unknown> = Object.create(null);
// A plain (non-enumerable) method, not a getter returning a closure: the
// getter form allocated a fresh arrow on every `.with` access, and `.with` is
// the primary modifier API called all over user construction code. The method
// binds `this` through the call, so no per-access closure is needed.
Object.defineProperty(schemaPrototype, "with", {
  value(this: Internal, fn: (self: Internal, ...args: unknown[]) => unknown, ...args: unknown[]): unknown {
    return fn(this, ...args);
  },
});
// Also has ~standard below
Schema.prototype = schemaPrototype;

// A self-reversing schema answers `reversed` from this prototype getter
// instead of an own property: the per-instance defineProperty cost an order
// of magnitude more than everything else baseSchema does. Object.assign never
// copies the getter, so a derived schema (copySchema) recomputes its reverse —
// correct, since a copy made to be modified no longer reverses to itself.
// No setter, so a plain `schema.reversed = …` throws: the cache is only ever
// written with defineProperty (parse.ts).
//
// `sr` is the cheap self-reverse probe: reading `.reversed` off a plain schema
// would *compute* the reverse (the general getter in parse.ts), so callers
// that only ask "does it reverse to itself?" (composites) read the marker.
// "r", not "reversed": internal-only (S.reverse is the public API), and short
// field names on hot objects survive minification (CLAUDE.md).
export const reversedKey = "r";
function SelfReverseSchema(this: Internal): void {}
const selfReversePrototype: Record<string, unknown> = Object.create(schemaPrototype);
Object.defineProperty(selfReversePrototype, reversedKey, {
  get() {
    return this;
  },
});
Object.defineProperty(selfReversePrototype, "sr", { value: true });
SelfReverseSchema.prototype = selfReversePrototype;

let seq = 1;

let exnId: unknown = {};
export const __setExnId = (id: unknown): void => {
  exnId = id;
}

export class SuryError extends Error {
  constructor(params: ErrorDetails | Record<string, unknown>) {
    super();
    Object.assign(this, params);
  }
  get message(): string {
    return formatErrorMessage(this as unknown as SuryErrorRecord);
  }
  get _1(): this {
    return this;
  }
  get RE_EXN_ID(): unknown {
    return exnId;
  }
}
Object.defineProperty(SuryError.prototype, "name", { value: "SuryError" });
Object.defineProperty(SuryError.prototype, "s", { value: s });

export const getOrRethrow = (exn: unknown): SuryErrorRecord => {
  if (exn && (exn as { s?: symbol }).s === s) return exn as SuryErrorRecord;
  throw exn;
}

// Internal invariant/misuse errors (bad schema construction, not input
// validation) — intentionally a plain Error, not SuryError: there's no
// ErrorDetails shape (code/path/reason) to attach at these call sites.
export const panic = (message: string): never => {
  throw new Error(`[Sury] ${message}`);
}

const formatErrorMessage = (error: SuryErrorRecord): string =>
  `${error.path.length ? `Failed at ${pathToText(error.path)}: ` : ""}${error.reason}`;

export const errorClass: unknown = SuryError;

export type GlobalConfig = {
  m: (error: SuryErrorRecord) => string; // messageFormatter
  d?: Record<string, Internal>; // defsAccumulator
  a: AdditionalItems; // defaultAdditionalItems
  f: Flag; // defaultFlag
}

export type GlobalConfigOverride = {
  defaultAdditionalItems?: AdditionalItemsMode;
  disableNanNumberValidation?: boolean;
}

export const initialOnAdditionalItems: AdditionalItemsMode = "strip";
export const initialDefaultFlag: Flag = 0;
export const globalConfig: GlobalConfig = {
  m: formatErrorMessage,
  d: U,
  a: initialOnAdditionalItems,
  f: initialDefaultFlag,
};

export const valueOptions: Record<string, unknown> = {};
export const configurableValueOptions = { configurable: true };
export const valKey = "value";

// `function` declarations have no construct signature in TS, so `new` needs a
// cast. A type is erased where a `const SchemaCtor = Schema as …` alias would
// survive minification as a real assignment.
type SchemaClass = new () => Internal;

// `decoder` is a parameter, not something the caller assigns afterwards, and
// that is load-bearing: a schema handed to a builder as a val's `s` becomes
// that value's output schema, an output schema is reachable as another
// operation's *target*, and the parse loop calls `e.decoder` on a target
// unconditionally. A site that forgot the assignment produced a TypeError deep
// inside compilation (#369); requiring the argument makes that unrepresentable.
// It also means every schema gains its fields in one order, so the instances
// share a single hidden class.
export const baseSchema = (tag: Tag, selfReverse: boolean, decoder: Builder): Internal => {
  const schema = new ((selfReverse ? SelfReverseSchema : Schema) as unknown as SchemaClass)();
  schema.type = tag;
  schema.seq = seq++;
  schema.decoder = decoder;
  return schema;
}

export const noopDecoder: Builder = (input: Val) => input;

// Every built-in singleton schema must be a module-level const initialized by
// a single `/* @__PURE__ */ initSchema(...)` expression: the module system is
// what guarantees one instance per schema (the compiled-decoder cache in
// getDecoder is keyed by `seq` and stored on the instance, so a fresh copy
// per use would recompile every time), and the single pure expression is what
// lets a consumer's bundler drop the unused ones.
// @__NO_SIDE_EFFECTS__
export const initSchema = (
  tag: Tag,
  decoder: Builder,
  init?: (schema: Internal) => void
): Internal => {
  const schema = baseSchema(tag, true, decoder);
  return init?.(schema), schema;
}

// Deliberately NOT the `/* @__PURE__ */` form the other singletons use:
// `unknown` is reachable from nearly every export, so it never tree-shakes
// anyway, and the bare call minifies smaller than any wrapper that would make
// it droppable.
export const unknown: Internal = baseSchema(unknownTag, true, noopDecoder);

export const copySchema = (schema: Internal): Internal => {
  const c: Internal = Object.assign(new (Schema as unknown as SchemaClass)(), schema);
  c.seq = seq++;
  // `content` is non-enumerable, so Object.assign skips it — carried by hand
  // here, which is also the only place that pays for it.
  if (schema.content !== U) setContent(c, schema.content);
  if (schema.bc !== U) setBytesCodec(c, schema.bc);
  return c;
}

// `S.base64` and `S.json` are their own content, and an enumerable
// self-reference makes `JSON.stringify(schema)` — and every error that embeds
// one — throw on a cycle. Non-enumerable everywhere rather than only there, so
// a carrier and its copies agree on the field count `unionIsTransparent` walks.
export const setContent = (schema: Internal, content: Internal): void => {
  valueOptions[valKey] = content;
  Object.defineProperty(schema, "content", valueOptions as PropertyDescriptor);
}

export const setBytesCodec = (schema: Internal, codec: BytesCodec): void => {
  valueOptions[valKey] = codec;
  Object.defineProperty(schema, "bc", valueOptions as PropertyDescriptor);
}

export const updateOutput = <TValue>(schema: Internal, fn: (schema: Internal) => void): TValue => {
  const root = copySchema(schema);
  let mut = root;
  while (mut.to) {
    const next = copySchema(mut.to);
    mut.to = next;
    mut = next;
  }
  // This should be the Output schema
  fn(mut);
  return root as unknown as TValue;
}

export const setHas = (has: Partial<Record<Tag, boolean>>, tag: Tag): void => {
  has[(tagFlags[tag]! & (256 | 512)) ? unknownTag : tag] = true;
}

// The JSON Schema pointer prefix. Shared rather than owned by jsonschema.ts:
// `S.recursive` mints `$ref`s against it and `S.json` names itself through it,
// and both sit below the converter in the layering.
export const defsPath = `#/$defs/`;

// `S.json`'s schema identity, recognised by name where importing the schema
// itself would close a cycle (composites' JSON-sourced object reads).
export const jsonName = `JSON`;
