// The base layer: the data model (`Internal`, `Val`, `Check`), the schema
// object and its prototype, tags, flags and paths - merged into one module
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

// Bit-flag literals (esbuild does not inline named consts).
//
// Compile semantics (`g.o` / op flag), 127 and below - what the generated code
// itself does: 0 none, 1 async, 2 disableNaN, 4 union-transform-context (custom
// transform inside a union case preserves the original exception so dispatch
// can distinguish Sury failures from foreign ones), 64 flatten.
//
// Return modes, 128 and above - what the operation hands back, read only by the
// operation tail (parse.ts, operations.ts): 128 JS Result
// (`{success, value, error}`), 256 ReScript Result (`{TAG, _0}`), 512
// promisable (1 without lifting a synchronous result into a promise), 1024
// Standard Schema (`{value}` / `{issues}`), 2048 yield the operation's input
// rather than its output (`makeInput`/`makeOutput`), 4096 answer a boolean
// (`isInput`/`isOutput`).
//
// Bit 1 permits async, it does not assert it: codegen may read `g.o & 1` as
// "a promise MAY appear here" (json.ts declines to fuse), never as "one will".
// An async operation also rejects rather than throwing when its value fails
// before the first await - decided by the operation tail (operations.ts), from
// whether the compile is nested, not by a flag of its own.
//
// The split at 128 is load-bearing: a nested operation compiled inside another
// (recursive.ts) masks with `& 127`, because generated code consumes its result
// and a return mode inherited from the outer operation would have the inner one
// answering `false` - or a Result object - into the middle of a value.
// 8192 is recursive.ts's memo-key bit for such a nested async node, which no
// operation flag ever carries.
//
// The modes ride the op flag the operation memo keys on, which is what makes
// each of them compile and cache as its own operation and leaves the throw
// path's generated code untouched.
// Val (`Val.f`): 0 none, 1 async.

// ── path ──────────────────────────────────────────────────────────────────────

// Root-first. Static segments are strings; a runtime loop index or dict key is
// a PathDyn `{e}` (the JS expression), not the string "[]". Baking "[]" into
// Path would collide with a field actually named "[]". compilePath projects
// PathDyn to "[]" for compile-time throws and JSON Schema; pathExpr splices
// `e` into generated fail calls so the runtime error carries the real index
// without a per-item try/catch.
//
// Never mutated: details objects, codegen closures and retained user errors
// share instances, so every prepend/concat allocates. A symbol only ever
// arrives from a user-written `path` (`S.refine`): every key codegen turns into
// a segment comes from `Object.keys` or a generated `for-in`, which skip them.
export type Path = readonly (string | number | symbol)[];
// A codegen path segment whose value is a generated JS expression (loop index,
// `for-in` key). The only object that appears in a CodePath.
export type PathDyn = { e: string };
export type CodePath = readonly (string | number | symbol | PathDyn)[];

export const pathEmpty: Path = [];
export const pathDynamic: Path = ["[]"];

export const hasPathDyn = (path: CodePath): boolean => {
  for (let i = 0; i < path.length; i++) if (typeof path[i] === "object") return true;
  return false;
};

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

// Property access: `i.id`, `i["my key"]`, `i["__proto__"]`. Digit-only keys
// stay quoted unless `numeric`: `obj[00]` is `obj[0]` in sloppy `new Function`,
// and an object key `"00"` is not `"0"`. Array index sites pass `numeric`.
export const inlinedProperty = (obj: string, key: string, numeric?: boolean): string =>
  numeric && /^\d+$/.test(key)
    ? `${obj}[${key}]`
    : key === "__proto__" || !jsIdentRe.test(key)
      ? `${obj}[${inlinedValueFromString(key)}]`
      : `${obj}.${key}`;

// @__NO_SIDE_EFFECTS__
export function pathConcat(path: Path, concatedPath: Path): Path;
export function pathConcat(path: CodePath, concatedPath: CodePath): CodePath;
export function pathConcat(path: CodePath, concatedPath: CodePath): CodePath {
  return path.length ? (concatedPath.length ? path.concat(concatedPath) : path) : concatedPath;
}

export const pathExpr = (path: CodePath, rest?: string): string => {
  let out = "[";
  for (let i = 0; i < path.length; i++) {
    if (i) out += ",";
    const s = path[i]!;
    out +=
      typeof s === "object"
        ? s.e
        : typeof s === "string"
          ? inlinedValueFromString(s)
          : (s as number);
  }
  if (rest) out += (path.length ? "," : "") + rest;
  return out + "]";
};

// Compile-time throw: a PathDyn has no value yet, so it becomes "[]".
export const compilePath = (path: CodePath): Path => {
  if (!hasPathDyn(path)) return path as Path;
  const out: (string | number | symbol)[] = [];
  for (let i = 0; i < path.length; i++) {
    const s = path[i]!;
    out.push(typeof s === "object" ? "[]" : s);
  }
  return out;
};

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

// Every number format describes integer-valued numbers - numberDecoder skips
// the "integer" check for any formatted source on that invariant.
export type NumberFormat = "int32" | "port" | "integer";
// Mirrored by `StringFormat` in index.d.ts, which is the surface TS users see -
// a name added here without being added there is invisible to them, and a third
// copy lives in `S.res`. The name is also what `inputExpression` renders, so it
// is the word every error message about the schema says.
//
// Members fall in three groups. Most are a JSON Schema format name verbatim,
// which is what lets jsonschema.ts pass them through in both directions. The
// content family (`json`, `base64`, `base64url`) names a keyword of its own
// (`contentMediaType`, `contentEncoding`) instead, which jsonschema.ts spells
// out per dialect. The rest have no JSON Schema keyword of that name and travel
// as `pattern`, listed in `jsonSchemaFormat` there - a member added here without
// an entry in that list emits a `format` no validator knows.
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
  | "relative-json-pointer"
  | "cuid2"
  | "ulid"
  | "ksuid"
  | "xid"
  | "nanoid"
  | "uuidv4"
  | "uuidv6"
  | "uuidv7"
  | "e164"
  | "mac"
  | "hex"
  | "cidrv4"
  | "cidrv6"
  | "http-url"
  | "env"
  | "queryString";
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
export type UnrecognizedKeyDetails = {
  code: "unrecognized_key";
  path: Path;
  reason: string;
  key: string;
}
export type ErrorDetails =
  | InvalidInputDetails
  | InvalidOperationDetails
  | UnsupportedDecodeDetails
  | InvalidConversionDetails
  | UnrecognizedKeyDetails;

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
  // Used as the operation-cache key.
  seq?: number;
  // Builder for transforming to the "to" schema. If missing, should apply
  // coercion logic.
  parser?: Builder;
  // A field on the "to" schema, to turn it into "parser", when reversing.
  serializer?: Builder;
  decoder: Builder;
  encoder?: Encoder;
  inputRefiner?: (input: Val) => Check[];
  refiner?: (input: Val) => Check[];
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
  // is stored as inside a JSON document - base64 text for bytes, the JSON value
  // itself for a JSON document. Two schemas that agree on it carry the same
  // kind of payload, so a link between them is a plain transfer; two that
  // disagree have two readings of it (store the value, or open it) and the
  // conversion asks instead of guessing. Absent means the value carries no
  // payload of its own.
  // On a string source it is a claim, and `format` is its verification: a
  // carrier's opened text (`openedText`) and a union's per-member narrow both
  // carry the marker without the format, and a decoder handed that pair must
  // check the text, never escape it as a value - `S.jsonString` inside
  // `S.optional` used to serialize `"a"` to `"\"a\""` for exactly that reason.
  // Written only through `setContent` (below), which keeps it non-enumerable.
  content?: Internal;
  // Bytes-as-text codec on a format singleton (`S.base64`, `S.base64url`).
  // Presence is the payload *kind* `B_contentDiffers` uses, so the two alphabets
  // are one family without importing either format into builder.ts - which holds
  // only while both sides of that comparison are content markers: a format's own
  // schema also carries `bc`, so passing one there reads as "same kind" against
  // any bytes marker. Carriers look it up off `content.bc`. Copies of a format
  // keep `bc` so alphabet recoding still sees it, `S.trim`'s tail included.
  bc?: BytesCodec;
  // The reading of the content link that converts INTO this schema
  // (CONTENT_CODEC_SPEC.md): `true` opens the source and hands its payload
  // over, `false` stores its value. One field for one link: the encode
  // reading is its negation, so `reverse` writes each node's from its forward
  // successor's rather than carrying a second slot.
  // Written by a slot the caller gave (rule 1), by a payload gaining a `.to`
  // (rule 3, materialized by `codecTo` and `compileChain` the moment it
  // becomes true, since `reverse` re-points `.to` and would lose it), and by
  // the document piece a field is stored into (rule 2, `jsonPiece`). Absent on
  // a link between two payloads of different kinds is therefore rule 4, and
  // the payload schemas reject it while compiling (`B_rejectUnsettled`).
  opens?: boolean;
  // Properties of every value a string schema admits, which let generated code
  // skip work: 1 escape-free (no `"`, `\`, controls or lone surrogates, so
  // jsonString splices it between bare quotes with no escaping). Set the bit
  // only where that is proven - a pattern whose range excludes the characters,
  // or a conversion that manufactures the string - and re-run
  // `pnpm --filter=sury fuzz:escfree`, because getting it wrong emits broken
  // JSON rather than merely over-escaped JSON. `noValidation` voids the proof;
  // the read sites handle that.
  formatFlag?: number;
  has?: Partial<Record<Tag, boolean>>;
  anyOf?: Internal[];
  additionalItems?: AdditionalItems;
  items?: Internal[];
  required?: string[];
  properties?: Record<string, Internal>;

  noValidation?: boolean;
  // Sury's own "this read may be absent" union - a dict value read by a fixed
  // key, modelled as `V | undefined`. The conversion rules (2-4) don't apply to
  // it: it isn't a user-written widening whose intent could be ambiguous, so
  // each variant converts to whatever the target is, and a variant with no
  // decoder to that target drops out with its error reported per value.
  perVariant?: boolean;
  // Which bounds the caller actually wrote. int32 and port put their own
  // range in the fields below, so the values can't tell a caller's bound from
  // a format's - this can, and only the bound constructors ever set it.
  // 1 lower inclusive · 2 upper inclusive · 4 lower exclusive · 8 upper
  // exclusive. A schema bounds exactly one of its value, its length or its
  // size, so one pair of bits covers minimum/minLength/minItems/minSize alike.
  bounds?: number;
  minimum?: number | bigint;
  maximum?: number | bigint;
  // S.gt/S.lt always land here and S.gte/S.lte always land on
  // minimum/maximum, whatever the numeric type - the bound a schema reports
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
  // Marks `S.protobuf`: `toProtoOrThrow` finds it on a chain by this rather
  // than by the codec's encoder, which would drag the codec into a
  // `toProtoOrThrow`-only bundle.
  protobufWire?: true;
  // What `S.protobufField` stored (`StoredField` in advanced/protobufField.ts).
  protobufField?: unknown;
  // Compile-time only, set on a per-operation schema copy by `fz` below: the
  // container's decoder left its contents to jsonStringAggregate, which walks
  // them once inside its own serialize pass. 1 the contents are UNVALIDATED -
  // the aggregate parses each from unknown; 2 they are typed, and it parses
  // each from the type it claims, so a trusted source is not re-checked. Both
  // mean the decoder emitted no walk, which is what the whole-value
  // JSON.stringify paths must not assume. Carried on the schema (not the val)
  // so it survives the parse loop's per-segment B_refine.
  uv?: number;
  // On a target that builds its document piecewise (`S.jsonString`): asked by
  // a container decoder (`B_fused` in composites.ts) whose `.to` it is, with
  // the dynamic item for an array or dict, and answers the container's schema
  // marked `uv` when the aggregate may validate it, or undefined to validate
  // here as usual. Lives on the target so a bundle without it ships nothing
  // of the decision.
  fz?: (input: Val, container: Internal, item?: Internal) => Internal | undefined;
  // Compile-time only, and `unionRewrite` (union.ts) is the ONLY producer: this
  // union's variants were rewritten from the variants of the union the value
  // was already typed as, so a dispatched case may convert from its own variant
  // instead of re-validating it. Spelling it `true` anywhere else licenses
  // skipping checks the value never passed - the rewrite is what makes it true,
  // because it drops the val's source to `unknown` and would otherwise lose the
  // guarantee the source union carried.
  tr?: boolean;
  "$ref"?: string;
  "$defs"?: Record<string, Internal>;
  // `S.json` and every copy of one: the marker that answers "is this the whole
  // document rather than a rendering of one", which several structural
  // decisions turn on. Nothing already on the schema answers it. Identity and
  // `content === schema` both fail because a chain node that IS json is a
  // `copySchema` of it; `name` and `$ref` are forgeable, since
  // `S.recursive("JSON", …)` builds the same `$ref`; and the decoder's
  // identity, which would need no field at all, is unreachable from `parse`,
  // `composites` and `modifiers` - all three read this and all three sit above
  // `advanced/json`, whose `S.json` is built at module init from `dictFactory`,
  // so inverting that import leaves the factory in TDZ.
  // Enumerable, so `Object.assign` carries it onto a copy; nothing public
  // writes it.
  isJson?: boolean;
  "~standard"?: unknown;
  // Overrides how inputExpression renders this schema. Only for a schema whose
  // expression its tag can't produce - compactColumns, whose columns live on
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
  // the reverse - probe `sr` instead when only self-reverseness is asked.
  r?: Internal;
  // Set on the self-reverse prototype only - the cheap "reverses to itself"
  // probe (see selfReversePrototype below).
  sr?: boolean;
}

export type BGlobal = {
  // @as("v") - varCounter
  v: number;
  // @as("o") - flag
  o: number;
  // @as("e") - embeded
  e: unknown[];
  // @as("d") - defs
  d?: Record<string, Internal>;
  // @as("t") - throwCounter. Bumped by every helper that emits a raise into
  // generated code, so a builder can bracket a stretch of emission and learn
  // whether what it produced can throw. Read the difference, never the value.
  t: number;
  // @as("r") - set by the one builder that assigns to the operation's own
  // parameter (union dispatch), so a `make*` tail knows the parameter no
  // longer holds the value it was given.
  r?: boolean;
  // @as("js") - the operation's asJsonString embed accessor, cached by
  // B_embedJsonStr (advanced/json.ts) on first use.
  js?: string;
  // @as("f") - the compiled operation, stored by `compileDecoder` once it
  // exists. The throw boundary's stack capture cuts the trace at it, and the
  // emitter that needs it runs first, so it is read through this rather than
  // closed over.
  f?: unknown;
  // @as("x") - exit. Where a failed check goes when the code around it can be
  // left by a jump rather than a raise: handed the failure's record (a thunk,
  // so a context that needs no reason embeds no builder), it answers the
  // statement to run - `return false` for a boolean operation, `return` of the
  // failure for one that answers with a Result, record-and-break for a union
  // case a later case may still accept - or nothing, for the raise a failure is
  // by default. Absent means raise everywhere.
  //
  // A jump can't leave a function, so whatever emits code into a callback (a
  // `.then`, an async dispatch) clears it for that stretch (`B_detached`).
  x?: (record?: Failure) => string | undefined;
  // @as("j") - jump counter, `t`'s twin: bumped by every failed check that
  // took `x`. Read the difference, never the value.
  j: number;
}

// A failure's record, as the expression that builds it. `d`, where present, is
// the same record left unbuilt - the builder and its arguments as a list - for
// an exit that keeps it and may never read it (union.ts).
export type Failure = { (): string; d?: () => string };

// Adjacent checks sharing `fail` by reference equality are fused with `&&`
// in `emitChecks`, so pass the same helper (e.g. failInvalidType) to every
// check on a val if you want them to emit as one `||`-throw line.
export type Check = {
  // @as("c") - cond
  c: (inputVar: string) => string;
  // @as("f") - fail
  f: (input: Val) => (value: unknown, path?: Path) => ErrorDetails;
}

export type Val = {
  // We might have the same value, but different instances of the val
  // object. Use the bond field, to connect the var call. @as("b") - bond
  b?: Val;
  // @as("p") - parent
  p?: Val;
  // @as("v") - var
  v: () => string;
  // @as("i") - inline
  i: string;
  // The schema of the value that is being parsed. @as("s") - schema
  s: Internal;
  // Whether the val is at output part of expected schema. Needed for
  // schemas like S.array(S.nullAsOption) where child schemas might be
  // transformed. @as("io") - isOutput
  io?: boolean;
  // The schema of the value that we expect to parse into. @as("e") - expected
  e: Internal;
  prev?: Val;
  // @as("f") - flag
  f: Flag;
  // @as("d") - vals
  d?: Record<string, Val>;
  // @as("fv") - flattenedVals
  fv?: Val[];
  // @as("cp") - codeFromPrev
  cp: string;
  // Comma-joined `let` declarations hoisted onto this val by descendants
  // that couldn't own them. Emitted after this val's checks in `merge` (the
  // old varsAllocation slot). @as("hd") - hoistedDecls
  hd: string;
  // Set by `merge` once this val's code is emitted, so a later cached-bond
  // materialization re-reads inline instead of hoisting onto it (#240).
  // @as("fz") - finalized
  fz?: boolean;
  // Invariant: absent iff no checks. Never stored as `[]` so callers can
  // test presence with a plain truthy check instead of length.
  // @as("vc") - checks
  vc?: Check[];
  // @as("u") - isUnion
  u?: boolean;
  // Whether the chain starting from the root prev has a transformation.
  // @as("t") - hasTransform
  t?: boolean;
  path: CodePath;
  // @as("g") - global
  g: BGlobal;
  // This is to mark an object field as optional. Fields like this should be
  // skipped when the value is undefined. @as("o") - optional
  o?: boolean;
}

// Shared `undefined` for every value-position use across the implementation:
// a bare `undefined` minifies to `void 0` (6 chars), this const to 1. Never
// interpolate it into generated-code strings - emitted JS text keeps literal
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

export const isLiteral = (schema: Internal): boolean => "const" in schema;

export const isOptional = (schema: Internal): boolean =>
  schema.type === undefinedTag || (schema.type === anyOfTag && undefinedTag in schema.has!);

// The constructor name worth printing, or a falsy value for anything a reader
// would learn nothing from: a plain object, a null prototype, an anonymous
// class (whose `name` is the empty string). Both callers below key off exactly
// this distinction - one to name the value, the other to decide whether to look
// inside it - so the `Object` comparison is written once.
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
// nested failure already reports its path (`Failed at user.id`) - the
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
// it - a bound, the only one today (`setBoundExpression` in refinements.ts) -
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
    // but render alike - two distinct classes both named Foo - collapse, so this
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

export const Schema = function (this: Internal): void {};
// One of exactly two schema prototypes, both rooted at `Object.create(null)`.
// `isOwnSchema` (below) recognises a Sury schema by identity against these two,
// which is what keeps operation dispatch from reading a payload as a schema -
// adding a third prototype breaks every operation's argument dispatch.
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
// copies the getter, so a derived schema (copySchema) recomputes its reverse -
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
const SelfReverseSchema = function (this: Internal): void {};
// The second (and last) schema prototype — see `isOwnSchema`.
const selfReversePrototype: Record<string, unknown> = Object.create(schemaPrototype);
Object.defineProperty(selfReversePrototype, reversedKey, {
  get() {
    return this;
  },
});
Object.defineProperty(selfReversePrototype, "sr", { value: true });
SelfReverseSchema.prototype = selfReversePrototype;

// The dispatch predicate: is this argument one of OUR schemas?
//
// Distinct from `isSchemaObject` above on purpose. That one duck-types on the
// Standard Schema marker, which is right where foreign Standard Schemas are
// legitimate (definition parsing) and wrong wherever an argument slot holds
// either a schema or untrusted data: `{"~standard":1}` from a JSON body would
// be read as the schema. Only the two prototypes above are Sury schemas, and
// both are `Object.create(null)`-rooted, so no plain object and no
// `JSON.parse` result can match - `JSON.parse` makes `__proto__` an own
// property, never a prototype.
// The `typeof` guard is for the data argument: `Object.getPrototypeOf` of a
// primitive boxes it, which is most of an immediate call's dispatch cost.
export const isOwnSchema = (value: unknown): boolean => {
  const proto = typeof value === objectTag && value && Object.getPrototypeOf(value);
  return proto === schemaPrototype || proto === selfReversePrototype;
};

// What every operation says when no argument in a schema slot is one. Shared so
// the sentence exists once: a foreign Standard Schema handed to an operation
// gets this rather than being silently read as the data to validate.
export const panicNotSchema = (): never => panic("Expected a Sury schema");

let seq = 1;

let exnId: unknown = {};
export const __setExnId = (id: unknown): void => {
  exnId = id;
}

// The public `S.Error`: what a refiner constructs to throw a failure of its
// own, so `super()` gives it a stack the way any hand-written throw has one.
// The library never builds a failure this way - see `toError`.
//
// It also makes own properties of everything it is handed, where a compiled
// failure keeps what its check settled on a shared prototype (`errorSite`). So
// the two print differently - a hand-built error shows its `expected` and
// `received`, a compiled one does not. That is the constructor's contract
// rather than an oversight: it is handed a bag of fields and has nothing to
// share them with.
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
const errorPrototype = SuryError.prototype;
Object.defineProperty(errorPrototype, "name", { value: "SuryError" });
Object.defineProperty(errorPrototype, "s", { value: s });

// A failure is a record first and an exception second. Capturing a stack is
// what `new SuryError` spends nearly all of its time on, and most failures
// never reach a throw anyone sees: a union tries each member and discards the
// losers, a Result outcome hands the record back, `is*` reads only that one
// exists. So the library reparents the details object it has already built and
// leaves `stack` unset. parse.ts attaches one at the two boundaries a failure
// can cross into user code - the compiled operation, and the compile itself -
// which is also where it can attach a better one, since only a boundary knows
// which frame to cut the trace at.
//
// Every failure the compiler raises goes through here, whether the operation is
// being built or run. `new SuryError` is the public constructor, for user code
// building a failure of its own to throw.
//
// Idempotent, and that is load-bearing rather than tidy: `B_throw` is handed a
// user's own error back when there is no path to prepend, and reparenting an
// instance someone else built and retained would rewrite it in place - a
// no-op for a SuryError, but a subclass would lose its identity for good.
export const toError = (details: ErrorDetails | Record<string, unknown>): SuryErrorRecord =>
  ((details as { s?: symbol }).s === s
    ? details
    : Object.setPrototypeOf(details, errorPrototype)) as SuryErrorRecord;

// The sentence a failure reads back as, given the expected schema already
// rendered. Written once for both readers below.
//
// `Expected Date, received Date` names the type twice and says nothing: the
// type is right and the value is not (an Invalid Date, an Error carrying the
// wrong payload). Saying `received invalid Date` is the only part of the
// message that carries information in that case.
const renderReason = (error: SuryErrorRecord, expectedExpression: string): string => {
  const receivedExpression = stringify(error.input);
  let reason = `Expected ${expectedExpression}, received ${
    expectedExpression === receivedExpression ? "invalid " : ""
  }${receivedExpression}`;
  const unionErrors = error.unionErrors as SuryErrorRecord[] | undefined;
  if (unionErrors) {
    const seenReasons = new Set<string>();
    for (let idx = 0; idx < unionErrors.length; idx++) {
      const caseError = unionErrors[idx]!;
      const line = `\n- ${caseError.path.length ? `At ${pathToText(caseError.path)}: ` : ""}${caseError.reason.split("\n").join("\n  ")}`;
      if (!seenReasons.has(line)) {
        seenReasons.add(line);
        reason += line;
      }
    }
  }
  return reason;
};

// `new S.Error({ reason })` assigns through the prototype, and an accessor with
// no setter makes that a TypeError rather than an error carrying the reason it
// was handed. Writing an own property is what an explicit reason means anyway -
// it shadows the renderer from then on.
const reasonSet = function (this: SuryErrorRecord, reason: string): void {
  Object.defineProperty(this, "reason", {
    value: reason,
    configurable: true,
    enumerable: true,
    writable: true,
  });
};

// `reason` is rendered on demand. It costs an `inputExpression` walk over the
// expected schema and a `stringify` of the received value, and it is also what
// puts a megabyte of request body inside an error message - all for a string
// nothing reads until someone asks for `message`. The fallback for an error
// nobody compiled: one the caller built with `new S.Error`, or a code that
// writes its own `reason` (every code but `invalid_input` has the words in
// hand). A compiled failure uses its check's renderer instead - see
// `errorSite`, which knows the expected schema and can render it once.
Object.defineProperty(errorPrototype, "reason", {
  configurable: true,
  set: reasonSet,
  get(this: SuryErrorRecord): string {
    return renderReason(this, inputExpression(this.expected as Internal));
  },
});

// One getter for every compiled failure, not one per check. Defining it per
// site cost an `Object.defineProperty` on every check the compiler emits, which
// measured as most of a compile; and a getter written in as many places as
// there are checks never settles into a monomorphic read. The per-site half is
// the memo, which is a plain field on the site the getter reaches through
// `this`.
const sitePrototype = Object.create(errorPrototype) as SuryErrorRecord;
Object.defineProperty(sitePrototype, "reason", {
  configurable: true,
  set: reasonSet,
  get(this: SuryErrorRecord): string {
    const site = Object.getPrototypeOf(this) as SuryErrorRecord;
    return renderReason(
      this,
      (site.expectedExpression ??= inputExpression(site.expected as Internal)) as string
    );
  },
});

// The prototype every failure of ONE check shares. `expected`, `received` and -
// where the check names its own message - the reason itself are settled the
// moment the check is compiled, so they are built once here instead of per
// failure. An instance enumerates only its own properties, so this is also what
// stops two schema objects filling every `console.log(error)`; `error.expected`
// still reads.
//
// The memo above is what the prototype-wide renderer cannot have: the expected
// schema's expression is the same for every failure of this check AND for every
// re-read of `message`, which the fallback getter pays for again each time.
export const errorSite = (
  expected: Internal,
  received: Internal,
  reasonOverride?: string
): object => {
  const site = Object.create(sitePrototype) as SuryErrorRecord;
  site.expected = expected;
  site.received = received;
  if (reasonOverride !== U) site.reason = reasonOverride;
  return site;
};

// The same, for an `invalid_conversion`: `from` and `to` are the two schemas
// the conversion sits between, settled where the coder is compiled.
//
// `reason` here reads off whatever was thrown, so each failure writes its own.
// The data property below is what lets it: a plain assignment walks the
// prototype chain looking for a setter, and the one on `errorPrototype` would
// make every write an `Object.defineProperty`. Shared for the same reason the
// getter above is.
const conversionPrototype = Object.create(errorPrototype) as SuryErrorRecord;
Object.defineProperty(conversionPrototype, "reason", {
  value: U,
  writable: true,
  enumerable: true,
});

export const conversionSite = (from: Internal, to: Internal): object => {
  const site = Object.create(conversionPrototype) as SuryErrorRecord;
  site.from = from;
  site.to = to;
  return site;
};

// One failure of the check `site` describes: only what the value decided. The
// keys are written rather than passed to `Object.create` because a data
// property is what the rest of the compiler expects - generated loop code
// assigns `error.path` on the way out to prepend a segment.
export const errorAt = (
  site: object,
  path: Path,
  input: unknown,
  unionErrors?: SuryErrorRecord[]
): ErrorDetails => {
  const error = Object.create(site) as SuryErrorRecord;
  error.code = "invalid_input";
  error.path = path;
  error.input = input;
  // Absent rather than `undefined` on the failures that have none: what a
  // shared hidden class used to buy is the site prototype's job now.
  if (unionErrors) error.unionErrors = unionErrors;
  return error as unknown as ErrorDetails;
};

export const getOrRethrow = (exn: unknown): SuryErrorRecord => {
  if (exn && (exn as { s?: symbol }).s === s) return exn as SuryErrorRecord;
  throw exn;
}

// Internal invariant/misuse errors (bad schema construction, not input
// validation) - intentionally a plain Error, not SuryError: there's no
// ErrorDetails shape (code/path/reason) to attach at these call sites.
export const panic = (message: string): never => {
  throw new Error(`[Sury] ${message}`);
}

const formatErrorMessage = (error: SuryErrorRecord): string =>
  `${error.path.length ? `Failed at ${pathToText(error.path)}: ` : ""}${error.reason}`;

export const errorClass: unknown = SuryError;

export type GlobalConfig = {
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
};

export const noopDecoder: Builder = (input: Val) => input;

// Every built-in singleton schema must be a module-level const initialized by
// a single `/* @__PURE__ */ initSchema(...)` expression: the module system is
// what guarantees one instance per schema (the compiled-decoder cache in
// getOp is keyed by `seq` and stored on the instance, so a fresh copy
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
  // `content` is non-enumerable, so Object.assign skips it - carried by hand
  // here, which is also the only place that pays for it.
  if (schema.content !== U) setContent(c, schema.content);
  if (schema.bc !== U) setBytesCodec(c, schema.bc);
  return c;
};

export const copyTo = (from: Internal, to: Internal): Internal => {
  const mut = copySchema(from);
  mut.to = to;
  return mut;
};

// `S.base64` and `S.json` are their own content, and an enumerable
// self-reference makes `JSON.stringify(schema)` - and every error that embeds
// one - throw on a cycle. Non-enumerable everywhere rather than only there, so
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
