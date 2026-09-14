// Checks a spec's example values against the JSON Schema the same spec
// records, with Ajv - a real, independent implementation of the keywords.
//
// Ajv rather than the spec's own `vs.zod` equivalent or Sury's
// `fromJSONSchemaOrThrow`: both of those READ a document into another type
// system first, so a keyword the reader doesn't implement becomes a weaker
// schema and the check passes on a document no validator would have accepted.
// Reading a document is the thing under test here, so the checker cannot be
// another reader of it. Ajv evaluates the keywords directly, which is what
// makes a pass mean "a consumer handed this document agrees with the parser".
//
// draft-07, because that is the target `toInputJSONSchemaOrThrow` emits when
// called with no options (src/jsonschema.ts) - and `jsonSchema.input`/`output`
// are exactly that call. The `draft-2020-12`/`openapi-3.0` blocks record only
// the fields that differ from it, so there is no whole document to hand a
// validator; they stay unchecked.
import Ajv from "ajv";

// `strict: false` - Sury emits keywords Ajv has no schema for (a `format` it
// publishes as an annotation, `contentSchema` on a draft-07 document), and
// strict mode makes an unknown keyword a compile error rather than the
// no-op the spec says it is.
//
// `validateFormats: false` - `format` is an annotation whose reading is left
// to the validator, so enforcing it would compare Sury's regex against Ajv's
// idea of an email and report the difference as a Sury bug. The structural
// keywords are the ones a document actually promises.
//
// `code.regExp` - a JSON Schema `pattern` is an ECMA-262 regex with no flags
// required, but Ajv compiles one with `u`, under which `\d{3}\-\d{4}` is an
// invalid escape and fails to compile at all. Unicode mode is tried first (it
// is the only way `\p{Letter}` means anything) and dropped when the pattern
// cannot be read that way.
const regExp = (pattern: string, flags: string): RegExp => {
  try {
    return new RegExp(pattern, flags);
  } catch {
    return new RegExp(pattern, flags.replace("u", ""));
  }
};

const ajv = new Ajv({
  strict: false,
  validateFormats: false,
  allErrors: false,
  code: { regExp: regExp as never },
});

export type DocumentValidator =
  | { validate: (value: unknown) => string | undefined }
  | { error: string };

// Compiling is the expensive half and a document is reused across every
// example of its spec, so each is compiled once per process. Keyed by the
// spec's own source text, which is the canonical one-line spelling the golden
// already holds.
const compiled = new Map<string, DocumentValidator>();

export const documentValidator = (source: string, doc: unknown): DocumentValidator => {
  const hit = compiled.get(source);
  if (hit) return hit;
  let result: DocumentValidator;
  // A boolean document is legal JSON Schema (`true` accepts everything), but a
  // spec records a non-object only when the conversion threw, in which case
  // the "document" is the thrown message - nothing to compile.
  if (typeof doc !== "object" || doc === null) {
    result = { error: "not a JSON Schema document" };
  } else {
    try {
      const fn = ajv.compile(doc as object);
      result = {
        validate: (value: unknown) => (fn(value) ? undefined : ajv.errorsText(fn.errors)),
      };
    } catch (e) {
      result = { error: (e as Error).message };
    }
  }
  compiled.set(source, result);
  return result;
};

// Ajv reads JSON, so a value outside it has no verdict to give: `undefined`,
// a bigint, a Date, a Blob, NaN and Infinity are all things Sury handles and
// JSON Schema has no way to describe. Skipped rather than coerced - passing
// `new Date()` to Ajv would silently validate whatever JSON.stringify would
// have made of it, not the value the operation returned.
export const isJsonValue = (v: unknown, seen: WeakSet<object> = new WeakSet()): boolean => {
  if (v === null || typeof v === "boolean" || typeof v === "string") return true;
  if (typeof v === "number") return Number.isFinite(v);
  if (typeof v !== "object") return false;
  // A cycle is not JSON, and is also what would hang the walk below.
  if (seen.has(v)) return false;
  seen.add(v);
  try {
    if (Array.isArray(v)) return v.every((x) => isJsonValue(x, seen));
    const proto = Object.getPrototypeOf(v);
    if (proto !== Object.prototype && proto !== null) return false;
    return Object.values(v).every((x) => isJsonValue(x, seen));
  } finally {
    seen.delete(v);
  }
};
