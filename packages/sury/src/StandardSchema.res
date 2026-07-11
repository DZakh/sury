// ReScript port of the Standard Schema spec (https://standardschema.dev) and its
// Standard JSON Schema extension (https://standardschema.dev/json-schema). The
// structure mirrors the `StandardSchemaV1` / `StandardTypedV1` /
// `StandardJSONSchemaV1` TypeScript namespaces in `S.d.ts`.

module Issue = {
  // `StandardSchemaV1.PathSegment`.
  type pathSegment = {key: string}

  // A single element of `StandardSchemaV1.Issue.path`: `PropertyKey |
  // PathSegment`. `PropertyKey` is `string | number | symbol`, but ReScript's
  // unboxed variants can't disambiguate a `symbol` case, so it's omitted here
  // (Sury never emits symbol path keys).
  // FIXME: Add a `Symbol(Symbol.t)` case when ReScript supports symbols in
  // `@unboxed` variants.
  // Each variant is unboxed, so at runtime this is just the underlying
  // string/float/`{key}` value.
  @unboxed
  type pathElement =
    | String(string)
    | Number(float)
    | Segment(pathSegment)

  // `StandardSchemaV1.Issue`. `path` is absent for top-level issues.
  type t = {
    message: string,
    path?: array<pathElement>,
  }
}

module Result = {
  // `StandardSchemaV1.SuccessResult`.
  type success<'output> = {value: 'output}

  // `StandardSchemaV1.FailureResult`.
  type failure = {issues: array<Issue.t>}

  // `StandardSchemaV1.Result` = `SuccessResult | FailureResult`. Untagged at
  // runtime: a success carries `value`, a failure carries `issues`. Use
  // `classify` to convert it into the standard `result<'a, 'b>` (`Ok`/`Error`).
  type t<'output> = {
    value?: 'output,
    issues?: array<Issue.t>,
  }

  external success: success<'output> => t<'output> = "%identity"
  external failure: failure => t<'output> = "%identity"

  let classify = (t: t<'output>): result<success<'output>, failure> =>
    if %raw(`t.issues`) {
      Error(t->Obj.magic)
    } else {
      Ok(t->Obj.magic)
    }
}

module JsonSchema = {
  // `StandardJSONSchemaV1.Target`. The known dialects are unboxed string
  // constants (via `@as`); `Unknown` carries anything else, so this mirrors
  // the TS `Target = "draft-07" | "draft-2020-12" | "openapi-3.0" |
  // ({} & string)` in a single, ordinary (monomorphic) variant - no row type
  // variable needed, so unlike an open polymorphic variant (`[> ...]`) it's
  // freely reusable from `options` below, `Sury.toJSONSchemaOptions`, and
  // `.resi` files. `toJSONSchema` throws for `Unknown` (an unsupported
  // target).
  @unboxed
  type target =
    | @as("draft-07") Draft07
    | @as("draft-2020-12") Draft202012
    | @as("openapi-3.0") OpenApi30
    | Unknown(string)

  // `StandardJSONSchemaV1.Options`.
  type options = {
    target: target,
    libraryOptions?: dict<unknown>,
  }

  // `StandardJSONSchemaV1.Converter`.
  type converter = {
    input: options => JSONSchema.t,
    output: options => JSONSchema.t,
  }
}

// The `~standard` property object: `StandardSchemaV1.Props`, with `jsonSchema`
// present when the library also implements the `StandardJSONSchemaV1`
// extension (as Sury does) - not every Standard Schema library does, so it's
// optional here rather than required as it is on `StandardJSONSchemaV1.Props`
// itself. Parametrized by the schema's inferred input/output types.
type props<'input, 'output> = {
  version: int,
  vendor: string,
  validate: 'any. 'any => Result.t<'output>,
  jsonSchema?: JsonSchema.converter,
}

// The Standard Schema interface (`StandardSchemaV1`): an object carrying the
// `~standard` property.
type t<'input, 'output> = {@as("~standard") standard: props<'input, 'output>}
