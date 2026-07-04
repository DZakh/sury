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
  // `StandardJSONSchemaV1.Target`. An open polymorphic variant: the three
  // known dialects are tagged for autocomplete and exhaustiveness, but
  // (mirroring the TS `Target = ... | ({} & string)`) it structurally accepts
  // any other tag too; `toJSONSchema` validates it at runtime and throws for
  // an unsupported one.
  type target<'a> = [> #"draft-07" | #"draft-2020-12" | #"openapi-3.0"] as 'a

  // `StandardJSONSchemaV1.Options`.
  type options<'a> = {
    target: target<'a>,
    libraryOptions?: dict<unknown>,
  }

  // `StandardJSONSchemaV1.Converter`. `input`/`output` are rank-2 polymorphic
  // (quantified per call), so a single `converter` value works for any target
  // row, without needing `props`/`t` to carry an extra type parameter.
  type converter = {
    input: 'a. options<'a> => JSONSchema.t,
    output: 'a. options<'a> => JSONSchema.t,
  }
}

// The `~standard` property object: `StandardSchemaV1.Props` intersected with
// `StandardJSONSchemaV1.Props`. Parametrized by the schema's inferred
// input/output types.
type props<'input, 'output> = {
  version: int,
  vendor: string,
  validate: 'any. 'any => Result.t<'output>,
  jsonSchema: JsonSchema.converter,
}

// The Standard Schema interface (`StandardSchemaV1`): an object carrying the
// `~standard` property.
type t<'input, 'output> = {@as("~standard") standard: props<'input, 'output>}
