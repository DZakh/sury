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
  // (Sury never emits symbol path keys). Each variant is unboxed, so at
  // runtime this is just the underlying string/float/`{key}` value.
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

  // `StandardSchemaV1.Result` = `SuccessResult | FailureResult`. Opaque: at
  // runtime it is a bare `{value}` or `{issues}`, so it can't be a plain
  // ReScript record (an optional `value` would be option-boxed). Call
  // `result` to pattern match on it.
  type t<'output>

  // The pattern-matchable form of `t`, obtained via `result`.
  type result<'output> = Success(success<'output>) | Failure(failure)

  external success: success<'output> => t<'output> = "%identity"
  external failure: failure => t<'output> = "%identity"

  let result = (t: t<'output>): result<'output> =>
    if %raw(`t.issues !== undefined`) {
      Failure(t->Obj.magic)
    } else {
      Success(t->Obj.magic)
    }
}

module JsonSchema = {
  // `StandardJSONSchemaV1.Target`, restricted to the dialects Sury actually
  // supports. A standalone open polymorphic variant (`[> ...]`) can't be used
  // here: ReScript requires an explicit row type variable on any named alias,
  // which would need threading through `options`/`converter`/`props`/`t`
  // (and turning the `~standard` getter's plain forward-reference `ref` into a
  // record with a polymorphic field, since a `ref` can't hold a rank-2
  // polymorphic function). `options.target` below is the actual open surface,
  // matching the TS `Target = ... | ({} & string)` escape hatch: it accepts
  // any string and is validated at runtime by `parseTarget`.
  type target = [#"draft-07" | #"draft-2020-12" | #"openapi-3.0"]

  // `StandardJSONSchemaV1.Options`. `target` is a raw `string` to mirror the TS
  // `Target = ... | ({} & string)`: any string is accepted and validated at
  // runtime (an unsupported target throws).
  type options = {
    target: string,
    libraryOptions?: dict<unknown>,
  }

  // `StandardJSONSchemaV1.Converter`.
  type converter = {
    input: options => JSONSchema.t,
    output: options => JSONSchema.t,
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
  types?: {"input": 'input, "output": 'output},
}

// The Standard Schema interface (`StandardSchemaV1`): an object carrying the
// `~standard` property.
type t<'input, 'output> = {@as("~standard") standard: props<'input, 'output>}
