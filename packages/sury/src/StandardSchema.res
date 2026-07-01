// ReScript port of the Standard Schema spec (https://standardschema.dev) and its
// Standard JSON Schema extension (https://standardschema.dev/json-schema). The
// structure mirrors the `StandardSchemaV1` / `StandardTypedV1` /
// `StandardJSONSchemaV1` TypeScript namespaces in `S.d.ts`.

// `StandardSchemaV1.Types` — the schema's inferred input/output types.
type types<'input, 'output> = {
  input: 'input,
  output: 'output,
}

module Issue = {
  // `StandardSchemaV1.PathSegment`.
  type pathSegment = {key: string}

  // `StandardSchemaV1.Issue`. The spec allows each path element to be a
  // `PropertyKey` or a `PathSegment`; Sury always emits plain string keys.
  type t = {
    message: string,
    path?: array<string>,
  }
}

module Result = {
  // `StandardSchemaV1.SuccessResult`.
  type success<'output> = {value: 'output}

  // `StandardSchemaV1.FailureResult`.
  type failure = {issues: array<Issue.t>}

  // `StandardSchemaV1.Result` = `SuccessResult | FailureResult`. Untagged at
  // runtime: a success carries `value`, a failure carries `issues`.
  type t<'output> = {
    value?: 'output,
    issues?: array<Issue.t>,
  }
}

module JsonSchema = {
  // `StandardJSONSchemaV1.Target` — the named JSON Schema dialects. Compiles to
  // the spec target strings, so it interops with the raw `target` string from
  // JS. (The TS `Target` additionally accepts any `string`.)
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

// The `~standard` value carried by every schema: `StandardSchemaV1.Props`
// intersected with `StandardJSONSchemaV1.Props`. Parametrized by the schema's
// inferred input/output types.
type t<'input, 'output> = {
  version: int,
  vendor: string,
  validate: unknown => Result.t<'output>,
  jsonSchema: JsonSchema.converter,
  types?: types<'input, 'output>,
}
