@genType
let stringSchema = S.string

@genType
let error: S.error = S.Error.make(
  InvalidOperation({
    path: S.Path.empty,
    reason: "Something went wrong",
  }),
)
