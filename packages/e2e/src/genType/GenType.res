@genType
let stringSchema = S.string

@genType
let error: S.error = U.error({
  operation: Parse,
  code: OperationFailed("Something went wrong"),
  path: S.Path.empty,
})
