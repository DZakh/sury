open Ava

test("Successfully parses valid data", t => {
  let schema = S.float->S.floatMax(1.)

  t->Assert.deepEqual(1->S.parseOrThrow(schema), 1., ())
  t->Assert.deepEqual(-1->S.parseOrThrow(schema), -1., ())
})

test("Fails to parse invalid data", t => {
  let schema = S.float->S.floatMax(1.)

  t->U.assertThrows(
    () => 1234->S.parseOrThrow(schema),
    {
      code: OperationFailed("Number must be lower than or equal to 1"),
      operation: Parse,
      path: S.Path.empty,
    },
  )
})

test("Successfully serializes valid value", t => {
  let schema = S.float->S.floatMax(1.)

  t->Assert.deepEqual(1.->S.reverseConvertOrThrow(schema), %raw(`1`), ())
  t->Assert.deepEqual(-1.->S.reverseConvertOrThrow(schema), %raw(`-1`), ())
})

test("Fails to serialize invalid value", t => {
  let schema = S.float->S.floatMax(1.)

  t->U.assertThrows(
    () => 1234.->S.reverseConvertOrThrow(schema),
    {
      code: OperationFailed("Number must be lower than or equal to 1"),
      operation: ReverseConvert,
      path: S.Path.empty,
    },
  )
})

test("Returns custom error message", t => {
  let schema = S.float->S.floatMax(~message="Custom", 1.)

  t->U.assertThrows(
    () => 12.->S.parseOrThrow(schema),
    {code: OperationFailed("Custom"), operation: Parse, path: S.Path.empty},
  )
})

test("Returns refinement", t => {
  let schema = S.float->S.floatMax(1.)

  t->Assert.deepEqual(
    schema->S.Float.refinements,
    [{kind: Max({value: 1.}), message: "Number must be lower than or equal to 1"}],
    (),
  )
})

test("Compiled parse code snapshot", t => {
  let schema = S.float->S.floatMax(~message="Custom", 1.)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{if(typeof i!=="number"||Number.isNaN(i)){e[0](i)}if(i>e[1]){e[2]()}return i}`,
  )
})
