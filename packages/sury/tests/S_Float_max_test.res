open Ava

test("Successfully parses valid data", t => {
  let schema = S.float->S.floatMax(1.)

  t->Assert.deepEqual(1->S.parseOrThrow(~to=schema), 1.)
  t->Assert.deepEqual(-1->S.parseOrThrow(~to=schema), -1.)
})

test("Fails to parse invalid data", t => {
  let schema = S.float->S.floatMax(1.)

  t->U.assertThrowsMessage(
    () => 1234->S.parseOrThrow(~to=schema),
    `Number must be lower than or equal to 1`,
  )
})

test("Successfully serializes valid value", t => {
  let schema = S.float->S.floatMax(1.)

  t->Assert.deepEqual(1.->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`1`))
  t->Assert.deepEqual(-1.->S.decodeOrThrow(~from=schema, ~to=S.unknown), %raw(`-1`))
})

test("Fails to serialize invalid value", t => {
  let schema = S.float->S.floatMax(1.)

  t->U.assertThrowsMessage(
    () => 1234.->S.decodeOrThrow(~from=schema, ~to=S.unknown),
    `Number must be lower than or equal to 1`,
  )
})

test("Returns custom error message", t => {
  let schema = S.float->S.floatMax(~message="Custom", 1.)

  t->U.assertThrowsMessage(() => 12.->S.parseOrThrow(~to=schema), `Custom`)
})

test("Returns refinement", t => {
  let schema = S.float->S.floatMax(1.)

  t->Assert.deepEqual(
    schema->S.Float.refinements,
    [{kind: Max({value: 1.}), message: "Number must be lower than or equal to 1"}],
  )
})

test("Compiled parse code snapshot", t => {
  let schema = S.float->S.floatMax(~message="Custom", 1.)

  t->U.assertCompiledCode(
    ~schema,
    ~op=#Parse,
    `i=>{typeof i==="number"&&!Number.isNaN(i)||e[0](i);i<=e[1]||e[2](i);return i}`,
  )
})
