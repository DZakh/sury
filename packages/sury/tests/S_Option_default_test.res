open Ava

test("Gets default value when Option.getOr is used", t => {
  let schema = S.option(S.float)->S.Option.getOr(123.)

  t->Assert.deepEqual(schema->S.Option.default, Some(Value(123.->(U.magic: float => unknown))), ())
})

test("Returns the last default value", t => {
  let schema =
    S.option(S.float)
    ->S.Option.getOr(123.)
    ->S.transform(_ => {
      parser: number =>
        if number > 0. {
          Some("positive")
        } else {
          None
        },
    })
    ->S.Option.getOr("not positive")

  t->Assert.deepEqual(
    schema->S.Option.default,
    Some(Value("not positive"->(U.magic: string => unknown))),
    (),
  )
})

test("Gets default value when Option.getOrWith is used", t => {
  let cb = () => 123.
  let schema = S.option(S.float)->S.Option.getOrWith(cb)

  t->Assert.deepEqual(
    schema->S.Option.default,
    Some(Callback(cb->(U.magic: (unit => float) => unit => unknown))),
    (),
  )
})

test("Doesn't get default value for schemas without default", t => {
  let schema = S.float

  t->Assert.deepEqual(schema->S.Option.default, None, ())
})
