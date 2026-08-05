open Vitest

test("Gets default value when Option.getOr is used", t => {
  let schema = S.option(S.float)->S.Option.getOr(123.)

  t->Assert.deepEqual((schema->S.untag).default, Some(123.->(U.magic: float => unknown)))
})

test("Returns the first default value, but can get the last one as well", t => {
  let schema =
    S.option(S.float)
    ->S.Option.getOr(123.)
    ->S.to(S.any, ~custom={decode: Sync(number =>
        if number > 0. {
          Some("positive")
        } else {
          None
        }), encode: Never})
    ->S.to(S.option(S.string))
    ->S.Option.getOr("not positive")

  // The getOr union carries the default itself (no `.to = item` link any
  // more) — the chain is getOr union -> unknown (the codec target) -> the
  // second getOr union with its own default.
  t->Assert.deepEqual((schema->S.untag).tag, AnyOf)
  t->Assert.deepEqual((schema->S.untag).default, Some(123.->U.magic))
  t->Assert.deepEqual(((schema->S.untag).to->Option.getOrThrow->S.untag).tag, Unknown)
  t->Assert.deepEqual(
    (((schema->S.untag).to->Option.getOrThrow->S.untag).to->Option.getOrThrow->S.untag).tag,
    AnyOf,
  )
  t->Assert.deepEqual(
    (((schema->S.untag).to->Option.getOrThrow->S.untag).to->Option.getOrThrow->S.untag).default,
    Some("not positive"->U.magic),
  )
})

test("Currently doesn't store default value on schema set by Option.getOrWith", t => {
  let cb = () => 123.
  let schema = S.option(S.float)->S.Option.getOrWith(cb)

  t->Assert.deepEqual((schema->S.untag).default, None)
})

test("Doesn't get default value for schemas without default", t => {
  let schema = S.float

  t->Assert.deepEqual((schema->S.untag).default, None)
})
