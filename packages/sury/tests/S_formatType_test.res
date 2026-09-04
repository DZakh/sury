open Vitest

test("Format type is erased at runtime", t => {
  t->Assert.deepEqual(
    "dzakh.dev@gmail.com"->S.parseOrThrow(~to=S.email)->Obj.magic,
    %raw(`"dzakh.dev@gmail.com"`),
  )
  t->Assert.deepEqual(8080->S.parseOrThrow(~to=S.port)->Obj.magic, %raw(`8080`))
  t->Assert.deepEqual(3000000000.->S.parseOrThrow(~to=S.integer)->Obj.magic, %raw(`3000000000`))
})

test("Format value coerces back to its payload", t => {
  let email = "dzakh.dev@gmail.com"->S.parseOrThrow(~to=S.email)

  t->Assert.deepEqual((email :> string), "dzakh.dev@gmail.com")
})

test("Format types don't unify with each other", t => {
  // The point of the nominal types: an email is not a uuid, even though both
  // are strings at runtime. Kept as a compile-time assertion.
  let toEmail = (email: S.email) => email
  t->Assert.deepEqual(toEmail(Email("a@b.com")), Email("a@b.com"))
})

test("S.nonEmpty wraps the value type", t => {
  let schema: S.t<S.nonEmpty<string>> = S.string->S.nonEmpty

  t->Assert.deepEqual("abc"->S.parseOrThrow(~to=schema), NonEmpty("abc"))
  t->U.assertThrowsMessage(
    () => ""->S.parseOrThrow(~to=schema),
    `Expected string.length >= 1, received ""`,
  )
})

test("S.nonEmpty works over an array", t => {
  let schema = S.array(S.string)->S.nonEmpty

  t->Assert.deepEqual(%raw(`["a"]`)->S.parseOrThrow(~to=schema), NonEmpty(["a"]))
})

test("S.pattern and S.trim apply to a format schema", t => {
  let schema = S.email->S.pattern(%re("/gmail\.com$/"))

  t->Assert.deepEqual(
    "dzakh.dev@gmail.com"->S.parseOrThrow(~to=schema),
    S.Email("dzakh.dev@gmail.com"),
  )
  t->U.assertThrowsMessage(() => "a@b.com"->S.parseOrThrow(~to=schema), `Invalid pattern`)

  // Trimming after a format runs the format check on the untrimmed input, so
  // this only pins that the widened `S.trim` accepts a format schema at all.
  t->Assert.deepEqual("a@b.com"->S.parseOrThrow(~to=S.email->S.trim), S.Email("a@b.com"))
})

test("Version-pinned uuids are distinct types from the loose one", t => {
  // `S.uuidv7`'s value satisfies `S.uuid`'s regex, but not its type — which is
  // the point: a function taking a v7 key can't be handed a v1.
  let toV7 = (id: S.uuidv7) => id

  t->Assert.deepEqual(
    toV7("0192f0e1-2b3c-7d4e-8b7a-1f2e3d4c5b6a"->S.parseOrThrow(~to=S.uuidv7)),
    Uuidv7("0192f0e1-2b3c-7d4e-8b7a-1f2e3d4c5b6a"),
  )
  t->U.assertThrowsMessage(
    () => "0192f0e1-2b3c-4d4e-8b7a-1f2e3d4c5b6a"->S.parseOrThrow(~to=S.uuidv7),
    `Expected uuidv7, received "0192f0e1-2b3c-4d4e-8b7a-1f2e3d4c5b6a"`,
  )
})

test("A format with no length of its own composes with S.length", t => {
  let schema = S.nanoid->S.length(21)

  t->Assert.deepEqual(
    "V1StGXR8_Z5jdHi6B-myT"->S.parseOrThrow(~to=schema),
    Nanoid("V1StGXR8_Z5jdHi6B-myT"),
  )
  // The bound reads off the format's own name, not `string`.
  t->U.assertThrowsMessage(
    () => "abc"->S.parseOrThrow(~to=schema),
    `Expected nanoid.length == 21, received "abc"`,
  )
})
