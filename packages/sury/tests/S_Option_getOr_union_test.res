open Ava

// Reproduction for https://github.com/DZakh/sury/issues/163
// S.option on a union/variant schema followed by S.Option.getOr panics with
// "Can't set default for ..." because getWithDefault assumes exactly one
// non-Undefined item in the anyOf array. S.option flattens the union's
// variants into the option union, producing multiple non-Undefined items.

test("Fails to use S.Option.getOr on S.option of a union of literals (issue #163)", t => {
  let variantSchema = S.union([S.literal(#a), S.literal(#b), S.literal(#c)])

  t->Assert.throws(
    () => {
      let _ = S.option(variantSchema)->S.Option.getOr(#a)
    },
    ~expectations={
      message: `[Sury] Can't set default for "a" | "b" | "c" | undefined`,
    },
  )
})

test("Fails to use S.Option.getOrWith on S.option of a union of literals (issue #163)", t => {
  let variantSchema = S.union([S.literal(#a), S.literal(#b), S.literal(#c)])

  t->Assert.throws(
    () => {
      let _ = S.option(variantSchema)->S.Option.getOrWith(() => #a)
    },
    ~expectations={
      message: `[Sury] Can't set default for "a" | "b" | "c" | undefined`,
    },
  )
})

test("Fails to use S.Option.getOr on S.option of a union of two literals (issue #163)", t => {
  let variantSchema = S.union([S.literal(#a), S.literal(#b)])

  t->Assert.throws(
    () => {
      let _ = S.option(variantSchema)->S.Option.getOr(#a)
    },
    ~expectations={
      message: `[Sury] Can't set default for "a" | "b" | undefined`,
    },
  )
})

type shape = Circle({radius: float}) | Square({side: float})

test(
  "Fails to use S.Option.getOr on S.option of a union of object schemas (issue #163)",
  t => {
    let circleSchema = S.object(s => {
      s.tag("kind", "circle")
      Circle({radius: s.field("radius", S.float)})
    })
    let squareSchema = S.object(s => {
      s.tag("kind", "square")
      Square({side: s.field("side", S.float)})
    })
    let unionSchema = S.union([circleSchema, squareSchema])

    t->Assert.throws(
      () => {
        let _ = S.option(unionSchema)->S.Option.getOr(Circle({radius: 1.0}))
      },
      ~expectations={
        message: `[Sury] Can't set default for { kind: "circle"; radius: number; } | { kind: "square"; side: number; } | undefined`,
      },
    )
  },
)
