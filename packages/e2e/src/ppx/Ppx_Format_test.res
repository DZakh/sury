@@warning("-3")
open Vitest

open U

// The format schemas and stdlib aliases are named identically as a type and as
// a schema, so `S.email` as a type resolves to the `S.email` schema.

@schema
type myEmail = S.email
test("Email schema", t => {
  t->assertEqualSchemas(myEmailSchema, S.email)
})

@schema
type myPort = S.port
test("Port schema", t => {
  t->assertEqualSchemas(myPortSchema, S.port)
})

@schema
type myInteger = S.integer
test("Integer schema", t => {
  t->assertEqualSchemas(myIntegerSchema, S.integer)
})

@schema
type myJsonString = S.jsonString
test("JsonString schema", t => {
  t->assertEqualSchemas(myJsonStringSchema, S.jsonString)
})

@schema
type myUrl = S.url
test("Url schema", t => {
  t->assertEqualSchemas(myUrlSchema, S.url)
})

@schema
type myDate = S.date
test("Date schema", t => {
  t->assertEqualSchemas(myDateSchema, S.date)
})

@schema
type myBlob = S.blob
test("Blob schema", t => {
  t->assertEqualSchemas(myBlobSchema, S.blob)
})

@schema
type myNonEmptyTags = S.nonEmpty<array<string>>
test("NonEmpty schema", t => {
  t->assertEqualSchemas(myNonEmptyTagsSchema, S.array(S.string)->S.nonEmpty)
})

@schema
type user = {
  id: S.uuid,
  email: S.email,
  tags: S.nonEmpty<array<string>>,
  homepage: option<S.uri>,
}
test("Record of formats", t => {
  t->assertEqualSchemas(
    userSchema,
    S.schema(s => {
      id: s.matches(S.uuid),
      email: s.matches(S.email),
      tags: s.matches(S.array(S.string)->S.nonEmpty),
      homepage: s.matches(S.option(S.uri)),
    }),
  )
  t->Assert.deepEqual(
    %raw(`{id: "123e4567-e89b-12d3-a456-426614174000", email: "a@b.com", tags: ["x"]}`)
    ->S.parseOrThrow(~to=userSchema),
    {
      id: Uuid("123e4567-e89b-12d3-a456-426614174000"),
      email: Email("a@b.com"),
      tags: NonEmpty(["x"]),
      homepage: None,
    },
  )
})
