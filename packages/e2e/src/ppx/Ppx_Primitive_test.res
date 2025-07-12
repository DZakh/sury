open Ava

open U

// TODO: Automatically enable it in PPX
S.enableJson()

@schema
type myString = string
test("String schema", t => {
  t->assertEqualSchemas(myStringSchema, S.string)
})

@schema
type myInt = int
test("Int schema", t => {
  t->assertEqualSchemas(myIntSchema, S.int)
})

@schema
type myFloat = float
test("Float schema", t => {
  t->assertEqualSchemas(myFloatSchema, S.float)
})

@schema
type myBool = bool
test("Bool schema", t => {
  t->assertEqualSchemas(myBoolSchema, S.bool)
})

@schema
type myUnit = unit
test("Unit schema", t => {
  t->assertEqualSchemas(myUnitSchema, S.unit)
})

@schema
type myUnknown = unknown
test("Unknown schema", t => {
  t->assertEqualSchemas(myUnknownSchema, S.unknown)
})

@schema
type myNever = S.never
test("Never schema", t => {
  t->assertEqualSchemas(myNeverSchema, S.never)
})

@schema
type myOptionOfString = option<string>
test("Option of string schema", t => {
  t->assertEqualSchemas(myOptionOfStringSchema, S.option(S.string))
})

@schema
type myNullableOfString = Nullable.t<string>
test("Nullable of string schema", t => {
  t->assertEqualSchemas(myNullableOfStringSchema, S.nullable(S.string))
})

@schema
type myArrayOfString = array<string>
test("Array of string schema", t => {
  t->assertEqualSchemas(myArrayOfStringSchema, S.array(S.string))
})

@schema
type myListOfString = list<string>
test("List of string schema", t => {
  t->assertEqualSchemas(myListOfStringSchema, S.list(S.string))
})

@schema
type myDictOfString = dict<string>
test("Dict of string schema", t => {
  t->assertEqualSchemas(myDictOfStringSchema, S.dict(S.string))
})

@schema
type myDictOfStringFromJs = Js.Dict.t<string>
test("Dict of string schema from Js", t => {
  t->assertEqualSchemas(myDictOfStringSchema, S.dict(S.string))
})

@schema
type myDictOfStringFromCore = Dict.t<string>
test("Dict of string schema from Core", t => {
  t->assertEqualSchemas(myDictOfStringFromCoreSchema, S.dict(S.string))
})

@schema
type myJson = Js.Json.t
test("Json schema", t => {
  t->assertEqualSchemas(myJsonSchema, S.json)
})

@schema
type myJsonFromCore = JSON.t
test("Json schema from Core", t => {
  t->assertEqualSchemas(myJsonFromCoreSchema, S.json)
})

@schema
type myTuple = (string, int)
test("Tuple schema", t => {
  t->assertEqualSchemas(myTupleSchema, S.tuple2(S.string, S.int))
})

@schema
type myBigTuple = (string, string, string, int, int, int, float, float, float, bool, bool, bool)
test("Big tuple schema", t => {
  t->assertEqualSchemas(
    myBigTupleSchema,
    S.schema(s => (
      s.matches(S.string),
      s.matches(S.string),
      s.matches(S.string),
      s.matches(S.int),
      s.matches(S.int),
      s.matches(S.int),
      s.matches(S.float),
      s.matches(S.float),
      s.matches(S.float),
      s.matches(S.bool),
      s.matches(S.bool),
      s.matches(S.bool),
    )),
  )
})

@schema
type myCustomString = @s.matches(S.string->S.email) string
test("Custom string schema", t => {
  t->assertEqualSchemas(myCustomStringSchema, S.string->S.email)
})

@schema
type myCustomLiteralString = @s.matches(S.literal("123")->S.email) string
test("Custom litaral string schema", t => {
  t->assertEqualSchemas(myCustomLiteralStringSchema, S.literal("123")->S.email)
})

@schema
type myCustomOptionalString = option<@s.matches(S.string->S.email) string>
test("Custom optional string schema", t => {
  t->assertEqualSchemas(myCustomOptionalStringSchema, S.string->S.email->S.option)
})

// @schema
// type myNullOfString = null<string>
// This will result with error:
// The incompatible parts: option<string> vs myNullOfString (defined as null<string>)
// So use the code below instead
@schema
type myNullOfString = @s.matches(S.null(S.string)) option<string>
test("Null of string schema", t => {
  t->assertEqualSchemas(myNullOfStringSchema, S.null(S.string))
})
