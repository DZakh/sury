// open Ava

// test("Supports String", t => {
//   let schema = S.string
//   t->Assert.deepEqual(schema->S.inline, `S.string`)
// })

// test("Doesn't support transforms and refinements", t => {
//   let schema = S.string->S.transform(_ => {parser: ignore})->S.refine(_ => ignore)
//   t->Assert.deepEqual(schema->S.inline, `S.string`)
// })

// test("Supports built-in String.email refinement", t => {
//   let schema = S.string->S.email
//   let schemaInlineResult = S.string->S.email(~message="Invalid email address")

//   t->U.assertEqualSchemas(schema, schemaInlineResult)
//   t->Assert.deepEqual(schema->S.inline, `S.string->S.email(~message="Invalid email address")`)
// })

// test("Supports built-in String.datetime refinement", t => {
//   let schema = S.string->S.datetime
//   let schemaInlineResult = S.string->S.datetime(~message="Invalid datetime string! Expected UTC")

//   t->U.assertEqualSchemas(schema, schemaInlineResult)
//   t->Assert.deepEqual(
//     schema->S.inline,
//     `S.string->S.datetime(~message="Invalid datetime string! Expected UTC")`,
//     (),
//   )
// })

// test("Supports built-in String.url refinement", t => {
//   let schema = S.string->S.url
//   let schemaInlineResult = S.string->S.url(~message="Invalid url")

//   t->U.assertEqualSchemas(schema, schemaInlineResult)
//   t->Assert.deepEqual(schema->S.inline, `S.string->S.url(~message="Invalid url")`)
// })

// test("Supports built-in String.uuid refinement", t => {
//   let schema = S.string->S.uuid
//   let schemaInlineResult = S.string->S.uuid(~message="Invalid UUID")

//   t->U.assertEqualSchemas(schema, schemaInlineResult)
//   t->Assert.deepEqual(schema->S.inline, `S.string->S.uuid(~message="Invalid UUID")`)
// })

// test("Supports built-in String.cuid refinement", t => {
//   let schema = S.string->S.cuid
//   let schemaInlineResult = S.string->S.cuid(~message="Invalid CUID")

//   t->U.assertEqualSchemas(schema, schemaInlineResult)
//   t->Assert.deepEqual(schema->S.inline, `S.string->S.cuid(~message="Invalid CUID")`)
// })

// test("Supports built-in String.min refinement", t => {
//   let schema = S.string->S.stringMinLength(5)
//   let schemaInlineResult =
//     S.string->S.stringMinLength(5, ~message="String must be 5 or more characters long")

//   t->U.assertEqualSchemas(schema, schemaInlineResult)
//   t->Assert.deepEqual(
//     schema->S.inline,
//     `S.string->S.stringMinLength(5, ~message="String must be 5 or more characters long")`,
//     (),
//   )
// })

// test("Supports built-in String.max refinement", t => {
//   let schema = S.string->S.stringMaxLength(5)
//   let schemaInlineResult =
//     S.string->S.stringMaxLength(5, ~message="String must be 5 or fewer characters long")

//   t->U.assertEqualSchemas(schema, schemaInlineResult)
//   t->Assert.deepEqual(
//     schema->S.inline,
//     `S.string->S.stringMaxLength(5, ~message="String must be 5 or fewer characters long")`,
//     (),
//   )
// })

// test("Supports built-in String.length refinement", t => {
//   let schema = S.string->S.stringLength(5)
//   let schemaInlineResult =
//     S.string->S.stringLength(~message="String must be exactly 5 characters long", 5)

//   t->U.assertEqualSchemas(schema, schemaInlineResult)
//   t->Assert.deepEqual(
//     schema->S.inline,
//     `S.string->S.stringLength(5, ~message="String must be exactly 5 characters long")`,
//     (),
//   )
// })

// test("Supports built-in String.pattern refinement", t => {
//   let schema = S.string->S.pattern(%re("/0-9/"))
//   let schemaInlineResult = S.string->S.pattern(~message="Invalid", %re("/0-9/"))

//   t->U.assertEqualSchemas(schema, schemaInlineResult)
//   t->Assert.deepEqual(schema->S.inline, `S.string->S.pattern(%re("/0-9/"), ~message="Invalid")`)
// })

// test("Supports Int", t => {
//   let schema = S.int
//   t->Assert.deepEqual(schema->S.inline, `S.int`)
// })

// test("Supports built-in Int.max refinement", t => {
//   let schema = S.int->S.intMax(4)
//   let schemaInlineResult = S.int->S.intMax(~message="Number must be lower than or equal to 4", 4)

//   t->U.assertEqualSchemas(schema, schemaInlineResult)
//   t->Assert.deepEqual(
//     schema->S.inline,
//     `S.int->S.intMax(4, ~message="Number must be lower than or equal to 4")`,
//     (),
//   )
// })

// test("Supports built-in Int.min refinement", t => {
//   let schema = S.int->S.intMin(4)
//   let schemaInlineResult = S.int->S.intMin(4, ~message="Number must be greater than or equal to 4")

//   t->U.assertEqualSchemas(schema, schemaInlineResult)
//   t->Assert.deepEqual(
//     schema->S.inline,
//     `S.int->S.intMin(4, ~message="Number must be greater than or equal to 4")`,
//     (),
//   )
// })

// test("Supports built-in Int.port refinement", t => {
//   let schema = S.int->S.port
//   let schemaInlineResult = S.int->S.port(~message="Invalid port")

//   t->U.assertEqualSchemas(schema, schemaInlineResult)
//   t->Assert.deepEqual(schema->S.inline, `S.int->S.port(~message="Invalid port")`)
// })

// test("Supports Float", t => {
//   let schema = S.float
//   t->Assert.deepEqual(schema->S.inline, `S.float`)
// })

// test("Supports built-in Float.max refinement", t => {
//   let schema = S.float->S.floatMax(4.)
//   let schemaInlineResult =
//     S.float->S.floatMax(~message="Number must be lower than or equal to 4", 4.)

//   t->U.assertEqualSchemas(schema, schemaInlineResult)
//   t->Assert.deepEqual(
//     schema->S.inline,
//     `S.float->S.floatMax(4., ~message="Number must be lower than or equal to 4")`,
//     (),
//   )
// })

// test("Supports built-in Float.max refinement with digits after decimal point", t => {
//   let schema = S.float->S.floatMax(4.4)
//   let schemaInlineResult =
//     S.float->S.floatMax(~message="Number must be lower than or equal to 4.4", 4.4)

//   t->U.assertEqualSchemas(schema, schemaInlineResult)
//   t->Assert.deepEqual(
//     schema->S.inline,
//     `S.float->S.floatMax(4.4, ~message="Number must be lower than or equal to 4.4")`,
//     (),
//   )
// })

// test("Supports built-in Float.min refinement", t => {
//   let schema = S.float->S.floatMin(4.)
//   let schemaInlineResult =
//     S.float->S.floatMin(~message="Number must be greater than or equal to 4", 4.)

//   t->U.assertEqualSchemas(schema, schemaInlineResult)
//   t->Assert.deepEqual(
//     schema->S.inline,
//     `S.float->S.floatMin(4., ~message="Number must be greater than or equal to 4")`,
//     (),
//   )
// })

// test("Supports built-in Float.min refinement with digits after decimal point", t => {
//   let schema = S.float->S.floatMin(4.4)
//   let schemaInlineResult =
//     S.float->S.floatMin(~message="Number must be greater than or equal to 4.4", 4.4)

//   t->U.assertEqualSchemas(schema, schemaInlineResult)
//   t->Assert.deepEqual(
//     schema->S.inline,
//     `S.float->S.floatMin(4.4, ~message="Number must be greater than or equal to 4.4")`,
//     (),
//   )
// })

// test("Supports multiple built-in refinements", t => {
//   let schema = S.string->S.stringMinLength(5)->S.stringMaxLength(10)
//   let schemaInlineResult =
//     S.string
//     ->S.stringMinLength(~message="String must be 5 or more characters long", 5)
//     ->S.stringMaxLength(~message="String must be 10 or fewer characters long", 10)

//   t->U.assertEqualSchemas(schema, schemaInlineResult)
//   t->Assert.deepEqual(
//     schema->S.inline,
//     `S.string->S.stringMinLength(5, ~message="String must be 5 or more characters long")->S.stringMaxLength(10, ~message="String must be 10 or fewer characters long")`,
//     (),
//   )
// })

// test("Supports Bool", t => {
//   let schema = S.bool
//   t->Assert.deepEqual(schema->S.inline, `S.bool`)
// })

// test("Supports Unknown", t => {
//   let schema = S.unknown
//   t->Assert.deepEqual(schema->S.inline, `S.unknown`)
// })

// test("Treats custom schema factory as Unknown", t => {
//   let schema = S.custom("Test", s => s.fail("User error"))
//   t->Assert.deepEqual(schema->S.inline, `S.unknown`)
// })

// test("Supports Never", t => {
//   let schema = S.never
//   t->Assert.deepEqual(schema->S.inline, `S.never`)
// })

// test("Supports JSON", t => {
//   let schema = S.json(~validate=false)
//   t->Assert.deepEqual(schema->S.inline, `S.json(~validate=false)`)

//   let schema = S.json(~validate=true)
//   t->Assert.deepEqual(schema->S.inline, `S.json(~validate=true)`)
// })

// test("Supports String Literal", t => {
//   let schema = S.literal("foo")
//   t->Assert.deepEqual(schema->S.inline, `S.literal(%raw(\`"foo"\`))`)
// })

// test("Escapes the String Literal value", t => {
//   let schema = S.literal(`"foo"`)
//   t->Assert.deepEqual(schema->S.inline, `S.literal(%raw(\`"\\"foo\\""\`))`)
// })

// test("Supports Number Literal like int", t => {
//   let schema = S.literal(3)
//   t->Assert.deepEqual(schema->S.inline, `S.literal(%raw(\`3\`))`)
// })

// test("Supports Number Literal", t => {
//   let schema = S.literal(3.)
//   t->Assert.deepEqual(schema->S.inline, `S.literal(%raw(\`3\`))`)
// })

// test("Supports Number Literal with decimal", t => {
//   let schema = S.literal(3.3)
//   t->Assert.deepEqual(schema->S.inline, `S.literal(%raw(\`3.3\`))`)
// })

// test("Supports Boolean Literal", t => {
//   let schema = S.literal(true)
//   t->Assert.deepEqual(schema->S.inline, `S.literal(%raw(\`true\`))`)
// })

// test("Supports Undefined Literal", t => {
//   let schema = S.literal()
//   t->Assert.deepEqual(schema->S.inline, `S.literal(%raw(\`undefined\`))`)
// })

// test("Supports Null Literal", t => {
//   let schema = S.literal(%raw(`null`))
//   t->Assert.deepEqual(schema->S.inline, `S.literal(%raw(\`null\`))`)
// })

// test("Supports NaN Literal", t => {
//   let schema = S.literal(%raw(`NaN`))
//   t->Assert.deepEqual(schema->S.inline, `S.literal(%raw(\`NaN\`))`)
// })

// test("Supports Option", t => {
//   let schema = S.option(S.string)
//   t->Assert.deepEqual(schema->S.inline, `S.option(S.string)`)
// })

// test("Supports Option.getOrWith", t => {
//   let schema = S.float->S.option->S.Option.getOrWith(() => 4.)
//   let _ = S.float->S.option->S.Option.getOrWith(() => %raw(`4`))

//   t->Assert.deepEqual(
//     schema->S.inline,
//     `S.option(S.float)->S.Option.getOrWith(() => %raw(\`4\`))`,
//     (),
//   )
// })

// test("Supports Option.getOr", t => {
//   let schema = S.float->S.option->S.Option.getOr(4.)
//   let _ = S.float->S.option->S.Option.getOr(%raw(`4`))

//   t->Assert.deepEqual(schema->S.inline, `S.option(S.float)->S.Option.getOr(%raw(\`4\`))`)
// })

// test("Supports Deprecated with message", t => {
//   let schema = S.string->S.deprecate("Will be removed in API v2.")
//   t->Assert.deepEqual(schema->S.inline, `S.string->S.deprecate("Will be removed in API v2.")`)
// })

// test("Supports Null", t => {
//   let schema = S.nullAsOption(S.string)
//   t->Assert.deepEqual(schema->S.inline, `S.nullAsOption(S.string)`)
// })

// test("Supports Array", t => {
//   let schema = S.array(S.string)
//   t->Assert.deepEqual(schema->S.inline, `S.array(S.string)`)
// })

// test("Supports built-in Array.max refinement", t => {
//   let schema = S.array(S.string)->S.arrayMaxLength(4)
//   let schemaInlineResult =
//     S.array(S.string)->S.arrayMaxLength(~message="Array must be 4 or fewer items long", 4)

//   t->U.assertEqualSchemas(schema, schemaInlineResult)
//   t->Assert.deepEqual(
//     schema->S.inline,
//     `S.array(S.string)->S.arrayMaxLength(4, ~message="Array must be 4 or fewer items long")`,
//     (),
//   )
// })

// test("Supports built-in Array.min refinement", t => {
//   let schema = S.array(S.string)->S.arrayMinLength(4)
//   let schemaInlineResult =
//     S.array(S.string)->S.arrayMinLength(~message="Array must be 4 or more items long", 4)

//   t->U.assertEqualSchemas(schema, schemaInlineResult)
//   t->Assert.deepEqual(
//     schema->S.inline,
//     `S.array(S.string)->S.arrayMinLength(4, ~message="Array must be 4 or more items long")`,
//     (),
//   )
// })

// test("Supports built-in Array.length refinement", t => {
//   let schema = S.array(S.string)->S.arrayLength(4)
//   let schemaInlineResult =
//     S.array(S.string)->S.arrayLength(~message="Array must be exactly 4 items long", 4)

//   t->U.assertEqualSchemas(schema, schemaInlineResult)
//   t->Assert.deepEqual(
//     schema->S.inline,
//     `S.array(S.string)->S.arrayLength(4, ~message="Array must be exactly 4 items long")`,
//     (),
//   )
// })

// test("Supports Dict", t => {
//   let schema = S.dict(S.string)
//   t->Assert.deepEqual(schema->S.inline, `S.dict(S.string)`)
// })

// test("Supports tuple1", t => {
//   let schema = S.tuple1(S.string)
//   let schemaInlineResult = S.tuple1(S.string)

//   t->U.assertEqualSchemas(schema, schemaInlineResult)
//   t->Assert.deepEqual(schema->S.inline, `S.tuple1(S.string)`)
// })

// test("Supports tuple2", t => {
//   let schema = S.tuple2(S.string, S.int)
//   let schemaInlineResult = S.tuple2(S.string, S.int)

//   t->U.assertEqualSchemas(schema, schemaInlineResult)
//   t->Assert.deepEqual(schema->S.inline, `S.tuple2(S.string, S.int)`)
// })

// test("Supports tuple3", t => {
//   let schema = S.tuple3(S.string, S.int, S.bool)
//   let schemaInlineResult = S.tuple3(S.string, S.int, S.bool)

//   t->U.assertEqualSchemas(schema, schemaInlineResult)
//   t->Assert.deepEqual(schema->S.inline, `S.tuple3(S.string, S.int, S.bool)`)
// })

// test("Supports Tuple with 4 items", t => {
//   let schema = S.tuple(s => (
//     s.item(0, S.string),
//     s.item(1, S.int),
//     s.item(2, S.bool),
//     s.item(3, S.string),
//   ))
//   let schemaInlineResult = S.tuple(s => (
//     s.item(0, S.string),
//     s.item(1, S.int),
//     s.item(2, S.bool),
//     s.item(3, S.string),
//   ))

//   t->U.assertEqualSchemas(schema, schemaInlineResult)
//   t->Assert.deepEqual(
//     schema->S.inline,
//     `S.tuple(s => (s.item(0, S.string), s.item(1, S.int), s.item(2, S.bool), s.item(3, S.string)))`,
//     (),
//   )
// })

// test("Supports Union", t => {
//   let schema = S.union([S.literal(#yes), S.literal(#no)])
//   let _schemaInlineResult = S.union([
//     S.literal(%raw(`"yes"`))->S.shape(v => #"\"yes\""(v)),
//     S.literal(%raw(`"no"`))->S.shape(v => #"\"no\""(v)),
//   ])

//   t->Assert.deepEqual(
//     schema->S.inline,
//     `S.union([S.literal(%raw(\`"yes"\`))->S.shape(v => #"\\"yes\\""(v)), S.literal(%raw(\`"no"\`))->S.shape(v => #"\\"no\\""(v))])`,
//     (),
//   )
// })

// test("Supports description", t => {
//   let schema = S.string->S.describe("It's a string")
//   t->Assert.deepEqual(schema->S.inline, `S.string->S.describe("It's a string")`)
// })

// // test("Uses S.transform for primitive schemas inside of union", t => {
// //   let schema = S.union([
// //     S.string->S.shape(v => #String(v)),
// //     S.bool->S.shape(v => #Bool(v)),
// //     S.float->S.shape(v => #Float(v)),
// //     S.int->S.shape(v => #Int(v)),
// //     S.unknown->S.shape(v => #Unknown(v)),
// //     S.never->S.shape(v => #Never(v)),
// //     S.json->S.shape(v => #JSON(v)),
// //   ])

// //   t->Assert.deepEqual(
// //     schema->S.inline,
// //     `S.union([S.string->S.shape(v => #"String"(v)), S.bool->S.shape(v => #"Bool"(v)), S.float->S.shape(v => #"Float"(v)), S.int->S.shape(v => #"Int"(v)), S.unknown->S.shape(v => #"Unknown"(v)), S.never->S.shape(v => #"Never"(v)), S.json->S.shape(v => #"JSON"(v))])`,
// //     (),
// //   )
// // })

// // test("Adds index for the same schemas inside of the union", t => {
// //   let schema = S.union([S.string, S.string])
// //   let schemaInlineResult = S.union([
// //     S.string->S.shape(v => #String(v)),
// //     S.string->S.shape(v => #String2(v)),
// //   ])

// //   schemaInlineResult->(
// //     U.magic: S.t<
// //       [
// //         | #String(string)
// //         | #String2(string)
// //       ],
// //     > => unit
// //   )

// //   t->Assert.deepEqual(
// //     schema->S.inline,
// //     `S.union([S.string->S.shape(v => #"String"(v)), S.string->S.shape(v => #"String2"(v))])`,
// //     (),
// //   )
// // })

// test("Supports Object (ignores transformations)", t => {
//   let schema = S.object(s =>
//     {
//       "name": s.field("Name", S.string),
//       "email": s.field("Email", S.string),
//       "age": s.field("Age", S.int),
//     }
//   )
//   t->Assert.deepEqual(
//     schema->S.inline,
//     `S.object(s =>
//   {
//     "Name": s.field("Name", S.string),
//     "Email": s.field("Email", S.string),
//     "Age": s.field("Age", S.int),
//   }
// )`,
//     (),
//   )
// })

// test("Supports strip", t => {
//   let schema = S.object(_ => ())->S.strip
//   t->Assert.deepEqual(schema->S.inline, `S.object(_ => ())`)
// })

// test("Supports strict", t => {
//   let schema = S.object(_ => ())->S.strict
//   t->Assert.deepEqual(schema->S.inline, `S.object(_ => ())->S.strict`)
// })

// test("Supports empty Object (ignores transformations)", t => {
//   let schema = S.object(_ => 123)
//   let schemaInlineResult = S.object(_ => ())

//   t->U.assertEqualSchemas(schema, schemaInlineResult->(U.magic: S.t<unit> => S.t<int>))
//   t->Assert.deepEqual(schema->S.inline, `S.object(_ => ())`)
// })

// test("Supports empty Object in union", t => {
//   let schema = S.union([S.object(_ => ()), S.object(_ => ())])
//   let schemaInlineResult = S.union([
//     S.object(_ => ())->S.shape(v => #"{}"(v)),
//     S.object(_ => ())->S.shape(v => #"{}2"(v)),
//   ])

//   schemaInlineResult->(U.magic: S.t<[#"{}"(unit) | #"{}2"(unit)]> => unit)

//   t->Assert.deepEqual(
//     schema->S.inline,
//     `S.union([S.object(_ => ())->S.shape(v => #"{}"(v)), S.object(_ => ())->S.shape(v => #"{}2"(v))])`,
//     (),
//   )
// })

// test("Supports  Tuple in union", t => {
//   let schema = S.union([S.tuple1(S.string), S.tuple1(S.string)])
//   let schemaInlineResult = S.union([
//     S.tuple1(S.string)->S.shape(v => #"[string]"(v)),
//     S.tuple1(S.string)->S.shape(v => #"[string]2"(v)),
//   ])

//   schemaInlineResult->(U.magic: S.t<[#"[string]"(string) | #"[string]2"(string)]> => unit)

//   t->Assert.deepEqual(
//     schema->S.inline,
//     `S.union([S.tuple1(S.string)->S.shape(v => #"[string]"(v)), S.tuple1(S.string)->S.shape(v => #"[string]2"(v))])`,
//     (),
//   )
// })

// // test("Supports Option schemas in union", t => {
// //   let schema = S.union([S.option(S.literalVariant(String("123"), 123.)), S.option(S.float)])
// //   let schemaInlineResult = S.union([
// //     S.option(S.literal(String("123")))->S.shape(v => #OptionOf123(v)),
// //     S.option(S.float)->S.shape(v => #OptionOfFloat(v)),
// //   ])

// //   schemaInlineResult->(
// //     U.magic: S.t<
// //       [
// //         | #OptionOf123(option<string>)
// //         | #OptionOfFloat(option<float>)
// //       ],
// //     > => unit
// //   )

// //   t->Assert.deepEqual(
// //     schema->S.inline,
// //     `S.union([S.option(S.literal(String("123")))->S.shape(v => #"OptionOf123"(v)), S.option(S.float)->S.shape(v => #"OptionOfFloat"(v))])`,
// //     (),
// //   )
// // })

// // test("Supports Null schemas in union", t => {
// //   let schema = S.union([S.nullAsOption(S.literalVariant(String("123"), 123.)), S.nullAsOption(S.float)])
// //   let schemaInlineResult = S.union([
// //     S.nullAsOption(S.literal(String("123")))->S.shape(v => #NullOf123(v)),
// //     S.nullAsOption(S.float)->S.shape(v => #NullOfFloat(v)),
// //   ])

// //   schemaInlineResult->(
// //     U.magic: S.t<
// //       [
// //         | #NullOf123(option<string>)
// //         | #NullOfFloat(option<float>)
// //       ],
// //     > => unit
// //   )

// //   t->Assert.deepEqual(
// //     schema->S.inline,
// //     `S.union([S.nullAsOption(S.literal(String("123")))->S.shape(v => #"NullOf123"(v)), S.nullAsOption(S.float)->S.shape(v => #"NullOfFloat"(v))])`,
// //     (),
// //   )
// // })

// // test("Supports Array schemas in union", t => {
// //   let schema = S.union([S.array(S.literalVariant(String("123"), 123.)), S.array(S.float)])
// //   let schemaInlineResult = S.union([
// //     S.array(S.literal(String("123")))->S.shape(v => #ArrayOf123(v)),
// //     S.array(S.float)->S.shape(v => #ArrayOfFloat(v)),
// //   ])

// //   schemaInlineResult->(
// //     U.magic: S.t<
// //       [
// //         | #ArrayOf123(array<string>)
// //         | #ArrayOfFloat(array<float>)
// //       ],
// //     > => unit
// //   )

// //   t->Assert.deepEqual(
// //     schema->S.inline,
// //     `S.union([S.array(S.literal(String("123")))->S.shape(v => #"ArrayOf123"(v)), S.array(S.float)->S.shape(v => #"ArrayOfFloat"(v))])`,
// //     (),
// //   )
// // })

// // test("Supports Dict schemas in union", t => {
// //   let schema = S.union([S.dict(S.literalVariant(String("123"), 123.)), S.dict(S.float)])
// //   let schemaInlineResult = S.union([
// //     S.dict(S.literal(String("123")))->S.shape(v => #DictOf123(v)),
// //     S.dict(S.float)->S.shape(v => #DictOfFloat(v)),
// //   ])

// //   schemaInlineResult->(
// //     U.magic: S.t<
// //       [
// //         | #DictOf123(Dict.t<string>)
// //         | #DictOfFloat(Dict.t<float>)
// //       ],
// //     > => unit
// //   )

// //   t->Assert.deepEqual(
// //     schema->S.inline,
// //     `S.union([S.dict(S.literal(String("123")))->S.shape(v => #"DictOf123"(v)), S.dict(S.float)->S.shape(v => #"DictOfFloat"(v))])`,
// //     (),
// //   )
// // })

// // test("Supports Object schemas in union", t => {
// //   let schema = S.union([
// //     S.object(s => s.field("field", S.literalVariant(String("123"), 123.))),
// //     S.object(s => s.field("field", S.float)),
// //   ])
// //   let schemaInlineResult = S.union([
// //     S.object(s =>
// //       {
// //         "field": s.field("field", S.literal(String("123"))),
// //       }
// //     )->S.shape(v => #Object(v)),
// //     S.object(s =>
// //       {
// //         "field": s.field("field", S.float),
// //       }
// //     )->S.shape(v => #Object2(v)),
// //   ])

// //   schemaInlineResult->(
// //     U.magic: S.t<
// //       [
// //         | #Object({"field": string})
// //         | #Object2({"field": float})
// //       ],
// //     > => unit
// //   )

// //   t->Assert.deepEqual(
// //     schema->S.inline,
// //     `S.union([S.object(s =>
// //   {
// //     "field": s.field("field", S.literal(String("123"))),
// //   }
// // )->S.shape(v => #"Object"(v)), S.object(s =>
// //   {
// //     "field": s.field("field", S.float),
// //   }
// // )->S.shape(v => #"Object2"(v))])`,
// //     (),
// //   )
// // })

// // test("Supports Tuple schemas in union", t => {
// //   let schema = S.union([S.tuple1(. S.literalVariant(String("123"), 123.)), S.tuple1(. S.float)])
// //   let schemaInlineResult = S.union([
// //     S.tuple1(. S.literal(String("123")))->S.shape(v => #Tuple(v)),
// //     S.tuple1(. S.float)->S.shape(v => #Tuple2(v)),
// //   ])

// //   schemaInlineResult->(
// //     U.magic: S.t<
// //       [
// //         | #Tuple(string)
// //         | #Tuple2(float)
// //       ],
// //     > => unit
// //   )

// //   t->Assert.deepEqual(
// //     schema->S.inline,
// //     `S.union([S.tuple1(. S.literal(String("123")))->S.shape(v => #"Tuple"(v)), S.tuple1(. S.float)->S.shape(v => #"Tuple2"(v))])`,
// //     (),
// //   )
// // })

// // test("Supports Union schemas in union", t => {
// //   let schema = S.union([
// //     S.union([S.literal(String("red")), S.literal(String("blue"))]),
// //     S.union([S.literalVariant(Int(0), "black"), S.literalVariant(Int(1), "white")]),
// //   ])
// //   let schemaInlineResult = S.union([
// //     S.union([
// //       S.literalVariant(String("red"), #red),
// //       S.literalVariant(String("blue"), #blue),
// //     ])->S.shape(v => #Union(v)),
// //     S.union([S.literalVariant(Int(0), #0), S.literalVariant(Int(1), #1)])->S.shape(v =>
// //       #Union2(v)
// //     ),
// //   ])

// //   schemaInlineResult->(U.magic: S.t<[#Union([#red | #blue]) | #Union2([#0 | #1])]> => unit)

// //   t->Assert.deepEqual(
// //     schema->S.inline,
// //     `S.union([S.union([S.literalVariant(String("red"), #"red"), S.literalVariant(String("blue"), #"blue")])->S.shape(v => #"Union"(v)), S.union([S.literalVariant(Int(0), #"0"), S.literalVariant(Int(1), #"1")])->S.shape(v => #"Union2"(v))])`,
// //     (),
// //   )
// // })

// // // TODO: Add support for recursive schema.
// // // TODO: Add support for list.

