open Vitest

// Every `S.Protobuf` schema is an external from `sury/protobuf/wkt`, an
// unchecked cast that nothing else holds to its ReScript type. Here a value
// written in ReScript has to write the bytes the same value written as JS
// does, and read back as itself; and the type's empty value has to be what
// empty bytes read as.
let holds = (t, schema: S.t<'value>, ~value: 'value, ~js: string, ~empty: 'value) => {
  let bytes = value->S.convertOrThrow(~from=schema, ~to=S.protobuf)
  let jsValue: 'value = %raw(`(js) => new Function("return " + js)()`)(js)
  t->Assert.deepEqual(bytes, jsValue->S.convertOrThrow(~from=schema, ~to=S.protobuf))
  t->Assert.deepEqual(bytes->S.convertOrThrow(~from=S.protobuf, ~to=schema), value)
  t->Assert.deepEqual(empty->S.convertOrThrow(~from=schema, ~to=S.protobuf), Uint8Array.fromLength(0))
  t->Assert.deepEqual(Uint8Array.fromLength(0)->S.convertOrThrow(~from=S.protobuf, ~to=schema), empty)
}

let bytes = Uint8Array.fromArray([1, 2, 255])
let any: S.Protobuf.Any.t = {typeUrl: "type.googleapis.com/acme.User", value: bytes}
let anyJs = `{typeUrl: "type.googleapis.com/acme.User", value: new Uint8Array([1, 2, 255])}`
let option: S.Protobuf.Option.t = {name: "deprecated", value: Some(any)}
let optionJs = `{name: "deprecated", value: ${anyJs}}`

test("Any", t =>
  t->holds(S.Protobuf.Any.schema, ~value=any, ~js=anyJs, ~empty={typeUrl: "", value: Uint8Array.fromLength(0)})
)

test("SourceContext", t =>
  t->holds(
    S.Protobuf.SourceContext.schema,
    ~value={fileName: "acme/user.proto"},
    ~js=`{fileName: "acme/user.proto"}`,
    ~empty={fileName: ""},
  )
)

test("Option", t =>
  t->holds(S.Protobuf.Option.schema, ~value=option, ~js=optionJs, ~empty={name: "", value: None})
)

test("Field, with every Field_Kind and Field_Cardinality", t => {
  let field = (kind, cardinality): S.Protobuf.Field.t => {
    kind,
    cardinality,
    number: 3,
    name: "id",
    typeUrl: "type.googleapis.com/acme.Id",
    oneofIndex: 1,
    packed: true,
    options: [option],
    jsonName: "id",
    defaultValue: "7",
  }
  let fieldJs = (kind, cardinality) =>
    `{kind: ${kind->Int.toString}, cardinality: ${cardinality->Int.toString}, number: 3, name: "id", typeUrl: "type.googleapis.com/acme.Id", oneofIndex: 1, packed: true, options: [${optionJs}], jsonName: "id", defaultValue: "7"}`
  let empty: S.Protobuf.Field.t = {
    kind: TypeUnknown,
    cardinality: Unknown,
    number: 0,
    name: "",
    typeUrl: "",
    oneofIndex: 0,
    packed: false,
    options: [],
    jsonName: "",
    defaultValue: "",
  }
  let kinds: array<S.Protobuf.Field_Kind.t> = [
    TypeUnknown,
    TypeDouble,
    TypeFloat,
    TypeInt64,
    TypeUint64,
    TypeInt32,
    TypeFixed64,
    TypeFixed32,
    TypeBool,
    TypeString,
    TypeGroup,
    TypeMessage,
    TypeBytes,
    TypeUint32,
    TypeEnum,
    TypeSfixed32,
    TypeSfixed64,
    TypeSint32,
    TypeSint64,
  ]
  let cardinalities: array<S.Protobuf.Field_Cardinality.t> = [Unknown, Optional, Required, Repeated]
  kinds->Array.forEachWithIndex((kind, number) =>
    t->holds(S.Protobuf.Field.schema, ~value=field(kind, Repeated), ~js=fieldJs(number, 3), ~empty)
  )
  cardinalities->Array.forEachWithIndex((cardinality, number) =>
    t->holds(S.Protobuf.Field.schema, ~value=field(TypeSint64, cardinality), ~js=fieldJs(18, number), ~empty)
  )
})

test("Type, with every Syntax", t => {
  let syntaxes: array<S.Protobuf.Syntax.t> = [Proto2, Proto3, Editions]
  syntaxes->Array.forEachWithIndex((syntax, number) =>
    t->holds(
      S.Protobuf.Type.schema,
      ~value={
        name: "acme.User",
        fields: [
          {
            kind: TypeString,
            cardinality: Optional,
            number: 1,
            name: "name",
            typeUrl: "",
            oneofIndex: 0,
            packed: false,
            options: [],
            jsonName: "name",
            defaultValue: "",
          },
        ],
        oneofs: ["contact"],
        options: [option],
        sourceContext: Some({fileName: "acme/user.proto"}),
        syntax,
        edition: "2023",
      },
      ~js=`{name: "acme.User", fields: [{kind: 9, cardinality: 1, number: 1, name: "name", typeUrl: "", oneofIndex: 0, packed: false, options: [], jsonName: "name", defaultValue: ""}], oneofs: ["contact"], options: [${optionJs}], sourceContext: {fileName: "acme/user.proto"}, syntax: ${number->Int.toString}, edition: "2023"}`,
      ~empty={
        name: "",
        fields: [],
        oneofs: [],
        options: [],
        sourceContext: None,
        syntax: Proto2,
        edition: "",
      },
    )
  )
})

test("EnumValue", t =>
  t->holds(
    S.Protobuf.EnumValue.schema,
    ~value={name: "ROBOT", number: -2, options: [option]},
    ~js=`{name: "ROBOT", number: -2, options: [${optionJs}]}`,
    ~empty={name: "", number: 0, options: []},
  )
)

test("Enum", t =>
  t->holds(
    S.Protobuf.Enum.schema,
    ~value={
      name: "acme.Kind",
      enumvalue: [{name: "ROBOT", number: 2, options: []}],
      options: [option],
      sourceContext: Some({fileName: "acme/kind.proto"}),
      syntax: Editions,
      edition: "2023",
    },
    ~js=`{name: "acme.Kind", enumvalue: [{name: "ROBOT", number: 2, options: []}], options: [${optionJs}], sourceContext: {fileName: "acme/kind.proto"}, syntax: 2, edition: "2023"}`,
    ~empty={
      name: "",
      enumvalue: [],
      options: [],
      sourceContext: None,
      syntax: Proto2,
      edition: "",
    },
  )
)

let method: S.Protobuf.Method.t = {
  name: "GetUser",
  requestTypeUrl: "type.googleapis.com/acme.GetUserRequest",
  requestStreaming: true,
  responseTypeUrl: "type.googleapis.com/acme.User",
  responseStreaming: true,
  options: [option],
  syntax: Proto3,
  edition: "2023",
}
let methodJs = `{name: "GetUser", requestTypeUrl: "type.googleapis.com/acme.GetUserRequest", requestStreaming: true, responseTypeUrl: "type.googleapis.com/acme.User", responseStreaming: true, options: [${optionJs}], syntax: 1, edition: "2023"}`

test("Method", t =>
  t->holds(
    S.Protobuf.Method.schema,
    ~value=method,
    ~js=methodJs,
    ~empty={
      name: "",
      requestTypeUrl: "",
      requestStreaming: false,
      responseTypeUrl: "",
      responseStreaming: false,
      options: [],
      syntax: Proto2,
      edition: "",
    },
  )
)

test("Mixin", t =>
  t->holds(
    S.Protobuf.Mixin.schema,
    ~value={name: "acme.Acl", root: "acls"},
    ~js=`{name: "acme.Acl", root: "acls"}`,
    ~empty={name: "", root: ""},
  )
)

test("Api", t =>
  t->holds(
    S.Protobuf.Api.schema,
    ~value={
      name: "acme.Users",
      methods: [method],
      options: [option],
      version: "1.2",
      sourceContext: Some({fileName: "acme/users.proto"}),
      mixins: [{name: "acme.Acl", root: "acls"}],
      syntax: Proto3,
      edition: "2023",
    },
    ~js=`{name: "acme.Users", methods: [${methodJs}], options: [${optionJs}], version: "1.2", sourceContext: {fileName: "acme/users.proto"}, mixins: [{name: "acme.Acl", root: "acls"}], syntax: 1, edition: "2023"}`,
    ~empty={
      name: "",
      methods: [],
      options: [],
      version: "",
      sourceContext: None,
      mixins: [],
      syntax: Proto2,
      edition: "",
    },
  )
)

test("Duration", t =>
  t->holds(
    S.Protobuf.Duration.schema,
    ~value={seconds: -315576000000n, nanos: -999999999},
    ~js=`{seconds: -315576000000n, nanos: -999999999}`,
    ~empty={seconds: 0n, nanos: 0},
  )
)

test("Empty", t => t->holds(S.Protobuf.Empty.schema, ~value={}, ~js=`{}`, ~empty={}))

test("FieldMask", t =>
  t->holds(
    S.Protobuf.FieldMask.schema,
    ~value={paths: ["user.display_name", "photo"]},
    ~js=`{paths: ["user.display_name", "photo"]}`,
    ~empty={paths: []},
  )
)

let values: array<(S.Protobuf.Value.t, string)> = [
  ({kind: NullValue({value: NullValue})}, `{kind: {case: "nullValue", value: 0}}`),
  ({kind: NumberValue({value: -1.5})}, `{kind: {case: "numberValue", value: -1.5}}`),
  ({kind: StringValue({value: "x"})}, `{kind: {case: "stringValue", value: "x"}}`),
  ({kind: BoolValue({value: true})}, `{kind: {case: "boolValue", value: true}}`),
  (
    {kind: StructValue({value: {fields: dict{"a": ({kind: NumberValue({value: 1.})}: S.Protobuf.Value.t)}}})},
    `{kind: {case: "structValue", value: {fields: {a: {kind: {case: "numberValue", value: 1}}}}}}`,
  ),
  (
    {kind: ListValue({value: {values: [{kind: StringValue({value: "y"})}, {kind: Unset({})}]}})},
    `{kind: {case: "listValue", value: {values: [{kind: {case: "stringValue", value: "y"}}, {kind: {case: undefined}}]}}}`,
  ),
  ({kind: Unset({})}, `{kind: {case: undefined}}`),
]

test("Value, with every kind and none", t =>
  values->Array.forEach(((value, js)) =>
    t->holds(S.Protobuf.Value.schema, ~value, ~js, ~empty={kind: Unset({})})
  )
)

test("Struct", t =>
  t->holds(
    S.Protobuf.Struct.schema,
    ~value={fields: values->Array.mapWithIndex(((value, _), idx) => (`k${idx->Int.toString}`, value))->Dict.fromArray},
    ~js=`{fields: {${values
      ->Array.mapWithIndex(((_, js), idx) => `k${idx->Int.toString}: ${js}`)
      ->Array.join(", ")}}}`,
    ~empty={fields: dict{}},
  )
)

test("ListValue", t =>
  t->holds(
    S.Protobuf.ListValue.schema,
    ~value={values: values->Array.map(((value, _)) => value)},
    ~js=`{values: [${values->Array.map(((_, js)) => js)->Array.join(", ")}]}`,
    ~empty={values: []},
  )
)

test("Timestamp", t =>
  t->holds(
    S.Protobuf.Timestamp.schema,
    ~value={seconds: 253402300799n, nanos: 999999999},
    ~js=`{seconds: 253402300799n, nanos: 999999999}`,
    ~empty={seconds: 0n, nanos: 0},
  )
)

test("the wrappers", t => {
  t->holds(
    S.Protobuf.DoubleValue.schema,
    ~value={value: Float.Constants.nan},
    ~js=`{value: NaN}`,
    ~empty={value: 0.},
  )
  t->holds(S.Protobuf.FloatValue.schema, ~value={value: -1.5}, ~js=`{value: -1.5}`, ~empty={value: 0.})
  t->holds(
    S.Protobuf.Int64Value.schema,
    ~value={value: -9223372036854775808n},
    ~js=`{value: -9223372036854775808n}`,
    ~empty={value: 0n},
  )
  t->holds(
    S.Protobuf.UInt64Value.schema,
    ~value={value: 18446744073709551615n},
    ~js=`{value: 18446744073709551615n}`,
    ~empty={value: 0n},
  )
  t->holds(
    S.Protobuf.Int32Value.schema,
    ~value={value: -2147483648},
    ~js=`{value: -2147483648}`,
    ~empty={value: 0},
  )
  t->holds(
    S.Protobuf.UInt32Value.schema,
    ~value={value: Integer(4294967295.)},
    ~js=`{value: 4294967295}`,
    ~empty={value: Integer(0.)},
  )
  t->holds(S.Protobuf.BoolValue.schema, ~value={value: true}, ~js=`{value: true}`, ~empty={value: false})
  t->holds(S.Protobuf.StringValue.schema, ~value={value: "é"}, ~js=`{value: "é"}`, ~empty={value: ""})
  t->holds(
    S.Protobuf.BytesValue.schema,
    ~value={value: bytes},
    ~js=`{value: new Uint8Array([1, 2, 255])}`,
    ~empty={value: Uint8Array.fromLength(0)},
  )
})
