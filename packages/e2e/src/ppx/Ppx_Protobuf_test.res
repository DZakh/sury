open Vitest

@schema
type address = {street: @s.with(S.protobufField(_, 1)) string}

@schema
type user = {
  id: @s.with(S.protobufField(_, 1)) int,
  home: @s.with(S.protobufField(_, 2)) option<address>,
}

test("A PPX record numbered with @s.with is a protobuf message", t => {
  let value = {id: 150, home: Some({street: "Main"})}
  let bytes = value->S.convertOrThrow(~from=userSchema, ~to=S.protobuf)
  t->Assert.deepEqual(bytes->S.convertOrThrow(~from=S.protobuf, ~to=userSchema), value)
})
