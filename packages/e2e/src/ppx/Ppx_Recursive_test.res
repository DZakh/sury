open Vitest
open U

@schema
type rec node = {
  id: string,
  children: array<node>,
}
test("Self-recursive record", t => {
  t->Assert.deepEqual(
    {"id": "1", "children": [{"id": "2", "children": []}]}->S.parseOrThrow(~to=nodeSchema),
    {id: "1", children: [{id: "2", children: []}]},
  )
  t->assertReverseParsesBack(nodeSchema, {id: "1", children: [{id: "2", children: []}]})
})

@schema
type rec tree = Leaf(int) | Branch(array<tree>)
test("Self-recursive variant", t => {
  t->assertReverseParsesBack(treeSchema, Branch([Leaf(1), Branch([Leaf(2)])]))
})

@schema
type rec polyTree = [#leaf(int) | #branch(array<polyTree>)]
test("Self-recursive polyvariant", t => {
  t->assertReverseParsesBack(polyTreeSchema, #branch([#leaf(1), #branch([#leaf(2)])]))
})

@schema
type rec chain = {next: option<chain>}
test("Self-recursive through option", t => {
  t->assertReverseParsesBack(chainSchema, {next: Some({next: Some({next: None})})})
})

@schema
type rec pair = {both: option<(pair, pair)>}
test("Self-recursive through tuple", t => {
  t->assertReverseParsesBack(pairSchema, {both: Some(({both: None}, {both: None}))})
})

@schema
type rec notActuallyRec = int
test("A rec type that never references itself stays a plain schema", t => {
  t->assertEqualSchemas(notActuallyRecSchema, S.int)
})

@schema
type rec expr = Num(int) | Block(array<stmt>)
@schema
and stmt = {label: string, body: expr}
test("Mutually recursive types work from either entry point", t => {
  t->assertReverseParsesBack(
    exprSchema,
    Block([{label: "a", body: Num(1)}, {label: "b", body: Block([])}]),
  )
  t->assertReverseParsesBack(stmtSchema, {label: "x", body: Block([{label: "y", body: Num(2)}])})
})

@schema
type rec a = {b: option<b>}
@schema
and b = {c: option<c>}
@schema
and c = {a: option<a>}
test("Three-way mutual cycle works from every entry point", t => {
  t->assertReverseParsesBack(aSchema, {b: Some({c: Some({a: Some({b: None})})})})
  t->assertReverseParsesBack(bSchema, {c: Some({a: Some({b: Some({c: None})})})})
  t->assertReverseParsesBack(cSchema, {a: Some({b: Some({c: Some({a: None})})})})
})

@schema
type rec root = {mid: option<mid>, last: option<last>}
@schema
and mid = {tail: option<last>}
@schema
and last = {back: option<root>}
test("A sibling needed by several members is bound once and reused", t => {
  t->assertReverseParsesBack(rootSchema, {mid: Some({tail: Some({back: None})}), last: None})
  t->assertReverseParsesBack(midSchema, {tail: Some({back: Some({mid: None, last: None})})})
  t->assertReverseParsesBack(lastSchema, {back: Some({mid: None, last: Some({back: None})})})
})

@schema
type rec g1 = {toG2: option<g2>}
@schema
and g2 = {toG3: option<g3>}
@schema
and g3 = {toG4: option<g4>}
@schema
and g4 = {toG5: option<g5>}
@schema
and g5 = {toG1: option<g1>}
test("Mutual groups are not capped in size", t => {
  t->assertReverseParsesBack(
    g1Schema,
    {toG2: Some({toG3: Some({toG4: Some({toG5: Some({toG1: None})})})})},
  )
  t->assertReverseParsesBack(g3Schema, {toG4: Some({toG5: Some({toG1: Some({toG2: None})})})})
})

@schema
type rec leaf = {name: string}
@schema
and holder = {leaf: leaf, next: option<holder>}
test("A rec group member that isn't recursive is left unwrapped and bound first", t => {
  t->assertEqualSchemas(leafSchema, S.schema(s => {name: s.matches(S.string)}))
  t->assertReverseParsesBack(
    holderSchema,
    {leaf: {name: "a"}, next: Some({leaf: {name: "b"}, next: None})},
  )
})

@schema @s.strict
type rec strictNode = {id: string, kids: array<strictNode>}
test("Type-level attributes apply to self-references too", t => {
  t->assertThrowsMessage(
    () =>
      {"id": "1", "kids": [{"id": "2", "kids": [], "extra": true}]}
      ->S.parseOrThrow(~to=strictNodeSchema)
      ->ignore,
    `Failed at kids[0]: Unrecognized key "extra"`,
  )
})

@schema
type rec matchesSelf = {
  id: string,
  children: @s.matches(S.array(matchesSelfSchema)) array<matchesSelf>,
}
test("@s.matches payload can reference the schema being defined", t => {
  t->assertReverseParsesBack(matchesSelfSchema, {id: "1", children: [{id: "2", children: []}]})
})

@schema
type rec cut = {self: @s.matches(S.unknown->magic) option<cut>}
test("A self-reference fully replaced by @s.matches leaves a plain schema", t => {
  t->assertEqualSchemas(cutSchema, S.schema(s => {self: s.matches(S.unknown->magic)}))
})

@schema
type rec aliased = {@as("Id") id: string, @as("Kids") kids: array<aliased>}
test("Field aliases survive the recursive wrapper", t => {
  t->Assert.deepEqual(
    {"Id": "1", "Kids": [{"Id": "2", "Kids": []}]}->S.parseOrThrow(~to=aliasedSchema),
    {id: "1", kids: [{id: "2", kids: []}]},
  )
  t->assertReverseParsesBack(aliasedSchema, {id: "1", kids: [{id: "2", kids: []}]})
})
