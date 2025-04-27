@schema
type t = string

@schema
type foo

type bar

@schema
type reusedTypes = (t, foo, bar, float)
