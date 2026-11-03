# serde-compat

Holds the schemas sury-ppx generates to [Serde](https://serde.rs). Every
`@schema` type in [`packages/e2e/src/ppx`](../e2e/src/ppx) has a Rust mirror
here: the type Serde reads the same JSON into, in its default mode. The mirror
writes what Serde does with sample values and with hand-picked inputs to
[`fixtures.json`](./fixtures.json), and each e2e test file holds its schemas to
that:

```rescript
assertSerdeCompatible("Record", [("simpleRecord", simpleRecordSchema->S.castToUnknown)])
```

For each case the schema has to read what Serde reads, refuse what Serde
refuses, and write back the JSON Serde writes. Serde reads its own output back
to the value it started from, so the same text goes both ways: Serde to Sury,
and Sury to Serde.

The read is `JSON.parse` then `S.parseOrThrow`. The write is
`S.convertOrThrow(~to=S.unknown)` then `JSON.stringify`, parsed again, so
whatever `JSON.stringify` loses on the wire is lost in the comparison too.

## Running it

```sh
cargo run            # rewrites fixtures.json
cargo run -- check   # fails on a stale fixtures.json, or a @schema type with no mirror
```

A mirror sits in the module named after the test file, `Ppx_Record_test.res`
in `src/record.rs`:

```rust
mirror::<SimpleRecord>("simpleRecord")
    // Serde writes these, and has to read each back to itself
    .values([SimpleRecord { label: "foo".into(), value: 1 }])
    // Serde reads these, though it never writes them
    .accepts(&[r#"{"value":1,"label":"foo","unknown":[1]}"#])
    // Serde refuses these
    .rejects(&[r#"{"label":"foo"}"#])
    // Sury answers these differently, for the reason given
    .diverges(r#"["foo",1]"#, "Serde also reads a struct from an array of its fields in order")
    .into(),
```

`.accepts` and `.rejects` check Serde agrees with the expectation, so a mirror
can't record a verdict it didn't mean. The e2e suite fails on a `.diverges`
case once Sury starts agreeing, so a closed gap can't linger in the list.
`skip(name, reason)` takes a type with no JSON form out.

## Mirror conventions

| ReScript | Rust |
| --- | --- |
| `int`, `float`, `S.port`, `S.integer` | `i32`, `f64`, `u16`, `i64` |
| a format (`S.email`, `S.uuid`, ...) | `String`, with values that pass Sury's check |
| `dict<'a>` | `BTreeMap<String, A>`, for a stable fixtures.json |
| a field name | the same name, `#![allow(non_snake_case)]` rather than `rename_all` |
| `x?: 'a` | `x: Option<A>` |
| a record spread | the spread fields, written out |
| `@as("k")`, `@tag("t")`, `@s.strict` | `rename = "k"`, `tag = "t"`, `deny_unknown_fields` |
| `@unboxed` | `untagged`, a payload-less constructor as a one-variant enum |
| `@s.default(v)` at the top level | the bare type: a JSON document is never missing |
| `@s.null @s.default(v)` | `#[serde(from = "Option<T>")]` mapping `None` to `v` |
| `@s.nullable` field | `#[serde(default, skip_serializing_if = "Option::is_none")]` |
| `x?: @s.null 'a` | `#[serde(deserialize_with = "Option::deserialize")]` |

A Sury-only check (`S.email`, `S.maxLength`) is a superset Serde knows nothing
about, so a mirror only feeds it values that pass.

## Known differences

The `.diverges` cases, by cause:

| Input | Serde | Sury |
| --- | --- | --- |
| `{"One": null}` for a unit variant | reads `One` | refuses: a unit variant is only its string |
| `["foo", 1]` for a struct or struct variant | reads the fields in order | refuses: a record is only an object |
| `1.0` for an `int` | refuses the float token | reads `1`: `JSON.parse` erased the `.0` |
| `{"value": 1, "value": 2}` | refuses the repeated key | reads `2`: `JSON.parse` kept the last |
| `-0.0` for a `float` | writes `-0.0` | writes `0`: `JSON.stringify` drops the sign |
| `null` for `@s.nullable option<'a>` at the top level | writes `null` | writes `undefined`, which isn't JSON |

## Not mirrored

| Type | Why |
| --- | --- |
| `S.url`, `S.date`, `S.blob`, `S.formData`, `S.urlSearchParams` | they read JS instances, which JSON can't hold |
| `@s.matches(S.option(S.string)) option<string>` | writes `None` as `undefined`, which a JSON document can't be |
| `@s.matches(S.string->S.shape(_ => #one)) [#one]` | reads any string as `#one` and can't write the string back |
| `@s.matches(S.unknown->magic) option<cut>` | takes whatever is there, a missing key included |

## Todo: full Serde compatibility

What Serde does that the ppx can't express yet, roughly in the order worth
doing. ✅ is supported, 🟡 partly, ⬜ not yet.

### Attributes

| Serde | Status | ppx |
| --- | --- | --- |
| `#[serde(rename_all = "...")]`, `rename_all_fields` | ⬜ | a `@s.renameAll("snake_case")` would spare a Rust service's snake_case keys one `@as` per field |
| `#[serde(tag = "t")]` | 🟡 | `@tag("t")` tags unit and struct variants in place. A newtype variant keeps its payload under `_0`, where Serde puts the struct's fields next to the tag |
| `#[serde(tag = "t", content = "c")]` | ⬜ | adjacently tagged: `{"t": "Variant", "c": payload}` |
| `#[serde(untagged)]` | 🟡 | `@unboxed`. A constructor without a payload reads its name, where Serde's unit variant is `null` |
| `#[serde(alias = "...")]` on a field or variant | ⬜ | more names to read, one to write |
| `#[serde(other)]` | ⬜ | a unit variant that takes any unknown tag |
| `#[serde(untagged)]` on one variant | ⬜ | an untagged fallback inside a tagged enum |
| `#[serde(skip)]`, `skip_serializing`, `skip_deserializing` | ⬜ | a field that isn't read, or isn't written |
| `#[serde(skip_serializing_if = "...")]` | 🟡 | `@s.nullable` is `Option::is_none`. No other predicate |
| `#[serde(flatten)]` | 🟡 | a record spread flattens a struct. A flattened map, collecting the unknown keys, isn't there |
| `#[serde(default)]` on a field | ✅ | `@s.default(value)`, `@s.defaultWith(fn)`, with the value spelled out |
| `#[serde(default)]` on a container | ⬜ | fill every missing field from one default value |
| `#[serde(rename = "...")]` on a field or variant | ✅ | `@as("...")` |
| `#[serde(deny_unknown_fields)]` | ✅ | `@s.strict`, `@s.deepStrict` |
| `#[serde(transparent)]` | 🟡 | `@unboxed type id = Id(string)` reads the payload. No form for a record with one field |
| `with`, `serialize_with`, `deserialize_with`, `from`, `try_from`, `into` | 🟡 | `@s.matches` and `@s.with` take a hand-written schema |
| `#[serde(expecting = "...")]` | 🟡 | `@s.meta({errorMessage: ...})` |

`bound`, `borrow`, `crate`, `remote`, `getter`, a container's `rename` and the
identifier attributes change nothing in the JSON.

### Default-mode reads and types

- ⬜ A field whose type is an alias of `option<_>`. Serde fills every missing
  `Option` field with `None`, however the type is spelled; the ppx sees
  `option<_>` written out, or `?`.
- ⬜ A unit variant read from its map form, `{"Variant": null}`.
- ⬜ A struct or struct variant read from an array of its fields in order.
- ⬜ Integer widths. Serde has `u8` to `u128` and `i8` to `i128`, the ppx `int`
  (`i32`), `S.port` (`u16`) and `S.integer` (up to 2^53). Past 2^53 needs a
  JSON reader that keeps the digits, since `JSON.parse` rounds them: then
  `bigint` could read `i64` and `u64`.
- ⬜ `char`, a one-character string.
- ⬜ Map keys that aren't strings: `HashMap<u32, T>` writes `{"1": ...}`.
- ⬜ A single-constructor variant as a struct. Serde writes the tuple struct
  `struct P(i32, i32)` as `[1, 2]` and the unit struct `struct U;` as `null`;
  the ppx reads `type p = P(int, int)` as the enum `{"P": [1, 2]}`.
- ⬜ Bytes. Serde writes `Vec<u8>` as an array of numbers, Sury `S.uint8Array`
  as base64.
- ⬜ Dates and URLs. chrono, time and `url::Url` write strings, `S.date` and
  `S.url` read JS instances.
- ⬜ Non-finite floats: Serde writes NaN and infinities as `null`.
- ⬜ `1.0` for an integer and repeated keys: Serde refuses both, and both are
  gone after `JSON.parse`. Same reader as past 2^53.
- ⬜ `-0.0`, written as `0` by `JSON.stringify`.
- ⬜ Reading through `S.json` rather than `S.parseOrThrow`: there `null` reads
  as missing for an `@s.default` field, which Serde refuses. Writing a
  top-level string to `S.jsonString` asks whether it's packed or unpacked.
