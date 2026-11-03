//! Ppx_Record_test.res

#![allow(non_snake_case)]

use crate::harness::{mirror, Group};
use serde::{Deserialize, Deserializer, Serialize};

#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(deny_unknown_fields)]
struct StrictRecord {
    strictEmail: String,
    strictName: String,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct StripRecord {
    stripEmail: String,
    stripName: String,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(deny_unknown_fields)]
struct DeepStrictRecord {
    deepStrictEmail: String,
    deepStrictName: String,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct DeepStripRecord {
    deepStripEmail: String,
    deepStripName: String,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct NoValidationRecord {
    noValEmail: String,
    noValName: String,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct MetaRecord {
    metaEmail: String,
    metaName: String,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(deny_unknown_fields)]
struct CombinedRecord {
    combinedEmail: String,
    combinedName: String,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct SimpleRecord {
    label: String,
    value: i32,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct RecordWithAlias {
    #[serde(rename = "aliased-label")]
    label: String,
    value: i32,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct RecordWithOptional {
    label: Option<String>,
    value: Option<i32>,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct RecordWithOptionalDefaulted {
    darkMode: Option<bool>,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct EmptyRecord {}

/// `@s.matches(S.option(S.nullAsOption(S.string))) option<option<string>>`:
/// a missing key, `null` and a string are three different values.
#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct RecordWithNullableField {
    #[serde(
        default,
        skip_serializing_if = "Option::is_none",
        deserialize_with = "present"
    )]
    subscription: Option<Option<String>>,
}

fn present<'de, D: Deserializer<'de>, T: Deserialize<'de>>(
    deserializer: D,
) -> Result<Option<T>, D::Error> {
    T::deserialize(deserializer).map(Some)
}

/// `foo?: @s.nullable string`: `None` writes as a missing key.
#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct RecordWithOptionalNullableField {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    foo: Option<String>,
}

/// `foo?: @s.null string`: the key has to be there, `null` is `None`.
#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct RecordWithOptionalNullField {
    #[serde(deserialize_with = "Option::deserialize")]
    foo: Option<String>,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct Meta {
    id: String,
    summary: Option<String>,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct Extra {
    score: f64,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct RecordWithSpread {
    id: String,
    summary: Option<String>,
    messages: Vec<String>,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct RecordWithMultipleSpreads {
    id: String,
    summary: Option<String>,
    score: f64,
    active: bool,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct RecordWithMultipleSpreadsNoFields {
    id: String,
    summary: Option<String>,
    score: f64,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct RecordWithSpreadAndAliasedField {
    id: String,
    summary: Option<String>,
    #[serde(rename = "ms")]
    messages: Vec<String>,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct RecordWithSpreadAndOptionalField {
    id: String,
    summary: Option<String>,
    extra: Option<i32>,
}

fn meta(id: &str, summary: Option<&str>) -> Meta {
    Meta {
        id: id.into(),
        summary: summary.map(Into::into),
    }
}

pub fn group() -> Group {
    Group {
        name: "Record",
        types: vec![
            mirror::<StrictRecord>("strictRecord")
                .values([StrictRecord { strictEmail: "a@b.c".into(), strictName: "A".into() }])
                .rejects(&[r#"{"strictEmail":"a@b.c","strictName":"A","extra":1}"#, r#"{"strictEmail":"a@b.c"}"#])
                .into(),
            mirror::<StripRecord>("stripRecord")
                .values([StripRecord { stripEmail: "a@b.c".into(), stripName: "A".into() }])
                .accepts(&[r#"{"stripEmail":"a@b.c","stripName":"A","extra":1}"#])
                .into(),
            mirror::<DeepStrictRecord>("deepStrictRecord")
                .values([DeepStrictRecord { deepStrictEmail: "a@b.c".into(), deepStrictName: "A".into() }])
                .rejects(&[r#"{"deepStrictEmail":"a@b.c","deepStrictName":"A","extra":{}}"#])
                .into(),
            mirror::<DeepStripRecord>("deepStripRecord")
                .values([DeepStripRecord { deepStripEmail: "a@b.c".into(), deepStripName: "A".into() }])
                .accepts(&[r#"{"deepStripEmail":"a@b.c","deepStripName":"A","extra":{}}"#])
                .into(),
            mirror::<NoValidationRecord>("noValidationRecord")
                .values([NoValidationRecord { noValEmail: "a@b.c".into(), noValName: "A".into() }])
                .into(),
            mirror::<MetaRecord>("metaRecord")
                .values([MetaRecord { metaEmail: "a@b.c".into(), metaName: "A".into() }])
                .rejects(&[r#"{"metaEmail":"a@b.c"}"#])
                .into(),
            mirror::<CombinedRecord>("combinedRecord")
                .values([CombinedRecord { combinedEmail: "a@b.c".into(), combinedName: "A".into() }])
                .rejects(&[r#"{"combinedEmail":"a@b.c","combinedName":"A","extra":1}"#])
                .into(),
            mirror::<SimpleRecord>("simpleRecord")
                .values([SimpleRecord { label: "foo".into(), value: 1 }])
                .accepts(&[r#"{"value":1,"label":"foo","unknown":[1]}"#])
                .rejects(&[
                    r#"{"label":"foo"}"#,
                    r#"{"label":"foo","value":null}"#,
                    r#"{"label":1,"value":1}"#,
                    "[]",
                    "null",
                ])
                .diverges(
                    r#"["foo",1]"#,
                    "Serde also reads a struct from an array of its fields in order; Sury reads only an object",
                )
                .diverges(
                    r#"{"label":"foo","value":1,"value":2}"#,
                    "Serde refuses a repeated key; JSON.parse keeps the last one",
                )
                .into(),
            mirror::<RecordWithAlias>("recordWithAlias")
                .values([RecordWithAlias { label: "foo".into(), value: 1 }])
                .rejects(&[r#"{"label":"foo","value":1}"#])
                .into(),
            mirror::<RecordWithOptional>("recordWithOptional")
                .values([
                    RecordWithOptional { label: Some("foo".into()), value: Some(1) },
                    RecordWithOptional { label: None, value: None },
                ])
                .accepts(&["{}", r#"{"label":null}"#, r#"{"value":2}"#])
                .rejects(&[r#"{"label":1}"#, r#"{"value":"1"}"#])
                .into(),
            mirror::<bool>("flagWithDefault").values([true, false]).rejects(&["null"]).into(),
            mirror::<RecordWithOptionalDefaulted>("recordWithOptionalDefaulted")
                .values([
                    RecordWithOptionalDefaulted { darkMode: Some(true) },
                    RecordWithOptionalDefaulted { darkMode: Some(false) },
                    RecordWithOptionalDefaulted { darkMode: None },
                ])
                .accepts(&["{}"])
                .rejects(&[r#"{"darkMode":"yes"}"#])
                .into(),
            mirror::<EmptyRecord>("emptyRecord")
                .values([EmptyRecord {}])
                .accepts(&[r#"{"any":1}"#])
                .rejects(&["null", r#""""#])
                .diverges(
                    "[]",
                    "Serde also reads a struct from an array of its fields in order; Sury reads only an object",
                )
                .into(),
            mirror::<RecordWithNullableField>("recordWithNullableField")
                .values([
                    RecordWithNullableField { subscription: None },
                    RecordWithNullableField { subscription: Some(None) },
                    RecordWithNullableField { subscription: Some(Some("pro".into())) },
                ])
                .rejects(&[r#"{"subscription":1}"#])
                .into(),
            mirror::<RecordWithOptionalNullableField>("recordWithOptionalNullableField")
                .values([
                    RecordWithOptionalNullableField { foo: None },
                    RecordWithOptionalNullableField { foo: Some("bar".into()) },
                ])
                .accepts(&[r#"{"foo":null}"#])
                .rejects(&[r#"{"foo":1}"#])
                .into(),
            mirror::<RecordWithOptionalNullField>("recordWithOptionalNullField")
                .values([
                    RecordWithOptionalNullField { foo: None },
                    RecordWithOptionalNullField { foo: Some("bar".into()) },
                ])
                .rejects(&["{}", r#"{"foo":1}"#])
                .into(),
            mirror::<Meta>("Meta.t")
                .values([meta("abc", Some("hello")), meta("abc", None)])
                .accepts(&[r#"{"id":"abc"}"#])
                .rejects(&[r#"{"summary":"hello"}"#])
                .into(),
            mirror::<RecordWithSpread>("recordWithSpread")
                .values([RecordWithSpread { id: "abc".into(), summary: Some("hello".into()), messages: vec!["a".into()] }])
                .accepts(&[r#"{"id":"abc","messages":[]}"#])
                .rejects(&[r#"{"id":"abc","summary":"hello"}"#])
                .into(),
            mirror::<Extra>("Extra.t").values([Extra { score: 9.5 }]).rejects(&["{}"]).into(),
            mirror::<RecordWithMultipleSpreads>("recordWithMultipleSpreads")
                .values([RecordWithMultipleSpreads { id: "abc".into(), summary: None, score: 9.5, active: true }])
                .rejects(&[r#"{"id":"abc","score":9.5}"#])
                .into(),
            mirror::<Meta>("recordWithOnlySpread")
                .values([meta("abc", Some("hi")), meta("abc", None)])
                .accepts(&[r#"{"id":"abc"}"#])
                .into(),
            mirror::<RecordWithMultipleSpreadsNoFields>("recordWithMultipleSpreadsNoFields")
                .values([RecordWithMultipleSpreadsNoFields { id: "abc".into(), summary: Some("hi".into()), score: 5. }])
                .rejects(&[r#"{"id":"abc"}"#])
                .into(),
            mirror::<RecordWithSpreadAndAliasedField>("recordWithSpreadAndAliasedField")
                .values([RecordWithSpreadAndAliasedField { id: "abc".into(), summary: Some("hi".into()), messages: vec!["a".into()] }])
                .rejects(&[r#"{"id":"abc","messages":[]}"#])
                .into(),
            mirror::<RecordWithSpreadAndOptionalField>("recordWithSpreadAndOptionalField")
                .values([
                    RecordWithSpreadAndOptionalField { id: "abc".into(), summary: Some("hi".into()), extra: Some(5) },
                    RecordWithSpreadAndOptionalField { id: "abc".into(), summary: None, extra: None },
                ])
                .accepts(&[r#"{"id":"abc"}"#, r#"{"id":"abc","extra":null}"#])
                .into(),
        ],
    }
}
