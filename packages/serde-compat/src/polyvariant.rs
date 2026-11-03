//! Ppx_Polyvariant_test.res

#![allow(non_camel_case_types)]

use crate::harness::{mirror, skip, Group};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

#[derive(Serialize, Deserialize, PartialEq, Debug)]
enum Poly {
    one,
    two,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
enum PolyWithSingleItem {
    single,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
enum One {
    one,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct RecordField {
    poly: One,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct Four {
    foo: String,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
enum PolyWithPayloads {
    one,
    two(i32),
    three(String, f64),
    four(Four),
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
enum PolyWithSinglePayload {
    value(String),
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
enum PolyWithInheritance {
    one,
    two,
    three,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
enum PolyWithPayloadInheritance {
    one,
    two(i32),
    three(String, f64),
    four(Four),
    five(bool),
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct PolyInheritanceField {
    variants: PolyWithInheritance,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
enum PolyInlineInheritance {
    a,
    b(i32),
    c,
}

pub fn group() -> Group {
    Group {
        name: "Polyvariant",
        types: vec![
            mirror::<Poly>("poly")
                .values([Poly::one, Poly::two])
                .rejects(&[r#""three""#, r#""One""#, r#"{"NAME":"one"}"#])
                .into(),
            mirror::<PolyWithSingleItem>("polyWithSingleItem")
                .values([PolyWithSingleItem::single])
                .rejects(&[r#""double""#])
                .into(),
            skip(
                "polyEmbeded",
                "@s.matches(S.string->S.shape(_ => #one)) reads any string as #one and can't write the string back",
            ),
            mirror::<BTreeMap<String, One>>("dictField")
                .values([BTreeMap::from([("a".to_string(), One::one)])])
                .rejects(&[r#"{"a":"two"}"#])
                .into(),
            mirror::<RecordField>("recordField")
                .values([RecordField { poly: One::one }])
                .rejects(&["{}", r#"{"poly":null}"#])
                .into(),
            mirror::<RecordField>("objectField")
                .values([RecordField { poly: One::one }])
                .rejects(&["{}"])
                .into(),
            mirror::<PolyWithPayloads>("polyWithPayloads")
                .values([
                    PolyWithPayloads::one,
                    PolyWithPayloads::two(123),
                    PolyWithPayloads::three("hello".into(), 1.5),
                    PolyWithPayloads::four(Four { foo: "bar".into() }),
                ])
                .rejects(&[
                    r#"{"NAME":"two","VAL":123}"#,
                    r#""two""#,
                    r#"{"two":"123"}"#,
                    r#"{"three":["hello"]}"#,
                    r#"{"four":{}}"#,
                    r#"{"two":1,"one":null}"#,
                ])
                .into(),
            mirror::<PolyWithSinglePayload>("polyWithSinglePayload")
                .values([PolyWithSinglePayload::value("hello".into())])
                .rejects(&[r#""value""#, r#"{"value":null}"#])
                .into(),
            mirror::<PolyWithInheritance>("polyWithInheritance")
                .values([PolyWithInheritance::one, PolyWithInheritance::two, PolyWithInheritance::three])
                .rejects(&[r#""four""#])
                .into(),
            mirror::<PolyWithPayloadInheritance>("polyWithPayloadInheritance")
                .values([
                    PolyWithPayloadInheritance::one,
                    PolyWithPayloadInheritance::two(123),
                    PolyWithPayloadInheritance::three("x".into(), 2.),
                    PolyWithPayloadInheritance::four(Four { foo: "y".into() }),
                    PolyWithPayloadInheritance::five(true),
                ])
                .rejects(&[r#"{"five":1}"#])
                .into(),
            mirror::<PolyInheritanceField>("polyInheritanceField")
                .values([
                    PolyInheritanceField { variants: PolyWithInheritance::one },
                    PolyInheritanceField { variants: PolyWithInheritance::three },
                ])
                .rejects(&[r#"{"variants":"four"}"#])
                .into(),
            mirror::<PolyInlineInheritance>("polyInlineInheritance")
                .values([PolyInlineInheritance::a, PolyInlineInheritance::b(7), PolyInlineInheritance::c])
                .rejects(&[r#""b""#, r#"{"a":1}"#])
                .into(),
        ],
    }
}
