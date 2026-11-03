//! Ppx_Variant_test.res

use crate::harness::{mirror, Group};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, PartialEq, Debug)]
enum Variant {
    One,
    Two,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
enum VariantWithSingleItem {
    Single,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
enum VariantWithAlias {
    #[serde(rename = "하나")]
    One,
    Two,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
enum VariantWithPayloads {
    Constant,
    SinglePayload(i32),
    TuplePayload(i32, String),
    RecordPayload { foo: f64 },
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
enum RenamedPayloads {
    #[serde(rename = "one")]
    One(i32),
    #[serde(rename = "two")]
    Two {
        value: String,
    },
    #[serde(rename = "maybe")]
    Maybe(Option<i32>),
    Empty(()),
}

/// `@unboxed`: a constructor without a payload is its name, the way a
/// single-variant enum reads inside an untagged one.
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(untagged)]
enum UnboxedVariant {
    Constant(ConstantTag),
    Int(i32),
    String(String),
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
enum ConstantTag {
    Constant,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag = "kind")]
enum TaggedVariant {
    #[serde(rename = "circle")]
    Circle { radius: f64 },
    #[serde(rename = "square")]
    Square { x: f64 },
    #[serde(rename = "triangle")]
    Triangle { x: f64, y: f64 },
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag = "kind")]
enum TaggedWithUnit {
    Idle,
    #[serde(rename = "busy")]
    Busy {
        since: f64,
    },
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(deny_unknown_fields)]
enum StrictVariant {
    StrictA,
    StrictB(i32),
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag = "type")]
enum TaggedInlinedAlias {
    Foo {
        #[serde(rename = "Foo")]
        foo: String,
    },
    Bar {
        #[serde(rename = "Bar")]
        bar: String,
    },
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
enum BaseColors {
    Red,
    Blue,
    Green,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
enum ExtendedColors {
    Red,
    Blue,
    Green,
    Yellow,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
enum SingleCase {
    Only,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
enum ExtendedFromSingleCase {
    Only,
    Another,
}

pub fn group() -> Group {
    Group {
        name: "Variant",
        types: vec![
            mirror::<Variant>("variant")
                .values([Variant::One, Variant::Two])
                .rejects(&[r#""Three""#, r#""one""#, "null", "1"])
                .diverges(
                    r#"{"One":null}"#,
                    "Serde also reads a unit variant from a map holding the variant's name over null; Sury reads only the string",
                )
                .into(),
            mirror::<VariantWithSingleItem>("variantWithSingleItem")
                .values([VariantWithSingleItem::Single])
                .rejects(&[r#""Other""#])
                .into(),
            mirror::<VariantWithAlias>("variantWithAlias")
                .values([VariantWithAlias::One, VariantWithAlias::Two])
                .rejects(&[r#""One""#])
                .into(),
            mirror::<VariantWithPayloads>("variantWithPayloads")
                .values([
                    VariantWithPayloads::Constant,
                    VariantWithPayloads::SinglePayload(1),
                    VariantWithPayloads::TuplePayload(1, "a".into()),
                    VariantWithPayloads::RecordPayload { foo: 1.5 },
                ])
                .accepts(&[r#"{"RecordPayload":{"foo":2,"bar":true}}"#])
                .rejects(&[
                    r#""SinglePayload""#,
                    r#"{"SinglePayload":"1"}"#,
                    r#"{"SinglePayload":null}"#,
                    r#"{"SinglePayload":1,"Constant":null}"#,
                    r#"{"TuplePayload":[1]}"#,
                    r#"{"TuplePayload":[1,"a",2]}"#,
                    r#"{"TuplePayload":{"0":1,"1":"a"}}"#,
                    r#"{"RecordPayload":{}}"#,
                    r#"{"Unknown":1}"#,
                    "{}",
                    r#"{"TAG":"SinglePayload","_0":1}"#,
                    "null",
                ])
                .diverges(
                    r#"{"RecordPayload":[1.5]}"#,
                    "Serde also reads a struct variant from an array of its fields in order; Sury reads only an object",
                )
                .into(),
            mirror::<RenamedPayloads>("renamedPayloads")
                .values([
                    RenamedPayloads::One(1),
                    RenamedPayloads::Two { value: "a".into() },
                    RenamedPayloads::Maybe(Some(2)),
                    RenamedPayloads::Maybe(None),
                    RenamedPayloads::Empty(()),
                ])
                .rejects(&[
                    r#"{"One":1}"#,
                    r#"{"maybe":"1"}"#,
                    "{}",
                    r#"{"Empty":{}}"#,
                    r#""Empty""#,
                ])
                .into(),
            mirror::<UnboxedVariant>("unboxedVariant")
                .values([
                    UnboxedVariant::Constant(ConstantTag::Constant),
                    UnboxedVariant::Int(1),
                    UnboxedVariant::String("a".into()),
                ])
                .rejects(&["null", "1.5", "true", "[]"])
                .into(),
            mirror::<TaggedVariant>("taggedVariant")
                .values([
                    TaggedVariant::Circle { radius: 1. },
                    TaggedVariant::Square { x: 2. },
                    TaggedVariant::Triangle { x: 1., y: 2.5 },
                ])
                .accepts(&[r#"{"radius":1,"kind":"circle","color":"red"}"#])
                .rejects(&[
                    r#"{"kind":"Circle","radius":1}"#,
                    r#"{"radius":1}"#,
                    r#"{"kind":"square"}"#,
                    r#"{"circle":{"radius":1}}"#,
                    r#""circle""#,
                ])
                .into(),
            mirror::<TaggedWithUnit>("taggedWithUnit")
                .values([TaggedWithUnit::Idle, TaggedWithUnit::Busy { since: 1.5 }])
                .accepts(&[r#"{"kind":"Idle","extra":1}"#])
                .rejects(&[r#""Idle""#, r#"{"kind":"idle"}"#, r#"{"kind":"busy"}"#])
                .into(),
            mirror::<StrictVariant>("strictVariant")
                .values([StrictVariant::StrictA, StrictVariant::StrictB(1)])
                .rejects(&[r#"{"StrictB":1,"extra":2}"#, r#"{"StrictB":"1"}"#])
                .into(),
            mirror::<TaggedInlinedAlias>("taggedInlinedAlias")
                .values([
                    TaggedInlinedAlias::Foo { foo: "a".into() },
                    TaggedInlinedAlias::Bar { bar: "b".into() },
                ])
                .rejects(&[r#"{"type":"Foo","foo":"a"}"#, r#"{"Foo":"a"}"#])
                .into(),
            mirror::<BaseColors>("baseColors")
                .values([BaseColors::Red, BaseColors::Blue, BaseColors::Green])
                .rejects(&[r#""Yellow""#])
                .into(),
            mirror::<ExtendedColors>("extendedColors")
                .values([ExtendedColors::Red, ExtendedColors::Blue, ExtendedColors::Green, ExtendedColors::Yellow])
                .rejects(&[r#""Purple""#])
                .into(),
            mirror::<SingleCase>("singleCase").values([SingleCase::Only]).rejects(&[r#""Another""#]).into(),
            mirror::<ExtendedFromSingleCase>("extendedFromSingleCase")
                .values([ExtendedFromSingleCase::Only, ExtendedFromSingleCase::Another])
                .rejects(&[r#""Other""#])
                .into(),
        ],
    }
}
