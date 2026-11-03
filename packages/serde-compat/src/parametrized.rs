//! Ppx_Parametrized_test.res

use crate::harness::{mirror, Group};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct Wrapper<A> {
    value: A,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct Parent {
    wrapped: Wrapper<i32>,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct Nested<A> {
    inner: Wrapper<A>,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct WithOption<A> {
    data: Option<A>,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct WithArray<A> {
    items: Vec<A>,
}

/// The hand-written `result2Schema` reads ReScript's own representation of a
/// constructor with one payload, which is Serde's adjacently tagged enum.
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag = "TAG", content = "_0")]
enum Result2<A, B> {
    R2A(A),
    R2B(B),
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct Holder {
    res: Result2<i32, String>,
}

pub fn group() -> Group {
    Group {
        name: "Parametrized",
        types: vec![
            mirror::<Wrapper<i32>>("wrapper<int>")
                .values([Wrapper { value: 42 }])
                .rejects(&["{}", r#"{"value":"42"}"#])
                .into(),
            mirror::<Wrapper<String>>("wrapper<string>")
                .values([Wrapper {
                    value: "hello".to_string(),
                }])
                .rejects(&[r#"{"value":42}"#])
                .into(),
            mirror::<Parent>("parent")
                .values([Parent {
                    wrapped: Wrapper { value: 42 },
                }])
                .rejects(&[r#"{"wrapped":42}"#])
                .into(),
            mirror::<Nested<String>>("nested<string>")
                .values([Nested {
                    inner: Wrapper {
                        value: "deep".to_string(),
                    },
                }])
                .rejects(&[r#"{"inner":{"value":1}}"#])
                .into(),
            mirror::<WithOption<i32>>("withOption<int>")
                .values([WithOption { data: Some(5) }, WithOption { data: None }])
                .accepts(&["{}"])
                .rejects(&[r#"{"data":"5"}"#])
                .into(),
            mirror::<WithArray<String>>("withArray<string>")
                .values([WithArray {
                    items: vec!["a".to_string(), "b".to_string()],
                }])
                .rejects(&[r#"{"items":"a"}"#])
                .into(),
            mirror::<Holder>("holder")
                .values([
                    Holder {
                        res: Result2::R2A(1),
                    },
                    Holder {
                        res: Result2::R2B("boom".into()),
                    },
                ])
                .rejects(&[r#"{"res":{"TAG":"R2A","_0":"1"}}"#, r#"{"res":{"R2A":1}}"#])
                .into(),
            mirror::<String>("id<int>")
                .values(["hello".to_string()])
                .rejects(&["1"])
                .into(),
        ],
    }
}
