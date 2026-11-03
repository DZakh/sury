//! Ppx_regression_test.res

use crate::harness::{mirror, Group};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct PayloadA {
    a: Option<String>,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct WrapsA {
    payload: PayloadA,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct PayloadB {
    b: Option<i32>,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct WrapsB {
    payload: PayloadB,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct A {
    x: i32,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct B {
    y: String,
}

/// The hand-written `testSchema`: `s.tag("type", ...)` over a flattened record,
/// which is Serde's internally tagged newtype variant.
#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(tag = "type")]
enum Test {
    #[serde(rename = "a")]
    A(A),
    #[serde(rename = "b")]
    B(B),
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct T {
    test: Option<Test>,
}

pub fn group() -> Group {
    Group {
        name: "Regression",
        types: vec![
            mirror::<PayloadA>("CknittelBugReport.A.payload")
                .values([
                    PayloadA {
                        a: Some("foo".into()),
                    },
                    PayloadA { a: None },
                ])
                .accepts(&["{}"])
                .into(),
            mirror::<WrapsA>("CknittelBugReport.A.t")
                .values([WrapsA {
                    payload: PayloadA {
                        a: Some("foo".into()),
                    },
                }])
                .rejects(&["{}"])
                .into(),
            mirror::<PayloadB>("CknittelBugReport.B.payload")
                .values([PayloadB { b: Some(42) }, PayloadB { b: None }])
                .accepts(&["{}"])
                .into(),
            mirror::<WrapsB>("CknittelBugReport.B.t")
                .values([WrapsB {
                    payload: PayloadB { b: Some(42) },
                }])
                .rejects(&[r#"{"payload":{"b":"42"}}"#])
                .into(),
            mirror::<A>("CknittelBugReport2.a")
                .values([A { x: 1 }])
                .rejects(&["{}"])
                .into(),
            mirror::<B>("CknittelBugReport2.b")
                .values([B { y: "y".into() }])
                .rejects(&["{}"])
                .into(),
            mirror::<T>("CknittelBugReport2.t")
                .values([
                    T { test: None },
                    T {
                        test: Some(Test::A(A { x: 1 })),
                    },
                    T {
                        test: Some(Test::B(B { y: "y".into() })),
                    },
                ])
                .accepts(&["{}"])
                .rejects(&[r#"{"test":{"type":"c"}}"#, r#"{"test":{"type":"a"}}"#])
                .into(),
        ],
    }
}
