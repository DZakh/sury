//! Ppx_Object_test.res

use crate::harness::{mirror, Group};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct SimpleObject {
    label: String,
    value: i32,
}

pub fn group() -> Group {
    Group {
        name: "Object",
        types: vec![mirror::<SimpleObject>("simpleObject")
            .values([SimpleObject {
                label: "foo".into(),
                value: 1,
            }])
            .accepts(&[r#"{"value":1,"label":"foo","other":[]}"#])
            .rejects(&[
                r#"{"label":"foo"}"#,
                r#"{"label":"foo","value":"1"}"#,
                "null",
            ])
            .into()],
    }
}
