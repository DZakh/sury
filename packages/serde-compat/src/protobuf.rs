//! Ppx_Protobuf_test.res - the same records read as JSON.

use crate::harness::{mirror, Group};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct Address {
    street: String,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct User {
    id: i32,
    home: Option<Address>,
}

pub fn group() -> Group {
    Group {
        name: "Protobuf",
        types: vec![
            mirror::<Address>("address")
                .values([Address {
                    street: "Main".into(),
                }])
                .rejects(&["{}"])
                .into(),
            mirror::<User>("user")
                .values([
                    User {
                        id: 150,
                        home: Some(Address {
                            street: "Main".into(),
                        }),
                    },
                    User { id: 1, home: None },
                ])
                .accepts(&[r#"{"id":1}"#])
                .rejects(&[r#"{"home":null}"#])
                .into(),
        ],
    }
}
