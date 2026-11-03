//! Ppx_Format_test.res
//!
//! A format is a string or number Sury also checks the syntax of, so Serde's
//! plain `String` or integer mirrors it for every value that passes the check.

#![allow(non_snake_case)]

use crate::harness::{mirror, skip, Group};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct User {
    id: String,
    email: String,
    tags: Vec<String>,
    homepage: Option<String>,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct Identifiers {
    key: String,
    legacyKey: String,
    slug: String,
    event: String,
    trace: String,
    token: String,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct Host {
    block: String,
    address: String,
    hardware: String,
    homepage: String,
    phone: String,
    digest: String,
}

pub fn group() -> Group {
    Group {
        name: "Format",
        types: vec![
            mirror::<String>("myEmail").values(["a@b.com".to_string()]).rejects(&["1", "null"]).into(),
            mirror::<u16>("myPort")
                .values([0, 8080, 65535])
                .rejects(&["65536", "-1", "80.5", r#""80""#])
                .into(),
            mirror::<i64>("myInteger")
                .values([0, -5, 3_000_000_000, 9_007_199_254_740_991])
                .rejects(&["1.5", r#""1""#])
                .into(),
            mirror::<String>("myJsonString")
                .values([r#"{"a":1}"#.to_string(), "[]".to_string()])
                .rejects(&["1", r#"{"a":1}"#])
                .into(),
            skip("myUrl", "S.url reads a JS URL instance, which JSON can't hold"),
            skip("myDate", "S.date reads a JS Date instance, which JSON can't hold"),
            skip("myBlob", "S.blob reads a JS Blob, which JSON can't hold"),
            skip("myFormData", "S.formData reads a JS FormData, which JSON can't hold"),
            mirror::<String>("myEnv").values(["42".to_string()]).rejects(&["null"]).into(),
            skip(
                "myUrlSearchParams",
                "S.urlSearchParams reads a JS URLSearchParams, which JSON can't hold",
            ),
            mirror::<String>("myQueryString").values(["a=1&b=2".to_string()]).rejects(&["null"]).into(),
            mirror::<Vec<String>>("myNonEmptyTags")
                .values([vec!["x".to_string()]])
                .rejects(&["null", "[1]"])
                .into(),
            mirror::<User>("user")
                .values([
                    User {
                        id: "123e4567-e89b-12d3-a456-426614174000".into(),
                        email: "a@b.com".into(),
                        tags: vec!["x".into()],
                        homepage: None,
                    },
                    User {
                        id: "123e4567-e89b-12d3-a456-426614174000".into(),
                        email: "a@b.com".into(),
                        tags: vec!["x".into(), "y".into()],
                        homepage: Some("https://example.com".into()),
                    },
                ])
                .accepts(&[r#"{"id":"123e4567-e89b-12d3-a456-426614174000","email":"a@b.com","tags":["x"]}"#])
                .rejects(&[r#"{"id":"123e4567-e89b-12d3-a456-426614174000","email":"a@b.com"}"#])
                .into(),
            mirror::<Identifiers>("identifiers")
                .values([Identifiers {
                    key: "018f3f8e-7b1a-7c3d-8e4f-9a0b1c2d3e4f".into(),
                    legacyKey: "123e4567-e89b-12d3-a456-426614174000".into(),
                    slug: "tz4a98xxat96iws9zmbrgj3a".into(),
                    event: "01ARZ3NDEKTSV4RRFFQ69G5FAV".into(),
                    trace: "9m4e2mr0ui3e8a215n4g".into(),
                    token: "V1StGXR8_Z5jdHi6B-myT".into(),
                }])
                .into(),
            mirror::<Host>("host")
                .values([Host {
                    block: "192.168.0.0/16".into(),
                    address: "192.168.0.1".into(),
                    hardware: "00:1b:44:11:3a:b7".into(),
                    homepage: "https://example.com".into(),
                    phone: "+14155552671".into(),
                    digest: "deadbeef".into(),
                }])
                .rejects(&[r#"{"block":"192.168.0.0/16"}"#])
                .into(),
        ],
    }
}
