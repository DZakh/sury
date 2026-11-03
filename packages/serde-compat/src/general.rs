//! Ppx_General_test.res

use crate::harness::{mirror, skip, Group};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct UserWithWith {
    name: String,
    age: i32,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct RecordWithOptionalWithField {
    maybe: Option<String>,
}

/// `@s.default("https://example.com") @s.null @s.matches(S.string->S.trim) string`:
/// `null` reads as the default.
#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
#[serde(from = "Option<String>", into = "String")]
struct NullOrExample(String);

impl From<Option<String>> for NullOrExample {
    fn from(value: Option<String>) -> Self {
        NullOrExample(value.unwrap_or_else(|| "https://example.com".to_string()))
    }
}

impl From<NullOrExample> for String {
    fn from(value: NullOrExample) -> Self {
        value.0
    }
}

pub fn group() -> Group {
    Group {
        name: "General",
        types: vec![
            mirror::<String>("t").values(["a".to_string()]).rejects(&["1"]).into(),
            mirror::<i32>("foo").values([1, -2147483648, 2147483647]).rejects(&["2147483648", "1.5", r#""1""#]).into(),
            mirror::<(String, i32, bool, f64)>("reusedTypes")
                .values([("a".to_string(), 1, true, 1.5)])
                .rejects(&[r#"["a",1,true]"#, r#"["a",1,true,1.5,null]"#, r#"{"0":"a"}"#])
                .into(),
            mirror::<String>("stringWithDefault").values(["a".to_string()]).rejects(&["null"]).into(),
            mirror::<String>("stringWithDefaultAndMatches").values(["a".to_string()]).rejects(&["null"]).into(),
            mirror::<NullOrExample>("stringWithDefaultNullAndMatches")
                .values([NullOrExample("a".into())])
                .accepts(&["null"])
                .rejects(&["1"])
                .into(),
            skip(
                "ignoredNullWithMatches",
                "@s.matches(S.option(S.string)) writes None as undefined, which a JSON document can't be",
            ),
            mirror::<String>("stringWithWith").values(["a".to_string()]).rejects(&["null"]).into(),
            mirror::<String>("stringWithMultipleWith").values(["abc".to_string()]).rejects(&["1"]).into(),
            mirror::<UserWithWith>("userWithWith")
                .values([UserWithWith { name: "Jo".into(), age: 18 }])
                .accepts(&[r#"{"name":"Jo","age":20,"extra":null}"#])
                .rejects(&[r#"{"name":"Jo"}"#, r#"{"name":"Jo","age":null}"#])
                .into(),
            mirror::<String>("stringWithDefaultAndWith").values(["a".to_string()]).rejects(&["null"]).into(),
            mirror::<String>("stringWithWithAndDefault").values(["a".to_string()]).rejects(&["null"]).into(),
            mirror::<i32>("intWithWithPlaceholder").values([1, 5]).rejects(&["null", "1.5"]).into(),
            mirror::<RecordWithOptionalWithField>("recordWithOptionalWithField")
                .values([
                    RecordWithOptionalWithField { maybe: Some("a".into()) },
                    RecordWithOptionalWithField { maybe: None },
                ])
                .accepts(&["{}"])
                .rejects(&[r#"{"maybe":1}"#])
                .into(),
            mirror::<Vec<String>>("paramWithWith<string>")
                .values([vec!["a".to_string()]])
                .rejects(&["[1]", "null"])
                .into(),
            mirror::<Vec<String>>("withOverNestedAttributes")
                .values([vec!["a".to_string(), "b".to_string()]])
                .rejects(&["[null]", "[1]"])
                .into(),
        ],
    }
}
