//! Ppx_Primitive_test.res

use crate::harness::{mirror, Group};
use serde::{Deserialize, Serialize};
use serde_json::json;
use std::collections::BTreeMap;

#[derive(Serialize, Deserialize, PartialEq, Debug)]
enum Never {}

/// `@s.matches(S.literal("123")) string`
#[derive(Serialize, Deserialize, PartialEq, Debug)]
enum Literal123 {
    #[serde(rename = "123")]
    Value,
}

fn dict(entries: &[(&str, &str)]) -> BTreeMap<String, String> {
    entries
        .iter()
        .map(|(k, v)| (k.to_string(), v.to_string()))
        .collect()
}

pub fn group() -> Group {
    Group {
        name: "Primitive",
        types: vec![
            mirror::<String>("myString")
                .values(["".to_string(), "Hello World!".to_string(), "\u{1F600} \"quoted\"\n".to_string()])
                .rejects(&["1", "null", "true", "[]"])
                .into(),
            mirror::<i32>("myInt")
                .values([0, 1, -1, 2147483647, -2147483648])
                .rejects(&["2147483648", "-2147483649", "1.5", r#""1""#, "null"])
                .diverges(
                    "1.0",
                    "Serde refuses a float token for an integer; JSON.parse has already made 1.0 the number 1",
                )
                .into(),
            mirror::<f64>("myFloat")
                .values([0., 1., 1.5, -2.25, 1e300, f64::MAX])
                .accepts(&["1", "-3", "1e2"])
                .rejects(&["null", r#""1""#])
                .diverges(
                    "-0.0",
                    "JSON.stringify writes -0 as 0, so Sury loses the sign Serde keeps",
                )
                .into(),
            mirror::<bool>("myBool").values([true, false]).rejects(&["0", r#""true""#, "null"]).into(),
            mirror::<()>("myUnit").values([()]).rejects(&["{}", "[]", "0", r#""""#]).into(),
            mirror::<serde_json::Value>("myUnknown")
                .values([json!(null), json!(1.5), json!("a"), json!([1, {"b": null}]), json!({"a": true})])
                .into(),
            mirror::<Never>("myNever").rejects(&["null", "1", r#""a""#, "{}"]).into(),
            mirror::<Option<String>>("myOptionOfString")
                .values([Some("a".to_string()), None])
                .rejects(&["1", "[]"])
                .into(),
            mirror::<Option<String>>("myNullableOfString")
                .values([Some("a".to_string()), None])
                .rejects(&["1"])
                .into(),
            mirror::<Vec<String>>("myArrayOfString")
                .values([vec![], vec!["a".to_string(), "b".to_string()]])
                .rejects(&["null", "[1]", r#"{"0":"a"}"#, r#""a""#])
                .into(),
            mirror::<Vec<String>>("myListOfString")
                .values([vec![], vec!["a".to_string(), "b".to_string()]])
                .rejects(&["null", "[1]"])
                .into(),
            mirror::<BTreeMap<String, String>>("myDictOfString")
                .values([dict(&[]), dict(&[("a", "x"), ("b", "y")])])
                .rejects(&["null", r#"{"a":1}"#, r#"["a"]"#])
                .into(),
            mirror::<BTreeMap<String, String>>("myDictOfStringFromJs")
                .values([dict(&[("a", "x")])])
                .rejects(&[r#"{"a":null}"#])
                .into(),
            mirror::<BTreeMap<String, String>>("myDictOfStringFromCore")
                .values([dict(&[("a", "x")])])
                .rejects(&[r#"{"a":null}"#])
                .into(),
            mirror::<serde_json::Value>("myJson")
                .values([json!(null), json!({"a": [1, 2.5, "x", false]})])
                .into(),
            mirror::<serde_json::Value>("myJsonFromCore").values([json!([null, {}])]).into(),
            mirror::<(String, i32)>("myTuple")
                .values([("a".to_string(), 1)])
                .rejects(&[r#"["a"]"#, r#"["a",1,2]"#, r#"[1,"a"]"#, "null"])
                .into(),
            mirror::<(String, String, String, i32, i32, i32, f64, f64, f64, bool, bool, bool)>("myBigTuple")
                .values([(
                    "a".to_string(),
                    "b".to_string(),
                    "c".to_string(),
                    1,
                    2,
                    3,
                    1.5,
                    2.5,
                    3.5,
                    true,
                    false,
                    true,
                )])
                .rejects(&[r#"["a","b","c",1,2,3,1.5,2.5,3.5,true,false]"#])
                .into(),
            mirror::<String>("myCustomString").values(["a".to_string()]).rejects(&["1"]).into(),
            mirror::<Literal123>("myCustomLiteralString")
                .values([Literal123::Value])
                .rejects(&[r#""124""#, "123"])
                .into(),
            mirror::<Option<String>>("myCustomOptionalString")
                .values([Some("a".to_string()), None])
                .rejects(&["1"])
                .into(),
            mirror::<Option<String>>("myNullOfString")
                .values([Some("a".to_string()), None])
                .rejects(&["1"])
                .into(),
        ],
    }
}
