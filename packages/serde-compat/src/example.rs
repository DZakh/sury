//! Ppx_Example_test.res

use crate::harness::{mirror, skip, Group};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, PartialEq, Debug)]
enum Rating {
    #[serde(rename = "G")]
    GeneralAudiences,
    #[serde(rename = "PG")]
    ParentalGuidanceSuggested,
    #[serde(rename = "PG13")]
    ParentalStronglyCautioned,
    #[serde(rename = "R")]
    Restricted,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct Film {
    #[serde(rename = "Id")]
    id: f64,
    #[serde(rename = "Title")]
    title: String,
    #[serde(rename = "Tags", default)]
    tags: Vec<String>,
    #[serde(rename = "Rating")]
    rating: Rating,
    #[serde(rename = "Age")]
    deprecated_age_restriction: Option<i32>,
}

/// `@s.null @s.default("Unknown") string`: `null` reads as the default.
#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
#[serde(from = "Option<String>", into = "String")]
struct NullOrUnknown(String);

impl From<Option<String>> for NullOrUnknown {
    fn from(value: Option<String>) -> Self {
        NullOrUnknown(value.unwrap_or_else(|| "Unknown".to_string()))
    }
}

impl From<NullOrUnknown> for String {
    fn from(value: NullOrUnknown) -> Self {
        value.0
    }
}

pub fn group() -> Group {
    Group {
        name: "Example",
        types: vec![
            mirror::<Rating>("rating")
                .values([
                    Rating::GeneralAudiences,
                    Rating::ParentalGuidanceSuggested,
                    Rating::ParentalStronglyCautioned,
                    Rating::Restricted,
                ])
                .rejects(&[r#""GeneralAudiences""#, r#""g""#, r#"1"#, r#"null"#])
                .diverges(
                    r#"{"G":null}"#,
                    "Serde also reads a unit variant from a map holding the variant's name over null; Sury reads only the string",
                )
                .into(),
            mirror::<Film>("film")
                .values([
                    Film {
                        id: 1.,
                        title: "My first film".into(),
                        tags: vec![],
                        rating: Rating::Restricted,
                        deprecated_age_restriction: Some(17),
                    },
                    Film {
                        id: 2.5,
                        title: "Sad & sed".into(),
                        tags: vec!["Loved".into()],
                        rating: Rating::ParentalStronglyCautioned,
                        deprecated_age_restriction: None,
                    },
                ])
                .accepts(&[
                    r#"{"Id":1,"Title":"My first film","Rating":"R","Age":17}"#,
                    r#"{"Id":1,"Title":"No age","Tags":["a"],"Rating":"G"}"#,
                    r#"{"Id":1,"Title":"Extra keys","Rating":"G","Age":null,"Director":"Somebody"}"#,
                ])
                .rejects(&[
                    r#"{"Title":"No id","Rating":"G"}"#,
                    r#"{"Id":1,"Title":"Null tags","Tags":null,"Rating":"G"}"#,
                    r#"{"Id":"1","Title":"String id","Rating":"G"}"#,
                    r#"{"Id":1,"Title":"Bad rating","Rating":"X"}"#,
                    r#"{"Id":1,"Title":"Float age","Rating":"G","Age":1.5}"#,
                ])
                .diverges(
                    r#"[1,"Film as an array",[],"G",null]"#,
                    "Serde also reads a struct from an array of its fields in order; Sury reads only an object",
                )
                .into(),
            mirror::<String>("matches")
                .values(["abc".to_string()])
                .rejects(&["1", "null"])
                .into(),
            skip("url", "S.url reads a JS URL instance, which JSON can't hold"),
            mirror::<String>("default")
                .values(["Hello".to_string()])
                .rejects(&["null", "1"])
                .into(),
            mirror::<Vec<String>>("defaultWith")
                .values([vec![], vec!["a".to_string()]])
                .rejects(&["null"])
                .into(),
            mirror::<Option<String>>("null")
                .values([Some("a".to_string()), None])
                .rejects(&["1"])
                .into(),
            mirror::<NullOrUnknown>("nullWithDefault")
                .values([NullOrUnknown("a".into())])
                .accepts(&["null"])
                .rejects(&["1"])
                .into(),
            mirror::<Option<String>>("nullable")
                .values([Some("a".to_string())])
                .rejects(&["1"])
                .diverges(
                    "null",
                    "@s.nullable writes None as undefined: a missing key in an object, but no JSON document on its own",
                )
                .into(),
            mirror::<NullOrUnknown>("nullableWithDefault")
                .values([NullOrUnknown("a".into())])
                .accepts(&["null"])
                .rejects(&["1"])
                .into(),
            mirror::<String>("deprecated")
                .values(["a".to_string()])
                .rejects(&["null"])
                .into(),
            mirror::<String>("describe")
                .values(["a".to_string()])
                .rejects(&["null"])
                .into(),
        ],
    }
}
