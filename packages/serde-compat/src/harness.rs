use serde::{de::DeserializeOwned, Serialize};
use std::fmt::Debug;

pub trait Mirror: Serialize + DeserializeOwned + PartialEq + Debug {}
impl<T: Serialize + DeserializeOwned + PartialEq + Debug> Mirror for T {}

/// One JSON text and what Serde makes of it. `Ok` holds what Serde writes back
/// for the value it read, `Err` the reason it refused the text.
pub struct Case {
    pub input: String,
    pub verdict: Result<String, String>,
    /// Set when Sury is known to answer differently. The e2e suite asserts it
    /// still does, so fixing one fails the suite until the case moves out of
    /// `diverges`.
    pub gap: Option<&'static str>,
}

pub enum Entry {
    Cases(Vec<Case>),
    Skipped(&'static str),
}

pub struct Mirrored {
    pub name: &'static str,
    pub entry: Entry,
}

pub struct Group {
    /// The `Ppx_<name>_test.res` file the types come from.
    pub name: &'static str,
    pub types: Vec<Mirrored>,
}

pub struct Builder<T> {
    name: &'static str,
    cases: Vec<Case>,
    _type: std::marker::PhantomData<T>,
}

/// The Serde type `T` mirrors the ppx schema called `name`.
pub fn mirror<T: Mirror>(name: &'static str) -> Builder<T> {
    Builder {
        name,
        cases: vec![],
        _type: std::marker::PhantomData,
    }
}

/// A ppx test type with no Serde counterpart, and why.
pub fn skip(name: &'static str, reason: &'static str) -> Mirrored {
    Mirrored {
        name,
        entry: Entry::Skipped(reason),
    }
}

fn read<T: Mirror>(name: &str, input: &str) -> Result<String, String> {
    serde_json::from_str::<T>(input)
        .map(|value| {
            serde_json::to_string(&value)
                .unwrap_or_else(|error| panic!("{name}: Serde can't write {value:?}: {error}"))
        })
        .map_err(|error| error.to_string())
}

impl<T: Mirror> Builder<T> {
    /// Values as Serde writes them. Each has to read back to itself, so the text
    /// is one Serde would also accept from Sury.
    pub fn values(mut self, values: impl IntoIterator<Item = T>) -> Self {
        for value in values {
            let json = serde_json::to_string(&value).unwrap_or_else(|error| {
                panic!("{}: Serde can't write {value:?}: {error}", self.name)
            });
            let back: T = serde_json::from_str(&json).unwrap_or_else(|error| {
                panic!("{}: Serde can't read its own {json}: {error}", self.name)
            });
            assert_eq!(
                back, value,
                "{}: {json} doesn't read back to the value it came from",
                self.name
            );
            self.cases.push(Case {
                input: json.clone(),
                verdict: Ok(json),
                gap: None,
            });
        }
        self
    }

    /// Texts Serde reads, though it never writes them.
    pub fn accepts(mut self, inputs: &[&str]) -> Self {
        for input in inputs {
            let verdict = read::<T>(self.name, input);
            if let Err(error) = &verdict {
                panic!(
                    "{}: expected Serde to accept {input}, it says: {error}",
                    self.name
                );
            }
            self.cases.push(Case {
                input: input.to_string(),
                verdict,
                gap: None,
            });
        }
        self
    }

    /// Texts Serde refuses.
    pub fn rejects(mut self, inputs: &[&str]) -> Self {
        for input in inputs {
            let verdict = read::<T>(self.name, input);
            if let Ok(output) = &verdict {
                panic!(
                    "{}: expected Serde to reject {input}, it reads it as {output}",
                    self.name
                );
            }
            self.cases.push(Case {
                input: input.to_string(),
                verdict,
                gap: None,
            });
        }
        self
    }

    /// Texts Sury answers differently than Serde does, with the reason.
    pub fn diverges(mut self, input: &str, gap: &'static str) -> Self {
        self.cases.push(Case {
            input: input.to_string(),
            verdict: read::<T>(self.name, input),
            gap: Some(gap),
        });
        self
    }
}

impl<T> From<Builder<T>> for Mirrored {
    fn from(builder: Builder<T>) -> Self {
        assert!(!builder.cases.is_empty(), "{} has no cases", builder.name);
        Mirrored {
            name: builder.name,
            entry: Entry::Cases(builder.cases),
        }
    }
}
