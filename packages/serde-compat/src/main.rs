//! Writes `fixtures.json`: every sury-ppx test type, mirrored as the Rust type
//! Serde would read the same JSON into, with what Serde writes for sample
//! values and what it makes of hand-picked inputs. The e2e suite holds each ppx
//! schema to those answers in both directions.
//!
//! `cargo run` rewrites the file, `cargo run -- check` fails when it is stale or
//! when a `@schema` type in packages/e2e/src/ppx has no mirror here.

// The mirrors keep the field names of the ReScript types they mirror, `foo`
// included.
#![allow(clippy::disallowed_names)]

mod example;
mod format;
mod general;
mod harness;
mod object;
mod parametrized;
mod polyvariant;
mod primitive;
mod protobuf;
mod record;
mod recursive;
mod regression;
mod variant;

use harness::{Entry, Group};
use std::collections::BTreeSet;
use std::path::{Path, PathBuf};

fn groups() -> Vec<Group> {
    vec![
        example::group(),
        format::group(),
        general::group(),
        object::group(),
        parametrized::group(),
        polyvariant::group(),
        primitive::group(),
        protobuf::group(),
        record::group(),
        recursive::group(),
        regression::group(),
        variant::group(),
    ]
}

fn json_string(s: &str) -> String {
    serde_json::to_string(s).expect("a string always serializes")
}

/// One line per case, so a diff of the file reads as a diff of verdicts.
fn render(groups: &[Group]) -> String {
    let mut out = String::from("{\n");
    for (group_idx, group) in groups.iter().enumerate() {
        out += &format!("  {}: {{\n", json_string(group.name));
        for (type_idx, mirrored) in group.types.iter().enumerate() {
            out += &format!("    {}: ", json_string(mirrored.name));
            match &mirrored.entry {
                Entry::Skipped(reason) => {
                    out += &format!("{{\"skipped\": {}}}", json_string(reason))
                }
                Entry::Cases(cases) => {
                    out += "{\"cases\": [\n";
                    for (case_idx, case) in cases.iter().enumerate() {
                        out += &format!("      {{\"input\": {}", json_string(&case.input));
                        match &case.verdict {
                            Ok(output) => out += &format!(", \"output\": {}", json_string(output)),
                            Err(error) => out += &format!(", \"error\": {}", json_string(error)),
                        }
                        if let Some(gap) = case.gap {
                            out += &format!(", \"gap\": {}", json_string(gap));
                        }
                        out += if case_idx + 1 < cases.len() {
                            "},\n"
                        } else {
                            "}\n"
                        };
                    }
                    out += "    ]}";
                }
            }
            out += if type_idx + 1 < group.types.len() {
                ",\n"
            } else {
                "\n"
            };
        }
        out += if group_idx + 1 < groups.len() {
            "  },\n"
        } else {
            "  }\n"
        };
    }
    out += "}\n";
    out
}

/// `Ppx_regression_test.res` holds the "Regression" group.
fn group_of_file(file_name: &str) -> Option<String> {
    let name = file_name.strip_prefix("Ppx_")?.strip_suffix("_test.res")?;
    let mut chars = name.chars();
    let first = chars.next()?;
    Some(first.to_uppercase().chain(chars).collect())
}

/// The names of the `@schema` type declarations in a ReScript file. A line
/// carrying `@schema` arms the scan, the next `type` or `and` declaration it
/// meets is the one the attribute is on.
fn schema_types(source: &str) -> Vec<String> {
    let mut names = vec![];
    let mut armed = false;
    for line in source.lines() {
        let code = line.split("//").next().unwrap_or("");
        let mut rest = code;
        if let Some(at) = code.find("@schema") {
            let after = &code[at + "@schema".len()..];
            if !after.starts_with(|c: char| c.is_alphanumeric() || c == '_') {
                armed = true;
                rest = after;
            }
        }
        if !armed {
            continue;
        }
        let words: Vec<&str> = rest
            .split(|c: char| !(c.is_alphanumeric() || c == '_' || c == '\''))
            .filter(|word| !word.is_empty())
            .collect();
        if let Some(at) = words
            .iter()
            .position(|word| *word == "type" || *word == "and")
        {
            let name = words[at + 1..].iter().find(|word| **word != "rec");
            if let Some(name) = name {
                names.push(name.to_string());
                armed = false;
            }
        }
    }
    names
}

/// `Meta.t` and `wrapper<int>` mirror the types `t` and `wrapper`.
fn type_of_key(key: &str) -> &str {
    let base = key.split('<').next().unwrap_or(key);
    base.rsplit('.').next().unwrap_or(base)
}

fn coverage_problems(groups: &[Group], ppx_tests: &Path) -> Vec<String> {
    let mut problems = vec![];
    let mut files: Vec<PathBuf> = std::fs::read_dir(ppx_tests)
        .unwrap_or_else(|error| panic!("can't read {}: {error}", ppx_tests.display()))
        .map(|entry| entry.expect("a directory entry").path())
        .filter(|path| path.extension().is_some_and(|ext| ext == "res"))
        .collect();
    files.sort();
    let mut seen_groups = BTreeSet::new();
    for file in files {
        let file_name = file.file_name().unwrap().to_string_lossy().to_string();
        let Some(group_name) = group_of_file(&file_name) else {
            continue;
        };
        let source = std::fs::read_to_string(&file).expect("a readable test file");
        let declared: BTreeSet<String> = schema_types(&source).into_iter().collect();
        if declared.is_empty() {
            continue;
        }
        seen_groups.insert(group_name.clone());
        let Some(group) = groups.iter().find(|group| group.name == group_name) else {
            problems.push(format!("{file_name} has no group \"{group_name}\""));
            continue;
        };
        let mirrored: BTreeSet<String> = group
            .types
            .iter()
            .map(|t| type_of_key(t.name).to_string())
            .collect();
        for name in declared.difference(&mirrored) {
            problems.push(format!(
                "{file_name}: `{name}` has no Serde mirror (or skip) in group \"{group_name}\""
            ));
        }
        for name in mirrored.difference(&declared) {
            problems.push(format!(
                "group \"{group_name}\": `{name}` isn't a @schema type in {file_name}"
            ));
        }
    }
    for group in groups {
        if !seen_groups.contains(group.name) {
            problems.push(format!(
                "group \"{}\" has no Ppx_*_test.res file",
                group.name
            ));
        }
    }
    problems
}

fn main() {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let groups = groups();
    let problems = coverage_problems(&groups, &root.join("../e2e/src/ppx"));
    if !problems.is_empty() {
        for problem in &problems {
            eprintln!("{problem}");
        }
        std::process::exit(1);
    }
    let rendered = render(&groups);
    let fixtures = root.join("fixtures.json");
    match std::env::args().nth(1).as_deref() {
        None => {
            std::fs::write(&fixtures, rendered).expect("fixtures.json is writable");
            let count: usize = groups
                .iter()
                .flat_map(|group| &group.types)
                .map(|t| match &t.entry {
                    Entry::Cases(cases) => cases.len(),
                    Entry::Skipped(_) => 0,
                })
                .sum();
            println!("wrote {count} cases to {}", fixtures.display());
        }
        Some("check") => {
            let committed = std::fs::read_to_string(&fixtures).unwrap_or_default();
            if committed != rendered {
                eprintln!(
                    "{} is stale - run `cargo run` in packages/serde-compat",
                    fixtures.display()
                );
                std::process::exit(1);
            }
            println!("{} is up to date", fixtures.display());
        }
        Some(other) => {
            eprintln!("unknown command `{other}`; run with no argument to write, or `check`");
            std::process::exit(2);
        }
    }
}
