//! Ppx_Recursive_test.res

#![allow(non_camel_case_types, non_snake_case)]

use crate::harness::{mirror, skip, Group};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct Node {
    id: String,
    children: Vec<Node>,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
enum Tree {
    Leaf(i32),
    Branch(Vec<Tree>),
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
enum PolyTree {
    leaf(i32),
    branch(Vec<PolyTree>),
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct Chain {
    next: Option<Box<Chain>>,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct Pair {
    both: Option<Box<(Pair, Pair)>>,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
enum Expr {
    Num(i32),
    Block(Vec<Stmt>),
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct Stmt {
    label: String,
    body: Expr,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct A {
    b: Option<Box<B>>,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct B {
    c: Option<Box<C>>,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct C {
    a: Option<Box<A>>,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct Root {
    mid: Option<Box<Mid>>,
    last: Option<Box<Last>>,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct Mid {
    tail: Option<Box<Last>>,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct Last {
    back: Option<Box<Root>>,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct G1 {
    toG2: Option<Box<G2>>,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct G2 {
    toG3: Option<Box<G3>>,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct G3 {
    toG4: Option<Box<G4>>,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct G4 {
    toG5: Option<Box<G5>>,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct G5 {
    toG1: Option<Box<G1>>,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct Leaf {
    name: String,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct Holder {
    leaf: Leaf,
    next: Option<Box<Holder>>,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
#[serde(deny_unknown_fields)]
struct StrictNode {
    id: String,
    kids: Vec<StrictNode>,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct Aliased {
    #[serde(rename = "Id")]
    id: String,
    #[serde(rename = "Kids")]
    kids: Vec<Aliased>,
}

fn node(id: &str, children: Vec<Node>) -> Node {
    Node {
        id: id.into(),
        children,
    }
}

fn chain(depth: usize) -> Chain {
    Chain {
        next: (depth > 0).then(|| Box::new(chain(depth - 1))),
    }
}

pub fn group() -> Group {
    Group {
        name: "Recursive",
        types: vec![
            mirror::<Node>("node")
                .values([node(
                    "1",
                    vec![node("2", vec![]), node("3", vec![node("4", vec![])])],
                )])
                .rejects(&[
                    r#"{"id":"1","children":[{"id":"2"}]}"#,
                    r#"{"id":"1","children":null}"#,
                ])
                .into(),
            mirror::<Tree>("tree")
                .values([
                    Tree::Branch(vec![Tree::Leaf(1), Tree::Branch(vec![Tree::Leaf(2)])]),
                    Tree::Leaf(0),
                ])
                .rejects(&[r#"{"Branch":[{"Leaf":"1"}]}"#, r#"{"Branch":[1]}"#])
                .into(),
            mirror::<PolyTree>("polyTree")
                .values([PolyTree::branch(vec![
                    PolyTree::leaf(1),
                    PolyTree::branch(vec![PolyTree::leaf(2)]),
                ])])
                .rejects(&[r#"{"branch":[{"NAME":"leaf","VAL":1}]}"#])
                .into(),
            mirror::<Chain>("chain")
                .values([chain(0), chain(3)])
                .accepts(&["{}", r#"{"next":{}}"#])
                .rejects(&[r#"{"next":{"next":1}}"#])
                .into(),
            mirror::<Pair>("pair")
                .values([
                    Pair { both: None },
                    Pair {
                        both: Some(Box::new((Pair { both: None }, Pair { both: None }))),
                    },
                ])
                .accepts(&[r#"{"both":[{},{"both":null}]}"#])
                .rejects(&[r#"{"both":[{}]}"#])
                .into(),
            mirror::<i32>("notActuallyRec")
                .values([1])
                .rejects(&["null"])
                .into(),
            mirror::<Expr>("expr")
                .values([Expr::Block(vec![
                    Stmt {
                        label: "a".into(),
                        body: Expr::Num(1),
                    },
                    Stmt {
                        label: "b".into(),
                        body: Expr::Block(vec![]),
                    },
                ])])
                .rejects(&[r#"{"Block":[{"label":"a"}]}"#])
                .into(),
            mirror::<Stmt>("stmt")
                .values([Stmt {
                    label: "x".into(),
                    body: Expr::Block(vec![Stmt {
                        label: "y".into(),
                        body: Expr::Num(2),
                    }]),
                }])
                .rejects(&[r#"{"label":"x","body":"Num"}"#])
                .into(),
            mirror::<A>("a")
                .values([A {
                    b: Some(Box::new(B {
                        c: Some(Box::new(C {
                            a: Some(Box::new(A { b: None })),
                        })),
                    })),
                }])
                .accepts(&[r#"{"b":{"c":{}}}"#])
                .into(),
            mirror::<B>("b")
                .values([B {
                    c: Some(Box::new(C { a: None })),
                }])
                .rejects(&[r#"{"c":{"a":{"b":1}}}"#])
                .into(),
            mirror::<C>("c").values([C { a: None }]).into(),
            mirror::<Root>("root")
                .values([Root {
                    mid: Some(Box::new(Mid {
                        tail: Some(Box::new(Last { back: None })),
                    })),
                    last: None,
                }])
                .accepts(&["{}"])
                .into(),
            mirror::<Mid>("mid")
                .values([Mid {
                    tail: Some(Box::new(Last {
                        back: Some(Box::new(Root {
                            mid: None,
                            last: None,
                        })),
                    })),
                }])
                .into(),
            mirror::<Last>("last")
                .values([Last {
                    back: Some(Box::new(Root {
                        mid: None,
                        last: Some(Box::new(Last { back: None })),
                    })),
                }])
                .into(),
            mirror::<G1>("g1")
                .values([G1 {
                    toG2: Some(Box::new(G2 {
                        toG3: Some(Box::new(G3 {
                            toG4: Some(Box::new(G4 {
                                toG5: Some(Box::new(G5 { toG1: None })),
                            })),
                        })),
                    })),
                }])
                .into(),
            mirror::<G2>("g2").values([G2 { toG3: None }]).into(),
            mirror::<G3>("g3")
                .values([G3 {
                    toG4: Some(Box::new(G4 {
                        toG5: Some(Box::new(G5 {
                            toG1: Some(Box::new(G1 { toG2: None })),
                        })),
                    })),
                }])
                .into(),
            mirror::<G4>("g4").values([G4 { toG5: None }]).into(),
            mirror::<G5>("g5").values([G5 { toG1: None }]).into(),
            mirror::<Leaf>("leaf")
                .values([Leaf { name: "a".into() }])
                .rejects(&["{}"])
                .into(),
            mirror::<Holder>("holder")
                .values([Holder {
                    leaf: Leaf { name: "a".into() },
                    next: Some(Box::new(Holder {
                        leaf: Leaf { name: "b".into() },
                        next: None,
                    })),
                }])
                .rejects(&[r#"{"next":null}"#])
                .into(),
            mirror::<StrictNode>("strictNode")
                .values([StrictNode {
                    id: "1".into(),
                    kids: vec![StrictNode {
                        id: "2".into(),
                        kids: vec![],
                    }],
                }])
                .rejects(&[r#"{"id":"1","kids":[{"id":"2","kids":[],"extra":true}]}"#])
                .into(),
            mirror::<Node>("matchesSelf")
                .values([node("1", vec![node("2", vec![])])])
                .into(),
            skip(
                "cut",
                "@s.matches(S.unknown->magic) takes whatever is there, a missing key included",
            ),
            mirror::<Aliased>("aliased")
                .values([Aliased {
                    id: "1".into(),
                    kids: vec![Aliased {
                        id: "2".into(),
                        kids: vec![],
                    }],
                }])
                .rejects(&[r#"{"id":"1","kids":[]}"#])
                .into(),
        ],
    }
}
