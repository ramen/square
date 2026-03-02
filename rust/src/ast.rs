use crate::name::Name;
use crate::names::NameMap;

/// The Abstract Syntax Tree for the Square language.
/// Mirrors the OCaml `AST` module.
#[derive(Debug, Clone)]
pub enum Ast {
    Name(Name),
    Symbol(Name),
    String(String),
    Int(i64),
    Float(f64),
    Char(char),
    List(Vec<Ast>),
    Record(NameMap<Ast>),
    Do(Vec<Ast>),
    Def(Name, Box<Ast>),
    Set(Name, Box<Ast>),
    Undef(Name),
    Let(Vec<(Name, Ast)>, Box<Ast>),
    If(Vec<Ast>),
    Fun(Pattern, Box<Ast>),
    App(Box<Ast>, Box<Ast>),
    Try(Box<Ast>, Vec<Catch>),
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Empty,
    Scalar(Name),
    List(Vec<Name>),
    Record(Vec<Name>),
}

#[derive(Debug, Clone)]
pub enum Catch {
    Val(Name, Ast),
    Pat(Pattern, Ast),
}

impl Ast {
    pub fn record(items: Vec<(Name, Ast)>) -> Ast {
        let mut map = NameMap::new();
        for (k, v) in items {
            map.insert(k, v);
        }
        Ast::Record(map)
    }
}
