use crate::name::Name;
use std::collections::BTreeMap;

/// Well-known interned names used throughout the language.
/// Mirrors OCaml `Names` module.
pub struct Names;

impl Names {
    pub fn true_() -> Name { Name::new("true") }
    pub fn false_() -> Name { Name::new("false") }
    pub fn none() -> Name { Name::new("none") }
    pub fn symbol() -> Name { Name::new("symbol") }
    pub fn string() -> Name { Name::new("string") }
    pub fn int() -> Name { Name::new("int") }
    pub fn float() -> Name { Name::new("float") }
    pub fn char_() -> Name { Name::new("char") }
    pub fn list() -> Name { Name::new("list") }
    pub fn record() -> Name { Name::new("record") }
    pub fn function_() -> Name { Name::new("function") }
    pub fn module_() -> Name { Name::new("module") }
    pub fn to_string() -> Name { Name::new("to_string") }

    pub fn lt() -> Name { Name::new("<") }
    pub fn eq() -> Name { Name::new("=") }
    pub fn gt() -> Name { Name::new(">") }

    pub fn e_type() -> Name { Name::new("TypeError") }
    pub fn e_value() -> Name { Name::new("ValueError") }
    pub fn e_name() -> Name { Name::new("NameError") }
    pub fn e_field_nf() -> Name { Name::new("FieldNotFound") }
    pub fn e_index_nf() -> Name { Name::new("IndexNotFound") }
    pub fn e_value_nf() -> Name { Name::new("ValueNotFound") }
    pub fn e_eval() -> Name { Name::new("EvalError") }
}

pub type NameMap<V> = BTreeMap<Name, V>;
