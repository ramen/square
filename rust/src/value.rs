use std::cell::RefCell;
use std::collections::BTreeMap;
use std::fmt;
use std::rc::Rc;

use crate::name::Name;
use crate::names::{NameMap, Names};

/// A native function type: takes a Value, returns a Result<Value, SquareError>.
pub type NativeFn = dyn Fn(Value) -> Result<Value, SquareError>;

/// Runtime value for the Square language.
/// Mirrors the OCaml `Value.t` type.
#[derive(Clone)]
pub enum Value {
    None,
    Symbol(Name),
    String(String),
    Int(i64),
    Float(f64),
    Char(char),
    List(Vec<Value>),
    Record(Name, NameMap<Value>),
    Function(Rc<NativeFn>),
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::None => write!(f, "None"),
            Value::Symbol(n) => write!(f, "Symbol(.{})", n),
            Value::String(s) => write!(f, "String({:?})", s),
            Value::Int(i) => write!(f, "Int({})", i),
            Value::Float(v) => write!(f, "Float({})", v),
            Value::Char(c) => write!(f, "Char({:?})", c),
            Value::List(l) => write!(f, "List({:?})", l),
            Value::Record(tag, _) => write!(f, "Record({})", tag),
            Value::Function(_) => write!(f, "Function"),
        }
    }
}

/// The error type, equivalent to OCaml's `Value.Error` exception.
#[derive(Debug, Clone)]
pub struct SquareError {
    pub value: Value,
}

impl fmt::Display for SquareError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value.to_display_string())
    }
}

impl std::error::Error for SquareError {}

impl SquareError {
    pub fn new(v: Value) -> Self {
        SquareError { value: v }
    }
}

/// The global generics dispatch table.
/// generics[func_name][tag] = function
pub struct Generics {
    table: BTreeMap<Name, BTreeMap<Name, Value>>,
}

impl Generics {
    pub fn new() -> Self {
        Generics {
            table: BTreeMap::new(),
        }
    }

    pub fn get(&self, func: Name, tag: Name) -> Option<&Value> {
        self.table.get(&func)?.get(&tag)
    }

    pub fn define(&mut self, func: Name, tag: Name, f: Value) {
        self.table.entry(func).or_default().insert(tag, f);
    }

    pub fn undefine(&mut self, func: Name, tag: Name) {
        if let Some(inner) = self.table.get_mut(&func) {
            inner.remove(&tag);
        }
    }

    pub fn to_value(&self) -> Value {
        let mut outer = NameMap::new();
        for (func, inner_map) in &self.table {
            let mut inner = NameMap::new();
            for (tag, val) in inner_map {
                inner.insert(*tag, val.clone());
            }
            outer.insert(*func, Value::Record(Names::record(), inner));
        }
        Value::Record(Names::record(), outer)
    }
}

thread_local! {
    pub static GENERICS: RefCell<Generics> = RefCell::new(Generics::new());
}

pub fn init_generics() {
    // Register default to_string for records
    let record_to_string = Value::Function(Rc::new(|v| {
        if let Value::Record(_, r) = &v {
            let parts: Vec<String> = r
                .iter()
                .map(|(k, v)| format!("{}: {}", k, v.to_display_string()))
                .collect();
            Ok(Value::String(format!("{{{}}}", parts.join(", "))))
        } else {
            Err(Value::error(Names::e_value(), "argument must be a record"))
        }
    }));
    GENERICS.with(|g| {
        g.borrow_mut()
            .define(Names::to_string(), Names::record(), record_to_string);
    });
}

impl Value {
    pub fn record(tag: Name, items: Vec<(Name, Value)>) -> Value {
        let mut map = NameMap::new();
        for (k, v) in items {
            map.insert(k, v);
        }
        Value::Record(tag, map)
    }

    pub fn error(code: Name, msg: &str) -> SquareError {
        SquareError::new(Value::record(
            Names::record(),
            vec![(code, Value::String(msg.to_string()))],
        ))
    }

    pub fn error_value(code: Name, data: Value) -> SquareError {
        SquareError::new(Value::record(Names::record(), vec![(code, data)]))
    }

    pub fn true_val() -> Value {
        Value::Symbol(Names::true_())
    }

    pub fn false_val() -> Value {
        Value::Symbol(Names::false_())
    }

    pub fn from_bool(b: bool) -> Value {
        if b {
            Value::true_val()
        } else {
            Value::false_val()
        }
    }

    pub fn to_bool(&self) -> bool {
        match self {
            Value::None => false,
            Value::String(s) if s.is_empty() => false,
            Value::Int(0) => false,
            Value::Float(f) if *f == 0.0 => false,
            Value::List(l) if l.is_empty() => false,
            Value::Symbol(n) if *n == Names::false_() => false,
            Value::Record(_, r) if r.is_empty() => false,
            _ => true,
        }
    }

    pub fn to_display_string(&self) -> String {
        match self {
            Value::None => "[]".to_string(),
            Value::Symbol(n) => format!(".{}", n),
            Value::String(s) => s.clone(),
            Value::Int(i) => i.to_string(),
            Value::Float(f) => {
                // Match OCaml's float formatting
                let s = format!("{}", f);
                if s.contains('.') || s.contains('e') || s.contains('E') {
                    s
                } else {
                    format!("{}.", f)
                }
            }
            Value::Char(c) => c.to_string(),
            Value::List(l) => {
                let parts: Vec<String> = l.iter().map(|v| v.to_display_string()).collect();
                format!("({})", parts.join(", "))
            }
            Value::Record(tag, _) => {
                // Clone function out of borrow scope to avoid re-entrant RefCell borrow
                let func = GENERICS.with(|g| {
                    let g = g.borrow();
                    g.get(Names::to_string(), *tag).cloned()
                });
                if let Some(Value::Function(f)) = func {
                    match f(self.clone()) {
                        Ok(Value::String(s)) => s,
                        _ => format!("<{}>", tag),
                    }
                } else {
                    format!("<{}>", tag)
                }
            }
            Value::Function(_) => "<function>".to_string(),
        }
    }

    /// Compare two values, returning an ordering.
    pub fn compare(&self, other: &Value) -> Result<std::cmp::Ordering, SquareError> {
        use std::cmp::Ordering;
        match (self, other) {
            (Value::None, Value::None) => Ok(Ordering::Equal),
            (Value::Symbol(a), Value::Symbol(b)) => Ok(a.cmp(b)),
            (Value::String(a), Value::String(b)) => Ok(a.cmp(b)),
            (Value::Int(a), Value::Int(b)) => Ok(a.cmp(b)),
            (Value::Int(a), Value::Float(b)) => Ok((*a as f64).partial_cmp(b).unwrap_or(Ordering::Equal)),
            (Value::Float(a), Value::Int(b)) => Ok(a.partial_cmp(&(*b as f64)).unwrap_or(Ordering::Equal)),
            (Value::Float(a), Value::Float(b)) => Ok(a.partial_cmp(b).unwrap_or(Ordering::Equal)),
            (Value::Char(a), Value::Char(b)) => Ok(a.cmp(b)),
            (Value::List(a), Value::List(b)) => {
                for (x, y) in a.iter().zip(b.iter()) {
                    let c = x.compare(y)?;
                    if c != Ordering::Equal {
                        return Ok(c);
                    }
                }
                Ok(a.len().cmp(&b.len()))
            }
            (Value::Record(t1, r1), Value::Record(t2, r2)) => {
                if *t1 == Names::record() && *t2 == Names::record() {
                    // Compare records field by field
                    let mut iter1 = r1.iter();
                    let mut iter2 = r2.iter();
                    loop {
                        match (iter1.next(), iter2.next()) {
                            (Option::None, Option::None) => return Ok(Ordering::Equal),
                            (Option::None, Some(_)) => return Ok(Ordering::Less),
                            (Some(_), Option::None) => return Ok(Ordering::Greater),
                            (Some((k1, v1)), Some((k2, v2))) => {
                                let kc = k1.cmp(k2);
                                if kc != Ordering::Equal {
                                    return Ok(kc);
                                }
                                let vc = v1.compare(v2)?;
                                if vc != Ordering::Equal {
                                    return Ok(vc);
                                }
                            }
                        }
                    }
                } else {
                    // Try generic compare
                    let func = GENERICS.with(|g| {
                        let g = g.borrow();
                        g.get(Name::new("compare"), *t1).cloned()
                    });
                    let result = if let Some(Value::Function(f)) = func {
                        Some(f(Value::List(vec![self.clone(), other.clone()])))
                    } else {
                        Option::None
                    };
                    if let Some(r) = result {
                        let v = r?;
                        match v {
                            Value::Symbol(n) if n == Names::lt() => Ok(Ordering::Less),
                            Value::Symbol(n) if n == Names::eq() => Ok(Ordering::Equal),
                            Value::Symbol(n) if n == Names::gt() => Ok(Ordering::Greater),
                            _ => Err(Value::error(Names::e_type(), "comparator must return .<, .=, or .>")),
                        }
                    } else {
                        // Fallback: compare tags
                        Ok(t1.cmp(t2))
                    }
                }
            }
            (Value::Function(_), _) | (_, Value::Function(_)) => {
                Err(Value::error(
                    Names::e_type(),
                    "functions cannot be compared",
                ))
            }
            // Different types: compare by type discriminant
            _ => {
                let a = type_order(self);
                let b = type_order(other);
                Ok(a.cmp(&b))
            }
        }
    }

    pub fn type_name(&self) -> Name {
        match self {
            Value::None => Names::none(),
            Value::Symbol(_) => Names::symbol(),
            Value::String(_) => Names::string(),
            Value::Int(_) => Names::int(),
            Value::Float(_) => Names::float(),
            Value::Char(_) => Names::char_(),
            Value::List(_) => Names::list(),
            Value::Record(tag, _) => *tag,
            Value::Function(_) => Names::function_(),
        }
    }
}

fn type_order(v: &Value) -> u8 {
    match v {
        Value::None => 0,
        Value::Symbol(_) => 1,
        Value::String(_) => 2,
        Value::Int(_) => 3,
        Value::Float(_) => 4,
        Value::Char(_) => 5,
        Value::List(_) => 6,
        Value::Record(_, _) => 7,
        Value::Function(_) => 8,
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::None, Value::None) => true,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Record(t1, r1), Value::Record(t2, r2)) => t1 == t2 && r1 == r2,
            _ => false,
        }
    }
}
