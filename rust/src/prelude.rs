use std::cell::RefCell;
use std::io::{self, BufRead, Write};
use std::rc::Rc;

use crate::env::{add_binding, Env};
use crate::eval::eval_string;
use crate::name::Name;
use crate::names::Names;
use crate::value::{SquareError, Value, GENERICS};

/// Identity comparison, analogous to OCaml's physical equality (==).
/// Scalars (None, Symbol, Int, Float, Char) are compared by value.
/// Rc-wrapped types (String, List, Record, Function) are compared by pointer identity.
fn value_is(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::None, Value::None) => true,
        (Value::Symbol(a), Value::Symbol(b)) => a == b,
        (Value::Int(a), Value::Int(b)) => a == b,
        (Value::Float(a), Value::Float(b)) => a == b,
        (Value::Char(a), Value::Char(b)) => a == b,
        (Value::String(a), Value::String(b)) => Rc::ptr_eq(a, b),
        (Value::List(a), Value::List(b)) => Rc::ptr_eq(a, b),
        (Value::Record(_, a), Value::Record(_, b)) => Rc::ptr_eq(a, b),
        (Value::Function(a), Value::Function(b)) => Rc::ptr_eq(a, b),
        _ => false,
    }
}

/// Helper: create a 1-arg native function.
fn func1(f: impl Fn(Value) -> Result<Value, SquareError> + 'static) -> Value {
    Value::Function(Rc::new(f))
}

/// Helper: create a curried 2-arg native function.
fn func2(
    f: impl Fn(Value, Value) -> Result<Value, SquareError> + 'static,
) -> Value {
    let f = Rc::new(f);
    Value::Function(Rc::new(move |a| {
        let f = f.clone();
        Ok(func1(move |b| f(a.clone(), b)))
    }))
}

/// Arithmetic op that works on Int and Float combinations.
fn arith_op(
    int_op: fn(i64, i64) -> i64,
    float_op: fn(f64, f64) -> f64,
) -> Value {
    func2(move |a, b| arith_op_apply(int_op, float_op, a, b))
}

fn arith_op_apply(
    int_op: fn(i64, i64) -> i64,
    float_op: fn(f64, f64) -> f64,
    a: Value,
    b: Value,
) -> Result<Value, SquareError> {
    match (&a, &b) {
        (Value::Int(a), Value::Int(b)) => Ok(Value::Int(int_op(*a, *b))),
        (Value::Int(a), Value::Float(b)) => Ok(Value::Float(float_op(*a as f64, *b))),
        (Value::Float(a), Value::Int(b)) => Ok(Value::Float(float_op(*a, *b as f64))),
        (Value::Float(a), Value::Float(b)) => Ok(Value::Float(float_op(*a, *b))),
        _ => Err(Value::error(Names::e_type(), "arguments must be numbers")),
    }
}

fn arith_unary(float_op: fn(f64) -> f64) -> Value {
    func1(move |v| match v {
        Value::Int(i) => Ok(Value::Float(float_op(i as f64))),
        Value::Float(f) => Ok(Value::Float(float_op(f))),
        _ => Err(Value::error(Names::e_type(), "argument must be a number")),
    })
}

fn arith_binary(float_op: fn(f64, f64) -> f64) -> Value {
    func2(move |a, b| {
        let (x, y) = match (&a, &b) {
            (Value::Int(a), Value::Int(b)) => (*a as f64, *b as f64),
            (Value::Float(a), Value::Int(b)) => (*a, *b as f64),
            (Value::Int(a), Value::Float(b)) => (*a as f64, *b),
            (Value::Float(a), Value::Float(b)) => (*a, *b),
            _ => return Err(Value::error(Names::e_type(), "arguments must be numbers")),
        };
        Ok(Value::Float(float_op(x, y)))
    })
}

/// Register all built-in functions in the global environment.
pub fn init_prelude(env: &Env) {
    let def = |name: &str, val: Value| {
        add_binding(env, name, val);
    };

    // -- throw --
    def("throw", func1(|x| Err(SquareError::new(x))));

    // -- hash --
    def(
        "hash",
        func1(|x| {
            use std::hash::{Hash, Hasher};
            let mut hasher = std::collections::hash_map::DefaultHasher::new();
            // Simple hash based on display string
            x.to_display_string().hash(&mut hasher);
            Ok(Value::Int(hasher.finish() as i64))
        }),
    );

    // -- is (identity comparison) --
    // Analogous to OCaml's physical equality (==). Uses Rc::ptr_eq for
    // String, List, Record, and Function; value equality for scalars.
    def(
        "is",
        func2(|x, y| {
            Ok(Value::from_bool(value_is(&x, &y)))
        }),
    );

    // -- not --
    def("not", func1(|x| Ok(Value::from_bool(!x.to_bool()))));

    // -- typeof --
    def(
        "typeof",
        func1(|x| Ok(Value::Symbol(x.type_name()))),
    );

    // -- bool --
    def("bool", func1(|x| Ok(Value::from_bool(x.to_bool()))));

    // -- string --
    def(
        "string",
        func1(|x| Ok(Value::string(x.to_display_string()))),
    );

    // -- symbol --
    def(
        "symbol",
        func1(|x| match x {
            Value::Symbol(_) => Ok(x),
            Value::String(s) => Ok(Value::Symbol(Name::new(&s))),
            _ => Err(Value::error(Names::e_type(), "argument must be a string or symbol")),
        }),
    );

    // -- symbol_name --
    def(
        "symbol_name",
        func1(|x| match x {
            Value::Symbol(n) => Ok(Value::string(n.to_string())),
            _ => Err(Value::error(Names::e_type(), "argument must be a symbol")),
        }),
    );

    // -- char --
    def(
        "char",
        func1(|x| match x {
            Value::Char(_) => Ok(x),
            Value::String(s) => {
                if s.len() == 1 {
                    Ok(Value::Char(s.chars().next().unwrap()))
                } else {
                    Err(Value::error(Names::e_value(), "string must have exactly 1 char"))
                }
            }
            Value::Int(i) => char::from_u32(i as u32)
                .map(Value::Char)
                .ok_or_else(|| Value::error(Names::e_value(), "invalid literal for 'char'")),
            _ => Err(Value::error(Names::e_type(), "invalid type for 'char'")),
        }),
    );

    // -- int --
    def(
        "int",
        func1(|x| match x {
            Value::Int(_) => Ok(x),
            Value::Float(f) => Ok(Value::Int(f as i64)),
            Value::Char(c) => Ok(Value::Int(c as i64)),
            Value::String(s) => s
                .parse::<i64>()
                .map(Value::Int)
                .map_err(|_| Value::error(Names::e_value(), "invalid literal for 'int'")),
            _ => Err(Value::error(Names::e_type(), "invalid type for 'int'")),
        }),
    );

    // -- float --
    def(
        "float",
        func1(|x| match x {
            Value::Float(_) => Ok(x),
            Value::Int(i) => Ok(Value::Float(i as f64)),
            Value::String(s) => s
                .parse::<f64>()
                .map(Value::Float)
                .map_err(|_| Value::error(Names::e_value(), "invalid literal for 'float'")),
            _ => Err(Value::error(Names::e_type(), "invalid type for 'float'")),
        }),
    );

    // -- print --
    def(
        "print",
        func1(|x| {
            match &x {
                Value::List(list) => {
                    for v in list.iter() {
                        print!("{}", v.to_display_string());
                    }
                }
                _ => {
                    print!("{}", x.to_display_string());
                }
            }
            let _ = io::stdout().flush();
            Ok(Value::None)
        }),
    );

    // -- cons --
    def(
        "cons",
        func2(|x, list| match list {
            Value::List(l) => {
                let mut v = (*l).clone();
                v.insert(0, x);
                Ok(Value::list(v))
            }
            _ => Err(Value::error(Names::e_type(), "second argument must be a list")),
        }),
    );

    // -- head --
    def(
        "head",
        func1(|x| match x {
            Value::List(l) if l.is_empty() => {
                Err(Value::error(Names::e_type(), "list must not be empty"))
            }
            Value::List(l) => Ok(l[0].clone()),
            _ => Err(Value::error(Names::e_type(), "argument must be a list")),
        }),
    );

    // -- tail --
    def(
        "tail",
        func1(|x| match x {
            Value::List(l) if l.is_empty() => {
                Err(Value::error(Names::e_type(), "list must not be empty"))
            }
            Value::List(l) => Ok(Value::list(l[1..].to_vec())),
            _ => Err(Value::error(Names::e_type(), "argument must be a list")),
        }),
    );

    // -- size --
    def(
        "size",
        func1(|x| match &x {
            Value::String(s) => Ok(Value::Int(s.len() as i64)),
            Value::List(l) => Ok(Value::Int(l.len() as i64)),
            Value::Record(_, r) => Ok(Value::Int(r.len() as i64)),
            _ => Err(Value::error(
                Names::e_type(),
                "argument must be a string, list, or record",
            )),
        }),
    );

    // -- fold --
    def(
        "fold",
        func1(|xs| {
            Ok(func1(move |f_val| {
                let xs = xs.clone();
                Ok(func1(move |init| {
                    let f = match &f_val {
                        Value::Function(f) => f.clone(),
                        _ => {
                            return Err(Value::error(
                                Names::e_type(),
                                "second argument must be a function",
                            ))
                        }
                    };
                    match &xs {
                        Value::String(s) => {
                            let mut acc = init;
                            for (i, c) in s.chars().enumerate() {
                                acc = f(Value::list(vec![
                                    Value::Int(i as i64),
                                    Value::Char(c),
                                    acc,
                                ]))?;
                            }
                            Ok(acc)
                        }
                        Value::List(l) => {
                            let mut acc = init;
                            for (i, v) in l.iter().enumerate() {
                                acc = f(Value::list(vec![
                                    Value::Int(i as i64),
                                    v.clone(),
                                    acc,
                                ]))?;
                            }
                            Ok(acc)
                        }
                        Value::Record(tag, r) => {
                            // Try generic fold first - clone out of borrow scope
                            let func = GENERICS.with(|g| {
                                let g = g.borrow();
                                g.get(Name::new("fold"), *tag).cloned()
                            });
                            if let Some(Value::Function(gf)) = func {
                                return gf(Value::list(vec![
                                    xs.clone(),
                                    Value::Function(f.clone()),
                                    init.clone(),
                                ]));
                            }
                            let mut acc = init;
                            for (k, v) in r.iter() {
                                acc = f(Value::list(vec![
                                    Value::Symbol(*k),
                                    v.clone(),
                                    acc,
                                ]))?;
                            }
                            Ok(acc)
                        }
                        _ => Err(Value::error(
                            Names::e_type(),
                            "first argument must be a string, list, or record",
                        )),
                    }
                }))
            }))
        }),
    );

    // -- in --
    def(
        "in",
        func2(|container, item| match &container {
            Value::String(s) => match &item {
                Value::Char(c) => Ok(Value::from_bool(s.contains(*c))),
                Value::String(s2) => Ok(Value::from_bool(s.contains(s2.as_str()))),
                _ => Err(Value::error(
                    Names::e_type(),
                    "second argument must be a char or string",
                )),
            },
            Value::List(l) => Ok(Value::from_bool(l.contains(&item))),
            Value::Record(_, r) => match &item {
                Value::Symbol(n) => Ok(Value::from_bool(r.contains_key(n))),
                _ => Err(Value::error(
                    Names::e_type(),
                    "second argument must be a symbol",
                )),
            },
            _ => Err(Value::error(
                Names::e_type(),
                "first argument must be a string, list, or record",
            )),
        }),
    );

    // -- index --
    def(
        "index",
        func2(|container, item| match &container {
            Value::String(s) => match &item {
                Value::Char(c) => s
                    .find(*c)
                    .map(|i| Value::Int(i as i64))
                    .ok_or_else(|| Value::error_value(Names::e_value_nf(), item)),
                Value::String(s2) => s
                    .find(s2.as_str())
                    .map(|i| Value::Int(i as i64))
                    .ok_or_else(|| Value::error_value(Names::e_value_nf(), item)),
                _ => Err(Value::error(
                    Names::e_type(),
                    "second argument must be a char or string",
                )),
            },
            Value::List(l) => l
                .iter()
                .position(|v| *v == item)
                .map(|i| Value::Int(i as i64))
                .ok_or_else(|| Value::error_value(Names::e_value_nf(), item)),
            Value::Record(_, r) => {
                for (k, v) in r.iter() {
                    if *v == item {
                        return Ok(Value::Symbol(*k));
                    }
                }
                Err(Value::error_value(Names::e_value_nf(), item))
            }
            _ => Err(Value::error(
                Names::e_type(),
                "first argument must be a string, list, or record",
            )),
        }),
    );

    // -- sort_with --
    def(
        "sort_with",
        func2(|cmp_fn, list| match (&cmp_fn, list) {
            (Value::Function(f), Value::List(l)) => {
                let f = f.clone();
                let mut v = (*l).clone();
                let mut err: Option<SquareError> = None;
                v.sort_by(|a, b| {
                    if err.is_some() {
                        return std::cmp::Ordering::Equal;
                    }
                    match f(Value::list(vec![a.clone(), b.clone()])) {
                        Ok(Value::Symbol(n)) if n == Names::lt() => std::cmp::Ordering::Less,
                        Ok(Value::Symbol(n)) if n == Names::eq() => std::cmp::Ordering::Equal,
                        Ok(Value::Symbol(n)) if n == Names::gt() => std::cmp::Ordering::Greater,
                        Ok(_) => {
                            err = Some(Value::error(
                                Names::e_type(),
                                "comparator must return .<, .=, or .>",
                            ));
                            std::cmp::Ordering::Equal
                        }
                        Err(e) => {
                            err = Some(e);
                            std::cmp::Ordering::Equal
                        }
                    }
                });
                if let Some(e) = err {
                    return Err(e);
                }
                Ok(Value::list(v))
            }
            (Value::Function(_), _) => Err(Value::error(
                Names::e_type(),
                "second argument must be a list",
            )),
            _ => Err(Value::error(
                Names::e_type(),
                "first argument must be a function",
            )),
        }),
    );

    // -- globals --
    {
        let env_clone = env.clone();
        def(
            "globals",
            func1(move |x| match x {
                Value::None => Ok(env_clone.borrow().to_value()),
                _ => Err(Value::error(Names::e_type(), "function takes no arguments")),
            }),
        );
    }

    // -- generics --
    def(
        "generics",
        func1(|x| match x {
            Value::None => Ok(GENERICS.with(|g| g.borrow().to_value())),
            _ => Err(Value::error(Names::e_type(), "function takes no arguments")),
        }),
    );

    // -- def_generic --
    def(
        "def_generic",
        func1(|func_sym| match func_sym {
            Value::Symbol(func) => Ok(func1(move |tag_sym| match tag_sym {
                Value::Symbol(tag) => Ok(func1(move |f| match &f {
                    Value::Function(_) => {
                        GENERICS.with(|g| g.borrow_mut().define(func, tag, f.clone()));
                        Ok(Value::None)
                    }
                    _ => Err(Value::error(
                        Names::e_type(),
                        "third argument must be a function",
                    )),
                })),
                _ => Err(Value::error(
                    Names::e_type(),
                    "second argument must be a symbol",
                )),
            })),
            _ => Err(Value::error(
                Names::e_type(),
                "first argument must be a symbol",
            )),
        }),
    );

    // -- undef_generic --
    def(
        "undef_generic",
        func2(|func_sym, tag_sym| match (&func_sym, &tag_sym) {
            (Value::Symbol(func), Value::Symbol(tag)) => {
                GENERICS.with(|g| g.borrow_mut().undefine(*func, *tag));
                Ok(Value::None)
            }
            (Value::Symbol(_), _) => Err(Value::error(
                Names::e_type(),
                "second argument must be a symbol",
            )),
            _ => Err(Value::error(
                Names::e_type(),
                "first argument must be a symbol",
            )),
        }),
    );

    // -- tag --
    def(
        "tag",
        func2(|sym, rec| match (&sym, rec) {
            (Value::Symbol(n), Value::Record(_, r)) => Ok(Value::Record(*n, r)),
            (Value::Symbol(_), _) => Err(Value::error(
                Names::e_type(),
                "second argument must be a record",
            )),
            _ => Err(Value::error(
                Names::e_type(),
                "first argument must be a symbol",
            )),
        }),
    );

    // -- compare --
    def(
        "compare",
        func1(|x| match x {
            Value::List(l) if l.len() == 2 => {
                let ordering = l[0].compare(&l[1])?;
                match ordering {
                    std::cmp::Ordering::Less => Ok(Value::Symbol(Names::lt())),
                    std::cmp::Ordering::Equal => Ok(Value::Symbol(Names::eq())),
                    std::cmp::Ordering::Greater => Ok(Value::Symbol(Names::gt())),
                }
            }
            Value::List(_) => Err(Value::error(
                Names::e_type(),
                "function requires a list of size 2",
            )),
            _ => Err(Value::error(Names::e_type(), "argument must be a list")),
        }),
    );

    // -- Arithmetic ops --
    def("+", arith_op(|a, b| a + b, |a, b| a + b));
    def("-", arith_op(|a, b| a - b, |a, b| a - b));
    def("*", arith_op(|a, b| a * b, |a, b| a * b));
    def(
        "/",
        arith_op(
            |a, b| {
                if b == 0 {
                    0 // or error, but OCaml would crash
                } else {
                    a / b
                }
            },
            |a, b| a / b,
        ),
    );
    def(
        "%",
        arith_op(
            |a, b| {
                if b == 0 {
                    0
                } else {
                    a % b
                }
            },
            |a, b| a % b,
        ),
    );
    def(
        "**",
        arith_op(
            |a, b| (a as f64).powf(b as f64) as i64,
            |a, b| a.powf(b),
        ),
    );

    // -- sum --
    def(
        "sum",
        func1(|x| match x {
            Value::List(l) => {
                let mut acc = Value::Int(0);
                for v in l.iter() {
                    acc = arith_op_apply(|a, b| a + b, |a, b| a + b, acc, v.clone())?;
                }
                Ok(acc)
            }
            _ => Err(Value::error(Names::e_type(), "argument must be a list")),
        }),
    );

    // -- product --
    def(
        "product",
        func1(|x| match x {
            Value::List(l) => {
                let mut acc = Value::Int(1);
                for v in l.iter() {
                    acc = arith_op_apply(|a, b| a * b, |a, b| a * b, acc, v.clone())?;
                }
                Ok(acc)
            }
            _ => Err(Value::error(Names::e_type(), "argument must be a list")),
        }),
    );

    // -- add --
    def(
        "add",
        func1(|container| match container {
            Value::List(l) => Ok(func1(move |x| {
                let mut new_list = (*l).clone();
                new_list.push(x);
                Ok(Value::list(new_list))
            })),
            Value::Record(tag, oldvals) => Ok(func1(move |sym| match sym {
                Value::Symbol(n) => {
                    let tag = tag;
                    let oldvals = oldvals.clone();
                    Ok(func1(move |x| {
                        let mut new_map = (*oldvals).clone();
                        new_map.insert(n, x);
                        Ok(Value::Record(tag, Rc::new(new_map)))
                    }))
                }
                _ => Err(Value::error(
                    Names::e_type(),
                    "second argument must be a symbol",
                )),
            })),
            _ => Err(Value::error(
                Names::e_type(),
                "first argument must be a list or record",
            )),
        }),
    );

    // -- append --
    def(
        "append",
        func1(|x| match x {
            Value::List(l) => {
                let mut result = Vec::new();
                for item in l.iter() {
                    match item {
                        Value::List(sub) => result.extend(sub.iter().cloned()),
                        _ => {
                            return Err(Value::error(
                                Names::e_type(),
                                "contents must be lists",
                            ))
                        }
                    }
                }
                Ok(Value::list(result))
            }
            _ => Err(Value::error(Names::e_type(), "argument must be a list")),
        }),
    );

    // -- join --
    def(
        "join",
        func2(|sep, list| {
            let sep_str = match &sep {
                Value::String(s) => (**s).clone(),
                Value::Char(c) => c.to_string(),
                _ => {
                    return Err(Value::error(
                        Names::e_type(),
                        "first argument must be a string or character",
                    ))
                }
            };
            match list {
                Value::List(l) => {
                    let parts: Vec<String> = l.iter().map(|v| v.to_display_string()).collect();
                    Ok(Value::string(parts.join(&sep_str)))
                }
                _ => Err(Value::error(
                    Names::e_type(),
                    "second argument must be a list",
                )),
            }
        }),
    );

    // -- split --
    def(
        "split",
        func2(|delim, s| {
            let delim_str = match &delim {
                Value::String(s) => (**s).clone(),
                Value::Char(c) => c.to_string(),
                _ => {
                    return Err(Value::error(
                        Names::e_type(),
                        "first argument must be a string or character",
                    ))
                }
            };
            match s {
                Value::String(s) => {
                    let parts: Vec<Value> = s
                        .split(&delim_str)
                        .map(|p| Value::string(p.to_string()))
                        .collect();
                    Ok(Value::list(parts))
                }
                _ => Err(Value::error(
                    Names::e_type(),
                    "second argument must be a string",
                )),
            }
        }),
    );

    // -- slice --
    def(
        "slice",
        func2(|args, target| {
            let (start, stop) = match &args {
                Value::List(l) => match l.as_slice() {
                    [Value::Int(s)] => (*s, None),
                    [Value::Int(s), Value::Int(e)] => (*s, Some(*e)),
                    _ => {
                        return Err(Value::error(
                            Names::e_type(),
                            "function requires a list of size 1-2 integers",
                        ))
                    }
                },
                _ => {
                    return Err(Value::error(
                        Names::e_type(),
                        "first argument must be a list",
                    ))
                }
            };
            match target {
                Value::String(s) => {
                    let len = s.len() as i64;
                    let start = if start >= 0 {
                        start
                    } else {
                        (len + start).max(0)
                    } as usize;
                    let end = match stop {
                        None => len as usize,
                        Some(e) if e >= 0 => e as usize,
                        Some(e) => (len + e) as usize,
                    };
                    let end = end.min(s.len());
                    let start = start.min(end);
                    Ok(Value::string(s[start..end].to_string()))
                }
                Value::List(l) => {
                    let len = l.len() as i64;
                    let start = if start >= 0 {
                        start
                    } else {
                        len + start
                    } as usize;
                    let end = match stop {
                        None => len as usize,
                        Some(e) if e >= 0 => e as usize,
                        Some(e) => (len + e) as usize,
                    };
                    let end = end.min(l.len());
                    let start = start.min(end);
                    Ok(Value::list(l[start..end].to_vec()))
                }
                _ => Err(Value::error(
                    Names::e_type(),
                    "second argument must be a string or list",
                )),
            }
        }),
    );

    // -- lowercase --
    def(
        "lowercase",
        func1(|x| match x {
            Value::String(s) => Ok(Value::string(s.to_lowercase())),
            Value::Char(c) => Ok(Value::Char(
                c.to_lowercase().next().unwrap_or(c),
            )),
            _ => Err(Value::error(
                Names::e_type(),
                "argument must be a string or character",
            )),
        }),
    );

    // -- uppercase --
    def(
        "uppercase",
        func1(|x| match x {
            Value::String(s) => Ok(Value::string(s.to_uppercase())),
            Value::Char(c) => Ok(Value::Char(
                c.to_uppercase().next().unwrap_or(c),
            )),
            _ => Err(Value::error(
                Names::e_type(),
                "argument must be a string or character",
            )),
        }),
    );

    // -- starts_with --
    def(
        "starts_with",
        func2(|prefix, s| match (&prefix, &s) {
            (Value::String(p), Value::String(s)) => {
                Ok(Value::from_bool(s.starts_with(p.as_str())))
            }
            (Value::String(_), _) => Err(Value::error(
                Names::e_type(),
                "second argument must be a string",
            )),
            _ => Err(Value::error(
                Names::e_type(),
                "first argument must be a string",
            )),
        }),
    );

    // -- ends_with --
    def(
        "ends_with",
        func2(|suffix, s| match (&suffix, &s) {
            (Value::String(e), Value::String(s)) => {
                Ok(Value::from_bool(s.ends_with(e.as_str())))
            }
            (Value::String(_), _) => Err(Value::error(
                Names::e_type(),
                "second argument must be a string",
            )),
            _ => Err(Value::error(
                Names::e_type(),
                "first argument must be a string",
            )),
        }),
    );

    // -- update --
    def(
        "update",
        func2(|a, b| match (a, b) {
            (Value::Record(tag, oldvals), Value::Record(_, newvals)) => {
                let mut merged = (*oldvals).clone();
                for (k, v) in newvals.iter() {
                    merged.insert(*k, v.clone());
                }
                Ok(Value::Record(tag, Rc::new(merged)))
            }
            (Value::Record(_, _), _) => Err(Value::error(
                Names::e_type(),
                "second argument must be a record",
            )),
            _ => Err(Value::error(
                Names::e_type(),
                "first argument must be a record",
            )),
        }),
    );

    // -- remove --
    def(
        "remove",
        func2(|rec, sym| match (rec, &sym) {
            (Value::Record(tag, r), Value::Symbol(n)) => {
                let mut new_r = (*r).clone();
                new_r.remove(n);
                Ok(Value::Record(tag, Rc::new(new_r)))
            }
            (Value::Record(_, _), _) => Err(Value::error(
                Names::e_type(),
                "second argument must be a symbol",
            )),
            _ => Err(Value::error(
                Names::e_type(),
                "first argument must be a record",
            )),
        }),
    );

    // -- forever (loop) --
    def(
        "forever",
        func1(|f| match &f {
            Value::Function(func) => loop {
                func(Value::None)?;
            },
            _ => Err(Value::error(Names::e_type(), "argument must be a function")),
        }),
    );

    // -- read_line --
    def(
        "read_line",
        func1(|_| {
            let mut line = String::new();
            io::stdin()
                .lock()
                .read_line(&mut line)
                .map_err(|e| Value::error(Name::new("IOError"), &e.to_string()))?;
            // Remove trailing newline
            if line.ends_with('\n') {
                line.pop();
                if line.ends_with('\r') {
                    line.pop();
                }
            }
            Ok(Value::string(line))
        }),
    );

    // -- File module --
    def("File", {
        Value::record(
            Names::module_(),
            vec![
                (
                    Name::new("open_in"),
                    func1(|x| match x {
                        Value::String(filename) => {
                            let content = std::fs::read_to_string(filename.as_ref()).map_err(|e| {
                                Value::error(Name::new("IOError"), &e.to_string())
                            })?;
                            // Return a simple file record with read method
                            Ok(Value::record(
                                Name::new("file"),
                                vec![
                                    (
                                        Name::new("read"),
                                        func1(move |_| Ok(Value::string(content.clone()))),
                                    ),
                                    (Name::new("close"), func1(|_| Ok(Value::None))),
                                ],
                            ))
                        }
                        _ => Err(Value::error(Names::e_type(), "argument must be a string")),
                    }),
                ),
                (
                    Name::new("open_out"),
                    func1(|x| match x {
                        Value::String(filename) => {
                            let file = Rc::new(RefCell::new(
                                std::fs::File::create(filename.as_ref()).map_err(|e| {
                                    Value::error(Name::new("IOError"), &e.to_string())
                                })?,
                            ));
                            let file2 = file.clone();
                            Ok(Value::record(
                                Name::new("file"),
                                vec![
                                    (
                                        Name::new("write"),
                                        func1(move |x| {
                                            let s = x.to_display_string();
                                            file.borrow_mut()
                                                .write_all(s.as_bytes())
                                                .map_err(|e| {
                                                    Value::error(
                                                        Name::new("IOError"),
                                                        &e.to_string(),
                                                    )
                                                })?;
                                            Ok(Value::None)
                                        }),
                                    ),
                                    (
                                        Name::new("close"),
                                        func1(move |_| {
                                            file2.borrow_mut().flush().map_err(|e| {
                                                Value::error(Name::new("IOError"), &e.to_string())
                                            })?;
                                            Ok(Value::None)
                                        }),
                                    ),
                                ],
                            ))
                        }
                        _ => Err(Value::error(Names::e_type(), "argument must be a string")),
                    }),
                ),
            ],
        )
    });

    // -- Math module --
    def("Math", {
        Value::record(
            Names::module_(),
            vec![
                (Name::new("acos"), arith_unary(f64::acos)),
                (Name::new("asin"), arith_unary(f64::asin)),
                (Name::new("atan"), arith_unary(f64::atan)),
                (Name::new("atan2"), arith_binary(f64::atan2)),
                (Name::new("ceil"), arith_unary(f64::ceil)),
                (Name::new("cos"), arith_unary(f64::cos)),
                (Name::new("cosh"), arith_unary(f64::cosh)),
                (Name::new("e"), Value::Float(std::f64::consts::E)),
                (Name::new("exp"), arith_unary(f64::exp)),
                (Name::new("floor"), arith_unary(f64::floor)),
                (Name::new("log"), arith_unary(f64::ln)),
                (Name::new("log10"), arith_unary(f64::log10)),
                (
                    Name::new("Phi"),
                    Value::Float((5.0_f64.sqrt() + 1.0) / 2.0),
                ),
                (
                    Name::new("phi"),
                    Value::Float((5.0_f64.sqrt() - 1.0) / 2.0),
                ),
                (Name::new("pi"), Value::Float(std::f64::consts::PI)),
                (Name::new("sin"), arith_unary(f64::sin)),
                (Name::new("sinh"), arith_unary(f64::sinh)),
                (Name::new("sqrt"), arith_unary(f64::sqrt)),
                (Name::new("tan"), arith_unary(f64::tan)),
                (Name::new("tanh"), arith_unary(f64::tanh)),
            ],
        )
    });

    // -- OS module --
    def("OS", {
        Value::record(
            Names::module_(),
            vec![
                (
                    Name::new("args"),
                    Value::list(
                        std::env::args()
                            .map(|a| Value::string(a))
                            .collect(),
                    ),
                ),
                (
                    Name::new("getcwd"),
                    func1(|_| {
                        Ok(Value::string(
                            std::env::current_dir()
                                .map(|p| p.to_string_lossy().to_string())
                                .unwrap_or_default(),
                        ))
                    }),
                ),
                (
                    Name::new("chdir"),
                    func1(|x| match x {
                        Value::String(s) => {
                            std::env::set_current_dir(s.as_ref()).map_err(|e| {
                                Value::error(Name::new("OSError"), &e.to_string())
                            })?;
                            Ok(Value::None)
                        }
                        _ => Err(Value::error(Names::e_type(), "argument must be a string")),
                    }),
                ),
                (
                    Name::new("listdir"),
                    func1(|x| match x {
                        Value::String(s) => {
                            let entries: Result<Vec<Value>, _> = std::fs::read_dir(s.as_ref())
                                .map_err(|e| {
                                    Value::error(Name::new("OSError"), &e.to_string())
                                })?
                                .map(|entry| {
                                    entry
                                        .map(|e| {
                                            Value::string(
                                                e.file_name().to_string_lossy().to_string(),
                                            )
                                        })
                                        .map_err(|e| {
                                            Value::error(Name::new("OSError"), &e.to_string())
                                        })
                                })
                                .collect();
                            Ok(Value::list(entries?))
                        }
                        _ => Err(Value::error(Names::e_type(), "argument must be a string")),
                    }),
                ),
                (
                    Name::new("system"),
                    func1(|x| match x {
                        Value::String(cmd) => {
                            let status = if cfg!(target_os = "windows") {
                                std::process::Command::new("cmd")
                                    .args(["/C", cmd.as_ref()])
                                    .status()
                            } else {
                                std::process::Command::new("sh")
                                    .args(["-c", cmd.as_ref()])
                                    .status()
                            };
                            match status {
                                Ok(s) => Ok(Value::Int(s.code().unwrap_or(-1) as i64)),
                                Err(e) => Err(Value::error(
                                    Name::new("OSError"),
                                    &e.to_string(),
                                )),
                            }
                        }
                        _ => Err(Value::error(Names::e_type(), "argument must be a string")),
                    }),
                ),
            ],
        )
    });

    // -- RE module (regex) --
    def("RE", {
        Value::record(
            Names::module_(),
            vec![
                (
                    Name::new("quote"),
                    func1(|x| {
                        Ok(Value::string(regex::escape(&x.to_display_string())))
                    }),
                ),
                (
                    Name::new("compile"),
                    func1(|x| match x {
                        Value::String(re_str) => {
                            let re = regex::Regex::new(&re_str).map_err(|e| {
                                Value::error(Names::e_value(), &e.to_string())
                            })?;
                            let re2 = re.clone();
                            Ok(Value::record(
                                Name::new("RE"),
                                vec![
                                    (
                                        Name::new("search"),
                                        func1(move |s| match s {
                                            Value::String(s) => {
                                                if let Some(m) = re.find(&s) {
                                                    let mut groups = Vec::new();
                                                    if let Some(caps) = re.captures(&s) {
                                                        for i in 0..caps.len() {
                                                            if let Some(g) = caps.get(i) {
                                                                groups.push(Value::string(
                                                                    g.as_str().to_string(),
                                                                ));
                                                            }
                                                        }
                                                    }
                                                    Ok(Value::record(
                                                        Names::record(),
                                                        vec![
                                                            (
                                                                Name::new("start"),
                                                                Value::Int(m.start() as i64),
                                                            ),
                                                            (
                                                                Name::new("end"),
                                                                Value::Int(m.end() as i64),
                                                            ),
                                                            (
                                                                Name::new("groups"),
                                                                Value::list(groups),
                                                            ),
                                                        ],
                                                    ))
                                                } else {
                                                    Ok(Value::None)
                                                }
                                            }
                                            _ => Err(Value::error(
                                                Names::e_type(),
                                                "argument must be a string",
                                            )),
                                        }),
                                    ),
                                    (
                                        Name::new("replace"),
                                        func2(move |repl, s| match (&repl, &s) {
                                            (Value::String(repl), Value::String(s)) => Ok(
                                                Value::string(
                                                    re2.replace_all(s.as_str(), repl.as_str()).to_string(),
                                                ),
                                            ),
                                            (Value::String(_), _) => Err(Value::error(
                                                Names::e_type(),
                                                "second argument must be a string",
                                            )),
                                            _ => Err(Value::error(
                                                Names::e_type(),
                                                "first argument must be a string",
                                            )),
                                        }),
                                    ),
                                ],
                            ))
                        }
                        _ => Err(Value::error(
                            Names::e_type(),
                            "argument must be a string",
                        )),
                    }),
                ),
            ],
        )
    });

    // -- Time module --
    def("Time", {
        Value::record(
            Names::module_(),
            vec![(
                Name::new("time"),
                func1(|_| {
                    let now = std::time::SystemTime::now()
                        .duration_since(std::time::UNIX_EPOCH)
                        .unwrap_or_default();
                    Ok(Value::Float(now.as_secs_f64()))
                }),
            )],
        )
    });

    // -- load (will be set up by main) --
    // Placeholder - the real load is set in main.rs
}

/// Initialize the Square prelude source code (the part written in Square itself).
pub fn init_prelude_source(env: &Env) {
    let prelude_source = r#"

def generic fun sym ->
  fun x ->
    let {g: try generics [] sym [typeof x]
            catch {FieldNotFound} ->
              throw {ValueError: join "" ([symbol_name sym],
                                            " is not defined for ",
                                            [symbol_name [typeof x]])}}
    g x;

def  = fun a -> fun b -> is .= [compare (a, b)];
def <> fun a -> fun b -> not [is .= [compare (a, b)]];
def <  fun a -> fun b -> is .< [compare (a, b)];
def >  fun a -> fun b -> is .> [compare (a, b)];
def <= fun a -> fun b -> not [is .> [compare (a, b)]];
def >= fun a -> fun b -> not [is .< [compare (a, b)]];

def abs fun x -> if or (< x 0, = x -0.0) then - 0 x else x;

def compose fun fs ->
  letrec {
    loop: fun (f, fs) ->
      if fs
      then [let {g: head fs, fs: tail fs}
            loop (fun x -> f [g x], fs)]
      else f
  }
  loop (head fs, tail fs);

def default fun d -> fun v -> fun x ->
  try v x
  catch {IndexNotFound} -> d
  catch {FieldNotFound} -> d;

def each fun xs -> fun f ->
  fold xs [fun (_, v, _) -> [f v; []]] [];

def eachi fun xs -> fun f ->
  fold xs [fun (i, v, _) -> [f (i, v); []]] [];

def empty fun xs -> = 0 [size xs];

def filter fun xs -> fun f ->
  reverse [fold xs [fun (_, v, a) -> if f v then cons v a else a] ()];

def filteri fun xs -> fun f ->
  reverse [fold xs [fun (i, v, a) -> if f (i, v) then cons (i, v) a else a] ()];

def flip fun f -> fun x -> fun y -> f y x;

def id fun x -> x;

def indexes fun xs -> mapi xs [fun (i, _) -> i];
def fields indexes;

def isa fun type -> fun value -> is type [typeof value];

def map fun xs -> fun f ->
  reverse [fold xs [fun (_, v, a) -> cons [f v] a] ()];

def mapi fun xs -> fun f ->
  reverse [fold xs [fun (i, v, a) -> cons [f (i, v)] a] ()];

def min [];
def max [];
let {
  optimum: fun f -> fun xs ->
    letrec {
      loop: fun (m, xs) ->
        if xs
        then [let {x: head xs, xs: tail xs}
              if f x m then loop (x, xs) else loop (m, xs)]
        else m
    }
    loop (head xs, xs)
}
do [
  := min optimum <;
  := max optimum >;
];

def module fun r -> tag .module r;

def neg fun x -> - 0 x;

def pairs fun x -> mapi x [fun p -> p];

def println fun x -> [print x; print "\n"];

def range fun args ->
  let {
    _: if not [isa .record args]
       then throw {TypeError: "function requires a record argument"},
    _: if not [in args .start]
       then [
         if not [in args .stop]
         then throw {ValueError: "either start or stop must be provided"}
       ],
  }
  let {
    start: if in args .start then args.start else 0,
    stop:  if in args .stop  then args.stop  else 0,
    step:  if in args .step  then args.step  else 1,
  }
  letrec {
    _: if = step 0 then throw {ValueError: "step must not be zero"},
    in_range: if < step 0 then > else <,
    result: (),
    loop: fun [] ->
      if in_range current stop
      then [let {v: current} [:= current [+ current step]; !add result v; loop []]]
      else result,
    current: start,
  }
  loop [];

def ref fun value ->
  tag .ref {value: fun [] -> value,
            set_value: fun x -> := value x};

def @ fun x -> x.value[];
def @= fun x -> fun y -> x.set_value y;

def replace fun from -> fun to -> fun s ->
  join [string to] [split [RE.quote from] s];

def reverse generic .reverse;

def sort sort_with compare;

def values fun xs ->
  if is .list [typeof xs]
  then xs
  else map xs id;

def_generic .reverse .string fun s ->
  join "" [fold s [fun (_, v, a) -> cons v a] ()];
def_generic .reverse .list fun l ->
  fold l [fun (_, v, a) -> cons v a] ();

def_generic .to_string .ref fun ref ->
  join "" ("[ref ", string [ref.value []], "]");

"#;

    match eval_string(env, prelude_source) {
        Ok(_) => {}
        Err(e) => {
            eprintln!("Prelude Error: {}", e);
        }
    }
}
