use std::rc::Rc;

use crate::ast::{Ast, Catch, Pattern};
use crate::env::{child_env, Env};
use crate::name::Name;
use crate::names::{NameMap, Names};
use crate::value::{SquareError, Value, GENERICS};

/// Evaluate an AST node in the given environment.
/// Directly mirrors the OCaml `eval` function.
pub fn eval(env: &Env, ast: &Ast) -> Result<Value, SquareError> {
    match ast {
        Ast::Name(n) => {
            let cell = env.borrow().get(*n).ok_or_else(|| {
                Value::error(Names::e_name(), &n.to_string())
            })?;
            Ok(cell.borrow().clone())
        }
        Ast::Symbol(n) => Ok(Value::Symbol(*n)),
        Ast::String(s) => Ok(Value::string(s.clone())),
        Ast::Int(i) => Ok(Value::Int(*i)),
        Ast::Float(f) => Ok(Value::Float(*f)),
        Ast::Char(c) => Ok(Value::Char(*c)),
        Ast::List(l) => {
            let vals: Result<Vec<Value>, _> = l.iter().map(|a| eval(env, a)).collect();
            Ok(Value::list(vals?))
        }
        Ast::Record(r) => {
            let mut map = NameMap::new();
            for (k, v) in r {
                map.insert(*k, eval(env, v)?);
            }
            Ok(Value::Record(Names::record(), Rc::new(map)))
        }
        Ast::Do(stmts) => {
            if stmts.is_empty() {
                return Ok(Value::None);
            }
            let mut result = Value::None;
            for stmt in stmts {
                result = eval(env, stmt)?;
            }
            Ok(result)
        }
        Ast::Def(n, v) => {
            let val = eval(env, v)?;
            env.borrow_mut().define(*n, val);
            Ok(Value::None)
        }
        Ast::Set(n, v) => {
            let val = eval(env, v)?;
            let cell = env.borrow().get(*n).ok_or_else(|| {
                Value::error(Names::e_name(), &n.to_string())
            })?;
            *cell.borrow_mut() = val;
            Ok(Value::None)
        }
        Ast::Undef(n) => {
            env.borrow_mut().remove(*n);
            Ok(Value::None)
        }
        Ast::Let(bindings, body) => {
            let new_env = child_env(env);
            for (n, v) in bindings {
                let val = eval(&new_env, v)?;
                new_env.borrow_mut().define(*n, val);
            }
            eval(&new_env, body)
        }
        Ast::If(clauses) => eval_if(env, clauses),
        Ast::App(f, a) => {
            let func_val = eval(env, f)?;
            eval_app(env, &func_val, a)
        }
        Ast::Fun(pat, body) => eval_fun(env, pat, body),
        Ast::Try(expr, catches) => eval_try(env, expr, catches),
    }
}

fn eval_if(env: &Env, clauses: &[Ast]) -> Result<Value, SquareError> {
    match clauses.len() {
        0 => Ok(Value::None),
        1 => eval(env, &clauses[0]),
        _ => {
            let cond = eval(env, &clauses[0])?;
            if cond.to_bool() {
                eval(env, &clauses[1])
            } else {
                eval_if(env, &clauses[2..])
            }
        }
    }
}

fn eval_app(env: &Env, func_val: &Value, arg_ast: &Ast) -> Result<Value, SquareError> {
    match func_val {
        Value::Function(f) => {
            let arg = eval(env, arg_ast)?;
            f(arg)
        }
        Value::String(s) => {
            let arg = eval(env, arg_ast)?;
            match arg {
                Value::Int(i) => {
                    let idx = if i < 0 { s.len() as i64 + i } else { i } as usize;
                    s.chars()
                        .nth(idx)
                        .map(Value::Char)
                        .ok_or_else(|| Value::error_value(Names::e_index_nf(), Value::Int(i)))
                }
                _ => Err(Value::error(
                    Names::e_type(),
                    "can't apply non-integer to string",
                )),
            }
        }
        Value::List(l) => {
            let arg = eval(env, arg_ast)?;
            match arg {
                Value::Int(i) => {
                    let idx = if i < 0 { l.len() as i64 + i } else { i } as usize;
                    l.get(idx)
                        .cloned()
                        .ok_or_else(|| Value::error_value(Names::e_index_nf(), Value::Int(i)))
                }
                _ => Err(Value::error(
                    Names::e_type(),
                    "can't apply non-integer to list",
                )),
            }
        }
        Value::Record(tag, r) => {
            let arg = eval(env, arg_ast)?;
            match &arg {
                Value::Symbol(n) => {
                    if let Some(v) = r.get(n) {
                        Ok(v.clone())
                    } else {
                        // Try generic call - clone out of borrow scope
                        let func = GENERICS.with(|g| {
                            let g = g.borrow();
                            g.get(Name::new("call"), *tag).cloned()
                        });
                        if let Some(Value::Function(f)) = func {
                            f(arg.clone())
                        } else {
                            Err(Value::error_value(
                                Names::e_field_nf(),
                                Value::Symbol(*n),
                            ))
                        }
                    }
                }
                _ => {
                    // Try generic call - clone out of borrow scope
                    let tag = *tag;
                    let func = GENERICS.with(|g| {
                        let g = g.borrow();
                        g.get(Name::new("call"), tag).cloned()
                    });
                    if let Some(Value::Function(f)) = func {
                        f(arg.clone())
                    } else {
                        Err(Value::error(
                            Names::e_type(),
                            &format!("call is not defined for {}", tag),
                        ))
                    }
                }
            }
        }
        _ => Err(Value::error(
            Names::e_type(),
            &format!(
                "call is not defined for {}",
                func_val.to_display_string()
            ),
        )),
    }
}

fn eval_fun(env: &Env, pat: &Pattern, body: &Ast) -> Result<Value, SquareError> {
    // Capture the env Rc directly (not a child copy). This mirrors OCaml's behavior
    // where closures capture the `ref` to the environment, so they see future `def`s.
    let captured_env = env.clone();
    let body = body.clone();
    let pat = pat.clone();

    match pat {
        Pattern::Empty => Ok(Value::Function(Rc::new(move |arg| {
            if let Value::None = arg {
                eval(&child_env(&captured_env), &body)
            } else {
                Err(Value::error(Names::e_type(), "function takes no arguments"))
            }
        }))),
        Pattern::Scalar(name) => Ok(Value::Function(Rc::new(move |arg| {
            let new_env = child_env(&captured_env);
            new_env.borrow_mut().define(name, arg);
            eval(&new_env, &body)
        }))),
        Pattern::List(names) => {
            let names = names.clone();
            Ok(Value::Function(Rc::new(move |arg| {
                if let Value::List(args) = arg {
                    if args.len() != names.len() {
                        return Err(Value::error(
                            Names::e_type(),
                            &format!(
                                "function requires a list of size {}",
                                names.len()
                            ),
                        ));
                    }
                    let new_env = child_env(&captured_env);
                    for (n, v) in names.iter().zip(args.iter().cloned()) {
                        new_env.borrow_mut().define(*n, v);
                    }
                    eval(&new_env, &body)
                } else {
                    Err(Value::error(
                        Names::e_type(),
                        "function requires a list argument",
                    ))
                }
            })))
        }
        Pattern::Record(names) => {
            let names = names.clone();
            Ok(Value::Function(Rc::new(move |arg| {
                if let Value::Record(_, ref r) = arg {
                    let new_env = child_env(&captured_env);
                    for n in &names {
                        let v = r.get(n).ok_or_else(|| {
                            Value::error(
                                Names::e_type(),
                                &format!(
                                    "function requires a record with field {}",
                                    n
                                ),
                            )
                        })?;
                        new_env.borrow_mut().define(*n, v.clone());
                    }
                    eval(&new_env, &body)
                } else {
                    Err(Value::error(
                        Names::e_type(),
                        "function requires a record argument",
                    ))
                }
            })))
        }
    }
}

fn eval_try(
    env: &Env,
    expr: &Ast,
    catches: &[Catch],
) -> Result<Value, SquareError> {
    match eval(env, expr) {
        Ok(v) => Ok(v),
        Err(err) => {
            let e = err.value;
            for catch in catches {
                match catch {
                    Catch::Val(n, a) => {
                        let new_env = child_env(env);
                        new_env.borrow_mut().define(*n, e.clone());
                        return eval(&new_env, a);
                    }
                    Catch::Pat(Pattern::Empty, a) => {
                        return eval(env, a);
                    }
                    Catch::Pat(Pattern::Scalar(n), a) => {
                        let new_env = child_env(env);
                        new_env.borrow_mut().define(*n, e.clone());
                        return eval(&new_env, a);
                    }
                    Catch::Pat(Pattern::List(names), a) => {
                        if let Value::List(ref l) = e {
                            if l.len() == names.len() {
                                let new_env = child_env(env);
                                for (n, v) in names.iter().zip(l.iter()) {
                                    new_env.borrow_mut().define(*n, v.clone());
                                }
                                return eval(&new_env, a);
                            }
                        }
                        // Pattern doesn't match, try next catch
                        continue;
                    }
                    Catch::Pat(Pattern::Record(names), a) => {
                        if let Value::Record(_, ref r) = e {
                            let mut all_found = true;
                            for n in names {
                                if !r.contains_key(n) {
                                    all_found = false;
                                    break;
                                }
                            }
                            if all_found {
                                let new_env = child_env(env);
                                for n in names {
                                    new_env
                                        .borrow_mut()
                                        .define(*n, r.get(n).unwrap().clone());
                                }
                                return eval(&new_env, a);
                            }
                        }
                        continue;
                    }
                }
            }
            // No catch matched, re-raise
            Err(SquareError::new(e))
        }
    }
}

/// Evaluate a string of Square source code in the given environment.
pub fn eval_string(env: &Env, source: &str) -> Result<Value, SquareError> {
    let mut lexer = crate::lexer::Lexer::new(source);
    let mut parser = crate::parser::Parser::new(&mut lexer);
    let program = parser.parse_program().map_err(|e| {
        Value::error(Names::e_eval(), &format!("Syntax error: {}", e))
    })?;
    let mut result = Value::None;
    for ast in &program {
        result = eval(env, ast)?;
    }
    Ok(result)
}
