mod ast;
mod env;
mod eval;
mod lexer;
mod name;
mod names;
mod parser;
mod prelude;
mod value;

use std::rc::Rc;

use env::{add_binding, new_env};
#[allow(unused_imports)]
use eval::{eval, eval_string};
use lexer::Lexer;
use name::Name;
use names::Names;
use parser::Parser;
use value::{init_generics, SquareError, Value};

const VERSION: &str = "0.2.3";
const PROMPT: &str = ":: ";

fn load_file(env: &env::Env, filename: &str) -> Result<(), SquareError> {
    let source = if filename == "-" {
        use std::io::Read;
        let mut s = String::new();
        std::io::stdin()
            .read_to_string(&mut s)
            .map_err(|e| Value::error(Name::new("IOError"), &e.to_string()))?;
        s
    } else {
        std::fs::read_to_string(filename)
            .map_err(|e| Value::error(Name::new("IOError"), &e.to_string()))?
    };

    // Strip shebang
    let source = if source.starts_with("#!") {
        if let Some(pos) = source.find('\n') {
            &source[pos + 1..]
        } else {
            ""
        }
    } else {
        &source
    };

    let mut lexer = Lexer::new(source);
    let mut parser = Parser::new(&mut lexer);
    let program = parser.parse_program().map_err(|e| {
        Value::error(
            Names::e_eval(),
            &format!("Syntax error in {}: {}", filename, e),
        )
    })?;

    for ast in &program {
        eval(env, ast).map_err(|e| {
            Value::error(
                Names::e_eval(),
                &format!(
                    "Error in {}: {}",
                    filename,
                    e.value.to_display_string()
                ),
            )
        })?;
    }
    Ok(())
}

fn toploop(env: &env::Env) {
    let config = rustyline::Config::builder()
        .auto_add_history(true)
        .build();
    let mut rl = rustyline::DefaultEditor::with_config(config)
        .expect("Failed to create line editor");

    loop {
        match rl.readline(PROMPT) {
            Ok(line) => {
                let line = line.trim();
                if line.is_empty() {
                    continue;
                }
                match eval_string(env, line) {
                    Ok(result) => {
                        println!("{}", result.to_display_string());
                    }
                    Err(e) => {
                        println!("Error: {}", e.value.to_display_string());
                    }
                }
            }
            Err(rustyline::error::ReadlineError::Interrupted) => {
                println!("^C");
                continue;
            }
            Err(rustyline::error::ReadlineError::Eof) => {
                break;
            }
            Err(e) => {
                println!("Error: {}", e);
                break;
            }
        }
    }
}

fn main() {
    // Initialize the generics table
    init_generics();

    // Create global environment
    let env = new_env();

    // Initialize prelude (native builtins)
    prelude::init_prelude(&env);

    // Add load function
    {
        let env_clone = env.clone();
        add_binding(
            &env,
            "load",
            Value::Function(Rc::new(move |x| match x {
                Value::String(filename) => {
                    load_file(&env_clone, &filename)?;
                    Ok(Value::None)
                }
                _ => Err(Value::error(
                    Names::e_type(),
                    "argument must be a string",
                )),
            })),
        );
    }

    // Initialize prelude source code (Square-language definitions)
    prelude::init_prelude_source(&env);

    let args: Vec<String> = std::env::args().collect();
    if args.len() <= 1 {
        println!("Welcome to [s]quare version {}!", VERSION);
        toploop(&env);
        println!();
    } else {
        match load_file(&env, &args[1]) {
            Ok(()) => {}
            Err(e) => {
                if let Value::Record(n, ref r) = e.value {
                    if n == Names::record() {
                        if let Some(eval_err) = r.get(&Names::e_eval()) {
                            eprintln!("{}", eval_err.to_display_string());
                            return;
                        }
                    }
                }
                eprintln!("{}", e.value.to_display_string());
            }
        }
    }
}
