mod ast;
mod env;
mod eval;
mod lexer;
mod name;
mod names;
mod parser;
mod prelude;
mod value;

use std::borrow::Cow;
use std::rc::Rc;

use env::{add_binding, new_env};
#[allow(unused_imports)]
use eval::{eval, eval_string};
use lexer::Lexer;
use name::Name;
use names::Names;
use parser::Parser;
use rustyline::highlight::Highlighter;
use value::{init_generics, SquareError, Value};

const VERSION: &str = "0.2.3";
const PROMPT: &str = ":: ";
const CONTINUATION_PROMPT: &str = ".. ";

struct SquareHelper;

impl Highlighter for SquareHelper {
    fn highlight_prompt<'b, 's: 'b, 'p: 'b>(
        &'s self,
        prompt: &'p str,
        _default: bool,
    ) -> Cow<'b, str> {
        if prompt == PROMPT {
            Cow::Owned(format!("\x1b[1;36m{}\x1b[0m", prompt))
        } else {
            // Continuation prompt
            Cow::Owned(format!("\x1b[36m{}\x1b[0m", prompt))
        }
    }
}

impl rustyline::completion::Completer for SquareHelper {
    type Candidate = String;
}

impl rustyline::hint::Hinter for SquareHelper {
    type Hint = String;
}

impl rustyline::validate::Validator for SquareHelper {}

/// Check if the input looks incomplete (unclosed delimiters or trailing arrow).
fn is_incomplete(input: &str) -> bool {
    let trimmed = input.trim();
    if trimmed.is_empty() {
        return false;
    }

    // Count unmatched delimiters, respecting strings and comments
    let mut parens = 0i32;
    let mut brackets = 0i32;
    let mut braces = 0i32;
    let chars: Vec<char> = input.chars().collect();
    let len = chars.len();
    let mut i = 0;

    while i < len {
        match chars[i] {
            '"' => {
                // Skip string literal
                i += 1;
                while i < len {
                    if chars[i] == '\\' {
                        i += 2;
                        continue;
                    }
                    if chars[i] == '"' {
                        i += 1;
                        break;
                    }
                    i += 1;
                }
                continue;
            }
            '\'' => {
                // Skip char literal: 'x' or '\n'
                i += 1;
                if i < len && chars[i] == '\\' {
                    i += 2;
                } else if i < len {
                    i += 1;
                }
                if i < len && chars[i] == '\'' {
                    i += 1;
                }
                continue;
            }
            '(' if i + 1 < len && chars[i + 1] == '*' => {
                // Skip block comment
                i += 2;
                let mut depth = 1;
                while i < len && depth > 0 {
                    if i + 1 < len && chars[i] == '(' && chars[i + 1] == '*' {
                        depth += 1;
                        i += 2;
                    } else if i + 1 < len && chars[i] == '*' && chars[i + 1] == ')' {
                        depth -= 1;
                        i += 2;
                    } else {
                        i += 1;
                    }
                }
                if depth > 0 {
                    return true; // Unclosed comment
                }
                continue;
            }
            '#' => {
                // Skip line comment
                while i < len && chars[i] != '\n' {
                    i += 1;
                }
                continue;
            }
            '(' => parens += 1,
            ')' => parens -= 1,
            '[' => brackets += 1,
            ']' => brackets -= 1,
            '{' => braces += 1,
            '}' => braces -= 1,
            _ => {}
        }
        i += 1;
    }

    if parens > 0 || brackets > 0 || braces > 0 {
        return true;
    }

    // Check if it ends with -> (expecting a body)
    if trimmed.ends_with("->") {
        return true;
    }

    // Check for trailing keywords that expect more input
    let last_word = trimmed.rsplit_once(|c: char| c.is_whitespace() || c == ';')
        .map(|(_, w)| w)
        .unwrap_or(trimmed);
    matches!(last_word, "then" | "else" | "elif" | "catch" | "def" | "fun" | "try" | "do")
}

impl rustyline::Helper for SquareHelper {}

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
    let mut rl = rustyline::Editor::with_config(config)
        .expect("Failed to create line editor");
    rl.set_helper(Some(SquareHelper));

    loop {
        match rl.readline(PROMPT) {
            Ok(first_line) => {
                let mut input = first_line;
                // Accumulate continuation lines while input looks incomplete
                while is_incomplete(&input) {
                    match rl.readline(CONTINUATION_PROMPT) {
                        Ok(cont) => {
                            input.push('\n');
                            input.push_str(&cont);
                        }
                        Err(rustyline::error::ReadlineError::Interrupted) => {
                            println!("^C");
                            input.clear();
                            break;
                        }
                        Err(_) => {
                            input.clear();
                            break;
                        }
                    }
                }
                let input = input.trim();
                if input.is_empty() {
                    continue;
                }
                match eval_string(env, input) {
                    Ok(result) => {
                        let s = result.to_display_string();
                        if s != "[]" {
                            println!("\x1b[32m{}\x1b[0m", s);
                        }
                    }
                    Err(e) => {
                        println!("\x1b[1;31mError:\x1b[0m \x1b[31m{}\x1b[0m", e.value.to_display_string());
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
        println!("\x1b[1mWelcome to \x1b[36m[s]quare\x1b[0m\x1b[1m version {}!\x1b[0m", VERSION);
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
