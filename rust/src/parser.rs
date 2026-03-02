use crate::ast::{Ast, Catch, Pattern};
use crate::lexer::{Lexer, Token};
use crate::name::Name;
use crate::names::Names;

/// Recursive descent parser for the Square language.
/// Directly mirrors the OCaml camlp4 stream parser.
pub struct Parser<'a> {
    lexer: &'a mut Lexer,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer) -> Self {
        Parser { lexer }
    }

    /// Parse a full expression (with application).
    /// parse = parse_expr (parse_rest)*
    pub fn parse(&mut self) -> Result<Ast, String> {
        let expr = self.parse_expr()?;
        self.parse_rest(expr)
    }

    /// Left-associative function application:
    /// parse_rest expr = parse_expr (parse_rest (App(expr, arg)))
    fn parse_rest(&mut self, expr: Ast) -> Result<Ast, String> {
        if self.can_start_expr() {
            let arg = self.parse_expr()?;
            self.parse_rest(Ast::App(Box::new(expr), Box::new(arg)))
        } else {
            Ok(expr)
        }
    }

    /// Can the next token start a new expression?
    /// Used to decide whether to continue parsing application arguments.
    fn can_start_expr(&self) -> bool {
        match self.lexer.peek() {
            Some(Token::String(_))
            | Some(Token::Int(_))
            | Some(Token::Float(_))
            | Some(Token::Char(_))
            | Some(Token::Ident(_)) => true,
            Some(Token::Kwd(k)) => matches!(
                k.as_str(),
                "." | "(" | "{" | "[" | "do" | "def" | ":=" | "!" | "undef" | "let" | "letrec"
                    | "if" | "and" | "or" | "fun" | "try"
            ),
            _ => false,
        }
    }

    /// Parse a single expression (no application).
    fn parse_expr(&mut self) -> Result<Ast, String> {
        let tok = self.lexer.next().ok_or("unexpected end of input")?;
        match tok {
            Token::String(s) => Ok(Ast::String(s)),
            Token::Int(i) => Ok(Ast::Int(i)),
            Token::Float(f) => Ok(Ast::Float(f)),
            Token::Char(c) => Ok(Ast::Char(c)),
            Token::Ident(s) => Ok(Ast::Name(Name::new(&s))),
            Token::Kwd(k) => match k.as_str() {
                "." => {
                    let name = self
                        .lexer
                        .expect_ident()
                        .map_err(|_| "symbol name expected after '.'")?;
                    Ok(Ast::Symbol(Name::new(&name)))
                }
                "(" => {
                    let list = self.parse_list()?;
                    Ok(Ast::List(list))
                }
                "{" => {
                    let items = self.parse_record()?;
                    Ok(Ast::record(items))
                }
                "[" => {
                    let stmts = self.parse_do()?;
                    Ok(Ast::Do(stmts))
                }
                "do" => {
                    self.lexer
                        .expect_kwd("[")
                        .map_err(|_| "'[' expected after 'do'")?;
                    let stmts = self.parse_do()?;
                    Ok(Ast::Do(stmts))
                }
                "def" => {
                    let name = self
                        .lexer
                        .expect_ident()
                        .map_err(|_| "name expected after 'def'")?;
                    let expr = self.parse().map_err(|_| "missing expression after 'def'")?;
                    Ok(Ast::Def(Name::new(&name), Box::new(expr)))
                }
                ":=" => {
                    let name = self
                        .lexer
                        .expect_ident()
                        .map_err(|_| "name expected after ':='")?;
                    let expr = self
                        .parse()
                        .map_err(|_| "missing expression after ':='")?;
                    Ok(Ast::Set(Name::new(&name), Box::new(expr)))
                }
                "!" => {
                    let func = self.parse_mutator()?;
                    let arg_name = self
                        .lexer
                        .expect_ident()
                        .map_err(|_| "name expected after '!' mutator")?;
                    let name = Name::new(&arg_name);
                    let app = Ast::App(Box::new(func), Box::new(Ast::Name(name)));
                    let rest = self.parse_rest(app)?;
                    Ok(Ast::Set(name, Box::new(rest)))
                }
                "undef" => {
                    let name = self
                        .lexer
                        .expect_ident()
                        .map_err(|_| "name expected after 'undef'")?;
                    Ok(Ast::Undef(Name::new(&name)))
                }
                "let" => {
                    self.lexer
                        .expect_kwd("{")
                        .map_err(|_| "'{' expected after 'let'")?;
                    let items = self.parse_record()?;
                    let expr = self
                        .parse()
                        .map_err(|_| "missing expression after 'let'")?;
                    Ok(Ast::Let(items, Box::new(expr)))
                }
                "letrec" => {
                    self.lexer
                        .expect_kwd("{")
                        .map_err(|_| "'{' expected after 'letrec'")?;
                    let items = self.parse_record()?;
                    // Desugar letrec: let {bindings all = []} do [set each; expr]
                    let inits: Vec<(Name, Ast)> =
                        items.iter().map(|(k, _)| (*k, Ast::Do(vec![]))).collect();
                    let mut binds: Vec<Ast> = items
                        .into_iter()
                        .rev()
                        .map(|(k, v)| Ast::Set(k, Box::new(v)))
                        .collect();
                    // Reverse so sets are in original order
                    binds.reverse();
                    let expr = self
                        .parse()
                        .map_err(|_| "missing expression after 'letrec'")?;
                    binds.push(expr);
                    Ok(Ast::Let(inits, Box::new(Ast::Do(binds))))
                }
                "if" => {
                    let clauses = self.parse_if()?;
                    Ok(Ast::If(clauses))
                }
                "and" => {
                    self.lexer
                        .expect_kwd("(")
                        .map_err(|_| "'(' expected after 'and'")?;
                    let list = self.parse_list()?;
                    // Fold: and(a, b, c) => if a then (if b then (if c then .true else .false) else .false) else .false
                    let result = list.into_iter().fold(Ast::Symbol(Names::true_()), |acc, v| {
                        Ast::If(vec![v, acc, Ast::Symbol(Names::false_())])
                    });
                    Ok(result)
                }
                "or" => {
                    self.lexer
                        .expect_kwd("(")
                        .map_err(|_| "'(' expected after 'or'")?;
                    let list = self.parse_list()?;
                    let result =
                        list.into_iter().fold(Ast::Symbol(Names::false_()), |acc, v| {
                            Ast::If(vec![v, Ast::Symbol(Names::true_()), acc])
                        });
                    Ok(result)
                }
                "fun" => {
                    let pat = self
                        .parse_pattern()
                        .map_err(|_| "pattern expected after 'fun'")?;
                    self.lexer
                        .expect_kwd("->")
                        .map_err(|_| "'->' expected after 'fun'")?;
                    let expr = self
                        .parse()
                        .map_err(|_| "missing expression after 'fun'")?;
                    Ok(Ast::Fun(pat, Box::new(expr)))
                }
                "try" => {
                    let expr = self
                        .parse()
                        .map_err(|_| "missing expression after 'try'")?;
                    self.lexer
                        .expect_kwd("catch")
                        .map_err(|_| "'try' requires at least one 'catch'")?;
                    let catches = self.parse_catch()?;
                    Ok(Ast::Try(Box::new(expr), catches))
                }
                other => Err(format!("unexpected keyword '{}'", other)),
            },
        }
    }

    /// Parse if/elif/else chain.
    fn parse_if(&mut self) -> Result<Vec<Ast>, String> {
        let cond = self.parse().map_err(|_| "missing condition after 'if'")?;
        self.lexer
            .expect_kwd("then")
            .map_err(|_| "'then' expected after 'if'")?;
        let then_expr = self
            .parse()
            .map_err(|_| "missing expression after 'then'")?;
        let mut clauses = vec![cond, then_expr];
        self.parse_if_else(&mut clauses)?;
        Ok(clauses)
    }

    fn parse_if_else(&mut self, clauses: &mut Vec<Ast>) -> Result<(), String> {
        if let Some(Token::Kwd(k)) = self.lexer.peek() {
            match k.as_str() {
                "elif" => {
                    self.lexer.next();
                    let cond = self
                        .parse()
                        .map_err(|_| "missing expression after 'elif'")?;
                    self.lexer
                        .expect_kwd("then")
                        .map_err(|_| "'then' expected after 'elif'")?;
                    let expr = self
                        .parse()
                        .map_err(|_| "missing expression after 'then'")?;
                    clauses.push(cond);
                    clauses.push(expr);
                    self.parse_if_else(clauses)?;
                }
                "else" => {
                    self.lexer.next();
                    let expr = self
                        .parse()
                        .map_err(|_| "missing expression after 'else'")?;
                    clauses.push(expr);
                }
                _ => {}
            }
        }
        Ok(())
    }

    /// Parse comma-separated list inside parens: (a, b, c)
    fn parse_list(&mut self) -> Result<Vec<Ast>, String> {
        let mut list = Vec::new();
        if let Some(Token::Kwd(k)) = self.lexer.peek() {
            if k == ")" {
                self.lexer.next();
                return Ok(list);
            }
        }
        let expr = self.parse()?;
        list.push(expr);
        self.parse_list_next(&mut list)?;
        Ok(list)
    }

    fn parse_list_next(&mut self, list: &mut Vec<Ast>) -> Result<(), String> {
        match self.lexer.peek() {
            Some(Token::Kwd(k)) if k == ")" => {
                self.lexer.next();
                Ok(())
            }
            Some(Token::Kwd(k)) if k == "," => {
                self.lexer.next();
                if let Some(Token::Kwd(k)) = self.lexer.peek() {
                    if k == ")" {
                        self.lexer.next();
                        return Ok(());
                    }
                }
                let expr = self.parse()?;
                list.push(expr);
                self.parse_list_next(list)
            }
            _ => Err("unexpected end of list".to_string()),
        }
    }

    /// Parse record fields: {name: expr, ...}
    fn parse_record(&mut self) -> Result<Vec<(Name, Ast)>, String> {
        let mut items = Vec::new();
        if let Some(Token::Kwd(k)) = self.lexer.peek() {
            if k == "}" {
                self.lexer.next();
                return Ok(items);
            }
        }
        let field = self
            .lexer
            .expect_ident()
            .map_err(|_| "field name expected in record")?;
        self.lexer
            .expect_kwd(":")
            .map_err(|_| "missing ':' in record")?;
        let expr = self
            .parse()
            .map_err(|_| "missing expression in record")?;
        items.push((Name::new(&field), expr));
        self.parse_record_next(&mut items)?;
        Ok(items)
    }

    fn parse_record_next(&mut self, items: &mut Vec<(Name, Ast)>) -> Result<(), String> {
        match self.lexer.peek() {
            Some(Token::Kwd(k)) if k == "}" => {
                self.lexer.next();
                Ok(())
            }
            Some(Token::Kwd(k)) if k == "," => {
                self.lexer.next();
                if let Some(Token::Kwd(k)) = self.lexer.peek() {
                    if k == "}" {
                        self.lexer.next();
                        return Ok(());
                    }
                }
                let field = self
                    .lexer
                    .expect_ident()
                    .map_err(|_| "field name expected in record")?;
                self.lexer
                    .expect_kwd(":")
                    .map_err(|_| "missing ':' in record")?;
                let expr = self
                    .parse()
                    .map_err(|_| "missing expression in record")?;
                items.push((Name::new(&field), expr));
                self.parse_record_next(items)
            }
            _ => Err("unexpected end of record".to_string()),
        }
    }

    /// Parse do-block: [stmt; stmt; ...]
    fn parse_do(&mut self) -> Result<Vec<Ast>, String> {
        let mut stmts = Vec::new();
        if let Some(Token::Kwd(k)) = self.lexer.peek() {
            if k == "]" {
                self.lexer.next();
                return Ok(stmts);
            }
        }
        let expr = self.parse()?;
        stmts.push(expr);
        self.parse_do_next(&mut stmts)?;
        Ok(stmts)
    }

    fn parse_do_next(&mut self, stmts: &mut Vec<Ast>) -> Result<(), String> {
        match self.lexer.peek() {
            Some(Token::Kwd(k)) if k == "]" => {
                self.lexer.next();
                Ok(())
            }
            Some(Token::Kwd(k)) if k == ";" => {
                self.lexer.next();
                if let Some(Token::Kwd(k)) = self.lexer.peek() {
                    if k == "]" {
                        self.lexer.next();
                        return Ok(());
                    }
                }
                let expr = self.parse()?;
                stmts.push(expr);
                self.parse_do_next(stmts)
            }
            _ => Err("unexpected end of do-block".to_string()),
        }
    }

    /// Parse mutator expression (after !)
    fn parse_mutator(&mut self) -> Result<Ast, String> {
        match self.lexer.peek() {
            Some(Token::Kwd(k)) if k == "[" => {
                self.lexer.next();
                let stmts = self.parse_do()?;
                Ok(Ast::Do(stmts))
            }
            Some(Token::Ident(_)) => {
                let name = self.lexer.expect_ident()?;
                Ok(Ast::Name(Name::new(&name)))
            }
            _ => Err("mutator expression expected after '!'".to_string()),
        }
    }

    /// Parse a pattern for function arguments.
    fn parse_pattern(&mut self) -> Result<Pattern, String> {
        match self.lexer.peek() {
            Some(Token::Kwd(k)) if k == "[" => {
                self.lexer.next();
                self.lexer
                    .expect_kwd("]")
                    .map_err(|_| "empty pattern expected")?;
                Ok(Pattern::Empty)
            }
            Some(Token::Kwd(k)) if k == "(" => {
                self.lexer.next();
                let names = self.parse_list_pattern()?;
                Ok(Pattern::List(names))
            }
            Some(Token::Kwd(k)) if k == "{" => {
                self.lexer.next();
                let names = self.parse_record_pattern()?;
                Ok(Pattern::Record(names))
            }
            Some(Token::Ident(_)) => {
                let name = self.lexer.expect_ident()?;
                Ok(Pattern::Scalar(Name::new(&name)))
            }
            _ => Err("pattern expected".to_string()),
        }
    }

    fn parse_list_pattern(&mut self) -> Result<Vec<Name>, String> {
        let mut names = Vec::new();
        if let Some(Token::Kwd(k)) = self.lexer.peek() {
            if k == ")" {
                self.lexer.next();
                return Ok(names);
            }
        }
        let name = self.lexer.expect_ident()?;
        names.push(Name::new(&name));
        self.parse_list_pattern_next(&mut names)?;
        Ok(names)
    }

    fn parse_list_pattern_next(&mut self, names: &mut Vec<Name>) -> Result<(), String> {
        match self.lexer.peek() {
            Some(Token::Kwd(k)) if k == ")" => {
                self.lexer.next();
                Ok(())
            }
            Some(Token::Kwd(k)) if k == "," => {
                self.lexer.next();
                let name = self.lexer.expect_ident()?;
                names.push(Name::new(&name));
                self.parse_list_pattern_next(names)
            }
            _ => Err("unexpected end of list pattern".to_string()),
        }
    }

    fn parse_record_pattern(&mut self) -> Result<Vec<Name>, String> {
        let mut names = Vec::new();
        if let Some(Token::Kwd(k)) = self.lexer.peek() {
            if k == "}" {
                self.lexer.next();
                return Ok(names);
            }
        }
        let name = self.lexer.expect_ident()?;
        names.push(Name::new(&name));
        self.parse_record_pattern_next(&mut names)?;
        Ok(names)
    }

    fn parse_record_pattern_next(&mut self, names: &mut Vec<Name>) -> Result<(), String> {
        match self.lexer.peek() {
            Some(Token::Kwd(k)) if k == "}" => {
                self.lexer.next();
                Ok(())
            }
            Some(Token::Kwd(k)) if k == "," => {
                self.lexer.next();
                let name = self.lexer.expect_ident()?;
                names.push(Name::new(&name));
                self.parse_record_pattern_next(names)
            }
            _ => Err("unexpected end of record pattern".to_string()),
        }
    }

    /// Parse catch clauses.
    fn parse_catch(&mut self) -> Result<Vec<Catch>, String> {
        let mut catches = Vec::new();
        // First catch clause (after the 'catch' keyword already consumed)
        self.parse_one_catch(&mut catches)?;
        // More 'catch' clauses
        loop {
            if let Some(Token::Kwd(k)) = self.lexer.peek() {
                if k == "catch" {
                    self.lexer.next();
                    self.parse_one_catch(&mut catches)?;
                    continue;
                }
            }
            break;
        }
        Ok(catches)
    }

    fn parse_one_catch(&mut self, catches: &mut Vec<Catch>) -> Result<(), String> {
        // Could be identifier (ValCatch) or pattern (PatCatch)
        match self.lexer.peek() {
            Some(Token::Ident(_)) => {
                // Could be a ValCatch (name -> expr) or ScalarPat
                // Look ahead: if followed by ->, it's a ValCatch
                let name_str = self.lexer.expect_ident()?;
                let name = Name::new(&name_str);
                if let Some(Token::Kwd(k)) = self.lexer.peek() {
                    if k == "->" {
                        self.lexer.next();
                        let expr = self
                            .parse()
                            .map_err(|_| "missing expression after 'catch'")?;
                        catches.push(Catch::Val(name, expr));
                        return Ok(());
                    }
                }
                // It's actually a ScalarPat catch
                self.lexer
                    .expect_kwd("->")
                    .map_err(|_| "'->' expected after 'catch'")?;
                let expr = self
                    .parse()
                    .map_err(|_| "missing expression after 'catch'")?;
                catches.push(Catch::Pat(Pattern::Scalar(name), expr));
                Ok(())
            }
            _ => {
                let pat = self.parse_pattern()?;
                self.lexer
                    .expect_kwd("->")
                    .map_err(|_| "'->' expected after 'catch'")?;
                let expr = self
                    .parse()
                    .map_err(|_| "missing expression after 'catch'")?;
                catches.push(Catch::Pat(pat, expr));
                Ok(())
            }
        }
    }

    /// Parse top-level statements separated by semicolons.
    pub fn parse_program(&mut self) -> Result<Vec<Ast>, String> {
        let mut stmts = Vec::new();
        while !self.lexer.is_empty() {
            let ast = self.parse()?;
            stmts.push(ast);
            // Consume optional semicolons between top-level statements
            while let Some(Token::Kwd(k)) = self.lexer.peek() {
                if k == ";" {
                    self.lexer.next();
                } else {
                    break;
                }
            }
        }
        Ok(stmts)
    }
}
