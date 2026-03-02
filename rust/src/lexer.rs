use std::fmt;

/// Token types for the Square lexer.
/// Replaces OCaml's Genlex tokens.
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Literals
    String(String),
    Int(i64),
    Float(f64),
    Char(char),
    Ident(String),
    // Keywords / punctuation
    Kwd(String),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::String(s) => write!(f, "\"{}\"", s),
            Token::Int(i) => write!(f, "{}", i),
            Token::Float(v) => write!(f, "{}", v),
            Token::Char(c) => write!(f, "'{}'", c),
            Token::Ident(s) => write!(f, "{}", s),
            Token::Kwd(s) => write!(f, "{}", s),
        }
    }
}

/// The lexer / tokenizer for Square.
/// Mirrors OCaml's Genlex.make_lexer with the Square keyword set.
pub struct Lexer {
    input: Vec<char>,
    pos: usize,
    tokens: Vec<Token>,
    token_pos: usize,
    line: usize,
}

const KEYWORDS: &[&str] = &[
    "[", "]", "{", "}", "(", ")", ".", ",", ":", ";", "!",
    "do", "def", ":=", "undef", "let", "letrec",
    "if", "then", "elif", "else", "and", "or",
    "fun", "try", "catch", "->",
];

impl Lexer {
    pub fn new(input: &str) -> Self {
        let mut lexer = Lexer {
            input: input.chars().collect(),
            pos: 0,
            tokens: Vec::new(),
            token_pos: 0,
            line: 1,
        };
        lexer.tokenize_all();
        lexer
    }

    #[allow(dead_code)]
    pub fn line(&self) -> usize {
        self.line
    }

    fn peek_char(&self) -> Option<char> {
        self.input.get(self.pos).copied()
    }

    fn next_char(&mut self) -> Option<char> {
        let c = self.input.get(self.pos).copied();
        if let Some(ch) = c {
            self.pos += 1;
            if ch == '\n' {
                self.line += 1;
            }
        }
        c
    }

    fn skip_whitespace_and_comments(&mut self) {
        loop {
            // Skip whitespace
            while let Some(c) = self.peek_char() {
                if c.is_whitespace() {
                    self.next_char();
                } else {
                    break;
                }
            }
            // Skip line comments: (* ... *)
            // OCaml's Genlex uses (* *) for comments
            if self.pos + 1 < self.input.len()
                && self.input[self.pos] == '('
                && self.input[self.pos + 1] == '*'
            {
                self.pos += 2;
                let mut depth = 1;
                while depth > 0 {
                    if self.pos + 1 < self.input.len()
                        && self.input[self.pos] == '('
                        && self.input[self.pos + 1] == '*' {
                        depth += 1;
                        self.pos += 2;
                    } else if self.pos + 1 < self.input.len()
                        && self.input[self.pos] == '*'
                        && self.input[self.pos + 1] == ')'
                    {
                        depth -= 1;
                        self.pos += 2;
                    } else if self.pos < self.input.len() {
                        if self.input[self.pos] == '\n' {
                            self.line += 1;
                        }
                        self.pos += 1;
                    } else {
                        break;
                    }
                }
                continue; // Check for more whitespace/comments
            }
            // Also support # line comments for convenience
            if self.peek_char() == Some('#') {
                while let Some(c) = self.next_char() {
                    if c == '\n' {
                        break;
                    }
                }
                continue;
            }
            break;
        }
    }

    fn read_string(&mut self) -> Token {
        let mut s = String::new();
        loop {
            match self.next_char() {
                Some('"') => break,
                Some('\\') => match self.next_char() {
                    Some('n') => s.push('\n'),
                    Some('t') => s.push('\t'),
                    Some('r') => s.push('\r'),
                    Some('\\') => s.push('\\'),
                    Some('"') => s.push('"'),
                    Some('\'') => s.push('\''),
                    Some(c) => {
                        s.push('\\');
                        s.push(c);
                    }
                    None => break,
                },
                Some(c) => s.push(c),
                None => break,
            }
        }
        Token::String(s)
    }

    fn read_char_literal(&mut self) -> Token {
        let c = match self.next_char() {
            Some('\\') => match self.next_char() {
                Some('n') => '\n',
                Some('t') => '\t',
                Some('r') => '\r',
                Some('\\') => '\\',
                Some('\'') => '\'',
                Some(c) => c,
                None => '?',
            },
            Some(c) => c,
            None => '?',
        };
        // consume closing quote
        self.next_char(); // '
        Token::Char(c)
    }

    fn read_number(&mut self, first: char) -> Token {
        let mut s = String::new();
        let neg = first == '-';
        if neg {
            s.push('-');
            // next char is a digit
            if let Some(c) = self.next_char() {
                s.push(c);
            }
        } else {
            s.push(first);
        }

        let mut is_float = false;
        while let Some(c) = self.peek_char() {
            if c.is_ascii_digit() {
                s.push(c);
                self.pos += 1;
            } else if c == '.' && !is_float {
                // Check if next char is a digit (to distinguish from field access)
                if self.pos + 1 < self.input.len() && self.input[self.pos + 1].is_ascii_digit() {
                    is_float = true;
                    s.push(c);
                    self.pos += 1;
                } else {
                    break;
                }
            } else if (c == 'e' || c == 'E') && !is_float {
                is_float = true;
                s.push(c);
                self.pos += 1;
                if let Some(c2) = self.peek_char() {
                    if c2 == '+' || c2 == '-' {
                        s.push(c2);
                        self.pos += 1;
                    }
                }
            } else {
                break;
            }
        }

        if is_float {
            Token::Float(s.parse().unwrap_or(0.0))
        } else {
            Token::Int(s.parse().unwrap_or(0))
        }
    }

    fn is_ident_char(c: char) -> bool {
        // OCaml Genlex treats these as identifier chars
        c.is_alphanumeric() || c == '_' || c == '\''
    }

    fn is_operator_char(c: char) -> bool {
        // Characters that can form operator identifiers
        matches!(c, '+' | '*' | '/' | '%' | '<' | '>' | '=' | '~' | '&' | '|' | '^' | '@' | '\\' | '?' | '`')
    }

    fn read_ident_or_keyword(&mut self, first: char) -> Token {
        let mut s = String::new();
        s.push(first);
        while let Some(c) = self.peek_char() {
            if Self::is_ident_char(c) {
                s.push(c);
                self.pos += 1;
            } else {
                break;
            }
        }
        // Check if it's a keyword
        if KEYWORDS.contains(&s.as_str()) {
            Token::Kwd(s)
        } else {
            Token::Ident(s)
        }
    }

    fn read_operator_ident(&mut self, first: char) -> Token {
        let mut s = String::new();
        s.push(first);
        while let Some(c) = self.peek_char() {
            if Self::is_operator_char(c) || c == '-' {
                s.push(c);
                self.pos += 1;
            } else {
                break;
            }
        }
        // Check for keywords that are operator-like
        if KEYWORDS.contains(&s.as_str()) {
            Token::Kwd(s)
        } else {
            Token::Ident(s)
        }
    }

    fn tokenize_all(&mut self) {
        loop {
            self.skip_whitespace_and_comments();
            let Some(c) = self.next_char() else {
                break;
            };
            let token = match c {
                '"' => self.read_string(),
                '\'' => self.read_char_literal(),

                // Single-char keywords
                '[' => Token::Kwd("[".into()),
                ']' => Token::Kwd("]".into()),
                '{' => Token::Kwd("{".into()),
                '}' => Token::Kwd("}".into()),
                '(' => Token::Kwd("(".into()),
                ')' => Token::Kwd(")".into()),
                ',' => Token::Kwd(",".into()),
                ';' => Token::Kwd(";".into()),
                '!' => Token::Kwd("!".into()),
                '.' => Token::Kwd(".".into()),

                // : or :=
                ':' => {
                    if self.peek_char() == Some('=') {
                        self.pos += 1;
                        Token::Kwd(":=".into())
                    } else {
                        Token::Kwd(":".into())
                    }
                }

                // ->
                '-' => {
                    if self.peek_char() == Some('>') {
                        self.pos += 1;
                        Token::Kwd("->".into())
                    } else if self.peek_char().is_some_and(|c| c.is_ascii_digit()) {
                        self.read_number('-')
                    } else if self.peek_char().is_some_and(|c| Self::is_operator_char(c) || c == '-') {
                        self.read_operator_ident('-')
                    } else {
                        // Could be unary minus as an identifier
                        Token::Ident("-".into())
                    }
                }

                c if c.is_ascii_digit() => self.read_number(c),
                c if c.is_alphabetic() || c == '_' => self.read_ident_or_keyword(c),
                c if Self::is_operator_char(c) => self.read_operator_ident(c),

                other => {
                    // Unknown character, skip
                    eprintln!("Warning: unexpected character '{}'", other);
                    continue;
                }
            };
            self.tokens.push(token);
        }
    }

    /// Peek at the current token without consuming it.
    pub fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.token_pos)
    }

    /// Consume and return the current token.
    pub fn next(&mut self) -> Option<Token> {
        let tok = self.tokens.get(self.token_pos).cloned();
        if tok.is_some() {
            self.token_pos += 1;
        }
        tok
    }

    /// Check if tokens are exhausted.
    pub fn is_empty(&self) -> bool {
        self.token_pos >= self.tokens.len()
    }

    /// Consume the next token if it matches the expected keyword.
    pub fn expect_kwd(&mut self, kwd: &str) -> Result<(), String> {
        match self.next() {
            Some(Token::Kwd(k)) if k == kwd => Ok(()),
            Some(t) => Err(format!("expected '{}', got '{}'", kwd, t)),
            None => Err(format!("expected '{}', got end of input", kwd)),
        }
    }

    /// Consume the next token if it's an identifier, returning the name.
    pub fn expect_ident(&mut self) -> Result<String, String> {
        match self.next() {
            Some(Token::Ident(s)) => Ok(s),
            Some(t) => Err(format!("expected identifier, got '{}'", t)),
            None => Err("expected identifier, got end of input".to_string()),
        }
    }
}
