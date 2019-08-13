use std::fmt;
use crate::{
    SyntaxError,
    src::SrcRef,
    ast::Ast,
};

#[derive(Debug)]
pub enum LexErrorKind<'a> {
    UnexpectedChar(char),
    InvalidNumber(&'a str),
    UnknownSymbol(&'a str),
}

#[derive(Debug)]
pub struct LexError<'a> {
    kind: LexErrorKind<'a>,
    src_ref: SrcRef,
}

impl<'a> LexError<'a> {
    pub fn unexpected_char(c: char, src_ref: SrcRef) -> Self {
        Self { kind: LexErrorKind::UnexpectedChar(c), src_ref }
    }

    pub fn invalid_number(s: &'a str, src_ref: SrcRef) -> Self {
        Self { kind: LexErrorKind::InvalidNumber(s), src_ref }
    }

    pub fn unknown_symbol(s: &'a str, src_ref: SrcRef) -> Self {
        Self { kind: LexErrorKind::UnknownSymbol(s), src_ref }
    }

    pub fn get_text(&self) -> String {
        match self.kind {
            LexErrorKind::UnexpectedChar(c) => format!("Unexpected character '{:?}'", c),
            LexErrorKind::InvalidNumber(s) => format!("Invalid number '{}'", s),
            LexErrorKind::UnknownSymbol(s) => format!("Unknown symbol '{}'", s),
        }
    }

    pub fn get_src_refs(&self) -> Vec<SrcRef> {
        vec![self.src_ref]
    }
}

#[derive(Clone, Debug)]
pub enum Lexeme<'a> {
    True,
    False,

    Pipe,
    Comma,
    Colon,
    Semicolon,
    LParen,
    RParen,
    LBrack,
    RBrack,
    LBrace,
    RBrace,
    Arrow,
    Universe,

    Const,
    Let,
    Type,
    Data,
    If,
    Then,
    Else,

    Add,
    Sub,
    Mul,
    Div,
    Rem,

    Eq,
    NotEq,
    Less,
    LessEq,
    More,
    MoreEq,

    Not,

    And,
    Or,
    Xor,

    Ident(&'a str),
    Int(i64),
    Float(f64),
    String(String),

    Eof,
}

impl<'a> fmt::Display for Lexeme<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Lexeme::True => write!(f, "true"),
            Lexeme::False => write!(f, "false"),

            Lexeme::Pipe => write!(f, "|"),
            Lexeme::Comma => write!(f, ","),
            Lexeme::Colon => write!(f, ":"),
            Lexeme::Semicolon => write!(f, ";"),
            Lexeme::LParen => write!(f, "("),
            Lexeme::RParen => write!(f, ")"),
            Lexeme::LBrack => write!(f, "["),
            Lexeme::RBrack => write!(f, "]"),
            Lexeme::LBrace => write!(f, "{{"),
            Lexeme::RBrace => write!(f, "}}"),
            Lexeme::Arrow => write!(f, "->"),
            Lexeme::Universe => write!(f, "@"),

            Lexeme::Const => write!(f, "const"),
            Lexeme::Let => write!(f, "let"),
            Lexeme::Type => write!(f, "type"),
            Lexeme::Data => write!(f, "data"),
            Lexeme::If => write!(f, "if"),
            Lexeme::Then => write!(f, "then"),
            Lexeme::Else => write!(f, "else"),

            Lexeme::Add => write!(f, "+"),
            Lexeme::Sub => write!(f, "-"),
            Lexeme::Mul => write!(f, "*"),
            Lexeme::Div => write!(f, "/"),
            Lexeme::Rem => write!(f, "%"),

            Lexeme::Eq => write!(f, "="),
            Lexeme::NotEq => write!(f, "!="),
            Lexeme::Less => write!(f, "<"),
            Lexeme::LessEq => write!(f, "<="),
            Lexeme::More => write!(f, ">"),
            Lexeme::MoreEq => write!(f, ">="),

            Lexeme::Not => write!(f, "!"),

            Lexeme::And => write!(f, "and"),
            Lexeme::Or => write!(f, "or"),
            Lexeme::Xor => write!(f, "xor"),

            Lexeme::Ident(ident) => write!(f, "{}", ident),
            Lexeme::Int(x) => write!(f, "{}", x),
            Lexeme::Float(x) => write!(f, "{}", x),
            Lexeme::String(x) => write!(f, "\"{}\"", x),

            Lexeme::Eof => write!(f, "EOF"),
        }
    }
}

#[derive(Debug)]
pub struct Token<'a>(Lexeme<'a>, SrcRef);

impl<'a> Token<'a> {
    pub fn lexeme(&self) -> &Lexeme {
        &self.0
    }

    pub fn src_ref(&self) -> SrcRef {
        self.1
    }
}

#[derive(Debug)]
pub struct TokenList<'a>(Vec<Token<'a>>);

impl<'a> TokenList<'a> {
    pub fn lex(code: &'a str) -> Result<Self, Vec<LexError>> {
        enum State {
            Default,
            Word,
            Number,
            Symbol,
            String(String),
            Comment,
        }

        let mut chars = code
            .chars()
            .enumerate()
            .chain(std::iter::repeat((code.len(), '\0')));
        let mut state = State::Default;
        let mut start = 0;
        let mut tokens = Vec::new();
        let mut errors = Vec::new();

        fn is_singular<'a>(c: char) -> Option<Lexeme<'a>> {
            Some(match c {
                '|' => Lexeme::Pipe,
                ',' => Lexeme::Comma,
                ';' => Lexeme::Semicolon,
                '(' => Lexeme::LParen,
                ')' => Lexeme::RParen,
                '[' => Lexeme::LBrack,
                ']' => Lexeme::RBrack,
                '{' => Lexeme::LBrace,
                '}' => Lexeme::RBrace,
                _ => return None,
            })
        }

        loop {
            let (i, c) = chars.clone().next().unwrap();
            let mut next = true;
            match &mut state {
                State::Default => {
                    start = i;
                    match c {
                        '\0' => {
                            tokens.push(Token(Lexeme::Eof, SrcRef::from(i)));
                            break;
                        },
                        '#' => state = State::Comment,
                        '"' => state = State::String(String::new()),
                        c if is_singular(c).is_some() => tokens.push(Token(is_singular(c).unwrap(), SrcRef::from(i))),
                        c if c.is_whitespace() => {},
                        c if c.is_alphabetic() || c == '_' => state = State::Word,
                        c if c.is_numeric() => state = State::Number,
                        c if c.is_ascii_punctuation() && c != '_' && is_singular(c).is_none() => state = State::Symbol,
                        c => errors.push(LexError::unexpected_char(c, SrcRef::from(i))),
                    }
                },
                State::Word => match c {
                    c if c.is_alphanumeric() || c == '_' => {},
                    _ => {
                        let r = SrcRef::from((start, i));
                        match &code[start..i] {
                            "const" => tokens.push(Token(Lexeme::Const, r)),
                            "let" => tokens.push(Token(Lexeme::Let, r)),
                            "type" => tokens.push(Token(Lexeme::Type, r)),
                            "data" => tokens.push(Token(Lexeme::Data, r)),
                            "true" => tokens.push(Token(Lexeme::True, r)),
                            "false" => tokens.push(Token(Lexeme::False, r)),
                            "if" => tokens.push(Token(Lexeme::If, r)),
                            "then" => tokens.push(Token(Lexeme::Then, r)),
                            "else" => tokens.push(Token(Lexeme::Else, r)),
                            "and" => tokens.push(Token(Lexeme::And, r)),
                            "or" => tokens.push(Token(Lexeme::Or, r)),
                            "xor" => tokens.push(Token(Lexeme::Xor, r)),
                            s => tokens.push(Token(Lexeme::Ident(s), r)),
                        }
                        next = false;
                        state = State::Default;
                    },
                },
                State::Number => match c {
                    c if c.is_alphabetic() || c.is_numeric() || c == '.' => {},
                    _ => {
                        let r = SrcRef::from((start, i));
                        let s = &code[start..i];
                        if let Ok(x) = s.parse() {
                            tokens.push(Token(Lexeme::Int(x), r));
                        } else if let Ok(x) = s.parse() {
                            tokens.push(Token(Lexeme::Float(x), r));
                        } else {
                            errors.push(LexError::invalid_number(s, r));
                        }
                        next = false;
                        state = State::Default;
                    },
                },
                State::Symbol => match c {
                    c if c.is_ascii_punctuation() && c != '_' && is_singular(c).is_none() => {},
                    _ => {
                        let r = SrcRef::from((start, i));
                        match &code[start..i] {
                            "=" => tokens.push(Token(Lexeme::Eq, r)),
                            "!=" => tokens.push(Token(Lexeme::NotEq, r)),
                            "<" => tokens.push(Token(Lexeme::Less, r)),
                            "<=" => tokens.push(Token(Lexeme::LessEq, r)),
                            ">" => tokens.push(Token(Lexeme::More, r)),
                            ">=" => tokens.push(Token(Lexeme::MoreEq, r)),
                            "+" => tokens.push(Token(Lexeme::Add, r)),
                            "-" => tokens.push(Token(Lexeme::Sub, r)),
                            "*" => tokens.push(Token(Lexeme::Mul, r)),
                            "/" => tokens.push(Token(Lexeme::Div, r)),
                            "%" => tokens.push(Token(Lexeme::Rem, r)),
                            "!" => tokens.push(Token(Lexeme::Not, r)),
                            ":" => tokens.push(Token(Lexeme::Colon, r)),
                            "->" => tokens.push(Token(Lexeme::Arrow, r)),
                            "@" => tokens.push(Token(Lexeme::Universe, r)),
                            s => errors.push(LexError::unknown_symbol(s, r)),
                        }
                        next = false;
                        state = State::Default;
                    },
                },
                State::String(buf) => match c {
                    '"' => {
                        tokens.push(Token(Lexeme::String(std::mem::replace(buf, String::new())), SrcRef::from((start, i))));
                        state = State::Default;
                    },
                    c => buf.push(c),
                },
                State::Comment => match c {
                    '\n' => state = State::Default,
                    _ => {},
                },
            }

            if next {
                chars.next();
            }
        }

        if errors.len() == 0 {
            Ok(TokenList(tokens))
        } else {
            Err(errors)
        }
    }

    pub fn tokens(&self) -> &[Token<'a>] {
        &self.0
    }

    pub fn parse<'b>(&'b self) -> Result<Ast<'a>, Vec<SyntaxError<'a, 'b>>>
        where 'b: 'a
    {
        Ast::parse(self).map_err(|err| vec![SyntaxError::Parse(err)])
    }
}
