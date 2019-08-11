use crate::{
    SyntaxError,
    src::SrcRef,
    ast::Ast,
};

#[derive(Debug)]
pub enum LexErrorKind<'a> {
    UnexpectedChar(char),
    InvalidNumber(&'a str),
    InvalidSymbol(&'a str),
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

    pub fn invalid_symbol(s: &'a str, src_ref: SrcRef) -> Self {
        Self { kind: LexErrorKind::InvalidSymbol(s), src_ref }
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
                        '|' => tokens.push(Token(Lexeme::Pipe, SrcRef::from(i))),
                        ',' => tokens.push(Token(Lexeme::Comma, SrcRef::from(i))),
                        ';' => tokens.push(Token(Lexeme::Semicolon, SrcRef::from(i))),
                        '(' => tokens.push(Token(Lexeme::LParen, SrcRef::from(i))),
                        ')' => tokens.push(Token(Lexeme::RParen, SrcRef::from(i))),
                        '[' => tokens.push(Token(Lexeme::LBrack, SrcRef::from(i))),
                        ']' => tokens.push(Token(Lexeme::RBrack, SrcRef::from(i))),
                        c if c.is_whitespace() => {},
                        c if c.is_alphabetic() || c == '_' => state = State::Word,
                        c if c.is_numeric() => state = State::Number,
                        c if c.is_ascii_punctuation() && c != '_' => state = State::Symbol,
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
                    c if c.is_ascii_punctuation() && c != '_' => {},
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
                            s => errors.push(LexError::invalid_symbol(s, r)),
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
