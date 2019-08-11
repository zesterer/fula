pub mod token;
pub mod ast;
pub mod src;
pub mod parse;

use crate::{
    token::{LexError, TokenList},
    parse::ParseError,
    src::SrcRef,
};

#[derive(Debug)]
pub enum SyntaxError<'a, 'b> {
    Lex(LexError<'a>),
    Parse(ParseError<'a, 'b>),
}

impl<'a, 'b> SyntaxError<'a, 'b> {
    pub fn get_src_refs(&self) -> Vec<SrcRef> {
        match self {
            SyntaxError::Lex(lex_err) => lex_err.get_src_refs(),
            SyntaxError::Parse(parse_err) => parse_err.get_src_refs(),
        }
    }
}

pub fn lex(code: &str) -> Result<TokenList, Vec<SyntaxError>> {
    match TokenList::lex(code) {
        Ok(tokens) => Ok(tokens),
        Err(errors) => Err(errors
            .into_iter()
            .map(|err| SyntaxError::Lex(err))
            .collect()),
    }
}
