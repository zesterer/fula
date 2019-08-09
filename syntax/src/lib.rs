pub mod token;
pub mod ast;
pub mod src;
pub mod parse;

use crate::{
    token::{LexError, TokenList},
    parse::ParseError,
};

#[derive(Debug)]
pub enum SyntaxError<'a, 'b> {
    Lex(LexError<'a>),
    Parse(ParseError<'a, 'b>),
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
