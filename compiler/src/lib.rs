#![feature(bind_by_move_pattern_guards)]

pub mod hir;
pub mod infer;

use infer::TypeError;

#[derive(Debug)]
pub enum CompileError<'a> {
    Type(TypeError<'a>),
}

impl<'a> From<TypeError<'a>> for CompileError<'a> {
    fn from(err: TypeError<'a>) -> Self {
        CompileError::Type(err)
    }
}
