#![feature(bind_by_move_pattern_guards)]

pub mod hir;
use hir::HirError;
//pub mod infer;

#[derive(Debug)]
pub enum CompileError<'a> {
    Hir(HirError<'a>),
}

impl<'a> From<HirError<'a>> for CompileError<'a> {
    fn from(err: HirError<'a>) -> Self {
        CompileError::Hir(err)
    }
}
