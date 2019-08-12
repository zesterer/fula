#![feature(bind_by_move_pattern_guards)]

pub mod hir;

use fula_syntax::src::SrcRef;
use crate::hir::HirError;

#[derive(Debug)]
pub enum CompileError<'a> {
    Hir(HirError<'a>),
}

impl<'a> CompileError<'a> {
    pub fn get_text(&self) -> String {
        match self {
            CompileError::Hir(hir_err) => hir_err.get_text(),
        }
    }

    pub fn get_src_refs(&self) -> Vec<SrcRef> {
        match self {
            CompileError::Hir(hir_err) => hir_err.get_src_refs(),
        }
    }
}

impl<'a> From<HirError<'a>> for CompileError<'a> {
    fn from(err: HirError<'a>) -> Self {
        CompileError::Hir(err)
    }
}
