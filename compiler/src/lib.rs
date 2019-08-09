pub mod hir;
pub mod infer;

use infer::TypeError;

#[derive(Debug)]
pub enum CompileError {
    Type(TypeError),
}

impl From<TypeError> for CompileError {
    fn from(err: TypeError) -> Self {
        CompileError::Type(err)
    }
}
