use std::fmt;
use fula_syntax::{
    src::SrcRef,
    ast,
};
use super::{TypeInfo, Type};

pub struct IrNode<'a, T> {
    pub inner: Box<T>,
    pub type_info: TypeInfo<'a>,
    pub src_ref: SrcRef,
}

impl<'a, T> IrNode<'a, T> {
    pub fn new(inner: T, type_info: TypeInfo<'a>, src_ref: SrcRef) -> Self {
        Self {
            inner: Box::new(inner),
            type_info,
            src_ref,
        }
    }
}

impl<'a, T: fmt::Debug> fmt::Debug for IrNode<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if f.alternate() {
            write!(f, "{:#?}: {:?} {:?}", &self.inner, self.type_info, self.src_ref)
        } else {
            write!(f, "{:?} [{:?}, {:?}]", &self.inner, self.type_info, self.src_ref)
        }
    }
}

impl<'a, 'b, T, U: From<&'b T>> From<&'b ast::AstNode<T>> for IrNode<'a, U> {
    fn from(node: &'b ast::AstNode<T>) -> Self {
        Self::new(node.inner().into(), TypeInfo::new(Type::Unknown, node.src_ref()), node.src_ref())
    }
}
