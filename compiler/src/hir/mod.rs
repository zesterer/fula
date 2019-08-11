mod convert;
mod node;
mod visit;
mod infer;

use std::{
    rc::Rc,
    cell::RefCell,
    fmt,
};
use self::node::IrNode;

use fula_syntax::{
    ast,
    src::SrcRef,
};
use crate::CompileError;

#[derive(Debug)]
pub enum HirError<'a> {
    ExpectedType(SrcRef, Type<'a>, Type<'a>),
    TypeMismatch(SrcRef, Type<'a>, SrcRef, Type<'a>),
    InvalidBinary(SrcRef, BinaryOp, Type<'a>, Type<'a>),
    InvalidUnary(SrcRef, UnaryOp, Type<'a>),
    CannotFindIdent(SrcRef, &'a str),
}

impl<'a> HirError<'a> {
    pub fn get_src_refs(&self) -> Vec<SrcRef> {
        match self {
            HirError::ExpectedType(r, _, _) => vec![*r],
            HirError::TypeMismatch(r0, _, r1, _) => vec![*r0, *r1],
            HirError::InvalidBinary(r, _, _, _) => vec![*r],
            HirError::InvalidUnary(r, _, _) => vec![*r],
            HirError::CannotFindIdent(r, _) => vec![*r],
        }
    }
}

#[derive(Debug)]
pub struct Program<'a> {
    decls: Vec<Decl<'a>>,
}

#[derive(Debug)]
pub enum Decl<'a> {
    Const(&'a str, IrNode<'a, Expr<'a>>),
    Data(&'a str, TypeInfo<'a>),
}

#[derive(Clone)]
pub struct TypeInfo<'a> {
    ty: Rc<RefCell<Type<'a>>>,
    src_ref: SrcRef,
}

#[derive(Clone, Debug)]
pub enum Type<'a> {
    Unknown,
    Primitive(PrimitiveType),
    Func(TypeInfo<'a>, TypeInfo<'a>),
    List(TypeInfo<'a>),
    Named(&'a str),
}

#[derive(Clone, Debug, PartialEq)]
pub enum PrimitiveType {
    Universe,
    Bool,
    Int,
    Float,
    String,
}

#[derive(Debug)]
pub enum Expr<'a> {
    Value(Value),
    Ident(&'a str),
    Unary(UnaryOp, IrNode<'a, Self>),
    Binary(BinaryOp, IrNode<'a, Self>, IrNode<'a, Self>),
    Ternary(TernaryOp, IrNode<'a, Self>, IrNode<'a, Self>, IrNode<'a, Self>),
    Bind(IrNode<'a, Pattern<'a>>, IrNode<'a, Self>, IrNode<'a, Self>),
    Func(IrNode<'a, Pattern<'a>>, IrNode<'a, Self>),
    Call(IrNode<'a, Self>, IrNode<'a, Self>),
    List(Vec<IrNode<'a, Self>>),
}

#[derive(Debug)]
pub enum Pattern<'a> {
    Ident(&'a str),
}

#[derive(Debug)]
pub enum Value {
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
}

#[derive(Clone, Debug, PartialEq)]
pub enum UnaryOp {
    Not,
    Neg,
}

#[derive(Clone, Debug, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,

    And,
    Or,
    Xor,

    Eq,
    NotEq,
    Less,
    LessEq,
    More,
    MoreEq,
}

#[derive(Clone, Debug)]
pub enum TernaryOp {
    IfElse,
}

impl<'a> Program<'a> {
    pub fn new() -> Self {
        Self { decls: Vec::new() }
    }

    pub fn insert_decl(&mut self, decl: Decl<'a>) {
        self.decls.push(decl);
    }

    pub fn declarations(&self) -> impl Iterator<Item=&Decl<'a>> {
        self.decls.iter()
    }

    pub fn declarations_mut(&mut self) -> impl Iterator<Item=&mut Decl<'a>> {
        self.decls.iter_mut()
    }

    pub fn compile(&mut self) -> Result<(), Vec<CompileError<'a>>> {
        match self.infer_types() {
            errors if errors.len() > 0 => return Err(errors
                .into_iter()
                .map(|err| err.into())
                .collect()),
            _ => {},
        }

        /*
        match self.get_type_ambiguities() {
            errors if errors.len() > 0 => return Err(errors
                .into_iter()
                .map(|err| err.into())
                .collect()),
            _ => {},
        }
        */

        Ok(())
    }
}

impl<'a, 'b> From<&'b ast::Ast<'a>> for Program<'a> {
    fn from(ast: &'b ast::Ast<'a>) -> Self {
        let mut this = Self::new();

        for decl in ast.declarations() {
            this.insert_decl(decl.into());
        }

        this
    }
}

impl<'a> TypeInfo<'a> {
    pub fn new(ty: Type<'a>, src_ref: SrcRef) -> Self {
        Self {
            ty: Rc::new(RefCell::new(ty)),
            src_ref,
        }
    }

    pub fn unknown() -> Self {
        Self::new(Type::Unknown, SrcRef::None)
    }
}

impl<'a> fmt::Debug for TypeInfo<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if f.alternate() {
            write!(f, "{:#?}", &self.ty.borrow())
        } else {
            write!(f, "{:?}", &self.ty.borrow())
        }
    }
}
