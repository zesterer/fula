mod convert;
mod node;
mod visit;
mod infer;

use self::node::IrNode;

use fula_syntax::{
    ast,
    src::SrcRef,
};
use crate::CompileError;

#[derive(Debug)]
pub enum HirError<'a> {
    ExpectedType(SrcRef, TypeInfo<'a>, TypeInfo<'a>),
    TypeMismatch(SrcRef, TypeInfo<'a>, SrcRef, TypeInfo<'a>),
    InvalidBinary(SrcRef, BinaryOp, TypeInfo<'a>, TypeInfo<'a>),
    InvalidUnary(SrcRef, UnaryOp, TypeInfo<'a>),
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

#[derive(Clone, Debug)]
pub enum TypeInfo<'a> {
    Unknown,
    Primitive(PrimitiveType),
    Func(Box<Self>, Box<Self>),
    List(Box<Self>),
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
