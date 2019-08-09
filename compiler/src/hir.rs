use std::rc::Rc;
use fula_syntax::{
    ast,
    src::SrcRef,
};

#[derive(Debug)]
pub struct Program<'a> {
    decls: Vec<Decl<'a>>,
}

#[derive(Debug)]
pub struct IrNode<'a, T>(Box<T>, SrcRef, TypeInfo<'a>);

#[derive(Debug)]
pub struct UntypedIrNode<T>(Box<T>, SrcRef);

#[derive(Debug)]
pub enum Decl<'a> {
    Const(&'a str, IrNode<'a, Expr<'a>>),
    Data(&'a str, TypeInfo<'a>),
}

#[derive(Debug)]
pub enum TypeInfo<'a> {
    Unknown,
    Derived(Rc<TypeInfo<'a>>),
    Definitely(Type<'a>),
}

#[derive(Debug)]
pub enum Type<'a> {
    Primitive(PrimitiveType),
    Func(Box<TypeInfo<'a>>, Box<TypeInfo<'a>>),
    Named(&'a str),
}

#[derive(Debug)]
pub enum PrimitiveType {
    Bool,
    Int,
    Float,
    Universe,
}

#[derive(Debug)]
pub enum Expr<'a> {
    Value(Value),
    Ident(&'a str),
    UnaryOp(UnaryOp, IrNode<'a, Self>),
    BinaryOp(BinaryOp, IrNode<'a, Self>, IrNode<'a, Self>),
    TernaryOp(TernaryOp, IrNode<'a, Self>, IrNode<'a, Self>),
    Bind(TypeInfo<'a>, UntypedIrNode<Pattern<'a>>, UntypedIrNode<Self>),
    Func(IrNode<'a, Pattern<'a>>, IrNode<'a, Self>),
    Call(IrNode<'a, Self>, IrNode<'a, Self>),
    Cast(IrNode<'a, Self>, TypeInfo<'a>),
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

#[derive(Debug)]
pub enum UnaryOp {
    Not,
    Neg,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub enum TernaryOp {
    IfElse,
}

impl<'a> Program<'a> {
    pub fn new() -> Self {
        Self { decls: Vec::new() }
    }
}

impl<'a, 'b> From<&'b ast::Ast<'a>> for Program<'a> {
    fn from(ast: &'b ast::Ast<'a>) -> Self {
        let mut this = Self::new();

        for decl in ast.declarations() {
            unimplemented!();
        }

        this
    }
}
