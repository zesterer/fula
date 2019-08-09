use std::{
    rc::Rc,
    cell::RefCell,
    fmt,
};
use fula_syntax::{
    ast,
    src::SrcRef,
};
use crate::CompileError;

#[derive(Debug)]
pub struct Program<'a> {
    decls: Vec<Decl<'a>>,
}

pub struct IrNode<'a, T>(Box<T>, TypeInfo<'a>, SrcRef);

impl<'a, T> IrNode<'a, T> {
    pub fn parts_mut(&mut self) -> (&mut T, &mut TypeInfo<'a>, SrcRef) {
        (&mut self.0, &mut self.1, self.2)
    }

    pub fn inner(&self) -> &T {
        &self.0
    }

    pub fn type_info(&self) -> &TypeInfo<'a> {
        &self.1
    }

    pub fn type_info_mut(&mut self) -> &mut TypeInfo<'a> {
        &mut self.1
    }

    pub fn src_ref(&self) -> SrcRef {
        self.2
    }
}

impl<'a, T: fmt::Debug> fmt::Debug for IrNode<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if f.alternate() {
            write!(f, "{:#?} [{:?}, {:?}]", &self.0, self.1, self.2)
        } else {
            write!(f, "{:?} [{:?}, {:?}]", &self.0, self.1, self.2)
        }
    }
}

impl<'a, 'b, T, U: From<&'b T>> From<&'b ast::AstNode<T>> for IrNode<'a, U> {
    fn from(node: &'b ast::AstNode<T>) -> Self {
        Self(Box::new(node.inner().into()), TypeInfo::Unknown, node.src_ref())
    }
}

pub struct UntypedIrNode<T>(Box<T>, SrcRef);

impl<T> UntypedIrNode<T> {
    pub fn inner(&self) -> &T {
        &self.0
    }

    pub fn src_ref(&self) -> SrcRef {
        self.1
    }
}

impl<T: fmt::Debug> fmt::Debug for UntypedIrNode<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if f.alternate() {
            write!(f, "{:#?}", &self.0)
        } else {
            write!(f, "{:?}", &self.0)
        }
    }
}

impl<'b, T, U: From<&'b T>> From<&'b ast::AstNode<T>> for UntypedIrNode<U> {
    fn from(node: &'b ast::AstNode<T>) -> Self {
        Self(Box::new(node.inner().into()), node.src_ref())
    }
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
    Func(Box<TypeInfo<'a>>, Box<TypeInfo<'a>>),
    Named(&'a str),
    Derived(Rc<RefCell<TypeInfo<'a>>>),
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
    Bind(TypeInfo<'a>, UntypedIrNode<Pattern<'a>>, UntypedIrNode<Self>, IrNode<'a, Self>),
    Func(IrNode<'a, Pattern<'a>>, IrNode<'a, Self>),
    Call(IrNode<'a, Self>, IrNode<'a, Self>),
    Cast(IrNode<'a, Self>, UntypedIrNode<TypeInfo<'a>>),
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

#[derive(Clone, Debug)]
pub enum UnaryOp {
    Not,
    Neg,
}

#[derive(Clone, Debug)]
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

        match self.get_type_ambiguities() {
            errors if errors.len() > 0 => return Err(errors
                .into_iter()
                .map(|err| err.into())
                .collect()),
            _ => {},
        }

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

impl<'a, 'b> From<&'b ast::Decl<'a>> for Decl<'a> {
    fn from(decl: &'b ast::Decl<'a>) -> Self {
        match decl {
            ast::Decl::Const(name, ty, expr) =>
                Decl::Const(name, IrNode(Box::new(expr.inner().into()), ty.inner().into(), expr.src_ref())),
            ast::Decl::Data(ident, ty) => Decl::Data(ident, ty.inner().into()),
        }
    }
}

impl<'a, 'b> From<&'b ast::Type<'a>> for TypeInfo<'a> {
    fn from(ty: &'b ast::Type<'a>) -> Self {
        match ty {
            ast::Type::Unspecified => TypeInfo::Unknown,
            ast::Type::Universe => TypeInfo::Primitive(PrimitiveType::Universe),
            ast::Type::Func(param, ret) => TypeInfo::Func(Box::new(param.inner().into()), Box::new(ret.inner().into())),
            ast::Type::Ident("Bool") => TypeInfo::Primitive(PrimitiveType::Bool),
            ast::Type::Ident("Int") => TypeInfo::Primitive(PrimitiveType::Int),
            ast::Type::Ident("Float") => TypeInfo::Primitive(PrimitiveType::Float),
            ast::Type::Ident("String") => TypeInfo::Primitive(PrimitiveType::String),
            ast::Type::Ident(ident) => TypeInfo::Named(ident),
        }
    }
}

impl<'a, 'b> From<&'b ast::Pattern<'a>> for Pattern<'a> {
    fn from(pat: &'b ast::Pattern<'a>) -> Self {
        match pat {
            ast::Pattern::Ident(ident) => Pattern::Ident(ident),
        }
    }
}

impl<'a, 'b> From<&'b ast::Expr<'a>> for Expr<'a> {
    fn from(expr: &'b ast::Expr<'a>) -> Self {
        match expr {
            ast::Expr::Literal(litr) => Expr::Value(litr.into()),
            ast::Expr::Ident(ident) => Expr::Ident(ident),
            ast::Expr::Unary(op, a) => Expr::Unary(op.into(), a.into()),
            ast::Expr::Binary(op, a, b) => Expr::Binary(op.into(), a.into(), b.into()),
            ast::Expr::Ternary(op, a, b, c) => Expr::Ternary(op.into(), a.into(), b.into(), c.into()),
            ast::Expr::Bind(pat, expr, body) => Expr::Bind(TypeInfo::Unknown, pat.into(), expr.into(), body.into()),
            ast::Expr::Func(pat, body) => Expr::Func(pat.into(), body.into()),
            ast::Expr::Call(expr, a) => Expr::Call(expr.into(), a.into()),
            ast::Expr::Cast(expr, ty) => Expr::Cast(expr.into(), ty.into()),
        }
    }
}

impl<'a> From<&'a ast::Literal> for Value {
    fn from(litr: &'a ast::Literal) -> Self {
        match litr {
            ast::Literal::Bool(x) => Value::Bool(*x),
            ast::Literal::Int(x) => Value::Int(*x),
            ast::Literal::Float(x) => Value::Float(*x),
            ast::Literal::String(x) => Value::String(x.clone()),
        }
    }
}

impl Value {
    pub fn type_info<'a>(&self) -> TypeInfo<'a> {
        match self {
            Value::Bool(_) => TypeInfo::Primitive(PrimitiveType::Bool),
            Value::Int(_) => TypeInfo::Primitive(PrimitiveType::Int),
            Value::Float(_) => TypeInfo::Primitive(PrimitiveType::Float),
            Value::String(_) => TypeInfo::Primitive(PrimitiveType::String),
        }
    }
}

impl<'a> From<&'a ast::UnaryOp> for UnaryOp {
    fn from(op: &'a ast::UnaryOp) -> Self {
        match op {
            ast::UnaryOp::Not => UnaryOp::Not,
            ast::UnaryOp::Neg => UnaryOp::Neg,
        }
    }
}

impl<'a> From<&'a ast::BinaryOp> for BinaryOp {
    fn from(op: &'a ast::BinaryOp) -> Self {
        match op {
            ast::BinaryOp::Add => BinaryOp::Add,
            ast::BinaryOp::Sub => BinaryOp::Sub,
            ast::BinaryOp::Mul => BinaryOp::Mul,
            ast::BinaryOp::Div => BinaryOp::Div,
            ast::BinaryOp::Rem => BinaryOp::Rem,

            ast::BinaryOp::And => BinaryOp::And,
            ast::BinaryOp::Or => BinaryOp::Or,
            ast::BinaryOp::Xor => BinaryOp::Xor,

            ast::BinaryOp::Eq => BinaryOp::Eq,
            ast::BinaryOp::NotEq => BinaryOp::NotEq,
            ast::BinaryOp::Less => BinaryOp::Less,
            ast::BinaryOp::LessEq => BinaryOp::LessEq,
            ast::BinaryOp::More => BinaryOp::More,
            ast::BinaryOp::MoreEq => BinaryOp::MoreEq,
        }
    }
}

impl<'a> From<&'a ast::TernaryOp> for TernaryOp {
    fn from(op: &'a ast::TernaryOp) -> Self {
        match op {
            ast::TernaryOp::IfElse => TernaryOp::IfElse,
        }
    }
}
