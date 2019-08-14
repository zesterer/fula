mod convert;
mod node;
mod visit;
mod infer;
mod gen;

use std::{
    rc::Rc,
    cell::RefCell,
    fmt,
    ops::Deref,
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
    TypeAmbiguity(SrcRef),
    InvalidBinary(SrcRef, BinaryOp, Type<'a>, Type<'a>),
    InvalidUnary(SrcRef, UnaryOp, Type<'a>),
    CannotFindIdent(SrcRef, &'a str),
}

impl<'a> HirError<'a> {
    pub fn get_text(&self) -> String {
        match self {
            HirError::ExpectedType(_, ty0, ty1) => format!("Expected type '{}' but found type '{}'", ty0, ty1),
            HirError::TypeMismatch(_, ty0, _, ty1) => format!("Type '{}' is incompatible with type '{}'", ty0, ty1),
            HirError::TypeAmbiguity(_) => format!("Type could not be inferred and is ambiguous"),
            HirError::InvalidBinary(_, op, ty0, ty1) => format!("Could not apply binary operation '{}' to operands of types '{}' and '{}'", op, ty0, ty1),
            HirError::InvalidUnary(_, op, ty0) => format!("Could not apply unary operation '{}' to operand of type '{}'", op, ty0),
            HirError::CannotFindIdent(_, ident) => format!("'{}' was not found within the current scope", ident),
        }
    }

    pub fn get_src_refs(&self) -> Vec<SrcRef> {
        match self {
            HirError::ExpectedType(r, _, _) => vec![*r],
            HirError::TypeMismatch(r0, _, r1, _) => vec![*r0, *r1],
            HirError::TypeAmbiguity(r) => vec![*r],
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
    Tuple(Vec<TypeInfo<'a>>),
    Unit,
    Singular(TypeInfo<'a>),
    Sum {
        fixed: bool,
        variants: Vec<TypeInfo<'a>>,
    },
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
    Unit,
    Singular(IrNode<'a, Self>),
    Tuple(Vec<IrNode<'a, Self>>),
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

    fn get_type_ambiguities(&self) -> Vec<HirError<'a>> {
        let mut errors = Vec::new();

        let mut visitor = |type_info: &TypeInfo| match type_info.ty.borrow().deref() {
            Type::Unknown | Type::Sum { fixed: false, .. } => Err(HirError::TypeAmbiguity(type_info.src_ref)),
            _ => Ok(()),
        };

        for decl in self.declarations() {
            match decl {
                Decl::Const(_, val) => match val.type_info.visit_type_info(&mut visitor) {
                    Ok(()) => {},
                    Err(err) => errors.push(err),
                },
                Decl::Data(_, _) => unimplemented!(),
            }
        }

        errors
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

impl<'a> TypeInfo<'a> {
    pub fn new(ty: Type<'a>, src_ref: SrcRef) -> Self {
        Self {
            ty: Rc::new(RefCell::new(ty)),
            src_ref,
        }
    }

    pub fn unknown(r: SrcRef) -> Self {
        Self::new(Type::Unknown, r)
    }
}

impl<'a> fmt::Debug for TypeInfo<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.ty.borrow())
    }
}

impl<'a> fmt::Display for Type<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Unknown => write!(f, "_"),
            Type::Primitive(PrimitiveType::Universe) => write!(f, "@"),
            Type::Primitive(PrimitiveType::Bool) => write!(f, "Bool"),
            Type::Primitive(PrimitiveType::Int) => write!(f, "Int"),
            Type::Primitive(PrimitiveType::Float) => write!(f, "Float"),
            Type::Primitive(PrimitiveType::String) => write!(f, "String"),
            Type::Func(x, y) => write!(f, "{} -> {}", x.ty.borrow(), y.ty.borrow()),
            Type::List(ty) => write!(f, "[{}]", ty.ty.borrow()),
            Type::Tuple(tys) => write!(f, "{{ {} }}", tys.iter().map(|ty| format!("{}", ty.ty.borrow())).collect::<Vec<_>>().join(", ")),
            Type::Unit => write!(f, "{{}}"),
            Type::Singular(ty) => write!(f, "{{ {} }}", ty.ty.borrow()),
            Type::Sum { variants, .. } => write!(f, "{{ {} }}", variants.iter().map(|ty| format!("{}", ty.ty.borrow())).collect::<Vec<_>>().join(" | ")),
            Type::Named(name) => write!(f, "{}", name),
        }
    }
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UnaryOp::Neg => write!(f, "-"),
            UnaryOp::Not => write!(f, "!"),
        }
    }
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BinaryOp::Add => write!(f, "+"),
            BinaryOp::Sub => write!(f, "-"),
            BinaryOp::Mul => write!(f, "*"),
            BinaryOp::Div => write!(f, "/"),
            BinaryOp::Rem => write!(f, "%"),

            BinaryOp::And => write!(f, "and"),
            BinaryOp::Or => write!(f, "or"),
            BinaryOp::Xor => write!(f, "xor"),

            BinaryOp::Eq => write!(f, "="),
            BinaryOp::NotEq => write!(f, "!="),
            BinaryOp::Less => write!(f, "<"),
            BinaryOp::LessEq => write!(f, "<="),
            BinaryOp::More => write!(f, ">"),
            BinaryOp::MoreEq => write!(f, ">="),
        }
    }
}
