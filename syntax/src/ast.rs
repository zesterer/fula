use std::{
    fmt,
    collections::VecDeque,
};
use crate::{
    src::SrcRef,
    parse::{ParseError, parse},
    token::TokenList,
};

pub struct AstNode<T>(Box<T>, SrcRef);

impl<T> AstNode<T> {
    pub fn inner(&self) -> &T {
        &self.0
    }

    pub fn into_inner(self) -> T {
        *self.0
    }

    pub fn src_ref(&self) -> SrcRef {
        self.1
    }
}

impl<T: fmt::Debug> fmt::Debug for AstNode<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if f.alternate() {
            write!(f, "{:#?}", &self.0)
        } else {
            write!(f, "{:?}", &self.0)
        }
    }
}

impl<T> From<(T, SrcRef)> for AstNode<T> {
    fn from((inner, r): (T, SrcRef)) -> Self {
        Self(inner.into(), r)
    }
}

#[derive(Debug)]
pub struct Ast<'a> {
    decls: Vec<Decl<'a>>,
}

#[derive(Debug)]
pub enum Decl<'a> {
    Const(&'a str, AstNode<Type<'a>>, AstNode<Expr<'a>>),
    Data(&'a str, AstNode<Type<'a>>),
}

#[derive(Debug)]
pub enum Expr<'a> {
    Literal(Literal),
    Ident(&'a str),
    Unary(UnaryOp, AstNode<Self>),
    Binary(BinaryOp, AstNode<Self>, AstNode<Self>),
    Ternary(TernaryOp, AstNode<Self>, AstNode<Self>, AstNode<Self>),
    Bind(AstNode<(Pattern<'a>, Type<'a>)>, AstNode<Self>, AstNode<Self>),
    Func(AstNode<(Pattern<'a>, Type<'a>)>, AstNode<Self>),
    Call(AstNode<Self>, AstNode<Self>),
    List(Vec<AstNode<Self>>),
}

#[derive(Debug)]
pub enum Pattern<'a> {
    Ident(&'a str),
}

#[derive(Debug)]
pub enum Type<'a> {
    Unspecified,
    Universe,
    Ident(&'a str),
    Func(AstNode<Self>, AstNode<Self>),
    List(AstNode<Self>),
}

impl<'a> Default for Type<'a> {
    fn default() -> Self {
        Type::Unspecified
    }
}

pub enum Literal {
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
}

#[derive(Debug)]
pub enum UnaryOp {
    Neg,
    Not,
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

impl<'a> Ast<'a> {
    pub fn new() -> Self {
        Self { decls: Vec::new() }
    }

    pub fn parse<'b: 'a>(token_list: &'b TokenList<'a>) -> Result<Self, ParseError<'a, 'b>> {
        parse(token_list.tokens())
    }

    pub fn insert_decl(&mut self, decl: Decl<'a>) {
        self.decls.push(decl);
    }

    pub fn declarations(&self) -> impl Iterator<Item=&Decl<'a>> {
        self.decls.iter()
    }
}

impl<'a> Expr<'a> {
    pub fn ident(s: &'a str, r: SrcRef) -> AstNode<Self> {
        AstNode(Expr::Ident(s).into(), r)
    }

    pub fn literal(litr: Literal, r: SrcRef) -> AstNode<Self> {
        AstNode(Expr::Literal(litr).into(), r)
    }

    pub fn func(
        mut params: VecDeque<AstNode<(Pattern<'a>, Type<'a>)>>,
        a: impl Into<AstNode<Expr<'a>>>,
    ) -> AstNode<Self> {
        let a = a.into();
        let r = a.src_ref();

        match params.pop_front() {
            Some(pat) => AstNode(Expr::Func(pat, Expr::func(params, a)).into(), r),
            None => a,
        }
    }

    pub fn call(
        a: impl Into<AstNode<Expr<'a>>>,
        mut args: VecDeque<AstNode<Expr<'a>>>,
    ) -> AstNode<Self> {
        let a = a.into();
        let r = a.src_ref();

        match args.pop_front() {
            Some(expr) => AstNode(Expr::Call(Expr::call(a, args), expr).into(), r),
            None => a,
        }
    }

    pub fn list(
        elements: VecDeque<AstNode<Expr<'a>>>,
        r: SrcRef,
    ) -> AstNode<Self> {
        AstNode(Expr::List(elements.into_iter().collect()).into(), r)
    }

    pub fn unary(
        op: UnaryOp,
        a: impl Into<AstNode<Expr<'a>>>,
    ) -> AstNode<Self> {
        let a = a.into();
        let r = a.src_ref();
        AstNode(Expr::Unary(op, a).into(), r)
    }

    pub fn binary(
        op: BinaryOp,
        a: impl Into<AstNode<Expr<'a>>>,
        b: impl Into<AstNode<Expr<'a>>>,
    ) -> AstNode<Self> {
        let (a, b) = (a.into(), b.into());
        let r = a.src_ref().union(b.src_ref());
        AstNode(Expr::Binary(op, a, b).into(), r)
    }

    pub fn ternary(
        op: TernaryOp,
        a: impl Into<AstNode<Expr<'a>>>,
        b: impl Into<AstNode<Expr<'a>>>,
        c: impl Into<AstNode<Expr<'a>>>,
    ) -> AstNode<Self> {
        let (a, b, c) = (a.into(), b.into(), c.into());
        let r = a.src_ref().union(b.src_ref()).union(c.src_ref());
        AstNode(Expr::Ternary(op, a, b, c).into(), r)
    }

    pub fn bind(
        pat: impl Into<AstNode<(Pattern<'a>, Type<'a>)>>,
        a: impl Into<AstNode<Expr<'a>>>,
        b: impl Into<AstNode<Expr<'a>>>,
    ) -> AstNode<Self> {
        let (pat, a, b) = (pat.into(), a.into(), b.into());
        let r = pat.src_ref().union(a.src_ref()).union(b.src_ref());
        AstNode(Expr::Bind(pat, a, b).into(), r)
    }
}

impl<'a> Pattern<'a> {
    pub fn ident(s: &'a str, r: SrcRef) -> AstNode<Self> {
        AstNode(Pattern::Ident(s).into(), r)
    }
}

impl<'a> Type<'a> {
    pub fn ident(s: &'a str, r: SrcRef) -> AstNode<Self> {
        AstNode(Type::Ident(s).into(), r)
    }

    pub fn unspecified(r: SrcRef) -> AstNode<Self> {
        AstNode(Type::Unspecified.into(), r)
    }

    pub fn universe(r: SrcRef) -> AstNode<Self> {
        AstNode(Type::Universe.into(), r)
    }

    pub fn func(
        param: impl Into<AstNode<Type<'a>>>,
        ret: impl Into<AstNode<Type<'a>>>,
    ) -> AstNode<Self> {
        let (param, ret) = (param.into(), ret.into());
        let r = param.src_ref().union(ret.src_ref());
        AstNode(Type::Func(param, ret).into(), r)
    }
}

impl fmt::Debug for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Literal::Bool(x) => write!(f, "{}", x),
            Literal::Int(x) => write!(f, "{}", x),
            Literal::Float(x) => write!(f, "{}", x),
            Literal::String(x) => write!(f, "{}", x),
        }
    }
}
