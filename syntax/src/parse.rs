use std::fmt::Debug;
use crate::{
    ast::{Ast, AstNode, Expr, Type, Literal, UnaryOp, BinaryOp, TernaryOp},
    token::{Token, Lexeme},
    src::SrcRef,
};

#[derive(Debug)]
pub enum ParseErrorKind<'a, 'b> {
    Expected {
        expected: Thing<'a, 'b>,
        found: Thing<'a, 'b>,
    },
}

#[derive(Debug)]
pub enum Thing<'a, 'b> {
    Atom,
    Expr,
    Lexeme(&'b Lexeme<'a>),
}

impl<'a, 'b> From<&'b Lexeme<'a>> for Thing<'a, 'b> {
    fn from(lexeme: &'b Lexeme<'a>) -> Self {
        Thing::Lexeme(lexeme)
    }
}

#[derive(Debug)]
pub struct ParseError<'a, 'b> {
    kind: ParseErrorKind<'a, 'b>,
    src_ref: SrcRef,
}

impl<'a, 'b> ParseError<'a, 'b> {
    pub fn expected(expected: impl Into<Thing<'a, 'b>>, found: impl Into<Thing<'a, 'b>>, src_ref: SrcRef) -> Self {
        Self {
            kind: ParseErrorKind::Expected {
                expected: expected.into(),
                found: found.into()
            },
            src_ref,
        }
    }
}

pub fn parse<'a, 'b>(tokens: &'b [Token<'a>]) -> Result<Ast<'a>, ParseError<'a, 'b>>
    where 'b: 'a,
{
    let mut ast = Ast::new();
    ast.insert_const("main", (Type::default(), SrcRef::none()), parse_expr(&mut tokens.iter())?);
    Ok(ast)
}

fn parse_expr<'a, 'b, I>(iter: &mut I) -> Result<AstNode<Expr<'a>>, ParseError<'a, 'b>>
    where 'b: 'a, I: Iterator<Item=&'b Token<'a>> + Clone + Debug
{
    parse_logical(iter)
}

fn parse_logical<'a, 'b, I>(iter: &mut I) -> Result<AstNode<Expr<'a>>, ParseError<'a, 'b>>
    where 'b: 'a, I: Iterator<Item=&'b Token<'a>> + Clone + Debug
{
    let mut expr = parse_comparison(iter)?;

    loop {
        expr = match try_parse(iter, |tok, _| match tok.lexeme() {
            Lexeme::And => Some(BinaryOp::And),
            Lexeme::Or => Some(BinaryOp::Or),
            Lexeme::Xor => Some(BinaryOp::Xor),
            _ => None,
        }) {
            Some(op) => Expr::binary(op, expr, parse_comparison(iter)?),
            None => break Ok(expr),
        };
    }
}

fn parse_comparison<'a, 'b, I>(iter: &mut I) -> Result<AstNode<Expr<'a>>, ParseError<'a, 'b>>
    where 'b: 'a, I: Iterator<Item=&'b Token<'a>> + Clone + Debug
{
    let mut expr = parse_addition(iter)?;

    loop {
        expr = match try_parse(iter, |tok, _| match tok.lexeme() {
            Lexeme::Eq => Some(BinaryOp::Eq),
            Lexeme::NotEq => Some(BinaryOp::NotEq),
            Lexeme::Less => Some(BinaryOp::Less),
            Lexeme::LessEq => Some(BinaryOp::LessEq),
            Lexeme::More => Some(BinaryOp::More),
            Lexeme::MoreEq => Some(BinaryOp::MoreEq),
            _ => None,
        }) {
            Some(op) => Expr::binary(op, expr, parse_addition(iter)?),
            None => break Ok(expr),
        };
    }
}

fn parse_addition<'a, 'b, I>(iter: &mut I) -> Result<AstNode<Expr<'a>>, ParseError<'a, 'b>>
    where 'b: 'a, I: Iterator<Item=&'b Token<'a>> + Clone + Debug
{
    let mut expr = parse_multiplication(iter)?;

    loop {
        expr = match try_parse(iter, |tok, _| match tok.lexeme() {
            Lexeme::Add => Some(BinaryOp::Add),
            Lexeme::Sub => Some(BinaryOp::Sub),
            _ => None,
        }) {
            Some(op) => Expr::binary(op, expr, parse_multiplication(iter)?),
            None => break Ok(expr),
        };
    }
}

fn parse_multiplication<'a, 'b, I>(iter: &mut I) -> Result<AstNode<Expr<'a>>, ParseError<'a, 'b>>
    where 'b: 'a, I: Iterator<Item=&'b Token<'a>> + Clone + Debug
{
    let mut expr = parse_unary(iter)?;

    loop {
        expr = match try_parse(iter, |tok, _| match tok.lexeme() {
            Lexeme::Mul => Some(BinaryOp::Mul),
            Lexeme::Div => Some(BinaryOp::Div),
            Lexeme::Rem => Some(BinaryOp::Rem),
            _ => None,
        }) {
            Some(op) => Expr::binary(op, expr, parse_unary(iter)?),
            None => break Ok(expr),
        };
    }
}

fn parse_unary<'a, 'b, I>(iter: &mut I) -> Result<AstNode<Expr<'a>>, ParseError<'a, 'b>>
    where 'b: 'a, I: Iterator<Item=&'b Token<'a>> + Clone + Debug
{
    match try_parse(iter, |tok, _| match tok.lexeme() {
        Lexeme::Not => Some(UnaryOp::Not),
        Lexeme::Sub => Some(UnaryOp::Neg),
        _ => None,
    }) {
        Some(op) => Ok(Expr::unary(op, parse_unary(iter)?)),
        None => parse_atom(iter),
    }
}

fn parse_atom<'a, 'b, I>(iter: &mut I) -> Result<AstNode<Expr<'a>>, ParseError<'a, 'b>>
    where 'b: 'a, I: Iterator<Item=&'b Token<'a>> + Clone + Debug
{
    try_parse(iter, |tok, iter| match tok.lexeme() {
        Lexeme::Ident(ident) => Some(Ok(Expr::ident(ident, tok.src_ref()))),
        Lexeme::True => Some(Ok(Expr::literal(Literal::Bool(true), tok.src_ref()))),
        Lexeme::False => Some(Ok(Expr::literal(Literal::Bool(false), tok.src_ref()))),
        Lexeme::Int(x) => Some(Ok(Expr::literal(Literal::Int(*x), tok.src_ref()))),
        Lexeme::Float(x) => Some(Ok(Expr::literal(Literal::Float(*x), tok.src_ref()))),
        Lexeme::String(x) => Some(Ok(Expr::literal(Literal::String(x.clone()), tok.src_ref()))),
        Lexeme::LParen => match parse_expr(iter) {
            Ok(inner) => try_parse(iter, |tok, _| match tok.lexeme() {
                Lexeme::RParen => Some(Ok(inner)),
                l => Some(Err(ParseError::expected(Thing::Expr, l, tok.src_ref()))),
            }),
            Err(err) => Some(Err(err)),
        },
        Lexeme::If => match parse_expr(iter) {
            Ok(predicate) => try_parse(iter, |tok, iter| match tok.lexeme() {
                Lexeme::Then => match parse_expr(iter) {
                    Ok(a) => try_parse(iter, |tok, iter| match tok.lexeme() {
                        Lexeme::Else => match parse_expr(iter) {
                            Ok(b) => Some(Ok(Expr::ternary(TernaryOp::IfElse, predicate, a, b))),
                            Err(err) => Some(Err(err)),
                        },
                        l => Some(Err(ParseError::expected(&Lexeme::Else, l, tok.src_ref()))),
                    }),
                    Err(err) => Some(Err(err)),
                },
                l => Some(Err(ParseError::expected(&Lexeme::Then, l, tok.src_ref()))),
            }),
            Err(err) => Some(Err(err)),
        },
        l => Some(Err(ParseError::expected(Thing::Atom, l, tok.src_ref()))),
    })
        .unwrap()
}

fn try_parse<'a, 'c: 'a, I, R, F>(iter: &mut I, f: F) -> Option<R>
    where
        I: Iterator<Item=&'a Token<'c>> + Clone,
        F: FnOnce(&'a Token<'c>, &mut I) -> Option<R>,
{
    let mut iter2 = iter.clone();
    let tok = iter2.next()?;

    let tok = f(tok, &mut iter2)?;
    *iter = iter2;
    Some(tok)
}
