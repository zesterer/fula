use std::{
    fmt::Debug,
    collections::VecDeque,
};
use crate::{
    ast::{Ast, AstNode, Decl, Expr, Type, Literal, Pattern, UnaryOp, BinaryOp, TernaryOp},
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
    Decl,
    Pattern,
    Ident,
    Expr,
    Atom,
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

    let mut tokens = tokens.iter();
    loop {
        match tokens.clone().next().unwrap().lexeme() {
            Lexeme::Eof => break,
            _ => ast.insert_decl(parse_decl(&mut tokens)?),
        }
    }

    Ok(ast)
}

fn parse_decl<'a, 'b, I>(iter: &mut I) -> Result<Decl<'a>, ParseError<'a, 'b>>
    where 'b: 'a, I: Iterator<Item=&'b Token<'a>> + Clone + Debug
{
    try_parse(iter, |tok, iter| match tok.lexeme() {
        Lexeme::Const => try_parse(iter, |tok, iter| match tok.lexeme() {
            Lexeme::Ident(name) => try_parse(iter, |tok, iter| match tok.lexeme() {
                Lexeme::Colon => match parse_type(iter) {
                    Ok(ty) => try_parse(iter, |tok, iter| match tok.lexeme() {
                        Lexeme::Eq => match parse_expr(iter) {
                            Ok(body) => try_parse(iter, |tok, _| match tok.lexeme() {
                                Lexeme::Semicolon => Some(Ok(Decl::Const(name, ty, body))),
                                l => Some(Err(ParseError::expected(&Lexeme::Semicolon, l, tok.src_ref()))),
                            }),
                            Err(err) => Some(Err(err)),
                        },
                        l => Some(Err(ParseError::expected(&Lexeme::Eq, l, tok.src_ref()))),
                    }),
                    Err(err) => Some(Err(err)),
                },
                l => Some(Err(ParseError::expected(&Lexeme::Colon, l, tok.src_ref()))),
            }),
            l => Some(Err(ParseError::expected(Thing::Ident, l, tok.src_ref()))),
        }),
        l => Some(Err(ParseError::expected(Thing::Decl, l, tok.src_ref()))),
    })
        .unwrap()
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
                l => Some(Err(ParseError::expected(&Lexeme::RParen, l, tok.src_ref()))),
            }),
            Err(err) => Some(Err(err)),
        },
        Lexeme::Pipe => match parse_params(iter) {
            Ok(params) => try_parse(iter, |tok, iter| match tok.lexeme() {
                Lexeme::Pipe => match parse_expr(iter) {
                    Ok(body) => Some(Ok(Expr::func(params, body))),
                    Err(err) => Some(Err(err)),
                },
                l => Some(Err(ParseError::expected(&Lexeme::Pipe, l, tok.src_ref()))),
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
        Lexeme::Let => match parse_pattern(iter) {
            Ok(pat) => try_parse(iter, |tok, iter| match tok.lexeme() {
                Lexeme::Eq => match parse_expr(iter) {
                    Ok(a) => try_parse(iter, |tok, iter| match tok.lexeme() {
                        Lexeme::Semicolon => match parse_expr(iter) {
                            Ok(b) => Some(Ok(Expr::bind(pat, a, b))),
                            Err(err) => Some(Err(err)),
                        },
                        l => Some(Err(ParseError::expected(&Lexeme::Semicolon, l, tok.src_ref()))),
                    }),
                    Err(err) => Some(Err(err)),
                },
                l => Some(Err(ParseError::expected(&Lexeme::Eq, l, tok.src_ref()))),
            }),
            Err(err) => Some(Err(err)),
        },
        l => Some(Err(ParseError::expected(Thing::Atom, l, tok.src_ref()))),
    })
        .unwrap()
}

fn parse_params<'a, 'b, I>(iter: &mut I) -> Result<VecDeque<AstNode<Pattern<'a>>>, ParseError<'a, 'b>>
    where 'b: 'a, I: Iterator<Item=&'b Token<'a>> + Clone + Debug
{
    let mut args = VecDeque::new();

    loop {
        match attempt(iter, parse_pattern) {
            Ok(pat) => args.push_back(pat),
            Err(_) => break,
        }

        match try_parse(iter, |tok, _| match tok.lexeme() {
            Lexeme::Comma => Some(()),
            _ => None,
        }) {
            Some(_) => {},
            None => break,
        }
    }

    Ok(args)
}

fn parse_pattern<'a, 'b, I>(iter: &mut I) -> Result<AstNode<Pattern<'a>>, ParseError<'a, 'b>>
    where 'b: 'a, I: Iterator<Item=&'b Token<'a>> + Clone + Debug
{
    try_parse(iter, |tok, _| match tok.lexeme() {
        Lexeme::Ident(ident) => Some(Ok(Pattern::ident(ident, tok.src_ref()))),
        l => Some(Err(ParseError::expected(Thing::Pattern, l, tok.src_ref()))),
    })
        .unwrap()
}

fn parse_type<'a, 'b, I>(iter: &mut I) -> Result<AstNode<Type<'a>>, ParseError<'a, 'b>>
    where 'b: 'a, I: Iterator<Item=&'b Token<'a>> + Clone + Debug
{
    let mut ty = parse_singular_type(iter)?;

    loop {
        ty = match try_parse(iter, |tok, _| match tok.lexeme() {
            Lexeme::Arrow => Some(()),
            _ => None,
        }) {
            Some(()) => Type::func(ty, parse_type(iter)?),
            None => break Ok(ty),
        };
    }
}

fn parse_singular_type<'a, 'b, I>(iter: &mut I) -> Result<AstNode<Type<'a>>, ParseError<'a, 'b>>
    where 'b: 'a, I: Iterator<Item=&'b Token<'a>> + Clone + Debug
{
    try_parse(iter, |tok, _| match tok.lexeme() {
        Lexeme::Ident(ident) => Some(Ok(Type::ident(ident, tok.src_ref()))),
        Lexeme::Universe => Some(Ok(Type::universe(tok.src_ref()))),
        l => Some(Err(ParseError::expected(Thing::Pattern, l, tok.src_ref()))),
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

fn attempt<'a, 'c: 'a, I, R, E, F>(iter: &mut I, f: F) -> Result<R, E>
    where
        I: Iterator<Item=&'a Token<'c>> + Clone,
        F: FnOnce(&mut I) -> Result<R, E>,
{
    let mut iter2 = iter.clone();
    let tok = f(&mut iter2)?;
    *iter = iter2;
    Ok(tok)
}
