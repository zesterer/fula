use std::{
    fmt,
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
    Variant,
    Type,
    Ident,
    Expr,
    Atom,
    Lexeme(&'b Lexeme<'a>),
}

impl<'a, 'b> fmt::Display for Thing<'a, 'b> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Thing::Decl => write!(f, "declaration"),
            Thing::Pattern => write!(f, "pattern"),
            Thing::Variant => write!(f, "variant"),
            Thing::Type => write!(f, "type"),
            Thing::Ident => write!(f, "identifier"),
            Thing::Expr => write!(f, "expression"),
            Thing::Atom => write!(f, "expression"),
            Thing::Lexeme(l) => write!(f, "`{}`", l),
        }
    }
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

    pub fn get_text(&self) -> String {
        match &self.kind {
            ParseErrorKind::Expected { expected, found } => format!("Expected {}, found {}", expected, found),
        }
    }

    pub fn get_src_refs(&self) -> Vec<SrcRef> {
        vec![self.src_ref]
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
    where 'b: 'a, I: Iterator<Item=&'b Token<'a>> + Clone + fmt::Debug
{
    try_parse(iter, |tok, iter| match tok.lexeme() {
        Lexeme::Const => try_parse(iter, |ident_tok, iter| match ident_tok.lexeme() {
            Lexeme::Ident(name) => match parse_type_annotation(iter) {
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
            l => Some(Err(ParseError::expected(Thing::Ident, l, tok.src_ref()))),
        }),
        l => Some(Err(ParseError::expected(Thing::Decl, l, tok.src_ref()))),
    })
        .unwrap()
}

fn parse_expr<'a, 'b, I>(iter: &mut I) -> Result<AstNode<Expr<'a>>, ParseError<'a, 'b>>
    where 'b: 'a, I: Iterator<Item=&'b Token<'a>> + Clone + fmt::Debug
{
    parse_logical(iter)
}

fn parse_logical<'a, 'b, I>(iter: &mut I) -> Result<AstNode<Expr<'a>>, ParseError<'a, 'b>>
    where 'b: 'a, I: Iterator<Item=&'b Token<'a>> + Clone + fmt::Debug
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
    where 'b: 'a, I: Iterator<Item=&'b Token<'a>> + Clone + fmt::Debug
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
    where 'b: 'a, I: Iterator<Item=&'b Token<'a>> + Clone + fmt::Debug
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
    where 'b: 'a, I: Iterator<Item=&'b Token<'a>> + Clone + fmt::Debug
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
    where 'b: 'a, I: Iterator<Item=&'b Token<'a>> + Clone + fmt::Debug
{
    match try_parse(iter, |tok, _| match tok.lexeme() {
        Lexeme::Not => Some(UnaryOp::Not),
        Lexeme::Sub => Some(UnaryOp::Neg),
        _ => None,
    }) {
        Some(op) => Ok(Expr::unary(op, parse_unary(iter)?)),
        None => parse_call(iter),
    }
}

fn parse_call<'a, 'b, I>(iter: &mut I) -> Result<AstNode<Expr<'a>>, ParseError<'a, 'b>>
    where 'b: 'a, I: Iterator<Item=&'b Token<'a>> + Clone + fmt::Debug
{
    let mut expr = parse_atom(iter)?;

    loop {
        expr = match attempt(iter, |iter| try_parse(iter, |tok, iter| match tok.lexeme() {
            Lexeme::LParen => match parse_args(iter) {
                Ok(args) => try_parse(iter, |tok_r, _| match tok_r.lexeme() {
                    Lexeme::RParen => Some(Ok(args)),
                    l => Some(Err(ParseError::expected(&Lexeme::RParen, l, tok_r.src_ref()))),
                }),
                Err(err) => Some(Err(err)),
            },
            l => Some(Err(ParseError::expected(&Lexeme::LParen, l, tok.src_ref()))),
        }).unwrap()) {
            Some(args) => Expr::call(expr, args),
            None => break Ok(expr),
        };
    }
}

fn parse_atom<'a, 'b, I>(iter: &mut I) -> Result<AstNode<Expr<'a>>, ParseError<'a, 'b>>
    where 'b: 'a, I: Iterator<Item=&'b Token<'a>> + Clone + fmt::Debug
{
    try_parse(iter, |tok, iter| match tok.lexeme() {
        Lexeme::Ident(ident) => Some(Ok(Expr::ident(ident, tok.src_ref()))),
        Lexeme::True => Some(Ok(Expr::literal(Literal::Bool(true), tok.src_ref()))),
        Lexeme::False => Some(Ok(Expr::literal(Literal::Bool(false), tok.src_ref()))),
        Lexeme::Int(x) => Some(Ok(Expr::literal(Literal::Int(*x), tok.src_ref()))),
        Lexeme::Float(x) => Some(Ok(Expr::literal(Literal::Float(*x), tok.src_ref()))),
        Lexeme::String(x) => Some(Ok(Expr::literal(Literal::String(x.clone()), tok.src_ref()))),
        Lexeme::LParen => match parse_args(iter) {
            Ok(mut fields) => try_parse(iter, |tok_r, _| match tok_r.lexeme() {
                Lexeme::RParen => {
                    let r = tok.src_ref().union(tok_r.src_ref());
                    Some(Ok(match fields.len() {
                        1 => fields.remove(0).unwrap(),
                        _ => Expr::tuple(fields, r),
                    }))
                },
                l => Some(Err(ParseError::expected(&Lexeme::RParen, l, tok_r.src_ref()))),
            }),
            Err(err) => Some(Err(err)),
        },
        Lexeme::LBrack => match parse_args(iter) {
            Ok(elements) => try_parse(iter, |tok_r, _| match tok_r.lexeme() {
                Lexeme::RBrack => {
                    let r = elements
                        .iter()
                        .fold(tok.src_ref(), |a, e| a.union(e.src_ref()))
                        .union(tok_r.src_ref());
                    Some(Ok(Expr::list(elements, r)))
                },
                l => Some(Err(ParseError::expected(&Lexeme::RBrack, l, tok_r.src_ref()))),
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

fn parse_args<'a, 'b, I>(iter: &mut I) -> Result<VecDeque<AstNode<Expr<'a>>>, ParseError<'a, 'b>>
    where 'b: 'a, I: Iterator<Item=&'b Token<'a>> + Clone + fmt::Debug
{
    let mut args = VecDeque::new();

    loop {
        match attempt(iter, parse_expr) {
            Some(pat) => args.push_back(pat),
            None => break,
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

fn parse_params<'a, 'b, I>(iter: &mut I) -> Result<VecDeque<AstNode<(Pattern<'a>, Type<'a>)>>, ParseError<'a, 'b>>
    where 'b: 'a, I: Iterator<Item=&'b Token<'a>> + Clone + fmt::Debug
{
    let mut params = VecDeque::new();

    loop {
        match attempt(iter, parse_pattern) {
            Some(pat) => params.push_back(pat),
            None => break,
        }

        match try_parse(iter, |tok, _| match tok.lexeme() {
            Lexeme::Comma => Some(()),
            _ => None,
        }) {
            Some(_) => {},
            None => break,
        }
    }

    Ok(params)
}

fn parse_type_list<'a, 'b, I>(iter: &mut I) -> Result<VecDeque<AstNode<Type<'a>>>, ParseError<'a, 'b>>
    where 'b: 'a, I: Iterator<Item=&'b Token<'a>> + Clone + fmt::Debug
{
    let mut types = VecDeque::new();

    loop {
        match attempt(iter, parse_type) {
            Some(pat) => types.push_back(pat),
            None => break,
        }

        match try_parse(iter, |tok, _| match tok.lexeme() {
            Lexeme::Comma => Some(()),
            _ => None,
        }) {
            Some(_) => {},
            None => break,
        }
    }

    Ok(types)
}

fn parse_variant<'a, 'b, I>(iter: &mut I) -> Result<(&'a str, AstNode<Type<'a>>), ParseError<'a, 'b>>
    where 'b: 'a, I: Iterator<Item=&'b Token<'a>> + Clone + fmt::Debug
{
    try_parse(iter, |tok_name, iter| match tok_name.lexeme() {
        Lexeme::Ident(name) => try_parse_or(iter, |tok, iter| match tok.lexeme() {
            Lexeme::Colon => Ok(parse_type(iter).map(|ty| (*name, ty))),
            _ => Err(Ok((name, Type::unit(tok_name.src_ref())))),
        }),
        l => Some(Err(ParseError::expected(Thing::Variant, l, tok_name.src_ref()))),
    })
        .unwrap()
}

fn parse_variant_list<'a, 'b, I>(iter: &mut I) -> Result<VecDeque<(&'a str, AstNode<Type<'a>>)>, ParseError<'a, 'b>>
    where 'b: 'a, I: Iterator<Item=&'b Token<'a>> + Clone + fmt::Debug
{
    let mut variants = VecDeque::new();

    loop {
        match attempt(iter, parse_variant) {
            Some(pat) => variants.push_back(pat),
            None => break,
        }

        match try_parse(iter, |tok, _| match tok.lexeme() {
            Lexeme::Comma => Some(()),
            _ => None,
        }) {
            Some(_) => {},
            None => break,
        }
    }

    Ok(variants)
}

fn parse_pattern<'a, 'b, I>(iter: &mut I) -> Result<AstNode<(Pattern<'a>, Type<'a>)>, ParseError<'a, 'b>>
    where 'b: 'a, I: Iterator<Item=&'b Token<'a>> + Clone + fmt::Debug
{
    let pat = try_parse(iter, |tok, _| match tok.lexeme() {
        Lexeme::Ident(ident) => Some(Ok(Pattern::ident(ident, tok.src_ref()))),
        l => Some(Err(ParseError::expected(Thing::Pattern, l, tok.src_ref()))),
    })
        .unwrap()?;

    match parse_type_annotation(iter) {
        Ok(ty) => {
            let r = pat.src_ref();
            Ok(((pat.into_inner(), ty.into_inner()), r).into())
        },
        Err(_) => panic!(),
    }
}

fn parse_type<'a, 'b, I>(iter: &mut I) -> Result<AstNode<Type<'a>>, ParseError<'a, 'b>>
    where 'b: 'a, I: Iterator<Item=&'b Token<'a>> + Clone + fmt::Debug
{
    let ty = parse_type_atom(iter)?;

    match try_parse(iter, |tok, _| match tok.lexeme() {
        Lexeme::Arrow => Some(()),
        _ => None,
    }) {
        Some(()) => Ok(Type::func(ty, parse_type(iter)?)),
        None => Ok(ty),
    }
}

fn parse_type_atom<'a, 'b, I>(iter: &mut I) -> Result<AstNode<Type<'a>>, ParseError<'a, 'b>>
    where 'b: 'a, I: Iterator<Item=&'b Token<'a>> + Clone + fmt::Debug
{
    try_parse(iter, |tok, iter| match tok.lexeme() {
        Lexeme::Ident("_") => Some(Ok(Type::unspecified(tok.src_ref()))),
        Lexeme::Ident(ident) => Some(Ok(Type::ident(ident, tok.src_ref()))),
        Lexeme::Universe => Some(Ok(Type::universe(tok.src_ref()))),
        Lexeme::LParen => match parse_type_list(iter) {
            Ok(mut fields) => try_parse(iter, |tok_r, _| match tok_r.lexeme() {
                Lexeme::RParen => {
                    let r = tok.src_ref().union(tok_r.src_ref());
                    Some(Ok(match fields.len() {
                        0 => Type::unit(r),
                        1 => fields.remove(0).unwrap(),
                        _ => Type::tuple(fields.into_iter().collect(), r),
                    }))
                },
                l => Some(Err(ParseError::expected(&Lexeme::RParen, l, tok_r.src_ref()))),
            }),
            Err(err) => Some(Err(err)),
        },
        Lexeme::LBrace => match parse_variant_list(iter) {
            Ok(variants) => try_parse(iter, |tok_r, _| match tok_r.lexeme() {
                Lexeme::RBrace => {
                    let r = tok.src_ref().union(tok_r.src_ref());
                    Some(Ok(Type::sum(variants.into_iter().collect(), r)))
                },
                l => Some(Err(ParseError::expected(&Lexeme::RBrace, l, tok_r.src_ref()))),
            }),
            Err(err) => Some(Err(err)),
        },
        Lexeme::LBrack => match parse_type(iter) {
            Ok(ty) => try_parse(iter, |tok_r, _| match tok_r.lexeme() {
                Lexeme::RBrack => {
                    let r = ty.src_ref().union(tok.src_ref()).union(tok_r.src_ref());
                    Some(Ok((Type::List(ty), r).into()))
                },
                l => Some(Err(ParseError::expected(&Lexeme::RBrack, l, tok_r.src_ref()))),
            }),
            Err(err) => Some(Err(err)),
        },
        l => Some(Err(ParseError::expected(Thing::Type, l, tok.src_ref()))),
    })
        .unwrap()
}

fn parse_type_annotation<'a, 'b, I>(iter: &mut I) -> Result<AstNode<Type<'a>>, ParseError<'a, 'b>>
    where 'b: 'a, I: Iterator<Item=&'b Token<'a>> + Clone + fmt::Debug
{
    try_parse(iter, |tok, iter| match tok.lexeme() {
        Lexeme::Colon => Some(parse_type(iter)),
        _ => None,
    })
        .unwrap_or(Ok((Type::Unspecified, SrcRef::None).into()))
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

fn try_parse_or<'a, 'c: 'a, I, R, F>(iter: &mut I, f: F) -> Option<R>
    where
        I: Iterator<Item=&'a Token<'c>> + Clone,
        F: FnOnce(&'a Token<'c>, &mut I) -> Result<R, R>,
{
    let mut iter2 = iter.clone();
    let tok = iter2.next()?;

    Some(match f(tok, &mut iter2) {
        Ok(tok) => {
            *iter = iter2;
            tok
        },
        Err(tok) => tok,
    })
}

fn attempt<'a, 'c: 'a, I, R, E, F>(iter: &mut I, f: F) -> Option<R>
    where
        I: Iterator<Item=&'a Token<'c>> + Clone,
        F: FnOnce(&mut I) -> Result<R, E>,
{
    let mut iter2 = iter.clone();
    let tok = f(&mut iter2).ok()?;
    *iter = iter2;
    Some(tok)
}
