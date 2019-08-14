use super::*;

impl<'a, 'b> From<&'b ast::Decl<'a>> for Decl<'a> {
    fn from(decl: &'b ast::Decl<'a>) -> Self {
        match decl {
            ast::Decl::Const(name, ty, expr) =>
                Decl::Const(name, IrNode::new(expr.inner().into(), TypeInfo::new(ty.inner().into(), ty.src_ref()), expr.src_ref())),
            ast::Decl::Data(ident, ty) => Decl::Data(ident, TypeInfo::new(ty.inner().into(), ty.src_ref())),
        }
    }
}

impl<'a, 'b> From<&'b ast::Type<'a>> for Type<'a> {
    fn from(ty: &'b ast::Type<'a>) -> Self {
        match ty {
            ast::Type::Unspecified => Type::Unknown,
            ast::Type::Universe => Type::Primitive(PrimitiveType::Universe),
            ast::Type::Ident("Bool") => Type::Primitive(PrimitiveType::Bool),
            ast::Type::Ident("Int") => Type::Primitive(PrimitiveType::Int),
            ast::Type::Ident("Float") => Type::Primitive(PrimitiveType::Float),
            ast::Type::Ident("String") => Type::Primitive(PrimitiveType::String),
            ast::Type::Ident(ident) => Type::Named(ident),
            ast::Type::Func(param, ret) => Type::Func(
                TypeInfo::new(param.inner().into(), param.src_ref()),
                TypeInfo::new(ret.inner().into(), ret.src_ref()),
            ),
            ast::Type::List(ty) => Type::List(TypeInfo::new(ty.inner().into(), ty.src_ref())),
            ast::Type::Unit => Type::Unit,
            ast::Type::Singular(ty) => Type::Singular(TypeInfo::new(ty.inner().into(), ty.src_ref()).into()),
            ast::Type::Tuple(fields) => Type::Tuple(fields.iter().map(|ty| TypeInfo::new(ty.inner().into(), ty.src_ref())).collect()),
            ast::Type::Sum(variants) => Type::Sum {
                fixed: true,
                variants: variants.iter().map(|ty| TypeInfo::new(ty.inner().into(), ty.src_ref())).collect(),
            },
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
            ast::Expr::Bind(pat, expr, body) => {
                let pat = IrNode::new((&pat.inner().0).into(), TypeInfo::new((&pat.inner().1).into(), pat.src_ref()), pat.src_ref());
                Expr::Bind(pat, expr.into(), body.into())
            },
            ast::Expr::Func(pat, body) => {
                let pat = IrNode::new((&pat.inner().0).into(), TypeInfo::new((&pat.inner().1).into(), pat.src_ref()), pat.src_ref());
                Expr::Func(pat, body.into())
            },
            ast::Expr::Call(expr, a) => Expr::Call(expr.into(), a.into()),
            ast::Expr::List(elements) => Expr::List(elements.iter().map(|e| e.into()).collect()),
            ast::Expr::Unit => Expr::Unit,
            ast::Expr::Singular(expr) => Expr::Singular(expr.into()),
            ast::Expr::Tuple(elements) => Expr::Tuple(elements.iter().map(|e| e.into()).collect()),
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
