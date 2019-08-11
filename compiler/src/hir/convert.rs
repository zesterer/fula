use super::*;

impl<'a, 'b> From<&'b ast::Decl<'a>> for Decl<'a> {
    fn from(decl: &'b ast::Decl<'a>) -> Self {
        match decl {
            ast::Decl::Const(name, ty, expr) =>
                Decl::Const(name, IrNode::new(expr.inner().into(), ty.inner().into(), expr.src_ref())),
            ast::Decl::Data(ident, ty) => Decl::Data(ident, ty.inner().into()),
        }
    }
}

impl<'a, 'b> From<&'b ast::Type<'a>> for TypeInfo<'a> {
    fn from(ty: &'b ast::Type<'a>) -> Self {
        match ty {
            ast::Type::Unspecified => TypeInfo::Unknown,
            ast::Type::Universe => TypeInfo::Primitive(PrimitiveType::Universe),
            ast::Type::Ident("Bool") => TypeInfo::Primitive(PrimitiveType::Bool),
            ast::Type::Ident("Int") => TypeInfo::Primitive(PrimitiveType::Int),
            ast::Type::Ident("Float") => TypeInfo::Primitive(PrimitiveType::Float),
            ast::Type::Ident("String") => TypeInfo::Primitive(PrimitiveType::String),
            ast::Type::Ident(ident) => TypeInfo::Named(ident),
            ast::Type::Func(param, ret) => TypeInfo::Func(Box::new(param.inner().into()), Box::new(ret.inner().into())),
            ast::Type::List(ty) => TypeInfo::List(Box::new(ty.inner().into())),
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
                let pat = IrNode::new((&pat.inner().0).into(), (&pat.inner().1).into(), pat.src_ref());
                Expr::Bind(pat, expr.into(), body.into())
            },
            ast::Expr::Func(pat, body) => {
                let pat = IrNode::new((&pat.inner().0).into(), (&pat.inner().1).into(), pat.src_ref());
                Expr::Func(pat, body.into())
            },
            ast::Expr::Call(expr, a) => Expr::Call(expr.into(), a.into()),
            ast::Expr::List(elements) => Expr::List(elements.iter().map(|e| e.into()).collect()),
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
