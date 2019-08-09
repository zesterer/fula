use fula_syntax::src::SrcRef;
use crate::hir::*;

#[derive(Debug)]
pub enum TypeError {
    Ambiguity(SrcRef),
}

impl<'a> TypeInfo<'a> {
    pub fn is_ambiguous(&self) -> bool {
        match self {
            TypeInfo::Unknown => true,
            TypeInfo::Definitely(ty) => match ty.as_ref() {
                Type::Primitive(_) => false,
                Type::Func(param, ret) => param.is_ambiguous() || ret.is_ambiguous(),
                Type::Named(_) => false,
            },
        }
    }
}

impl<'a> Expr<'a> {
    fn get_type_ambiguities(&self, errors: &mut Vec<TypeError>) {
        match self {
            Expr::Value(_) => {},
            Expr::Ident(_) => {},
            Expr::Unary(_, a) => {
                a.get_type_ambiguities(errors);
            },
            Expr::Binary(_, a, b) => {
                a.get_type_ambiguities(errors);
                b.get_type_ambiguities(errors);
            },
            Expr::Ternary(_, a, b, c) => {
                a.get_type_ambiguities(errors);
                b.get_type_ambiguities(errors);
                c.get_type_ambiguities(errors);
            },
            Expr::Bind(_, _, expr, body) => {
                expr.inner().get_type_ambiguities(errors);
                body.get_type_ambiguities(errors);
            },
            Expr::Func(_, expr) => {
                expr.get_type_ambiguities(errors);
            },
            Expr::Call(expr, a) => {
                expr.get_type_ambiguities(errors);
                a.get_type_ambiguities(errors);
            },
            Expr::Cast(expr, _) => {
                expr.get_type_ambiguities(errors);
            },
        }
    }
}

impl<'a> IrNode<'a, Expr<'a>> {
    fn get_type_ambiguities(&self, errors: &mut Vec<TypeError>) {
        if self.type_info().is_ambiguous() {
            errors.push(TypeError::Ambiguity(self.src_ref()));
        } else {
            self.inner().get_type_ambiguities(errors);
        }
    }
}

impl<'a> Program<'a> {
    pub fn get_type_ambiguities(&self) -> Vec<TypeError> {
        let mut errors = Vec::new();

        for decl in self.declarations() {
            match decl {
                Decl::Const(_, expr) => expr.get_type_ambiguities(&mut errors),
                Decl::Data(_, _) => unimplemented!(),
            }
        }

        errors
    }
}
