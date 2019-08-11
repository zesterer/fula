use std::ops::Deref;
use super::*;

impl<'a> TypeInfo<'a> {
    pub fn visit_type_info<R>(&self, f: &mut impl FnMut(&TypeInfo<'a>) -> Result<(), R>) -> Result<(), R> {
        f(self)?;

        match self.ty.borrow().deref() {
            Type::Func(a, b) => {
                a.visit_type_info(f)?;
                b.visit_type_info(f)?;
            },
            Type::List(a) => {
                a.visit_type_info(f)?;
            },
            _ => {},
        }

        Ok(())
    }
}

impl<'a> IrNode<'a, Expr<'a>> {
    pub fn visit<R>(&self, mut f: impl FnMut(&IrNode<'a, Expr<'a>>) -> Result<(), R>) -> Result<(), R> {
        f(self)?;

        match self.inner.as_ref() {
            Expr::Unary(_, a) => {
                a.visit(&mut f)?;
            },
            Expr::Binary(_, a, b) => {
                a.visit(&mut f)?;
                b.visit(&mut f)?;
            },
            Expr::Ternary(_, a, b, c) => {
                a.visit(&mut f)?;
                b.visit(&mut f)?;
                c.visit(&mut f)?;
            },
            Expr::Bind(_, val, body) => {
                val.visit(&mut f)?;
                body.visit(&mut f)?;
            },
            Expr::Func(_, body) => {
                body.visit(&mut f)?;
            },
            Expr::Call(func, a) => {
                func.visit(&mut f)?;
                a.visit(&mut f)?;
            },
            _ => {},
        }

        Ok(())
    }
}
