use super::*;

impl<'a> IrNode<'a, Expr<'a>> {
    pub fn visit(&mut self, mut f: impl FnMut(&mut IrNode<'a, Expr<'a>>)) {
        f(self);

        match self.inner.as_mut() {
            Expr::Unary(_, a) => {
                a.visit(&mut f);
            },
            Expr::Binary(_, a, b) => {
                a.visit(&mut f);
                b.visit(&mut f);
            },
            Expr::Ternary(_, a, b, c) => {
                a.visit(&mut f);
                b.visit(&mut f);
                c.visit(&mut f);
            },
            Expr::Bind(_, val, body) => {
                val.visit(&mut f);
                body.visit(&mut f);
            },
            Expr::Func(_, body) => {
                body.visit(&mut f);
            },
            Expr::Call(func, a) => {
                func.visit(&mut f);
                a.visit(&mut f);
            },
            _ => {},
        }
    }
}
