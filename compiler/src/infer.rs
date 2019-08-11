use std::collections::HashMap;
use fula_syntax::src::SrcRef;
use crate::hir::*;

#[derive(Debug)]
pub enum TypeError<'a> {
    Ambiguity(SrcRef),
    Conflict(SrcRef, TypeInfo<'a>, TypeInfo<'a>),
    ExpectedFunc(SrcRef, TypeInfo<'a>),
    UnknownIdent(SrcRef, &'a str),
    InvalidUnaryOp(SrcRef, UnaryOp, TypeInfo<'a>),
    InvalidBinaryOp(SrcRef, BinaryOp, TypeInfo<'a>, TypeInfo<'a>),
}

impl<'a> TypeInfo<'a> {
    pub fn compatible_with(&self, ty: &TypeInfo<'a>) -> bool {
        match (self, ty) {
            (TypeInfo::Unknown, _) => true,
            (_, TypeInfo::Unknown) => true,
            (TypeInfo::Primitive(a), TypeInfo::Primitive(b)) => a == b,
            (TypeInfo::Func(a, b), TypeInfo::Func(c, d)) => a.compatible_with(c) && b.compatible_with(d),
            (TypeInfo::Named(a), TypeInfo::Named(b)) => a == b,
            (TypeInfo::Derived(a), TypeInfo::Derived(b)) => a.borrow().compatible_with(&b.borrow()),
            _ => false,
        }
    }

    pub fn substitute(&mut self, ty: &TypeInfo<'a>) {
        match (self, ty) {
            (this @ TypeInfo::Unknown, ty) => *this = ty.clone(),
            (TypeInfo::Func(a, b), TypeInfo::Func(c, d)) => {
                a.substitute(c);
                b.substitute(d);
            },
            _ => {}, // TODO: Panic if this fails?
        }
    }

    pub fn reify(&mut self, ty: &TypeInfo<'a>, r: SrcRef, errors: &mut Vec<TypeError<'a>>) {
        if self.compatible_with(ty) {
            self.substitute(ty);
        } else {
            errors.push(TypeError::Conflict(r, self.clone(), ty.clone()));
        }
    }

    pub fn is_ambiguous(&self) -> bool {
        match self {
            TypeInfo::Unknown => true,
            TypeInfo::Primitive(_) => false,
            TypeInfo::Named(_) => false,
            TypeInfo::Func(param, ret) => param.is_ambiguous() || ret.is_ambiguous(),
            TypeInfo::Derived(ty) => ty.borrow().is_ambiguous(),
        }
    }
}

impl<'a> Expr<'a> {
    fn get_type_ambiguities(&self, errors: &mut Vec<TypeError<'a>>) {
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
            Expr::Func(pat, expr) => {
                if pat.type_info().is_ambiguous() {
                    errors.push(TypeError::Ambiguity(pat.src_ref()));
                }
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

impl UnaryOp {
    fn get_type_info<'a>(&self, ty: &TypeInfo<'a>, a: &TypeInfo<'a>, r: SrcRef) -> Result<(TypeInfo<'a>, [TypeInfo<'a>; 1]), TypeError<'a>> {
        match (self, ty, a) {
            (_, TypeInfo::Unknown, TypeInfo::Unknown) => Ok((TypeInfo::Unknown, [TypeInfo::Unknown])),
            (UnaryOp::Not, TypeInfo::Unknown, TypeInfo::Primitive(PrimitiveType::Bool)) => Ok((TypeInfo::Primitive(PrimitiveType::Bool), [TypeInfo::Primitive(PrimitiveType::Bool)])),
            (UnaryOp::Not, TypeInfo::Primitive(PrimitiveType::Bool), TypeInfo::Unknown) => Ok((TypeInfo::Primitive(PrimitiveType::Bool), [TypeInfo::Primitive(PrimitiveType::Bool)])),
            (UnaryOp::Neg, TypeInfo::Primitive(PrimitiveType::Int), TypeInfo::Unknown) => Ok((TypeInfo::Primitive(PrimitiveType::Int), [TypeInfo::Primitive(PrimitiveType::Int)])),
            (UnaryOp::Neg, TypeInfo::Unknown, TypeInfo::Primitive(PrimitiveType::Int)) => Ok((TypeInfo::Primitive(PrimitiveType::Int), [TypeInfo::Primitive(PrimitiveType::Int)])),
            (UnaryOp::Neg, TypeInfo::Primitive(PrimitiveType::Float), TypeInfo::Unknown) => Ok((TypeInfo::Primitive(PrimitiveType::Float), [TypeInfo::Primitive(PrimitiveType::Float)])),
            (UnaryOp::Neg, TypeInfo::Unknown, TypeInfo::Primitive(PrimitiveType::Float)) => Ok((TypeInfo::Primitive(PrimitiveType::Float), [TypeInfo::Primitive(PrimitiveType::Float)])),
            (op, _, a) => Err(TypeError::InvalidUnaryOp(r, op.clone(), a.clone())),
        }
    }
}

impl BinaryOp {
    fn get_type_info<'a>(&self, ty: &TypeInfo<'a>, a: &TypeInfo<'a>, b: &TypeInfo<'a>, r: SrcRef) -> Result<(TypeInfo<'a>, [TypeInfo<'a>; 2]), TypeError<'a>> {
        let ops = [
            (BinaryOp::Add, PrimitiveType::Int, (PrimitiveType::Int, PrimitiveType::Int)),
            (BinaryOp::Add, PrimitiveType::Float, (PrimitiveType::Float, PrimitiveType::Float)),
            (BinaryOp::Sub, PrimitiveType::Int, (PrimitiveType::Int, PrimitiveType::Int)),
            (BinaryOp::Sub, PrimitiveType::Float, (PrimitiveType::Float, PrimitiveType::Float)),
            (BinaryOp::Mul, PrimitiveType::Int, (PrimitiveType::Int, PrimitiveType::Int)),
            (BinaryOp::Mul, PrimitiveType::Float, (PrimitiveType::Float, PrimitiveType::Float)),
            (BinaryOp::Div, PrimitiveType::Int, (PrimitiveType::Int, PrimitiveType::Int)),
            (BinaryOp::Div, PrimitiveType::Float, (PrimitiveType::Float, PrimitiveType::Float)),
            (BinaryOp::Rem, PrimitiveType::Int, (PrimitiveType::Int, PrimitiveType::Int)),
            (BinaryOp::Rem, PrimitiveType::Float, (PrimitiveType::Float, PrimitiveType::Float)),

            (BinaryOp::And, PrimitiveType::Bool, (PrimitiveType::Bool, PrimitiveType::Bool)),
            (BinaryOp::Or, PrimitiveType::Bool, (PrimitiveType::Bool, PrimitiveType::Bool)),
            (BinaryOp::Xor, PrimitiveType::Bool, (PrimitiveType::Bool, PrimitiveType::Bool)),

            (BinaryOp::Eq, PrimitiveType::Bool, (PrimitiveType::Int, PrimitiveType::Int)),
            (BinaryOp::Eq, PrimitiveType::Bool, (PrimitiveType::Float, PrimitiveType::Float)),
            (BinaryOp::Eq, PrimitiveType::Bool, (PrimitiveType::Bool, PrimitiveType::Bool)),
            (BinaryOp::NotEq, PrimitiveType::Bool, (PrimitiveType::Int, PrimitiveType::Int)),
            (BinaryOp::NotEq, PrimitiveType::Bool, (PrimitiveType::Float, PrimitiveType::Float)),
            (BinaryOp::NotEq, PrimitiveType::Bool, (PrimitiveType::Bool, PrimitiveType::Bool)),

            (BinaryOp::Less, PrimitiveType::Int, (PrimitiveType::Int, PrimitiveType::Int)),
            (BinaryOp::Less, PrimitiveType::Float, (PrimitiveType::Float, PrimitiveType::Float)),
            (BinaryOp::LessEq, PrimitiveType::Int, (PrimitiveType::Int, PrimitiveType::Int)),
            (BinaryOp::LessEq, PrimitiveType::Float, (PrimitiveType::Float, PrimitiveType::Float)),
            (BinaryOp::More, PrimitiveType::Int, (PrimitiveType::Int, PrimitiveType::Int)),
            (BinaryOp::More, PrimitiveType::Float, (PrimitiveType::Float, PrimitiveType::Float)),
            (BinaryOp::MoreEq, PrimitiveType::Int, (PrimitiveType::Int, PrimitiveType::Int)),
            (BinaryOp::MoreEq, PrimitiveType::Float, (PrimitiveType::Float, PrimitiveType::Float)),
        ];

        for (op, ret, (x, y)) in &ops {
            if self == op {
                match (a, b) {
                    (TypeInfo::Primitive(a), TypeInfo::Unknown) if a == x => {}
                    (TypeInfo::Unknown, TypeInfo::Primitive(b)) if b == y => {}
                    (TypeInfo::Primitive(a), TypeInfo::Primitive(b)) if a == x && b == y => {},
                    _ => match (ty, a, b) {
                        (TypeInfo::Primitive(ty), TypeInfo::Unknown, TypeInfo::Unknown) if ty == ret => {},
                        _ => continue,
                    },
                }

                return Ok((TypeInfo::Primitive(ret.clone()), [TypeInfo::Primitive(x.clone()), TypeInfo::Primitive(y.clone())]));
            }
        }

        return Err(TypeError::InvalidBinaryOp(r, self.clone(), a.clone(), b.clone()));
    }
}

impl TernaryOp {
    fn get_type_info<'a>(&self, ty: &TypeInfo<'a>, a: &TypeInfo<'a>, b: &TypeInfo<'a>, c: &TypeInfo<'a>, r: SrcRef) -> Result<(TypeInfo<'a>, [TypeInfo<'a>; 3]), TypeError<'a>> {
        match (self, ty, a, b, c) {
            (_, TypeInfo::Unknown, TypeInfo::Unknown, TypeInfo::Unknown, TypeInfo::Unknown) => Ok((TypeInfo::Unknown, [TypeInfo::Unknown, TypeInfo::Unknown, TypeInfo::Unknown])),
            (TernaryOp::IfElse, x, TypeInfo::Primitive(PrimitiveType::Bool), TypeInfo::Unknown, TypeInfo::Unknown) => Ok((x.clone(), [TypeInfo::Primitive(PrimitiveType::Bool), TypeInfo::Unknown, TypeInfo::Unknown])),
            (TernaryOp::IfElse, x, TypeInfo::Primitive(PrimitiveType::Bool), b, TypeInfo::Unknown) if x.compatible_with(b) => Ok((b.clone(), [TypeInfo::Primitive(PrimitiveType::Bool), b.clone(), b.clone()])),
            (TernaryOp::IfElse, x, TypeInfo::Primitive(PrimitiveType::Bool), TypeInfo::Unknown, c) if x.compatible_with(c) => Ok((c.clone(), [TypeInfo::Primitive(PrimitiveType::Bool), c.clone(), c.clone()])),
            (TernaryOp::IfElse, x, TypeInfo::Primitive(PrimitiveType::Bool), b, c) => if x.compatible_with(b) && b.compatible_with(c) {
                Ok((b.clone(), [TypeInfo::Primitive(PrimitiveType::Bool), b.clone(), b.clone()]))
            } else {
                Err(TypeError::Conflict(r, a.clone(), b.clone()))
            },
            (TernaryOp::IfElse, _, a, _, _) => Err(TypeError::Conflict(r, a.clone(), TypeInfo::Primitive(PrimitiveType::Bool))),
        }
    }
}

enum Scope<'a, 'b> {
    Global(HashMap<&'a str, TypeInfo<'a>>),
    Single(&'a str, &'b TypeInfo<'a>, &'b Self),
}

impl<'a, 'b> Scope<'a, 'b> {
    fn with<'c>(&'c self, name: &'a str, ty: &'c TypeInfo<'a>) -> Scope<'a, 'c>
        where 'b: 'c
    {
        Scope::Single(name, ty, self)
    }

    fn get(&self, name: &'a str) -> Option<TypeInfo<'a>> {
        match self {
            Scope::Global(consts) => consts.get(name).cloned(),
            Scope::Single(ident, ty, _) if *ident == name => Some((*ty).clone()),
            Scope::Single(_, _, parent) => parent.get(name),
        }
    }
}

impl<'a> IrNode<'a, Expr<'a>> {
    fn infer_types<'b>(&mut self, scope: &Scope<'a, 'b>, errors: &mut Vec<TypeError<'a>>) {
        let (expr, ty, r) = self.parts_mut();
        match expr {
            Expr::Value(val) => ty.reify(&val.type_info(), r, errors),
            Expr::Ident(ident) => match scope.get(ident) {
                Some(ident_ty) => ty.reify(&ident_ty, r, errors),
                None => errors.push(TypeError::UnknownIdent(r, ident)),
            },
            Expr::Unary(op, a) => {
                a.infer_types(scope, errors);
                match op.get_type_info(ty, a.type_info(), r) {
                    Ok((op_ty, [a_ty])) => {
                        ty.reify(&op_ty, r, errors);
                        a.type_info_mut().reify(&a_ty, r, errors);
                        a.infer_types(scope, errors);
                    },
                    Err(err) => errors.push(err),
                }
            },
            Expr::Binary(op, a, b) => {
                a.infer_types(scope, errors);
                b.infer_types(scope, errors);
                match op.get_type_info(ty, a.type_info(), b.type_info(), r) {
                    Ok((op_ty, [a_ty, b_ty])) => {
                        ty.reify(&op_ty, r, errors);
                        a.type_info_mut().reify(&a_ty, r, errors);
                        a.infer_types(scope, errors);
                        b.type_info_mut().reify(&b_ty, r, errors);
                        b.infer_types(scope, errors);
                    },
                    Err(err) => errors.push(err),
                }
            },
            Expr::Ternary(op, a, b, c) => {
                a.infer_types(scope, errors);
                b.infer_types(scope, errors);
                c.infer_types(scope, errors);
                match op.get_type_info(ty, a.type_info(), b.type_info(), c.type_info(), r) {
                    Ok((op_ty, [a_ty, b_ty, c_ty])) => {
                        ty.reify(&op_ty, r, errors);
                        a.type_info_mut().reify(&a_ty, r, errors);
                        a.infer_types(scope, errors);
                        b.type_info_mut().reify(&b_ty, r, errors);
                        b.infer_types(scope, errors);
                        c.type_info_mut().reify(&c_ty, r, errors);
                        c.infer_types(scope, errors);
                    },
                    Err(err) => errors.push(err),
                }
            },
            Expr::Func(param, body) => {
                //ty.reify(&TypeInfo::Func(TypeInfo::Unknown.into(), TypeInfo::Unknown.into()), r, errors);
                let param_ty = param.type_info().clone();
                let scope = match param.inner() {
                    Pattern::Ident(name) => scope.with(name, &param_ty),
                };

                body.infer_types(&scope, errors);

                match ty {
                    TypeInfo::Func(param_ty, body_ty) => {
                        body.type_info_mut().reify(body_ty, r, errors);
                        param.type_info_mut().reify(param_ty, r, errors);
                    },
                    TypeInfo::Unknown => {},
                    ty => errors.push(TypeError::ExpectedFunc(r, ty.clone())),
                }

                body.infer_types(&scope, errors);
            },
            Expr::Call(expr, arg) => {
                expr.infer_types(scope, errors);
                arg.infer_types(scope, errors);
                expr.type_info_mut().reify(&TypeInfo::Func(arg.type_info().clone().into(), ty.clone().into()), r, errors);
                match expr.type_info() {
                    TypeInfo::Func(param_ty, body_ty) => {
                        arg.type_info_mut().reify(param_ty, r, errors);
                        ty.reify(body_ty, r, errors);
                    },
                    TypeInfo::Unknown => {},
                    ty => errors.push(TypeError::ExpectedFunc(r, ty.clone())),
                }

                expr.infer_types(scope, errors);
                arg.infer_types(scope, errors);
            },
            Expr::Cast(expr, arg) => {
                expr.infer_types(scope, errors);
                ty.reify(arg.inner(), arg.src_ref(), errors);
            },
            expr => unimplemented!("Type inference for: {:?}", expr),
        }
    }

    fn get_type_ambiguities(&self, errors: &mut Vec<TypeError<'a>>) {
        if self.type_info().is_ambiguous() {
            errors.push(TypeError::Ambiguity(self.src_ref()));
        } else {
            self.inner().get_type_ambiguities(errors);
        }
    }
}

impl<'a> Program<'a> {
    pub fn infer_types(&mut self) -> Vec<TypeError<'a>> {
        let mut errors = Vec::new();

        let mut scope = Scope::Global(self
            .declarations()
            .filter_map(|decl| match decl {
                Decl::Const(name, expr) => Some((*name, expr.type_info().clone())),
                Decl::Data(_, _) => unimplemented!(),
            })
            .collect());

        for decl in self.declarations_mut() {
            match decl {
                Decl::Const(_, expr) => expr.infer_types(&scope, &mut errors),
                Decl::Data(_, _) => unimplemented!(),
            }
        }

        errors
    }

    pub fn get_type_ambiguities(&self) -> Vec<TypeError<'a>> {
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
