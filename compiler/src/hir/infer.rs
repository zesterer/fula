use std::ops::{Deref, DerefMut};
use super::*;

impl Value {
    pub fn get_type<'a>(&self) -> Type<'a> {
        match self {
            Value::Bool(_) => Type::Primitive(PrimitiveType::Bool),
            Value::Int(_) => Type::Primitive(PrimitiveType::Int),
            Value::Float(_) => Type::Primitive(PrimitiveType::Float),
            Value::String(_) => Type::Primitive(PrimitiveType::String),
        }
    }
}

impl<'a> Type<'a> {
    pub fn homogenize(&mut self, r0: SrcRef, other: &mut Type<'a>, r1: SrcRef) -> Result<(), HirError<'a>> {
        match (self, other) {
            (this @ Type::Unknown, other) => *this = other.clone(),
            (this, other @ Type::Unknown) => *other = this.clone(),
            (Type::Func(a, b), Type::Func(c, d)) => {
                a.homogenize(r0, c, r1)?;
                b.homogenize(r0, d, r1)?;
            },
            (Type::List(a), Type::List(b)) => {
                a.homogenize(r0, b, r1)?;
            },
            (Type::Primitive(a), Type::Primitive(b)) if a == b => {},
            (this, other) => return Err(HirError::TypeMismatch(r0, this.clone(), r1, other.clone())),
        }
        Ok(())
    }

    pub fn known_or<'b>(&'b self, other: &'b Type<'a>) -> &'b Self {
        if let Type::Unknown = self {
            other
        } else {
            self
        }
    }
}

impl<'a> TypeInfo<'a> {
    pub fn homogenize(&mut self, r0: SrcRef, other: &mut TypeInfo<'a>, r1: SrcRef) -> Result<(), HirError<'a>> {
        if Rc::ptr_eq(&self.ty, &other.ty) {
            Ok(())
        } else {
            self.ty.borrow_mut().homogenize(r0, &mut other.ty.borrow_mut(), r1)?;
            self.ty = other.ty.clone();
            Ok(())
        }
    }
}

impl UnaryOp {
    fn homogenize<'a>(&self, val_type_info: &mut TypeInfo<'a>, r: SrcRef, a: &mut IrNode<'a, Expr<'a>>) -> Result<(), HirError<'a>> {
        if Rc::ptr_eq(&val_type_info.ty, &a.type_info.ty) {
            return Ok(());
        }

        let ops = [
            (UnaryOp::Neg, PrimitiveType::Int, (PrimitiveType::Int,)),
            (UnaryOp::Neg, PrimitiveType::Float, (PrimitiveType::Float,)),

            (UnaryOp::Not, PrimitiveType::Bool, (PrimitiveType::Bool,)),
        ];

        for (op, ret, (x,)) in &ops {
            if self == op {

                if match (
                    val_type_info.ty.borrow_mut().known_or(&Type::Primitive(ret.clone())),
                    a.type_info.ty.borrow_mut().known_or(&Type::Primitive(x.clone())),
                ) {
                    (Type::Primitive(val_ty), Type::Primitive(a_ty)) if val_ty == ret && a_ty == x => true,
                    _ => false,
                } {
                    a.type_info.homogenize(a.src_ref, &mut TypeInfo::from(Type::Primitive(x.clone())), r)?;
                    val_type_info.homogenize(r, &mut TypeInfo::from(Type::Primitive(ret.clone())), r)?;
                    return Ok(());
                }
            }
        }

        return Err(HirError::InvalidUnary(r, self.clone(), a.type_info.ty.borrow().clone()));
    }
}

impl BinaryOp {
    fn homogenize<'a>(&self, val_type_info: &mut TypeInfo<'a>, r: SrcRef, a: &mut IrNode<'a, Expr<'a>>, b: &mut IrNode<'a, Expr<'a>>) -> Result<(), HirError<'a>> {
        // List operations

        if match (
            self,
            val_type_info.ty.borrow_mut().known_or(&Type::List(TypeInfo::unknown())),
            a.type_info.ty.borrow_mut().known_or(&Type::List(TypeInfo::unknown())),
            b.type_info.ty.borrow_mut().known_or(&Type::List(TypeInfo::unknown())),
        ) {
            (BinaryOp::Eq, Type::List(_), Type::List(_), Type::List(_)) => true,
            (BinaryOp::NotEq, Type::List(_), Type::List(_), Type::List(_)) => true,
            _ => false,
        } {
            a.type_info.homogenize(a.src_ref, &mut b.type_info, b.src_ref)?;
            val_type_info.homogenize(r, &mut TypeInfo::from(Type::Primitive(PrimitiveType::Bool)), SrcRef::None)?;
            return Ok(());
        }

        // Primitive operations

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
                if match (
                    val_type_info.ty.borrow_mut().known_or(&Type::Primitive(ret.clone())),
                    a.type_info.ty.borrow_mut().known_or(&Type::Primitive(x.clone())),
                    b.type_info.ty.borrow_mut().known_or(&Type::Primitive(y.clone())),
                ) {
                    (Type::Primitive(val_ty), Type::Primitive(a_ty), Type::Primitive(b_ty)) if val_ty == ret && a_ty == x && b_ty == y => true,
                    _ => false,
                } {
                    a.type_info.homogenize(a.src_ref, &mut TypeInfo::from(Type::Primitive(x.clone())), r)?;
                    b.type_info.homogenize(b.src_ref, &mut TypeInfo::from(Type::Primitive(y.clone())), r)?;
                    val_type_info.homogenize(r, &mut TypeInfo::from(Type::Primitive(ret.clone())), r)?;
                    return Ok(());
                }
            }
        }

        return Err(HirError::InvalidBinary(r, self.clone(), a.type_info.ty.borrow().clone(), b.type_info.ty.borrow().clone()));
    }
}

impl TernaryOp {
    fn homogenize<'a>(
        &self,
        val_type_info: &mut TypeInfo<'a>,
        r: SrcRef,
        a: &mut IrNode<'a, Expr<'a>>,
        b: &mut IrNode<'a, Expr<'a>>,
        c: &mut IrNode<'a, Expr<'a>>,
    ) -> Result<(), HirError<'a>> {
        match self {
            TernaryOp::IfElse => {
                match a.type_info.ty.borrow().deref() {
                    Type::Unknown => {},
                    Type::Primitive(PrimitiveType::Bool) => {},
                    a_ty => return Err(HirError::ExpectedType(a.src_ref, a_ty.clone(), Type::Primitive(PrimitiveType::Bool))),
                }

                a.type_info.homogenize(a.src_ref, &mut TypeInfo::from(Type::Primitive(PrimitiveType::Bool)), r)?;
                c.type_info.homogenize(c.src_ref, &mut b.type_info, b.src_ref)?;
                val_type_info.homogenize(r, &mut b.type_info, r)?;
                Ok(())
            },
        }
    }
}

impl<'a> IrNode<'a, Expr<'a>> {
    pub fn infer_types(&mut self) -> Result<(), HirError<'a>> {
        match self.inner.as_mut() {
            Expr::Value(val) => self.type_info.ty.borrow_mut().homogenize(self.src_ref, &mut val.get_type(), self.src_ref)?,
            Expr::Ident(_) => {},
            Expr::Unary(op, a) => {
                a.infer_types()?;
                op.homogenize(&mut self.type_info, self.src_ref, a)?;
            },
            Expr::Binary(op, a, b) => {
                a.infer_types()?;
                b.infer_types()?;
                op.homogenize(&mut self.type_info, self.src_ref, a, b)?;
            },
            Expr::Ternary(op, a, b, c) => {
                a.infer_types()?;
                b.infer_types()?;
                c.infer_types()?;
                op.homogenize(&mut self.type_info, self.src_ref, a, b, c)?;
            },
            Expr::Bind(_, val, body) => {
                val.infer_types()?;
                body.infer_types()?;
            },
            Expr::Call(func, a) => {
                func.infer_types()?;
                a.infer_types()?;
                func.type_info.homogenize(self.src_ref, &mut TypeInfo::from(Type::Func(TypeInfo::unknown(), a.type_info.clone())), self.src_ref)?;
                if let Type::Func(a_ty, body_ty) = &mut func.type_info.ty.borrow_mut().deref_mut() {
                    a.type_info.homogenize(a.src_ref, a_ty, a.src_ref)?;
                    self.type_info.homogenize(self.src_ref, body_ty, func.src_ref)?;
                } else {
                    panic!();
                }
            },
            Expr::Func(_, body) => {
                body.infer_types()?;
                self.type_info.homogenize(self.src_ref, &mut TypeInfo::from(Type::Func(TypeInfo::unknown(), body.type_info.clone())), self.src_ref)?;
                if let Type::Func(_, body_ty) = &mut self.type_info.ty.borrow_mut().deref_mut() {
                    body.type_info.homogenize(body.src_ref, body_ty, body.src_ref)?;
                } else {
                    panic!();
                }
            },
            Expr::List(elements) => {
                elements
                    .iter_mut()
                    .map(|e| e.infer_types())
                    .collect::<Result<(), _>>()?;

                let mut list_type = TypeInfo::from(Type::List(elements.first_mut().map(|e| e.type_info.clone()).unwrap_or(TypeInfo::unknown())));
                // Homogenize the list type with the inner type
                self.type_info.homogenize(self.src_ref, &mut list_type, self.src_ref)?;
                // Homogenize the inner types with the list type
                let r = self.src_ref;
                if let Type::List(inner_ty) = list_type.ty.borrow_mut().deref_mut() {
                    elements
                        .iter_mut()
                        .rev()
                        .map(|e| e.type_info.homogenize(e.src_ref, inner_ty, r))
                        .collect::<Result<(), _>>()?;
                } else {
                    panic!();
                };
            },
        }

        Ok(())
    }
}

impl<'a> Program<'a> {
    pub fn infer_types(&mut self) -> Vec<HirError<'a>> {
        let mut errors = Vec::new();

        for decl in self.declarations_mut() {
            match decl {
                Decl::Const(_, val) => match val.infer_types() {
                    Ok(()) => {},
                    Err(err) => errors.push(err),
                },
                Decl::Data(_, _) => unimplemented!(),
            }
        }

        errors
    }
}
