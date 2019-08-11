use super::*;

impl Value {
    pub fn get_type_info<'a>(&self) -> TypeInfo<'a> {
        match self {
            Value::Bool(_) => TypeInfo::Primitive(PrimitiveType::Bool),
            Value::Int(_) => TypeInfo::Primitive(PrimitiveType::Int),
            Value::Float(_) => TypeInfo::Primitive(PrimitiveType::Float),
            Value::String(_) => TypeInfo::Primitive(PrimitiveType::String),
        }
    }
}

impl<'a> TypeInfo<'a> {
    pub fn homogenize(&mut self, r0: SrcRef, other: &mut TypeInfo<'a>, r1: SrcRef) -> Result<(), HirError<'a>> {
        match (self, other) {
            (this @ TypeInfo::Unknown, other) => *this = other.clone(),
            (this, other @ TypeInfo::Unknown) => *other = this.clone(),
            (TypeInfo::Func(a, b), TypeInfo::Func(c, d)) => {
                a.homogenize(r0, c.as_mut(), r1)?;
                b.homogenize(r0, d.as_mut(), r1)?;
            },
            (TypeInfo::List(a), TypeInfo::List(b)) => {
                a.homogenize(r0, b.as_mut(), r1)?;
            },
            (TypeInfo::Primitive(a), TypeInfo::Primitive(b)) if a == b => {},
            (this, other) => return Err(HirError::TypeMismatch(r0, this.clone(), r1, other.clone())),
        }
        Ok(())
    }
}

impl UnaryOp {
    fn homogenize<'a>(&self, val_ty: &mut TypeInfo<'a>, r: SrcRef, a: &mut IrNode<'a, Expr<'a>>) -> Result<(), HirError<'a>> {
        let ops = [
            (UnaryOp::Neg, PrimitiveType::Int, (PrimitiveType::Int,)),
            (UnaryOp::Neg, PrimitiveType::Float, (PrimitiveType::Float,)),

            (UnaryOp::Not, PrimitiveType::Bool, (PrimitiveType::Bool,)),
        ];

        for (op, ret, (x,)) in &ops {
            if self == op {
                match &a.ty {
                    TypeInfo::Primitive(a) if a == x => {}
                    TypeInfo::Unknown => {}
                    a => match (&val_ty, a) {
                        (TypeInfo::Unknown, TypeInfo::Unknown) => return Ok(()),
                        (TypeInfo::Primitive(val_ty), TypeInfo::Unknown) if val_ty == ret => {},
                        _ => continue,
                    },
                }

                a.ty.homogenize(a.src_ref, &mut TypeInfo::Primitive(x.clone()), r)?;
                val_ty.homogenize(r, &mut TypeInfo::Primitive(ret.clone()), r)?;

                return Ok(());
            }
        }

        return Err(HirError::InvalidUnary(r, self.clone(), a.ty.clone()));
    }
}

impl BinaryOp {
    fn homogenize<'a>(&self, val_ty: &mut TypeInfo<'a>, r: SrcRef, a: &mut IrNode<'a, Expr<'a>>, b: &mut IrNode<'a, Expr<'a>>) -> Result<(), HirError<'a>> {
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
                match (&a.ty, &b.ty) {
                    (TypeInfo::Primitive(a), TypeInfo::Unknown) if a == x => {}
                    (TypeInfo::Unknown, TypeInfo::Primitive(b)) if b == y => {}
                    (TypeInfo::Primitive(a), TypeInfo::Primitive(b)) if a == x && b == y => {},
                    (a, b) => match (&val_ty, a, b) {
                        (TypeInfo::Unknown, TypeInfo::Unknown, TypeInfo::Unknown) => return Ok(()),
                        (TypeInfo::Primitive(val_ty), TypeInfo::Unknown, TypeInfo::Unknown) if val_ty == ret => {},
                        _ => continue,
                    },
                }

                a.ty.homogenize(a.src_ref, &mut TypeInfo::Primitive(x.clone()), r)?;
                b.ty.homogenize(b.src_ref, &mut TypeInfo::Primitive(y.clone()), r)?;
                val_ty.homogenize(r, &mut TypeInfo::Primitive(ret.clone()), r)?;

                return Ok(());
            }
        }

        return Err(HirError::InvalidBinary(r, self.clone(), a.ty.clone(), b.ty.clone()));
    }
}

impl TernaryOp {
    fn homogenize<'a>(
        &self,
        val_ty: &mut TypeInfo<'a>,
        r: SrcRef,
        a: &mut IrNode<'a, Expr<'a>>,
        b: &mut IrNode<'a, Expr<'a>>,
        c: &mut IrNode<'a, Expr<'a>>,
    ) -> Result<(), HirError<'a>> {
        match self {
            TernaryOp::IfElse => {
                match &a.ty {
                    TypeInfo::Unknown => {},
                    TypeInfo::Primitive(PrimitiveType::Bool) => {},
                    a_ty => return Err(HirError::ExpectedType(a.src_ref, a_ty.clone(), TypeInfo::Primitive(PrimitiveType::Bool))),
                }

                a.ty.homogenize(a.src_ref, &mut TypeInfo::Primitive(PrimitiveType::Bool), r)?;
                c.ty.homogenize(c.src_ref, &mut b.ty, b.src_ref)?;
                val_ty.homogenize(r, &mut b.ty, r)?;
                Ok(())
            },
        }
    }
}

impl<'a> IrNode<'a, Expr<'a>> {
    pub fn infer_types(&mut self) -> Result<(), HirError<'a>> {
        match self.inner.as_mut() {
            Expr::Value(val) => self.ty.homogenize(self.src_ref, &mut val.get_type_info(), self.src_ref)?,
            Expr::Ident(_) => {},
            Expr::Unary(op, a) => {
                a.infer_types()?;
                op.homogenize(&mut self.ty, self.src_ref, a)?;
                a.infer_types()?;
            },
            Expr::Binary(op, a, b) => {
                a.infer_types()?;
                b.infer_types()?;
                op.homogenize(&mut self.ty, self.src_ref, a, b)?;
                a.infer_types()?;
                b.infer_types()?;
            },
            Expr::Ternary(op, a, b, c) => {
                a.infer_types()?;
                b.infer_types()?;
                c.infer_types()?;
                op.homogenize(&mut self.ty, self.src_ref, a, b, c)?;
                a.infer_types()?;
                b.infer_types()?;
                c.infer_types()?;
            },
            Expr::Bind(_, val, body) => {
                val.infer_types()?;
                body.infer_types()?;
            },
            Expr::Call(func, a) => {
                func.infer_types()?;
                a.infer_types()?;
                func.ty.homogenize(self.src_ref, &mut TypeInfo::Func(TypeInfo::Unknown.into(), a.ty.clone().into()), self.src_ref)?;
                if let TypeInfo::Func(a_ty, body_ty) = &mut func.ty {
                    a.ty.homogenize(a.src_ref, a_ty, a.src_ref)?;
                    self.ty.homogenize(self.src_ref, body_ty, func.src_ref)?;
                } else {
                    panic!();
                }
                func.infer_types()?;
                a.infer_types()?;
            },
            Expr::Func(_, body) => {
                body.infer_types()?;
                self.ty.homogenize(self.src_ref, &mut TypeInfo::Func(TypeInfo::Unknown.into(), body.ty.clone().into()), self.src_ref)?;
                if let TypeInfo::Func(_, body_ty) = &mut self.ty {
                    body.ty.homogenize(body.src_ref, body_ty, body.src_ref)?;
                } else {
                    panic!();
                }
                body.infer_types()?;
            },
            Expr::List(elements) => {
                elements
                    .iter_mut()
                    .map(|e| e.infer_types())
                    .collect::<Result<(), _>>()?;

                // Homogenize each element with its rightward neighbour (i.e: ensure they're all the same type)
                for i in 1..elements.len() {
                    let (a, b) = elements.split_at_mut(i);
                    let (a, b) = (a.last_mut().unwrap(), b.first_mut().unwrap());
                    a.ty.homogenize(a.src_ref, &mut b.ty, b.src_ref)?;
                }
                let mut list_type = TypeInfo::List(elements.first_mut().map(|e| e.ty.clone()).unwrap_or(TypeInfo::Unknown).into());
                // Homogenize the list type with the inner type
                self.ty.homogenize(self.src_ref, &mut list_type, self.src_ref)?;
                // Homogenize the inner types with the list type
                let r = self.src_ref;
                if let TypeInfo::List(inner_ty) = &mut list_type {
                    elements
                        .iter_mut()
                        .rev()
                        .map(|e| e.ty.homogenize(e.src_ref, inner_ty, r))
                        .collect::<Result<(), _>>()?;
                } else {
                    panic!();
                }
                // Homogenize the list type again
                self.ty.homogenize(self.src_ref, &mut list_type, self.src_ref)?;

                elements
                    .iter_mut()
                    .map(|e| e.infer_types())
                    .collect::<Result<(), _>>()?;
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
