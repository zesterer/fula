use std::{
    ops::{Deref, DerefMut},
    collections::HashMap,
};
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
                a.homogenize(c)?;
                b.homogenize(d)?;
            },
            (Type::List(a), Type::List(b)) => {
                a.homogenize(b)?;
            },
            (Type::Primitive(a), Type::Primitive(b)) if a == b => {},
            (Type::Tuple(a), Type::Tuple(b)) if a.len() == b.len() => {
                a
                    .iter_mut()
                    .zip(b.iter_mut())
                    .map(|(a, b)| a.homogenize(b))
                    .collect::<Result<(), _>>()?;
            },
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
    pub fn homogenize(&mut self, other: &mut TypeInfo<'a>) -> Result<(), HirError<'a>> {
        if Rc::ptr_eq(&self.ty, &other.ty) {
            Ok(())
        } else {
            self.ty.borrow_mut().homogenize(self.src_ref, &mut other.ty.borrow_mut(), other.src_ref)?;
            self.ty = other.ty.clone();
            self.src_ref = self.src_ref.homogenize(other.src_ref);
            Ok(())
        }
    }
}

impl UnaryOp {
    fn homogenize<'a>(&self, val_type_info: &mut TypeInfo<'a>, a: &mut IrNode<'a, Expr<'a>>) -> Result<(), HirError<'a>> {
        if Rc::ptr_eq(&val_type_info.ty, &a.type_info.ty) {
            return Ok(());
        }

        // Not enough information

        if let (Type::Unknown, Type::Unknown) = (
            val_type_info.ty.borrow().deref(),
            a.type_info.ty.borrow().deref(),
        ) {
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
                    a.type_info.homogenize(&mut TypeInfo::new(Type::Primitive(x.clone()), val_type_info.src_ref))?;
                    val_type_info.homogenize(&mut TypeInfo::new(Type::Primitive(ret.clone()), val_type_info.src_ref))?;
                    return Ok(());
                }
            }
        }

        return Err(HirError::InvalidUnary(val_type_info.src_ref, self.clone(), a.type_info.ty.borrow().clone()));
    }
}

impl BinaryOp {
    fn homogenize<'a>(&self, val_type_info: &mut TypeInfo<'a>, a: &mut TypeInfo<'a>, b: &mut TypeInfo<'a>) -> Result<(), HirError<'a>> {
        // Not enough information

        if let (Type::Unknown, Type::Unknown, Type::Unknown) = (
            val_type_info.ty.borrow().deref(),
            a.ty.borrow().deref(),
            b.ty.borrow().deref(),
        ) {
            return Ok(());
        }


        // List operations

        if match (
            self,
            val_type_info.ty.borrow_mut().known_or(&Type::List(TypeInfo::unknown(val_type_info.src_ref))),
            a.ty.borrow_mut().known_or(&Type::List(TypeInfo::unknown(a.src_ref))),
            b.ty.borrow_mut().known_or(&Type::List(TypeInfo::unknown(b.src_ref))),
        ) {
            (BinaryOp::Eq, Type::List(_), Type::List(_), Type::List(_)) => true,
            (BinaryOp::NotEq, Type::List(_), Type::List(_), Type::List(_)) => true,
            _ => false,
        } {
            a.homogenize(b)?;
            val_type_info.homogenize(&mut TypeInfo::new(Type::Primitive(PrimitiveType::Bool), val_type_info.src_ref))?;

            println!("{:?}, ", Rc::ptr_eq(&a.ty, &b.ty));

            if !Rc::ptr_eq(&a.ty, &b.ty) {
                match (
                    val_type_info.ty.borrow_mut().deref_mut(),
                    a.ty.borrow_mut().deref_mut(),
                    b.ty.borrow_mut().deref_mut(),
                ) {
                    (Type::List(val_ty), Type::List(a_ty), Type::List(b_ty)) => self.homogenize(val_ty, a_ty, b_ty)?,
                    _ => panic!(),
                }
            }

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

        let (mut num_matches, mut match_idx) = (0, None);
        for (i, (op, ret, (x, y))) in ops.iter().enumerate() {
            if self == op {
                if match (
                    val_type_info.ty.borrow_mut().known_or(&Type::Primitive(ret.clone())),
                    a.ty.borrow_mut().known_or(&Type::Primitive(x.clone())),
                    b.ty.borrow_mut().known_or(&Type::Primitive(y.clone())),
                ) {
                    (Type::Primitive(val_ty), Type::Primitive(a_ty), Type::Primitive(b_ty)) if val_ty == ret && a_ty == x && b_ty == y => true,
                    _ => false,
                } {
                    num_matches += 1;
                    match_idx = Some(i);
                }
            }
        }

        match (num_matches, match_idx) {
            (1, Some(match_idx)) => {
                let (_, ret, (x, y)) = &ops[match_idx];
                a.homogenize(&mut TypeInfo::new(Type::Primitive(x.clone()), val_type_info.src_ref))?;
                b.homogenize(&mut TypeInfo::new(Type::Primitive(y.clone()), val_type_info.src_ref))?;
                val_type_info.homogenize(&mut TypeInfo::new(Type::Primitive(ret.clone()), val_type_info.src_ref))?;
                Ok(())
            },
            _ => Err(HirError::InvalidBinary(val_type_info.src_ref, self.clone(), a.ty.borrow().clone(), b.ty.borrow().clone())),
        }
    }
}

impl TernaryOp {
    fn homogenize<'a>(
        &self,
        val_type_info: &mut TypeInfo<'a>,
        a: &mut IrNode<'a, Expr<'a>>,
        b: &mut IrNode<'a, Expr<'a>>,
        c: &mut IrNode<'a, Expr<'a>>,
    ) -> Result<(), HirError<'a>> {
        // Not enough information

        if let (Type::Unknown, Type::Unknown, Type::Unknown, Type::Unknown) = (
            val_type_info.ty.borrow().deref(),
            a.type_info.ty.borrow().deref(),
            b.type_info.ty.borrow().deref(),
            c.type_info.ty.borrow().deref(),
        ) {
            return Ok(());
        }

        match self {
            TernaryOp::IfElse => {
                match a.type_info.ty.borrow().deref() {
                    Type::Unknown => {},
                    Type::Primitive(PrimitiveType::Bool) => {},
                    a_ty => return Err(HirError::ExpectedType(a.src_ref, a_ty.clone(), Type::Primitive(PrimitiveType::Bool))),
                }

                a.type_info.homogenize(&mut TypeInfo::new(Type::Primitive(PrimitiveType::Bool), val_type_info.src_ref))?;
                c.type_info.homogenize(&mut b.type_info)?;
                val_type_info.homogenize(&mut b.type_info)?;
                Ok(())
            },
        }
    }
}

pub enum Scope<'a, 'b> {
    Global(HashMap<&'a str, TypeInfo<'a>>),
    Local(&'a str, TypeInfo<'a>, &'b Self),
}

impl<'a, 'b> Scope<'a, 'b> {
    pub fn global(types: HashMap<&'a str, TypeInfo<'a>>) -> Self {
        Scope::Global(types)
    }

    pub fn with<'c>(&'c self, name: &'a str, ty: TypeInfo<'a>) -> Scope<'a, 'c>
        where 'b: 'c
    {
        Scope::Local(name, ty, self)
    }

    pub fn get(&self, name: &str) -> Option<TypeInfo<'a>> {
        match self {
            Scope::Global(types) => types.get(name).cloned(),
            Scope::Local(local, ty, _) if *local == name => Some(ty.clone()),
            Scope::Local(_, _, scope) => scope.get(name),
        }
    }
}

impl<'a> IrNode<'a, Expr<'a>> {
    pub fn infer_types(&mut self, scope: &Scope<'a, '_>) -> Result<(), HirError<'a>> {
        match self.inner.as_mut() {
            Expr::Value(val) => self.type_info.homogenize(&mut TypeInfo::new(val.get_type(), self.src_ref))?,
            Expr::Ident(name) => match scope.get(name) {
                Some(mut ty) => self.type_info.homogenize(&mut ty)?,
                None => return Err(HirError::CannotFindIdent(self.src_ref, name)),
            },
            Expr::Unary(op, a) => {
                a.infer_types(scope)?;
                op.homogenize(&mut self.type_info, a)?;
            },
            Expr::Binary(op, a, b) => {
                a.infer_types(scope)?;
                b.infer_types(scope)?;
                op.homogenize(&mut self.type_info, &mut a.type_info, &mut b.type_info)?;
            },
            Expr::Ternary(op, a, b, c) => {
                a.infer_types(scope)?;
                b.infer_types(scope)?;
                c.infer_types(scope)?;
                op.homogenize(&mut self.type_info, a, b, c)?;
            },
            Expr::Bind(pat, val, body) => {
                val.infer_types(scope)?;
                let scope = match pat.inner.deref() {
                    Pattern::Ident(name) => scope.with(name, val.type_info.clone()),
                };
                pat.type_info.homogenize(&mut val.type_info)?;

                body.infer_types(&scope)?;
                self.type_info.homogenize(&mut body.type_info)?;
            },
            Expr::Call(func, a) => {
                func.infer_types(scope)?;
                a.infer_types(scope)?;
                func.type_info.homogenize(&mut TypeInfo::new(Type::Func(a.type_info.clone(), TypeInfo::unknown(self.src_ref)), func.src_ref))?;
                if let Type::Func(a_ty, body_ty) = &mut func.type_info.ty.borrow_mut().deref_mut() {
                    a.type_info.homogenize(a_ty)?;
                    self.type_info.homogenize(body_ty)?;
                } else {
                    panic!();
                }
            },
            Expr::Func(pat, body) => {
                let scope = match pat.inner.deref() {
                    Pattern::Ident(name) => scope.with(name, pat.type_info.clone()),
                };
                body.infer_types(&scope)?;
                self.type_info.homogenize(&mut TypeInfo::new(Type::Func(pat.type_info.clone(), body.type_info.clone()), self.src_ref))?;
                if let Type::Func(_, body_ty) = &mut self.type_info.ty.borrow_mut().deref_mut() {
                    body.type_info.homogenize(body_ty)?;
                } else {
                    panic!();
                }
            },
            Expr::List(elements) => {
                elements
                    .iter_mut()
                    .map(|e| e.infer_types(scope))
                    .collect::<Result<(), _>>()?;

                let mut list_type = TypeInfo::new(
                    Type::List(elements.first_mut().map(|e| e.type_info.clone()).unwrap_or(TypeInfo::unknown(self.src_ref))),
                    self.src_ref,
                );

                // Homogenize the list type
                self.type_info.homogenize(&mut list_type)?;
                // Homogenize the inner types with the list type
                if let Type::List(inner_ty) = list_type.ty.borrow_mut().deref_mut() {
                    elements
                        .iter_mut()
                        .rev()
                        .map(|e| e.type_info.homogenize(inner_ty))
                        .collect::<Result<(), _>>()?;
                } else {
                    panic!();
                };
            },
            Expr::Tuple(fields) => {
                fields
                    .iter_mut()
                    .map(|f| f.infer_types(scope))
                    .collect::<Result<(), _>>()?;

                let mut tuple_type = TypeInfo::new(
                    Type::Tuple(fields.iter().map(|f| f.type_info.clone()).collect()),
                    self.src_ref,
                );

                // Homogenize the list type
                self.type_info.homogenize(&mut tuple_type)?;

                // Homogenize the inner types with the list type
                if let Type::Tuple(field_tys) = tuple_type.ty.borrow_mut().deref_mut() {
                    fields
                        .iter_mut()
                        .zip(field_tys.iter_mut())
                        .map(|(f, f_ty)| f.type_info.homogenize(f_ty))
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

        let scope = Scope::global(self
            .declarations()
            .filter_map(|decl| match decl {
                Decl::Const(name, expr) => Some((*name, expr.type_info.clone())),
                Decl::Data(_, _) => None,
            })
            .collect());

        for decl in self.declarations_mut() {
            match decl {
                Decl::Const(_, val) => match val.infer_types(&scope) {
                    Ok(()) => {},
                    Err(err) => errors.push(err),
                },
                Decl::Data(_, _) => unimplemented!(),
            }
        }

        println!("{:#?}", self);

        errors
    }
}
