use displaydoc::Display;

use crate::{
    ast::{Loc, Op, Tm, Ty},
    tcx::{Binding, Tcx},
};

#[derive(Debug, Display)]
pub enum TyCheckErr {
    /// {0}: The variable `{1}` is undefined!
    UnboundVariable(Loc, String),

    /// {0}: The term `{1}` cannot be called as a function!
    NonCallableFuncApp(Loc, Tm),

    /// {0}: The term `{1}` is a function, not a `{2}` value!
    GotUnexpectedFunction(Loc, Tm, Ty),

    /// {0}: I needed `{1}` to be a subtype of `{2}`, but it's not!
    NonSubtype(Loc, Ty, Ty),

    /// {0}: I don't know what type `{1}` refers to!
    UnboundTyVariable(Loc, String),
}

pub struct Check {
    tcx: Tcx,
}

const BUILTIN_TYPES: &[(&'static str, Ty)] = &[("Text", Ty::text())];

impl Check {
    pub fn new() -> Self {
        Self {
            tcx: BUILTIN_TYPES
                .iter()
                .map(|(str, ty)| Binding::TyVarBind(str.to_string(), ty.clone()))
                .collect(),
        }
    }

    fn lookup_var(&self, loc: &Loc, name: &str) -> Result<Ty, TyCheckErr> {
        for binding in self.tcx.iter().rev() {
            match binding {
                Binding::VarBind(n, ty) if n == name => return Ok(ty.clone()),
                _ => continue,
            }
        }
        Err(TyCheckErr::UnboundVariable(loc.clone(), name.to_string()))
    }

    fn bind_tm_var(&mut self, name: &str, ty: &Ty) {
        self.tcx
            .push(Binding::VarBind(name.to_string(), ty.clone()));
    }

    fn in_new_scope<T>(&mut self, mut program: impl FnMut(&mut Self) -> T) -> T {
        let old_tcx = self.tcx.clone();
        let res = program(self);
        self.tcx = old_tcx;
        res
    }

    pub fn infer(&mut self, tm: &Tm) -> Result<Ty, TyCheckErr> {
        println!("infer {}\n\t-| {:?}", tm, self.tcx);
        match tm {
            Tm::Text(_, _) => Ok(Ty::text()),

            Tm::Nat(_, _) => Ok(Ty::nat()),

            Tm::Bool(_, _) => Ok(Ty::bool()),

            Tm::Var(loc, name) => self.lookup_var(loc, name),

            Tm::Lam(loc, param, param_ty, body) => {
                let body_ty = self.in_new_scope(|this| {
                    this.bind_tm_var(param, param_ty);
                    let body_ty = this.infer(body)?;
                    let body_ty = this.resolve_ty_vars(loc, &body_ty)?;
                    Ok(body_ty)
                })?;

                Ok(Ty::Arr(
                    Box::new(param_ty.clone()),
                    Box::new(body_ty.clone()),
                ))
            }

            Tm::App(loc, func, arg) => {
                if let Ty::Arr(param_ty, ret_ty) = self.infer(func)? {
                    self.check(arg, param_ty.as_ref())?;
                    Ok(ret_ty.as_ref().clone())
                } else {
                    Err(TyCheckErr::NonCallableFuncApp(
                        loc.clone(),
                        func.as_ref().clone(),
                    ))
                }
            }

            Tm::Block(_loc, tms) => {
                if let Some((last, init)) = tms.split_last() {
                    init.iter()
                        .map(|tm| self.check(tm, &Ty::void()))
                        .collect::<Result<Vec<_>, TyCheckErr>>()?;
                    self.infer(last)
                } else {
                    Ok(Ty::void())
                }
            }

            Tm::Def(_loc, name, tm) => {
                let tm_ty = self.infer(tm)?;
                self.bind_tm_var(name, &tm_ty);
                Ok(Ty::void())
            }

            Tm::Op(loc, x, op, y) => match op {
                Op::SpaceConcat | Op::Concat => {
                    self.check(x, &Ty::text())?;
                    self.check(y, &Ty::text())?;
                    Ok(Ty::text())
                }
                Op::Add | Op::Sub | Op::Mul | Op::Div => {
                    self.check(x, &Ty::nat())?;
                    self.check(y, &Ty::nat())?;
                    Ok(Ty::nat())
                }
                Op::Apply => self.infer(&Tm::App(loc.clone(), x.clone(), y.clone())),
                Op::And | Op::Or => {
                    self.check(x, &Ty::bool())?;
                    self.check(y, &Ty::bool())?;
                    Ok(Ty::bool())
                }
                Op::Lt | Op::Lte | Op::Gt | Op::Gte | Op::NatEq => {
                    self.check(x, &Ty::nat())?;
                    self.check(y, &Ty::nat())?;
                    Ok(Ty::bool())
                }
                Op::TextEq => {
                    self.check(x, &Ty::text())?;
                    self.check(y, &Ty::text())?;
                    Ok(Ty::bool())
                }
            },
        }
    }

    pub fn check(&mut self, tm: &Tm, ty: &Ty) -> Result<(), TyCheckErr> {
        println!("check {} {}\n\t-| {:?}", tm, ty, self.tcx);
        match tm {
            Tm::Text(loc, _) => self.subtype(loc, ty, &Ty::text()),

            Tm::Var(loc, name) => {
                let var_ty = self.lookup_var(loc, name)?;
                self.subtype(loc, &var_ty, ty)?;
                Ok(())
            }

            Tm::Lam(loc, param, param_ty, body) => {
                if let Ty::Arr(expected_param_ty, expected_ret_ty) = ty {
                    self.subtype(loc, param_ty, expected_param_ty)?;
                    self.in_new_scope(|this| {
                        this.bind_tm_var(param, param_ty);
                        let body_ty = this.infer(body)?;
                        this.subtype(loc, &body_ty, &expected_ret_ty)?;
                        Ok(())
                    })?;
                    Ok(())
                } else {
                    Err(TyCheckErr::GotUnexpectedFunction(
                        loc.clone(),
                        tm.clone(),
                        ty.clone(),
                    ))
                }
            }

            Tm::App(loc, func, arg) => {
                if let Ty::Arr(param_ty, ret_ty) = self.infer(func)? {
                    self.check(arg, param_ty.as_ref())?;
                    self.subtype(loc, ret_ty.as_ref(), ty)?;
                    Ok(())
                } else {
                    Err(TyCheckErr::NonCallableFuncApp(
                        loc.clone(),
                        func.as_ref().clone(),
                    ))
                }
            }

            tm => {
                let actual = self.infer(tm)?;
                self.subtype(tm.loc(), &actual, ty)
            }
        }
    }

    pub fn subtype(&self, loc: &Loc, sub: &Ty, sup: &Ty) -> Result<(), TyCheckErr> {
        println!("subtype {} {}\n\t-| {:?}", sub, sup, self.tcx);
        // Optimization.
        if sub == sup {
            return Ok(());
        }

        match (sub, sup) {
            (Ty::Var(name), _) => {
                let sub = self.lookup_ty_var(loc, name)?;
                self.subtype(loc, &sub, sup)?;
                Ok(())
            }

            (_, Ty::Var(name)) => {
                let sup = self.lookup_ty_var(loc, name)?;
                self.subtype(loc, sub, &sup)?;
                Ok(())
            }

            // (X1 -> Y1) <: (X2 -> Y2) if X2 <: X1 && Y1 <: Y2
            // Contravariance of param types, covariance of return types.
            (Ty::Arr(x1, y1), Ty::Arr(x2, y2)) => {
                self.subtype(loc, x2, x1)?;
                self.subtype(loc, y1, y2)?;
                Ok(())
            }

            _ => Err(TyCheckErr::NonSubtype(
                loc.clone(),
                sub.clone(),
                sup.clone(),
            )),
        }
    }

    /// Ensures that the type returned is NOT a `Ty::Var`.
    pub fn lookup_ty_var(&self, loc: &Loc, name: &str) -> Result<Ty, TyCheckErr> {
        let mut name = name.to_string();
        'outer: loop {
            'inner: for binding in self.tcx.iter().rev() {
                match binding {
                    Binding::TyVarBind(n, ty) if n == &name => {
                        if let Ty::Var(new_name) = ty {
                            name = new_name.clone();
                            continue 'outer; // Resolve the new type variable.
                        } else {
                            return Ok(ty.clone());
                        }
                    }
                    _ => continue 'inner,
                }
            }
            break 'outer Err(TyCheckErr::UnboundTyVariable(loc.clone(), name.to_string()));
        }
    }

    fn resolve_ty_vars(&self, loc: &Loc, ty: &Ty) -> Result<Ty, TyCheckErr> {
        match ty {
            Ty::Var(name) => self.lookup_ty_var(loc, name),
            Ty::Arr(x, y) => {
                let x = self.resolve_ty_vars(loc, x.as_ref())?;
                let y = self.resolve_ty_vars(loc, y.as_ref())?;
                Ok(Ty::Arr(Box::new(x), Box::new(y)))
            }
            Ty::Builtin(_) => Ok(ty.clone()),
        }
    }
}
