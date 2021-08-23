use crate::ast::Ty;

#[derive(Clone)]
pub enum Binding {
    VarBind(String, Ty),
    TyVarBind(String, Ty),
}

impl std::fmt::Debug for Binding {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Binding::VarBind(name, ty) => write!(f, "Var({})={}", name, ty),
            Binding::TyVarBind(name, ty) => write!(f, "TyVar({})={}", name, ty),
        }
    }
}

pub type Tcx = Vec<Binding>;
