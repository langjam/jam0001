#[derive(Debug, Clone)]
pub struct Loc;

#[derive(Debug, Clone)]
pub enum Tm {
    Text(Loc, String),
    // Doc(Loc, Box<Tm>, Doc),
    Var(Loc, String),
    Lam(Loc, String, Ty, Box<Tm>),
    App(Loc, Box<Tm>, Box<Tm>),
    // TyLam(Loc, String, Box<Tm>),
    // TyApp(Loc, Box<Tm>, Ty),
}

// #[derive(Debug)]
// pub enum Doc {}

#[derive(Debug, Clone)]
pub enum Ty {
    Var(String),
    Arr(Box<Ty>, Box<Ty>),
    ForAll(String, Box<Ty>),
}

#[derive(Debug, Clone)]
pub enum Binding {
    NameBind,
    VarBind(Ty),
    // TyVarBind,
}
