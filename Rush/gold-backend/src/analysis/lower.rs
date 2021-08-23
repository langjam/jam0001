use std::collections::HashMap;
use std::ops::Range;

use gold_frontend::error::{report_type_error, TypeError};
use gold_frontend::frontend::{Expr, Type};
use gold_standard::load::{PRINT_SYMBOL, PRINTLN_SYMBOL, PRINT_INT_SYMBOL, PRINT_FLOAT_SYMBOL};

pub struct FuncSig {
    pub return_type: Type,
    pub param_types: Vec<(Type, Range<usize>)>,
    pub scope_index: usize,
}

#[derive(Clone)]
pub struct VarSig {
    pub ty: Type,
}

pub struct VariableRegistry {
    pub scopes: Vec<HashMap<String, VarSig>>,
}

impl VariableRegistry {
    fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }

    fn unify(&self) -> HashMap<String, VarSig> {
        let mut unified_scope = HashMap::new();

        for x in self.scopes.iter() {
            let cloned = x.to_owned();
            unified_scope.extend(cloned);
        }

        unified_scope
    }

    fn top_stack(&mut self) -> &mut HashMap<String, VarSig> {
        self.scopes.last_mut().unwrap()
    }

    pub fn push(&mut self, name: String, sig: VarSig) {
        self.top_stack().insert(name, sig);
    }

    pub fn get(&mut self, index: usize, name: String) -> &VarSig {
        self.scopes.get(index).unwrap().get(&name).unwrap()
    }
}

pub struct Analyzer {
    pub functions: HashMap<String, FuncSig>,
    pub variables: VariableRegistry,
    pub source: String,
    pub filename: String,
    pub errors: usize,
}

impl Analyzer {
    pub fn new(src: String, filename: String) -> Self {
        let mut functions = HashMap::new();
        functions.insert(PRINT_SYMBOL.to_owned(), FuncSig {
            return_type: Type::Int,
            param_types: vec![(Type::String, Range::default())],
            scope_index: 0,
        });
        functions.insert(PRINTLN_SYMBOL.to_owned(), FuncSig {
            return_type: Type::Int,
            param_types: vec![(Type::String, Range::default())],
            scope_index: 0,
        });
        functions.insert(PRINT_INT_SYMBOL.to_owned(), FuncSig {
            return_type: Type::Void,
            param_types: vec![(Type::Int, Range::default())],
            scope_index: 0,
        });
        functions.insert(PRINT_FLOAT_SYMBOL.to_owned(), FuncSig {
            return_type: Type::Void,
            param_types: vec![(Type::Float, Range::default())],
            scope_index: 0,
        });
        Self {
            functions,
            variables: VariableRegistry::new(),
            source: src,
            filename: filename,
            errors: 0,
        }
    }
}

pub trait Lower {
    fn get_type(&self, func_ref: &HashMap<String, FuncSig>, var_ref: &HashMap<String, VarSig>) -> Type;
    fn typecheck(&self, typechecker: &mut Analyzer);
}

impl Lower for Expr {
    fn get_type(&self, func_ref: &HashMap<String, FuncSig>, var_ref: &HashMap<String, VarSig>) -> Type {
        match self {
            Expr::NoExpr => unreachable!(),
            Expr::Number(_, _) => Type::Int,
            Expr::String(_, _) => Type::String,
            Expr::Parameter(param, _) => param.typename,
            Expr::Function(_, _, ty, _, _) => *ty,
            Expr::Else(block, _) => {
                match block.as_ref() {
                    Expr::Block(stmts, _) => stmts.last().unwrap().get_type(func_ref, var_ref),
                    _ => unreachable!()
                }
            }
            Expr::Elif(_, block, _) => {
                match block.as_ref() {
                    Expr::Block(stmts, _) => stmts.last().unwrap().get_type(func_ref, var_ref),
                    _ => unreachable!()
                }
            }
            Expr::If(_, block, _, _, _) => {
                match block.as_ref() {
                    Expr::Block(stmts, _) => stmts.last().unwrap().get_type(func_ref, var_ref),
                    _ => unreachable!()
                }
            }
            Expr::While(_, block, _) => {
                match block.as_ref() {
                    Expr::Block(stmts, _) => stmts.last().unwrap().get_type(func_ref, var_ref),
                    _ => unreachable!()
                }
            }
            Expr::Call(name, _, _, _) => {
                let probably_correct_func = func_ref.get(name).unwrap();
                probably_correct_func.return_type
            }
            Expr::List(values, _) => values.first().unwrap().get_type(func_ref, var_ref),
            Expr::Equality(_, _) => Type::Bool,
            Expr::NotEqual(_, _) => Type::Bool,
            Expr::GreaterThan(_, _) => Type::Bool,
            Expr::LessThan(_, _) => Type::Bool,
            Expr::GreaterThanEqual(_, _) => Type::Bool,
            Expr::LessThanEqual(_, _) => Type::Bool,
            Expr::Addition(_, _) => Type::Number,
            Expr::Subtraction(_, _) => Type::Number,
            Expr::Multiplication(_, _) => Type::Number,
            Expr::Division(_, _) => Type::Number,
            Expr::Power(_, _) => Type::Number,
            Expr::Var(ident, _) => todo!(),
            Expr::Assign(_, e, _) => e.get_type(func_ref, var_ref),
            Expr::Reassign(_, e, _) => e.get_type(func_ref, var_ref),
            Expr::Block(_, _) => Type::Void,
        }
    }

    fn typecheck(&self, typechecker: &mut Analyzer) {
        let unified_theory_of_shit = typechecker.variables.unify();
        match self {
            Expr::NoExpr => unreachable!(),
            Expr::Var(sym, err) => {
                match unified_theory_of_shit.get(sym) {
                    Some(var) => {}
                    None => {
                        report_type_error(TypeError::NotDefined(err.to_owned()), typechecker.filename.as_str(), typechecker.source.as_str());
                    }
                }
            }
            Expr::Assign(name, expr, _) => {
                let fun = &typechecker.functions;
                typechecker.variables.push(name.to_owned(), VarSig { ty: expr.get_type(fun, &unified_theory_of_shit) });
                expr.typecheck(typechecker);
            }
            Expr::Reassign(name, expr, err) => {
                match typechecker.variables.unify().get(name) {
                    Some(var) => { expr.typecheck(typechecker); }
                    None => {
                        report_type_error(TypeError::NotDefined(err.to_owned()), typechecker.filename.as_str(), typechecker.source.as_str());
                    }
                }
            }
            Expr::Number(_, _) => {}
            Expr::String(_, _) => {}
            Expr::Parameter(_, _) => todo!(),
            Expr::Function(name, params, ty, block, loc) => {
                // Add the function scope
                typechecker.variables.scopes.push(HashMap::new());
                typechecker.functions.insert(name.to_owned(), FuncSig {
                    return_type: *ty,
                    param_types: params.iter().map(|p| ((*p).0.typename, (*p).1.to_owned())).collect::<Vec<(Type, Range<usize>)>>(),
                    scope_index: typechecker.variables.scopes.len() - 1,
                });
                block.typecheck(typechecker);
            }
            Expr::Block(stmts, _) => {
                for stmt in stmts {
                    stmt.typecheck(typechecker)
                }
            }
            Expr::Else(block, _) => block.typecheck(typechecker),
            Expr::Elif(block, _, _) => block.typecheck(typechecker),
            Expr::If(cond, body, elifs, else_body, loc) => {
                cond.typecheck(typechecker);
                if elifs.is_some() {
                    for elif in elifs.as_ref().unwrap() { elif.typecheck(typechecker); }
                }
                if else_body.is_some() {
                    else_body.as_ref().unwrap().typecheck(typechecker)
                }
            }
            Expr::Call(name, args, nloc, arg_loc) => {
                let probably_correct_func = (&typechecker).functions.get(name);
                match probably_correct_func {
                    Some(func) => {
                        if args.len() != func.param_types.len() {
                            report_type_error(TypeError::IncorrectNumberOfFunctionArguments(arg_loc.to_owned(), func.param_types.len(), args.len()), typechecker.filename.as_str(), typechecker.source.as_str());
                        }
                        for (pos, arg) in args.iter().enumerate() {
                            let (ty, def) = func.param_types.get(pos).unwrap();
                            let arg_type = arg.get_type(&typechecker.functions, &HashMap::new());
                            if arg_type != *ty {
                                report_type_error(TypeError::IncorrectTypeValueForArgument(def.to_owned(), arg.expression_range(), *ty, arg_type), typechecker.filename.as_str(), typechecker.source.as_str());
                            }
                        }
                    }
                    None => {
                        report_type_error(TypeError::FunctionDoesNotExist(name.to_string(), nloc.to_owned()), typechecker.filename.as_str(), typechecker.source.as_str());
                    }
                }
            }
            Expr::While(cond, body, _) => {
                cond.typecheck(typechecker);
                body.typecheck(typechecker);
            }
            Expr::List(_, _) => todo!(),
            Expr::Equality(lhs, rhs) => {
                let (lhs_ty, rhs_ty) = (lhs.get_type(&typechecker.functions, &unified_theory_of_shit), rhs.get_type(&typechecker.functions, &unified_theory_of_shit));
                if lhs_ty != rhs_ty {
                    report_type_error(TypeError::InvaidTypesForOperation(lhs.expression_range(), rhs.expression_range(), lhs_ty, rhs_ty), typechecker.filename.as_str(), typechecker.source.as_str());
                }
            }
            Expr::NotEqual(lhs, rhs) => {
                let (lhs_ty, rhs_ty) = (lhs.get_type(&typechecker.functions, &unified_theory_of_shit), rhs.get_type(&typechecker.functions, &unified_theory_of_shit));
                if lhs_ty != rhs_ty {
                    report_type_error(TypeError::InvaidTypesForOperation(lhs.expression_range(), rhs.expression_range(), lhs_ty, rhs_ty), typechecker.filename.as_str(), typechecker.source.as_str());
                }
            }
            Expr::GreaterThan(lhs, rhs) => {
                let (lhs_ty, rhs_ty) = (lhs.get_type(&typechecker.functions, &typechecker.variables.unify()), rhs.get_type(&typechecker.functions, &typechecker.variables.unify()));
                if lhs_ty != rhs_ty {
                    report_type_error(TypeError::InvaidTypesForOperation(lhs.expression_range(), rhs.expression_range(), lhs_ty, rhs_ty), typechecker.filename.as_str(), typechecker.source.as_str());
                }
            }
            Expr::LessThan(lhs, rhs) => {
                let (lhs_ty, rhs_ty) = (lhs.get_type(&typechecker.functions, &unified_theory_of_shit), rhs.get_type(&typechecker.functions, &unified_theory_of_shit));
                if lhs_ty != rhs_ty {
                    report_type_error(TypeError::InvaidTypesForOperation(lhs.expression_range(), rhs.expression_range(), lhs_ty, rhs_ty), typechecker.filename.as_str(), typechecker.source.as_str());
                }
            }
            Expr::GreaterThanEqual(lhs, rhs) => {
                let (lhs_ty, rhs_ty) = (lhs.get_type(&typechecker.functions, &unified_theory_of_shit), rhs.get_type(&typechecker.functions, &unified_theory_of_shit));
                if lhs_ty != rhs_ty {
                    report_type_error(TypeError::InvaidTypesForOperation(lhs.expression_range(), rhs.expression_range(), lhs_ty, rhs_ty), typechecker.filename.as_str(), typechecker.source.as_str());
                }
            }
            Expr::LessThanEqual(lhs, rhs) => {
                let (lhs_ty, rhs_ty) = (lhs.get_type(&typechecker.functions, &unified_theory_of_shit), rhs.get_type(&typechecker.functions, &unified_theory_of_shit));
                if lhs_ty != rhs_ty {
                    report_type_error(TypeError::InvaidTypesForOperation(lhs.expression_range(), rhs.expression_range(), lhs_ty, rhs_ty), typechecker.filename.as_str(), typechecker.source.as_str());
                }
            }
            Expr::Addition(lhs, rhs) => {
                let (lhs_ty, rhs_ty) = (lhs.get_type(&typechecker.functions, &unified_theory_of_shit), rhs.get_type(&typechecker.functions, &unified_theory_of_shit));
                if lhs_ty != rhs_ty {
                    report_type_error(TypeError::InvaidTypesForOperation(lhs.expression_range(), rhs.expression_range(), lhs_ty, rhs_ty), typechecker.filename.as_str(), typechecker.source.as_str());
                }
            }
            Expr::Subtraction(lhs, rhs) => {
                let (lhs_ty, rhs_ty) = (lhs.get_type(&typechecker.functions, &unified_theory_of_shit), rhs.get_type(&typechecker.functions, &unified_theory_of_shit));
                if lhs_ty != rhs_ty {
                    report_type_error(TypeError::InvaidTypesForOperation(lhs.expression_range(), rhs.expression_range(), lhs_ty, rhs_ty), typechecker.filename.as_str(), typechecker.source.as_str());
                }
            }
            Expr::Multiplication(lhs, rhs) => {
                let (lhs_ty, rhs_ty) = (lhs.get_type(&typechecker.functions, &unified_theory_of_shit), rhs.get_type(&typechecker.functions, &unified_theory_of_shit));
                if lhs_ty != rhs_ty {
                    report_type_error(TypeError::InvaidTypesForOperation(lhs.expression_range(), rhs.expression_range(), lhs_ty, rhs_ty), typechecker.filename.as_str(), typechecker.source.as_str());
                }
            }
            Expr::Division(lhs, rhs) => {
                let (lhs_ty, rhs_ty) = (lhs.get_type(&typechecker.functions, &unified_theory_of_shit), rhs.get_type(&typechecker.functions, &unified_theory_of_shit));
                if lhs_ty != rhs_ty {
                    report_type_error(TypeError::InvaidTypesForOperation(lhs.expression_range(), rhs.expression_range(), lhs_ty, rhs_ty), typechecker.filename.as_str(), typechecker.source.as_str());
                }
            }
            Expr::Power(lhs, rhs) => {
                let (lhs_ty, rhs_ty) = (lhs.get_type(&typechecker.functions, &unified_theory_of_shit), rhs.get_type(&typechecker.functions, &unified_theory_of_shit));
                if lhs_ty != rhs_ty {
                    report_type_error(TypeError::InvaidTypesForOperation(lhs.expression_range(), rhs.expression_range(), lhs_ty, rhs_ty), typechecker.filename.as_str(), typechecker.source.as_str());
                }
            }
        }
    }
}