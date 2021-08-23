use std::{any::Any, collections::HashMap, mem, ops::Range};

use cranelift::{codegen::{Context, ir::function}, codegen, frontend::{FunctionBuilder, FunctionBuilderContext, Variable}, prelude::{AbiParam, Block, EntityRef, InstBuilder, IntCC, Signature, Value, types}};
use cranelift::codegen::binemit::NullStackMapSink;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataContext, Linkage, Module};

use gold_frontend::frontend::{Expr, Parameter, Type};
use gold_standard::{declare_functions, load_symbols};

use crate::analysis::lower::{Analyzer, Lower, VarSig};
use std::cell::RefCell;
use std::borrow::BorrowMut;

pub struct Compilation {
    variables: HashMap<String, Variable>,
    variable_index: usize,

    uncompiled_functions: Vec<Expr>,
    syntax_analyzer: Analyzer,

    data_context: DataContext,
}

impl Compilation {
    pub fn new(analyzer: Analyzer, functions: Vec<Expr>) -> Self {
        Self {
            variables: HashMap::new(),
            variable_index: 0,
            uncompiled_functions: functions,
            syntax_analyzer: analyzer,
            data_context: DataContext::new()
        }
    }

    pub fn gen_expr(&mut self,
                scope_index: &usize,
                expr: &Expr,
                module: &mut JITModule,
                builder: &mut FunctionBuilder) -> Value {
        match expr {
            Expr::Number(lit, _) => {
                builder.ins().iconst(types::I64, *lit)
            }
            Expr::String(lit, _) => {
                self.data_context.define(lit.to_string().into_boxed_str().into());
                let data = module.declare_data(lit, Linkage::Export, true, false).unwrap();
                module.define_data(data, &self.data_context).unwrap();
                self.data_context.clear();
                module.finalize_definitions();

                let data_id = module.declare_data_in_func(data, builder.func);

                let string = module.target_config().pointer_type();
                builder.ins().symbol_value(string, data_id)
            }
            Expr::Var(name, _) => {
                let var_sig = self.syntax_analyzer.variables.get(*scope_index, name.to_owned());
                let var = self.variables.entry(name.to_string()).or_insert({
                    let var = Variable::new(self.variable_index);
                    builder.declare_var(var, var_sig.ty.into());
                    var
                });
                builder.use_var(*var)
            }
            Expr::Assign(name, value, _) => {
                let val = self.gen_expr(scope_index, value, module, builder);
                let var_sig = self.syntax_analyzer.variables.get(*scope_index, name.to_owned());
                let var = self.variables.entry(name.to_string()).or_insert({
                    let var = Variable::new(self.variable_index);
                    builder.declare_var(var, var_sig.ty.into());
                    var
                });
                builder.def_var(*var, val);
                val
            }
            Expr::Reassign(name, value, _) => {
                let val = self.gen_expr(scope_index, value, module, builder);
                let var = self.variables.get(name).unwrap();
                builder.def_var(*var, val);
                val
            }
            Expr::Block(stmts, _) => {
                for pos in 0..stmts.len() - 1 {
                    let stmt = stmts.get(pos).unwrap();
                    self.gen_expr(scope_index, stmt, module, builder);
                }
                self.gen_expr(scope_index, stmts.get(stmts.len() - 1).unwrap(), module, builder)
            }
            Expr::Else(_, _) => todo!(),
            Expr::Elif(_, _, _) => todo!(),
            Expr::If(_, _, _, _, _) => todo!(),
            Expr::Call(name, args, _, _) => {
                let mut fn_sig = module.make_signature();
                let mut arg_values = Vec::new();
                for arg in args {
                    arg_values.push(self.gen_expr(scope_index, arg, module, builder));
                    fn_sig.params.push(arg.get_type(&self.syntax_analyzer.functions, &self.syntax_analyzer.variables.scopes.get(*scope_index).unwrap()).into());
                }
                fn_sig.returns.push(self.syntax_analyzer.functions.get(name).unwrap().return_type.into());

                let func = module.declare_function(&name, Linkage::Import, &fn_sig).unwrap();
                let func_ref = module.declare_func_in_func(func, &mut builder.func);
                let call = builder.ins().call(func_ref, &arg_values);
                builder.inst_results(call)[0]
            }
            Expr::While(cond, block, _) => {
                let cond_block = builder.create_block();
                let while_body = builder.create_block();
                let exit = builder.create_block();
                
                let condition = self.gen_expr(scope_index, cond, module, builder);

                builder.ins().jump(cond_block, &[]);
                builder.switch_to_block(cond_block);

                // If cond = false jump to exit else go to while block
                builder.ins().brz(condition, exit, &[]);
                builder.ins().jump(while_body, &[]);

                builder.switch_to_block(while_body);
                builder.seal_block(while_body);
                self.gen_expr(scope_index, block, module, builder);

                builder.ins().jump(cond_block, &[]);
                builder.switch_to_block(exit);

                builder.seal_block(cond_block);
                builder.seal_block(exit);
                builder.ins().iconst(types::I64, 0)
            }
            Expr::List(_, _) => todo!(),


            Expr::Equality(lhs, rhs) => {
                let left = self.gen_expr(scope_index, lhs, module, builder);
                let right = self.gen_expr(scope_index, rhs, module, builder);
                let compare = builder.ins().icmp(IntCC::Equal, left, right);
                builder.ins().bint(
                    lhs.get_type(&self.syntax_analyzer.functions, &self.syntax_analyzer.variables.scopes.get(*scope_index).unwrap()).into(),
                        compare
                )
            }
            Expr::NotEqual(lhs, rhs) => {
                let left = self.gen_expr(scope_index, lhs, module, builder);
                let right = self.gen_expr(scope_index, rhs, module, builder);
                let compare = builder.ins().icmp(IntCC::NotEqual, left, right);
                builder.ins().bint(
                    lhs.get_type(&self.syntax_analyzer.functions, &self.syntax_analyzer.variables.scopes.get(*scope_index).unwrap()).into(),
                        compare
                )
            }
            Expr::GreaterThan(lhs, rhs) => {
                let left = self.gen_expr(scope_index, lhs, module, builder);
                let right = self.gen_expr(scope_index, rhs, module, builder);
                let compare = builder.ins().icmp(IntCC::SignedGreaterThan, left, right);
                builder.ins().bint(
                    lhs.get_type(&self.syntax_analyzer.functions, &self.syntax_analyzer.variables.scopes.get(*scope_index).unwrap()).into(),
                        compare
                )
            }
            Expr::LessThan(lhs, rhs) => {
                let left = self.gen_expr(scope_index, lhs, module, builder);
                let right = self.gen_expr(scope_index, rhs, module, builder);
                let compare = builder.ins().icmp(IntCC::SignedLessThan, left, right);
                builder.ins().bint(
                    lhs.get_type(&self.syntax_analyzer.functions, &self.syntax_analyzer.variables.scopes.get(*scope_index).unwrap()).into(),
                        compare
                )
            }
            Expr::GreaterThanEqual(lhs, rhs) => {
                let left = self.gen_expr(scope_index, lhs, module, builder);
                let right = self.gen_expr(scope_index, rhs, module, builder);
                let compare = builder.ins().icmp(IntCC::SignedGreaterThanOrEqual, left, right);
                builder.ins().bint(
                    lhs.get_type(&self.syntax_analyzer.functions, &self.syntax_analyzer.variables.scopes.get(*scope_index).unwrap()).into(),
                        compare
                )
            }
            Expr::LessThanEqual(lhs, rhs) => {
                let left = self.gen_expr(scope_index, lhs, module, builder);
                let right = self.gen_expr(scope_index, rhs, module, builder);
                let compare = builder.ins().icmp(IntCC::SignedLessThanOrEqual, left, right);
                builder.ins().bint(
                    lhs.get_type(&self.syntax_analyzer.functions, &self.syntax_analyzer.variables.scopes.get(*scope_index).unwrap()).into(),
                        compare
                )
            }
            Expr::Addition(lhs, rhs) => {
                let left = self.gen_expr(scope_index, lhs, module, builder);
                let right = self.gen_expr(scope_index, rhs, module, builder);
                builder.ins().iadd(left, right)
            }
            Expr::Subtraction(lhs, rhs) => {
                let left = self.gen_expr(scope_index, lhs, module, builder);
                let right = self.gen_expr(scope_index, rhs, module, builder);
                builder.ins().isub(left, right)
            }
            Expr::Multiplication(lhs, rhs) => {
                let left = self.gen_expr(scope_index, lhs, module, builder);
                let right = self.gen_expr(scope_index, rhs, module, builder);
                builder.ins().imul(left, right)
            }
            Expr::Division(lhs, rhs) => {
                let left = self.gen_expr(scope_index, lhs, module, builder);
                let right = self.gen_expr(scope_index, rhs, module, builder);
                builder.ins().udiv(left, right)
            }
            Expr::Power(lhs, rhs) => todo!(),
            _ => todo!()
        }
    }

    pub fn gen_fn(&mut self, function: Expr) {
        let mut builder = JITBuilder::new(cranelift_module::default_libcall_names());

        // Declare all compiler builtins
        load_symbols(&mut builder);

        let mut module = JITModule::new(builder);
        let instrinsics = declare_functions(&mut module);

        let mut codegen_ctx = module.make_context();
        let mut function_ctx = FunctionBuilderContext::new();


        match function {
            Expr::Function(name, params, ty, mut function_body, _) => {

                // Cranelift Function Signature
                let mut fn_signature = module.make_signature();
                fn_signature.returns.push(ty.into());

                for p in &params {
                    fn_signature.params.push(p.0.typename.into());
                }


                let function_id = module
                    .declare_function(name.as_str(), Linkage::Local, &fn_signature)
                    .unwrap();

                codegen_ctx.func.signature = fn_signature;

                //===================== Code generation =====================

                // Setup the function builder
                let mut function_builder = FunctionBuilder::new(&mut codegen_ctx.func, &mut function_ctx);
                let function_builder = RefCell::new(function_builder);

                // Create entry block into the function and paramters
                let entry = function_builder.borrow_mut().create_block();
                function_builder.borrow_mut().append_block_params_for_function_params(entry);

                // Start codegen at the entry block and seal it to tell Cranelift 
                // that we have no blocks previous to this one.
                function_builder.borrow_mut().switch_to_block(entry);
                function_builder.borrow_mut().seal_block(entry);

                //========================================================================================
                let mut define_variable = |ty, name: &String| {
                    let var = Variable::new(self.variable_index);
                    if let None = self.variables.get(name) {
                        self.variables.insert(name.to_owned(), var);
                        self.variable_index += 1;
                        function_builder.borrow_mut().declare_var(var, ty);
                    }
                    var
                };
                //========================================================================================


                for (pos, param) in params.iter().enumerate() {
                    let val = function_builder.borrow_mut().block_params(entry)[pos];
                    let var =  define_variable(param.0.typename.into(), &param.0.name.to_owned());
                    function_builder.borrow_mut().def_var(var, val);
                }


                let zero = function_builder.borrow_mut().ins().iconst(types::I64, 21);
                let return_variable = define_variable(types::I64, &"why".to_owned());
                function_builder.borrow_mut().def_var(return_variable, zero);

                // Generate Cranelift IR for function body
                let variable_index = self.syntax_analyzer.functions.get(&name).unwrap().scope_index;
                match function_body.as_mut() {
                    Expr::Block(stmts, _) => {
                        for stmt in stmts {
                            self.gen_expr(&variable_index, stmt, &mut module, function_builder.borrow_mut().borrow_mut());
                        }
                    }
                    _ => unreachable!()
                }

                let return_var = self.variables.get(&"why".to_owned()).unwrap();
                let return_val = function_builder.borrow_mut().use_var(*return_var);

                function_builder.borrow_mut().ins().return_(&[return_val]);
                function_builder.borrow_mut().finalize();

                module.define_function(
                    function_id,
                    &mut codegen_ctx,
                    &mut codegen::binemit::NullTrapSink {},
                    &mut NullStackMapSink {},
                ).unwrap();

                module.clear_context(&mut codegen_ctx);
                module.finalize_definitions();

                let code = module.get_finalized_function(function_id);
                let code_fn = unsafe {
                    mem::transmute::<_, fn(i64, i64) -> i64>(code)
                };
                let v = code_fn(1, 2);
                println!("{}", v);
            }
            _ => unreachable!()
        };
    }
}