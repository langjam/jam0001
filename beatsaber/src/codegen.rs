use crate::ast2;
use anyhow::Result;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple,
};
use inkwell::types::{BasicTypeEnum, IntType};
use inkwell::values::{BasicValueEnum, FunctionValue, GlobalValue, IntValue, PointerValue};
use inkwell::{AddressSpace, OptimizationLevel};
use std::collections::{BTreeMap, HashMap};
use std::io::Write;
use std::mem;
use std::path::Path;

/// Look for any identifier that is not declared in this function and assume they are captures.
fn find_captures(stmts: &[ast2::DecoratedStmt], params: &[usize]) -> Vec<usize> {
    let mut locals = params.to_vec();
    let mut captures = Vec::new();
    fn process_expr(locals: &[usize], captures: &mut Vec<usize>, expr: &ast2::DecoratedExpr) {
        match expr {
            ast2::DecoratedExpr::Identifier(ident) => {
                if !locals.contains(&ident.id) {
                    captures.push(ident.id);
                }
            }
            ast2::DecoratedExpr::CallExpr(expr) => {
                process_expr(locals, captures, &*expr.p1);
                if let Some(p2) = &expr.p2 {
                    process_expr(locals, captures, &*p2);
                }
            }
        }
    }
    fn process_stmt(
        locals: &mut Vec<usize>,
        captures: &mut Vec<usize>,
        stmt: &ast2::DecoratedStmt,
    ) {
        match stmt {
            ast2::DecoratedStmt::LoadLiteralNumber(stmt) => {
                locals.push(stmt.ident.id);
            }
            ast2::DecoratedStmt::Callable(ast2::Callable::FuncBlock(block)) => {
                locals.push(block.decl.p1.id);
                if let Some(p2) = block.decl.p2 {
                    locals.push(p2.id);
                }
                for stmt in &block.block {
                    process_stmt(locals, captures, stmt);
                }
            }
            ast2::DecoratedStmt::Conditional(stmt) => {
                process_stmt(locals, captures, &*stmt.success);
            }
            ast2::DecoratedStmt::Assignment(stmt) => {
                process_expr(locals, captures, &stmt.value);
                if let Some(ident) = stmt.name {
                    locals.push(ident.id);
                }
            }
            ast2::DecoratedStmt::ReturnStmt(stmt) => {
                process_expr(locals, captures, &stmt.expr);
            }
            _ => {}
        }
    }
    for stmt in stmts {
        process_stmt(&mut locals, &mut captures, stmt);
    }
    captures
}

fn get_line_count(stmts: &[ast2::DecoratedStmt]) -> u32 {
    let mut count = 0;
    fn process_stmt(count: &mut u32, stmt: &ast2::DecoratedStmt) {
        if let ast2::DecoratedStmt::Callable(ast2::Callable::FuncBlock(block)) = stmt {
            for stmt in &block.block {
                process_stmt(count, stmt);
            }
        }
        *count = (*count).max(stmt.line_number() as u32);
    }
    for stmt in stmts {
        process_stmt(&mut count, stmt);
    }
    count
}

pub type OptLevel = OptimizationLevel;

pub struct CodegenOptions<'a> {
    pub output: &'a Path,
    pub optimization: OptLevel,
    /// Position Independent Code
    pub pic: bool,
    /// Target triple, None for host
    pub target: Option<String>,
    pub include_c: Vec<String>,
}

pub struct Codegen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    i64: IntType<'ctx>,
    func_compile_queue: Vec<ast2::FuncBlock>,
    functions: HashMap<usize, FunctionValue<'ctx>>,
    /// fn id -> [capture id]
    function_captures: HashMap<usize, Vec<usize>>,
    line_lut: GlobalValue<'ctx>,
    // line -> basic block addr
    lut_entries: BTreeMap<usize, PointerValue<'ctx>>,

    cur_locals: HashMap<usize, PointerValue<'ctx>>,
    cur_func: Option<FunctionValue<'ctx>>,
    cur_line_map: HashMap<usize, BasicBlock<'ctx>>,
}

impl<'ctx> Codegen<'ctx> {
    pub fn compile(ast: Vec<ast2::DecoratedStmt>, options: CodegenOptions) -> Result<()> {
        let context = Context::create();
        let module = context.create_module("beat saber");

        let line_count = get_line_count(&ast);
        let line_lut_ty = context
            .i8_type()
            .ptr_type(AddressSpace::Generic)
            .array_type(line_count);
        let line_lut = module.add_global(line_lut_ty, Some(AddressSpace::Const), "line_lut");

        let mut codegen = Codegen {
            context: &context,
            module,
            builder: context.create_builder(),
            i64: context.i64_type(),
            func_compile_queue: Vec::new(),
            functions: HashMap::new(),
            function_captures: HashMap::new(),
            line_lut,
            lut_entries: BTreeMap::new(),

            cur_locals: HashMap::new(),
            cur_func: None,
            cur_line_map: HashMap::new(),
        };

        codegen.declare_func_children(&ast);
        codegen.build_main(ast);

        let mut lut_data = Vec::new();
        for (&line, &ptr) in &codegen.lut_entries {
            while lut_data.len() < line {
                lut_data.push(ptr);
            }
        }
        let elem_ty = context.i8_type().ptr_type(AddressSpace::Generic);
        line_lut.set_initializer(&elem_ty.const_array(&lut_data));

        codegen.write_object(options)
    }

    fn declare_func_children(&mut self, stmts: &[ast2::DecoratedStmt]) {
        for stmt in stmts {
            match stmt {
                ast2::DecoratedStmt::Callable(ast2::Callable::ExternFunction(stmt)) => {
                    self.function_captures.insert(stmt.ident.id, Vec::new());
                    let mut param_types = vec![BasicTypeEnum::IntType(self.i64)];
                    if stmt.two_param {
                        param_types.push(BasicTypeEnum::IntType(self.i64));
                    }
                    let fn_type = self.i64.fn_type(&param_types, false);
                    let fn_val =
                        self.module
                            .add_function(&stmt.name, fn_type, Some(Linkage::External));
                    self.functions.insert(stmt.ident.id, fn_val);
                }
                ast2::DecoratedStmt::Callable(ast2::Callable::FuncBlock(stmt)) => {
                    let mut param_types = vec![BasicTypeEnum::IntType(self.i64)];
                    let mut params = vec![stmt.decl.p1.id];
                    if let Some(p2) = stmt.decl.p2 {
                        params.push(p2.id);
                        param_types.push(BasicTypeEnum::IntType(self.i64));
                    }

                    let captures = find_captures(&stmt.block, &params);
                    for _ in &captures {
                        param_types.push(BasicTypeEnum::IntType(self.i64));
                    }
                    self.function_captures.insert(stmt.decl.id.id, captures);

                    let fn_type = self.i64.fn_type(&param_types, false);
                    let fn_val = self.module.add_function(
                        &stmt.decl.id.id.to_string(),
                        fn_type,
                        Some(Linkage::Internal),
                    );
                    self.functions.insert(stmt.decl.id.id, fn_val);

                    self.declare_func_children(&stmt.block);
                }
                _ => {}
            }
        }
    }

    fn get_local(&mut self, id: usize, create: bool) -> PointerValue<'ctx> {
        if let Some(local) = self.cur_locals.get(&id) {
            *local
        } else if create {
            let builder = self.context.create_builder();

            let entry = self.cur_func.unwrap().get_first_basic_block().unwrap();

            match entry.get_first_instruction() {
                Some(first_instr) => builder.position_before(&first_instr),
                None => builder.position_at_end(entry),
            }

            let ptr = builder.build_alloca(self.i64, "");
            self.cur_locals.insert(id, ptr);
            ptr
        } else {
            panic!("Could not find local {}", id);
        }
    }

    fn build_main(&mut self, body: Vec<ast2::DecoratedStmt>) {
        let param_ty = BasicTypeEnum::IntType(self.i64);
        let fn_type = self.i64.fn_type(&[param_ty, param_ty], false);
        let fn_val = self
            .module
            .add_function("main", fn_type, Some(Linkage::External));
        self.cur_func = Some(fn_val);

        let params = fn_val.get_params();
        let entry = self.context.append_basic_block(fn_val, "");
        self.builder.position_at_end(entry);

        let p1alloca = self.builder.build_alloca(self.i64, "argc");
        self.builder.build_store(p1alloca, params[0]);
        self.cur_locals.insert(ast2::ARGC_IDENT.id, p1alloca);

        let p2alloca = self.builder.build_alloca(self.i64, "argv");
        self.builder.build_store(p2alloca, params[1]);
        self.cur_locals.insert(ast2::ARGV_IDENT.id, p2alloca);

        for stmt in &body {
            let line = stmt.line_number();
            if matches!(stmt, ast2::DecoratedStmt::Callable(_)) {
                continue;
            }
            let block = self.context.append_basic_block(fn_val, "");
            self.cur_line_map.insert(line, block);
            let addr = unsafe {
                let f = mem::transmute::<_, _>(self.cur_func.unwrap());
                let bb = mem::transmute::<_, _>(block);
                let val = llvm_sys::core::LLVMBlockAddress(f, bb);
                mem::transmute::<_, PointerValue>(val)
            };
            self.lut_entries.insert(line, addr);
        }

        self.builder
            .build_unconditional_branch(entry.get_next_basic_block().unwrap());

        for stmt in body {
            self.build_stmt(stmt, None);
        }

        while !self.func_compile_queue.is_empty() {
            let func = self.func_compile_queue.pop().unwrap();
            let p1 = func.decl.p1.id;
            let p2 = func.decl.p2.map(|id| id.id);
            self.build_func(func.block, func.decl.id.id, p1, p2);
        }

        if let Some(func) = self.cur_func {
            if func.verify(true) {
            } else {
                unsafe {
                    func.delete();
                }

                panic!("stack bad");
            }
        }
    }

    fn build_func(
        &mut self,
        body: Vec<ast2::DecoratedStmt>,
        id: usize,
        p1: usize,
        p2: Option<usize>,
    ) {
        self.cur_locals.clear();
        self.cur_line_map.clear();
        let fn_val = *self.functions.get(&id).unwrap();
        self.cur_func = Some(fn_val);

        let params = fn_val.get_params();

        let entry = self.context.append_basic_block(fn_val, "");
        self.builder.position_at_end(entry);
        let p1alloca = self.builder.build_alloca(self.i64, "");
        self.builder.build_store(p1alloca, params[0]);
        self.cur_locals.insert(p1, p1alloca);

        if let Some(p2) = p2 {
            let p2alloca = self.builder.build_alloca(self.i64, "");
            self.builder.build_store(p2alloca, params[1]);
            self.cur_locals.insert(p2, p2alloca);
        }

        let capture_offset = p2.is_some() as usize + 1;
        for (i, &capture) in self.function_captures[&id].iter().enumerate() {
            let alloca = self.builder.build_alloca(self.i64, "");
            self.builder.build_store(alloca, params[capture_offset + i]);
            self.cur_locals.insert(capture, alloca);
        }

        for stmt in &body {
            let line = stmt.line_number();
            if matches!(stmt, ast2::DecoratedStmt::Callable(_)) {
                continue;
            }
            let block = self.context.append_basic_block(fn_val, "");
            self.cur_line_map.insert(line, block);
            let addr = unsafe {
                let f = mem::transmute::<_, _>(self.cur_func.unwrap());
                let bb = mem::transmute::<_, _>(block);
                let val = llvm_sys::core::LLVMBlockAddress(f, bb);
                mem::transmute::<_, PointerValue>(val)
            };
            self.lut_entries.insert(line, addr);
        }

        self.builder
            .build_unconditional_branch(entry.get_next_basic_block().unwrap());

        for stmt in body {
            self.build_stmt(stmt, None);
        }

        if let Some(func) = self.cur_func {
            if func.verify(true) {
            } else {
                unsafe {
                    func.delete();
                }

                panic!("stack bad");
            }
        }
    }

    fn build_expr(&mut self, expr: ast2::DecoratedExpr) -> IntValue<'ctx> {
        match expr {
            ast2::DecoratedExpr::CallExpr(expr) => {
                let fn_val = self.functions[&expr.function.id];
                let mut args = Vec::new();
                let p1 = self.build_expr(*expr.p1);
                args.push(BasicValueEnum::IntValue(p1));
                if let Some(p2) = expr.p2 {
                    args.push(BasicValueEnum::IntValue(self.build_expr(*p2)));
                }
                for &capture in &self.function_captures[&expr.function.id].clone() {
                    let ptr = self.get_local(capture, false);
                    args.push(self.builder.build_load(ptr, ""));
                }
                self.builder
                    .build_call(fn_val, &args, "")
                    .try_as_basic_value()
                    .left()
                    .unwrap()
                    .into_int_value()
            }
            ast2::DecoratedExpr::Identifier(expr) => {
                let ptr = self.get_local(expr.id, false);
                self.builder.build_load(ptr, "").into_int_value()
            }
        }
    }

    fn build_stmt(&mut self, stmt: ast2::DecoratedStmt, continue_block: Option<BasicBlock>) {
        if let ast2::DecoratedStmt::Callable(stmt) = stmt {
            if let ast2::Callable::FuncBlock(stmt) = stmt {
                // println!("Pushing function block to compile queue {:?}", stmt);
                self.func_compile_queue.push(stmt);
            }
            return;
        }
        let is_terminator = matches!(
            stmt,
            ast2::DecoratedStmt::ReturnStmt(_)
                | ast2::DecoratedStmt::GotoStmt(_)
                | ast2::DecoratedStmt::Conditional(_)
        );

        let line = stmt.line_number();
        let block = self.cur_line_map[&line];
        self.builder.position_at_end(block);
        match stmt {
            ast2::DecoratedStmt::LoadLiteralNumber(stmt) => {
                let ptr = self.get_local(stmt.ident.id, true);
                let val = self.i64.const_int(stmt.value as u64, false);
                self.builder.build_store(ptr, val);
            }
            ast2::DecoratedStmt::Conditional(stmt) => {
                let cond = self.build_expr(ast2::DecoratedExpr::Identifier(stmt.condition));
                let z = self.i64.const_zero();
                let cond_c = self
                    .builder
                    .build_int_compare(inkwell::IntPredicate::NE, cond, z, "");
                let else_block = block.get_next_basic_block().unwrap();
                let then_block = self.context.append_basic_block(self.cur_func.unwrap(), "");
                // shitty hack to get it to write this statement to the then block
                self.cur_line_map.insert(line, then_block);
                self.build_stmt(*stmt.success, Some(else_block));
                self.cur_line_map.insert(line, block);

                self.builder.position_at_end(block);
                self.builder
                    .build_conditional_branch(cond_c, then_block, else_block);
            }
            ast2::DecoratedStmt::Assignment(stmt) => {
                let val = self.build_expr(stmt.value);
                if let Some(id) = stmt.name {
                    let ptr = self.get_local(id.id, true);
                    self.builder.build_store(ptr, val);
                }
            }
            ast2::DecoratedStmt::ReturnStmt(stmt) => {
                let val = self.build_expr(stmt.expr);
                self.builder.build_return(Some(&val));
            }
            ast2::DecoratedStmt::GotoStmt(stmt) => {
                let val = self.build_expr(stmt.target);
                // subtract line num by one
                let val = self
                    .builder
                    .build_int_sub(val, self.i64.const_int(1, false), "");
                let lut = self.line_lut.as_pointer_value();

                let lut_elem = unsafe {
                    let z = self.i64.const_zero();
                    self.builder.build_in_bounds_gep(lut, &[z, val], "")
                };
                let target = self.builder.build_load(lut_elem, "").into_pointer_value();

                let destinations: Vec<BasicBlock> = self.cur_line_map.values().cloned().collect();
                self.builder.build_indirect_branch(target, &destinations);
            }
            ast2::DecoratedStmt::LoadLiteralString(stmt) => {
                let elem_type = self.context.i8_type();
                let ty = elem_type.array_type(stmt.value.len() as u32 + 1);
                let global = self.module.add_global(ty, Some(AddressSpace::Const), "");
                let mut items: Vec<_> = stmt
                    .value
                    .bytes()
                    .map(|b| elem_type.const_int(b as u64, false))
                    .collect();
                items.push(elem_type.const_zero());
                let val = elem_type.const_array(&items);
                global.set_initializer(&val);
                let ptr = global.as_pointer_value();

                let local = self.get_local(stmt.ident.id, true);
                let int = self.builder.build_ptr_to_int(ptr, self.i64, "");
                self.builder.build_store(local, int);
            }
            a => unreachable!("{:?}", a),
        }

        if !is_terminator {
            if let Some(block) = continue_block {
                self.builder.build_unconditional_branch(block);
            } else if let Some(next_block) = block.get_next_basic_block() {
                self.builder.build_unconditional_branch(next_block);
            } else {
                let val = self.i64.const_int(0, false);
                self.builder.build_return(Some(&val));
            }
        }
    }

    fn write_object(&self, options: CodegenOptions) -> Result<()> {
        Target::initialize_all(&InitializationConfig::default());

        let triple = if let Some(triple) = &options.target {
            TargetTriple::create(triple)
        } else {
            TargetMachine::get_default_triple()
        };
        let target = Target::from_triple(&triple).unwrap();
        let (cpu, features) = if options.target.is_some() {
            // TODO: cli option for cpu and features
            (String::new(), String::new())
        } else {
            let cpu = TargetMachine::get_host_cpu_name().to_string();
            let features = TargetMachine::get_host_cpu_features().to_string();
            (cpu, features)
        };

        let reloc = if options.pic {
            RelocMode::PIC
        } else {
            RelocMode::Default
        };
        let model = CodeModel::Default;
        let opt = options.optimization;

        let target_machine = target
            .create_target_machine(&triple, &cpu, &features, opt, reloc, model)
            .unwrap();

        // self.module.print_to_stderr();
        let tmp_out = format!("{}.tmp", options.output.display());
        target_machine
            .write_to_file(&self.module, FileType::Object, Path::new(&tmp_out))
            .unwrap();

        let includes = options
            .include_c
            .iter()
            .map(|c| self.compile_c(c, &triple, opt as u32))
            .collect::<Result<Vec<_>, _>>()?;

        let cc = cc::Build::new()
            .target(triple.as_str().to_str().unwrap())
            .host(env!("HOST"))
            .opt_level(opt as u32)
            .cargo_metadata(false)
            .try_get_compiler()?;
        let out_path_flag = if cc.is_like_msvc() {
            format!("/Fo\"{}\"", options.output.display())
        } else {
            format!("-o{}", options.output.display())
        };
        let output = cc
            .to_command()
            .arg(&tmp_out)
            .args(includes.iter())
            .arg(out_path_flag)
            .output()?;
        if !output.status.success() {
            std::io::stderr().lock().write_all(&output.stderr)?;
            std::io::stderr().lock().write_all(&output.stderr)?;
        }

        std::fs::remove_file(tmp_out)?;
        for include in includes {
            std::fs::remove_file(include)?;
        }

        Ok(())
    }

    fn compile_c(&self, file: impl AsRef<Path>, target: &TargetTriple, opt: u32) -> Result<String> {
        let file = file.as_ref();
        let out_file = format!("{}.tmp", file.display());
        let cc = cc::Build::new()
            .target(target.as_str().to_str().unwrap())
            .host(env!("HOST"))
            .opt_level(opt)
            .cargo_metadata(false)
            .try_get_compiler()?;
        let no_link_flag = if cc.is_like_msvc() { "/c" } else { "-c" };
        let out_path_flag = if cc.is_like_msvc() {
            format!("/Fo\"{}\"", out_file)
        } else {
            format!("-o{}", out_file)
        };
        let output = cc
            .to_command()
            .arg(file)
            .arg(no_link_flag)
            .arg(out_path_flag)
            .output()?;
        if !output.status.success() {
            std::io::stderr().lock().write_all(&output.stderr)?;
            std::io::stderr().lock().write_all(&output.stderr)?;
        }
        Ok(out_file)
    }
}
