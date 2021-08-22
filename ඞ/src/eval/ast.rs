use std::fs;

use crate::parse_input;

use super::{value::Function, *};

#[allow(unused)]
macro_rules! default {
    ($_:expr, $list:expr) => {
        $list
    };
    ($list:expr,) => {
        $list
    };
}

macro_rules! declare_builtin {
    ($name:literal $(;$($field:literal $(=> $value:expr)?),*)?) => {
        {
            #[allow(unused_mut)]
            let mut class = Class::new($name.to_string());
            $($(
                class.fields.insert($field.to_string(), default!(Value::None, $($value)?));
            )*)?
            class
        }
    };
}

#[macro_export]
macro_rules! ast_obj {
    ($name:literal $(; $($field:literal => $value:expr),*)?) => {
        $crate::eval::Value::Object($crate::eval::Object {
        class: $name.into(),
        fields: IntoIterator::into_iter([
            $($(
                ($field.to_string(), $value),
            )*)?
        ])
        .collect(),
    })
    };
}

impl Evaluator {
    pub(super) fn register_ast_classes(&mut self) {
        for class in [
            declare_builtin!("ASTBoolLiteral"; "value"),
            declare_builtin!("ASTIntLiteral"; "value"),
            declare_builtin!("ASTFloatLiteral"; "value"),
            declare_builtin!("ASTStringLiteral"; "text"),
            declare_builtin!("ASTComment"; "text"),
            declare_builtin!("ASTIdent"; "name"),
            declare_builtin!("ASTListLiteral"; "elements"),
            declare_builtin!("ASTBinaryExpr"; "op", "lhs", "rhs"),
            declare_builtin!("ASTRefExpr"; "value"),
            declare_builtin!("ASTObjectLiteral"; "class", "fields"),
            declare_builtin!("ASTFieldLiteral"; "name", "value"),
            declare_builtin!("ASTForLoop"; "var_name", "target", "body"),
            declare_builtin!("ASTIfStmt"; "condition", "body", "elses"),
            declare_builtin!("ASTFnCall"; "name", "args"),
            declare_builtin!("ASTLetStmt"; "var_name", "value"),
            declare_builtin!("ASTRetStmt"; "value"),
            declare_builtin!("ASTFnDef"; "name", "params", "body"),
            declare_builtin!("ASTClassDef"; "name", "fields", "methods"),
            declare_builtin!("ASTFile"; "name", "elements"),
        ] {
            self.add_class(class.name.clone(), class);
        }
    }

    pub fn eval_ast(&mut self, ast: &mut Value) -> EvalResult {
        let obj = match ast {
            Value::Object(o) => o,
            Value::ObjectRef(v) => return Ok(Value::Ptr(v.as_mut() as *mut _)),
            v => return Err(format!("Tried to evaluate AST, but found {:?}", v)),
        };
        let result = match obj.class.as_str() {
            "ASTBoolLiteral" | "ASTIntLiteral" | "ASTFloatLiteral" => obj.get_field("value")?,
            "ASTStringLiteral" => obj.get_field("text")?,
            "ASTComment" => {
                let text = match obj.get_field("text")? {
                    Value::String(s) => s,
                    _ => unreachable!(),
                };
                Value::Comment { content: text }
            }
            "ASTIdent" => {
                let name = match obj.get_field("name")? {
                    Value::String(s) => s,
                    _ => unreachable!(),
                };
                Value::VarRef { name }
            }
            "ASTListLiteral" => match obj.get_field_mut("elements")? {
                Value::List { elems } => {
                    let mut list_elems = Vec::new();
                    for elem in elems {
                        let mut list_elem = self.eval_ast(elem)?;
                        list_elems.push(self.deref_val(&mut list_elem)?.clone());
                    }
                    Value::List { elems: list_elems }
                }
                _ => unreachable!(),
            },
            "ASTBinaryExpr" => self.eval_bin_op(obj)?,
            "ASTRefExpr" => match &mut self.eval_ast(obj.get_field_mut("value")?)? {
                Value::VarRef { name } => {
                    Value::ObjectRef(Box::new(Value::Ptr(self.get_var(name.as_str())?)))
                }
                v @ Value::Ptr(_) => Value::ObjectRef(Box::new(v.clone())),
                v => return Err(format!("Cannot take a reference to value {}", v)),
            },
            "ASTLetStmt" => {
                let name = match obj.get_field("var_name")? {
                    Value::String(s) => s,
                    _ => unreachable!(),
                };
                let mut val = obj.get_field("value")?;
                let mut val = self.eval_ast(&mut val)?;
                let val = self.deref_val(&mut val)?;
                self.create_var(name, val.clone());
                Value::Unit
            }
            "ASTIfStmt" => {
                let elses: Vec<_> = match obj.get_field_mut("elses")? {
                    Value::List { elems } => elems.into_iter().map(|elem| elem as *mut _).collect(),
                    _ => unreachable!(),
                };
                let mut current_if = obj;
                let mut i = 0;
                loop {
                    let mut condition = self.eval_ast(current_if.get_field_mut("condition")?)?;
                    let condition = match self.deref_val(&mut condition)? {
                        Value::Bool(b) => *b,
                        _ => unreachable!(),
                    };
                    if !condition {
                        // current case is not hit, check next
                        match elses.get(i) {
                            Some(&new_if) => {
                                current_if = match unsafe { &mut *new_if } {
                                    Value::Object(o) => o,
                                    _ => unreachable!(),
                                }
                            }
                            None => break, // no more cases
                        }
                        i += 1;
                        continue;
                    } // else: current case is hit. eval.
                    let body = match current_if.get_field_mut("body")? {
                        Value::List { elems } => elems,
                        _ => unreachable!(),
                    };
                    self.enter_scope();
                    for stmt in body {
                        self.eval_ast(stmt)?;
                        if self.returning.is_some() {
                            break;
                        }
                    }
                    self.exit_scope();
                    break;
                }
                Value::Unit
            }
            "ASTForLoop" => {
                let name = match obj.get_field("var_name")? {
                    Value::String(s) => s,
                    _ => unreachable!(),
                };
                let target = obj.get_field_mut("target")?;
                let mut target = self.eval_ast(target)?;
                let elems = match self.deref_val(&mut target)? {
                    Value::List { elems } => elems,
                    v => return Err(format!("Cannot iterate over {}", v)),
                };
                let body = match obj.get_field_mut("body")? {
                    Value::List { elems } => elems,
                    _ => unreachable!(),
                };
                self.enter_scope();
                self.create_var(name.clone(), Value::None);
                'all_its: for elem in elems {
                    let loop_var = unsafe { &mut *self.get_var(&name).unwrap() };
                    *loop_var = if matches!(elem, Value::ObjectRef(_)) {
                        self.eval_ast(elem)?
                    } else {
                        elem.clone()
                    };
                    for stmt in &mut *body {
                        self.eval_ast(stmt)?;
                        if self.returning.is_some() {
                            break 'all_its;
                        }
                    }
                }
                self.exit_scope();
                Value::Unit
            }
            "ASTObjectLiteral" => {
                let class = match obj.get_field("class")? {
                    Value::String(s) => s,
                    _ => unreachable!(),
                };
                let fields = match obj.get_field("fields")? {
                    Value::List { elems } => elems,
                    _ => unreachable!(),
                };
                let mut obj_fields = HashMap::with_capacity(fields.len());
                for field in fields {
                    let obj = match field {
                        Value::Object(o) => o,
                        _ => unreachable!(),
                    };
                    assert_eq!(obj.class, "ASTFieldLiteral");
                    let name = match obj.get_field("name")? {
                        Value::String(s) => s,
                        _ => unreachable!(),
                    };
                    let mut value = self.eval_ast(&mut obj.get_field("value")?)?;
                    let value = self.deref_val(&mut value)?;
                    obj_fields.insert(name, value.clone());
                }
                Value::Object(Object {
                    class,
                    fields: obj_fields,
                })
            }
            "ASTFnDef" => {
                let function = self.eval_fn_def(obj)?;
                self.create_fn(function.name.clone(), function);
                Value::Unit
            }
            "ASTRetStmt" => {
                let mut val = self.eval_ast(&mut obj.get_field("value")?)?;
                let val = self.deref_val(&mut val)?;
                self.returning = Some(val.clone());
                Value::Unit
            }
            "ASTFnCall" => {
                let (name, mut eval_args) = self.extract_fn_info(obj)?;
                match self.get_fn(&name) {
                    Ok(mut f) => {
                        // user-defined
                        if eval_args.len() != f.params.len() {
                            return Err(format!(
                                "Expected {} arguments for function {}, but got {}",
                                f.params.len(),
                                name,
                                eval_args.len()
                            ));
                        }
                        self.enter_scope();
                        for (name, val) in f.params.iter().zip(eval_args.into_iter()) {
                            self.create_var(name, val);
                        }
                        let mut result = Value::Unit;
                        for stmt in &mut f.body {
                            self.eval_ast(stmt)?;
                            if let Some(val) = self.returning.take() {
                                result = val;
                                break;
                            }
                        }
                        self.exit_scope();
                        result
                    }
                    Err(s) => {
                        // maybe built-in
                        match name.as_str() {
                            "print" => {
                                for arg in eval_args {
                                    print!("{}", arg);
                                }
                                println!();
                                Value::Unit
                            }
                            "eval" => {
                                if eval_args.len() != 1 {
                                    return Err(format!(
                                        "Expected 1 arguments for function `eval`, but got {}",
                                        eval_args.len()
                                    ));
                                }
                                let ast = &mut eval_args[0];
                                match ast {
                                    Value::Object(o) => {
                                        if !o.class.starts_with("AST") {
                                            return Err(format!(
                                                "Cannot `eval` non-ast class {}",
                                                &o.class
                                            ));
                                        }
                                    }
                                    v => return Err(format!("Cannot `eval` non-object {}", v)),
                                }
                                remove_refs(ast);
                                self.eval_ast(ast)?
                            }
                            "read" => {
                                if eval_args.len() != 1 {
                                    return Err(format!(
                                        "Expected 1 arguments for function `read`, but got {}",
                                        eval_args.len()
                                    ));
                                }
                                let ast = &eval_args[0];
                                let path = match ast {
                                    Value::String(s) => s,
                                    v => {
                                        return Err(format!(
                                            "Argument to `read` must be a string (found {})",
                                            v
                                        ))
                                    }
                                };
                                match fs::read_to_string(&path).map_err(|e| e.to_string()) {
                                    Ok(content) => Value::String(content),
                                    Err(e) => ast_obj! { "Error";
                                        "msg" => Value::String(e)
                                    },
                                }
                            }
                            "parse" => {
                                if eval_args.len() != 2 {
                                    return Err(format!(
                                        "Expected 2 arguments for function `parse`, but got {}",
                                        eval_args.len()
                                    ));
                                }
                                let input = match &eval_args[0] {
                                    Value::String(s) => s,
                                    v => {
                                        return Err(format!(
                                            "First argument to `parse` must be a source string (found {})",
                                            v
                                        ))
                                    }
                                };
                                let input = input.as_str();
                                let name = match &eval_args[1] {
                                    Value::String(s) => s.clone(),
                                    v => {
                                        return Err(format!(
                                            "Second argument to `parse` must be a name string (found {})",
                                            v
                                        ))
                                    }
                                };
                                match parse_input(input, name) {
                                    Ok(ast) => ast.to_value(),
                                    Err(e) => ast_obj! { "Error";
                                        "msg" => Value::String(e.to_string())
                                    },
                                }
                            }
                            "range" => {
                                let (start, stop) = match eval_args.len() {
                                    1 => (
                                        0,
                                        match &eval_args[0] {
                                            Value::Int(i) => *i,
                                            v => {
                                                return Err(format!(
                                                "Argument to `range` must be integers (found {})",
                                                v
                                            ))
                                            }
                                        },
                                    ),
                                    2 => match (&eval_args[0], &eval_args[1]) {
                                        (Value::Int(start), Value::Int(stop)) => (*start, *stop),
                                        (v1, v2) => {
                                            return Err(format!(
                                            "Arguments to `range` must be integers (found {}, {})",
                                            v1, v2
                                        ))
                                        }
                                    },
                                    _ => {
                                        return Err(format!(
                                        "Expected 1 or 2 arguments for function `range`, but got {}",
                                        eval_args.len()
                                    ));
                                    }
                                };
                                if start > stop {
                                    return Err(format!(
                                        "Negative range from {} to {}",
                                        start, stop
                                    ));
                                }
                                let mut elems = Vec::with_capacity((stop - start) as usize);
                                for i in start..stop {
                                    elems.push(Value::Int(i));
                                }
                                Value::List { elems }
                            }
                            _ => return Err(s),
                        }
                    }
                }
            }
            "ASTFile" => {
                let elems = match obj.get_field_mut("elements")? {
                    Value::List { elems } => elems,
                    _ => unreachable!(),
                };
                for elem in elems {
                    if let Value::Object(o) = elem {
                        if o.class == "ASTClassDef" {
                            self.eval_class_def(&o)?;
                            continue;
                        }
                    }
                    self.eval_ast(elem)?;
                }
                Value::Unit
            }
            class => return Err(format!("{} is not an AST class", class)),
        };

        Ok(result)
    }

    fn extract_fn_info(&mut self, obj: &mut Object) -> Result<(String, Vec<Value>), String> {
        let name = match obj.get_field("name")? {
            Value::String(s) => s,
            _ => unreachable!(),
        };
        let args = match obj.get_field_mut("args")? {
            Value::List { elems } => elems,
            _ => unreachable!(),
        };
        let mut eval_args = Vec::with_capacity(args.len());
        for arg in args {
            let mut val = self.eval_ast(arg)?;
            let val = self.deref_val(&mut val)?;
            eval_args.push(val.clone());
        }
        Ok((name, eval_args))
    }

    fn eval_class_def(&mut self, obj: &Object) -> EvalResult {
        assert_eq!(obj.class, "ASTClassDef");
        let name = match obj.get_field("name")? {
            Value::String(s) => s,
            _ => unreachable!(),
        };
        let fields = match obj.get_field("fields")? {
            Value::List { elems } => elems,
            _ => unreachable!(),
        };
        let methods = match obj.get_field("methods")? {
            Value::List { elems } => elems,
            _ => unreachable!(),
        };
        let mut class_fields = HashMap::with_capacity(fields.len());
        let mut class_methods = HashMap::with_capacity(methods.len());
        for field in fields {
            let field_name = match field {
                Value::String(s) => s,
                _ => unreachable!(),
            };
            class_fields.insert(field_name, Value::None);
        }
        for method in methods {
            let method = match method {
                Value::Object(o) => o,
                _ => unreachable!(),
            };
            assert_eq!(method.class, "ASTFnDef");
            let method_name = match method.get_field("name")? {
                Value::String(s) => s,
                _ => unreachable!(),
            };
            class_methods.insert(method_name, self.eval_fn_def(&method)?);
        }
        self.add_class(
            name.clone(),
            Class {
                name,
                fields: class_fields,
                methods: class_methods,
            },
        );
        Ok(Value::Unit)
    }

    fn eval_fn_def(&mut self, obj: &Object) -> Result<Function, String> {
        assert_eq!(obj.class, "ASTFnDef");
        let name = match obj.get_field("name")? {
            Value::String(s) => s,
            _ => unreachable!(),
        };
        let params = match obj.get_field("params")? {
            Value::List { elems } => elems
                .into_iter()
                .map(|v| match v {
                    Value::String(s) => s,
                    _ => unreachable!(),
                })
                .collect(),
            _ => unreachable!(),
        };
        let body = match obj.get_field("body")? {
            Value::List { elems } => elems,
            _ => unreachable!(),
        };
        for stmt in &body {
            assert!(matches!(stmt, Value::Object(_)));
        }
        Ok(Function { name, params, body })
    }

    fn eval_bin_op(&mut self, obj: &mut Object) -> EvalResult {
        let op = match obj.get_field("op")? {
            Value::String(op) => op,
            _ => unreachable!(),
        };

        let mut lhs = &mut self.eval_ast(obj.get_field_mut("lhs")?)?;
        let mut rhs = obj.get_field_mut("rhs")?;
        let mut maybe_eval_rhs;
        rhs = match (op.as_str(), &rhs) {
            // Don't try to resolve method calls without their target
            (".", Value::Object(o)) if o.class == "ASTFnCall" => rhs,
            _ => {
                maybe_eval_rhs = self.eval_ast(obj.get_field_mut("rhs")?)?;
                &mut maybe_eval_rhs
            }
        };

        // Depending on the type of op, we need to resolve variables to their values
        match op.as_str() {
            "+" | "-" | "*" | "/" | "==" | "!=" | "<" | "<=" | ">" | ">=" | "||" | "&&" => {
                // deref and clone since arith. ops only apply to primitives
                lhs = self.deref_val(lhs)?;
                rhs = self.deref_val(rhs)?;
            }
            "=" => {
                // only resolve rhs, since lhs will be updated.
                rhs = self.deref_val(rhs)?;
            }
            _ => {}
        }

        let result = match op.as_str() {
            op @ "+" | op @ "-" | op @ "*" | op @ "/" => match (&lhs, &rhs) {
                (Value::Int(i1), Value::Int(i2)) => Value::Int(match op {
                    "+" => i1 + i2,
                    "-" => i1 - i2,
                    "*" => i1 * i2,
                    "/" => i1 / i2,
                    _ => unreachable!(),
                }),
                (Value::Float(f1), Value::Float(f2)) => Value::Float(match op {
                    "+" => f1 + f2,
                    "-" => f1 - f2,
                    "*" => f1 * f2,
                    "/" => f1 / f2,
                    _ => unreachable!(),
                }),
                (Value::String(s1), Value::String(s2)) => Value::String(match op {
                    "+" => s1.clone() + s2,
                    op => return Err(format!("Cannot use `{}` on strings", op)),
                }),
                _ => return Err(format!("Type error using `{}` on {:?}", op, (lhs, rhs))),
            },
            op @ "==" | op @ "!=" | op @ "<" | op @ "<=" | op @ ">" | op @ ">=" => {
                match (&lhs, &rhs) {
                    (Value::Int(i1), Value::Int(i2)) => Value::Bool(match op {
                        "==" => i1 == i2,
                        "!=" => i1 != i2,
                        "<" => i1 < i2,
                        "<=" => i1 <= i2,
                        ">" => i1 > i2,
                        ">=" => i1 >= i2,
                        _ => unreachable!(),
                    }),
                    (Value::Float(f1), Value::Float(f2)) => Value::Bool(match op {
                        "==" => f1 == f2,
                        "!=" => f1 != f2,
                        "<" => f1 < f2,
                        "<=" => f1 <= f2,
                        ">" => f1 > f2,
                        ">=" => f1 >= f2,
                        _ => unreachable!(),
                    }),
                    (Value::String(s1), Value::String(s2)) => Value::Bool(match op {
                        "==" => s1 == s2,
                        "!=" => s1 != s2,
                        op => return Err(format!("Cannot use `{}` on strings", op)),
                    }),
                    _ => return Err(format!("Type error using `{}` on {:?}", op, (lhs, rhs))),
                }
            }
            op @ "||" | op @ "&&" => match (&lhs, &rhs) {
                (Value::Bool(b1), Value::Bool(b2)) => Value::Bool(match op {
                    "||" => *b1 || *b2,
                    "&&" => *b1 && *b2,
                    _ => unreachable!(),
                }),
                _ => return Err(format!("Type error using `{}` on {:?}", op, (lhs, rhs))),
            },
            "=" => {
                let dest = match &lhs {
                    Value::VarRef { name } => self.get_var(name)?,
                    Value::Ptr(ptr) => *ptr,
                    v => return Err(format!("Cannot assign to {:?}", v)),
                };

                unsafe { *dest = rhs.clone() };
                Value::Unit
            }
            "." => {
                let target = match self.deref_val(lhs)? {
                    Value::Object(o) => o,
                    r @ Value::ObjectRef(_) => match self.eval_ast(r)? {
                        Value::Ptr(ptr) => match self.deref_val(unsafe { &mut *ptr })? {
                            Value::Object(o) => o,
                            Value::ObjectRef(o) => match o.as_mut() {
                                Value::Object(o) => o,
                                _ => unreachable!(),
                            },
                            _ => unreachable!(),
                        },
                        _ => unreachable!(),
                    },
                    v => {
                        match rhs {
                            Value::VarRef { name } => {
                                if name == "class" {
                                    let class = match v {
                                        Value::None => "None",
                                        Value::Unit => "Unit",
                                        Value::Bool(_) => "Bool",
                                        Value::Int(_) => "Int",
                                        Value::Float(_) => "Float",
                                        Value::String(_) => "String",
                                        Value::VarRef { .. } => "VarRef",
                                        Value::Ptr(_) => "Ptr",
                                        Value::Comment { .. } => "Comment",
                                        Value::List { .. } => "List",
                                        Value::Object(_) | Value::ObjectRef(_) => unreachable!(),
                                    };
                                    return Ok(Value::String(class.to_string()));
                                } else {
                                    return Err(format!("Cannot access a field of type {:?}", v));
                                }
                            }
                            Value::Object(o) if o.class == "ASTFnCall" => {
                                // check for intrinsic function
                                let (name, eval_args) = self.extract_fn_info(o)?;
                                match v {
                                    Value::String(s) => match name.as_str() {
                                        "len" => {
                                            if eval_args.len() != 0 {
                                                return Err(format!(
                                                    "Expected 0 arguments for method `string::len`, but got {}",
                                                    eval_args.len()
                                                ));
                                            }
                                            return Ok(Value::Int(s.len() as isize));
                                        }
                                        "contains" => {
                                            if eval_args.len() != 1 {
                                                return Err(format!(
                                                    "Expected 1 argument for method `string::contains`, but got {}",
                                                    eval_args.len()
                                                ));
                                            }
                                            let pattern = match &eval_args[0] {
                                                Value::String(s) => s,
                                                v => {
                                                    return Err(format!(
                                                        "Argument to `string::contains` must be a string (found {})",
                                                        v
                                                    ))
                                                }
                                            };
                                            return Ok(Value::Bool(s.contains(pattern)));
                                        }
                                        "starts_with" => {
                                            if eval_args.len() != 1 {
                                                return Err(format!(
                                                    "Expected 1 argument for method `string::starts_with`, but got {}",
                                                    eval_args.len()
                                                ));
                                            }
                                            let pattern = match &eval_args[0] {
                                                Value::String(s) => s,
                                                v => {
                                                    return Err(format!(
                                                        "Argument to `string::starts_with` must be a string (found {})",
                                                        v
                                                    ))
                                                }
                                            };
                                            return Ok(Value::Bool(s.starts_with(pattern)));
                                        }
                                        "split" => {
                                            if eval_args.len() == 0 {
                                                return Ok(Value::List {
                                                    elems: s
                                                        .split_whitespace()
                                                        .map(str::to_string)
                                                        .map(Value::String)
                                                        .collect(),
                                                });
                                            } else if eval_args.len() == 1 {
                                                let pattern = match &eval_args[0] {
                                                    Value::String(s) => s,
                                                    v => {
                                                        return Err(format!(
                                                            "Argument to `string::split` must be a string (found {})",
                                                            v
                                                        ))
                                                    }
                                                };
                                                return Ok(Value::List {
                                                    elems: s
                                                        .split(pattern)
                                                        .map(str::to_string)
                                                        .map(Value::String)
                                                        .collect(),
                                                });
                                            } else {
                                                return Err(format!(
                                                    "Expected 0 or 1 arguments for method `string::split`, but got {}",
                                                    eval_args.len()
                                                ));
                                            }
                                        }
                                        _ => {
                                            return Err(format!(
                                                "Unknown method on string: {}",
                                                name
                                            ))
                                        }
                                    },
                                    Value::List { elems } => match name.as_str() {
                                        "len" => {
                                            if eval_args.len() != 0 {
                                                return Err(format!(
                                                    "Expected 0 arguments for method `list::len`, but got {}",
                                                    eval_args.len()
                                                ));
                                            }
                                            return Ok(Value::Int(elems.len() as isize));
                                        }
                                        "contains" => {
                                            if eval_args.len() != 1 {
                                                return Err(format!(
                                                    "Expected 1 argument for method `list::contains`, but got {}",
                                                    eval_args.len()
                                                ));
                                            }
                                            return Ok(Value::Bool(elems.contains(&eval_args[0])));
                                        }
                                        "add" => {
                                            if eval_args.len() != 1 {
                                                return Err(format!(
                                                    "Expected 1 argument for method `list::add`, but got {}",
                                                    eval_args.len()
                                                ));
                                            }
                                            elems.push(eval_args[0].clone());
                                            return Ok(Value::Unit);
                                        }
                                        _ => {
                                            return Err(format!(
                                                "Unknown method on string: {}",
                                                name
                                            ))
                                        }
                                    },
                                    _ => todo!("intrinsic methods"),
                                }
                            }
                            _ => {
                                return Err(format!("Cannot access a field of type {:?}", v));
                            }
                        }
                    }
                };

                match rhs {
                    Value::VarRef { name } if name == "class" => {
                        Value::String(target.class.clone())
                    }
                    Value::VarRef { name } => Value::Ptr(target.get_field_mut(&name)?),
                    Value::Object(o) if o.class == "ASTFnCall" => {
                        todo!("function calls");
                    }
                    v => return Err(format!("Cannot access field {:?}", v)),
                }
            }
            _ => unreachable!(),
        };

        Ok(result)
    }

    fn deref_val<'v>(&mut self, mut val: &'v mut Value) -> Result<&'v mut Value, String> {
        if let Value::VarRef { name } = val {
            val = unsafe { &mut *self.get_var(&name)? };
        }
        if let Value::Ptr(mut ptr) = val {
            while let Value::Ptr(p) = unsafe { &mut *ptr } {
                ptr = *p;
            }
            val = unsafe { &mut *ptr };
        }
        Ok(val)
    }
}

pub fn remove_refs(ast: &mut Value) {
    match ast {
        Value::ObjectRef(r) => {
            *ast = r.as_mut().clone();
            remove_refs(ast);
        }
        Value::Object(o) => {
            for field in &mut o.fields.values_mut() {
                remove_refs(field);
            }
        }
        Value::List { elems } => {
            for elem in elems {
                remove_refs(elem);
            }
        }
        _ => {}
    }
}

#[test]
fn int_math() -> Result<(), String> {
    let three = ast_obj! { "ASTIntLiteral";
        "value" => Value::Int(3)
    };
    let x = ast_obj! { "ASTLetStmt";
        "var_name" => Value::String("x".into()),
        "value" => three
    };
    let neg_two = ast_obj! { "ASTIntLiteral";
        "value" => Value::Int(-2)
    };
    let x_ident = ast_obj! { "ASTIdent";
        "name" => Value::String("x".into())
    };
    let add = ast_obj! { "ASTBinaryExpr";
        "op" => Value::String("+".into()),
        "lhs" => x_ident.clone(),
        "rhs" => neg_two.clone()
    };
    let y = ast_obj! { "ASTLetStmt";
        "var_name" => Value::String("y".into()),
        "value" => add
    };
    let ass = ast_obj! { "ASTBinaryExpr";
        "op" => Value::String("=".into()),
        "lhs" => x_ident,
        "rhs" => neg_two
    };
    let mut ast = ast_obj! { "ASTFile";
        "elements" => Value::List { elems: vec![x, y, ass] }
    };

    let mut eval = Evaluator::new();
    eval.eval_ast(&mut ast)?;

    assert_eq!(unsafe { &*eval.get_var("y")? }, &Value::Int(1));
    assert_eq!(unsafe { &*eval.get_var("x")? }, &Value::Int(-2));

    Ok(())
}

#[test]
fn field_access() -> Result<(), String> {
    let three = ast_obj! { "ASTIntLiteral";
        "value" => Value::Int(3)
    };
    let neg_two = ast_obj! { "ASTIntLiteral";
        "value" => Value::Int(-2)
    };
    let x = ast_obj! { "ASTLetStmt";
        "var_name" => Value::String("x".into()),
        "value" => ast_obj!{ "ASTObjectLiteral";
            "class" => Value::String("Foo".into()),
            "fields" => Value::List {
                elems: vec![
                    ast_obj!{ "ASTFieldLiteral";
                        "name" => Value::String("three".into()),
                        "value" => three
                    }
                ]
            }
        }
    };
    let x_ident = ast_obj! { "ASTIdent";
        "name" => Value::String("x".into())
    };
    let y = ast_obj! { "ASTLetStmt";
        "var_name" => Value::String("y".into()),
        "value" => ast_obj!{ "ASTObjectLiteral";
            "class" => Value::String("Bar".into()),
            "fields" => Value::List {
                elems: vec![
                    ast_obj!{ "ASTFieldLiteral";
                        "name" => Value::String("x".into()),
                        "value" => x_ident.clone()
                    },
                    ast_obj!{ "ASTFieldLiteral";
                        "name" => Value::String("neg".into()),
                        "value" => neg_two
                    }
                ]
            }
        }
    };
    let y_ident = ast_obj! { "ASTIdent";
        "name" => Value::String("y".into())
    };
    let three_ident = ast_obj! { "ASTIdent";
        "name" => Value::String("three".into())
    };
    let neg_ident = ast_obj! { "ASTIdent";
        "name" => Value::String("neg".into())
    };
    let y_to_x = ast_obj! { "ASTBinaryExpr";
        "op" => Value::String(".".into()),
        "lhs" => y_ident.clone(),
        "rhs" => x_ident.clone()
    };
    let x_to_three = ast_obj! { "ASTBinaryExpr";
        "op" => Value::String(".".into()),
        "lhs" => y_to_x.clone(),
        "rhs" => three_ident.clone()
    };
    let result_x = ast_obj! { "ASTLetStmt";
        "var_name" => Value::String("result_x".into()),
        "value" => x_to_three
    };
    let y_to_neg = ast_obj! { "ASTBinaryExpr";
        "op" => Value::String(".".into()),
        "lhs" => y_ident.clone(),
        "rhs" => neg_ident.clone()
    };
    let result_neg = ast_obj! { "ASTLetStmt";
        "var_name" => Value::String("result_neg".into()),
        "value" => y_to_neg
    };
    let mut ast = ast_obj! { "ASTFile";
        "elements" => Value::List { elems: vec![x, y, result_x, result_neg] }
    };

    let mut eval = Evaluator::new();
    eval.eval_ast(&mut ast)?;

    assert_eq!(unsafe { &*eval.get_var("result_x")? }, &Value::Int(3));
    assert_eq!(unsafe { &*eval.get_var("result_neg")? }, &Value::Int(-2));

    Ok(())
}

#[test]
fn print_in_fn() -> Result<(), String> {
    let three = ast_obj! { "ASTIntLiteral";
        "value" => Value::Int(3)
    };
    let neg_two = ast_obj! { "ASTIntLiteral";
        "value" => Value::Int(-2)
    };
    let x = ast_obj! { "ASTLetStmt";
        "var_name" => Value::String("x".into()),
        "value" => three.clone()
    };
    let x_ident = ast_obj! { "ASTIdent";
        "name" => Value::String("x".into())
    };
    let y = ast_obj! { "ASTLetStmt";
        "var_name" => Value::String("y".into()),
        "value" => neg_two.clone()
    };
    let y_ident = ast_obj! { "ASTIdent";
        "name" => Value::String("y".into())
    };
    let x_str = ast_obj! { "ASTStringLiteral";
        "text" => Value::String("x = ".into())
    };
    let print_x = ast_obj! { "ASTFnCall";
        "name" => Value::String("print".into()),
        "args" => Value::List {
            elems: vec![
                x_str,
                x_ident.clone()
            ]
        }
    };
    let y_str = ast_obj! { "ASTStringLiteral";
        "text" => Value::String("y = ".into())
    };
    let print_y = ast_obj! { "ASTFnCall";
        "name" => Value::String("print".into()),
        "args" => Value::List {
            elems: vec![
                y_str,
                y_ident.clone()
            ]
        }
    };
    let ret = ast_obj! { "ASTRetStmt";
        "value" => x_ident.clone()
    };
    let def = ast_obj! { "ASTFnDef";
        "name" => Value::String("do".into()),
        "params" => Value::List { elems: vec![
            Value::String("x".into()),
            Value::String("y".into()),
        ]},
        "body" => Value::List {
            elems: vec![
                print_x,
                print_y,
                ret
            ]
        }
    };
    let call = ast_obj! { "ASTFnCall";
        "name" => Value::String("do".into()),
        "args" => Value::List {
            elems: vec![
                y_ident,
                x_ident
            ]
        }
    };
    let result = ast_obj! { "ASTLetStmt";
        "var_name" => Value::String("result".into()),
        "value" => call
    };
    let mut ast = ast_obj! { "ASTFile";
        "elements" => Value::List { elems: vec![x, y, def, result] }
    };

    let mut eval = Evaluator::new();
    eval.eval_ast(&mut ast)?;

    assert_eq!(unsafe { &*eval.get_var("result")? }, &Value::Int(-2));

    Ok(())
}

#[test]
fn if_else() -> Result<(), String> {
    let zero = ast_obj! { "ASTIntLiteral";
        "value" => Value::Int(0)
    };
    let one = ast_obj! { "ASTIntLiteral";
        "value" => Value::Int(1)
    };
    let three = ast_obj! { "ASTIntLiteral";
        "value" => Value::Int(3)
    };
    let neg_two = ast_obj! { "ASTIntLiteral";
        "value" => Value::Int(-2)
    };

    let x = ast_obj! { "ASTLetStmt";
        "var_name" => Value::String("x".into()),
        "value" => zero.clone()
    };
    let x_ident = ast_obj! { "ASTIdent";
        "name" => Value::String("x".into())
    };

    let x_to_three = ast_obj! { "ASTBinaryExpr";
        "op" => Value::String("=".into()),
        "lhs" => x_ident.clone(),
        "rhs" => three.clone()
    };
    let x_to_neg_two = ast_obj! { "ASTBinaryExpr";
        "op" => Value::String("=".into()),
        "lhs" => x_ident.clone(),
        "rhs" => neg_two.clone()
    };
    let x_to_one = ast_obj! { "ASTBinaryExpr";
        "op" => Value::String("=".into()),
        "lhs" => x_ident.clone(),
        "rhs" => one.clone()
    };

    let cond1 = ast_obj! { "ASTBinaryExpr";
        "op" => Value::String("<".into()),
        "lhs" => three.clone(),
        "rhs" => neg_two.clone()
    };
    let cond2 = ast_obj! { "ASTBinaryExpr";
        "op" => Value::String("<".into()),
        "lhs" => neg_two.clone(),
        "rhs" => three.clone()
    };
    let cond3 = ast_obj! { "ASTBinaryExpr";
        "op" => Value::String("==".into()),
        "lhs" => one.clone(),
        "rhs" => one.clone()
    };

    let if_stmt = ast_obj! { "ASTIfStmt";
        "condition" => cond1,
        "body" => Value::List {
            elems: vec![
                x_to_three
            ]
        },
        "elses" => Value::List {
            elems: vec![
                ast_obj!{ "ASTIfStmt";
                    "condition" => cond2,
                    "body" => Value::List {
                        elems: vec![
                            x_to_neg_two
                        ]
                    },
                    "elses" => Value::List {elems: vec![]}
                },
                ast_obj!{ "ASTIfStmt";
                    "condition" => cond3,
                    "body" => Value::List {
                        elems: vec![
                            x_to_one
                        ]
                    },
                    "elses" => Value::List {elems: vec![]}
                },
            ]
        }
    };
    let mut ast = ast_obj! { "ASTFile";
        "elements" => Value::List { elems: vec![x, if_stmt] }
    };

    let mut eval = Evaluator::new();
    eval.eval_ast(&mut ast)?;

    assert_eq!(unsafe { &*eval.get_var("x")? }, &Value::Int(-2));

    Ok(())
}

#[test]
fn for_loop() -> Result<(), String> {
    let zero = ast_obj! { "ASTIntLiteral";
        "value" => Value::Int(0)
    };
    let one = ast_obj! { "ASTIntLiteral";
        "value" => Value::Int(1)
    };
    let range = ast_obj! { "ASTFnCall";
        "name" => Value::String("range".into()),
        "args" => Value::List {
            elems: vec![
                ast_obj!{ "ASTIntLiteral";
                    "value" => Value::Int(-10)
                },
                ast_obj!{ "ASTIntLiteral";
                    "value" => Value::Int(-5)
                }
            ]
        }
    };

    let x = ast_obj! { "ASTLetStmt";
        "var_name" => Value::String("x".into()),
        "value" => zero.clone()
    };
    let y = ast_obj! { "ASTLetStmt";
        "var_name" => Value::String("y".into()),
        "value" => zero.clone()
    };
    let x_ident = ast_obj! { "ASTIdent";
        "name" => Value::String("x".into())
    };
    let y_ident = ast_obj! { "ASTIdent";
        "name" => Value::String("y".into())
    };
    let it_ident = ast_obj! { "ASTIdent";
        "name" => Value::String("it".into())
    };

    let x_plus_one = ast_obj! { "ASTBinaryExpr";
        "op" => Value::String("+".into()),
        "lhs" => x_ident.clone(),
        "rhs" => one.clone()
    };
    let x_ass = ast_obj! { "ASTBinaryExpr";
        "op" => Value::String("=".into()),
        "lhs" => x_ident.clone(),
        "rhs" => x_plus_one
    };

    let y_minus_it = ast_obj! { "ASTBinaryExpr";
        "op" => Value::String("-".into()),
        "lhs" => y_ident.clone(),
        "rhs" => it_ident.clone()
    };
    let y_ass = ast_obj! { "ASTBinaryExpr";
        "op" => Value::String("=".into()),
        "lhs" => y_ident.clone(),
        "rhs" => y_minus_it
    };

    let for_loop = ast_obj! { "ASTForLoop";
        "var_name" => Value::String("it".into()),
        "target" => range,
        "body" => Value::List {
            elems: vec![
                x_ass,
                y_ass
            ]
        }
    };

    let mut ast = ast_obj! { "ASTFile";
        "elements" => Value::List { elems: vec![x, y, for_loop] }
    };

    let mut eval = Evaluator::new();
    eval.eval_ast(&mut ast)?;

    assert_eq!(unsafe { &*eval.get_var("x")? }, &Value::Int(5));
    let it_sum: isize = (-10..-5).into_iter().sum();
    assert_eq!(unsafe { &*eval.get_var("y")? }, &Value::Int(0 - it_sum));

    Ok(())
}

#[test]
fn return_from_if_in_loop() -> Result<(), String> {
    let two = ast_obj! { "ASTIntLiteral";
        "value" => Value::Int(2)
    };
    let range = ast_obj! { "ASTFnCall";
        "name" => Value::String("range".into()),
        "args" => Value::List {
            elems: vec![
                ast_obj!{ "ASTIntLiteral";
                    "value" => Value::Int(5)
                }
            ]
        }
    };

    let it_ident = ast_obj! { "ASTIdent";
        "name" => Value::String("it".into())
    };
    let skip_ident = ast_obj! { "ASTIdent";
        "name" => Value::String("skip".into())
    };

    // skip && it >= 2
    let cond = ast_obj! { "ASTBinaryExpr";
        "op" => Value::String("&&".into()),
        "lhs" => skip_ident.clone(),
        "rhs" => { ast_obj! { "ASTBinaryExpr";
            "op" => Value::String(">=".into()),
            "lhs" => it_ident.clone(),
            "rhs" => two.clone()
        }}
    };
    let ret = ast_obj! { "ASTRetStmt";
        "value" => it_ident.clone()
    };
    let if_stmt = ast_obj! { "ASTIfStmt";
        "condition" => cond,
        "body" => Value::List {
            elems: vec![ret]
        },
        "elses" => Value::List {elems: vec![]}
    };

    let for_loop = ast_obj! { "ASTForLoop";
        "var_name" => Value::String("it".into()),
        "target" => range,
        "body" => Value::List {
            elems: vec![
                if_stmt,
            ]
        }
    };

    let fn_def = ast_obj! { "ASTFnDef";
        "name" => Value::String("test".into()),
        "params" => Value::List{
            elems: vec![
                Value::String("skip".into())
            ]
        },
        "body" => Value::List {
            elems: vec![
                for_loop
            ]
        }
    };

    let t = ast_obj! { "ASTBoolLiteral";
        "value" => Value::Bool(true)
    };
    let f = ast_obj! { "ASTBoolLiteral";
        "value" => Value::Bool(false)
    };

    let call_with_skip = ast_obj! { "ASTFnCall";
        "name" => Value::String("test".into()),
        "args" => Value::List{
            elems: vec![t]
        }
    };
    let call_without_skip = ast_obj! { "ASTFnCall";
        "name" => Value::String("test".into()),
        "args" => Value::List{
            elems: vec![f]
        }
    };

    let result_with_skip = ast_obj! { "ASTLetStmt";
        "var_name" => Value::String("result_with_skip".into()),
        "value" => call_with_skip
    };
    let result_without_skip = ast_obj! { "ASTLetStmt";
        "var_name" => Value::String("result_without_skip".into()),
        "value" => call_without_skip
    };

    let mut ast = ast_obj! { "ASTFile";
        "elements" => Value::List { elems: vec![fn_def, result_with_skip, result_without_skip] }
    };

    let mut eval = Evaluator::new();
    eval.eval_ast(&mut ast)?;

    assert_eq!(
        unsafe { &*eval.get_var("result_with_skip")? },
        &Value::Int(2)
    );
    assert_eq!(
        unsafe { &*eval.get_var("result_without_skip")? },
        &Value::Unit
    );

    Ok(())
}

#[test]
fn meta() -> Result<(), String> {
    let one = ast_obj! { "ASTIntLiteral";
        "value" => Value::Int(1)
    };
    let two = ast_obj! { "ASTIntLiteral";
        "value" => Value::Int(2)
    };
    let x = ast_obj! { "ASTLetStmt";
        "var_name" => Value::String("x".into()),
        "value" => Value::ObjectRef(Box::new(two.clone()))
    };
    let ast = ast_obj! { "ASTFile";
        "elements" => Value::List { elems: vec![
            Value::ObjectRef(Box::new(x.clone()))
        ] }
    };

    let elem_ident = ast_obj! { "ASTIdent";
        "name" => Value::String("elem".into())
    };
    let class_ident = ast_obj! { "ASTIdent";
        "name" => Value::String("class".into())
    };
    let elements_ident = ast_obj! { "ASTIdent";
        "name" => Value::String("elements".into())
    };
    let value_ident = ast_obj! { "ASTIdent";
        "name" => Value::String("value".into())
    };
    let ast_ident = ast_obj! { "ASTIdent";
        "name" => Value::String("ast".into())
    };

    // elem.class == "ASTLetStmt"
    let class = ast_obj! { "ASTBinaryExpr";
        "op" => Value::String(".".into()),
        "lhs" => elem_ident.clone(),
        "rhs" => class_ident.clone()
    };
    let cond = ast_obj! { "ASTBinaryExpr";
        "op" => Value::String("==".into()),
        "lhs" => class,
        "rhs" => { ast_obj! { "ASTStringLiteral";
            "text" => Value::String("ASTLetStmt".into())
        }}
    };

    // elem.value = ASTBinaryExpr {
    //    op: "+",
    //    lhs: elem.value,
    //    rhs: AstIntLiteral {
    //        value: 1
    //    }
    // };
    let value = ast_obj! { "ASTBinaryExpr";
        "op" => Value::String(".".into()),
        "lhs" => elem_ident.clone(),
        "rhs" => value_ident.clone()
    };
    let modification = ast_obj! { "ASTBinaryExpr";
        "op" => Value::String("=".into()),
        "lhs" => value.clone(),
        "rhs" => { ast_obj!{ "ASTObjectLiteral";
            "class" => Value::String("ASTBinaryExpr".into()),
            "fields" => Value::List {
                elems: vec![
                    ast_obj!{ "ASTFieldLiteral";
                        "name" => Value::String("op".into()),
                        "value" => {ast_obj!{ "ASTStringLiteral";
                            "text" => Value::String("+".into())
                        }}
                    },
                    ast_obj!{ "ASTFieldLiteral";
                        "name" => Value::String("lhs".into()),
                        "value" => value.clone()
                    },
                    ast_obj!{ "ASTFieldLiteral";
                        "name" => Value::String("rhs".into()),
                        "value" => {ast_obj!{ "ASTObjectLiteral";
                            "class" => Value::String("ASTIntLiteral".into()),
                            "fields" => Value::List {
                                elems: vec![
                                    ast_obj!{ "ASTFieldLiteral";
                                        "name" => Value::String("value".into()),
                                        "value" => one.clone()
                                    },
                                ]
                            }
                        }}
                    },
                ]
            }
        }}
    };

    let if_stmt = ast_obj! { "ASTIfStmt";
        "condition" => cond,
        "body" => Value::List {
            elems: vec![modification]
        },
        "elses" => Value::List {elems: vec![]}
    };

    // for elem in ast.elements
    let elements = ast_obj! { "ASTBinaryExpr";
        "op" => Value::String(".".into()),
        "lhs" =>ast_ident.clone(),
        "rhs" => elements_ident.clone()
    };
    let for_loop = ast_obj! { "ASTForLoop";
        "var_name" => Value::String("elem".into()),
        "target" => elements,
        "body" => Value::List {
            elems: vec![
                if_stmt,
            ]
        }
    };

    let ret = ast_obj! { "ASTRetStmt";
        "value" => ast_ident.clone()
    };
    let fn_def = ast_obj! { "ASTFnDef";
        "name" => Value::String("meta".into()),
        "params" => Value::List{
            elems: vec![
                Value::String("ast".into())
            ]
        },
        "body" => Value::List {
            elems: vec![
                for_loop,
                ret
            ]
        }
    };
    let call = ast_obj! { "ASTFnCall";
        "name" => Value::String("meta".into()),
        "args" => Value::List{
            elems: vec![Value::ObjectRef(Box::new(ast.clone()))]
        }
    };
    let result = ast_obj! { "ASTLetStmt";
        "var_name" => Value::String("result".into()),
        "value" => call
    };
    let mut ast = ast_obj! { "ASTFile";
        "elements" => Value::List { elems: vec![fn_def, result] }
    };

    let mut eval = Evaluator::new();
    eval.eval_ast(&mut ast)?;
    let ast = unsafe { &mut *eval.get_var("result")? };
    remove_refs(ast);
    let mut eval = Evaluator::new();
    eval.eval_ast(ast)?;

    assert_eq!(unsafe { &*eval.get_var("x")? }, &Value::Int(2 + 1));

    Ok(())
}
