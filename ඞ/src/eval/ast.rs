use super::*;

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

impl Evaluator {
    pub(super) fn register_ast_classes(&mut self) {
        for class in [
            declare_builtin!("ASTIntLiteral"; "value"),
            declare_builtin!("ASTFloatLiteral"; "value"),
            declare_builtin!("ASTStringLiteral"; "text"),
            declare_builtin!("ASTComment"; "text"),
            declare_builtin!("ASTIdent"; "name"),
            declare_builtin!("ASTBinaryExpr"; "op", "lhs", "rhs"),
            declare_builtin!("ASTList"; "op", "lhs", "rhs"),
            declare_builtin!("ASTObjectLiteral"; "class", "fields"),
            declare_builtin!("ASTFieldLiteral"; "name", "value"),
            declare_builtin!("ASTForLoop"; "op", "lhs", "rhs"),
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

    pub fn eval_ast(&mut self, ast: &Value) -> EvalResult {
        let obj = match ast {
            Value::Object(o) => o,
            v => return Err(format!("Tried to evaluate AST, but found {:?}", v)),
        };
        let result = match obj.class.as_str() {
            "ASTIntLiteral" | "ASTFloatLiteral" => obj.get_field("value")?,
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
            "ASTBinaryExpr" => self.eval_bin_op(obj)?,
            "ASTLetStmt" => {
                let name = match obj.get_field("var_name")? {
                    Value::String(s) => s,
                    _ => unreachable!(),
                };
                let val = obj.get_field("value")?;
                let val = self.eval_ast(&val)?;
                let val = self.deref_val(val)?;
                self.create_var(name, val);
                Value::Unit
            }
            "ASTObjectLiteral" => {
                let class = obj.get_field("class")?;
                let class = match class {
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
                    let mut value = self.eval_ast(&obj.get_field("value")?)?;
                    if let Value::VarRef { name } = value {
                        value = unsafe { &*self.get_var(&name)? }.clone();
                    }
                    obj_fields.insert(name, value);
                }
                Value::Object(Object {
                    class,
                    fields: obj_fields,
                })
            }
            "ASTFile" => {
                let elems = match obj.get_field("elements")? {
                    Value::List { elems } => elems,
                    _ => unreachable!(),
                };
                for elem in &elems {
                    self.eval_ast(elem)?;
                }
                Value::Unit
            }
            class => return Err(format!("{} is not an AST class", class)),
        };

        Ok(result)
    }

    fn eval_bin_op(&mut self, obj: &Object) -> EvalResult {
        let op = match obj.get_field("op")? {
            Value::String(op) => op,
            _ => unreachable!(),
        };

        let mut lhs = self.eval_ast(&obj.get_field("lhs")?)?;
        let mut rhs = self.eval_ast(&obj.get_field("rhs")?)?;

        // Depending on the type of op, we need to resolve variables to their values
        match op.as_str() {
            "+" | "-" | "*" | "/" => {
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
            "=" => {
                let dest = match &lhs {
                    Value::VarRef { name } => self.get_var(name)?,
                    Value::Ptr(ptr) => *ptr,
                    v => return Err(format!("Cannot assign to {:?}", v)),
                };
                unsafe { *dest = rhs };
                Value::Unit
            }
            "." => {
                let target = match &mut lhs {
                    Value::VarRef { name } => self.get_var(name)?,
                    v @ Value::Object(_) => v as *mut _,
                    Value::Ptr(ptr) => *ptr,
                    v => return Err(format!("Cannot access a field of {:?}", v)),
                };
                let target = match unsafe { &mut *target } {
                    Value::Object(o) => o,
                    v => return Err(format!("Cannot access a field of type {:?}", v)),
                };

                match rhs {
                    Value::VarRef { name } => Value::Ptr(target.get_field_mut(&name)?),
                    _ => todo!("function calls"),
                }
            }
            _ => unreachable!(),
        };

        Ok(result)
    }

    fn deref_val(&mut self, val: Value) -> EvalResult {
        Ok(match val {
            Value::VarRef { name } => unsafe { &*self.get_var(&name)? }.clone(),
            Value::Ptr(ptr) => unsafe { &*ptr }.clone(),
            _ => val,
        })
    }
}

#[allow(unused)]
macro_rules! ast_obj {
    ($name:literal $(; $($field:literal => $value:expr),*)?) => {
        Value::Object(Object {
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
    let ast = ast_obj! { "ASTFile";
        "elements" => Value::List { elems: vec![x, y, ass] }
    };

    let mut eval = Evaluator::new();
    eval.eval_ast(&ast)?;

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
    let ast = ast_obj! { "ASTFile";
        "elements" => Value::List { elems: vec![x, y, result_x, result_neg] }
    };

    let mut eval = Evaluator::new();
    eval.eval_ast(&ast)?;

    assert_eq!(unsafe { &*eval.get_var("result_x")? }, &Value::Int(3));
    assert_eq!(unsafe { &*eval.get_var("result_neg")? }, &Value::Int(-2));

    Ok(())
}
