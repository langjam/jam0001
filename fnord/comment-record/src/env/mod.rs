use crate::ast::{Ast, Script, StructType as StructTypeAst, TypeAst, ValueAst};
use crate::types::{FieldType, ResolvedType, StructType, StructTypeData};
use crate::values::{Value, CommentValue, CommentValueData, StructValue, StructValueData, FieldValue};

#[derive(Debug, Default, Clone)]
pub struct Env {
    type_registry: std::collections::HashMap<String, ResolvedType>,
    value_registry: std::collections::HashMap<String, Value>,
}

impl Env {
    pub fn new() -> Env {
        let mut start = Env::default();
        start.type_registry.insert("Unit".into(), ResolvedType::Unit);
        start.type_registry.insert("Number".into(), ResolvedType::Number);
        start.type_registry.insert("Text".into(), ResolvedType::Text);
        start
    }

    pub fn run_script(&mut self, script: &Script) -> Option<Value> {
        let mut last_value = None;
        for stmt in &script.statements {
            println!("Running statement: {:?}", stmt);

            match stmt {
                Ast::TypeDef(ty) => {
                    let type_name = ty.name.as_ref().expect("struct must have name");
                    let resolved_type = self.resolve_type(ty).expect("invalid type");

                    println!("Type: {:?}", resolved_type);

                    self.type_registry.insert(type_name.clone(), resolved_type);

                    last_value = None;
                }
                Ast::ValueDef(name, val) => {
                    let resolved_value = self.evaluate(val).expect("invalid value");

                    println!("Value: {:?}", resolved_value);

                    self.value_registry.insert(name.clone(), resolved_value.clone());

                    last_value = Some(resolved_value);
                }
            }
        }
        last_value
    }

    pub fn resolve_type(&self, ty: &StructTypeAst) -> Result<ResolvedType, String> {
        let type_name = ty.name.as_ref().ok_or("struct must have name")?.clone();

        let mut resolved_fields = vec![];
        for source_field in &ty.fields {
            resolved_fields.push(FieldType {
                name: source_field.name.clone(),
                ty: self.resolve_type_reference(&source_field.ty)?,
                comment: CommentValue::from_lines(&source_field.comment.lines),
            });
        }

        let resolved = ResolvedType::Struct(StructType::from(StructTypeData {
            name: Some(type_name),
            fields: resolved_fields,
            comment: CommentValue::from_lines(&ty.comment.lines),
        }));

        println!("");
        println!("");
        println!("Comment: {:?}", ty.comment);
        println!("Resolved Comment: {:?}", CommentValue::from_lines(&ty.comment.lines));
        println!("Source Type: {:?}", ty);
        println!("Resolved Type: {:?}", resolved);
        println!("");
        println!("");

        Ok(resolved)
    }

    pub fn resolve_type_reference(&self, ty: &TypeAst) -> Result<ResolvedType, String> {
        match ty {
            TypeAst::Named(n) => Ok(self.type_registry.get(n).ok_or_else(|| format!("type not found: {}", n))?.clone()),
            TypeAst::Struct(s) => self.resolve_type(s),
        }
    }

    pub fn evaluate(&self, val: &ValueAst) -> Result<Value, String> {
        match val {
            ValueAst::Number(n) => Ok(Value::Number(*n)),
            ValueAst::Text(t) => Ok(Value::Text(t.clone())),
            ValueAst::Struct(s) => {
                let name = s.name.clone();

                let mut comment = if let Some(n) = &s.name {
                    println!("resolving named struct: {:?}", n);
                    let resolved_type = self.type_registry.get(n).ok_or_else(|| format!("type not found: {}", n))?;
                    match resolved_type {
                        ResolvedType::Struct(s) => s.comment.inner().clone(),
                        _ => return Err(format!("type not a struct: {}", n)),
                    }
                }
                else {
                    CommentValueData::empty()
                };
                println!("prior comment: {:?}", comment);
                comment.extend_with_lines(&s.comment.lines);
                let comment = comment.into();
                println!("new comment: {:?}", comment);

                let mut fields = std::collections::HashMap::new();
                for field_value in &s.fields {
                    fields.insert(
                        field_value.name.clone(),
                        FieldValue {
                            value: self.evaluate(&field_value.value)?,
                            comment: CommentValue::from_lines(&field_value.comment.lines),
                        },
                    );
                }

                Ok(Value::Struct(StructValue::from(StructValueData {
                    name,
                    fields,
                    comment,
                })))
            },
            ValueAst::Reference(r) => Ok(self.value_registry.get(r).ok_or_else(|| format!("value not found: {}", r))?.clone()),
            ValueAst::CommentAccess(source, field) => {
                match self.evaluate(source)? {
                    Value::Unit => Err("unit has no comments (why?)".into()),
                    Value::Number(_) => Err("number has no comments (why?)".into()),
                    Value::Text(_) => Err("text has no comments (why?)".into()),
                    Value::Comment(c) => Err("comment has no comments (why?)".into()),
                    Value::Struct(s) => s.comment.get_field(field),
                }
            }
            ValueAst::FieldAccess(source, field) => {
                match self.evaluate(source)? {
                    Value::Unit => Err("unit has no fields".into()),
                    Value::Number(_) => Err("number has no fields".into()),
                    Value::Text(_) => Err("text has no fields".into()),
                    Value::Comment(c) => c.get_field(field),
                    Value::Struct(s) => {
                        match s.fields.get(field) {
                            None => Err(format!("struct has no field named {}", field)),
                            Some(f) => Ok(f.value.clone())
                        }
                    }
                }
            }
            ValueAst::CommentGet(source) => {
                match self.evaluate(source)? {
                    Value::Unit => Err("unit has no comments (why?)".into()),
                    Value::Number(_) => Err("number has no comments (why?)".into()),
                    Value::Text(_) => Err("text has no comments (why?)".into()),
                    Value::Comment(c) => Err("comment has no comments (why?)".into()),
                    Value::Struct(s) => Ok(Value::Comment(s.comment.clone())),
                }
            }
        }
    }
}
