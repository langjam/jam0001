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
            debug!("Running statement: {:?}", stmt);

            match stmt {
                Ast::TypeDef(ty) => {
                    let type_name = ty.name.as_ref().expect("struct must have name");
                    let resolved_type = self.resolve_type(ty).expect("invalid type");

                    debug!("Type: {:?}", resolved_type);

                    self.type_registry.insert(type_name.clone(), resolved_type);

                    last_value = None;
                }
                Ast::ValueDef(name, val) => {
                    let resolved_value = self.evaluate(val).expect("invalid value");

                    debug!("Value: {:?}", resolved_value);

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
            let comment = if source_field.comment.lines.len() == 1 && source_field.comment.lines[0].chars().next() == Some('!') {
                let mut comment_source = source_field.comment.lines[0].split_at(1).1.trim_start().to_owned();
                comment_source.push_str("!!");
                let comment_query = crate::parse::parse_value(&comment_source)
                    .map_err(|e| format!("Error in comment reference: {:?}", e))?.1;
                debug!();
                debug!("Evaluating comment source: {:?}", comment_source);
                debug!("Query: {:?}", comment_query);
                match self.evaluate(&comment_query)? {
                    Value::Comment(c) => c,
                    other => return Err(format!("Invalid type from comment reference: {:?}", other)),
                }
            }
            else {
                CommentValue::from_lines(&source_field.comment.lines)
            };

            resolved_fields.push(FieldType {
                name: source_field.name.clone(),
                ty: self.resolve_type_reference(&source_field.ty)?,
                comment,
            });
        }

        let comment = if ty.comment.lines.len() == 1 && ty.comment.lines[0].chars().next() == Some('!') {
            let mut comment_source = ty.comment.lines[0].split_at(1).1.trim_start().to_owned();
            comment_source.push_str("!!");
            let comment_query = crate::parse::parse_value(&comment_source)
                .map_err(|e| format!("Error in comment reference: {:?}", e))?.1;
            debug!();
            debug!("Evaluating comment source: {:?}", comment_source);
            debug!("Query: {:?}", comment_query);
            match self.evaluate(&comment_query)? {
                Value::Comment(c) => c,
                other => return Err(format!("Invalid type from comment reference: {:?}", other)),
            }
        }
        else {
            CommentValue::from_lines(&ty.comment.lines)
        };


        let resolved = ResolvedType::Struct(StructType::from(StructTypeData {
            name: Some(type_name),
            fields: resolved_fields,
            comment,
        }));

        debug!("");
        debug!("");
        debug!("Comment: {:?}", ty.comment);
        debug!("Resolved Comment: {:?}", CommentValue::from_lines(&ty.comment.lines));
        debug!("Source Type: {:?}", ty);
        debug!("Resolved Type: {:?}", resolved);
        debug!("");
        debug!("");

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
            ValueAst::Unit => Ok(Value::Unit),
            ValueAst::Number(n) => Ok(Value::Number(*n)),
            ValueAst::Text(t) => Ok(Value::Text(t.clone())),
            ValueAst::Struct(s) => {
                let name = s.name.clone();

                let (fields, mut comment) = if let Some(n) = &s.name {
                    debug!("resolving named struct: {:?}", n);
                    let resolved_type = self.type_registry.get(n).ok_or_else(|| format!("type not found: {}", n))?;
                    match resolved_type {
                        ResolvedType::Struct(s) => (s.fields.clone(), s.comment.inner().clone()),
                        _ => return Err(format!("type not a struct: {}", n)),
                    }
                }
                else {
                    (vec![], CommentValueData::empty())
                };

                if s.comment.lines.len() == 1 && s.comment.lines[0].chars().next() == Some('!') {
                    let mut comment_source = s.comment.lines[0].split_at(1).1.trim_start().to_owned();
                    comment_source.push_str("!!");
                    let comment_query = crate::parse::parse_value(&comment_source)
                        .map_err(|e| format!("Error in comment reference: {:?}", e))?.1;
                    debug!();
                    debug!("Evaluating comment source: {:?}", comment_source);
                    debug!("Query: {:?}", comment_query);
                    match self.evaluate(&comment_query)? {
                        Value::Comment(c) => comment = c.inner().clone(),
                        other => return Err(format!("Invalid type from comment reference: {:?}", other)),
                    }
                }
                else {
                    debug!("prior comment: {:?}", comment);
                    comment.extend_with_lines(&s.comment.lines);
                    debug!("new comment: {:?}", comment);
                }

                let comment = comment.into();

                let field_comments = fields.into_iter()
                    .map(|f| (f.name.clone(), f.comment.inner().clone()))
                    .collect::<std::collections::HashMap<_, _>>();

                let mut fields = std::collections::HashMap::new();
                for field_value in &s.fields {
                    let comment = if field_value.comment.lines.len() == 1 && field_value.comment.lines[0].chars().next() == Some('!') {
                        let mut comment_source = field_value.comment.lines[0].split_at(1).1.trim_start().to_owned();
                        comment_source.push_str("!!");
                        let comment_query = crate::parse::parse_value(&comment_source)
                            .map_err(|e| format!("Error in comment reference: {:?}", e))?.1;
                        debug!();
                        debug!("Evaluating comment source: {:?}", comment_source);
                        debug!("Query: {:?}", comment_query);
                        match self.evaluate(&comment_query)? {
                            Value::Comment(c) => c,
                            other => return Err(format!("Invalid type from comment reference: {:?}", other)),
                        }
                    }
                    else {
                        let mut field_comment = field_comments
                            .get(&field_value.name)
                            .map(|f| f.clone())
                            .unwrap_or_else(CommentValueData::empty);
                        field_comment.extend_with_lines(&field_value.comment.lines);
                        field_comment.into()
                    };
                    fields.insert(
                        field_value.name.clone(),
                        FieldValue {
                            value: self.evaluate(&field_value.value)?,
                            comment,
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
                    Value::Comment(_) => Err("comment has no comments (why?)".into()),
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
                    Value::Comment(_) => Err("comment has no comments (why?)".into()),
                    Value::Struct(s) => Ok(Value::Comment(s.comment.clone())),
                }
            }
            ValueAst::FieldCommentGet(source, field) => {
                match self.evaluate(source)? {
                    Value::Unit => Err("unit has no fields".into()),
                    Value::Number(_) => Err("number has no fields".into()),
                    Value::Text(_) => Err("text has no fields".into()),
                    Value::Comment(_) => Err("comment has no field comments (why?)".into()),
                    Value::Struct(s) => {
                        match s.fields.get(field) {
                            None => Err(format!("struct has no field named {}", field)),
                            Some(f) => Ok(Value::Comment(f.comment.clone()))
                        }
                    }
                }
            }
            ValueAst::Add(left, right) => {
                let left = self.evaluate(left)?;
                let right = self.evaluate(right)?;

                let left_ty = left.get_ty();
                let right_ty = right.get_ty();

                if !left_ty.is_compatible_with(&right_ty) {
                    return Err("incompatible value types".to_string());
                }

                Ok(left.add(&right))
            }
        }
    }
}
