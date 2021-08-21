#[derive(Debug, Clone)]
pub enum Value {
    Unit,
    Number(i64),
    Text(String),
    Comment(CommentValue),
    Struct(StructValue),
}

#[derive(Debug, Clone)]
pub struct CommentValue(std::rc::Rc<CommentValueData>);

impl CommentValue {
    pub fn from_lines(lines: &[String]) -> Self {
        Self::from(CommentValueData::from_lines(lines))
    }

    pub fn empty() -> Self {
        Self::from(CommentValueData::empty())
    }

    pub fn inner(&self) -> &CommentValueData {
        &self.0
    }
}

impl std::ops::Deref for CommentValue {
    type Target = CommentValueData;

    fn deref(&self) -> &CommentValueData {
        &self.0
    }
}

impl From<CommentValueData> for CommentValue {
    fn from(data: CommentValueData) -> Self {
        CommentValue(std::rc::Rc::new(data))
    }
}

#[derive(Debug, Clone)]
pub struct StructValue(std::rc::Rc<StructValueData>);

impl std::ops::Deref for StructValue {
    type Target = StructValueData;

    fn deref(&self) -> &StructValueData {
        &self.0
    }
}

impl From<StructValueData> for StructValue {
    fn from(data: StructValueData) -> Self {
        StructValue(std::rc::Rc::new(data))
    }
}

#[derive(Debug, Clone)]
pub struct CommentValueData {
    pub text: String,
    pub record: Value,
}

impl CommentValueData {
    pub fn from_lines(lines: &[String]) -> Self {
        let mut item = CommentValueData::empty();
        item.extend_with_lines(lines);
        item
    }

    pub fn empty() -> Self {
        CommentValueData { 
            text: String::new(),
            record: Value::Unit,
        }
    }

    pub fn get_field(&self, field: &str) -> Result<Value, String> {
        if field == "text" {
            return Ok(Value::Text(self.text.clone()));
        }

        match &self.record {
            Value::Unit => Err(format!("comment has no field named {}", field)),
            Value::Struct(s) => Ok(s.fields.get(field).ok_or_else(|| format!("comment has no field named {}", field))?.value.clone()),
            _ => panic!("this should not allow non-structs really"),
        }
    }

    pub fn extend_with_lines(&mut self, lines: &[String]) {
        println!();
        println!("extending with lines");
        println!("self: {:?}", self);
        println!("lines: {:?}", lines);

        let mut text = self.text.clone();
        let mut fields = match &self.record {
            Value::Struct(s) => s.fields.iter().map(|(k, v)| (k.clone(), v.clone())).collect(),
            _ => vec![],
        };
        for line in lines {
            if line.contains(':') {
                let mut parts = line.splitn(2, ':');
                let name = parts.next().expect("field name").trim().to_string();
                let value = Value::Text(parts.next().expect("field value").trim().to_string());
                let comment = CommentValue::empty();
                fields.push((name, FieldValue { value, comment }));
            }
            else {
                text.push_str(line);
                text.push('\n');
            }
        }

        let record = if fields.is_empty() {
            Value::Unit
        }
        else {
            Value::Struct(StructValue::from(StructValueData {
                name: None,
                fields: fields.into_iter().collect(),
                comment: CommentValue::empty(),
            }))
        };

        self.text = text;
        self.record = record;

        println!("extended: {:?}", self);
        println!();
    }
}

#[derive(Debug, Clone)]
pub struct FieldValue {
    pub value: Value,
    pub comment: CommentValue,
}

#[derive(Debug, Clone)]
pub struct StructValueData {
    pub name: Option<String>,
    pub fields: std::collections::HashMap<String, FieldValue>,
    pub comment: CommentValue,
}
