use crate::{ast_obj, eval::Value};

#[derive(Debug, PartialEq, Clone)]
pub enum AST {
    BoolLiteral {
        value: bool,
    },
    IntLiteral {
        value: isize,
    },
    FloatLiteral {
        value: f64,
    },
    StringLiteral {
        text: String,
    },
    Comment {
        text: String,
    },
    Ident {
        name: String,
    },
    ListLiteral {
        elements: Vec<AST>,
    },
    BinaryExpr {
        op: String,
        lhs: Box<AST>,
        rhs: Box<AST>,
    },
    RefExpr {
        value: Box<AST>,
    },
    ObjectLiteral {
        class: String,
        fields: Vec<AST>,
    },
    FieldLiteral {
        name: String,
        value: Box<AST>,
    },
    ForLoop {
        var_name: String,
        target: Box<AST>,
        body: Vec<AST>,
    },
    IfStmt {
        condition: Box<AST>,
        body: Vec<AST>,
        elses: Vec<AST>,
    },
    FnCall {
        name: String,
        args: Vec<AST>,
    },
    LetStmt {
        var_name: String,
        value: Box<AST>,
    },
    RetStmt {
        value: Box<AST>,
    },
    FnDef {
        name: String,
        params: Vec<String>,
        body: Vec<AST>,
    },
    ClassDef {
        name: String,
        fields: Vec<String>,
        methods: Vec<AST>,
    },
    File {
        name: String,
        elements: Vec<AST>,
    },
}

impl AST {
    pub fn to_value(self) -> Value {
        match self {
            AST::BoolLiteral { value } => ast_obj! { "ASTBoolLiteral";
                "value" => Value::Bool(value)
            },
            AST::IntLiteral { value } => ast_obj! { "ASTIntLiteral";
                "value" => Value::Int(value)
            },
            AST::FloatLiteral { value } => ast_obj! { "ASTFloatLiteral";
                "value" => Value::Float(value)
            },
            AST::StringLiteral { text } => ast_obj! { "ASTStringLiteral";
                "text" => Value::String(text)
            },
            AST::Comment { text } => ast_obj! { "ASTComment";
                "text" => Value::String(text)
            },
            AST::Ident { name } => ast_obj! { "ASTIdent";
                "name" => Value::String(name)
            },
            AST::ListLiteral { elements } => ast_obj! { "ASTListLiteral";
                "elements" => Value::List {
                    elems: elements.into_iter().map(AST::to_value).map(Box::new).map(Value::ObjectRef).collect()
                }
            },
            AST::BinaryExpr { op, lhs, rhs } => ast_obj! { "ASTBinaryExpr";
                "op" => Value::String(op),
                "lhs" => Value::ObjectRef(Box::new(lhs.to_value())),
                "rhs" => Value::ObjectRef(Box::new(rhs.to_value()))
            },
            AST::RefExpr { value } => ast_obj! { "ASTRefExpr";
                "value" => Value::ObjectRef(Box::new(value.to_value()))
            },
            AST::ObjectLiteral { class, fields } => ast_obj! { "ASTObjectLiteral";
                "class" => Value::String(class),
                "fields" => Value::List {
                    elems: fields.into_iter().map(AST::to_value).map(Box::new).map(Value::ObjectRef).collect()
                }
            },
            AST::FieldLiteral { name, value } => ast_obj! { "ASTFieldLiteral";
                "name" => Value::String(name),
                "value" => Value::ObjectRef(Box::new(value.to_value()))
            },
            AST::ForLoop {
                var_name,
                target,
                body,
            } => ast_obj! { "ASTForLoop";
                "var_name" => Value::String(var_name),
                "target" => Value::ObjectRef(Box::new(target.to_value())),
                "body" => Value::List {
                    elems: body.into_iter().map(AST::to_value).map(Box::new).map(Value::ObjectRef).collect()
                }
            },
            AST::IfStmt {
                condition,
                body,
                elses,
            } => ast_obj! { "ASTIfStmt";
                "condition" => Value::ObjectRef(Box::new(condition.to_value())),
                "body" => Value::List {
                    elems: body.into_iter().map(AST::to_value).map(Box::new).map(Value::ObjectRef).collect()
                },
                "elses" => Value::List {
                    elems: elses.into_iter().map(AST::to_value).map(Box::new).map(Value::ObjectRef).collect()
                }
            },
            AST::FnCall { name, args } => ast_obj! { "ASTFnCall";
                "name" => Value::String(name),
                "args" => Value::List {
                    elems: args.into_iter().map(AST::to_value).map(Box::new).map(Value::ObjectRef).collect()
                }
            },
            AST::LetStmt { var_name, value } => ast_obj! { "ASTLetStmt";
                "var_name" => Value::String(var_name),
                "value" => Value::ObjectRef(Box::new(value.to_value()))
            },
            AST::RetStmt { value } => ast_obj! { "ASTRetStmt";
                "value" => Value::ObjectRef(Box::new(value.to_value()))
            },
            AST::FnDef { name, params, body } => ast_obj! { "ASTFnDef";
                "name" => Value::String(name),
                "params" => Value::List {
                    elems: params.into_iter().map(Value::String).collect()
                },
                "body" => Value::List {
                    elems: body.into_iter().map(AST::to_value).map(Box::new).map(Value::ObjectRef).collect()
                }
            },
            AST::ClassDef {
                name,
                fields,
                methods,
            } => ast_obj! { "ASTClassDef";
                "name" => Value::String(name),
                "fields" => Value::List {
                    elems: fields.into_iter().map(Value::String).collect()
                },
                "methods" => Value::List {
                    elems: methods.into_iter().map(AST::to_value).map(Box::new).map(Value::ObjectRef).collect()
                }
            },
            AST::File { name, elements } => ast_obj! { "ASTFile";
                "name" => Value::String(name),
                "elements" => Value::List {
                    elems: elements.into_iter().map(AST::to_value).map(Box::new).map(Value::ObjectRef).collect()
                }
            },
        }
    }
}
