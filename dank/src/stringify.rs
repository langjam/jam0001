use crate::ast::*;

const INDENT_SIZE: usize = 4;

pub trait Stringify {
    fn stringify(&self) -> String {
        self.stringify_impl(0)
    }
    fn stringify_impl(&self, indent_level: usize) -> String;
}

impl<'s> Stringify for Stmt<'s> {
    fn stringify_impl(&self, indent_level: usize) -> String {
        match &*self.kind.borrow() {
            StmtKind::LetDecl(name, initializer) => {
                format!(
                    "let {}{};",
                    name,
                    initializer
                        .as_ref()
                        .map(|e| format!(" = {}", e.stringify_impl(indent_level)))
                        .unwrap_or_else(String::new)
                )
            }
            StmtKind::FuncDecl(f) => f.stringify_impl(indent_level + 1),
            StmtKind::If(_) => todo!(),
            StmtKind::While(_, _) => todo!(),
            StmtKind::ExprStmt(e) => format!("{};", e.stringify_impl(indent_level)),
            StmtKind::Print(args) => format!(
                "print {};",
                args.iter()
                    .map(|a| a.stringify_impl(indent_level))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            StmtKind::Block(b) => {
                let lines = b
                    .statements
                    .iter()
                    .filter_map(|line| line.stmt.as_ref())
                    .map(|stmt| indent(indent_level, stmt.stringify_impl(indent_level + 1)))
                    .collect::<Vec<_>>()
                    .join("\n");
                format!(
                    "{{\n{}{}",
                    lines,
                    indent(indent_level.saturating_sub(1), "}".into())
                )
            }
            StmtKind::UnscopedBlock(b) => b
                .iter()
                .filter_map(|line| line.stmt.as_ref())
                .map(|stmt| indent(indent_level, stmt.stringify_impl(indent_level + 1)))
                .collect::<Vec<_>>()
                .join("\n"),
            StmtKind::Return(val) => format!(
                "return {};",
                val.as_ref()
                    .map(|e| format!("({})", e.stringify_impl(indent_level)))
                    .unwrap_or_default()
            ),
            StmtKind::Assignment(name, value) => {
                format!("{} = {};", name, value.stringify_impl(indent_level))
            }
            StmtKind::PropAssignment(obj, name, value) => {
                format!(
                    "({}).{} = {};",
                    obj.stringify_impl(indent_level),
                    name,
                    value.stringify_impl(indent_level)
                )
            }
            StmtKind::DynPropAssignment(obj, key, value) => {
                format!(
                    "({})[{}] = {};",
                    obj.stringify_impl(indent_level),
                    key.stringify_impl(indent_level),
                    value.stringify_impl(indent_level)
                )
            }
            StmtKind::Break => "break;".into(),
            StmtKind::Continue => "continue;".into(),
        }
    }
}

impl<'s> Stringify for Expr<'s> {
    fn stringify_impl(&self, indent_level: usize) -> String {
        match &*self.kind.borrow() {
            ExprKind::ObjectLiteral(s) => {
                format!(
                    "({{ {} }})",
                    s.iter()
                        .map(|(k, v)| format!("{}: {}", k, v.stringify_impl(indent_level)))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            ExprKind::LambdaLiteral(f) => f.stringify_impl(indent_level + 1),
            ExprKind::Literal(l) => l.to_string(),
            ExprKind::Variable(v) => v.to_string(),
            ExprKind::Property(name, obj) => {
                format!("({}).{}", obj.stringify_impl(indent_level), name)
            }
            ExprKind::DynProperty(key, obj) => {
                format!(
                    "({})[{}]",
                    obj.stringify_impl(indent_level),
                    key.stringify_impl(indent_level)
                )
            }
            ExprKind::Call(callee, args) => format!(
                "({})({})",
                callee.stringify_impl(indent_level),
                args.iter()
                    .map(|a| a.stringify_impl(indent_level))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            ExprKind::Binary(l, op, r) => format!(
                "({} {} {})",
                l.stringify_impl(indent_level),
                op.symbol(),
                r.stringify_impl(indent_level)
            ),
            ExprKind::Unary(op, expr) => {
                format!("({}({}))", op.symbol(), expr.stringify_impl(indent_level))
            }
        }
    }

    fn stringify(&self) -> String {
        self.stringify_impl(0)
    }
}

impl<'s> Stringify for Function<'s> {
    fn stringify_impl(&self, indent_level: usize) -> String {
        let name = if self.name.starts_with("lambda@") {
            ""
        } else {
            &self.name
        };
        format!(
            "fn {}({}) {{\n{}\n{}",
            name,
            self.args
                .iter()
                .map(|a| a.to_string())
                .collect::<Vec<_>>()
                .join(", "),
            self.body.stringify_impl(indent_level),
            indent(indent_level.saturating_sub(1), "}".into()),
        )
    }
}

pub fn indent(level: usize, code: String) -> String {
    format!("{}{}", " ".repeat(INDENT_SIZE * level), code)
}
