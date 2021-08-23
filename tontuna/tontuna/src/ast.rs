use std::rc::Rc;

use crate::Span;
pub(crate) use crate::lexer::TokenKind;

#[derive(Debug, Clone)]
pub(crate) struct Program {
    pub(crate) code: NakedBlock,
    pub(crate) code_markers: Vec<Token>,
}

#[derive(Debug, Clone)]
pub(crate) enum Expr {
    Name {
        name: Token,
    },
    Number {
        tok: Token,
        value: i64,
    },
    Bool {
        tok: Token,
        value: bool,
    },
    Str {
        tok: Token,
        value: String,
    },
    Nil {
        tok: Token,
    },
    SelfExpr {
        tok: Token,
    },
    Call {
        func: Box<Expr>,
        left_paren: Token,
        args: CommaList<Expr>,
        right_paren: Token,
    },
    Paren {
        left_paren: Token,
        inner: Box<Expr>,
        right_paren: Token,
    },
    BinOp {
        lhs: Box<Expr>,
        operator: Token,
        rhs: Box<Expr>,
    },
    Field {
        obj: Box<Expr>,
        dot: Token,
        field: Token,
    },
    AssignVar {
        name: Token,
        eq: Token,
        value: Box<Expr>,
    },
    AssignField {
        obj: Box<Expr>,
        dot: Token,
        field: Token,
        eq: Token,
        value: Box<Expr>,
    },
}

impl Expr {
    pub(crate) fn span(&self) -> Span {
        match self {
            Expr::Name { name } => name.span,
            Expr::Number { tok, .. } |
            Expr::Bool { tok, .. } |
            Expr::Str { tok, .. } |
            Expr::Nil { tok } |
            Expr::SelfExpr { tok } => tok.span,
            Expr::Call { func, right_paren, .. } => func.span().merge(right_paren.span),
            Expr::Paren { left_paren, right_paren, .. } => left_paren.span.merge(right_paren.span),
            Expr::BinOp { lhs, rhs, .. } => lhs.span().merge(rhs.span()),
            Expr::Field { obj, field, .. } => obj.span().merge(field.span),
            Expr::AssignVar { name, value, .. } => name.span.merge(value.span()),
            Expr::AssignField { obj, value, .. } => obj.span().merge(value.span()),
        }
    }
}

pub(crate) type CommaList<T> = Vec<ListItem<T>>;

#[derive(Debug, Clone)]
pub(crate) struct ListItem<T> {
    pub(crate) item: T,
    pub(crate) comma: Option<Token>,
}

#[derive(Debug, Clone)]
pub(crate) enum Stmt {
    If {
        if_tok: Token,
        cond: IfCond,
        body: Block,
        tail: IfTail,
    },
    While {
        while_tok: Token,
        cond: IfCond,
        body: Block,
    },
    Expr {
        expr: Expr,
        semi: Token,
    },
    For {
        for_tok: Token,
        name: Token,
        in_tok: Token,
        iterable: Expr,
        body: Block,
    },
    Return {
        ret: Token,
        value: Expr,
        semi: Token,
    },
    Let {
        let_tok: Token,
        name: Token,
        eq: Token,
        value: Expr,
        semi: Token,
    },
    Comment(Comment),
    FnDef(Rc<FnDef>),
    StructDef {
        struct_tok: Token,
        name: Token,
        left_curly: Token,
        fns: Vec<FnDef>,
        right_curly: Token,
    },
    Block(Block),
}

impl Stmt {
    pub(crate) fn span(&self) -> Span {
        match self {
            Stmt::While { while_tok, body, .. } => while_tok.span.merge(body.span()),
            Stmt::If { if_tok, body, tail, .. } => tail
                .span()
                .unwrap_or_else(|| body.span())
                .merge(if_tok.span),
            Stmt::Expr { expr, semi } => expr.span().merge(semi.span),
            Stmt::For { for_tok, body, .. } => for_tok.span.merge(body.span()),
            Stmt::Return { ret, semi, .. } => ret.span.merge(semi.span),
            Stmt::Let { let_tok, semi, .. } => let_tok.span.merge(semi.span),
            Stmt::Comment(c) => c.span(),
            Stmt::FnDef(d) => d.span(),
            Stmt::StructDef { struct_tok, right_curly, .. } => struct_tok.span.merge(right_curly.span),
            Stmt::Block(b) => b.span(),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum IfCond {
    Expr(Expr),
    TypeTest {
        let_tok: Token,
        name: Token,
        colon: Token,
        ty: Expr,
        eq: Token,
        value: Expr,
    },
}

impl IfCond {
    pub(crate) fn span(&self) -> Span {
        match self {
            IfCond::Expr(e) => e.span(),
            IfCond::TypeTest { let_tok, name, colon, ty, eq, value } => let_tok.span.merge(value.span()),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum IfTail {
    None,
    Else {
        else_tok: Token,
        body: Block,
    },
    ElseIf {
        else_tok: Token,
        if_tok: Token,
        cond: IfCond,
        body: Block,
        tail: Box<IfTail>,
    },
}

impl IfTail {
    pub(crate) fn span(&self) -> Option<Span> {
        match self {
            IfTail::None => None,
            IfTail::Else { else_tok, body } => Some(else_tok.span.merge(body.span())),
            IfTail::ElseIf { else_tok, body, tail, .. } => Some(tail
                .span()
                .unwrap_or_else(|| body.span())
                .merge(else_tok.span)),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct FnDef {
    pub(crate) fn_tok: Token,
    pub(crate) name: Token,
    pub(crate) left_paren: Token,
    pub(crate) params: CommaList<Token>,
    pub(crate) right_paren: Token,
    pub(crate) body: Block,
}

impl FnDef {
    pub(crate) fn span(&self) -> Span {
        self.fn_tok.span.merge(self.body.span())
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Block {
    pub(crate) left_curly: Token,
    pub(crate) contents: NakedBlock,
    pub(crate) right_curly: Token,
}

impl Block {
    pub(crate) fn span(&self) -> Span {
        self.left_curly.span.merge(self.right_curly.span)
    }
}

#[derive(Debug, Clone)]
pub(crate) struct NakedBlock {
    pub(crate) stmts: Vec<Rc<Stmt>>,
}

impl NakedBlock {
    pub(crate) fn span(&self) -> Option<Span> {
        Some(self.stmts.first()?.span().merge(self.stmts.last()?.span()))
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Comment {
    // can be interleaved with or even inside comment elements
    pub(crate) markers: Vec<Token>,
    pub(crate) elements: Vec<CommentElem>,
}

impl Comment {
    pub(crate) fn span(&self) -> Span {
        let elements = self.elements.iter().filter_map(|c| c.span()).reduce(Span::merge);
        let markers = match self.markers.as_slice() {
            [] => None,
            [single] => Some(single.span),
            [a, .., b] => Some(a.span.merge(b.span)),
        };
        match (markers, elements) {
            (Some(a), Some(b)) => a.merge(b),
            (Some(a), None) => a,
            (None, Some(a)) => a,
            (None, None) => panic!("empty comment"),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum CommentElem {
    Text(Token),
    Code {
        // can be inside code contents
        markers: Vec<Token>,
        code: NakedBlock,
    },
}

impl CommentElem {
    pub(crate) fn span(&self) -> Option<Span> {
        match self {
            CommentElem::Text(t) => Some(t.span),
            CommentElem::Code { markers, code } => {
                let start = markers.iter().map(|t| t.span.start).min();
                let end = markers.iter().map(|t| t.span.start).max();
                let code = code.span();
                let markers = match (start, end) {
                    (Some(s), Some(e)) => Some(Span::new(s, e)),
                    _ => None,
                };
                match (markers, code) {
                    (Some(a), Some(b)) => Some(a.merge(b)),
                    (Some(a), None) => Some(a),
                    (None, Some(a)) => Some(a),
                    (None, None) => None,
                }
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct Token {
    pub(crate) span: Span,
    pub(crate) kind: TokenKind,
}
