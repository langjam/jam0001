use logos::Span;

use crate::error::{Diagnostic, Label, Reporter};
use crate::lexer::{Lexer, Token};

#[derive(Debug)]
pub struct Stmt {
    pub line: usize,
    pub expr: Option<Expr>,
    pub sep: Span,
    pub behaviour: Behaviour,
}

#[derive(Debug)]
pub enum Expr {
    Binop {
        lhs: Box<Expr>,
        op: Span,
        rhs: Box<Expr>,
    },
    Unop {
        expr: Box<Expr>,
        op: Span,
    },
    Paren {
        l: Span,
        expr: Box<Expr>,
        r: Span,
    },
    Ident(Span),
}

#[derive(Debug)]
pub enum Behaviour {
    StillIn {
        still_in: Span,
        ident: Span,
        behaviour: Box<Behaviour>,
    },
    Cond {
        if_: Span,
        cond: Span,
        behaviour: Box<Behaviour>,
    },
    Assign {
        target: AssignTarget,
        is: Span,
        value: AssignValue,
    },
}

#[derive(Debug)]
pub enum AssignTarget {
    Discard(Span),
    Return(Span),
    Goto(Span),
    Ident(Span),
}

#[derive(Debug)]
pub struct NotHere {
    pub not_here: Span,
    pub but_is_in: Option<Span>,
    pub ident: Option<Span>,
    pub and_is_big: Option<Span>,
}

#[derive(Debug)]
pub enum AssignValue {
    Ops(Vec<Op>),
    Fn(Fn),
    Number(Span, usize),
    String(Span),
    NotHere(NotHere),
}

#[derive(Debug)]
pub struct Op {
    pub ident: Span,
    pub then: Option<Span>,
}

#[derive(Debug)]
pub struct Fn {
    pub with: Span,
    pub params: FnParams,
    pub ops: Vec<Op>,
}

#[derive(Debug)]
pub struct FnParams {
    pub p1: Span,
    pub and: Option<Span>,
    pub p2: Option<Span>,
}

pub struct Parser1<'a> {
    tokens: Lexer<'a>,
    line: usize,
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Binop { lhs, rhs, .. } => (lhs.span().start)..(rhs.span().end),
            Expr::Unop { expr, op } => (expr.span().start)..(op.end),
            Expr::Paren { l, r, .. } => (l.start)..(r.end),
            Expr::Ident(ident) => ident.clone(),
        }
    }
}

impl Iterator for Parser1<'_> {
    type Item = Stmt;

    fn next(&mut self) -> Option<Self::Item> {
        match self.tokens.peek() {
            Some((Token::Newline, _)) => {
                self.tokens.next();
                self.line += 1;
                self.next()
            }
            Some((Token::BehaviourStart, sep)) => {
                self.tokens.next();
                let behaviour = parse_behaviour(&mut self.tokens);
                Some(Stmt {
                    line: self.line,
                    expr: None,
                    sep,
                    behaviour,
                })
            }
            Some((Token::Identifier | Token::ParenLeft, _)) => {
                let expr = parse_expr(&mut self.tokens);
                let sep = self.tokens.monch(Token::BehaviourStart);
                let behaviour = parse_behaviour(&mut self.tokens);
                Some(Stmt {
                    line: self.line,
                    expr: Some(expr),
                    sep,
                    behaviour,
                })
            }
            None => None,
            Some((t, s)) => self.tokens.reporter().report_and_exit(
                &Diagnostic::error()
                    .with_message(format!("unexpected token `{}` at statement start", t))
                    .with_labels(vec![Label::primary((), s).with_message("not valid here")]),
            ),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let lines = self.tokens.src().split('\n').count();
        let remaining = 1 + lines - self.line;
        (0, Some(remaining))
    }
}

impl<'a> Parser1<'a> {
    pub fn reporter(&self) -> Reporter<'a> {
        self.tokens.reporter()
    }

    pub fn src(&self) -> &'a str {
        self.tokens.src()
    }
}

pub fn parser(lexer: Lexer) -> Parser1 {
    Parser1 {
        tokens: lexer,
        line: 1,
    }
}

fn parse_expr(tokens: &mut Lexer) -> Expr {
    fn parse_expr_inner(expr: Expr, tokens: &mut Lexer) -> Expr {
        let (token, op) = tokens.peek().unwrap();
        match token {
            Token::Operator => {
                tokens.next();
                match tokens.peek().unwrap() {
                    (Token::Operator, _) => parse_expr_inner(
                        Expr::Unop {
                            expr: Box::new(expr),
                            op,
                        },
                        tokens,
                    ),
                    (Token::ParenRight | Token::BehaviourStart, _) => Expr::Unop {
                        expr: Box::new(expr),
                        op,
                    },
                    _ => Expr::Binop {
                        lhs: Box::new(expr),
                        op,
                        rhs: Box::new(parse_expr(tokens)),
                    },
                }
            }
            _ => expr,
        }
    }

    match tokens.next().unwrap() {
        (Token::Identifier, ident) => parse_expr_inner(Expr::Ident(ident), tokens),
        (Token::ParenLeft, l) => {
            let expr = parse_expr(tokens);
            let r = tokens.monch(Token::ParenRight);
            parse_expr_inner(
                Expr::Paren {
                    l,
                    expr: Box::new(expr),
                    r,
                },
                tokens,
            )
        }
        (t, s) => tokens.reporter().report_and_exit(
            &Diagnostic::bug()
                .with_message(format!("unexpected token `{}`", t))
                .with_labels(vec![Label::primary((), s).with_message("not valid here")]),
        ),
    }
}

fn parse_behaviour(tokens: &mut Lexer) -> Behaviour {
    let (token, span) = tokens.next().unwrap();
    match token {
        Token::StillIn => {
            let still_in = span;
            let ident = tokens.monch(Token::Identifier);
            let behaviour = parse_behaviour(tokens);
            Behaviour::StillIn {
                still_in,
                ident,
                behaviour: Box::new(behaviour),
            }
        }
        Token::If => {
            let if_ = span;
            let cond = tokens.monch(Token::Identifier);
            let behaviour = parse_behaviour(tokens);
            Behaviour::Cond {
                if_,
                cond,
                behaviour: Box::new(behaviour),
            }
        }
        Token::Identifier | Token::Discard | Token::Return | Token::Goto => {
            let target = match token {
                Token::Identifier => AssignTarget::Ident(span),
                Token::Discard => AssignTarget::Discard(span),
                Token::Return => AssignTarget::Return(span),
                Token::Goto => AssignTarget::Goto(span),
                _ => unreachable!(),
            };
            let is = tokens.monch(Token::Is);
            let value = parse_assign_value(tokens);
            Behaviour::Assign { target, is, value }
        }
        _ => tokens.reporter().report_and_exit(
            &Diagnostic::bug()
                .with_message(format!("unexpected token `{}` in behaviour", token))
                .with_labels(vec![Label::primary((), span).with_message("not valid here")]),
        ),
    }
}

fn parse_assign_value(tokens: &mut Lexer) -> AssignValue {
    let value = match tokens.peek() {
        Some((Token::Identifier, _)) => return AssignValue::Ops(parse_ops(tokens)),
        Some((Token::With, _)) => return AssignValue::Fn(parse_fn(tokens)),
        Some((Token::Number(n), span)) => AssignValue::Number(span, n),
        Some((Token::StringLiteral, span)) => AssignValue::String(span),
        Some((Token::NotHere, _)) => return AssignValue::NotHere(parse_not_here(tokens)),
        Some((Token::Newline, _)) | None => return AssignValue::Ops(Vec::new()),
        Some((t, s)) => tokens.reporter().report_and_exit(
            &Diagnostic::error()
                .with_message(format!("unexpected token `{}` as assignment value", t))
                .with_labels(vec![Label::primary((), s).with_message("not valid here")]),
        ),
    };
    tokens.next();
    value
}

fn parse_not_here(tokens: &mut Lexer) -> NotHere {
    let (_, not_here) = tokens.next().unwrap();
    match tokens.peek() {
        Some((Token::ButIsIn, but_is_in)) => {
            tokens.next(); // Skip ButIsIn
            let ident = tokens.monch(Token::Identifier);
            let and_is_big = match tokens.peek() {
                Some((Token::ThisIsBig, sp)) => {
                    tokens.next();
                    Some(sp)
                }
                _ => None,
            };
            NotHere {
                not_here,
                but_is_in: Some(but_is_in),
                ident: Some(ident),
                and_is_big,
            }
        }
        _ => {
            let and_is_big = match tokens.peek() {
                Some((Token::ThisIsBig, sp)) => {
                    tokens.next();
                    Some(sp)
                }
                _ => None,
            };
            NotHere {
                not_here,
                but_is_in: None,
                ident: None,
                and_is_big,
            }
        }
    }
}

fn parse_fn(tokens: &mut Lexer) -> Fn {
    let (_, with) = tokens.next().unwrap();
    let p1 = tokens.monch(Token::Identifier);
    let (and, p2) = match tokens.peek() {
        Some((Token::And, and)) => {
            tokens.next();
            let p2 = tokens.monch(Token::Identifier);
            (Some(and), Some(p2))
        }
        _ => (None, None),
    };
    let ops = parse_ops(tokens);
    Fn {
        with,
        params: FnParams { p1, and, p2 },
        ops,
    }
}

fn parse_ops(tokens: &mut Lexer) -> Vec<Op> {
    let mut ops = Vec::new();
    while let Some((token, span)) = tokens.peek() {
        match token {
            Token::Identifier => {
                tokens.next();

                let ident = span;
                let then = match tokens.peek() {
                    Some((Token::Then, span)) => Some(span),
                    Some((Token::Newline, _)) | None => None,
                    Some((t, s)) => tokens.reporter().report_and_exit(
                        &Diagnostic::error()
                            .with_message(&format!("unexpected token `{}` in operator list", t))
                            .with_labels(
                                vec![Label::primary((), s).with_message("not valid here")],
                            ),
                    ),
                };
                ops.push(Op {
                    ident,
                    then: then.clone(),
                });

                if then.is_none() {
                    break;
                }
                tokens.next();
            }
            Token::Newline => break,
            _ => tokens.reporter().report_and_exit(
                &Diagnostic::error()
                    .with_message(&format!("unexpected token `{}` in operator list", token))
                    .with_labels(vec![Label::primary((), span).with_message("not valid here")]),
            ),
        }
    }
    ops
}
