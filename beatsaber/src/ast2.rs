use crate::ast1::{AssignTarget, AssignValue, Behaviour, Expr, Op, Parser1};
use crate::error::{Diagnostic, Label, Reporter};
use logos::Span;
use std::collections::HashMap;

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub struct Identifier {
    pub id: usize,
    //pub name: Span,
}

#[derive(Clone, Debug)]
pub struct CallExpr {
    pub function: Identifier,
    pub p1: Box<DecoratedExpr>,
    pub p2: Option<Box<DecoratedExpr>>,
}

#[derive(Clone, Debug)]
pub enum DecoratedExpr {
    CallExpr(CallExpr),
    Identifier(Identifier),
}

#[derive(Clone, Debug)]
pub struct FuncBlock {
    // Function declaration
    pub decl: FuncDecl,
    pub block: Vec<DecoratedStmt>,
}

#[derive(Clone, Debug)]
pub struct LoadLiteralNumber {
    pub line: usize,
    pub ident: Identifier,
    pub value: usize,
}

#[derive(Clone, Debug)]
pub struct LoadLiteralString {
    pub line: usize,
    pub ident: Identifier,
    pub value: String,
}

#[derive(Clone, Debug)]
pub struct FuncDecl {
    pub line: usize,
    pub id: Identifier,
    pub p1: Identifier,
    pub p2: Option<Identifier>,
}

#[derive(Clone, Debug)]
pub struct Conditional {
    pub condition: Identifier,
    pub success: Box<DecoratedStmt>,
}

#[derive(Clone, Debug)]
pub struct Assignment {
    pub line: usize,
    pub name: Option<Identifier>,
    pub value: DecoratedExpr,
}

#[derive(Clone, Debug)]
pub struct ExternFunction {
    pub line: usize,
    pub name: String,
    pub ident: Identifier,
    pub module: Option<String>,
    pub two_param: bool,
}

#[derive(Clone, Debug)]
pub struct ReturnStmt {
    pub line: usize,
    pub expr: DecoratedExpr,
}

#[derive(Clone, Debug)]
pub struct GotoStmt {
    pub line: usize,
    pub target: DecoratedExpr,
}

#[derive(Clone, Debug)]
pub enum Callable {
    ExternFunction(ExternFunction),
    FuncBlock(FuncBlock),
}

#[derive(Clone, Debug)]
pub enum DecoratedStmt {
    LoadLiteralNumber(LoadLiteralNumber),
    LoadLiteralString(LoadLiteralString),
    Callable(Callable),
    Conditional(Conditional),
    Assignment(Assignment),
    ReturnStmt(ReturnStmt),
    GotoStmt(GotoStmt),
}

impl Callable {
    pub fn two_params(&self) -> bool {
        match self {
            Callable::FuncBlock(FuncBlock { decl, .. }) => decl.p2.is_some(),
            Callable::ExternFunction(ExternFunction { two_param, .. }) => *two_param,
        }
    }
}

pub const ARGC_IDENT: Identifier = Identifier { id: usize::MAX };
pub const ARGV_IDENT: Identifier = Identifier { id: usize::MAX - 1 };

impl DecoratedStmt {
    pub fn line_number(&self) -> usize {
        match self {
            DecoratedStmt::LoadLiteralNumber(stmt) => stmt.line,
            DecoratedStmt::LoadLiteralString(stmt) => stmt.line,
            DecoratedStmt::Callable(c) => match c {
                Callable::FuncBlock(FuncBlock { decl, .. }) => decl.line,
                Callable::ExternFunction(ExternFunction { line, .. }) => *line,
            },
            DecoratedStmt::Conditional(stmt) => stmt.success.line_number(),
            DecoratedStmt::Assignment(stmt) => stmt.line,
            DecoratedStmt::ReturnStmt(stmt) => stmt.line,
            DecoratedStmt::GotoStmt(stmt) => stmt.line,
        }
    }
}

fn create_identifier<'a>(
    ids: &mut HashMap<&'a str, Identifier>,
    id: Option<&'a str>,
    id_span: Option<Span>,
    reporter: &Reporter<'a>,
) -> Identifier {
    let identifier = id.unwrap();
    let span = id_span.unwrap();
    if ids.contains_key(identifier) {
        reporter.report_and_exit(
            &Diagnostic::error()
                .with_message("duplicate identifier")
                .with_labels(vec![
                    Label::primary((), span).with_message("this identifier is already declared")
                ]),
        );
    }
    let ident = Identifier { id: ids.len() };
    ids.insert(identifier, ident);
    ident
}

fn create_or_shadow_ident_opt<'a>(
    ids: &mut HashMap<&'a str, Identifier>,
    id: Option<&'a str>,
) -> Option<Identifier> {
    let len = ids.len();
    id.map(|id| *ids.entry(id).or_insert_with(|| Identifier { id: len }))
}

fn create_or_shadow_ident<'a>(
    ids: &mut HashMap<&'a str, Identifier>,
    src: &'a str,
    id: Span,
) -> Identifier {
    let len = ids.len();
    *ids.entry(&src[id])
        .or_insert_with(|| Identifier { id: len })
}

fn parse_behaviour<'a>(
    line: usize,
    behaviour: Behaviour,
    expr: Option<Expr>,
    ids: &mut HashMap<&'a str, Identifier>,
    func_ids: &mut HashMap<Identifier, Callable>,
    parser: &Parser1<'a>,
) -> Option<DecoratedStmt> {
    let src = parser.src();
    match behaviour {
        Behaviour::Assign {
            target: target @ (AssignTarget::Ident(_) | AssignTarget::Discard(_)),
            value,
            ..
        } => {
            let (id, id_span) = match target {
                AssignTarget::Ident(span) => (Some(&src[span.clone()]), Some(span)),
                _ => (None, None),
            };
            match value {
                AssignValue::Number(_, n) => {
                    // Must have an identifier for loading literals
                    Some(DecoratedStmt::LoadLiteralNumber(LoadLiteralNumber {
                        line,
                        ident: create_or_shadow_ident(ids, src, id_span.unwrap()),
                        value: n,
                    }))
                }
                AssignValue::String(str_span) => {
                    // Must have an identifier for loading literals
                    Some(DecoratedStmt::LoadLiteralString(LoadLiteralString {
                        line,
                        ident: create_or_shadow_ident(ids, src, id_span.unwrap()),
                        value: unescape::unescape(&src[(str_span.start + 1)..(str_span.end - 1)])
                            .unwrap_or_else(|| {
                                parser.reporter().report_and_exit(
                                    &Diagnostic::error()
                                        .with_message("invalid string literal")
                                        .with_labels(vec![Label::primary((), str_span)
                                            .with_message(
                                                "this literal contains illegal escape sequences",
                                            )]),
                                )
                            }),
                    }))
                }
                AssignValue::NotHere(not_here) => {
                    // Must have an identifier for exported functions
                    let ident = create_identifier(ids, id, id_span, &parser.reporter());
                    func_ids.insert(
                        ident,
                        Callable::ExternFunction(ExternFunction {
                            line,
                            name: id.unwrap().to_owned(),
                            ident,
                            module: not_here.ident.map(|value| src[value].to_owned()),
                            two_param: not_here.and_is_big.is_some(),
                        }),
                    );
                    None
                }
                AssignValue::Ops(ops) => {
                    // Made up of CallExprs
                    // Each op needs to match the expression op
                    let ident = create_or_shadow_ident_opt(ids, id);

                    Some(DecoratedStmt::Assignment(Assignment {
                        line,
                        name: ident,
                        value: zip_ops_with_expr(
                            &expr.unwrap(),
                            &ops,
                            ids,
                            func_ids,
                            src,
                            &parser.reporter(),
                        ),
                    }))
                }
                AssignValue::Fn(f) => {
                    // Create a function declaration for this, make a function definition for this, add to function collection
                    // All functions will be added to the output vector before being returned
                    let ident = create_identifier(ids, id, id_span, &parser.reporter());
                    let p1 = create_or_shadow_ident(ids, src, f.params.p1);
                    let p2 = create_or_shadow_ident_opt(ids, f.params.p2.map(|v| &src[v]));
                    let mut block = FuncBlock {
                        decl: FuncDecl {
                            line,
                            id: ident,
                            p1,
                            p2,
                        },
                        block: Vec::new(),
                    };
                    if let Some(expr) = &expr {
                        block.block.push(DecoratedStmt::Assignment(Assignment {
                            line,
                            name: None,
                            value: zip_ops_with_expr(
                                expr,
                                &f.ops,
                                ids,
                                func_ids,
                                src,
                                &parser.reporter(),
                            ),
                        }));
                    }
                    func_ids.insert(ident, Callable::FuncBlock(block));
                    // This should NOT be added right away, since it will not hold all of the potential blocks.
                    // See func_ids instead.
                    None
                }
            }
        }
        Behaviour::Assign {
            target: AssignTarget::Goto(span),
            value,
            ..
        } => {
            if expr.is_none() {
                parser.reporter().report_and_exit(
                    &Diagnostic::error()
                        .with_message("goto without expression")
                        .with_labels(vec![Label::primary((), span)
                            .with_message("this goto is missing a target expression")]),
                );
            }
            match value {
                AssignValue::Ops(ops) => Some(DecoratedStmt::GotoStmt(GotoStmt {
                    line,
                    target: zip_ops_with_expr(
                        &expr.unwrap(),
                        &ops,
                        ids,
                        func_ids,
                        src,
                        &parser.reporter(),
                    ),
                })),
                _ => parser.reporter().report_and_exit(
                    &Diagnostic::error()
                        .with_message("goto with invalid assignment")
                        .with_labels(vec![Label::primary((), span).with_message(
                            "this goto is assigned something other than an expression",
                        )]),
                ),
            }
        }
        Behaviour::Assign {
            target: AssignTarget::Return(span),
            value,
            ..
        } => {
            if expr.is_none() {
                parser.reporter().report_and_exit(
                    &Diagnostic::error()
                        .with_message("return without expression")
                        .with_labels(vec![Label::primary((), span)
                            .with_message("this return is missing a value expression")]),
                );
            }
            match value {
                AssignValue::Ops(ops) => Some(DecoratedStmt::ReturnStmt(ReturnStmt {
                    line,
                    expr: zip_ops_with_expr(
                        &expr.unwrap(),
                        &ops,
                        ids,
                        func_ids,
                        src,
                        &parser.reporter(),
                    ),
                })),
                _ => parser.reporter().report_and_exit(
                    &Diagnostic::error()
                        .with_message("return with invalid assignment")
                        .with_labels(vec![Label::primary((), span).with_message(
                            "this return is assigned something other than an expression",
                        )]),
                ),
            }
        }
        Behaviour::StillIn {
            ident, behaviour, ..
        } => {
            // Recursive parse behaviour
            // Map ident to correct function
            let ident_str = &src[ident.clone()];
            let func = ids.get(ident_str).cloned();
            if func.is_none() {
                parser.reporter().report_and_exit(
                    &Diagnostic::error()
                        .with_message("unbound function body")
                        .with_labels(vec![
                            Label::primary((), ident).with_message("this function is not declared")
                        ]),
                );
            }
            let ret = parse_behaviour(line, *behaviour, expr, ids, func_ids, parser).unwrap();
            let body = func_ids.get_mut(&func.unwrap());
            if body.is_none() {
                parser.reporter().report_and_exit(
                    &Diagnostic::error()
                        .with_message("function body bound to variable")
                        .with_labels(vec![Label::primary((), ident)
                            .with_message("this is a variable and not a function")]),
                );
            }
            match body.unwrap() {
                Callable::FuncBlock(FuncBlock { block, .. }) => block.push(ret),
                Callable::ExternFunction(ExternFunction { .. }) => {
                    parser.reporter().report_and_exit(
                        &Diagnostic::error()
                            .with_message("function body bound to external function")
                            .with_labels(vec![
                                Label::primary((), ident).with_message("this function is external")
                            ]),
                    )
                }
            }
            None
        }
        Behaviour::Cond {
            cond,
            behaviour,
            if_,
        } => {
            // Recursive parse behaviour
            // Map cond to identifier
            let ident_str = &src[cond.clone()];
            let ident = ids.get(ident_str).unwrap_or_else(|| {
                parser.reporter().report_and_exit(
                    &Diagnostic::error()
                        .with_message("unbound condition")
                        .with_labels(vec![Label::primary((), cond.clone())
                            .with_message("this variable is not defined")]),
                );
            });
            Some(DecoratedStmt::Conditional(Conditional {
                condition: *ident,
                success: Box::new(
                    parse_behaviour(line, *behaviour, expr, ids, func_ids, parser).unwrap_or_else(
                        || {
                            parser.reporter().report_and_exit(
                                &Diagnostic::error()
                                    .with_message("conditional function declaration")
                                    .with_labels(vec![Label::primary((), (if_.start)..(cond.end))
                                        .with_message("conditional statement here")])
                                    .with_notes(vec![
                                        "functions cannot be conditionally declared".to_string()
                                    ]),
                            )
                        },
                    ),
                ),
            }))
        }
    }
}

pub fn parse(mut parser: Parser1) -> Vec<DecoratedStmt> {
    let mut outp = Vec::new();
    let mut ids = HashMap::new();
    ids.insert("argc", ARGC_IDENT);
    ids.insert("argv", ARGV_IDENT);

    let mut func_ids = HashMap::new();
    while let Some(stmt) = parser.next() {
        let val = parse_behaviour(
            stmt.line,
            stmt.behaviour,
            stmt.expr,
            &mut ids,
            &mut func_ids,
            &parser,
        );
        if let Some(val) = val {
            outp.push(val);
        }
    }
    outp.extend(func_ids.into_values().map(DecoratedStmt::Callable));
    outp
}

fn zip_ops_with_expr<'a>(
    expr: &Expr,
    ops: &[Op],
    ids: &HashMap<&'a str, Identifier>,
    fn_ids: &HashMap<Identifier, Callable>,
    src: &'a str,
    reporter: &Reporter<'a>,
) -> DecoratedExpr {
    fn inner<'a, 'ops>(
        expr: &Expr,
        ops: &'ops [Op],
        ids: &HashMap<&'a str, Identifier>,
        fn_ids: &HashMap<Identifier, Callable>,
        src: &'a str,
        reporter: &Reporter<'a>,
    ) -> (DecoratedExpr, &'ops [Op]) {
        match expr {
            Expr::Binop {
                lhs: lhs_expr,
                rhs: rhs_expr,
                op: dot,
            } => {
                let (lhs, ops) = inner(lhs_expr, ops, ids, fn_ids, src, reporter);
                let (rhs, ops) = inner(rhs_expr, ops, ids, fn_ids, src, reporter);
                let (op, ops) = ops.split_first().unwrap_or_else(|| {
                    reporter.report_and_exit(
                        &Diagnostic::error()
                            .with_message("unbound operation")
                            .with_labels(vec![Label::primary((), dot.clone())
                                .with_message("this operation is not bound to any function")]),
                    )
                });

                let ident = ids.get(&src[op.ident.clone()]).unwrap_or_else(|| {
                    reporter.report_and_exit(
                        &Diagnostic::error()
                            .with_message("unbound operation")
                            .with_labels(vec![
                                Label::primary((), op.ident.clone())
                                    .with_message("this function is not defined"),
                                Label::secondary((), dot.clone())
                                    .with_message("for this operation"),
                            ]),
                    )
                });
                let fun = fn_ids.get(ident).unwrap_or_else(|| {
                    reporter.report_and_exit(
                        &Diagnostic::error()
                            .with_message("operation bound to variable")
                            .with_labels(vec![
                                Label::primary((), op.ident.clone())
                                    .with_message("this is a variable and not a function"),
                                Label::secondary((), dot.clone())
                                    .with_message("for this operation"),
                            ]),
                    )
                });
                if !fun.two_params() {
                    reporter.report_and_exit(
                        &Diagnostic::error()
                            .with_message("invalid parameter count")
                            .with_labels(vec![
                                Label::primary((), op.ident.clone())
                                    .with_message("this function only takes one parameter"),
                                Label::secondary((), lhs_expr.span())
                                    .with_message("first parameter provided here"),
                                Label::secondary((), rhs_expr.span())
                                    .with_message("second parameter provided here"),
                            ]),
                    )
                }

                (
                    DecoratedExpr::CallExpr(CallExpr {
                        function: *ident,
                        p1: Box::new(lhs),
                        p2: Some(Box::new(rhs)),
                    }),
                    ops,
                )
            }
            Expr::Unop {
                expr: expr_expr,
                op: dot,
            } => {
                let (expr, ops) = inner(expr_expr, ops, ids, fn_ids, src, reporter);
                let (op, ops) = ops.split_first().unwrap_or_else(|| {
                    reporter.report_and_exit(
                        &Diagnostic::error()
                            .with_message("unbound operation")
                            .with_labels(vec![Label::primary((), dot.clone())
                                .with_message("this operation is not bound to any function")]),
                    )
                });

                let ident = ids.get(&src[op.ident.clone()]).unwrap_or_else(|| {
                    reporter.report_and_exit(
                        &Diagnostic::error()
                            .with_message("unbound operation")
                            .with_labels(vec![
                                Label::primary((), op.ident.clone())
                                    .with_message("this function is not defined"),
                                Label::secondary((), dot.clone())
                                    .with_message("for this operation"),
                            ]),
                    )
                });
                let fun = fn_ids.get(ident).unwrap_or_else(|| {
                    reporter.report_and_exit(
                        &Diagnostic::error()
                            .with_message("operation bound to variable")
                            .with_labels(vec![
                                Label::primary((), op.ident.clone())
                                    .with_message("this is a variable and not a function"),
                                Label::secondary((), dot.clone())
                                    .with_message("for this operation"),
                            ]),
                    )
                });
                if fun.two_params() {
                    reporter.report_and_exit(
                        &Diagnostic::error()
                            .with_message("invalid parameter count")
                            .with_labels(vec![
                                Label::primary((), op.ident.clone())
                                    .with_message("this function takes two parameters"),
                                Label::secondary((), expr_expr.span())
                                    .with_message("single parameter provided here"),
                            ]),
                    )
                }

                (
                    DecoratedExpr::CallExpr(CallExpr {
                        function: *ident,
                        p1: Box::new(expr),
                        p2: None,
                    }),
                    ops,
                )
            }
            Expr::Paren { expr, .. } => inner(expr, ops, ids, fn_ids, src, reporter),
            Expr::Ident(span) => {
                let ident = ids.get(&src[span.clone()]).unwrap_or_else(|| {
                    reporter.report_and_exit(
                        &Diagnostic::error()
                            .with_message("unbound identifier")
                            .with_labels(vec![Label::primary((), span.clone())
                                .with_message("this variable is not defined")]),
                    )
                });
                (DecoratedExpr::Identifier(*ident), ops)
            }
        }
    }

    let (expr, ops) = inner(expr, ops, ids, fn_ids, src, reporter);
    if !ops.is_empty() {
        reporter.report_and_exit(
            &Diagnostic::error()
                .with_message("extranuous operations")
                .with_labels(
                    ops.iter()
                        .map(|op| {
                            Label::primary((), op.ident.clone())
                                .with_message("this operation is not bound to anything")
                        })
                        .collect(),
                ),
        )
    }
    expr
}
