use std::rc::Rc;

use crate::{
    ast::{self, TokenKind},
    Pos,
    Span,
};

// #[derive(Debug)]
// pub(crate) struct Error {
//     pub(crate) span: Span,
//     pub(crate) message: String,
// }
use crate::Error;

type Result<T> = std::result::Result<T, Error>;

trait NakedBlockExt {
    fn consume_stmts(self) -> Vec<ast::Stmt>;
}

impl NakedBlockExt for ast::NakedBlock {
    fn consume_stmts(self) -> Vec<ast::Stmt> {
        self.stmts
            .into_iter()
            .map(|s| match Rc::try_unwrap(s) {
                Ok(s) => s,
                Err(_) => panic!("statement was aliased during parsing"),
            })
            .collect()
    }
}

#[derive(Debug, Clone, Copy)]
struct Line<'a> {
    start_pos: Pos,
    text: &'a str,
    levels: u32,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Marker {
    Code,
    Comment,
}

impl Marker {
    fn other(self) -> Marker {
        match self {
            Marker::Code => Marker::Comment,
            Marker::Comment => Marker::Code,
        }
    }

    fn text(self) -> &'static str {
        match self {
            Marker::Code => ">",
            Marker::Comment => "#",
        }
    }

    fn into_token_kind(self) -> TokenKind {
        match self {
            Marker::Code => TokenKind::CodeMarker,
            Marker::Comment => TokenKind::CommentMarker,
        }
    }
}

fn find_leading_marker(line: &str) -> Option<(Marker, usize)> {
    let suffix = line.trim_start_matches(|c: char| c.is_ascii_whitespace());
    let kind = match suffix.chars().next() {
        Some('#') => Marker::Comment,
        Some('>') => Marker::Code,
        _ => return None,
    };
    let offset = line.len() - suffix.len();
    Some((kind, offset))
}

#[test]
fn find_leading_marker_test() {
    fn check(text: &str, expected: Option<(Marker, usize)>) {
        assert_eq!(expected, find_leading_marker(text));
    }
    check("#  ", Some((Marker::Comment, 0)));
    check("##  ", Some((Marker::Comment, 0)));
    check("  #  ", Some((Marker::Comment, 2)));
    check(" > ", Some((Marker::Code, 1)));
    check("", None);
    check("< # >", None);
    check("foo # ", None);
}

impl<'a> Line<'a> {
    fn new(start_pos: Pos, text: &'a str, first_level: Marker) -> Line<'a> {
        let mut levels = 0;
        let mut trail_text = text;
        let mut level = first_level;
        while let Some((marker, offset)) = find_leading_marker(trail_text) {
            if marker == level {
                trail_text = &trail_text[(offset + 1)..];
                level = level.other();
                levels += 1;
            } else {
                break;
            }
        }
        Line { start_pos, text, levels }
    }

    fn leading_marker(&self, level: u32) -> ast::Token {
        assert!(self.levels != 0);
        let (kind, offset) = find_leading_marker(self.text).unwrap();
        let span = Span::new(
            self.start_pos.plus_text(&self.text[..offset]),
            self.start_pos.plus_text(&self.text[..(offset + 1)]),
        );
        ast::Token { span, kind: kind.into_token_kind() }
    }

    fn strip_one_marker(&self) -> (ast::Token, Line<'a>) {
        assert!(self.levels != 0);
        let (kind, offset) = find_leading_marker(self.text).unwrap();
        let token_start = self.start_pos.plus_text(&self.text[..offset]);
        let token_end = token_start.plus_text(kind.text());
        let token = ast::Token {
            kind: kind.into_token_kind(),
            span: Span::new(token_start, token_end),
        };
        let line = Line {
            start_pos: token_end,
            text: &self.text[(offset + 1)..],
            levels: self.levels - 1,
        };
        (token, line)
    }

    fn end(&self) -> Line<'a> {
        Line {
            start_pos: self.start_pos.plus_text(self.text),
            text: "",
            levels: 0,
        }
    }
}

pub(crate) fn parse(source: &str) -> Result<ast::Program> {
    let first_marker = detect_first_marker_type(source)?;
    let mut lines = Vec::new();
    let mut pos = Pos::START;
    for line in source.split_inclusive('\n') {
        lines.push(Line::new(pos, line, first_marker));
        pos = pos.plus_text(line);
    }
    match first_marker {
        Marker::Comment => {
            let code = parse_code(source, &lines)?;
            Ok(ast::Program {
                code,
                code_markers: Vec::new(),
            })
        }
        Marker::Code => {
            let comment = parse_comment(source, &lines, Vec::new())?.elements;
            let mut stmts = Vec::new();
            let mut code_markers = Vec::new();
            for element in comment {
                match element {
                    e @ ast::CommentElem::Text(_) => {
                        match stmts.last_mut() {
                            Some(ast::Stmt::Comment(c)) => c.elements.push(e),
                            Some(_) | None => {
                                stmts.push(ast::Stmt::Comment(ast::Comment {
                                    markers: Vec::new(),
                                    elements: vec![e],
                                }));
                            }
                        }
                    }
                    ast::CommentElem::Code { markers, code } => {
                        code_markers.extend(markers);
                        stmts.extend(code.consume_stmts());
                    }
                }
            }
            Ok(ast::Program {
                code: ast::NakedBlock { stmts: stmts.into_iter().map(Rc::new).collect() },
                code_markers,
            })
        }
    }
}

fn detect_first_marker_type(source: &str) -> Result<Marker> {
    let mut first_marker = None;
    let mut pos = Pos::START;
    for line in source.split_inclusive('\n') {
        if let Some((marker, offset)) = find_leading_marker(line) {
            let pos = pos.plus_text(&line[..offset]);
            match first_marker {
                Some((first, _first_pos)) if first != marker => {
                    return Err(Error {
                        message: "inconsistent file mode".to_owned(),
                        span: Span::new(pos, pos.plus_text(marker.text())),
                    });
                }
                Some(_) => {}
                None => first_marker = Some((
                    marker,
                    pos,
                )),
            }
        }
        pos = pos.plus_text(line);
    }
    Ok(first_marker.map(|(kind, _)| kind).unwrap_or(Marker::Comment))
}

fn parse_comment(source: &str, mut lines: &[Line<'_>], markers: Vec<ast::Token>) -> Result<ast::Comment> {
    let mut elements = Vec::new();
    while lines.len() > 0 {
        let first_line = &lines[0];
        if first_line.levels == 0 {
            let span = Span::new(
                first_line.start_pos,
                first_line.start_pos.plus_text(first_line.text),
            );
            elements.push(ast::CommentElem::Text(ast::Token {
                span,
                kind: TokenKind::CommentText,
            }));
            lines = &lines[1..];
        } else {
            let mut code_lines = Vec::new();
            let mut code_markers = Vec::new();
            while lines.get(0).map(|l| l.levels > 0).unwrap_or(false) {
                let (marker, line) = lines[0].strip_one_marker();
                code_lines.push(line);
                code_markers.push(marker);
                lines = &lines[1..];
            }
            let code = parse_code(source, &code_lines)?;
            elements.push(ast::CommentElem::Code {
                markers: code_markers,
                code,
            });
        }
    }
    Ok(ast::Comment {
        markers,
        elements,
    })
}

fn parse_code(src: &str, lines: &[Line<'_>]) -> Result<ast::NakedBlock> {
    if lines.is_empty() {
        return Ok(ast::NakedBlock { stmts: Vec::new() })
    }
    let mut parser = Parser::new(src, lines);
    let block = parser.parse_naked_block()?;
    if parser.peek().is_some() {
        eprintln!("some input left");
        match parser.parse_stmt() {
            Ok(_) => panic!("parser didn't finish parsing"),
            Err(e) => return Err(e),
        }
    }
    Ok(block)
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
enum ParseHint {
    Stmt,
    Expr,
    Operator,
    Token(TokenKind),
}

impl ParseHint {
    fn covers(self, other: ParseHint) -> bool {
        match (self, other) {
            (ParseHint::Stmt, ParseHint::Stmt) |
            (ParseHint::Stmt, ParseHint::Expr) => true,
            (ParseHint::Stmt, ParseHint::Operator) => false,
            (ParseHint::Stmt, ParseHint::Token(t)) => can_start_stmt(t),
            (ParseHint::Expr, ParseHint::Stmt) |
            (ParseHint::Expr, ParseHint::Operator) => false,
            (ParseHint::Expr, ParseHint::Expr) => true,
            (ParseHint::Expr, ParseHint::Token(t)) => can_start_expr(t),
            (ParseHint::Operator, ParseHint::Stmt) |
            (ParseHint::Operator, ParseHint::Expr) => false,
            (ParseHint::Operator, ParseHint::Operator) => true,
            (ParseHint::Operator, ParseHint::Token(t)) => is_operator(t),
            (ParseHint::Token(_), ParseHint::Stmt) |
            (ParseHint::Token(_), ParseHint::Expr) |
            (ParseHint::Token(_), ParseHint::Operator) => false,
            (ParseHint::Token(a), ParseHint::Token(b)) => a == b,
        }
    }

    fn to_str(&self) -> &'static str {
        match self {
            ParseHint::Stmt => "statement",
            ParseHint::Expr => "expression",
            ParseHint::Operator => "operator",
            ParseHint::Token(t) => t.to_str(),
        }
    }
}

struct Parser<'a, 'src> {
    hints: Vec<ParseHint>,
    remaining_lines: &'a [Line<'src>],
    current_line: Line<'src>,
    src: &'src str,
}

impl<'a, 'src> Parser<'a, 'src> {
    fn new(src: &'src str, lines: &'a [Line<'src>]) -> Self {
        let first_line = lines[0];
        let mut parser = Parser {
            hints: Vec::new(),
            remaining_lines: &lines[1..],
            current_line: first_line,
            src,
        };
        parser.skip_whitespace();
        parser
    }

    fn current_pos(&self) -> Pos {
        self.current_line.start_pos
    }

    fn peek_token(&mut self) -> Option<ast::Token> {
        loop {
            if self.current_line.text != "" {
                let (kind, len) = crate::lexer::next_token(&self.current_line.text).unwrap();
                let text = &self.current_line.text[..len];
                let end_pos = self.current_line.start_pos.plus_text(text);
                let token = ast::Token {
                    span: Span::new(self.current_pos(), end_pos),
                    kind,
                };
                break Some(token);
            } else if self.remaining_lines.len() > 0 {
                self.current_line = self.remaining_lines[0];
                self.remaining_lines = &self.remaining_lines[1..];
            } else {
                break None;
            }
        }
    }

    fn peek(&mut self) -> Option<ast::TokenKind> {
        self.peek_token().map(|t| t.kind)
    }

    fn advance_raw(&mut self) -> Option<ast::Token> {
        self.hints.clear();
        loop {
            if self.current_line.text != "" {
                let (kind, len) = crate::lexer::next_token(&self.current_line.text).unwrap();
                let text = &self.current_line.text[..len];
                let end_pos = self.current_line.start_pos.plus_text(text);
                let token = ast::Token {
                    span: Span::new(self.current_pos(), end_pos),
                    kind,
                };
                self.current_line.start_pos = end_pos;
                self.current_line.text = &self.current_line.text[len..];
                if text.trim() != "" {
                    self.current_line.levels = 0;
                }
                break Some(token);
            } else if self.remaining_lines.len() > 0 {
                self.current_line = self.remaining_lines[0];
                self.remaining_lines = &self.remaining_lines[1..];
            } else {
                break None;
            }
        }
    }

    fn skip_whitespace(&mut self) {
        while matches!(self.peek(), Some(TokenKind::Space | TokenKind::Newline)) {
            self.advance_raw();
        }
    }

    fn consume(&mut self) -> Option<ast::Token> {
        let token = self.advance_raw()?;
        self.skip_whitespace();
        Some(token)
    }

    fn parse_error(&mut self) -> Error {
        let message = if self.peek() == Some(TokenKind::Error) {
            "bad token".to_owned()
        } else {
            self.hints.sort();
            self.hints.dedup();
            let hints = &self.hints;
            let mut final_hints = Vec::<ParseHint>::new();
            for &hint in &self.hints {
                if final_hints.iter().all(|h| !h.covers(hint)) {
                    final_hints.push(hint);
                }
            }
            let expectations = final_hints.iter()
                .map(|h| h.to_str())
                .collect::<Vec<_>>();
            let mut msg = String::from("expected ");
            match expectations.as_slice() {
                [] => panic!("no expectations"),
                [e] => msg += e,
                [e1, e2] => {
                    msg += e1;
                    msg += " or ";
                    msg += e2;
                }
                [expectations @ .., last] => {
                    for e in expectations {
                        msg += e;
                        msg += ", ";
                    }
                    msg += "or ";
                    msg += last;
                }
            }
            if let Some(token) = self.peek() {
                msg += ", got ";
                msg += token.to_str();
            }
            msg
        };
        Error {
            span: self.peek_token()
                .map(|t| t.span)
                .unwrap_or(Span::new(self.current_pos(), self.current_pos())),
            message,
        }
    }

    fn hint(&mut self, hint: ParseHint) {
        self.hints.push(hint);
    }

    fn check(&mut self, kind: TokenKind) -> Option<ast::Token> {
        self.hint(ParseHint::Token(kind));
        if self.peek() == Some(kind) {
            self.consume()
        } else {
            None
        }
    }

    fn expect(&mut self, kind: TokenKind) -> Result<ast::Token> {
        if let Some(tok) = self.check(kind) {
            Ok(tok)
        } else {
            Err(self.parse_error())
        }
    }

    fn parse_block(&mut self) -> Result<ast::Block> {
        let left_curly = self.expect(TokenKind::LeftCurly)?;
        let contents = self.parse_naked_block()?;
        let right_curly = self.expect(TokenKind::RightCurly)?;
        Ok(ast::Block {
            left_curly,
            contents,
            right_curly,
        })
    }

    fn parse_naked_block(&mut self) -> Result<ast::NakedBlock> {
        let mut stmts = Vec::new();
        while self.peek().map(|t| can_start_stmt(t)).unwrap_or(false) {
            stmts.push(Rc::new(self.parse_stmt()?));
        }
        Ok(ast::NakedBlock { stmts })
    }

    fn parse_stmt(&mut self) -> Result<ast::Stmt> {
        self.hint(ParseHint::Stmt);
        if self.peek() == Some(TokenKind::LeftCurly) {
            let block = self.parse_block()?;
            Ok(ast::Stmt::Block(block))
        } else if let Some(while_tok) = self.check(TokenKind::While) {
            let cond = self.parse_if_cond()?;
            let body = self.parse_block()?;
            Ok(ast::Stmt::While { while_tok, cond, body })
        } else if let Some(if_tok) = self.check(TokenKind::If) {
            let cond = self.parse_if_cond()?;
            let body = self.parse_block()?;
            let tail = self.parse_if_tail()?;
            Ok(ast::Stmt::If { if_tok, cond, body, tail })
        } else if let Some(for_tok) = self.check(TokenKind::For) {
            let name = self.expect(TokenKind::Name)?;
            let in_tok = self.expect(TokenKind::In)?;
            let iterable = self.parse_expr(Prec::Min)?;
            let body = self.parse_block()?;
            Ok(ast::Stmt::For {
                for_tok,
                name,
                in_tok,
                iterable,
                body,
            })
        } else if let Some(struct_tok) = self.check(TokenKind::Struct) {
            let name = self.expect(TokenKind::Name)?;
            let left_curly = self.expect(TokenKind::LeftCurly)?;
            let mut fns = Vec::new();
            loop {
                if let Some(right_curly) = self.check(TokenKind::RightCurly) {
                    break Ok(ast::Stmt::StructDef {
                        struct_tok,
                        name,
                        left_curly,
                        fns,
                        right_curly,
                    });
                }
                let fn_tok = self.expect(TokenKind::Fn)?;
                fns.push(self.parse_fn_def(fn_tok)?);
            }

        } else if let Some(fn_tok) = self.check(TokenKind::Fn) {
            Ok(ast::Stmt::FnDef(Rc::new(self.parse_fn_def(fn_tok)?)))
        } else if let Some(ret) = self.check(TokenKind::Return) {
            let value = self.parse_expr(Prec::Min)?;
            let semi = self.expect(TokenKind::Semicolon)?;
            Ok(ast::Stmt::Return { ret, value, semi })
        } else if self.peek() == Some(TokenKind::CommentMarker) {
            if self.current_line.levels == 0 {
                return Err(Error {
                    span: self.peek_token().unwrap().span,
                    message: "limitation of current implementation: comment must be at the start of a line".to_owned(),
                });
            }
            let mut comment_lines = Vec::new();
            let mut comment_markers = Vec::new();
            let (marker, line) = self.current_line.strip_one_marker();
            comment_lines.push(line);
            comment_markers.push(marker);
            self.current_line = self.current_line.end();
            while self.remaining_lines.len() > 0 {
                let line = self.remaining_lines[0];
                if line.levels > 0 {
                    let (marker, line) = line.strip_one_marker();
                    comment_lines.push(line);
                    comment_markers.push(marker);
                } else {
                    break;
                }
                self.current_line = line.end();
                self.remaining_lines = &self.remaining_lines[1..];
            }
            let comment = parse_comment(self.src, &comment_lines, comment_markers)?;
            self.skip_whitespace();
            Ok(ast::Stmt::Comment(comment))
        } else if let Some(let_tok) = self.check(TokenKind::Let) {
            let name = self.expect(TokenKind::Name)?;
            let eq = self.expect(TokenKind::Equals)?;
            let value = self.parse_expr(Prec::Min)?;
            let semi = self.expect(TokenKind::Semicolon)?;
            Ok(ast::Stmt::Let { let_tok, name, eq, value, semi })
        } else {
            let expr = self.parse_expr(Prec::Min)?;
            let semi = self.expect(TokenKind::Semicolon)?;
            Ok(ast::Stmt::Expr { expr, semi })
        }
    }

    fn parse_if_cond(&mut self) -> Result<ast::IfCond> {
        if let Some(let_tok) = self.check(TokenKind::Let) {
            let name = self.expect(TokenKind::Name)?;
            let colon = self.expect(TokenKind::Colon)?;
            let ty = self.parse_expr(Prec::Or)?;
            let eq = self.expect(TokenKind::Equals)?;
            let value = self.parse_expr(Prec::Min)?;
            Ok(ast::IfCond::TypeTest { let_tok, name, colon, ty, eq, value })
        } else {
            let expr = self.parse_expr(Prec::Min)?;
            Ok(ast::IfCond::Expr(expr))
        }
    }

    fn parse_if_tail(&mut self) -> Result<ast::IfTail> {
        if let Some(else_tok) = self.check(TokenKind::Else) {
            if let Some(if_tok) = self.check(TokenKind::If) {
                let cond = self.parse_if_cond()?;
                let body = self.parse_block()?;
                let tail = Box::new(self.parse_if_tail()?);
                Ok(ast::IfTail::ElseIf { else_tok, if_tok, cond, body, tail })
            } else {
                let body = self.parse_block()?;
                Ok(ast::IfTail::Else { else_tok, body })
            }
        } else {
            Ok(ast::IfTail::None)
        }
    }

    fn parse_fn_def(&mut self, fn_tok: ast::Token) -> Result<ast::FnDef> {
        let name = self.expect(TokenKind::Name)?;
        let left_paren = self.expect(TokenKind::LeftParen)?;
        let params = self.parse_list(|p| p.expect(TokenKind::Name))?;
        let right_paren = self.expect(TokenKind::RightParen)?;
        let body = self.parse_block()?;
        Ok(ast::FnDef {
            fn_tok,
            name,
            left_paren,
            params,
            right_paren,
            body,
        })
    }

    fn parse_expr(&mut self, min_prec: Prec) -> Result<ast::Expr> {
        self.hint(ParseHint::Expr);
        let mut expr = self.parse_atom_expr()?;
        while let Some(kind) = self.peek() {
            self.hint(ParseHint::Operator);
            self.hint(ParseHint::Token(TokenKind::Dot));
            self.hint(ParseHint::Token(TokenKind::LeftParen));
            match binop_prec(kind) {
                Some((prec, rhs_prec)) if prec >= min_prec => {
                    let operator = self.expect(kind).unwrap();
                    let rhs = self.parse_expr(rhs_prec)?;
                    if kind == TokenKind::Equals {
                        match expr {
                            ast::Expr::Name { name } => {
                                expr = ast::Expr::AssignVar {
                                    name,
                                    eq: operator,
                                    value: Box::new(rhs),
                                };
                            }
                            ast::Expr::Field { obj, dot, field } => {
                                expr = ast::Expr::AssignField {
                                    obj,
                                    dot,
                                    field,
                                    eq: operator,
                                    value: Box::new(rhs),
                                };
                            }
                            _ => {
                                return Err(Error {
                                    span: expr.span(),
                                    message: "invalid assignment target".to_owned(),
                                });
                            }
                        }
                    } else {
                        expr = ast::Expr::BinOp {
                            lhs: Box::new(expr),
                            operator,
                            rhs: Box::new(rhs),
                        };
                    }
                }
                Some(_) => break,
                None if kind == TokenKind::Dot && Prec::CallField >= min_prec => {
                    let dot = self.expect(TokenKind::Dot).unwrap();
                    let field = self.expect(TokenKind::Name)?;
                    expr = ast::Expr::Field {
                        obj: Box::new(expr),
                        dot,
                        field,
                    };
                }
                None if kind == TokenKind::LeftParen && Prec::CallField >= min_prec => {
                    let left_paren = self.expect(TokenKind::LeftParen).unwrap();
                    let args = self.parse_list(|p| p.parse_expr(Prec::Min))?;
                    let right_paren = self.expect(TokenKind::RightParen)?;
                    expr = ast::Expr::Call {
                        func: Box::new(expr),
                        left_paren,
                        args,
                        right_paren,
                    };
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn parse_list<T>(&mut self, parse_item: impl Fn(&mut Self) -> Result<T>) -> Result<ast::CommaList<T>> {
        let mut list = Vec::new();
        loop {
            if self.peek() == Some(TokenKind::RightParen) {
                return Ok(list);
            }
            let item = parse_item(self)?;
            if let Some(comma) = self.check(TokenKind::Comma) {
                list.push(ast::ListItem { item, comma: Some(comma) });
            } else {
                list.push(ast::ListItem { item, comma: None });
                return Ok(list);
            }
        }
    }

    fn parse_atom_expr(&mut self) -> Result<ast::Expr> {
        if let Some(name) = self.check(TokenKind::Name) {
            Ok(ast::Expr::Name { name })
        } else if let Some(tok) = self.check(TokenKind::Number) {
            match self.token_source(tok).parse::<i64>() {
                Ok(value) => Ok(ast::Expr::Number { tok, value }),
                Err(_) => return Err(Error {
                    span: tok.span,
                    message: "invalid number".to_owned(),
                }),
            }
        } else if let Some(tok) = self.check(TokenKind::False) {
            Ok(ast::Expr::Bool { tok, value: false })
        } else if let Some(tok) = self.check(TokenKind::True) {
            Ok(ast::Expr::Bool { tok, value: true })
        } else if let Some(tok) = self.check(TokenKind::Nil) {
            Ok(ast::Expr::Nil { tok })
        } else if let Some(tok) = self.check(TokenKind::SelfKw) {
            Ok(ast::Expr::SelfExpr { tok })
        } else if let Some(tok) = self.check(TokenKind::Str) {
            let source = self.token_source(tok);
            let value = parse_string_value(
                tok.span.start.plus_text("\""),
                &source[1..(source.len() - 1)],
            )?;
            Ok(ast::Expr::Str { tok, value })
        } else if let Some(left_paren) = self.check(TokenKind::LeftParen) {
            let inner = self.parse_expr(Prec::Min)?;
            let right_paren = self.expect(TokenKind::RightParen)?;
            Ok(ast::Expr::Paren {
                left_paren,
                inner: Box::new(inner),
                right_paren,
            })
        } else {
            Err(self.parse_error())
        }
    }

    fn token_source(&self, token: ast::Token) -> &str {
        &self.src[token.span.source_range()]
    }
}

fn parse_string_value(mut pos: Pos, text: &str) -> Result<String> {
    let mut result = String::new();
    let mut chars = text.chars();
    while let Some(c) = chars.next() {
        if c == '\\' {
            let start_pos = pos;
            pos = pos.plus_char(c);
            let next = chars.next();
            pos = pos.plus_char(c);
            let escape_span = Span::new(start_pos, pos);
            let escaped = match next {
                Some('n') => '\n',
                Some('r') => '\r',
                Some('\\') => '\\',
                Some('"') => '"',
                _ => return Err(Error {
                    span: escape_span,
                    message: "invalid escape sequence".to_owned(),
                }),
            };
            result.push(escaped);
        } else {
            result.push(c);
            pos = pos.plus_char(c);
        }
    }
    Ok(result)
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
enum Prec {
    Min,
    Assign,
    Or,
    And,
    Equals,
    Compare,
    AddSub,
    MulDiv,
    CallField,
    Atom,
}

fn binop_prec(token: TokenKind) -> Option<(Prec, Prec)> {
    match token {
        TokenKind::Equals => Some((Prec::Assign, Prec::Assign)),
        TokenKind::Or => Some((Prec::Or, Prec::And)),
        TokenKind::And => Some((Prec::And, Prec::Equals)),
        TokenKind::EqEq |
        TokenKind::NotEq => Some((Prec::Equals, Prec::Compare)),
        TokenKind::Less |
        TokenKind::LessEq |
        TokenKind::Greater |
        TokenKind::GreaterEq => Some((Prec::Compare, Prec::AddSub)),
        TokenKind::Plus |
        TokenKind::Minus => Some((Prec::AddSub, Prec::MulDiv)),
        TokenKind::Star |
        TokenKind::Slash => Some((Prec::MulDiv, Prec::CallField)),
        _ => None,
    }
}

fn can_start_expr(token: TokenKind) -> bool {
    match token {
        TokenKind::True |
        TokenKind::False |
        TokenKind::Str |
        TokenKind::Nil |
        TokenKind::SelfKw |
        TokenKind::LeftParen |
        TokenKind::Name |
        TokenKind::Number => true,
        _ => false,
    }
}

fn can_start_stmt(token: TokenKind) -> bool {
    match token {
        TokenKind::Fn |
        TokenKind::Let |
        TokenKind::While |
        TokenKind::If |
        TokenKind::For |
        TokenKind::Return |
        TokenKind::Struct |
        TokenKind::True |
        TokenKind::False |
        TokenKind::Str |
        TokenKind::Nil |
        TokenKind::SelfKw |
        TokenKind::LeftParen |
        TokenKind::LeftCurly |
        TokenKind::Name |
        TokenKind::Number |
        TokenKind::CommentMarker => true,
        _ => false,
    }
}

fn is_operator(token: TokenKind) -> bool {
    match token {
        TokenKind::Equals |
        TokenKind::And |
        TokenKind::Or |
        TokenKind::Plus |
        TokenKind::Minus |
        TokenKind::Star |
        TokenKind::Slash |
        TokenKind::Less |
        TokenKind::LessEq |
        TokenKind::Greater |
        TokenKind::GreaterEq |
        TokenKind::EqEq |
        TokenKind::NotEq => true,
        _ => false,
    }
}
