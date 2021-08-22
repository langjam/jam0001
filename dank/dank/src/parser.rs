#![allow(clippy::nonstandard_macro_braces)]
#![allow(clippy::redundant_closure_call)]

use crate::ast::*;
use crate::data::Value;

macro_rules! op_node {
    ($x:ident, $y:ident, $kind:expr) => {
        Expr {
            span: $x.span.start..$y.span.end,
            kind: ExprKind::Binary(Box::new($x), $kind, Box::new($y)),
        }
    };
    ($x:ident, $kind:expr) => {
        Expr {
            span: $x.span.clone(),
            kind: ExprKind::Unary($kind, Box::new($x)),
        }
    };
}

fn parse_comment(comment: Vec<&str>) -> CommentBody<'_> {
    let comment_text = comment
        .into_iter()
        .map(|c| c.trim())
        .collect::<Vec<_>>()
        .join("\n")
        .into();

    // TODO: do not leak the string here to get around lifetimes
    let text_on_heap = Box::leak(Box::new(format!("{{\n{}\n}}", comment_text)));

    match dank::block(text_on_heap) {
        Ok(block) => CommentBody::Stmt(block),
        Err(e) => {
            // TODO: optionally log the error
            eprintln!("Failed to evaluate a comment: {}", e);
            CommentBody::Text(comment_text)
        }
    }
}

peg::parser!(pub grammar dank() for str {
    /// Parses whitespace
    rule _() = [' ' | '\t']*
    /// Parses newlines
    rule __() = ['\n' | '\r']*
    /// Parses whitespace or newlines
    rule ___() = [' ' | '\t' | '\n' | '\r']*

    rule line_comment_group() -> Vec<&'input str>
        = (_ "//" t:$([^'\n']*) "\n"? {t})+

    /// Parses a group of single-line comments, optionally followed by a statement.
    rule line_comment() -> LineComment<'input>
        = g:line_comment_group() "\n"? _ s:stmt() {
                LineComment {
                    body: parse_comment(g),
                    stmt: Some(s),
                }
           }
           / s:stmt() {
                LineComment {
                    body: CommentBody::Empty,
                    stmt: Some(s),
                }
           }
           / g:line_comment_group() {
                LineComment {
                    body: parse_comment(g),
                    stmt: None,
                }
           }

    pub rule binding() -> Stmt<'input>
    =  start:position!() "let" ___ i:ident() ___ initializer:("=" ___ i:expr() {i})? end:position!() {
        Stmt { kind: StmtKind::LetDecl(i.into(), initializer.map(Box::new)), span: start..end }
    }

    pub rule stmt() -> Stmt<'input>
         = p:print() ___ ";" {p}
         / b:binding() ___ ";" {b}
         / w:while_loop() {w}
         / b:block() {b}
         / f:func() {f}
         / start:position!() "break" ___ ";" end:position!() { Stmt { span: start..end, kind: StmtKind::Break } }
         / start:position!() "continue" ___ ";" end:position!() { Stmt { span: start..end, kind: StmtKind::Continue } }
         / start:position!() "return" ___ value:expr()? ";" end:position!() {
            Stmt { span: start..end, kind: StmtKind::Return(value.map(Box::new)) }
         }
         / start:position!() name:ident() ___ "=" ___ value:expr() ___ ";" end:position!() {
          Stmt { span: start..end, kind: StmtKind::Assignment(name.into(), Box::new(value)) }
         }
         / e:expr() ___ ";" end:position!() { Stmt {  span: e.span.start..end, kind: StmtKind::ExprStmt(Box::new(e)) } }


    pub rule func() -> Stmt<'input>
    = start:position!() "fn" ___ name:ident() _ "(" args:ident() ** ("," ___) ")" ___ body:block() end:position!()
    {
        Stmt { span: start..end, kind: StmtKind::FuncDecl(Box::new(Function {
            name: name.into(),
            args: args.into_iter().map(Into::into).collect(),
            body: match body.kind { StmtKind::Block(b) => b, _ => unreachable!() }
        })) }
    }

    pub rule lambda() -> Expr<'input>
    = start:position!() "fn" _ "(" args:ident() ** ("," ___) ")" ___ body:block() end:position!()
    {
      Expr { span: start..end, kind: ExprKind::LambdaLiteral(Box::new(Function {
        name: format!("fn@{}:{}", start, end).into(),
        args: args.into_iter().map(Into::into).collect(),
        body: match body.kind { StmtKind::Block(b) => b, _ => unreachable!() }
    })) }
    }

    pub rule block() -> Stmt<'input>
    = start:position!() "{" ___ statements:(s:line_comment() ___ {s})* ___ "}" end:position!() {
        Stmt {
            span: start..end,
            kind:  StmtKind::Block(statements)
        }
    }

    pub rule while_loop() -> Stmt<'input>
    = start:position!() "while" _ c:expr() _ b:block() {
        Stmt { span: start..b.span.end, kind: StmtKind::While(Box::new(c), Box::new(b)) }
    }

    pub rule expr() -> Expr<'input> = operators()

    pub rule operators() -> Expr<'input>
    = precedence! {
        x:(@) ___ "&&" ___ y:@ { op_node!(x, y, BinOpKind::And) }
        x:(@) ___ "||" ___ y:@ { op_node!(x, y, BinOpKind::Or) }
        --
        x:(@) ___ "==" ___ y:@ { op_node!(x, y, BinOpKind::Eq) }
        x:(@) ___ "!=" ___ y:@ { op_node!(x, y, BinOpKind::Ne) }
        --
        x:(@) ___ "<" ___ y:@ { op_node!(x, y, BinOpKind::Lt) }
        x:(@) ___ "<=" ___ y:@ { op_node!(x, y, BinOpKind::Le) }
        x:(@) ___ ">" ___ y:@ { op_node!(x, y, BinOpKind::Gt) }
        x:(@) ___ ">=" ___ y:@ { op_node!(x, y, BinOpKind::Ge) }
        --
        x:(@) ___ "+" ___ y:@ { op_node!(x,y, BinOpKind::Add) }
        x:(@) ___ "-" ___ y:@ { op_node!(x,y, BinOpKind::Sub) }
        --
        x:(@) ___ "*" ___ y:@ { op_node!(x, y, BinOpKind::Mul) }
        x:(@) ___ "/" ___ y:@ { op_node!(x, y, BinOpKind::Div) }
        --
        "-" x:(@) { op_node!(x, UnOpKind::Neg) }
        "!" x:(@) { op_node!(x, UnOpKind::Not) }
        --
        a:access() { a }
    }

    #[cache_left_rec]
    pub rule access() -> Expr<'input>
    = start:position!() obj:access()  "." prop:ident() end:position!() {
        Expr { span: start..end, kind: ExprKind::Property(prop.into(), Box::new(obj)) }
      }
      / start:position!() callee:access() "(" args:expr() ** ("," ___) ")" end:position!() {
        Expr { span: start..end, kind: ExprKind::Call(Box::new(callee), args) }
      }
      / primary()

    pub rule primary() -> Expr<'input>
        = lambda()
        / start:position!() l:literal() end:position!() { Expr { kind: ExprKind::Literal(l), span: start..end }}
        / start:position!() i:ident() end:position!() { Expr { kind: ExprKind::Variable(i.into()), span: start..end } }
        / "(" _ e:expr() _ ")" { e }
        / object()

    pub rule object() -> Expr<'input>
        = start:position!()
         "{" ___ entries:(k:ident() _ ":" _ v:expr() {(k, v)})  ** ("," _) ___ "}"
          end:position!() {
            Expr {
                span: start..end,
                kind: ExprKind::ObjectLiteral(entries.into_iter().map(|(k, v)| {(k.into(), v)}).collect())
            }
        }

    pub rule literal() -> Value<'input>
    =  n:$(['0'..='9']+ ("." ['0'..='9']*)?) { Value::Num(n.parse().unwrap()) }
        / "\"" s:$([^ '"' | '\n']*) "\"" { Value::Str(s.into())}
        / "\'" s:$([^ '\'' | '\n']*) "\'" { Value::Str(s.into())}
        / "true"  { Value::Bool(true) }
        / "false" { Value::Bool(false) }
        / "null" { Value::Null }

    rule print() -> Stmt<'input>
         = start:position!() "print" _ args:(expr() ** ("," ___)) end:position!()
         { Stmt { kind: StmtKind::Print(args), span: Span { start, end } } }

    rule string() -> &'input str
        = s:$(['a'..='z'|'A'..='Z'|'0'..='9'|'_']*) { s }

    /// Parses reserved keywords
    rule reserved()
        = "null"
        / "false"
        / "true"
        / "fn"
        / "return"
        / "print"

    /// A list of identifiers separated by whitespace.
    rule ident_with_whitespace() -> Vec<&'input str>
        = i:$(ident()) ** [' ' | '\t'] { i }

    /// Parses the first character of an identifier, which cannot contain numbers
    rule ident_start() -> &'input str = s:$(['a'..='z'|'A'..='Z'|'_']) { s }

    /// Parses any alphanumeric characters as part of an identifier
    rule ident_chars() -> &'input str = s:$(['a'..='z'|'A'..='Z'|'0'..='9'|'_']) { s }

    /// Parses an entire identifier, ensuring no reserved keywords are used
    rule ident() -> &'input str
        = i:quiet!{ $(ident_start() ident_chars()*) } { i }

    rule line() -> LineComment<'input>
        = _ l:line_comment() __ { l }

    /// Parses a file.
    pub rule file() -> Ast<'input>
        = ___?
          lines:(line()*)
          ___?
        {
            Ast { statements: lines }
        }
});

#[cfg(test)]
pub mod tests {
    use ast2str::AstToStr;
    use pretty_assertions::assert_eq;

    use super::*;

    macro_rules! test_eq {
        ($input:expr, $expected:expr) => {
            assert_eq!(
                dank::file($input)
                    .unwrap()
                    .ast_to_str()
                    .replace(|c| ['├', '─', '│', '╰', '✕', '↓'].contains(&c), " ")
                    .trim()
                    .as_display(),
                $expected.trim().as_display()
            )
        };
    }

    #[test]
    fn expression_statements() {
        let test = r#"
        5;
        call();
        a.b;
        obj.method();
      "#;

        test_eq!(
            test,
            r#"
Ast
  statements= 
    LineComment
      body: CommentBody::Empty
      stmt: StmtKind::ExprStmt
        expr: ExprKind::Literal
          field0: Num(5.0)
    LineComment
      body: CommentBody::Empty
      stmt: StmtKind::ExprStmt
        expr: ExprKind::Call
          callee: ExprKind::Variable
            name: "call"
          args= 
    LineComment
      body: CommentBody::Empty
      stmt: StmtKind::ExprStmt
        expr: ExprKind::Property
          name: "b"
          obj: ExprKind::Variable
            name: "a"
    LineComment
      body: CommentBody::Empty
      stmt: StmtKind::ExprStmt
        expr: ExprKind::Call
          callee: ExprKind::Property
            name: "method"
            obj: ExprKind::Variable
              name: "obj"
          args=
"#
        );
    }

    #[test]
    fn fn_declarations() {
        let test = r#"
        fn test() { }
        fn with_args(a, b) { print a, b; }

        let f = fn() {};
        let f_with_args = fn(a, b) { print a, b; };

        fn returns_x(x) { return x; }
        fn returns_nothing() { return; }
      "#;

        test_eq!(
            test,
            r#"
Ast
  statements= 
    LineComment
      body: CommentBody::Empty
      stmt: Function
        name: "test"
        args= 
        body= 
    LineComment
      body: CommentBody::Empty
      stmt: Function
        name: "with_args"
        args= 
          "a"
          "b"
        body= 
          LineComment
            body: CommentBody::Empty
            stmt: StmtKind::Print
              args= 
                ExprKind::Variable
                  name: "a"
                ExprKind::Variable
                  name: "b"
    LineComment
      body: CommentBody::Empty
      stmt: StmtKind::LetDecl
        name: "f"
        initializer: ExprKind::LambdaLiteral
          field0: Function
            name: "fn@83:90"
            args= 
            body= 
    LineComment
      body: CommentBody::Empty
      stmt: StmtKind::LetDecl
        name: "f_with_args"
        initializer: ExprKind::LambdaLiteral
          field0: Function
            name: "fn@118:142"
            args= 
              "a"
              "b"
            body= 
              LineComment
                body: CommentBody::Empty
                stmt: StmtKind::Print
                  args= 
                    ExprKind::Variable
                      name: "a"
                    ExprKind::Variable
                      name: "b"
    LineComment
      body: CommentBody::Empty
      stmt: Function
        name: "returns_x"
        args= 
          "x"
        body= 
          LineComment
            body: CommentBody::Empty
            stmt: StmtKind::Return
              value: ExprKind::Variable
                name: "x"
    LineComment
      body: CommentBody::Empty
      stmt: Function
        name: "returns_nothing"
        args= 
        body= 
          LineComment
            body: CommentBody::Empty
            stmt: StmtKind::Return
              value: None
        "#
        );
    }

    #[test]
    fn properties_and_calls() {
        let test = r#"

        let x;
        let y = x.abc;
        let z = x.a.b.c.d;

        let a = z();
        let b = x.abc();
        let c = y.a.b.c.d();
        let d = c()()(1, 2, 3)();

        "#;

        test_eq!(
            test,
            r#"Ast
  statements= 
    LineComment
      body: CommentBody::Empty
      stmt: StmtKind::LetDecl
        name: "x"
        initializer: None
    LineComment
      body: CommentBody::Empty
      stmt: StmtKind::LetDecl
        name: "y"
        initializer: ExprKind::Property
          name: "abc"
          obj: ExprKind::Variable
            name: "x"
    LineComment
      body: CommentBody::Empty
      stmt: StmtKind::LetDecl
        name: "z"
        initializer: ExprKind::Property
          name: "d"
          obj: ExprKind::Property
            name: "c"
            obj: ExprKind::Property
              name: "b"
              obj: ExprKind::Property
                name: "a"
                obj: ExprKind::Variable
                  name: "x"
    LineComment
      body: CommentBody::Empty
      stmt: StmtKind::LetDecl
        name: "a"
        initializer: ExprKind::Call
          callee: ExprKind::Variable
            name: "z"
          args= 
    LineComment
      body: CommentBody::Empty
      stmt: StmtKind::LetDecl
        name: "b"
        initializer: ExprKind::Call
          callee: ExprKind::Property
            name: "abc"
            obj: ExprKind::Variable
              name: "x"
          args= 
    LineComment
      body: CommentBody::Empty
      stmt: StmtKind::LetDecl
        name: "c"
        initializer: ExprKind::Call
          callee: ExprKind::Property
            name: "d"
            obj: ExprKind::Property
              name: "c"
              obj: ExprKind::Property
                name: "b"
                obj: ExprKind::Property
                  name: "a"
                  obj: ExprKind::Variable
                    name: "y"
          args= 
    LineComment
      body: CommentBody::Empty
      stmt: StmtKind::LetDecl
        name: "d"
        initializer: ExprKind::Call
          callee: ExprKind::Call
            callee: ExprKind::Call
              callee: ExprKind::Call
                callee: ExprKind::Variable
                  name: "c"
                args= 
              args= 
            args= 
              ExprKind::Literal
                field0: Num(1.0)
              ExprKind::Literal
                field0: Num(2.0)
              ExprKind::Literal
                field0: Num(3.0)
          args=
        "#
        );
    }

    #[test]
    fn lone_comment() {
        let test = r#"
        
            // test comment

        "#;
        assert_eq!(
            dank::file(test).unwrap(),
            Ast {
                statements: vec![LineComment {
                    body: CommentBody::Text("test comment".into()),
                    stmt: None,
                }],
            }
        )
    }

    #[test]
    fn attached_comment() {
        let test = r#"
        
        // attached comment
        print variable;


        // detached comment


        print some_statement_way_down_below;


        // This is a comment group
        // where each line belongs to the same comment,
        // as long as there is no gaps between the lines.


        // These two lines do not form a comment group

        // since there are gaps between them.

        "#;
        test_eq!(
            test,
            r#"
Ast
  statements= 
    LineComment
      body: CommentBody::Text
        text: "attached comment"
      stmt: StmtKind::Print
        args= 
          ExprKind::Variable
            name: "variable"
    LineComment
      body: CommentBody::Text
        text: "detached comment"
      stmt: None
    LineComment
      body: CommentBody::Empty
      stmt: StmtKind::Print
        args= 
          ExprKind::Variable
            name: "some_statement_way_down_below"
    LineComment
      body: CommentBody::Text
        text: "This is a comment group\nwhere each line belongs to the same comment,\nas long as there is no gaps between the lines."
      stmt: None
    LineComment
      body: CommentBody::Text
        text: "These two lines do not form a comment group"
      stmt: None
    LineComment
      body: CommentBody::Text
        text: "since there are gaps between them."
      stmt: None
"#
        );
    }

    #[test]
    fn block() {
        let test = r#"
            {}
            {
                print("code inside the block");
                let x = 5;
            }
        "#;
        test_eq!(
            test,
            r#"
Ast
  statements= 
    LineComment
      body: CommentBody::Empty
      stmt: StmtKind::Block
        statements= 
    LineComment
      body: CommentBody::Empty
      stmt: StmtKind::Block
        statements= 
          LineComment
            body: CommentBody::Empty
            stmt: StmtKind::Print
              args= 
                ExprKind::Literal
                  field0: Str("code inside the block")
          LineComment
            body: CommentBody::Empty
            stmt: StmtKind::LetDecl
              name: "x"
              initializer: ExprKind::Literal
                field0: Num(5.0)
"#
        );
    }

    #[test]
    fn while_loop() {
        let test = r#"
        while true { print("hello" + "world"); }
        while true { break; continue; }
        "#;
        test_eq!(
            test,
            r#"
Ast
  statements= 
    LineComment
      body: CommentBody::Empty
      stmt: StmtKind::While
        condition: ExprKind::Literal
          field0: Bool(true)
        body: StmtKind::Block
          statements= 
            LineComment
              body: CommentBody::Empty
              stmt: StmtKind::Print
                args= 
                  ExprKind::Binary
                    left: ExprKind::Literal
                      field0: Str("hello")
                    op: BinOpKind::Add
                    right: ExprKind::Literal
                      field0: Str("world")
    LineComment
      body: CommentBody::Empty
      stmt: StmtKind::While
        condition: ExprKind::Literal
          field0: Bool(true)
        body: StmtKind::Block
          statements= 
            LineComment
              body: CommentBody::Empty
              stmt: StmtKind::Break
            LineComment
              body: CommentBody::Empty
              stmt: StmtKind::Continue
"#
        );
    }

    #[test]
    fn arithmetics() {
        let test = r#"
        let a = 1 + 2 * 3 / 4;
        let b = -false;
        let c = null;
        let d = a && b || (!c);
        let e = d < 3 && c > 5 && b <= 2 && a >= 1 || a == 7 && b != 8;
        "#;
        test_eq!(
            test,
            r#"
Ast
  statements= 
    LineComment
      body: CommentBody::Empty
      stmt: StmtKind::LetDecl
        name: "a"
        initializer: ExprKind::Binary
          left: ExprKind::Literal
            field0: Num(1.0)
          op: BinOpKind::Add
          right: ExprKind::Binary
            left: ExprKind::Binary
              left: ExprKind::Literal
                field0: Num(2.0)
              op: BinOpKind::Mul
              right: ExprKind::Literal
                field0: Num(3.0)
            op: BinOpKind::Div
            right: ExprKind::Literal
              field0: Num(4.0)
    LineComment
      body: CommentBody::Empty
      stmt: StmtKind::LetDecl
        name: "b"
        initializer: ExprKind::Unary
          op: UnOpKind::Neg
          operand: ExprKind::Literal
            field0: Bool(false)
    LineComment
      body: CommentBody::Empty
      stmt: StmtKind::LetDecl
        name: "c"
        initializer: ExprKind::Literal
          field0: Null
    LineComment
      body: CommentBody::Empty
      stmt: StmtKind::LetDecl
        name: "d"
        initializer: ExprKind::Binary
          left: ExprKind::Binary
            left: ExprKind::Variable
              name: "a"
            op: BinOpKind::And
            right: ExprKind::Variable
              name: "b"
          op: BinOpKind::Or
          right: ExprKind::Unary
            op: UnOpKind::Not
            operand: ExprKind::Variable
              name: "c"
    LineComment
      body: CommentBody::Empty
      stmt: StmtKind::LetDecl
        name: "e"
        initializer: ExprKind::Binary
          left: ExprKind::Binary
            left: ExprKind::Binary
              left: ExprKind::Binary
                left: ExprKind::Binary
                  left: ExprKind::Binary
                    left: ExprKind::Variable
                      name: "d"
                    op: BinOpKind::Lt
                    right: ExprKind::Literal
                      field0: Num(3.0)
                  op: BinOpKind::And
                  right: ExprKind::Binary
                    left: ExprKind::Variable
                      name: "c"
                    op: BinOpKind::Gt
                    right: ExprKind::Literal
                      field0: Num(5.0)
                op: BinOpKind::And
                right: ExprKind::Binary
                  left: ExprKind::Variable
                    name: "b"
                  op: BinOpKind::Le
                  right: ExprKind::Literal
                    field0: Num(2.0)
              op: BinOpKind::And
              right: ExprKind::Binary
                left: ExprKind::Variable
                  name: "a"
                op: BinOpKind::Ge
                right: ExprKind::Literal
                  field0: Num(1.0)
            op: BinOpKind::Or
            right: ExprKind::Binary
              left: ExprKind::Variable
                name: "a"
              op: BinOpKind::Eq
              right: ExprKind::Literal
                field0: Num(7.0)
          op: BinOpKind::And
          right: ExprKind::Binary
            left: ExprKind::Variable
              name: "b"
            op: BinOpKind::Ne
            right: ExprKind::Literal
              field0: Num(8.0)"#
        );
    }

    #[allow(clippy::approx_constant)]
    #[test]
    fn let_declarations() {
        let test = r#"
        let a = 3.141592;
        let b = "hello";
        let c = true;
        let d = false;
        let e = null;
        let f = { object: "value", second: 2 };
        let g;
        "#;
        test_eq!(
            test,
            r#"
Ast
  statements= 
    LineComment
      body: CommentBody::Empty
      stmt: StmtKind::LetDecl
        name: "a"
        initializer: ExprKind::Literal
          field0: Num(3.141592)
    LineComment
      body: CommentBody::Empty
      stmt: StmtKind::LetDecl
        name: "b"
        initializer: ExprKind::Literal
          field0: Str("hello")
    LineComment
      body: CommentBody::Empty
      stmt: StmtKind::LetDecl
        name: "c"
        initializer: ExprKind::Literal
          field0: Bool(true)
    LineComment
      body: CommentBody::Empty
      stmt: StmtKind::LetDecl
        name: "d"
        initializer: ExprKind::Literal
          field0: Bool(false)
    LineComment
      body: CommentBody::Empty
      stmt: StmtKind::LetDecl
        name: "e"
        initializer: ExprKind::Literal
          field0: Null
    LineComment
      body: CommentBody::Empty
      stmt: StmtKind::LetDecl
        name: "f"
        initializer: ExprKind::ObjectLiteral
          field0= 
            tuple
              field0: "object"
              field1: ExprKind::Literal
                field0: Str("value")
            tuple
              field0: "second"
              field1: ExprKind::Literal
                field0: Num(2.0)
    LineComment
      body: CommentBody::Empty
      stmt: StmtKind::LetDecl
        name: "g"
        initializer: None"#
        );
    }
    pub trait AsDisplay: std::fmt::Display {
        fn as_display(&self) -> DisplayAsDebugWrapper<&'_ Self> {
            DisplayAsDebugWrapper(self)
        }
    }
    impl AsDisplay for str {}

    #[derive(Clone, PartialEq)]
    pub struct DisplayAsDebugWrapper<T>(T);

    impl<T> std::fmt::Debug for DisplayAsDebugWrapper<T>
    where
        T: std::fmt::Display,
    {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.0)
        }
    }

    impl<T> std::ops::Deref for DisplayAsDebugWrapper<T> {
        type Target = T;

        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }
}
