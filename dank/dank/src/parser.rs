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

peg::parser!(pub grammar dank() for str {
    /// Parses whitespace
    rule _() = [' ' | '\t']*
    /// Parses newlines
    rule __() = ['\n' | '\r']*
    /// Parses whitespace or newlines
    rule ___() = [' ' | '\t' | '\n' | '\r']*

    // TODO: parse code inside comments
    rule line_comment_group() -> Vec<&'input str>
        = ("//" t:$([ch if ch != '\n']*) "\n" "\r"? {t})+

    /// Parses a single-line comment
    rule line_comment() -> LineComment<'input>
        = g:line_comment_group() s:stmt() {
              LineComment {
                  body: CommentBody::Text(g.join("\n").into()),
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
                   body: CommentBody::Text(g.join("\n").into()),
                   stmt: None,
               }
           }

    pub rule binding() -> Stmt<'input>
    =  start:position!() "let" ___ i:ident() ___ initializer:("=" ___ i:expr() {i})? end:position!() {
        Stmt { kind: StmtKind::LetDecl(i.into(), initializer.map(Box::new)), span: start..end }
    }

    pub rule stmt() -> Stmt<'input>
         = p:print() ";" {p}
         / b:binding() ";" {b}

    pub rule expr() -> Expr<'input> = operators()

    pub rule operators() -> Expr<'input>
    = precedence! {
        x:(@) _ "&&" _ y:@ { op_node!(x, y, BinOpKind::And) }
        x:(@) _ "||" _ y:@ { op_node!(x, y, BinOpKind::Or) }
        --
        x:(@) _ "+" _ y:@ { op_node!(x,y, BinOpKind::Add) }
        x:(@) _ "-" _ y:@ { op_node!(x,y, BinOpKind::Sub) }
        --
        x:(@) _ "*" _ y:@ { op_node!(x, y, BinOpKind::Mul) }
        x:(@) _ "/" _ y:@ { op_node!(x, y, BinOpKind::Div) }
        --
        "-" x:(@) { op_node!(x, UnOpKind::Neg) }
        "!" x:(@) { op_node!(x, UnOpKind::Not) }
        --
        p:primary() { p }
    }

    pub rule primary() -> Expr<'input>
        = start:position!() i:ident() end:position!() { Expr { kind: ExprKind::Variable(i.into()), span: start..end } }
        / start:position!() l:literal() end:position!() { Expr { kind: ExprKind::Literal(l), span: start..end }}
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
        / "true"  { Value::Bool(true) }
        / "false" { Value::Bool(false) }
        / "null" { Value::Null }

    // TODO: accept expressions here
    rule print() -> Stmt<'input>
         = start:position!() "print" _ args:(expr() ** ",") end:position!()
         { Stmt { kind: StmtKind::Print(args), span: Span { start, end } } }

    /// Prase a multi-line header comment (allowed only at the top of the file)
    rule header_comment() -> HeaderComment<'input>
        = "/*" name:ident_with_whitespace() ['\n' | '\r']?

        "*/" ___ {
            HeaderComment {
                name: name.into_iter().filter(|x| !x.is_empty()).collect(),
                body: vec![]
            }
        }

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
        = i:$(ident_chars()*) ** [' ' | '\t'] { i }

    /// Parses the first character of an identifier, which cannot contain numbers
    rule ident_start() -> &'input str = s:$(['a'..='z'|'A'..='Z'|'_']) { s }

    /// Parses any alphanumeric characters as part of an identifier
    rule ident_chars() -> &'input str = s:$(['a'..='z'|'A'..='Z'|'0'..='9'|'_']) { s }

    /// Parses an entire identifier, ensuring no reserved keywords are used
    rule ident() -> &'input str
        = i:quiet!{ $(!reserved() ident_start() ident_chars()*) } { i }

    rule line() -> LineComment<'input>
        = _ l:line_comment() __ { l }
        // / _ e:(export()) __ { Some(e) }
        // / _ s:(decl()) __ { Some(s) }

    /// Parses a file.
    pub rule file() -> FileAst<'input>
        = ___? header_comments:(header_comment()*)
          lines:(line()*)
          ___?
        {
            FileAst { header_comments, code: Ast { statements: lines } }
        }
});

#[cfg(test)]
pub mod tests {
    use ast2str::AstToStr;
    use peg::str::LineCol;
    use pretty_assertions::assert_eq;

    use super::*;

    macro_rules! test_eq {
        ($input:expr, $expected:expr) => {
            assert_eq!(
                dank::file($input)
                    .unwrap()
                    .ast_to_str()
                    .replace(|c| ['├', '─', '│', '╰', '✕', '↓'].contains(&c), " ")
                    .as_display(),
                $expected.as_display()
            )
        };
    }

    #[test]
    fn header_comments() {
        let test = r#"

        /* this is another header comment */ 
        /* this_is_a_header_comment */

        "#;

        assert_eq!(
            dank::file(test).unwrap(),
            FileAst {
                header_comments: vec![
                    HeaderComment {
                        name: vec!["this", "is", "another", "header", "comment"],
                        body: vec![],
                    },
                    HeaderComment {
                        name: vec!["this_is_a_header_comment"],
                        body: vec![],
                    },
                ],
                code: Ast { statements: vec![] }
            }
        )
    }

    #[test]
    fn lone_comment() {
        let test = r#"
        
            // test comment

        "#;
        assert_eq!(
            dank::file(test).unwrap(),
            FileAst {
                header_comments: vec![],
                code: Ast {
                    statements: vec![LineComment {
                        body: CommentBody::Text(" test comment".into()),
                        stmt: None,
                    }],
                }
            }
        )
    }

    #[test]
    fn attached_comment() {
        let test = r#"
        
        // attached comment
        print variable;

        "#;
        assert_eq!(
            dank::file(test).unwrap(),
            FileAst {
                header_comments: vec![],
                code: Ast {
                    statements: vec![LineComment {
                        body: CommentBody::Text(" attached comment".into()),
                        stmt: Some(Stmt {
                            kind: StmtKind::Print(vec![Expr {
                                kind: ExprKind::Variable("variable".into()),
                                span: 52..60,
                            },]),
                            span: 46..60,
                        }),
                    }],
                }
            }
        )
    }

    #[test]
    fn arithmetics() {
        let test = r#"
        let a = 1 + 2 * 3 / 4;
        let b = -false;
        let c = null;
        let d = a && b || (!c);
        "#;
        test_eq!(
            test,
            r#"FileAst
  header_comments= 
  code: Ast
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
                name: "c""#
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
            r#"FileAst
  header_comments= 
  code: Ast
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
