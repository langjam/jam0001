#![allow(clippy::nonstandard_macro_braces)]
use crate::ast::*;

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



    pub rule stmt() -> Stmt<'input>
         = p:print() ";" {p}

    // TODO: accept expressions here
    rule print() -> Stmt<'input>
         = start:position!() "print" _ ident() end:position!()
         { Stmt { kind: StmtKind::Print(vec![]), span: Span { start, end } } }

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
            FileAst { header_comments, code: Ast { statements: vec![] } }
        }
});

#[cfg(test)]
pub mod tests {
    use peg::str::LineCol;
    use pretty_assertions::assert_eq;

    use super::*;

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
                code: Ast { statements: vec![] }
            }
        )
    }

    #[test]
    fn attached_comment() {
        let test = r#"print variable;"#;
        assert_eq!(
            dank::file(test).unwrap(),
            FileAst {
                header_comments: vec![],
                code: Ast { statements: vec![] }
            }
        )
    }
}
