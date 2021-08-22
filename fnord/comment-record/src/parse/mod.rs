mod strings;

use nom::IResult;

use crate::ast::{
    Ast,
    Comment as CommentAst,
    FieldType as FieldTypeAst,
    FieldValue as FieldValueAst,
    Script,
    StructType as StructTypeAst,
    StructTypeData,
    StructValue as StructValueAst,
    TypeAst,
    ValueAst,
};

pub fn parse(name: &str, input: &str) -> Result<Script, String> {
    use nom::combinator::all_consuming;
    use nom::multi::many0;

    let statements = all_consuming(many0(ws(parse_ast)))(input)
        .map_err(|e| format!("Error parsing file {}: {}", name, e))?
        .1;

    Ok(Script { name: name.to_string(), statements })
}

/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing whitespace, returning the output of `inner`.
fn ws<'a, F: 'a, O, E: nom::error::ParseError<&'a str>>(inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
    where
    F: FnMut(&'a str) -> IResult<&'a str, O, E>,
{
    use nom::character::complete::multispace0;
    use nom::sequence::delimited;

    delimited(
        multispace0,
        inner,
        multispace0
    )
}

fn number(input: &str) -> IResult<&str, ValueAst> {
    use nom::character::complete::{char, digit1};
    use nom::combinator::{map, map_res, opt};
    use nom::sequence::pair;

    map(
        pair(
            map(
                opt(char('-')),
                |v| v.is_some(),
            ),
            map_res(
                digit1,
                |s: &str| i64::from_str_radix(s, 10),
            ),
        ),
        |(negative, base): (bool, i64)| {
            let mut num = base;
            if negative {
                num *= -1;
            }
            ValueAst::Number(num)
        },
    )(input)
}

fn text(input: &str) -> IResult<&str, ValueAst> {
    use nom::combinator::map;

    map(
        strings::parse_string,
        |s: String| ValueAst::Text(s),
    )(input)
}

pub fn reference(input: &str) -> IResult<&str, ValueAst> {
    use nom::combinator::map;

    map(
        identifier,
        |s: &str| ValueAst::Reference(s.into()),
    )(input)
}

pub fn identifier(input: &str) -> IResult<&str, &str> {
    use nom::branch::alt;
    use nom::bytes::complete::tag;
    use nom::character::complete::{alpha1, alphanumeric1};
    use nom::combinator::recognize;
    use nom::multi::many0;
    use nom::sequence::pair;

    recognize(
        pair(
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_")))),
        ),
    )(input)
}

pub fn parse_comment_line(input: &str) -> IResult<&str, &str> {
    use nom::sequence::{delimited, pair};
    use nom::multi::many0;
    use nom::character::complete::{char, line_ending, one_of, not_line_ending};

    delimited(
        pair(
            char('#'),
            many0(one_of(" \t")),
        ),
        not_line_ending,
        line_ending,
    )(input)
}

pub fn parse_comment(input: &str) -> IResult<&str, CommentAst> {
    use nom::combinator::map;
    use nom::multi::many0;

    map(
        many0(ws(
            map(
                parse_comment_line,
                |line| line.to_string(),
            ),
        )),
        |lines| CommentAst { lines },
    )(input)
}

pub fn struct_value(input: &str) -> IResult<&str, ValueAst> {
    use nom::character::complete::char;
    use nom::combinator::{map, opt};
    use nom::multi::many0;
    use nom::sequence::{delimited, terminated, tuple};

    use crate::ast::{StructValue, StructValueData};

    map(
        tuple((
            parse_comment,
            map(
                opt(identifier),
                |v| v.map(|s| s.to_string()),
            ),
            delimited(
                ws(char('{')),
                many0(
                    terminated(
                        ws(field_value),
                        ws(char(',')),
                    ),
                ),
                ws(char('}')),
            ),
        )),
        |(comment, name, fields)| {
            ValueAst::Struct(StructValue::from(StructValueData {
                name,
                fields,
                comment,
            }))
        }
    )(input)
}

pub fn field_value(input: &str) -> IResult<&str, FieldValueAst> {
    use nom::character::complete::char;
    use nom::combinator::map;
    use nom::sequence::{terminated, tuple};

    map(
        tuple((
            parse_comment,
            terminated(
                map(identifier, ToString::to_string),
                ws(char(':')),
            ),
            parse_value,
        )),
        |(comment, name, value)| {
            FieldValueAst {
                name,
                value,
                comment,
            }
        }
    )(input)
}

pub fn parse_value(input: &str) -> IResult<&str, ValueAst> {
    use nom::branch::alt;
    use nom::bytes::complete::tag;
    use nom::character::complete::{char, one_of};
    use nom::combinator::map;
    use nom::sequence::preceded;

    let first = alt((
        map(ws(tag("()")), |_| ValueAst::Unit),
        number,
        text,
        struct_value,
        reference,
    ))(input)?;

    let mut access_following = ws::<_, _, ()>(one_of(".!+"));
    let mut parse_field_access = preceded(
        ws(char('.')),
        identifier,
    );
    let mut parse_comment_access = preceded(
        ws(char('!')),
        identifier,
    );
    let mut parse_comment_get = ws::<_, _, ()>(tag("!!"));

    let mut parse_add = preceded(
        ws(char('+')),
        parse_value,
    );

    let mut prior = first;

    while access_following(prior.0).is_ok() {
        if let Ok(another) = parse_comment_get(prior.0) {
            prior = (
                another.0,
                ValueAst::CommentGet(Box::new(prior.1)),
            );
        }
        else if let Ok(second) = parse_field_access(prior.0) {
            if let Ok(gnarly) = parse_comment_get(second.0) {
                prior = (
                    gnarly.0,
                    ValueAst::FieldCommentGet(Box::new(prior.1), second.1.to_string()),
                );
            } else {
                prior = (
                    second.0,
                    ValueAst::FieldAccess(Box::new(prior.1), second.1.to_string()),
                );
            }
        }
        else if let Ok(following) = parse_add(prior.0) {
            return Ok((
                following.0,
                ValueAst::Add(Box::new(prior.1), Box::new(following.1)),
            ));
        }
        else {
            let third = parse_comment_access(prior.0)?;
            prior = (
                third.0,
                ValueAst::CommentAccess(Box::new(prior.1), third.1.to_string()),
            );
        }
    }

    Ok(prior)
}

pub fn parse_ast(input: &str) -> IResult<&str, Ast> {
    use nom::branch::alt;
    use nom::character::complete::char;
    use nom::combinator::map;
    use nom::sequence::{terminated, tuple};

    let parse_value_def = map(
        tuple((
            parse_comment,
            terminated(
                identifier,
                ws(char('=')),
            ),
            parse_value,
        )),
        |(comment, name, value)| {
            debug!("GOT {:?} {:?} {:?}", comment, name, value);
            let new_value = match value {
                ValueAst::Struct(s) => {
                    let mut struct_value = s.inner().clone();
                    struct_value.comment = comment; // TODO: extend instead probably!
                    ValueAst::Struct(StructValueAst::from(struct_value))
                }
                _ => value,
            };

            Ast::ValueDef(name.to_string(), new_value)
        },
    );

    let parse_type = map(
        parse_struct_type,
        |struct_type| Ast::TypeDef(struct_type),
    );

    debug!("parsing {}", input);

    let res = terminated(
        alt((
            parse_type,
            parse_value_def,
        )),
        ws(char(';')),
    )(input);

    debug!("result {:?}", res);
    res
}

pub fn parse_struct_type(input: &str) -> IResult<&str, StructTypeAst> {
    use nom::bytes::complete::tag;
    use nom::character::complete::{multispace1, char};
    use nom::combinator::{map, opt};
    use nom::multi::many0;
    use nom::sequence::{delimited, preceded, terminated, tuple};

    map(
        tuple((
            parse_comment,
            preceded(
                terminated(tag("struct"), multispace1),
                opt(map(identifier, ToString::to_string)),
            ),
            delimited(
                ws(char('{')),
                many0(
                    terminated(
                        ws(field_type),
                        ws(char(',')),
                    ),
                ),
                ws(char('}')),
            ),
        )),
        |(comment, name, fields)| StructTypeAst::from(StructTypeData {
            name,
            fields,
            comment,
        }),
    )(input)
}

pub fn field_type(input: &str) -> IResult<&str, FieldTypeAst> {
    use nom::character::complete::char;
    use nom::combinator::map;
    use nom::sequence::{terminated, tuple};

    map(
        tuple((
            parse_comment,
            terminated(
                map(identifier, ToString::to_string),
                ws(char(':')),
            ),
            parse_type,
        )),
        |(comment, name, ty)| {
            FieldTypeAst {
                name,
                ty,
                comment,
            }
        }
    )(input)
}

pub fn parse_type(input: &str) -> IResult<&str, TypeAst> {
    use nom::branch::alt;
    use nom::combinator::map;

    alt((
        map(identifier, |s| TypeAst::Named(s.to_string())),
        map(parse_struct_type, |s| TypeAst::Struct(s)),
    ))(input)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn units() {
        use nom::combinator::all_consuming;

        let res = all_consuming(parse_value)("()").expect("parse").1;
        assert!(matches!(res, ValueAst::Unit));
    }

    #[test]
    fn numbers() {
        use nom::combinator::all_consuming;

        let res = all_consuming(parse_value)("42").expect("parse").1;
        assert!(matches!(res, ValueAst::Number(42)));
        let res = all_consuming(parse_value)("-42").expect("parse").1;
        assert!(matches!(res, ValueAst::Number(-42)));
        let res = all_consuming(parse_value)("0").expect("parse").1;
        assert!(matches!(res, ValueAst::Number(0)));
    }

    #[test]
    fn texts() {
        use nom::combinator::all_consuming;

        let res = all_consuming(parse_value)("\"foobar\"").expect("parse").1;
        match res {
            ValueAst::Text(s) => assert_eq!(s, "foobar"),
            _ => assert!(false, "invalid result type"),
        }
    }

    #[test]
    fn references() {
        use nom::combinator::all_consuming;

        let res = all_consuming(parse_value)("foobar").expect("parse").1;
        match res {
            ValueAst::Reference(n) => assert_eq!(n, "foobar"),
            _ => assert!(false, "invalid result type"),
        }
    }

    #[test]
    fn struct_values() {
        use nom::combinator::all_consuming;

        let res = all_consuming(parse_value)("{\n\tfoo: 42,\n}\n").expect("parse").1;
        match res {
            ValueAst::Struct(s) => {
                assert_eq!(None, s.name);
                assert_eq!(1, s.fields.len());
                assert_eq!("foo", s.fields[0].name);
                assert!(matches!(s.fields[0].value, ValueAst::Number(42)));
            }
            _ => assert!(false, "invalid result type"),
        }
    }

    #[test]
    fn field_accesses() {
        use nom::combinator::all_consuming;

        let res = all_consuming(parse_value)("foo.bar").expect("parse").1;
        match res {
            ValueAst::FieldAccess(source, field) => {
                match *source {
                    ValueAst::Reference(n) => assert_eq!(n, "foo"),
                    _ => assert!(false, "invalid result type"),
                }
                assert_eq!(field, "bar");
            },
            _ => assert!(false, "invalid result type"),
        }
    }

    #[test]
    fn comment_accesses() {
        use nom::combinator::all_consuming;

        let res = all_consuming(parse_value)("foo!bar").expect("parse").1;
        match res {
            ValueAst::CommentAccess(source, field) => {
                match *source {
                    ValueAst::Reference(n) => assert_eq!(n, "foo"),
                    _ => assert!(false, "invalid result type"),
                }
                assert_eq!(field, "bar");
            },
            _ => assert!(false, "invalid result type {:?}", res),
        }
    }

    #[test]
    fn adds() {
        use nom::combinator::all_consuming;

        let res = all_consuming(parse_value)("2 + \"blue\"").expect("parse").1;
        match res {
            ValueAst::Add(left, right) => {
                assert!(matches!(*left, ValueAst::Number(2)));
                assert!(matches!(*right, ValueAst::Text(_)));
            }
            _ => assert!(false, "invalid result type: {:?}", res),
        }
    }

    #[test]
    fn value_defs() {
        use nom::combinator::all_consuming;

        let res = all_consuming(parse_ast)("foo=42;").expect("parse").1;
        match res {
            Ast::ValueDef(name, ValueAst::Number(42)) => assert_eq!("foo", name),
            _ => assert!(false, "invalid result type"),
        }
    }

    #[test]
    fn type_defs() {
        use nom::combinator::all_consuming;

        let res = all_consuming(parse_ast)("struct Color { red: Number, green: Number, blue: Number, };").expect("parse").1;
        match res {
            Ast::TypeDef(s) => {
                assert_eq!(Some("Color".into()), s.name);
                assert_eq!(3, s.fields.len());
                let mut field_names = std::collections::HashSet::<&str>::new();
                for field in &s.fields {
                    match &field.ty {
                        TypeAst::Named(n) => assert_eq!(n, "Number"),
                        _ => assert!(false, "wrong type"),
                    }
                    field_names.insert(&field.name);
                }
                assert!(field_names.contains("red"));
                assert!(field_names.contains("green"));
                assert!(field_names.contains("blue"));
            }
            _ => assert!(false, "invalid result type {:?}", res),
        }
    }

    #[test]
    fn comment_single() {
        use nom::combinator::all_consuming;

        let res = all_consuming(parse_comment_line)("# FOOBAR\n").expect("parse").1;
        assert_eq!(res, "FOOBAR");
    }

    #[test]
    fn comment_group() {
        use nom::combinator::all_consuming;

        let res = all_consuming(parse_comment)("    # FOO\n    # BAR\n").expect("parse").1;

        assert_eq!(2, res.lines.len());
        assert_eq!("FOO", res.lines[0]);
        assert_eq!("BAR", res.lines[1]);
    }

    #[test]
    fn commented_struct_type() {
        use nom::combinator::all_consuming;

        let res = all_consuming(parse_ast)("# usually red\nstruct Color { red: Number, green: Number, blue: Number, };").expect("parse").1;
        match res {
            Ast::TypeDef(s) => {
                assert_eq!(1, s.comment.lines.len());
                assert_eq!("usually red", s.comment.lines[0]);
            }
            _ => assert!(false, "invalid result type {:?}", res),
        }
    }

    #[test]
    fn commented_field_type() {
        use nom::combinator::all_consuming;

        let res = all_consuming(parse_ast)("struct Color {\n    # always zero\n    red: Number,\n    green: Number,\n    blue: Number,\n};").expect("parse").1;
        match res {
            Ast::TypeDef(s) => {
                let red = &s.fields[0];
                assert_eq!(1, red.comment.lines.len());
                assert_eq!("always zero", red.comment.lines[0]);
            }
            _ => assert!(false, "invalid result type {:?}", res),
        }
    }

    #[test]
    fn commented_struct_value() {
        use nom::combinator::all_consuming;

        let res = all_consuming(parse_value)("# my nice struct\n{\n\tfoo: 42,\n}\n").expect("parse").1;
        match res {
            ValueAst::Struct(s) => {
                assert_eq!(1, s.comment.lines.len());
                assert_eq!("my nice struct", s.comment.lines[0]);
            }
            _ => assert!(false, "invalid result type"),
        }
    }

    #[test]
    fn commented_field_value() {
        use nom::combinator::all_consuming;

        let res = all_consuming(parse_value)("{\n\t# my nice field\n\tfoo: 42,\n}\n").expect("parse").1;
        match res {
            ValueAst::Struct(s) => {
                let f = &s.fields[0];
                assert_eq!(1, f.comment.lines.len());
                assert_eq!("my nice field", f.comment.lines[0]);
            }
            _ => assert!(false, "invalid result type"),
        }
    }

    #[test]
    fn commented_value_def() {
        use nom::combinator::all_consuming;

        let res = all_consuming(parse_ast)("# here we are\nfoo = { number: 42, };").expect("parse").1;
        match res {
            Ast::ValueDef(_, v) => {
                match v {
                    ValueAst::Struct(s) => {
                        assert_eq!(1, s.comment.lines.len());
                        assert_eq!("here we are", s.comment.lines[0]);
                    }
                    _ => assert!(false, "invalid result type"),
                }
            }
            _ => assert!(false, "invalid result type"),
        }
    }
}
