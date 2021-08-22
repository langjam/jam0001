mod strings;

use nom::IResult;

use crate::ast::{
    Ast,
    FieldType as FieldTypeAst,
    FieldValue as FieldValueAst,
    Script,
    StructType as StructTypeAst,
    StructTypeData,
    TypeAst,
    ValueAst,
};

pub fn parse(name: &str, input: &str) -> Result<Script, String> {
    use nom::branch::alt;
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
    F: Fn(&'a str) -> IResult<&'a str, O, E>,
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

pub fn struct_value(input: &str) -> IResult<&str, ValueAst> {
    use nom::character::complete::char;
    use nom::combinator::{map, opt};
    use nom::multi::many0;
    use nom::sequence::{delimited, pair, terminated};

    use crate::ast::{Comment, StructValue, StructValueData};

    map(
        pair(
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
        ),
        |(name, fields)| {
            ValueAst::Struct(StructValue::from(StructValueData {
                name,
                fields,
                comment: Comment { lines: vec![] }, // TODO
            }))
        }
    )(input)
}

pub fn field_value(input: &str) -> IResult<&str, FieldValueAst> {
    use nom::character::complete::char;
    use nom::combinator::map;
    use nom::sequence::{pair, terminated};

    use crate::ast::Comment;

    map(
        pair(
            terminated(
                map(identifier, ToString::to_string),
                ws(char(':')),
            ),
            parse_value,
        ),
        |(name, value)| {
            FieldValueAst {
                name,
                value,
                comment: Comment { lines: vec![] }, // TODO
            }
        }
    )(input)
}

pub fn parse_value(input: &str) -> IResult<&str, ValueAst> {
    use nom::branch::alt;
    use nom::character::complete::{char, one_of};
    use nom::sequence::preceded;

    let first = alt((
        number,
        text,
        struct_value,
        reference,
    ))(input)?;

    let mut access_following = ws::<_, _, ()>(one_of(".!"));
    let mut parse_field_access = preceded(
        ws(char('.')),
        identifier,
    );
    let mut parse_comment_access = preceded(
        ws(char('!')),
        identifier,
    );

    let mut prior = first;

    while access_following(prior.0).is_ok() {
        if let Ok(second) = parse_field_access(prior.0) {
            prior = (
                second.0, 
                ValueAst::FieldAccess(Box::new(prior.1), second.1.to_string()),
            );
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
    use nom::sequence::{pair, terminated};

    let parse_value = map(
        pair(
            terminated(
                identifier,
                ws(char('=')),
            ),
            parse_value,
        ),
        |(name, value)| Ast::ValueDef(name.to_string(), value),
    );

    let parse_type = map(
        parse_struct_type,
        |struct_type| Ast::TypeDef(struct_type),
    );

    terminated(
        alt((
            parse_value,
            parse_type,
        )),
        ws(char(';')),
    )(input)
}

pub fn parse_struct_type(input: &str) -> IResult<&str, StructTypeAst> {
    use nom::bytes::complete::tag;
    use nom::character::complete::{multispace1, char};
    use nom::combinator::{map, opt};
    use nom::multi::many0;
    use nom::sequence::{pair, preceded, terminated, delimited};

    use crate::ast::Comment;

    map(
        pair(
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
        ),
        |(name, fields)| StructTypeAst::from(StructTypeData {
            name,
            fields,
            comment: Comment { lines: vec![] }
        }),
    )(input)
}

pub fn field_type(input: &str) -> IResult<&str, FieldTypeAst> {
    use nom::character::complete::char;
    use nom::combinator::map;
    use nom::sequence::{pair, terminated};

    use crate::ast::Comment;

    map(
        pair(
            terminated(
                map(identifier, ToString::to_string),
                ws(char(':')),
            ),
            parse_type,
        ),
        |(name, ty)| {
            FieldTypeAst {
                name,
                ty,
                comment: Comment { lines: vec![] }, // TODO
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
    fn numbers() {
        let res = parse_value("42").expect("parse").1;
        assert!(matches!(res, ValueAst::Number(42)));
        let res = parse_value("-42").expect("parse").1;
        assert!(matches!(res, ValueAst::Number(-42)));
        let res = parse_value("0").expect("parse").1;
        assert!(matches!(res, ValueAst::Number(0)));
    }

    #[test]
    fn texts() {
        let res = parse_value("\"foobar\"").expect("parse").1;
        match res {
            ValueAst::Text(s) => assert_eq!(s, "foobar"),
            _ => assert!(false, "invalid result type"),
        }
    }

    #[test]
    fn references() {
        let res = parse_value("foobar").expect("parse").1;
        match res {
            ValueAst::Reference(n) => assert_eq!(n, "foobar"),
            _ => assert!(false, "invalid result type"),
        }
    }

    #[test]
    fn struct_values() {
        let res = parse_value("{\n\tfoo: 42,\n}\n").expect("parse").1;
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
}
