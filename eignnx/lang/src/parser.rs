use std::path::Path;

use nom::{
    branch::alt,
    bytes::complete::{tag, take_till},
    character::complete::{alpha1, alphanumeric1, multispace0},
    combinator::{recognize, verify},
    error::ParseError,
    multi::{fold_many1, many0, many1, separated_list0},
    sequence::{delimited, pair, tuple},
    IResult, Parser,
};
use nom_supreme::{final_parser::final_parser, ParserExt};

use crate::ast::{Loc, Op, Tm, Ty};

const KEYWORDS: &'static [&'static str] = &[
    "fn", "any", "def", "true", "false", "and", "or", "lt", "lte", "gt", "gte", "nat_eq", "text_eq",
];

pub fn parse_from_file(path: impl AsRef<Path>) -> Result<Tm, String> {
    let src = std::fs::read_to_string(&path).map_err(|e| format!("{}", e))?;
    parse(&src.trim_end())
}

pub fn parse(i: &str) -> Result<Tm, String> {
    let res: Result<_, nom::error::Error<&str>> =
        final_parser(ws::allowed::before(top_lvl_block_tm))(i);
    match res {
        Ok(tm) => Ok(tm),
        Err(e) => Err(format!("{}", e)),
    }
}

fn top_lvl_block_tm<'i>(i: &'i str) -> IResult<&'i str, Tm> {
    many1(ws::allowed::before(tm).terminated(ws::allowed::before(tag(";;"))))
        .map(|tms| Tm::Block(Loc, tms))
        .parse(i)
}

fn tm(i: &str) -> IResult<&str, Tm> {
    alt((nested_ops, tm_first))(i)
}

fn nested_ops(i: &str) -> IResult<&str, Tm> {
    let (i, first) = tm_first(i)?;
    fold_many1(
        tuple((ws::allowed::before(op), ws::allowed::before(tm))),
        (vec![Box::new(first)], vec![]),
        |(mut tms, mut ops), (op, tm)| {
            tms.push(Box::new(tm));
            ops.push(op);
            (tms, ops)
        },
    )
    .map(|x| todo!())
    .parse(i)
}

fn op(i: &str) -> IResult<&str, Op> {
    alt((
        tag("+++").value(Op::SpaceConcat),
        tag("++").value(Op::Concat),
        tag("+").value(Op::Add),
        tag("-").value(Op::Sub),
        tag("*").value(Op::Mul),
        tag("/").value(Op::Div),
        tag("$").value(Op::Apply),
        tag("and").value(Op::And),
        tag("or").value(Op::Or),
        tag("lt").value(Op::Lt),
        tag("lte").value(Op::Lte),
        tag("gt").value(Op::Gt),
        tag("gte").value(Op::Gte),
        tag("nat_eq").value(Op::NatEq),
        tag("text_eq").value(Op::TextEq),
    ))(i)
}

#[test]
fn test_op() {
    let (_, actual) = tm("this $ that $ the_other").unwrap();
    let expected = Tm::Op(
        Loc,
        Box::new(Tm::Op(
            Loc,
            Box::new(Tm::Var(Loc, "this".to_string())),
            Op::Apply,
            Box::new(Tm::Var(Loc, "that".to_string())),
        )),
        Op::Apply,
        Box::new(Tm::Var(Loc, "the_other".to_string())),
    );
    assert_eq!(actual, expected);
}

fn tm_first(i: &str) -> IResult<&str, Tm> {
    alt((
        paren_tm, var_tm, text_tm, nat_tm, bool_tm, lam_tm, app_tm, block_tm, def_tm,
    ))(i)
}

fn paren_tm(i: &str) -> IResult<&str, Tm> {
    delimited(
        tag("("),
        tm.preceded_by(multispace0).terminated(multispace0),
        tag(")"),
    )(i)
}

fn var_tm(i: &str) -> IResult<&str, Tm> {
    identifier.map(|s| Tm::Var(Loc, s.to_string())).parse(i)
}

fn text_tm(i: &str) -> IResult<&str, Tm> {
    parse_string.map(|s| Tm::Text(Loc, s)).parse(i)
}

fn nat_tm(i: &str) -> IResult<&str, Tm> {
    nom::character::complete::digit1
        .map(|u: &str| Tm::Nat(Loc, u.parse().unwrap()))
        .parse(i)
}

fn bool_tm(i: &str) -> IResult<&str, Tm> {
    alt((
        tag("true").value(Tm::Bool(Loc, true)),
        tag("false").value(Tm::Bool(Loc, false)),
    ))(i)
}

fn parse_string(i: &str) -> IResult<&str, String> {
    delimited(tag("\""), take_till(|ch| ch == '"'), tag("\""))
        .map(|s: &str| s.to_string())
        .parse(i)
}

fn lam_tm(i: &str) -> IResult<&str, Tm> {
    tuple((
        tag("fn"),
        ws::required::before(identifier),
        ws::allowed::before(tag(":")),
        ws::allowed::before(ty),
        ws::allowed::before(tag("->")),
        ws::allowed::before(tm),
    ))
    .map(|(_, param, _, ty, _, body)| Tm::Lam(Loc, param.into(), ty, Box::new(body)))
    .parse(i)
}

fn app_tm(i: &str) -> IResult<&str, Tm> {
    bracketed(tuple((tm, ws::allowed::before(tm))))
        .map(|(func, arg)| Tm::App(Loc, Box::new(func), Box::new(arg)))
        .parse(i)
}

fn block_tm(i: &str) -> IResult<&str, Tm> {
    braced(separated_list0(
        ws::allowed::before(tag(";")),
        ws::allowed::before(tm),
    ))
    .map(|tms| Tm::Block(Loc, tms))
    .parse(i)
}

fn def_tm(i: &str) -> IResult<&str, Tm> {
    tuple((
        tag("def"),
        ws::required::before(identifier),
        ws::allowed::before(tag("=")),
        ws::allowed::before(tm),
    ))
    .map(|(_, name, _, body)| Tm::Def(Loc, name.to_string(), Box::new(body)))
    .parse(i)
}

fn ty(i: &str) -> IResult<&str, Ty> {
    alt((arr_ty, var_ty))(i)
}

fn arr_ty(i: &str) -> IResult<&str, Ty> {
    bracketed(tuple((
        ty,
        ws::allowed::before(tag("->")),
        ws::allowed::before(ty),
    )))
    .map(|(x, _, y)| Ty::Arr(Box::new(x), Box::new(y)))
    .parse(i)
}

fn var_ty(i: &str) -> IResult<&str, Ty> {
    identifier.map(|s| Ty::Var(s.to_string())).parse(i)
}

fn bracketed<'i, O, E>(
    mut p: impl Parser<&'i str, O, E>,
) -> impl FnMut(&'i str) -> IResult<&'i str, O, E>
where
    E: ParseError<&'i str>,
{
    move |i: &str| {
        let (i, _) = tag("[")(i)?;
        let (i, _) = multispace0(i)?;
        let (i, o) = p.parse(i)?;
        let (i, _) = multispace0(i)?;
        let (i, _) = tag("]")(i)?;
        Ok((i, o))
    }
}

fn braced<'i, O, E>(
    mut p: impl Parser<&'i str, O, E>,
) -> impl FnMut(&'i str) -> IResult<&'i str, O, E>
where
    E: ParseError<&'i str>,
{
    move |i: &str| {
        let (i, _) = tag("{")(i)?;
        let (i, _) = multispace0(i)?;
        let (i, o) = p.parse(i)?;
        let (i, _) = multispace0(i)?;
        let (i, _) = tag("}")(i)?;
        Ok((i, o))
    }
}

pub fn identifier(input: &str) -> IResult<&str, &str> {
    verify(
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_")))),
        )),
        |sym| !KEYWORDS.contains(sym),
    )(input)
}

mod ws {
    pub mod required {
        use nom::{
            character::complete::multispace1, error::ParseError, sequence::preceded, IResult,
        };

        pub fn before<'i, T, E: ParseError<&'i str>>(
            mut p: impl FnMut(&'i str) -> IResult<&'i str, T, E>,
        ) -> impl FnMut(&'i str) -> IResult<&'i str, T, E> {
            move |i: &str| preceded(multispace1, &mut p)(i)
        }
    }

    pub mod allowed {
        use nom::{
            character::complete::multispace0, error::ParseError, sequence::preceded, IResult,
        };

        pub fn before<'i, T, E: ParseError<&'i str>>(
            mut p: impl FnMut(&'i str) -> IResult<&'i str, T, E>,
        ) -> impl FnMut(&'i str) -> IResult<&'i str, T, E> {
            move |i: &str| preceded(multispace0, &mut p)(i)
        }
    }
}
