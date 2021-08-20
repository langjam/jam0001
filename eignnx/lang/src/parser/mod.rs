use std::path::Path;

use nom::{
    branch::alt,
    bytes::complete::{tag, take_till},
    character::complete::{alpha1, alphanumeric1, multispace0},
    combinator::{recognize, verify},
    error::ParseError,
    multi::many0,
    sequence::{delimited, pair, tuple},
    IResult, Parser,
};
use nom_supreme::final_parser;

use crate::ast::{Loc, Tm, Ty};

pub fn parse_from_file(path: impl AsRef<Path>) -> Result<Tm, String> {
    let src = std::fs::read_to_string(path).map_err(|e| format!("{}", e))?;
    parse(&src.trim_end())
}

pub fn parse(i: &str) -> Result<Tm, String> {
    let res: Result<Tm, nom::error::Error<&str>> =
        final_parser::final_parser(ws::allowed::before(expr))(i);
    match res {
        Ok(tm) => Ok(tm),
        Err(e) => Err(format!("{}", e)),
    }
}

fn expr(i: &str) -> IResult<&str, Tm> {
    alt((var_expr, text_expr, lam_expr, app_expr))(i)
}

fn var_expr(i: &str) -> IResult<&str, Tm> {
    identifier.map(|s| Tm::Var(Loc, s.to_string())).parse(i)
}

fn text_expr(i: &str) -> IResult<&str, Tm> {
    let (i, text) = parse_string(i)?;
    Ok((i, Tm::Text(Loc, text)))
}

fn parse_string(i: &str) -> IResult<&str, String> {
    delimited(tag("\""), take_till(|ch| ch == '"'), tag("\""))(i).map(|(i, s)| (i, s.to_string()))
}

fn lam_expr(i: &str) -> IResult<&str, Tm> {
    bracketed(tuple((
        tag("fn"),
        ws::required::before(identifier),
        ws::allowed::before(tag(":")),
        ws::allowed::before(ty),
        ws::allowed::before(tag("->")),
        ws::allowed::before(expr),
    )))
    .map(|(_, param, _, ty, _, body)| Tm::Lam(Loc, param.into(), ty, Box::new(body)))
    .parse(i)
}

fn ty(i: &str) -> IResult<&str, Ty> {
    alt((arr_ty, forall_ty, var_ty))(i)
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

fn forall_ty(i: &str) -> IResult<&str, Ty> {
    bracketed(tuple((
        tag("any"),
        ws::required::before(identifier),
        ws::required::before(ty),
    )))
    .map(|(_, var, ty)| Ty::ForAll(var.to_string(), Box::new(ty)))
    .parse(i)
}

fn var_ty(i: &str) -> IResult<&str, Ty> {
    identifier.map(|s| Ty::Var(s.to_string())).parse(i)
}

fn app_expr(i: &str) -> IResult<&str, Tm> {
    bracketed(tuple((expr, ws::allowed::before(expr))))
        .map(|(func, arg)| Tm::App(Loc, Box::new(func), Box::new(arg)))
        .parse(i)
}

fn bracketed<'i, O, E>(
    mut p: impl FnMut(&'i str) -> IResult<&'i str, O, E>,
) -> impl FnMut(&'i str) -> IResult<&'i str, O, E>
where
    E: ParseError<&'i str>,
{
    move |i: &str| {
        let (i, _) = tag("[")(i)?;
        let (i, _) = multispace0(i)?;
        let (i, o) = p(i)?;
        let (i, _) = multispace0(i)?;
        let (i, _) = tag("]")(i)?;
        Ok((i, o))
    }
}

const KEYWORDS: &'static [&'static str] = &["fn", "any"];

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
