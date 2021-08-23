use std::ops::Range;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::{Error, SimpleFiles};
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use peg::error::ParseError;
use peg::str::LineCol;

use crate::frontend::{Expr, Type};

impl Expr {
    pub fn expression_range(&self) -> Range<usize> {
        match self {
            Expr::NoExpr => unreachable!(),
            Expr::Number(_, r) => r.to_owned(),
            Expr::String(_, r) => r.to_owned(),
            Expr::Parameter(_, r) => r.to_owned(),
            Expr::Function(_, _, _, _, r) => r.to_owned(),
            Expr::Else(_, r) => r.to_owned(),
            Expr::Elif(_, _, r) => r.to_owned(),
            Expr::If(_, _, _, _, r) => r.to_owned(),
            Expr::Call(_, _, r, _) => r.to_owned(),
            Expr::While(_, _, r) => r.to_owned(),
            Expr::List(_, r) => r.to_owned(),
            Expr::Var(_, r) => r.to_owned(),
            Expr::Assign(_, _, r) => r.to_owned(),
            Expr::Reassign(_, _, r) => r.to_owned(),
            Expr::Block(_, r) => r.to_owned(),
            Expr::Equality(lhs, rhs) => lhs.expression_range().start..rhs.expression_range().end,
            Expr::NotEqual(lhs, rhs) => lhs.expression_range().start..rhs.expression_range().end,
            Expr::GreaterThan(lhs, rhs) => lhs.expression_range().start..rhs.expression_range().end,
            Expr::LessThan(lhs, rhs) => lhs.expression_range().start..rhs.expression_range().end,
            Expr::GreaterThanEqual(lhs, rhs) => lhs.expression_range().start..rhs.expression_range().end,
            Expr::LessThanEqual(lhs, rhs) => lhs.expression_range().start..rhs.expression_range().end,
            Expr::Addition(lhs, rhs) => lhs.expression_range().start..rhs.expression_range().end,
            Expr::Subtraction(lhs, rhs) => lhs.expression_range().start..rhs.expression_range().end,
            Expr::Multiplication(lhs, rhs) => lhs.expression_range().start..rhs.expression_range().end,
            Expr::Division(lhs, rhs) => lhs.expression_range().start..rhs.expression_range().end,
            Expr::Power(lhs, rhs) => lhs.expression_range().start..rhs.expression_range().end,
        }
    }
}

pub enum TypeError {
    // TODO: Make this something useful
    InvaidTypesForOperation(Range<usize>, Range<usize>, Type, Type),
    NotEqualFunctionReturnType(Range<usize>, Range<usize>),
    FunctionDoesNotExist(String, Range<usize>),
    IncorrectNumberOfFunctionArguments(Range<usize>, usize, usize),
    IncorrectTypeValueForArgument(Range<usize>, Range<usize>, Type, Type),
    ExpectedExpression(Range<usize>, Type),
    NotDefined(Range<usize>),
}

pub fn report_type_error(error: TypeError, filename: &str, source: &str) {
    let mut file_handler = SimpleFiles::new();
    let file_id = file_handler.add(filename, source);

    let err: Diagnostic<usize>;
    match error {
        TypeError::InvaidTypesForOperation(s1, s2, mut expected, mut got) => {
            err = Diagnostic::error()
                .with_message("Both sides of the expression must be the same type")
                .with_labels(vec![
                    Label::secondary(file_id, s1).with_message(["is of type `", expected.as_str(), "`"].join("")),
                    Label::secondary(file_id, s2).with_message(["is of type `", got.as_str(), "`"].join("")),
                ])
                .with_notes(vec![
                    "Both expression should be the same type".to_owned(),
                ])
        }
        TypeError::NotEqualFunctionReturnType(s1, s2) => todo!(),
        TypeError::FunctionDoesNotExist(name, loc) => {
            err = Diagnostic::error()
                .with_message(["Function with the name '", name.as_str(), "'", " does not exist"].join(""))
                .with_labels(vec![
                    Label::primary(file_id, loc)
                ]);
        }
        TypeError::IncorrectNumberOfFunctionArguments(s1, expected_args, got) => {
            err = Diagnostic::error()
                .with_message("Function call has incorrect number of arguments")
                .with_labels(vec![
                    Label::secondary(file_id, s1).with_message(["Expected ", expected_args.to_string().as_str(), " arguments"].join(""))
                ])
                .with_notes(vec![
                    ["expected ", expected_args.to_string().as_str(), " arguments but got ", got.to_string().as_str()].join("")
                ])
        }
        TypeError::IncorrectTypeValueForArgument(defined, error, mut expected, mut got) => {
            err = Diagnostic::error()
                .with_message("Incorrect type for argument")
                .with_labels(vec![
                    //Label::primary(file_id, s1),
                    Label::secondary(file_id, defined).with_message("Type defined here"),
                    Label::primary(file_id, error).with_message(["Expected type `", expected.as_str(), "` but got type `", got.as_str(), "`"].join("")),
                ]);
        }
        TypeError::ExpectedExpression(loc, mut ty) => {
            err = Diagnostic::error()
                .with_message("Invalid expression")
                .with_labels(vec![
                    Label::primary(file_id, loc).with_message(["Expected type `", ty.as_str(), "`"].join(""))
                ]);
        }
        TypeError::NotDefined(loc) => {
            err = Diagnostic::error()
                .with_message("Value has not been defined")
                .with_labels(vec![
                    Label::primary(file_id, loc).with_message("Expected variable to be defined")
                ]);
        }
    }
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();
    let res = codespan_reporting::term::emit(&mut writer.lock(), &config, &file_handler, &err);
    match res {
        Ok(_) => {}
        Err(e) => println!("{}", e.to_string())
    }
    std::process::exit(1);
}

pub fn report_parse_error(filename: &str, source: &str, err: ParseError<LineCol>) -> Result<(), Error> {
    let mut file_handler = SimpleFiles::new();

    let file_id = file_handler.add(filename, source);
    let mut expected = "Expected ".to_string();
    expected.push_str(err.expected.to_string().as_str());
    let err = Diagnostic::error()
        .with_message(expected.to_string())
        .with_labels(vec![
            Label::primary(file_id, err.location.offset..err.location.offset)
        ]);
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();
    codespan_reporting::term::emit(&mut writer.lock(), &config, &file_handler, &err)?;
    std::process::exit(1);
    Ok(())
}