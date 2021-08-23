use core::ops::Range;

use cranelift::prelude::{AbiParam, types};
use std::collections::HashMap;
use std::mem::transmute;

use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Linkage, Module};
use target_lexicon::Triple;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Int,
    Float,
    Number,
    String,
    Bool,
    Void,
}

#[derive(Clone)]
pub struct Parameter {
    pub name: String,
    pub typename: Type,
}

#[derive(Clone)]
pub enum Expr {
    NoExpr,
    Number(i64, Range<usize>),
    String(String, Range<usize>),
    Var(String, Range<usize>),
    Parameter(Parameter, Range<usize>),
    Block(Vec<Expr>, Range<usize>),
    Function(/* name */ String, /* params */ Vec<(Parameter, Range<usize>)>, /* Ret */ Type, /* stmts */ Box<Expr>, Range<usize>),

    Else(/* block */ Box<Expr>, Range<usize>),
    Elif(/* block */ Box<Expr>, Box<Expr>, Range<usize>),
    If(Box<Expr>, /* block */ Box<Expr>, /*elifs*/ Option<Vec<Expr>>, Option<Box<Expr>>, Range<usize>),

    Call(String, Vec<Expr>, Range<usize>, Range<usize>),

    While(Box<Expr>, /* block */ Box<Expr>, Range<usize>),
    List(Vec<Expr>, Range<usize>),

    Assign(String, Box<Expr>, Range<usize>),
    Reassign(String, Box<Expr>, Range<usize>),

    // is
    Equality(Box<Expr>, Box<Expr>),

    // !is
    NotEqual(Box<Expr>, Box<Expr>),

    // >
    GreaterThan(Box<Expr>, Box<Expr>),

    // <
    LessThan(Box<Expr>, Box<Expr>),

    // >=
    GreaterThanEqual(Box<Expr>, Box<Expr>),

    // <=
    LessThanEqual(Box<Expr>, Box<Expr>),

    // +
    Addition(Box<Expr>, Box<Expr>),

    // -
    Subtraction(Box<Expr>, Box<Expr>),

    // *
    Multiplication(Box<Expr>, Box<Expr>),

    // /
    Division(Box<Expr>, Box<Expr>),

    // Exponent
    Power(Box<Expr>, Box<Expr>),
}

impl From<String> for Type {
    fn from(input: String) -> Type {
        match input.as_str() {
            "Int" => Type::Int,
            "String" => Type::String,
            "Void" => Type::Void,
            "Bool" => Type::Bool,
            _ => Type::Int
        }
    }
}

impl From<Type> for types::Type {
  #[inline]
  fn from(ty: Type) -> Self {
      match ty {
          Type::Int =>   types::I64,
          Type::Float => types::F32,
          Type::String => todo!(),
          Type::Bool =>  types::B1,
          _ => todo!()
      }
  }
}

impl From<Type> for AbiParam {
    #[inline]
    fn from(ty: Type) -> Self {
        match ty {
          Type::Int =>  AbiParam::new(types::I64),
          Type::Float => AbiParam::new(types::F32),
          Type::String => AbiParam::new(cranelift::prelude::Type::triple_pointer_type(&target_lexicon::Triple::host())),
          Type::Bool => AbiParam::new(types::B1),
          _ => todo!()
        }
    }
}

impl Type {
    pub fn as_str(&mut self) -> &str {
        match self {
            Type::Int => "Int",
            Type::Float => "Float",
            Type::Number => "Number",
            Type::String => "String",
            Type::Bool => "Bool",
            Type::Void => "Void",
        }
    }
}


peg::parser!(pub grammar parser() for str {


  pub rule function() -> Expr 
      = "//" start:position!() _ function_name:identifier() end:position!() _ "is" _ "a" _ "function." _ 
        "//" _ "Params:" 
        params:parameters() _ 
        ret:return_stmt() _ 
        "fn" _ body:block()
      {
          Expr::Function(function_name, params, ret, Box::new(body), start..end)
      }  
  pub rule block() -> Expr 
      = start:position!() "{" _ stmts:statements() _ "}" end:position!()
      {
        Expr::Block(stmts, start..end)
      }

  pub rule assignment() -> Expr 
      = start:position!() "var" _ i:identifier() _ "=" _ e:binary_op() ";" end:position!()
      { Expr::Assign(i, Box::new(e), start..end) }

  pub rule reassignment() -> Expr 
      = start:position!() i:identifier() _ "=" _ e:binary_op() ";" end:position!()
      { Expr::Reassign(i, Box::new(e), start..end) }

  pub rule parameter_decl() -> (Parameter, Range<usize>)
      = _ "//" _  "'" param_name:identifier() "'" _ "is" _ "of" _ "type" _ 
      start:position!() ty:identifier() end:position!() "."
      {
          (Parameter {
              name: param_name,
              typename: Type::from(ty)
          }, start..end)
      }


  pub rule list() -> Expr 
      = start:position!() "[" _ values:((_ expr:expression() _ {expr}) ** ",") _ "]" end:position!()
      {Expr::List(values, start..end)}
    
  pub rule expression() -> Expr
      = 
      while_expr()
      / if_expr()
      / reassignment()
      / while_expr()
      / binary_op()


  pub rule statements() -> Vec<Expr>
      = stmt:(expression()*) { stmt }
  
  pub rule else_expr() -> Expr
      = start:position!() "else" _ body:block() end:position!()
      {
        Expr::Else(Box::new(body), start..end)
      }
  
  pub rule elif() -> Expr 
      = start:position!() "elif" _ expr:binary_op() _ body:block() end:position!()
      {
        Expr::Elif(Box::new(expr), Box::new(body), start..end)
      }
  
  pub rule if_expr() -> Expr
      = start:position!() "if" _ expr:binary_op() _ if_body:block() end:position!() _ elif_body:(elif()*) _ else_body:(else_expr()?) 

      {
        Expr::If(Box::new(expr), Box::new(if_body), 
          if elif_body.len() > 0 {
            Some(elif_body)
          } else {
            None
          }, 
          match else_body {
            Some(v) => Some(Box::new(v)),
            None => None
          }, start..end)
      }

  pub rule while_expr() -> Expr 
      = start:position!() "while" _ cond:binary_op() _ stmts:block() end:position!() 
      {
        Expr::While(Box::new(cond), Box::new(stmts), start..end)
      }

  pub rule binary_op() -> Expr = precedence! {
    lhs:@ _ "is" _ start:position!() rhs:(@) { Expr::Equality(Box::new(lhs), Box::new(rhs)) }
    lhs:@ _ "is" _ "not" _ rhs:(@) { Expr::NotEqual(Box::new(lhs), Box::new(rhs)) }
    lhs:@ _ "<" _ rhs:(@) { Expr::LessThan(Box::new(lhs), Box::new(rhs)) }
    lhs:@ _ ">" _ rhs:(@) { Expr::GreaterThan(Box::new(lhs), Box::new(rhs)) }
    lhs:@ _ "<=" _ rhs:(@) { Expr::GreaterThanEqual(Box::new(lhs), Box::new(rhs)) }
    lhs:@ _ ">=" _ rhs:(@) { Expr::LessThanEqual(Box::new(lhs), Box::new(rhs)) }
    --
    lhs:@ _ "+" _ rhs:(@) { Expr::Addition(Box::new(lhs), Box::new(rhs)) }
    lhs:@ _ "-" _ rhs:(@) { Expr::Subtraction(Box::new(lhs), Box::new(rhs)) }
    --
    lhs:@ _ "*" _ rhs:(@) { Expr::Multiplication(Box::new(lhs), Box::new(rhs)) }
    lhs:@ _ "/" _ rhs:(@) { Expr::Division(Box::new(lhs), Box::new(rhs)) }
    --
    lhs:@ _ "^" _ rhs:(@) { Expr::Power(Box::new(lhs), Box::new(rhs)) }
    --

    start:position!() func_name:identifier() end:position!() _ "(" s2:position!() values:((_ expr:expression() _ {expr}) ** ",") e2:position!() _ ")"
    { Expr::Call(func_name, values, start..end, s2..e2) }

    "(" _ expr:expression() _ ")" { expr }
    lit:literal() { lit }
    li:list() { li }
    start:position!() "" i:identifier() "" end:position!() { Expr::Var(i, start..end) }
  }

  pub rule parameters() -> Vec<(Parameter, Range<usize>)>
      = params:(parameter_decl()*) { params }

  pub rule return_stmt() -> Type 
      = "//" _ "Returns:" _ ty:identifier() {
          Type::from(ty)
      }

  #[cache]
  pub rule identifier() -> String 
    = ident:$(['a'..='z' | 'A'..='Z' | '_']['a'..='z' | 'A'..='Z' | '_' | '0'..='9']*) 
    { ident.to_owned() }

  
  pub rule literal() -> Expr 
    = start:position!() number:$(['0'..='9']+) end:position!() {
      Expr::Number(number.parse().unwrap(), start..end)
    }
    / start:position!() "\"" s:$([^'"'..='"']+) "\"" end:position!() {
      Expr::String(s.to_owned(), start..end)
    }
    

  // Ignore these rules
  #[cache]
  rule _() = quiet!{[' ' | '\t' | '\n' | '\r']*}
});