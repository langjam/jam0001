use core::fmt;
use std::{
    cell::RefCell, collections::BTreeMap, env, fs, iter::Peekable, rc::Rc,
};

macro_rules! bail {
    ($fmt:literal $(, $arg:expr)* $(,)?) => {
        return Err(format!($fmt $(,$arg)*))
    };
}

#[derive(Debug, Clone, Copy)]
enum TokenType {
    LeftParen,
    RightParen,
    Ident,
    Comment,
    Number,
    String,
    True,
    False,
    Nil,
    Quote,
    Whitespace,
}

#[derive(Debug, Clone)]
struct Token<'s> {
    ty: TokenType,
    lexeme: &'s str,
    pos: (usize, usize),
}

struct Tokenizer<'s> {
    s: &'s str,
    current: usize,
    pos: (usize, usize),
}

impl<'s> Tokenizer<'s> {
    fn new(s: &'s str) -> Self {
        Self {
            s,
            current: 0,
            pos: (1, 1),
        }
    }
}

fn is_ident_char(ch: char, first: bool) -> bool {
    match ch {
        '0'..='9' if !first => true,
        'a'..='z'
        | 'A'..='Z'
        // Based on Scheme
        | '!'
        | '$'
        | '%'
        | '&'
        | '*'
        | '+'
        | '-'
        | '.'
        | '/'
        | ':'
        | '<'
        | '='
        | '>'
        | '?'
        | '@'
        | '^'
        | '_'
        | '~' => true,
        _ => false,
    }
}

impl<'s> Iterator for Tokenizer<'s> {
    type Item = Result<Token<'s>, String>;

    fn next(&mut self) -> Option<Self::Item> {
        let char = self.s[self.current..].chars().next()?;
        let pos = self.pos;
        let mut next_pos = (pos.0, pos.1 + 1);
        let mut len = char.len_utf8();
        let ty = match char {
            '\n' => {
                next_pos = (next_pos.0 + 1, 1);
                TokenType::Whitespace
            }
            ' ' | '\t' | '\r' => TokenType::Whitespace,
            '(' => TokenType::LeftParen,
            ')' => TokenType::RightParen,
            '`' => TokenType::Quote,
            '#' => {
                let next = match self.s[self.current..][len..].chars().next() {
                    Some(c) => c,
                    None => return Some(Err(format!("Unterminated constant"))),
                };
                next_pos.1 += 1;
                len += next.len_utf8();
                match next {
                    't' => TokenType::True,
                    'f' => TokenType::False,
                    _ => return Some(Err(format!("Invalid constant"))),
                }
            }
            ';' => {
                loop {
                    let next = self.s[self.current..][len..].chars().next();
                    match next {
                        Some(next) => {
                            next_pos.1 += 1;
                            len += next.len_utf8();
                            if next == ';' {
                                break;
                            }
                        }
                        None => break,
                    }
                }
                TokenType::Comment
            }
            '0'..='9' => {
                loop {
                    let next = self.s[self.current..][len..].chars().next();
                    if let Some(ch @ '0'..='9') = next {
                        next_pos.1 += 1;
                        len += ch.len_utf8();
                    } else {
                        break;
                    }
                }
                TokenType::Number
            }
            ch if is_ident_char(ch, true) => {
                loop {
                    let next = self.s[self.current..][len..].chars().next()?;
                    if is_ident_char(next, false) {
                        next_pos.1 += 1;
                        len += next.len_utf8();
                    } else {
                        break;
                    }
                }
                if &self.s[self.current..][..len] == "nil" {
                    TokenType::Nil
                } else {
                    TokenType::Ident
                }
            }
            '"' => {
                loop {
                    let next = self.s[self.current..][len..].chars().next()?;
                    next_pos.1 += 1;
                    len += next.len_utf8();
                    if next == '"' {
                        break;
                    }
                }
                TokenType::String
            }
            _ => return Some(Err(format!("Unexpected char: {}", char))),
        };
        let mut lexeme = &self.s[self.current..][..len];
        if matches!(ty, TokenType::String) {
            lexeme = &lexeme[1..lexeme.len() - 1];
        } else if matches!(ty, TokenType::Comment) {
            lexeme = lexeme.trim_matches(';').trim();
        }
        let token = Token { ty, lexeme, pos };

        self.current += len;
        self.pos = next_pos;

        Some(Ok(token))
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Value {
    Bool(bool),
    Number(i64),
    String(String),
    Ident(String),
    Fn(LispFn),
    NativeFn(NativeFn),
    Comment(String),
    List(List),
    Nil,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Bool(true) => write!(f, "#t"),
            Value::Bool(false) => write!(f, "#f"),
            Value::Number(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "{}", s),
            Value::Ident(s) => write!(f, "{}", s),
            Value::Comment(s) => write!(f, "; {} ;", s),
            Value::Fn(_) => write!(f, "<fn>"),
            Value::NativeFn(_) => write!(f, "<fn>"),
            Value::List(l) => {
                write!(f, "(")?;
                let mut join = l.iter().map(ToString::to_string);
                if let Some(first) = join.next() {
                    write!(f, "{}", first)?;
                }
                for v in join {
                    write!(f, ", {}", v)?;
                }
                write!(f, ")")?;
                Ok(())
            }
            Value::Nil => write!(f, "nil"),
        }
    }
}

type BuildIn = fn(&mut Interpreter, &[Value]) -> Result<Value, String>;

#[derive(Clone)]
struct NativeFn(BuildIn);

impl PartialEq for NativeFn {
    fn eq(&self, other: &Self) -> bool {
        self.0 as usize == other.0 as usize
    }
}

impl fmt::Debug for NativeFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<native fn>")
    }
}

#[derive(Debug, Clone, PartialEq)]
struct LispFn {
    params: Vec<String>,
    body: List,
}

type List = Vec<Value>;

struct Parser<I: Iterator> {
    tokens: Peekable<I>,
}

impl<'s, I: Iterator<Item = Token<'s>>> Parser<I> {
    pub fn new(tokens: I) -> Self {
        Self {
            tokens: tokens.peekable(),
        }
    }

    fn parse_value(&mut self) -> Option<Result<Value, String>> {
        let token = self.tokens.peek()?.clone();
        let res = match token.ty {
            TokenType::LeftParen => {
                return Some(self.parse_list()?.map(|x| Value::List(x)));
            }
            TokenType::Quote => {
                let _ = self.tokens.next();
                let value = match self.parse_value()? {
                    Ok(v) => v,
                    Err(e) => return Some(Err(e)),
                };
                return Some(Ok(Value::List(vec![
                    Value::Ident("quote".into()),
                    value,
                ])));
            }
            TokenType::Comment => Ok(Value::Comment(token.lexeme.into())),
            TokenType::Ident => Ok(Value::Ident(token.lexeme.into())),
            TokenType::String => Ok(Value::String(token.lexeme.into())),
            TokenType::True => Ok(Value::Bool(true)),
            TokenType::False => Ok(Value::Bool(false)),
            TokenType::Nil => Ok(Value::Nil),
            TokenType::Number => token
                .lexeme
                .parse()
                .map(Value::Number)
                .map_err(|e| format!("Not a number: {}", e)),
            _ => {
                return Some(Err(format!(
                    "Unexpected token type for value: {:?}",
                    token
                )))
            }
        };
        let _ = self.tokens.next();
        Some(res)
    }

    fn parse_list(&mut self) -> Option<Result<List, String>> {
        let mut list = vec![];
        let left_paren = match self.tokens.next()? {
            token
            @
            Token {
                ty: TokenType::LeftParen,
                ..
            } => token,
            Token { pos, .. } => {
                return Some(Err(format!("Expected left paren at {:?}", pos)))
            }
        };
        loop {
            let token = match self.tokens.peek() {
                None => {
                    return Some(Err(format!(
                        "Unterminated list at: {:?}",
                        left_paren.pos
                    )))
                }
                Some(token) => token,
            };
            if matches!(token.ty, TokenType::RightParen) {
                let _ = self.tokens.next();
                break;
            }
            match self.parse_value()? {
                Ok(value) => list.push(value),
                Err(e) => return Some(Err(e)),
            }
        }
        Some(Ok(list))
    }

    pub fn parse(mut self) -> Result<Vec<List>, String> {
        let mut lists = vec![];

        while self.tokens.peek().is_some() {
            // Consume top level comments
            while let Some(comment) =
                self.tokens.next_if(|x| matches!(x.ty, TokenType::Comment))
            {
                if let Some(expect) = comment.lexeme.strip_prefix("assert: ") {
                    if let Some(list) = self.parse_list() {
                        // Wraps next expr with `assert`
                        lists.push(vec![
                            Value::Ident("assert".into()),
                            Value::List(vec![
                                Value::Ident("==".into()),
                                parse_value(expect)?,
                                Value::List(list?),
                            ]),
                        ]);
                    }
                }
            }
            if let Some(list) = self.parse_list() {
                lists.push(list?);
            }
        }

        Ok(lists)
    }
}

fn parse_value(s: &str) -> Result<Value, String> {
    let tokens = Tokenizer::new(s)
        .filter(|x| {
            x.as_ref()
                .map(|x| !matches!(x.ty, TokenType::Whitespace))
                .unwrap_or(true)
        })
        .collect::<Result<Vec<_>, _>>()?;
    Parser::new(tokens.into_iter())
        .parse_value()
        .ok_or_else(|| format!("Missing value"))?
}

type EnvRef = Rc<RefCell<Env>>;

#[derive(Clone)]
struct Env {
    values: BTreeMap<String, (Option<String>, Value)>,
    prev: Option<EnvRef>,
}

impl Env {
    fn enclose(&mut self) {
        let new_self = Self {
            values: Default::default(),
            prev: None,
        };
        let old_self = std::mem::replace(self, new_self);
        self.prev = Some(Rc::new(RefCell::new(old_self)));
    }

    fn unwrap(&mut self) {
        let prev = self
            .prev
            .as_ref()
            .unwrap()
            .try_borrow_mut()
            .unwrap()
            .clone();
        *self = prev;
    }

    fn def(&mut self, name: &str, doc: Option<String>, value: Value) {
        self.values.insert(name.to_owned(), (doc, value));
    }

    fn set(&mut self, name: &str, value: Value) -> Option<()> {
        if let Some(v) = self.values.get_mut(name) {
            Some(v.1 = value)
        } else if let Some(prev) = &self.prev {
            prev.try_borrow_mut().unwrap().set(name, value)
        } else {
            None
        }
    }

    fn set_doc(&mut self, name: &str, doc: Option<String>) -> Option<()> {
        if let Some(v) = self.values.get_mut(name) {
            Some(v.0 = doc)
        } else if let Some(prev) = &self.prev {
            prev.try_borrow_mut().unwrap().set_doc(name, doc)
        } else {
            None
        }
    }

    fn get(&self, name: &str) -> Option<Value> {
        if let Some(v) = self.values.get(name) {
            Some(v.1.clone())
        } else if let Some(prev) = &self.prev {
            prev.try_borrow().unwrap().get(name)
        } else {
            None
        }
    }

    fn get_doc(&self, name: &str) -> Option<String> {
        if let Some(v) = self.values.get(name) {
            v.0.clone()
        } else if let Some(prev) = &self.prev {
            prev.try_borrow().unwrap().get_doc(name)
        } else {
            None
        }
    }
}

struct Interpreter<'c> {
    code: &'c [List],
    output: String,
    env: Env,
}

const BUILD_INS: &[(&str, Option<&str>, BuildIn)] = &[
    ("+", Some("Sums arguments"), |int, params| {
        params
            .iter()
            .map(|x| int.eval(x))
            .try_fold(0, |acc, x| match x? {
                Value::Number(n) => Ok(acc + n),
                _ => Err(format!("Expected number")),
            })
            .map(Value::Number)
    }),
    ("*", Some("Multiples arguments together"), |int, params| {
        params
            .iter()
            .map(|x| int.eval(x))
            .try_fold(1, |acc, x| match x? {
                Value::Number(n) => Ok(acc * n),
                _ => Err(format!("Expected number")),
            })
            .map(Value::Number)
    }),
    ("print", Some("Prints arguments"), |int, params| {
        for param in params {
            let param = int.eval(param)?;
            int.output += &param.to_string();
        }
        Ok(Value::Nil)
    }),
    (
        "println",
        Some("Prints arguments, then newline"),
        |int, params| {
            for param in params {
                let param = int.eval(param)?;
                int.output += &param.to_string();
            }
            int.output += "\n";
            Ok(Value::Nil)
        },
    ),
    ("define", Some("Defines new variable"), |int, params| {
        let (doc, name, value) = match params {
            [Value::Comment(doc), Value::Ident(name), value] => {
                (Some(doc.clone()), name, value)
            }
            [Value::Ident(name), value] => (None, name, value),
            [_, _] => bail!("Expected identifier"),
            _ => {
                bail!("Expected 2 or 3 arguments, got {}", params.len())
            }
        };
        let value = int.eval(value)?;
        int.env.def(name, doc, value);
        Ok(Value::Nil)
    }),
    (
        "get-doc",
        Some("Returns comment associated with a variable"),
        |int, params| {
            let name = match params {
                [name] => name,
                _ => bail!("Expected 1 argument"),
            };
            let name = match int.eval(name)? {
                Value::Ident(n) => n,
                _ => bail!("Expected identifier"),
            };
            let value = int.env.get_doc(&name);
            Ok(value.map(Value::Comment).unwrap_or(Value::Nil))
        },
    ),
    (
        "set-doc",
        Some("Sets comment to a variable"),
        |int, params| {
            let (name, doc) = match params {
                [name, doc] => (name, Some(doc)),
                [name] => (name, None),
                _ => bail!("Expected 1 or 2 arguments"),
            };
            let name = match int.eval(name)? {
                Value::Ident(n) => n,
                _ => bail!("Expected identifier"),
            };
            let doc = match doc {
                Some(Value::Comment(doc)) => Some(doc.clone()),
                None => None,
                _ => bail!("Expected comment"),
            };
            int.env.set_doc(&name, doc);
            Ok(Value::Nil)
        },
    ),
    (
        "built-ins",
        Some("Returns list of built-ins"),
        |int, params| {
            if !params.is_empty() {
                bail!("Expected 0 arguments");
            }
            let mut global = int.env.clone();
            while let Some(prev) = global.prev {
                global = prev.try_borrow().unwrap().clone();
            }
            let list = global
                .values
                .keys()
                .map(|x| Value::Ident(x.clone()))
                .collect();
            Ok(Value::List(list))
        },
    ),
    (
        "for",
        Some(
            "[syntax: (for [name] [list] [body])] \
    Iterates through list, binding elements to name",
        ),
        |int, params| {
            let (formal, list, body) = match params {
                [Value::Ident(formal), list, body] => Ok((formal, list, body)),
                _ => Err(format!("Expected 3 arguments, got {}", params.len())),
            }?;
            let list = match int.eval(list)? {
                Value::List(list) => list,
                _ => bail!("Expected list"),
            };
            let body = match body {
                Value::List(list) => list.clone(),
                expr => vec![expr.clone()],
            };
            let mut ret = Value::Nil;
            int.env.enclose();
            int.env.def(formal, None, Value::Nil);
            for value in list {
                int.env.set(formal, value);
                ret = int.eval_call(&body)?;
            }
            int.env.unwrap();
            Ok(ret)
        },
    ),
    (
        "quote",
        Some("Prevents immediate evaluation. Returns arguments as literal"),
        |_, params| {
            let value = match params {
                [v] => v.clone(),
                _ => {
                    bail!("Expected 1 argument, got {}", params.len())
                }
            };
            Ok(value)
        },
    ),
    (
        "empty?",
        Some("Checks if argument is empty and return boolean"),
        |int, params| {
            let value = match params {
                [v] => v,
                _ => bail!("Expected 1 argument, got {}", params.len()),
            };
            let res = match int.eval(value)? {
                Value::String(s) => s.is_empty(),
                Value::List(l) => l.is_empty(),
                Value::Comment(c) => c.is_empty(),
                _ => bail!("Expected string, list, or comment"),
            };
            Ok(Value::Bool(res))
        },
    ),
    ("nil?", Some("Is value nil?"), |int, params| {
        let value = match params {
            [v] => v,
            _ => bail!("Expected 1 argument, got {}", params.len()),
        };
        let res = match int.eval(value)? {
            Value::Nil => true,
            _ => false,
        };
        Ok(Value::Bool(res))
    }),
    ("not", Some("Inverts boolean value"), |int, params| {
        let value = match params {
            [v] => v,
            _ => bail!("Expected 1 argument, got {}", params.len()),
        };
        let res = match int.eval(value)? {
            Value::Bool(b) => !b,
            _ => bail!("Expected bool"),
        };
        Ok(Value::Bool(res))
    }),
    ("set!", Some("Sets variable"), |int, params| {
        let (name, value) = match params {
            [Value::Ident(name), value] => (name, value),
            [_, _] => bail!("Expected identifier"),
            _ => bail!("Expected 1 argument, got {}", params.len()),
        };
        let value = int.eval(value)?;
        int.env.set(name, value);
        Ok(Value::Nil)
    }),
    (
        "if",
        Some(
            "Depending on first argument, \
            evaluates and return second or optional third argument",
        ),
        |int, params| {
            let (cond, cons, alt) = match params {
                [cond, cons, alt] => (cond, cons, Some(alt)),
                [cond, cons] => (cond, cons, None),
                _ => {
                    bail!("Expected 2 or 3 arguments, got {}", params.len())
                }
            };
            let cond = match int.eval(cond)? {
                Value::Bool(b) => b,
                _ => bail!("Expected boolean"),
            };
            if cond {
                int.eval(cons)
            } else if let Some(alt) = alt {
                int.eval(alt)
            } else {
                Ok(Value::Nil)
            }
        },
    ),
    (
        "assert",
        Some("Errors if argument is false"),
        |int, params| {
            let arg = match params {
                [arg] => arg,
                _ => bail!("Expected 1 argument, got {}", params.len()),
            };
            let assert = match int.eval(arg)? {
                Value::Bool(b) => b,
                _ => bail!("Expected bool"),
            };
            if !assert {
                bail!("Assertion failed");
            }
            Ok(Value::Nil)
        },
    ),
    (
        "==",
        Some("Compares arguments for equality"),
        |int, params| {
            let mut iter = params.iter().map(|x| int.eval(x));
            if let Some(mut prev) = iter.next().transpose()? {
                for arg in iter {
                    let arg = arg?;
                    if prev != arg {
                        return Ok(Value::Bool(false));
                    }
                    prev = arg;
                }
            }
            Ok(Value::Bool(true))
        },
    ),
    ("lambda", None, |_, params| {
        let (formals, body) = match params {
            [formals, body] => (formals, body),
            _ => bail!("Expected 2 arguments, got {}", params.len()),
        };
        let params = match formals {
            Value::List(l) => l
                .iter()
                .map(|x| match x {
                    Value::Ident(s) => Ok(s.clone()),
                    _ => Err(format!("Formals must be idents")),
                })
                .collect::<Result<_, _>>()?,
            Value::Ident(s) => vec![s.clone()],
            _ => bail!("Expected list or ident"),
        };
        let body = match body {
            Value::List(list) => list.clone(),
            expr => vec![expr.clone()],
        };
        Ok(Value::Fn(LispFn { params, body }))
    }),
];

impl<'c> Interpreter<'c> {
    fn new(code: &'c [List]) -> Self {
        let values = BUILD_INS
            .iter()
            .map(|(n, c, f)| {
                (
                    n.to_string(),
                    (c.map(ToString::to_string), Value::NativeFn(NativeFn(*f))),
                )
            })
            .collect();
        let mut env = Env { values, prev: None };
        env.enclose();
        Self {
            code,
            output: String::new(),
            env,
        }
    }

    fn eval(&mut self, value: &Value) -> Result<Value, String> {
        match value {
            Value::List(list) => self.eval_call(list),
            Value::Ident(ident) => self
                .env
                .get(ident)
                .ok_or_else(|| format!("Couldn't find value for {}", ident)),
            value => Ok(value.clone()),
        }
    }

    fn eval_call(&mut self, list: &List) -> Result<Value, String> {
        let (op, args) =
            list.split_first().ok_or_else(|| format!("Empty list"))?;
        match self.eval(op)? {
            Value::Fn(LispFn { params, body }) => {
                self.env.enclose();
                for (name, value) in params.iter().zip(args) {
                    let value = self.eval(value)?;
                    self.env.def(name, None, value);
                }
                let value = self.eval_call(&body)?;
                self.env.unwrap();
                Ok(value)
            }
            Value::NativeFn(NativeFn(f)) => f(self, args),
            _ => Err(format!("Expected function")),
        }
    }

    fn run(mut self) -> Result<String, String> {
        for list in self.code {
            self.eval_call(list)?;
        }
        Ok(self.output)
    }
}

fn main() {
    let filename = env::args().nth(1).expect("Missing input file");
    let file = fs::read_to_string(&filename)
        .expect(&format!("Can't open file: {}", filename));

    let s = file.replace("\r\n", "\n");
    let tokenizer = Tokenizer::new(&s);
    let tokens = tokenizer
        .filter(|x| {
            x.as_ref()
                .map(|x| !matches!(x.ty, TokenType::Whitespace))
                .unwrap_or(true)
        })
        .collect::<Result<Vec<_>, _>>()
        .unwrap();
    // for token in &tokens {
    //     eprintln!("{:?}", token);
    // }
    let code = Parser::new(tokens.into_iter()).parse().unwrap();
    // for token in &code {
    //     eprintln!("{:?}", token);
    // }
    let interpreter = Interpreter::new(&code);
    let output = interpreter.run().unwrap();
    println!("{}", output);
}
