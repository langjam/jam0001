use core::num;
use std::collections::HashMap;

type DefId = usize;

#[derive(Debug, Clone)]
enum Value {
    String(String),
    Nothing,
}

#[derive(Debug, Clone)]
enum Internal {
    Print,
}

#[derive(Debug, Clone)]
enum Definition {
    Internal(Internal),
    Block {
        body: Block,
        comment: Option<String>,
    },
}

#[derive(Debug, Clone)]
struct Block {
    body: Vec<Expression>,
}

#[derive(Debug, Clone)]
enum JamError {
    NotEnoughComments(String),
    BadParse(String),
    EvalError(String),
}

#[derive(Debug, Clone)]
struct Expression {
    expr: Expr,
    comment: Option<String>,
}

#[derive(Debug, Clone)]
enum Expr {
    Call(DefId),
    PushString(String),
}

#[derive(Debug, Clone)]
enum Token {
    Word(String),
    LCurly,
    RCurly,
    LParen,
    RParen,
    Comment(String),
    Quote(String),
}

#[derive(Debug, Clone)]
struct Jam {
    definitions: Vec<Definition>,
    def_name: HashMap<String, DefId>,
    stack: Vec<Value>,
}

impl Jam {
    fn new() -> Jam {
        let mut s = Self {
            definitions: vec![],
            def_name: HashMap::new(),
            stack: vec![],
        };

        s.definitions.push(Definition::Internal(Internal::Print));
        s.def_name
            .insert("print".to_string(), s.definitions.len() - 1);

        s
    }
    fn eval(&mut self, ast: &Expr) -> Value {
        match ast {
            Expr::Call(def_id) => {
                let definition = &self.definitions[*def_id];

                match definition {
                    Definition::Block { body: block, .. } => {
                        let block = block.clone();
                        let mut output = Value::Nothing;
                        for expression in &block.body {
                            output = self.eval(&expression.expr);
                        }
                        output
                    }
                    Definition::Internal(internal) => match internal {
                        Internal::Print => self.print(),
                    },
                }
            }
            Expr::PushString(s) => {
                self.stack.push(Value::String(s.clone()));
                Value::Nothing
            }
        }
    }

    fn print(&mut self) -> Value {
        if let Some(v) = self.stack.pop() {
            println!("{:?}", v);
        }

        Value::Nothing
    }

    fn lex(&mut self, fname: &str) -> Result<Vec<Token>, JamError> {
        let contents = std::fs::read_to_string(fname).unwrap();
        let mut output = vec![];

        let mut chars = contents.chars().peekable();

        while let Some(c) = chars.peek() {
            let mut c = *c;
            if c == '{' {
                chars.next();
                output.push(Token::LCurly);
            } else if c == '}' {
                chars.next();
                output.push(Token::RCurly);
            } else if c == '(' {
                chars.next();
                output.push(Token::LParen);
            } else if c == ')' {
                chars.next();
                output.push(Token::RParen);
            } else if c == '#' {
                let mut comment = String::new();
                chars.next();
                c = if let Some(c) = chars.peek() { *c } else { c };

                while c != '\n' {
                    chars.next();
                    comment.push(c);

                    c = if let Some(c) = chars.peek() {
                        *c
                    } else {
                        break;
                    }
                }
                output.push(Token::Comment(comment.trim().to_string()));
            } else if c == '"' {
                chars.next();
                c = if let Some(c) = chars.next() {
                    c
                } else {
                    break;
                };

                let mut string = String::new();
                while c != '"' {
                    string.push(c);

                    c = if let Some(c) = chars.next() {
                        c
                    } else {
                        break;
                    }
                }
                output.push(Token::Quote(string));
            } else if c.is_alphabetic() {
                let mut word = String::new();
                while c.is_alphabetic() {
                    chars.next();
                    word.push(c);

                    c = if let Some(c) = chars.peek() {
                        *c
                    } else {
                        break;
                    }
                }
                output.push(Token::Word(word));
            } else {
                chars.next();
            }
        }

        Ok(output)
    }

    fn validate_exprs(
        &mut self,
        def_name: &str,
        expressions: Vec<Expression>,
    ) -> Result<Vec<Expression>, JamError> {
        let num_exprs = expressions.len();
        if expressions.is_empty() {
            return Ok(expressions);
        }
        let minimum_comments = std::cmp::max(num_exprs / 3, 1);

        let mut total_comments = 0;

        for expression in &expressions {
            if expression.comment.is_some() {
                total_comments += 1;
            }
        }

        if total_comments < minimum_comments {
            Err(JamError::NotEnoughComments(format!(
                "{} has {} comments, needs {}",
                def_name, total_comments, minimum_comments
            )))
        } else {
            Ok(expressions)
        }
    }

    fn parse_exprs(
        &mut self,
        def_name: &str,
        tokens: &[Token],
        top_level: bool,
    ) -> Result<Vec<Expression>, JamError> {
        let mut output = vec![];
        let mut idx = 0;
        let mut comment = None;

        loop {
            match tokens.get(idx) {
                Some(Token::Comment(c)) => {
                    comment = Some(c.clone());
                    idx += 1;
                }
                Some(Token::Word(word)) => {
                    if let Some(def_id) = self.def_name.get(word) {
                        let def_id = *def_id;
                        idx += 1;
                        match tokens.get(idx) {
                            Some(Token::LParen) => {
                                // parse the arg list
                                idx += 1;
                                let start = idx;
                                loop {
                                    if let Some(x) = tokens.get(idx) {
                                        match x {
                                            Token::RParen => {
                                                break;
                                            }
                                            _ => {
                                                idx += 1;
                                            }
                                        }
                                    } else {
                                        return Err(JamError::BadParse(
                                            "can't find closing }".into(),
                                        ));
                                    }
                                }

                                let args =
                                    self.parse_exprs(def_name, &tokens[start..idx], false)?;
                                idx += 1;
                                output.extend(args);
                                output.push(Expression {
                                    expr: Expr::Call(def_id),
                                    comment,
                                });
                                comment = None;
                            }
                            _ => return Err(JamError::BadParse("Expected {".into())),
                        }
                    } else {
                        idx += 1;
                    }
                }
                Some(Token::Quote(quote)) => {
                    idx += 1;
                    output.push(Expression {
                        expr: Expr::PushString(quote.clone()),
                        comment,
                    });
                    comment = None;
                }
                None => {
                    break;
                }
                _ => return Err(JamError::BadParse("expected call".into())),
            }
        }

        if top_level {
            self.validate_exprs(def_name, output)
        } else {
            Ok(output)
        }
    }

    fn parse_block(
        &mut self,
        def_name: &str,
        tokens: &[Token],
        idx: &mut usize,
    ) -> Result<Block, JamError> {
        if let Some(Token::LCurly) = tokens.get(*idx) {
            *idx += 1;

            let start = *idx;

            while let Some(token) = tokens.get(*idx) {
                match token {
                    Token::RCurly => {
                        break;
                    }
                    _ => *idx += 1,
                }
            }

            let body = self.parse_exprs(def_name, &tokens[start..*idx], true)?;

            Ok(Block { body })
        } else {
            Err(JamError::BadParse("expect {".into()))
        }
    }

    fn parse_def(
        &mut self,
        tokens: &[Token],
        idx: &mut usize,
    ) -> Result<(String, Block), JamError> {
        let name = tokens.get(*idx + 1);
        *idx += 2;

        if let Some(Token::Word(name)) = &name {
            let block = self.parse_block(name, tokens, idx)?;
            *idx += 1;

            Ok((name.clone(), block))
        } else {
            Err(JamError::BadParse("expected word".into()))
        }
    }

    fn parse(&mut self, tokens: &[Token]) -> Result<(), JamError> {
        let mut idx = 0;
        let mut comment = None;

        while idx < tokens.len() {
            match tokens.get(idx) {
                Some(Token::Comment(c)) => {
                    comment = Some(c.clone());
                    idx += 1;
                }
                Some(Token::Word(w)) if w == "def" => {
                    let (name, block) = self.parse_def(tokens, &mut idx)?;
                    let def_id = self.definitions.len();
                    self.definitions.push(Definition::Block {
                        body: block,
                        comment,
                    });
                    self.def_name.insert(name, def_id);
                    comment = None;
                }
                x => {
                    println!("{:?}", x);
                    return Err(JamError::BadParse("expected def keyword".into()));
                }
            }
        }
        Ok(())
    }
}

fn run(fname: &str) -> Result<(), JamError> {
    let mut engine = Jam::new();
    let tokens = engine.lex(fname)?;

    engine.parse(&tokens)?;

    println!("{:#?}", engine);

    //let _ = engine.eval(&expr);
    if let Some(main_def_id) = engine.def_name.get("main") {
        if let Some(Definition::Block { body: block, .. }) = engine.definitions.get(*main_def_id) {
            let block = block.clone();

            for expression in &block.body {
                engine.eval(&expression.expr);
            }
        }
    } else {
        return Err(JamError::EvalError("can't find main".into()));
    }

    Ok(())
}

fn pseudo(fname: &str) -> Result<(), JamError> {
    let mut engine = Jam::new();
    let tokens = engine.lex(fname)?;

    engine.parse(&tokens)?;

    for (name, def_id) in engine.def_name {
        println!("function '{}':", name);
        if let Some(definition) = engine.definitions.get(def_id) {
            match definition {
                Definition::Internal(internal) => match internal {
                    Internal::Print => {
                        println!("  <internal print>");
                    }
                },
                Definition::Block { body, comment } => {
                    if let Some(comment) = comment {
                        println!("  Note: {}", comment);
                    }
                    for expression in &body.body {
                        if let Some(comment) = &expression.comment {
                            println!("  {}", comment);
                        }
                    }
                }
            }
        }
        println!("");
    }

    Ok(())
}

fn main() -> Result<(), JamError> {
    let mut args = std::env::args().skip(1);

    let command = args.next();
    let filename = args.next();

    match (command, filename) {
        (Some(command), Some(filename)) => match command.as_str() {
            "run" => run(&filename)?,
            "pseudo" => pseudo(&filename)?,
            _ => {}
        },
        _ => {
            println!("To run, call: jam <run/pseudo> <filename>");
        }
    }

    Ok(())
}
