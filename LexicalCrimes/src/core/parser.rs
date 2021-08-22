use super::{
    lexer::{Lexer, Analyser},
    tokens::*,
    builtins::{Function, Builtin},
    states::{ProgramState, Operation},
    memory::{MemoryLayout, Value, Manager},
    messages::*,
};

pub struct Parser {
    pub(super) token: Token,
    pub(super) slice: String,
}

use std::process;
use std::str;

impl Parser {
    // parsing is performed in a deterministic syntax tree lookup
    // everything is seperately defined for assignment and execution because of different behaviours
    pub fn parse(&self) {
        match self.token {
            Token::Variable => {
                let token_set = Lexer::tokenize(&Lexer, &self.slice);
                let slice_set = Lexer::slice(&Lexer, &self.slice);      

                let mut key: &str = &self.slice;
                let mut assigned: bool = false;
                let mut key_set: bool = false;
                let mut pointer = 0;

                if token_set.contains(&Token::Assign) {
                    assigned = true;
                }

                // only if the value being assigned is a math expression
                if token_set.contains(&Token::Math) {
                    let mut expression = String::new();
                    for token in token_set {
                        match token {
                            Token::Variable => (),
                            Token::Assign => (),
                            Token::Keyword => {
                                let value = &slice_set[pointer];
                                if !key_set {
                                    key = value;
                                    key_set = true;
                                } else {
                                    // handle variables and assemble math expression with their respective values
                                    if Lexer::tokenize(&Lexer, value)[0] == Token::Number {
                                        expression.push_str(value)
                                    } else {
                                        let mem_return = MemoryLayout::fetch(value);
                                        if mem_return.is_some() {
                                            let value = match mem_return.unwrap() {
                                                Value::String(value) => value,
                                                Value::FInt(value) => value.to_string(),
                                                Value::Int(value) => value.to_string(),
                                                Value::Nothing => unimplemented!(),
                                            };
                                            expression.push_str(&value)
                                        } else {
                                            push_error(
                                                format!("`{}` is not initialized.", value)
                                            );
                                            process::exit(1)
                                        }
                                    }
                                }
                            },
                            _ => {
                                if assigned {
                                    // get value
                                    let value = &slice_set[pointer];

                                    let token_type = Lexer::tokenize(&Lexer, value)[0];

                                    // handle variables and assemble math expression with their respective values
                                    if token_type == Token::Keyword {
                                        let mem_return = MemoryLayout::fetch(value);
                                        if mem_return.is_some() {
                                            let value = match mem_return.unwrap() {
                                                Value::String(value) => value,
                                                Value::FInt(value) => value.to_string(),
                                                Value::Int(value) => value.to_string(),
                                                Value::Nothing => unimplemented!(),
                                            };
                                            expression.push_str(&value)
                                        } else {
                                            push_error(
                                                format!("`{}` is not initialized.", value)
                                            );
                                            process::exit(1)
                                        }
                                    } else {
                                        expression.push_str(value)
                                    }
                                } else {
                                    let state = ProgramState::read_state();
                                    push_error(
                                        format!("Syntax error in `let` assign in line no: {}.", state.line)
                                    );
                                    process::exit(1)
                                }
                            },
                        }
    
                        pointer += 1;
                    }

                    let ret = Function::math_evaluator(&expression).to_string();
                    
                    // allocate memory
                    MemoryLayout::alloc(
                        key.to_string(),
                        Value::String(ret),
                    )
                } else {
                    for token in token_set {
                        match token {
                            Token::Variable => (),
                            Token::Assign => (),
                            Token::LibFunction => {
                                // lib function slice set should follow syntax spec of
                                // being the 3rd element of an assignment
                                let func_slice = &slice_set[3];

                                // parse function name
                                let keyword = STRING.replace_all(func_slice, "").to_string();
        
                                // get input message
                                let value: String;
                                let message_match = &STRING.captures(&self.slice).unwrap();

                                // ensure matches exist otherwise fallback to default value
                                if message_match.len() > 0 {
                                    value = message_match[0].to_string()
                                } else {
                                    value = String::from("")
                                }

                                // define function struct
                                let function = Function {
                                    keyword: keyword.trim().to_string(),
                                    value,
                                };

                                // execute library function
                                let func_ret = Function::execute(&function);

                                // if the library function emit output, allocate on assigned key
                                if func_ret.is_some() {
                                    MemoryLayout::alloc(
                                        key.to_string(),
                                        Value::String(func_ret.unwrap()),
                                    )
                                }
                            },
                            Token::Keyword => key = &slice_set[pointer],
                            _ => {
                                if assigned {
                                    // get value
                                    let value = &slice_set[pointer];
    
                                    // allocate memory
                                    MemoryLayout::alloc(
                                        key.to_string(),
                                        Value::String(value.to_string()),
                                    )
                                } else {
                                    let state = ProgramState::read_state();
                                    push_error(
                                        format!("Syntax error in `let` assign in line no: {}.", state.line)
                                    );
                                    process::exit(1)
                                }
                            },
                        }
    
                        pointer += 1;
                    }
                }
            },
            Token::Keyword => {
                let token_set = Lexer::tokenize(&Lexer, &self.slice);
                let slice_set = Lexer::slice(&Lexer, &self.slice);

                // value set
                let keyword: String;
                let value: String;

                // creating value set
                // `token_set[1]` is the argument to a keyword
                if token_set.len() > 1 {
                    match token_set[1] {
                        Token::Keyword => {
                            let keyword = &slice_set[0];
                            let value = self.slice.replace(keyword, "");
                            let value = &value.trim();
    
                            // evaluating math expressions executed from a keyword
                            if token_set.contains(&Token::Math) {
                                let mut construct = String::new();
                                let temp_eval = value.chars();
                                for c in temp_eval {
                                    match c {
                                        '+' | '-' | '*' | '/' => {
                                            let append = format!(" {} ", c);
                                            construct.push_str(&append)
                                        },
                                        _ => construct.push(c)
                                    }
                                }
                                let construct = construct.split(" ");
                                let mut expression = String::new();
                                for c in construct {
                                    // denoting a math expression
                                    match c {
                                        "+" | "-" | "*" | "/" | "" => expression.push_str(c),
                                        _ => {
                                            if Lexer::tokenize(&Lexer, c)[0] == Token::Number {
                                                expression.push_str(c)
                                            } else {
                                                let mem_return = MemoryLayout::fetch(c);
                                                if mem_return.is_some() {
                                                    let value = match mem_return.unwrap() {
                                                        Value::String(value) => value,
                                                        Value::FInt(value) => value.to_string(),
                                                        Value::Int(value) => value.to_string(),
                                                        Value::Nothing => unimplemented!(),
                                                    };
                                                    expression.push_str(&value)
                                                } else {
                                                    push_error(
                                                        format!("`{}` is not initialized.", value)
                                                    );
                                                    process::exit(1)
                                                }
                                            }
                                        }
                                    }
                                }
    
                                // evaluate math expressions
                                let ret = Function::math_evaluator(&expression).to_string();
    
                                let function = Function {
                                    keyword: keyword.trim().to_string(),
                                    value: ret,
                                };
    
                                Function::execute(&function);
                            } else {
                                let mem_return = MemoryLayout::fetch(value);
    
                                if mem_return.is_some() {
                                    let value = match mem_return.unwrap() {
                                        Value::String(value) => value,
                                        Value::FInt(value) => value.to_string(),
                                        Value::Int(value) => value.to_string(),
                                        Value::Nothing => unimplemented!(),
                                    };
        
                                    let function = Function {
                                        keyword: keyword.trim().to_string(),
                                        value,
                                    };
        
                                    Function::execute(&function);
                                } else {
                                    push_error(
                                        format!("`{}` is not initialized.", value)
                                    );
                                    process::exit(1)
                                }
                            }
                        }
                        Token::String => {
                            keyword = STRING.replace_all(&self.slice, "").to_string();
                            value = self.slice.replace(&keyword, "");
                            let function = Function {
                                keyword: keyword.trim().to_string(),
                                value,
                            };
                            Function::execute(&function);
                        },
                        Token::Number => {
                            let keyword = &slice_set[0];
                            let value = self.slice.replace(keyword, "");
    
                            // evaluate math expressions
                            let ret = Function::math_evaluator(&value).to_string();
    
                            let function = Function {
                                keyword: keyword.trim().to_string(),
                                value: ret,
                            };
                            Function::execute(&function);
                        }
                        _ => (),
                    }                    
                }
            },
            Token::Statement => {
                // // a statement should only be executed inside a command block of course!
                // let state = ProgramState::read_state();
                // // verify command block state
                // let curr_function = state.function;
                // unresolved conditionals (`Token::ConditionMet`) shouldn't proceed actions
                // if curr_function == Token::CommandStart {
                    // parse command
                    let statement = &self.slice;

                    // construct statement
                    let mut parsed_slice: &str = statement;

                    // parse statement
                    let mut rem_token = parsed_slice.chars();
                    while parsed_slice.ends_with("|") {
                        rem_token.next();
                        rem_token.next_back();
                        parsed_slice = rem_token.as_str();
                    }

                    // remove whitespace
                    parsed_slice = parsed_slice.trim();

                    let mut rem_token = parsed_slice.chars();
                    while parsed_slice.starts_with("|") {
                        rem_token.next();
                        parsed_slice = rem_token.as_str();
                    }

                    // handle else statement
                    if parsed_slice.trim().starts_with("else") {
                        // get program state
                        let state = ProgramState::read_state();

                        // else statement runs when conditions are not met of if else statements
                        if state.function == Token::ConditionNotMet {
                            // else statements have functions, it should be met in the same condition
                            let mut rem_token = parsed_slice.chars();
                            parsed_slice = parsed_slice.trim();

                            while parsed_slice.starts_with("else") {
                                for _ in 0..5 {
                                    rem_token.next();
                                }
                                parsed_slice = rem_token.as_str()
                            }

                            // a statement contains a keyword and it's arguments
                            let parser = Self {
                                token: Token::Keyword,
                                slice: parsed_slice.trim().to_string(),
                            };
                            // execute keyword functions via recursive parsing
                            parser.parse()
                        }
                    } else {
                        let mut parser: Self;

                        // a statement contains a keyword and it's arguments
                        parser = Self {
                            token: Token::Keyword,
                            slice: parsed_slice.trim().to_string(),
                        };

                        if parsed_slice.trim().starts_with("let") {
                            parser = Self {
                                token: Token::Variable,
                                slice: parsed_slice.trim().to_string(),
                            };
                        }                        

                        // execute keyword functions via recursive parsing
                        parser.parse()
                    }
                // }
                // else {
                //     push_error("You cannot execute `function` statements outside comments.".to_string());
                //     process::exit(1)
                // }
            },
            Token::IfCondition => {
                // a statement should only be executed inside a command block of course!
                // // verify command block state
                // if state.function == Token::CommandStart {
                    // if condition is met, skip if statements (readtime++)
                    // if state.function == Token::IfCondition {
                        // parse command
                let seperator = self.slice.split("->");

                // parse flow chart
                for mut conditionals in seperator {
                    // construct statement
                    let mut statement: &str = "";

                    conditionals = conditionals.trim();
                    let mut parse_chars = conditionals.chars();

                    statement = parse_chars.as_str();
                    while parse_chars.as_str().ends_with("-") {
                        parse_chars.next_back();
                        statement = parse_chars.as_str()
                    }

                    // parse statement
                    let mut rem_token = statement.chars();
                    while statement.starts_with("|") && statement.ends_with("|") {
                        rem_token.next();
                        rem_token.next_back();
                        statement = rem_token.as_str()
                    }

                    let parser: Self;
                    statement = statement.trim();

                    if statement.starts_with("if") {
                        parser = Self {
                            token: Token::ConditionHandler,
                            slice: statement.to_string(),
                        };

                        parser.parse()
                    } else {
                        let state = ProgramState::read_state();
                        if state.function == Token::ConditionMet || state.function == Token::IfCondition {
                            parser = Self {
                                token: Token::Keyword,
                                slice: statement.to_string(),
                            };

                            parser.parse()
                        }
                    }
                }
                    // }              
                // }
                // else {
                //     println!("BRUH: {}", self.slice);
                //     push_error("You cannot execute `if` statements outside comments.".to_string());
                //     process::exit(1)
                // }                
            },
            Token::ConditionHandler => {
                let conditionals = &self.slice;

                let mut candidates: &str = "";

                // I just remembered i have to be somewhere... rushing time

                // parse statement
                if conditionals.starts_with("if") {
                    let mut parse_chars = conditionals.chars();
                    for _ in 0..2 {
                        parse_chars.next();
                    }
                    candidates = parse_chars.as_str();
                }

                // get candidations on an existential crisis
                let token_set = Lexer::tokenize(&Lexer, candidates);
                let slice_set = Lexer::slice(&Lexer, candidates);
                
                if slice_set.len() == 3 {
                    let first_conditional = (&slice_set[0], token_set[0]);
                    let check = token_set[1];
                    let second_conditional = (&slice_set[2], token_set[2]);

                    let mut first_temp = String::new();
                    let mut second_temp = String::new();

                    match first_conditional {
                        (key, Token::Keyword) => {
                            let mem_return = MemoryLayout::fetch(key);

                            if mem_return.is_some() {
                                let value = match mem_return.unwrap() {
                                    Value::String(value) => value,
                                    Value::FInt(value) => value.to_string(),
                                    Value::Int(value) => value.to_string(),
                                    Value::Nothing => unimplemented!(),
                                };
    
                                first_temp = value
                            } else {
                                push_error(
                                    format!("`{}` is not initialized.", key)
                                );
                                process::exit(1)
                            }
                        },
                        (value, _) => first_temp = value.to_string(),
                    }

                    match second_conditional {
                        (key, Token::Keyword) => {
                            let mem_return = MemoryLayout::fetch(key);

                            if mem_return.is_some() {
                                let value = match mem_return.unwrap() {
                                    Value::String(value) => value,
                                    Value::FInt(value) => value.to_string(),
                                    Value::Int(value) => value.to_string(),
                                    Value::Nothing => unimplemented!(),
                                };
    
                                second_temp = value
                            } else {
                                push_error(
                                    format!("`{}` is not initialized.", key)
                                );
                                process::exit(1)
                            }
                        },
                        (value, _) => second_temp = value.to_string(),
                    }

                    let mut result: bool = false;

                    // format based on tokens
                    first_temp = match Lexer::tokenize(&Lexer, &first_temp)[0] {
                        // numbers represent as they are
                        Token::Number => first_temp,
                        // strings/objects need formatting (avoid pretty print)
                        Token::String => {
                            let mut chars = first_temp.trim().chars();
                            chars.next();
                            chars.next_back();
                            chars.as_str().to_string()
                        },
                        _ => first_temp,
                    };

                    second_temp = match Lexer::tokenize(&Lexer, &second_temp)[0] {
                        // numbers represent as they are
                        Token::Number => second_temp,
                        // strings/objects need formatting (avoid pretty print)
                        Token::String => {
                            let mut chars = second_temp.trim().chars();
                            chars.next();
                            chars.next_back();
                            chars.as_str().to_string()
                        },
                        _ => second_temp,
                    };

                    // whitespace on both ends should be cleared
                    // NOTE: what if user puts that in? I will deal with that later, I gotta rush now
                    first_temp = first_temp.trim().to_string();
                    second_temp = second_temp.trim().to_string();

                    match check {
                        Token::Equals => {
                            if first_temp == second_temp {
                                result = true
                            } else {
                                result = false
                            }
                        },
                        Token::NotEquals => {
                            if first_temp != second_temp {
                                result = true
                            } else {
                                result = false
                            }
                        },
                        Token::GreaterThan => {
                            let first_temp = match first_temp.parse::<i32>() {
                                Ok(first_temp) => first_temp,
                                Err(_) => {
                                    push_error(
                                        format!("`{}` is not an integer to compare with.", first_temp)
                                    );
                                    process::exit(1)                                    
                                }
                            };

                            let second_temp = match second_temp.parse::<i32>() {
                                Ok(second_temp) => second_temp,
                                Err(_) => {
                                    push_error(
                                        format!("`{}` is not an integer to compare with.", second_temp)
                                    );
                                    process::exit(1)                                    
                                }
                            };

                            if first_temp > second_temp {
                                result = true
                            } else {
                                result = false
                            }
                        },
                        Token::LesserThan => {
                            let first_temp = match first_temp.parse::<i32>() {
                                Ok(first_temp) => first_temp,
                                Err(_) => {
                                    push_error(
                                        format!("`{}` is not an integer to compare with.", first_temp)
                                    );
                                    process::exit(1)                                    
                                }
                            };

                            let second_temp = match second_temp.parse::<i32>() {
                                Ok(second_temp) => second_temp,
                                Err(_) => {
                                    push_error(
                                        format!("`{}` is not an integer to compare with.", second_temp)
                                    );
                                    process::exit(1)                                    
                                }
                            };

                            if first_temp < second_temp {
                                result = true
                            } else {
                                result = false
                            }
                        },
                        Token::GreaterThanOrEquals => {
                            let first_temp = match first_temp.parse::<i32>() {
                                Ok(first_temp) => first_temp,
                                Err(_) => {
                                    push_error(
                                        format!("`{}` is not an integer to compare with.", first_temp)
                                    );
                                    process::exit(1)                                    
                                }
                            };

                            let second_temp = match second_temp.parse::<i32>() {
                                Ok(second_temp) => second_temp,
                                Err(_) => {
                                    push_error(
                                        format!("`{}` is not an integer to compare with.", second_temp)
                                    );
                                    process::exit(1)                                    
                                }
                            };

                            if first_temp >= second_temp {
                                result = true
                            } else {
                                result = false
                            }
                        },
                        Token::LesserThanOrEquals => {
                            let first_temp = match first_temp.parse::<i32>() {
                                Ok(first_temp) => first_temp,
                                Err(_) => {
                                    push_error(
                                        format!("`{}` is not an integer to compare with.", first_temp)
                                    );
                                    process::exit(1)                                    
                                }
                            };

                            let second_temp = match second_temp.parse::<i32>() {
                                Ok(second_temp) => second_temp,
                                Err(_) => {
                                    push_error(
                                        format!("`{}` is not an integer to compare with.", second_temp)
                                    );
                                    process::exit(1)                                    
                                }
                            };

                            if first_temp <= second_temp {
                                result = true
                            } else {
                                result = false
                            }
                        },
                        _ => (),
                    }

                    if result {
                        ProgramState::set_state(
                            Token::ConditionMet,
                            Operation::StateChange,
                            0,
                        )
                    } else {
                        ProgramState::set_state(
                            Token::ConditionNotMet,
                            Operation::StateChange,
                            0,
                        )                        
                    }
                } else {
                    push_error("Improper if statement found.".to_string());
                    process::exit(1)                    
                }
            },
            Token::LoopBack => {
                let seperator = self.slice.split("<-");

                // parse flow chart
                for mut conditionals in seperator {
                    // construct statement
                    let mut statement: &str = "";

                    conditionals = conditionals.trim();
                    let mut parse_chars = conditionals.chars();

                    statement = parse_chars.as_str();
                    while parse_chars.as_str().ends_with("-") {
                        parse_chars.next_back();
                        statement = parse_chars.as_str()
                    }

                    // parse statement
                    let mut rem_token = statement.chars();
                    while statement.starts_with("|") && statement.ends_with("|") {
                        rem_token.next();
                        rem_token.next_back();
                        statement = rem_token.as_str()
                    }

                    // a statement contains a keyword and it's arguments
                    let parser = Self {
                        token: Token::Keyword,
                        slice: statement.trim().to_string(),
                    };
                    // execute keyword functions via recursive parsing
                    parser.parse()                    
                }                
            }
            _ => (),
        }
    }
}