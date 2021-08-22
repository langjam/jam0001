use std::process;

use logos::Logos;

use super::{
    tokens::*,
    messages::*,
    states::{ProgramState, Operation},
    memory::{MemoryLayout, Value, Manager},
    parser::Parser,
};

pub struct Lexer;

pub trait Analyser {
    fn analyse(&self, slice: &str) -> Option<Token>;
    fn tokenize(&self, slice: &str) -> Vec<Token>;
    fn slice(&self, slice: &str) -> Vec<String>;
    fn lexerize(&self, source: &str);
}

/// Lexical Analysis
impl Analyser for Lexer {
    // stepped analysis
    fn analyse(&self, slice: &str) -> Option<Token> {
        let state = ProgramState::read_state();
        // TODO: it's seperated for now, make it a single block with default iteration count as 1
        // running inside a loop state
        if state.function == Token::Loop {
            // iterate parser `iter_count` times
            // statement grammar
            match token_grammar(slice) {
                Token::Statement => {
                    let slice_parse = Parser {
                        token: Token::Statement,
                        slice: slice.to_string(),
                    };
                    Parser::parse(&slice_parse);
                },
                Token::String => return Some(Token::String),
                Token::Math => return Some(Token::Math),
                Token::IfCondition => {
                    let state = ProgramState::read_state();

                    // lexer skip if conditions are met on state to avoid parsing unneeded blocks
                    match state.function {
                        Token::ConditionMet => return None,
                        _ => {
                            ProgramState::set_state(
                                Token::IfCondition,
                                Operation::StateChange,
                                0
                            );
                            let slice_parse = Parser {
                                token: Token::IfCondition,
                                slice: slice.to_string(),
                            };
                            Parser::parse(&slice_parse);                                                                               
                        }
                    }
                    return None
                },
                _ => return None
            }

            // maintains seperate code bodies
            let codebody_key = "<CODEBODY>".to_string();
            let mem_fetch = MemoryLayout::fetch(&codebody_key);
            if mem_fetch.is_some() {
                let mut body = String::new();
                let value = match mem_fetch.unwrap() {
                    Value::String(value) => value,
                    Value::FInt(value) => value.to_string(),
                    Value::Int(value) => value.to_string(),
                    Value::Nothing => unimplemented!(),
                };
                body.push_str(&value);

                MemoryLayout::alloc(
                    codebody_key,
                     Value::String(body)
                );
            } else {
                MemoryLayout::alloc(
                    codebody_key,
                     Value::String(slice.to_string())
                );
            }

            None
        } else {
            // statement grammar
            match token_grammar(slice) {
                Token::Statement => {
                    let slice_parse = Parser {
                        token: Token::Statement,
                        slice: slice.to_string(),
                    };
                    Parser::parse(&slice_parse);
                    return None
                },
                Token::String => return Some(Token::String),
                Token::Math => return Some(Token::Math),
                Token::IfCondition => {
                    let state = ProgramState::read_state();

                    // lexer skip if conditions are met on state to avoid parsing unneeded blocks
                    match state.function {
                        Token::ConditionMet => return None,
                        _ => {
                            ProgramState::set_state(
                                Token::IfCondition,
                                Operation::StateChange,
                                0
                            );
                            let slice_parse = Parser {
                                token: Token::IfCondition,
                                slice: slice.to_string(),
                            };
                            Parser::parse(&slice_parse);                                                                               
                        }
                    }

                    return None
                },
                Token::LoopBack => {
                    let slice_parse = Parser {
                        token: Token::LoopBack,
                        slice: slice.to_string(),
                    };
                    Parser::parse(&slice_parse);
                    None
                },
                _ => return None
            }
        }
    }

    fn tokenize(&self, slice: &str) -> Vec<Token> {
        let mut lex = Token::lexer(slice);
        // maximum statement scope stays 4
        // confident lookaheads here -> on assignments lexer decides slice type based on format
        // parser takes care of the rest (i hope lol)
        let mut tokens: Vec<Token> = Vec::with_capacity(4);

        loop {
            let (token, _span, slice) = (lex.next(), lex.span(), lex.slice());

            if token.is_some() {
                let curr_token = token.unwrap();
                match token {
                    Some(Token::PassLex) => {
                        let token = self.analyse(slice);
                        if token.is_some() {
                            tokens.push(token.unwrap())
                        }
                    },
                    _ => tokens.push(curr_token)
                }
            } else {
                break
            }
        }

        tokens
    }

    fn slice(&self, slice: &str) -> Vec<String> {
        let mut lex = Token::lexer(slice);
        // maximum statement scope stays 4
        let mut slices: Vec<String> = Vec::with_capacity(4);

        loop {
            let (token, slice) = (lex.next(), lex.slice());

            if token.is_some() {
                match token {
                    Some(Token::PassLex) => {
                        let token = self.analyse(slice);
                        if token.is_some() {
                            slices.push(slice.to_string())
                        }
                    },
                    _ => slices.push(slice.to_string())
                }
            } else {
                break
            }
        }

        slices
    }    

    // main lexer
    fn lexerize(&self, source: &str) {
        let mut lex = Token::lexer(source);
        let mut line: usize = 1;
    
        loop {
            let (token, _span, slice) = (lex.next(), lex.span(), lex.slice());
    
            // println!("\n\n{:?} :: {:?}", slice, token);
    
            match token {
                // line coint
                Some(Token::Newline) => line += 1,
                // entry point
                Some(Token::MainFunction) => {
                    ProgramState::set_state(
                        Token::MainFunction,
                        Operation::StateChange,
                        line
                    );                
                },
                // end of a function
                Some(Token::FunctionEnd) => {
                    ProgramState::set_state(
                        Token::FunctionEnd,
                        Operation::StateChange,
                        line
                    )
                },            
                // start of a comand
                Some(Token::CommandStart) => {
                    ProgramState::set_state(
                        Token::CommandStart,
                        Operation::StateChange,
                        line
                    )                
                },
                // end of a command
                Some(Token::CommandEnd) => {
                    ProgramState::set_state(
                        Token::CommandEnd,
                        Operation::StateChange,
                        line
                    )
                },
                Some(Token::Variable) => {
                    // set state to allocation
                    ProgramState::set_state(
                        Token::Variable,
                        Operation::Allocation,
                        line
                    );

                    // take full slice
                    let mut scope_lex = lex.clone();
                    let mut scope_slice = String::new();

                    // add variable key
                    scope_slice.push_str(slice);
                    scope_slice.push(' ');

                    loop {
                        // get arbitrary syntax
                        let (token, slice) = (scope_lex.next(), scope_lex.slice());

                        // read until end of statement
                        if token != Some(Token::Newline) {
                            scope_slice.push_str(slice);
                            scope_slice.push(' ')
                        } else {
                            break
                        }
                    }

                    // pass the complete slice to parser
                    let slice_parse = Parser {
                        token: Token::Variable,
                        slice: scope_slice,
                    };

                    Parser::parse(&slice_parse);
                },
                // stepped parsing
                Some(Token::PassLex) => {
                    self.analyse(slice);
                },
                // finish point
                None => process::exit(0),
                // nope!
                _ => (),
            }
    
            // main syntax validation
            if line == 1 && token != Some(Token::MainFunction) {
                push_error("Program should start with a main function.".to_string());
                process::exit(1)
            }        
        }
    }
}