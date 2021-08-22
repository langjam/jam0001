use lazy_static::lazy_static;
use std::collections::HashMap;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Primary0,
    LoopStart,
    LoopEnd,
    Pow,
    Double,
    Pop,
    Push,
    Compare,
    Main,
    If,
    ElseIf,
    Else,
    EndIf,
    Primary911,
    Primary101,
    Write,
    Read,
    FnCall,
    Break,
    PortClose,
    Panic,
    Sum,
    PortOpen,
    Return,
    Inc,
    Dec,
    Leave,
    Swap,
    Save,
    FnStart,
    FnEnd,
    Xor,
    Primary1,
    Eof,
}

// LUT of first class comments (vine quotes) to tokens
lazy_static! {
    static ref COMMENTS: HashMap<&'static str, TokenKind> = {
        let mut m = HashMap::new();
        m.insert("Hi, welcome to Chili's", TokenKind::Main);
        m.insert("Miss Keisha! Miss Keisha!", TokenKind::Primary0);
        m.insert("Road work ahead?", TokenKind::LoopStart);
        m.insert("Yeah I sure hope it does", TokenKind::LoopEnd);
        m.insert("The volume in this bus is astronomical", TokenKind::Pow);
        m.insert(
            "Double cheeked up on a Thursday afternoon",
            TokenKind::Double,
        );
        m.insert("I can't swim", TokenKind::Pop);
        m.insert("Let's go to the beach beach!", TokenKind::Push);
        m.insert("I am confusion", TokenKind::Compare);
        m.insert("Happy Chrismis", TokenKind::If);
        m.insert("Its Chrismin", TokenKind::ElseIf);
        m.insert("Merry Chrisis", TokenKind::Else);
        m.insert("Merry Chrysler", TokenKind::EndIf);
        m.insert("Chris! Is that a weed??", TokenKind::Primary911);
        m.insert("Oooh", TokenKind::Primary101);
        m.insert("And they were roomates", TokenKind::Write);
        m.insert("ELLO SUSIE", TokenKind::Read);
        m.insert("IMMA BUY ME A SUBARU", TokenKind::FnCall);
        m.insert(
            "I don't even know which way the Quiznos is",
            TokenKind::Break,
        );
        m.insert("A child", TokenKind::PortClose);
        m.insert("No off-topic questions", TokenKind::Panic);
        m.insert("You have been stopped", TokenKind::Panic);
        m.insert("Look at all those chickens!", TokenKind::Sum);
        m.insert("I can't believe you've done this", TokenKind::PortOpen);
        m.insert("Zack stop", TokenKind::Return);
        m.insert("Hey Ron", TokenKind::Inc);
        m.insert("Hey Billy", TokenKind::Dec);
        m.insert("I'm a giraffe!", TokenKind::Leave);
        m.insert("Wow", TokenKind::Swap);
        m.insert("What are those??", TokenKind::Save);
        m.insert("I never went to Oovoo Javer", TokenKind::Primary0);
        m.insert("I'm in me mum's car", TokenKind::FnStart);
        m.insert("Get out me car", TokenKind::FnEnd);
        m.insert("It is Wednesday my dudes", TokenKind::Xor);
        m.insert("Eh", TokenKind::Primary1);
        m
    };
}

pub struct Lexer<'i> {
    input: &'i str,
    position: usize,
    eof: bool,
}

impl<'i> Lexer<'i> {
    pub fn new(input: &'i str) -> Self {
        Self {
            input,
            position: 0,
            eof: false,
        }
    }

    pub fn tokenize(&mut self) -> Vec<TokenKind> {
        self.collect()
    }

    fn next_token(&mut self, input: &str) -> TokenKind {
        // iterate through each vine quote to find the earliest match
        let (kind, len) = COMMENTS
            .iter()
            .filter_map(|(comment, kind)| {
                input
                    .find(comment)
                    .map(|idx| (kind, idx, idx + comment.len()))
            })
            .min_by_key(|&(_, start, _)| start)
            .map(|(kind, _, stop)| ((*kind, stop)))
            .unwrap_or((TokenKind::Eof, 0));
        self.position += len;
        kind
    }
}

impl<'i> Iterator for Lexer<'i> {
    type Item = TokenKind;

    fn next(&mut self) -> Option<TokenKind> {
        if self.eof {
            return None;
        }
        if self.position >= self.input.len() {
            self.eof = true;
            return Some(TokenKind::Eof);
        }
        let next = self.next_token(&self.input[self.position..]);
        if next == TokenKind::Eof {
            self.eof = true;
        }
        Some(next)
    }
}

#[test]
fn it_lexes() {
    let mut lexer = Lexer::new("Hi, welcome to Chili's Wow");
    assert_eq!(
        lexer.tokenize(),
        vec![TokenKind::Main, TokenKind::Swap, TokenKind::Eof]
    );
}

#[test]
fn it_ignores_intermediate_text() {
    let mut lexer =
        Lexer::new("Well.. Hi, welcome to Chili's! Have a seat. Wow, you have a big party!");
    assert_eq!(
        lexer.tokenize(),
        vec![TokenKind::Main, TokenKind::Swap, TokenKind::Eof]
    );
}

pub struct Vm {
    // registers
    primary: i64,
    secondary: i64,
    // arithmetic status
    status: VmStatus,
    // stack
    stack: Vec<i64>,
}

#[derive(Debug, PartialEq, Eq)]
enum VmStatus {
    EqZero,
    GtZero,
    LtZero,
}

use std::{
    fs::File,
    io::{Read, Write},
    os::unix::io::FromRawFd,
};
impl Vm {
    pub fn new() -> Self {
        Self {
            primary: 0,
            secondary: 0,
            status: VmStatus::EqZero,
            stack: Vec::new(),
        }
    }

    fn set_status(&mut self, value: i64) {
        self.status = if value == 0 {
            VmStatus::EqZero
        } else if value < 0 {
            VmStatus::LtZero
        } else {
            VmStatus::GtZero
        }
    }

    fn pop(&mut self) -> Result<i64, &'static str> {
        self.stack.pop().ok_or("Stack is empty")
    }

    fn push(&mut self, value: i64) -> Result<(), &'static str> {
        self.stack.push(value);
        Ok(())
    }

    /// execute modifies the VM given the instruction token
    pub fn execute(&mut self, token: TokenKind) -> Result<(), &'static str> {
        let last_primary = self.primary;
        match token {
            TokenKind::Primary0 => {
                self.primary = 0;
            }
            TokenKind::Pow => {
                self.primary = self.primary.pow(self.secondary as u32);
            }
            TokenKind::Double => {
                self.primary *= 2;
            }
            TokenKind::Pop => {
                self.primary = self.pop()?;
            }
            TokenKind::Push => {
                self.push(self.primary)?;
            }
            TokenKind::Compare => {
                self.set_status(self.primary - self.secondary);
            }
            TokenKind::Primary911 => {
                self.primary = 911;
            }
            TokenKind::Primary101 => {
                self.primary = 101;
            }
            TokenKind::Write => {
                let mut f = unsafe { File::from_raw_fd(self.secondary as i32) };
                f.write(&[self.primary as u8]).map_err(|_| "Write failed")?;
            }
            TokenKind::Read => {
                let mut f = unsafe { File::from_raw_fd(self.secondary as i32) };
                let mut buf = [0; 1];
                f.read_exact(&mut buf).map_err(|_| "Read failed")?;
                self.primary = buf[0] as i64;
            }
            TokenKind::PortClose => {
                // TODO
            }
            TokenKind::Panic => {
                panic!("You have been stopped");
            }
            TokenKind::Sum => {
                self.primary += self.secondary;
            }
            TokenKind::PortOpen => {
                // TODO
            }
            TokenKind::Inc => {
                self.primary += 1;
            }
            TokenKind::Dec => {
                self.primary -= 1;
            }
            TokenKind::Leave => {
                self.secondary = self.pop()?;
                self.primary = self.pop()?;
            }
            TokenKind::Swap => {
                let tmp = self.primary;
                self.primary = self.secondary;
                self.secondary = tmp;
            }
            TokenKind::Save => {
                self.push(self.primary)?;
                self.push(self.secondary)?;
            }
            TokenKind::Xor => {
                self.primary ^= self.secondary;
            }
            TokenKind::Primary1 => {
                self.primary = 1;
            }
            _ => (),
        };
        if self.primary != last_primary {
            self.set_status(self.primary);
        }
        Ok(())
    }

    /// Execute all tokens and panic on error.
    /// This function is also responsible for control flow such as
    /// function definitions and if statements.
    pub fn run(&mut self, tokens: Vec<TokenKind>) {
        let mut fn_tokens = Vec::new();
        let mut loop_tokens = Vec::new();
        let mut iter = tokens.into_iter();

        let mut token = iter.next();
        while token.is_some() {
            match token.unwrap() {
                TokenKind::FnStart => {
                    // save instructions into fn_tokens until we see FnEnd
                    fn_tokens.clear();
                    let mut token = iter.next();
                    while token.is_some() {
                        match token.unwrap() {
                            TokenKind::FnEnd => break,
                            TokenKind::FnStart => {
                                panic!("No nested functions")
                            }
                            TokenKind::Eof => {
                                panic!("Unexpected EOF")
                            }
                            token => {
                                fn_tokens.push(token);
                            }
                        };
                        token = iter.next();
                    }
                }
                TokenKind::If => {
                    for token in self.if_stmt(&mut iter) {
                        if let Err(err) = self.execute(token) {
                            panic!("{}", err);
                        }
                    }
                }
                TokenKind::LoopStart => {
                    // save and execute instructions in loop_tokens
                    loop_tokens.clear();
                    let mut token = iter.next();
                    while token.is_some() {
                        match token.unwrap() {
                            TokenKind::LoopEnd => break,
                            token => loop_tokens.push(token),
                        }
                        token = iter.next();
                    }
                    'outer: while self.primary != 0 {
                        let mut iter = loop_tokens.iter().copied();
                        let mut token = iter.next();
                        while token.is_some() {
                            match token.unwrap() {
                                TokenKind::If => {
                                    for token in self.if_stmt(&mut iter) {
                                        if token == TokenKind::Break {
                                            break 'outer;
                                        }
                                        if token == TokenKind::Return {
                                            return;
                                        } else {
                                            if let Err(err) = self.execute(token) {
                                                panic!("{}", err);
                                            }
                                        }
                                    }
                                }
                                TokenKind::Break => break 'outer,
                                TokenKind::Return => return,
                                token => {
                                    if let Err(err) = self.execute(token) {
                                        panic!("{}", err);
                                    }
                                }
                            }
                            token = iter.next();
                        }
                    }
                }
                TokenKind::FnCall => {
                    // TODO
                }
                TokenKind::Eof | TokenKind::Return => return,
                token => {
                    if let Err(err) = self.execute(token) {
                        panic!("{}", err);
                    }
                }
            }
            token = iter.next();
        }
    }

    fn if_stmt(&self, iter: &mut dyn Iterator<Item = TokenKind>) -> Vec<TokenKind> {
        let mut inst = Vec::new();
        if self.status == VmStatus::GtZero {
            // advance iter until else
            let mut token = iter.next();
            while token.is_some() {
                match token.unwrap() {
                    TokenKind::Else => break,
                    TokenKind::EndIf => return inst,
                    TokenKind::Eof => {
                        panic!("Unexpected EOF")
                    }
                    _ => (),
                }
                token = iter.next();
            }
        } else if self.status == VmStatus::LtZero {
            // advance iter until elseif / else
            let mut token = iter.next();
            while token.is_some() {
                match token.unwrap() {
                    TokenKind::ElseIf | TokenKind::Else => break,
                    TokenKind::EndIf => return inst,
                    TokenKind::Eof => {
                        panic!("Unexpected EOF")
                    }
                    _ => (),
                }
                token = iter.next();
            }
        }
        // save instructions until elseif / else / endif
        let mut token = iter.next();
        while token.is_some() {
            match token.unwrap() {
                TokenKind::ElseIf | TokenKind::Else | TokenKind::EndIf => break,
                TokenKind::Eof => {
                    panic!("Unexpected EOF")
                }
                token => inst.push(token),
            }
            token = iter.next();
        }
        // read until endif
        while token.is_some() {
            match token.unwrap() {
                TokenKind::EndIf => break,
                _ => (),
            }
            token = iter.next();
        }
        inst
    }
}

#[test]
fn vm_sum() {
    let mut vm = Vm::new();
    vm.execute(TokenKind::Primary911).unwrap();
    assert_eq!(vm.status, VmStatus::GtZero);
    vm.execute(TokenKind::Swap).unwrap();
    assert_eq!(vm.status, VmStatus::EqZero);
    vm.execute(TokenKind::Primary101).unwrap();
    assert_eq!(vm.status, VmStatus::GtZero);
    vm.execute(TokenKind::Sum).unwrap();
    assert_eq!(vm.primary, 911 + 101);
}

#[test]
fn vm_push_pop() {
    let mut vm = Vm::new();
    vm.execute(TokenKind::Primary911).unwrap();
    vm.execute(TokenKind::Push).unwrap();
    vm.execute(TokenKind::Primary101).unwrap();
    vm.execute(TokenKind::Push).unwrap();
    vm.execute(TokenKind::Primary0).unwrap();
    assert_eq!(vm.primary, 0);
    vm.execute(TokenKind::Pop).unwrap();
    assert_eq!(vm.primary, 101);
    vm.execute(TokenKind::Pop).unwrap();
    assert_eq!(vm.primary, 911);
}

#[test]
fn vm_save_leave() {
    let mut vm = Vm::new();
    vm.execute(TokenKind::Primary911).unwrap();
    vm.execute(TokenKind::Swap).unwrap();
    vm.execute(TokenKind::Primary101).unwrap();
    vm.execute(TokenKind::Save).unwrap();

    // clear registers
    vm.primary = 0;
    vm.secondary = 0;

    vm.execute(TokenKind::Leave).unwrap();
    assert_eq!(vm.primary, 101);
    assert_eq!(vm.secondary, 911);
}

#[test]
#[rustfmt::skip]
fn vm_if() {
    let mut vm = Vm::new();
    vm.run(vec![TokenKind::Main, TokenKind::If, TokenKind::Primary911, TokenKind::EndIf, TokenKind::Eof]);
    assert_eq!(vm.primary, 911);

    vm = Vm::new();
    vm.run(vec![
        TokenKind::Main, TokenKind::Primary911,
        TokenKind::If, // == 0
            TokenKind::Primary0,
        TokenKind::ElseIf, // < 0
            TokenKind::Primary1,
        TokenKind::Else, // > 0
            TokenKind::Primary101,
        TokenKind::EndIf,
        TokenKind::Eof
    ]);
    assert_eq!(vm.primary, 101);

    vm = Vm::new();
    vm.run(vec![
        TokenKind::Main, TokenKind::Dec,
        TokenKind::If, // == 0
            TokenKind::Primary0,
        TokenKind::ElseIf, // < 0
            TokenKind::Primary1,
        TokenKind::Else, // > 0
            TokenKind::Primary101,
        TokenKind::EndIf,
        TokenKind::Eof
    ]);
    assert_eq!(vm.primary, 1);

    vm = Vm::new();
    vm.run(vec![
        TokenKind::Main, TokenKind::Dec,
        TokenKind::If, // == 0
            TokenKind::Primary911,
        TokenKind::Else, // != 0
            TokenKind::Primary101,
        TokenKind::EndIf,
        TokenKind::Eof
    ]);
    assert_eq!(vm.primary, 101);

    vm = Vm::new();
    vm.run(vec![
        TokenKind::Main,
        TokenKind::If, // == 0
            TokenKind::Primary911,
        TokenKind::Else, // != 0
            TokenKind::Primary101,
        TokenKind::EndIf,
        TokenKind::Eof
    ]);
    assert_eq!(vm.primary, 911);
}

#[test]
#[rustfmt::skip]
fn vm_loop() {
    let mut vm = Vm::new();
    vm.run(vec![
        TokenKind::Main,
        TokenKind::Inc, TokenKind::Double, TokenKind::Double, // primary = 4
        TokenKind::LoopStart,
            TokenKind::Swap,
            TokenKind::Inc, // secondary += 1
            TokenKind::Swap,
            TokenKind::Dec, // primary -= 1
        TokenKind::LoopEnd,
        TokenKind::Eof
    ]);
    assert_eq!(vm.primary, 0);
    assert_eq!(vm.secondary, 4);

    vm = Vm::new();
    vm.run(vec![
        TokenKind::Main,
        TokenKind::Inc, TokenKind::Double, TokenKind::Double, // primary = 4
        TokenKind::LoopStart,
            TokenKind::Swap,
            TokenKind::Inc, // secondary += 1
            TokenKind::Swap,
            TokenKind::Dec, // primary -= 1
            TokenKind::Break,
        TokenKind::LoopEnd,
        TokenKind::Eof
    ]);
    assert_eq!(vm.primary, 3);
    assert_eq!(vm.secondary, 1);

    vm = Vm::new();
    vm.run(vec![
        TokenKind::Main,
        TokenKind::Inc, TokenKind::Double, TokenKind::Double, // primary = 4
        TokenKind::LoopStart,
            TokenKind::Swap,
            TokenKind::Inc, // secondary += 1
            TokenKind::Swap,
            TokenKind::Dec, // primary -= 1
            TokenKind::Compare, TokenKind::If,
                TokenKind::Break, // if primary == secondary
            TokenKind::EndIf,
        TokenKind::LoopEnd,
        TokenKind::Eof
    ]);
    assert_eq!(vm.primary, 2);
    assert_eq!(vm.secondary, 2);
}
