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
        m.insert("Hi, welcome to Chili's", TokenKind::Main);
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
            .map(|(kind, _, stop)| dbg!((*kind, stop)))
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
