use std::collections::HashMap;
use std::fmt::{Display, Formatter};

/// returns program without comments and map of line number to comment
pub fn split_comments(s: &str) -> (String, HashMap<usize, String>) {

    let mut clean_code = Vec::new();
    let mut comment_map = HashMap::new();

    let s = s.trim_end();

    for (line_num, text) in s.split('\n').enumerate() {
        let text = text.trim();
        let (s1, s2) = text.split_once("//")
            .map(|pair| {(Some(pair.0), Some(pair.1))})
            .or_else(|| {
            if text.starts_with("//") {Some((None, Some(text)))}
                else {Some((Some(text), None))}
        }).unwrap(); //these Some(Some) look cursed

        clean_code.push( match s1 {
            None => {"".to_string()}
            Some(x) => {x.trim().to_string()}
        });
        if let Some(s2) = s2 {
            comment_map.insert(line_num, s2.trim().to_string());
        }

    }

    (clean_code.join("\n"), comment_map)

}


#[derive(Debug, Clone, PartialEq)]
pub struct Index {
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Plus(Index), Minus(Index), Star(Index), Slash(Index), Modulo(Index),
    LParen(Index), RParen(Index),
    Equals(Index), EqualsEquals(Index),
    GT(Index), GE(Index), LT(Index), LE(Index), NE(Index),

    Number(Index, i64),
    String(Index, String),
    Word(Index, String),
    Print(Index),
    Semicolon(Index),
    Comma(Index),
    Goto(Index),

    Meh(Index),
    Function(Index),

}

impl Token {
    pub fn unwrap_string(&self) -> Option<String> {
        match self {
            Token::Word(_, string) => Some(string.clone()),
            Token::String(_, string) => Some(string.clone()),
            _ => None
        }
    }

    pub fn unwrap_number(&self) -> Option<i64> {
        match self {
            Token::Number(_, n) => Some(*n),
            _ => None
        }
    }

    pub fn get_line(&self) -> usize {
        use Token::*;
        match self {
            Plus(idx)|
            Minus(idx)|
            Star(idx) |
            Slash(idx) |
            Modulo(idx) |
            LParen(idx) |
            RParen(idx) |
            Equals(idx) |
            EqualsEquals(idx) |
            GT(idx) |
            GE(idx) |
            LT(idx) |
            LE(idx) |
            NE(idx) |
            Print(idx) |
            Semicolon(idx) |
            Meh(idx) |
            Comma(idx) |
            Function(idx) |
            Goto(idx) => {idx.line}

            Number(idx, _) => {idx.line}
            Word(idx, _) => {idx.line}

            String(idx, _) => {idx.line}


        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Plus(_) => {write!(f, "+")}
            Token::Minus(_) => {write!(f, "-")}
            Token::Star(_) => {write!(f, "*")}
            Token::Slash(_) => {write!(f, "/")}
            Token::Modulo(_) => {write!(f, "%")}
            Token::LParen(_) => {write!(f, "(")}
            Token::RParen(_) => {write!(f, ")")}
            Token::Equals(_) => {write!(f, "=")}
            Token::EqualsEquals(_) => {write!(f, "==")}
            Token::GT(_) => {write!(f, ">")}
            Token::GE(_) => {write!(f, ">=")}
            Token::LT(_) => {write!(f, "<")}
            Token::LE(_) => {write!(f, "<=")}
            Token::NE(_) => {write!(f, "!=")}
            Token::Number(_, n) => {write!(f, "{}", *n)}
            Token::String(_, s) => {write!(f, "{:?}", s)}
            Token::Word(_, w) => {write!(f, "{}", w)}
            Token::Print(_) => {write!(f, "print")}
            Token::Semicolon(_) => {write!(f, ";")}
            Token::Comma(_) => {write!(f, ",")}
            Token::Goto(_) => {write!(f, "goto")}
            Token::Meh(_) => {write!(f, "meh")}
            Token::Function(_) => {write!(f, "function")}
        }
    }
}

pub fn tokenize(input: &str, line_idx:usize) -> Result<(Vec<(usize, Token, usize)>, usize), String> {
    let mut iterator = input.char_indices().peekable();

    let mut res = vec![];

    let mut lines_inside = 0;

    let mut line_num = line_idx;
    let mut line_start = 0;

    use Token::*;

    while let Some((idx, c)) = iterator.next() {
        let start = idx;

        let index = Index{ line: line_num, column: idx-line_start };


        let token = match c {
            '+' => Plus(index),
            '-' => Minus(index),
            '*' => Star(index),
            '/' => Slash(index),
            '%' => Modulo(index),

            '<' => {
                match iterator.peek() {
                    Some((_, '=')) => {
                        iterator.next();
                        LE(index)
                    }
                    _ => {LT(index)}
                }
            }

            '>' => {
                match iterator.peek() {
                    Some((_, '=')) => {
                        iterator.next();
                        GE(index)
                    }
                    _ => {GT(index)}
                }
            }

            '=' => {
                match iterator.peek() {
                    Some((_, '=')) => {
                        iterator.next();
                        EqualsEquals(index)
                    }
                    _ => {Equals(index)}
                }
            }

            '!' => {
                match iterator.next() {
                    Some((_, '=')) => {
                        NE(index)
                    }

                    x => return match x {
                        Some((_, c2)) => {
                            Err(format!("Unexpected character '{}' after parsing ! at {}:{}", c2, line_num + 1, idx + 1))?
                        }
                        None => { Err(format!("Unexpected end after parsing ! at {}:{}", line_num + 1, idx + 1))? }
                    }
                }
            }

            '(' => LParen(index),
            ')' => RParen(index),

            x if x.is_numeric() => {
                let mut sub = std::string::String::from(x);
                while let Some((_, c)) = iterator.peek() {
                    if c.is_numeric() {
                        sub.push(*c);
                        iterator.next();
                    }else{
                        break;
                    }
                }
                Number(index, sub.parse().unwrap())
            }

            x if x.is_alphabetic() => {
                let mut sub = std::string::String::from(x);
                while let Some((_, c)) = iterator.peek() {
                    if c.is_alphanumeric() {
                        sub.push(*c);
                        iterator.next();
                    }else{
                        break;
                    }
                }

                match sub.as_str() {
                    "print" => {Print(index)}
                    "meh" => {Meh(index)}
                    "function" => {Function(index)}
                    "goto" => {
                        let token = Goto(index.clone());
                        //special goto mode - eveything after space and until ;
                        // is considered label and transformed into token
                        iterator.next(); //skip space
                        let mut goto_label = std::string::String::new();

                        let goto_label_index = Index{
                            line: index.line,
                            column: index.column+5
                        };

                        let mut end= 0;
                        let mut finished = false;
                        while let Some((idx,c)) = iterator.peek() {
                            let c = *c;
                            if c==';' {
                                end = *idx;
                                finished = true;
                                break;
                            }else if c=='\n' {
                                Err("goto labels may only span across single line")?;
                            }

                            else{goto_label.push(c)}
                            iterator.next();
                        }

                        if !finished {
                            Err("expected ; after goto label")?;
                        }

                        let label = Token::Word(goto_label_index, goto_label);

                        res.push((start, token, start+4));
                        res.push((start+5, label, end));
                        continue;
                    }
                    _ => {Word(index, sub)}
                }


            }

            ';' => {Semicolon(index)}

            ',' => Comma(index),

            '"' => {
                //parsing string
                let mut sub = std::string::String::new();
                //also, support escape
                let mut closed = false;
                while let Some((idx, c)) = iterator.peek() {
                    match c {
                        '"' => {closed = true; iterator.next(); break;}
                        '\\' => {
                            iterator.next();

                            match iterator.peek() {
                                None => {break;},
                                Some((_, 't')) => {
                                    sub.push('\t');
                                },

                                Some((_, 'n')) => {
                                    sub.push('\n');
                                },

                                Some((_, '\\')) => {
                                    sub.push('\\');
                                },

                                Some(c) => {
                                    Err(format!("unsupported escape sequence \\{}", c.1))?;
                                }
                            }
                            iterator.next();
                        }

                        '\n' => { //heh, multiline strings? why not? (comments are removed anyway)
                            sub.push(*c);
                            line_num+=1;
                            lines_inside+=1;
                            line_start = *idx;

                        }

                        _ => {sub.push(*c);}
                    }
                    iterator.next();
                }

                if !closed {Err(format!("unclosed string at {}:{}",index.line, index.column))?;}

                String(index, sub)
            }

            ' ' | '\t' => {continue;}

            '\n' => {
                line_num+=1;
                lines_inside+=1;
                line_start = idx+1;
                continue;
            }

            x => Err(format!("unexpected character {} at {}:{}", x, line_num+1, idx+1))?
        };

        let end = iterator.peek().map(|p| {p.0}).unwrap_or(input.len());

        res.push((start, token, end));
    }
    Ok((res, lines_inside))
}
