use crate::tokenizer::{Token, Tokens, Variables};
enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
}

fn process_operator(op: &mut Option<Operator>, current_value: &mut Option<f64>, value: f64) {
    match op {
        Some(op) => match current_value {
            Some(prev_value) => match op {
                Operator::Add => *current_value = Some(*prev_value + value),
                Operator::Subtract => *current_value = Some(*prev_value - value),
                Operator::Multiply => *current_value = Some(*prev_value * value),
                Operator::Divide => *current_value = Some(*prev_value / value),
            },
            None => *current_value = Some(value),
        },
        None => *current_value = Some(value),
    }
}

pub fn evaluate(tokens: Tokens, mut vars: Variables) -> String {
    let mut output = String::new();
    let mut tokens = tokens.into_iter().peekable();
    let mut current_value: Option<f64> = None;
    let mut assignment: Option<String> = None;
    let mut last_op: Option<Operator> = None;
    while let Some(token) = tokens.next() {
        match token {
            Token::Comment(string) => {
                output.push_str(&format!("// {}\n", &string));
            }
            Token::EndOfExpression => {
                if let Some(value) = current_value {
                    output.push_str(&format!("// {}\n", value));
                    if let Some(string) = &assignment {
                        match vars.get_mut(string) {
                            Some(var) => {
                                *var = current_value.expect("value");
                            }
                            None => {
                                vars.insert(string.clone(), current_value.expect("value"));
                            }
                        }
                    }
                    current_value = None;
                    last_op = None;
                    assignment = None;
                }
            }
            Token::Symbol(string) => {
                if let Some(Token::Assignment) = tokens.peek() {
                    output.push_str(&format!("{}: ", &string));
                    if current_value.is_none() {
                        if let Some(Token::Assignment) = tokens.peek() {
                            tokens.next();
                            assignment = Some(string);
                        }
                    }
                } else {
                    output.push_str(&format!("{} ", &string));
                    match vars.get(&string) {
                        Some(value) => process_operator(&mut last_op, &mut current_value, *value),
                        None => panic!("Cannot find var {}", string),
                    }
                }
            }
            Token::Number(value) => {
                output.push_str(&format!("{} ", value.to_string()));
                process_operator(&mut last_op, &mut current_value, value);
            }
            Token::Assignment => unreachable!(""),
            Token::Add => {
                output.push_str("+ ");
                last_op = Some(Operator::Add);
            }
            Token::Subtract => {
                output.push_str("- ");
                last_op = Some(Operator::Subtract);
            }
            Token::Multiply => {
                output.push_str("* ");
                last_op = Some(Operator::Multiply);
            }
            Token::Divide => {
                output.push_str("/ ");
                last_op = Some(Operator::Divide);
            }
            Token::Sin(value) => {
                output.push_str(&format!("sin {} ", value));
                process_operator(&mut last_op, &mut current_value, f64::sin(value));
            }
            Token::SinSym(string) => {
                output.push_str(&format!("sin {} ", string));
                match vars.get(&string) {
                    Some(value) => {
                        process_operator(&mut last_op, &mut current_value, f64::sin(*value));
                    }
                    None => panic!("Cannot find var {}", string),
                }
            }
            Token::Cos(value) => {
                output.push_str(&format!("cos {} ", value));
                process_operator(&mut last_op, &mut current_value, f64::cos(value));
            }
            Token::CosSym(string) => {
                output.push_str(&format!("cos {} ", string));
                match vars.get(&string) {
                    Some(value) => {
                        process_operator(&mut last_op, &mut current_value, f64::cos(*value));
                    }
                    None => panic!("Cannot find var {}", string),
                }
            }
            Token::Tan(value) => {
                output.push_str(&format!("tan {} ", value));
                process_operator(&mut last_op, &mut current_value, f64::tan(value));
            }
            Token::TanSym(string) => {
                output.push_str(&format!("tan {} ", string));
                match vars.get(&string) {
                    Some(value) => {
                        process_operator(&mut last_op, &mut current_value, f64::tan(*value));
                    }
                    None => panic!("Cannot find var {}", string),
                }
            }
            Token::Floor => {
                output.push_str("floor ");
                match current_value {
                    Some(value) => current_value = Some(value.floor()),
                    None => panic!("no value to floor!"),
                }
            }
            Token::Ceil => {
                output.push_str("ceil ");
                match current_value {
                    Some(value) => current_value = Some(value.ceil()),
                    None => panic!("no value to ceil!"),
                }
            }
            Token::Rounded => {
                output.push_str("rounded ");
                match current_value {
                    Some(value) => current_value = Some(value.round()),
                    None => panic!("no value to round!"),
                }
            }
            Token::Mod(modulo) => {
                output.push_str(&format!("mod {} ", modulo));
                match current_value {
                    Some(value) => current_value = Some((value as i64 % modulo) as f64),
                    None => panic!("no value to mod!"),
                }
            }
            Token::Newline => {
                output.push('\n');
            }
        }
    }
    output
}
