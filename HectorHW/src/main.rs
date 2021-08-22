//type hints: i is int


//what fist-class is:
//can be parameters of functions
//can be returned
//can be assigned
//can be tested for equality

/*
comments as control flow

let a = input;
let b = 2; //only when $()$


 */

//the lang itself has no (conditional) control flow, only assignment, goto, call and some builtin functions

mod parsing;
mod execution;

fn main() {
    let filename = std::env::args().skip(1).next();
    if let Some(filename) = filename {
        let content = std::fs::read_to_string(filename).expect("error reading file");

        let program = parsing::parse_str(&content).unwrap();

        /*for item in &program {
            println!("{:?}", item);
        }*/
        println!("running:");
        match execution::visitor::run_program(&program){
            Ok(_) => {}
            Err(e) => {println!("error: {}", e);}
        }


    }else {
        println!("usage: condcom <file>")
    }
}
