use toml_edit::{Array, Document, Item, Value};

use std::env;
use std::fs;

use std::collections::HashMap;

fn main() {
    let mut functions: HashMap<&'static str, Box<dyn Fn(&Array)>> = HashMap::new();

    functions.insert(
        "put",
        Box::new(|args| {
            let mut iter = args.iter();
            let contents = iter.next().expect("must have at least one argument");
            let s = contents.as_str().expect("argument must be a string");

            print!("{}", s);
        }),
    );

    functions.insert(
        "puts",
        Box::new(|args| {
            let mut iter = args.iter();
            let contents = iter.next().expect("must have at least one argument");
            let s = contents.as_str().expect("argument must be a string");

            println!("{}", s);
        }),
    );

    let mut args = env::args();

    // throw away program name
    args.next();

    let path = args
        .next()
        .expect("Please pass a file path as the first argument");

    let toml = fs::read_to_string(path).expect("Could not read the file");

    let doc = toml.parse::<Document>().expect("invalid doc");

    let (name, aot) = doc.iter().next().expect("document was empty");

    if name != "function" {
        panic!("must have an array of tables named 'function'");
    }

    let aot = match aot {
        Item::ArrayOfTables(aot) => aot,
        _ => panic!("Must be an array of tables"),
    };

    for table in aot.iter() {
        /*
        let table = aot
            .iter()
            .next()
            .expect("array of tables must not be empty");
            */

        if !table.contains_key("name") {
            panic!("function must have a name");
        }

        let item = table.get("name").unwrap();

        match item {
            Item::Value(value) => {
                if value.as_str() != Some("main") {
                    panic!("function must be named main");
                }
            }
            _ => panic!("name must be a string"),
        };

        // we have now validated that we have a table of functions and our first
        // one is named 'main'

        let item = table.get("body").expect("must have a body");

        match item {
            Item::Value(value) => {
                let a = value.as_array().expect("body must be an array");
                for line in a.iter() {
                    match line {
                        Value::InlineTable(it) => {
                            let mut statement = it.iter();
                            let (name, args) = statement
                                .next()
                                .expect("needs to have at least one function call");

                            let f = functions
                                .get(name)
                                .expect("could not find a function with that name");

                            let args = args.as_array().expect("arguments must be an array");

                            f(args);
                        }
                        _ => panic!("the body of functions must be inline tables"),
                    }
                }
            }
            _ => panic!("body must be a value"),
        }
    }

    //dbg!(item);
}
