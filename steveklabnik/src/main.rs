use toml_edit::{Array, Document, Item, Value};

use std::env;
use std::fs;

use std::collections::HashMap;

fn main() {
    let mut builtin_functions: HashMap<&'static str, Box<dyn Fn(&Array)>> = HashMap::new();

    builtin_functions.insert(
        "put",
        Box::new(|args| {
            let mut iter = args.iter();
            let contents = iter.next().expect("must have at least one argument");
            let s = contents.as_str().expect("argument must be a string");

            print!("{}", s);
        }),
    );

    builtin_functions.insert(
        "puts",
        Box::new(|args| {
            let mut iter = args.iter();
            let contents = iter.next().expect("must have at least one argument");
            let s = contents.as_str().expect("argument must be a string");

            println!("{}", s);
        }),
    );

    let mut userdefined_functions: HashMap<String, Array> = HashMap::new();

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
        let item = table.get("name").expect("function must have a name");

        let name = match item {
            Item::Value(value) => value
                .as_str()
                .expect("function name must be a string")
                .to_string(),
            _ => panic!("name must be a string"),
        };

        let item = table.get("body").expect("function must have a body");

        match item {
            Item::Value(value) => {
                let body = value.as_array().expect("body must be an array");

                userdefined_functions.insert(name, body.clone());
            }
            _ => panic!("body must be a value"),
        }
    }

    let args = Value::Array(Array::default());
    call_function("main", &args, &userdefined_functions, &builtin_functions);
}

fn call_function(
    name: &str,
    args: &Value,
    userdefined_functions: &HashMap<String, Array>,
    builtin_functions: &HashMap<&'static str, Box<dyn Fn(&Array)>>,
) {
    if let Some(f) = builtin_functions.get(name) {
        let args = args.as_array().expect("arguments must be an array");

        f(args);
    } else if let Some(body) = userdefined_functions.get(name) {
        let _args = args.as_array().expect("arguments must be an array");

        for line in body.iter() {
            match line {
                Value::InlineTable(it) => {
                    let mut statement = it.iter();
                    let (name, args) = statement
                        .next()
                        .expect("needs to have at least one function call");

                    call_function(name, args, userdefined_functions, builtin_functions);
                }
                _ => panic!("the body of functions must be inline tables"),
            }
        }
    } else {
        panic!("Could not find function named {}", name);
    }
}
