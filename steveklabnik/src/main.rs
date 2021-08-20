use toml_edit::{Array, Document, Item, Value};

use std::env;
use std::fs;

use std::collections::HashMap;
use std::cell::RefCell;

struct Vm {
    builtin_functions: HashMap<&'static str, Box<dyn Fn(&Array)>>,
    userdefined_functions: HashMap<String, Array>,
    variables: RefCell<HashMap<String, Value>>,
}

impl Vm {
    fn define_builtin_function<F: Fn(&Array) + 'static>(&mut self, name: &'static str, body: F) {
        self.builtin_functions.insert(name, Box::new(body));
    }

    fn define_userdefined_function(&mut self, name: String, body: Array) {
        self.userdefined_functions.insert(name, body);
    }

    fn set_variable(&self, name: String, value: Value) {
        self.variables.borrow_mut().insert(name, value);
    }

    fn call_function(&self, name: &str, args: &Value) {
        if name == "set" {
            self.set_variable(name.to_string(), args.clone());
        } else if let Some(f) = self.builtin_functions.get(name) {
            let args = args.as_array().expect("arguments must be an array");

            f(args);
        } else if let Some(body) = self.userdefined_functions.get(name) {
            let _args = args.as_array().expect("arguments must be an array");

            for line in body.iter() {
                match line {
                    Value::InlineTable(it) => {
                        let mut statement = it.iter();
                        let (name, args) = statement
                            .next()
                            .expect("needs to have at least one function call");

                        self.call_function(name, args);
                    }
                    _ => panic!("the body of functions must be inline tables"),
                }
            }
        } else {
            panic!("Could not find function named {}", name);
        }
    }
}

fn main() {
    let mut vm = Vm {
        builtin_functions: HashMap::new(),
        userdefined_functions: HashMap::new(),
        variables: RefCell::new(HashMap::new()),
    };

    vm.define_builtin_function("comment", |_| {});

    vm.define_builtin_function("put", |args| {
        let mut iter = args.iter();
        let contents = iter.next().expect("must have at least one argument");
        let s = contents.as_str().expect("argument must be a string");

        print!("{}", s);
    });

    vm.define_builtin_function("puts", |args| {
        let mut iter = args.iter();
        let contents = iter.next().expect("must have at least one argument");
        let s = contents.as_str().expect("argument must be a string");

        println!("{}", s);
    });

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

                vm.define_userdefined_function(name, body.clone());
            }
            _ => panic!("body must be a value"),
        }
    }

    let args = Value::Array(Array::default());

    vm.call_function("main", &args);
}
