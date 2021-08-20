use toml_edit::{Array, Document, Item, Value};

use std::env;
use std::fs;

use std::collections::HashMap;
use std::cell::RefCell;

#[derive(Debug)]
struct Vm {
    values: RefCell<HashMap<String, Value>>,
}

impl Vm {
    fn set_value(&self, name: String, value: Value) {
        self.values.borrow_mut().insert(name, value);
    }

    fn eval(&self, name: &str, args: &Value) -> Value {
        if name == "puts" {
            match args {
                Value::String(_) => println!("{}", args.as_str().unwrap()),
                _ => panic!("not yet supported"),
            }
            return Value::Array(Array::default());
        }

        let value = self.values.borrow();
        let value = value.get(name).expect("lookup failed");

        match value {
            Value::Array(array) => {
                for i in array.iter() {
                    match i {
                        Value::InlineTable(it) => {
                            for (name, args) in it.iter() {
                                self.eval(name, args);
                            }
                        }
                        _ => panic!("not supported"),
                    }
                }
            }
            _ => panic!("not supported"),
        }

        return Value::Array(Array::default());
    }

    /*
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
    */
}

fn main() {
    let vm = Vm {
        values: RefCell::new(HashMap::new()),
    };

    //vm.define_builtin_function("comment", |_| {});

    /*
    vm.define_builtin_function("put", |args| {
        let mut iter = args.iter();
        let contents = iter.next().expect("must have at least one argument");
        let s = contents.as_str().expect("argument must be a string");

        print!("{}", s);
    });
    */

    /*
    vm.define_builtin_function("puts", |args| {
        let mut iter = args.iter();
        let contents = iter.next().expect("must have at least one argument");
        let s = contents.as_str().expect("argument must be a string");

        println!("{}", s);
    });
    */

    let mut args = env::args();

    // throw away program name
    args.next();

    let path = args
        .next()
        .expect("Please pass a file path as the first argument");

    let toml = fs::read_to_string(path).expect("Could not read the file");

    let doc = toml.parse::<Document>().expect("invalid toml");

    //let (name, item) = doc.iter().next().expect("document was empty");

    for (name, item) in doc.iter() {
        let value = match item {
            Item::Value(value) => value,
            _ => panic!("top level must be a value"),
        } ;

        vm.set_value(name.to_string(), value.clone());
    }

    let args = Value::Array(Array::default());
    vm.eval("main", &args);
}

    /*
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

*/
