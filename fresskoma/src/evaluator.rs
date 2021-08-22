use crate::parser::AstNode;
use std::collections::HashMap;
use std::rc::Rc;
use crate::parser::AstNode::Integer;

#[derive(Clone)]
pub struct ScopeItem {
    identifier: AstNode,
    value: AstNode,
}

pub struct Scope {
    vars: HashMap<String, ScopeItem>
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            vars: Default::default()
        }
    }
}

pub struct State {
    fns: HashMap<String, Rc<dyn Eval>>,
    arguments: Vec<AstNode>,
    scopes: Vec<Scope>,

}

impl State {
    pub fn new() -> State {
        let mut fns: HashMap<String, Rc<dyn Eval>> = HashMap::new();

        // Built-in functions
        fns.insert("print".into(), Rc::new(BuiltinPrint{}));
        fns.insert("plus".into(), Rc::new(BuiltinPlus{}));
        fns.insert("multiply".into(), Rc::new(BuiltinMultiply{}));
        fns.insert("length".into(), Rc::new(BuiltinLength{}));
        fns.insert("concat".into(), Rc::new(BuiltinConcat{}));

        State {
            fns,
            arguments: vec![],
            // Top-level scope is the global scope
            scopes: vec![Scope::new()]
        }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(Scope::new());
    }

    pub fn pop_scope(&mut self) -> Scope {
        assert!(self.scopes.len() > 1, "Make sure we're not popping the global scope");
        self.scopes.pop().unwrap()
    }

    pub fn set_var(&mut self, name: String, value: ScopeItem) {
        let len = self.scopes.len();
        self.scopes[len-1].vars.insert(name, value);
    }

    pub fn get_var(&self, name: &String) -> ScopeItem {
        let v = self.scopes[self.scopes.len()-1].vars.get(name);
        if v.is_none() {
            panic!("Variable '{}' is not known in the current scope", name)
        }
        v.unwrap().clone()
    }
}

pub trait Eval {
    fn evaluate(&self, state: &mut State) -> AstNode;
}

impl Eval for Vec<AstNode> {
    fn evaluate(&self, state: &mut State) -> AstNode {
        let mut res = AstNode::NoOp;
        for node in self {
            res = node.evaluate(state);
        }
        res
    }
}

impl Eval for AstNode {
    fn evaluate(&self, mut state: &mut State) -> AstNode {
        //println!("{:#?}", self);
        match self {
            AstNode::Call {
                identifier,
                arguments,
            } => {
                state.arguments = arguments.iter().map(|arg| arg.evaluate(state)).collect();
                let function= state.fns.get(identifier);

                if function.is_none() {
                    panic!("Tried to call unknown function '{}'", identifier)
                }

                function.unwrap().clone().evaluate(state)
            }
            AstNode::Integer(_) => self.clone(),
            AstNode::String(_) => self.clone(),

            AstNode::Definition { identifier, arguments, body } => {
                let inv = Invocation {
                    identifier: identifier.into(),
                    arguments: arguments.clone(),
                    body: body.clone()
                };

                state.fns.insert(identifier.into(), Rc::new(inv));
                AstNode::NoOp
            }

            AstNode::Identifier{ name, comment: _comment } => {
                state.get_var(name).value
            }

            AstNode::MetaPropertyAccess {
                lhs,
                rhs: _rhs
            } => {
                //AstNode::String("".into())
                match state.get_var(lhs).identifier {
                    AstNode::Identifier { comment, .. } => {
                        comment.evaluate(state)
                    }
                    x => panic!("Unknown node '{:?}'", x)
                }
            }

            AstNode::Assignment {
                identifier,
                value,
            } => {
                let node = value.as_ref().clone().evaluate(state);
                state.set_var(identifier.into(), ScopeItem { identifier: AstNode::NoOp, value: node });
                AstNode::NoOp
            }

            x => panic!("Unknown node '{:?}'", x)
        }
    }
}

struct Invocation {
    identifier: String,
    arguments: Vec<AstNode>,
    body: Vec<AstNode>,
}

impl Eval for Invocation {
    fn evaluate(&self, state: &mut State) -> AstNode {
        if state.arguments.len() != self.arguments.len() {
            panic!("Function '{}' expected {} arguments, but got {}", self.identifier, self.arguments.len(), state.arguments.len())
        }

        state.push_scope();

        for i in 0..state.arguments.len() {
            let value = state.arguments.get(i).unwrap().clone();

            let arg = self.arguments.get(i).unwrap();
            if let AstNode::Identifier {name, ..} = arg {
                let item = ScopeItem { identifier: arg.clone(), value: value.clone() };
                state.set_var(name.clone(), item);
            } else {
                panic!("Argument was not an AstNode::String {:?}", arg)
            }
        }

        let res = self.body.evaluate(state);
        state.pop_scope();

        res
    }
}

struct BuiltinPrint;
impl Eval for BuiltinPrint {
    fn evaluate(&self, state: &mut State) -> AstNode {
        for arg in &state.arguments {
            println!("# => {:?}", arg)
        }
        AstNode::NoOp
    }
}

struct BuiltinPlus;
impl Eval for BuiltinPlus {
    fn evaluate(&self, state: &mut State) -> AstNode {
        let mut sum:i64 = 0;
        for arg in &state.arguments {
            match arg {
                AstNode::Integer(x) => {
                    sum += x;
                }
                x => panic!("Tried to call plus with invalid value '{:?}'", x)
            }
        }
        AstNode::Integer(sum)
    }
}

struct BuiltinMultiply;
impl Eval for BuiltinMultiply {
    fn evaluate(&self, state: &mut State) -> AstNode {
        let mut res:i64 = 1;
        for arg in &state.arguments {
            match arg {
                AstNode::Integer(x) => {
                    res *= x;
                }
                x => panic!("Tried to call multiply with invalid value '{:?}'", x)
            }
        }
        AstNode::Integer(res)
    }
}

struct BuiltinLength;
impl Eval for BuiltinLength {
    fn evaluate(&self, state: &mut State) -> AstNode {
        assert_eq!(state.arguments.len(), 1, "Tried to call `length` with invalid number of arguments {}", state.arguments.len());
        let arg = state.arguments.get(0).unwrap();

        match arg {
            AstNode::String(x) => {
                Integer(x.len() as i64)
            }
            x => panic!("Tried to call length with invalid value (not a string) '{:?}'", x)
        }
    }
}

struct BuiltinConcat;
impl Eval for BuiltinConcat {
    fn evaluate(&self, state: &mut State) -> AstNode {
        assert!(state.arguments.len() > 1, "`concat` requires two or more arguments");

        let mut res: String = "".into();

        for arg in &state.arguments {
            match arg {
                AstNode::String(x) => {
                    res.push_str(x);
                }
                x => panic!("Tried to call `concat` with invalid value (not a string) '{:?}'", x)
            }
        }

        AstNode::String(res)
    }
}
