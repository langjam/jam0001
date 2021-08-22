use crate::execution::objects::Object;
use crate::execution::visitor::State;
use std::io::{BufReader, BufRead};
use std::rc::Rc;
use std::io::Stdin;


static mut STDIN: Option<BufReader<Stdin>> = None;

static mut READ_STRING_FN: Option<RcNative> = None;

static mut READ_NUMBER_FN:Option<RcNative> = None;

static mut IS_NUM:Option<RcNative> = None;

static mut IS_STRING:Option<RcNative> = None;

pub(super) fn define_natives(state: &mut State) -> () {
    unsafe {
        maybe_define_statics();

        let read_number_fn = READ_NUMBER_FN.as_ref().unwrap().clone();
        state.vars.insert("readnum".to_string(),
          Object::NativeFunction(read_number_fn, 0));

        state.vars.insert("read".to_string(),
          Object::NativeFunction(READ_STRING_FN.as_mut().unwrap().clone(), 0));

        state.vars.insert("isnum".to_string(),
            Object::NativeFunction(IS_NUM.as_mut().unwrap().clone(), 1));

        state.vars.insert("isstring".to_string(),
          Object::NativeFunction(IS_STRING.as_mut().unwrap().clone(), 1));
    }


    /*state.vars.insert("read".to_string(),
         Object::NativeFunction(read_string_fn.clone(), 0));*/


}

fn maybe_define_statics(){
    unsafe {
        if STDIN.is_none() {
            STDIN = Some(BufReader::new(std::io::stdin()));

            READ_NUMBER_FN = Some(Rc::new(|_o| {
                let mut string = String::new();
                STDIN.as_mut().unwrap().read_line(&mut string).unwrap();
                string.truncate(string.trim_end().len());
                if let Ok(n) = string.parse() {
                    Object::Num(n)
                }else{
                    Object::String("err".to_string())
                }
            }));

            READ_STRING_FN = Some(Rc::new(|_o| {
                let mut string = String::new();
                STDIN.as_mut().unwrap().read_line(&mut string).unwrap();
                string.truncate(string.trim_end().len());
                Object::String(string)
            }));

            IS_NUM = Some(Rc::new(|o|{
                let arg = o.get(0);
                if let Some(x) = arg {
                    match x {

                        Object::Num(_) => {Object::Num(1)}
                        _ => {Object::Num(0)}
                    }
                }else{
                    Object::String("err".to_string())
                }
            }));

            IS_STRING = Some(Rc::new(|o|{
                let arg = o.get(0);
                if let Some(x) = arg {
                    match x {

                        Object::String(_) => {Object::Num(1)}
                        _ => {Object::Num(0)}
                    }
                }else{
                    Object::String("err".to_string())
                }
            }));
        }
    }
}

pub type RcNative = Rc<dyn Fn(Vec<Object>) -> Object>;






