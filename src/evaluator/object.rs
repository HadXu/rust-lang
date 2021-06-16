use crate::ast::*;
use crate::evaluator::env::*;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;
#[derive(PartialEq, Clone, Debug)]
pub enum Object {
    Int(i64),
    Bool(bool),
    NULL,
    ReturnValue(Box<Object>),
    Error(String),
    Func(Vec<Ident>, BlockStmt, Rc<RefCell<Env>>),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Object::Int(ref value) => write!(f, "{}", value),
            Object::Bool(ref flag) => write!(f, "{}", flag),
            Object::NULL => write!(f, "null"),
            Object::ReturnValue(ref value) => write!(f, "{}", value),
            Object::Error(ref value) => write!(f, "{}", value),
            Object::Func(ref params, _, _) => {
                let mut result = String::new();
                for (i, Ident(ref s)) in params.iter().enumerate() {
                    if i < 1 {
                        result.push_str(&format!("{}", s));
                    } else {
                        result.push_str(&format!(", {}", s));
                    }
                }
                write!(f, "fn({}) {{ ... }}", result)
            }
        }
    }
}
