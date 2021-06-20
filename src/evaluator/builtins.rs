use crate::evaluator::object::*;
use std::collections::HashMap;

pub fn new_builtins() -> HashMap<String, Object> {
    let mut builtins = HashMap::new();
    builtins.insert(String::from("len"), Object::Builtin(1, monkey_len));
    builtins.insert(String::from("first"), Object::Builtin(1, monkey_first));
    builtins.insert(String::from("last"), Object::Builtin(1, monkey_last));
    builtins.insert(String::from("push"), Object::Builtin(2, monkey_push));
    builtins
}

fn monkey_len(args: Vec<Object>) -> Object {
    match &args[0] {
        Object::String(s) => Object::Int(s.len() as i64),
        Object::Array(a) => Object::Int(a.len() as i64),
        o => Object::Error(format!("argument to `len` not supported, got {}", o)),
    }
}

fn monkey_first(args: Vec<Object>) -> Object {
    match &args[0] {
        Object::Array(o) => o.first().unwrap().clone(),
        o => Object::Error(format!("argument to `len` not supported, got {}", o)),
    }
}


fn monkey_last(args: Vec<Object>) -> Object {
    match &args[0] {
        Object::Array(o) => o.last().unwrap().clone(),
        o => Object::Error(format!("argument to `len` not supported, got {}", o)),
    }
}

fn monkey_push(args: Vec<Object>) -> Object {
    match &args[0] {
        Object::Array(o) => {
            let mut arr = o.clone();
            arr.push(args[1].clone());
            Object::Array(arr)
        },
        o => Object::Error(format!("argument to `len` not supported, got {}", o)),
    }
}


