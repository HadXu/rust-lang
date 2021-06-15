use std::fmt;
#[derive(PartialEq, Clone, Debug)]
pub enum Object {
    Int(i64),
    Bool(bool),
    NULL,
    ReturnValue(Box<Object>),
    Error(String),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Object::Int(ref value) => write!(f, "{}", value),
            Object::Bool(ref flag) => write!(f, "{}", flag),
            Object::NULL => write!(f, "null"),
            Object::ReturnValue(ref value) => write!(f, "{}", value),
            Object::Error(ref value) => write!(f, "{}", value),
        }
    }
}
