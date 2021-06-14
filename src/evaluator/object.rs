use std::fmt;
#[derive(PartialEq, Clone, Debug)]
pub enum Object {
    Int(i64),
    Bool(bool),
    NULL,
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Object::Int(ref value) => write!(f, "{}", value),
            Object::Bool(ref flag) => write!(f, "{}", flag),
            Object::NULL => write!(f, "null"),
        }
    }
}
