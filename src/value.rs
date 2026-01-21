use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Void,
    Int(i64),
    Bool(bool),
    String(String),
    Array(Vec<Value>),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Void => write!(f, "пусто"),
            Self::Int(i) => write!(f, "{}", i),
            Self::Bool(b) => write!(f, "{}", b),
            Self::String(s) => write!(f, "{}", s),
            Self::Array(a) => write!(f, "{:?}", a)
        }
    }
}
