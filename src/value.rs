use std::fmt;
use crate::ast::Expr; // <-- Добавить импорт
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Void,
    Closure {
        param: String,
        body: Expr,
        env: Vec<HashMap<String, Value>> 
    },
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Array(Vec<Value>),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Closure { .. } => write!(f, "<функция>"), 
            Self::Void => write!(f, "пусто"),
            Self::Float(fl) => write!(f, "{}", fl),
            Self::Int(i) => write!(f, "{}", i),
            Self::Bool(b) => write!(f, "{}", b),
            Self::String(s) => write!(f, "{}", s),
            Self::Array(a) => write!(f, "{:?}", a)
        }
    }
}

impl Value {
    pub fn expect_int(&self) -> Result<i64, String> {
        match self {
            Value::Int(v) => Ok(*v),
            _ => Err(format!("Ожидалось Цел, получено {}", self)),
        }
    }

    pub fn expect_float(&self) -> Result<f64, String> {
        match self {
            Value::Float(v) => Ok(*v),
            Value::Int(v) => Ok(*v as f64),
            _ => Err(format!("Ожидалось Десятич, получено {}", self)),
        }
    }

    pub fn expect_bool(&self) -> Result<bool, String> {
        match self {
            Value::Bool(v) => Ok(*v),
            _ => Err(format!("Ожидалось Лог, получено {}", self)),
        }
    }

    pub fn expect_string(&self) -> Result<String, String> {
        match self {
            Value::String(v) => Ok(v.clone()),
            _ => Err(format!("Ожидалась Строка, получено {}", self)),
        }
    }
}

pub trait IntoValue {
    fn into_value(self) -> Value;
}

impl IntoValue for i64 { fn into_value(self) -> Value { Value::Int(self) } }
impl IntoValue for f64 { fn into_value(self) -> Value { Value::Float(self) } }
impl IntoValue for bool { fn into_value(self) -> Value { Value::Bool(self) } }
impl IntoValue for String { fn into_value(self) -> Value { Value::String(self) } }
impl IntoValue for () { fn into_value(self) -> Value { Value::Void } }
