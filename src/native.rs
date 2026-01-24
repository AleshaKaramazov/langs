use crate::value::Value;
use std::collections::HashMap;

pub type NativeHandler = Box<dyn Fn(Vec<Value>) -> Result<Value, String>>;

pub struct NativeRegistry {
    pub functions: HashMap<String, NativeHandler>,
}

impl NativeRegistry {
    pub fn new() -> Self {
        Self { functions: HashMap::new() }
    }

    pub fn register<F>(&mut self, name: &str, func: F) 
    where F: Fn(Vec<Value>) -> Result<Value, String> + 'static {
        self.functions.insert(name.to_string(), Box::new(func));
    }
    
    pub fn call(&self, name: &str, args: Vec<Value>) -> Result<Value, String> {
        match self.functions.get(name) {
            Some(func) => func(args),
            None => Err(format!("Нативная функция '{}' не найдена", name)),
        }
    }
}

pub fn convert_val<T: FromValue>(v: &Value) -> Result<T, String> {
    T::from_value(v)
}

#[macro_export]
macro_rules! bind_native {
    ($registry:expr, $name:expr, |$($arg_name:ident : $arg_type:ty),*| $body:block) => {
        $registry.register($name, move |args: Vec<Value>| -> Result<Value, String> {
            let mut _iter = args.into_iter();
            
            $(
                let $arg_name = _iter.next()
                    .ok_or(format!("Недостаточно аргументов для {}", $name))?;
                
                let $arg_name: $arg_type = $crate::native::convert_val(&$arg_name)?;
            )*
            
            let result = $body;
            
            use $crate::value::IntoValue;
            Ok(result.into_value())
        });
    };
}


pub trait FromValue: Sized {
    fn from_value(v: &Value) -> Result<Self, String>;
}

impl FromValue for i64 { fn from_value(v: &Value) -> Result<Self, String> { v.expect_int() } }
impl FromValue for f64 { fn from_value(v: &Value) -> Result<Self, String> { v.expect_float() } }
impl FromValue for bool { fn from_value(v: &Value) -> Result<Self, String> { v.expect_bool() } }
impl FromValue for String { fn from_value(v: &Value) -> Result<Self, String> { 
        v.expect_string().map(|rc_s| (*rc_s).clone()) 
    } 
}
