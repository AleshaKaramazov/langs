use std::collections::HashMap;
use crate::value::Value;

pub struct Env {
    scopes: Vec<HashMap<String, Value>>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn declare(&mut self, name: String, val: Value) {
        if let Some(last) = self.scopes.last() 
            && last.contains_key(&name) {
            panic!("переменная `{}` уже объявлена в этой области видимости", name);
        }
        self.scopes.last_mut().unwrap().insert(name, val);
    }

    pub fn assign(&mut self, name: &str, val: Value) {
        for scope in self.scopes.iter_mut().rev() {
            if scope.contains_key(name) {
                scope.insert(name.to_string(), val);
                return;
            }
        }
        panic!("переменная `{}` не найдена", name);
    }

    pub fn get(&self, name: &str) -> Value {
        for scope in self.scopes.iter().rev() {
            if let Some(v) = scope.get(name) {
                return v.clone();
            }
        }
        panic!("переменная `{}` не найдена", name);
    }
}
