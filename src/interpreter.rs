use crate::ast::*;
use crate::env::Env;
use crate::value::Value;
use std::io::{self, Write};

pub struct Interpreter {
    env: Env,
}

impl Interpreter {
    pub fn new() -> Self {
        Self { env: Env::new() }
    }

    pub fn run(&mut self, alg: &Algorithm) {
        if alg.name != "Главная" {
            panic!("точкой входа должен быть Алгоритм Главная");
        }

        self.env.enter_scope();
        self.exec_block(&alg.body);
        self.env.exit_scope();
    }

    fn exec_block(&mut self, stmts: &[Stmt]) -> Option<Value> {
        self.env.enter_scope();
        for stmt in stmts {
            if let Some(v) = self.exec_stmt(stmt) {
                self.env.exit_scope();
                return Some(v);
            }
        }
        self.env.exit_scope();
        None
    }

    fn exec_stmt(&mut self, stmt: &Stmt) -> Option<Value> {
        match stmt {
            Stmt::Let { name, expr, .. } => {
                let val = self.eval_expr(expr);
                self.env.declare(name.clone(), val);
                None
            }

            Stmt::Assign { name, expr } => {
                let val = self.eval_expr(expr);
                self.env.assign(name, val);
                None
            }

            Stmt::AssignAdd { name, expr } => {
                let left = self.env.get(name);
                let right = self.eval_expr(expr);
                match (left, right) {
                    (Value::Int(a), Value::Int(b)) => {
                        self.env.assign(name, Value::Int(a + b));
                    }
                    _ => panic!("+= возможно только для целых чисел"),
                }
                None
            }
            Stmt::AssignSub { name, expr } => {
                let left = self.env.get(name);
                let right = self.eval_expr(expr);
                match (left, right) {
                    (Value::Int(a), Value::Int(b)) => {
                        self.env.assign(name, Value::Int(a - b));
                    }
                    _=> panic!("оператор -= можно использовать только для 2х чисел")
                }
                None
            }
            Stmt::AssignMult { name, expr } => {
                let left = self.env.get(name);
                let right = self.eval_expr(expr);
                match (left, right) {
                    (Value::Int(a), Value::Int(b)) => {
                        self.env.assign(name, Value::Int(a * b));
                    }
                    _=> panic!("оператор *= можно использовать только для 2х чисел")
                }
                None
            }
            Stmt::AssignDiv { name, expr } => {
                let left = self.env.get(name);
                let right = self.eval_expr(expr);
                match (left, right) {
                    (Value::Int(a), Value::Int(b)) => {
                        self.env.assign(name, Value::Int(a / b));
                    }
                    _=> panic!("оператор *= можно использовать только для 2х чисел")
                }
                None
            }
            Stmt::For { var, start, end, body } => {
                let start_val = self.eval_expr(start);
                let end_val = self.eval_expr(end);

                let start = match start_val {
                    Value::Int(v) => v,
                    _ => panic!("начало цикла должно быть числом"),
                };

                let end = match end_val {
                    Value::Int(v) => v,
                    _ => panic!("конец цикла должно быть числом"),
                };

                self.env.enter_scope();

                for i in start..=end {
                    self.env.define(var.clone(), Value::Int(i));
                    self.exec_block(body);
                }

                self.env.exit_scope();
                None
            }

                        
            Stmt::If {
                cond,
                then_body,
                else_if,
                else_body,
            } => {
                if self.eval_expr(cond) == Value::Bool(true) {
                    self.exec_block(then_body);
                    return None;
                }

                for (cond, body) in else_if {
                    if self.eval_expr(cond) == Value::Bool(true) {
                        self.exec_block(body);
                        return None;
                    }
                }

                if let Some(body) = else_body {
                    self.exec_block(body);
                }

                None
            }


            Stmt::While { cond, body } => {
                while self.eval_expr(cond) == Value::Bool(true) {
                    if let Some(v) = self.exec_block(body) {
                        return Some(v);
                    }
                }
                None
            }

            Stmt::Expr(expr) => {
                self.eval_expr(expr);
                None
            }
        }
    }
    

    fn eval_expr(&mut self, expr: &Expr) -> Value {
        match expr {
            Expr::Int(v) => Value::Int(*v),
            Expr::Bool(b) => Value::Bool(*b),
            Expr::String(s) => Value::String(s.clone()),
            Expr::Var(name) => self.env.get(name),
            Expr::Binary { left, op, right } => {
                let l = self.eval_expr(left);
                let r = self.eval_expr(right);
                self.eval_binary(l, r, op)
            }

            Expr::Call {
                name,
                args,
                intrinsic,
            } => {
                if *intrinsic {
                    self.call_intrinsic(name, args)
                } else {
                    panic!("пользовательские функции пока не реализованы");
                }
            }
        }
    }

    fn eval_binary(&self, l: Value, r: Value, op: &BinOp) -> Value {
        match (l, r, op) {
            (Value::Int(a), Value::Int(b), BinOp::Equal) => Value::Bool(a == b),
            (Value::Int(a), Value::Int(b), BinOp::Greater) => Value::Bool(a > b),
            (Value::Int(a), Value::Int(b), BinOp::Less) => Value::Bool(a < b),
            (Value::Int(a), Value::Int(b), BinOp::Mod) => Value::Int(a % b),
            (Value::Bool(a), Value::Bool(b), BinOp::Or) => Value::Bool(a || b),
            (Value::Int(a), Value::Int(b), BinOp::Plus) => Value::Int(a + b),
            (Value::Int(a), Value::Int(b), BinOp::Mult) => Value::Int(a * b),
            (Value::Int(a), Value::Int(b), BinOp::Div) => Value::Int(a / b),
            (Value::String(a), Value::String(b), BinOp::Equal) => Value::Bool(a == b),
            (Value::Int(a), Value::Int(b), BinOp::Sub) => Value::Int(a - b),
            _ => panic!("некорректная бинарная операция"),
        }
    }

    fn call_intrinsic(&mut self, name: &str, args: &[Expr]) -> Value {
        match name {
            "написать" => {
                let values:Vec<Value> =
                    args.iter().map(|e| self.eval_expr(e)).collect();
                self.print(values);
                Value::Int(0)
            }

            "считать" => {
                let prompt = if !args.is_empty() {
                    match self.eval_expr(&args[0]) {
                        Value::String(s) => s,
                        _ => panic!("считать! ожидает строку"),
                    }
                } else {
                    String::new()
                };

                print!("{}", prompt);
                let _ = io::stdout().flush();

                let mut input = String::new();
                io::stdin().read_line(&mut input).unwrap();
                let input = input.trim();

                if let Ok(v) = input.parse::<i64>() {
                    Value::Int(v)
                } else {
                    Value::String(input.to_string())
                }
            }

            _ => panic!("неизвестная встроенная функция {}", name),
        }
    }

    fn print(&self, values: Vec<Value>) {
        if values.is_empty() {
            println!();
            return;
        }

        let fmt;
        let args;

        if let Value::String(s) = &values[0] {
            fmt = s.clone();
            args = values[1..].to_vec();
        } else {
            fmt = "{}".to_string();
            args = values;
        }

        let mut result = String::new();
        let mut arg_iter = args.iter();

        let mut chars = fmt.chars().peekable();
        while let Some(c) = chars.next() {
            if c == '{' && chars.peek() == Some(&'}') {
                chars.next();
                if let Some(v) = arg_iter.next() {
                    result.push_str(&format!("{}", v));
                }
            } else {
                result.push(c);
            }
        }

        println!("{}", result);
    }
}
