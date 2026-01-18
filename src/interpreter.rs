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
            panic!("Точкой входа должен быть Алгоритм 'Главная'");
        }
        self.env.enter_scope();
        self.exec_block(&alg.body);
        self.env.exit_scope();
    }

    fn exec_block(&mut self, stmts: &[Stmt]) -> Option<Value> {
        self.env.enter_scope();
        let mut ret = None;
        for stmt in stmts {
            if let Some(v) = self.exec_stmt(stmt) {
                ret = Some(v);
                break;
            }
        }
        self.env.exit_scope();
        ret
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
            Stmt::AssignAdd { name, expr } => self.exec_compound(name, expr, BinOp::Plus),
            Stmt::AssignSub { name, expr } => self.exec_compound(name, expr, BinOp::Sub),
            Stmt::AssignMult { name, expr } => self.exec_compound(name, expr, BinOp::Mult),
            Stmt::AssignDiv { name, expr } => self.exec_compound(name, expr, BinOp::Div),

            Stmt::If { cond, then_body, else_if, else_body } => {
                if self.is_truthy(cond) {
                    self.exec_block(then_body);
                } else {
                    let mut handled = false;
                    for (elif_cond, elif_body) in else_if {
                        if self.is_truthy(elif_cond) {
                            self.exec_block(elif_body);
                            handled = true;
                            break;
                        }
                    }
                    if !handled {
                        if let Some(body) = else_body {
                            self.exec_block(body);
                        }
                    }
                }
                None
            }

            Stmt::While { cond, body } => {
                while self.is_truthy(cond) {
                    if let Some(v) = self.exec_block(body) {
                        return Some(v);
                    }
                }
                None
            }

           Stmt::For { var, start, end, body } => {
                let start_val_raw = self.eval_expr(start);
                let end_val_raw = self.eval_expr(end);

                let start_val = self.get_int(start_val_raw);
                let end_val = self.get_int(end_val_raw);

                self.env.enter_scope();
                for i in start_val..=end_val {
                    self.env.declare(var.clone(), Value::Int(i));
                    if let Some(v) = self.exec_block(body) {
                        self.env.exit_scope();
                        return Some(v);
                    }
                }
                self.env.exit_scope();
                None
            }
            Stmt::Expr(expr) => {
                self.eval_expr(expr);
                None
            }
        }
    }

    fn exec_compound(&mut self, name: &str, expr: &Expr, op: BinOp) -> Option<Value> {
        let left = self.env.get(name);
        let right = self.eval_expr(expr);
        let result = self.eval_binary_values(left, right, &op);
        self.env.assign(name, result);
        None
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
                self.eval_binary_values(l, r, op)
            }
            Expr::Call { name, args, intrinsic } => {
                if *intrinsic {
                    self.call_intrinsic(name, args)
                } else {
                    panic!("Пользовательские функции пока не реализованы");
                }
            }
        }
    }

    fn eval_binary_values(&self, l: Value, r: Value, op: &BinOp) -> Value {
        match (l, r, op) {
            (Value::Int(a), Value::Int(b), BinOp::Plus) => Value::Int(a + b),
            (Value::Int(a), Value::Int(b), BinOp::Sub) => Value::Int(a - b),
            (Value::Int(a), Value::Int(b), BinOp::Mult) => Value::Int(a * b),
            (Value::Int(a), Value::Int(b), BinOp::Div) => {
                if b == 0 { panic!("Деление на ноль"); }
                Value::Int(a / b)
            }
            (Value::Int(a), Value::Int(b), BinOp::Mod) => Value::Int(a % b),
            (Value::Int(a), Value::Int(b), BinOp::Equal) => Value::Bool(a == b),
            (Value::Int(a), Value::Int(b), BinOp::Less) => Value::Bool(a < b),
            (Value::Int(a), Value::Int(b), BinOp::Greater) => Value::Bool(a > b),
            
            (Value::String(a), Value::String(b), BinOp::Plus) => Value::String(a + &b), 
            (Value::String(a), Value::String(b), BinOp::Equal) => Value::Bool(a == b),
            
            (Value::Bool(a), Value::Bool(b), BinOp::Or) => Value::Bool(a || b),
            (Value::Bool(a), Value::Bool(b), BinOp::Equal) => Value::Bool(a == b),

            (l, r, op) => panic!("Недопустимая операция {:?} между {:?} и {:?}", op, l, r),
        }
    }

    fn is_truthy(&mut self, expr: &Expr) -> bool {
        match self.eval_expr(expr) {
            Value::Bool(b) => b,
            _ => panic!("Условие должно быть логическим (Истина/Ложь)"),
        }
    }

    fn get_int(&self, val: Value) -> i64 {
        match val {
            Value::Int(i) => i,
            _ => panic!("Ожидалось число, получено {}", val),
        }
    }

    fn call_intrinsic(&mut self, name: &str, args: &[Expr]) -> Value {
        match name {
            "написать" => {
                let vals: Vec<Value> = args.iter().map(|e| self.eval_expr(e)).collect();
                self.print_values(vals);
                Value::Int(0)
            }
            "считать" => {
                let prompt = if !args.is_empty() {
                    match self.eval_expr(&args[0]) {
                        Value::String(s) => s,
                        _ => String::new(),
                    }
                } else {
                    String::new()
                };
                print!("{}", prompt);
                io::stdout().flush().unwrap();
                
                let mut input = String::new();
                io::stdin().read_line(&mut input).unwrap();
                let input = input.trim();
                
                if let Ok(i) = input.parse::<i64>() {
                    Value::Int(i)
                } else {
                    Value::String(input.to_string())
                }
            }
            _ => panic!("Неизвестная функция: {}", name),
        }
    }

    fn print_values(&self, values: Vec<Value>) {
        if values.is_empty() {
            println!();
            return;
        }
        let mut iter = values.iter();
        if let Some(first) = iter.next() {
            if let Value::String(fmt) = first {
                let mut res = String::new();
                let mut chars = fmt.chars().peekable();
                while let Some(c) = chars.next() {
                    if c == '{' && chars.peek() == Some(&'}') {
                        chars.next();
                        if let Some(arg) = iter.next() {
                            res.push_str(&format!("{}", arg));
                        } else {
                            res.push_str("{}");
                        }
                    } else {
                        res.push(c);
                    }
                }
                println!("{}", res);
            } else {
                print!("{}", first);
                for v in iter {
                    print!(" {}", v);
                }
                println!();
            }
        }
    }
}
