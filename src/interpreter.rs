use crate::ast::*;
use crate::env::Env;
use crate::native::NativeRegistry;
use crate::value::Value;
use std::{
    cell::RefCell,
    collections::HashMap,
    io::{self, Write},
    rc::Rc,
};

type RuntimeResult<T> = Result<T, String>;

pub struct Interpreter {
    env: Env,
    functions: HashMap<String, Rc<Algorithm>>,
    native: NativeRegistry,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut native = NativeRegistry::new();
        Self::math_module(&mut native);

        Self {
            env: Env::new(),
            functions: HashMap::new(),
            native,
        }
    }

    pub fn math_module(native: &mut NativeRegistry) {
        bind_native!(native, "Степень", |base: i64, exp: i64| {
            if exp < 0 { 0 } else { base.pow(exp as u32) }
        });

        bind_native!(native, "ПИ", || { std::f64::consts::PI });
        bind_native!(native, "Корень2", |val: f64| { val.sqrt() });
    }

    pub fn run(&mut self, prog: &Program) -> RuntimeResult<()> {
        for alg in &prog.algorithms {
            self.functions
                .insert(alg.name.clone(), Rc::new(alg.clone()));
        }

        let main_alg = self
            .functions
            .get("Главная")
            .ok_or("В программе должен быть Алгоритм 'Главная'".to_string())?
            .clone();
        self.exec_function(&main_alg, vec![])?;
        Ok(())
    }

    fn exec_function(&mut self, alg: &Algorithm, args: Vec<Value>) -> RuntimeResult<Value> {
        self.env.enter_scope();

        for ((name, _), val) in alg.args.iter().zip(args.into_iter()) {
            self.env.declare(name.clone(), val);
        }

        let mut result = Value::Void;
        if let Some(ret_val) = self.exec_block(&alg.body)? {
            result = ret_val;
        }

        self.env.exit_scope();
        Ok(result)
    }

    fn exec_block(&mut self, stmts: &[Stmt]) -> RuntimeResult<Option<Value>> {
        self.env.enter_scope();
        let mut ret = None;

        for stmt in stmts {
            if let Some(v) = self.exec_stmt(stmt)? {
                ret = Some(v);
                break;
            }
        }

        self.env.exit_scope();
        Ok(ret)
    }

    fn exec_stmt(&mut self, stmt: &Stmt) -> RuntimeResult<Option<Value>> {
        match stmt {
            Stmt::Return(maybe_expr) => {
                let val = match maybe_expr {
                    Some(expr) => self.eval_expr(expr)?,
                    None => Value::Void,
                };
                Ok(Some(val))
            }
            Stmt::Let { name, expr, .. } => {
                let val = self.eval_expr(expr)?;
                self.env.declare(name.clone(), val);
                Ok(None)
            }
            Stmt::Assign { name, expr } => {
                let val = self.eval_expr(expr)?;
                self.env.assign(name, val);
                Ok(None)
            }
            // Сахар: x += 1
            Stmt::AssignAdd { name, expr } => self.exec_compound_assign(name, expr, BinOp::Plus),
            Stmt::AssignSub { name, expr } => self.exec_compound_assign(name, expr, BinOp::Sub),
            Stmt::AssignMult { name, expr } => self.exec_compound_assign(name, expr, BinOp::Mult),
            Stmt::AssignDiv { name, expr } => self.exec_compound_assign(name, expr, BinOp::Div),

            Stmt::If {
                cond,
                then_body,
                else_body,
                ..
            } => {
                let val = self.eval_expr(cond)?;
                if let Value::Bool(true) = val {
                    if let Some(r) = self.exec_block(then_body)? {
                        return Ok(Some(r));
                    }
                } else if let Some(else_b) = else_body
                    && let Some(r) = self.exec_block(else_b)?
                {
                    return Ok(Some(r));
                }
                Ok(None)
            }
            Stmt::ForEach {
                var,
                collection,
                body,
            } => {
                let coll_val = self.eval_expr(collection)?;

                if let Value::Array(elements_rc) = coll_val {
                    let elements = elements_rc.borrow().clone();

                    for val in elements {
                        self.env.enter_scope();
                        self.env.declare(var.clone(), val);
                        let result = self.exec_block(body)?;
                        self.env.exit_scope();

                        if let Some(ret) = result {
                            return Ok(Some(ret));
                        }
                    }
                } else {
                    return Err("Ожидался массив для итерации".to_string());
                }
                Ok(None)
            }
            Stmt::While { cond, body } => {
                loop {
                    let cond_val = self.eval_expr(cond)?;
                    if !self.is_truthy(&cond_val)? {
                        break;
                    }
                    if let Some(ret) = self.exec_block(body)? {
                        return Ok(Some(ret));
                    }
                }
                Ok(None)
            }
            Stmt::For {
                var,
                start,
                cont,
                end,
                body,
            } => {
                let start_val = self.eval_expr(start)?;
                let end_val = self.eval_expr(end)?;

                match (start_val, end_val) {
                    (Value::Int(s), Value::Int(mut e)) => {
                        self.env.enter_scope();
                        self.env.declare(var.clone(), Value::Int(s));
                        if !cont {
                            e -= 1
                        }
                        for i in s..=e {
                            self.env.assign(var, Value::Int(i));
                            if let Some(ret) = self.exec_block(body)? {
                                self.env.exit_scope();
                                return Ok(Some(ret));
                            }
                        }
                        self.env.exit_scope();
                        Ok(None)
                    }
                    (t1, t2) => Err(format!(
                        "Границы цикла 'для' должны быть Цел, получено {} и {}",
                        t1, t2
                    )),
                }
            }
            Stmt::Expr(expr) => {
                self.eval_expr(expr)?;
                Ok(None)
            }
        }
    }

    fn exec_compound_assign(
        &mut self,
        name: &str,
        expr: &Expr,
        op: BinOp,
    ) -> RuntimeResult<Option<Value>> {
        let current_val = self.env.get(name);
        let operand_val = self.eval_expr(expr)?;

        let new_val = self.apply_binary_op(current_val, operand_val, op)?;
        self.env.assign(name, new_val);
        Ok(None)
    }

    fn eval_expr(&mut self, expr: &Expr) -> RuntimeResult<Value> {
        match expr {
            Expr::Int(i) => Ok(Value::Int(*i)),
            Expr::Float(f) => Ok(Value::Float(*f)),
            Expr::Char(c) => Ok(Value::Char(*c)),
            Expr::Bool(b) => Ok(Value::Bool(*b)),
            Expr::String(s) => Ok(Value::String(Rc::new(s.clone()))),
            Expr::Lambda { param, body, .. } => Ok(Value::Closure {
                param: param.clone(),
                body: body.clone(),
                env: self.env.snapshot(),
            }),
            Expr::NativeCall { path, args } => {
                let mut evaluated_args = Vec::new();
                for arg in args {
                    evaluated_args.push(self.eval_expr(arg)?);
                }

                self.native.call(path, evaluated_args)
            }
            Expr::Var(name) => Ok(self.env.get(name)),
            Expr::MethodCall {
                target,
                method,
                args,
            } => {
                if method == "Где" {
                    let target_val = self.eval_expr(target)?;
                    let closure_val = self.eval_expr(&args[0])?;

                    if let (Value::Array(elements_rc), Value::Closure { param, body, env }) =
                        (target_val, closure_val)
                    {
                        let mut res_arr = Vec::new();
                        let elements = elements_rc.borrow();

                        for item in elements.iter() {
                            let old_scopes = self.env.replace_scopes(env.clone());
                            self.env.enter_scope();
                            self.env.declare(param.clone(), item.clone());

                            let res = self.eval_expr(&body)?;

                            self.env.exit_scope();
                            self.env.replace_scopes(Rc::new(old_scopes));

                            if let Value::Bool(true) = res {
                                res_arr.push(item.clone());
                            }
                        }
                        return Ok(Value::Array(Rc::new(RefCell::new(res_arr))));
                    }
                } else if method == "Добавить"
                    && let Expr::Var(name) = &**target
                {
                    let item = self.eval_expr(&args[0])?;
                    let val = self.env.get(name);

                    if let Value::Array(arr_rc) = val {
                        arr_rc.borrow_mut().push(item);
                        return Ok(Value::Void);
                    } else {
                        return Err("Метод 'Добавить' вызван не у массива".to_string());
                    }
                } else if method == "Содержит" {
                    let target_val = self.eval_expr(target)?;
                    let arg = self.eval_expr(&args[0])?;

                    match (target_val, arg) {
                        (Value::String(s), Value::String(prefix)) => {
                            return Ok(Value::Bool(s.contains(prefix.as_ref())));
                        }
                        (Value::String(s), Value::Char(pr)) => {
                            return Ok(Value::Bool(s.contains(pr)));
                        }
                        _ => {
                            return Err(
                                "Метод 'НачинаетсяС' работает только со строками".to_string()
                            );
                        }
                    }
                } else if method == "НачинаетсяС" {
                    let target_val = self.eval_expr(target)?;
                    let arg = self.eval_expr(&args[0])?;

                    match (target_val, arg) {
                        (Value::String(s), Value::String(prefix)) => {
                            return Ok(Value::Bool(s.starts_with(prefix.as_ref())));
                        }
                        (Value::String(s), Value::Char(pr)) => {
                            return Ok(Value::Bool(s.starts_with(pr)));
                        }
                        _ => {
                            return Err(
                                "Метод 'НачинаетсяС' работает только со строками".to_string()
                            );
                        }
                    }
                } else if method == "РазделитьПо" {
                    let target_val = self.eval_expr(target)?;
                    let arg = self.eval_expr(&args[0])?;

                    match (target_val, arg) {
                        (Value::String(s), Value::String(c)) => {
                            let array: Vec<Value> = s
                                .split(c.as_ref())
                                .map(|x| Value::String(Rc::new(x.to_string())))
                                .collect();
                            return Ok(Value::Array(Rc::new(RefCell::new(array))));
                        }
                        (Value::String(s), Value::Char(c)) => {
                            let array: Vec<Value> = s
                                .split(c)
                                .map(|x| Value::String(Rc::new(x.to_string())))
                                .collect();
                            return Ok(Value::Array(Rc::new(RefCell::new(array))));
                        }
                        _ => {
                            return Err(
                                "Метод 'РазделитьПо' работает только со строками".to_string()
                            );
                        }
                    }
                }
                let val = self.eval_expr(target)?;
                match (val, method.as_str()) {
                    (Value::String(s), "Длинна") => Ok(Value::Int(s.chars().count() as i64)),
                    (Value::String(s), "РазделитьПоПробелам") => {
                        let array: Vec<Value> = s
                            .split_whitespace()
                            .map(|x| Value::String(Rc::new(x.to_string())))
                            .collect();
                        Ok(Value::Array(Rc::new(RefCell::new(array))))
                    }

                    (Value::Array(arr), "Длинна") => {
                        Ok(Value::Int(arr.borrow().len() as i64))
                    }
                    _ => Err(format!("Метод {} не реализован", method)),
                }
            }
            Expr::Array(elems) => {
                let mut values = Vec::with_capacity(elems.len());
                for e in elems {
                    values.push(self.eval_expr(e)?);
                }
                Ok(Value::Array(Rc::new(RefCell::new(values))))
            }
            Expr::Index { target, index } => {
                let target_val = self.eval_expr(target)?;
                let index_val = self.eval_expr(index)?;

                match (target_val, index_val) {
                    (Value::Array(arr), Value::Int(idx)) => {
                        let arr = arr.borrow();
                        let i = idx as usize;
                        if i >= arr.len() {
                            return Err(format!(
                                "Индекс массива вне границ: длина {}, индекс {}",
                                arr.len(),
                                i
                            ));
                        }
                        Ok(arr[i].clone())
                    }
                    (Value::Array(_), t) => Err(format!("Индекс должен быть Цел, получено {}", t)),
                    (t, _) => Err(format!(
                        "Индексация применима только к массивам, получено {}",
                        t
                    )),
                }
            }
            Expr::Binary { left, op, right } => {
                let l = self.eval_expr(left)?;
                let r = self.eval_expr(right)?;
                self.apply_binary_op(l, r, *op)
            }
            Expr::Unary { op, right } => {
                let val = self.eval_expr(right)?;
                match (op, val) {
                    (UnaryOp::Not, Value::Bool(b)) => Ok(Value::Bool(!b)),
                    (UnaryOp::Not, v) => Err(format!("Оператор 'не' требует Лог, получено {}", v)),
                }
            }
            Expr::Call {
                name,
                args,
                intrinsic,
            } => {
                let mut evaluated_args = Vec::new();
                for arg in args {
                    evaluated_args.push(self.eval_expr(arg)?);
                }

                if *intrinsic {
                    return self.call_intrinsic(name, evaluated_args);
                }

                let func_alg = self
                    .functions
                    .get(name)
                    .ok_or(format!("Функция {} не найдена", name))?
                    .clone();

                self.exec_function(&func_alg, evaluated_args)
            }
        }
    }

    fn apply_binary_op(
        &self,
        mut left: Value,
        mut right: Value,
        op: BinOp,
    ) -> RuntimeResult<Value> {
        if matches!(left, Value::Int(_)) && matches!(right, Value::Float(_)) {
            //the float type is always on the right side
            std::mem::swap(&mut left, &mut right);
        }
        match (left, right, op) {
            (Value::Int(l), Value::Int(r), BinOp::Plus) => Ok(Value::Int(l + r)),
            (Value::Int(l), Value::Int(r), BinOp::Sub) => Ok(Value::Int(l - r)),
            (Value::Int(l), Value::Int(r), BinOp::Mult) => Ok(Value::Int(l * r)),
            (Value::Int(l), Value::Int(r), BinOp::Div) => {
                if r == 0 {
                    return Err("Деление на ноль!".to_string());
                }
                Ok(Value::Int(l / r))
            }
            (Value::Int(l), Value::Int(r), BinOp::Mod) => {
                if r == 0 {
                    return Err("Деление на ноль (остаток)!".to_string());
                }
                Ok(Value::Int(l % r))
            }

            (Value::Int(l), Value::Int(r), BinOp::Greater) => Ok(Value::Bool(l > r)),
            (Value::Int(l), Value::Int(r), BinOp::Less) => Ok(Value::Bool(l < r)),
            (Value::Int(l), Value::Int(r), BinOp::Equal) => Ok(Value::Bool(l == r)),
            (Value::Int(l), Value::Int(r), BinOp::NotEqual) => Ok(Value::Bool(l != r)),
            (Value::Int(l), Value::Int(r), BinOp::GreaterOrEqual) => Ok(Value::Bool(l >= r)),
            (Value::Int(l), Value::Int(r), BinOp::LessOrEqual) => Ok(Value::Bool(l <= r)),

            (Value::Char(l), Value::Char(r), BinOp::Greater) => Ok(Value::Bool(l > r)),
            (Value::Char(l), Value::Char(r), BinOp::Less) => Ok(Value::Bool(l < r)),
            (Value::Char(l), Value::Char(r), BinOp::Equal) => Ok(Value::Bool(l == r)),
            (Value::Char(l), Value::Char(r), BinOp::NotEqual) => Ok(Value::Bool(l != r)),
            (Value::Char(l), Value::Char(r), BinOp::GreaterOrEqual) => Ok(Value::Bool(l >= r)),
            (Value::Char(l), Value::Char(r), BinOp::LessOrEqual) => Ok(Value::Bool(l <= r)),

            (Value::Float(l), Value::Float(r), BinOp::Plus) => Ok(Value::Float(l + r)),
            (Value::Float(l), Value::Float(r), BinOp::Sub) => Ok(Value::Float(l - r)),
            (Value::Float(l), Value::Float(r), BinOp::Mult) => Ok(Value::Float(l * r)),
            (Value::Float(l), Value::Float(r), BinOp::Div) => {
                if r == 0.0 {
                    return Err("Деление на ноль!".to_string());
                }
                Ok(Value::Float(l / r))
            }
            (Value::Float(l), Value::Float(r), BinOp::Mod) => {
                if r == 0.0 {
                    return Err("Деление на ноль (остаток)!".to_string());
                }
                Ok(Value::Float(l % r))
            }

            (Value::Float(l), Value::Float(r), BinOp::Greater) => Ok(Value::Bool(l > r)),
            (Value::Float(l), Value::Float(r), BinOp::Less) => Ok(Value::Bool(l < r)),
            (Value::Float(l), Value::Float(r), BinOp::Equal) => Ok(Value::Bool(l == r)),
            (Value::Float(l), Value::Float(r), BinOp::NotEqual) => Ok(Value::Bool(l != r)),
            (Value::Float(l), Value::Float(r), BinOp::GreaterOrEqual) => Ok(Value::Bool(l >= r)),
            (Value::Float(l), Value::Float(r), BinOp::LessOrEqual) => Ok(Value::Bool(l <= r)),

            //variations of float and int operations
            (Value::Float(l), Value::Int(r), BinOp::Plus) => Ok(Value::Float(l + r as f64)),
            (Value::Float(l), Value::Int(r), BinOp::Sub) => Ok(Value::Float(l - r as f64)),
            (Value::Float(l), Value::Int(r), BinOp::Mult) => Ok(Value::Float(l * r as f64)),
            (Value::Float(l), Value::Int(r), BinOp::Div) => {
                if r == 0 {
                    return Err("Деление на ноль!".to_string());
                }
                Ok(Value::Float(l / r as f64))
            }
            (Value::Float(l), Value::Int(r), BinOp::Mod) => {
                if r == 0 {
                    return Err("Деление на ноль (остаток)!".to_string());
                }
                Ok(Value::Float(l % r as f64))
            }

            (Value::Float(l), Value::Int(r), BinOp::Greater) => Ok(Value::Bool(l > r as f64)),
            (Value::Float(l), Value::Int(r), BinOp::Less) => Ok(Value::Bool(l < r as f64)),
            (Value::Float(l), Value::Int(r), BinOp::Equal) => Ok(Value::Bool(l == r as f64)),
            (Value::Float(l), Value::Int(r), BinOp::NotEqual) => Ok(Value::Bool(l != r as f64)),
            (Value::Float(l), Value::Int(r), BinOp::GreaterOrEqual) => {
                Ok(Value::Bool(l >= r as f64))
            }
            (Value::Float(l), Value::Int(r), BinOp::LessOrEqual) => Ok(Value::Bool(l <= r as f64)),
            //=================
            (Value::Bool(l), Value::Bool(r), BinOp::And) => Ok(Value::Bool(l && r)),
            (Value::Bool(l), Value::Bool(r), BinOp::Or) => Ok(Value::Bool(l || r)),
            (Value::Bool(l), Value::Bool(r), BinOp::Equal) => Ok(Value::Bool(l == r)),
            (Value::Bool(l), Value::Bool(r), BinOp::NotEqual) => Ok(Value::Bool(l != r)),

            (Value::String(l), Value::String(r), BinOp::Plus) => {
                Ok(Value::String(Rc::new(format!("{}{}", l, r))))
            }
            (Value::String(l), Value::String(r), BinOp::Equal) => Ok(Value::Bool(l == r)),
            (Value::String(l), Value::String(r), BinOp::NotEqual) => Ok(Value::Bool(l != r)),

            (l, r, op) => Err(format!(
                "Невозможно выполнить операцию '{:?}' над типами {:?} и {:?}",
                op, l, r
            )),
        }
    }

    fn is_truthy(&self, val: &Value) -> RuntimeResult<bool> {
        match val {
            Value::Bool(b) => Ok(*b),
            _ => Err(format!(
                "Ожидалось логическое значение (Истина/Ложь), получено {}",
                val
            )),
        }
    }

    fn call_intrinsic(&self, name: &str, args: Vec<Value>) -> RuntimeResult<Value> {
        match name {
            "Написать" => {
                self.print_values(args);
                Ok(Value::Int(0))
            }
            "Считать" => {
                if let Some(prompt) = args.first() {
                    print!("{}", prompt);
                }

                let _ = io::stdout().flush();

                let mut input = String::new();
                io::stdin()
                    .read_line(&mut input)
                    .map_err(|e| e.to_string())?;
                let input = input.trim();

                if let Ok(f) = input.parse::<f64>() {
                    Ok(Value::Float(f))
                } else if let Ok(i) = input.parse::<i64>() {
                    Ok(Value::Int(i))
                } else if let Ok(c) = input.parse::<char>() {
                    Ok(Value::Char(c))
                } else {
                    Ok(Value::String(Rc::new(input.to_string())))
                }
            }
            _ => Err(format!("Неизвестная встроенная функция: {}", name)),
        }
    }

    fn print_values(&self, values: Vec<Value>) {
        if values.is_empty() {
            println!();
            return;
        }

        let mut iter = values.iter();
        if let Some(first) = iter.next() {
            if let Value::String(fmt) = first
                && fmt.contains("{}")
            {
                let mut res = fmt.to_string();
                for v in iter {
                    res = res.replacen("{}", &v.to_string(), 1);
                }
                println!("{}", res);
                return;
            }

            print!("{}", first);
            for v in iter {
                print!(" {}", v);
            }
            println!();
        }
    }
}
