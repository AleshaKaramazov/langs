// checker.rs
use std::collections::HashMap;
use crate::ast::*;

pub struct TypeEnv {
    scopes: Vec<HashMap<String, Type>>,
}

impl TypeEnv {
    pub fn new() -> Self {
        Self { scopes: vec![HashMap::new()] }
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn declare(&mut self, name: String, ty: Type) -> Result<(), String> {
        if self.scopes.last().unwrap().contains_key(&name) {
            return Err(format!("Переменная '{}' уже объявлена в этой области видимости.", name));
        }
        self.scopes.last_mut().unwrap().insert(name, ty);
        Ok(())
    }

    pub fn lookup(&self, name: &str) -> Result<Type, String> {
        for scope in self.scopes.iter().rev() {
            if let Some(t) = scope.get(name) {
                return Ok(t.clone());
            }
        }
        Err(format!("Переменная '{}' не найдена или не объявлена.", name))
    }

    pub fn check_assign(&self, name: &str, new_ty: &Type) -> Result<(), String> {
        let current_ty = self.lookup(name)?;
        if &current_ty != new_ty {
            return Err(format!(
                "Несоответствие типов для '{}': ожидалось {:?}, получено {:?}",
                name, current_ty, new_ty
            ));
        }
        Ok(())
    }
}

pub struct TypeChecker {
    env: TypeEnv,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self { env: TypeEnv::new() }
    }

    pub fn check_algorithm(&mut self, alg: &Algorithm) -> Result<(), String> {
        self.env.enter_scope();
        for (name, ty) in &alg.args {
            self.env.declare(name.clone(), ty.clone())?;
        }
        self.check_block(&alg.body)?;
        self.env.exit_scope();
        Ok(())
    }

    fn check_block(&mut self, stmts: &[Stmt]) -> Result<(), String> {
        self.env.enter_scope();
        for stmt in stmts {
            self.check_stmt(stmt)?;
        }
        self.env.exit_scope();
        Ok(())
    }

    fn check_stmt(&mut self, stmt: &Stmt) -> Result<(), String> {
        match stmt {
            Stmt::Let { name, ty, expr } => {
                let expr_ty = self.check_expr(expr)?;
                
                let final_ty = if *ty == Type::Infer {
                    expr_ty.clone()
                } else {
                    if *ty != expr_ty {
                        return Err(format!("Ошибка в 'пусть {}': ожидался тип {:?}, получен {:?}", name, ty, expr_ty));
                    }
                    ty.clone()
                };

                self.env.declare(name.clone(), final_ty)?;
            },
            Stmt::ForEach { var, collection, body } => {
                let coll_ty = self.check_expr(collection)?;
                
                if let Type::Array(inner_ty) = coll_ty {
                    self.env.enter_scope();
                    // Переменная цикла получает тип элементов массива
                    self.env.declare(var.clone(), *inner_ty)?; 
                    self.check_block(body)?;
                    self.env.exit_scope();
                } else {
                    return Err(format!("Нельзя итерироваться по типу {:?}", coll_ty));
                }
            }
            Stmt::Assign { name, expr } => {
                let expr_ty = self.check_expr(expr)?;
                self.env.check_assign(name, &expr_ty)?;
            }
            Stmt::AssignAdd { name, expr } | 
            Stmt::AssignSub { name, expr } |
            Stmt::AssignMult { name, expr } |
            Stmt::AssignDiv { name, expr } => {
                 let expr_ty = self.check_expr(expr)?;
                 let var_ty = self.env.lookup(name)?;
                 
                 if var_ty != Type::Int || expr_ty != Type::Int {
                     return Err(format!("Арифметическое присваивание работает только с Числами. Получено: {:?} и {:?}", var_ty, expr_ty));
                 }
            }
            Stmt::If { cond, then_body, else_if, else_body } => {
                let cond_ty = self.check_expr(cond)?;
                if cond_ty != Type::Bool {
                    return Err(format!("Условие 'если' должно быть Лог (Bool), получено {:?}", cond_ty));
                }
                self.check_block(then_body)?;
                
                for (e_cond, e_body) in else_if {
                    let t = self.check_expr(e_cond)?;
                    if t != Type::Bool { return Err("Условие 'иначе если' должно быть Лог".into()); }
                    self.check_block(e_body)?;
                }
                if let Some(body) = else_body {
                    self.check_block(body)?;
                }
            }
            Stmt::While { cond, body } => {
                let cond_ty = self.check_expr(cond)?;
                if cond_ty != Type::Bool { return Err("Условие 'пока' должно быть Лог".into()); }
                self.check_block(body)?;
            }
            Stmt::For { var, start, end, body } => {
                let start_ty = self.check_expr(start)?;
                let end_ty = self.check_expr(end)?;
                if start_ty != Type::Int || end_ty != Type::Int {
                    return Err("Границы цикла 'для' должны быть Числами (Int)".into());
                }
                
                self.env.enter_scope();
                self.env.declare(var.clone(), Type::Int)?; 
                self.check_block(body)?; 
                self.env.exit_scope();
            }
            Stmt::Expr(expr) => {
                self.check_expr(expr)?;
            }
        }
        Ok(())
    }

    fn check_expr(&mut self, expr: &Expr) -> Result<Type, String> {
        match expr {
            Expr::Int(_) => Ok(Type::Int),
            Expr::Bool(_) => Ok(Type::Bool),
            Expr::String(_) => Ok(Type::String),
            Expr::Var(name) => self.env.lookup(name),
            Expr::Binary { left, op, right } => {
                let l_ty = self.check_expr(left)?;
                let r_ty = self.check_expr(right)?;

                match op {
                    BinOp::Plus | BinOp::Sub | BinOp::Mult | BinOp::Div | BinOp::Mod => {
                        if *op == BinOp::Plus && l_ty == Type::String && r_ty == Type::String {
                             return Ok(Type::String);
                        }
                        if l_ty == Type::Int && r_ty == Type::Int {
                            Ok(Type::Int)
                        } else {
                            Err(format!("Арифметическая операция {:?} требует Int, получено {:?} и {:?}", op, l_ty, r_ty))
                        }
                    }
                    BinOp::Equal | BinOp::NotEqual => {
                        if l_ty == r_ty { Ok(Type::Bool) } 
                        else { Err("Нельзя сравнивать разные типы".into()) }
                    }
                    BinOp::Less | BinOp::Greater => {
                        if l_ty == Type::Int && r_ty == Type::Int { Ok(Type::Bool) }
                        else { Err("Сравнение больше/меньше только для чисел".into()) }
                    }
                    BinOp::And | BinOp::Or => {
                        if l_ty == Type::Bool && r_ty == Type::Bool { Ok(Type::Bool) }
                        else { Err("Логические И/ИЛИ только для булевых значений".into()) }
                    }
                }
            }
            Expr::Unary { op, right } => {
                let r_ty = self.check_expr(right)?;
                match op {
                    UnaryOp::Not => {
                        if r_ty == Type::Bool { Ok(Type::Bool) }
                        else { Err("Оператор 'не' только для булевых значений".into()) }
                    }
                }
            }
            Expr::Array(elems) => {
                if elems.is_empty() {
                     return Ok(Type::Int); 
                }
                let first_ty = self.check_expr(&elems[0])?;
                for e in &elems[1..] {
                    if self.check_expr(e)? != first_ty {
                        return Err("Все элементы массива должны быть одного типа".into());
                    }
                }
                Ok(Type::Array(Box::new(first_ty)))
            }
            Expr::Index { target, index } => {
                let t_ty = self.check_expr(target)?;
                let i_ty = self.check_expr(index)?;
                
                if i_ty != Type::Int {
                    return Err("Индекс массива должен быть целым числом".into());
                }
                match t_ty {
                    Type::Array(inner_ty) => Ok(*inner_ty), 
                    _ => Err(format!("Индексация применима только к массивам, получено {:?}", t_ty))
                }
            }
            Expr::Call { name, args, intrinsic } => {
                if *intrinsic && name == "написать" {
                    for arg in args { self.check_expr(arg)?; }
                    return Ok(Type::Int); 
                }
                 if *intrinsic && name == "считать" {
                    return Ok(Type::Int); 
                }
                Err("Функции пока не поддерживаются чекером".into())
            }
        }
    }
}
