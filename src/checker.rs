use crate::ast::*;
use std::collections::HashMap;

pub struct TypeEnv {
    scopes: Vec<HashMap<String, Type>>,
}

#[derive(Clone, Debug)]
struct FuncSignature {
    args: Vec<Type>,
    ret_type: Type,
}

impl TypeEnv {
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

    pub fn declare(&mut self, name: String, ty: Type) -> Result<(), String> {
        if self.scopes.last().unwrap().contains_key(&name) {
            return Err(format!(
                "Переменная '{}' уже объявлена в этой области видимости.",
                name
            ));
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
        Err(format!(
            "Переменная '{}' не найдена или не объявлена.",
            name
        ))
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
    functions: HashMap<String, FuncSignature>,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            env: TypeEnv::new(),
            functions: HashMap::new(),
        }
    }

    pub fn check_program(&mut self, prog: &Program) -> Result<(), String> {
        for alg in &prog.algorithms {
            let arg_types: Vec<Type> = alg.args.iter().map(|(_, t)| t.clone()).collect();

            if self.functions.contains_key(&alg.name) {
                return Err(format!("Алгоритм '{}' уже объявлен", alg.name));
            }

            self.functions.insert(
                alg.name.clone(),
                FuncSignature {
                    args: arg_types,
                    ret_type: alg.ret_type.clone(),
                },
            );
        }

        for alg in &prog.algorithms {
            self.check_algorithm(alg)?;
        }
        Ok(())
    }

    pub fn check_algorithm(&mut self, alg: &Algorithm) -> Result<(), String> {
        self.env.enter_scope();
        for (name, ty) in &alg.args {
            self.env.declare(name.clone(), ty.clone())?;
        }
        for stmt in &alg.body {
            self.check_stmt(stmt, &alg.ret_type)?;
        }
        self.env.exit_scope();
        Ok(())
    }

    fn check_block(&mut self, stmts: &[Stmt], expected_ret: &Type) -> Result<(), String> {
        self.env.enter_scope();
        for stmt in stmts {
            self.check_stmt(stmt, expected_ret)?;
        }
        self.env.exit_scope();
        Ok(())
    }

    fn check_stmt(&mut self, stmt: &Stmt, expected_ret: &Type) -> Result<(), String> {
        match stmt {
            Stmt::Return(maybe_expr) => match maybe_expr {
                Some(expr) => {
                    let actual = self.check_expr(expr)?;
                    if &actual != expected_ret && actual != Type::Unknown {
                        return Err(format!(
                            "Функция должна возвращать {:?}, но возвращает {:?}",
                            expected_ret, actual
                        ));
                    }
                }
                None => {
                    if expected_ret != &Type::Void {
                        return Err(format!(
                            "Ожидалось возвращаемое значение типа {:?}",
                            expected_ret
                        ));
                    }
                }
            },
            Stmt::Let { name, ty, expr } => {
                let expr_ty = self.check_expr(expr)?;

                let final_ty = if *ty == Type::Infer {
                    expr_ty.clone()
                } else {
                    if expr_ty != Type::Unknown
                        && *ty != expr_ty
                        && match ty {
                            Type::Array(inner) => **inner != expr_ty,
                            _ => true,
                        }
                    {
                        return Err(format!(
                            "Ошибка в 'пусть {}': ожидался тип {:?}, получен {:?}",
                            name, ty, expr_ty
                        ));
                    }
                    ty.clone()
                };

                self.env.declare(name.clone(), final_ty)?;
            }
            Stmt::ForEach {
                var,
                collection,
                body,
            } => {
                let coll_ty = self.check_expr(collection)?;

                if let Type::Array(inner_ty) = coll_ty {
                    self.env.enter_scope();
                    self.env.declare(var.clone(), *inner_ty)?;
                    self.check_block(body, expected_ret)?;
                    self.env.exit_scope();
                } else {
                    return Err(format!("Нельзя итерироваться по типу {:?}", coll_ty));
                }
            }
            Stmt::Assign { name, expr } => {
                let expr_ty = self.check_expr(expr)?;
                self.env.check_assign(name, &expr_ty)?;
            }
            Stmt::AssignAdd { name, expr }
            | Stmt::AssignSub { name, expr }
            | Stmt::AssignMult { name, expr }
            | Stmt::AssignDiv { name, expr } => {
                let expr_ty = self.check_expr(expr)?;
                let var_ty = self.env.lookup(name)?;

                if var_ty != Type::Int || expr_ty != Type::Int {
                    return Err(format!(
                        "Арифметическое присваивание работает только с Числами. Получено: {:?} и {:?}",
                        var_ty, expr_ty
                    ));
                }
            }
            Stmt::If {
                cond,
                then_body,
                else_if,
                else_body,
            } => {
                let cond_ty = self.check_expr(cond)?;
                if cond_ty != Type::Bool {
                    return Err(format!(
                        "Условие 'если' должно быть Лог (Bool), получено {:?}",
                        cond_ty
                    ));
                }
                self.check_block(then_body, expected_ret)?;

                for (e_cond, e_body) in else_if {
                    let t = self.check_expr(e_cond)?;
                    if t != Type::Bool {
                        return Err("Условие 'иначе если' должно быть Лог".into());
                    }
                    self.check_block(e_body, expected_ret)?;
                }
                if let Some(body) = else_body {
                    self.check_block(body, expected_ret)?;
                }
            }
            Stmt::While { cond, body } => {
                let cond_ty = self.check_expr(cond)?;
                if cond_ty != Type::Bool {
                    return Err("Условие 'пока' должно быть Лог".into());
                }
                self.check_block(body, expected_ret)?;
            }
            Stmt::For {
                var,
                start,
                cont: _,
                end,
                body,
            } => {
                let start_ty = self.check_expr(start)?;
                let end_ty = self.check_expr(end)?;
                if start_ty != Type::Int || end_ty != Type::Int {
                    return Err("Границы цикла 'для' должны быть Числами (Int)".into());
                }

                self.env.enter_scope();
                self.env.declare(var.clone(), Type::Int)?;
                self.check_block(body, expected_ret)?;
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
            Expr::NativeCall { .. } => Ok(Type::Unknown),
            Expr::Lambda {
                param,
                param_ty,
                body,
            } => {
                self.env.enter_scope();
                self.env.declare(param.clone(), param_ty.clone())?;
                let body_ty = self.check_expr(body)?;
                self.env.exit_scope();
                Ok(Type::Function(
                    Box::new(param_ty.clone()),
                    Box::new(body_ty),
                ))
            }
            Expr::Int(_) => Ok(Type::Int),
            Expr::Bool(_) => Ok(Type::Bool),
            Expr::Float(_) => Ok(Type::Float),
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
                        } else if l_ty == Type::Float && r_ty == Type::Float ||
                            ((l_ty == Type::Float && r_ty == Type::Int) || (r_ty == Type::Float && l_ty == Type::Int)){
                            Ok(Type::Float)
                        } else {
                            Err(format!(
                                "Арифметическая операция {:?} требует Int, получено {:?} и {:?}",
                                op, l_ty, r_ty
                            ))
                        }
                    }
                    BinOp::Equal | BinOp::NotEqual => {
                        if l_ty == r_ty {
                            Ok(Type::Bool)
                        } else {
                            Err("Нельзя сравнивать разные типы".into())
                        }
                    }
                    BinOp::Less | BinOp::Greater | BinOp::LessOrEqual | BinOp::GreaterOrEqual => {
                        if l_ty == Type::Int && r_ty == Type::Int {
                            Ok(Type::Bool)
                        } else {
                            Err("Сравнение больше/меньше только для чисел".into())
                        }
                    }
                    BinOp::And | BinOp::Or => {
                        if l_ty == Type::Bool && r_ty == Type::Bool {
                            Ok(Type::Bool)
                        } else {
                            Err("Логические И/ИЛИ только для булевых значений".into())
                        }
                    }
                }
            }
            Expr::Unary { op, right } => {
                let r_ty = self.check_expr(right)?;
                match op {
                    UnaryOp::Not => {
                        if r_ty == Type::Bool {
                            Ok(Type::Bool)
                        } else {
                            Err("Оператор 'не' только для булевых значений".into())
                        }
                    }
                }
            }
            Expr::MethodCall {
                target,
                method,
                args,
            } => {
                let target_ty = self.check_expr(target)?;

                match (target_ty, method.as_str()) {
                    (Type::String, "Длинна") => {
                        if !args.is_empty() {
                            return Err("Метод 'Длинна' не принимает аргументов".into());
                        }
                        Ok(Type::Int)
                    }
                    (Type::String, "КончаетсяНа") => Ok(Type::Bool),
                    (Type::String, "НачинаетсяС") => Ok(Type::Bool),
                    (Type::String, "РазделитьПо") => Ok(Type::Array(Box::new(Type::String))),
                    (Type::String, "РазделитьПоПробелам") => Ok(Type::Array(Box::new(Type::String))),
                    (Type::Array(_), "Длинна") => Ok(Type::Int),
                    (Type::Array(_), "Содержит") | (Type::String, "Содержит") => {
                        if args.len() != 1 {
                            return Err("Метод 'Содержит' ожидает 1 аргумент".into());
                        }
                        Ok(Type::Bool)
                    }
                    (Type::Array(inner_ty), "Где") => {
                        if args.len() != 1 {
                            return Err("Метод 'Где' требует 1 аргумент".into());
                        }
                        let arg_ty = self.check_expr(&args[0])?;

                        if let Type::Function(arg, ret) = arg_ty {
                            if *arg != *inner_ty {
                                return Err(format!(
                                    "Лямбда ожидает {:?}, а в массиве {:?}",
                                    arg, inner_ty
                                ));
                            }
                            if *ret != Type::Bool {
                                return Err("Лямбда должна возвращать Лог".into());
                            }
                        } else {
                            return Err("Аргумент должен быть функцией".into());
                        }
                        Ok(Type::Array(inner_ty))
                    }
                    (Type::Array(inner_ty), "Добавить") => {
                        if args.len() != 1 {
                            return Err("Метод 'Добавить' ожидает 1 аргумент".into());
                        }
                        let arg_ty = self.check_expr(&args[0])?;
                        if arg_ty != *inner_ty {
                            return Err(format!(
                                "Нельзя добавить {:?} в массив типа {:?}",
                                arg_ty, inner_ty
                            ));
                        }
                        Ok(Type::Void)
                    }
                    (t, m) => Err(format!("Тип {:?} не имеет метода '{}'", t, m)),
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
                    _ => Err(format!(
                        "Индексация применима только к массивам, получено {:?}",
                        t_ty
                    )),
                }
            }
            Expr::Call {
                name,
                args,
                intrinsic,
            } => {
                if *intrinsic && name == "Написать" {
                    for arg in args {
                        self.check_expr(arg)?;
                    }
                    return Ok(Type::Int);
                }
                if *intrinsic && name == "Считать" {
                    return Ok(Type::Unknown);
                }

                let sig = match self.functions.get(name) {
                    Some(s) => s.clone(),
                    None => return Err(format!("Неизвестная функция: {}", name)),
                };

                if args.len() != sig.args.len() {
                    return Err(format!(
                        "Функция {} ожидает {} аргументов, получено {}",
                        name,
                        sig.args.len(),
                        args.len()
                    ));
                }

                for (i, arg_expr) in args.iter().enumerate() {
                    let arg_ty = self.check_expr(arg_expr)?;

                    if arg_ty != sig.args[i] {
                        return Err(format!(
                            "Аргумент {} функции {} имеет неверный тип. Ожидалось {:?}, получено {:?}",
                            i + 1,
                            name,
                            sig.args[i],
                            arg_ty
                        ));
                    }
                }

                Ok(sig.ret_type)
            }
        }
    }
}
