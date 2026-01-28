use crate::ast::*;
use std::fmt::Write;

pub struct Visualizer {
    dot: String,
    node_count: usize,
}

impl Visualizer {
    pub fn new() -> Self {
        let mut dot = String::new();
        // Настройка заголовка графа
        writeln!(dot, "digraph Algorithm {{").unwrap();
        // Шрифт и общие настройки
        writeln!(dot, "  fontname=\"Segoe UI, Roboto, Helvetica, Arial, sans-serif\";").unwrap();
        writeln!(dot, "  nodesep=0.5; ranksep=0.5;").unwrap();
        writeln!(dot, "  rankdir=TB;").unwrap(); // Сверху вниз
        writeln!(dot, "  splines=ortho;").unwrap(); // Ортогональные линии (прямые углы)
        writeln!(dot, "  concentrate=true;").unwrap();
        
        // Стили по умолчанию
        writeln!(dot, "  node [fontname=\"Segoe UI, Roboto\", fontsize=11, shape=box, style=\"filled,rounded\", fillcolor=\"#ECEFF1\", color=\"#455A64\", penwidth=1.0, margin=\"0.2,0.1\"];").unwrap();
        writeln!(dot, "  edge [fontname=\"Segoe UI, Roboto\", fontsize=9, color=\"#546E7A\", penwidth=1.2, arrowhead=vee];").unwrap();

        Self { dot, node_count: 0 }
    }

    fn next_id(&mut self) -> String {
        self.node_count += 1;
        format!("n{}", self.node_count)
    }

    /// Красивое форматирование выражений для отображения в узлах
    fn fmt_expr(&self, expr: &Expr) -> String {
        match expr {
            Expr::Int(i) => i.to_string(),
            Expr::Float(f) => format!("{:.2}", f),
            Expr::Bool(b) => (if *b { "ИСТИНА" } else { "ЛОЖЬ" }).to_string(),
            Expr::String(s) => format!("\\\"{}\\\"", s.replace("\"", "\\\"")),
            Expr::Char(c) => format!("'{}'", c),
            Expr::Var(v) => v.clone(),
            Expr::Array(elements) => {
                let parts: Vec<String> = elements.iter().map(|e| self.fmt_expr(e)).collect();
                format!("[{}]", parts.join(", "))
            }
            Expr::Index { target, index } => {
                format!("{}[{}]", self.fmt_expr(target), self.fmt_expr(index))
            }
            Expr::Unary { op, right } => {
                let s = match op {
                    UnaryOp::Not => "НЕ ",
                };
                format!("{}{}", s, self.fmt_expr(right))
            }
            Expr::Binary { left, op, right } => {
                let op_str = match op {
                    BinOp::Plus => "+",
                    BinOp::Sub => "-",
                    BinOp::Mult => "×",
                    BinOp::Div => "/",
                    BinOp::Mod => "%",
                    BinOp::Equal => "=",
                    BinOp::Less => "<",
                    BinOp::Greater => ">",
                    BinOp::Or => "ИЛИ",
                    BinOp::And => "И",
                    BinOp::NotEqual => "≠",
                    BinOp::GreaterOrEqual => "≥",
                    BinOp::LessOrEqual => "≤",
                };
                // Убираем лишние скобки для чистоты, если это простые типы
                format!("{} {} {}", self.fmt_expr(left), op_str, self.fmt_expr(right))
            }
            Expr::Call { name, args, intrinsic } => {
                let formatted_args: Vec<String> = args.iter().map(|a| self.fmt_expr(a)).collect();
                let prefix = if *intrinsic { "!" } else { "" };
                // Сокращаем длинные списки аргументов
                let args_str = formatted_args.join(", ");
                if args_str.len() > 30 {
                    format!("{}{}(...)", prefix, name)
                } else {
                    format!("{}{}({})", prefix, name, args_str)
                }
            }
            Expr::MethodCall { target, method, args } => {
                let formatted_args: Vec<String> = args.iter().map(|a| self.fmt_expr(a)).collect();
                format!("{}.{}({})", self.fmt_expr(target), method, formatted_args.join(", "))
            }
            Expr::NativeCall { path, args } => {
                let formatted_args: Vec<String> = args.iter().map(|a| self.fmt_expr(a)).collect();
                format!("{}[{}]", path, formatted_args.join(", "))
            }
            Expr::Lambda { param, .. } => format!("λ({}) -> ...", param),
            Expr::PreInc(e) => format!("++{}", self.fmt_expr(e)),
            Expr::PreDec(e) => format!("--{}", self.fmt_expr(e)),
            Expr::PostInc(e) => format!("{}++", self.fmt_expr(e)),
            Expr::PostDec(e) => format!("{}--", self.fmt_expr(e)),
        }
    }

    pub fn translate(mut self, alg: Algorithm) -> String {
        // Узел НАЧАЛО
        let start_node = self.next_id();
        let args_str: Vec<String> = alg.args.iter().map(|(n, _)| n.clone()).collect();
        let label = if args_str.is_empty() {
            format!("НАЧАЛО: {}", alg.name)
        } else {
            format!("НАЧАЛО: {}\\n({})", alg.name, args_str.join(", "))
        };
        
        writeln!(
            self.dot,
            "  {} [label=\"{}\", shape=rect, style=\"filled,rounded\", fillcolor=\"#2196F3\", fontcolor=\"white\", penwidth=0];",
            start_node, label
        ).unwrap();

        // Тело алгоритма
        let (body_entry, body_exit) = self.translate_block(&alg.body);

        // Узел КОНЕЦ
        let end_node = self.next_id();
        writeln!(
            self.dot,
            "  {} [label=\"КОНЕЦ\", shape=rect, style=\"filled,rounded\", fillcolor=\"#2196F3\", fontcolor=\"white\", penwidth=0];",
            end_node
        ).unwrap();

        // Связывание
        if let Some(entry) = body_entry {
            writeln!(self.dot, "  {} -> {};", start_node, entry).unwrap();
            if let Some(exit) = body_exit {
                writeln!(self.dot, "  {} -> {};", exit, end_node).unwrap();
            }
        } else {
            writeln!(self.dot, "  {} -> {};", start_node, end_node).unwrap();
        }

        self.dot.push_str("}\n");
        self.dot
    }

    /// Возвращает (id_входа, id_выхода)
    fn translate_block(&mut self, stmts: &[Stmt]) -> (Option<String>, Option<String>) {
        if stmts.is_empty() {
            return (None, None);
        }

        let mut first_entry = None;
        let mut prev_exit: Option<String> = None;

        for stmt in stmts {
            let (curr_entry, curr_exit) = self.translate_stmt(stmt);

            if first_entry.is_none() {
                first_entry = Some(curr_entry.clone());
            }

            if let Some(prev) = prev_exit {
                // Если предыдущий блок имеет выход, соединяем с текущим входом
                writeln!(self.dot, "  {} -> {};", prev, curr_entry).unwrap();
            }

            // Если текущий оператор прерывает поток (Return, Break, Continue),
            // то curr_exit будет указывать на этот терминальный узел, 
            // но логически "выхода" для продолжения последовательности нет.
            // Однако для translate_block мы возвращаем последний узел, чтобы 
            // внешний код мог привязать его к "Конец", если это Return.
            match stmt {
                Stmt::Return(_) | Stmt::Break | Stmt::Continue => {
                    prev_exit = None; // Поток прерван, следующий стейтмент недостижим (dead code)
                    // Мы не break'аем цикл здесь, чтобы обработать весь вектор, 
                    // но связи рисоваться не будут.
                }
                _ => {
                    prev_exit = Some(curr_exit);
                }
            }
        }
        
        // Возвращаем вход первого и выход последнего (если он не прерван)
        // Если блок заканчивается return, prev_exit будет None, что корректно.
        (first_entry, prev_exit)
    }

    fn translate_stmt(&mut self, stmt: &Stmt) -> (String, String) {
        let id = self.next_id();

        match stmt {
            Stmt::Let { name, expr, .. } => {
                let expr_str = match expr {
                    Some(e) => self.fmt_expr(e),
                    None => "NULL".to_string(),
                };
                writeln!(
                    self.dot,
                    "  {} [label=\"Пусть {} = {}\"];",
                    id, name, expr_str
                ).unwrap();
                (id.clone(), id)
            }
            Stmt::Assign { name, expr } => {
                writeln!(self.dot, "  {} [label=\"{} := {}\"];", id, name, self.fmt_expr(expr)).unwrap();
                (id.clone(), id)
            }
            Stmt::AssignAdd { name, expr } => {
                writeln!(self.dot, "  {} [label=\"{} += {}\"];", id, name, self.fmt_expr(expr)).unwrap();
                (id.clone(), id)
            }
            Stmt::AssignSub { name, expr } => {
                writeln!(self.dot, "  {} [label=\"{} -= {}\"];", id, name, self.fmt_expr(expr)).unwrap();
                (id.clone(), id)
            }
            Stmt::AssignMult { name, expr } => {
                writeln!(self.dot, "  {} [label=\"{} *= {}\"];", id, name, self.fmt_expr(expr)).unwrap();
                (id.clone(), id)
            }
            Stmt::AssignDiv { name, expr } => {
                writeln!(self.dot, "  {} [label=\"{} /= {}\"];", id, name, self.fmt_expr(expr)).unwrap();
                (id.clone(), id)
            }
            Stmt::Expr(expr) => {
                // Особая обработка для ввода/вывода, чтобы рисовать параллелограммы
                let is_io = match expr {
                    Expr::Call { name, intrinsic, .. } => *intrinsic && (name == "Написать" || name == "Считать"),
                    Expr::MethodCall { method, .. } => method == "Считать", 
                    _ => false,
                };

                let label = self.fmt_expr(expr);
                
                if is_io {
                    // Параллелограмм для IO
                    writeln!(
                        self.dot, 
                        "  {} [label=\"{}\", shape=parallelogram, fillcolor=\"#C8E6C9\", color=\"#2E7D32\"];", 
                        id, label
                    ).unwrap();
                } else {
                    writeln!(self.dot, "  {} [label=\"{}\"];", id, label).unwrap();
                }
                (id.clone(), id)
            }
            Stmt::Return(maybe_expr) => {
                let label = match maybe_expr {
                    Some(e) => format!("ВЕРНУТЬ {}", self.fmt_expr(e)),
                    None => "ВЕРНУТЬ".to_string(),
                };
                // Красный цвет для выхода
                writeln!(
                    self.dot,
                    "  {} [label=\"{}\", shape=cds, fillcolor=\"#FFCDD2\", color=\"#C62828\"];",
                    id, label
                ).unwrap();
                // Return не имеет "выхода" в поток, он уходит в никуда (или в Конец)
                (id.clone(), id) 
            }
            Stmt::Break => {
                writeln!(
                    self.dot,
                    "  {} [label=\"ПРЕРВАТЬ\", shape=trapezium, fillcolor=\"#FFE0B2\", color=\"#EF6C00\"];",
                    id
                ).unwrap();
                (id.clone(), id)
            }
            Stmt::Continue => {
                writeln!(
                    self.dot,
                    "  {} [label=\"ПРОДОЛЖИТЬ\", shape=invtrapezium, fillcolor=\"#FFE0B2\", color=\"#EF6C00\"];",
                    id
                ).unwrap();
                (id.clone(), id)
            }
            Stmt::If { cond, then_body, else_if, else_body } => {
                let cond_str = self.fmt_expr(cond);
                // Ромб для условия
                writeln!(
                    self.dot, 
                    "  {} [label=\"{}?\", shape=diamond, fillcolor=\"#FFF9C4\", color=\"#FBC02D\"];", 
                    id, cond_str
                ).unwrap();

                // Точка схода всех веток
                let merge_id = self.next_id();
                writeln!(self.dot, "  {} [label=\"\", shape=point, width=0];", merge_id).unwrap();

                // Ветка THEN
                let (then_entry, then_exit) = self.translate_block(then_body);
                if let Some(entry) = then_entry {
                    writeln!(self.dot, "  {} -> {} [label=\"да\", fontcolor=\"#2E7D32\"];", id, entry).unwrap();
                    if let Some(exit) = then_exit {
                        writeln!(self.dot, "  {} -> {};", exit, merge_id).unwrap();
                    }
                } else {
                    // Пустое тело then
                    writeln!(self.dot, "  {} -> {} [label=\"да\", fontcolor=\"#2E7D32\"];", id, merge_id).unwrap();
                }

                // Обработка цепочки ELSE IF
                let mut last_cond_id = id.clone();

                for (elif_cond, elif_body) in else_if {
                    let elif_id = self.next_id();
                    writeln!(
                        self.dot,
                        "  {} [label=\"{}?\", shape=diamond, fillcolor=\"#FFF9C4\", color=\"#FBC02D\"];",
                        elif_id, self.fmt_expr(elif_cond)
                    ).unwrap();

                    // Связь от предыдущего условия (НЕТ) к этому
                    writeln!(self.dot, "  {} -> {} [label=\"нет\", fontcolor=\"#C62828\"];", last_cond_id, elif_id).unwrap();

                    let (b_entry, b_exit) = self.translate_block(elif_body);
                    if let Some(entry) = b_entry {
                        writeln!(self.dot, "  {} -> {} [label=\"да\", fontcolor=\"#2E7D32\"];", elif_id, entry).unwrap();
                        if let Some(exit) = b_exit {
                            writeln!(self.dot, "  {} -> {};", exit, merge_id).unwrap();
                        }
                    } else {
                         writeln!(self.dot, "  {} -> {} [label=\"да\", fontcolor=\"#2E7D32\"];", elif_id, merge_id).unwrap();
                    }
                    last_cond_id = elif_id;
                }

                // Ветка ELSE
                if let Some(body) = else_body {
                    let (else_entry, else_exit) = self.translate_block(body);
                    if let Some(entry) = else_entry {
                        writeln!(self.dot, "  {} -> {} [label=\"нет\", fontcolor=\"#C62828\"];", last_cond_id, entry).unwrap();
                        if let Some(exit) = else_exit {
                            writeln!(self.dot, "  {} -> {};", exit, merge_id).unwrap();
                        }
                    } else {
                         writeln!(self.dot, "  {} -> {} [label=\"нет\", fontcolor=\"#C62828\"];", last_cond_id, merge_id).unwrap();
                    }
                } else {
                    // Если else нет, просто соединяем последнее условие с точкой схода
                    writeln!(self.dot, "  {} -> {} [label=\"нет\", fontcolor=\"#C62828\"];", last_cond_id, merge_id).unwrap();
                }

                (id, merge_id)
            }
            Stmt::While { cond, body } => {
                // Условие цикла
                writeln!(
                    self.dot,
                    "  {} [label=\"Пока {}?\", shape=diamond, fillcolor=\"#BBDEFB\", color=\"#1976D2\"];",
                    id, self.fmt_expr(cond)
                ).unwrap();

                let (b_entry, b_exit) = self.translate_block(body);
                
                // Если есть тело
                if let Some(entry) = b_entry {
                    writeln!(self.dot, "  {} -> {} [label=\"да\", fontcolor=\"#2E7D32\"];", id, entry).unwrap();
                    
                    // Обратный путь от конца тела к условию (пунктиром)
                    if let Some(exit) = b_exit {
                        writeln!(
                            self.dot, 
                            "  {} -> {} [constraint=false, style=dashed, color=\"#90A4AE\"];", 
                            exit, id
                        ).unwrap();
                    }
                } else {
                    // Бесконечный пустой цикл: сам в себя
                    writeln!(self.dot, "  {} -> {} [label=\"да\"];", id, id).unwrap();
                }

                let exit_id = self.next_id();
                writeln!(self.dot, "  {} [label=\"\", shape=point, width=0];", exit_id).unwrap();
                writeln!(self.dot, "  {} -> {} [label=\"нет\", fontcolor=\"#C62828\"];", id, exit_id).unwrap();

                (id, exit_id)
            }
            Stmt::For { var, start, cont, end, body } => {
                // Блок инициализации
                let init_expr = format!("{} = {}", var, self.fmt_expr(start));
                writeln!(
                    self.dot, "  {} [label=\"{}\", shape=hexagon, fillcolor=\"#E1BEE7\", color=\"#7B1FA2\"];", 
                    id, init_expr
                ).unwrap();

                // Условие
                let cond_id = self.next_id();
                let op = if *cont { "<=" } else { "<" };
                writeln!(
                    self.dot,
                    "  {} [label=\"{} {} {}?\", shape=diamond, fillcolor=\"#BBDEFB\", color=\"#1976D2\"];",
                    cond_id, var, op, self.fmt_expr(end)
                ).unwrap();

                writeln!(self.dot, "  {} -> {};", id, cond_id).unwrap();

                // Тело
                let (b_entry, b_exit) = self.translate_block(body);
                
                // Инкремент
                let inc_id = self.next_id();
                writeln!(self.dot, "  {} [label=\"{} += 1\", style=dashed];", inc_id, var).unwrap();

                if let Some(entry) = b_entry {
                    writeln!(self.dot, "  {} -> {} [label=\"да\", fontcolor=\"#2E7D32\"];", cond_id, entry).unwrap();
                    // Если тело не прерывается (нет return/break), идем к инкременту
                    if let Some(exit) = b_exit {
                        writeln!(self.dot, "  {} -> {};", exit, inc_id).unwrap();
                    }
                } else {
                    writeln!(self.dot, "  {} -> {} [label=\"да\"];", cond_id, inc_id).unwrap();
                }

                // Возврат к условию
                writeln!(self.dot, "  {} -> {} [constraint=false, style=dashed, color=\"#90A4AE\"];", inc_id, cond_id).unwrap();

                // Выход
                let exit_id = self.next_id();
                writeln!(self.dot, "  {} [label=\"\", shape=point, width=0];", exit_id).unwrap();
                writeln!(self.dot, "  {} -> {} [label=\"нет\", fontcolor=\"#C62828\"];", cond_id, exit_id).unwrap();

                (id, exit_id)
            }
            Stmt::ForEach { var, collection, body } => {
                // Инициализация итератора
                let coll_str = self.fmt_expr(collection);
                writeln!(
                    self.dot,
                    "  {} [label=\"Итератор для {}\", shape=hexagon, fillcolor=\"#E1BEE7\", color=\"#7B1FA2\"];",
                    id, coll_str
                ).unwrap();

                // Условие "Есть следующий?"
                let check_id = self.next_id();
                writeln!(
                    self.dot,
                    "  {} [label=\"Есть элементы?\", shape=diamond, fillcolor=\"#BBDEFB\", color=\"#1976D2\"];",
                    check_id
                ).unwrap();

                writeln!(self.dot, "  {} -> {};", id, check_id).unwrap();

                // Извлечение (Next)
                let next_id = self.next_id();
                writeln!(self.dot, "  {} [label=\"{} = След. элемент\"];", next_id, var).unwrap();

                writeln!(self.dot, "  {} -> {} [label=\"да\", fontcolor=\"#2E7D32\"];", check_id, next_id).unwrap();

                // Тело
                let (b_entry, b_exit) = self.translate_block(body);
                
                if let Some(entry) = b_entry {
                    writeln!(self.dot, "  {} -> {};", next_id, entry).unwrap();
                    if let Some(exit) = b_exit {
                        writeln!(
                            self.dot, 
                            "  {} -> {} [constraint=false, style=dashed, color=\"#90A4AE\"];", 
                            exit, check_id
                        ).unwrap();
                    }
                } else {
                     writeln!(
                        self.dot, 
                        "  {} -> {} [constraint=false, style=dashed, color=\"#90A4AE\"];", 
                        next_id, check_id
                    ).unwrap();
                }

                // Выход
                let exit_id = self.next_id();
                writeln!(self.dot, "  {} [label=\"\", shape=point, width=0];", exit_id).unwrap();
                writeln!(self.dot, "  {} -> {} [label=\"нет\", fontcolor=\"#C62828\"];", check_id, exit_id).unwrap();

                (id, exit_id)
            }
        }
    }
}
