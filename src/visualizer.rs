use crate::ast::*;
use std::fmt::Write;

pub struct Visualizer {
    dot: String,
    node_count: usize,
}

impl Visualizer {
    pub fn new() -> Self {
        let mut dot = String::new();
        writeln!(dot, "digraph Flowchart {{").unwrap();
        
        writeln!(dot, "  fontname=\"Arial, Helvetica, sans-serif\";").unwrap();
        writeln!(dot, "  node [fontname=\"Arial, Helvetica, sans-serif\", fontsize=12];").unwrap();
        writeln!(dot, "  edge [fontname=\"Arial, Helvetica, sans-serif\", fontsize=10];").unwrap();
        writeln!(dot, "  forcelabels=true;").unwrap(); 
        
        writeln!(dot, "  splines=ortho;").unwrap(); 
        writeln!(dot, "  nodesep=0.8;").unwrap(); 
        writeln!(dot, "  ranksep=0.6;").unwrap(); 
        
        writeln!(dot, "  node [shape=rect, style=\"filled,rounded\", fillcolor=\"#E3F2FD\", color=\"#90CAF9\", penwidth=1];").unwrap();
        
        Self { dot, node_count: 0 }
    }

    fn next_id(&mut self) -> String {
        self.node_count += 1;
        format!("node_{}", self.node_count)
    }

    pub fn translate(mut self, alg: Algorithm) -> String {
        let start_id = self.next_id();
        let args_str: Vec<String> = alg.args.iter().map(|(n, _)| n.clone()).collect();
        let start_label = if args_str.is_empty() {
            format!("НАЧАЛО\n{}", alg.name)
        } else {
            format!("НАЧАЛО\n{}({})", alg.name, args_str.join(", "))
        };
        
        self.emit_node(&start_id, &start_label, "terminator", "#C8E6C9", "#2E7D32");

        let (entry, exit) = self.process_block(&alg.body);

        if let Some(e) = entry {
            self.emit_edge(&start_id, &e);
        }

        let end_id = self.next_id();
        self.emit_node(&end_id, "КОНЕЦ", "terminator", "#FFCDD2", "#C62828");

        if let Some(ex) = exit {
            self.emit_edge(&ex, &end_id);
        } else if alg.body.is_empty() {
             self.emit_edge(&start_id, &end_id);
        }

        self.dot.push_str("}\n");
        self.dot
    }

    fn process_block(&mut self, stmts: &[Stmt]) -> (Option<String>, Option<String>) {
        if stmts.is_empty() {
            return (None, None);
        }

        let mut first_node = None;
        let mut prev_exit_node: Option<String> = None;

        for stmt in stmts {
            let (curr_entry, curr_exit) = self.process_stmt(stmt);

            if first_node.is_none() {
                first_node = Some(curr_entry.clone());
            }

            if let Some(prev) = prev_exit_node {
                self.emit_edge(&prev, &curr_entry);
            }

            match stmt {
                Stmt::Return(_) | Stmt::Break | Stmt::Continue => {
                    prev_exit_node = None;
                }
                _ => {
                    prev_exit_node = curr_exit;
                }
            }
        }

        (first_node, prev_exit_node)
    }

    fn process_stmt(&mut self, stmt: &Stmt) -> (String, Option<String>) {
        match stmt {
            Stmt::Let { name, expr, .. } => {
                let id = self.next_id();
                let label = match expr {
                    Some(e) => format!("{} := {}", name, self.fmt_expr(e)),
                    None => format!("Объявить {}", name),
                };
                self.emit_node(&id, &label, "process", "#E3F2FD", "#42A5F5");
                (id.clone(), Some(id))
            }
            Stmt::Assign { name, expr } => {
                let id = self.next_id();
                let label = format!("{} := {}", name, self.fmt_expr(expr));
                self.emit_node(&id, &label, "process", "#E3F2FD", "#42A5F5");
                (id.clone(), Some(id))
            }
            Stmt::AssignAdd { name, expr } => {
                let id = self.next_id();
                let label = format!("{} ← {} + {}", name, name, self.fmt_expr(expr));
                self.emit_node(&id, &label, "process", "#E3F2FD", "#42A5F5");
                (id.clone(), Some(id))
            }
            Stmt::AssignSub { name, expr } => {
                let id = self.next_id();
                let label = format!("{} ← {} - {}", name, name, self.fmt_expr(expr));
                self.emit_node(&id, &label, "process", "#E3F2FD", "#42A5F5");
                (id.clone(), Some(id))
            }
            Stmt::AssignMult { name, expr } => {
                let id = self.next_id();
                let label = format!("{} ← {} × {}", name, name, self.fmt_expr(expr));
                self.emit_node(&id, &label, "process", "#E3F2FD", "#42A5F5");
                (id.clone(), Some(id))
            }
            Stmt::AssignDiv { name, expr } => {
                let id = self.next_id();
                let label = format!("{} ← {} / {}", name, name, self.fmt_expr(expr));
                self.emit_node(&id, &label, "process", "#E3F2FD", "#42A5F5");
                (id.clone(), Some(id))
            }
            Stmt::Expr(expr) => {
                let id = self.next_id();
                let label = self.fmt_expr(expr);
                
                let is_io = match expr {
                    Expr::Call { name, intrinsic, .. } => *intrinsic && (name == "Написать" || name == "Считать" || name == "ЧистКонсоль"),
                    Expr::MethodCall { method, .. } => method == "Считать", 
                    _ => false,
                };

                if is_io {
                    self.emit_node(&id, &label, "io", "#FFE0B2", "#FB8C00");
                } else {
                    self.emit_node(&id, &label, "process", "#E3F2FD", "#42A5F5");
                }
                (id.clone(), Some(id))
            }
            Stmt::Return(maybe_expr) => {
                let id = self.next_id();
                let label = match maybe_expr {
                    Some(e) => format!("Вернуть {}", self.fmt_expr(e)),
                    None => "Вернуть".to_string(),
                };
                self.emit_node(&id, &label, "terminator", "#FFCDD2", "#E53935");
                (id, None)
            }
            Stmt::Break => {
                let id = self.next_id();
                self.emit_node(&id, "ПРЕРВАТЬ", "terminator", "#FFCCBC", "#D84315");
                (id, None)
            }
            Stmt::Continue => {
                let id = self.next_id();
                self.emit_node(&id, "ПРОДОЛЖИТЬ", "terminator", "#FFCCBC", "#D84315");
                (id, None)
            }
            Stmt::If { cond, then_body, else_if, else_body } => {
                let cond_id = self.next_id();
                let label = format!("{}?", self.fmt_expr(cond));
                self.emit_node(&cond_id, &label, "decision", "#FFF9C4", "#FBC02D");

                let merge_id = self.next_id();
                writeln!(self.dot, "  {} [shape=point, width=0];", merge_id).unwrap();

                let (then_entry, then_exit) = self.process_block(then_body);
                if let Some(entry) = then_entry {
                    self.emit_edge_label(&cond_id, &entry, "Да", "green");
                    if let Some(exit) = then_exit {
                        self.emit_edge(&exit, &merge_id);
                    }
                } else {
                    self.emit_edge_label(&cond_id, &merge_id, "Да", "green");
                }

                let mut current_cond_id = cond_id.clone();
                
                for (elif_cond, elif_body) in else_if {
                    let elif_id = self.next_id();
                    let e_label = format!("{}?", self.fmt_expr(elif_cond));
                    self.emit_node(&elif_id, &e_label, "decision", "#FFF9C4", "#FBC02D");
                    
                    self.emit_edge_label(&current_cond_id, &elif_id, "Нет", "red");
                    
                    let (b_entry, b_exit) = self.process_block(elif_body);
                    if let Some(entry) = b_entry {
                        self.emit_edge_label(&elif_id, &entry, "Да", "green");
                        if let Some(exit) = b_exit {
                            self.emit_edge(&exit, &merge_id);
                        }
                    } else {
                        self.emit_edge_label(&elif_id, &merge_id, "Да", "green");
                    }
                    current_cond_id = elif_id;
                }

                if let Some(body) = else_body {
                    let (else_entry, else_exit) = self.process_block(body);
                    if let Some(entry) = else_entry {
                        self.emit_edge_label(&current_cond_id, &entry, "Нет", "red");
                        if let Some(exit) = else_exit {
                            self.emit_edge(&exit, &merge_id);
                        }
                    } else {
                        self.emit_edge_label(&current_cond_id, &merge_id, "Нет", "red");
                    }
                } else {
                    self.emit_edge_label(&current_cond_id, &merge_id, "Нет", "red");
                }

                (cond_id, Some(merge_id))
            }
            Stmt::While { cond, body } => {
                let check_id = self.next_id();
                let label = format!("Пока {}?", self.fmt_expr(cond));
                self.emit_node(&check_id, &label, "decision", "#FFF9C4", "#FBC02D");

                let (b_entry, b_exit) = self.process_block(body);

                if let Some(entry) = b_entry {
                    self.emit_edge_label(&check_id, &entry, "Да", "green");
                    if let Some(exit) = b_exit {
                        writeln!(self.dot, "  {} -> {} [constraint=false, color=\"#546E7A\"];", exit, check_id).unwrap();
                    }
                } else {
                    writeln!(self.dot, "  {} -> {} [xlabel=\"Да\", fontcolor=\"#2E7D32\", constraint=false];", check_id, check_id).unwrap();
                }
                
                let exit_id = self.next_id();
                writeln!(self.dot, "  {} [shape=point, width=0];", exit_id).unwrap();
                self.emit_edge_label(&check_id, &exit_id, "Нет", "red");

                (check_id, Some(exit_id))
            }
            Stmt::For { var, start, cont: _, end, body } => {
                let init_id = self.next_id();
                let init_label = format!("{} = {}", var, self.fmt_expr(start));
                self.emit_node(&init_id, &init_label, "preparation", "#E1BEE7", "#8E24AA");

                let cond_id = self.next_id();
                let cond_label = format!("{} <= {}?", var, self.fmt_expr(end));
                self.emit_node(&cond_id, &cond_label, "decision", "#FFF9C4", "#FBC02D");

                self.emit_edge(&init_id, &cond_id);

                let (b_entry, b_exit) = self.process_block(body);
                
                let inc_id = self.next_id();
                let inc_label = format!("{} += 1", var);
                self.emit_node(&inc_id, &inc_label, "process", "#F3E5F5", "#8E24AA");

                if let Some(entry) = b_entry {
                    self.emit_edge_label(&cond_id, &entry, "Да", "green");
                    if let Some(exit) = b_exit {
                         self.emit_edge(&exit, &inc_id);
                    }
                } else {
                     self.emit_edge_label(&cond_id, &inc_id, "Да", "green");
                }

                writeln!(self.dot, "  {} -> {} [constraint=false, color=\"#546E7A\"];", inc_id, cond_id).unwrap();

                let exit_id = self.next_id();
                writeln!(self.dot, "  {} [shape=point, width=0];", exit_id).unwrap();
                self.emit_edge_label(&cond_id, &exit_id, "Нет", "red");

                (init_id, Some(exit_id))
            }
            Stmt::ForEach { var, collection, body } => {
                let init_id = self.next_id();
                let init_label = format!("Итератор\n{}", self.fmt_expr(collection));
                self.emit_node(&init_id, &init_label, "preparation", "#E1BEE7", "#8E24AA");

                let check_id = self.next_id();
                self.emit_node(&check_id, "Есть элементы?", "decision", "#FFF9C4", "#FBC02D");
                self.emit_edge(&init_id, &check_id);

                let fetch_id = self.next_id();
                let fetch_label = format!("{} = след.", var);
                self.emit_node(&fetch_id, &fetch_label, "process", "#E1BEE7", "#8E24AA");
                
                self.emit_edge_label(&check_id, &fetch_id, "Да", "green");

                let (b_entry, b_exit) = self.process_block(body);
                
                if let Some(entry) = b_entry {
                    self.emit_edge(&fetch_id, &entry);
                    if let Some(exit) = b_exit {
                        writeln!(self.dot, "  {} -> {} [constraint=false, color=\"#546E7A\"];", exit, check_id).unwrap();
                    }
                } else {
                     writeln!(self.dot, "  {} -> {} [constraint=false, color=\"#546E7A\"];", fetch_id, check_id).unwrap();
                }

                let exit_id = self.next_id();
                writeln!(self.dot, "  {} [shape=point, width=0];", exit_id).unwrap();
                self.emit_edge_label(&check_id, &exit_id, "Нет", "red");

                (init_id, Some(exit_id))
            }
        }
    }


    fn emit_node(&mut self, id: &str, label: &str, kind: &str, fill: &str, stroke: &str) {
        let safe_label = label.replace("\"", "\\\"");
        
        let shape_attr = match kind {
            "terminator" => "shape=box, style=\"filled,rounded\"", 
            "process" => "shape=box, style=\"filled\"",
            "decision" => "shape=diamond, style=\"filled\"",
            "io" => "shape=parallelogram, style=\"filled\"",
            "preparation" => "shape=hexagon, style=\"filled\"",
            _ => "shape=box, style=\"filled\"",
        };

        writeln!(
            self.dot,
            "  {} [label=\"{}\", {}, fillcolor=\"{}\", color=\"{}\"];",
            id, safe_label, shape_attr, fill, stroke
        ).unwrap();
    }

    fn emit_edge(&mut self, from: &str, to: &str) {
         writeln!(self.dot, "  {} -> {} [color=\"#546E7A\"];", from, to).unwrap();
    }

    fn emit_edge_label(&mut self, from: &str, to: &str, label: &str, color: &str) {
        let font_color = match color {
            "green" => "#2E7D32",
            "red" => "#C62828",
            _ => "black"
        };
        writeln!(
            self.dot, 
            "  {} -> {} [xlabel=\"{}\", fontcolor=\"{}\", color=\"#546E7A\", fontsize=10];", 
            from, to, label, font_color
        ).unwrap();
    }

    fn fmt_expr(&self, expr: &Expr) -> String {
        match expr {
            Expr::Cast { target_type, expr } => {
                let type_str = match target_type {
                    Type::Int => "Цел",
                    Type::UInt => "Нат",
                    Type::Float => "Десятич",
                    Type::Bool => "Лог",
                    Type::String => "Строка",
                    Type::Char => "Символ",
                    _ => "?",
                };
                format!("({}) {}", type_str, self.fmt_expr(expr))
            } 
            Expr::UInt(i) => i.to_string(),
            Expr::Int(i) => i.to_string(),
            Expr::Float(f) => format!("{:.2}", f),
            Expr::Bool(b) => (if *b { "Истина" } else { "Ложь" }).to_string(),
            Expr::String(s) => format!("\"{}\"", s), 
            Expr::Char(c) => format!("'{}'", c),
            Expr::Var(v) => v.clone(),
            Expr::Array(_) => "[...]".to_string(),
            Expr::Index { target, index } => format!("{}[{}]", self.fmt_expr(target), self.fmt_expr(index)),
            Expr::Unary { op, right } => {
                let s = match op { UnaryOp::Not => "НЕ " };
                format!("{}{}", s, self.fmt_expr(right))
            }
            Expr::Binary { left, op, right } => {
                let op_str = match op {
                    BinOp::Plus => "+", BinOp::Sub => "-", BinOp::Mult => "×", BinOp::Div => "/",
                    BinOp::Mod => "%", BinOp::Equal => "=", BinOp::Less => "<", BinOp::Greater => ">",
                    BinOp::Or => "ИЛИ", BinOp::And => "И", BinOp::NotEqual => "≠",
                    BinOp::GreaterOrEqual => "≥", BinOp::LessOrEqual => "≤",
                };
                format!("{} {} {}", self.fmt_expr(left), op_str, self.fmt_expr(right))
            }
            Expr::Call { name, args, .. } => {
                 let args_str: Vec<String> = args.iter().map(|a| self.fmt_expr(a)).collect();
                 if name == "Написать" || name == "Считать" {
                      args_str.join(", ")
                 } else {
                      format!("{}({})", name, args_str.join(", "))
                 }
            }
            Expr::MethodCall { target, method, args } => {
                let args_str: Vec<String> = args.iter().map(|a| self.fmt_expr(a)).collect();
                if args_str.is_empty() {
                    format!("{}.{}", self.fmt_expr(target), method)
                } else {
                    format!("{}.{}({})", self.fmt_expr(target), method, args_str.join(", "))
                }
            }
            Expr::NativeCall { path, .. } => format!("Нативная[{}]", path),
            Expr::Lambda { .. } => "λ(...)".to_string(),
            Expr::PreInc(e) => format!("++{}", self.fmt_expr(e)),
            Expr::PreDec(e) => format!("--{}", self.fmt_expr(e)),
            Expr::PostInc(e) => format!("{}++", self.fmt_expr(e)),
            Expr::PostDec(e) => format!("{}--", self.fmt_expr(e)),
        }
    }
}
