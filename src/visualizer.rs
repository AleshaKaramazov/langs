use crate::ast::*;
use std::fmt::Write;

pub struct Visualizer {
    dot: String,
    node_count: usize,
}

impl Visualizer {
    pub fn new() -> Self {
        let mut dot = String::new();
        writeln!(dot, "digraph G {{").unwrap();
        writeln!(dot, "  fontname=\"Inter,Segoe UI,Arial\";").unwrap();
        writeln!(dot, "  nodesep=0.6; ranksep=0.5;").unwrap();
        writeln!(dot, "  rankdir=TB;").unwrap();
        writeln!(dot, "  splines=ortho;").unwrap();
        writeln!(dot, "  forcelabels=true;").unwrap();

        writeln!(dot, "  node [fontname=\"Inter,Segoe UI,Arial\", fontsize=12, shape=box, style=\"rounded,filled\", fillcolor=\"#f8f9fa\", color=\"#2d3436\", penwidth=1.2, margin=\"0.2,0.1\"];").unwrap();
        writeln!(dot, "  edge [fontname=\"Inter,Segoe UI,Arial\", fontsize=10, color=\"#636e72\", penwidth=1.0, arrowhead=vee, arrowsize=0.7];").unwrap();

        Self { dot, node_count: 0 }
    }

    fn next_id(&mut self) -> String {
        self.node_count += 1;
        format!("n{}", self.node_count)
    }

    fn fmt_expr(&self, expr: &Expr) -> String {
        match expr {
            Expr::Int(i) => i.to_string(),
            Expr::Bool(b) => (if *b { "истина" } else { "ложь" }).to_string(),
            Expr::String(s) => format!("\\\"{}\\\"", s),
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
                    UnaryOp::Not => "¬",
                };
                format!("{} {}", s, self.fmt_expr(right))
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
                    BinOp::Or => "или",
                    BinOp::And => "и",
                    BinOp::NotEqual => "≠",
                    BinOp::GreaterOrEqual => ">=",
                    BinOp::LessOrEqual => "<=",
                };
                format!(
                    "({} {} {})",
                    self.fmt_expr(left),
                    op_str,
                    self.fmt_expr(right)
                )
            }
            Expr::Call {
                name,
                args,
                intrinsic,
            } => {
                let formatted_args: Vec<String> = args.iter().map(|a| self.fmt_expr(a)).collect();
                format!(
                    "{}{}({})",
                    name,
                    if *intrinsic { "!" } else { "" },
                    formatted_args.join(", ")
                )
            }
            _ => "can't ".to_string(),
        }
    }

    pub fn translate(mut self, alg: Algorithm) -> String {
        let start_node = self.next_id();
        writeln!(
            self.dot,
            "  {} [label=\"НАЧАЛО: {}\", shape=ellipse, fillcolor=\"#e1f5fe\", color=\"#01579b\"];",
            start_node, alg.name
        )
        .unwrap();

        let (body_entry, body_exit) = self.translate_block(&alg.body);

        let end_node = self.next_id();
        writeln!(
            self.dot,
            "  {} [label=\"КОНЕЦ\", shape=ellipse, fillcolor=\"#e1f5fe\", color=\"#01579b\"];",
            end_node
        )
        .unwrap();

        if let Some(entry) = body_entry {
            writeln!(self.dot, "  {} -> {};", start_node, entry).unwrap();
            writeln!(self.dot, "  {} -> {};", body_exit.unwrap(), end_node).unwrap();
        } else {
            writeln!(self.dot, "  {} -> {};", start_node, end_node).unwrap();
        }

        self.dot.push_str("}\n");
        self.dot
    }

    fn translate_block(&mut self, stmts: &[Stmt]) -> (Option<String>, Option<String>) {
        if stmts.is_empty() {
            return (None, None);
        }
        let mut first_id = None;
        let mut prev_exit = None;

        for stmt in stmts {
            let (stmt_entry, stmt_exit) = self.translate_stmt(stmt);
            if let Some(prev) = prev_exit {
                writeln!(self.dot, "  {} -> {};", prev, stmt_entry).unwrap();
            } else {
                first_id = Some(stmt_entry.clone());
            }
            prev_exit = Some(stmt_exit);
        }
        (first_id, prev_exit)
    }

    fn translate_stmt(&mut self, stmt: &Stmt) -> (String, String) {
        let current_id = self.next_id();

        match stmt {
            Stmt::Let { name, expr, .. } => {
                writeln!(
                    self.dot,
                    "  {} [label=\"пусть {} = {}\"];",
                    current_id,
                    name,
                    match expr {
                        Some(expr) => self.fmt_expr(expr),
                        None => "NULL".to_string(),
                    }
                )
                .unwrap();
                (current_id.clone(), current_id)
            }
            Stmt::Assign { name, expr }
            | Stmt::AssignAdd { name, expr }
            | Stmt::AssignSub { name, expr }
            | Stmt::AssignMult { name, expr }
            | Stmt::AssignDiv { name, expr } => {
                let op = match stmt {
                    Stmt::AssignAdd { .. } => "+=",
                    Stmt::AssignSub { .. } => "-=",
                    Stmt::AssignMult { .. } => "*=",
                    Stmt::AssignDiv { .. } => "/=",
                    _ => "=",
                };
                writeln!(
                    self.dot,
                    "  {} [label=\"{} {} {}\"];",
                    current_id,
                    name,
                    op,
                    self.fmt_expr(expr)
                )
                .unwrap();
                (current_id.clone(), current_id)
            }
            Stmt::Expr(expr) => {
                let is_io = if let Expr::Call { intrinsic, .. } = expr {
                    *intrinsic
                } else {
                    false
                };
                let label = self.fmt_expr(expr);
                if is_io {
                    writeln!(self.dot, "  {} [label=\"{}\", shape=parallelogram, fillcolor=\"#e8f5e9\", color=\"#2e7d32\"];", current_id, label).unwrap();
                } else {
                    writeln!(self.dot, "  {} [label=\"{}\"];", current_id, label).unwrap();
                }
                (current_id.clone(), current_id)
            }
            Stmt::If {
                cond,
                then_body,
                else_if,
                else_body,
            } => {
                writeln!(self.dot, "  {} [label=\"{}?\", shape=diamond, fillcolor=\"#fff9c4\", color=\"#fbc02d\"];", current_id, self.fmt_expr(cond)).unwrap();
                let exit_id = self.next_id();
                writeln!(self.dot, "  {} [label=\"\", shape=circle, width=0.1, height=0.1, fillcolor=\"#2d3436\"];", exit_id).unwrap();

                let (then_entry, then_exit) = self.translate_block(then_body);
                if let Some(entry) = then_entry {
                    writeln!(self.dot, "  {} -> {} [xlabel=\"да\"];", current_id, entry).unwrap();
                    writeln!(self.dot, "  {} -> {};", then_exit.unwrap(), exit_id).unwrap();
                } else {
                    writeln!(self.dot, "  {} -> {} [xlabel=\"да\"];", current_id, exit_id).unwrap();
                }

                let mut last_cond_id = current_id.clone();
                for (elif_cond, elif_body) in else_if {
                    let elif_id = self.next_id();
                    writeln!(
                        self.dot,
                        "  {} [label=\"{}?\", shape=diamond, fillcolor=\"#fff9c4\"];",
                        elif_id,
                        self.fmt_expr(elif_cond)
                    )
                    .unwrap();
                    writeln!(
                        self.dot,
                        "  {} -> {} [xlabel=\"нет\"];",
                        last_cond_id, elif_id
                    )
                    .unwrap();

                    let (e_entry, e_exit) = self.translate_block(elif_body);
                    if let Some(entry) = e_entry {
                        writeln!(self.dot, "  {} -> {} [xlabel=\"да\"];", elif_id, entry).unwrap();
                        writeln!(self.dot, "  {} -> {};", e_exit.unwrap(), exit_id).unwrap();
                    } else {
                        writeln!(self.dot, "  {} -> {} [xlabel=\"да\"];", elif_id, exit_id)
                            .unwrap();
                    }
                    last_cond_id = elif_id;
                }

                if let Some(eb) = else_body {
                    let (else_entry, else_exit) = self.translate_block(eb);
                    if let Some(entry) = else_entry {
                        writeln!(
                            self.dot,
                            "  {} -> {} [xlabel=\"нет\"];",
                            last_cond_id, entry
                        )
                        .unwrap();
                        writeln!(self.dot, "  {} -> {};", else_exit.unwrap(), exit_id).unwrap();
                    } else {
                        writeln!(self.dot, "  {} -> {};", last_cond_id, exit_id).unwrap();
                    }
                } else {
                    writeln!(
                        self.dot,
                        "  {} -> {} [xlabel=\"нет\"];",
                        last_cond_id, exit_id
                    )
                    .unwrap();
                }
                (current_id, exit_id)
            }
            Stmt::While { cond, body } => {
                writeln!(self.dot, "  {} [label=\"{}?\", shape=diamond, fillcolor=\"#ffe0b2\", color=\"#e65100\"];", current_id, self.fmt_expr(cond)).unwrap();
                let exit_id = self.next_id();
                writeln!(
                    self.dot,
                    "  {} [label=\"\", shape=circle, width=0.1, height=0.1];",
                    exit_id
                )
                .unwrap();

                let (b_entry, b_exit) = self.translate_block(body);
                if let Some(entry) = b_entry {
                    writeln!(self.dot, "  {} -> {} [xlabel=\"да\"];", current_id, entry).unwrap();
                    writeln!(
                        self.dot,
                        "  {} -> {} [constraint=false, style=dashed, xlabel=\"loop\"];",
                        b_exit.unwrap(),
                        current_id
                    )
                    .unwrap();
                } else {
                    writeln!(
                        self.dot,
                        "  {} -> {} [xlabel=\"да\"];",
                        current_id, current_id
                    )
                    .unwrap();
                }
                writeln!(
                    self.dot,
                    "  {} -> {} [xlabel=\"нет\"];",
                    current_id, exit_id
                )
                .unwrap();
                (current_id, exit_id)
            }
            Stmt::For {
                var,
                start,
                cont,
                end,
                body,
            } => {
                let init_id = self.next_id();
                writeln!(
                    self.dot,
                    "  {} [label=\"{} = {}\"];",
                    init_id,
                    var,
                    self.fmt_expr(start)
                )
                .unwrap();
                let cond_id = self.next_id();
                writeln!(
                    self.dot,
                    "  {} [label=\"{} <{} {}?\", shape=diamond, fillcolor=\"#ffe0b2\"];",
                    cond_id,
                    var,
                    if *cont { "=" } else { "" },
                    self.fmt_expr(end)
                )
                .unwrap();
                writeln!(self.dot, "  {} -> {};", init_id, cond_id).unwrap();

                let (b_entry, b_exit) = self.translate_block(body);
                let inc_id = self.next_id();
                writeln!(self.dot, "  {} [label=\"{}++\"];", inc_id, var).unwrap();

                if let Some(entry) = b_entry {
                    writeln!(self.dot, "  {} -> {} [xlabel=\"да\"];", cond_id, entry).unwrap();
                    writeln!(self.dot, "  {} -> {};", b_exit.unwrap(), inc_id).unwrap();
                } else {
                    writeln!(self.dot, "  {} -> {} [xlabel=\"да\"];", cond_id, inc_id).unwrap();
                }
                writeln!(
                    self.dot,
                    "  {} -> {} [constraint=false, style=dashed];",
                    inc_id, cond_id
                )
                .unwrap();

                let exit_id = self.next_id();
                writeln!(
                    self.dot,
                    "  {} [label=\"\", shape=circle, width=0.1];",
                    exit_id
                )
                .unwrap();
                writeln!(self.dot, "  {} -> {} [xlabel=\"нет\"];", cond_id, exit_id).unwrap();
                (init_id, exit_id)
            }
            Stmt::ForEach {
                var: _,
                collection: _,
                body: _,
            } => todo!(),
            _ => unreachable!(),
        }
    }
}
