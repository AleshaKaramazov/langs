// visualizer.rs
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::ast::*;

pub struct Visualizer {
    dot: String,
    node_count: usize,
}

impl Visualizer {
    pub fn new() -> Self {
        Self {
            dot: String::from("digraph G {\n  node [fontname=\"Arial\", shape=box, style=filled, fillcolor=\"#f9f9f9\"];\n  edge [fontname=\"Arial\"];\n  rankdir=TB;\n"),
            node_count: 0,
        }
    }

    fn next_id(&mut self) -> String {
        self.node_count += 1;
        format!("n{}", self.node_count)
    }

    fn fmt_expr(&self, expr: &Expr) -> String {
        match expr {
            Expr::Int(i) => i.to_string(),
            Expr::Bool(b) => (if *b { "Истина" } else { "Ложь" }).to_string(),
            Expr::String(s) => format!("\\\"{}\\\"", s),
            Expr::Var(v) => v.clone(),
            Expr::Unary { op: UnaryOp::Not, right } => format!("не {}", self.fmt_expr(right)),
            Expr::Binary { left, op, right } => {
                let op_str = match op {
                    BinOp::Plus => "+", BinOp::Sub => "-", BinOp::Mult => "*",
                    BinOp::Div => "/", BinOp::Mod => "%", BinOp::Equal => "==",
                    BinOp::Less => "<", BinOp::Greater => ">",
                    BinOp::Or => "или", BinOp::And => "и",
                };
                format!("{} {} {}", self.fmt_expr(left), op_str, self.fmt_expr(right))
            }
            Expr::Call { name, args, intrinsic } => {
                let formatted_args: Vec<String> = args.iter().map(|a| self.fmt_expr(a)).collect();
                let suffix = if *intrinsic { "!" } else { "" };
                format!("{}{}({})", name, suffix, formatted_args.join(", "))
            } 
            Expr::Array(v) => {
                format!("{:?}", v) 
            }
            Expr::Index { target, index } => {
                format!("{:?} -> index {:?}", target, index)
            }
        }
    }

    pub fn translate(mut self, text: String) -> String {
        let lexer = Lexer::new(&text);
        let mut parser = Parser::new(lexer);
        let alg = parser.parse_algorithm();

        let start_node = self.next_id();
        self.dot.push_str(&format!("  {} [label=\"Начало: {}\", shape=ellipse, fillcolor=\"#e1f5fe\"];\n", start_node, alg.name));

        let last_node = self.translate_block(&alg.body, start_node);

        let end_node = self.next_id();
        self.dot.push_str(&format!("  {} [label=\"Конец\", shape=ellipse, fillcolor=\"#e1f5fe\"];\n", end_node));
        self.dot.push_str(&format!("  {} -> {};\n", last_node, end_node));

        self.dot.push_str("}\n");
        self.dot
    }

    // Обрабатывает блок и возвращает ID последнего узла в этом блоке
    fn translate_block(&mut self, stmts: &[Stmt], mut prev_node: String) -> String {
        for stmt in stmts {
            prev_node = self.translate_stmt(stmt, prev_node, None);
        }
        prev_node
    }

    fn translate_stmt(&mut self, stmt: &Stmt, prev_node: String, edge_label: Option<&str>) -> String {
        let current = self.next_id();
        let label_attr = if let Some(l) = edge_label { format!(" [label=\"{}\"]", l) } else { String::new() };

        match stmt {
            Stmt::Let { name, expr, .. } => {
                self.dot.push_str(&format!("  {} [label=\"пусть {} = {}\"];\n", current, name, self.fmt_expr(expr)));
                self.dot.push_str(&format!("  {} -> {}{};\n", prev_node, current, label_attr));
                current
            }
            Stmt::Assign { name, expr } => {
                self.dot.push_str(&format!("  {} [label=\"{} = {}\"];\n", current, name, self.fmt_expr(expr)));
                self.dot.push_str(&format!("  {} -> {}{};\n", prev_node, current, label_attr));
                current
            }
            Stmt::Expr(expr) => {
                let expr_str = self.fmt_expr(expr);
                // Подсвечиваем ввод/вывод зеленым
                let color = if expr_str.contains('!') { ", fillcolor=\"#c8e6c9\"" } else { "" };
                self.dot.push_str(&format!("  {} [label=\"{}\"{}];\n", current, expr_str, color));
                self.dot.push_str(&format!("  {} -> {}{};\n", prev_node, current, label_attr));
                current
            }
            Stmt::If { cond, then_body, else_if, else_body } => {
                // Рисуем ромб условия
                self.dot.push_str(&format!("  {} [label=\"{}?\", shape=diamond, fillcolor=\"#fff9c4\"];\n", current, self.fmt_expr(cond)));
                self.dot.push_str(&format!("  {} -> {}{};\n", prev_node, current, label_attr));

                let exit_if = self.next_id();
                self.dot.push_str(&format!("  {} [label=\"\", shape=point];\n", exit_if));

                // Ветка "ДА"
                if then_body.is_empty() {
                    self.dot.push_str(&format!("  {} -> {} [label=\"да\"];\n", current, exit_if));
                } else {
                    let last_in_then = self.translate_stmt(&then_body[0], current.clone(), Some("да"));
                    let final_then = self.translate_block(&then_body[1..], last_in_then);
                    self.dot.push_str(&format!("  {} -> {};\n", final_then, exit_if));
                }

                let mut last_cond_node = current;

                // Ветки "ИНАЧЕ ЕСЛИ"
                for (elif_cond, elif_body) in else_if {
                    let elif_id = self.next_id();
                    self.dot.push_str(&format!("  {} [label=\"{}?\", shape=diamond, fillcolor=\"#fff9c4\"];\n", elif_id, self.fmt_expr(elif_cond)));
                    self.dot.push_str(&format!("  {} -> {} [label=\"нет\"];\n", last_cond_node, elif_id));

                    if elif_body.is_empty() {
                        self.dot.push_str(&format!("  {} -> {} [label=\"да\"];\n", elif_id, exit_if));
                    } else {
                        let last_in_elif = self.translate_stmt(&elif_body[0], elif_id.clone(), Some("да"));
                        let final_elif = self.translate_block(&elif_body[1..], last_in_elif);
                        self.dot.push_str(&format!("  {} -> {};\n", final_elif, exit_if));
                    }
                    last_cond_node = elif_id;
                }

                // Ветка "ИНАЧЕ"
                if let Some(eb) = else_body {
                    if eb.is_empty() {
                        self.dot.push_str(&format!("  {} -> {} [label=\"нет\"];\n", last_cond_node, exit_if));
                    } else {
                        let last_in_else = self.translate_stmt(&eb[0], last_cond_node, Some("нет"));
                        let final_else = self.translate_block(&eb[1..], last_in_else);
                        self.dot.push_str(&format!("  {} -> {};\n", final_else, exit_if));
                    }
                } else {
                    self.dot.push_str(&format!("  {} -> {} [label=\"нет\"];\n", last_cond_node, exit_if));
                }

                exit_if
            }
            Stmt::While { cond, body } => {
                self.dot.push_str(&format!("  {} [label=\"пока {}\\n?\", shape=diamond, fillcolor=\"#fff9c4\"];\n", current, self.fmt_expr(cond)));
                self.dot.push_str(&format!("  {} -> {}{};\n", prev_node, current, label_attr));

                if !body.is_empty() {
                    let last_in_body = self.translate_stmt(&body[0], current.clone(), Some("да"));
                    let final_body = self.translate_block(&body[1..], last_in_body);
                    self.dot.push_str(&format!("  {} -> {} [constraint=false];\n", final_body, current));
                }

                let exit_node = self.next_id();
                self.dot.push_str(&format!("  {} [label=\"\", shape=point];\n", exit_node));
                self.dot.push_str(&format!("  {} -> {} [label=\"нет\"];\n", current, exit_node));
                exit_node
            }
            _ => {
                // Для остальных (For, AssignAdd и т.д.) логика аналогична Let/Expr
                self.dot.push_str(&format!("  {} [label=\"неподдерживаемый узел\"];\n", current));
                self.dot.push_str(&format!("  {} -> {}{};\n", prev_node, current, label_attr));
                current
            }
        }
    }
}
