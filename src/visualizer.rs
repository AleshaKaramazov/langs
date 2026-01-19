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
            dot: String::from("digraph G {\n  node [fontname=\"Arial\", shape=box, style=filled, fillcolor=\"#f9f9f9\"];\n  edge [fontname=\"Arial\"];\n"),
            node_count: 0,
        }
    }
    fn fmt_expr(&self, expr: &Expr) -> String {
        match expr {
            Expr::Int(i) => i.to_string(),
            Expr::Bool(b) => (if *b { "Истина" } else { "Ложь" }).to_string(),
            Expr::String(s) => format!("\\\"{}\\\"", s), // Экранируем кавычки для DOT
            Expr::Var(v) => v.clone(),
            Expr::Binary { left, op, right } => {
                let op_str = match op {
                BinOp::Plus => "+",
                BinOp::Sub => "-",
                BinOp::Mult => "*",
                BinOp::Div => "/",
                BinOp::Mod => "%",
                BinOp::Equal => "==",
                BinOp::Less => "<",
                BinOp::Greater => ">",
                BinOp::Or => "или",
                BinOp::And => "и",
                };
                format!("{} {} {}", self.fmt_expr(left), op_str, self.fmt_expr(right))
            }
            Expr::Call { name, args, .. } => {
                let formatted_args: Vec<String> = args.iter().map(|a| self.fmt_expr(a)).collect();
                format!("{}({})", name, formatted_args.join(", "))
            }
        }
    }

    fn next_id(&mut self) -> String {
        self.node_count += 1;
        format!("n{}", self.node_count)
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

    fn translate_block(&mut self, stmts: &[Stmt], mut prev_node: String) -> String {
        for stmt in stmts {
            prev_node = self.translate_stmt(stmt, prev_node);
        }
        prev_node
    }

    fn translate_stmt(&mut self, stmt: &Stmt, prev_node: String) -> String {
        let current = self.next_id();

        match stmt {
            Stmt::For { var, start, end, body } => {
                let label = format!("Для {} в диапазоне {}..{}", var, self.fmt_expr(start), self.fmt_expr(end));
                self.dot.push_str(&format!("  {} [label=\"{}\", shape=diamond, fillcolor=\"#e1bee7\"];\n", current, label));
                self.dot.push_str(&format!("  {} -> {};\n", prev_node, current));

                let last_body_node = self.translate_block(body, current.clone());
                // Возврат к началу цикла
                self.dot.push_str(&format!("  {} -> {} [constraint=false, color=\"#9c27b0\"];\n", last_body_node, current));

                let after_for = self.next_id();
                self.dot.push_str(&format!("  {} [label=\"\", shape=point];\n", after_for));
                self.dot.push_str(&format!("  {} -> {} [label=\"завершено\"];\n", current, after_for));
                after_for
            }
            Stmt::If { cond, then_body, else_body, .. } => {
                self.dot.push_str(&format!("  {} [label=\"{}?\", shape=diamond, fillcolor=\"#fff9c4\"];\n", current, self.fmt_expr(cond)));
                self.dot.push_str(&format!("  {} -> {};\n", prev_node, current));

                let end_if = self.next_id();
                self.dot.push_str(&format!("  {} [label=\"\", shape=point];\n", end_if));

                let last_then = self.translate_block(then_body, current.clone());
                self.dot.push_str(&format!("  {} -> {} [label=\"да\"];\n", current, last_then));
                self.dot.push_str(&format!("  {} -> {};\n", last_then, end_if));

                if let Some(else_stmts) = else_body {
                    let last_else = self.translate_block(else_stmts, current.clone());
                    self.dot.push_str(&format!("  {} -> {} [label=\"нет\"];\n", current, last_else));
                    self.dot.push_str(&format!("  {} -> {};\n", last_else, end_if));
                } else {
                    self.dot.push_str(&format!("  {} -> {} [label=\"нет\"];\n", current, end_if));
                }
                end_if
            }
            Stmt::Expr(expr) => {
                self.dot.push_str(&format!("  {} [label=\"{}\"];\n", current, self.fmt_expr(expr)));
                self.dot.push_str(&format!("  {} -> {};\n", prev_node, current));
                current
            }
            Stmt::Assign { name, expr } => {
                self.dot.push_str(&format!("  {} [label=\"{} = {}\"];\n", current, name, self.fmt_expr(expr)));
                self.dot.push_str(&format!("  {} -> {};\n", prev_node, current));
                current
            }
            _ => {
                self.dot.push_str(&format!("  {} [label=\"Команда\"];\n", current));
                self.dot.push_str(&format!("  {} -> {};\n", prev_node, current));
                current
            }
        }
    }
}
