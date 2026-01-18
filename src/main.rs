mod lexer;
mod parser;
mod ast;
mod interpreter;
mod env;
mod value;

use std::fs;

use lexer::Lexer;
use parser::Parser;
use interpreter::Interpreter;

fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() < 2 {
        eprintln!("использование: язык <файл.алг>");
        return;
    }

    let filename = &args[1];
    let source = fs::read_to_string(filename)
        .expect("не удалось прочитать файл");

    let lexer = Lexer::new(&source);
    let mut parser = Parser::new(lexer);

    let algorithm = parser.parse_algorithm();

    if algorithm.name != "Главная" {
        panic!("в программе должен быть Алгоритм Главная");
    }

    let mut interpreter = Interpreter::new();
    interpreter.run(&algorithm);
}
