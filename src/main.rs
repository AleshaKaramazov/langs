mod lexer;
mod parser;
mod ast;
mod interpreter;
mod visualizer;
mod env;
mod value;
mod checker; 

use std::fs;
use std::process;

use visualizer::Visualizer;
use lexer::Lexer;
use parser::Parser;
use interpreter::Interpreter;
use checker::TypeChecker; 

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Использование: язык <файл.алг>");
        process::exit(1);
    }

    let filename = &args[1];
    
    let source = match fs::read_to_string(filename) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("ОШИБКА: Не удалось прочитать файл '{}': {}", filename, e);
            process::exit(1);
        }
    };


    let lexer = Lexer::new(&source);
    
    let mut parser = Parser::new(lexer);
    let algorithm = parser.parse_algorithm();

    if args.len() > 2 && args[2] == "--draw" {
        let viz = Visualizer::new();
        let dot_output = viz.translate(algorithm);
        if let Err(e) = fs::write("graph.dot", dot_output) {
             eprintln!("ОШИБКА: Не удалось записать DOT файл: {}", e);
             process::exit(1);
        }
        println!("Граф сохранен в graph.dot. Используйте 'dot -Tpng graph.dot -o out.png'.");
        return;
    }

    if algorithm.name != "Главная" {
        eprintln!("ОШИБКА: В программе должен быть основной алгоритм с именем 'Главная'. Найдено: '{}'", algorithm.name);
        process::exit(1);
    }

    let mut checker = TypeChecker::new();
    if let Err(e) = checker.check_algorithm(&algorithm) {
        eprintln!("ОШИБКА ТИПИЗАЦИИ: {}", e);
        process::exit(1);
    }
    let mut interpreter = Interpreter::new();
    
    match interpreter.run(&algorithm) {
        Ok(_) => {
            println!("ПРОГРАММА ЗАВЕРШИЛАСЬ УСПЕШНО!");
        },
        Err(e) => {
            eprintln!("ОШИБКА ВЫПОЛНЕНИЯ: {}", e);
            process::exit(1);
        }
    }
}
