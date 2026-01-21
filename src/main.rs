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
    
    let program = parser.parse_program(); 

    let main_algorithm = program.algorithms.iter()
        .find(|alg| alg.name == "Главная")
        .cloned();

    if args.len() > 2 && args[2] == "--draw" {
        if let Some(alg) = main_algorithm {
            let viz = Visualizer::new();
            let dot_output = viz.translate(alg);
            if let Err(e) = fs::write("graph.dot", dot_output) {
                 eprintln!("ОШИБКА: Не удалось записать DOT файл: {}", e);
                 process::exit(1);
            }
            println!("Граф сохранен в graph.dot.");
        } else {
            eprintln!("ОШИБКА: Нечего рисовать, алгоритм 'Главная' не найден.");
        }
        return;
    }

    if main_algorithm.is_none() {
        eprintln!("ОШИБКА: В программе должен быть основной алгоритм с именем 'Главная'.");
        process::exit(1);
    }

    let mut checker = TypeChecker::new();
    if let Err(e) = checker.check_program(&program) {
        eprintln!("ОШИБКА ТИПИЗАЦИИ: {}", e);
        process::exit(1);
    }

    let mut interpreter = Interpreter::new();
    match interpreter.run(&program) {
        Ok(_) => {
            //println!("ПРОГРАММА ЗАВЕРШИЛАСЬ УСПЕШНО!");
        },
        Err(e) => {
            eprintln!("ОШИБКА ВЫПОЛНЕНИЯ: {}", e);
            process::exit(1);
        }
    }
}
