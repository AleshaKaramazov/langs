mod lexer;
mod parser;
mod ast;
mod interpreter;
mod visualizer;
mod env;
mod value;
mod checker; 
mod library;

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

    if args.len() > 2 && args[2] == "--export" {
        match process_export(&source, filename) {
            Ok(count) => println!("ГОТОВО! Экспортировано функций: {}", count),
            Err(e) => eprintln!("Ошибка: {}", e),
        }
        return;
    }



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


fn process_export(source: &str, filename: &str) -> Result<usize, Box<dyn std::error::Error>> {
    use std::io::{BufWriter, Write};
    
    let mut writer = BufWriter::new(fs::File::create(filename)?);
    let mut export_count = 0;
    
    for (line_num, line) in source.lines().enumerate() {
        if line.starts_with("экспорт") {
            let func_name = if let Some(name) = line.split_whitespace().nth(1) {
                name 
            }
            else {
                println!("Строка {}: некорректный формат экспорта", line_num + 1 );
                println!("Строка   : {:?}", line.get(1..) );
                writer.write_all(line.as_bytes())?;
                writer.write_all(b"\n")?;
                continue;
            };
            
            let func_code = library::get_math_func(func_name)
                .ok_or_else(|| format!("Строка {}: функция '{}' не найдена", line_num + 1, func_name))?;
            
            writer.write_all(func_code.as_bytes())?;
            export_count += 1;
            
            writer.write_all(b"\n")?;
        } else {
            writer.write_all(line.as_bytes())?;
            writer.write_all(b"\n")?;
        }
    }
    
    writer.flush()?;
    Ok(export_count)
}
