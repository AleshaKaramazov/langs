#[macro_use]
mod native;
mod ast;
mod checker;
mod env;
mod interpreter;
mod lexer;
mod library;
mod parser;
mod value;
mod visualizer;

use std::fs;
use std::process;

use checker::TypeChecker;
use interpreter::Interpreter;
use lexer::Lexer;
use parser::Parser;
use visualizer::Visualizer;

struct Mode {
    check: bool,
    export: bool,
    draw: bool
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Использование: язык <файл.алг>");
        process::exit(1);
    }

    let mut filename = None;
    let mut mode: Mode = Mode {check: true, export: false, draw: false };
    for i in args.iter().skip(1) {
        if i.starts_with('-') {
            match i.as_str() {
                "--export" | "--экспорт" | "--Экспорт" | "--Экспортировать" => mode.export = true,
                "--draw" | "--нарисовать" | "--Нарисовать" => mode.draw = true,
                "--non-check" | "--без-проверки" | "--БезПроверки" => mode.check = false,
                _ => {
                    eprintln!("ОШИБКА: Неизвестный флаг: {}", i);
                    process::exit(1)
                }
            } 
        } else {
            filename = Some(i);
        }
    }
    let filename: &str = match filename {
        None => {
            eprintln!("ОШИБКА: не передано имя файла");
            process::exit(1);
        }
        Some(i) => i
    };

    let mut source = match fs::read_to_string(filename) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("ОШИБКА: Не удалось прочитать файл '{}': {}", filename, e);
            process::exit(1);
        }
    };

    if mode.export {
        match process_export(&source, filename) {
            Ok(count) => println!("ГОТОВО! Экспортировано функций: {}", count),
            Err(e) => eprintln!("Ошибка: {}", e),
        }
        source = match fs::read_to_string(filename) {
            Ok(s) => s,
            Err(_) => {
                eprintln!("ОШИБКА: Не удалось прочитать файл ПОСЛЕ экспорта функций");
                process::exit(1);
            }
        };
    }

    let lexer = Lexer::new(&source);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program();
    
    let main_algorithm = program
        .algorithms
        .iter()
        .find(|alg| alg.name == "Глав" || alg.name == "Главная")
        .cloned();

    if mode.draw {
        if let Some(alg) = &main_algorithm {
            let viz = Visualizer::new();
            let dot_output = viz.translate(alg);
            if let Err(e) = fs::write("graph.dot", dot_output) {
                eprintln!("ОШИБКА: Не удалось записать DOT файл: {}", e);
                process::exit(1);
            }
            use std::process::Command;
            let status = Command::new("dot")
                .arg("-Tpng")
                .arg("graph.dot")
                .arg("-o")
                .arg("алгоритм.png")
                .status() 
                .expect("Не удалось запустить dot. Проверьте, установлен ли Graphviz.");
            if status.success() {
                println!("Картинка успешно создана!");
                if let Err(e) = fs::remove_file("graph.dot") {
                    println!("не удалить убрать graph.dot: {}", e);
                }
            } else {
                eprintln!("Команда завершилась с ошибкой: {}", status);
            }            
        } else {
            eprintln!("ОШИБКА: Нечего рисовать, алгоритм 'Главная' не найден.");
        }
    }

    if mode.check {
        if main_algorithm.is_none() {
            eprintln!("ОШИБКА: В программе должен быть основной алгоритм с именем 'Главная'.");
            process::exit(1);
        }

        let mut checker = TypeChecker::new();
        if let Err(e) = checker.check_program(&program) {
            eprintln!("ОШИБКА ТИПИЗАЦИИ: {}", e);
            process::exit(1);
        }
    }

    let mut interpreter = Interpreter::new();
    match interpreter.run(program) {
        Ok(_) => {
            //println!("ПРОГРАММА ЗАВЕРШИЛАСЬ УСПЕШНО!");
        }
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
        if line.starts_with("экспорт") || line.starts_with("Экспорт") {
            let func_name = if let Some(name) = line.split_whitespace().nth(1) {
                name
            } else {
                println!("Строка {}: некорректный формат экспорта", line_num + 1);
                println!("Строка   : {:?}", line.get(1..));
                writer.write_all(line.as_bytes())?;
                writer.write_all(b"\n")?;
                continue;
            };

            let func_code = library::get_math_func(func_name).ok_or_else(|| {
                format!(
                    "Строка {}: функция '{}' не найдена",
                    line_num + 1,
                    func_name
                )
            })?;

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
