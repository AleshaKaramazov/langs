const MATH_PAIRS: [(&str, &str); 2] = [("Степень;", POW), ("Модуль;", ABS)];

pub fn get_math_func(name: &str) -> Option<&'static str> {
    MATH_PAIRS
        .iter()
        .find(|(key, _)| *key == name)
        .map(|(_, value)| *value)
}

const ABS: &str = r"
Алгоритм Модуль -> Цел
аргументы: а: Цел
Начало 
    если а < 0 то 
        вернуть а * -1;
    иначе вернуть а;
Конец
";

const POW: &str = r"
Алгоритм Степень -> Нат
аргументы:
    а: Нат,
    б: Нат,
Начало
    вернуть ![std::i64::pow](а, б);
Конец
";
