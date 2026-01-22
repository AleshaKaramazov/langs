use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Algorithm,
    Arguments,
    BeginFunc,   
    EndFunc,     
    Begin,       
    End,         
    From,
    To,
    For,
    Let,
    If,
    Then,
    Else,
    While,
    Do,
    Return,

    Array,
    LBracket,
    RBracket,
    Pipe,

    TypeNat,
    TypeInt,
    TypeFloat,
    TypeBool,
    TypeString,

    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Ident(String),

    Assign,        
    Define,        
    MinusAssign,   
    Plus,
    Minus,         
    Div,
    Colon,         
    PlusAssign,    
    DivAssign,
    Equal,         
    Less,
    Greater,       
    Mult,
    LessOrEqual,
    GreaterOrEqual,
    MultAssigment,
    Mod,     
    And,
    Or,  
    Not,
    NotEqual,

    Arrow, // -> 

    LParen,
    RParen,
    Comma,
    Semicolon,

    Bang,       
    Eof,
    
    //syntax sugar
    ForAll,
    Всех,
    В,
    Массиве,
    Диапазоне,
    DotDot,  
    Dot,
}

pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(text: &'a str) -> Self {
        Self {
            input: text.chars().peekable(),
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let ch = match self.input.peek() {
            Some(c) => *c,
            None => return Token::Eof,
        };

        if ch == '"' {
            return self.read_string();
        }

        if ch.is_ascii_digit() {
            return self.read_number();
        }

        if is_ident_start(ch) {
            return self.read_ident_or_keyword();
        }

        match ch {
            '/' => {
                self.input.next(); 

                if self.match_next('/') {
                    while let Some(c) = self.input.peek() {
                        if *c == '\n' {
                            break;
                        }
                        self.input.next();
                    }
                    self.next_token()
                } else if self.match_next('*') {
                    while let Some(c) = self.input.next() {
                        if c == '*' {
                            if self.match_next('/') {
                                break;
                            }
                        }
                    }
                    self.next_token()
                } else if self.match_next('=') {
                    Token::DivAssign
                } else {
                    Token::Div
                }
            }
            ':' => {
                self.input.next();
                if self.match_next('=') {
                    Token::Define
                } else {
                    Token::Colon
                }
            }
            '=' => {
                self.input.next();
                if self.match_next('=') {
                    Token::Equal
                } else {
                    Token::Assign
                }
            }
            '+' => {
                self.input.next();
                if self.match_next('=') {
                    Token::PlusAssign
                } else {
                    Token::Plus
                }
            }
            '|' => {
                self.input.next(); 
                if self.match_next('|') {
                    Token::Or
                } else {
                    Token::Pipe
                }
            }
            '[' => { self.input.next(); Token::LBracket }
            ']' => { self.input.next(); Token::RBracket }
            '&' => {
                self.input.next(); 
                if self.match_next('&') {
                    Token::And
                } else {
                    panic!("Ожидался оператор &&");
                }
            }
            '<' => {
                self.input.next();
                if self.match_next('=') {
                    Token::LessOrEqual
                } else {
                    Token::Less
                }
            }
            '>' => {
                self.input.next();
                if self.match_next('=') {
                    Token::GreaterOrEqual
                } else {
                    Token::Greater
                }
            }
            '%' => {
                self.input.next();
                Token::Mod
            }
            '(' => {
                self.input.next();
                Token::LParen
            }
            ')' => {
                self.input.next();
                Token::RParen
            }
            ',' => {
                self.input.next();
                Token::Comma
            }
            ';' => {
                self.input.next();
                Token::Semicolon
            }
            '*' => {
                self.input.next();
                if self.input.peek() == Some(&'=') {
                    self.input.next();
                    Token::MultAssigment
                } else {
                    Token::Mult
                }
            }
            '-' => {
                self.input.next(); 
                if let Some(&'>') = self.input.peek() {
                    self.input.next();
                    Token::Arrow
                } else if let Some(&'=') = self.input.peek() {
                    self.input.next(); 
                    Token::MinusAssign
                } else {
                    Token::Minus
                }
            }
            '!' => {
                self.input.next();
                Token::Bang
            }
            '.' => {
                self.input.next();
                if self.input.peek() == Some(&'.') {
                    self.input.next();
                    Token::DotDot
                } else {
                    Token::Dot
                }
            }
            _ => panic!("неизвестный символ: {}", ch),
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.input.peek() {
            if c.is_whitespace() {
                self.input.next();
            } else {
                break;
            }
        }
    }

    fn match_next(&mut self, expected: char) -> bool {
        match self.input.peek() {
            Some(c) if *c == expected => {
                self.input.next();
                true
            }
            _ => false,
        }
    }

    fn read_number(&mut self) -> Token {
        let mut s = String::new();
        let mut is_float = false;
        while let Some(&ch) = self.input.peek() {
            if ch.is_ascii_digit() {
                s.push(self.input.next().unwrap());
            } else if (ch == '.' && self.input.peek() != Some(&'.')) && !is_float {
                is_float = true;
                s.push(self.input.next().unwrap());
            } else {
                break;
            }
        }
        if is_float {
            Token::Float(s.parse().unwrap())
        } else {
            Token::Int(s.parse().unwrap())
        }
    }

    fn read_string(&mut self) -> Token {
        self.input.next(); 
        let mut s = String::new();
        while let Some(c) = self.input.next() {
            if c == '"' {
                break;
            }
            s.push(c);
        }
        Token::String(s)
    }

    fn read_ident_or_keyword(&mut self) -> Token {
        let mut ident = String::new();
        while let Some(c) = self.input.peek() {
            if is_ident_part(*c) {
                ident.push(*c);
                self.input.next();
            } else {
                break;
            }
        }

        match ident.as_str() {
            "Алгоритм" => Token::Algorithm,
            "аргументы" => Token::Arguments,
            "Начало" => Token::BeginFunc,
            "Конец" => Token::EndFunc,
            "нч" => Token::Begin,
            "кц" => Token::End,
            "пусть" => Token::Let,
            "если" | "Если" => Token::If,
            "то" => Token::Then,
            "иначе"| "Иначе" => Token::Else,

            "пока" => Token::While,
            "для_" => Token::For,
            "от" => Token::From,
            "до" => Token::To,

            "выполнить" => Token::Do,

            "вернуть" | "конец" => Token::Return,

            "Нат" => Token::TypeNat,
            "Цел" => Token::TypeInt,
            "Десятич" => Token::TypeFloat,
            "Лог" => Token::TypeBool,
            "Строка" => Token::TypeString,

            "не" | "Не" => {
                if let Some(&'=') = self.input.peek() {
                    self.input.next(); 
                    Token::NotEqual
                } else {
                    Token::Not 
                }
            }
            "Истина" => Token::Bool(true),
            "Ложь" => Token::Bool(false),

            "массив" | "Массив" => Token::Array,
                
            //syntax sugar
            "и" | "И" => Token::And,
            "или" | "ИЛИ" => Token::Or,
            
            "для" | "Для" => Token::ForAll,
            "всех" => Token::Всех,
            "в" => Token::В,
            "массиве" => Token::Массиве,
            "диапазоне" => Token::Диапазоне,
            ".." => Token::DotDot,

            _ => Token::Ident(ident),
        }
    }
}

fn is_ident_start(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}

fn is_ident_part(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}
