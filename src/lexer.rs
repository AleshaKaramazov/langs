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
    Let,
    If,
    Then,
    Else,
    While,
    Do,
    Return,

    TypeNat,
    TypeInt,
    TypeBool,
    TypeString,

    Int(i64),
    Bool(bool),
    String(String),
    Ident(String),

    Assign,        
    Define,        
    MinusAssign,   
    Minus,         
    Colon,         
    PlusAssign,    
    Equal,         
    Less,
    Greater,       
    Mod,           
    Or,            

    LParen,
    RParen,
    Comma,
    Semicolon,

    Bang,       
    Eof,
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
                    panic!("неожиданный +");
                }
            }
            '|' => {
                self.input.next();
                if self.match_next('|') {
                    Token::Or
                } else {
                    panic!("ожидался ||");
                }
            }
            '<' => {
                self.input.next();
                Token::Less
            }
            '>' => {
                self.input.next();
                Token::Greater
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
            '-' => {
                self.input.next();
                if self.input.peek() == Some(&'=') {
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
        let mut num = String::new();
        while let Some(c) = self.input.peek() {
            if c.is_ascii_digit() {
                num.push(*c);
                self.input.next();
            } else {
                break;
            }
        }
        Token::Int(num.parse().unwrap())
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
            "начало" => Token::Begin,
            "конец" => Token::End,
            "пусть" => Token::Let,
            "если" => Token::If,
            "то" => Token::Then,
            "иначе" => Token::Else,
            "пока" => Token::While,
            "выполнить" => Token::Do,
            "вернуть" => Token::Return,

            "Нат" => Token::TypeNat,
            "Цел" => Token::TypeInt,
            "Лог" => Token::TypeBool,
            "Строка" => Token::TypeString,

            "Истина" => Token::Bool(true),
            "Ложь" => Token::Bool(false),

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
