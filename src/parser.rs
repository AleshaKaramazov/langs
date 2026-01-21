use crate::lexer::{Lexer, Token};
use crate::ast::*;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current: Token,
}

impl<'a> Parser<'a> {
    pub fn new(mut lexer: Lexer<'a>) -> Self {
        let current = lexer.next_token();
        Self { lexer, current }
    }

    fn advance(&mut self) {
        self.current = self.lexer.next_token();
    }

    fn expect(&mut self, token: Token) {
        if self.current == token {
            self.advance();
        } else {
            panic!("Ожидался {:?}, но получен {:?}", token, self.current);
        }
    }

    pub fn parse_algorithm(&mut self) -> Algorithm {
        self.expect(Token::Algorithm);
        
        let name = match &self.current {
            Token::Ident(s) => s.clone(),
            _ => panic!("Ожидалось имя алгоритма"),
        };
        self.advance();

        let mut ret_type = Type::Void;
        if self.current == Token::Arrow {
            self.advance();
            ret_type = self.parse_type();
        }

        let mut args = Vec::new();
        if self.current == Token::Arguments {
            self.advance();
            self.expect(Token::Colon);
            
            loop {
                if let Token::Ident(arg_name) = &self.current {
                    let name = arg_name.clone();
                    self.advance();
                    self.expect(Token::Colon);
                    let ty = self.parse_type();
                    args.push((name, ty));
                    
                    if self.current == Token::Comma {
                        self.advance();
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            }
        }

        self.expect(Token::BeginFunc);

        let mut body = Vec::new();
        while self.current != Token::EndFunc && self.current != Token::Eof {
            body.push(self.parse_stmt());
        }
        self.expect(Token::EndFunc);
        Algorithm { name, args, ret_type, body }
    }

    fn parse_block(&mut self, end_tok: Token) -> Vec<Stmt> {
        let mut stmts = Vec::new();
        while self.current != end_tok && self.current != Token::Eof {
            stmts.push(self.parse_stmt());
        }
        if self.current == end_tok {
            self.advance();
        } else {
            panic!("Ожидался конец блока {:?}", end_tok);
        }
        stmts
    }

    fn parse_stmt(&mut self) -> Stmt {
        match self.current {
            Token::Return => {
                self.advance();
                if self.current == Token::Semicolon {
                    self.advance();
                    Stmt::Return(None)
                } else {
                    let expr = self.parse_expr();
                    self.expect(Token::Semicolon);
                    Stmt::Return(Some(expr))
                }
            }
            Token::Let => self.parse_let(),
            Token::If => self.parse_if(),
            Token::While => self.parse_while(),
            Token::For => self.parse_for(),
            Token::ForAll => self.parse_human_for(),
            Token::Ident(_) => self.parse_assignment_or_expr(),
            _ => panic!("Неожиданный оператор: {:?}", self.current),
        }
    }

    
    fn parse_assignment_or_expr(&mut self) -> Stmt {
        let expr = self.parse_expr();

        match self.current {
            Token::Assign => {
                let name = self.extract_var_name(expr);
                self.advance();
                let val = self.parse_expr();
                self.expect(Token::Semicolon);
                Stmt::Assign { name, expr: val }
            }
            Token::PlusAssign => {
                let name = self.extract_var_name(expr);
                self.advance();
                let val = self.parse_expr();
                self.expect(Token::Semicolon);
                Stmt::AssignAdd { name, expr: val }
            }
            Token::MinusAssign => {
                let name = self.extract_var_name(expr);
                self.advance();
                let val = self.parse_expr();
                self.expect(Token::Semicolon);
                Stmt::AssignSub { name, expr: val }
            }
            Token::MultAssigment => {
                let name = self.extract_var_name(expr);
                self.advance();
                let val = self.parse_expr();
                self.expect(Token::Semicolon);
                Stmt::AssignMult { name, expr: val }
            }
            Token::DivAssign => {
                let name = self.extract_var_name(expr);
                self.advance();
                let val = self.parse_expr();
                self.expect(Token::Semicolon);
                Stmt::AssignDiv { name, expr: val }
            }
            _ => {
                self.expect(Token::Semicolon);
                Stmt::Expr(expr)
            }
        }
    }

    fn extract_var_name(&self, expr: Expr) -> String {
        match expr {
            Expr::Var(n) => n,
            _ => panic!("Слева от присваивания должна быть переменная"),
        }
    }

    fn parse_let(&mut self) -> Stmt {
        self.advance(); 
        let name = match &self.current {
            Token::Ident(s) => s.clone(),
            _ => panic!("Ожидалось имя переменной"),
        };
        self.advance();

        let mut ty = Type::Infer;
        if self.current == Token::Colon {
            self.advance();
            ty = self.parse_type();
        }

        if self.current == Token::Define || self.current == Token::Assign {
            self.advance();
        } else {
            panic!("Ожидалось := или =");
        }

        let expr = self.parse_expr();
        self.expect(Token::Semicolon);

        Stmt::Let { name, ty, expr }
    }

    fn parse_if(&mut self) -> Stmt {
        self.advance();
        let cond = self.parse_expr();
        
        let then_body = if self.current == Token::Begin {
            self.advance();
            self.parse_block(Token::End)
        } else {
            self.expect(Token::Then);
            vec![self.parse_stmt()]
        };

        let mut else_if = Vec::new();
        let mut else_body = None;

        while self.current == Token::Else {
            self.advance();
            if self.current == Token::If {
                self.advance();
                let cond = self.parse_expr();
                let body = if self.current == Token::Begin {
                    self.advance();
                    self.parse_block(Token::End)
                } else {
                    self.expect(Token::Then);
                    vec![self.parse_stmt()]
                };
                else_if.push((cond, body));
            } else {
                else_body = Some(if self.current == Token::Begin {
                    self.advance();
                    self.parse_block(Token::End)
                } else {
                    vec![self.parse_stmt()]
                });
                break;
            }
        }
        Stmt::If { cond, then_body, else_if, else_body }
    }

    fn parse_while(&mut self) -> Stmt {
        self.advance();
        let cond = self.parse_expr();
        if self.current == Token::Do { self.advance(); }
        
        let body = if self.current == Token::Begin {
            self.advance();
            self.parse_block(Token::End)
        } else {
            vec![self.parse_stmt()]
        };
        Stmt::While { cond, body }
    }

    fn parse_for(&mut self) -> Stmt {
        self.advance();
        let var = match &self.current {
            Token::Ident(s) => s.clone(),
            _ => panic!("Ожидалось имя переменной цикла"),
        };
        self.advance();
        self.expect(Token::From);
        let start = self.parse_expr();
        self.expect(Token::To);
        let end = self.parse_expr();
        if self.current == Token::Do { self.advance(); }

        let body = if self.current == Token::Begin {
            self.advance();
            self.parse_block(Token::End)
        } else {
            vec![self.parse_stmt()]
        };
        Stmt::For { var, start, end, body }
    }

    fn parse_human_for(&mut self) -> Stmt {
        self.expect(Token::ForAll);
        self.expect(Token::Всех);
        self.expect(Token::LParen);
        let var = match &self.current {
            Token::Ident(s) => s.clone(),
            _ => panic!("Ожидалось имя переменной"),
        };
        self.advance();
        self.expect(Token::RParen);
        self.expect(Token::В);

        if self.current == Token::Диапазоне {
            self.advance();
            self.expect(Token::LParen);
            let start = self.parse_expr();
            self.expect(Token::DotDot);
            let end = self.parse_expr();
            self.expect(Token::RParen);
            if self.current == Token::Do { self.advance(); }
            let body = self.parse_block_helper();
            Stmt::For { var, start, end, body }
        } else if self.current == Token::Массиве {
            self.advance();
            self.expect(Token::LParen);
            let collection = self.parse_expr();
            self.expect(Token::RParen);
            if self.current == Token::Do { self.advance(); }
            let body = self.parse_block_helper();
            Stmt::ForEach { var, collection, body }
        } else {
            panic!("Ожидалось 'диапазоне' или 'массиве'");
        }
    }

    fn parse_block_helper(&mut self) -> Vec<Stmt> {
        if self.current == Token::Begin {
            self.advance();
            self.parse_block(Token::End)
        } else {
            vec![self.parse_stmt()]
        }
    }

    fn parse_type(&mut self) -> Type {
        let t = match self.current {
            Token::TypeNat | Token::TypeInt => Type::Int,
            Token::TypeBool => Type::Bool,
            Token::TypeString => Type::String,
            _ => panic!("Неизвестный тип"),
        };
        self.advance();
        t
    }

    pub fn parse_program(&mut self) -> Program {
        let mut algorithms = Vec::new();
        while self.current != Token::Eof {
            algorithms.push(self.parse_algorithm());
        }
        Program { algorithms }
    }

    fn parse_expr(&mut self) -> Expr {
        let mut left = self.parse_and(); 
        while self.current == Token::Or {
            self.advance();
            let right = self.parse_and();
            left = Expr::Binary {
                left: Box::new(left),
                op: BinOp::Or,
                right: Box::new(right),
            };
        }
        left
    }

    fn parse_and(&mut self) -> Expr {
        let mut left = self.parse_comparison();
        while self.current == Token::And {
            self.advance();
            let right = self.parse_comparison();
            left = Expr::Binary {
                left: Box::new(left),
                op: BinOp::And,
                right: Box::new(right),
            };
        }
        left
    }

    fn parse_comparison(&mut self) -> Expr {
        let mut left = self.parse_arithmetic();
        while matches!(self.current, Token::Equal | Token::NotEqual | Token::Less | Token::Greater) {
            let op = match self.current {
                Token::Equal => BinOp::Equal,
                Token::NotEqual => BinOp::NotEqual,
                Token::Less => BinOp::Less,
                Token::Greater => BinOp::Greater,
                _ => unreachable!(),
            };
            self.advance();
            let right = self.parse_arithmetic();
            left = Expr::Binary {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }
        left
    }

    fn parse_arithmetic(&mut self) -> Expr {
        let mut left = self.parse_term();
        while matches!(self.current, Token::Plus | Token::Minus) {
            let op = if self.current == Token::Plus { BinOp::Plus } else { BinOp::Sub };
            self.advance();
            let right = self.parse_term();
            left = Expr::Binary {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }
        left
    }

    fn parse_term(&mut self) -> Expr {
        let mut left = self.parse_primary();
        while matches!(self.current, Token::Mult | Token::Div | Token::Mod) {
            let op = match self.current {
                Token::Mult => BinOp::Mult,
                Token::Div => BinOp::Div,
                Token::Mod => BinOp::Mod,
                _ => unreachable!(),
            };
            self.advance();
            let right = self.parse_primary();
            left = Expr::Binary {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }
        left
    }

    fn parse_primary(&mut self) -> Expr {
        match &self.current {
            Token::Not => {
                self.advance();
                let expr = self.parse_primary();
                Expr::Unary {
                    op: UnaryOp::Not,
                    right: Box::new(expr),
                }
            }
            Token::Minus => {
                self.advance();
                let expr = self.parse_primary();
                Expr::Binary {
                    left: Box::new(Expr::Int(0)),
                    op: BinOp::Sub,
                    right: Box::new(expr),
                }
            }
            Token::Int(v) => {
                let e = Expr::Int(*v);
                self.advance();
                e
            }
            Token::Array => {
                self.advance();
                self.expect(Token::Bang); 
                self.expect(Token::LBracket);
                let mut elements = Vec::new();
                if self.current != Token::RBracket {
                    loop {
                        elements.push(self.parse_expr());
                        if self.current == Token::Comma { 
                            self.advance(); 
                        } else { 
                            break; 
                        }
                    }
                }
                self.expect(Token::RBracket);
                Expr::Array(elements)
            }
            Token::Bool(b) => {
                let e = Expr::Bool(*b);
                self.advance();
                e
            }
            Token::String(s) => {
                let e = Expr::String(s.clone());
                self.advance();
                e
            }
            Token::Ident(name) => {
                let n = name.clone();
                self.advance();
                
                let expr = if self.current == Token::LParen {
                    self.finish_call(n, false)
                } else if self.current == Token::Bang {
                    self.advance();
                    
                    if self.current == Token::LParen {
                        self.finish_call(n, true)
                    } else if self.current == Token::LBracket {
                        self.advance(); 
                        let index = self.parse_expr();
                        self.expect(Token::RBracket);
                        Expr::Index {
                            target: Box::new(Expr::Var(n)),
                            index: Box::new(index),
                        }
                    } else {
                        panic!("Ожидалось ( или [ после '!'");
                    }
                } else {
                    Expr::Var(n)
                };

                expr
            }
            Token::LParen => {
                self.advance();
                let expr = self.parse_expr();
                self.expect(Token::RParen);
                expr
            }
            _ => panic!("Неожиданный токен в выражении: {:?}", self.current),
        }
    }

    fn finish_call(&mut self, name: String, intrinsic: bool) -> Expr {
        self.expect(Token::LParen);
        let mut args = Vec::new();
        if self.current != Token::RParen {
            loop {
                args.push(self.parse_expr());
                if self.current == Token::Comma {
                    self.advance();
                } else {
                    break;
                }
            }
        }
        self.expect(Token::RParen);
        Expr::Call { name, args, intrinsic }
    }
}
