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
            panic!("ожидался {:?}, получен {:?}", token, self.current);
        }
    }

    pub fn parse_algorithm(&mut self) -> Algorithm {
        self.expect(Token::Algorithm);

        let name = match &self.current {
            Token::Ident(s) => {
                let n = s.clone();
                self.advance();
                n
            }
            _ => panic!("ожидалось имя алгоритма"),
        };

        let mut args = Vec::new();

        if self.current == Token::Arguments {
            self.advance();
            self.expect(Token::Colon);

            while let Token::Ident(name) = &self.current {
                let arg_name = name.clone();
                self.advance();
                self.expect(Token::Colon);
                let ty = self.parse_type();
                args.push((arg_name, ty));
                if self.current == Token::Comma {
                    self.advance();
                }
            }
        }

        self.expect(Token::BeginFunc);
        let body = self.parse_block(Token::EndFunc);

        Algorithm { name, args, body }
    }

    fn parse_block(&mut self, end_tok: Token) -> Vec<Stmt> {
        let mut stmts = Vec::new();
        while self.current != end_tok {
            stmts.push(self.parse_stmt());
        }
        self.expect(end_tok);
        stmts
    }

    fn parse_stmt(&mut self) -> Stmt {
        match &self.current {
            Token::Let => self.parse_let(),
            Token::If => self.parse_if(),
            Token::While => self.parse_while(),
            Token::Ident(_) => self.parse_assignment_or_expr(),
            _ => panic!("неожиданный statement: {:?}", self.current),
        }
    }

    fn parse_let(&mut self) -> Stmt {
        self.advance(); 

        let name = match &self.current {
            Token::Ident(s) => {
                let n = s.clone();
                self.advance();
                n
            }
            _ => panic!("ожидалось имя переменной"),
        };

        let mut ty = Type::Infer;

        if self.current == Token::Colon {
            self.advance();
            ty = self.parse_type();
        }

        if self.current == Token::Define || self.current == Token::Assign {
            self.advance();
        } else {
            panic!("ожидалось := или =");
        }

        let expr = self.parse_expr();
        self.expect(Token::Semicolon);

        Stmt::Let { name, ty, expr }
    }



    fn parse_if(&mut self) -> Stmt {
        self.advance(); // если

        let cond = self.parse_expr();

        let then_body = if self.current == Token::Begin {
            self.advance(); // начало
            self.parse_block(Token::End)
        } else {
            self.expect(Token::Then);
            vec![self.parse_stmt()]
        };

        let mut else_body = None;

    
        if self.current == Token::Else {
            self.advance(); 

            if self.current == Token::If {
                let else_if = self.parse_if();
                else_body = Some(vec![else_if]);
            } else if self.current == Token::Begin {
                self.advance();
                else_body = Some(self.parse_block(Token::End));
            } else {
                self.expect(Token::Then);
                else_body = Some(vec![self.parse_stmt()]);
            }
        }

        Stmt::If {
            cond,
            then_body,
            else_if: vec![],
            else_body,
        }
    }

    fn parse_while(&mut self) -> Stmt {
        self.advance();
        let cond = self.parse_expr();
        self.expect(Token::Do);

        let body = if self.current == Token::Begin {
            self.advance();
            self.parse_block(Token::End)
        } else {
            vec![self.parse_stmt()]
        };

        Stmt::While { cond, body }
    }


    fn parse_assignment_or_expr(&mut self) -> Stmt {
        let name = match &self.current {
            Token::Ident(s) => s.clone(),
            _ => unreachable!(),
        };

        self.advance();

        match self.current {
            Token::Assign => {
                self.advance();
                let expr = self.parse_expr();
                self.expect(Token::Semicolon);
                Stmt::Assign { name, expr }
            }

            Token::PlusAssign => {
                self.advance();
                let expr = self.parse_expr();
                self.expect(Token::Semicolon);
                Stmt::AssignAdd { name, expr }
            }
            Token::MinusAssign => {
                self.advance();
                let expr = self.parse_expr();
                self.expect(Token::Semicolon);
                Stmt::AssignSub { name, expr }
            }
            Token::Bang | Token::LParen => {
                let call = self.finish_call(name);
                self.expect(Token::Semicolon);
                Stmt::Expr(call)
            }
            _ => {
                let expr = self.parse_expr_tail(Expr::Var(name));
                self.expect(Token::Semicolon);
                Stmt::Expr(expr)
            }
        }
    }
 

    fn parse_expr_tail(&mut self, left: Expr) -> Expr {
    let mut expr = left;

    while matches!(
        self.current,
        Token::Equal | Token::Greater | Token::Mod | Token::Or | Token::Minus
    ) {
        let op = match self.current {
            Token::Equal => BinOp::Equal,
            Token::Greater => BinOp::Greater,
            Token::Mod => BinOp::Mod,
            Token::Or => BinOp::Or,
            Token::Minus => BinOp::Sub,
            _ => unreachable!(),
        };

        self.advance();
        let right = self.parse_primary();

        expr = Expr::Binary {
            left: Box::new(expr),
            op,
            right: Box::new(right),
        };
    }

    expr
    }

    fn parse_expr(&mut self) -> Expr {
        self.parse_or()
    }

    fn parse_or(&mut self) -> Expr {
        let mut expr = self.parse_cmp();
        while self.current == Token::Or {
            self.advance();
            let right = self.parse_cmp();
            expr = Expr::Binary {
                left: Box::new(expr),
                op: BinOp::Or,
                right: Box::new(right),
            };
        }
        expr
    }

    fn parse_cmp(&mut self) -> Expr {
        let mut expr = self.parse_primary();
        while matches!(self.current, 
            Token::Equal | Token::Greater | Token::Less 
            | Token::Or | Token::Mod | Token::Minus) {
            let op = match self.current {
                Token::Equal => BinOp::Equal,
                Token::Greater => BinOp::Greater,
                Token::Mod => BinOp::Mod,
                Token::Minus => BinOp::Sub,
                Token::Less => BinOp::Less,
                _ => unreachable!(),
            };
            self.advance();
            let right = self.parse_primary();
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            };
        }
        expr
    }

    fn parse_primary(&mut self) -> Expr {
        if self.current == Token::Minus {
            self.advance();
            let expr = self.parse_primary();
            return Expr::Binary {
                left: Box::new(Expr::Int(0)),
                op: BinOp::Sub,
                right: Box::new(expr),
            };
        }
        match &self.current {

            Token::Int(v) => {
                let e = Expr::Int(*v);
                self.advance();
                e
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
                if self.current == Token::Bang || self.current == Token::LParen {
                    self.finish_call(n)
                } else {
                    self.parse_expr_tail(Expr::Var(n))
                }
            }
            Token::LParen => {
                self.advance();
                let expr = self.parse_expr();
                self.expect(Token::RParen);
                expr
            }
            Token::Assign | Token::PlusAssign => {
                panic!("оператор присваивания не является выражением");
            }
            _ => panic!("неожиданное выражение: {:?}", self.current),
        }
    }

    fn finish_call(&mut self, name: String) -> Expr {
        let intrinsic = self.current == Token::Bang;
        if intrinsic {
            self.advance();

        }

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

    fn parse_type(&mut self) -> Type {
        let ty = match self.current {
            Token::TypeNat => Type::Nat,
            Token::TypeInt => Type::Int,
            Token::TypeBool => Type::Bool,
            Token::TypeString => Type::String,
            _ => panic!("ожидался тип"),
        };
        self.advance();
        ty
    }
}
