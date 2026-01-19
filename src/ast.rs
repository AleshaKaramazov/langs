#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Nat,
    Int,
    Bool,
    String,
    Infer,
}

#[derive(Debug, Clone)]
pub struct Algorithm {
    pub name: String,
    pub args: Vec<(String, Type)>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Let {
        name: String,
        ty: Type,
        expr: Expr,
    },
    Assign {
        name: String,
        expr: Expr,
    },
    AssignAdd { name: String, expr: Expr },
    AssignSub { name: String, expr: Expr },
    AssignMult { name: String, expr: Expr },
    AssignDiv { name: String, expr: Expr },
    
    If {
        cond: Expr,
        then_body: Vec<Stmt>,
        else_if: Vec<(Expr, Vec<Stmt>)>,
        else_body: Option<Vec<Stmt>>,
    },
    For {
        var: String,
        start: Expr,
        end: Expr,
        body: Vec<Stmt>,
    },
    While {
        cond: Expr,
        body: Vec<Stmt>,
    },
    Expr(Expr),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Int(i64),
    Bool(bool),
    String(String),
    Var(String),
    Call {
        name: String,
        args: Vec<Expr>,
        intrinsic: bool,
    },
    Binary {
        left: Box<Expr>,
        op: BinOp,
        right: Box<Expr>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinOp {
    Equal,
    Plus,
    Less,
    Greater,
    Mod,
    Mult,
    Or,
    And,
    Div,
    Sub,
}
