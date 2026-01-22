#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    Bool,
    String,
    Array(Box<Type>),
    Void, 
    Unknown,
    Infer, 
}

#[derive(Debug, Clone)]
pub struct Algorithm {
    pub name: String,
    pub args: Vec<(String, Type)>,
    pub ret_type: Type,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub algorithms: Vec<Algorithm>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Return(Option<Expr>),
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
    ForEach {
        var: String,
        collection: Expr,
        body: Vec<Stmt>,
    },
    While {
        cond: Expr,
        body: Vec<Stmt>,
    },
    Expr(Expr),
}


#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOp {
    Not,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Var(String),
    NativeCall {
        path: String,
        args: Vec<Expr>,
    },
    MethodCall {
        target: Box<Expr>, 
        method: String,    
        args: Vec<Expr>,   
    },
    Array(Vec<Expr>), 
    Index {
        target: Box<Expr>,
        index: Box<Expr>,
    },
    Unary {
        op: UnaryOp,
        right: Box<Expr>,
    },
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
    NotEqual,
    Plus,
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,
    Mod,
    Mult,
    Or,
    And,
    Div,
    Sub,
}
