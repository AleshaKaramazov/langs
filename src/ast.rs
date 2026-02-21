use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    UInt,
    Float,
    Bool,
    String,
    Char,
    Function(Box<Type>, Box<Type>),
    Array(Box<Type>),
    Void,
    Unknown,
    Infer,
}

impl Type {
    pub fn is_numeric(&self) -> bool {
        matches!(self, Type::Int | Type::UInt | Type::Float | Type::Unknown)
    }
    pub fn is_compatible(&self, other: &Type) -> bool {
        if (self == &Type::Unknown || other == &Type::Unknown) 
            || (self.is_numeric() && other.is_numeric()){
            return true;
        } 
        
        match (self, other) {
            (Type::Function(a1, r1), Type::Function(a2, r2)) => {
                a1.is_compatible(a2) && r1.is_compatible(r2)
            }
            (Type::Array(t1), Type::Array(t2)) => t1.is_compatible(t2),
            _ => self == other,
        }
    }
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
    Break,
    Continue,
    Let {
        name: String,
        ty: Type,
        expr: Option<Expr>,
    },
    Assign {
        name: String,
        expr: Expr,
    },
    AssignAdd {
        name: String,
        expr: Expr,
    },
    AssignSub {
        name: String,
        expr: Expr,
    },
    AssignMult {
        name: String,
        expr: Expr,
    },
    AssignDiv {
        name: String,
        expr: Expr,
    },

    If {
        cond: Expr,
        then_body: Vec<Stmt>,
        else_if: Vec<(Expr, Vec<Stmt>)>,
        else_body: Option<Vec<Stmt>>,
    },
    For {
        var: String,
        start: Expr,
        cont: bool,
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

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    UInt(u64),
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Var(String),
    Char(char),
    // ++x
    PreInc(Box<Expr>),
    // --x
    PreDec(Box<Expr>),
    // x++
    PostInc(Box<Expr>),
    // x--
    PostDec(Box<Expr>),
    Lambda {
        param: String,
        param_ty: Type,
        body: Rc<Expr>,
    },
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
    Cast {
        target_type: Type,
        expr: Box<Expr>,
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
