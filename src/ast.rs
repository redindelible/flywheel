

pub enum Stmt {
    If(Box<Expr>, Vec<Stmt>, Vec<Stmt>),
    Return(Box<Expr>),
    Expr(Box<Expr>)
}


pub enum UnaryOp {
    Not,
    Neg
}

pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod
}

pub enum Expr {
    Bool(bool),
    Float(f64),
    Integer(i64),
    String(Box<str>),
    Name(Box<str>),
    Attr(Box<Expr>, Box<str>),
    Index(Box<Expr>, Box<Expr>),
    Unary(UnaryOp, Box<Expr>),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    Call(Box<Expr>, Box<[Expr]>),
}