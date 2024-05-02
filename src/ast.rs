

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
    Call(Box<str>, Box<[Expr]>),
}