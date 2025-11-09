use std::marker::PhantomData;

use bumpalo::Bump;
use flywheel_sources::Span;

pub struct FileAST {
    _arena: Bump,
    top_levels: &'static [TopLevel<'static>],
}

impl FileAST {
    pub fn new<E, F>(constructor: F) -> Result<Self, E>
    where
        F: for<'ast> FnOnce(&'ast Bump) -> Result<&'ast [TopLevel<'ast>], E>,
    {
        let arena = Bump::new();
        let top_levels = {
            let arena_ref = &arena;
            let temp_top_levels = constructor(arena_ref)?;
            unsafe { std::mem::transmute::<&'_ [TopLevel<'_>], &'static [TopLevel<'static>]>(temp_top_levels) }
        };
        Ok(FileAST { _arena: arena, top_levels })
    }

    pub fn top_levels(&self) -> &[TopLevel] {
        self.top_levels
    }
}

pub enum TopLevel<'ast> {
    Function(&'ast Function<'ast>),
    Struct(&'ast Struct<'ast>),
    Import(&'ast Import<'ast>),
}

pub struct Function<'ast> {
    pub name: Span,
    pub return_type: Type<'ast>,
    pub body: Block<'ast>,
    pub span: Span,
}

pub struct Struct<'ast> {
    pub name: Span,
    pub fields: &'ast [StructField<'ast>],
    pub span: Span,
}

pub struct Import<'ast> {
    pub relative_path: Span,
    pub span: Span,
}

pub struct StructField<'ast> {
    pub name: Span,
    pub ty: Type<'ast>,
    pub span: Span,
}

pub struct Block<'ast> {
    pub stmts: &'ast [Stmt<'ast>],
    pub trailing_expr: Option<Expr<'ast>>,
    pub span: Span,
}

pub enum Stmt<'ast> {
    Let(&'ast Let<'ast>),
    While(&'ast While<'ast>),
    Return(&'ast Return<'ast>),
    Expr(&'ast Expr<'ast>),
}

pub struct Let<'ast> {
    pub name: Span,
    pub ty: Option<Type<'ast>>,
    pub value: Expr<'ast>,
    pub span: Span,
}

pub struct While<'ast> {
    pub condition: Expr<'ast>,
    pub body: Block<'ast>,
    pub span: Span,
}

pub struct Return<'ast> {
    pub expr: Expr<'ast>,
    pub span: Span,
}

pub enum Expr<'ast> {
    Bool(Span),
    Integer(Span),
    Float(Span),
    String(Span),
    Name(Span),
    Block(&'ast Block<'ast>),
    Attr(&'ast Attr<'ast>),
    Index(&'ast Index<'ast>),
    Unary(&'ast Unary<'ast>),
    Binary(&'ast Binary<'ast>),
    IfElse(&'ast IfElse<'ast>),
    Call(&'ast Call<'ast>),
}

impl Expr<'_> {
    pub fn span(&self) -> Span {
        use Expr::*;
        match self {
            Bool(span) | Integer(span) | Float(span) | String(span) | Name(span) => *span,
            Block(inner) => inner.span,
            Attr(inner) => inner.span,
            Index(inner) => inner.span,
            Unary(inner) => inner.span,
            Binary(inner) => inner.span,
            IfElse(inner) => inner.span,
            Call(inner) => inner.span,
        }
    }
}

pub struct Attr<'ast> {
    pub object: Expr<'ast>,
    pub attr: Span,
    pub span: Span,
}

pub struct Index<'ast> {
    pub object: Expr<'ast>,
    pub index: Expr<'ast>,
    pub span: Span,
}

pub struct Unary<'ast> {
    pub op: UnaryOp,
    pub right: Expr<'ast>,
    pub span: Span,
}

pub struct Binary<'ast> {
    pub op: BinaryOp,
    pub left: Expr<'ast>,
    pub right: Expr<'ast>,
    pub span: Span,
}

pub struct IfElse<'ast> {
    pub condition: Expr<'ast>,
    pub then_do: Expr<'ast>,
    pub else_do: Expr<'ast>,
    pub span: Span,
}

pub struct Call<'ast> {
    pub callee: Expr<'ast>,
    pub arguments: &'ast [Expr<'ast>],
    pub span: Span,
}

pub enum UnaryOp {
    Not,
    Neg,
}

pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

pub struct Type<'ast>(Span, PhantomData<&'ast ()>);
