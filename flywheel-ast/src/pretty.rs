use std::fmt::{Debug, Formatter};
use flywheel_sources::SourceMap;
use crate::*;

impl FileAST {
    pub fn pretty<'a>(&'a self, sources: &'a SourceMap) -> Pretty<'a, &'a FileAST> {
        Pretty { inner: self, sources }
    }
}

pub struct Pretty<'a, T> {
    inner: T,
    sources: &'a SourceMap,
}

impl<'a, T> Pretty<'a, T> {
    fn child<C>(&self, inner: C) -> Pretty<'a, C> {
        Pretty { inner, sources: self.sources }
    }
}

impl<'a> Debug for Pretty<'a, &'a FileAST> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FileAST")
            .field("top_levels", &self.child(&self.inner.top_levels))
            .finish()
    }
}

impl<'a, T> Debug for Pretty<'a, &'a &'a [T]> where Pretty<'a, &'a T>: Debug {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut debug = f.debug_list();
        for item in *self.inner {
            debug.entry(&self.child(item));
        }
        debug.finish()
    }
}

impl<'a, T> Debug for Pretty<'a, &'a Option<T>> where Pretty<'a, &'a T>: Debug {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.inner {
            Some(item) => f.debug_tuple("Some")
                .field(&self.child(item))
                .finish(),
            None => f.write_str("None"),
        }
    }
}

impl<'a> Debug for Pretty<'a, &'a Symbol> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.sources.get_span(self.inner.span()))
    }
}

impl<'a> Debug for Pretty<'a, &'a Span> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let span_info = self.sources.get_span_info(*self.inner);
        f.debug_struct("Span")
            .field("source", &span_info.source.num())
            .field("range", &(span_info.start..span_info.end))
            .finish()
    }
}


impl<'a> Debug for Pretty<'a, &'a UnaryOp> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.inner {
            UnaryOp::Neg => f.write_str("Neg"),
            UnaryOp::Not => f.write_str("Not"),
        }
    }
}


impl<'a> Debug for Pretty<'a, &'a BinaryOp> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.inner {
            BinaryOp::Add => f.write_str("Add"),
            BinaryOp::Sub => f.write_str("Sub"),
            BinaryOp::Mul => f.write_str("Mul"),
            BinaryOp::Div => f.write_str("Div"),
            BinaryOp::Mod => f.write_str("Mod"),
        }
    }
}


macro_rules! ast_pretty {
    (enum $name:ident { $($variant:ident $(=> $variant_name:ident)?),* $(,)? } ) => {
        impl<'a> Debug for Pretty<'a, &'a $name<'a>> {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                match self.inner {
                    $(
                    $name::$variant(item) => ast_enum_pretty_impl!(self, f, item, $($variant_name)?)
                    ),*
                }
            }
        }
    };
    (struct $name:ident { $($field:ident),* $(,)? } ) => {
        impl<'a> Debug for Pretty<'a, &'a $name<'a>> {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(stringify!($name))
                    $(.field(stringify!($field), &self.child(&self.inner.$field)))*
                    .finish()
            }
        }
    }
}


macro_rules! ast_enum_pretty_impl {
    ($self:expr, $fmt:expr, $item:expr,) => {
        Debug::fmt(&$self.child(*$item), $fmt)
    };
    ($self:expr, $fmt:expr, $item:expr, $variant_name:ident) => {
        $fmt.debug_tuple(stringify!($variant_name))
            .field(&$self.child($item))
            .finish()
    };
}

ast_pretty!(enum TopLevel {
    Function,
    Struct,
});

ast_pretty!(struct Function {
    name,
    return_type,
    body,
    span,
});

ast_pretty!(struct Struct {
    name,
    fields,
    span,
});

ast_pretty!(struct StructField {
    name,
    ty,
    span,
});

ast_pretty!(enum Expr {
    Bool => Bool,
    Integer => Integer,
    Float => Float,
    String => String,
    Name => Name,
    Block,
    Attr,
    Index,
    Unary,
    Binary,
    IfElse,
    Call,
});

ast_pretty!(struct Unary {
    op,
    right,
    span,
});

ast_pretty!(struct Binary {
    op,
    left,
    right,
    span,
});

ast_pretty!(struct Attr {
    object,
    attr,
    span,
});

ast_pretty!(struct Index {
    object,
    index,
    span,
});

ast_pretty!(struct IfElse {
    condition,
    then_do,
    else_do,
    span,
});

ast_pretty!(struct Call {
    callee,
    arguments,
    span,
});

ast_pretty!(struct Block {
    stmts,
    trailing_expr,
    span,
});

ast_pretty!(enum Stmt {
    Let,
    While,
    Return,
    Expr,
});


ast_pretty!(struct Let {
    name,
    ty,
    value,
    span,
});

ast_pretty!(struct While {
    condition,
    body,
    span,
});

ast_pretty!(struct Return {
    expr,
    span,
});

ast_pretty!(struct Type {
    name
});