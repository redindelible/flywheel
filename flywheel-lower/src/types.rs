use flywheel_ast as ast;

#[derive(Clone)]
pub enum Type<'ast> {
    Unit,
    Bool,
    U32,
    Struct(&'ast ast::Struct<'ast>),
}

impl<'ast> PartialEq for Type<'ast> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (&Type::Unit, &Type::Unit) => true,
            (&Type::Bool, &Type::Bool) => true,
            (&Type::U32, &Type::U32) => true,
            (&Type::Struct(a_ptr), &Type::Struct(b_ptr)) => std::ptr::eq(a_ptr, b_ptr),
            (_, _) => false,
        }
    }
}
