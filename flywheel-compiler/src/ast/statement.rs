use crate::ast::{Expression, FileAST};
use crate::ast::ast_type::Type;
use crate::ast::block::Block;
use crate::id::beacon::Beacon;
use crate::utils::InternedString;
use crate::utils::pretty_tree::{AsDebugTree, PrettyTree};

pub enum Statement {
    Let { name: Beacon<InternedString>, ty: Option<Beacon<Type>>, value: Expression },
    While { condition: Expression, body: Beacon<Block> },
    Return(Expression),
    Expr(Expression),
}

impl AsDebugTree for Beacon<Statement> {
    fn as_debug_tree(&self, context: &FileAST) -> PrettyTree {
        let location = context.get_location(self.id()).unwrap();
        match self.as_ref() {
            Statement::Return(expr) => PrettyTree::from_struct("Return", [
                ("value", expr.as_debug_tree(context)),
                ("location", PrettyTree::from_location(location)),
            ]),
            Statement::Expr(expr) => PrettyTree::from_struct("Expr", [
                ("value", expr.as_debug_tree(context)),
                ("location", PrettyTree::from_location(location)),
            ]),
            Statement::Let { name, ty, value } => {
                let text = context.strings.resolve(**name);
                let ty = PrettyTree::from_option(ty.as_ref().map(|ty| ty.as_debug_tree(context)));
                let value = value.as_debug_tree(context);
                PrettyTree::from_struct("Let", [
                    ("name", PrettyTree::from_string(format!("{:?}", text))),
                    ("ty", ty),
                    ("value", value),
                    ("location", PrettyTree::from_location(location)),
                ])
            }
            Statement::While { condition, body } => {
                let condition = condition.as_debug_tree(context);
                let body = body.as_debug_tree(context);
                PrettyTree::from_struct("While", [
                    ("condition", condition),
                    ("body", body),
                    ("location", PrettyTree::from_location(location)),
                ])
            }
        }
    }
}