use crate::ast::{Expression, FileAST};
use crate::ast::statement::Statement;
use crate::id::beacon::Beacon;
use crate::utils::pretty_tree::{AsDebugTree, PrettyTree};

pub struct Block {
    pub statements: Vec<Beacon<Statement>>,
    pub trailing_expr: Option<Expression>,
}

impl AsDebugTree for Beacon<Block> {
    fn as_debug_tree(&self, context: &FileAST) -> PrettyTree {
        let stmts = PrettyTree::from_list(self.statements.iter().map(|statement| statement.as_debug_tree(context)));
        let trailing_expr = PrettyTree::from_option(self.trailing_expr.as_ref().map(|expr| expr.as_debug_tree(context)));

        PrettyTree::from_struct("Block", [
            ("stmts", stmts),
            ("trailing_expr", trailing_expr),
            ("location", PrettyTree::from_location(context.get_location(self.id()).unwrap())),
        ])
    }
}