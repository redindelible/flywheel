use crate::ast::{Expression, FileAST};
use crate::id::beacon::Beacon;
use crate::utils::pretty_tree::{AsDebugTree, PrettyTree};

pub struct IfElse {
    pub condition: Expression,
    pub then_do: Expression,
    pub else_do: Option<Expression>
}

impl AsDebugTree for Beacon<IfElse> {
    fn as_debug_tree(&self, context: &FileAST) -> PrettyTree {
        let condition = self.condition.as_debug_tree(context);
        let then_do = self.then_do.as_debug_tree(context);
        let else_do = PrettyTree::from_option(self.else_do.as_ref().map(|expr| expr.as_debug_tree(context)));
        PrettyTree::from_struct("IfElse", [
            ("condition", condition),
            ("then_do", then_do),
            ("else_do", else_do),
            ("location", PrettyTree::from_location(context.get_location(self.id()).unwrap())),
        ])
    }
}