use crate::ast::{Expression, FileAST};
use crate::id::beacon::Beacon;
use crate::utils::pretty_tree::{AsDebugTree, PrettyTree};

pub struct Call {
    pub callee: Expression,
    pub arguments: Vec<Expression>
}

impl AsDebugTree for Beacon<Call> {
    fn as_debug_tree(&self, context: &FileAST) -> PrettyTree {
        let arguments = PrettyTree::from_list(self.arguments.iter().map(|arg| arg.as_debug_tree(context)));
        let callee = self.callee.as_debug_tree(context);
        PrettyTree::from_struct("Call", [
            ("callee", callee),
            ("arguments", arguments),
            ("location", PrettyTree::from_location(context.get_location(self.id()).unwrap())),
        ])
    }
}