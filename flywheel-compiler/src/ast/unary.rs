use crate::ast::{Expression, FileAST};
use crate::id::beacon::Beacon;
use crate::utils::pretty_tree::{AsDebugTree, PrettyTree};

pub enum UnaryOp {
    Not,
    Neg,
}

impl UnaryOp {
    pub fn name(&self) -> &'static str {
        match self {
            UnaryOp::Not => "Not",
            UnaryOp::Neg => "Neg",
        }
    }
}

pub struct Unary {
    pub op: Beacon<UnaryOp>,
    pub right: Expression
}

impl AsDebugTree for Beacon<Unary> {
    fn as_debug_tree(&self, context: &FileAST) -> PrettyTree {
        let right = self.right.as_debug_tree(context);
        PrettyTree::from_struct("Unary", [
            ("op", PrettyTree::from_string(self.op.name())),
            ("right", right),
            ("location", PrettyTree::from_location(context.get_location(self.id()).unwrap())),
        ])
    }
}