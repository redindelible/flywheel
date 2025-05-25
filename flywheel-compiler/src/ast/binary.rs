use crate::ast::{Expression, FileAST};
use crate::id::beacon::Beacon;
use crate::utils::pretty_tree::{AsDebugTree, PrettyTree};

pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

impl BinaryOp {
    pub fn name(&self) -> &'static str {
        match self {
            BinaryOp::Add => "Add",
            BinaryOp::Sub => "Sub",
            BinaryOp::Mul => "Mul",
            BinaryOp::Div => "Div",
            BinaryOp::Mod => "Mod",
        }
    }
}


pub struct Binary {
    pub op: Beacon<BinaryOp>,
    pub left: Expression,
    pub right: Expression
}


impl AsDebugTree for Beacon<Binary> {
    fn as_debug_tree(&self, context: &FileAST) -> PrettyTree {
        let left = self.left.as_debug_tree(context);
        let right = self.right.as_debug_tree(context);
        PrettyTree::from_struct("Binary", [
            ("left", left),
            ("op", PrettyTree::from_string(self.op.name())),
            ("right", right),
            ("location", PrettyTree::from_location(context.get_location(self.id()).unwrap())),
        ])
    }
}