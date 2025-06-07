use crate::ast::{Expression, FileAST};
use crate::id::beacon::Beacon;
use crate::utils::pretty_tree::{AsDebugTree, PrettyTree};

pub struct Index {
    pub object: Expression,
    pub index: Expression
}

impl AsDebugTree for Beacon<Index> {
    fn as_debug_tree(&self, context: &FileAST) -> PrettyTree {
        let object = self.object.as_debug_tree(context);
        let index = self.index.as_debug_tree(context);
        PrettyTree::from_struct("Index", [
            ("object", object),
            ("index", index),
            ("location", PrettyTree::from_location(context.get_location(self.id()).unwrap())),
        ])
    }
}