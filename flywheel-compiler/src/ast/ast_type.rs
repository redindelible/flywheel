use crate::ast::FileAST;
use crate::id::beacon::Beacon;
use crate::utils::InternedString;
use crate::utils::pretty_tree::{AsDebugTree, PrettyTree};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Name(InternedString),
}

impl AsDebugTree for Beacon<Type> {
    fn as_debug_tree(&self, context: &FileAST) -> PrettyTree {
        match self.as_ref() {
            Type::Name(name) => {
                let text = context.strings.resolve(*name);
                PrettyTree::from_struct("Name", [
                    ("name", PrettyTree::from_string(format!("{:?}", text))),
                    ("location", PrettyTree::from_location(context.get_location(self.id()).unwrap())),
                ])
            }
        }
    }
}