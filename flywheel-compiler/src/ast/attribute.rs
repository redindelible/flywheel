use crate::ast::{Expression, FileAST};
use crate::id::beacon::Beacon;
use crate::utils::InternedString;
use crate::utils::pretty_tree::{AsDebugTree, PrettyTree};

pub struct Attribute {
    pub object: Expression,
    pub name: Beacon<InternedString>
}

impl AsDebugTree for Beacon<Attribute> {
    fn as_debug_tree(&self, context: &FileAST) -> PrettyTree {
        let object = self.object.as_debug_tree(context);
        let name = context.strings.resolve(*self.name);
        PrettyTree::from_struct("Attr", [
            ("object", object),
            ("name", PrettyTree::from_string(format!("{:?}", name))),
            ("location", PrettyTree::from_location(context.get_location(self.id()).unwrap())),
        ])
    }
}