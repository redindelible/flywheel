use crate::ast::{Block, FileAST};
use crate::ast::ast_type::Type;
use crate::id::beacon::Beacon;
use crate::utils::InternedString;
use crate::utils::pretty_tree::{AsDebugTree, PrettyTree};

pub struct Function {
    pub name: Beacon<InternedString>,
    pub return_type: Beacon<Type>,
    pub body: Beacon<Block>,
}

impl AsDebugTree for Beacon<Function> {
    fn as_debug_tree(&self, context: &FileAST) -> PrettyTree {
        let name = PrettyTree::from_string(format!("{:?}", context.strings.resolve(*self.name)));
        let return_type = self.return_type.as_debug_tree(context);
        let body = self.body.as_debug_tree(context);
        PrettyTree::from_struct("Function", [
            ("name", name),
            ("return_type", return_type),
            ("body", body),
            ("location", PrettyTree::from_location(context.get_location(self.id()).unwrap())),
        ])
    }
}
