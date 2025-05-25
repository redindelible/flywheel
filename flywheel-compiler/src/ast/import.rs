use crate::ast::FileAST;
use crate::id::beacon::Beacon;
use crate::utils::InternedString;
use crate::utils::pretty_tree::{AsDebugTree, PrettyTree};

pub struct Import {
    pub relative_path: InternedString,
}

impl AsDebugTree for Beacon<Import> {
    fn as_debug_tree(&self, context: &FileAST) -> PrettyTree {
        let relative_path = PrettyTree::from_string(format!("{:?}", context.strings.resolve(self.relative_path)));
        PrettyTree::from_struct("Import", [
            ("relative_path", relative_path),
            ("location", PrettyTree::from_location(context.get_location(self.id()).unwrap())),
        ])
    }
}