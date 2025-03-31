use triomphe::{Arc, ArcBorrow};
use crate::ast::TopLevel;
use crate::source::SourceID;
use crate::utils::{InternedString, Interner};
use crate::utils::pretty_tree::{AsDebugTree, PrettyOptions, PrettyTree};

pub struct FileAST {
    pub source_id: SourceID,
    pub strings: Arc<Interner>,
    pub top_levels: Vec<TopLevel>,
}

impl FileAST {
    pub(super) fn new(source_id: SourceID, strings: Arc<Interner>, top_levels: Vec<TopLevel>) -> FileAST
    {
        FileAST {
            source_id,
            strings,
            top_levels,
        }
    }

    pub fn strings(&self) -> ArcBorrow<'_, Interner> {
        self.strings.borrow_arc()
    }

    pub fn resolve(&self, interned: InternedString) -> &str {
        self.strings.resolve(interned)
    }

    pub fn source(&self) -> SourceID {
        self.source_id
    }

    pub fn top_levels(&self) -> &[TopLevel] {
        &self.top_levels
    }

    /*fn to_tree<T: Pretty>(&self, node: AstRef<T>) -> PrettyNode {
        let location = self.get_location(node);
        self.get_node(node).to_tree(self, location)
    }*/
}

impl FileAST {
    pub fn pretty(&self, indent: usize) -> String {
        let tree = PrettyTree::from_struct("File", [(
            "top_levels",
            PrettyTree::from_list(self.top_levels.iter().map(|top_level| top_level.as_debug_tree(self))),
        )]);
        tree.render(&PrettyOptions { indent: String::from_iter(std::iter::repeat_n(' ', indent)), _max_width: 80 }, "")
    }
}
