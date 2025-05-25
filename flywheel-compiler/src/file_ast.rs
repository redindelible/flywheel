use triomphe::{Arc, ArcBorrow};
use crate::ast::TopLevel;
use crate::id::AstId;
use crate::source::{Location, SourceID};
use crate::table::AstTable;
use crate::utils::{InternedString, Interner};
use crate::utils::pretty_tree::{AsDebugTree, PrettyOptions, PrettyTree};

pub struct FileAST {
    pub source_id: SourceID,
    pub strings: Arc<Interner>,
    pub top_levels: Vec<TopLevel>,
    pub locations: AstTable<Location>
}

impl FileAST {
    pub(super) fn new(source_id: SourceID, strings: Arc<Interner>, top_levels: Vec<TopLevel>, locations: AstTable<Location>) -> FileAST
    {
        FileAST {
            source_id,
            strings,
            top_levels,
            locations
        }
    }
    
    pub fn get_location(&self, ast_id: AstId) -> Option<Location> {
        self.locations.get(&ast_id).map(|x| *x)
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
