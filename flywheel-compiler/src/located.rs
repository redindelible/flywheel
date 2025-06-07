use crate::id::AstId;
use crate::source::Location;
use crate::table::AstTable;

pub trait Located {
    fn locate_in(ast_id: AstId, locations: &AstTable<Location>) -> Option<Location> {
        locations.get(&ast_id).map(|x| *x)
    }
    fn locate(&self, locations: &AstTable<Location>) -> Option<Location>;
}
