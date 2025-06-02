use crate::ast::ast_type::Type;
use crate::ast::FileAST;
use crate::id::beacon::Beacon;
use crate::utils::InternedString;
use crate::utils::pretty_tree::{AsDebugTree, PrettyTree};

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Struct {
    pub name: Beacon<InternedString>,
    pub fields: Vec<Beacon<Field>>,
}


#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Field {
    pub name: Beacon<InternedString>,
    pub ty: Beacon<Type>,
}


impl AsDebugTree for Beacon<Struct> {
    fn as_debug_tree(&self, context: &FileAST) -> PrettyTree {
        let name = PrettyTree::from_string(format!("{:?}", context.strings.resolve(*self.name)));
        let fields = PrettyTree::from_list(self.fields.iter().map(|field| field.as_debug_tree(context)));
        PrettyTree::from_struct("Struct", [
            ("name", name),
            ("fields", fields),
            ("location", PrettyTree::from_location(context.get_location(self.id()).unwrap())),
        ])
    }
}

impl AsDebugTree for Beacon<Field> {
    fn as_debug_tree(&self, context: &FileAST) -> PrettyTree {
        let name = PrettyTree::from_string(format!("{:?}", context.strings.resolve(*self.name)));
        let ty = self.ty.as_debug_tree(context);
        PrettyTree::from_struct("Field", [
            ("name", name),
            ("ty", ty),
            ("location", PrettyTree::from_location(*context.locations.get(&self.id()).unwrap())),
        ])
    }
}