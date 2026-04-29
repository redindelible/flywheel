use std::collections::HashMap;

use flywheel_ast as ast;
use flywheel_exchange as ex;
use flywheel_error::{CompileMessage, CompileResult};
use flywheel_sources::{Span, Symbol};
use crate::types::Type;

pub enum Builtin {
    U32,
}

pub enum Item<'ast> {
    Imported(&'ast ast::Import<'ast>),
    Function(&'ast ast::Function<'ast>),
    Struct(&'ast ast::Struct<'ast>),
    Builtin(Builtin),
    Local { index: ex::LocalId, ty: Type<'ast>, span: Span }
}

impl<'ast> Item<'ast> {
    pub fn span(&self) -> Option<Span> {
        match self {
            Item::Imported(item) => Some(item.span),
            Item::Function(item) => Some(item.span),
            Item::Struct(item) => Some(item.span),
            Item::Builtin(_) => None,
            Item::Local { span, ..} => Some(*span),
        }
    }
}

pub enum Value<'ast> {
    Local { index: ex::LocalId, ty: Type<'ast>, span: Span }
}

pub struct Namespace<'ast> {
    pub items: HashMap<Symbol, Item<'ast>>,
}

impl<'ast> Namespace<'ast> {
    pub fn new() -> Namespace<'ast> {
        Namespace { items: HashMap::new() }
    }

    pub fn add(&mut self, name: Symbol, item: Item<'ast>) -> CompileResult<()> {
        use std::collections::hash_map::Entry;

        match self.items.entry(name) {
            Entry::Vacant(vacant) => {
                vacant.insert(item);
            }
            Entry::Occupied(occupied) => {
                let message = CompileMessage::error_dyn(move |s| {
                    format!("There's already a thing called {}", s.get_symbol(name))
                })
                .with_span(item.span().unwrap())
                .with_child(CompileMessage::note("Previously declared here").with_span(occupied.get().span().unwrap()));
                return Err(message);
            }
        }
        Ok(())
    }

    pub fn search<'a>(search: impl SearchPath<'a, 'ast>, name: ast::Name) -> CompileResult<&'a Item<'ast>> where 'ast: 'a {
        for ns in search {
            if let Some(item) = ns.items.get(&name.symbol) {
                return Ok(item);
            }
        }
        Err(CompileMessage::error_dyn(move |s| format!("Could not resolve `{}`", s.get_symbol(name.symbol))).with_span(name.span))
    }
}


pub(crate) trait SearchPath<'a, 'ast: 'a>: IntoIterator<Item=&'a Namespace<'ast>> {

}

impl<'a, 'ast, T> SearchPath<'a, 'ast> for T where T: IntoIterator<Item=&'a Namespace<'ast>>, 'ast: 'a {

}
