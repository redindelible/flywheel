use std::collections::HashMap;
use std::sync::Arc;

use flywheel_ast as ast;
use flywheel_error::{CompileMessage, CompileResult};
use flywheel_sources::{Span, Symbol};

pub enum Builtin {
    U32,
}

pub enum Item<'ast> {
    Imported(&'ast ast::Import<'ast>),
    Function(&'ast ast::Function<'ast>),
    Struct(&'ast ast::Struct<'ast>),
    Builtin(Builtin),
}

impl<'ast> Item<'ast> {
    pub fn span(&self) -> Option<Span> {
        match self {
            Item::Imported(item) => Some(item.span),
            Item::Function(item) => Some(item.span),
            Item::Struct(item) => Some(item.span),
            Item::Builtin(_) => None,
        }
    }
}

pub struct Namespace<'ast> {
    items: HashMap<Symbol, Item<'ast>>,
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

    fn resolve(&self, name: Symbol) -> Option<&Item<'ast>> {
        self.items.get(&name)
    }
}


pub struct SearchPath<'a, 'ast>(Vec<&'a [Namespace<'ast>]>);

impl<'a, 'ast> From<&'a Namespace<'ast>> for SearchPath<'a, 'ast> {
    fn from(value: &'a Namespace<'ast>) -> SearchPath<'a, 'ast> {
        SearchPath(vec![std::slice::from_ref(value)])
    }
}

impl<'a, 'ast> From<&'a [Namespace<'ast>]> for SearchPath<'a, 'ast> {
    fn from(value: &'a [Namespace<'ast>]) -> SearchPath<'a, 'ast> {
        SearchPath(vec![value])
    }
}

impl<'a, 'ast> SearchPath<'a, 'ast> {
    pub fn then(mut self, ns: &'a Namespace<'ast>) -> SearchPath<'a, 'ast> {
        self.0.push(std::slice::from_ref(ns));
        self
    }

    pub fn resolve(&self, name: Symbol) -> Option<&'a Item<'ast>> {
        self.0.iter().copied().flatten().find_map(|ns| ns.resolve(name))
    }
}