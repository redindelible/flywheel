use std::collections::HashMap;
use std::sync::Arc;

use flywheel_ast as ast;
use flywheel_error::{CompileMessage, CompileResult};
use flywheel_sources::{SourceMap, Span, Symbol};

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

pub struct Namespace<'ast, 'ns> {
    sources: Arc<SourceMap>,
    parent: Option<&'ns Namespace<'ast, 'ns>>,
    items: HashMap<Symbol, Item<'ast>>,
}

impl<'ast, 'ns> Namespace<'ast, 'ns> {
    pub fn new(sources: Arc<SourceMap>, parent: Option<&'ns Namespace<'ast, 'ns>>) -> Self {
        Namespace { sources, parent, items: HashMap::new() }
    }

    pub fn add(&mut self, name: Symbol, item: Item<'ast>) -> CompileResult<()> {
        use std::collections::hash_map::Entry;

        match self.items.entry(name) {
            Entry::Vacant(vacant) => {
                vacant.insert(item);
            }
            Entry::Occupied(occupied) => {
                let name = self.sources.get_symbol(name);
                let message = CompileMessage::error(format!("There's already a thing called {}", name))
                    .with_span(item.span().unwrap())
                    .with_child(
                        CompileMessage::note("Previously declared here").with_span(occupied.get().span().unwrap()),
                    );
                return Err(Box::new(message));
            }
        }
        Ok(())
    }

    pub fn resolve(&self, name: Symbol) -> Option<&Item<'ast>> {
        let mut search_in = self;
        loop {
            // dbg!(search_in.items.keys().map(|&sym| self.resolve_symbol(sym)).collect::<Vec<_>>());
            if let Some(item) = search_in.items.get(&name) {
                return Some(item);
            } else if let Some(parent) = &search_in.parent {
                search_in = parent;
            } else {
                return None;
            }
        }
    }

    pub fn resolve_symbol(&self, name: Symbol) -> &str {
        self.sources.get_symbol(name)
    }
}
