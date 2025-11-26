use std::convert::Infallible;
use std::sync::Arc;

use crate::source::SourceMap;
use crate::span::Span;

#[derive(Copy, Clone)]
pub struct Symbol(Span);

impl Symbol {
    pub fn span(self) -> Span {
        self.0
    }
}

pub struct InternerState {
    deduplicator: Arc<dashmap::DashMap<&'static str, Symbol>>,
    sources: Arc<SourceMap>
}

impl InternerState {
    pub fn new(sources: Arc<SourceMap>) -> InternerState {
        InternerState {
            deduplicator: Arc::new(dashmap::DashMap::new()),
            sources,
        }
    }
    
    pub fn interner(&self) -> Interner {
        Interner {
            cache: quick_cache::unsync::Cache::new(1024),
            deduplicator: Arc::clone(&self.deduplicator),
            sources: Arc::clone(&self.sources),
        }
    }

    pub fn resolve(&self, symbol: Symbol) -> &str {
        self.sources.get_span(symbol.0)
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
struct StaticString(&'static str);

pub struct Interner {
    cache: quick_cache::unsync::Cache<StaticString, Symbol>,
    deduplicator: Arc<dashmap::DashMap<&'static str, Symbol>>,
    sources: Arc<SourceMap>,
}

impl Interner {
    pub fn new(sources: Arc<SourceMap>) -> Interner {
        Interner {
            cache: quick_cache::unsync::Cache::new(1024),
            deduplicator: Arc::new(dashmap::DashMap::new()),
            sources
        }
    }
    
    pub fn get_or_intern(&mut self, span: Span) -> Symbol {
        let text: &'static str = unsafe { transmute_lifetime(self.sources.get_span(span)) };
        *self
            .cache
            .get_or_insert_with(&StaticString(text), || -> Result<Symbol, Infallible> {
                Ok(*self.deduplicator.entry(text).or_insert(Symbol(span)))
            })
            .unwrap()
            .unwrap()
    }

    pub fn resolve(&self, symbol: Symbol) -> &str {
        self.sources.get_span(symbol.0)
    }
}

unsafe fn transmute_lifetime<'a, T: ?Sized>(value: &T) -> &'a T {
    unsafe { std::mem::transmute(value) }
}