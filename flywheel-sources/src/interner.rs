use std::collections::HashMap;
use std::convert::Infallible;
use std::sync::Arc;

use crate::source::SourceMap;
use crate::span::Span;

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct Symbol(Span);

impl Symbol {
    pub(crate) fn span(&self) -> Span {
        self.0
    }
}

pub struct InternerState {
    deduplicator: Arc<dashmap::DashMap<&'static str, Symbol>>,
    sources: Arc<SourceMap>,
}

impl InternerState {
    pub fn new(sources: Arc<SourceMap>) -> InternerState {
        InternerState { deduplicator: Arc::new(dashmap::DashMap::new()), sources }
    }

    pub fn add_builtins(&self, builtins: &[&'static str]) -> HashMap<&'static str, Symbol> {
        let source = self.sources.add_builtins(builtins.iter().copied().collect());
        let mut builtin_map = HashMap::new();
        let mut offset = 0;  // todo we need to increment offset
        for &builtin in builtins {
            let symbol = self
                .deduplicator
                .entry(builtin)
                .or_insert_with(|| Symbol(source.add_span(offset..offset + builtin.len())));
            builtin_map.insert(builtin, *symbol);
            offset += builtin.len();
        }
        builtin_map
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
            sources,
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

#[cfg(test)]
mod test {
    use std::sync::Arc;

    use crate::source::SourceMap;
    use crate::interner::{Interner, InternerState};

    #[test]
    fn test_get_or_intern_round_trip() {
        let sources = Arc::new(SourceMap::new());
        let source = sources.add_string("test", "hello world".to_string());
        let hello_span = source.add_span(0..5);
        let world_span = source.add_span(6..11);

        let mut interner = Interner::new(Arc::clone(&sources));
        let hello_sym = interner.get_or_intern(hello_span);
        let world_sym = interner.get_or_intern(world_span);

        assert_eq!(interner.resolve(hello_sym), "hello");
        assert_eq!(interner.resolve(world_sym), "world");
        assert_ne!(hello_sym, world_sym);
    }

    #[test]
    fn test_deduplication_by_content() {
        let sources = Arc::new(SourceMap::new());
        let source1 = sources.add_string("a", "hello".to_string());
        let source2 = sources.add_string("b", "hello".to_string());
        let span1 = source1.add_span(0..5);
        let span2 = source2.add_span(0..5);

        let mut interner = Interner::new(Arc::clone(&sources));
        let sym1 = interner.get_or_intern(span1);
        let sym2 = interner.get_or_intern(span2);

        assert_eq!(sym1, sym2, "same text from different spans should produce the same symbol");
    }

    #[test]
    fn test_add_builtins_resolves_correctly() {
        let sources = Arc::new(SourceMap::new());
        let state = InternerState::new(Arc::clone(&sources));
        let map = state.add_builtins(&["fn", "let", "if"]);

        assert_eq!(map.len(), 3);
        assert_eq!(state.resolve(map["fn"]), "fn");
        assert_eq!(state.resolve(map["let"]), "let");
        assert_eq!(state.resolve(map["if"]), "if");
    }

    #[test]
    fn test_builtins_deduplicate_with_interned_spans() {
        let sources = Arc::new(SourceMap::new());
        let state = InternerState::new(Arc::clone(&sources));
        let map = state.add_builtins(&["fn", "let"]);

        let source = sources.add_string("test", "fn let".to_string());
        let mut interner = state.interner();
        let fn_sym = interner.get_or_intern(source.add_span(0..2));
        let let_sym = interner.get_or_intern(source.add_span(3..6));

        assert_eq!(fn_sym, map["fn"], "interned 'fn' should equal the builtin symbol");
        assert_eq!(let_sym, map["let"], "interned 'let' should equal the builtin symbol");
    }

    #[test]
    fn test_add_builtins_duplicate_names() {
        // Duplicate builtin strings deduplicate via the DashMap: the second "fn"
        // reuses the first symbol. The offset still advances past the duplicate's
        // position in the concatenated text, so the symbol after it ("let") must
        // still resolve to the correct slice.
        let sources = Arc::new(SourceMap::new());
        let state = InternerState::new(Arc::clone(&sources));
        let map = state.add_builtins(&["fn", "let", "fn"]);

        assert_eq!(map.len(), 2, "duplicate keys collapse in the HashMap");
        assert_eq!(state.resolve(map["fn"]), "fn");
        assert_eq!(state.resolve(map["let"]), "let");
        assert_eq!(map["fn"], map["fn"]); // same symbol both times
    }

    #[test]
    fn test_interner_state_resolve_after_get_or_intern() {
        let sources = Arc::new(SourceMap::new());
        let state = InternerState::new(Arc::clone(&sources));
        let source = sources.add_string("test", "hello".to_string());
        let sym = state.interner().get_or_intern(source.add_span(0..5));
        assert_eq!(state.resolve(sym), "hello");
    }

    #[test]
    fn test_cross_interner_deduplication() {
        let sources = Arc::new(SourceMap::new());
        let state = InternerState::new(Arc::clone(&sources));

        let source1 = sources.add_string("a", "hello".to_string());
        let source2 = sources.add_string("b", "hello".to_string());

        let mut interner1 = state.interner();
        let mut interner2 = state.interner();
        let sym1 = interner1.get_or_intern(source1.add_span(0..5));
        let sym2 = interner2.get_or_intern(source2.add_span(0..5));

        assert_eq!(sym1, sym2, "interners from the same state should deduplicate across instances");
    }
}
