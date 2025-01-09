use parking_lot::RwLock;
use string_interner::backend::BufferBackend;
use string_interner::StringInterner;
use string_interner::symbol::SymbolU32;

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct InternedString(SymbolU32);

#[derive(Debug)]
pub struct Interner {
    symbols: RwLock<StringInterner<BufferBackend<SymbolU32>>>
}

impl Interner {
    pub fn new() -> Self {
        Interner {
            symbols: RwLock::new(StringInterner::new())
        }
    }

    pub fn get_or_intern_static(&self, text: &'static str) -> InternedString {
        self.get_or_intern(text)
    }
    
    pub fn get_or_intern(&self, text: &str) -> InternedString {
        let maybe_symbol = self.symbols.read().get(text);
        match maybe_symbol {
            Some(symbol) => InternedString(symbol),
            None => InternedString(self.symbols.write().get_or_intern(text))
        }
    }

    // todo make this not need a guard
    pub(crate) fn resolve(&self, symbol: InternedString) -> Option<parking_lot::MappedRwLockReadGuard<'_, str>> {
        parking_lot::RwLockReadGuard::try_map(self.symbols.read(), |interner| interner.resolve(symbol.0)).ok()
    }
}