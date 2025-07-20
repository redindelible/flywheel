use std::hash::BuildHasher;
use std::num::NonZero;
use std::sync::{Arc, LazyLock};

use hashbrown::HashTable;
use parking_lot::{Mutex, RwLock};
use rangemap::RangeMap;
use stable_deref_trait::StableDeref;

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct InternedString(NonZero<u32>);

#[derive(Clone)]
struct StrBuffer<'a>(&'a str, Arc<dyn StableDeref<Target = str> + Sync + 'a>);

impl<'a> StrBuffer<'a> {
    fn new<B>(buffer: B) -> Self
    where
        B: StableDeref<Target = str> + Sync + 'a,
    {
        let item = unsafe { std::mem::transmute::<&str, &'a str>(&*buffer) };
        StrBuffer(item, Arc::new(buffer))
    }

    fn str(&self) -> &'a str {
        self.0
    }
}

impl<'a> PartialEq<Self> for StrBuffer<'a> {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.1, &other.1)
    }
}

impl<'a> Eq for StrBuffer<'a> {}

struct Buffers<'a>(RangeMap<usize, StrBuffer<'a>>);

impl<'a> Buffers<'a> {
    fn new() -> Self {
        Buffers(RangeMap::new())
    }

    fn insert<B>(&mut self, buffer: B)
    where
        B: StableDeref<Target = str> + Sync + 'a,
    {
        let buffer = StrBuffer::new(buffer);
        let ptr_range = buffer.str().as_bytes().as_ptr_range();
        self.0.insert(ptr_range.start.addr()..ptr_range.end.addr(), buffer);
    }

    fn get_str(&self, str: &str) -> Option<&'a str> {
        if self.0.contains_key(&str.as_ptr().addr()) {
            Some(unsafe { std::mem::transmute::<&str, &'a str>(str) })
        } else {
            None
        }
    }
}

pub struct Interner<S = rustc_hash::FxBuildHasher> {
    default_buffers: Mutex<Vec<Box<str>>>,
    buffers: Mutex<Buffers<'static>>,

    shards: Vec<RwLock<HashTable<(&'static str, InternedString)>>>,
    mask: u64,
    hasher: S,

    symbols: RwLock<Vec<&'static str>>,
}

static SHARDS_COUNT: LazyLock<usize> =
    LazyLock::new(|| (std::thread::available_parallelism().map_or(8, NonZero::get) * 4).next_power_of_two());

impl<S> Interner<S> {
    pub fn new() -> Self
    where
        S: Default,
    {
        Self::with_hasher(S::default())
    }

    pub fn with_hasher(hasher: S) -> Self {
        Self::with_shards_and_hasher(*SHARDS_COUNT, hasher)
    }

    pub fn with_shards_and_hasher(shard_count: usize, hasher: S) -> Self {
        assert!(shard_count.is_power_of_two());
        Interner {
            default_buffers: Mutex::new(Vec::new()),
            buffers: Mutex::new(Buffers::new()),
            shards: std::iter::repeat_with(|| RwLock::new(HashTable::new())).take(shard_count).collect(),
            mask: shard_count as u64 - 1,
            hasher,
            symbols: RwLock::new(vec![""]),
        }
    }

    pub fn add_buffer<B>(&self, buffer: B)
    where
        B: StableDeref<Target = str> + Sync + 'static,
    {
        self.buffers.lock().insert(buffer);
    }
}

impl<S: BuildHasher> Interner<S> {
    pub fn get_or_intern_static(&self, text: &'static str) -> InternedString {
        self.get_or_intern_with_inserter(text, |text| text)
    }

    pub fn get_or_intern_in_buffer(&self, text: &str) -> InternedString {
        self.get_or_intern_with_inserter(text, |text| self.buffers.lock().get_str(text).unwrap())
    }

    pub fn get_or_intern(&self, text: &str) -> InternedString {
        self.get_or_intern_with_inserter(text, |text| {
            let maybe_str = self.buffers.lock().get_str(text);
            if let Some(maybe_str) = maybe_str {
                maybe_str
            } else {
                let text_box = text.to_owned().into_boxed_str();
                let text_static = unsafe { std::mem::transmute::<&str, &'static str>(&*text_box) };
                self.default_buffers.lock().push(text_box);
                text_static
            }
        })
    }

    #[allow(clippy::needless_lifetimes)]
    fn get_or_intern_with_inserter<'t>(
        &self,
        text: &'t str,
        inserter: impl FnOnce(&'t str) -> &'static str,
    ) -> InternedString {
        let hash_of_text = self.hasher.hash_one(text);
        let shard_index = hash_of_text & self.mask;
        let shard = self.shards.get(shard_index as usize).unwrap();

        let shard_guard = shard.read();
        let maybe_entry = shard_guard.find(hash_of_text, |&(other, _)| other == text);
        if let Some(&(_, symbol)) = maybe_entry {
            return symbol;
        }
        drop(shard_guard);

        let mut shard_guard = shard.write();
        let maybe_entry =
            shard_guard.entry(hash_of_text, |&(other, _)| other == text, |&(key, _)| self.hasher.hash_one(key));
        match maybe_entry {
            hashbrown::hash_table::Entry::Occupied(occupied) => occupied.get().1,
            hashbrown::hash_table::Entry::Vacant(vacant) => {
                let text_static = inserter(text);
                let mut write_guard = self.symbols.write();
                let symbol = InternedString(NonZero::new(write_guard.len() as u32).unwrap());
                write_guard.push(text_static);
                drop(write_guard);

                vacant.insert((text_static, symbol));
                symbol
            }
        }
    }

    pub fn resolve(&self, symbol: InternedString) -> &str {
        self.symbols.read()[symbol.0.get() as usize]
    }
}

unsafe impl<S> Send for Interner<S> {}
unsafe impl<S> Sync for Interner<S> {}
