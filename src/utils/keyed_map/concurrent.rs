use std::marker::PhantomData;
use std::num::{NonZero, NonZeroU32};
use std::sync::Arc;
use parking_lot::RwLock;
use super::{KeyData, KeyMapKey};

pub struct ConcurrentMap<K: KeyMapKey, V> {
    items: RwLock<Vec<Arc<V>>>,
    _phantom: PhantomData<K>
}

impl<K: KeyMapKey, V> ConcurrentMap<K, V> {
    pub fn new() -> Self {
        ConcurrentMap {
            items: RwLock::new(Vec::new()),
            _phantom: PhantomData
        }
    }
    
    pub fn get(&self, key: K) -> Option<Arc<V>> {
        self.items.read().get(key.data().0.get() as usize - 1).map(Arc::clone)
    }
    
    pub fn push(&self, value: V) -> K {
        let new_item = Arc::new(value);
        let key = {
            let mut write_guard = self.items.write();
            let key = write_guard.len();
            write_guard.push(new_item);
            KeyData(NonZero::new(key as u32 + 1).unwrap())
        };
        K::from(key)
    }
}