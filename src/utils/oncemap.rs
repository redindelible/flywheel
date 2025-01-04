use std::collections::HashMap;
use std::future::Future;
use std::hash::Hash;
use tokio::sync::{OnceCell, RwLock};

pub struct OnceMap<K, V> {
    inner: RwLock<HashMap<K, OnceCell<V>>>
}

impl<K, V> OnceMap<K, V> where K: Eq + Hash, V: Clone {
    pub fn new() -> Self {
        OnceMap { inner: RwLock::new(HashMap::new()) }
    }
    
    pub async fn get_or_init<F, Fut>(&self, k: K, init: F) -> V where F: FnOnce() -> Fut, Fut: Future<Output=V> {
        let read_lock = self.inner.read().await;
        if let Some(cell) = read_lock.get(&k) {
            cell.get_or_init(init).await.clone()
        } else {
            drop(read_lock);
            let mut write_lock = self.inner.write().await;
            let cell = write_lock.entry(k).or_default();
            cell.get_or_init(init).await.clone()
        }
    }
}