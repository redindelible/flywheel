use std::future::Future;
use std::hash::Hash;
use std::sync::Arc;
use std::time::Instant;
use dashmap::DashMap;
use tokio::sync::OnceCell;


pub struct OnceMap<K, V> {
    inner: DashMap<K, Arc<OnceCell<V>>>
}

impl<K, V> OnceMap<K, V> where K: Eq + Hash, V: Clone {
    pub fn new() -> Self {
        OnceMap { inner: DashMap::new() }
    }
    
    pub async fn get_or_init<F, Fut>(&self, k: K, init: F) -> V where K: Clone, F: FnOnce() -> Fut, Fut: Future<Output=V> {
        // let start = Instant::now();
        let cell = {
            if let Some(cell) = self.inner.get(&k) {
                cell.clone()
            } else {
                self.inner.entry(k).or_default().downgrade().clone()
            }
        };
        // println!("Time to get cell: {:?}", start.elapsed());
        cell.get_or_init(init).await.clone()
    }
}