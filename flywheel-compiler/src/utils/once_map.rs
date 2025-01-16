use std::future::Future;
use std::hash::Hash;

use dashmap::DashMap;
use tokio::sync::OnceCell;

pub struct OnceMap<K, V> {
    inner: DashMap<K, Box<OnceCell<V>>>,
}

impl<K, V> OnceMap<K, V>
where
    K: Eq + Hash,
{
    pub fn new() -> Self {
        OnceMap { inner: DashMap::new() }
    }

    pub async fn get_or_init<F, Fut>(&self, k: K, init: F) -> &V
    where
        K: Clone,
        F: FnOnce(K) -> Fut,
        Fut: Future<Output = V>,
    {
        let cell: &'_ OnceCell<V> = {
            let cell_ref = match self.inner.get(&k) {
                Some(cell_ref) => cell_ref,
                None => self.inner.entry(k.clone()).or_default().downgrade(),
            };
            unsafe { std::mem::transmute::<&OnceCell<V>, &'_ OnceCell<V>>(&*cell_ref) }
        };
        cell.get_or_init(move || init(k)).await
    }
}
