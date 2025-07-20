use std::future::Future;
use std::hash::Hash;

use dashmap::DashMap;
use futures_util::TryFutureExt;
use tokio::sync::OnceCell;
use triomphe::{Arc, ArcBorrow};

use crate::driver::Handle;
use crate::error::{CompileError, CompileResult};

pub trait QueryEngine {
    fn get_processor<T>(&self) -> &T
    where
        T: Processor,
        Self: SupportsQueryOn<T>,
    {
        &self.select().processor
    }

    async fn query<T>(&self, handle: &Handle, input: T::Input) -> CompileResult<ArcBorrow<'_, T::Output>>
    where
        T: Processor,
        Self: SupportsQueryOn<T>,
    {
        self.select().query(handle, input).await
    }
}

pub trait Processor: 'static {
    type Input: Eq + Hash + Clone + Send;
    type Output: Send + Sync + 'static;

    fn process(handle: Handle, input: Self::Input) -> impl Future<Output = CompileResult<Self::Output>> + Send;
}

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

pub struct State<P: Processor> {
    processor: P,
    stored: OnceMap<P::Input, CompileResult<Arc<P::Output>>>,
}

impl<P: Processor> State<P> {
    pub fn new(processor: P) -> Self {
        State { processor, stored: OnceMap::new() }
    }

    async fn query(&self, handle: &Handle, input: P::Input) -> CompileResult<ArcBorrow<'_, P::Output>> {
        let result_cached = self
            .stored
            .get_or_init(input, |input| {
                let handle_clone = handle.clone();
                let task = handle.spawn(async move {
                    let processed = P::process(handle_clone, input).await?;
                    Ok(Arc::new(processed))
                });
                async move { task.await.unwrap() }
            })
            .await;
        match result_cached {
            Ok(processed) => Ok(processed.borrow_arc()),
            Err(error) => Err(error.clone()),
        }
    }
}

pub trait SupportsQueryOn<T>
where
    T: Processor,
{
    fn select(&self) -> &State<T>;
}

macro_rules! query_engine {
    {$vis:vis struct $name:ident { $($field:ident: $ty:ty),* $(,)? } } => {
        $vis struct $name {
            $(
                $field: $crate::query::State<$ty>
            ),*
        }

        const _: () = {
            #[allow(private_bounds)]
            impl $name {
                pub fn new($($field: $ty),*) -> Self {
                    Self {
                        $($field: $crate::query::State::new($field)),*
                    }
                }
            }

            impl $crate::query::QueryEngine for $name { }

            $(
                impl $crate::query::SupportsQueryOn<$ty> for $name {
                    fn select(&self) -> &$crate::query::State<$ty> {
                        &self.$field
                    }
                }
            )*
        };
    };
}
pub(crate) use query_engine;
