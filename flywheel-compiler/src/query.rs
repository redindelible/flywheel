use std::future::Future;
use std::hash::Hash;

use futures_util::FutureExt;
use triomphe::{Arc, ArcBorrow};

use crate::driver::Handle;
use crate::error::CompileResult;

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

pub struct State<P: Processor> {
    processor: P,
    stored: OnceMap<P::Input, CompileResult<Arc<P::Output>>>,
}

impl<P: Processor> State<P> {
    pub fn new(processor: P) -> Self {
        State { processor, stored: OnceMap::new() }
    }

    async fn query(&self, handle: &Handle, input: P::Input) -> CompileResult<ArcBorrow<'_, P::Output>> {
        self.stored
            .get_or_init(input, |input| {
                let handle_clone = handle.clone();
                handle.spawn(async move { P::process(handle_clone, input).await.map(Arc::new) }).map(Result::unwrap)
            })
            .await
            .as_ref()
            .map(Arc::borrow_arc)
            .map_err(Clone::clone)
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

use crate::utils::OnceMap;
