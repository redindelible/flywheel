use std::future::Future;
use std::pin::Pin;
use std::task::{Context, Poll};
use futures::future::{BoxFuture, Shared};
use futures::FutureExt;

pub struct SharedPromise<T>(Shared<BoxFuture<'static, T>>);

impl<T> SharedPromise<T> where T: Clone {
    fn new<Fut>(fut: Fut) -> Self where Fut: Future<Output=T> + Send + 'static {
        SharedPromise(fut.boxed().shared())
    }
}

impl<T> Future for SharedPromise<T> where T: Clone {
    type Output = T;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        unsafe { self.map_unchecked_mut(|s| &mut s.0).poll(cx) }
    }
}