pub mod keymap;

use std::ptr::NonNull;
use std::sync::atomic::{fence, AtomicUsize, Ordering};

pub(crate) use keymap::{ReservableKeyMap, KeyData, declare_key_type};

struct ArcSliceHeader<T> {
    ref_count: AtomicUsize,
    count: usize,
    items: [T; 0]
}

impl<T> Drop for ArcSliceHeader<T> {
    fn drop(&mut self) {
        unsafe {
            for index in 0..self.count {
                self.items.as_mut_ptr().add(index).drop_in_place();
            }
        }
    }
}

pub struct ArcSlice<T> {
    header: NonNull<ArcSliceHeader<T>>,
    start: NonNull<T>,
    count: usize
}


impl<T> ArcSlice<T> {
    pub fn new() -> ArcSlice<T> {
        let header = Box::leak(Box::new(ArcSliceHeader {
            ref_count: AtomicUsize::new(1),
            count: 0,
            items: []
        }));
        let start = NonNull::from(&mut header.items).cast();
        ArcSlice {
            header: NonNull::from(header),
            start,
            count: 0
        }
    }
}

impl<T> Drop for ArcSlice<T> {
    fn drop(&mut self) {
        unsafe {
            if self.header.as_ref().ref_count.fetch_sub(1, Ordering::Release) == 1 {
                fence(Ordering::Acquire);
                drop(Box::from_raw(self.header.as_ptr()));
            }
        }
    }
}

