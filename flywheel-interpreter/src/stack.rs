use std::alloc::Layout;
use std::marker::PhantomData;
use std::mem::offset_of;
use std::ptr::NonNull;

struct Chunk {
    
}

pub struct Stack;

#[repr(C)]
struct Object<H, S> {
    header: H,
    prev: Option<NonNull<Object<H, S>>>,
    count: usize,
    slice: [S; 0]
}

unsafe fn slice_ptr<H, S>(object: NonNull<Object<H, S>>) -> *mut [S] {
    let ptr = object.as_ptr().wrapping_add(offset_of!(Object<H, S>, slice)).cast::<S>();
    let count = unsafe { (*object.as_ptr()).count };
    std::ptr::slice_from_raw_parts_mut(ptr, count)
}

pub struct StackObject<H, S> {
    object: NonNull<Object<H, S>>
}

impl<H, S> StackObject<H, S> {
    const fn layout() -> Layout {
        
    }
    
    pub fn header(&self) -> &H {
        unsafe { &self.object.as_ref().header }
    }

    pub fn header_mut(&mut self) -> &mut H {
        unsafe { &mut self.object.as_mut().header }
    }

    pub fn slice(&self) -> &[S] {
        unsafe { &*slice_ptr(self.object) }
    }

    pub fn slice_mut(&mut self) -> &mut [S] {
        unsafe { &mut *slice_ptr(self.object) }
    }
}

impl<H, S> Drop for StackObject<H, S> {
    fn drop(&mut self) {
        let mut current = Some(self.object);
        while let Some(this) = current.take() {
            unsafe {
                current = (*this.as_ptr()).prev;
                
                let slice = slice_ptr(this);
                for i in 0..slice.len() {
                    slice.cast::<S>().wrapping_add(i).drop_in_place();
                }
                
                (&raw mut (*this.as_ptr()).header).drop_in_place();
                
            }
        }
    }
}