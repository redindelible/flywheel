use std::alloc::{handle_alloc_error, Layout, LayoutError};
use std::hint::assert_unchecked;
use std::mem::{forget, offset_of};
use std::ptr::NonNull;

use allocator_api2::alloc::Allocator;
use bytemuck::Zeroable;

struct Inner<T, H> {
    header: H,
    count: usize,
    items: [T; 0]
}

const fn layout_for_inner<T, H>(count: usize) -> Result<Layout, LayoutError> {
    let slice_layout = match Layout::array::<T>(count) {
        Ok(layout) => layout,
        Err(e) => return Err(e)
    };
    let (layout, offset) = match Layout::new::<Inner<T, H>>().extend(slice_layout) {
        Ok(pair) => pair,
        Err(e) => return Err(e)
    };
    debug_assert!(offset == offset_of!(Inner<T, H>, items));
    unsafe {
        assert_unchecked(layout.align() == align_of::<usize>());
    }
    Ok(layout.pad_to_align())
}

fn slice_ptr<T, H>(ptr: *mut Inner<T, H>) -> *mut T {
    ptr.wrapping_byte_add(offset_of!(Inner<T, H>, items)).cast::<T>()
}

pub struct ThinList<T, H=()>(NonNull<Inner<T, H>>);

impl<T: Zeroable, H> ThinList<T, H> {
    pub fn from_header_zeroed_in<A: Allocator>(header: H, count: usize, allocator: A) -> Self {
        let layout = layout_for_inner::<T, H>(count).unwrap();
        let ptr = unsafe {
            let ptr = allocator.allocate_zeroed(layout).unwrap_or_else(|_| handle_alloc_error(layout)).cast::<Inner<T, H>>().as_ptr();
            (&raw mut (*ptr).header).write(header);
            (&raw mut (*ptr).count).write(count);

            NonNull::new_unchecked(ptr)
        };
        ThinList(ptr)
    }
}

impl<T, H> ThinList<T, H> {
    pub fn header(&self) -> &H {
        unsafe { &self.0.as_ref().header }
    }
    
    pub fn header_mut(&mut self) -> &mut H {
        unsafe { &mut self.0.as_mut().header }
    }
    
    pub fn deallocate<A: Allocator>(self, allocator: A) -> H {
        unsafe {
            let ptr = self.0.as_ptr();
            
            let header = (&raw mut (*ptr).header).read();
            let count = (*ptr).count;

            let slice_ptr = slice_ptr(ptr);
            for i in 0..count {
                slice_ptr.wrapping_add(i).drop_in_place();
            }

            allocator.deallocate(self.0.cast(), layout_for_inner::<T, H>(count).unwrap());
            forget(self);
            
            header
        }
    }
    
    pub fn len(&self) -> usize {
        unsafe { (*self.0.as_ptr()).count }
    }
    
    fn slice_raw(&self) -> *mut [T] {
        let slice_ptr = slice_ptr(self.0.as_ptr());
        let count = self.len();
        std::ptr::slice_from_raw_parts_mut(slice_ptr, count)
    }
    
    pub fn slice(&self) -> &[T] {
        unsafe { &*self.slice_raw() }
    }

    pub fn slice_mut(&mut self) -> &mut [T] {
        unsafe { &mut *self.slice_raw() }
    }
    
    pub unsafe fn get_unchecked(&self, index: usize) -> &T {
        let slice_ptr = slice_ptr(self.0.as_ptr());
        unsafe {
            &*slice_ptr.add(index)
        }
    }

    pub unsafe fn get_unchecked_mut(&mut self, index: usize) -> &mut T {
        let slice_ptr = slice_ptr(self.0.as_ptr());
        unsafe {
            &mut *slice_ptr.add(index)
        }
    }

    pub fn parts_mut(&mut self) -> (&mut H, &mut [T]) {
        unsafe { (&mut self.0.as_mut().header, &mut *self.slice_raw()) }
    }
}

impl<T, H> Drop for ThinList<T, H> {
    fn drop(&mut self) {
        unsafe {
            let ptr = self.0.as_ptr();
            
            (&raw mut (*ptr).header).drop_in_place();
            let count = (*ptr).count;

            if std::mem::needs_drop::<T>() {
                let slice_ptr = slice_ptr(ptr);
                for i in 0..count {
                    slice_ptr.wrapping_add(i).drop_in_place();
                }
            }
        }
    }
}