use std::alloc::{alloc, dealloc, handle_alloc_error, Layout, LayoutError};
use std::mem::{forget, offset_of};
use std::ptr::NonNull;

struct Inner<T, H> {
    header: H,
    count: usize,
    items: [T; 0]
}

const fn layout_for_inner<T, H>(count: usize) -> Result<(Layout, usize), LayoutError> {
    let slice_layout = match Layout::array::<T>(count) {
        Ok(layout) => layout,
        Err(e) => return Err(e)
    };
    let (layout, offset) = match Layout::new::<Inner<T, H>>().extend(slice_layout) {
        Ok(pair) => pair,
        Err(e) => return Err(e)
    };
    debug_assert!(offset == offset_of!(Inner<T, H>, items));
    Ok((layout.pad_to_align(), offset))
}

fn slice_ptr<T, H>(ptr: *mut Inner<T, H>) -> *mut T {
    ptr.wrapping_byte_add(offset_of!(Inner<T, H>, items)).cast::<T>()
}

pub struct ThinList<T, H=()>(NonNull<Inner<T, H>>);

impl<T: Copy, H> ThinList<T, H> {
    pub fn from_header_fill_copy(header: H, fill: T, count: usize) -> Self {
        let (layout, _) = layout_for_inner::<T, H>(count).unwrap();
        let ptr = unsafe {
            let ptr = alloc(layout).cast::<Inner<T, H>>();
            if ptr.is_null() {
                handle_alloc_error(layout)
            }
            (&raw mut (*ptr).header).write(header);
            (&raw mut (*ptr).count).write(count);

            let slice_ptr = slice_ptr(ptr);
            
            for i in 0..count {
                slice_ptr.wrapping_add(i).write(fill);
            }
            NonNull::new_unchecked(ptr)
        };
        Self(ptr)
    }
}

impl<T, H> ThinList<T, H> {
    pub fn header(&self) -> &H {
        unsafe { &self.0.as_ref().header }
    }
    
    pub fn header_mut(&mut self) -> &mut H {
        unsafe { &mut self.0.as_mut().header }
    }
    
    pub fn into_header(self) -> H {
        unsafe {
            let ptr = self.0.as_ptr();
            forget(self);
            
            let header = (&raw mut (*ptr).header).read();
            let count = (*ptr).count;

            let slice_ptr = slice_ptr(ptr);
            for i in 0..count {
                slice_ptr.wrapping_add(i).drop_in_place();
            }

            dealloc(ptr.cast(), layout_for_inner::<T, H>(count).unwrap().0);
            
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

    pub fn slice_mut(&self) -> &mut [T] {
        unsafe { &mut *self.slice_raw() }
    }
}

impl<T, H> Drop for ThinList<T, H> {
    fn drop(&mut self) {
        unsafe {
            let ptr = self.0.as_ptr();
            (&raw mut (*ptr).header).drop_in_place();
            let count = (*ptr).count;
            
            let slice_ptr = slice_ptr(ptr);
            for i in 0..count {
                slice_ptr.wrapping_add(i).drop_in_place();
            }
            
            dealloc(ptr.cast(), layout_for_inner::<T, H>(count).unwrap().0)
        }
    }
}