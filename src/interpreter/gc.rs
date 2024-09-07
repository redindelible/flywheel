use std::cell::Cell;
use std::collections::HashMap;
use std::num::NonZeroUsize;
use std::ptr::NonNull;

pub struct GC {
    all: Option<GCRef>,
    gray: Option<GCRef>,
    sweep: *mut Option<GCRef>,
    step: GCStep
}

enum GCStep {
    Wait,
    Mark,
    Sweep
}

struct GCHeader {
    next_gc: Option<GCRef>,
    next_list: Option<GCRef>,
    mark: Cell<u8>,
    ty: Cell<GCObjectType>
}

#[repr(u8)]
#[derive(Copy, Clone)]
enum GCObjectType {
    Dictionary = 0
}

enum GCObjectPtr {
    Dictionary(NonNull<GCDictionaryObject>)
}

enum GCObjectRef<'a> {
    Dictionary(&'a GCDictionaryObject)
}

#[repr(C)]
struct GCDictionaryObject {
    header: GCHeader,
    items: HashMap<String, GCRef>
}

#[derive(Copy, Clone)]
pub struct GCRef(pub(crate) NonNull<GCHeader>);

#[derive(Copy, Clone)]
pub struct GCDictionaryRef(NonNull<GCDictionaryObject>);

impl GCDictionaryRef {
    fn as_gcref(&self) -> GCRef {
        GCRef(self.0.cast())
    }
}


impl GCRef {
    pub fn addr(&self) -> NonZeroUsize {
        unsafe { NonZeroUsize::new_unchecked(self.0.as_ptr() as usize) }
    }

    pub fn from_pointer(pointer: NonNull<()>) -> Self {
        GCRef(pointer.cast())
    }

    pub unsafe fn get_mark(&self) -> u8 {
        self.0.as_ref().mark.get()
    }

    pub unsafe fn set_mark(&self, value: u8) {
        self.0.as_ref().mark.set(value)
    }

    pub unsafe fn ty(&self) -> GCObjectType {
        self.0.as_ref().ty.get()
    }
}

impl GC {
    unsafe fn downcast_ptr(&self, gc_ref: GCRef) -> GCObjectPtr {
        match gc_ref.ty() {
            GCObjectType::Dictionary => GCObjectPtr::Dictionary(gc_ref.0.cast::<GCDictionaryObject>())
        }
    }

    unsafe fn downcast_ref(&self, gc_ref: GCRef) -> GCObjectRef {
        match gc_ref.ty() {
            GCObjectType::Dictionary => GCObjectRef::Dictionary(unsafe { gc_ref.0.cast::<GCDictionaryObject>().as_ref() })
        }
    }

    pub fn allocate_dictionary(&mut self) -> GCDictionaryRef {
        let pointer = GCDictionaryRef(Box::leak(Box::new(GCDictionaryObject {
            header: GCHeader {
                next_gc: self.all,
                next_list: None,
                mark: Cell::new(self.current_white()),
                ty: Cell::new(GCObjectType::Dictionary)
            },
            items: HashMap::new()
        })).into());
        self.all = Some(pointer.as_gcref());
        unsafe { self.mark(pointer.as_gcref()) };
        pointer
    }

    fn current_white(&self) -> u8 {
        0
    }

    fn current_gray(&self) -> u8 {
        1
    }

    unsafe fn mark(&mut self, mut gc_ref: GCRef) {
        if gc_ref.get_mark() == self.current_white() {
            gc_ref.set_mark(self.current_gray());
            gc_ref.0.as_mut().next_list = self.gray;
            self.gray = Some(gc_ref);
        }
    }

    unsafe fn propagate(&mut self, next_gray: GCRef) {
        match self.downcast_ptr(next_gray) {
            GCObjectPtr::Dictionary(as_dict) => {
                for gc_ref in as_dict.as_ref().items.values() {
                    self.mark(*gc_ref);
                }
            }
        }
    }

    unsafe fn sweep(&mut self, object: GCRef) {
        match self.downcast_ptr(object) {
            GCObjectPtr::Dictionary(as_dict) => {
                drop(Box::from_raw(as_dict.as_ptr()));
            }
        }
    }

    pub fn step_once(&mut self) {
        match self.step {
            GCStep::Wait => {

            }
            GCStep::Mark => {
                if let Some(mut next_gray) = self.gray {
                    self.gray = unsafe { next_gray.0.as_mut() }.next_list.take();
                    unsafe { self.propagate(next_gray) };
                } else {
                    self.sweep = &mut self.all;
                    self.step = GCStep::Sweep;
                }
            }
            GCStep::Sweep => {
                if let Some(to_sweep) = unsafe { &mut *self.sweep } {
                    if unsafe { to_sweep.get_mark() } == self.current_white() {
                        unsafe { *self.sweep = to_sweep.0.as_ref().next_gc };
                        unsafe { self.sweep(*to_sweep) };
                    } else {
                        self.sweep = unsafe { &mut to_sweep.0.as_mut().next_gc };
                    }
                } else {
                    self.step = GCStep::Wait;
                }
            }
        }
    }
}