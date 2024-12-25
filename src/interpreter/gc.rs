use std::collections::HashMap;
use std::ptr::NonNull;

#[derive(Copy, Clone)]
pub struct GcRef(NonNull<GcHeader>);

impl GcRef {
    pub(crate) fn as_ptr(&self) -> NonNull<()> {
        self.0.cast()
    }

    pub(crate) fn from_ptr(ptr: NonNull<()>) -> GcRef {
        GcRef(ptr.cast())
    }

    unsafe fn as_mut<'a>(self) -> &'a mut GcHeader {
        &mut *self.0.as_ptr()
    }

    unsafe fn downcast<'a>(self) -> DowncastObject<'a> {
        match self.as_mut().ty {
            GcType::Dictionary => {
                DowncastObject::Dictionary(self.0.cast::<GcDictionaryObject>().as_mut())
            }
        }
    }
}

enum DowncastObject<'a> {
    Dictionary(&'a mut GcDictionaryObject)
}

struct GcHeader {
    mark: u8,
    ty: GcType,
    prev: Option<GcRef>,
    next: Option<GcRef>,
}

struct ObjectLinkedList {
    first: Option<GcRef>,
    last: Option<GcRef>,
}

impl ObjectLinkedList {
    fn is_empty(&self) -> bool {
        if self.first.is_none() {
            assert!(self.last.is_none());
            true
        } else {
            false
        }
    }

    fn empty() -> Self {
        ObjectLinkedList {
            first: None,
            last: None
        }
    }

    unsafe fn pop_front(&mut self) -> Option<GcRef> {
        if let Some(first_ref) = self.first {
            self.remove(first_ref);
            Some(first_ref)
        } else {
            debug_assert!(self.last.is_none());
            None
        }
    }

    unsafe fn push_front(&mut self, element: GcRef) {
        if let Some(first) = self.first {
            first.as_mut().prev = Some(element);
            element.as_mut().next = Some(first);
            element.as_mut().prev = None;
        } else {
            self.first = Some(element);
            self.last = Some(element);
            element.as_mut().prev = None;
            element.as_mut().next = None;
        }
    }

    unsafe fn remove(&mut self, element: GcRef) {
        let element = &mut *element.0.as_ptr();
        if let Some(prev) = element.prev {
            prev.as_mut().next = element.next;
        } else {
            self.first = element.next;
        }
        if let Some(next) = element.next {
            next.as_mut().prev = element.prev;
        } else {
            self.last = element.prev;
        }
    }
}

#[derive(Copy, Clone)]
#[repr(u8)]
enum GcType {
    Dictionary = 0
}

#[repr(C)]
pub struct GcDictionaryObject {
    header: GcHeader,
    items: HashMap<String, GcRef>
}

enum ColorSet {
    Flip,
    Flop
}

impl ColorSet {
    fn current_white(&self) -> u8 {
        match self {
            ColorSet::Flip => 0,
            ColorSet::Flop => 2
        }
    }

    fn current_gray(&self) -> u8 {
        1
    }

    fn current_black(&self) -> u8 {
        match self {
            ColorSet::Flip => 2,
            ColorSet::Flop => 0
        }
    }

    fn toggle(&mut self) {
        match self {
            ColorSet::Flip => *self = ColorSet::Flop,
            ColorSet::Flop => *self = ColorSet::Flip,
        }
    }
}

pub struct Gc(GcInner);

enum GcInner {
    Wait {
        all: ObjectLinkedList,
        color_set: ColorSet
    },
    Mark {
        white: ObjectLinkedList,
        gray: ObjectLinkedList,
        black: ObjectLinkedList,
        color_set: ColorSet
    },
    Sweep {
        dead: ObjectLinkedList,
        alive: ObjectLinkedList,
        color_set: ColorSet
    },
    Invalid
}

impl GcInner {
    fn allocate_dictionary(&mut self) -> (&mut GcDictionaryObject, GcRef) {
        let object = Box::leak(Box::new(GcDictionaryObject {
            header: GcHeader {
                mark: self.new_object_color(),
                ty: GcType::Dictionary,
                prev: None,
                next: None,
            },
            items: HashMap::new()
        }));
        let object_ref = GcRef::from_ptr(NonNull::new(object).unwrap().cast());

        unsafe {
            match self {
                GcInner::Wait { all, .. } => all.push_front(object_ref),
                GcInner::Mark { gray, .. } => gray.push_front(object_ref),
                GcInner::Sweep { alive, .. } => alive.push_front(object_ref),
                GcInner::Invalid => unreachable!()
            }
        }

        (object, object_ref)
    }

    fn new_object_color(&self) -> u8 {
        match self {
            GcInner::Wait { color_set, .. } => color_set.current_black(),
            GcInner::Mark { color_set, .. } => color_set.current_gray(),
            GcInner::Sweep { color_set, .. } => color_set.current_black(),
            GcInner::Invalid => unreachable!()
        }
    }

    fn step_once(&mut self, start_cycle: bool) {
        match self {
            GcInner::Wait { .. } => {
                if start_cycle {
                    let GcInner::Wait { all, mut color_set } = std::mem::replace(self, GcInner::Invalid) else { unreachable!() };
                    color_set.toggle();
                    *self = GcInner::Mark {
                        white: all,
                        gray: todo!(),
                        black: ObjectLinkedList::empty(),
                        color_set
                    };
                }
            }
            GcInner::Mark { white, gray, black, color_set } => unsafe {
                while let Some(mut next_object) = gray.pop_front() {
                    let mark = next_object.as_mut().mark;
                    if mark == color_set.current_white() {
                        unreachable!();
                    } else if mark == color_set.current_gray() {
                        match next_object.downcast() {
                            DowncastObject::Dictionary(object) => {
                                for item in object.items.values() {
                                    if item.as_mut().mark == color_set.current_white() {
                                        item.as_mut().mark = color_set.current_gray();
                                        white.remove(*item);
                                        gray.push_front(*item);
                                    }
                                }
                            }
                        }
                        next_object.as_mut().mark = color_set.current_black();
                        black.push_front(next_object);
                    } else if mark == color_set.current_black() {
                        // do nothing
                    } else {
                        unreachable!()
                    }
                }

                let GcInner::Mark { white, gray, black, color_set } = std::mem::replace(self, GcInner::Invalid) else { unreachable!() };
                assert!(gray.is_empty());
                *self = GcInner::Sweep { alive: black, dead: white, color_set };
            }
            GcInner::Sweep { alive: _, dead, color_set: _ } => unsafe {
                if let Some(object) = dead.pop_front() {
                    match object.downcast() {
                        DowncastObject::Dictionary(object) => {
                            let object = Box::from_raw(object);
                            drop(object);
                        }
                    }
                } else {
                    let GcInner::Sweep { alive, dead, color_set } = std::mem::replace(self, GcInner::Invalid) else { unreachable!() };
                    assert!(dead.is_empty());
                    *self = GcInner::Wait { all: alive, color_set };
                }
            }
            GcInner::Invalid => unreachable!()
        }
    }
}