pub mod beacon;
pub mod has_id;
pub type AstId = usize;
pub fn from_pointer<T>(from: &Box<T>) -> AstId {
    assert_ne!(size_of::<T>(), 0, "{} is not sized, so we can't turn a pointer from it into an id", core::any::type_name::<T>());
    from.as_ref() as *const T as usize
}