use std::ptr::NonNull;
use bytemuck::Zeroable;

#[cfg(not(target_pointer_width = "64"))]
compile_error!("Currently only 64-bit pointers are supported");

/// Stores a number of different types of values in a space efficient representation.
///
/// A `Value` can store any [`f64`] value, a [`GCRef`], an [`i32`], or a [`u32`].
///
/// # Representation
/// The small footprint of a `Value` is accomplished by exploiting the excess bits in the
/// representation of a NaN, as well as the fact that pointers in 64-bit systems as actually just 48
/// bits with the upper 16 bits zeroed out for user allocated memory.
///
/// Layout of a 64-bit IEEE-754 floating point number:
///
///     (S)(-----E-----)(--------------------------M-------------------------)
///     S: Sign field (1 bit)
///     E: Exponent field (11 bits)
///     M: Mantissa (fraction) field (52 bits)
///
/// A NaN is defined by the exponent field being all 1's and the mantissa being
/// nonzero. This means that there is a lot of space in a NaN to store information. As long as we
/// set the most significant bit of the mantissa to 1 to guarantee that the mantissa is non-zero,
/// the other 51 bits can be used to store a payload. We also need to distinguish "true" NaNs
/// (NaNs created by floating point operations like 1/0) from payload NaNs which we are using to
/// store other data instead of being true floating point numbers. We can do this by canonicalizing
/// true NaNs with a sign field set to 0, while payload NaNs have a sign field set to 1. Crucially,
/// this means that all payload NaNs will have their top 13 bits set to 1, while all actual floating
/// point numbers will have some other bit pattern there.
///
/// The representation of a `Value` actually uses the bitwise inverse of the pattern described
/// above. That is, if the top 13 bits are zero, then this `Value` has a payload. Otherwise, it's
/// a floating point number which can be recovered by simply inverting all the bits of the `Value`.
/// This has the advantage that for payload `Value`s, the top 13 bits do not need to be masked out
/// since they're already zero. We can use the 51 remaining bits of the `Value` as-is.
///
/// Due to the way that 64-bit machines and operating systems are implemented, we can assume that
/// any memory allocated by a user process will live in the bottom half of the virtual address
/// space (kernel memory can be in the upper half, however). In fact, we can assume even more than
/// that: the top 16 bits of the address will be zero since no modern machines have support for
/// addressing the full range of bytes afforded by a 64 bit pointer. The upshot of all of this is
/// that we can assume that all pointers to memory allocated by the program will only need the lower
/// order 48 bits. Furthermore, we know that all [`GCRef`]s will be 8 byte aligned since the
/// smallest amount of memory the VM's GC will allocate is 8 bytes. Thus, the lowest 3 bits will
/// always be 0 for a [`GCRef`].
///
/// This opens up the lower 3 bits for use as a tag of sorts. If those bits are 000, we know we can
/// use the `Value` as a [`GCRef`] as-is. Otherwise, the specific value of the tag tells us how
/// to interpret the 48 remaining payload bits.
#[derive(Copy, Clone, Debug, Zeroable)]
pub struct Value(*mut ());

#[derive(Copy, Clone, Debug)]
pub enum UnwrappedValue {
    None,
    Bool(bool),
    Float(f64),
    Pointer(NonNull<()>),
    Integer(i32),
}

const CANONICAL_NAN: f64 = f64::from_bits(0x7FF8000000000000);

impl Value {
    const fn from_data(data: usize) -> Value {
        Value(std::ptr::without_provenance_mut(data))
    }

    #[inline(always)]
    pub const fn new_none() -> Value {
        Value::from_data(0)
    }

    #[inline(always)]
    pub const fn from_float(num: f64) -> Value {
        let mut bits = !num.to_bits();
        if bits >> 51 == 0 {
            bits = !CANONICAL_NAN.to_bits();
        }
        Value::from_data(bits as usize)
    }

    #[inline(always)]
    pub fn from_gc(ptr: NonNull<()>) -> Value {
        let numeric = ptr.as_ptr().addr();
        assert_eq!(numeric >> 51, 0);
        assert_eq!(numeric & 0b111, 0);
        assert_ne!(numeric, 0);
        Value(ptr.as_ptr())
    }

    #[inline(always)]
    pub fn from_i32(num: i32) -> Value {
        Value::from_data(((num as u32 as usize) << 3) | 0b001)
    }

    #[inline(always)]
    pub fn from_bool(value: bool) -> Value {
        Value::from_data(if value { 0b011 } else { 0b010 })
    }

    pub fn unwrap(self) -> UnwrappedValue {
        let numeric = self.0.addr();
        if numeric >> 51 == 0 {
            match numeric & 0b111 {
                0b000 => {
                    if let Some(ptr) = NonNull::new(self.0) {
                        UnwrappedValue::Pointer(ptr)
                    } else {
                        UnwrappedValue::None
                    }
                }
                0b001 => UnwrappedValue::Integer((numeric >> 3) as i32),
                0b010 => UnwrappedValue::Bool(false),
                0b011 => UnwrappedValue::Bool(true),
                0b100..=0b111 => {
                    todo!()
                }
                _ => unreachable!(),
            }
        } else {
            UnwrappedValue::Float(f64::from_bits(!numeric as u64))
        }
    }
    
    pub unsafe fn as_int_unchecked(self) -> i32 {
        // match self.unwrap() {
        //     UnwrappedValue::Integer(num) => num,
        //     _ => unsafe { unreachable_unchecked() }
        // }
        (self.0.addr() >> 3) as i32
    }

    pub unsafe fn as_bool_unchecked(self) -> bool {
        // match self.unwrap() {
        //     UnwrappedValue::Bool(value) => value,
        //     _ => unsafe { unreachable_unchecked() }
        // }
        self.0.addr() & 1 == 1
    }
}
