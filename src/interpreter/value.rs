use std::hint::unreachable_unchecked;
use std::num::NonZeroU64;
use std::ptr::NonNull;
use crate::interpreter::gc::GCRef;

/// Stores a number of different types of values in a space efficient representation.
///
/// A `Value` can store any [`f64`] value, a [`GCRef`], an [`i32`], or a [`u32`]. Additionally, its
/// bit representation is guaranteed to never be equal to 0, so `Option<Value>` is the same size as
/// a `Value`.
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
/// to interpret the 48 remaining payload bits. For example, a tag of 001 denotes a signed 32 bit
/// integer, and a tag of 010 denotes an unsigned 32 bit integer.
#[derive(Copy, Clone, Debug, Default)]
struct Value(u64);

enum UnwrappedValue {
    None,
    Float(f64),
    Pointer(GCRef),
    SignedInteger(i32),
    UnsignedInteger(u32)
}

impl Value {
    fn new_none() -> Value {
        Value(0)
    }

    // fn from_u64(num: NonZeroU64) -> Value {
    //     Value(num)
    // }

    fn from_float(num: f64) -> Value {
        let num = if num.is_nan() {
            f64::from_bits(0x7FF8000000000000)
        } else {
            num
        };
        Value(unsafe { !num.to_bits() })
    }

    fn from_gc(ptr: GCRef) -> Value {
        let numeric = ptr.addr().get() as u64;
        assert_eq!(numeric >> 51, 0);
        assert_eq!(numeric & 0b111, 0);
        assert_ne!(numeric, 0);
        Value(numeric)
    }

    fn from_i32(num: i32) -> Value {
        Value(((num as u32 as u64) << 3) | 0b001)
    }

    fn from_u32(num: i32) -> Value {
        Value(((num as u32 as u64) << 3) | 0b010)
    }

    fn unwrap(&self) -> UnwrappedValue {
        let numeric = self.0;
        if numeric >> 51 == 0 {
            match numeric & 0b111 {
                0 => {
                    if let Some(ptr) = NonNull::new(numeric as *mut ()) {
                        UnwrappedValue::Pointer(GCRef::from_pointer(ptr))
                    } else {
                        UnwrappedValue::None
                    }
                }
                1 => {
                    UnwrappedValue::SignedInteger((numeric >> 3) as i32)
                }
                2 => {
                    UnwrappedValue::UnsignedInteger((numeric >> 3) as u32)
                }
                3..=7 => {
                    todo!()
                }
                _ => unsafe { unreachable_unchecked() }
            }
        } else {
            UnwrappedValue::Float(f64::from_bits(!numeric))
        }
    }
}

const fn mask(size: u8) -> u64 {
    (1u64 << size) - 1
}

const fn bextr(bits: u64, lower: u8, num: u8) -> u64 {
    (bits >> lower) & mask(num)
}

const fn f64_mantissa_bits(bits: u64) -> u64 {
    bextr(bits, 0, 52)
}

const fn f64_exponent_bits(bits: u64) -> u64 {
    bextr(bits, 52, 11)
}

const fn f64_sign_bit(bits: u64) -> u64 {
    bextr(bits, 63, 1)
}