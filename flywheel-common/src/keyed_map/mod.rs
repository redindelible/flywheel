use std::fmt::Debug;
use std::hash::Hash;
use std::num::NonZero;

pub mod reservable;

pub use reservable::*;

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct KeyData(NonZero<u32>);

pub trait KeyMapKey: From<KeyData> + Copy + Clone + Hash + Eq + Debug {
    fn data(&self) -> KeyData;
}

#[macro_export]
macro_rules! declare_key_type {
    { $($vis:vis struct $name:ident;)* }  => {
        $(
            #[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
            $vis struct $name($crate::KeyData);

            impl ::std::convert::From<$crate::KeyData> for $name {
                fn from(value: $crate::KeyData) -> Self {
                    Self(value)
                }
            }

            impl $crate::KeyMapKey for $name {
                #[must_use]
                fn data(&self) -> $crate::KeyData { self.0 }
            }
        )*
    };
}

pub use declare_key_type;
