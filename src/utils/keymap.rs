use std::hash::Hash;
use std::fmt::Debug;
use std::marker::PhantomData;
use std::ops::{Index, IndexMut};

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct KeyData(u32);

impl KeyData {
    pub fn to_u32(&self) -> u32 { self.0 } 
}

pub trait KeyMapKey : From<KeyData> + Copy + Clone + Hash + Eq + Debug {
    fn data(&self) -> KeyData;
}

macro_rules! declare_key_type {
    { $($vis:vis struct $name:ident;)* }  => {
        $(
            #[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
            $vis struct $name($crate::utils::keymap::KeyData);

            impl ::std::convert::From<$crate::utils::keymap::KeyData> for $name {
                fn from(value: $crate::utils::keymap::KeyData) -> Self {
                    Self(value)
                }
            }

            impl $crate::utils::keymap::KeyMapKey for $name {
                #[must_use]
                fn data(&self) -> $crate::utils::keymap::KeyData { self.0 }
            }
        )*
    };
}

pub(crate) use declare_key_type;

#[derive(Clone)]
pub struct ReservableKeyMap<K: KeyMapKey, T> {
    items: Vec<Option<T>>,
    _phantom: PhantomData<K>
}

impl<K: KeyMapKey, T> ReservableKeyMap<K, T> {
    pub fn new() -> Self {
        ReservableKeyMap { items: Vec::new(), _phantom: PhantomData }
    }

    pub fn add(&mut self, value: T) -> K {
        self.add_with(|_| value)
    }
    
    pub fn add_with(&mut self, generator: impl FnOnce(K) -> T) -> K {
        let index = self.items.len();
        let key = K::from(KeyData(index as u32));
        let value = generator(key);
        self.items.push(Some(value));
        key
    }

    pub fn contains_key(&self, key: K) -> bool {
        self.items.get(key.data().0 as usize).is_some()
    }

    pub fn is_initialized(&self, key: K) -> bool {
        self.items.get(key.data().0 as usize).unwrap().is_some()
    }

    pub fn insert(&mut self, key: K, value: T) -> Option<T> {
        self.items[key.data().0 as usize].replace(value)
    }

    pub fn reserve(&mut self) -> K {
        let index = self.items.len();
        self.items.push(None);
        K::from(KeyData(index as u32))
    }

    pub fn get(&self, key: K) -> Option<&T> {
        self.items.get(key.data().0 as usize).map(Option::as_ref).flatten()
    }

    pub fn values(&self) -> impl Iterator<Item=&T> {
        self.items.iter().filter_map(|item| item.as_ref())
    }

    pub fn iter(&self) -> impl Iterator<Item=(K, &T)> {
        self.items.iter()
            .enumerate()
            .filter_map(|(index, item)| item.as_ref().map(|item| (K::from(KeyData(index as u32)), item)))
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item=(K, &mut T)> {
        self.items.iter_mut()
            .enumerate()
            .filter_map(|(index, item)| item.as_mut().map(|item| (K::from(KeyData(index as u32)), item)))
    }
}

impl<K: KeyMapKey, T> Index<K> for ReservableKeyMap<K, T> {
    type Output = T;

    fn index(&self, index: K) -> &T {
        self.items[index.data().0 as usize].as_ref().unwrap()
    }
}

impl<K: KeyMapKey, T> IndexMut<K> for ReservableKeyMap<K, T> {
    fn index_mut(&mut self, index: K) -> &mut T {
        self.items[index.data().0 as usize].as_mut().unwrap()
    }
}
