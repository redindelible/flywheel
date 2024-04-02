
use std::hash::Hash;
use std::fmt::Debug;
use std::marker::PhantomData;
use std::ops::{Index, IndexMut};

#[repr(transparent)]
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct KeyData(usize);

pub trait KeyMapKey : From<KeyData> + Copy + Clone + Hash + Eq + Debug {
    fn data(&self) -> KeyData;
}

macro_rules! declare_key_type {
    { $($vis:vis struct $name:ident;)* }  => {
        $(
            #[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
            #[repr(transparent)]
            $vis struct $name($crate::keymap::KeyData);

            impl ::std::convert::From<$crate::keymap::KeyData> for $name {
                fn from(value: $crate::keymap::KeyData) -> Self {
                    Self(value)
                }
            }

            impl $crate::keymap::KeyMapKey for $name {
                #[must_use]
                fn data(&self) -> $crate::keymap::KeyData { self.0 }
            }
        )*
    };
}

pub(crate) use declare_key_type;

#[derive(Clone)]
pub struct KeyMap<K: KeyMapKey, T> {
    items: Vec<Option<T>>,
    _phantom: PhantomData<K>
}

impl<K: KeyMapKey, T> KeyMap<K, T> {
    pub fn new() -> Self {
        KeyMap { items: Vec::new(), _phantom: PhantomData }
    }

    pub fn add(&mut self, value: T) -> K {
        let index = self.items.len();
        self.items.push(Some(value));
        K::from(KeyData(index))
    }

    pub fn contains_key(&self, key: K) -> bool {
        self.items.get(key.data().0).is_some()
    }

    pub fn is_initialized(&self, key: K) -> bool {
        self.items.get(key.data().0).unwrap().is_some()
    }

    pub fn insert(&mut self, key: K, value: T) -> Option<T> {
        self.items[key.data().0].replace(value)
    }

    pub fn reserve(&mut self) -> K {
        let index = self.items.len();
        self.items.push(None);
        K::from(KeyData(index))
    }

    pub fn get(&self, key: K) -> Option<&T> {
        self.items.get(key.data().0).map(Option::as_ref).flatten()
    }

    pub fn values(&self) -> impl Iterator<Item=&T> {
        self.items.iter().filter_map(|item| item.as_ref())
    }

    pub fn iter(&self) -> impl Iterator<Item=(K, &T)> {
        self.items.iter()
            .enumerate()
            .filter_map(|(index, item)| item.as_ref().map(|item| (K::from(KeyData(index)), item)))
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item=(K, &mut T)> {
        self.items.iter_mut()
            .enumerate()
            .filter_map(|(index, item)| item.as_mut().map(|item| (K::from(KeyData(index)), item)))
    }

    // pub fn map<O>(self, mut f: impl FnMut(K, T) -> O) -> KeyMap<K, O> {
    //     let mut new_items = IndexMap::with_capacity(self.items.len());
    //     for (key, value) in self.items {
    //         new_items.insert(key, f(key, value));
    //     }
    //     KeyMap { next_key: self.next_key, items: new_items }
    // }
}

impl<K: KeyMapKey, T> Index<K> for KeyMap<K, T> {
    type Output = T;

    fn index(&self, index: K) -> &T {
        self.items[index.data().0].as_ref().unwrap()
    }
}

impl<K: KeyMapKey, T> IndexMut<K> for KeyMap<K, T> {
    fn index_mut(&mut self, index: K) -> &mut T {
        self.items[index.data().0].as_mut().unwrap()
    }
}


// impl<K: KeyMapKey, T> IntoIterator for KeyMap<K, T> {
//     type Item = (K, T);
//     type IntoIter = <IndexMap<K, T> as IntoIterator>::IntoIter;
//
//     fn into_iter(self) -> Self::IntoIter {
//         self.items.into_iter()
//     }
// }
//
// impl<'a, K: KeyMapKey, T> IntoIterator for &'a KeyMap<K, T> {
//     type Item = (K, &'a T);
//     type IntoIter = Iter<'a, K, T>;
//
//     fn into_iter(self) -> Self::IntoIter {
//         Iter { items: self.items.iter() }
//     }
// }
//
// pub struct Iter<'a, K: KeyMapKey, T> {
//     items: <&'a IndexMap<K, T> as IntoIterator>::IntoIter
// }
//
// impl<'a, K: KeyMapKey, T> Iterator for Iter<'a, K, T> {
//     type Item = (K, &'a T);
//
//     fn next(&mut self) -> Option<(K, &'a T)> {
//         self.items.next().map(|(k, t)| (*k, t))
//     }
// }
