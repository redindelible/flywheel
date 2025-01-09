use std::marker::PhantomData;
use std::num::NonZero;
use std::ops::{Index, IndexMut};

use super::{KeyData, KeyMapKey};

#[derive(Clone)]
pub struct ReservableMap<K: KeyMapKey, T> {
    items: Vec<Option<T>>,
    _phantom: PhantomData<K>
}

impl<K: KeyMapKey, T> ReservableMap<K, T> {
    pub fn new() -> Self {
        ReservableMap { items: Vec::new(), _phantom: PhantomData }
    }

    pub fn add(&mut self, value: T) -> K {
        self.add_with(|_| value)
    }
    
    pub fn add_with(&mut self, generator: impl FnOnce(K) -> T) -> K {
        let index = self.items.len();
        let key = K::from(KeyData(NonZero::new(index as u32 + 1).unwrap()));
        let value = generator(key);
        self.items.push(Some(value));
        key
    }

    pub fn contains_key(&self, key: K) -> bool {
        self.items.get(key.data().0.get() as usize - 1).is_some()
    }

    pub fn is_initialized(&self, key: K) -> bool {
        self.items.get(key.data().0.get() as usize - 1).unwrap().is_some()
    }

    pub fn insert(&mut self, key: K, value: T) -> Option<T> {
        self.items[key.data().0.get() as usize - 1].replace(value)
    }

    pub fn reserve(&mut self) -> K {
        let index = self.items.len();
        self.items.push(None);
        K::from(KeyData(NonZero::new(index as u32 + 1).unwrap()))
    }

    pub fn get(&self, key: K) -> Option<&T> {
        self.items.get(key.data().0.get() as usize - 1).map(Option::as_ref).flatten()
    }

    pub fn values(&self) -> impl Iterator<Item=&T> {
        self.items.iter().filter_map(|item| item.as_ref())
    }

    pub fn iter(&self) -> impl Iterator<Item=(K, &T)> {
        self.items.iter()
            .enumerate()
            .filter_map(|(index, item)| item.as_ref().map(|item| (K::from(KeyData(NonZero::new(index as u32 + 1).unwrap())), item)))
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item=(K, &mut T)> {
        self.items.iter_mut()
            .enumerate()
            .filter_map(|(index, item)| item.as_mut().map(|item| (K::from(KeyData(NonZero::new(index as u32 + 1).unwrap())), item)))
    }
}

impl<K: KeyMapKey, T> Index<K> for ReservableMap<K, T> {
    type Output = T;

    fn index(&self, index: K) -> &T {
        self.items[index.data().0.get() as usize - 1].as_ref().unwrap()
    }
}

impl<K: KeyMapKey, T> IndexMut<K> for ReservableMap<K, T> {
    fn index_mut(&mut self, index: K) -> &mut T {
        self.items[index.data().0.get() as usize - 1].as_mut().unwrap()
    }
}
