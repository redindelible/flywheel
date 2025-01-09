use std::hash::Hash;

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub struct ShardedKeyData<S: KeyMapKey> {
    shard: S,
    index: u32
}

pub unsafe trait ShardedKey: From<ShardedKeyData<Self::Shard>> + Copy + Eq + Hash {
    type Shard: KeyMapKey;
    const SENTINELS: u32;
    
    fn data(&self) -> ShardedKeyData<Self::Shard>;

    fn sentinel(shard_id: Self::Shard, which: u32) -> Self {
        assert!(which < Self::SENTINELS);
        Self::from(ShardedKeyData { shard: shard_id, index: which })
    }
}

#[macro_export]
macro_rules! declare_sharded_key {
    ( $($vis:vis struct $name:ident($shard:ty, $sentinels:literal);)* ) => {
        $(
            #[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
            $vis struct $name($crate::utils::ShardedKeyData<$shard>);

            impl ::std::convert::From<$crate::utils::ShardedKeyData<$shard>> for $name {
                fn from(value: $crate::utils::ShardedKeyData<$shard>) -> Self {
                    Self(value)
                }
            }

            unsafe impl $crate::utils::ShardedKey for $name {
                type Shard = $shard;
                const SENTINELS: u32 = $sentinels;
                
                #[must_use]
                fn data(&self) -> $crate::utils::ShardedKeyData<$shard> { self.0 }
            }
        )*
    };
}

pub use declare_sharded_key;
use crate::utils::KeyMapKey;


pub struct MapShard<K: ShardedKey, V> {
    shard_id: K::Shard,
    items: Vec<V>
}

impl<K: ShardedKey, V> MapShard<K, V> {
    pub fn new(shard_id: K::Shard, sentinels: impl IntoIterator<Item=V>) -> Self {
        let items: Vec<V> = sentinels.into_iter().collect();
        assert_eq!(items.len(), K::SENTINELS as usize);
        MapShard {
            shard_id,
            items
        }
    }
    
    pub fn insert(&mut self, item: V) -> K {
        let key = K::from(ShardedKeyData { shard: self.shard_id, index: self.items.len() as u32 });
        self.items.push(item);
        key
    }
}


// pub struct ShardedMap<K: ShardedKey, V> {
//     shards: RwLock<HashMap<K::Shard, Arc<Shard<K, V>>>>
// }
// 
// pub struct ShardBuilder<K: ShardedKey, V> {
//     shard_id: K::Shard,
//     items: Vec<V>
// }
// 
// pub struct Shard<K: ShardedKey, V> {
//     shard_id: K::Shard,
//     items: Box<[V]>
// }
// 
// impl<K: ShardedKey, V> ShardedMap<K, V> {
//     pub fn new() -> Self {
//         ShardedMap {
//             shards: RwLock::new(HashMap::new())
//         }
//     }
// 
//     pub fn new_shard(&self, shard_id: K::Shard, build: impl for<'a> FnOnce(&'a mut ShardBuilder<K, V>)) {
//         let mut builder = ShardBuilder { shard_id, items: Vec::new() };
//         build(&mut builder);
//         let shard = Arc::new(Shard { shard_id, items: builder.items.into_boxed_slice() });
//         self.shards.write().insert(shard_id, shard);
//     }
// 
//     pub fn get_shard<'a>(&'a self, id: K::Shard) -> Option<&'a Shard<K, V>> {
//         let read_guard = self.shards.read();
//         let shard = &*read_guard.get(&id)?;
//         Some(unsafe { std::mem::transmute::<&Shard<K, V>, &'a Shard<K, V>>(shard) })
//     }
// }
// 
// impl<K: ShardedKey, V> Index<K> for ShardedMap<K, V> {
//     type Output = V;
// 
//     fn index(&self, index: K) -> &Self::Output {
//         &self.get_shard(index.data().shard).unwrap().items[index.data().index as usize]
//     }
// }
// 
// impl<K: ShardedKey, V> ShardBuilder<K, V> {
//     pub fn insert(&mut self, value: V) -> K {
//         let key = K::from(ShardedKeyData { shard: self.shard_id, index: self.items.len() as u32 });
//         self.items.push(value);
//         key
//     }
// }
// 
// impl<K: ShardedKey, V> Shard<K, V> {
//     pub fn get(&self, key: K) -> Option<&V> {
//         if self.shard_id != key.data().shard {
//             panic!();
//         }
//         self.items.get(key.data().index as usize)
//     }
//     
//     pub fn iter(&self) -> impl Iterator<Item=(K, &V)> + '_ {
//         self.items.iter().enumerate().map(|(index, item)| {
//             (K::from(ShardedKeyData { shard: self.shard_id, index: index as u32}), item)
//         })
//     }
// }
