use std::collections::HashMap;
use crate::id::AstId;

pub type AstTable<T> = HashMap<AstId, T>;