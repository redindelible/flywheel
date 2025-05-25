use crate::id::AstId;

pub trait HasId {
    fn id(&self) -> AstId;
}