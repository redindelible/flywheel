use std::collections::HashMap;

use arcstr::ArcStr;
use enum_map::Enum;
use strum::{EnumDiscriminants, VariantArray};
use triomphe::ThinArc;

#[derive(Debug)]
pub struct Module {
    pub globals: Vec<Global>,
}

#[derive(Debug)]
pub enum Global {
    Function(Function),
}

#[derive(Debug)]
pub struct Function {
    pub name: ArcStr,
    pub parameters: Vec<Type>,
    pub return_type: Type,
    pub blocks: HashMap<BlockID, Block>,
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub struct BlockID(pub u32);

#[derive(Debug)]
pub struct Block {
    pub retained_locals: Vec<Type>,
    pub new_locals: Vec<Type>,
    pub instructions: Vec<Instruction>,
    pub terminator: Terminator,
}

#[derive(EnumDiscriminants, Debug)]
#[strum_discriminants(name(InstructionKind))]
#[strum_discriminants(derive(Enum, VariantArray))]
pub enum Instruction {
    LoadConst { name: ArcStr },
    LoadLocal { index: u32 },
    StoreLocal { index: u32 },
    LoadUnit,
    LoadInteger(i64),
    LoadFloat(f64),
    Upcast { to_ty: Type },
    Call { arguments: u32 },
    LessEqual,
    Equal,
    Add,
    Mul,
    Div,
    Mod,
}

#[derive(EnumDiscriminants, Debug)]
#[strum_discriminants(name(TerminatorKind))]
#[strum_discriminants(derive(Enum, VariantArray))]
pub enum Terminator {
    Jump { target: BlockID },
    Loop { target: BlockID },
    IfElse { true_target: BlockID, false_target: BlockID },
    Return,
}

#[derive(Clone, Debug)]
pub enum Type {
    Unit,
    Integer,
    Float,
    Generic { slot: u32 },
    Name(ArcStr),
    Tuple(TupleType),
    Function(FunctionType),
}

#[derive(Clone, Debug)]
pub struct TupleType(ThinArc<(), Type>);

impl TupleType {
    pub fn new<I>(items: I) -> Self
    where
        I: IntoIterator,
        I::IntoIter: ExactSizeIterator<Item = Type>,
    {
        Self(ThinArc::from_header_and_iter((), items.into_iter()))
    }

    pub fn items(&self) -> &[Type] {
        &self.0.slice
    }
}

impl From<TupleType> for Type {
    fn from(value: TupleType) -> Self {
        Type::Tuple(value)
    }
}

#[derive(Clone, Debug)]
pub struct FunctionType(ThinArc<Type, Type>);

impl FunctionType {
    fn new<I: ExactSizeIterator<Item = Type>>(ret: Type, params: impl IntoIterator<IntoIter = I>) -> Self {
        Self(ThinArc::from_header_and_iter(ret, params.into_iter()))
    }

    fn ret(&self) -> &Type {
        &self.0.header.header
    }

    fn params(&self) -> &[Type] {
        &self.0.slice
    }
}

impl From<FunctionType> for Type {
    fn from(value: FunctionType) -> Self {
        Type::Function(value)
    }
}
