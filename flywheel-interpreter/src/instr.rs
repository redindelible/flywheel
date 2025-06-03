use bytemuck::{Pod, Zeroable};

pub trait Instruction<const WORDS: usize = 1>: Copy + Sized {
    fn to_bits(self) -> [u32; WORDS];
    fn from_bits(bits: [u32; WORDS]) -> Self;

    fn registers(self) -> impl Iterator<Item=usize>;

    unsafe fn from_ptr(ptr: *const u32) -> Self {
        Self::from_bits(unsafe { ptr.cast::<[u32; WORDS]>().read() })
    }
}

trait Field {
    fn registers(self) -> impl Iterator<Item=usize>;
}

pub trait RegType: Pod + Zeroable + Into<usize> { }
impl RegType for u8 { }
impl RegType for u16 { }

#[derive(Copy, Clone, Pod, Zeroable)]
#[repr(transparent)]
pub struct Reg<T>(pub T);

impl<T: RegType> Field for Reg<T> {
    fn registers(self) -> impl Iterator<Item=usize> {
        [self.0.into()].into_iter()
    }
}

impl<T: RegType> Reg<T> {
    pub fn index(self) -> usize { self.0.into() }
}

#[derive(Copy, Clone, Pod, Zeroable)]
#[repr(C, packed)]
pub struct RegRange<T> {
    start: T,
    count: T
}

impl<T: RegType> Field for RegRange<T> {
    fn registers(self) -> impl Iterator<Item=usize> {
        self.start.into()..(self.start.into() + self.count.into())
    }
}

pub trait ImmType: Pod + Zeroable { }
impl ImmType for u8 { }
impl ImmType for u16 { }
impl ImmType for i8 { }
impl ImmType for i16 { }

#[derive(Copy, Clone, Pod, Zeroable)]
#[repr(transparent)]
pub struct Imm<T>(pub T);

impl<T> Field for Imm<T> {
    fn registers(self) -> impl Iterator<Item=usize> {
        std::iter::empty()
    }
}

impl<T: ImmType + Into<i32>> Imm<T> {
    pub fn as_i32(self) -> i32 {
        self.0.into()
    }
}

impl<T: ImmType + Into<u32>> Imm<T> {
    pub fn as_u32(self) -> u32 {
        self.0.into()
    }
}


macro_rules! generate_instructions {
    { $vis:vis mod $instrs:ident(enum $opcode:ident) { $( $instr:ident <$words:literal> { $($field:ident : $ty:ty),* $(,)? } ),* $(,)? } } => {
        $vis mod $instrs {
            use super::{Reg, RegRange, Imm, Field};
        $(
            #[allow(non_camel_case_types)]
            #[derive(Copy, Clone)]
            pub struct $instr {
                $(pub $field: $ty),*
            }

            const _: () = {
                #[derive(Copy, Clone, bytemuck::NoUninit)]
                #[repr(C)]
                struct Repr(u8, $($ty),*);

                assert!(size_of::<Repr>() == size_of::<[u32; $words]>());
            };

            impl $crate::Instruction<$words> for $instr {
                fn to_bits(self) -> [u32; $words] {
                    type Repr = (u8, $($ty),*);
                    let value = ((super::$opcode::$instr).bits(), $(self.$field),*);
                    unsafe { std::mem::transmute::<Repr, _>(value) }
                }

                fn from_bits(bits: [u32; $words]) -> Self {
                    type Repr = (u8, $($ty),*);
                    let (_, $($field),*) = unsafe { std::mem::transmute::<_, Repr>(bits) };
                    Self { $($field),* }
                }

                fn registers(self) -> impl Iterator<Item=usize> {
                    std::iter::empty()$(.chain(self.$field.registers()))*
                }
            }
        )*
        }

        #[allow(non_camel_case_types)]
        #[derive(Copy, Clone)]
        #[repr(u8)]
        $vis enum $opcode {
            $($instr),*
        }

        impl $opcode {
            pub fn bits(self) -> u8 {
                self as u8
            }
        }
    };
}

generate_instructions! {
    pub mod instrs(enum InstructionCode) {
        lzi16<1> { dst: Reg<u8>, imm: Imm<u16> },              // Load integer from 16 bit immediate zero-extended to 32 bits
        iaddrr<1> { dst: Reg<u8>, left: Reg<u8>, right: Reg<u8> },
        imulrr<1> { dst: Reg<u8>, left: Reg<u8>, right: Reg<u8> },
        idivrr<1> { dst: Reg<u8>, left: Reg<u8>, right: Reg<u8> },
        imodrr<1> { dst: Reg<u8>, left: Reg<u8>, right: Reg<u8> },
        iaddri<1> { dst: Reg<u8>, left: Reg<u8>, right: Imm<u8> },
        imulri<1> { dst: Reg<u8>, left: Reg<u8>, right: Imm<u8> },
        idivri<1> { dst: Reg<u8>, left: Reg<u8>, right: Imm<u8> },
        imodri<1> { dst: Reg<u8>, left: Reg<u8>, right: Imm<u8> },
        ieqrr<1> { dst: Reg<u8>, left: Reg<u8>, right: Reg<u8> },
        ieqri<1> { dst: Reg<u8>, left: Reg<u8>, right: Imm<u8> },
        jtr<1> { src: Reg<u8>, off: Imm<i16> },
        jmp<1> { _pad: Reg<u8>, off: Imm<i16> },
        ret<1> { src: Reg<u8>, _pad: Imm<u16> }
    }
}