use bytemuck::{Pod, Zeroable};

pub trait InstructionRepr: Pod {
    fn registers(&self) -> impl IntoIterator<Item=u8>;
}

pub trait Instruction: Copy + Sized {
    type Repr: InstructionRepr;

    fn to_repr(self) -> Self::Repr;
    fn from_repr(repr: Self::Repr) -> Self;

    unsafe fn from_ptr(ptr: *const u32) -> Self {
        Self::from_repr(unsafe { ptr.cast::<Self::Repr>().read() })
    }
}

#[derive(Copy, Clone, Pod, Zeroable)]
#[repr(C, align(4))]
pub struct RegImm16 {
    opcode: u8,
    r1: u8,
    imm16: u16
}

impl InstructionRepr for RegImm16 {
    fn registers(&self) -> impl IntoIterator<Item=u8> {
        [self.r1]
    }
}

#[derive(Copy, Clone, Pod, Zeroable)]
#[repr(C, align(4))]
pub struct RegOff16 {
    opcode: u8,
    r1: u8,
    off16: i16
}

impl InstructionRepr for RegOff16 {
    fn registers(&self) -> impl IntoIterator<Item=u8> {
        [self.r1]
    }
}

#[derive(Copy, Clone, Pod, Zeroable)]
#[repr(C, align(4))]
pub struct RegRegReg {
    opcode: u8,
    r1: u8,
    r2: u8,
    r3: u8,
}

impl InstructionRepr for RegRegReg {
    fn registers(&self) -> impl IntoIterator<Item=u8> {
        [self.r1, self.r2, self.r3]
    }
}

#[derive(Copy, Clone, Pod, Zeroable)]
#[repr(C, align(4))]
pub struct RegRegImm8 {
    opcode: u8,
    r1: u8,
    r2: u8,
    imm8: i8,
}


impl InstructionRepr for RegRegImm8 {
    fn registers(&self) -> impl IntoIterator<Item=u8> {
        [self.r1, self.r2]
    }
}

macro_rules! _generate_instruction {
    {$instr:ident $opcode:ident r1: $r1:ident, imm16: $imm16:ident} => {
        #[allow(non_camel_case_types)]
        #[derive(Copy, Clone)]
        pub struct $instr {
            pub $r1: u8, pub $imm16: u16
        }


        impl $crate::Instruction for $instr {
            type Repr = $crate::instr::RegImm16;

            fn to_repr(self) -> Self::Repr {
                Self::Repr { opcode: (super::$opcode::$instr).bits(), r1: self.$r1, imm16: self.$imm16 }
            }

            fn from_repr(repr: Self::Repr) -> Self {
                Self { $r1: repr.r1, $imm16: repr.imm16 }
            }
        }
    };
    
    {$instr:ident $opcode:ident r1: $r1:ident, off16: $off16:ident} => {
        #[allow(non_camel_case_types)]
        #[derive(Copy, Clone)]
        pub struct $instr {
            pub $r1: u8, pub $off16: i16
        }


        impl $crate::Instruction for $instr {
            type Repr = $crate::instr::RegOff16;

            fn to_repr(self) -> Self::Repr {
                Self::Repr { opcode: (super::$opcode::$instr).bits(), r1: self.$r1, off16: self.$off16 }
            }

            fn from_repr(repr: Self::Repr) -> Self {
                Self { $r1: repr.r1, $off16: repr.off16 }
            }
        }
    };

    {$instr:ident $opcode:ident r1: $r1:ident, r2: $r2:ident, r3: $r3:ident} => {
        #[allow(non_camel_case_types)]
        #[derive(Copy, Clone)]
        pub struct $instr {
            pub $r1: u8, pub $r2: u8, pub $r3: u8
        }

        impl $crate::Instruction for $instr {
            type Repr = $crate::instr::RegRegReg;

            fn to_repr(self) -> Self::Repr {
                Self::Repr { opcode: (super::$opcode::$instr).bits(), r1: self.$r1, r2: self.$r2, r3: self.$r3 }
            }

            fn from_repr(repr: Self::Repr) -> Self {
                Self { $r1: repr.r1, $r2: repr.r2, $r3: repr.r3 }
            }
        }
    };
    
    {$instr:ident $opcode:ident r1: $r1:ident, r2: $r2:ident, imm8: $imm8:ident} => {
        #[allow(non_camel_case_types)]
        #[derive(Copy, Clone)]
        pub struct $instr {
            pub $r1: u8, pub $r2: u8, pub $imm8: i8
        }

        impl $crate::Instruction for $instr {
            type Repr = $crate::instr::RegRegImm8;

            fn to_repr(self) -> Self::Repr {
                Self::Repr { opcode: (super::$opcode::$instr).bits(), r1: self.$r1, r2: self.$r2, imm8: self.$imm8 }
            }

            fn from_repr(repr: Self::Repr) -> Self {
                Self { $r1: repr.r1, $r2: repr.r2, $imm8: repr.imm8 }
            }
        }
    };
}


macro_rules! generate_instructions {
    { $vis:vis mod $instrs:ident(enum $opcode:ident) { $( $instr:ident { $($field:ident : $ty:ident),* $(,)? } ),* $(,)? } } => {
        $vis mod $instrs {
            $(_generate_instruction! { $instr $opcode $($field: $ty),* })*
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
        lzi16 { r1: dst, imm16: imm },              // Load integer from 16 bit immediate zero-extended to 32 bits
        iaddrr { r1: dst, r2: left, r3: right },
        imulrr { r1: dst, r2: left, r3: right },
        idivrr { r1: dst, r2: left, r3: right },
        imodrr { r1: dst, r2: left, r3: right },
        iaddri { r1: dst, r2: left, imm8: right },
        imulri { r1: dst, r2: left, imm8: right },
        idivri { r1: dst, r2: left, imm8: right },
        imodri { r1: dst, r2: left, imm8: right },
        ieqrr { r1: dst, r2: left, r3: right },
        ieqri { r1: dst, r2: left, imm8: right },
        jtr { r1: src, off16: off },
        jmp { r1: _pad, off16: off },
        ret { r1: src, imm16: _pad }
    }
}