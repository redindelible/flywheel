use std::borrow::Cow;

use num_enum::{IntoPrimitive, TryFromPrimitive};

pub struct Function<'b, 'c> {
    required_stack: u32,
    bytecode: Cow<'b, [Instruction]>,
    constants: Cow<'c, [u8]>,
}

impl<'b, 'c> Function<'b, 'c> {
    pub fn required_stack(&self) -> u32 {
        self.required_stack
    }

    pub fn bytecode(&self) -> &[Instruction] {
        &self.bytecode
    }

    pub fn constants(&self) -> &[u8] {
        &self.constants
    }
}

#[allow(non_camel_case_types)]
#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct u24([u8; 3]);

impl u24 {
    fn as_u32(&self) -> u32 {
        let [a, b, c] = self.0;
        u32::from_le_bytes([a, b, c, 0])
    }

    fn from_le_bytes(bytes: [u8; 3]) -> Self {
        u24(bytes)
    }

    fn to_le_bytes(&self) -> [u8; 3] {
        self.0
    }
}

#[allow(non_camel_case_types)]
#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct i24([u8; 3]);

impl i24 {
    pub fn to_i32(self) -> i32 {
        let [a, b, c] = self.0;
        i32::from_le_bytes([0, a, b, c]) >> 8
    }

    pub fn to_isize(self) -> isize {
        self.to_i32() as isize
    }

    fn from_le_bytes(bytes: [u8; 3]) -> Self {
        i24(bytes)
    }

    fn to_le_bytes(self) -> [u8; 3] {
        self.0
    }
}

macro_rules! instructions {
    {$vis:vis enum $instr_enum_name:ident ($tag_vis:vis $instr_tag_name:ident) { $( $name:ident { $($field_name:ident : $ty:ty),* $(,)? } = $value:expr ),* $(,)? }} => {
        #[derive(Copy, Clone)]
        #[repr(u8)]
        $vis enum $instr_enum_name {
            $($name { $($field_name : $ty),* } = $instr_tag_name::$name as u8 ),*
        }

        #[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, IntoPrimitive, TryFromPrimitive)]
        #[repr(u8)]
        $tag_vis enum $instr_tag_name {
            $($name = $value),*
        }

        impl $instr_enum_name {
            $vis fn tag(&self) -> $instr_tag_name {
                unsafe { *(self as *const Self as *const u8 as *const $instr_tag_name) }
            }
        }
    };
}

instructions! {
    pub enum Instruction(pub InstructionTag) {
        LoadU16 { dest: u8, imm: u16 } = 0,
        Add { dest: u8, left: u8, right: u8 } = 1,
        LessThan { dest: u8, left: u8, right: u8 } = 2,
        GreaterThan { dest: u8, left: u8, right: u8 } = 3,
        Equal { dest: u8, left: u8, right: u8 } = 4,
        Branch { offset: i24 } = 5,
        BranchIf { cond: u8, offset: i16 } = 6,
        Return { src: u8 } = 7,
        Call { dest: u8, src: u8, count: u8 } = 8,
    }
}

const _: () = const {
    assert!(size_of::<Instruction>() == 4);
};

// #[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
// pub struct Instruction(pub u32);
//
// impl Instruction {
//     pub fn instruction_tag(&self) -> Option<InstructionTag> {
//         InstructionTag::try_from(self.0.to_le_bytes()[0]).ok()
//     }
//
//     pub fn as_reg_u16(&self) -> (u8, u16) {
//         let [_, reg, imm16 @ ..] = self.0.to_le_bytes();
//         (reg, u16::from_le_bytes(imm16))
//     }
//
//     pub fn as_reg_i16(&self) -> (u8, i16) {
//         let [_, reg, imm16 @ ..] = self.0.to_le_bytes();
//         (reg, i16::from_le_bytes(imm16))
//     }
//
//     pub fn as_reg_reg_reg(&self) -> (u8, u8, u8) {
//         let [_, a, b, c] = self.0.to_le_bytes();
//         (a, b, c)
//     }
//
//     pub fn as_i24(&self) -> i24 {
//         let [_, imm24 @ ..] = self.0.to_le_bytes();
//         i24::from_le_bytes(imm24)
//     }
// }

// pub struct Decoder<'src> {
//     instructions: &'src [u32],
//     index: usize,
// }
//
// impl<'src> Decoder<'src> {
//     pub fn new(instructions: &[u32]) -> Decoder {
//         Decoder { instructions, index: 0 }
//     }
//
//     pub fn num_instructions(&self) -> u32 {
//         self.instructions.len() as u32
//     }
//
//     pub fn index(&self) -> u32 {
//         self.index as u32
//     }
//
//     pub fn set_index(&mut self, index: u32) {
//         self.index = index as usize;
//     }
//
//     #[inline(always)]
//     pub fn decode_next(&mut self) -> Option<DecodedInstruction> {
//         let instr = Instruction(*self.instructions.get(self.index)?);
//         self.index += 1;
//         DecodedInstruction::decode(instr)
//     }
// }
//
// impl<'src> Iterator for Decoder<'src> {
//     type Item = DecodedInstruction;
//
//     fn next(&mut self) -> Option<DecodedInstruction> {
//         self.decode_next()
//     }
// }
