use num_enum::{IntoPrimitive, TryFromPrimitive};

pub struct Function<B> {
    required_stack: u32,
    bytecode: B,
}

impl<B> Function<B> {
    pub fn required_stack(&self) -> u32 {
        self.required_stack
    }
}

impl<B: AsRef<[u32]>> Function<B> {
    pub fn bytecode(&self) -> &[u32] {
        self.bytecode.as_ref()
    }

    pub fn from_untrusted_bytecode(required_stack: u32, bytecode: B) -> Option<Self> {
        let mut decoder = Decoder::new(bytecode.as_ref());

        let check_register = move |register: u8| if (register as u32) < required_stack { Some(()) } else { None };
        let check_offset = move |decoder: &mut Decoder, offset: i32| {
            let next_index = decoder.index().wrapping_add_signed(offset);
            if (0..decoder.num_instructions()).contains(&next_index) { Some(()) } else { None }
        };

        let mut last_instruction = None;
        while let Some(instruction) = decoder.decode_next() {
            last_instruction = Some(instruction);
            match instruction {
                DecodedInstruction::LoadU16 { dest, imm: _ } => {
                    check_register(dest)?;
                }
                DecodedInstruction::Add { dest, left, right }
                | DecodedInstruction::LessThan { dest, left, right }
                | DecodedInstruction::GreaterThan { dest, left, right }
                | DecodedInstruction::LessEqual { dest, left, right }
                | DecodedInstruction::GreaterEqual { dest, left, right }
                | DecodedInstruction::Equal { dest, left, right }
                | DecodedInstruction::NotEqual { dest, left, right } => {
                    check_register(dest)?;
                    check_register(left)?;
                    check_register(right)?;
                }
                DecodedInstruction::Jump { offset } => {
                    check_offset(&mut decoder, offset.to_i32())?;
                }
                DecodedInstruction::JumpIfTrue { cond, offset } | DecodedInstruction::JumpIfFalse { cond, offset } => {
                    check_register(cond)?;
                    check_offset(&mut decoder, offset as i32)?;
                }
                DecodedInstruction::Return { src } => {
                    check_register(src)?;
                }
            }
        }

        if last_instruction.is_some_and(|last| last.is_terminator()) {
            Some(Function { required_stack, bytecode })
        } else {
            None
        }
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
    fn to_i32(self) -> i32 {
        let [a, b, c] = self.0;
        i32::from_le_bytes([0, a, b, c]) >> 8
    }

    fn from_le_bytes(bytes: [u8; 3]) -> Self {
        i24(bytes)
    }

    fn to_le_bytes(self) -> [u8; 3] {
        self.0
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, IntoPrimitive, TryFromPrimitive)]
#[repr(u8)]
pub enum InstructionTag {
    LoadU16 = 0,
    Add,
    LessThan,
    GreaterThan,
    LessEqual,
    GreaterEqual,
    Equal,
    NotEqual,
    Jump,
    JumpIfTrue,
    JumpIfFalse,
    Return,
}

#[repr(u8)]
#[derive(Copy, Clone)]
pub enum DecodedInstruction {
    LoadU16 { dest: u8, imm: u16 } = InstructionTag::LoadU16 as u8,
    Add { dest: u8, left: u8, right: u8 } = InstructionTag::Add as u8,
    LessThan { dest: u8, left: u8, right: u8 } = InstructionTag::LessThan as u8,
    GreaterThan { dest: u8, left: u8, right: u8 } = InstructionTag::GreaterThan as u8,
    LessEqual { dest: u8, left: u8, right: u8 } = InstructionTag::LessEqual as u8,
    GreaterEqual { dest: u8, left: u8, right: u8 } = InstructionTag::GreaterEqual as u8,
    Equal { dest: u8, left: u8, right: u8 } = InstructionTag::Equal as u8,
    NotEqual { dest: u8, left: u8, right: u8 } = InstructionTag::NotEqual as u8,
    Jump { offset: i24 } = InstructionTag::Jump as u8,
    JumpIfTrue { cond: u8, offset: i16 } = InstructionTag::JumpIfTrue as u8,
    JumpIfFalse { cond: u8, offset: i16 } = InstructionTag::JumpIfFalse as u8,
    Return { src: u8 } = InstructionTag::Return as u8,
}

impl DecodedInstruction {
    pub fn is_terminator(&self) -> bool {
        match self {
            DecodedInstruction::Jump { .. }
            | DecodedInstruction::JumpIfFalse { .. }
            | DecodedInstruction::JumpIfTrue { .. }
            | DecodedInstruction::Return { .. } => true,
            _ => false,
        }
    }

    pub fn tag(&self) -> InstructionTag {
        unsafe { *(self as *const Self as *const u8 as *const InstructionTag) }
    }

    pub fn decode(instr: Instruction) -> Option<Self> {
        Some(match instr.instruction_tag()? {
            InstructionTag::LoadU16 => {
                let (dest, imm) = instr.as_reg_u16();
                DecodedInstruction::LoadU16 { dest, imm }
            }
            InstructionTag::Add => {
                let (dest, left, right) = instr.as_reg_reg_reg();
                DecodedInstruction::Add { dest, left, right }
            }
            InstructionTag::Jump => {
                let offset = instr.as_i24();
                DecodedInstruction::Jump { offset }
            }
            InstructionTag::LessThan => {
                let (dest, left, right) = instr.as_reg_reg_reg();
                DecodedInstruction::LessThan { dest, left, right }
            }
            InstructionTag::GreaterThan => {
                let (dest, left, right) = instr.as_reg_reg_reg();
                DecodedInstruction::GreaterThan { dest, left, right }
            }
            InstructionTag::LessEqual => {
                let (dest, left, right) = instr.as_reg_reg_reg();
                DecodedInstruction::LessEqual { dest, left, right }
            }
            InstructionTag::GreaterEqual => {
                let (dest, left, right) = instr.as_reg_reg_reg();
                DecodedInstruction::GreaterEqual { dest, left, right }
            }
            InstructionTag::Equal => {
                let (dest, left, right) = instr.as_reg_reg_reg();
                DecodedInstruction::Equal { dest, left, right }
            }
            InstructionTag::NotEqual => {
                let (dest, left, right) = instr.as_reg_reg_reg();
                DecodedInstruction::NotEqual { dest, left, right }
            }
            InstructionTag::JumpIfTrue => {
                let (cond, offset) = instr.as_reg_i16();
                DecodedInstruction::JumpIfTrue { cond, offset }
            }
            InstructionTag::JumpIfFalse => {
                let (cond, offset) = instr.as_reg_i16();
                DecodedInstruction::JumpIfFalse { cond, offset }
            }
            InstructionTag::Return => {
                let (src, _) = instr.as_reg_u16();
                DecodedInstruction::Return { src }
            }
        })
    }
}

pub struct Decoder<'src> {
    instructions: &'src [u32],
    index: usize,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct Instruction(pub u32);

impl Instruction {
    pub fn instruction_tag(&self) -> Option<InstructionTag> {
        InstructionTag::try_from(self.0.to_le_bytes()[0]).ok()
    }

    pub fn as_reg_u16(&self) -> (u8, u16) {
        let [_, reg, imm16 @ ..] = self.0.to_le_bytes();
        (reg, u16::from_le_bytes(imm16))
    }

    pub fn as_reg_i16(&self) -> (u8, i16) {
        let [_, reg, imm16 @ ..] = self.0.to_le_bytes();
        (reg, i16::from_le_bytes(imm16))
    }

    pub fn as_reg_reg_reg(&self) -> (u8, u8, u8) {
        let [_, a, b, c] = self.0.to_le_bytes();
        (a, b, c)
    }

    pub fn as_i24(&self) -> i24 {
        let [_, imm24 @ ..] = self.0.to_le_bytes();
        i24::from_le_bytes(imm24)
    }
}

impl<'src> Decoder<'src> {
    pub fn new(instructions: &[u32]) -> Decoder {
        Decoder { instructions, index: 0 }
    }

    pub fn num_instructions(&self) -> u32 {
        self.instructions.len() as u32
    }

    pub fn index(&self) -> u32 {
        self.index as u32
    }

    pub fn set_index(&mut self, index: u32) {
        self.index = index as usize;
    }

    #[inline(always)]
    pub fn decode_next(&mut self) -> Option<DecodedInstruction> {
        let instr = Instruction(*self.instructions.get(self.index)?);
        self.index += 1;
        DecodedInstruction::decode(instr)
    }
}

impl<'src> Iterator for Decoder<'src> {
    type Item = DecodedInstruction;

    fn next(&mut self) -> Option<DecodedInstruction> {
        self.decode_next()
    }
}
