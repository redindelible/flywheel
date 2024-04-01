use crate::codegen::Block;
use crate::codegen::code_builder::BlockKey;

#[repr(u8)]
#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub enum Reg {
    RAX = 0b0000,
    RCX = 0b0001,
    RDX = 0b0010,
    RBX = 0b0011,
    RSP = 0b0100,
    RBP = 0b0101,
    RSI = 0b0110,
    RDI = 0b0111,
    R8  = 0b1000,
    R9  = 0b1001,
    R10 = 0b1010,
    R11 = 0b1011,
    R12 = 0b1100,
    R13 = 0b1101,
    R14 = 0b1110,
    R15 = 0b1111,
}

impl Reg {
    #[inline(always)]
    fn lower(&self) -> u8 {
        *self as u8 & 0b0111
    }

    #[inline(always)]
    fn is_extended(&self) -> bool {
        (*self as u8 & 0b1000) != 0
    }
}

impl From<Reg> for u8 {
    fn from(value: Reg) -> Self {
        value as u8
    }
}


#[inline(always)]
fn rex(w: bool, r: bool, x: bool, b: bool) -> u8 {
    0b01000000 | ((w as u8) << 3) | ((r as u8) << 2) | ((x as u8) << 1) | (b as u8)
}

#[inline(always)]
fn modrm_direct(r: impl Into<u8>, rm: Reg) -> u8 {
    0b11000000 | ((r.into() & 0b111) << 3) | rm.lower()
}


pub struct X64Writer<'a> {
    block: &'a mut Block
}

impl<'a> X64Writer<'a> {
    pub fn new(block: &'a mut Block) -> Self {
        X64Writer { block }
    }

    pub fn mov_r64_r64(&mut self, dst: Reg, src: Reg) {
        self.block.write([
            rex(true, dst.is_extended(), false, src.is_extended()),
            0x8B,
            modrm_direct(dst, src)
        ]);
    }

    pub fn jump(&mut self, dst: BlockKey) {
        self.block.write([
            0xE9
        ]);
        self.block.write_relocation_i32(dst, -4);
    }

    pub fn ret(&mut self) {
        self.block.write([
            0xC3
        ]);
    }
}