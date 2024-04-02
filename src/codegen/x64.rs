use crate::codegen::Block;
use crate::codegen::code_builder::{Asm, BlockKey};

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
    fn ext(&self) -> u8 {
        (*self as u8 & 0b1000) >> 3
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


pub enum X64Fixup {
    Jump
}

pub struct X64;

impl Asm for X64 {
    type Writer<'a> = X64Writer<'a>;
    type Fixup = X64Fixup;

    fn new_writer(block: &mut Block<X64>) -> Self::Writer<'_> {
        X64Writer { block }
    }

    fn fix_pessimistic_size(data: &X64Fixup) -> u8 {
        match data {
            X64Fixup::Jump => 5
        }
    }

    fn fix_size_estimate(offset_estimate: isize, data: &X64Fixup) -> u8 {
        match data {
            X64Fixup::Jump => {
                if offset_estimate == 5 {
                    0
                } else if -128 <= (offset_estimate + 5 - 2) || (offset_estimate + 5 - 2) <= 127 {
                    2
                } else {
                    5
                }
            }
        }
    }

    fn fix<'m>(raw_offset: isize, size: u8, data: &X64Fixup, mem: &mut &'m mut [u8]) {
        use std::io::Write;

        match data {
            X64Fixup::Jump => {
                if size == 0 {
                    debug_assert_eq!(raw_offset, 0);
                } else if size == 2 {
                    let _ = mem.write(&[0xEB]);
                    let _ = mem.write(&((raw_offset - 2) as i8).to_le_bytes());
                } else if size == 5 {
                    let _ = mem.write(&[0xE9]);
                    let _ = mem.write(&((raw_offset - 5) as i32).to_le_bytes());
                } else {
                    unreachable!()
                }
            }
        }
    }
}

pub struct X64Writer<'a> {
    block: &'a mut Block<X64>
}

impl<'a> X64Writer<'a> {
    pub fn mov_r64_r64(&mut self, dst: Reg, src: Reg) {
        self.block.write([
            rex(true, dst.ext() == 1, false, src.ext() == 1),
            0x8B,
            modrm_direct(dst, src)
        ]);
    }

    pub fn nops(&mut self, mut num: usize) {
        loop {
            match num {
                0 => break,
                1 => { self.block.write([0x90]); break },
                2 => { self.block.write([0x66, 0x90]); break },
                3 => { self.block.write([0x0F, 0x1F, 0x00]); break },
                4 => { self.block.write([0x0F, 0x1F, 0x40, 0x00]); break },
                5 => { self.block.write([0x0F, 0x1F, 0x44, 0x00, 0x00]); break },
                6 => { self.block.write([0x66, 0x0F, 0x1F, 0x44, 0x00, 0x00]); break },
                7 => { self.block.write([0x0F, 0x1F, 0x80, 0x00, 0x00, 0x00, 0x00]); break },
                8 => { self.block.write([0x0F, 0x1F, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00]); break },
                _ => { self.block.write([0x66, 0x0F, 0x1F, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00]); num -= 9; },
            }
        }
    }

    pub fn jump(&mut self, dst: BlockKey) {
        self.block.write_fixup(dst, X64Fixup::Jump);
    }

    pub fn ret(&mut self) {
        self.block.write([0xC3]);
    }
}


#[cfg(test)]
mod test {
    use crate::codegen::CodeBuilder;
    use crate::codegen::x64::{Reg, X64};

    #[test]
    fn test_sized_forward_jumps() {
        for len in 0..=256 {
            let mut code: CodeBuilder<X64> = CodeBuilder::new();
            let entry = code.add_block();
            let stuff = code.add_block();
            let exit = code.add_block();

            code.build(entry, |builder| {
                builder.mov_r64_r64(Reg::RAX, Reg::RCX);
                builder.jump(exit);
            });
            code.build(stuff, |builder| {
                builder.nops(len);
            });
            code.build(exit, |builder| {
                builder.ret();
            });

            let _ = code.finish(|size| vec![0; size], |mem| mem);
        }
    }
}