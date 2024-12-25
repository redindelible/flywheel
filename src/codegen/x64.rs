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
fn modrm(mod_: u8, reg: impl Into<u8>, rm: impl Into<u8>) -> u8 {
    ((mod_ & 0b11) << 6) | ((reg.into() & 0b111) << 3) | (rm.into() & 0b111)
}

#[inline(always)]
fn sib(scale: impl Into<u8>, index: impl Into<u8>, base: impl Into<u8>) -> u8 {
    ((scale.into() & 0b11) << 6) | ((index.into() & 0b111) << 3) | (base.into() & 0b111)
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

#[repr(u8)]
#[derive(Copy, Clone)]
pub enum Scale {
    X1 = 0,
    X2 = 1,
    X4 = 2,
    X8 = 3
}

impl From<Scale> for u8 {
    fn from(value: Scale) -> Self { value as u8 }
}

#[derive(Copy, Clone)]
pub enum Addressing {
    Direct(Reg),
    IndirectReg(Reg),
    SIB {
        base: Option<Reg>,
        scale: Scale,
        index: Reg,
        disp: i32
    },
}

impl Addressing {
    #[inline]
    fn generate<A: Asm>(&self, instruction: u8, reg: impl Into<u8>, block: &mut Block<A>) {
        let reg = reg.into();
        match *self {
            Addressing::Direct(src) => {
                block.write([
                    rex(true, reg & 0b1000 != 0, false, src.ext() == 1),
                    instruction,
                    modrm(0b11, reg, src)
                ]);
            }
            Addressing::IndirectReg(src) => {
                if src.lower() == 0b101 {
                    block.write([
                        rex(true, reg & 0b1000 != 0, false, src.ext() == 1),
                        instruction,
                        modrm(0b01, reg, 0b101),
                        0x00
                    ]);
                } else if src.lower() == 0b100 {
                    block.write([
                        rex(true, reg & 0b1000 != 0, false, src.ext() == 1),
                        instruction,
                        modrm(0b00, reg, 0b100),
                        sib(0b00, 0b100, 0b100)
                    ]);
                } else {
                    block.write([
                        rex(true, reg & 0b1000 != 0, false, src.ext() == 1),
                        instruction,
                        modrm(0b00, reg, src)
                    ]);
                }
            }
            Addressing::SIB { base: Option::None, scale: Scale::X1, index: src, disp } => {
                if let Ok(disp) = i8::try_from(disp) {
                    if src.lower() == 0b100 {
                        block.write([
                            rex(true, reg & 0b1000 != 0, false, src.ext() == 1),
                            instruction,
                            modrm(0b01, reg, 0b100),
                            sib(0b00, 0b100, 0b100)
                        ]);
                        block.write(disp.to_le_bytes());
                    } else {
                        block.write([
                            rex(true, reg & 0b1000 != 0, false, src.ext() == 1),
                            instruction,
                            modrm(0b01, reg, src)
                        ]);
                        block.write(disp.to_le_bytes());
                    }
                } else if src.lower() == 0b100 {
                    block.write([
                        rex(true, reg & 0b1000 != 0, false, src.ext() == 1),
                        instruction,
                        modrm(0b10, reg, 0b100),
                        sib(0b00, 0b100, 0b100)
                    ]);
                    block.write(disp.to_le_bytes());
                } else {
                    block.write([
                        rex(true, reg & 0b1000 != 0, false, src.ext() == 1),
                        instruction,
                        modrm(0b10, reg, src)
                    ]);
                    block.write(disp.to_le_bytes());
                }
            }
            Addressing::SIB { base: Option::None, scale, index: src, disp } => {
                if src == Reg::RSP {
                    panic!("Not supported in x64");
                } else {
                    block.write([
                        rex(true, reg & 0b1000 != 0, src.ext() == 1, false),
                        instruction,
                        modrm(0b00, reg, 0b100),
                        sib(scale, src.lower(), 0b101)
                    ]);
                    block.write(disp.to_le_bytes());
                }
            }
            Addressing::SIB { base: Some(base), scale, index: src, disp } => {
                if src.lower() == 0b100 {
                    panic!("Not supported in x64");
                }
                if disp == 0 && base.lower() != 0b101 {
                    block.write([
                        rex(true, reg & 0b1000 != 0, src.ext() == 1, base.ext() == 1),
                        instruction,
                        modrm(0b00, reg, 0b100),
                        sib(scale, src.lower(), base.lower())
                    ]);
                } else if let Ok(disp) = i8::try_from(disp) {
                    block.write([
                        rex(true, reg & 0b1000 != 0, src.ext() == 1, base.ext() == 1),
                        instruction,
                        modrm(0b01, reg, 0b100),
                        sib(scale, src.lower(), base.lower())
                    ]);
                    block.write(disp.to_le_bytes());
                } else {
                    block.write([
                        rex(true, reg & 0b1000 != 0, src.ext() == 1, base.ext() == 1),
                        instruction,
                        modrm(0b10, reg, 0b100),
                        sib(scale, src.lower(), base.lower())
                    ]);
                    block.write(disp.to_le_bytes());
                }
            }
        }
    }
}

impl<'a> X64Writer<'a> {
    #[inline]
    pub fn mov_r64_rm64(&mut self, dst: Reg, src: Addressing) {
        src.generate(0x8B, dst, self.block);
    }

    #[inline]
    pub fn mov_rm64_r64(&mut self, dst: Addressing, src: Reg) {
        dst.generate(0x89, src, self.block);
    }

    #[inline]
    pub fn lea_r64_rm64(&mut self, dst: Reg, src: Addressing) {
        debug_assert!(!matches!(&src, Addressing::Direct(_)));
        src.generate(0x8D, dst, self.block);
    }

    #[inline]
    pub fn add_rm64_imm32(&mut self, dst: Addressing, imm: i32) {
        if let Ok(imm) = i8::try_from(imm) {
            dst.generate(0x83, 0b0000, self.block);
            self.block.write(imm.to_le_bytes());
        } else {
            dst.generate(0x81, 0b0000, self.block);
            self.block.write(imm.to_le_bytes());
        }
    }

    #[inline]
    pub fn add_rm64_r64(&mut self, dst: Addressing, src: Reg) {
        dst.generate(0x01, src, self.block);
    }

    #[inline]
    pub fn add_r64_rm64(&mut self, dst: Reg, src: Addressing) {
        src.generate(0x03, dst, self.block);
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
    use crate::codegen::x64::{Addressing, Reg, X64};

    #[test]
    fn test_sized_forward_jumps() {
        for len in 0..=256 {
            let mut code: CodeBuilder<X64> = CodeBuilder::new();
            let entry = code.add_block();
            let stuff = code.add_block();
            let exit = code.add_block();

            code.build(entry, |builder| {
                builder.mov_r64_rm64(Reg::RAX, Addressing::Direct(Reg::RCX));
                builder.jump(exit);
            });
            code.build(stuff, |builder| {
                builder.nops(len);
            });
            code.build(exit, |builder| {
                builder.ret();
            });

            let _ = code.finish(|size| vec![0; size]);
        }
    }

    #[test]
    fn test_sized_backward_jumps() {
        for len in 0..=256 {
            let mut code: CodeBuilder<X64> = CodeBuilder::new();
            let entry = code.add_block();
            let stuff = code.add_block();
            let back = code.add_block();
            let exit = code.add_block();

            code.build(entry, |builder| {
                // builder.mov_r64_r64(Reg::RAX, Reg::RCX);
                builder.jump(back);
            });
            code.build(stuff, |builder| {
                builder.nops(len);
                builder.jump(exit);
            });
            code.build(back, |builder| {
                builder.jump(stuff);
            });
            code.build(exit, |builder| {
                builder.ret();
            });

            let _ = code.finish(|size| vec![0; size]);
        }
    }
}