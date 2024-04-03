use crate::codegen::CodeBuilder;
use crate::codegen::x64::{Addressing, Reg, Scale, X64};

mod keymap;
mod codegen;


#[allow(unused)]
fn as_hex_string(s: &[u8]) -> String {
    let mut str = String::with_capacity(s.len() * 2);
    for c in s {
        str.push_str(&format!("{:02X?}", c));
    }
    str
}


fn main() {
    let mut code: CodeBuilder<X64> = CodeBuilder::new();
    let entry = code.add_block();
    let stuff = code.add_block();
    let exit = code.add_block();

    code.build(entry, |builder| {
        builder.mov_r64_rm64(Reg::RAX, Addressing::IndirectReg(Reg::RCX));
        builder.mov_rm64_r64(Addressing::IndirectReg(Reg::RCX), Reg::RDX);
        builder.jump(exit);
    });
    code.build(stuff, |builder| {
        // builder.mov_r64_r64(Reg::RAX, Reg::RCX);
        // builder.jump(entry);
    });
    code.build(exit, |builder| {
        builder.ret();
    });

    let mem = code.finish(|size| memmap2::MmapMut::map_anon(size).unwrap(), |mem| mem);
    dbg!(as_hex_string(&mem));
    let exec = mem.make_exec().unwrap();
    let ptr = exec.as_ptr();

    let mut ret = 0;
    let num = unsafe {
        let f: unsafe extern "C" fn (*mut i32, i32) -> i32 = std::mem::transmute(ptr);
        f(&mut ret as *mut i32, 5);
    };

    println!("{:?}", ret);
}
