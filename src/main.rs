use crate::codegen::CodeBuilder;
use crate::codegen::x64::{Reg, X64Writer};

mod keymap;
mod codegen;


fn as_hex_string(s: &[u8]) -> String {
    let mut str = String::with_capacity(s.len() * 2);
    for c in s {
        str.push_str(&format!("{:02X?}", c));
    }
    str
}


fn main() {
    let mut code = CodeBuilder::new();
    let entry = code.add_block();

    code.build(entry, |builder| {
        let mut builder = X64Writer::new(builder);

        builder.mov_r64_r64(Reg::RAX, Reg::RCX);
        builder.ret()
    });

    let mem = code.finish(|size| memmap2::MmapMut::map_anon(size).unwrap(), |mem| mem);
    dbg!(as_hex_string(&mem));
    let exec = mem.make_exec().unwrap();
    let ptr = exec.as_ptr();
    let num = unsafe {
        let f: unsafe extern "C" fn (i32) -> i32 = std::mem::transmute(ptr);
        f(4)
    };

    println!("{:?}", num);
}
