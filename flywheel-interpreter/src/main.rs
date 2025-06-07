use std::cell::OnceCell;
use std::sync::Arc;
use std::time::Instant;

use flywheel_interpreter::builder::CodeChunk;
use flywheel_interpreter::{Function, VirtualMachine};
use flywheel_interpreter::instr::{Imm, Reg, RegRange};
use flywheel_interpreter::value::{UnwrappedValue, Value};

fn main() {
    use flywheel_interpreter::instr::instrs::*;

    let code = CodeChunk::builder(1)
        .instr(iltri { dst: Reg(1), left: Reg(0), right: Imm(2) })
        .instr(jif { src: Reg(1), off: Imm(5) })

        .instr(isubri { dst: Reg(1), left: Reg(0), right: Imm(1) })
        .instr(callr { dst: Reg(1), args: RegRange { start: 1, count: 1 }})
        .instr(isubri { dst: Reg(0), left: Reg(0), right: Imm(2) })
        .instr(callr { dst: Reg(0), args: RegRange { start: 0, count: 1 }})
        .instr(iaddrr { dst: Reg(0), left: Reg(1), right: Reg(0) })

        .instr(ret { src: Reg(0), _pad: Imm(0) })

        .finish();

    let function = Arc::new(Function { code: Arc::new(code), called: OnceCell::new() });
    function.called.set(vec![function.clone()].into_boxed_slice()).ok().unwrap();
    
    let mut vm = VirtualMachine::new();

    let start = Instant::now();
    for _ in 0..5 {
        let result = vm.execute(&function, &[Value::from_i32(28)]);
        match result.unwrap().unwrap() {
            UnwrappedValue::Integer(num) => assert_eq!(num, 317811),
            other => panic!("{:?}", other)
        };
    }
    println!("Took {:?}", start.elapsed());
}

