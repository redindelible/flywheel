mod value;
mod instr;
mod builder;
mod thin;
// mod stack;

use std::marker::PhantomData;
use std::ptr::NonNull;
use std::sync::Arc;
use std::time::Instant;

use crate::builder::CodeChunk;
use crate::instr::{Imm, Instruction, InstructionCode, Reg};
use crate::thin::ThinList;
use crate::value::{UnwrappedValue, Value};

macro_rules! define {
    { $vis:vis unsafe fn $name:ident($frame:ident: $frame_ty:ty) -> $ret:ty { $( $op:ident { $($field:ident),* } => $body:expr )* } } => {
        $vis unsafe fn $name(mut $frame: $frame_ty) -> $ret {
            loop {
                let opcode = unsafe { $frame.ip().get_code() };
                match opcode {
                    $(
                        InstructionCode::$op => {
                            let instr::instrs::$op { $($field),* } = unsafe { instr::instrs::$op::from_ptr($frame.ip().0.as_ptr()) };
                            $body;
                        }
                    ),*
                }
                unsafe { $frame.offset_ip(1) };
            }
        }
    };
}

type DispatchResult = Result<Value, ()>;

define! {
    unsafe fn execute(frame: CallFrame) -> DispatchResult {
        lzi16 { dst, imm } => unsafe {
            frame.write_unchecked(dst.index(), Value::from_i32(imm.as_i32()));
        }
        iaddrr { dst, left, right } => unsafe {
            let left = frame.read_unchecked(left.index()).as_int_unchecked();
            let right = frame.read_unchecked(right.index()).as_int_unchecked();
            frame.write_unchecked(dst.index(), Value::from_i32(left + right));
        }
        imulrr { dst, left, right } => unsafe {
            let left = frame.read_unchecked(left.index()).as_int_unchecked();
            let right = frame.read_unchecked(right.index()).as_int_unchecked();
            frame.write_unchecked(dst.index(), Value::from_i32(left * right));
        }
        idivrr { dst, left, right } => unsafe {
            let left = frame.read_unchecked(left.index()).as_int_unchecked();
            let right = frame.read_unchecked(right.index()).as_int_unchecked();
            frame.write_unchecked(dst.index(), Value::from_i32(left / right));
        }
        imodrr { dst, left, right } => unsafe {
            let left = frame.read_unchecked(left.index()).as_int_unchecked();
            let right = frame.read_unchecked(right.index()).as_int_unchecked();
            frame.write_unchecked(dst.index(), Value::from_i32(left % right));
        }
        iaddri { dst, left, right } => unsafe {
            let left = frame.read_unchecked(left.index()).as_int_unchecked();
            frame.write_unchecked(dst.index(), Value::from_i32(left + right.as_i32()));
        }
        imulri { dst, left, right } => unsafe {
            let left = frame.read_unchecked(left.index()).as_int_unchecked();
            frame.write_unchecked(dst.index(), Value::from_i32(left * right.as_i32()));
        }
        idivri { dst, left, right } => unsafe {
            let left = frame.read_unchecked(left.index()).as_int_unchecked();
            frame.write_unchecked(dst.index(), Value::from_i32(left / right.as_i32()));
        }
        imodri { dst, left, right } => unsafe {
            let left = frame.read_unchecked(left.index()).as_int_unchecked();
            frame.write_unchecked(dst.index(), Value::from_i32(left % right.as_i32()));
        }
        ieqrr { dst, left, right } => unsafe {
            let left = frame.read_unchecked(left.index()).as_int_unchecked();
            let right = frame.read_unchecked(right.index()).as_int_unchecked();
            frame.write_unchecked(dst.index(), Value::from_bool(left == right));
        }
        ieqri { dst, left, right } => unsafe {
            let left = frame.read_unchecked(left.index()).as_int_unchecked();
            frame.write_unchecked(dst.index(), Value::from_bool(left == (right.as_i32())));
        }
        jtr { src, off } => unsafe {
            let bool = frame.read_unchecked(src.index()).as_bool_unchecked();
            if bool {
                frame.offset_ip(off.as_i32() as isize)
            }
        }
        jmp { _pad, off } => unsafe {
            frame.offset_ip(off.as_i32() as isize)
        }
        ret { src, _pad } => unsafe {
            let value = frame.read_unchecked(src.index());
            if let Some(_frame) = frame.pop_call_frame() {
                todo!()
            } else {
                return Ok(value)
            }
        }
    }
}

#[derive(Copy, Clone)]
struct InstrPtr<'f>(NonNull<u32>, PhantomData<&'f u32>);

impl<'f> InstrPtr<'f> {
    fn new(instrs: &'f [u32]) -> Self {
        Self(NonNull::<[u32]>::from(instrs).cast::<u32>(), PhantomData)
    }

    unsafe fn get_code(&self) -> InstructionCode {
        unsafe { self.0.cast::<InstructionCode>().read() }
    }

    unsafe fn offset(&mut self, amount: isize) -> Self {
        unsafe { Self(self.0.byte_offset(amount * 4), PhantomData) }
    }
}

struct CallFrameHeader<'f> {
    ip: InstrPtr<'f>,
    prev: Option<CallFrame<'f>>
}

struct CallFrame<'f> {
    inner: ThinList<Value, CallFrameHeader<'f>>
}

impl<'f> CallFrame<'f> {
    fn new_initial(function: &'f Function) -> Self {
        let ip = InstrPtr::new(function.code.instructions());
        let required = function.code.required_registers();

        Self {
            inner: ThinList::from_header_fill_copy(CallFrameHeader { ip, prev: None }, Value::new_none(), required as usize)
        }
    }
    
    fn ip(&self) -> InstrPtr<'f> {
        self.inner.header().ip
    }
    
    unsafe fn offset_ip(&mut self, offset: isize) {
        let ip_ref = &mut self.inner.header_mut().ip;
        *ip_ref = unsafe { (*ip_ref).offset(offset) };
    }

    unsafe fn write_unchecked(&mut self, register: impl Into<usize>, value: Value) {
        unsafe {
            let register = register.into();
            debug_assert!(register < self.inner.len());
            *self.inner.slice_mut().get_unchecked_mut(register) = value;
        }
    }

    unsafe fn read_unchecked(&self, register: impl Into<usize>) -> Value {
        unsafe {
            let register = register.into();
            debug_assert!(register < self.inner.len());
            *self.inner.slice().get_unchecked(register)
        }
    }

    unsafe fn pop_call_frame(self) -> Option<Self> {
        self.inner.into_header().prev.take()
    }
}

struct VirtualMachine {

}

impl VirtualMachine {
    fn new() -> Self {
        VirtualMachine {

        }
    }

    #[inline(never)]
    fn execute<'f>(&mut self, function: &'f Function, args: &[Value]) -> Result<Value, ()> {
        assert!(args.len() <= function.parameters as usize);
        let result = unsafe {
            let mut frame = CallFrame::new_initial(function);
            for (i, arg) in args.iter().copied().enumerate() {
                frame.write_unchecked(i, arg)
            }
            execute(frame)
        };
        result
    }
}


struct Function {
    code: Arc<CodeChunk>,
    parameters: u32
}


struct Interpreter {

}


fn main() {
    use instr::instrs::*;

    let code = CodeChunk::builder()
        .instr(lzi16 { dst: Reg(1), imm: Imm(0) })

        .instr(ieqri { dst: Reg(2), left: Reg(0), right: Imm(1) })
        .instr(jtr { src: Reg(2), off: Imm(9) })

        .instr(imodri { dst: Reg(2), left: Reg(0), right: Imm(2) })
        .instr(ieqri { dst: Reg(2), left: Reg(2), right: Imm(1) })
        .instr(jtr { src: Reg(2), off: Imm(2) })

        .instr(idivri { dst: Reg(0), left: Reg(0), right: Imm(2) })
        .instr(jmp { _pad: Reg(0), off: Imm(2) })

        .instr(imulri { dst: Reg(0), left: Reg(0), right: Imm(3) })
        .instr(iaddri { dst: Reg(0), left: Reg(0), right: Imm(1) })

        .instr(iaddri { dst: Reg(1), left: Reg(1), right: Imm(1) })
        .instr(jmp { _pad: Reg(0), off: Imm(-11) })

        .instr(ret { src: Reg(1), _pad: Imm(0) })
        .finish();

    let function = Function { parameters: 1, code: Arc::new(code) };
    
    let mut vm = VirtualMachine::new();

    let start = Instant::now();
    for _ in 0..10000 {
        let result = vm.execute(&function, &[Value::from_i32(6171)]);
        match result.unwrap().unwrap() {
            UnwrappedValue::Integer(num) => assert_eq!(num, 261),
            other => panic!("{:?}", other)
        };
    }
    
    let feature = if cfg!(feature = "threaded-loop") {
        "Threaded Loop"
    } else {
        "Match"
    };
    println!("(Using {feature}) Took {:?}", start.elapsed());
}


#[cfg(test)]
mod test {
    use std::sync::Arc;
    use crate::{CodeChunk, Function, VirtualMachine};
    use crate::instr::{Imm, Reg};
    use crate::value::{UnwrappedValue, Value};

    #[test]
    fn test_collatz() {
        use crate::instr::instrs::*;

        let function = CodeChunk::builder()
            .instr(lzi16 { dst: Reg(1), imm: Imm(0) })
            
            .instr(ieqri { dst: Reg(2), left: Reg(0), right: Imm(1) })
            .instr(jtr { src: Reg(2), off: Imm(9) })
            
            .instr(imodri { dst: Reg(2), left: Reg(0), right: Imm(2) })
            .instr(ieqri { dst: Reg(2), left: Reg(2), right: Imm(1) })
            .instr(jtr { src: Reg(2), off: Imm(2) })
            
            .instr(idivri { dst: Reg(0), left: Reg(0), right: Imm(2) })
            .instr(jmp { _pad: Reg(0), off: Imm(2) })
            
            .instr(imulri { dst: Reg(0), left: Reg(0), right: Imm(3) })
            .instr(iaddri { dst: Reg(0), left: Reg(0), right: Imm(1) })
            
            .instr(iaddri { dst: Reg(1), left: Reg(1), right: Imm(1) })
            .instr(jmp { _pad: Reg(0), off: Imm(-11) })
            
            .instr(ret { src: Reg(1), _pad: Imm(0) })
            .finish();
        
        let function = Function { code: Arc::new(function), parameters: 1 };

        let mut vm = VirtualMachine::new();

        let result = vm.execute(&function, &[Value::from_i32(6171)]);
        match result.unwrap().unwrap() {
            UnwrappedValue::Integer(num) => assert_eq!(num, 261),
            other => panic!("{:?}", other)
        };
    }
}
