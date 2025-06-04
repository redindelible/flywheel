mod value;
mod instr;
mod builder;
mod thin;
// mod stack;

use std::cell::OnceCell;
use std::marker::PhantomData;
use std::ops::Range;
use std::ptr::NonNull;
use std::sync::Arc;
use std::time::Instant;

use crate::builder::CodeChunk;
use crate::instr::{Imm, Instruction, InstructionCode, Reg, RegRange};
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
        isubri { dst, left, right } => unsafe {
            let left = frame.read_unchecked(left.index()).as_int_unchecked();
            frame.write_unchecked(dst.index(), Value::from_i32(left - right.as_i32()));
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
        iltri { dst, left, right } => unsafe {
            let left = frame.read_unchecked(left.index()).as_int_unchecked();
            frame.write_unchecked(dst.index(), Value::from_bool(left < (right.as_i32())));
        }
        igtri { dst, left, right } => unsafe {
            let left = frame.read_unchecked(left.index()).as_int_unchecked();
            frame.write_unchecked(dst.index(), Value::from_bool(left > (right.as_i32())));
        }
        jif { src, off } => unsafe {
            let bool = frame.read_unchecked(src.index()).as_bool_unchecked();
            if bool {
                frame.offset_ip(off.as_i32() as isize)
            }
        }
        jifn { src, off } => unsafe {
            let bool = frame.read_unchecked(src.index()).as_bool_unchecked();
            if !bool {
                frame.offset_ip(off.as_i32() as isize)
            }
        }
        jmp { _pad, off } => unsafe {
            frame.offset_ip(off.as_i32() as isize)
        }
        callr { dst, args } => unsafe {
            let this_fn = frame.inner.header().function;
            let range = args.start as usize..(args.start as usize+args.count as usize);
            frame = frame.push_call_frame(this_fn, range, dst.index_u32());
            continue;
        }
        ret { src, _pad } => unsafe {
            let value = frame.read_unchecked(src.index());
            if let Some((new_frame, dst)) = frame.pop_call_frame() {
                frame = new_frame;
                frame.write_unchecked(dst as usize, value);
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

type Bump = bump_scope::Bump<allocator_api2::alloc::Global, 8, false>;
// type Bump = bump_scope::Bump;

struct CallFrameHeader<'f> {
    ip: InstrPtr<'f>,
    function: &'f Function,
    prev: Option<(ThinList<Value, CallFrameHeader<'f>>, u32)>
}

struct CallFrame<'vm, 'f> {
    bump: &'vm Bump,
    inner: ThinList<Value, CallFrameHeader<'f>>
}

impl<'vm, 'f> CallFrame<'vm, 'f> {
    fn new_initial(bump: &'vm Bump, function: &'f Function) -> Self {
        let required = function.code.required_registers;
        let header = CallFrameHeader {
            ip: InstrPtr::new(&function.code.instructions),
            function,
            prev: None
        };
        
        let inner = ThinList::from_header_zeroed_in(header, required as usize, bump);

        CallFrame {
            bump,
            inner
        }
    }

    unsafe fn push_call_frame(self, function: &'f Function, args_from: Range<usize>, dst: u32) -> CallFrame<'vm, 'f> {
        let CallFrame { bump, inner } = self;
        let required = function.code.required_registers;
        let header = CallFrameHeader {
            ip: InstrPtr::new(&function.code.instructions),
            function,
            prev: Some((inner, dst))
        };

        let mut new = ThinList::from_header_zeroed_in(header, required as usize, bump);
        let (header, slice) = new.parts_mut();
        let prev_frame = &header.prev.as_ref().unwrap().0;

        slice[..args_from.len()].copy_from_slice(&prev_frame.slice()[args_from]);

        CallFrame { bump, inner: new }
    }

    unsafe fn pop_call_frame(self) -> Option<(Self, u32)> {
        let CallFrame { bump, inner } = self;
        
        let (new_inner, dst) = inner.deallocate(bump).prev?;
        Some((CallFrame { bump, inner: new_inner }, dst))
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
            *self.inner.get_unchecked_mut(register) = value;
        }
    }

    unsafe fn read_unchecked(&self, register: impl Into<usize>) -> Value {
        unsafe {
            let register = register.into();
            debug_assert!(register < self.inner.len());
            *self.inner.get_unchecked(register)
        }
    }
}

struct VirtualMachine {
    stack: Bump,
}

impl VirtualMachine {
    fn new() -> Self {
        VirtualMachine {
            stack: Bump::with_size(8 * 1024)
        }
    }

    #[inline(never)]
    fn execute<'f>(&mut self, function: &'f Function, args: &[Value]) -> Result<Value, ()> {
        assert!(args.len() <= function.code.parameters as usize);
        let result = unsafe {
            let mut frame = CallFrame::new_initial(&mut self.stack, function);
            for (i, arg) in args.iter().copied().enumerate() {
                frame.write_unchecked(i, arg)
            }
            execute(frame)
        };
        assert_eq!(self.stack.stats().allocated(), 0);
        result
    }
}


struct Function {
    code: Arc<CodeChunk>,
    called: OnceCell<Box<[Arc<Function>]>>
}


struct Interpreter {

}


fn main() {
    use instr::instrs::*;

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


#[cfg(test)]
mod test {
    use std::cell::OnceCell;
    use std::sync::Arc;
    use crate::{CodeChunk, Function, VirtualMachine};
    use crate::instr::{Imm, Reg, RegRange};
    use crate::value::{UnwrappedValue, Value};

    #[test]
    fn test_collatz() {
        use crate::instr::instrs::*;

        let function = CodeChunk::builder(1)
            .instr(lzi16 { dst: Reg(1), imm: Imm(0) })
            
            .instr(ieqri { dst: Reg(2), left: Reg(0), right: Imm(1) })
            .instr(jif { src: Reg(2), off: Imm(9) })
            
            .instr(imodri { dst: Reg(2), left: Reg(0), right: Imm(2) })
            .instr(ieqri { dst: Reg(2), left: Reg(2), right: Imm(1) })
            .instr(jif { src: Reg(2), off: Imm(2) })
            
            .instr(idivri { dst: Reg(0), left: Reg(0), right: Imm(2) })
            .instr(jmp { _pad: Reg(0), off: Imm(2) })
            
            .instr(imulri { dst: Reg(0), left: Reg(0), right: Imm(3) })
            .instr(iaddri { dst: Reg(0), left: Reg(0), right: Imm(1) })
            
            .instr(iaddri { dst: Reg(1), left: Reg(1), right: Imm(1) })
            .instr(jmp { _pad: Reg(0), off: Imm(-11) })
            
            .instr(ret { src: Reg(1), _pad: Imm(0) })
            .finish();
        
        let function = Function { code: Arc::new(function), called: OnceCell::from(vec![].into_boxed_slice()) };

        let mut vm = VirtualMachine::new();

        let result = vm.execute(&function, &[Value::from_i32(6171)]);
        match result.unwrap().unwrap() {
            UnwrappedValue::Integer(num) => assert_eq!(num, 261),
            other => panic!("{:?}", other)
        };
    }

    #[test]
    fn test_fibo_rec_sequence() {
        use crate::instr::instrs::*;

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

        let function = Function { code: Arc::new(code), called: OnceCell::from(vec![].into_boxed_slice()) };

        let mut vm = VirtualMachine::new();

        let pairs = [(0, 0), (1, 1), (2, 1), (3, 2), (4, 3), (5, 5), (6, 8), (7, 13), (8, 21), (9, 34)];
        for (index, expected) in pairs {
            let result = vm.execute(&function, &[Value::from_i32(index)]);
            match result.unwrap().unwrap() {
                UnwrappedValue::Integer(num) => assert_eq!(num, expected),
                other => panic!("{:?}", other)
            };
        }
    }

    #[test]
    fn test_fibo_rec_high() {
        use crate::instr::instrs::*;

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

        let function = Function { code: Arc::new(code), called: OnceCell::from(vec![].into_boxed_slice()) };

        let mut vm = VirtualMachine::new();

        let result = vm.execute(&function, &[Value::from_i32(28)]);
        match result.unwrap().unwrap() {
            UnwrappedValue::Integer(num) => assert_eq!(num, 610),
            other => panic!("{:?}", other)
        };
    }
}
