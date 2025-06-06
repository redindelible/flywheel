mod value;
mod instr;
mod builder;
mod thin;
mod utils;
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
            frame = frame.push_call_frame(this_fn, args.start as usize, args.count as usize, dst.index_u32());
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
    ip: InstrPtr<'f>,
    stack: &'vm Bump,
    inner: ThinList<Value, CallFrameHeader<'f>>
}

impl<'vm, 'f> CallFrame<'vm, 'f> {
    fn new_initial(stack: &'vm Bump, function: &'f Function) -> Self {
        let ip = InstrPtr::new(&function.code.instructions);
        let required = function.code.required_registers;
        
        let header = CallFrameHeader {
            ip,
            function,
            prev: None
        };

        let mut frame = ThinList::from_header_uninit_in(header, required as usize, stack);
        for slot in frame.slice_mut() {
            slot.write(Value::new_none());
        }
        let frame = unsafe { frame.assume_init() };

        CallFrame {
            ip,
            stack,
            inner: frame
        }
    }

    unsafe fn push_call_frame(self, function: &'f Function, start: usize, count: usize, dst: u32) -> CallFrame<'vm, 'f> {
        let CallFrame { ip: old_ip, stack: bump, mut inner } = self;
        inner.header_mut().ip = old_ip;
        
        let ip = InstrPtr::new(&function.code.instructions);
        let required = function.code.required_registers;
        let header = CallFrameHeader {
            ip,
            function,
            prev: Some((inner, dst))
        };

        let mut frame = ThinList::from_header_uninit_in(header, required as usize, bump);
        let (header, slice) = frame.parts_mut();
        let prev_frame = &header.prev.as_ref().unwrap().0;
        
        let (parameters, others) = slice.split_at_mut(count);
        unsafe { utils::copy_silly(parameters.as_mut_ptr().cast(), prev_frame.slice()[start..].as_ptr(), count); }
        unsafe { utils::set_zero_silly(others.as_mut_ptr().cast::<Value>(), others.len()); }
        // others.iter_mut().for_each(|slot| { slot.write(Value::new_none()); });
        
        let frame = unsafe { frame.assume_init() };

        CallFrame { ip, stack: bump, inner: frame }
    }

    unsafe fn pop_call_frame(self) -> Option<(Self, u32)> {
        let CallFrame { ip: _, stack: bump, inner } = self;

        let (new_inner, dst) = inner.deallocate(bump).prev?;
        Some((CallFrame { ip: new_inner.header().ip, stack: bump, inner: new_inner }, dst))
    }
    
    fn ip(&self) -> InstrPtr<'f> {
        self.ip
        // self.inner.header().ip
    }
    
    unsafe fn offset_ip(&mut self, offset: isize) {
        unsafe { self.ip = self.ip.offset(offset); }
        // let ip_ref = &mut self.inner.header_mut().ip;
        // *ip_ref = unsafe { (*ip_ref).offset(offset) };
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
            let mut frame = CallFrame::new_initial(&self.stack, function);
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
    for _ in 0..300 {
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
    #[cfg_attr(miri, ignore)]
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
            UnwrappedValue::Integer(num) => assert_eq!(num, 317811),
            other => panic!("{:?}", other)
        };
    }
}
