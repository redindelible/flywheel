use crate::interpreter::bytecode::{Function, Instruction, InstructionTag};
use crate::interpreter::value::{UnwrappedValue, Value};

struct StateInner {
    registers: Vec<Value>,
    call_stack: Vec<CallFrame>
}

struct CallFrame {
    registers_count: usize,
    instr: *const u32
}

impl StateInner {
    fn register_mut(&mut self, register: impl Into<usize>) -> Option<&mut Value> {
        self.registers.get_mut(register.into())
    }

    fn register(&self, register: impl Into<usize>) -> Option<&Value> {
        self.registers.get(register.into())
    }
}


macro_rules! dispatch_table {
    [..$d:expr $(, $i:expr => $v:expr)* $(,)?] => {
        const {
            let mut arr: [InstrFn; 256] = [$d; 256];
            $(
            arr[$i as usize] = $v;
            )*
            arr
        }
    };
}

type InstrFn = unsafe fn(&mut StateInner, Instruction, *const u32);
const DISPATCH_TABLE: [InstrFn; 256] = dispatch_table![
    ..default_instr,
    InstructionTag::LoadU16 => instr_load_u16,
    InstructionTag::Add => instr_add,
    InstructionTag::LessThan => |_, _, _| todo!(),
    InstructionTag::GreaterThan => |_, _, _| todo!(),
    InstructionTag::LessEqual => |_, _, _| todo!(),
    InstructionTag::GreaterEqual => |_, _, _| todo!(),
    InstructionTag::Equal => |_, _, _| todo!(),
    InstructionTag::NotEqual => |_, _, _| todo!(),
    InstructionTag::Jump => |_, _, _| todo!(),
    InstructionTag::JumpIfTrue => |_, _, _| todo!(),
    InstructionTag::JumpIfFalse => |_, _, _| todo!(),
    InstructionTag::Return => |_, _, _| todo!()
];

#[inline(always)]
unsafe fn dispatch(state: &mut StateInner, mut instr_ptr: *const u32) {
    let instr = Instruction(*instr_ptr);
    instr_ptr = instr_ptr.offset(1);
    let tag = instr.instruction_tag().unwrap_unchecked();
    DISPATCH_TABLE.get_unchecked(tag as usize)(state, instr, instr_ptr)
}

pub unsafe fn instr_load_u16(state: &mut StateInner, instr: Instruction, instr_ptr: *const u32) {
    let (dest, imm) = instr.as_reg_u16();
    *state.register_mut(dest).unwrap_unchecked() = Value::from_i32(imm as i32);
    dispatch(state, instr_ptr)
}

pub unsafe fn instr_add(state: &mut StateInner, instr: Instruction, instr_ptr: *const u32) {
    let (dest, left, right) = instr.as_reg_reg_reg();
    let left = *state.register(left).unwrap_unchecked();
    let right = *state.register(right).unwrap_unchecked();

    use UnwrappedValue::*;
    let value = match (left.unwrap(), right.unwrap()) {
        (Float(left), Float(right)) => Value::from_float(left + right),
        (Integer(left), Float(right)) => Value::from_float(left as f64 + right),
        (Float(left), Integer(right)) => Value::from_float(left + right as f64),
        (Integer(left), Integer(right)) => Value::from_i32(left + right),
        (_, _) => todo!()
    };

    *state.register_mut(dest).unwrap_unchecked() = value;
    dispatch(state, instr_ptr)
}

pub unsafe fn default_instr(state: &mut StateInner, _: Instruction, instr_ptr: *const u32) {
    dispatch(state, instr_ptr)
}

fn run_function<B: AsRef<[u32]>>(function: &Function<B>) {
    let mut state_inner = StateInner {
        registers: vec![Value::new_none(); function.required_stack() as usize],
        call_stack: vec![]
    };
    unsafe {
        let start = function.bytecode().as_ptr();
        dispatch(&mut state_inner, start);
    }
}