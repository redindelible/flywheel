use std::hint::unreachable_unchecked;

use thin_vec::{ThinVec, thin_vec};

use crate::bytecode::{Function, Instruction, InstructionTag};
use crate::value::{UnwrappedValue, Value};

struct Context {
    call_stack: Vec<CallFrame>,
    registers: Vec<Value>,
}

struct CallFrame {
    registers_start: usize,
    registers_count: usize,
    instr: *const Instruction,
}

struct Registers {
    registers: *mut Value,
}

impl Registers {
    unsafe fn get(&self, index: impl Into<u32>) -> Value {
        *self.registers.wrapping_add(index.into() as usize)
    }

    unsafe fn set(&mut self, index: impl Into<u32>, value: Value) {
        *self.registers.wrapping_add(index.into() as usize) = value;
    }
}

macro_rules! dispatch_table {
    [..$d:expr $(, $i:path => $v:expr)* $(,)?] => {
        const {
            let mut arr: [InstrFn; 256] = [$d; 256];
            #[allow(unreachable_code)]
            if false {
                match (unreachable!() as InstructionTag) {
                    $($i => ()),*
                }
            }
            $(
            arr[$i as usize] = $v;
            )*
            arr
        }
    };
}

type InstrFn = unsafe fn(Instruction, *const Instruction, Registers, &mut Context);
const DISPATCH_TABLE: [InstrFn; 256] = dispatch_table![
    ..default_instr,
    InstructionTag::LoadU16 => instr_load_u16,
    InstructionTag::Add => instr_add,
    InstructionTag::LessThan => instr_less_than,
    InstructionTag::GreaterThan => instr_greater_than,
    InstructionTag::Equal => instr_equal,
    InstructionTag::Branch => instr_branch,
    InstructionTag::BranchIf => instr_branch_if,
    InstructionTag::Return => instr_return,
];

#[inline(always)]
unsafe fn dispatch(mut instr_ptr: *const Instruction, registers: Registers, context: &mut Context) {
    instr_ptr = instr_ptr.wrapping_offset(1); // todo ptr.offset()
    let instr = *instr_ptr;
    let tag = instr.tag();
    DISPATCH_TABLE.get_unchecked(tag as usize)(instr, instr_ptr, registers, context)
}

unsafe fn instr_return(
    instr: Instruction,
    instr_ptr: *const Instruction,
    mut registers: Registers,
    context: &mut Context,
) {
    let Instruction::Return { src } = instr else { unreachable_unchecked() };
    let value = registers.get(src);
    context.call_stack.pop().unwrap_unchecked();
    if let Some(new_frame) = context.call_stack.last() {
        registers.registers = context.registers.as_mut_ptr().add(new_frame.registers_start);
    }
}

unsafe fn instr_load_u16(
    instr: Instruction,
    instr_ptr: *const Instruction,
    mut registers: Registers,
    context: &mut Context,
) {
    let Instruction::LoadU16 { dest, imm } = instr else { unreachable_unchecked() };
    registers.set(dest, Value::from_i32(imm as i32));
    dispatch(instr_ptr, registers, context)
}

unsafe fn instr_add(
    instr: Instruction,
    instr_ptr: *const Instruction,
    mut registers: Registers,
    context: &mut Context,
) {
    let Instruction::Add { dest, left, right } = instr else { unreachable_unchecked() };
    let left = registers.get(left);
    let right = registers.get(right);

    use UnwrappedValue::*;
    let value = match (left.unwrap(), right.unwrap()) {
        (Float(left), Float(right)) => Value::from_float(left + right),
        (Integer(left), Float(right)) => Value::from_float(left as f64 + right),
        (Float(left), Integer(right)) => Value::from_float(left + right as f64),
        (Integer(left), Integer(right)) => Value::from_i32(left + right),
        (_, _) => todo!(),
    };

    registers.set(dest, value);
    dispatch(instr_ptr, registers, context)
}

unsafe fn instr_less_than(
    instr: Instruction,
    instr_ptr: *const Instruction,
    mut registers: Registers,
    context: &mut Context,
) {
    let Instruction::LessThan { dest, left, right } = instr else { unreachable_unchecked() };
    let left = registers.get(left);
    let right = registers.get(right);

    use UnwrappedValue::*;
    let value = match (left.unwrap(), right.unwrap()) {
        (Float(left), Float(right)) => Value::from_bool(left < right),
        (Integer(left), Float(right)) => Value::from_bool((left as f64) < right),
        (Float(left), Integer(right)) => Value::from_bool(left < (right as f64)),
        (Integer(left), Integer(right)) => Value::from_bool(left < right),
        (_, _) => todo!(),
    };

    registers.set(dest, value);
    dispatch(instr_ptr, registers, context)
}

unsafe fn instr_equal(
    instr: Instruction,
    instr_ptr: *const Instruction,
    mut registers: Registers,
    context: &mut Context,
) {
    let Instruction::Equal { dest, left, right } = instr else { unreachable_unchecked() };
    let left = registers.get(left);
    let right = registers.get(right);

    use UnwrappedValue::*;
    let value = match (left.unwrap(), right.unwrap()) {
        (Float(left), Float(right)) => Value::from_bool(left == right),
        (Integer(left), Float(right)) => Value::from_bool((left as f64) == right),
        (Float(left), Integer(right)) => Value::from_bool(left == (right as f64)),
        (Integer(left), Integer(right)) => Value::from_bool(left == right),
        (_, _) => todo!(),
    };

    registers.set(dest, value);
    dispatch(instr_ptr, registers, context)
}

unsafe fn instr_greater_than(
    instr: Instruction,
    instr_ptr: *const Instruction,
    mut registers: Registers,
    context: &mut Context,
) {
    let Instruction::GreaterThan { dest, left, right } = instr else { unreachable_unchecked() };
    let left = registers.get(left);
    let right = registers.get(right);

    use UnwrappedValue::*;
    let value = match (left.unwrap(), right.unwrap()) {
        (Float(left), Float(right)) => Value::from_bool(left > right),
        (Integer(left), Float(right)) => Value::from_bool((left as f64) > right),
        (Float(left), Integer(right)) => Value::from_bool(left > (right as f64)),
        (Integer(left), Integer(right)) => Value::from_bool(left > right),
        (_, _) => todo!(),
    };

    registers.set(dest, value);
    dispatch(instr_ptr, registers, context)
}

unsafe fn instr_branch(instr: Instruction, instr_ptr: *const Instruction, registers: Registers, context: &mut Context) {
    let Instruction::Branch { offset } = instr else { unreachable_unchecked() };
    dispatch(instr_ptr.wrapping_offset(offset.to_isize()), registers, context)
}

unsafe fn instr_branch_if(
    instr: Instruction,
    mut instr_ptr: *const Instruction,
    registers: Registers,
    context: &mut Context,
) {
    let Instruction::BranchIf { cond, offset } = instr else { unreachable_unchecked() };
    let cond = registers.get(cond);
    if let UnwrappedValue::Bool(cond) = cond.unwrap() {
        if cond {
            instr_ptr = instr_ptr.wrapping_offset(offset as isize);
        }
    } else {
        todo!()
    }
    dispatch(instr_ptr, registers, context)
}

unsafe fn default_instr(_: Instruction, instr_ptr: *const Instruction, registers: Registers, context: &mut Context) {
    dispatch(instr_ptr, registers, context)
}

fn run_function(function: &Function) {
    let mut context = Context {
        call_stack: vec![CallFrame {
            registers_count: function.required_stack() as usize,
            instr: function.bytecode().as_ptr(),
        }],
    };
    let registers = Registers { registers: thin_vec![Value::new_none(); function.required_stack() as usize] };
    unsafe {
        let start = function.bytecode().as_ptr();
        dispatch(start, registers, &mut context);
    }
}
