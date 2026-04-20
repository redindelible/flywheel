use std::collections::HashMap;
use arcstr::ArcStr;
use crate::{Block, BlockId, Function, Instruction, Terminator, Type};

#[derive(Copy, Clone)]
pub struct LocalId(u32);

enum BlockInformation {
    New,
    WithPredecessor {
        predecessors: Vec<BlockId>,
        stack: im::Vector<Type>,
    },
    Complete {
        predecessors: Vec<BlockId>,
        stack: im::Vector<Type>,

        instructions: Vec<Instruction>,
        terminator: Terminator,
    }
}

impl BlockInformation {
    fn is_complete(&self) -> bool {
        match self {
            BlockInformation::New | BlockInformation::WithPredecessor { .. } => false,
            BlockInformation::Complete { .. } => true,
        }
    }

    fn add_predecessor(&mut self, predecessor: BlockId, predecessor_stack: &im::Vector<Type>) {
        match self {
            BlockInformation::New => {
                *self = BlockInformation::WithPredecessor {
                    predecessors: vec![predecessor],
                    stack: im::Vector::clone(predecessor_stack),
                }
            }
            BlockInformation::WithPredecessor { predecessors, stack }
            | BlockInformation::Complete { predecessors, stack, .. } => {
                assert!(!predecessors.contains(&predecessor));
                predecessors.push(predecessor);

                for (this, target) in stack.iter().zip(predecessor_stack) {
                    assert_eq!(this, target);
                }
                assert_eq!(stack.len(), predecessor_stack.len());
            }
        }
    }
}

struct BlockBuilder {
    id: BlockId,
    stack: im::Vector<Type>,
    current_instructions: Vec<Instruction>,
    terminator: Option<Terminator>,
}

pub struct FunctionBuilder {
    name: ArcStr,
    parameters: Vec<Type>,
    return_type: Type,

    blocks: HashMap<BlockId, BlockInformation>,
    last_block_id: u32,
    builder: BlockBuilder,
}

impl FunctionBuilder {
    pub fn new(name: &str, arguments: Vec<Type>, return_type: Type) -> FunctionBuilder {
        let stack = im::Vector::from(arguments.clone());
        FunctionBuilder {
            name: ArcStr::from(name),
            parameters: arguments.clone(),
            return_type,
            builder: BlockBuilder {
                id: BlockId(0),
                current_instructions: Vec::new(),
                terminator: None,
                stack: im::Vector::clone(&stack),
            },
            last_block_id: 0,
            blocks: HashMap::from([(BlockId(0), BlockInformation::WithPredecessor {
                predecessors: vec![],
                stack,
            })]),
        }
    }

    pub fn finish(mut self) -> Function {
        self.complete_block();

        Function {
            name: self.name,
            parameters: self.parameters,
            return_type: self.return_type,
            blocks: self.blocks.into_values().map(|info| {
                let BlockInformation::Complete {
                    predecessors: _,
                    stack,
                    instructions,
                    terminator,
                } = info else { panic!() };

                Block {
                    retained_locals: Vec::from_iter(stack),
                    instructions,
                    terminator
                }
            }).collect()
        }
    }

    pub fn new_block(&mut self) -> BlockId {
        self.last_block_id += 1;
        let block_id = self.last_block_id;
        self.blocks.insert(BlockId(block_id), BlockInformation::New);
        BlockId(block_id)
    }

    fn complete_block(&mut self) {
        let block_info_mut = self.blocks.get_mut(&self.builder.id).unwrap();
        let BlockInformation::WithPredecessor { predecessors, stack } = std::mem::replace(block_info_mut, BlockInformation::New) else { panic!() };

        *block_info_mut = BlockInformation::Complete {
            predecessors,
            stack,
            instructions: std::mem::take(&mut self.builder.current_instructions),
            terminator: std::mem::take(&mut self.builder.terminator).unwrap(),
        }
    }

    pub fn switch_block(&mut self, block: BlockId) {
        assert_ne!(self.builder.id, block);

        self.complete_block();

        let new_block = &self.blocks[&block];

        match new_block {
            BlockInformation::New => panic!("new block does not yet have any predecessors"),
            BlockInformation::Complete { .. } => panic!("new block is already complete"),
            BlockInformation::WithPredecessor { stack, .. } => {
                self.builder = BlockBuilder {
                    id: block,
                    stack: im::Vector::clone(stack),
                    current_instructions: vec![],
                    terminator: None
                };
            }
        }
    }

    pub fn mark_local(&mut self) -> LocalId {
        LocalId(self.builder.stack.len() as u32)
    }

    fn add_successor(&mut self, target_id: BlockId) {
        let target = self.blocks.get_mut(&target_id).unwrap();
        target.add_predecessor(self.builder.id, &self.builder.stack);
    }

    pub fn jump(&mut self, target: BlockId) {
        assert!(self.builder.terminator.is_none());

        self.add_successor(target);

        self.builder.terminator = Some(Terminator::Jump { target });
    }

    pub fn if_else(&mut self, then_target: BlockId, else_target: BlockId) {
        assert!(self.builder.terminator.is_none());

        let ty = self.builder.stack.pop_back();
        assert!(matches!(ty, Some(Type::Bool)));

        self.add_successor(then_target);
        self.add_successor(else_target);

        self.builder.terminator = Some(Terminator::IfElse { true_target: then_target, false_target: else_target });
    }

    pub fn pop(&mut self) {
        if self.builder.terminator.is_some() { return; }

        self.builder.stack.pop_back().unwrap();
        self.builder.current_instructions.push(Instruction::Pop);
    }

    pub fn return_(&mut self) {
        assert!(self.builder.terminator.is_none());

        self.builder.stack.pop_back().unwrap();
        self.builder.terminator = Some(Terminator::Return);
    }

    pub fn push_unit(&mut self) {
        if self.builder.terminator.is_some() { return; }

        self.builder.current_instructions.push(Instruction::LoadUnit);
        self.builder.stack.push_back(Type::Unit);
    }

    pub fn push_bool(&mut self, value: bool) {
        if self.builder.terminator.is_some() { return; }

        self.builder.current_instructions.push(if value { Instruction::LoadTrue } else { Instruction::LoadFalse });
        self.builder.stack.push_back(Type::Bool);
    }

    pub fn push_integer(&mut self, value: i64) {
        if self.builder.terminator.is_some() { return; }

        self.builder.current_instructions.push(Instruction::LoadInteger(value));
        self.builder.stack.push_back(Type::Integer);
    }

    pub fn load_local(&mut self, local: LocalId) {
        if self.builder.terminator.is_some() { return; }

        let ty = self.builder.stack.get(local.0 as usize).unwrap().clone();

        self.builder.current_instructions.push(Instruction::LoadLocal { index: local.0 });
        self.builder.stack.push_back(ty);
    }
}
