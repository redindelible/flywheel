use crate::codegen::CodeBuilder;
use crate::codegen::x64::{Addressing, Reg, X64};
use crate::parser::{lex, Parser};

mod keymap;
mod codegen;
mod ast;
mod parser;
mod bytecode;
mod interpreter;


fn main() {
//     let tokens = lex(r"
// if b:
//     return 4
// else:
//     return val
//         ").unwrap();
//     dbg!(Parser::from_tokens(tokens).parse_stmt());
}
