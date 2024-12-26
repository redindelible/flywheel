use std::time::Instant;
use crate::frontend::FrontendDriver;

mod codegen;
mod interpreter;
mod utils;
mod frontend;

fn main() {
    let mut frontend = FrontendDriver::new();
    let source = frontend.add_string_source(include_str!("../test/simple-struct.fly"), "simple.fly".into());
    
    let start = Instant::now();
    let maybe_ast = frontend.parse_source(source.id());
    match maybe_ast {
        Ok(ast) => {
            print!("{}", ast.pretty(2));
        },
        Err(e) => { dbg!(e); }
    }
    dbg!(start.elapsed());
}
